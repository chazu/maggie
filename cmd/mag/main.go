// Maggie CLI - the main entry point for running Maggie programs
package main

import (
	_ "embed"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime/pprof"
	"strings"
	"sync"
	"time"

	"context"
	"crypto/ed25519"
	"net/http"

	"connectrpc.com/connect"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/compiler/hash"
	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
	"github.com/chazu/maggie/gen/maggie/v1/maggiev1connect"
	"github.com/chazu/maggie/manifest"
	"github.com/chazu/maggie/pipeline"
	"github.com/chazu/maggie/server"
	"github.com/chazu/maggie/vm"
	"github.com/chazu/maggie/vm/dist"
)

//go:embed maggie.image
var embeddedImage []byte

// projectEntryPoint is the default entry point for full-system builds.
// When non-empty, the binary runs this entry point when invoked with no
// arguments instead of dropping to the REPL. Set via init() in a generated
// project_config.go during "mag build --full".
var projectEntryPoint string

// projectNamespace is the project namespace for full-system builds.
// Used to qualify the projectEntryPoint class name.
var projectNamespace string

// projectWrapperRegistrars holds Go-wrap package registration functions
// for full-system builds. Populated by init() in generated project_config.go.
var projectWrapperRegistrars []func(*vm.VM)

// reorderArgs moves flags (arguments starting with "-") before positional
// arguments so that Go's flag package parses them even when interspersed
// with paths (e.g., "mag ./src/... --save-image app.image").
//
// Once a known subcommand is encountered, all remaining arguments are passed
// through unchanged so that subcommand-specific flags (like --help) are not
// intercepted by the top-level flag parser.
func reorderArgs(args []string) []string {
	// Subcommands that handle their own flags
	subcommands := map[string]bool{
		"build": true, "deps": true, "fmt": true, "new": true,
		"wrap": true, "sync": true, "doc": true, "doctest": true,
		"help": true, "lsp": true, "test": true, "run": true,
	}
	// Known top-level flags that take a value argument
	valueFlags := map[string]bool{
		"-m": true, "--m": true,
		"-save-image": true, "--save-image": true,
		"-image": true, "--image": true,
		"-port": true, "--port": true,
		"-yutani-addr": true, "--yutani-addr": true,
		"-ide-tool": true, "--ide-tool": true,
		"-profile-rate": true, "--profile-rate": true,
		"-profile-output": true, "--profile-output": true,
	}
	var flags, positional []string
	for i := 0; i < len(args); i++ {
		a := args[i]
		// If this is a known subcommand, pass it and everything after it through as-is
		if subcommands[a] {
			positional = append(positional, args[i:]...)
			break
		}
		if strings.HasPrefix(a, "-") {
			flags = append(flags, a)
			// If this flag takes a value and it's not using "=" syntax, consume next arg too
			if !strings.Contains(a, "=") && valueFlags[a] && i+1 < len(args) {
				i++
				flags = append(flags, args[i])
			}
		} else {
			positional = append(positional, a)
		}
	}
	return append(flags, positional...)
}

func main() {
	os.Exit(run())
}

func run() (exitCode int) {
	// Recover from panics so deferred profiler/pprof flushes still run.
	defer func() {
		if r := recover(); r != nil {
			fmt.Fprintf(os.Stderr, "%v\n", r)
			exitCode = 1
		}
	}()
	os.Args = append(os.Args[:1], reorderArgs(os.Args[1:])...)
	verbose := flag.Bool("v", false, "Verbose output")
	interactive := flag.Bool("i", false, "Start interactive REPL")
	mainEntry := flag.String("m", "", "Main entry point (e.g., 'Main.run' or just 'main')")
	noRC := flag.Bool("no-rc", false, "Skip loading ~/.maggierc")
	yutaniMode := flag.Bool("yutani", false, "Start Yutani IDE mode")
	yutaniAddr := flag.String("yutani-addr", "localhost:7755", "Yutani server address")
	yutaniTool := flag.String("ide-tool", "launcher", "IDE tool to start: launcher, inspector, repl")
	useMaggieCompiler := flag.Bool("experimental-maggie-compiler", false, "Use experimental Maggie self-hosting compiler instead of Go compiler")
	saveImagePath := flag.String("save-image", "", "Save VM state to image file after loading sources")
	customImagePath := flag.String("image", "", "Load custom image instead of embedded default")
	serveMode := flag.Bool("serve", false, "Start language server (gRPC + Connect HTTP/JSON)")
	servePort := flag.Int("port", 4567, "Language server port (used with --serve)")
	lspMode := flag.Bool("lsp", false, "Start LSP server on stdio")
	profileMode := flag.Bool("profile", false, "Enable wall-clock sampling profiler")
	profileRate := flag.Int("profile-rate", 1000, "Sampling rate in Hz (default 1000)")
	profileOutput := flag.String("profile-output", "profile.folded", "Profile output file")
	pprofMode := flag.Bool("pprof", false, "Enable Go pprof CPU profiler")

	flag.Usage = func() {
		p := progName()
		fmt.Fprintf(os.Stderr, "Usage: %s [options] [paths...]\n", p)
		fmt.Fprintf(os.Stderr, "       %s deps [resolve|update|list]\n", p)
		fmt.Fprintf(os.Stderr, "       %s fmt [--check] [files or dirs...]\n", p)
		fmt.Fprintf(os.Stderr, "       %s wrap [packages...]                 (generate Go bindings)\n", p)
		fmt.Fprintf(os.Stderr, "       %s build [--full] [-o output]           (compile custom binary)\n", p)
		fmt.Fprintf(os.Stderr, "       %s help [ClassName|Class>>method]       (show help for classes/methods)\n", p)
		fmt.Fprintf(os.Stderr, "       %s sync [push|pull|status]             (content distribution)\n", p)
		fmt.Fprintf(os.Stderr, "       %s doc [--output dir] [--title title]\n", p)
		fmt.Fprintf(os.Stderr, "       %s doctest [--verbose] [--class name]\n\n", p)
		fmt.Fprintf(os.Stderr, "Starts Maggie with the default image and compiles .mag files from the given paths.\n")
		fmt.Fprintf(os.Stderr, "If no paths are given and a maggie.toml exists, loads the project from it.\n\n")
		fmt.Fprintf(os.Stderr, "Options:\n")
		flag.PrintDefaults()
		fmt.Fprintf(os.Stderr, "\nExamples:\n")
		fmt.Fprintf(os.Stderr, "  %s -i                 # Start REPL\n", p)
		fmt.Fprintf(os.Stderr, "  %s ./src -m main      # Load src/, run 'main' method\n", p)
		fmt.Fprintf(os.Stderr, "  %s ./... -m App.start # Load recursively, run App.start\n", p)
		fmt.Fprintf(os.Stderr, "  %s --yutani           # Start Yutani IDE launcher (connects to localhost:7755)\n", p)
		fmt.Fprintf(os.Stderr, "  %s --yutani --ide-tool inspector  # Start Inspector directly\n", p)
		fmt.Fprintf(os.Stderr, "  %s --yutani --yutani-addr host:port  # Connect to specific server\n", p)
		fmt.Fprintf(os.Stderr, "\nImages:\n")
		fmt.Fprintf(os.Stderr, "  %s ./src/... --save-image app.image  # Compile sources, save image\n", p)
		fmt.Fprintf(os.Stderr, "  %s --image app.image -m Main.start   # Run from custom image\n", p)
		fmt.Fprintf(os.Stderr, "\nProject & Dependencies:\n")
		fmt.Fprintf(os.Stderr, "  %s -m Main.start      # Load project from maggie.toml, run entry point\n", p)
		fmt.Fprintf(os.Stderr, "  %s deps               # Resolve and fetch dependencies\n", p)
		fmt.Fprintf(os.Stderr, "  %s deps update         # Re-resolve ignoring lock file\n", p)
		fmt.Fprintf(os.Stderr, "  %s deps list           # Show dependency tree\n", p)
		fmt.Fprintf(os.Stderr, "\nFormatting:\n")
		fmt.Fprintf(os.Stderr, "  %s fmt                        # Format all .mag files in current dir\n", p)
		fmt.Fprintf(os.Stderr, "  %s fmt lib/Array.mag          # Format a specific file\n", p)
		fmt.Fprintf(os.Stderr, "  %s fmt --check lib/           # Check formatting (non-zero exit if changes needed)\n", p)
		fmt.Fprintf(os.Stderr, "\nProject:\n")
		fmt.Fprintf(os.Stderr, "  %s new myapp                  # Create a new Maggie project\n", p)
		fmt.Fprintf(os.Stderr, "\nHelp:\n")
		fmt.Fprintf(os.Stderr, "  %s help                       # List all classes\n", p)
		fmt.Fprintf(os.Stderr, "  %s help Array                 # Show Array class help\n", p)
		fmt.Fprintf(os.Stderr, "  %s help Array>>at:            # Show method docstring\n", p)
		fmt.Fprintf(os.Stderr, "  %s help Yutani::Button        # Show FQN class help\n", p)
		fmt.Fprintf(os.Stderr, "\nDocumentation:\n")
		fmt.Fprintf(os.Stderr, "  %s doc                        # Generate HTML docs from image\n", p)
		fmt.Fprintf(os.Stderr, "  %s doc --output ./site        # Generate to specific directory\n", p)
		fmt.Fprintf(os.Stderr, "  %s doc --serve                # Generate docs and serve on :8080\n", p)
		fmt.Fprintf(os.Stderr, "  %s doc --serve --port 3000    # Generate docs and serve on :3000\n", p)
		fmt.Fprintf(os.Stderr, "  %s doctest                    # Run docstring tests\n", p)
		fmt.Fprintf(os.Stderr, "  %s doctest --verbose          # Run with detailed output\n", p)
		fmt.Fprintf(os.Stderr, "\nLanguage Server:\n")
		fmt.Fprintf(os.Stderr, "  %s --serve                    # Start language server on :4567\n", p)
		fmt.Fprintf(os.Stderr, "  %s ./lib/... --serve --port 8080  # Load libs, serve on :8080\n", p)
		fmt.Fprintf(os.Stderr, "  %s --lsp                      # Start LSP server on stdio\n", p)
		fmt.Fprintf(os.Stderr, "  %s lsp                        # Same as --lsp (subcommand form)\n", p)
		fmt.Fprintf(os.Stderr, "\nExperimental:\n")
		fmt.Fprintf(os.Stderr, "  %s -i --experimental-maggie-compiler  # Use self-hosting compiler\n", p)
		fmt.Fprintf(os.Stderr, "\nLearning Maggie:\n")
		fmt.Fprintf(os.Stderr, "  %s help <topic>                       # API reference for any class or method\n", p)
		fmt.Fprintf(os.Stderr, "  %s doc --serve                        # Browsable HTML docs on localhost\n", p)
		fmt.Fprintf(os.Stderr, "  %s doctest                            # Run doc tests to verify examples\n", p)
		fmt.Fprintf(os.Stderr, "  examples/                              # Runnable example programs\n")
		fmt.Fprintf(os.Stderr, "  lib/guide/                             # Tutorial chapters (start here!)\n")
	}
	flag.Parse()

	// Handle subcommands
	args := flag.Args()
	var docMode string  // "doc" or "doctest"
	var docArgs []string
	var syncArgs []string
	var helpArgs []string
	var typecheckArgs []string
	if len(args) > 0 {
		switch args[0] {
		case "lsp":
			*lspMode = true
		case "deps":
			handleDepsCommand(args[1:], *verbose)
			return
		case "fmt":
			handleFmtCommand(args[1:])
			return
		case "new":
			handleNewCommand(args[1:])
			return
		case "wrap":
			handleWrapCommand(args[1:], *verbose)
			return
		case "build":
			handleBuildCommand(args[1:], *verbose)
			return
		case "test":
			handleTestCommand(args[1:], *verbose)
			return
		case "run":
			handleRunCommand(args[1:], *verbose)
			return
		case "typecheck":
			typecheckArgs = args[1:]
		case "help":
			helpArgs = args[1:]
		case "sync":
			syncArgs = args[1:]
		case "doc":
			docMode = "doc"
			docArgs = args[1:]
		case "doctest":
			docMode = "doctest"
			docArgs = args[1:]
		}
		// Also look for doc/doctest after source paths (e.g., "mag ./src doc --output dir")
		if docMode == "" {
			for i, arg := range args {
				if arg == "doc" || arg == "doctest" {
					docMode = arg
					docArgs = args[i+1:]
					args = args[:i] // keep only source paths before the subcommand
					break
				}
			}
		}
	}

	// Create VM and load image
	vmInst := vm.NewVM()
	if *customImagePath != "" {
		if err := vmInst.LoadImage(*customImagePath); err != nil {
			fmt.Fprintf(os.Stderr, "Error loading image %s: %v\n", *customImagePath, err)
			return 1
		}
	} else {
		if err := vmInst.LoadImageFromBytes(embeddedImage); err != nil {
			fmt.Fprintf(os.Stderr, "Error loading embedded image: %v\n", err)
			return 1
		}
	}
	// Re-register critical primitives that may have been overwritten by image methods
	vmInst.ReRegisterNilPrimitives()

	// Apply docstrings from .mag <primitive> stubs onto Go-registered primitives.
	// These aren't persisted in the image, so we apply them from generated code.
	applyPrimitiveDocstrings(vmInst)
	vmInst.ReRegisterBooleanPrimitives()

	// Load disk cache into ContentStore (restores previously synced content)
	var diskCache *dist.DiskCache
	cacheDir := ".maggie/cache"
	if dc, err := dist.NewDiskCache(cacheDir); err == nil {
		diskCache = dc
		loaded, loadErr := dc.LoadInto(vmInst.ContentStore())
		if loadErr != nil {
			fmt.Fprintf(os.Stderr, "Warning: failed to load disk cache: %v\n", loadErr)
		} else if *verbose && loaded > 0 {
			fmt.Printf("Loaded %d chunks from disk cache\n", loaded)
		}
	}

	// Set up compiler backend (Go compiler by default)
	vmInst.UseGoCompiler(compiler.Compile)

	// Wire NodeRefFactory for remote messaging
	vmInst.NodeRefFactory = buildNodeRefFactory(vmInst)

	// Wire RemoteChannelFactory for distributed channels
	vmInst.RemoteChannelFactory = buildRemoteChannelFactory(vmInst)

	// Register wrapper packages from full-system builds
	for _, register := range projectWrapperRegistrars {
		register(vmInst)
	}

	// Switch to experimental Maggie compiler if requested
	if *useMaggieCompiler {
		vmInst.UseMaggieCompiler()
		if *verbose {
			fmt.Println("Using experimental Maggie self-hosting compiler")
		}
	}

	// Set up pipeline for compilation
	var verboseWriter io.Writer
	if *verbose {
		verboseWriter = os.Stdout
	}
	pipe := &pipeline.Pipeline{VM: vmInst, Verbose: verboseWriter}

	// Wire up fileIn support so Compiler fileIn: primitives work
	vmInst.SetFileInFunc(func(v *vm.VM, source string, sourcePath string, nsOverride string, verbose bool) (int, error) {
		p := &pipeline.Pipeline{VM: v}
		if verbose {
			p.Verbose = os.Stdout
		}
		return p.CompileSourceFile(source, sourcePath, nsOverride)
	})

	// Wire up batch fileIn for two-pass directory loading (Compiler fileInAll:)
	vmInst.SetFileInBatchFunc(func(v *vm.VM, dirPath string, verbose bool) (int, error) {
		p := &pipeline.Pipeline{VM: v}
		if verbose {
			p.Verbose = os.Stdout
		}
		return p.CompilePath(dirPath + "/...")
	})

	if *verbose {
		fmt.Printf("Loaded default image (%d bytes)\n", len(embeddedImage))
		fmt.Printf("Compiler: %s\n", vmInst.CompilerName())
	}

	// Load ~/.maggierc if it exists
	if !*noRC {
		if err := loadRC(vmInst, *verbose); err != nil {
			fmt.Fprintf(os.Stderr, "Warning: error loading ~/.maggierc: %v\n", err)
		}
	}

	// Start Go pprof CPU profiler if requested
	if *pprofMode {
		f, err := os.Create("cpu.pprof")
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error creating cpu.pprof: %v\n", err)
			return 1
		}
		if err := pprof.StartCPUProfile(f); err != nil {
			fmt.Fprintf(os.Stderr, "Error starting pprof: %v\n", err)
			return 1
		}
		defer func() {
			pprof.StopCPUProfile()
			f.Close()
			if *verbose {
				fmt.Println("Go CPU profile written to cpu.pprof")
			}
		}()
	}

	// Start wall-clock sampling profiler if requested
	if *profileMode {
		interval := time.Second / time.Duration(*profileRate)
		vmInst.StartSamplingProfiler(interval)
		if *verbose {
			fmt.Printf("Sampling profiler started at %d Hz\n", *profileRate)
		}
		defer func() {
			sp := vmInst.StopSamplingProfiler()
			if sp != nil {
				f, err := os.Create(*profileOutput)
				if err != nil {
					fmt.Fprintf(os.Stderr, "Error creating profile output: %v\n", err)
					return
				}
				defer f.Close()
				if err := sp.WriteFoldedStacks(f); err != nil {
					fmt.Fprintf(os.Stderr, "Error writing profile: %v\n", err)
					return
				}
				stats := sp.Stats()
				if *verbose {
					fmt.Printf("Profile written to %s (%d samples, %d dropped)\n",
						*profileOutput, stats.TotalSamples, stats.Dropped)
				} else {
					fmt.Printf("Profile written to %s\n", *profileOutput)
				}
			}
		}()
	}

	// Compile paths from command line or project manifest
	// Use trimmed args if doc/doctest was found mid-args, otherwise flag.Args()
	paths := args
	if *lspMode && len(paths) > 0 && paths[0] == "lsp" {
		paths = paths[1:] // strip "lsp" subcommand from paths
	}
	if docMode != "" && len(paths) > 0 && (paths[0] == "doc" || paths[0] == "doctest") {
		paths = nil // doc/doctest at start: no source paths
	}
	if syncArgs != nil && len(paths) > 0 && paths[0] == "sync" {
		paths = nil // sync at start: no source paths
	}
	if helpArgs != nil && len(paths) > 0 && paths[0] == "help" {
		paths = nil // help at start: no source paths
	}
	if typecheckArgs != nil && len(paths) > 0 && paths[0] == "typecheck" {
		paths = nil // typecheck at start: no source paths
	}

	var loadedManifest *manifest.Manifest
	if len(paths) > 0 {
		totalMethods := 0
		for _, path := range paths {
			methods, err := pipe.CompilePath(path)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error: %v\n", err)
				return 1
			}
			totalMethods += methods
		}
		if *verbose && totalMethods > 0 {
			fmt.Printf("Compiled %d methods\n", totalMethods)
		}
	} else if docMode != "" || syncArgs != nil || helpArgs != nil || *mainEntry != "" || *saveImagePath != "" {
		// No paths but doc/doctest/main/save-image requested: try loading from maggie.toml
		if m, _ := manifest.FindAndLoad("."); m != nil {
			loadedManifest = m
			if projectNamespace == "" {
				projectNamespace = m.Project.Namespace
			}
			methods, err := pipe.LoadProject(m)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error loading project: %v\n", err)
				return 1
			}
			if *verbose && methods > 0 {
				fmt.Printf("Project %s: compiled %d methods\n", m.Project.Name, methods)
			}
			// Use project entry point as main if not specified
			if *mainEntry == "" && m.Source.Entry != "" {
				*mainEntry = m.Source.Entry
			}
		}
	}

	// Save image if requested (after loading all sources, before running main)
	if *saveImagePath != "" {
		if err := vmInst.SaveImage(*saveImagePath); err != nil {
			fmt.Fprintf(os.Stderr, "Error saving image: %v\n", err)
			return 1
		}
		if *verbose {
			fmt.Printf("Image saved to %s\n", *saveImagePath)
		}
	}

	// Wire sync restriction list into the VM for sandboxed execution
	if loadedManifest != nil && len(loadedManifest.Sync.Capabilities) > 0 {
		vmInst.SetSyncRestrictions(loadedManifest.Sync.Capabilities)
	}

	// Start sync server if configured via manifest
	if loadedManifest != nil && loadedManifest.Sync.Listen != "" {
		syncAddr := loadedManifest.Sync.Listen
		peerAddrs := &sync.Map{}
		var syncOpts []server.ServerOption
		syncOpts = append(syncOpts, server.WithCompileFunc(buildCompileFunc(vmInst)))
		if diskCache != nil {
			syncOpts = append(syncOpts, server.WithDiskCache(diskCache))
		}
		trustStore := loadTrustStore(loadedManifest)
		syncOpts = append(syncOpts, server.WithTrustStore(trustStore))
		syncOpts = append(syncOpts, server.WithSpawnResultFunc(buildSpawnResultFunc(vmInst)))
		syncOpts = append(syncOpts, server.WithPullFunc(buildPullFunc(vmInst, peerAddrs)))
		syncOpts = append(syncOpts, server.WithPeerAddrRegistry(peerAddrs))
		srv := server.New(vmInst, syncOpts...)
		go func() {
			if err := srv.ListenAndServe(syncAddr); err != nil {
				fmt.Fprintf(os.Stderr, "Sync server error: %v\n", err)
			}
		}()
		vmInst.LocalListenAddr = syncAddr
		if *verbose {
			fmt.Printf("Sync server listening on %s\n", syncAddr)
		}
	}

	// Handle doc/doctest subcommands (after sources are compiled)
	if docMode == "doc" {
		handleDocCommand(vmInst, docArgs)
		// Check if --serve was requested
		if docArgsContain(docArgs, "--serve") {
			port := docArgPort(docArgs)
			outputDir := docArgOutput(docArgs)
			handleDocServe(vmInst, outputDir, port)
		}
		return 0
	}
	if docMode == "doctest" {
		handleDoctestCommand(vmInst, docArgs)
		return 0
	}

	// Handle sync subcommand (after sources are compiled)
	if syncArgs != nil {
		handleSyncCommand(syncArgs, vmInst, loadedManifest, *verbose, diskCache)
		return 0
	}

	// Handle help subcommand (after sources are compiled)
	if helpArgs != nil {
		handleHelpCommand(vmInst, helpArgs)
		return 0
	}

	// Handle typecheck subcommand (after VM initialized)
	if typecheckArgs != nil {
		handleTypecheckCommand(typecheckArgs, vmInst)
		return 0
	}

	// If --serve and -m are both specified, start the server before the
	// entry point so that the process can receive remote messages.
	if *serveMode && *mainEntry != "" {
		addr := fmt.Sprintf(":%d", *servePort)
		servePeerAddrs := &sync.Map{}
		srv := server.New(vmInst,
			server.WithCompileFunc(buildCompileFunc(vmInst)),
			server.WithPullFunc(buildPullFunc(vmInst, servePeerAddrs)),
			server.WithPeerAddrRegistry(servePeerAddrs),
		)
		go func() {
			if err := srv.ListenAndServe(addr); err != nil {
				fmt.Fprintf(os.Stderr, "Server error: %v\n", err)
			}
		}()
		vmInst.LocalListenAddr = addr
		time.Sleep(50 * time.Millisecond)
		// The serve-only path below will be skipped since mainEntry is set
	}

	// Run main entry point if specified
	if *mainEntry != "" {
		result, err := runMain(vmInst, *mainEntry, *verbose)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			return 1
		}
		// If main returns a small integer, use it as exit code
		if result.IsSmallInt() {
			return int(result.SmallInt())
		}
		return 0
	}

	// Start LSP server if requested
	if *lspMode {
		lsp := server.NewLSP(vmInst)
		if err := lsp.Run(); err != nil {
			fmt.Fprintf(os.Stderr, "LSP error: %v\n", err)
			return 1
		}
		return 0
	}

	// Start language server if requested.
	// If -m is also specified, the server runs in the background and the
	// entry point runs in the foreground. Otherwise, the server is the
	// foreground (blocking) operation.
	if *serveMode {
		addr := fmt.Sprintf(":%d", *servePort)
		servePeerAddrs2 := &sync.Map{}
		srv := server.New(vmInst,
			server.WithCompileFunc(buildCompileFunc(vmInst)),
			server.WithPullFunc(buildPullFunc(vmInst, servePeerAddrs2)),
			server.WithPeerAddrRegistry(servePeerAddrs2),
		)
		defer srv.Stop()
		vmInst.LocalListenAddr = addr

		if *mainEntry != "" {
			// Both --serve and -m: start server in background, entry point is foreground
			go func() {
				if err := srv.ListenAndServe(addr); err != nil {
					fmt.Fprintf(os.Stderr, "Server error: %v\n", err)
				}
			}()
			// Small delay to ensure server is listening before entry point runs
			time.Sleep(50 * time.Millisecond)
			// Fall through to entry point execution below
		} else {
			// Serve-only mode (no -m)
			if err := srv.ListenAndServe(addr); err != nil {
				fmt.Fprintf(os.Stderr, "Server error: %v\n", err)
				return 1
			}
			return 0
		}
	}

	// Start Yutani IDE mode if requested
	if *yutaniMode {
		if err := runYutaniIDE(vmInst, *yutaniAddr, *yutaniTool, *verbose); err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			return 1
		}
		return 0
	}

	// If this is a full-system build with a default entry point, run it
	// when no explicit -m was given and no other action was taken.
	if projectEntryPoint != "" && *mainEntry == "" && len(paths) == 0 && !*interactive {
		result, err := runMain(vmInst, projectEntryPoint, *verbose)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			return 1
		}
		if result.IsSmallInt() {
			return int(result.SmallInt())
		}
		return 0
	}

	// Start REPL if requested or if no paths given
	if *interactive || (len(paths) == 0 && *mainEntry == "") {
		runREPL(vmInst)
	}
	return 0
}

// loadRC loads ~/.maggierc if it exists
func loadRC(vmInst *vm.VM, verbose bool) error {
	home, err := os.UserHomeDir()
	if err != nil {
		return nil // Can't find home dir, skip silently
	}

	rcPath := filepath.Join(home, ".maggierc")
	if _, err := os.Stat(rcPath); os.IsNotExist(err) {
		return nil // No .maggierc, that's fine
	}

	if verbose {
		fmt.Printf("Loading %s\n", rcPath)
	}

	p := &pipeline.Pipeline{VM: vmInst}
	if verbose {
		p.Verbose = os.Stdout
	}
	_, err = p.CompileFile(rcPath)
	return err
}

// runMain executes the specified main entry point
func runMain(vmInst *vm.VM, entry string, verbose bool) (vm.Value, error) {
	var className, methodName string

	// Parse entry point: "ClassName.methodName" or just "methodName"
	if strings.Contains(entry, ".") {
		parts := strings.SplitN(entry, ".", 2)
		className = parts[0]
		methodName = parts[1]
	} else {
		// Look for a global method or Object method
		className = "Object"
		methodName = entry
	}

	if verbose {
		fmt.Printf("Looking for %s.%s\n", className, methodName)
	}

	// Find the class — try bare name first, then with project namespace
	qualifiedName := className
	class := vmInst.Classes.Lookup(className)
	if class == nil && projectNamespace != "" {
		qualifiedName = projectNamespace + "::" + className
		class = vmInst.Classes.Lookup(qualifiedName)
	}
	if class == nil {
		return vm.Nil, fmt.Errorf("class %q not found", className)
	}

	if verbose {
		fmt.Printf("Found class %s\n", qualifiedName)
	}

	// Check for class method first, then instance method
	selectorID := vmInst.Selectors.Intern(methodName)
	classMethod := class.ClassVTable.Lookup(selectorID)
	if classMethod != nil {
		if verbose {
			fmt.Printf("Found class method %s, executing...\n", methodName)
		}
		// Execute class method - send to the class itself
		// Class values are represented as symbols of the class name
		classValue := vmInst.Symbols.SymbolValue(qualifiedName)
		result := vmInst.Send(classValue, methodName, nil)
		return result, nil
	}

	if verbose {
		fmt.Printf("No class method found, checking instance methods...\n")
	}

	// Check for instance method
	method := class.LookupMethod(vmInst.Selectors, methodName)
	if method == nil {
		return vm.Nil, fmt.Errorf("method %q not found on %s", methodName, className)
	}

	if verbose {
		fmt.Printf("Found instance method %s, executing...\n", methodName)
	}

	// Execute instance method on nil
	result := vmInst.Send(vm.Nil, methodName, nil)
	return result, nil
}

// buildCompileFunc creates a compile function suitable for the sync service's
// method chunk verification. It parses source text into an AST and returns
// both the semantic content hash and the typed content hash.
func buildCompileFunc(vmInst *vm.VM) func(string) (dist.CompileResult, error) {
	return func(source string) (dist.CompileResult, error) {
		parser := compiler.NewParser(source)
		methodDef := parser.ParseMethod()
		if errs := parser.Errors(); len(errs) > 0 {
			return dist.CompileResult{}, fmt.Errorf("parse errors: %v", errs)
		}
		semantic := hash.HashMethod(methodDef, nil, nil)
		typed := hash.HashTypedMethod(methodDef, nil, nil)
		return dist.CompileResult{SemanticHash: semantic, TypedHash: typed}, nil
	}
}

// buildNodeRefFactory creates a factory function that produces fully-wired
// NodeRefData instances with gRPC SendFunc and PingFunc.
func buildNodeRefFactory(vmInst *vm.VM) vm.NodeRefFactory {
	return func(addr string, pub ed25519.PublicKey, priv ed25519.PrivateKey) *vm.NodeRefData {
		ref := vm.NewNodeRefData(addr, pub, priv)

		baseURL := "http://" + addr
		client := maggiev1connect.NewSyncServiceClient(http.DefaultClient, baseURL)

		ref.SendFunc = func(envelope []byte) ([]byte, string, string, error) {
			ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
			defer cancel()
			resp, err := client.DeliverMessage(ctx, connect.NewRequest(
				&maggiev1.DeliverMessageRequest{Envelope: envelope}))
			if err != nil {
				return nil, "", "", err
			}
			if !resp.Msg.Success {
				return nil, resp.Msg.ErrorKind, resp.Msg.ErrorMessage, nil
			}
			return resp.Msg.ResponsePayload, "", "", nil
		}

		ref.PingFunc = func() bool {
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()
			_, err := client.Ping(ctx, connect.NewRequest(&maggiev1.PingRequest{}))
			return err == nil
		}

		ref.SpawnFunc = func(spawnBlockBytes []byte) (string, error) {
			// Build a signed envelope containing the SpawnBlock
			envBytes, err := vm.BuildSignedEnvelope(ref, "", "", spawnBlockBytes, false)
			if err != nil {
				return "", fmt.Errorf("spawn: envelope: %w", err)
			}
			ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
			defer cancel()
			spawnReq := connect.NewRequest(
				&maggiev1.SpawnProcessRequest{Envelope: envBytes})
			// Send our listen address so the remote node can pull code back.
			if vmInst.LocalListenAddr != "" {
				spawnReq.Header().Set("X-Maggie-Return-Addr", vmInst.LocalListenAddr)
			}
			resp, err := client.SpawnProcess(ctx, spawnReq)
			if err != nil {
				return "", err
			}
			if !resp.Msg.Accepted {
				return "", fmt.Errorf("spawn rejected: %s", resp.Msg.Error)
			}
			return resp.Msg.ProcessName, nil
		}

		return ref
	}
}

// buildRemoteChannelFactory creates a factory that wires RPC callbacks on
// RemoteChannelRef instances when channels are deserialized from the network.
func buildRemoteChannelFactory(vmInst *vm.VM) func(ref *vm.RemoteChannelRef) {
	// Cache gRPC clients per node address to avoid creating duplicate connections
	clientCache := make(map[string]maggiev1connect.SyncServiceClient)
	var cacheMu sync.Mutex

	getClient := func(ownerNode [32]byte) maggiev1connect.SyncServiceClient {
		nodeRef := vmInst.FindNodeRefByPublicKey(ownerNode)
		if nodeRef == nil {
			return nil
		}
		addr := nodeRef.Addr
		cacheMu.Lock()
		defer cacheMu.Unlock()
		if c, ok := clientCache[addr]; ok {
			return c
		}
		c := maggiev1connect.NewSyncServiceClient(http.DefaultClient, "http://"+addr)
		clientCache[addr] = c
		return c
	}

	return func(ref *vm.RemoteChannelRef) {
		ref.SendFunc = func(channelID uint64, data []byte) error {
			client := getClient(ref.OwnerNode)
			if client == nil {
				return fmt.Errorf("channel: owner node not connected")
			}
			ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
			defer cancel()
			resp, err := client.ChannelSend(ctx, connect.NewRequest(
				&maggiev1.ChannelSendRequest{ChannelId: channelID, Value: data}))
			if err != nil {
				return err
			}
			if !resp.Msg.Success {
				return fmt.Errorf("%s", resp.Msg.Error)
			}
			return nil
		}

		ref.ReceiveFunc = func(channelID uint64) ([]byte, bool, error) {
			client := getClient(ref.OwnerNode)
			if client == nil {
				return nil, false, fmt.Errorf("channel: owner node not connected")
			}
			ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
			defer cancel()
			resp, err := client.ChannelReceive(ctx, connect.NewRequest(
				&maggiev1.ChannelReceiveRequest{ChannelId: channelID}))
			if err != nil {
				return nil, false, err
			}
			if !resp.Msg.Success {
				return nil, false, fmt.Errorf("%s", resp.Msg.Error)
			}
			return resp.Msg.Value, resp.Msg.ChannelOpen, nil
		}

		ref.TrySendFunc = func(channelID uint64, data []byte) (bool, error) {
			client := getClient(ref.OwnerNode)
			if client == nil {
				return false, fmt.Errorf("channel: owner node not connected")
			}
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()
			resp, err := client.ChannelTrySend(ctx, connect.NewRequest(
				&maggiev1.ChannelSendRequest{ChannelId: channelID, Value: data}))
			if err != nil {
				return false, err
			}
			return resp.Msg.Sent, nil
		}

		ref.TryReceiveFunc = func(channelID uint64) ([]byte, bool, bool, error) {
			client := getClient(ref.OwnerNode)
			if client == nil {
				return nil, false, false, fmt.Errorf("channel: owner node not connected")
			}
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()
			resp, err := client.ChannelTryReceive(ctx, connect.NewRequest(
				&maggiev1.ChannelReceiveRequest{ChannelId: channelID}))
			if err != nil {
				return nil, false, false, err
			}
			return resp.Msg.Value, resp.Msg.GotValue, resp.Msg.ChannelOpen, nil
		}

		ref.CloseFunc = func(channelID uint64) error {
			client := getClient(ref.OwnerNode)
			if client == nil {
				return fmt.Errorf("channel: owner node not connected")
			}
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()
			_, err := client.ChannelClose(ctx, connect.NewRequest(
				&maggiev1.ChannelCloseRequest{ChannelId: channelID}))
			return err
		}

		ref.StatusFunc = func(channelID uint64) (int, int, bool, error) {
			client := getClient(ref.OwnerNode)
			if client == nil {
				return 0, 0, true, fmt.Errorf("channel: owner node not connected")
			}
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()
			resp, err := client.ChannelStatus(ctx, connect.NewRequest(
				&maggiev1.ChannelStatusRequest{ChannelId: channelID}))
			if err != nil {
				return 0, 0, true, err
			}
			return int(resp.Msg.Size), int(resp.Msg.Capacity), resp.Msg.Closed, nil
		}
	}
}

// buildSpawnResultFunc creates a callback that delivers forkOn: results
// back to the spawning node via DeliverMessage with __spawn_result__ selector.
func buildSpawnResultFunc(vmInst *vm.VM) func(spawnerID dist.NodeID, futureID uint64, resultBytes []byte, errMsg error) {
	return func(spawnerID dist.NodeID, futureID uint64, resultBytes []byte, errMsg error) {
		// Find the node ref for the spawner
		ref := vmInst.FindNodeRefByPublicKey(spawnerID)
		if ref == nil || ref.SendFunc == nil {
			return
		}

		// Build the spawn result payload
		type spawnResultPayload struct {
			FutureID    uint64 `cbor:"1,keyasint"`
			ResultBytes []byte `cbor:"2,keyasint"`
			ErrorMsg    string `cbor:"3,keyasint,omitempty"`
		}
		payload := spawnResultPayload{FutureID: futureID, ResultBytes: resultBytes}
		if errMsg != nil {
			payload.ErrorMsg = errMsg.Error()
		}
		payloadBytes, err := vm.CborSerialEncode(payload)
		if err != nil {
			return
		}

		envBytes, err := vm.BuildSignedEnvelope(ref, "", vm.SelectorSpawnResult, payloadBytes, false)
		if err != nil {
			return
		}

		go ref.SendFunc(envBytes)
	}
}

// loadTrustStore creates a TrustStore from the manifest's [trust] section.
func loadTrustStore(m *manifest.Manifest) *dist.TrustStore {
	policy := dist.TrustPolicy{
		DefaultPerms: dist.PermSync, // safe default: sync only
		BanThreshold: 3,
	}
	if m != nil && m.Trust.Default != "" {
		policy.DefaultPerms = dist.ParsePerm(m.Trust.Default)
	}
	if m != nil && m.Trust.BanThreshold > 0 {
		policy.BanThreshold = m.Trust.BanThreshold
	}
	if m != nil && len(m.Trust.SpawnRestrictions) > 0 {
		policy.SpawnRestrictions = m.Trust.SpawnRestrictions
	}
	ts := dist.NewTrustStore(policy)
	if m != nil {
		for _, p := range m.Trust.Peers {
			id, err := dist.ParseNodeID(p.ID)
			if err != nil {
				fmt.Fprintf(os.Stderr, "trust: ignoring invalid peer ID %q: %v\n", p.ID, err)
				continue
			}
			ts.AddConfiguredPeer(id, p.Name, dist.ParsePerm(p.Perms))
		}
	}
	return ts
}

// vmConfigFromManifest converts a manifest's [runtime] section into a vm.VMConfig.
// Zero-valued fields are left at zero so that VMConfig.mergeDefaults() fills them.
func vmConfigFromManifest(m *manifest.Manifest) vm.VMConfig {
	if m == nil {
		return vm.VMConfig{}
	}
	cfg := vm.VMConfig{
		MaxStackDepth:   m.Runtime.MaxStackDepth,
		MaxFrameDepth:   m.Runtime.MaxFrameDepth,
		InitialStack:    m.Runtime.InitialStack,
		InitialFrames:   m.Runtime.InitialFrames,
		MailboxCapacity: m.Runtime.MailboxCapacity,
	}
	if m.Runtime.GCInterval != "" {
		if d, err := time.ParseDuration(m.Runtime.GCInterval); err == nil {
			cfg.GCInterval = d
		}
	}
	return cfg
}
