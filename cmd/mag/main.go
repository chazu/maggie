// Maggie CLI - the main entry point for running Maggie programs
package main

import (
	"bufio"
	_ "embed"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime/pprof"
	"sort"
	"strings"
	"time"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/compiler/hash"
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
		"help": true, "lsp": true,
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
		var syncOpts []server.ServerOption
		syncOpts = append(syncOpts, server.WithCompileFunc(buildCompileFunc(vmInst)))
		if diskCache != nil {
			syncOpts = append(syncOpts, server.WithDiskCache(diskCache))
		}
		if len(loadedManifest.Sync.Capabilities) > 0 {
			syncOpts = append(syncOpts, server.WithSyncPolicy(
				dist.NewRestrictedPolicy(loadedManifest.Sync.Capabilities),
			))
		}
		srv := server.New(vmInst, syncOpts...)
		go func() {
			if err := srv.ListenAndServe(syncAddr); err != nil {
				fmt.Fprintf(os.Stderr, "Sync server error: %v\n", err)
			}
		}()
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

	// Start language server if requested
	if *serveMode {
		addr := fmt.Sprintf(":%d", *servePort)
		srv := server.New(vmInst, server.WithCompileFunc(buildCompileFunc(vmInst)))
		defer srv.Stop()
		if err := srv.ListenAndServe(addr); err != nil {
			fmt.Fprintf(os.Stderr, "Server error: %v\n", err)
			return 1
		}
		return 0
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

// runREPL starts an interactive read-eval-print loop
func runREPL(vmInst *vm.VM) {
	fmt.Println("Maggie REPL (type 'exit' to quit, ':help' for commands)")
	fmt.Printf("Compiler: %s\n", vmInst.CompilerName())
	fmt.Println()

	scanner := bufio.NewScanner(os.Stdin)
	lineBuffer := strings.Builder{}

	for {
		// Show prompt
		if lineBuffer.Len() == 0 {
			fmt.Print(">> ")
		} else {
			fmt.Print(".. ")
		}

		if !scanner.Scan() {
			break
		}

		line := scanner.Text()

		// Handle exit
		if lineBuffer.Len() == 0 && (line == "exit" || line == "quit") {
			break
		}

		// Handle REPL commands (start with ':')
		if lineBuffer.Len() == 0 && strings.HasPrefix(line, ":") {
			handleREPLCommand(vmInst, line)
			continue
		}

		// Empty line executes accumulated input
		if line == "" && lineBuffer.Len() > 0 {
			input := strings.TrimSpace(lineBuffer.String())
			lineBuffer.Reset()

			if input != "" {
				evalAndPrint(vmInst, input)
			}
			continue
		}

		// Accumulate lines
		if lineBuffer.Len() > 0 {
			lineBuffer.WriteString("\n")
		}
		lineBuffer.WriteString(line)

		// If line ends with '.', execute immediately
		if strings.HasSuffix(strings.TrimSpace(line), ".") {
			input := strings.TrimSpace(lineBuffer.String())
			lineBuffer.Reset()

			if input != "" {
				evalAndPrint(vmInst, input)
			}
		}
	}

	fmt.Println()
}

// handleREPLCommand handles REPL meta-commands
func handleREPLCommand(vmInst *vm.VM, cmd string) {
	// Parse command and arguments
	parts := strings.Fields(cmd)
	baseCmd := parts[0]

	switch baseCmd {
	case ":help", ":h", ":?":
		if len(parts) > 1 {
			// :help ClassName or :help ClassName>>methodName
			handleHelpLookup(vmInst, parts[1])
			return
		}
		fmt.Println("REPL Commands:")
		fmt.Println("  :help, :h, :?           Show this help")
		fmt.Println("  :help ClassName          Show class documentation")
		fmt.Println("  :help Class>>method      Show method documentation")
		fmt.Println("  :compiler                Show current compiler")
		fmt.Println("  :use-go                  Switch to Go compiler (default)")
		fmt.Println("  :use-maggie              Switch to Maggie compiler (experimental)")
		fmt.Println("  exit, quit               Exit REPL")
	case ":compiler":
		fmt.Printf("Current compiler: %s\n", vmInst.CompilerName())
	case ":use-go":
		vmInst.UseGoCompiler(compiler.Compile)
		fmt.Println("Switched to Go compiler")
	case ":use-maggie":
		vmInst.UseMaggieCompiler()
		fmt.Printf("Switched to Maggie compiler (experimental)\n")
		fmt.Println("Note: Falls back to Go compiler if Maggie compiler unavailable")
	default:
		fmt.Printf("Unknown command: %s (type :help for commands)\n", cmd)
	}
}

// handleHelpCommand handles the "mag help" subcommand.
// With no args, lists all classes. With an arg, shows class or method help.
func handleHelpCommand(vmInst *vm.VM, args []string) {
	if len(args) == 0 {
		// List all classes sorted by name
		classes := vmInst.Classes.All()
		sort.Slice(classes, func(i, j int) bool {
			return classes[i].Name < classes[j].Name
		})
		for _, cls := range classes {
			name := cls.Name
			if cls.Namespace != "" {
				name = cls.Namespace + "::" + cls.Name
			}
			if cls.DocString != "" {
				// Show first line of docstring as summary
				summary := cls.DocString
				if idx := strings.IndexByte(summary, '\n'); idx != -1 {
					summary = summary[:idx]
				}
				fmt.Printf("  %-30s %s\n", name, summary)
			} else {
				fmt.Printf("  %s\n", name)
			}
		}
		return
	}

	// Join all remaining args as the query (handles "mag help Array >> at:" with spaces)
	query := strings.Join(args, " ")
	query = strings.ReplaceAll(query, " >> ", ">>")
	query = strings.TrimSpace(query)

	handleHelpLookup(vmInst, query)
}

// handleHelpLookup handles :help ClassName and :help ClassName>>methodName
func handleHelpLookup(vmInst *vm.VM, query string) {
	// Check for >> separator: ClassName>>methodName
	if idx := strings.Index(query, ">>"); idx != -1 {
		className := query[:idx]
		methodName := query[idx+2:]

		cls := vmInst.Classes.Lookup(className)
		if cls == nil {
			fmt.Printf("Unknown class: %s\n", className)
			return
		}

		cm := cls.MethodNamed(methodName)
		if cm == nil {
			fmt.Printf("%s does not define #%s\n", className, methodName)
			return
		}

		fmt.Printf("%s>>%s\n", className, methodName)
		if cm.DocString() != "" {
			fmt.Printf("\n%s\n", cm.DocString())
		} else {
			fmt.Println("\n(no documentation)")
		}
		return
	}

	// Just a class name
	cls := vmInst.Classes.Lookup(query)
	if cls == nil {
		fmt.Printf("Unknown class: %s\n", query)
		return
	}

	fmt.Printf("%s", vm.FormatClassHelp(cls, vmInst.Selectors))
}

// evalAndPrint compiles and executes an expression, printing the result
func evalAndPrint(vmInst *vm.VM, input string) {
	// Wrap input in a method body if it doesn't look like a method definition
	source := input
	if !looksLikeMethodDef(input) {
		// Wrap as a doIt method
		source = "doIt\n    ^" + strings.TrimSuffix(input, ".")
	}

	// Compile the method using the active compiler backend
	method, err := vmInst.Compile(source, nil)
	if err != nil {
		fmt.Printf("Compile error: %v\n", err)
		return
	}
	if method == nil {
		fmt.Println("Compile error: compiler returned nil")
		return
	}

	// Execute on nil
	result := vmInst.Execute(method, vm.Nil, nil)

	// Print result
	printValue(vmInst, result)
}

// looksLikeMethodDef checks if input appears to be a method definition
func looksLikeMethodDef(input string) bool {
	lines := strings.Split(input, "\n")
	if len(lines) == 0 {
		return false
	}

	first := strings.TrimSpace(lines[0])
	// A method def starts with a selector (word, binary op, or keyword)
	// and the next lines are indented
	if len(lines) == 1 {
		return false // Single line is probably an expression
	}

	// Check if second line is indented
	if len(lines) > 1 && len(lines[1]) > 0 {
		if lines[1][0] == ' ' || lines[1][0] == '\t' {
			// First line doesn't start with whitespace, second does
			if len(first) > 0 && first[0] != ' ' && first[0] != '\t' {
				return true
			}
		}
	}

	return false
}

// printValue prints a value in a readable format
func printValue(vmInst *vm.VM, v vm.Value) {
	switch {
	case v == vm.Nil:
		fmt.Println("nil")
	case v == vm.True:
		fmt.Println("true")
	case v == vm.False:
		fmt.Println("false")
	case v.IsSmallInt():
		fmt.Println(v.SmallInt())
	case v.IsFloat():
		fmt.Println(v.Float64())
	case vm.IsStringValue(v):
		// String values (must check before IsSymbol since both use symbol tag)
		fmt.Printf("'%s'\n", vmInst.Registry().GetStringContent(v))
	case vm.IsDictionaryValue(v):
		// Dictionary values (must check before IsSymbol since both use symbol tag)
		// Call printString to display
		result := vmInst.Send(v, "printString", nil)
		if vm.IsStringValue(result) {
			fmt.Printf("'%s'\n", vmInst.Registry().GetStringContent(result))
		} else {
			fmt.Println("a Dictionary")
		}
	case v.IsSymbol():
		name := vmInst.Symbols.Name(v.SymbolID())
		fmt.Printf("#%s\n", name)
	case v.IsObject():
		// Try to call printString on the object
		result := vmInst.Send(v, "printString", nil)
		if result.IsSmallInt() {
			// printString returned something weird, just show class
			fmt.Println("an Object")
		} else {
			printValue(vmInst, result)
		}
	default:
		fmt.Printf("<%v>\n", v)
	}
}


// buildCompileFunc creates a compile function suitable for the sync service's
// method chunk verification. It parses source text into an AST, compiles it
// to verify validity, and returns the content hash.
func buildCompileFunc(vmInst *vm.VM) func(string) ([32]byte, error) {
	return func(source string) ([32]byte, error) {
		parser := compiler.NewParser(source)
		methodDef := parser.ParseMethod()
		if errs := parser.Errors(); len(errs) > 0 {
			return [32]byte{}, fmt.Errorf("parse errors: %v", errs)
		}
		h := hash.HashMethod(methodDef, nil, nil)
		return h, nil
	}
}


// runYutaniIDE loads the Yutani library and starts the IDE
func runYutaniIDE(vmInst *vm.VM, addr string, tool string, verbose bool) error {
	// Find the yutani library path
	yutaniPath, err := findYutaniLib()
	if err != nil {
		return fmt.Errorf("cannot find Yutani library: %w", err)
	}

	if verbose {
		fmt.Printf("Loading Yutani library from %s\n", yutaniPath)
	}

	// Load all yutani .mag files recursively
	p := &pipeline.Pipeline{VM: vmInst}
	if verbose {
		p.Verbose = os.Stdout
	}
	_, err = p.CompilePath(yutaniPath + "/...")
	if err != nil {
		return fmt.Errorf("loading Yutani library: %w", err)
	}

	// Determine startup code based on tool selection
	var startupCode string
	switch tool {
	case "launcher", "ide", "desktop":
		startupCode = fmt.Sprintf("MaggieDesktop openIn: (YutaniSession connectTo: '%s')", addr)
	case "browser":
		return fmt.Errorf("ClassBrowser is shelved (see lib/yutani/ide/shelved/). Use: launcher, inspector, repl")
	case "inspector":
		startupCode = fmt.Sprintf("Inspector inspect: nil in: (YutaniSession connectTo: '%s')", addr)
	case "repl":
		startupCode = fmt.Sprintf("MaggieREPL openIn: (YutaniSession connectTo: '%s')", addr)
	case "editor":
		return fmt.Errorf("CodeEditor is shelved (see lib/yutani/ide/shelved/). Use: launcher, inspector, repl")
	default:
		return fmt.Errorf("unknown IDE tool: %s (use: launcher, inspector, repl)", tool)
	}

	fmt.Printf("Starting Yutani IDE (%s), connecting to %s...\n", tool, addr)
	fmt.Printf("Make sure the Yutani server is running: yutani server\n")

	// Wrap as a doIt method and execute
	source := "doIt\n    ^" + startupCode
	if verbose {
		fmt.Printf("Compiling: %s\n", startupCode)
	}
	method, err := compiler.Compile(source, vmInst.Selectors, vmInst.Symbols, vmInst.Registry())
	if err != nil {
		return fmt.Errorf("compiling startup code: %w", err)
	}

	if verbose {
		fmt.Println("Executing startup code...")
	}
	result, err := vmInst.ExecuteSafe(method, vm.Nil, nil)
	if err != nil {
		return err
	}
	if verbose {
		fmt.Printf("Execution returned: %v (isNil=%v)\n", result, result == vm.Nil)
	}
	if result == vm.Nil {
		fmt.Println("IDE exited. (If it exited immediately, check that yutani server is running)")
	}
	return nil
}

// findYutaniLib locates the Yutani library directory
func findYutaniLib() (string, error) {
	// Try relative to executable
	exe, err := os.Executable()
	if err == nil {
		exeDir := filepath.Dir(exe)
		// Check ../lib/yutani (for installed binary)
		candidate := filepath.Join(exeDir, "..", "lib", "yutani")
		if info, err := os.Stat(candidate); err == nil && info.IsDir() {
			return candidate, nil
		}
		// Check ../../lib/yutani (for cmd/mag/mag binary)
		candidate = filepath.Join(exeDir, "..", "..", "lib", "yutani")
		if info, err := os.Stat(candidate); err == nil && info.IsDir() {
			return candidate, nil
		}
	}

	// Try relative to current working directory
	cwd, err := os.Getwd()
	if err == nil {
		candidate := filepath.Join(cwd, "lib", "yutani")
		if info, err := os.Stat(candidate); err == nil && info.IsDir() {
			return candidate, nil
		}
	}

	// Try MAGGIE_HOME environment variable
	if home := os.Getenv("MAGGIE_HOME"); home != "" {
		candidate := filepath.Join(home, "lib", "yutani")
		if info, err := os.Stat(candidate); err == nil && info.IsDir() {
			return candidate, nil
		}
	}

	return "", fmt.Errorf("Yutani library not found. Set MAGGIE_HOME or run from project root")
}


// handleDepsCommand handles the "mag deps" subcommand.
func handleDepsCommand(args []string, verbose bool) {
	if wantsHelp(args) {
		subcmdUsage("deps [command]",
			"Manage project dependencies declared in maggie.toml.",
			usageSubcommands([][2]string{
				{"resolve", "Resolve and fetch all dependencies (default)"},
				{"update", "Re-resolve ignoring the lock file"},
				{"list", "Show the dependency tree"},
			}),
			usageExamples([][2]string{
				{"mag deps", "Resolve and fetch dependencies"},
				{"mag deps update", "Re-resolve from scratch"},
				{"mag deps list", "Show dependency tree"},
			}),
		)
	}

	m, err := manifest.FindAndLoad(".")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
	if m == nil {
		fmt.Fprintf(os.Stderr, "Error: no maggie.toml found\n")
		os.Exit(1)
	}

	subcmd := ""
	if len(args) > 0 {
		subcmd = args[0]
	}

	switch subcmd {
	case "", "resolve":
		// Default: resolve and fetch dependencies
		resolver := manifest.NewResolver(m, verbose)
		deps, err := resolver.Resolve()
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error resolving dependencies: %v\n", err)
			os.Exit(1)
		}
		fmt.Printf("Resolved %d dependencies:\n", len(deps))
		for _, dep := range deps {
			fmt.Printf("  %s -> %s\n", dep.Name, dep.LocalPath)
		}

	case "update":
		// Re-resolve ignoring lock file
		// Remove lock file first
		lockPath := m.LockFilePath()
		os.Remove(lockPath)

		resolver := manifest.NewResolver(m, verbose)
		deps, err := resolver.Resolve()
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error resolving dependencies: %v\n", err)
			os.Exit(1)
		}
		fmt.Printf("Updated %d dependencies:\n", len(deps))
		for _, dep := range deps {
			fmt.Printf("  %s -> %s\n", dep.Name, dep.LocalPath)
		}

	case "list":
		// Show resolved dependency tree
		if len(m.Dependencies) == 0 {
			fmt.Println("No dependencies configured.")
			return
		}
		fmt.Printf("Dependencies for %s:\n", m.Project.Name)
		for name, dep := range m.Dependencies {
			if dep.Git != "" {
				tag := dep.Tag
				if tag == "" {
					tag = "(latest)"
				}
				fmt.Printf("  %s: %s @ %s\n", name, dep.Git, tag)
			} else if dep.Path != "" {
				fmt.Printf("  %s: path %s\n", name, dep.Path)
			}
		}

	default:
		fmt.Fprintf(os.Stderr, "Unknown deps subcommand: %s\n", subcmd)
		fmt.Fprintf(os.Stderr, "Usage: %s deps [resolve|update|list]\n", progName())
		os.Exit(1)
	}
}
