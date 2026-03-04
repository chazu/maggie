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
	"strings"

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

func main() {
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

	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: mag [options] [paths...]\n")
		fmt.Fprintf(os.Stderr, "       mag deps [resolve|update|list]\n")
		fmt.Fprintf(os.Stderr, "       mag fmt [--check] [files or dirs...]\n")
		fmt.Fprintf(os.Stderr, "       mag wrap [packages...]                 (generate Go bindings)\n")
		fmt.Fprintf(os.Stderr, "       mag build [-o output]                   (compile custom binary)\n")
		fmt.Fprintf(os.Stderr, "       mag sync [push|pull|status]             (content distribution)\n")
		fmt.Fprintf(os.Stderr, "       mag doc [--output dir] [--title title]\n")
		fmt.Fprintf(os.Stderr, "       mag doctest [--verbose] [--class name]\n\n")
		fmt.Fprintf(os.Stderr, "Starts Maggie with the default image and compiles .mag files from the given paths.\n")
		fmt.Fprintf(os.Stderr, "If no paths are given and a maggie.toml exists, loads the project from it.\n\n")
		fmt.Fprintf(os.Stderr, "Options:\n")
		flag.PrintDefaults()
		fmt.Fprintf(os.Stderr, "\nExamples:\n")
		fmt.Fprintf(os.Stderr, "  mag -i                 # Start REPL\n")
		fmt.Fprintf(os.Stderr, "  mag ./src -m main      # Load src/, run 'main' method\n")
		fmt.Fprintf(os.Stderr, "  mag ./... -m App.start # Load recursively, run App.start\n")
		fmt.Fprintf(os.Stderr, "  mag --yutani           # Start Yutani IDE launcher (connects to localhost:7755)\n")
		fmt.Fprintf(os.Stderr, "  mag --yutani --ide-tool inspector  # Start Inspector directly\n")
		fmt.Fprintf(os.Stderr, "  mag --yutani --yutani-addr host:port  # Connect to specific server\n")
		fmt.Fprintf(os.Stderr, "\nImages:\n")
		fmt.Fprintf(os.Stderr, "  mag ./src/... --save-image app.image  # Compile sources, save image\n")
		fmt.Fprintf(os.Stderr, "  mag --image app.image -m Main.start   # Run from custom image\n")
		fmt.Fprintf(os.Stderr, "\nProject & Dependencies:\n")
		fmt.Fprintf(os.Stderr, "  mag -m Main.start      # Load project from maggie.toml, run entry point\n")
		fmt.Fprintf(os.Stderr, "  mag deps               # Resolve and fetch dependencies\n")
		fmt.Fprintf(os.Stderr, "  mag deps update         # Re-resolve ignoring lock file\n")
		fmt.Fprintf(os.Stderr, "  mag deps list           # Show dependency tree\n")
		fmt.Fprintf(os.Stderr, "\nFormatting:\n")
		fmt.Fprintf(os.Stderr, "  mag fmt                        # Format all .mag files in current dir\n")
		fmt.Fprintf(os.Stderr, "  mag fmt lib/Array.mag          # Format a specific file\n")
		fmt.Fprintf(os.Stderr, "  mag fmt --check lib/           # Check formatting (non-zero exit if changes needed)\n")
		fmt.Fprintf(os.Stderr, "\nDocumentation:\n")
		fmt.Fprintf(os.Stderr, "  mag doc                        # Generate HTML docs from image\n")
		fmt.Fprintf(os.Stderr, "  mag doc --output ./site        # Generate to specific directory\n")
		fmt.Fprintf(os.Stderr, "  mag doc --serve                # Generate docs and serve on :8080\n")
		fmt.Fprintf(os.Stderr, "  mag doc --serve --port 3000    # Generate docs and serve on :3000\n")
		fmt.Fprintf(os.Stderr, "  mag doctest                    # Run docstring tests\n")
		fmt.Fprintf(os.Stderr, "  mag doctest --verbose          # Run with detailed output\n")
		fmt.Fprintf(os.Stderr, "\nLanguage Server:\n")
		fmt.Fprintf(os.Stderr, "  mag --serve                    # Start language server on :4567\n")
		fmt.Fprintf(os.Stderr, "  mag ./lib/... --serve --port 8080  # Load libs, serve on :8080\n")
		fmt.Fprintf(os.Stderr, "  mag --lsp                      # Start LSP server on stdio\n")
		fmt.Fprintf(os.Stderr, "  mag lsp                        # Same as --lsp (subcommand form)\n")
		fmt.Fprintf(os.Stderr, "\nExperimental:\n")
		fmt.Fprintf(os.Stderr, "  mag -i --experimental-maggie-compiler  # Use self-hosting compiler\n")
	}
	flag.Parse()

	// Handle subcommands
	args := flag.Args()
	var docMode string  // "doc" or "doctest"
	var docArgs []string
	var syncArgs []string
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
		case "wrap":
			handleWrapCommand(args[1:], *verbose)
			return
		case "build":
			handleBuildCommand(args[1:], *verbose)
			return
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
			os.Exit(1)
		}
	} else {
		if err := vmInst.LoadImageFromBytes(embeddedImage); err != nil {
			fmt.Fprintf(os.Stderr, "Error loading embedded image: %v\n", err)
			os.Exit(1)
		}
	}
	// Re-register critical primitives that may have been overwritten by image methods
	vmInst.ReRegisterNilPrimitives()
	vmInst.ReRegisterBooleanPrimitives()

	// Set up compiler backend (Go compiler by default)
	vmInst.UseGoCompiler(compiler.Compile)

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

	var loadedManifest *manifest.Manifest
	if len(paths) > 0 {
		totalMethods := 0
		for _, path := range paths {
			methods, err := pipe.CompilePath(path)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error: %v\n", err)
				os.Exit(1)
			}
			totalMethods += methods
		}
		if *verbose && totalMethods > 0 {
			fmt.Printf("Compiled %d methods\n", totalMethods)
		}
	} else if docMode != "" || syncArgs != nil || *mainEntry != "" || *saveImagePath != "" {
		// No paths but doc/doctest/main/save-image requested: try loading from maggie.toml
		if m, _ := manifest.FindAndLoad("."); m != nil {
			loadedManifest = m
			methods, err := pipe.LoadProject(m)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error loading project: %v\n", err)
				os.Exit(1)
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
			os.Exit(1)
		}
		if *verbose {
			fmt.Printf("Image saved to %s\n", *saveImagePath)
		}
	}

	// Start sync server if configured via manifest
	if loadedManifest != nil && loadedManifest.Sync.Listen != "" {
		syncAddr := loadedManifest.Sync.Listen
		var syncOpts []server.ServerOption
		syncOpts = append(syncOpts, server.WithCompileFunc(buildCompileFunc(vmInst)))
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
		return
	}
	if docMode == "doctest" {
		handleDoctestCommand(vmInst, docArgs)
		return
	}

	// Handle sync subcommand (after sources are compiled)
	if syncArgs != nil {
		handleSyncCommand(syncArgs, vmInst, loadedManifest, *verbose)
		return
	}

	// Run main entry point if specified
	if *mainEntry != "" {
		result, err := runMain(vmInst, *mainEntry, *verbose)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
		// If main returns a small integer, use it as exit code
		if result.IsSmallInt() {
			os.Exit(int(result.SmallInt()))
		}
		os.Exit(0)
	}

	// Start LSP server if requested
	if *lspMode {
		lsp := server.NewLSP(vmInst)
		if err := lsp.Run(); err != nil {
			fmt.Fprintf(os.Stderr, "LSP error: %v\n", err)
			os.Exit(1)
		}
		os.Exit(0)
	}

	// Start language server if requested
	if *serveMode {
		addr := fmt.Sprintf(":%d", *servePort)
		srv := server.New(vmInst, server.WithCompileFunc(buildCompileFunc(vmInst)))
		defer srv.Stop()
		if err := srv.ListenAndServe(addr); err != nil {
			fmt.Fprintf(os.Stderr, "Server error: %v\n", err)
			os.Exit(1)
		}
		os.Exit(0)
	}

	// Start Yutani IDE mode if requested
	if *yutaniMode {
		if err := runYutaniIDE(vmInst, *yutaniAddr, *yutaniTool, *verbose); err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
		os.Exit(0)
	}

	// Start REPL if requested or if no paths given
	if *interactive || (len(paths) == 0 && *mainEntry == "") {
		runREPL(vmInst)
	}
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

	// Find the class
	class := vmInst.Classes.Lookup(className)
	if class == nil {
		return vm.Nil, fmt.Errorf("class %q not found", className)
	}

	if verbose {
		fmt.Printf("Found class %s\n", className)
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
		classValue := vmInst.Symbols.SymbolValue(className)
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
		fmt.Fprintf(os.Stderr, "Usage: mag deps [resolve|update|list]\n")
		os.Exit(1)
	}
}
