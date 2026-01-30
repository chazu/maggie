// Maggie CLI - the main entry point for running Maggie programs
package main

import (
	"bufio"
	_ "embed"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/server"
	"github.com/chazu/maggie/vm"
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
	serveMode := flag.Bool("serve", false, "Start language server (gRPC + Connect HTTP/JSON)")
	servePort := flag.Int("port", 4567, "Language server port (used with --serve)")

	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: mag [options] [paths...]\n\n")
		fmt.Fprintf(os.Stderr, "Starts Maggie with the default image and compiles .mag files from the given paths.\n\n")
		fmt.Fprintf(os.Stderr, "Options:\n")
		flag.PrintDefaults()
		fmt.Fprintf(os.Stderr, "\nExamples:\n")
		fmt.Fprintf(os.Stderr, "  mag -i                 # Start REPL\n")
		fmt.Fprintf(os.Stderr, "  mag ./src -m main      # Load src/, run 'main' method\n")
		fmt.Fprintf(os.Stderr, "  mag ./... -m App.start # Load recursively, run App.start\n")
		fmt.Fprintf(os.Stderr, "  mag --yutani           # Start Yutani IDE launcher (connects to localhost:7755)\n")
		fmt.Fprintf(os.Stderr, "  mag --yutani --ide-tool inspector  # Start Inspector directly\n")
		fmt.Fprintf(os.Stderr, "  mag --yutani --yutani-addr host:port  # Connect to specific server\n")
		fmt.Fprintf(os.Stderr, "\nLanguage Server:\n")
		fmt.Fprintf(os.Stderr, "  mag --serve                    # Start language server on :4567\n")
		fmt.Fprintf(os.Stderr, "  mag ./lib/... --serve --port 8080  # Load libs, serve on :8080\n")
		fmt.Fprintf(os.Stderr, "\nExperimental:\n")
		fmt.Fprintf(os.Stderr, "  mag -i --experimental-maggie-compiler  # Use self-hosting compiler\n")
	}
	flag.Parse()

	// Create VM and load embedded image
	vmInst := vm.NewVM()
	if err := vmInst.LoadImageFromBytes(embeddedImage); err != nil {
		fmt.Fprintf(os.Stderr, "Error loading embedded image: %v\n", err)
		os.Exit(1)
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

	// Compile any paths specified on command line
	paths := flag.Args()
	if len(paths) > 0 {
		totalMethods := 0
		for _, path := range paths {
			methods, err := compilePath(path, vmInst, *verbose)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error: %v\n", err)
				os.Exit(1)
			}
			totalMethods += methods
		}
		if *verbose && totalMethods > 0 {
			fmt.Printf("Compiled %d methods\n", totalMethods)
		}
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

	// Start language server if requested
	if *serveMode {
		addr := fmt.Sprintf(":%d", *servePort)
		srv := server.New(vmInst)
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

	_, err = compileFile(rcPath, vmInst, verbose)
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
	switch cmd {
	case ":help", ":h", ":?":
		fmt.Println("REPL Commands:")
		fmt.Println("  :help, :h, :?     Show this help")
		fmt.Println("  :compiler         Show current compiler")
		fmt.Println("  :use-go           Switch to Go compiler (default)")
		fmt.Println("  :use-maggie       Switch to Maggie compiler (experimental)")
		fmt.Println("  exit, quit        Exit REPL")
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
		fmt.Printf("'%s'\n", vm.GetStringContent(v))
	case vm.IsDictionaryValue(v):
		// Dictionary values (must check before IsSymbol since both use symbol tag)
		// Call printString to display
		result := vmInst.Send(v, "printString", nil)
		if vm.IsStringValue(result) {
			fmt.Printf("'%s'\n", vm.GetStringContent(result))
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

// compilePath compiles .mag files from a path into the VM.
// Supports ./... syntax for recursive loading.
func compilePath(path string, vmInst *vm.VM, verbose bool) (int, error) {
	// Check for recursive pattern
	recursive := false
	if strings.HasSuffix(path, "/...") {
		recursive = true
		path = strings.TrimSuffix(path, "/...")
	}

	// Resolve path
	path, err := filepath.Abs(path)
	if err != nil {
		return 0, fmt.Errorf("invalid path %q: %w", path, err)
	}

	info, err := os.Stat(path)
	if err != nil {
		return 0, fmt.Errorf("cannot access %q: %w", path, err)
	}

	var files []string
	if info.IsDir() {
		if recursive {
			// Walk directory tree
			err = filepath.Walk(path, func(p string, info os.FileInfo, err error) error {
				if err != nil {
					return err
				}
				if !info.IsDir() && strings.HasSuffix(p, ".mag") {
					files = append(files, p)
				}
				return nil
			})
			if err != nil {
				return 0, fmt.Errorf("walking %q: %w", path, err)
			}
		} else {
			// Just this directory
			entries, err := os.ReadDir(path)
			if err != nil {
				return 0, fmt.Errorf("reading %q: %w", path, err)
			}
			for _, e := range entries {
				if !e.IsDir() && strings.HasSuffix(e.Name(), ".mag") {
					files = append(files, filepath.Join(path, e.Name()))
				}
			}
		}
	} else {
		// Single file
		if strings.HasSuffix(path, ".mag") {
			files = append(files, path)
		} else {
			return 0, fmt.Errorf("%q is not a .mag file", path)
		}
	}

	// Compile each file
	totalMethods := 0
	for _, file := range files {
		methods, err := compileFile(file, vmInst, verbose)
		if err != nil {
			return totalMethods, fmt.Errorf("compiling %q: %w", file, err)
		}
		totalMethods += methods
	}

	return totalMethods, nil
}

// compileFile compiles a single .mag file into the VM using Trashtalk syntax.
func compileFile(path string, vmInst *vm.VM, verbose bool) (int, error) {
	content, err := os.ReadFile(path)
	if err != nil {
		return 0, err
	}

	sf, err := compiler.ParseSourceFileFromString(string(content))
	if err != nil {
		return 0, fmt.Errorf("parse error: %v", err)
	}

	compiled := 0

	// Compile classes
	for _, classDef := range sf.Classes {
		// Look up or create the class
		class := vmInst.Classes.Lookup(classDef.Name)
		if class == nil {
			// Find superclass
			var superclass *vm.Class
			if classDef.Superclass != "" {
				superclass = vmInst.Classes.Lookup(classDef.Superclass)
				if superclass == nil {
					superclass = vmInst.ObjectClass
				}
			} else {
				superclass = vmInst.ObjectClass
			}

			class = vm.NewClassWithInstVars(classDef.Name, superclass, classDef.InstanceVariables)
			vmInst.Classes.Register(class)
			// Also add to Globals so it can be referenced as a variable
			vmInst.Globals[classDef.Name] = vmInst.Symbols.SymbolValue(classDef.Name)
			if verbose {
				fmt.Printf("  Created class %s\n", classDef.Name)
			}
		}

		// Compile instance methods (with instance variable context)
		// Need to include inherited instance variables for proper slot indexing
		allIvars := class.AllInstVarNames()
		for _, methodDef := range classDef.Methods {
			method, err := compiler.CompileMethodDefWithIvars(methodDef, vmInst.Selectors, vmInst.Symbols, allIvars)
			if err != nil {
				return compiled, fmt.Errorf("error compiling %s>>%s: %v", classDef.Name, methodDef.Selector, err)
			}

			method.SetClass(class)
			selectorID := vmInst.Selectors.Intern(method.Name())
			class.VTable.AddMethod(selectorID, method)
			compiled++
		}

		// Compile class methods
		for _, methodDef := range classDef.ClassMethods {
			method, err := compiler.CompileMethodDef(methodDef, vmInst.Selectors, vmInst.Symbols)
			if err != nil {
				return compiled, fmt.Errorf("error compiling %s class>>%s: %v", classDef.Name, methodDef.Selector, err)
			}

			method.SetClass(class)
			selectorID := vmInst.Selectors.Intern(method.Name())
			class.ClassVTable.AddMethod(selectorID, method)
			compiled++
		}
	}

	if verbose && compiled > 0 {
		fmt.Printf("  %s: %d methods\n", filepath.Base(path), compiled)
	}

	return compiled, nil
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
	_, err = compilePath(yutaniPath+"/...", vmInst, verbose)
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
	method, err := compiler.Compile(source, vmInst.Selectors, vmInst.Symbols)
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
