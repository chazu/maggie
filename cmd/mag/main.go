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
	"github.com/chazu/maggie/vm"
)

//go:embed maggie.image
var embeddedImage []byte

func main() {
	verbose := flag.Bool("v", false, "Verbose output")
	interactive := flag.Bool("i", false, "Start interactive REPL")
	mainEntry := flag.String("m", "", "Main entry point (e.g., 'Main.run' or just 'main')")
	noRC := flag.Bool("no-rc", false, "Skip loading ~/.maggierc")
	yutaniMode := flag.Bool("yutani", false, "Start Yutani IDE mode (connects to yutani-server)")
	yutaniAddr := flag.String("yutani-addr", "localhost:7755", "Yutani server address")

	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: mag [options] [paths...]\n\n")
		fmt.Fprintf(os.Stderr, "Starts Maggie with the default image and compiles .mag files from the given paths.\n\n")
		fmt.Fprintf(os.Stderr, "Options:\n")
		flag.PrintDefaults()
		fmt.Fprintf(os.Stderr, "\nExamples:\n")
		fmt.Fprintf(os.Stderr, "  mag -i                 # Start REPL\n")
		fmt.Fprintf(os.Stderr, "  mag ./src -m main      # Load src/, run 'main' method\n")
		fmt.Fprintf(os.Stderr, "  mag ./... -m App.start # Load recursively, run App.start\n")
		fmt.Fprintf(os.Stderr, "  mag --yutani           # Start Yutani IDE\n")
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

	if *verbose {
		fmt.Printf("Loaded default image (%d bytes)\n", len(embeddedImage))
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

	// Yutani IDE mode
	if *yutaniMode {
		if err := startYutaniIDE(vmInst, *yutaniAddr, *verbose); err != nil {
			fmt.Fprintf(os.Stderr, "Yutani IDE error: %v\n", err)
			os.Exit(1)
		}
		os.Exit(0)
	}

	// Run main entry point if specified
	if *mainEntry != "" {
		result, err := runMain(vmInst, *mainEntry)
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
func runMain(vmInst *vm.VM, entry string) (vm.Value, error) {
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

	// Find the class
	class := vmInst.Classes.Lookup(className)
	if class == nil {
		return vm.Nil, fmt.Errorf("class %q not found", className)
	}

	// Check if method exists
	method := class.LookupMethod(vmInst.Selectors, methodName)
	if method == nil {
		return vm.Nil, fmt.Errorf("method %q not found on %s", methodName, className)
	}

	// Execute the method on nil (for class methods) or create an instance
	// For simplicity, we'll send the message to nil which will find it on Object
	result := vmInst.Send(vm.Nil, methodName, nil)
	return result, nil
}

// runREPL starts an interactive read-eval-print loop
func runREPL(vmInst *vm.VM) {
	fmt.Println("Maggie REPL (type 'exit' to quit)")
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

// evalAndPrint compiles and executes an expression, printing the result
func evalAndPrint(vmInst *vm.VM, input string) {
	// Wrap input in a method body if it doesn't look like a method definition
	source := input
	if !looksLikeMethodDef(input) {
		// Wrap as a doIt method
		source = "doIt\n    ^" + strings.TrimSuffix(input, ".")
	}

	// Compile the method
	method, err := compiler.Compile(source, vmInst.Selectors, vmInst.Symbols)
	if err != nil {
		fmt.Printf("Compile error: %v\n", err)
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
			if verbose {
				fmt.Printf("  Created class %s\n", classDef.Name)
			}
		}

		// Compile instance methods (with instance variable context)
		for _, methodDef := range classDef.Methods {
			method, err := compiler.CompileMethodDefWithIvars(methodDef, vmInst.Selectors, vmInst.Symbols, classDef.InstanceVariables)
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

// startYutaniIDE loads the Yutani classes and starts the IDE
func startYutaniIDE(vmInst *vm.VM, addr string, verbose bool) error {
	// Find the lib/yutani directory relative to the executable or working directory
	yutaniLibPath := findYutaniLib()
	if yutaniLibPath == "" {
		return fmt.Errorf("cannot find lib/yutani directory")
	}

	if verbose {
		fmt.Printf("Loading Yutani classes from %s\n", yutaniLibPath)
	}

	// Load Yutani library classes in dependency order
	yutaniFiles := []string{
		"YWidget.mag",
		"YEvent.mag",
		"YClient.mag",
		"YList.mag",
		"YFlex.mag",
		"YTextView.mag",
		"YTextArea.mag",
		"YButton.mag",
		"YInputField.mag",
		"YGrid.mag",
		"YPages.mag",
		"YTable.mag",
		"YTreeView.mag",
		"MaggieIDE.mag",
	}

	for _, file := range yutaniFiles {
		path := filepath.Join(yutaniLibPath, file)
		if _, err := os.Stat(path); os.IsNotExist(err) {
			if file == "MaggieIDE.mag" {
				// MaggieIDE is optional, we can use a default
				continue
			}
			return fmt.Errorf("missing Yutani class file: %s", path)
		}

		_, err := compileFile(path, vmInst, verbose)
		if err != nil {
			return fmt.Errorf("loading %s: %w", file, err)
		}
	}

	// Store the server address in a global
	vmInst.Globals["YutaniServerAddress"] = vm.NewStringValue(addr)

	// Check if MaggieIDE class exists, if not use built-in
	ideClass := vmInst.Classes.Lookup("MaggieIDE")
	if ideClass == nil {
		if verbose {
			fmt.Println("MaggieIDE.mag not found, using built-in IDE")
		}
		if err := loadBuiltinIDE(vmInst, verbose); err != nil {
			return err
		}
	}

	if verbose {
		fmt.Printf("Connecting to Yutani server at %s\n", addr)
	}

	// Run MaggieIDE.start
	result := vmInst.Send(vm.Nil, "startIDE", nil)
	if result == vm.Nil {
		return fmt.Errorf("IDE returned nil (connection failed?)")
	}

	return nil
}

// findYutaniLib looks for the lib/yutani directory
func findYutaniLib() string {
	// Try relative to working directory
	candidates := []string{
		"lib/yutani",
		"../lib/yutani",
		"../../lib/yutani",
	}

	// Also try relative to executable
	if exe, err := os.Executable(); err == nil {
		exeDir := filepath.Dir(exe)
		candidates = append(candidates,
			filepath.Join(exeDir, "lib/yutani"),
			filepath.Join(exeDir, "../lib/yutani"),
			filepath.Join(exeDir, "../../lib/yutani"),
		)
	}

	for _, path := range candidates {
		if abs, err := filepath.Abs(path); err == nil {
			if info, err := os.Stat(abs); err == nil && info.IsDir() {
				return abs
			}
		}
	}

	return ""
}

// loadBuiltinIDE compiles the built-in IDE code
func loadBuiltinIDE(vmInst *vm.VM, verbose bool) error {
	ideSource := `
startIDE
    "Minimal test - just try to connect"
    | client |
    client := YClient connectTo: 'localhost:7755'.
    ^ client
`
	// Compile as Object method
	method, err := compiler.Compile(ideSource, vmInst.Selectors, vmInst.Symbols)
	if err != nil {
		return fmt.Errorf("compiling built-in IDE: %w", err)
	}

	method.SetClass(vmInst.ObjectClass)
	vmInst.ObjectClass.VTable.AddMethod(vmInst.Selectors.Intern("startIDE"), method)

	if verbose {
		fmt.Println("Loaded built-in IDE")
	}

	return nil
}
