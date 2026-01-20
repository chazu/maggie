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

	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: mag [options] [paths...]\n\n")
		fmt.Fprintf(os.Stderr, "Starts Maggie with the default image and compiles .mag files from the given paths.\n\n")
		fmt.Fprintf(os.Stderr, "Options:\n")
		flag.PrintDefaults()
		fmt.Fprintf(os.Stderr, "\nExamples:\n")
		fmt.Fprintf(os.Stderr, "  mag -i                 # Start REPL\n")
		fmt.Fprintf(os.Stderr, "  mag ./src -m main      # Load src/, run 'main' method\n")
		fmt.Fprintf(os.Stderr, "  mag ./... -m App.start # Load recursively, run App.start\n")
	}
	flag.Parse()

	// Create VM and load embedded image
	vmInst := vm.NewVM()
	if err := vmInst.LoadImageFromBytes(embeddedImage); err != nil {
		fmt.Fprintf(os.Stderr, "Error loading embedded image: %v\n", err)
		os.Exit(1)
	}

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

// compileFile compiles a single .mag file into the VM.
// The class is determined by the filename (e.g., Foo.mag -> Foo class).
func compileFile(path string, vmInst *vm.VM, verbose bool) (int, error) {
	// Determine class name from filename
	base := filepath.Base(path)
	className := strings.TrimSuffix(base, ".mag")

	// Special case: .maggierc compiles into Object
	if className == ".maggierc" {
		className = "Object"
	}

	// Look up or create the class
	class := vmInst.Classes.Lookup(className)
	if class == nil {
		// Create new class inheriting from Object
		class = vm.NewClass(className, vmInst.ObjectClass)
		vmInst.Classes.Register(class)
		if verbose {
			fmt.Printf("  Created class %s\n", className)
		}
	}

	// Read and compile the file
	content, err := os.ReadFile(path)
	if err != nil {
		return 0, err
	}

	methods := parseMethodChunks(string(content))
	compiled := 0

	for _, methodSource := range methods {
		methodSource = strings.TrimSpace(methodSource)
		if methodSource == "" {
			continue
		}

		method, err := compiler.Compile(methodSource, vmInst.Selectors, vmInst.Symbols)
		if err != nil {
			return compiled, fmt.Errorf("method error: %v\n  source: %s", err, truncate(methodSource, 50))
		}

		method.SetClass(class)
		class.VTable.AddMethod(vmInst.Selectors.Intern(method.Name()), method)
		compiled++
	}

	if verbose {
		fmt.Printf("  %s: %d methods\n", className, compiled)
	}

	return compiled, nil
}

// parseMethodChunks splits a .mag file into individual method sources.
func parseMethodChunks(content string) []string {
	var methods []string
	var current strings.Builder

	lines := strings.Split(content, "\n")
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)

		if trimmed == "" || strings.HasPrefix(trimmed, "--") {
			if current.Len() > 0 && trimmed == "" {
				current.WriteString("\n")
			}
			continue
		}

		isMethodHeader := len(line) > 0 && line[0] != ' ' && line[0] != '\t'

		if isMethodHeader && current.Len() > 0 {
			methods = append(methods, strings.TrimRight(current.String(), "\n"))
			current.Reset()
		}

		if current.Len() > 0 {
			current.WriteString("\n")
		}
		current.WriteString(line)
	}

	if current.Len() > 0 {
		methods = append(methods, strings.TrimRight(current.String(), "\n"))
	}

	return methods
}

func truncate(s string, n int) string {
	if len(s) <= n {
		return s
	}
	return s[:n] + "..."
}
