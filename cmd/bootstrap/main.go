// Bootstrap tool for Maggie VM
// Compiles core class library from .mag source files and saves to a binary image.
package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// classMapping maps class names to VM classes (for core classes pre-created in Go)
var classMapping = map[string]func(*vm.VM) *vm.Class{
	"Object":          func(v *vm.VM) *vm.Class { return v.ObjectClass },
	"Boolean":         func(v *vm.VM) *vm.Class { return v.BooleanClass },
	"True":            func(v *vm.VM) *vm.Class { return v.TrueClass },
	"False":           func(v *vm.VM) *vm.Class { return v.FalseClass },
	"UndefinedObject": func(v *vm.VM) *vm.Class { return v.UndefinedObjectClass },
	"SmallInteger":    func(v *vm.VM) *vm.Class { return v.SmallIntegerClass },
	"Float":           func(v *vm.VM) *vm.Class { return v.FloatClass },
	"String":          func(v *vm.VM) *vm.Class { return v.StringClass },
	"Symbol":          func(v *vm.VM) *vm.Class { return v.SymbolClass },
	"Array":           func(v *vm.VM) *vm.Class { return v.ArrayClass },
	"Block":           func(v *vm.VM) *vm.Class { return v.BlockClass },
	"Channel":         func(v *vm.VM) *vm.Class { return v.ChannelClass },
	"Process":         func(v *vm.VM) *vm.Class { return v.ProcessClass },
	"Result":          func(v *vm.VM) *vm.Class { return v.ResultClass },
	"Success":         func(v *vm.VM) *vm.Class { return v.SuccessClass },
	"Failure":         func(v *vm.VM) *vm.Class { return v.FailureClass },
	"Dictionary":      func(v *vm.VM) *vm.Class { return v.DictionaryClass },
}

func main() {
	libDir := flag.String("lib", "lib", "Directory containing .mag source files")
	output := flag.String("o", "maggie.image", "Output image file")
	verbose := flag.Bool("v", false, "Verbose output")
	newSyntax := flag.Bool("new-syntax", false, "Use new Trashtalk-style syntax")
	flag.Parse()

	// Create a fresh VM with primitives
	vmInst := vm.NewVM()

	// Find all .mag files
	files, err := filepath.Glob(filepath.Join(*libDir, "*.mag"))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error finding source files: %v\n", err)
		os.Exit(1)
	}

	if len(files) == 0 {
		fmt.Fprintf(os.Stderr, "No .mag files found in %s\n", *libDir)
		os.Exit(1)
	}

	if *verbose {
		fmt.Printf("Found %d source files\n", len(files))
	}

	// Compile each file
	totalMethods := 0
	for _, file := range files {
		var methods int
		var err error

		if *newSyntax {
			// New Trashtalk-style syntax
			methods, err = compileFileNew(file, vmInst, *verbose)
		} else {
			// Old syntax: filename determines class
			className := strings.TrimSuffix(filepath.Base(file), ".mag")
			classGetter, ok := classMapping[className]
			if !ok {
				fmt.Fprintf(os.Stderr, "Warning: unknown class %s, skipping\n", className)
				continue
			}

			class := classGetter(vmInst)
			methods, err = compileFileOld(file, class, vmInst)
			if *verbose {
				fmt.Printf("  %s: %d methods\n", className, methods)
			}
		}

		if err != nil {
			fmt.Fprintf(os.Stderr, "Error compiling %s: %v\n", file, err)
			os.Exit(1)
		}

		totalMethods += methods
	}

	fmt.Printf("Compiled %d methods from %d files\n", totalMethods, len(files))

	// Save the image
	if err := vmInst.SaveImage(*output); err != nil {
		fmt.Fprintf(os.Stderr, "Error saving image: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Image saved to %s\n", *output)
}

// compileFileOld reads a .mag file in the old format and compiles all methods into the given class.
// Returns the number of methods compiled.
func compileFileOld(path string, class *vm.Class, vmInst *vm.VM) (int, error) {
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
			return compiled, fmt.Errorf("method compilation error: %v\n  source: %s", err, truncate(methodSource, 50))
		}

		// Set the method's class (important for image serialization)
		method.SetClass(class)

		// Add method to class
		class.VTable.AddMethod(vmInst.Selectors.Intern(method.Name()), method)
		compiled++
	}

	return compiled, nil
}

// compileFileNew reads a .mag file in the new Trashtalk-style format.
// Returns the number of methods compiled.
func compileFileNew(path string, vmInst *vm.VM, verbose bool) (int, error) {
	content, err := os.ReadFile(path)
	if err != nil {
		return 0, err
	}

	sf, err := compiler.ParseSourceFileFromString(string(content))
	if err != nil {
		return 0, fmt.Errorf("parse error: %v", err)
	}

	compiled := 0

	// First pass: Compile trait definitions
	for _, traitDef := range sf.Traits {
		trait := vm.NewTrait(traitDef.Name)

		// Compile trait methods
		for _, methodDef := range traitDef.Methods {
			method, err := compiler.CompileMethodDef(methodDef, vmInst.Selectors, vmInst.Symbols)
			if err != nil {
				return compiled, fmt.Errorf("error compiling trait %s>>%s: %v", traitDef.Name, methodDef.Selector, err)
			}

			selectorID := vmInst.Selectors.Intern(method.Name())
			trait.AddMethod(selectorID, method)
			compiled++
		}

		// Add required method selectors
		for _, reqSelector := range traitDef.Requires {
			selectorID := vmInst.Selectors.Intern(reqSelector)
			trait.AddRequires(selectorID)
		}

		// Register trait in VM
		vmInst.Traits.Register(trait)

		if verbose {
			fmt.Printf("  trait %s: %d methods\n", traitDef.Name, len(traitDef.Methods))
		}
	}

	// Second pass: Process each class definition
	for _, classDef := range sf.Classes {
		// Look up the class in the VM
		classGetter, ok := classMapping[classDef.Name]
		if !ok {
			return compiled, fmt.Errorf("unknown class: %s (not found in classMapping)", classDef.Name)
		}

		class := classGetter(vmInst)

		// Compile instance methods
		for _, methodDef := range classDef.Methods {
			method, err := compiler.CompileMethodDef(methodDef, vmInst.Selectors, vmInst.Symbols)
			if err != nil {
				return compiled, fmt.Errorf("error compiling %s>>%s: %v", classDef.Name, methodDef.Selector, err)
			}

			method.SetClass(class)
			class.VTable.AddMethod(vmInst.Selectors.Intern(method.Name()), method)
			compiled++
		}

		// Compile class methods (add to metaclass)
		for _, methodDef := range classDef.ClassMethods {
			method, err := compiler.CompileMethodDef(methodDef, vmInst.Selectors, vmInst.Symbols)
			if err != nil {
				return compiled, fmt.Errorf("error compiling %s class>>%s: %v", classDef.Name, methodDef.Selector, err)
			}

			// Class methods go on the class object's vtable (metaclass)
			// For now, we'll add them to the class's own vtable
			// TODO: Proper metaclass support
			method.SetClass(class)
			class.VTable.AddMethod(vmInst.Selectors.Intern(method.Name()), method)
			compiled++
		}

		// Apply included traits
		for _, traitName := range classDef.Traits {
			errMsg := class.IncludeTraitByName(traitName, vmInst.Traits, vmInst.Selectors)
			if errMsg != "" {
				return compiled, fmt.Errorf("error including trait %s in %s: %s", traitName, classDef.Name, errMsg)
			}
			if verbose {
				trait := vmInst.Traits.Lookup(traitName)
				if trait != nil {
					fmt.Printf("    included trait %s (%d methods)\n", traitName, trait.MethodCount())
				}
			}
		}

		if verbose {
			fmt.Printf("  %s: %d methods\n", classDef.Name, len(classDef.Methods)+len(classDef.ClassMethods))
		}
	}

	return compiled, nil
}

// parseMethodChunks splits a .mag file into individual method sources (old format).
// Methods are detected by looking for unindented method headers.
// Lines starting with "--" are comments and are stripped.
func parseMethodChunks(content string) []string {
	var methods []string
	var current strings.Builder

	lines := strings.Split(content, "\n")
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)

		// Skip blank lines and comments between methods
		if trimmed == "" || strings.HasPrefix(trimmed, "--") {
			// If we're in a method, preserve blank lines (but not leading ones)
			if current.Len() > 0 && trimmed == "" {
				current.WriteString("\n")
			}
			continue
		}

		// Check if this line starts a new method (unindented, non-blank, non-comment)
		// A method header has no leading whitespace
		isMethodHeader := len(line) > 0 && line[0] != ' ' && line[0] != '\t'

		if isMethodHeader && current.Len() > 0 {
			// Save previous method and start a new one
			methods = append(methods, strings.TrimRight(current.String(), "\n"))
			current.Reset()
		}

		// Add line to current method
		if current.Len() > 0 {
			current.WriteString("\n")
		}
		current.WriteString(line)
	}

	// Don't forget the last method
	if current.Len() > 0 {
		methods = append(methods, strings.TrimRight(current.String(), "\n"))
	}

	return methods
}

// truncate returns the first n characters of s, with "..." if truncated.
func truncate(s string, n int) string {
	if len(s) <= n {
		return s
	}
	return s[:n] + "..."
}
