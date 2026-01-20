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

	// Set up the Go compiler backend
	vmInst.UseGoCompiler(compiler.Compile)

	// Find all .mag files in lib/
	files, err := filepath.Glob(filepath.Join(*libDir, "*.mag"))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error finding source files: %v\n", err)
		os.Exit(1)
	}

	// Also find compiler files in lib/compiler/
	compilerDir := filepath.Join(*libDir, "compiler")
	compilerFiles, err := filepath.Glob(filepath.Join(compilerDir, "*.mag"))
	if err != nil && !os.IsNotExist(err) {
		fmt.Fprintf(os.Stderr, "Error finding compiler files: %v\n", err)
		os.Exit(1)
	}

	if len(files) == 0 {
		fmt.Fprintf(os.Stderr, "No .mag files found in %s\n", *libDir)
		os.Exit(1)
	}

	if *verbose {
		fmt.Printf("Found %d core library files\n", len(files))
		if len(compilerFiles) > 0 {
			fmt.Printf("Found %d compiler files\n", len(compilerFiles))
		}
	}

	// Compile files
	totalMethods := 0

	if *newSyntax {
		// New Trashtalk-style syntax: two-pass compilation
		// Pass 1: Parse all files and compile traits
		// Pass 2: Compile classes (traits are now available)

		// Combine core and compiler files (core first, then compiler)
		allFiles := append(files, compilerFiles...)

		methods, err := compileAllFilesNew(allFiles, vmInst, *verbose)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
		totalMethods = methods
	} else {
		// Old syntax: compile each file individually
		for _, file := range files {
			className := strings.TrimSuffix(filepath.Base(file), ".mag")
			classGetter, ok := classMapping[className]
			if !ok {
				fmt.Fprintf(os.Stderr, "Warning: unknown class %s, skipping\n", className)
				continue
			}

			class := classGetter(vmInst)
			methods, err := compileFileOld(file, class, vmInst)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error compiling %s: %v\n", file, err)
				os.Exit(1)
			}
			if *verbose {
				fmt.Printf("  %s: %d methods\n", className, methods)
			}
			totalMethods += methods
		}
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

// compileAllFilesNew compiles all files using the new Trashtalk-style syntax.
// Uses a two-pass approach: first compile all traits, then compile all classes.
// This ensures traits are available when classes include them.
func compileAllFilesNew(files []string, vmInst *vm.VM, verbose bool) (int, error) {
	// Parse all files first
	type parsedFile struct {
		path string
		sf   *compiler.SourceFile
	}
	var parsed []parsedFile

	for _, file := range files {
		content, err := os.ReadFile(file)
		if err != nil {
			return 0, fmt.Errorf("error reading %s: %v", file, err)
		}

		sf, err := compiler.ParseSourceFileFromString(string(content))
		if err != nil {
			return 0, fmt.Errorf("parse error in %s: %v", file, err)
		}

		parsed = append(parsed, parsedFile{path: file, sf: sf})
	}

	compiled := 0

	// Pass 1: Compile all traits from all files
	for _, pf := range parsed {
		for _, traitDef := range pf.sf.Traits {
			trait := vm.NewTrait(traitDef.Name)

			// Compile trait methods
			for _, methodDef := range traitDef.Methods {
				method, err := compiler.CompileMethodDef(methodDef, vmInst.Selectors, vmInst.Symbols)
				if err != nil {
					return compiled, fmt.Errorf("error compiling trait %s>>%s in %s: %v", traitDef.Name, methodDef.Selector, pf.path, err)
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
	}

	// Pass 2: Compile all classes from all files
	for _, pf := range parsed {
		for _, classDef := range pf.sf.Classes {
			var class *vm.Class

			// Look up the class in the VM (for core classes)
			classGetter, ok := classMapping[classDef.Name]
			if ok {
				class = classGetter(vmInst)
			} else {
				// Dynamically create the class (for compiler and user classes)
				var superclass *vm.Class
				if classDef.Superclass != "" && classDef.Superclass != "Object" {
					// Look up superclass - try classMapping first, then globals
					if superGetter, ok := classMapping[classDef.Superclass]; ok {
						superclass = superGetter(vmInst)
					} else if superVal, ok := vmInst.Globals[classDef.Superclass]; ok {
						if superVal.IsObject() {
							superclass = (*vm.Class)(superVal.ObjectPtr())
						}
					}
				} else {
					superclass = vmInst.ObjectClass
				}

				// Debug: check superclass setup
				if classDef.Name == "Parser" || classDef.Name == "Compiler" {
					fmt.Printf("DEBUG bootstrap: creating class %s, superclass=%v\n", classDef.Name, superclass)
					if superclass != nil {
						fmt.Printf("DEBUG bootstrap: superclass.VTable=%v\n", superclass.VTable)
					}
				}

				// Create the class with instance variables
				class = vm.NewClassWithInstVars(classDef.Name, superclass, classDef.InstanceVariables)

				// Debug: verify VTable parent
				if classDef.Name == "Parser" || classDef.Name == "Compiler" {
					fmt.Printf("DEBUG bootstrap: class.VTable.Parent()=%v\n", class.VTable.Parent())
				}

				// Register the class in the class table (required for image serialization and method lookup)
				vmInst.Classes.Register(class)

				// Register the class in globals as a symbol (same as core classes)
				// This allows vm.Send to find the class when the symbol is used as receiver
				vmInst.Globals[classDef.Name] = vmInst.Symbols.SymbolValue(classDef.Name)

				if verbose {
					fmt.Printf("  created class %s (superclass: %s, instVars: %v)\n",
						classDef.Name,
						classDef.Superclass,
						classDef.InstanceVariables)
				}
			}

			// Compile instance methods (with instance variable context)
			for _, methodDef := range classDef.Methods {
				method, err := compiler.CompileMethodDefWithIvars(methodDef, vmInst.Selectors, vmInst.Symbols, classDef.InstanceVariables)
				if err != nil {
					return compiled, fmt.Errorf("error compiling %s>>%s in %s: %v", classDef.Name, methodDef.Selector, pf.path, err)
				}

				method.SetClass(class)
				class.VTable.AddMethod(vmInst.Selectors.Intern(method.Name()), method)
				compiled++
			}

			// Compile class methods (add to ClassVTable for class-side dispatch)
			for _, methodDef := range classDef.ClassMethods {
				method, err := compiler.CompileMethodDef(methodDef, vmInst.Selectors, vmInst.Symbols)
				if err != nil {
					return compiled, fmt.Errorf("error compiling %s class>>%s in %s: %v", classDef.Name, methodDef.Selector, pf.path, err)
				}

				// Class methods go on the ClassVTable (metaclass VTable)
				method.SetClass(class)
				class.ClassVTable.AddMethod(vmInst.Selectors.Intern(method.Name()), method)
				compiled++
			}

			// Apply included traits
			for _, traitName := range classDef.Traits {
				errMsg := class.IncludeTraitByName(traitName, vmInst.Traits, vmInst.Selectors)
				if errMsg != "" {
					return compiled, fmt.Errorf("error including trait %s in %s (%s): %s", traitName, classDef.Name, pf.path, errMsg)
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

		// Compile instance methods (with instance variable context)
		for _, methodDef := range classDef.Methods {
			method, err := compiler.CompileMethodDefWithIvars(methodDef, vmInst.Selectors, vmInst.Symbols, classDef.InstanceVariables)
			if err != nil {
				return compiled, fmt.Errorf("error compiling %s>>%s: %v", classDef.Name, methodDef.Selector, err)
			}

			method.SetClass(class)
			class.VTable.AddMethod(vmInst.Selectors.Intern(method.Name()), method)
			compiled++
		}

		// Compile class methods (add to metaclass) - no instance variable context
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
