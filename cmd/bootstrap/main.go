// Bootstrap tool for Maggie VM
// Compiles core class library from .mag source files and saves to a binary image.
package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/compiler/hash"
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
	"GrpcClient":      func(v *vm.VM) *vm.Class { return v.GrpcClientClass },
	"GrpcStream":      func(v *vm.VM) *vm.Class { return v.GrpcStreamClass },
}

func main() {
	libDir := flag.String("lib", "lib", "Directory containing .mag source files")
	output := flag.String("o", "maggie.image", "Output image file")
	verbose := flag.Bool("v", false, "Verbose output")
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

	// Combine core and compiler files (core first, then compiler)
	allFiles := append(files, compilerFiles...)

	methods, err := compileAllFiles(allFiles, vmInst, *verbose)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Compiled %d methods from %d files\n", methods, len(files))

	// Save the image
	if err := vmInst.SaveImage(*output); err != nil {
		fmt.Fprintf(os.Stderr, "Error saving image: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Image saved to %s\n", *output)
}

// compileAllFiles compiles all files using Trashtalk syntax.
// Uses a two-pass approach: first compile all traits, then compile all classes.
func compileAllFiles(files []string, vmInst *vm.VM, verbose bool) (int, error) {
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

			// Preserve docstring on trait
			if traitDef.DocString != "" {
				trait.DocString = traitDef.DocString
			}

			// Compile trait methods
			for _, methodDef := range traitDef.Methods {
				method, err := compiler.CompileMethodDef(methodDef, vmInst.Selectors, vmInst.Symbols, vmInst.Registry())
				if err != nil {
					return compiled, fmt.Errorf("error compiling trait %s>>%s in %s: %v", traitDef.Name, methodDef.Selector, pf.path, err)
				}

				// Preserve docstring on compiled method
				if methodDef.DocString != "" {
					method.SetDocString(methodDef.DocString)
				}

				// Compute content hash
				h := hash.HashMethod(methodDef, nil, nil)
				method.SetContentHash(h)

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
						if vm.IsClassValue(superVal) {
							superclass = vmInst.GetClassFromValue(superVal)
						} else if superVal.IsObject() {
							superclass = (*vm.Class)(superVal.ObjectPtr())
						}
					} else if c := vmInst.Classes.Lookup(classDef.Superclass); c != nil {
						superclass = c
					}
				} else {
					superclass = vmInst.ObjectClass
				}

				// Create the class with instance variables
				class = vm.NewClassWithInstVars(classDef.Name, superclass, classDef.InstanceVariables)

				// Register the class in the class table
				vmInst.Classes.Register(class)

				// Register the class in globals as a first-class class value
				vmInst.Globals[classDef.Name] = vmInst.ClassValue(class)

				if verbose {
					fmt.Printf("  created class %s (superclass: %s, instVars: %v)\n",
						classDef.Name,
						classDef.Superclass,
						classDef.InstanceVariables)
				}
			}

			// Preserve docstring on class
			if classDef.DocString != "" {
				class.DocString = classDef.DocString
			}

			// Compile instance methods (with instance variable context)
			allIvars := class.AllInstVarNames()
			for _, methodDef := range classDef.Methods {
				method, err := compiler.CompileMethodDefWithIvars(methodDef, vmInst.Selectors, vmInst.Symbols, vmInst.Registry(), classDef.InstanceVariables)
				if err != nil {
					return compiled, fmt.Errorf("error compiling %s>>%s in %s: %v", classDef.Name, methodDef.Selector, pf.path, err)
				}

				// Preserve docstring on compiled method
				if methodDef.DocString != "" {
					method.SetDocString(methodDef.DocString)
				}

				// Compute content hash with instance variable context
				instVarMap := make(map[string]int, len(allIvars))
				for idx, name := range allIvars {
					instVarMap[name] = idx
				}
				h := hash.HashMethod(methodDef, instVarMap, nil)
				method.SetContentHash(h)

				method.SetClass(class)
				class.VTable.AddMethod(vmInst.Selectors.Intern(method.Name()), method)
				compiled++
			}

			// Compile class methods (add to ClassVTable for class-side dispatch)
			for _, methodDef := range classDef.ClassMethods {
				method, err := compiler.CompileMethodDef(methodDef, vmInst.Selectors, vmInst.Symbols, vmInst.Registry())
				if err != nil {
					return compiled, fmt.Errorf("error compiling %s class>>%s in %s: %v", classDef.Name, methodDef.Selector, pf.path, err)
				}

				// Preserve docstring on compiled method
				if methodDef.DocString != "" {
					method.SetDocString(methodDef.DocString)
				}

				// Compute content hash
				ch := hash.HashMethod(methodDef, nil, nil)
				method.SetContentHash(ch)

				method.SetClass(class)
				method.IsClassMethod = true
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
