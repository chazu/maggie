// Maggie CLI - the main entry point for running Maggie programs
package main

import (
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
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: mag [options] [paths...]\n\n")
		fmt.Fprintf(os.Stderr, "Starts Maggie with the default image and compiles .mag files from the given paths.\n\n")
		fmt.Fprintf(os.Stderr, "Options:\n")
		flag.PrintDefaults()
		fmt.Fprintf(os.Stderr, "\nExamples:\n")
		fmt.Fprintf(os.Stderr, "  mag                    # Start with default image only\n")
		fmt.Fprintf(os.Stderr, "  mag ./src              # Load .mag files from ./src\n")
		fmt.Fprintf(os.Stderr, "  mag ./...              # Load .mag files recursively\n")
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
		if totalMethods > 0 {
			fmt.Printf("Compiled %d methods\n", totalMethods)
		}
	}

	// For now, just report success
	// TODO: Add REPL or script execution
	if *verbose {
		fmt.Println("Maggie ready.")
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
