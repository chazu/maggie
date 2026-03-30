package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/types"
	"github.com/chazu/maggie/vm"
)

// handleTypecheckCommand handles the "mag typecheck" subcommand.
func handleTypecheckCommand(args []string, vmInst *vm.VM) {
	fs := flag.NewFlagSet("typecheck", flag.ExitOnError)
	verbose := fs.Bool("verbose", false, "Show all checks, not just warnings")
	fs.Parse(args)

	paths := fs.Args()
	if len(paths) == 0 {
		paths = []string{"."}
	}

	checker := types.NewChecker(vmInst)
	totalFiles := 0

	for _, path := range paths {
		files, err := collectMagFiles([]string{path})
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			continue
		}
		for _, file := range files {
			if *verbose {
				fmt.Printf("Checking %s\n", file)
			}
			typecheckFile(checker, file)
			totalFiles++
		}
	}

	// Report diagnostics
	if len(checker.Diagnostics) > 0 {
		for _, d := range checker.Diagnostics {
			fmt.Fprintf(os.Stderr, "warning: %s\n", d)
		}
		fmt.Fprintf(os.Stderr, "\n%d warning(s) in %d file(s)\n", len(checker.Diagnostics), totalFiles)
	} else if *verbose {
		fmt.Printf("No type warnings in %d file(s)\n", totalFiles)
	}
}

func typecheckFile(checker *types.Checker, path string) {
	content, err := os.ReadFile(path)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading %s: %v\n", path, err)
		return
	}

	p := compiler.NewParser(string(content))
	sf := p.ParseSourceFile()
	if sf == nil {
		return
	}

	checker.CheckSourceFile(sf)
}
