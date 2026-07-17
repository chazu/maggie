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
	checker.Verbose = *verbose
	totalFiles := 0
	parseErrors := 0

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
			parseErrors += typecheckFile(checker, file)
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

	// Exit non-zero on parse errors or type warnings so `mag typecheck` can gate
	// CI, instead of always succeeding.
	if parseErrors > 0 || len(checker.Diagnostics) > 0 {
		os.Exit(1)
	}
}

// typecheckFile checks one file and returns the number of parse errors (0 on
// success). Parse errors are reported and counted so the command can exit
// non-zero instead of silently "checking" a broken partial AST.
func typecheckFile(checker *types.Checker, path string) int {
	content, err := os.ReadFile(path)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading %s: %v\n", path, err)
		return 1
	}

	p := compiler.NewParser(string(content))
	sf := p.ParseSourceFile()
	if errs := p.Errors(); len(errs) > 0 {
		for _, e := range errs {
			fmt.Fprintf(os.Stderr, "%s: parse error: %s\n", path, e)
		}
		return len(errs)
	}
	if sf == nil {
		return 1
	}

	checker.CheckSourceFile(sf)
	return 0
}
