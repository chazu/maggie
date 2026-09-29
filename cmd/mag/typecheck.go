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
	if wantsHelp(args) {
		subcmdUsage("typecheck [--verbose] [files or dirs...]",
			"Type-check .mag files (Strongtalk-style optional annotations).",
			"Exits non-zero on parse errors or type warnings.")
		return
	}
	fs := flag.NewFlagSet("typecheck", flag.ExitOnError)
	verbose := fs.Bool("verbose", false, "Show all checks, not just warnings")
	fs.Parse(args)

	paths := fs.Args()
	if len(paths) == 0 {
		paths = []string{"."}
	}

	checker := types.NewChecker(vmInst)
	checker.Verbose = *verbose

	var files []string
	for _, path := range paths {
		found, err := collectMagFiles([]string{path})
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			continue
		}
		files = append(files, found...)
	}
	if *verbose {
		for _, file := range files {
			fmt.Printf("Checking %s\n", file)
		}
	}
	totalFiles := len(files)
	parseErrors := typecheckFiles(checker, files)

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

// typecheckFiles parses every file, declares all of their classes and traits
// with the checker, and only then checks each one — so a class defined in
// one file of the set is a known type in all the others, regardless of
// order. Returns the total number of parse errors; files that fail to parse
// are reported and skipped rather than "checked" as a broken partial AST.
func typecheckFiles(checker *types.Checker, files []string) int {
	parseErrors := 0
	var parsed []*compiler.SourceFile
	for _, file := range files {
		sf, errs := parseTypecheckFile(file)
		parseErrors += errs
		if sf != nil {
			checker.DeclareTypes(sf)
			parsed = append(parsed, sf)
		}
	}
	for _, sf := range parsed {
		checker.CheckSourceFile(sf)
	}
	return parseErrors
}

// parseTypecheckFile parses one file, returning the source file (nil on
// failure) and the number of errors reported.
func parseTypecheckFile(path string) (*compiler.SourceFile, int) {
	content, err := os.ReadFile(path)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading %s: %v\n", path, err)
		return nil, 1
	}

	p := compiler.NewParser(string(content))
	sf := p.ParseSourceFile()
	if errs := p.Errors(); len(errs) > 0 {
		for _, e := range errs {
			fmt.Fprintf(os.Stderr, "%s: parse error: %s\n", path, e)
		}
		return nil, len(errs)
	}
	if sf == nil {
		return nil, 1
	}
	return sf, 0
}
