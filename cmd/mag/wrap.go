package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/chazu/maggie/gowrap"
	"github.com/chazu/maggie/manifest"
)

// handleWrapCommand processes the `mag wrap` subcommand.
// Usage:
//
//	mag wrap                      # all packages from maggie.toml
//	mag wrap encoding/json        # single package, ad-hoc
//	mag wrap --output ./wrappers  # custom output dir
func handleWrapCommand(args []string, verbose bool) {
	var outputDir string
	var packages []wrapTarget

	// Parse flags
	remaining := make([]string, 0, len(args))
	for i := 0; i < len(args); i++ {
		if args[i] == "--output" || args[i] == "-o" {
			if i+1 < len(args) {
				outputDir = args[i+1]
				i++
			} else {
				fmt.Fprintln(os.Stderr, "Error: --output requires a directory path")
				os.Exit(1)
			}
		} else {
			remaining = append(remaining, args[i])
		}
	}

	if len(remaining) > 0 {
		// Ad-hoc package wrapping from command line
		for _, pkg := range remaining {
			packages = append(packages, wrapTarget{ImportPath: pkg})
		}
	} else {
		// Load from maggie.toml
		m, err := manifest.FindAndLoad(".")
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error loading manifest: %v\n", err)
			os.Exit(1)
		}
		if m == nil {
			fmt.Fprintln(os.Stderr, "Error: no maggie.toml found and no packages specified")
			fmt.Fprintln(os.Stderr, "Usage: mag wrap [packages...] or configure [go-wrap] in maggie.toml")
			os.Exit(1)
		}

		if len(m.GoWrap.Packages) == 0 {
			fmt.Fprintln(os.Stderr, "No [go-wrap.packages] configured in maggie.toml")
			os.Exit(1)
		}

		if outputDir == "" {
			outputDir = m.WrapOutputDir()
		}

		for _, pkg := range m.GoWrap.Packages {
			packages = append(packages, wrapTarget{
				ImportPath: pkg.Import,
				Include:    pkg.Include,
			})
		}
	}

	// Default output dir
	if outputDir == "" {
		outputDir = ".maggie/wrap"
	}

	for _, pkg := range packages {
		if err := wrapPackage(pkg, outputDir, verbose); err != nil {
			fmt.Fprintf(os.Stderr, "Error wrapping %s: %v\n", pkg.ImportPath, err)
			os.Exit(1)
		}
	}

	if verbose {
		fmt.Printf("Wrapped %d package(s) to %s\n", len(packages), outputDir)
	}
}

type wrapTarget struct {
	ImportPath string
	Include    []string
}

func wrapPackage(target wrapTarget, outputDir string, verbose bool) error {
	if verbose {
		fmt.Printf("Wrapping %s...\n", target.ImportPath)
	}

	// Build include filter
	var filter map[string]bool
	if len(target.Include) > 0 {
		filter = make(map[string]bool)
		for _, name := range target.Include {
			filter[name] = true
		}
	}

	// Introspect
	model, err := gowrap.IntrospectPackage(target.ImportPath, filter)
	if err != nil {
		return fmt.Errorf("introspecting: %w", err)
	}

	if verbose {
		fmt.Printf("  Found %d functions, %d types, %d constants\n",
			len(model.Functions), len(model.Types), len(model.Constants))
	}

	// Generate Go glue
	goCode, err := gowrap.GenerateGoGlue(model)
	if err != nil {
		return fmt.Errorf("generating Go glue: %w", err)
	}

	// Generate Maggie stubs
	magCode, err := gowrap.GenerateMaggieStubs(model)
	if err != nil {
		return fmt.Errorf("generating Maggie stubs: %w", err)
	}

	// Write files
	pkgDir := filepath.Join(outputDir, sanitizePkgDir(model.Name))
	if err := os.MkdirAll(pkgDir, 0o755); err != nil {
		return fmt.Errorf("creating output dir: %w", err)
	}

	goPath := filepath.Join(pkgDir, "wrap.go")
	if err := os.WriteFile(goPath, []byte(goCode), 0o644); err != nil {
		return fmt.Errorf("writing %s: %w", goPath, err)
	}

	magPath := filepath.Join(pkgDir, "stubs.mag")
	if err := os.WriteFile(magPath, []byte(magCode), 0o644); err != nil {
		return fmt.Errorf("writing %s: %w", magPath, err)
	}

	if verbose {
		fmt.Printf("  Wrote %s\n", goPath)
		fmt.Printf("  Wrote %s\n", magPath)
	}

	return nil
}

func sanitizePkgDir(name string) string {
	return strings.ReplaceAll(name, "-", "_")
}
