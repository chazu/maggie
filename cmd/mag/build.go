package main

import (
	"fmt"
	"os"

	"github.com/chazu/maggie/gowrap"
	"github.com/chazu/maggie/manifest"
)

// handleBuildCommand processes the `mag build` subcommand.
// Usage:
//
//	mag build              # ./mag-custom
//	mag build -o myapp     # custom output
func handleBuildCommand(args []string, verbose bool) {
	var outputBinary string

	// Parse flags
	for i := 0; i < len(args); i++ {
		if args[i] == "-o" || args[i] == "--output" {
			if i+1 < len(args) {
				outputBinary = args[i+1]
				i++
			} else {
				fmt.Fprintln(os.Stderr, "Error: -o requires an output path")
				os.Exit(1)
			}
		}
	}

	if outputBinary == "" {
		outputBinary = "mag-custom"
	}

	// Load manifest
	m, err := manifest.FindAndLoad(".")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error loading manifest: %v\n", err)
		os.Exit(1)
	}
	if m == nil {
		fmt.Fprintln(os.Stderr, "Error: no maggie.toml found")
		fmt.Fprintln(os.Stderr, "mag build requires a maggie.toml with [go-wrap] configuration")
		os.Exit(1)
	}

	if len(m.GoWrap.Packages) == 0 {
		fmt.Fprintln(os.Stderr, "No [go-wrap.packages] configured in maggie.toml")
		os.Exit(1)
	}

	// Ensure wrappers are generated
	wrapDir := m.WrapOutputDir()
	for _, pkg := range m.GoWrap.Packages {
		target := wrapTarget{
			ImportPath: pkg.Import,
			Include:    pkg.Include,
		}
		if err := wrapPackage(target, wrapDir, verbose); err != nil {
			fmt.Fprintf(os.Stderr, "Error wrapping %s: %v\n", pkg.Import, err)
			os.Exit(1)
		}
	}

	// Collect wrapper package paths
	var wrapperPkgs []gowrap.WrapperPackageInfo
	for _, pkg := range m.GoWrap.Packages {
		model, err := gowrap.IntrospectPackage(pkg.Import, nil)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error introspecting %s: %v\n", pkg.Import, err)
			os.Exit(1)
		}
		wrapperPkgs = append(wrapperPkgs, gowrap.WrapperPackageInfo{
			ImportPath: pkg.Import,
			PkgName:    model.Name,
		})
	}

	// Run build
	opts := gowrap.BuildOptions{
		OutputBinary:  outputBinary,
		WrapDir:       wrapDir,
		WrapperPkgs:   wrapperPkgs,
		ProjectDir:    m.Dir,
		Verbose:       verbose,
	}

	if err := gowrap.Build(opts); err != nil {
		fmt.Fprintf(os.Stderr, "Error building: %v\n", err)
		os.Exit(1)
	}

	if verbose {
		fmt.Printf("Built %s\n", outputBinary)
	}
}
