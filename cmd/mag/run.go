package main

import (
	"fmt"
	"os"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/manifest"
	"github.com/chazu/maggie/pipeline"
	"github.com/chazu/maggie/vm"
)

// handleRunCommand processes the `mag run` subcommand.
func handleRunCommand(args []string, verbose bool) {
	if wantsHelp(args) {
		subcmdUsage("run [options]",
			"Run a Maggie project's entry point.",
			usageFlags([][2]string{
				{"-t, --target <name>", "Run a specific build target"},
			}),
			usageExamples([][2]string{
				{"mag run", "Run the default entry point"},
				{"mag run -t server", "Run the 'server' target"},
			}),
			"\nRuns the entry point defined in maggie.toml or the named target.\n",
		)
	}

	var targetName string

	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "-t", "--target":
			if i+1 < len(args) {
				targetName = args[i+1]
				i++
			} else {
				fmt.Fprintln(os.Stderr, "Error: -t requires a target name")
				os.Exit(1)
			}
		}
	}

	// Load manifest
	m, err := manifest.FindAndLoad(".")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error loading manifest: %v\n", err)
		os.Exit(1)
	}
	if m == nil {
		fmt.Fprintln(os.Stderr, "Error: no maggie.toml found")
		os.Exit(1)
	}

	// Resolve target
	var target *manifest.ResolvedTarget
	if targetName != "" {
		target, err = m.ResolveTarget(targetName)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
	} else {
		target = m.ResolveDefaultTarget()
	}

	if target.Entry == "" {
		fmt.Fprintln(os.Stderr, "Error: no entry point specified")
		os.Exit(1)
	}

	// Create VM and load image
	vmInst := vm.NewVM()
	defer vmInst.Shutdown()

	if err := vmInst.LoadImageFromBytes(embeddedImage); err != nil {
		fmt.Fprintf(os.Stderr, "Error loading embedded image: %v\n", err)
		os.Exit(1)
	}
	vmInst.ReRegisterNilPrimitives()
	vmInst.ReRegisterBooleanPrimitives()
	vmInst.UseGoCompiler(compiler.Compile)

	// Compile using target configuration
	pipe := &pipeline.Pipeline{
		VM:      vmInst,
		Exclude: target.Exclude,
	}
	if verbose {
		pipe.Verbose = os.Stdout
	}

	if _, err := pipe.LoadTarget(m, target); err != nil {
		fmt.Fprintf(os.Stderr, "Error compiling project: %v\n", err)
		os.Exit(1)
	}

	// Run entry point
	result, err := runMain(vmInst, target.Entry, verbose)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
	if result.IsSmallInt() {
		os.Exit(int(result.SmallInt()))
	}
}
