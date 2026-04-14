package main

import (
	"fmt"
	"os"
	"time"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/manifest"
	"github.com/chazu/maggie/pipeline"
	"github.com/chazu/maggie/vm"
)

// handleTestCommand processes the `mag test` subcommand.
func handleTestCommand(args []string, verbose bool) {
	if wantsHelp(args) {
		subcmdUsage("test [options]",
			"Run tests for a Maggie project.",
			usageFlags([][2]string{
				{"--timeout <ms>", "Override test timeout (milliseconds)"},
				{"--entry <entry>", "Override test entry point"},
			}),
			usageExamples([][2]string{
				{"mag test", "Run tests using [test] config from maggie.toml"},
				{"mag test --timeout 5000", "Run with 5-second timeout"},
				{"mag test --entry MyTestRunner.run", "Run with custom entry point"},
			}),
			"\nRequires a maggie.toml with a [test] section.\n"+
				"Test sources are compiled in addition to project sources.\n"+
				"Dev-dependencies are included automatically.\n",
		)
	}

	var flagTimeout int
	var flagEntry string

	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "--timeout":
			if i+1 < len(args) {
				fmt.Sscanf(args[i+1], "%d", &flagTimeout)
				i++
			}
		case "--entry":
			if i+1 < len(args) {
				flagEntry = args[i+1]
				i++
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

	// Determine test entry point
	entry := flagEntry
	if entry == "" {
		entry = m.Test.Entry
	}
	if entry == "" {
		fmt.Fprintln(os.Stderr, "Error: no test entry point specified")
		fmt.Fprintln(os.Stderr, "Set [test].entry in maggie.toml or use --entry")
		os.Exit(1)
	}

	// Run prebuild script
	if err := manifest.RunScript("pretest", m.Scripts.Pretest, m.Dir, verbose); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	// Create VM and load image
	vmInst := vm.NewVM(vmConfigFromManifest(m))
	defer vmInst.Shutdown()

	if err := vmInst.LoadImageFromBytes(embeddedImage); err != nil {
		fmt.Fprintf(os.Stderr, "Error loading embedded image: %v\n", err)
		os.Exit(1)
	}
	vmInst.ReRegisterNilPrimitives()
	vmInst.ReRegisterBooleanPrimitives()
	vmInst.UseGoCompiler(compiler.Compile)

	// Compile project sources with dev-deps included
	pipe := &pipeline.Pipeline{
		VM:      vmInst,
		Exclude: m.Source.Exclude,
		DevMode: true,
	}
	if verbose {
		pipe.Verbose = os.Stdout
	}

	if _, err := pipe.LoadProject(m); err != nil {
		fmt.Fprintf(os.Stderr, "Error compiling project: %v\n", err)
		os.Exit(1)
	}

	// Compile test source directories
	testDirs := m.TestDirPaths()
	if len(testDirs) == 0 {
		// Default to "test" directory if it exists
		testDirs = m.TestDirPaths()
	}
	for _, testDir := range testDirs {
		if _, err := os.Stat(testDir); err != nil {
			if verbose {
				fmt.Printf("Skipping missing test dir: %s\n", testDir)
			}
			continue
		}
		if _, err := pipe.CompilePath(testDir + "/..."); err != nil {
			fmt.Fprintf(os.Stderr, "Error compiling tests from %s: %v\n", testDir, err)
			os.Exit(1)
		}
	}

	// Determine timeout
	timeout := m.Test.Timeout
	if flagTimeout > 0 {
		timeout = flagTimeout
	}

	// Run test entry point
	if timeout > 0 {
		done := make(chan int, 1)
		go func() {
			result, err := runMain(vmInst, entry, verbose)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error: %v\n", err)
				done <- 1
				return
			}
			if result.IsSmallInt() {
				done <- int(result.SmallInt())
				return
			}
			done <- 0
		}()

		select {
		case code := <-done:
			// Run posttest
			_ = manifest.RunScript("posttest", m.Scripts.Posttest, m.Dir, verbose)
			os.Exit(code)
		case <-time.After(time.Duration(timeout) * time.Millisecond):
			fmt.Fprintf(os.Stderr, "Test timeout: %dms exceeded\n", timeout)
			os.Exit(2)
		}
	} else {
		result, err := runMain(vmInst, entry, verbose)
		// Run posttest
		_ = manifest.RunScript("posttest", m.Scripts.Posttest, m.Dir, verbose)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
		if result.IsSmallInt() {
			os.Exit(int(result.SmallInt()))
		}
	}
}
