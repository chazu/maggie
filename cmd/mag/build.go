package main

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/gowrap"
	"github.com/chazu/maggie/manifest"
	"github.com/chazu/maggie/pipeline"
	"github.com/chazu/maggie/vm"
)

// handleBuildCommand processes the `mag build` subcommand.
// Usage:
//
//	mag build              # ./mag-custom (entry-point-only binary)
//	mag build --full       # ./mag-custom (full mag CLI with project baked in)
//	mag build -o myapp     # custom output
//
// The project must have [source] dirs. Sources are compiled into an image that
// is embedded in the output binary.
//
// Without --full, the binary loads the image on startup and runs the entry point.
// With --full, the binary is a complete mag CLI (REPL, fmt, doctest, help, etc.)
// with the project's code baked into the embedded image. When invoked with no
// arguments, it runs the project's entry point; all mag subcommands still work.
func handleBuildCommand(args []string, verbose bool) {
	if wantsHelp(args) {
		subcmdUsage("build [--full] [-o output]",
			"Compile a Maggie project into a standalone binary.",
			usageFlags([][2]string{
				{"--full", "Build a full mag system (REPL, fmt, doctest, etc.) with project baked in"},
				{"-o, --output <path>", "Output binary path (default: mag-custom)"},
			}),
			usageExamples([][2]string{
				{"mag build", "Build entry-point-only binary"},
				{"mag build --full", "Build full mag system with project code"},
				{"mag build --full -o myapp", "Full system with custom output name"},
			}),
			"\nWithout --full, the binary only runs the entry point.\n"+
				"With --full, the binary is a complete mag CLI with your project's classes\n"+
				"pre-loaded. Running it with no arguments executes your entry point;\n"+
				"all mag subcommands (fmt, doctest, help, -i, etc.) still work.\n",
		)
	}

	var outputBinary string
	fullSystem := false

	// Parse flags
	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "-o", "--output":
			if i+1 < len(args) {
				outputBinary = args[i+1]
				i++
			} else {
				fmt.Fprintln(os.Stderr, "Error: -o requires an output path")
				os.Exit(1)
			}
		case "--full":
			fullSystem = true
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
		fmt.Fprintf(os.Stderr, "%s build requires a maggie.toml\n", progName())
		os.Exit(1)
	}

	if len(m.Source.Dirs) == 0 {
		fmt.Fprintln(os.Stderr, "Error: maggie.toml has no [source] dirs")
		fmt.Fprintf(os.Stderr, "%s build requires source files to compile into an embedded image\n", progName())
		os.Exit(1)
	}

	hasGoWrap := len(m.GoWrap.Packages) > 0

	// Compile sources into an image
	imagePath, err := compileProjectImage(m, verbose)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error compiling image: %v\n", err)
		os.Exit(1)
	}
	defer os.Remove(imagePath)

	// Wrap Go packages if configured
	var wrapperPkgs []gowrap.WrapperPackageInfo
	var wrapDir string

	if hasGoWrap {
		wrapDir = m.WrapOutputDir()
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
	}

	maggieDir := detectMaggieDir()

	if fullSystem {
		opts := gowrap.FullSystemBuildOptions{
			OutputBinary: outputBinary,
			ImagePath:    imagePath,
			EntryPoint:   m.Source.Entry,
			Namespace:    m.Project.Namespace,
			WrapDir:      wrapDir,
			WrapperPkgs:  wrapperPkgs,
			ProjectDir:   m.Dir,
			MaggieDir:    maggieDir,
			Verbose:      verbose,
		}
		if err := gowrap.BuildFullSystem(opts); err != nil {
			fmt.Fprintf(os.Stderr, "Error building: %v\n", err)
			os.Exit(1)
		}
	} else {
		opts := gowrap.EmbeddedBuildOptions{
			OutputBinary: outputBinary,
			ImagePath:    imagePath,
			EntryPoint:   m.Source.Entry,
			Namespace:    m.Project.Namespace,
			WrapDir:      wrapDir,
			WrapperPkgs:  wrapperPkgs,
			ProjectDir:   m.Dir,
			MaggieDir:    maggieDir,
			Verbose:      verbose,
		}
		if err := gowrap.BuildEmbedded(opts); err != nil {
			fmt.Fprintf(os.Stderr, "Error building: %v\n", err)
			os.Exit(1)
		}
	}

	if fullSystem {
		fmt.Printf("Built %s (full system)\n", outputBinary)
	} else {
		fmt.Printf("Built %s\n", outputBinary)
	}
}

// compileProjectImage creates a VM, compiles the project sources, and saves
// the resulting image to a temp file. Returns the path to the temp image file.
func compileProjectImage(m *manifest.Manifest, verbose bool) (string, error) {
	vmInst := vm.NewVM()
	defer vmInst.Shutdown()

	// Load the default image first (provides stdlib)
	if err := vmInst.LoadImageFromBytes(embeddedImage); err != nil {
		return "", fmt.Errorf("loading base image: %w", err)
	}
	vmInst.ReRegisterNilPrimitives()
	vmInst.ReRegisterBooleanPrimitives()
	vmInst.UseGoCompiler(compiler.Compile)

	// Compile project sources
	pipe := &pipeline.Pipeline{VM: vmInst}
	if verbose {
		pipe.Verbose = os.Stdout
	}
	methods, err := pipe.LoadProject(m)
	if err != nil {
		return "", fmt.Errorf("compiling project: %w", err)
	}
	if verbose {
		fmt.Printf("Compiled %d methods into image\n", methods)
	}

	// Save to temp file
	tmpFile, err := os.CreateTemp("", "mag-build-image-*.image")
	if err != nil {
		return "", fmt.Errorf("creating temp image: %w", err)
	}
	tmpPath := tmpFile.Name()
	tmpFile.Close()

	if err := vmInst.SaveImage(tmpPath); err != nil {
		os.Remove(tmpPath)
		return "", fmt.Errorf("saving image: %w", err)
	}

	if verbose {
		info, _ := os.Stat(tmpPath)
		if info != nil {
			fmt.Printf("Image size: %d bytes\n", info.Size())
		}
	}

	return tmpPath, nil
}

// detectMaggieDir finds the maggie module directory.
// It uses the runtime to find the maggie package path.
func detectMaggieDir() string {
	// The mag binary is built from the maggie module, so we can find it
	// relative to our own executable or via GOPATH/module cache.
	// Most reliable: use runtime caller info to find our own source.
	_, filename, _, ok := runtime.Caller(0)
	if ok {
		// filename is something like /path/to/maggie/cmd/mag/build.go
		// maggie dir is two levels up
		return filepath.Dir(filepath.Dir(filepath.Dir(filename)))
	}
	// Fallback: check MAGGIE_DIR env var
	if dir := os.Getenv("MAGGIE_DIR"); dir != "" {
		return dir
	}
	fmt.Fprintln(os.Stderr, "Warning: could not detect maggie module directory, set MAGGIE_DIR")
	return "."
}
