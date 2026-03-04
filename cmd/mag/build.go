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
//	mag build              # ./mag-custom (gowrap-only, legacy)
//	mag build -o myapp     # custom output
//
// If the project has [source] dirs and an entry point, the sources are compiled
// into an image that is embedded in the output binary. The binary loads the image
// on startup and runs the entry point. No Go code is needed in the project.
//
// If [go-wrap] packages are present, they are also included in the binary.
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
		fmt.Fprintln(os.Stderr, "mag build requires a maggie.toml")
		os.Exit(1)
	}

	hasSources := len(m.Source.Dirs) > 0
	hasGoWrap := len(m.GoWrap.Packages) > 0

	if !hasSources && !hasGoWrap {
		fmt.Fprintln(os.Stderr, "Error: maggie.toml has no [source] dirs and no [go-wrap] packages")
		os.Exit(1)
	}

	// If we have sources, compile them into an image
	var imagePath string
	if hasSources {
		imagePath, err = compileProjectImage(m, verbose)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error compiling image: %v\n", err)
			os.Exit(1)
		}
		defer os.Remove(imagePath)
	}

	if imagePath != "" {
		// Embedded image build — may or may not have gowrap packages
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
	} else {
		// Legacy gowrap-only build (no sources to embed)
		if !hasGoWrap {
			fmt.Fprintln(os.Stderr, "No [go-wrap.packages] configured in maggie.toml")
			os.Exit(1)
		}

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

		opts := gowrap.BuildOptions{
			OutputBinary: outputBinary,
			WrapDir:      wrapDir,
			WrapperPkgs:  wrapperPkgs,
			ProjectDir:   m.Dir,
			Verbose:      verbose,
		}

		if err := gowrap.Build(opts); err != nil {
			fmt.Fprintf(os.Stderr, "Error building: %v\n", err)
			os.Exit(1)
		}
	}

	fmt.Printf("Built %s\n", outputBinary)
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
