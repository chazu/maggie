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
func handleBuildCommand(args []string, verbose bool) {
	if wantsHelp(args) {
		subcmdUsage("build [--full] [-o output] [-t target] [--all]",
			"Compile a Maggie project into a standalone binary.",
			usageFlags([][2]string{
				{"--full", "Build a full mag system (REPL, fmt, doctest, etc.) with project baked in"},
				{"-o, --output <path>", "Output binary path (overrides target output)"},
				{"-t, --target <name>", "Build a specific target from [[target]] in maggie.toml"},
				{"--all", "Build all targets"},
			}),
			usageExamples([][2]string{
				{"mag build", "Build entry-point-only binary"},
				{"mag build --full", "Build full mag system with project code"},
				{"mag build --full -o myapp", "Full system with custom output name"},
				{"mag build -t server", "Build the 'server' target"},
				{"mag build --all", "Build all declared targets"},
			}),
			"\nWithout --full, the binary only runs the entry point.\n"+
				"With --full, the binary is a complete mag CLI with your project's classes\n"+
				"pre-loaded. Running it with no arguments executes your entry point;\n"+
				"all mag subcommands (fmt, doctest, help, -i, etc.) still work.\n"+
				"\nWith [[target]] sections in maggie.toml, use -t to build a specific target\n"+
				"or --all to build all targets. Without targets, builds from top-level config.\n",
		)
	}

	var outputBinary string
	var targetName string
	var buildAll bool
	fullOverride := -1 // -1 = not set, 0 = false, 1 = true

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
			fullOverride = 1
		case "-t", "--target":
			if i+1 < len(args) {
				targetName = args[i+1]
				i++
			} else {
				fmt.Fprintln(os.Stderr, "Error: -t requires a target name")
				os.Exit(1)
			}
		case "--all":
			buildAll = true
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
		fmt.Fprintf(os.Stderr, "%s build requires a maggie.toml\n", progName())
		os.Exit(1)
	}

	if len(m.Source.Dirs) == 0 {
		fmt.Fprintln(os.Stderr, "Error: maggie.toml has no [source] dirs")
		fmt.Fprintf(os.Stderr, "%s build requires source files to compile into an embedded image\n", progName())
		os.Exit(1)
	}

	// Run prebuild script
	if err := manifest.RunScript("prebuild", m.Scripts.Prebuild, m.Dir, verbose); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	// Resolve targets
	var targets []manifest.ResolvedTarget
	if buildAll {
		targets, err = m.ResolveAllTargets()
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
	} else if targetName != "" {
		t, err := m.ResolveTarget(targetName)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
		targets = []manifest.ResolvedTarget{*t}
	} else {
		targets = []manifest.ResolvedTarget{*m.ResolveDefaultTarget()}
	}

	// Apply CLI overrides
	for i := range targets {
		if outputBinary != "" {
			targets[i].Output = outputBinary
		}
		if fullOverride >= 0 {
			targets[i].Full = fullOverride == 1
		}
		if targets[i].Output == "" {
			targets[i].Output = "mag-custom"
		}
	}

	// Build each target
	for _, target := range targets {
		if err := buildTarget(m, &target, verbose); err != nil {
			fmt.Fprintf(os.Stderr, "Error building target %q: %v\n", target.Name, err)
			os.Exit(1)
		}
		if target.Full {
			fmt.Printf("Built %s (full system)\n", target.Output)
		} else {
			fmt.Printf("Built %s\n", target.Output)
		}
	}

	// Run postbuild script
	if err := manifest.RunScript("postbuild", m.Scripts.Postbuild, m.Dir, verbose); err != nil {
		fmt.Fprintf(os.Stderr, "Warning: %v\n", err)
	}
}

// buildTarget compiles and links a single build target.
func buildTarget(m *manifest.Manifest, target *manifest.ResolvedTarget, verbose bool) error {
	// Compile sources into an image
	imagePath, err := compileTargetImage(m, target, verbose)
	if err != nil {
		return fmt.Errorf("compiling image: %w", err)
	}
	defer os.Remove(imagePath)

	// Collect go-wrap packages from dependencies
	depGoWrapPkgs, depGoModDirs, err := collectDepGoWrapPackages(m, verbose)
	if err != nil {
		return fmt.Errorf("collecting dependency go-wrap packages: %w", err)
	}

	// Merge dependency go-wrap packages with target's own (dedup by import path)
	allGoWrapPkgs := make([]manifest.GoWrapPackage, len(target.GoWrap.Packages))
	copy(allGoWrapPkgs, target.GoWrap.Packages)
	if len(depGoWrapPkgs) > 0 {
		seen := make(map[string]bool, len(allGoWrapPkgs))
		for _, pkg := range allGoWrapPkgs {
			seen[pkg.Import] = true
		}
		for _, pkg := range depGoWrapPkgs {
			if !seen[pkg.Import] {
				allGoWrapPkgs = append(allGoWrapPkgs, pkg)
				seen[pkg.Import] = true
			}
		}
	}

	// Wrap Go packages if configured (including inherited from deps)
	var wrapperPkgs []gowrap.WrapperPackageInfo
	var wrapDir string

	if len(allGoWrapPkgs) > 0 {
		wrapDir = m.WrapOutputDir()
		if target.GoWrap.Output != "" {
			wrapDir = filepath.Join(m.Dir, target.GoWrap.Output)
		}
		if verbose {
			fmt.Fprintf(os.Stderr, "go-wrap: %d packages (%d from deps), wrapDir=%s\n",
				len(allGoWrapPkgs), len(depGoWrapPkgs), wrapDir)
		}
		for _, pkg := range allGoWrapPkgs {
			wt := wrapTarget{
				ImportPath: pkg.Import,
				Include:    pkg.Include,
			}
			if err := wrapPackage(wt, wrapDir, verbose); err != nil {
				return fmt.Errorf("wrapping %s: %w", pkg.Import, err)
			}
		}
		for _, pkg := range allGoWrapPkgs {
			model, err := gowrap.IntrospectPackage(pkg.Import, nil)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Warning: could not introspect %s: %v (skipping wrapper registration)\n", pkg.Import, err)
				continue
			}
			if verbose {
				fmt.Fprintf(os.Stderr, "go-wrap: registered %s (pkg=%s)\n", pkg.Import, model.Name)
			}
			wrapperPkgs = append(wrapperPkgs, gowrap.WrapperPackageInfo{
				ImportPath: pkg.Import,
				PkgName:    model.Name,
			})
		}
	}

	maggieDir := detectMaggieDir()

	if target.Full {
		opts := gowrap.FullSystemBuildOptions{
			OutputBinary:   target.Output,
			ImagePath:      imagePath,
			EntryPoint:     target.Entry,
			Namespace:      m.Project.Namespace,
			WrapDir:        wrapDir,
			WrapperPkgs:    wrapperPkgs,
			ProjectDir:     m.Dir,
			MaggieDir:      maggieDir,
			ExtraGoModDirs: depGoModDirs,
			Verbose:        verbose,
		}
		return gowrap.BuildFullSystem(opts)
	}

	opts := gowrap.EmbeddedBuildOptions{
		OutputBinary:   target.Output,
		ImagePath:      imagePath,
		EntryPoint:     target.Entry,
		Namespace:      m.Project.Namespace,
		WrapDir:        wrapDir,
		WrapperPkgs:    wrapperPkgs,
		ProjectDir:     m.Dir,
		MaggieDir:      maggieDir,
		ExtraGoModDirs: depGoModDirs,
		Verbose:        verbose,
	}
	return gowrap.BuildEmbedded(opts)
}

// collectDepGoWrapPackages resolves dependencies and collects any go-wrap
// packages declared in their manifests. Returns the collected packages and
// a list of dependency directories whose go.mod should be merged into the build.
func collectDepGoWrapPackages(m *manifest.Manifest, verbose bool) ([]manifest.GoWrapPackage, []string, error) {
	if len(m.Dependencies) == 0 {
		return nil, nil, nil
	}

	resolver := manifest.NewResolver(m, verbose)
	deps, err := resolver.Resolve()
	if err != nil {
		return nil, nil, fmt.Errorf("dependency resolution: %w", err)
	}

	var packages []manifest.GoWrapPackage
	var depDirs []string
	seen := make(map[string]bool)

	for _, dep := range deps {
		if dep.Manifest == nil || len(dep.Manifest.GoWrap.Packages) == 0 {
			continue
		}

		depDirs = append(depDirs, dep.LocalPath)

		for _, pkg := range dep.Manifest.GoWrap.Packages {
			if seen[pkg.Import] {
				continue
			}
			seen[pkg.Import] = true
			packages = append(packages, pkg)
			if verbose {
				fmt.Fprintf(os.Stderr, "go-wrap: inherited %s from dependency %s\n", pkg.Import, dep.Name)
			}
		}
	}

	return packages, depDirs, nil
}

// compileTargetImage creates a VM, compiles a target's sources, and saves the image.
func compileTargetImage(m *manifest.Manifest, target *manifest.ResolvedTarget, verbose bool) (string, error) {
	vmInst := vm.NewVM()
	defer vmInst.Shutdown()

	if err := vmInst.LoadImageFromBytes(embeddedImage); err != nil {
		return "", fmt.Errorf("loading base image: %w", err)
	}
	vmInst.ReRegisterNilPrimitives()
	vmInst.ReRegisterBooleanPrimitives()
	vmInst.UseGoCompiler(compiler.Compile)

	pipe := &pipeline.Pipeline{
		VM:      vmInst,
		Exclude: target.Exclude,
	}
	if verbose {
		pipe.Verbose = os.Stdout
	}

	methods, err := pipe.LoadTarget(m, target)
	if err != nil {
		return "", fmt.Errorf("compiling project: %w", err)
	}
	if verbose {
		fmt.Printf("Compiled %d methods into image\n", methods)
	}

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

// compileProjectImage creates a VM, compiles the project sources, and saves
// the resulting image to a temp file. Returns the path to the temp image file.
// Kept for backward compatibility with non-target-aware callers.
func compileProjectImage(m *manifest.Manifest, verbose bool) (string, error) {
	target := m.ResolveDefaultTarget()
	return compileTargetImage(m, target, verbose)
}

// detectMaggieDir finds the maggie module directory.
func detectMaggieDir() string {
	_, filename, _, ok := runtime.Caller(0)
	if ok {
		return filepath.Dir(filepath.Dir(filepath.Dir(filename)))
	}
	if dir := os.Getenv("MAGGIE_DIR"); dir != "" {
		return dir
	}
	fmt.Fprintln(os.Stderr, "Warning: could not detect maggie module directory, set MAGGIE_DIR")
	return "."
}
