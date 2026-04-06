package gowrap

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// WrapperPackageInfo describes a wrapper package to include in the build.
type WrapperPackageInfo struct {
	ImportPath string // original Go import path (e.g., "strings")
	PkgName    string // short package name (e.g., "strings")
}

func detectModulePath(projectDir string) (string, error) {
	goModPath := filepath.Join(projectDir, "go.mod")
	data, err := os.ReadFile(goModPath)
	if err != nil {
		return "", fmt.Errorf("reading go.mod: %w", err)
	}
	for _, line := range strings.Split(string(data), "\n") {
		line = strings.TrimSpace(line)
		if strings.HasPrefix(line, "module ") {
			return strings.TrimSpace(strings.TrimPrefix(line, "module ")), nil
		}
	}
	return "", fmt.Errorf("no module directive found in %s", goModPath)
}

// EmbeddedBuildOptions configures a build that embeds a compiled image.
type EmbeddedBuildOptions struct {
	OutputBinary   string
	ImagePath      string // path to compiled .image file to embed
	EntryPoint     string // e.g. "Main.start" — from maggie.toml [source] entry
	Namespace      string // e.g. "ProcyonPark" — from maggie.toml [project] namespace
	WrapDir        string // optional: directory containing gowrap packages
	WrapperPkgs    []WrapperPackageInfo
	ProjectDir     string // project root (must have go.mod if WrapperPkgs non-empty)
	MaggieDir      string // path to maggie repo (for go.mod replace)
	ExtraGoModDirs []string // additional dirs whose go.mod should be merged (e.g., deps with go-wrap)
	Verbose        bool
}

// BuildEmbedded creates a standalone binary with a compiled Maggie image embedded.
// The generated binary loads the image on startup and runs the entry point.
func BuildEmbedded(opts EmbeddedBuildOptions) error {
	tmpDir, err := os.MkdirTemp("", "mag-build-*")
	if err != nil {
		return fmt.Errorf("creating temp dir: %w", err)
	}
	defer os.RemoveAll(tmpDir)

	// Copy image file into temp dir
	imageData, err := os.ReadFile(opts.ImagePath)
	if err != nil {
		return fmt.Errorf("reading image: %w", err)
	}
	if err := os.WriteFile(filepath.Join(tmpDir, "app.image"), imageData, 0o644); err != nil {
		return fmt.Errorf("writing embedded image: %w", err)
	}

	// Detect maggie module path
	maggieModule, err := detectModulePath(opts.MaggieDir)
	if err != nil {
		return fmt.Errorf("detecting maggie module: %w", err)
	}

	// Generate go.mod
	goModContent := generateEmbeddedGoMod(maggieModule, opts.MaggieDir, opts.ProjectDir, opts.WrapDir, opts.WrapperPkgs, opts.ExtraGoModDirs)
	if err := os.WriteFile(filepath.Join(tmpDir, "go.mod"), []byte(goModContent), 0o644); err != nil {
		return fmt.Errorf("writing go.mod: %w", err)
	}

	// Generate main.go
	mainContent := generateEmbeddedMain(maggieModule, opts.EntryPoint, opts.Namespace, opts.ProjectDir, opts.WrapDir, opts.WrapperPkgs)
	if err := os.WriteFile(filepath.Join(tmpDir, "main.go"), []byte(mainContent), 0o644); err != nil {
		return fmt.Errorf("writing main.go: %w", err)
	}

	if opts.Verbose {
		fmt.Printf("Build temp dir: %s\n", tmpDir)
		fmt.Printf("Image size: %d bytes\n", len(imageData))
	}

	// Resolve output path
	outputPath := opts.OutputBinary
	if !filepath.IsAbs(outputPath) {
		cwd, _ := os.Getwd()
		outputPath = filepath.Join(cwd, outputPath)
	}

	// Run go mod tidy to resolve indirect dependencies
	tidy := exec.Command("go", "mod", "tidy")
	tidy.Dir = tmpDir
	tidy.Stdout = os.Stdout
	tidy.Stderr = os.Stderr
	if err := tidy.Run(); err != nil {
		return fmt.Errorf("go mod tidy: %w", err)
	}

	// Run go build
	cmd := exec.Command("go", "build", "-o", outputPath, ".")
	cmd.Dir = tmpDir
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	if err := cmd.Run(); err != nil {
		return fmt.Errorf("go build: %w", err)
	}

	return nil
}

// FullSystemBuildOptions configures a full-system build. The resulting binary
// is a complete mag CLI (REPL, fmt, doctest, etc.) with the project's compiled
// image embedded instead of the stock maggie.image. When invoked with no
// arguments, it runs the project's entry point.
type FullSystemBuildOptions struct {
	OutputBinary   string
	ImagePath      string // compiled .image with project code baked in
	EntryPoint     string // default entry point (e.g., "Main.start")
	Namespace      string // project namespace
	WrapDir        string // optional: directory containing gowrap packages
	WrapperPkgs    []WrapperPackageInfo
	ProjectDir     string
	MaggieDir      string   // path to maggie repo root (source of cmd/mag/)
	ExtraGoModDirs []string // additional dirs whose go.mod should be merged (e.g., deps with go-wrap)
	Verbose        bool
}

// BuildFullSystem creates a binary that is a full mag CLI with the project's
// image embedded. It copies cmd/mag/*.go from the maggie repo, replaces the
// embedded image, and generates a project_config.go that sets the default
// entry point and registers any wrapped Go packages.
func BuildFullSystem(opts FullSystemBuildOptions) error {
	tmpDir, err := os.MkdirTemp("", "mag-full-build-*")
	if err != nil {
		return fmt.Errorf("creating temp dir: %w", err)
	}
	defer os.RemoveAll(tmpDir)

	// Copy compiled image as maggie.image (the name cmd/mag/main.go expects)
	imageData, err := os.ReadFile(opts.ImagePath)
	if err != nil {
		return fmt.Errorf("reading image: %w", err)
	}
	if err := os.WriteFile(filepath.Join(tmpDir, "maggie.image"), imageData, 0o644); err != nil {
		return fmt.Errorf("writing embedded image: %w", err)
	}

	// Copy all non-test .go files from cmd/mag/
	cmdMagDir := filepath.Join(opts.MaggieDir, "cmd", "mag")
	entries, err := os.ReadDir(cmdMagDir)
	if err != nil {
		return fmt.Errorf("reading cmd/mag: %w", err)
	}
	for _, entry := range entries {
		name := entry.Name()
		if !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_test.go") {
			continue
		}
		data, err := os.ReadFile(filepath.Join(cmdMagDir, name))
		if err != nil {
			return fmt.Errorf("reading %s: %w", name, err)
		}
		if err := os.WriteFile(filepath.Join(tmpDir, name), data, 0o644); err != nil {
			return fmt.Errorf("writing %s: %w", name, err)
		}
	}

	// Detect maggie module path
	maggieModule, err := detectModulePath(opts.MaggieDir)
	if err != nil {
		return fmt.Errorf("detecting maggie module: %w", err)
	}

	// Generate project_config.go — sets the default entry point and
	// registers wrapper packages via init().
	configContent := generateProjectConfig(maggieModule, opts.EntryPoint, opts.Namespace, opts.ProjectDir, opts.WrapDir, opts.WrapperPkgs)
	if err := os.WriteFile(filepath.Join(tmpDir, "project_config.go"), []byte(configContent), 0o644); err != nil {
		return fmt.Errorf("writing project_config.go: %w", err)
	}

	// Generate go.mod — same structure as embedded builds but using
	// the full set of cmd/mag dependencies (maggie module provides all).
	goModContent := generateEmbeddedGoMod(maggieModule, opts.MaggieDir, opts.ProjectDir, opts.WrapDir, opts.WrapperPkgs, opts.ExtraGoModDirs)
	if err := os.WriteFile(filepath.Join(tmpDir, "go.mod"), []byte(goModContent), 0o644); err != nil {
		return fmt.Errorf("writing go.mod: %w", err)
	}

	if opts.Verbose {
		fmt.Printf("Full-system build temp dir: %s\n", tmpDir)
		fmt.Printf("Image size: %d bytes\n", len(imageData))
	}

	// Resolve output path
	outputPath := opts.OutputBinary
	if !filepath.IsAbs(outputPath) {
		cwd, _ := os.Getwd()
		outputPath = filepath.Join(cwd, outputPath)
	}

	// Run go mod tidy
	tidy := exec.Command("go", "mod", "tidy")
	tidy.Dir = tmpDir
	tidy.Stdout = os.Stdout
	tidy.Stderr = os.Stderr
	if err := tidy.Run(); err != nil {
		return fmt.Errorf("go mod tidy: %w", err)
	}

	// Run go build
	cmd := exec.Command("go", "build", "-o", outputPath, ".")
	cmd.Dir = tmpDir
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("go build: %w", err)
	}

	return nil
}

// generateProjectConfig creates a project_config.go that sets the default
// entry point and registers wrapper packages for full-system builds.
func generateProjectConfig(maggieModule, entryPoint, namespace, projectDir, wrapDir string, wrapperPkgs []WrapperPackageInfo) string {
	var b strings.Builder

	fmt.Fprintf(&b, "// Code generated by mag build --full. DO NOT EDIT.\n")
	fmt.Fprintf(&b, "package main\n\n")

	// Import wrapper packages if needed
	if len(wrapperPkgs) > 0 && projectDir != "" {
		projectModule, _ := detectModulePath(projectDir)
		wrapBase := filepath.Base(wrapDir)
		if wrapBase == "" {
			wrapBase = "gowrap"
		}
		fmt.Fprintf(&b, "import (\n")
		for _, pkg := range wrapperPkgs {
			alias := "wrap_" + sanitizePackageName(pkg.PkgName)
			fmt.Fprintf(&b, "\t%s \"%s/%s/%s\"\n", alias, projectModule, wrapBase, sanitizePackageName(pkg.PkgName))
		}
		fmt.Fprintf(&b, ")\n\n")
	}

	fmt.Fprintf(&b, "func init() {\n")
	if entryPoint != "" {
		fmt.Fprintf(&b, "\tprojectEntryPoint = %q\n", entryPoint)
	}
	if namespace != "" {
		fmt.Fprintf(&b, "\tprojectNamespace = %q\n", namespace)
	}

	// Register wrapper packages — they need to be registered after VM init,
	// so we add them to a hook slice that main() will call.
	if len(wrapperPkgs) > 0 {
		fmt.Fprintf(&b, "\tprojectWrapperRegistrars = append(projectWrapperRegistrars,\n")
		for _, pkg := range wrapperPkgs {
			alias := "wrap_" + sanitizePackageName(pkg.PkgName)
			fmt.Fprintf(&b, "\t\t%s.RegisterPrimitives,\n", alias)
		}
		fmt.Fprintf(&b, "\t)\n")
	}

	fmt.Fprintf(&b, "}\n")
	return b.String()
}

// parseGoModDirectives extracts require and replace directives from a go.mod file.
// It returns them as raw lines so they can be merged into the generated go.mod.
func parseGoModDirectives(projectDir string) (requires []string, replaces []string) {
	goModPath := filepath.Join(projectDir, "go.mod")
	data, err := os.ReadFile(goModPath)
	if err != nil {
		return nil, nil
	}

	lines := strings.Split(string(data), "\n")
	inRequire := false
	inReplace := false

	for _, line := range lines {
		trimmed := strings.TrimSpace(line)

		// Track block boundaries
		if trimmed == "require (" {
			inRequire = true
			continue
		}
		if trimmed == "replace (" {
			inReplace = true
			continue
		}
		if trimmed == ")" {
			inRequire = false
			inReplace = false
			continue
		}

		// Single-line directives
		if strings.HasPrefix(trimmed, "require ") && !strings.HasSuffix(trimmed, "(") {
			requires = append(requires, strings.TrimPrefix(trimmed, "require "))
			continue
		}
		if strings.HasPrefix(trimmed, "replace ") && !strings.HasSuffix(trimmed, "(") {
			replaces = append(replaces, strings.TrimPrefix(trimmed, "replace "))
			continue
		}

		// Block entries
		if inRequire && trimmed != "" && !strings.HasPrefix(trimmed, "//") {
			requires = append(requires, trimmed)
		}
		if inReplace && trimmed != "" && !strings.HasPrefix(trimmed, "//") {
			replaces = append(replaces, trimmed)
		}
	}

	return requires, replaces
}

func generateEmbeddedGoMod(maggieModule, maggieDir, projectDir, wrapDir string, wrapperPkgs []WrapperPackageInfo, extraGoModDirs []string) string {
	var b strings.Builder
	fmt.Fprintf(&b, "module mag-embedded-build\n\n")
	fmt.Fprintf(&b, "go 1.24\n\n")

	// Collect requires and replaces from the project's go.mod
	projectRequires, projectReplaces := parseGoModDirectives(projectDir)

	// Build a set of modules we already handle so we don't duplicate
	handledRequires := map[string]bool{maggieModule: true}
	handledReplaces := map[string]bool{maggieModule: true}

	// Collect deferred replace directives from extra dirs (written after require block)
	type replaceEntry struct {
		module string
		target string
	}
	var extraReplaces []replaceEntry

	fmt.Fprintf(&b, "require (\n")
	fmt.Fprintf(&b, "\t%s v0.0.0\n", maggieModule)

	// If there are wrapper packages, we need the project module too
	if len(wrapperPkgs) > 0 && projectDir != "" {
		projectModule, err := detectModulePath(projectDir)
		if err == nil {
			fmt.Fprintf(&b, "\t%s v0.0.0\n", projectModule)
			handledRequires[projectModule] = true
			handledReplaces[projectModule] = true
		}
	}

	// Propagate project's require directives
	for _, req := range projectRequires {
		parts := strings.Fields(req)
		if len(parts) >= 1 && !handledRequires[parts[0]] {
			fmt.Fprintf(&b, "\t%s\n", req)
			handledRequires[parts[0]] = true
		}
	}

	// Propagate require/replace directives from dependency go.mod files
	// (for dependencies that declare go-wrap packages)
	for _, dir := range extraGoModDirs {
		depModule, err := detectModulePath(dir)
		if err != nil {
			continue // skip deps without go.mod
		}

		// Add require + replace for the dependency module itself
		if !handledRequires[depModule] {
			fmt.Fprintf(&b, "\t%s v0.0.0\n", depModule)
			handledRequires[depModule] = true
		}
		if !handledReplaces[depModule] {
			absDir, _ := filepath.Abs(dir)
			extraReplaces = append(extraReplaces, replaceEntry{depModule, absDir})
			handledReplaces[depModule] = true
		}

		// Merge the dependency's own require/replace directives
		depRequires, depReplacesRaw := parseGoModDirectives(dir)
		for _, req := range depRequires {
			parts := strings.Fields(req)
			if len(parts) >= 1 && !handledRequires[parts[0]] {
				fmt.Fprintf(&b, "\t%s\n", req)
				handledRequires[parts[0]] = true
			}
		}
		for _, rep := range depReplacesRaw {
			parts := strings.Fields(rep)
			if len(parts) >= 3 && parts[1] == "=>" && !handledReplaces[parts[0]] {
				target := parts[2]
				if !filepath.IsAbs(target) {
					target = filepath.Join(dir, target)
				}
				extraReplaces = append(extraReplaces, replaceEntry{parts[0], target})
				handledReplaces[parts[0]] = true
			}
		}
	}

	fmt.Fprintf(&b, ")\n\n")
	fmt.Fprintf(&b, "replace (\n")
	fmt.Fprintf(&b, "\t%s => %s\n", maggieModule, maggieDir)

	if len(wrapperPkgs) > 0 && projectDir != "" {
		projectModule, err := detectModulePath(projectDir)
		if err == nil {
			fmt.Fprintf(&b, "\t%s => %s\n", projectModule, projectDir)
		}
	}

	// Propagate project's replace directives, resolving relative paths
	for _, rep := range projectReplaces {
		parts := strings.Fields(rep)
		if len(parts) >= 3 && parts[1] == "=>" && !handledReplaces[parts[0]] {
			target := parts[2]
			// Resolve relative paths against the project directory
			if !filepath.IsAbs(target) {
				target = filepath.Join(projectDir, target)
			}
			fmt.Fprintf(&b, "\t%s => %s\n", parts[0], target)
			handledReplaces[parts[0]] = true
		}
	}

	// Write deferred replace directives from dependency go.mod files
	for _, r := range extraReplaces {
		fmt.Fprintf(&b, "\t%s => %s\n", r.module, r.target)
	}

	fmt.Fprintf(&b, ")\n")
	return b.String()
}

func generateEmbeddedMain(maggieModule, entryPoint, namespace, projectDir, wrapDir string, wrapperPkgs []WrapperPackageInfo) string {
	var b strings.Builder

	fmt.Fprintf(&b, "// Code generated by mag build. DO NOT EDIT.\n")
	fmt.Fprintf(&b, "package main\n\n")
	fmt.Fprintf(&b, "import (\n")
	fmt.Fprintf(&b, "\t_ \"embed\"\n")
	fmt.Fprintf(&b, "\t\"fmt\"\n")
	fmt.Fprintf(&b, "\t\"os\"\n")
	fmt.Fprintf(&b, "\t\"strings\"\n\n")
	fmt.Fprintf(&b, "\t\"%s/compiler\"\n", maggieModule)
	fmt.Fprintf(&b, "\t\"%s/vm\"\n", maggieModule)

	// Import wrapper packages if any — they live under <projectModule>/gowrap/<pkgName>
	if len(wrapperPkgs) > 0 && projectDir != "" {
		projectModule, _ := detectModulePath(projectDir)
		wrapBase := filepath.Base(wrapDir)
		if wrapBase == "" {
			wrapBase = "gowrap"
		}
		for _, pkg := range wrapperPkgs {
			alias := "wrap_" + sanitizePackageName(pkg.PkgName)
			fmt.Fprintf(&b, "\t%s \"%s/%s/%s\"\n", alias, projectModule, wrapBase, sanitizePackageName(pkg.PkgName))
		}
	}

	fmt.Fprintf(&b, ")\n\n")

	// Embed the image
	fmt.Fprintf(&b, "//go:embed app.image\n")
	fmt.Fprintf(&b, "var embeddedImage []byte\n\n")

	fmt.Fprintf(&b, "func main() {\n")
	fmt.Fprintf(&b, "\tv := vm.NewVM()\n")
	fmt.Fprintf(&b, "\tdefer v.Shutdown()\n\n")

	// Load the embedded image
	fmt.Fprintf(&b, "\tif err := v.LoadImageFromBytes(embeddedImage); err != nil {\n")
	fmt.Fprintf(&b, "\t\tfmt.Fprintf(os.Stderr, \"Error loading embedded image: %%v\\n\", err)\n")
	fmt.Fprintf(&b, "\t\tos.Exit(1)\n")
	fmt.Fprintf(&b, "\t}\n\n")

	// Re-register primitives after image load
	fmt.Fprintf(&b, "\tv.ReRegisterNilPrimitives()\n")
	fmt.Fprintf(&b, "\tv.ReRegisterBooleanPrimitives()\n")
	fmt.Fprintf(&b, "\tv.UseGoCompiler(compiler.Compile)\n\n")

	// Register wrapper packages if any
	for _, pkg := range wrapperPkgs {
		alias := "wrap_" + sanitizePackageName(pkg.PkgName)
		fmt.Fprintf(&b, "\t%s.RegisterPrimitives(v)\n", alias)
	}

	// Run the entry point
	if entryPoint != "" {
		fmt.Fprintf(&b, "\n\t// Run entry point: %s\n", entryPoint)
		fmt.Fprintf(&b, "\tentry := %q\n", entryPoint)
		fmt.Fprintf(&b, "\tnamespace := %q\n", namespace)
		fmt.Fprintf(&b, "\tvar className, methodName string\n")
		fmt.Fprintf(&b, "\tif strings.Contains(entry, \".\") {\n")
		fmt.Fprintf(&b, "\t\tparts := strings.SplitN(entry, \".\", 2)\n")
		fmt.Fprintf(&b, "\t\tclassName = parts[0]\n")
		fmt.Fprintf(&b, "\t\tmethodName = parts[1]\n")
		fmt.Fprintf(&b, "\t} else {\n")
		fmt.Fprintf(&b, "\t\tclassName = \"Object\"\n")
		fmt.Fprintf(&b, "\t\tmethodName = entry\n")
		fmt.Fprintf(&b, "\t}\n\n")
		// Try bare name first, then namespaced
		fmt.Fprintf(&b, "\tqualifiedName := className\n")
		fmt.Fprintf(&b, "\tif v.Classes.Lookup(className) == nil && namespace != \"\" {\n")
		fmt.Fprintf(&b, "\t\tqualifiedName = namespace + \"::\" + className\n")
		fmt.Fprintf(&b, "\t}\n")
		fmt.Fprintf(&b, "\tif v.Classes.Lookup(qualifiedName) == nil {\n")
		fmt.Fprintf(&b, "\t\tfmt.Fprintf(os.Stderr, \"Error: class %%q not found\\n\", className)\n")
		fmt.Fprintf(&b, "\t\tos.Exit(1)\n")
		fmt.Fprintf(&b, "\t}\n\n")
		// Find the class method and execute via interpreter
		fmt.Fprintf(&b, "\tclass := v.Classes.Lookup(qualifiedName)\n")
		fmt.Fprintf(&b, "\tselectorID := v.Selectors.Intern(methodName)\n")
		fmt.Fprintf(&b, "\tvar method vm.Method\n")
		fmt.Fprintf(&b, "\tif class.ClassVTable != nil {\n")
		fmt.Fprintf(&b, "\t\tmethod = class.ClassVTable.Lookup(selectorID)\n")
		fmt.Fprintf(&b, "\t}\n")
		fmt.Fprintf(&b, "\tif method == nil {\n")
		fmt.Fprintf(&b, "\t\tmethod = class.LookupMethod(v.Selectors, methodName)\n")
		fmt.Fprintf(&b, "\t}\n")
		fmt.Fprintf(&b, "\tif method == nil {\n")
		fmt.Fprintf(&b, "\t\tfmt.Fprintf(os.Stderr, \"Error: method %%q not found on %%s\\n\", methodName, qualifiedName)\n")
		fmt.Fprintf(&b, "\t\tos.Exit(1)\n")
		fmt.Fprintf(&b, "\t}\n")
		fmt.Fprintf(&b, "\tclassValue := v.Symbols.SymbolValue(qualifiedName)\n")
		fmt.Fprintf(&b, "\tif cm, ok := method.(*vm.CompiledMethod); ok {\n")
		fmt.Fprintf(&b, "\t\tv.Execute(cm, classValue, nil)\n")
		fmt.Fprintf(&b, "\t} else {\n")
		fmt.Fprintf(&b, "\t\tv.Send(classValue, methodName, nil)\n")
		fmt.Fprintf(&b, "\t}\n")
	} else {
		fmt.Fprintf(&b, "\n\t// No entry point specified — binary loaded image only\n")
		fmt.Fprintf(&b, "\tfmt.Println(\"Image loaded successfully\")\n")
	}

	fmt.Fprintf(&b, "\t_ = fmt.Sprintf\n")
	fmt.Fprintf(&b, "}\n")

	return b.String()
}

