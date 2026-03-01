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

// BuildOptions configures the custom binary build.
type BuildOptions struct {
	OutputBinary string
	WrapDir      string
	WrapperPkgs  []WrapperPackageInfo
	ProjectDir   string
	Verbose      bool
}

// Build creates a custom Maggie binary with wrapped Go packages baked in.
// It generates a temporary Go module, writes a main.go that imports the VM
// plus all wrapper packages, and runs `go build`.
func Build(opts BuildOptions) error {
	// Create temp dir for the build
	tmpDir, err := os.MkdirTemp("", "mag-build-*")
	if err != nil {
		return fmt.Errorf("creating temp dir: %w", err)
	}
	defer os.RemoveAll(tmpDir)

	// Resolve project module path from go.mod
	projectModule, err := detectModulePath(opts.ProjectDir)
	if err != nil {
		return fmt.Errorf("detecting module path: %w", err)
	}

	// Generate go.mod
	goModContent := generateGoMod(projectModule, opts.ProjectDir, opts.WrapDir)
	if err := os.WriteFile(filepath.Join(tmpDir, "go.mod"), []byte(goModContent), 0o644); err != nil {
		return fmt.Errorf("writing go.mod: %w", err)
	}

	// Generate main.go
	mainContent := generateMain(projectModule, opts.WrapDir, opts.WrapperPkgs)
	if err := os.WriteFile(filepath.Join(tmpDir, "main.go"), []byte(mainContent), 0o644); err != nil {
		return fmt.Errorf("writing main.go: %w", err)
	}

	if opts.Verbose {
		fmt.Printf("Build temp dir: %s\n", tmpDir)
	}

	// Resolve output path
	outputPath := opts.OutputBinary
	if !filepath.IsAbs(outputPath) {
		cwd, _ := os.Getwd()
		outputPath = filepath.Join(cwd, outputPath)
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

func generateGoMod(projectModule, projectDir, wrapDir string) string {
	var b strings.Builder
	fmt.Fprintf(&b, "module mag-custom-build\n\n")
	fmt.Fprintf(&b, "go 1.24\n\n")
	fmt.Fprintf(&b, "require (\n")
	fmt.Fprintf(&b, "\t%s v0.0.0\n", projectModule)
	fmt.Fprintf(&b, ")\n\n")
	fmt.Fprintf(&b, "replace (\n")
	fmt.Fprintf(&b, "\t%s => %s\n", projectModule, projectDir)

	// Add replace for wrapper packages
	absWrapDir, _ := filepath.Abs(wrapDir)
	entries, _ := os.ReadDir(absWrapDir)
	for _, entry := range entries {
		if entry.IsDir() {
			pkgPath := fmt.Sprintf("mag-custom-build/wrap/%s", entry.Name())
			fmt.Fprintf(&b, "\t%s => %s\n", pkgPath, filepath.Join(absWrapDir, entry.Name()))
		}
	}

	fmt.Fprintf(&b, ")\n")
	return b.String()
}

// EmbeddedBuildOptions configures a build that embeds a compiled image.
type EmbeddedBuildOptions struct {
	OutputBinary string
	ImagePath    string // path to compiled .image file to embed
	EntryPoint   string // e.g. "Main.start" — from maggie.toml [source] entry
	Namespace    string // e.g. "ProcyonPark" — from maggie.toml [project] namespace
	WrapDir      string // optional: directory containing gowrap packages
	WrapperPkgs  []WrapperPackageInfo
	ProjectDir   string // project root (must have go.mod if WrapperPkgs non-empty)
	MaggieDir    string // path to maggie repo (for go.mod replace)
	Verbose      bool
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
	goModContent := generateEmbeddedGoMod(maggieModule, opts.MaggieDir, opts.ProjectDir, opts.WrapDir, opts.WrapperPkgs)
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

func generateEmbeddedGoMod(maggieModule, maggieDir, projectDir, wrapDir string, wrapperPkgs []WrapperPackageInfo) string {
	var b strings.Builder
	fmt.Fprintf(&b, "module mag-embedded-build\n\n")
	fmt.Fprintf(&b, "go 1.24\n\n")
	fmt.Fprintf(&b, "require (\n")
	fmt.Fprintf(&b, "\t%s v0.0.0\n", maggieModule)

	// If there are wrapper packages, we need the project module too
	if len(wrapperPkgs) > 0 && projectDir != "" {
		projectModule, err := detectModulePath(projectDir)
		if err == nil {
			fmt.Fprintf(&b, "\t%s v0.0.0\n", projectModule)
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

func generateMain(projectModule, wrapDir string, wrapperPkgs []WrapperPackageInfo) string {
	var b strings.Builder

	fmt.Fprintf(&b, "// Code generated by mag build. DO NOT EDIT.\n")
	fmt.Fprintf(&b, "package main\n\n")
	fmt.Fprintf(&b, "import (\n")
	fmt.Fprintf(&b, "\t\"fmt\"\n")
	fmt.Fprintf(&b, "\t\"os\"\n\n")
	fmt.Fprintf(&b, "\t\"%s/vm\"\n", projectModule)

	// Import wrapper packages
	for _, pkg := range wrapperPkgs {
		alias := "wrap_" + sanitizePackageName(pkg.PkgName)
		fmt.Fprintf(&b, "\t%s \"mag-custom-build/wrap/%s\"\n", alias, sanitizePackageName(pkg.PkgName))
	}

	fmt.Fprintf(&b, ")\n\n")

	fmt.Fprintf(&b, "func main() {\n")
	fmt.Fprintf(&b, "\tv := vm.NewVM()\n")
	fmt.Fprintf(&b, "\tdefer v.Shutdown()\n\n")

	// Register all wrapper packages
	for _, pkg := range wrapperPkgs {
		alias := "wrap_" + sanitizePackageName(pkg.PkgName)
		fmt.Fprintf(&b, "\t%s.RegisterPrimitives(v)\n", alias)
	}

	fmt.Fprintf(&b, "\n")
	fmt.Fprintf(&b, "\t// Load and run from command-line args\n")
	fmt.Fprintf(&b, "\tif len(os.Args) < 2 {\n")
	fmt.Fprintf(&b, "\t\tfmt.Fprintln(os.Stderr, \"Usage: <binary> <source-path> [-m entry]\")\n")
	fmt.Fprintf(&b, "\t\tos.Exit(1)\n")
	fmt.Fprintf(&b, "\t}\n")
	fmt.Fprintf(&b, "\t_ = fmt.Sprintf // suppress unused\n")
	fmt.Fprintf(&b, "\t_ = v\n")
	fmt.Fprintf(&b, "}\n")

	return b.String()
}
