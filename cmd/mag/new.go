package main

import (
	"fmt"
	"os"
	"path/filepath"
	"unicode"
	"unicode/utf8"

	"github.com/chazu/maggie/manifest"
)

// ---------------------------------------------------------------------------
// mag new — Create a new Maggie project
// ---------------------------------------------------------------------------

func handleNewCommand(args []string) {
	if len(args) == 0 || wantsHelp(args) {
		subcmdUsage("new <project-name>",
			"Create a new Maggie project with standard structure.",
			usageExamples([][2]string{
				{"mag new myapp", "Create a new project in ./myapp/"},
			}),
			"\nCreates a directory with maggie.toml, src/Main.mag, and a basic project layout.\n",
		)
	}

	// The argument is where to create the project; the project name and
	// namespace come from its last path element (`mag new work/myapp`
	// names the project "myapp", not "work/myapp").
	dir := args[0]
	name := filepath.Base(filepath.Clean(dir))
	namespace := manifest.ToPascalCase(name)
	if first, _ := utf8.DecodeRuneInString(namespace); !unicode.IsLetter(first) {
		fmt.Fprintf(os.Stderr, "Error: cannot derive a namespace from %q: it must start with a letter\n", name)
		os.Exit(1)
	}
	if manifest.IsReservedNamespace(namespace) {
		fmt.Fprintf(os.Stderr, "Error: namespace %q (from %q) is reserved for a core class; choose another project name\n", namespace, name)
		os.Exit(1)
	}

	// Error if directory already exists
	if _, err := os.Stat(dir); err == nil {
		fmt.Fprintf(os.Stderr, "Error: directory %q already exists\n", dir)
		os.Exit(1)
	}

	// Create directory structure
	srcDir := filepath.Join(dir, "src")
	if err := os.MkdirAll(srcDir, 0755); err != nil {
		fmt.Fprintf(os.Stderr, "Error creating directories: %v\n", err)
		os.Exit(1)
	}

	// Write maggie.toml
	tomlContent := fmt.Sprintf(`[project]
name = %q
namespace = %q
version = "0.1.0"

[source]
dirs = ["src"]
entry = "Main.start"

# [test]
# dirs = ["test"]
# entry = "TestRunner.run"

# [scripts]
# prebuild = "mag fmt --check"

# [dev-dependencies]
# test-helpers = { path = "../test-helpers" }
`, name, namespace)

	tomlPath := filepath.Join(dir, "maggie.toml")
	if err := os.WriteFile(tomlPath, []byte(tomlContent), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "Error writing %s: %v\n", tomlPath, err)
		os.Exit(1)
	}

	// Write src/Main.mag, laid out exactly as `mag fmt` would, so the
	// suggested `prebuild = "mag fmt --check"` passes on a fresh project.
	mainContent := fmt.Sprintf(`Main subclass: Object

  classMethod: start [
      '%s started!' println
  ]
`, namespace)

	mainPath := filepath.Join(srcDir, "Main.mag")
	if err := os.WriteFile(mainPath, []byte(mainContent), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "Error writing %s: %v\n", mainPath, err)
		os.Exit(1)
	}

	// Write .gitignore
	gitignoreContent := `.maggie/
*.image
`
	gitignorePath := filepath.Join(dir, ".gitignore")
	if err := os.WriteFile(gitignorePath, []byte(gitignoreContent), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "Error writing %s: %v\n", gitignorePath, err)
		os.Exit(1)
	}

	// Print instructions
	fmt.Printf("Created project %q in %s\n\n", name, dir)
	fmt.Printf("  cd %s\n", dir)
	fmt.Printf("  mag -m Main.start\n\n")
}
