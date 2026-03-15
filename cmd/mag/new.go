package main

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/chazu/maggie/manifest"
)

// ---------------------------------------------------------------------------
// mag new — Create a new Maggie project
// ---------------------------------------------------------------------------

func handleNewCommand(args []string) {
	if len(args) == 0 || args[0] == "--help" || args[0] == "-h" {
		fmt.Fprintf(os.Stderr, "Usage: mag new <project-name>\n\n")
		fmt.Fprintf(os.Stderr, "Create a new Maggie project with standard structure.\n")
		if len(args) == 0 {
			os.Exit(1)
		}
		os.Exit(0)
	}

	name := args[0]
	namespace := manifest.ToPascalCase(name)

	// Error if directory already exists
	if _, err := os.Stat(name); err == nil {
		fmt.Fprintf(os.Stderr, "Error: directory %q already exists\n", name)
		os.Exit(1)
	}

	// Create directory structure
	srcDir := filepath.Join(name, "src")
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
`, name, namespace)

	tomlPath := filepath.Join(name, "maggie.toml")
	if err := os.WriteFile(tomlPath, []byte(tomlContent), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "Error writing %s: %v\n", tomlPath, err)
		os.Exit(1)
	}

	// Write src/Main.mag
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
	gitignorePath := filepath.Join(name, ".gitignore")
	if err := os.WriteFile(gitignorePath, []byte(gitignoreContent), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "Error writing %s: %v\n", gitignorePath, err)
		os.Exit(1)
	}

	// Print instructions
	fmt.Printf("Created project %q in ./%s/\n\n", name, name)
	fmt.Printf("  cd %s\n", name)
	fmt.Printf("  mag -m Main.start\n\n")
}
