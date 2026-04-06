package gowrap

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestGenerateEmbeddedGoMod(t *testing.T) {
	code := generateEmbeddedGoMod("github.com/chazu/maggie", "/path/to/maggie", "/path/to/project", "", nil, nil)

	if !strings.Contains(code, "module mag-embedded-build") {
		t.Error("expected module declaration")
	}
	if !strings.Contains(code, "github.com/chazu/maggie v0.0.0") {
		t.Error("expected maggie dependency")
	}
	if !strings.Contains(code, "github.com/chazu/maggie => /path/to/maggie") {
		t.Error("expected replace directive")
	}
}

func TestGenerateEmbeddedMain(t *testing.T) {
	code := generateEmbeddedMain("github.com/chazu/maggie", "Main.start", "MyApp", "", "", nil)

	if !strings.Contains(code, "package main") {
		t.Error("expected package main")
	}
	if !strings.Contains(code, `"github.com/chazu/maggie/vm"`) {
		t.Error("expected vm import")
	}
	if !strings.Contains(code, `"github.com/chazu/maggie/compiler"`) {
		t.Error("expected compiler import")
	}
	if !strings.Contains(code, "embeddedImage") {
		t.Error("expected embedded image variable")
	}
	if !strings.Contains(code, "Main.start") {
		t.Error("expected entry point")
	}
}

func TestGenerateEmbeddedMainWithWrappers(t *testing.T) {
	pkgs := []WrapperPackageInfo{
		{ImportPath: "strings", PkgName: "strings"},
	}
	code := generateEmbeddedMain("github.com/chazu/maggie", "Main.start", "MyApp", "/path/to/project", "/path/to/wrap", pkgs)

	if !strings.Contains(code, "wrap_strings") {
		t.Error("expected wrapper import")
	}
	if !strings.Contains(code, "wrap_strings.RegisterPrimitives(v)") {
		t.Error("expected RegisterPrimitives call")
	}
}

func TestGenerateEmbeddedGoModWithExtraDirs(t *testing.T) {
	// Create a temp dir with a fake dependency go.mod
	tmpDir := t.TempDir()
	depDir := filepath.Join(tmpDir, "my-dep")
	if err := os.MkdirAll(depDir, 0o755); err != nil {
		t.Fatal(err)
	}

	depGoMod := `module github.com/example/my-dep

go 1.24

require (
	github.com/gdamore/tcell/v2 v2.7.0
)
`
	if err := os.WriteFile(filepath.Join(depDir, "go.mod"), []byte(depGoMod), 0o644); err != nil {
		t.Fatal(err)
	}

	code := generateEmbeddedGoMod(
		"github.com/chazu/maggie", "/path/to/maggie", "/path/to/project", "",
		nil, []string{depDir},
	)

	// Should include the dependency module require
	if !strings.Contains(code, "github.com/example/my-dep v0.0.0") {
		t.Error("expected dependency module in require block")
	}

	// Should include the dependency's own requires (tcell)
	if !strings.Contains(code, "github.com/gdamore/tcell/v2 v2.7.0") {
		t.Error("expected transitive dependency (tcell) in require block")
	}

	// Should include replace directive for dependency module
	if !strings.Contains(code, "github.com/example/my-dep =>") {
		t.Error("expected replace directive for dependency module")
	}
}

func TestGenerateEmbeddedGoModExtraDirsNoDuplicate(t *testing.T) {
	// If the project already requires a module, extra dirs shouldn't duplicate it
	tmpDir := t.TempDir()

	// Create project go.mod
	projectDir := filepath.Join(tmpDir, "project")
	if err := os.MkdirAll(projectDir, 0o755); err != nil {
		t.Fatal(err)
	}
	projectGoMod := `module github.com/example/project

go 1.24

require (
	github.com/gdamore/tcell/v2 v2.7.0
)
`
	if err := os.WriteFile(filepath.Join(projectDir, "go.mod"), []byte(projectGoMod), 0o644); err != nil {
		t.Fatal(err)
	}

	// Create dep go.mod that also requires tcell
	depDir := filepath.Join(tmpDir, "dep")
	if err := os.MkdirAll(depDir, 0o755); err != nil {
		t.Fatal(err)
	}
	depGoMod := `module github.com/example/dep

go 1.24

require (
	github.com/gdamore/tcell/v2 v2.7.0
)
`
	if err := os.WriteFile(filepath.Join(depDir, "go.mod"), []byte(depGoMod), 0o644); err != nil {
		t.Fatal(err)
	}

	code := generateEmbeddedGoMod(
		"github.com/chazu/maggie", "/path/to/maggie", projectDir, "",
		nil, []string{depDir},
	)

	// tcell should appear only once in requires
	count := strings.Count(code, "github.com/gdamore/tcell/v2")
	if count != 1 {
		t.Errorf("expected tcell to appear once in go.mod, got %d times", count)
	}
}
