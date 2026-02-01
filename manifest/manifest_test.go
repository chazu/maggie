package manifest

import (
	"os"
	"path/filepath"
	"testing"
)

func TestLoadManifest(t *testing.T) {
	// Create a temporary directory with a maggie.toml
	dir := t.TempDir()
	tomlContent := `
[project]
name = "test-app"
namespace = "TestApp"
version = "0.1.0"

[source]
dirs = ["src", "lib"]
entry = "Main.start"

[dependencies]
helper = { path = "../helper" }

[image]
output = "test.image"
include-source = true
`
	if err := os.WriteFile(filepath.Join(dir, "maggie.toml"), []byte(tomlContent), 0644); err != nil {
		t.Fatal(err)
	}

	m, err := Load(dir)
	if err != nil {
		t.Fatalf("Load failed: %v", err)
	}

	if m.Project.Name != "test-app" {
		t.Errorf("project name = %q, want test-app", m.Project.Name)
	}
	if m.Project.Namespace != "TestApp" {
		t.Errorf("project namespace = %q, want TestApp", m.Project.Namespace)
	}
	if m.Project.Version != "0.1.0" {
		t.Errorf("project version = %q, want 0.1.0", m.Project.Version)
	}
	if len(m.Source.Dirs) != 2 {
		t.Errorf("source dirs count = %d, want 2", len(m.Source.Dirs))
	}
	if m.Source.Entry != "Main.start" {
		t.Errorf("source entry = %q, want Main.start", m.Source.Entry)
	}
	if len(m.Dependencies) != 1 {
		t.Errorf("dependencies count = %d, want 1", len(m.Dependencies))
	}
	if dep, ok := m.Dependencies["helper"]; !ok || dep.Path != "../helper" {
		t.Errorf("helper dep = %v, want path ../helper", m.Dependencies["helper"])
	}
	if m.Image.Output != "test.image" {
		t.Errorf("image output = %q, want test.image", m.Image.Output)
	}
	if !m.Image.IncludeSource {
		t.Error("image include-source = false, want true")
	}
}

func TestLoadManifestDefaults(t *testing.T) {
	dir := t.TempDir()
	tomlContent := `
[project]
name = "minimal"
`
	if err := os.WriteFile(filepath.Join(dir, "maggie.toml"), []byte(tomlContent), 0644); err != nil {
		t.Fatal(err)
	}

	m, err := Load(dir)
	if err != nil {
		t.Fatalf("Load failed: %v", err)
	}

	// Default source dir should be "src"
	if len(m.Source.Dirs) != 1 || m.Source.Dirs[0] != "src" {
		t.Errorf("default source dirs = %v, want [src]", m.Source.Dirs)
	}
}

func TestFindAndLoad(t *testing.T) {
	// Create nested directory structure
	dir := t.TempDir()
	subDir := filepath.Join(dir, "a", "b", "c")
	if err := os.MkdirAll(subDir, 0755); err != nil {
		t.Fatal(err)
	}

	tomlContent := `[project]
name = "found-project"
`
	if err := os.WriteFile(filepath.Join(dir, "maggie.toml"), []byte(tomlContent), 0644); err != nil {
		t.Fatal(err)
	}

	// Should find manifest when starting from a deep subdirectory
	m, err := FindAndLoad(subDir)
	if err != nil {
		t.Fatalf("FindAndLoad failed: %v", err)
	}
	if m == nil {
		t.Fatal("FindAndLoad returned nil")
	}
	if m.Project.Name != "found-project" {
		t.Errorf("project name = %q, want found-project", m.Project.Name)
	}
}

func TestFindAndLoadNotFound(t *testing.T) {
	dir := t.TempDir()
	m, err := FindAndLoad(dir)
	if err != nil {
		t.Fatalf("FindAndLoad error: %v", err)
	}
	if m != nil {
		t.Error("expected nil manifest when no maggie.toml exists")
	}
}

func TestSourceDirPaths(t *testing.T) {
	m := &Manifest{
		Dir: "/app",
		Source: Source{
			Dirs: []string{"src", "lib"},
		},
	}

	paths := m.SourceDirPaths()
	if len(paths) != 2 {
		t.Fatalf("expected 2 paths, got %d", len(paths))
	}
	if paths[0] != "/app/src" {
		t.Errorf("paths[0] = %q, want /app/src", paths[0])
	}
	if paths[1] != "/app/lib" {
		t.Errorf("paths[1] = %q, want /app/lib", paths[1])
	}
}

func TestLockFileRoundTrip(t *testing.T) {
	dir := t.TempDir()
	lockPath := filepath.Join(dir, "lock.toml")

	lf := &LockFile{
		Deps: []LockedDep{
			{Name: "yutani", Git: "https://github.com/chazu/yutani-mag", Commit: "abc123", Tag: "v0.5.0"},
			{Name: "helper", Path: "../helper"},
		},
	}

	if err := WriteLock(lockPath, lf); err != nil {
		t.Fatalf("WriteLock failed: %v", err)
	}

	loaded, err := ReadLock(lockPath)
	if err != nil {
		t.Fatalf("ReadLock failed: %v", err)
	}

	if len(loaded.Deps) != 2 {
		t.Fatalf("expected 2 deps, got %d", len(loaded.Deps))
	}
	if loaded.Deps[0].Name != "yutani" {
		t.Errorf("dep[0].Name = %q, want yutani", loaded.Deps[0].Name)
	}
	if loaded.Deps[0].Commit != "abc123" {
		t.Errorf("dep[0].Commit = %q, want abc123", loaded.Deps[0].Commit)
	}

	// FindLockedDep
	found := loaded.FindLockedDep("helper")
	if found == nil || found.Path != "../helper" {
		t.Errorf("FindLockedDep(helper) = %v, want path ../helper", found)
	}

	notFound := loaded.FindLockedDep("nonexistent")
	if notFound != nil {
		t.Errorf("FindLockedDep(nonexistent) = %v, want nil", notFound)
	}
}

func TestReadLockNotFound(t *testing.T) {
	lf, err := ReadLock("/nonexistent/path/lock.toml")
	if err != nil {
		t.Errorf("ReadLock should return nil,nil for missing file, got err: %v", err)
	}
	if lf != nil {
		t.Errorf("ReadLock should return nil for missing file, got %v", lf)
	}
}
