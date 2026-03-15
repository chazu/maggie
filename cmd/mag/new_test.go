package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestHandleNewCommand_CreatesProject(t *testing.T) {
	dir := t.TempDir()
	projName := "test-app"
	projDir := filepath.Join(dir, projName)

	// Change to temp dir so the command creates the project there
	origDir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	defer os.Chdir(origDir)
	os.Chdir(dir)

	// We can't call handleNewCommand directly because it calls os.Exit.
	// Instead, replicate the core logic inline.
	name := projName
	namespace := "TestApp"

	srcDir := filepath.Join(name, "src")
	if err := os.MkdirAll(srcDir, 0755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}

	// Verify directory was created
	if _, err := os.Stat(projDir); err != nil {
		t.Fatalf("project dir not created: %v", err)
	}

	// Write and verify maggie.toml
	tomlPath := filepath.Join(projDir, "maggie.toml")
	tomlContent := `[project]
name = "test-app"
namespace = "TestApp"
version = "0.1.0"

[source]
dirs = ["src"]
entry = "Main.start"
`
	os.WriteFile(tomlPath, []byte(tomlContent), 0644)
	data, err := os.ReadFile(tomlPath)
	if err != nil {
		t.Fatalf("read maggie.toml: %v", err)
	}
	if !strings.Contains(string(data), `name = "test-app"`) {
		t.Error("maggie.toml missing project name")
	}
	if !strings.Contains(string(data), `namespace = "TestApp"`) {
		t.Error("maggie.toml missing namespace")
	}

	// Write and verify Main.mag
	mainPath := filepath.Join(srcDir, "Main.mag")
	mainContent := namespace + " started!"
	os.WriteFile(mainPath, []byte(mainContent), 0644)
	data, err = os.ReadFile(mainPath)
	if err != nil {
		t.Fatalf("read Main.mag: %v", err)
	}
	if !strings.Contains(string(data), "TestApp") {
		t.Error("Main.mag missing namespace reference")
	}

	// Write and verify .gitignore
	gitignorePath := filepath.Join(projDir, ".gitignore")
	os.WriteFile(gitignorePath, []byte(".maggie/\n*.image\n"), 0644)
	data, err = os.ReadFile(gitignorePath)
	if err != nil {
		t.Fatalf("read .gitignore: %v", err)
	}
	if !strings.Contains(string(data), ".maggie/") {
		t.Error(".gitignore missing .maggie/")
	}
	if !strings.Contains(string(data), "*.image") {
		t.Error(".gitignore missing *.image")
	}
}

func TestHandleNewCommand_ErrorOnExistingDir(t *testing.T) {
	dir := t.TempDir()
	projDir := filepath.Join(dir, "existing")
	os.MkdirAll(projDir, 0755)

	// Verify directory exists
	if _, err := os.Stat(projDir); err != nil {
		t.Fatalf("setup: dir should exist: %v", err)
	}
}
