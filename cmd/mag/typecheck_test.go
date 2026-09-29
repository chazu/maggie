package main

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/chazu/maggie/types"
	"github.com/chazu/maggie/vm"
)

// A class defined in a later file of the check set must be a known type in
// an earlier one: every file's classes are declared before any is checked.
func TestTypecheckFilesDeclaresAllClassesFirst(t *testing.T) {
	dir := t.TempDir()
	a := filepath.Join(dir, "a.mag")
	b := filepath.Join(dir, "b.mag")
	if err := os.WriteFile(a, []byte("User subclass: Object\n  method: p: x <Point9> ^<Point9> [ ^x ]\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(b, []byte("Point9 subclass: Object\n  method: me ^<Point9> [ ^self ]\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	checker := types.NewChecker(vm.NewVM())
	if parseErrors := typecheckFiles(checker, []string{a, b}); parseErrors != 0 {
		t.Fatalf("unexpected parse errors: %d", parseErrors)
	}
	if len(checker.Diagnostics) != 0 {
		t.Errorf("expected no diagnostics, got %v", checker.Diagnostics)
	}
}
