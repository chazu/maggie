package gowrap

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestGenerateGoGlue_Strings(t *testing.T) {
	model, err := IntrospectPackage("strings", map[string]bool{
		"Contains":  true,
		"HasPrefix": true,
		"Builder":   true,
	})
	if err != nil {
		t.Fatalf("IntrospectPackage: %v", err)
	}

	code, err := GenerateGoGlue(model)
	if err != nil {
		t.Fatalf("GenerateGoGlue: %v", err)
	}

	// Basic sanity checks
	if !strings.Contains(code, "package wrap_strings") {
		t.Error("expected package declaration")
	}
	if !strings.Contains(code, `pkg "strings"`) {
		t.Error("expected strings import")
	}
	if !strings.Contains(code, "RegisterPrimitives") {
		t.Error("expected RegisterPrimitives function")
	}
	if !strings.Contains(code, `"contains:substr:"`) {
		t.Error("expected contains:substr: selector")
	}
	if !strings.Contains(code, `"hasPrefix:prefix:"`) {
		t.Error("expected hasPrefix:prefix: selector")
	}
	if !strings.Contains(code, "RegisterGoType") {
		t.Error("expected RegisterGoType call for Builder")
	}

	// Golden file test
	goldenFile := filepath.Join("testdata", "strings_wrap.go.golden")
	updateGolden(t, goldenFile, code)
	compareGolden(t, goldenFile, code)
}

func TestGenerateGoGlue_ErrorHandling(t *testing.T) {
	model, err := IntrospectPackage("encoding/json", map[string]bool{
		"Marshal": true,
	})
	if err != nil {
		t.Fatalf("IntrospectPackage: %v", err)
	}

	code, err := GenerateGoGlue(model)
	if err != nil {
		t.Fatalf("GenerateGoGlue: %v", err)
	}

	// Should use Failure results, not panic
	if strings.Contains(code, "panic(") {
		t.Error("generated code should NOT contain panic — errors must return Failure results")
	}
	if !strings.Contains(code, "NewFailureResult") {
		t.Error("expected NewFailureResult for error-returning function")
	}
	if !strings.Contains(code, "NewSuccessResult") {
		t.Error("expected NewSuccessResult for error-returning function's success path")
	}
}

func TestGenerateGoGlue_ErrorOnlyReturn(t *testing.T) {
	// Test functions that return only an error (no value), e.g., func Close() error
	model, err := IntrospectPackage("strings", map[string]bool{
		"Builder": true,
	})
	if err != nil {
		t.Fatalf("IntrospectPackage: %v", err)
	}

	code, err := GenerateGoGlue(model)
	if err != nil {
		t.Fatalf("GenerateGoGlue: %v", err)
	}

	// Builder.WriteByte returns (error) — should use Failure result, not panic
	if strings.Contains(code, "panic(") {
		t.Error("generated code should NOT contain panic — errors must return Failure results")
	}

	// Verify both error-only and value+error patterns use the Result pattern
	if !strings.Contains(code, "v.NewFailureResult(err.Error())") {
		t.Error("expected v.NewFailureResult(err.Error()) for error handling")
	}
	if !strings.Contains(code, "v.NewSuccessResult(") {
		t.Error("expected v.NewSuccessResult for success path")
	}
}

func TestGenerateGoGlue_EmptyModel(t *testing.T) {
	model := &PackageModel{
		ImportPath: "empty/pkg",
		Name:       "pkg",
	}

	code, err := GenerateGoGlue(model)
	if err != nil {
		t.Fatalf("GenerateGoGlue: %v", err)
	}

	if !strings.Contains(code, "RegisterPrimitives") {
		t.Error("expected RegisterPrimitives even for empty package")
	}
}

// Golden file helpers

func updateGolden(t *testing.T, path, content string) {
	t.Helper()
	if os.Getenv("UPDATE_GOLDEN") == "" {
		return
	}
	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("creating testdata dir: %v", err)
	}
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("updating golden file: %v", err)
	}
}

func compareGolden(t *testing.T, path, got string) {
	t.Helper()
	expected, err := os.ReadFile(path)
	if os.IsNotExist(err) {
		t.Logf("Golden file %s does not exist. Run with UPDATE_GOLDEN=1 to create.", path)
		return
	}
	if err != nil {
		t.Fatalf("reading golden file: %v", err)
	}
	if string(expected) != got {
		t.Errorf("output differs from golden file %s.\nRun with UPDATE_GOLDEN=1 to update.", path)
	}
}
