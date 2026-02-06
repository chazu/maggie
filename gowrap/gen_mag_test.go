package gowrap

import (
	"path/filepath"
	"strings"
	"testing"
)

func TestGenerateMaggieStubs_Strings(t *testing.T) {
	model, err := IntrospectPackage("strings", map[string]bool{
		"Contains":  true,
		"HasPrefix": true,
		"Builder":   true,
	})
	if err != nil {
		t.Fatalf("IntrospectPackage: %v", err)
	}

	code, err := GenerateMaggieStubs(model)
	if err != nil {
		t.Fatalf("GenerateMaggieStubs: %v", err)
	}

	if !strings.Contains(code, "namespace: 'Go'") {
		t.Error("expected namespace declaration")
	}
	if !strings.Contains(code, "Strings subclass: Object") {
		t.Error("expected Strings class")
	}
	if !strings.Contains(code, "classMethod: contains:") {
		t.Error("expected contains: class method")
	}
	if !strings.Contains(code, "classMethod: hasPrefix:") {
		t.Error("expected hasPrefix: class method")
	}
	if !strings.Contains(code, "Builder subclass: Object") {
		t.Error("expected Builder class")
	}
	if !strings.Contains(code, "<primitive>") {
		t.Error("expected <primitive> annotations")
	}

	// Golden file test
	goldenFile := filepath.Join("testdata", "strings_stubs.mag.golden")
	updateGolden(t, goldenFile, code)
	compareGolden(t, goldenFile, code)
}

func TestGenerateMaggieStubs_EmptyModel(t *testing.T) {
	model := &PackageModel{
		ImportPath: "empty/pkg",
		Name:       "pkg",
	}

	code, err := GenerateMaggieStubs(model)
	if err != nil {
		t.Fatalf("GenerateMaggieStubs: %v", err)
	}

	if !strings.Contains(code, "namespace: 'Go'") {
		t.Error("expected namespace declaration")
	}
}

func TestGenerateMaggieStubs_WithMethods(t *testing.T) {
	model, err := IntrospectPackage("encoding/json", map[string]bool{
		"Decoder": true,
	})
	if err != nil {
		t.Fatalf("IntrospectPackage: %v", err)
	}

	code, err := GenerateMaggieStubs(model)
	if err != nil {
		t.Fatalf("GenerateMaggieStubs: %v", err)
	}

	if !strings.Contains(code, "Decoder subclass: Object") {
		t.Error("expected Decoder class")
	}
	if !strings.Contains(code, "method:") {
		t.Error("expected instance methods on Decoder")
	}
}
