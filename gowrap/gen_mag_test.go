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

	files, err := GenerateMaggieStubs(model)
	if err != nil {
		t.Fatalf("GenerateMaggieStubs: %v", err)
	}

	// Check stubs.mag (package-level functions)
	stubsCode, ok := files["stubs.mag"]
	if !ok {
		t.Fatal("expected stubs.mag in output")
	}
	if !strings.Contains(stubsCode, "namespace: 'Go'") {
		t.Error("expected namespace 'Go' in stubs.mag")
	}
	if !strings.Contains(stubsCode, "Strings subclass: Object") {
		t.Error("expected Strings class in stubs.mag")
	}
	if !strings.Contains(stubsCode, "classMethod: contains:") {
		t.Error("expected contains: class method")
	}
	if !strings.Contains(stubsCode, "classMethod: hasPrefix:") {
		t.Error("expected hasPrefix: class method")
	}
	if !strings.Contains(stubsCode, "<primitive>") {
		t.Error("expected <primitive> annotations in stubs.mag")
	}

	// Check stubs_types.mag (struct types)
	typesCode, ok := files["stubs_types.mag"]
	if !ok {
		t.Fatal("expected stubs_types.mag in output")
	}
	if !strings.Contains(typesCode, "namespace: 'Go::Strings'") {
		t.Error("expected namespace 'Go::Strings' in stubs_types.mag")
	}
	if !strings.Contains(typesCode, "Builder subclass: Object") {
		t.Error("expected Builder class in stubs_types.mag")
	}
	if !strings.Contains(typesCode, "<primitive>") {
		t.Error("expected <primitive> annotations in stubs_types.mag")
	}

	// Golden file tests
	stubsGolden := filepath.Join("testdata", "strings_stubs.mag.golden")
	updateGolden(t, stubsGolden, stubsCode)
	compareGolden(t, stubsGolden, stubsCode)

	typesGolden := filepath.Join("testdata", "strings_stubs_types.mag.golden")
	updateGolden(t, typesGolden, typesCode)
	compareGolden(t, typesGolden, typesCode)
}

func TestGenerateMaggieStubs_EmptyModel(t *testing.T) {
	model := &PackageModel{
		ImportPath: "empty/pkg",
		Name:       "pkg",
	}

	files, err := GenerateMaggieStubs(model)
	if err != nil {
		t.Fatalf("GenerateMaggieStubs: %v", err)
	}

	stubsCode, ok := files["stubs.mag"]
	if !ok {
		t.Fatal("expected stubs.mag in output")
	}
	if !strings.Contains(stubsCode, "namespace: 'Go'") {
		t.Error("expected namespace declaration")
	}

	// No types → no stubs_types.mag
	if _, ok := files["stubs_types.mag"]; ok {
		t.Error("expected no stubs_types.mag for empty model")
	}
}

func TestGenerateMaggieStubs_WithMethods(t *testing.T) {
	model, err := IntrospectPackage("encoding/json", map[string]bool{
		"Decoder": true,
	})
	if err != nil {
		t.Fatalf("IntrospectPackage: %v", err)
	}

	files, err := GenerateMaggieStubs(model)
	if err != nil {
		t.Fatalf("GenerateMaggieStubs: %v", err)
	}

	typesCode, ok := files["stubs_types.mag"]
	if !ok {
		t.Fatal("expected stubs_types.mag in output")
	}
	if !strings.Contains(typesCode, "Decoder subclass: Object") {
		t.Error("expected Decoder class")
	}
	if !strings.Contains(typesCode, "method:") {
		t.Error("expected instance methods on Decoder")
	}
}

// TestGenerateMaggieStubs_ClassNamesMatchGoGlue verifies that the FQNs of
// struct classes in the Maggie stubs match the class names registered by the
// Go glue code. This is the core invariant that prevents the naming mismatch
// bug where Go glue registers "Go::Strings::Builder" but stubs define "Go::Builder".
func TestGenerateMaggieStubs_ClassNamesMatchGoGlue(t *testing.T) {
	model, err := IntrospectPackage("strings", map[string]bool{
		"Contains":  true,
		"HasPrefix": true,
		"Builder":   true,
	})
	if err != nil {
		t.Fatalf("IntrospectPackage: %v", err)
	}

	namespace := GoPackageToMaggieNamespace(model.ImportPath)

	// Compute the FQN that gen_go.go would use for each struct type
	goGlueNames := make(map[string]bool)
	for _, tp := range model.Types {
		if !tp.IsStruct {
			continue
		}
		fqn := GoNameToMaggieClassName(namespace, tp.Name)
		goGlueNames[fqn] = true
	}

	// Compute the FQN that the Maggie stubs would produce after namespace resolution
	files, err := GenerateMaggieStubs(model)
	if err != nil {
		t.Fatalf("GenerateMaggieStubs: %v", err)
	}

	stubFQNs := make(map[string]bool)

	// Parse stubs_types.mag to extract namespace and class names
	typesCode, ok := files["stubs_types.mag"]
	if !ok {
		if len(goGlueNames) > 0 {
			t.Fatal("Go glue has struct types but no stubs_types.mag was generated")
		}
		return
	}

	var stubNamespace string
	for _, line := range strings.Split(typesCode, "\n") {
		line = strings.TrimSpace(line)
		if strings.HasPrefix(line, "namespace: '") {
			stubNamespace = strings.TrimPrefix(line, "namespace: '")
			stubNamespace = strings.TrimSuffix(stubNamespace, "'")
		}
		if strings.Contains(line, "subclass: Object") {
			className := strings.TrimSpace(strings.Split(line, "subclass:")[0])
			fqn := stubNamespace + "::" + className
			stubFQNs[fqn] = true
		}
	}

	// Verify they match
	for fqn := range goGlueNames {
		if !stubFQNs[fqn] {
			t.Errorf("Go glue registers %q but no matching stub class found (stubs have: %v)", fqn, stubFQNs)
		}
	}
	for fqn := range stubFQNs {
		if !goGlueNames[fqn] {
			t.Errorf("stub defines %q but Go glue does not register it", fqn)
		}
	}
}
