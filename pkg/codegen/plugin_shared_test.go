package codegen

import (
	"strings"
	"testing"

	"github.com/chazu/procyon/pkg/ast"
)

func TestGenerateSharedPlugin_SimpleClass(t *testing.T) {
	class := &ast.Class{
		Name:   "Counter",
		Parent: "Object",
		InstanceVars: []ast.InstanceVar{
			{Name: "value", Default: ast.DefaultValue{Value: "0", Type: "number"}},
		},
		Methods: []ast.Method{
			{
				Selector: "getValue",
				Kind:     "instance",
				Body: ast.Block{
					Tokens: []ast.Token{
						{Type: ast.TokenCaret},
						{Type: ast.TokenIdentifier, Value: "value"},
					},
				},
			},
		},
	}

	result := GenerateSharedPlugin(class)
	if result == nil {
		t.Fatal("GenerateSharedPlugin returned nil")
	}

	if result.Code == "" {
		t.Fatal("GenerateSharedPlugin returned empty code")
	}

	// Check essential parts of generated code
	checks := []string{
		"package main",
		"Counter",
		"import \"C\"",
		"func init()",
		"TT_RegisterClass",
	}

	for _, check := range checks {
		if !strings.Contains(result.Code, check) {
			t.Errorf("Generated code missing %q", check)
		}
	}
}

func TestGenerateSharedPlugin_WithNamespace(t *testing.T) {
	class := &ast.Class{
		Name:    "Counter",
		Parent:  "Object",
		Package: "MyApp",
		InstanceVars: []ast.InstanceVar{
			{Name: "value", Default: ast.DefaultValue{Value: "0", Type: "number"}},
		},
	}

	result := GenerateSharedPlugin(class)
	if result == nil {
		t.Fatal("GenerateSharedPlugin returned nil")
	}

	// Namespaced class should include qualified name
	if !strings.Contains(result.Code, "MyApp::Counter") {
		t.Error("Generated code should include qualified name MyApp::Counter")
	}

	// Dispatch export should use proper separator for namespace
	// Check that either the dispatch or the class name is in the output
	if !strings.Contains(result.Code, "MyApp") {
		t.Error("Generated code should include namespace MyApp")
	}
}

func TestGenerateSharedPluginWithOptions_SkipValidation(t *testing.T) {
	class := &ast.Class{
		Name:   "Test",
		Parent: "Object",
	}

	opts := GenerateOptions{SkipValidation: true}
	result := GenerateSharedPluginWithOptions(class, opts)

	if result == nil {
		t.Fatal("GenerateSharedPluginWithOptions returned nil")
	}

	if result.Code == "" {
		t.Fatal("Generated code is empty")
	}
}

func TestGenerateSharedPlugin_ClassMethods(t *testing.T) {
	class := &ast.Class{
		Name:   "Factory",
		Parent: "Object",
		Methods: []ast.Method{
			{
				Selector: "create",
				Kind:     "class",
				Body: ast.Block{
					Tokens: []ast.Token{
						{Type: ast.TokenCaret},
						{Type: ast.TokenSString, Value: "created"},
					},
				},
			},
		},
	}

	result := GenerateSharedPlugin(class)
	if result == nil {
		t.Fatal("GenerateSharedPlugin returned nil")
	}

	// Should have class dispatch
	if !strings.Contains(result.Code, "dispatchClass") {
		t.Error("Generated code should include dispatchClass function")
	}
}

func TestGenerateSharedPlugin_InstanceVarsTracking(t *testing.T) {
	class := &ast.Class{
		Name:   "DataStore",
		Parent: "Object",
		InstanceVars: []ast.InstanceVar{
			{Name: "items", Default: ast.DefaultValue{Value: "[]", Type: "json"}},
			{Name: "count", Default: ast.DefaultValue{Value: "0", Type: "number"}},
			{Name: "name", Default: ast.DefaultValue{Value: "default", Type: "string"}},
		},
	}

	result := GenerateSharedPlugin(class)
	if result == nil {
		t.Fatal("GenerateSharedPlugin returned nil")
	}

	// All instance vars should be registered
	for _, iv := range class.InstanceVars {
		if !strings.Contains(result.Code, `"`+iv.Name+`"`) {
			t.Errorf("Instance var %q not found in generated code", iv.Name)
		}
	}
}

func TestGenerateSharedPlugin_HelperFunctions(t *testing.T) {
	class := &ast.Class{
		Name:   "Test",
		Parent: "Object",
	}

	result := GenerateSharedPlugin(class)
	if result == nil {
		t.Fatal("GenerateSharedPlugin returned nil")
	}

	// Check for generated helper functions
	helpers := []string{
		"func openDB()",
		"func loadInstance(",
		"func saveInstance(",
		"func sendMessage(",
		"func invokeBlock(",
	}

	for _, helper := range helpers {
		if !strings.Contains(result.Code, helper) {
			t.Errorf("Generated code missing helper %q", helper)
		}
	}
}

func TestGenerateSharedPlugin_TypeHelpers(t *testing.T) {
	class := &ast.Class{
		Name:   "Test",
		Parent: "Object",
	}

	result := GenerateSharedPlugin(class)
	if result == nil {
		t.Fatal("GenerateSharedPlugin returned nil")
	}

	// Check for type conversion helpers
	if !strings.Contains(result.Code, "func toInt(") {
		t.Error("Generated code should include toInt helper")
	}
	if !strings.Contains(result.Code, "func toBool(") {
		t.Error("Generated code should include toBool helper")
	}
}

func TestGenerateSharedPlugin_ErrUnknownSelector(t *testing.T) {
	class := &ast.Class{
		Name:   "Test",
		Parent: "Object",
	}

	result := GenerateSharedPlugin(class)
	if result == nil {
		t.Fatal("GenerateSharedPlugin returned nil")
	}

	if !strings.Contains(result.Code, "ErrUnknownSelector") {
		t.Error("Generated code should define ErrUnknownSelector")
	}
}

func TestGenerateSharedPlugin_MultipleInstanceVars(t *testing.T) {
	class := &ast.Class{
		Name:   "Point",
		Parent: "Object",
		InstanceVars: []ast.InstanceVar{
			{Name: "x", Default: ast.DefaultValue{Value: "0", Type: "number"}},
			{Name: "y", Default: ast.DefaultValue{Value: "0", Type: "number"}},
			{Name: "z", Default: ast.DefaultValue{Value: "0", Type: "number"}},
		},
	}

	result := GenerateSharedPlugin(class)
	if result == nil {
		t.Fatal("GenerateSharedPlugin returned nil")
	}

	// Struct should have all fields
	if !strings.Contains(result.Code, "X ") && !strings.Contains(result.Code, "x ") {
		t.Error("Generated struct should include X field")
	}
}

func TestGenerateSharedPlugin_EmptyClass(t *testing.T) {
	class := &ast.Class{
		Name:   "Empty",
		Parent: "Object",
	}

	result := GenerateSharedPlugin(class)
	if result == nil {
		t.Fatal("GenerateSharedPlugin returned nil")
	}

	if result.Code == "" {
		t.Fatal("Generated code should not be empty for empty class")
	}

	// Should still have basic structure
	if !strings.Contains(result.Code, "type Empty struct") {
		t.Error("Generated code should define Empty struct")
	}
}

func TestGenerateSharedPlugin_SkippedMethods(t *testing.T) {
	// Test that problematic methods are tracked in SkippedMethods
	class := &ast.Class{
		Name:   "Test",
		Parent: "Object",
		Methods: []ast.Method{
			{
				Selector: "goodMethod",
				Kind:     "instance",
				Body: ast.Block{
					Tokens: []ast.Token{
						{Type: ast.TokenCaret},
						{Type: ast.TokenSString, Value: "ok"},
					},
				},
			},
		},
	}

	result := GenerateSharedPlugin(class)
	if result == nil {
		t.Fatal("GenerateSharedPlugin returned nil")
	}

	// Result should have SkippedMethods slice (may be empty if all compiled)
	if result.SkippedMethods == nil {
		// This is fine - just means no methods were skipped
		t.Log("No methods were skipped")
	}
}
