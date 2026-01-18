package codegen

import (
	"testing"

	"github.com/chazu/procyon/pkg/ast"
	"github.com/chazu/procyon/pkg/parser"
)

func TestIsNumericString(t *testing.T) {
	tests := []struct {
		input    string
		expected bool
	}{
		{"0", true},
		{"123", true},
		{"-42", true},
		{"", false},
		{"abc", false},
		{"12.34", false},
		{"12abc", false},
		{"-", true},  // A single minus is technically accepted
		{"--5", false},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			result := isNumericString(tt.input)
			if result != tt.expected {
				t.Errorf("isNumericString(%q) = %v, want %v", tt.input, result, tt.expected)
			}
		})
	}
}

func TestGeneratorInferType(t *testing.T) {
	g := &generator{
		class: &ast.Class{Name: "Test"},
	}

	tests := []struct {
		name     string
		iv       ast.InstanceVar
		wantType string
	}{
		{
			name:     "number type",
			iv:       ast.InstanceVar{Name: "count", Default: ast.DefaultValue{Type: "number", Value: "0"}},
			wantType: "int",
		},
		{
			name:     "string type",
			iv:       ast.InstanceVar{Name: "name", Default: ast.DefaultValue{Type: "string", Value: "test"}},
			wantType: "string",
		},
		{
			name:     "numeric fallback",
			iv:       ast.InstanceVar{Name: "value", Default: ast.DefaultValue{Value: "42"}},
			wantType: "int",
		},
		{
			name:     "json default",
			iv:       ast.InstanceVar{Name: "items", Default: ast.DefaultValue{Value: "[]"}},
			wantType: "json.RawMessage",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := g.inferType(tt.iv)
			if result == nil {
				t.Fatal("inferType returned nil")
			}
			// The result is a jen statement, which we can't easily inspect directly
			// Just verify it doesn't panic and returns something
		})
	}
}

func TestGeneratorIsJSONArrayType(t *testing.T) {
	g := &generator{
		class: &ast.Class{Name: "Test"},
	}

	// Currently always returns false
	if g.isJSONArrayType("items") {
		t.Error("isJSONArrayType should return false")
	}
}

func TestGeneratorIsJSONObjectType(t *testing.T) {
	g := &generator{
		class: &ast.Class{Name: "Test"},
	}

	// Currently always returns false
	if g.isJSONObjectType("data") {
		t.Error("isJSONObjectType should return false")
	}
}

func TestGeneratorExprResultsInArray(t *testing.T) {
	g := &generator{
		class:    &ast.Class{Name: "Test"},
		jsonVars: map[string]bool{},
	}

	tests := []struct {
		name     string
		expr     parser.Expr
		expected bool
	}{
		{
			name:     "identifier",
			expr:     &parser.Identifier{Name: "items"},
			expected: false,
		},
		{
			name:     "number literal",
			expr:     &parser.NumberLit{Value: "42"},
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := g.exprResultsInArray(tt.expr)
			if result != tt.expected {
				t.Errorf("exprResultsInArray = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestGeneratorExprResultsInObject(t *testing.T) {
	g := &generator{
		class:    &ast.Class{Name: "Test"},
		jsonVars: map[string]bool{},
	}

	tests := []struct {
		name     string
		expr     parser.Expr
		expected bool
	}{
		{
			name:     "identifier",
			expr:     &parser.Identifier{Name: "data"},
			expected: false,
		},
		{
			name:     "string literal",
			expr:     &parser.StringLit{Value: "test"},
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := g.exprResultsInObject(tt.expr)
			if result != tt.expected {
				t.Errorf("exprResultsInObject = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestSelectorToGoName(t *testing.T) {
	tests := []struct {
		selector string
		expected string
	}{
		{"getValue", "GetValue"},
		{"setValue:", "SetValue:"},   // Preserves colon
		{"at:put:", "At:put:"},       // Just capitalizes first letter
		{"increment", "Increment"},
		{"reset", "Reset"},
	}

	for _, tt := range tests {
		t.Run(tt.selector, func(t *testing.T) {
			result := selectorToGoName(tt.selector)
			if result != tt.expected {
				t.Errorf("selectorToGoName(%q) = %q, want %q", tt.selector, result, tt.expected)
			}
		})
	}
}

func TestCapitalize(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"value", "Value"},
		{"getValue", "GetValue"},
		{"x", "X"},
		{"", ""},
		{"X", "X"},
		{"ABC", "ABC"},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			result := capitalize(tt.input)
			if result != tt.expected {
				t.Errorf("capitalize(%q) = %q, want %q", tt.input, result, tt.expected)
			}
		})
	}
}

func TestResult_Struct(t *testing.T) {
	result := Result{
		Code:     "package main",
		Warnings: []string{"warning 1"},
		SkippedMethods: []SkippedMethod{
			{Selector: "test", Reason: "reason"},
		},
	}

	if result.Code == "" {
		t.Error("Code should not be empty")
	}
	if len(result.Warnings) != 1 {
		t.Errorf("Expected 1 warning, got %d", len(result.Warnings))
	}
	if len(result.SkippedMethods) != 1 {
		t.Errorf("Expected 1 skipped method, got %d", len(result.SkippedMethods))
	}
}

func TestSkippedMethod_Struct(t *testing.T) {
	sm := SkippedMethod{
		Selector: "badMethod:",
		Reason:   "unsupported construct",
	}

	if sm.Selector != "badMethod:" {
		t.Errorf("Selector = %q, want %q", sm.Selector, "badMethod:")
	}
	if sm.Reason != "unsupported construct" {
		t.Errorf("Reason = %q, want %q", sm.Reason, "unsupported construct")
	}
}

func TestGenerateOptions_Struct(t *testing.T) {
	opts := GenerateOptions{
		SkipValidation: true,
	}

	if !opts.SkipValidation {
		t.Error("SkipValidation should be true")
	}

	opts2 := GenerateOptions{}
	if opts2.SkipValidation {
		t.Error("Default SkipValidation should be false")
	}
}

func TestGoPredicateTest(t *testing.T) {
	// Test that all expected predicates are recognized
	expectedPredicates := []string{
		"isEmpty",
		"notEmpty",
		"fileExists",
		"isFile",
		"isDirectory",
		"isSymlink",
		"isReadable",
		"isWritable",
		"isExecutable",
	}

	for _, pred := range expectedPredicates {
		t.Run(pred, func(t *testing.T) {
			gen, ok := goPredicateTest(pred)
			if !ok {
				t.Errorf("goPredicateTest(%q) should return true, got false", pred)
			}
			if gen == nil {
				t.Errorf("goPredicateTest(%q) returned nil generator", pred)
			}
		})
	}

	// Test that non-predicates return false
	nonPredicates := []string{
		"getValue",
		"setValue:",
		"send",
		"at:put:",
		"increment",
	}

	for _, np := range nonPredicates {
		t.Run("not_"+np, func(t *testing.T) {
			_, ok := goPredicateTest(np)
			if ok {
				t.Errorf("goPredicateTest(%q) should return false for non-predicate", np)
			}
		})
	}
}
