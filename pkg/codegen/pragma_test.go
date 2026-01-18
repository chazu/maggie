package codegen

import (
	"strings"
	"testing"

	"github.com/chazu/procyon/pkg/ast"
)

// TestBashOnlyPragmaSkipsNativeGeneration verifies that methods with bashOnly pragma
// are not compiled to native code - they should fall back to Bash via exit 200.
func TestBashOnlyPragmaSkipsNativeGeneration(t *testing.T) {
	class := &ast.Class{
		Type: "class",
		Name: "TestClass",
		Methods: []ast.Method{
			{
				Type:     "method",
				Kind:     "instance",
				Selector: "normalMethod",
				Args:     []string{},
				Pragmas:  []string{},
				Body: ast.Block{
					Tokens: []ast.Token{
						{Type: "CARET", Value: "^"},
						{Type: "NUMBER", Value: "42"},
					},
				},
			},
			{
				Type:     "method",
				Kind:     "instance",
				Selector: "bashOnlyMethod",
				Args:     []string{},
				Pragmas:  []string{"bashOnly"},
				Body: ast.Block{
					Tokens: []ast.Token{
						{Type: "CARET", Value: "^"},
						{Type: "NUMBER", Value: "99"},
					},
				},
			},
		},
	}

	result := GenerateSharedPluginWithOptions(class, GenerateOptions{SkipValidation: true})

	// normalMethod should be generated
	if !strings.Contains(result.Code, "NormalMethod") {
		t.Error("normalMethod should be generated in native code")
	}

	// bashOnlyMethod should NOT appear in the generated code
	if strings.Contains(result.Code, "BashOnlyMethod") {
		t.Error("bashOnlyMethod should NOT be generated in native code")
	}

	// bashOnlyMethod should be in the skipped list
	found := false
	for _, skipped := range result.SkippedMethods {
		if skipped.Selector == "bashOnlyMethod" && skipped.Reason == "bashOnly pragma" {
			found = true
			break
		}
	}
	if !found {
		t.Error("bashOnlyMethod should be in skipped methods with reason 'bashOnly pragma'")
	}
}

// TestProcyonOnlyPragmaEnablesRawMethodCompilation verifies that raw methods
// with procyonOnly pragma ARE compiled (normally raw methods are skipped).
func TestProcyonOnlyPragmaEnablesRawMethodCompilation(t *testing.T) {
	class := &ast.Class{
		Type: "class",
		Name: "TestClass",
		Methods: []ast.Method{
			{
				Type:     "method",
				Kind:     "instance",
				Selector: "rawMethodNoPragma",
				Args:     []string{},
				Raw:      true,
				Pragmas:  []string{},
				Body: ast.Block{
					Tokens: []ast.Token{
						{Type: "CARET", Value: "^"},
						{Type: "STRING", Value: "raw"},
					},
				},
			},
			{
				Type:     "method",
				Kind:     "instance",
				Selector: "rawMethodWithProcyonOnly",
				Args:     []string{},
				Raw:      true,
				Pragmas:  []string{"procyonOnly"},
				Body: ast.Block{
					Tokens: []ast.Token{
						{Type: "CARET", Value: "^"},
						{Type: "STRING", Value: "native"},
					},
				},
			},
		},
	}

	result := GenerateSharedPluginWithOptions(class, GenerateOptions{SkipValidation: true})

	// Raw method without pragma should be skipped
	rawSkipped := false
	for _, skipped := range result.SkippedMethods {
		if skipped.Selector == "rawMethodNoPragma" {
			rawSkipped = true
			break
		}
	}
	if !rawSkipped {
		t.Error("rawMethodNoPragma should be skipped")
	}

	// Raw method with procyonOnly should be compiled
	if !strings.Contains(result.Code, "RawMethodWithProcyonOnly") {
		t.Error("rawMethodWithProcyonOnly should be generated in native code")
	}
}

// TestProcyonNativePragmaEnablesRawMethodCompilation verifies that raw methods
// with procyonNative pragma ARE compiled (similar to procyonOnly).
func TestProcyonNativePragmaEnablesRawMethodCompilation(t *testing.T) {
	class := &ast.Class{
		Type: "class",
		Name: "TestClass",
		Methods: []ast.Method{
			{
				Type:     "method",
				Kind:     "instance",
				Selector: "rawMethodWithProcyonNative",
				Args:     []string{},
				Raw:      true,
				Pragmas:  []string{"procyonNative"},
				Body: ast.Block{
					Tokens: []ast.Token{
						{Type: "CARET", Value: "^"},
						{Type: "STRING", Value: "native"},
					},
				},
			},
		},
	}

	result := GenerateSharedPluginWithOptions(class, GenerateOptions{SkipValidation: true})

	// Raw method with procyonNative should be compiled
	if !strings.Contains(result.Code, "RawMethodWithProcyonNative") {
		t.Error("rawMethodWithProcyonNative should be generated in native code")
	}
}

// TestMethodHasPragma tests the HasPragma method on ast.Method.
func TestMethodHasPragma(t *testing.T) {
	tests := []struct {
		name     string
		pragmas  []string
		check    string
		expected bool
	}{
		{"no pragmas, check bashOnly", []string{}, "bashOnly", false},
		{"has bashOnly, check bashOnly", []string{"bashOnly"}, "bashOnly", true},
		{"has procyonOnly, check bashOnly", []string{"procyonOnly"}, "bashOnly", false},
		{"multiple pragmas, check present", []string{"direct", "procyonOnly"}, "procyonOnly", true},
		{"multiple pragmas, check absent", []string{"direct", "procyonOnly"}, "bashOnly", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			m := ast.Method{Pragmas: tt.pragmas}
			result := m.HasPragma(tt.check)
			if result != tt.expected {
				t.Errorf("HasPragma(%q) = %v, want %v", tt.check, result, tt.expected)
			}
		})
	}
}

// TestBashOnlyWithOtherPragmas verifies bashOnly takes precedence over other pragmas.
func TestBashOnlyWithOtherPragmas(t *testing.T) {
	class := &ast.Class{
		Type: "class",
		Name: "TestClass",
		Methods: []ast.Method{
			{
				Type:     "method",
				Kind:     "instance",
				Selector: "conflictingPragmas",
				Args:     []string{},
				// Both bashOnly and procyonOnly - bashOnly should win
				Pragmas: []string{"bashOnly", "procyonOnly"},
				Body: ast.Block{
					Tokens: []ast.Token{
						{Type: "CARET", Value: "^"},
						{Type: "NUMBER", Value: "1"},
					},
				},
			},
		},
	}

	result := GenerateSharedPluginWithOptions(class, GenerateOptions{SkipValidation: true})

	// bashOnly should take precedence - method should NOT be compiled
	if strings.Contains(result.Code, "ConflictingPragmas") {
		t.Error("bashOnly should take precedence - method should not be compiled")
	}

	// Should be in skipped list with bashOnly reason
	found := false
	for _, skipped := range result.SkippedMethods {
		if skipped.Selector == "conflictingPragmas" && skipped.Reason == "bashOnly pragma" {
			found = true
			break
		}
	}
	if !found {
		t.Error("conflictingPragmas should be skipped with reason 'bashOnly pragma'")
	}
}

// TestDirectPragmaDoesNotAffectCompilation verifies 'direct' pragma doesn't affect
// whether a method is compiled (it affects calling convention, not compilation).
func TestDirectPragmaDoesNotAffectCompilation(t *testing.T) {
	class := &ast.Class{
		Type: "class",
		Name: "TestClass",
		Methods: []ast.Method{
			{
				Type:     "method",
				Kind:     "instance",
				Selector: "directMethod",
				Args:     []string{},
				Pragmas:  []string{"direct"},
				Body: ast.Block{
					Tokens: []ast.Token{
						{Type: "CARET", Value: "^"},
						{Type: "NUMBER", Value: "42"},
					},
				},
			},
		},
	}

	result := GenerateSharedPluginWithOptions(class, GenerateOptions{SkipValidation: true})

	// Method with direct pragma should still be compiled
	if !strings.Contains(result.Code, "DirectMethod") {
		t.Error("directMethod should be generated in native code")
	}
}

// TestSkippedMethodsIncludeSelectorAndReason verifies the SkippedMethod struct
// properly captures both selector and reason.
func TestSkippedMethodsIncludeSelectorAndReason(t *testing.T) {
	class := &ast.Class{
		Type: "class",
		Name: "TestClass",
		Methods: []ast.Method{
			{
				Type:     "method",
				Kind:     "instance",
				Selector: "bashOnly:",
				Args:     []string{"arg"},
				Pragmas:  []string{"bashOnly"},
				Body: ast.Block{
					Tokens: []ast.Token{
						{Type: "CARET", Value: "^"},
						{Type: "IDENTIFIER", Value: "arg"},
					},
				},
			},
			{
				Type:     "method",
				Kind:     "instance",
				Selector: "rawMethod",
				Args:     []string{},
				Raw:      true,
				Pragmas:  []string{},
				Body: ast.Block{
					Tokens: []ast.Token{
						{Type: "CARET", Value: "^"},
						{Type: "NUMBER", Value: "1"},
					},
				},
			},
		},
	}

	result := GenerateSharedPluginWithOptions(class, GenerateOptions{SkipValidation: true})

	// Check both methods are in skipped list with correct reasons
	foundBashOnly := false
	foundRaw := false

	for _, skipped := range result.SkippedMethods {
		if skipped.Selector == "bashOnly:" && skipped.Reason == "bashOnly pragma" {
			foundBashOnly = true
		}
		if skipped.Selector == "rawMethod" && skipped.Reason == "raw method" {
			foundRaw = true
		}
	}

	if !foundBashOnly {
		t.Error("bashOnly: should be skipped with reason 'bashOnly pragma'")
	}
	if !foundRaw {
		t.Error("rawMethod should be skipped with reason 'raw method'")
	}
}

// TestPreIdentifySkippedMethodsMarksCorrectMethods verifies the pre-identification
// pass correctly identifies all methods that will be skipped.
func TestPreIdentifySkippedMethodsMarksCorrectMethods(t *testing.T) {
	class := &ast.Class{
		Type: "class",
		Name: "TestClass",
		Methods: []ast.Method{
			{
				Type:     "method",
				Kind:     "instance",
				Selector: "normal",
				Args:     []string{},
				Body: ast.Block{
					Tokens: []ast.Token{
						{Type: "CARET", Value: "^"},
						{Type: "NUMBER", Value: "1"},
					},
				},
			},
			{
				Type:     "method",
				Kind:     "instance",
				Selector: "bashOnly",
				Args:     []string{},
				Pragmas:  []string{"bashOnly"},
				Body: ast.Block{
					Tokens: []ast.Token{
						{Type: "CARET", Value: "^"},
						{Type: "NUMBER", Value: "2"},
					},
				},
			},
		},
	}

	// The generator's pre-identification should mark bashOnly method
	g := &generator{
		class:          class,
		skippedMethods: make(map[string]string),
		jsonVars:       make(map[string]bool),
	}

	g.preIdentifySkippedMethods()

	if g.skippedMethods["normal"] != "" {
		t.Error("normal method should not be marked as skipped")
	}
	if g.skippedMethods["bashOnly"] == "" {
		t.Error("bashOnly method should be marked as skipped")
	}
	if g.skippedMethods["bashOnly"] != "bashOnly pragma" {
		t.Errorf("bashOnly method should have reason 'bashOnly pragma', got '%s'", g.skippedMethods["bashOnly"])
	}
}

// TestClassMethodWithBashOnlyPragma verifies class methods with bashOnly are also skipped.
func TestClassMethodWithBashOnlyPragma(t *testing.T) {
	class := &ast.Class{
		Type: "class",
		Name: "TestClass",
		Methods: []ast.Method{
			{
				Type:     "method",
				Kind:     "class",
				Selector: "bashOnlyClassMethod",
				Args:     []string{},
				Pragmas:  []string{"bashOnly"},
				Body: ast.Block{
					Tokens: []ast.Token{
						{Type: "CARET", Value: "^"},
						{Type: "STRING", Value: "bash only"},
					},
				},
			},
		},
	}

	result := GenerateSharedPluginWithOptions(class, GenerateOptions{SkipValidation: true})

	// Class method with bashOnly should NOT be compiled
	if strings.Contains(result.Code, "BashOnlyClassMethod") {
		t.Error("bashOnlyClassMethod should NOT be generated in native code")
	}

	// Should be in skipped list
	found := false
	for _, skipped := range result.SkippedMethods {
		if skipped.Selector == "bashOnlyClassMethod" {
			found = true
			break
		}
	}
	if !found {
		t.Error("bashOnlyClassMethod should be in skipped methods")
	}
}

// TestAllMethodsGeneratedWithNoPragmas verifies default behavior:
// methods without special pragmas are compiled normally.
func TestAllMethodsGeneratedWithNoPragmas(t *testing.T) {
	class := &ast.Class{
		Type: "class",
		Name: "Counter",
		InstanceVars: []ast.InstanceVar{
			{Name: "count", Default: ast.DefaultValue{Type: "number", Value: "0"}},
		},
		Methods: []ast.Method{
			{
				Type:     "method",
				Kind:     "instance",
				Selector: "increment",
				Args:     []string{},
				Body: ast.Block{
					Tokens: []ast.Token{
						{Type: "IDENTIFIER", Value: "count"},
						{Type: "ASSIGN", Value: ":="},
						{Type: "IDENTIFIER", Value: "count"},
						{Type: "PLUS", Value: "+"},
						{Type: "NUMBER", Value: "1"},
					},
				},
			},
			{
				Type:     "method",
				Kind:     "instance",
				Selector: "value",
				Args:     []string{},
				Body: ast.Block{
					Tokens: []ast.Token{
						{Type: "CARET", Value: "^"},
						{Type: "IDENTIFIER", Value: "count"},
					},
				},
			},
		},
	}

	result := GenerateSharedPluginWithOptions(class, GenerateOptions{SkipValidation: true})

	// Both methods should be generated
	if !strings.Contains(result.Code, "Increment") {
		t.Error("increment method should be generated")
	}
	if !strings.Contains(result.Code, "Value") {
		t.Error("value method should be generated")
	}

	// No methods should be skipped
	if len(result.SkippedMethods) > 0 {
		t.Errorf("Expected no skipped methods, got %d: %v", len(result.SkippedMethods), result.SkippedMethods)
	}
}

// TestKeywordMethodWithBashOnlyPragma verifies keyword methods (with colons) are handled.
func TestKeywordMethodWithBashOnlyPragma(t *testing.T) {
	class := &ast.Class{
		Type: "class",
		Name: "TestClass",
		Methods: []ast.Method{
			{
				Type:     "method",
				Kind:     "instance",
				Selector: "setValue:",
				Args:     []string{"val"},
				Pragmas:  []string{"bashOnly"},
				Body: ast.Block{
					Tokens: []ast.Token{
						{Type: "IDENTIFIER", Value: "value"},
						{Type: "ASSIGN", Value: ":="},
						{Type: "IDENTIFIER", Value: "val"},
					},
				},
			},
		},
	}

	result := GenerateSharedPluginWithOptions(class, GenerateOptions{SkipValidation: true})

	// Method should NOT be generated
	if strings.Contains(result.Code, "SetValue:") || strings.Contains(result.Code, "SetValue_") {
		t.Error("setValue: should NOT be generated in native code")
	}

	// Should be in skipped list with proper selector
	found := false
	for _, skipped := range result.SkippedMethods {
		if skipped.Selector == "setValue:" {
			found = true
			break
		}
	}
	if !found {
		t.Error("setValue: should be in skipped methods")
	}
}

// TestMultiplePragmasOnSingleMethod verifies methods can have multiple pragmas.
func TestMultiplePragmasOnSingleMethod(t *testing.T) {
	class := &ast.Class{
		Type: "class",
		Name: "TestClass",
		Methods: []ast.Method{
			{
				Type:     "method",
				Kind:     "instance",
				Selector: "multiPragma",
				Args:     []string{},
				Pragmas:  []string{"direct", "procyonOnly"},
				Raw:      true,
				Body: ast.Block{
					Tokens: []ast.Token{
						{Type: "CARET", Value: "^"},
						{Type: "NUMBER", Value: "42"},
					},
				},
			},
		},
	}

	result := GenerateSharedPluginWithOptions(class, GenerateOptions{SkipValidation: true})

	// Method should be compiled (procyonOnly overrides raw)
	if !strings.Contains(result.Code, "MultiPragma") {
		t.Error("multiPragma should be generated (procyonOnly enables compilation)")
	}
}
