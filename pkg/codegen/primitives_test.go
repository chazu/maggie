package codegen

import (
	"testing"
)

func TestHasPrimitiveImpl(t *testing.T) {
	tests := []struct {
		className string
		selector  string
		expected  bool
	}{
		// File class
		{"File", "at_", true},
		{"File", "read", true},
		{"File", "write_", true},
		{"File", "exists", true},
		{"File", "nonExistent", false},

		// Console class
		{"Console", "print_", true},
		{"Console", "write_", true},
		{"Console", "error_", true},
		{"Console", "newline", true},
		{"Console", "invalid", false},

		// Env class
		{"Env", "get_", true},
		{"Env", "set_to_", true},
		{"Env", "has_", true},
		{"Env", "invalid", false},

		// Unknown class
		{"UnknownClass", "anyMethod", false},

		// Block class
		{"Block", "params_code_captured_", true},
		{"Block", "numArgs", true},
		{"Block", "value", false}, // value requires eval - bash only

		// String class
		{"String", "length_", true},
		{"String", "isEmpty_", true},
		{"String", "uppercase_", true},

		// Shell class
		{"Shell", "exec_", true},
		{"Shell", "run_", true},
	}

	for _, tt := range tests {
		t.Run(tt.className+"_"+tt.selector, func(t *testing.T) {
			result := hasPrimitiveImpl(tt.className, tt.selector)
			if result != tt.expected {
				t.Errorf("hasPrimitiveImpl(%q, %q) = %v, want %v", tt.className, tt.selector, result, tt.expected)
			}
		})
	}
}

func TestPrimitiveRegistry(t *testing.T) {
	// Check that primitiveRegistry is populated
	if len(primitiveRegistry) == 0 {
		t.Fatal("primitiveRegistry is empty")
	}

	// Check that known classes exist
	classes := []string{"File", "Console", "Env", "Block", "String", "Shell"}
	for _, class := range classes {
		if _, ok := primitiveRegistry[class]; !ok {
			t.Errorf("primitiveRegistry missing class %q", class)
		}
	}

	// Check that File has expected methods
	fileMethods := primitiveRegistry["File"]
	expectedMethods := []string{"at_", "read", "write_", "exists", "delete"}
	for _, m := range expectedMethods {
		if !fileMethods[m] {
			t.Errorf("File class missing method %q", m)
		}
	}
}

func TestPrimitiveRegistryCompleteness(t *testing.T) {
	// File class should have many methods
	if len(primitiveRegistry["File"]) < 20 {
		t.Errorf("File class has fewer methods than expected: %d", len(primitiveRegistry["File"]))
	}

	// Console class should have print methods
	if len(primitiveRegistry["Console"]) < 3 {
		t.Errorf("Console class has fewer methods than expected: %d", len(primitiveRegistry["Console"]))
	}

	// String class should have string manipulation methods
	if len(primitiveRegistry["String"]) < 10 {
		t.Errorf("String class has fewer methods than expected: %d", len(primitiveRegistry["String"]))
	}
}

func TestValidatePrimitiveClassParity(t *testing.T) {
	t.Run("unknown class returns nil", func(t *testing.T) {
		result := ValidatePrimitiveClassParity("UnknownClass", []string{"foo", "bar"})
		if result != nil {
			t.Errorf("expected nil for unknown class, got %+v", result)
		}
	})

	t.Run("perfect match has no errors or warnings", func(t *testing.T) {
		// Console has: print_, write_, error_, newline
		selectors := []string{"print_", "write_", "error_", "newline"}
		result := ValidatePrimitiveClassParity("Console", selectors)
		if result == nil {
			t.Fatal("expected non-nil result for Console")
		}
		if len(result.OrphanedNative) > 0 {
			t.Errorf("unexpected orphaned native: %v", result.OrphanedNative)
		}
		if len(result.MissingNative) > 0 {
			t.Errorf("unexpected missing native: %v", result.MissingNative)
		}
	})

	t.Run("orphaned native detected", func(t *testing.T) {
		// Only provide some of Console's selectors
		selectors := []string{"print_", "write_"}
		result := ValidatePrimitiveClassParity("Console", selectors)
		if result == nil {
			t.Fatal("expected non-nil result")
		}
		if !result.HasErrors() {
			t.Error("expected HasErrors() to return true")
		}
		// error_ and newline should be orphaned
		if len(result.OrphanedNative) != 2 {
			t.Errorf("expected 2 orphaned native, got %d: %v", len(result.OrphanedNative), result.OrphanedNative)
		}
	})

	t.Run("missing native detected as warning", func(t *testing.T) {
		// Provide all Console selectors plus an extra one
		selectors := []string{"print_", "write_", "error_", "newline", "extraMethod"}
		result := ValidatePrimitiveClassParity("Console", selectors)
		if result == nil {
			t.Fatal("expected non-nil result")
		}
		if result.HasErrors() {
			t.Error("expected no errors for missing native")
		}
		// extraMethod should be missing native
		if len(result.MissingNative) != 1 || result.MissingNative[0] != "extraMethod" {
			t.Errorf("expected ['extraMethod'] missing native, got %v", result.MissingNative)
		}
	})

	t.Run("both orphaned and missing", func(t *testing.T) {
		// Remove one Console method and add an extra
		selectors := []string{"print_", "write_", "error_", "customMethod"}
		result := ValidatePrimitiveClassParity("Console", selectors)
		if result == nil {
			t.Fatal("expected non-nil result")
		}
		if !result.HasErrors() {
			t.Error("expected errors for orphaned native")
		}
		// newline is orphaned
		hasNewline := false
		for _, s := range result.OrphanedNative {
			if s == "newline" {
				hasNewline = true
			}
		}
		if !hasNewline {
			t.Errorf("expected 'newline' in orphaned, got %v", result.OrphanedNative)
		}
		// customMethod is missing native
		hasCustom := false
		for _, s := range result.MissingNative {
			if s == "customMethod" {
				hasCustom = true
			}
		}
		if !hasCustom {
			t.Errorf("expected 'customMethod' in missing, got %v", result.MissingNative)
		}
	})
}
