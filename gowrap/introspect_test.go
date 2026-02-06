package gowrap

import (
	"testing"
)

func TestIntrospectPackage_Strings(t *testing.T) {
	model, err := IntrospectPackage("strings", nil)
	if err != nil {
		t.Fatalf("IntrospectPackage(strings): %v", err)
	}

	if model.ImportPath != "strings" {
		t.Errorf("expected import path 'strings', got %q", model.ImportPath)
	}
	if model.Name != "strings" {
		t.Errorf("expected package name 'strings', got %q", model.Name)
	}

	// Should have well-known functions
	foundContains := false
	foundReplace := false
	for _, fn := range model.Functions {
		switch fn.Name {
		case "Contains":
			foundContains = true
			if len(fn.Params) != 2 {
				t.Errorf("Contains: expected 2 params, got %d", len(fn.Params))
			}
			if len(fn.Results) != 1 {
				t.Errorf("Contains: expected 1 result, got %d", len(fn.Results))
			}
		case "Replace":
			foundReplace = true
		}
	}
	if !foundContains {
		t.Error("expected to find Contains function")
	}
	if !foundReplace {
		t.Error("expected to find Replace function")
	}

	// Should have types like Builder, Reader, Replacer
	foundBuilder := false
	for _, tp := range model.Types {
		if tp.Name == "Builder" {
			foundBuilder = true
			// Builder should have methods
			if len(tp.Methods) == 0 {
				t.Error("Builder: expected methods")
			}
		}
	}
	if !foundBuilder {
		t.Error("expected to find Builder type")
	}
}

func TestIntrospectPackage_WithFilter(t *testing.T) {
	filter := map[string]bool{
		"Contains": true,
		"HasPrefix": true,
	}
	model, err := IntrospectPackage("strings", filter)
	if err != nil {
		t.Fatalf("IntrospectPackage(strings, filter): %v", err)
	}

	if len(model.Functions) != 2 {
		t.Errorf("expected 2 functions with filter, got %d", len(model.Functions))
	}
	if len(model.Types) != 0 {
		t.Errorf("expected 0 types with filter, got %d", len(model.Types))
	}
}

func TestIntrospectPackage_EncodingJson(t *testing.T) {
	model, err := IntrospectPackage("encoding/json", nil)
	if err != nil {
		t.Fatalf("IntrospectPackage(encoding/json): %v", err)
	}

	if model.Name != "json" {
		t.Errorf("expected package name 'json', got %q", model.Name)
	}

	// Should have Marshal function
	foundMarshal := false
	for _, fn := range model.Functions {
		if fn.Name == "Marshal" {
			foundMarshal = true
			if !fn.ReturnsErr {
				t.Error("Marshal should return error")
			}
		}
	}
	if !foundMarshal {
		t.Error("expected to find Marshal function")
	}

	// Should have Decoder type with Decode method
	foundDecoder := false
	for _, tp := range model.Types {
		if tp.Name == "Decoder" {
			foundDecoder = true
			foundDecode := false
			for _, m := range tp.Methods {
				if m.Name == "Decode" {
					foundDecode = true
					if !m.ReturnsErr {
						t.Error("Decode should return error")
					}
				}
			}
			if !foundDecode {
				t.Error("expected Decoder to have Decode method")
			}
		}
	}
	if !foundDecoder {
		t.Error("expected to find Decoder type")
	}
}

func TestIntrospectPackage_BadPath(t *testing.T) {
	_, err := IntrospectPackage("nonexistent/package/path", nil)
	if err == nil {
		t.Error("expected error for nonexistent package")
	}
}

func TestIntrospectPackage_Constants(t *testing.T) {
	model, err := IntrospectPackage("math", nil)
	if err != nil {
		t.Fatalf("IntrospectPackage(math): %v", err)
	}

	foundPi := false
	for _, c := range model.Constants {
		if c.Name == "Pi" {
			foundPi = true
			if c.Value == "" {
				t.Error("Pi should have a value")
			}
		}
	}
	if !foundPi {
		t.Error("expected to find Pi constant")
	}
}
