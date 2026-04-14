package hash

import (
	"crypto/sha256"
	"encoding/hex"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/chazu/maggie/compiler"
)

// parseMethod parses a standalone method definition.
func parseMethod(t *testing.T, source string) *compiler.MethodDef {
	t.Helper()
	p := compiler.NewParser(source)
	m := p.ParseMethod()
	if m == nil {
		t.Fatalf("expected method, got nil")
	}
	if errs := p.Errors(); len(errs) > 0 {
		t.Fatalf("parse errors: %v", errs)
	}
	return m
}

// TestTypedHashUntypedMethod verifies that for an untyped method, the typed
// hash differs from the semantic hash (due to the presence byte).
func TestTypedHashUntypedMethod(t *testing.T) {
	md := parseMethod(t, "answer [ ^42 ]")

	semanticHash := HashMethod(md, nil, nil)
	typedHash := HashTypedMethod(md, nil, nil)

	if semanticHash == typedHash {
		t.Errorf("typed hash should differ from semantic hash for untyped method (presence byte)")
	}

	// Both should be non-zero
	if semanticHash == [32]byte{} {
		t.Error("semantic hash should not be zero")
	}
	if typedHash == [32]byte{} {
		t.Error("typed hash should not be zero")
	}
}

// TestTypedHashSameCodeDifferentTypes verifies that two methods with the same
// code but different type annotations have the same semantic hash but different
// typed hashes.
func TestTypedHashSameCodeDifferentTypes(t *testing.T) {
	mdUntyped := parseMethod(t, "add: x to: y [ ^x + y ]")
	mdTyped := parseMethod(t, "add: x <Integer> to: y <Integer> ^<Integer> [ ^x + y ]")

	// Semantic hashes should be the same (same code)
	semUntyped := HashMethod(mdUntyped, nil, nil)
	semTyped := HashMethod(mdTyped, nil, nil)
	if semUntyped != semTyped {
		t.Errorf("semantic hashes should match for same code:\n  untyped: %x\n  typed:   %x",
			semUntyped, semTyped)
	}

	// Typed hashes should differ (different type annotations)
	typUntyped := HashTypedMethod(mdUntyped, nil, nil)
	typTyped := HashTypedMethod(mdTyped, nil, nil)
	if typUntyped == typTyped {
		t.Errorf("typed hashes should differ when type annotations differ:\n  untyped: %x\n  typed:   %x",
			typUntyped, typTyped)
	}
}

// TestTypedHashSameCodeSameTypes verifies that two methods with the same code
// and same type annotations produce the same typed hash.
func TestTypedHashSameCodeSameTypes(t *testing.T) {
	md1 := parseMethod(t, "add: x <Integer> to: y <Integer> ^<Integer> [ ^x + y ]")
	md2 := parseMethod(t, "add: a <Integer> to: b <Integer> ^<Integer> [ ^a + b ]")

	typ1 := HashTypedMethod(md1, nil, nil)
	typ2 := HashTypedMethod(md2, nil, nil)
	if typ1 != typ2 {
		t.Errorf("typed hashes should match for same code+types:\n  1: %x\n  2: %x",
			typ1, typ2)
	}
}

// TestTypedHashWithReturnTypeOnly verifies typed hash when only return type
// is annotated.
func TestTypedHashWithReturnTypeOnly(t *testing.T) {
	mdNo := parseMethod(t, "answer [ ^42 ]")
	mdWith := parseMethod(t, "answer ^<Integer> [ ^42 ]")

	typNo := HashTypedMethod(mdNo, nil, nil)
	typWith := HashTypedMethod(mdWith, nil, nil)
	if typNo == typWith {
		t.Errorf("typed hashes should differ when return type annotation differs")
	}
}

// TestTypedHashGoldenFiles verifies that typed hashes produce expected results.
func TestTypedHashGoldenFiles(t *testing.T) {
	cases := []struct {
		name string
		src  string
	}{
		{
			name: "typed_unary_no_types",
			src:  "answer [ ^42 ]",
		},
		{
			name: "typed_with_param_types",
			src:  "add: x <Integer> to: y <Integer> [ ^x + y ]",
		},
		{
			name: "typed_with_return_type",
			src:  "answer ^<Integer> [ ^42 ]",
		},
		{
			name: "typed_mixed",
			src:  "greet: name <String> loudly: flag ^<String> [ ^name ]",
		},
	}

	goldenDir := filepath.Join("testdata")
	if err := os.MkdirAll(goldenDir, 0o755); err != nil {
		t.Fatalf("create testdata dir: %v", err)
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			md := parseMethod(t, tc.src)

			hm := normalizeTypedMethod(md, nil, nil)
			data := serializeTyped(hm)
			h := sha256.Sum256(data)

			serializedHex := hex.EncodeToString(data)
			hashHex := hex.EncodeToString(h[:])

			goldenPath := filepath.Join(goldenDir, tc.name+".golden")
			expected, err := os.ReadFile(goldenPath)
			if err != nil {
				// First run: create golden file
				content := serializedHex + "\n" + hashHex + "\n"
				if writeErr := os.WriteFile(goldenPath, []byte(content), 0o644); writeErr != nil {
					t.Fatalf("write golden file: %v", writeErr)
				}
				t.Logf("created golden file: %s", goldenPath)
				return
			}

			lines := strings.Split(strings.TrimSpace(string(expected)), "\n")
			if len(lines) != 2 {
				t.Fatalf("golden file %s: expected 2 lines, got %d", goldenPath, len(lines))
			}

			if serializedHex != lines[0] {
				t.Errorf("serialized bytes mismatch:\n  got:  %s\n  want: %s", serializedHex, lines[0])
			}
			if hashHex != lines[1] {
				t.Errorf("hash mismatch:\n  got:  %s\n  want: %s", hashHex, lines[1])
			}
		})
	}
}

// TestNormalizeTypedMethodPopulatesTypes verifies that normalizeTypedMethod
// correctly populates type annotation fields from the AST.
func TestNormalizeTypedMethodPopulatesTypes(t *testing.T) {
	md := parseMethod(t, "add: x <Integer> to: y <Object> ^<Number> [ ^x + y ]")

	hm := normalizeTypedMethod(md, nil, nil)

	if len(hm.ParamTypes) != 2 {
		t.Fatalf("expected 2 param types, got %d", len(hm.ParamTypes))
	}
	if hm.ParamTypes[0].Name != "Integer" {
		t.Errorf("param 0 type = %q, want 'Integer'", hm.ParamTypes[0].Name)
	}
	if hm.ParamTypes[1].Name != "Object" {
		t.Errorf("param 1 type = %q, want 'Object'", hm.ParamTypes[1].Name)
	}
	if hm.ReturnType.Name != "Number" {
		t.Errorf("return type = %q, want 'Number'", hm.ReturnType.Name)
	}
}

// TestTypedHashWithResolveGlobal verifies FQN resolution for type names.
func TestTypedHashWithResolveGlobal(t *testing.T) {
	md := parseMethod(t, "process: val <Button> ^<Widget> [ ^val ]")

	resolve := func(name string) string {
		if name == "Button" {
			return "Widgets::Button"
		}
		if name == "Widget" {
			return "Widgets::Widget"
		}
		return name
	}

	hm := normalizeTypedMethod(md, nil, resolve)
	if hm.ParamTypes[0].Name != "Widgets::Button" {
		t.Errorf("param type = %q, want 'Widgets::Button'", hm.ParamTypes[0].Name)
	}
	if hm.ReturnType.Name != "Widgets::Widget" {
		t.Errorf("return type = %q, want 'Widgets::Widget'", hm.ReturnType.Name)
	}

	// Verify different resolution produces different typed hash
	hashResolved := HashTypedMethod(md, nil, resolve)
	hashUnresolved := HashTypedMethod(md, nil, nil)
	if hashResolved == hashUnresolved {
		t.Error("resolved and unresolved typed hashes should differ")
	}
}
