package types

import (
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

func testVM() *vm.VM {
	return vm.NewVM()
}

func TestCheckKnownTypes(t *testing.T) {
	v := testVM()
	c := NewChecker(v)

	source := `MyClass subclass: Object
  method: foo: x <Integer> ^<String> [
    ^x printString
  ]
`
	p := compiler.NewParser(source)
	sf := p.ParseSourceFile()
	c.CheckSourceFile(sf)

	if len(c.Diagnostics) != 0 {
		t.Errorf("expected no diagnostics for known types, got %d:", len(c.Diagnostics))
		for _, d := range c.Diagnostics {
			t.Logf("  %s", d)
		}
	}
}

func TestCheckUnknownType(t *testing.T) {
	v := testVM()
	c := NewChecker(v)

	source := `MyClass subclass: Object
  method: foo: x <NonExistentType> [ ^x ]
`
	p := compiler.NewParser(source)
	sf := p.ParseSourceFile()
	c.CheckSourceFile(sf)

	if len(c.Diagnostics) == 0 {
		t.Error("expected diagnostic for unknown type <NonExistentType>")
	}
	found := false
	for _, d := range c.Diagnostics {
		if d.Message == "unknown type <NonExistentType> in x" {
			found = true
		}
	}
	if !found {
		t.Errorf("expected diagnostic about NonExistentType, got: %v", c.Diagnostics)
	}
}

func TestCheckDynamicAndSelf(t *testing.T) {
	v := testVM()
	c := NewChecker(v)

	source := `MyClass subclass: Object
  method: foo: x <Dynamic> ^<Self> [ ^self ]
`
	p := compiler.NewParser(source)
	sf := p.ParseSourceFile()
	c.CheckSourceFile(sf)

	if len(c.Diagnostics) != 0 {
		t.Errorf("Dynamic and Self should not produce diagnostics, got %d:", len(c.Diagnostics))
		for _, d := range c.Diagnostics {
			t.Logf("  %s", d)
		}
	}
}

func TestCheckProtocolType(t *testing.T) {
	v := testVM()
	c := NewChecker(v)

	source := `Sizeable protocol
  size ^<Integer>.

MyClass subclass: Object
  method: measure: thing <Sizeable> ^<Integer> [
    ^thing size
  ]
`
	p := compiler.NewParser(source)
	sf := p.ParseSourceFile()
	c.CheckSourceFile(sf)

	if len(c.Diagnostics) != 0 {
		t.Errorf("protocol type should be valid, got %d diagnostics:", len(c.Diagnostics))
		for _, d := range c.Diagnostics {
			t.Logf("  %s", d)
		}
	}
}

func TestCheckUntypedMethodNoDiagnostics(t *testing.T) {
	v := testVM()
	c := NewChecker(v)

	source := `MyClass subclass: Object
  method: foo: x [ ^x + 1 ]
`
	p := compiler.NewParser(source)
	sf := p.ParseSourceFile()
	c.CheckSourceFile(sf)

	if len(c.Diagnostics) != 0 {
		t.Errorf("untyped method should produce no diagnostics, got %d", len(c.Diagnostics))
	}
}

func TestProtocolSatisfaction(t *testing.T) {
	v := testVM()
	c := NewChecker(v)

	// Register a protocol
	proto := &ProtocolType{
		Name: "Sizeable",
		Methods: map[string]*MethodSig{
			"size": {ReturnType: &NamedType{Name: "Integer"}},
		},
	}
	c.Protocols.Register(proto)

	// Array has a `size` method — should satisfy Sizeable
	c.CheckProtocolSatisfaction("Array", "Sizeable")
	if len(c.Diagnostics) != 0 {
		t.Errorf("Array should satisfy Sizeable, got %d diagnostics:", len(c.Diagnostics))
		for _, d := range c.Diagnostics {
			t.Logf("  %s", d)
		}
	}
}

func TestProtocolSatisfactionFailure(t *testing.T) {
	v := testVM()
	c := NewChecker(v)

	// Register a protocol with a method that SmallInteger doesn't have
	proto := &ProtocolType{
		Name: "Appendable",
		Methods: map[string]*MethodSig{
			"append:": {
				ParamTypes: []MaggieType{&NamedType{Name: "Object"}},
				ReturnType: &NamedType{Name: "Self"},
			},
		},
	}
	c.Protocols.Register(proto)

	c.CheckProtocolSatisfaction("SmallInteger", "Appendable")
	if len(c.Diagnostics) == 0 {
		t.Error("SmallInteger should NOT satisfy Appendable")
	}
}

func TestProtocolWithIncludes(t *testing.T) {
	v := testVM()
	c := NewChecker(v)

	source := `Sizeable protocol
  size ^<Integer>.

Indexable protocol
  includes: Sizeable.
  at: <Integer> ^<Object>.
`
	p := compiler.NewParser(source)
	sf := p.ParseSourceFile()
	c.CheckSourceFile(sf)

	// Verify Indexable includes Sizeable's methods
	indexable := c.Protocols.Lookup("Indexable")
	if indexable == nil {
		t.Fatal("Indexable protocol not registered")
	}
	if _, ok := indexable.Methods["size"]; !ok {
		t.Error("Indexable should include 'size' from Sizeable")
	}
	if _, ok := indexable.Methods["at:"]; !ok {
		t.Error("Indexable should have 'at:' directly")
	}
}

// --- Integration tests: inference via Checker ---

func TestCheckerInferenceIntegration(t *testing.T) {
	v := testVM()
	c := NewChecker(v)

	// Method returns string literal but declares ^<Integer> -- should warn
	source := `MyClass subclass: Object
  method: bad ^<Integer> [
    ^'hello'
  ]
`
	p := compiler.NewParser(source)
	sf := p.ParseSourceFile()
	c.CheckSourceFile(sf)

	found := false
	for _, d := range c.Diagnostics {
		if d.Message == "inferred return type String is not assignable to declared Integer" {
			found = true
		}
	}
	if !found {
		t.Errorf("expected return type mismatch diagnostic, got: %v", c.Diagnostics)
	}
}

func TestCheckerInferenceNoDiagnosticsForMatchingTypes(t *testing.T) {
	v := testVM()
	c := NewChecker(v)

	source := `MyClass subclass: Object
  method: good ^<String> [
    ^'hello'
  ]
`
	p := compiler.NewParser(source)
	sf := p.ParseSourceFile()
	c.CheckSourceFile(sf)

	if len(c.Diagnostics) != 0 {
		t.Errorf("matching return types should produce no diagnostics, got %d:", len(c.Diagnostics))
		for _, d := range c.Diagnostics {
			t.Logf("  %s", d)
		}
	}
}

func TestCheckerHarvestReturnTypes(t *testing.T) {
	v := testVM()
	c := NewChecker(v)

	// Define a class with an annotated method, then use it in inference
	source := `Counter subclass: Object
  method: count ^<SmallInteger> [
    ^42
  ]
  method: label ^<String> [
    ^'items'
  ]
`
	p := compiler.NewParser(source)
	sf := p.ParseSourceFile()
	c.CheckSourceFile(sf)

	// Verify the return types were harvested
	typ, ok := c.ReturnTypes.Lookup("Counter", "count")
	if !ok {
		t.Fatal("expected harvested return type for Counter>>count")
	}
	if typ.String() != "SmallInteger" {
		t.Errorf("expected SmallInteger, got %s", typ)
	}

	typ, ok = c.ReturnTypes.Lookup("Counter", "label")
	if !ok {
		t.Fatal("expected harvested return type for Counter>>label")
	}
	if typ.String() != "String" {
		t.Errorf("expected String, got %s", typ)
	}
}

func TestCheckerInferenceDoesNotUnderstand(t *testing.T) {
	v := testVM()
	c := NewChecker(v)

	// SmallInteger does not understand #isEmpty
	source := `MyClass subclass: Object
  method: test [
    ^42 isEmpty
  ]
`
	p := compiler.NewParser(source)
	sf := p.ParseSourceFile()
	c.CheckSourceFile(sf)

	found := false
	for _, d := range c.Diagnostics {
		if d.Message == "SmallInteger does not understand #isEmpty" {
			found = true
		}
	}
	if !found {
		t.Errorf("expected 'does not understand' diagnostic, got: %v", c.Diagnostics)
	}
}

func TestCheckerInferencePrimitiveStubSkipped(t *testing.T) {
	v := testVM()
	c := NewChecker(v)

	// Primitive stubs should not be inferred
	source := `MyClass subclass: Object
  method: prim [ <primitive> ]
`
	p := compiler.NewParser(source)
	sf := p.ParseSourceFile()
	c.CheckSourceFile(sf)

	if len(c.Diagnostics) != 0 {
		t.Errorf("primitive stubs should produce no diagnostics, got %d:", len(c.Diagnostics))
		for _, d := range c.Diagnostics {
			t.Logf("  %s", d)
		}
	}
}

// ---------------------------------------------------------------------------
// Effect checking tests
// ---------------------------------------------------------------------------

func TestCheckerEffectPureViolation(t *testing.T) {
	v := testVM()
	c := NewChecker(v)

	source := `MyClass subclass: Object
  method: compute ^<Integer> ! <Pure> [
    | x |
    x := 42.
    ^x
  ]
`
	p := compiler.NewParser(source)
	sf := p.ParseSourceFile()
	c.CheckSourceFile(sf)

	// This method is pure (only local vars), should have no diagnostic
	for _, d := range c.Diagnostics {
		if contains(d.Message, "Pure") && contains(d.Message, "effects") {
			t.Errorf("pure method should not trigger effect warning: %s", d.Message)
		}
	}
}

func TestCheckerEffectUndeclared(t *testing.T) {
	v := testVM()
	c := NewChecker(v)

	// Method declares IO but body doesn't use IO classes -- no warning
	// (undeclared effect check only fires if body has MORE effects than declared)
	source := `MyClass subclass: Object
  method: simple ! <IO> [
    ^42
  ]
`
	p := compiler.NewParser(source)
	sf := p.ParseSourceFile()
	c.CheckSourceFile(sf)

	// Declaring IO but not using it: no diagnostic (over-declaring is fine)
	for _, d := range c.Diagnostics {
		if contains(d.Message, "undeclared") {
			t.Errorf("over-declared effects should not trigger: %s", d.Message)
		}
	}
}

func TestCheckerEffectUnknownName(t *testing.T) {
	v := testVM()
	c := NewChecker(v)

	source := `MyClass subclass: Object
  method: test ! <Bogus> [
    ^42
  ]
`
	p := compiler.NewParser(source)
	sf := p.ParseSourceFile()
	c.CheckSourceFile(sf)

	found := false
	for _, d := range c.Diagnostics {
		if contains(d.Message, "unknown effect") && contains(d.Message, "Bogus") {
			found = true
		}
	}
	if !found {
		t.Errorf("expected unknown effect diagnostic, got: %v", c.Diagnostics)
	}
}

func TestCheckerNoEffectAnnotationNoDiagnostics(t *testing.T) {
	v := testVM()
	c := NewChecker(v)

	// No effect annotation -> no effect warnings (gradual)
	source := `MyClass subclass: Object
  method: test [
    ^42
  ]
`
	p := compiler.NewParser(source)
	sf := p.ParseSourceFile()
	c.CheckSourceFile(sf)

	for _, d := range c.Diagnostics {
		if contains(d.Message, "effect") {
			t.Errorf("unannotated method should not trigger effect warnings: %s", d.Message)
		}
	}
}

func contains(s, sub string) bool {
	return len(s) >= len(sub) && (s == sub || len(s) > 0 && containsHelper(s, sub))
}

func containsHelper(s, sub string) bool {
	for i := 0; i+len(sub) <= len(s); i++ {
		if s[i:i+len(sub)] == sub {
			return true
		}
	}
	return false
}

func TestTypeCompatibility(t *testing.T) {
	tests := []struct {
		from, to MaggieType
		want     bool
	}{
		{&DynamicType{}, &NamedType{Name: "Integer"}, true},
		{&NamedType{Name: "Integer"}, &DynamicType{}, true},
		{&NamedType{Name: "Integer"}, &NamedType{Name: "Integer"}, true},
		{&NamedType{Name: "Integer"}, &NamedType{Name: "String"}, false},
		{&NamedType{Name: "Integer"}, &SelfType{}, true},
		{nil, &NamedType{Name: "Integer"}, true}, // nil = Dynamic
	}

	for _, tc := range tests {
		got := Compatible(tc.from, tc.to)
		if got != tc.want {
			t.Errorf("Compatible(%v, %v) = %v, want %v", tc.from, tc.to, got, tc.want)
		}
	}
}
