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
