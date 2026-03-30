package compiler

import (
	"testing"
)

// ---------------------------------------------------------------------------
// Type annotation parsing tests
// ---------------------------------------------------------------------------

func TestParseUnaryMethodWithReturnType(t *testing.T) {
	source := "size ^<Integer> [ ^items size ]"
	p := NewParser(source)
	m := p.ParseMethod()
	if m == nil {
		t.Fatal("expected method, got nil")
	}
	if m.Selector != "size" {
		t.Errorf("selector = %q, want 'size'", m.Selector)
	}
	if m.ReturnType == nil {
		t.Fatal("expected return type annotation")
	}
	if m.ReturnType.Name != "Integer" {
		t.Errorf("return type = %q, want 'Integer'", m.ReturnType.Name)
	}
}

func TestParseBinaryMethodWithTypes(t *testing.T) {
	source := "+ other <Number> ^<Number> [ ^self primPlus: other ]"
	p := NewParser(source)
	m := p.ParseMethod()
	if m == nil {
		t.Fatal("expected method, got nil")
	}
	if m.Selector != "+" {
		t.Errorf("selector = %q, want '+'", m.Selector)
	}
	if len(m.ParamTypes) != 1 || m.ParamTypes[0] == nil {
		t.Fatal("expected param type for 'other'")
	}
	if m.ParamTypes[0].Name != "Number" {
		t.Errorf("param type = %q, want 'Number'", m.ParamTypes[0].Name)
	}
	if m.ReturnType == nil || m.ReturnType.Name != "Number" {
		t.Errorf("return type = %v, want 'Number'", m.ReturnType)
	}
}

func TestParseKeywordMethodWithTypes(t *testing.T) {
	source := "at: index <Integer> put: value <Object> ^<Object> [ ^self primAt: index put: value ]"
	p := NewParser(source)
	m := p.ParseMethod()
	if m == nil {
		t.Fatal("expected method, got nil")
	}
	if m.Selector != "at:put:" {
		t.Errorf("selector = %q, want 'at:put:'", m.Selector)
	}
	if len(m.ParamTypes) != 2 {
		t.Fatalf("expected 2 param types, got %d", len(m.ParamTypes))
	}
	if m.ParamTypes[0] == nil || m.ParamTypes[0].Name != "Integer" {
		t.Errorf("param 0 type = %v, want 'Integer'", m.ParamTypes[0])
	}
	if m.ParamTypes[1] == nil || m.ParamTypes[1].Name != "Object" {
		t.Errorf("param 1 type = %v, want 'Object'", m.ParamTypes[1])
	}
	if m.ReturnType == nil || m.ReturnType.Name != "Object" {
		t.Errorf("return type = %v, want 'Object'", m.ReturnType)
	}
}

func TestParseKeywordMethodPartialTypes(t *testing.T) {
	// Only first param typed, second untyped
	source := "at: index <Integer> put: value [ ^self primAt: index put: value ]"
	p := NewParser(source)
	m := p.ParseMethod()
	if m == nil {
		t.Fatal("expected method, got nil")
	}
	if len(m.ParamTypes) != 2 {
		t.Fatalf("expected 2 param types, got %d", len(m.ParamTypes))
	}
	if m.ParamTypes[0] == nil || m.ParamTypes[0].Name != "Integer" {
		t.Errorf("param 0 type = %v, want 'Integer'", m.ParamTypes[0])
	}
	if m.ParamTypes[1] != nil {
		t.Errorf("param 1 type = %v, want nil (untyped)", m.ParamTypes[1])
	}
}

func TestParseMethodNoTypes(t *testing.T) {
	// Existing untyped method — must still work
	source := "at: index put: value [ ^self primAt: index put: value ]"
	p := NewParser(source)
	m := p.ParseMethod()
	if m == nil {
		t.Fatal("expected method, got nil")
	}
	if m.Selector != "at:put:" {
		t.Errorf("selector = %q, want 'at:put:'", m.Selector)
	}
	if m.ReturnType != nil {
		t.Errorf("return type = %v, want nil", m.ReturnType)
	}
	// ParamTypes should be all nil
	for i, pt := range m.ParamTypes {
		if pt != nil {
			t.Errorf("param %d type = %v, want nil", i, pt)
		}
	}
}

func TestParseTypedTemporaries(t *testing.T) {
	source := "example | count <Integer> name <String> | ^count"
	p := NewParser(source)
	m := p.ParseMethod()
	if m == nil {
		t.Fatal("expected method, got nil")
	}
	if len(m.Temps) != 2 {
		t.Fatalf("expected 2 temps, got %d", len(m.Temps))
	}
	if len(m.TempTypes) != 2 {
		t.Fatalf("expected 2 temp types, got %d", len(m.TempTypes))
	}
	if m.TempTypes[0] == nil || m.TempTypes[0].Name != "Integer" {
		t.Errorf("temp 0 type = %v, want 'Integer'", m.TempTypes[0])
	}
	if m.TempTypes[1] == nil || m.TempTypes[1].Name != "String" {
		t.Errorf("temp 1 type = %v, want 'String'", m.TempTypes[1])
	}
}

func TestParseSelfReturnType(t *testing.T) {
	source := "init ^<Self> [ ^self ]"
	p := NewParser(source)
	m := p.ParseMethod()
	if m == nil {
		t.Fatal("expected method, got nil")
	}
	if m.ReturnType == nil || m.ReturnType.Name != "Self" {
		t.Errorf("return type = %v, want 'Self'", m.ReturnType)
	}
}

func TestParseDynamicType(t *testing.T) {
	source := "process: thing <Dynamic> ^<Dynamic> [ ^thing printString ]"
	p := NewParser(source)
	m := p.ParseMethod()
	if m == nil {
		t.Fatal("expected method, got nil")
	}
	if m.ParamTypes[0] == nil || m.ParamTypes[0].Name != "Dynamic" {
		t.Errorf("param type = %v, want 'Dynamic'", m.ParamTypes[0])
	}
	if m.ReturnType == nil || m.ReturnType.Name != "Dynamic" {
		t.Errorf("return type = %v, want 'Dynamic'", m.ReturnType)
	}
}

// ---------------------------------------------------------------------------
// Protocol parsing tests
// ---------------------------------------------------------------------------

func TestParseProtocolDefinition(t *testing.T) {
	source := `Sizeable protocol
  size ^<Integer>.
  isEmpty ^<Boolean>.
`
	p := NewParser(source)
	sf := p.ParseSourceFile()
	if sf == nil {
		t.Fatal("expected source file, got nil")
	}
	if len(sf.Protocols) != 1 {
		t.Fatalf("expected 1 protocol, got %d", len(sf.Protocols))
	}
	proto := sf.Protocols[0]
	if proto.Name != "Sizeable" {
		t.Errorf("name = %q, want 'Sizeable'", proto.Name)
	}
	if len(proto.Entries) != 2 {
		t.Fatalf("expected 2 entries, got %d", len(proto.Entries))
	}

	// size ^<Integer>
	if proto.Entries[0].Selector != "size" {
		t.Errorf("entry 0 selector = %q, want 'size'", proto.Entries[0].Selector)
	}
	if proto.Entries[0].ReturnType == nil || proto.Entries[0].ReturnType.Name != "Integer" {
		t.Errorf("entry 0 return type = %v, want 'Integer'", proto.Entries[0].ReturnType)
	}

	// isEmpty ^<Boolean>
	if proto.Entries[1].Selector != "isEmpty" {
		t.Errorf("entry 1 selector = %q, want 'isEmpty'", proto.Entries[1].Selector)
	}
	if proto.Entries[1].ReturnType == nil || proto.Entries[1].ReturnType.Name != "Boolean" {
		t.Errorf("entry 1 return type = %v, want 'Boolean'", proto.Entries[1].ReturnType)
	}
}

func TestParseProtocolWithKeywordEntry(t *testing.T) {
	source := `Indexable protocol
  at: <Integer> ^<Object>.
  at: <Integer> put: <Object> ^<Object>.
`
	p := NewParser(source)
	sf := p.ParseSourceFile()
	if len(sf.Protocols) != 1 {
		t.Fatalf("expected 1 protocol, got %d", len(sf.Protocols))
	}
	proto := sf.Protocols[0]

	if len(proto.Entries) != 2 {
		t.Fatalf("expected 2 entries, got %d", len(proto.Entries))
	}

	// at: <Integer> ^<Object>
	e0 := proto.Entries[0]
	if e0.Selector != "at:" {
		t.Errorf("entry 0 selector = %q, want 'at:'", e0.Selector)
	}
	if len(e0.ParamTypes) != 1 || e0.ParamTypes[0] == nil || e0.ParamTypes[0].Name != "Integer" {
		t.Errorf("entry 0 param type = %v, want 'Integer'", e0.ParamTypes)
	}

	// at:put:
	e1 := proto.Entries[1]
	if e1.Selector != "at:put:" {
		t.Errorf("entry 1 selector = %q, want 'at:put:'", e1.Selector)
	}
	if len(e1.ParamTypes) != 2 {
		t.Fatalf("entry 1 expected 2 param types, got %d", len(e1.ParamTypes))
	}
}

func TestParseProtocolWithIncludes(t *testing.T) {
	source := `Indexable protocol
  includes: Sizeable.
  at: <Integer> ^<Object>.
`
	p := NewParser(source)
	sf := p.ParseSourceFile()
	if len(sf.Protocols) != 1 {
		t.Fatalf("expected 1 protocol, got %d", len(sf.Protocols))
	}
	proto := sf.Protocols[0]
	if len(proto.Includes) != 1 || proto.Includes[0] != "Sizeable" {
		t.Errorf("includes = %v, want ['Sizeable']", proto.Includes)
	}
	if len(proto.Entries) != 1 {
		t.Errorf("expected 1 entry (after includes), got %d", len(proto.Entries))
	}
}

func TestParseProtocolWithDocstring(t *testing.T) {
	source := `"""A collection that has a size."""
Sizeable protocol
  size ^<Integer>.
`
	p := NewParser(source)
	sf := p.ParseSourceFile()
	if len(sf.Protocols) != 1 {
		t.Fatalf("expected 1 protocol, got %d", len(sf.Protocols))
	}
	if sf.Protocols[0].DocString == "" {
		t.Error("expected docstring on protocol")
	}
}

// ---------------------------------------------------------------------------
// Source file with mixed definitions
// ---------------------------------------------------------------------------

func TestParseSourceFileWithProtocolAndClass(t *testing.T) {
	source := `Sizeable protocol
  size ^<Integer>.

Counter subclass: Object
  instanceVars: count <Integer>

  method: size ^<Integer> [
    ^count
  ]
`
	p := NewParser(source)
	sf := p.ParseSourceFile()
	if len(sf.Protocols) != 1 {
		t.Errorf("expected 1 protocol, got %d", len(sf.Protocols))
	}
	if len(sf.Classes) != 1 {
		t.Errorf("expected 1 class, got %d", len(sf.Classes))
	}
	cls := sf.Classes[0]
	if len(cls.InstanceVarTypes) != 1 || cls.InstanceVarTypes[0] == nil {
		t.Errorf("expected typed instance var, got %v", cls.InstanceVarTypes)
	}
	if cls.InstanceVarTypes[0].Name != "Integer" {
		t.Errorf("ivar type = %q, want 'Integer'", cls.InstanceVarTypes[0].Name)
	}
	if len(cls.Methods) != 1 {
		t.Fatalf("expected 1 method, got %d", len(cls.Methods))
	}
	m := cls.Methods[0]
	if m.ReturnType == nil || m.ReturnType.Name != "Integer" {
		t.Errorf("method return type = %v, want 'Integer'", m.ReturnType)
	}
}

// ---------------------------------------------------------------------------
// Backward compatibility: <primitive> must still work
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Effect annotation parsing tests
// ---------------------------------------------------------------------------

func TestParseEffectSinglePure(t *testing.T) {
	source := "compute ^<Integer> ! <Pure> [ ^42 ]"
	p := NewParser(source)
	m := p.ParseMethod()
	if m == nil {
		t.Fatal("expected method, got nil")
	}
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}
	if len(m.Effects) != 1 {
		t.Fatalf("expected 1 effect, got %d", len(m.Effects))
	}
	if m.Effects[0].Name != "Pure" {
		t.Errorf("effect = %q, want 'Pure'", m.Effects[0].Name)
	}
}

func TestParseEffectMultiple(t *testing.T) {
	source := "fetch: url <String> ^<String> ! <IO, Network> [ ^url ]"
	p := NewParser(source)
	m := p.ParseMethod()
	if m == nil {
		t.Fatal("expected method, got nil")
	}
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}
	if len(m.Effects) != 2 {
		t.Fatalf("expected 2 effects, got %d", len(m.Effects))
	}
	if m.Effects[0].Name != "IO" {
		t.Errorf("effect 0 = %q, want 'IO'", m.Effects[0].Name)
	}
	if m.Effects[1].Name != "Network" {
		t.Errorf("effect 1 = %q, want 'Network'", m.Effects[1].Name)
	}
}

func TestParseEffectNoAnnotation(t *testing.T) {
	source := "simple [ ^42 ]"
	p := NewParser(source)
	m := p.ParseMethod()
	if m == nil {
		t.Fatal("expected method, got nil")
	}
	if len(m.Effects) != 0 {
		t.Errorf("expected no effects, got %d", len(m.Effects))
	}
}

func TestParseEffectInBrackets(t *testing.T) {
	source := `MyClass subclass: Object
  method: write: data <String> ! <IO> [
    ^data
  ]
`
	p := NewParser(source)
	sf := p.ParseSourceFile()
	if len(sf.Classes) != 1 {
		t.Fatalf("expected 1 class, got %d", len(sf.Classes))
	}
	if len(sf.Classes[0].Methods) != 1 {
		t.Fatalf("expected 1 method, got %d", len(sf.Classes[0].Methods))
	}
	m := sf.Classes[0].Methods[0]
	if len(m.Effects) != 1 || m.Effects[0].Name != "IO" {
		t.Errorf("expected IO effect, got %v", m.Effects)
	}
}

func TestParseEffectOnProtocolEntry(t *testing.T) {
	source := `Writable protocol
  write: <String> ^<Boolean> ! <IO>.
`
	p := NewParser(source)
	sf := p.ParseSourceFile()
	if len(sf.Protocols) != 1 {
		t.Fatalf("expected 1 protocol, got %d", len(sf.Protocols))
	}
	proto := sf.Protocols[0]
	if len(proto.Entries) != 1 {
		t.Fatalf("expected 1 entry, got %d", len(proto.Entries))
	}
	entry := proto.Entries[0]
	if len(entry.Effects) != 1 || entry.Effects[0].Name != "IO" {
		t.Errorf("expected IO effect on protocol entry, got %v", entry.Effects)
	}
}

func TestParseEffectWithReturnTypeAndTypes(t *testing.T) {
	source := "save: path <String> ^<Boolean> ! <IO, State> [ ^true ]"
	p := NewParser(source)
	m := p.ParseMethod()
	if m == nil {
		t.Fatal("expected method, got nil")
	}
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}
	if m.ReturnType == nil || m.ReturnType.Name != "Boolean" {
		t.Errorf("return type = %v, want 'Boolean'", m.ReturnType)
	}
	if len(m.Effects) != 2 {
		t.Fatalf("expected 2 effects, got %d", len(m.Effects))
	}
	if m.Effects[0].Name != "IO" || m.Effects[1].Name != "State" {
		t.Errorf("effects = [%s, %s], want [IO, State]", m.Effects[0].Name, m.Effects[1].Name)
	}
}

func TestPrimitiveStubStillWorks(t *testing.T) {
	source := `MyClass subclass: Object
  method: primFoo [ <primitive> ]
`
	p := NewParser(source)
	sf := p.ParseSourceFile()
	if len(sf.Classes) != 1 {
		t.Fatalf("expected 1 class, got %d", len(sf.Classes))
	}
	if len(sf.Classes[0].Methods) != 1 {
		t.Fatalf("expected 1 method, got %d", len(sf.Classes[0].Methods))
	}
	m := sf.Classes[0].Methods[0]
	if !m.IsPrimitiveStub {
		t.Error("expected primitive stub")
	}
}
