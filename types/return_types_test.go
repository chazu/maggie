package types

import (
	"testing"

	"github.com/chazu/maggie/compiler"
)

func TestReturnTypeTableBuiltins(t *testing.T) {
	rt := NewReturnTypeTable()

	tests := []struct {
		class    string
		selector string
		want     string
	}{
		{"SmallInteger", "+", "SmallInteger"},
		{"SmallInteger", "=", "Boolean"},
		{"SmallInteger", "asFloat", "Float"},
		{"SmallInteger", "asString", "String"},
		{"Float", "+", "Float"},
		{"Float", "=", "Boolean"},
		{"Float", "ceiling", "SmallInteger"},
		{"Float", "asString", "String"},
		{"String", "size", "SmallInteger"},
		{"String", ",", "String"},
		{"String", "=", "Boolean"},
		{"String", "asInteger", "SmallInteger"},
		{"Array", "size", "SmallInteger"},
		{"Array", "isEmpty", "Boolean"},
		{"Array", ",", "Array"},
		{"Boolean", "not", "Boolean"},
		{"Boolean", "and:", "Boolean"},
		{"Object", "isNil", "Boolean"},
		{"Object", "yourself", "Self"},
	}

	for _, tc := range tests {
		typ, ok := rt.Lookup(tc.class, tc.selector)
		if !ok {
			t.Errorf("Lookup(%s, %s): expected entry, got not found", tc.class, tc.selector)
			continue
		}
		if typ.String() != tc.want {
			t.Errorf("Lookup(%s, %s) = %s, want %s", tc.class, tc.selector, typ, tc.want)
		}
	}
}

func TestReturnTypeTableIntegerAlias(t *testing.T) {
	rt := NewReturnTypeTable()

	// "Integer" should alias to SmallInteger entries
	typ, ok := rt.Lookup("Integer", "+")
	if !ok {
		t.Fatal("expected Integer alias to find SmallInteger entry")
	}
	if typ.String() != "SmallInteger" {
		t.Errorf("expected SmallInteger, got %s", typ)
	}
}

func TestReturnTypeTableUnknown(t *testing.T) {
	rt := NewReturnTypeTable()
	_, ok := rt.Lookup("SmallInteger", "nonExistentSelector")
	if ok {
		t.Error("expected unknown selector to return false")
	}
}

func TestReturnTypeTableRegister(t *testing.T) {
	rt := NewReturnTypeTable()
	rt.Register("MyClass", "doStuff", &NamedType{Name: "String"})

	typ, ok := rt.Lookup("MyClass", "doStuff")
	if !ok {
		t.Fatal("expected registered entry")
	}
	if typ.String() != "String" {
		t.Errorf("expected String, got %s", typ)
	}
}

func TestReturnTypeTableHarvestFromMethod(t *testing.T) {
	rt := NewReturnTypeTable()

	md := &compiler.MethodDef{
		Selector:   "name",
		ReturnType: &compiler.TypeExpr{Name: "String"},
	}
	rt.HarvestFromMethod("Person", md)

	typ, ok := rt.Lookup("Person", "name")
	if !ok {
		t.Fatal("expected harvested entry")
	}
	if typ.String() != "String" {
		t.Errorf("expected String, got %s", typ)
	}
}

func TestReturnTypeTableHarvestDynamic(t *testing.T) {
	rt := NewReturnTypeTable()

	// Dynamic return type should not be harvested
	md := &compiler.MethodDef{
		Selector:   "something",
		ReturnType: &compiler.TypeExpr{Name: "Dynamic"},
	}
	rt.HarvestFromMethod("MyClass", md)

	_, ok := rt.Lookup("MyClass", "something")
	if ok {
		t.Error("Dynamic return types should not be harvested")
	}
}

func TestReturnTypeTableHarvestNilReturnType(t *testing.T) {
	rt := NewReturnTypeTable()

	md := &compiler.MethodDef{
		Selector:   "doIt",
		ReturnType: nil, // no annotation
	}
	rt.HarvestFromMethod("MyClass", md)

	_, ok := rt.Lookup("MyClass", "doIt")
	if ok {
		t.Error("nil return type should not be harvested")
	}
}
