package vm

import (
	"reflect"
	"testing"
)

type goObjectWrapFixture struct{ N int }

// TestValueToGoUnwrapsGoObject: GoObjects are heap Values since the
// pointer-value migration; ValueToGo only looked for them among
// symbol-encoded Values, answered nil, and generated wrappers then panicked
// on nil.(*pkg.T).
func TestValueToGoUnwrapsGoObject(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	v.RegisterGoType("Go::Test::Fixture", reflect.TypeOf((*goObjectWrapFixture)(nil)))
	orig := &goObjectWrapFixture{N: 7}
	val := v.GoToValue(orig)
	if val == Nil {
		t.Fatal("GoToValue of a registered *T answered nil")
	}
	got, ok := v.ValueToGo(val).(*goObjectWrapFixture)
	if !ok || got != orig {
		t.Fatalf("ValueToGo = %#v, want the original *goObjectWrapFixture", v.ValueToGo(val))
	}

	sym := v.Symbols.SymbolValue("foo")
	if got := v.ValueToGo(sym); got != "foo" {
		t.Fatalf("ValueToGo(#foo) = %#v, want \"foo\"", got)
	}
}

func TestGoStringBoolFloatArgs(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	if got := v.GoStringArg(v.registry.NewStringValue("hi")); got != "hi" {
		t.Fatalf("GoStringArg = %q", got)
	}
	if got := v.GoStringArg(v.Symbols.SymbolValue("sym")); got != "sym" {
		t.Fatalf("GoStringArg(#sym) = %q", got)
	}
	if !v.GoBoolArg(True) || v.GoBoolArg(False) {
		t.Fatal("GoBoolArg misread a Boolean")
	}
	if got := v.GoFloatArg(FromFloat64(1.5)); got != 1.5 {
		t.Fatalf("GoFloatArg(1.5) = %v", got)
	}
	if got := v.GoFloatArg(FromSmallInt(3)); got != 3 {
		t.Fatalf("GoFloatArg(3) = %v", got)
	}
	if got := v.GoFloatArg(v.registry.NewIntegerValue(1 << 50)); got != 1<<50 {
		t.Fatalf("GoFloatArg(2^50) = %v", got)
	}

	for _, bad := range []func(){
		func() { v.GoStringArg(FromSmallInt(1)) },
		func() { v.GoStringArg(Nil) },
		func() { v.GoBoolArg(Nil) },
		func() { v.GoBoolArg(FromSmallInt(0)) },
		func() { v.GoFloatArg(v.registry.NewStringValue("1.5")) },
		func() { v.GoFloatArg(Nil) },
	} {
		expectPrimitiveError(t, v, bad)
	}
}

// Generated bindings for *T and struct-valued T parameters used unchecked
// ValueToGo(x).(*pkg.T) assertions, so a wrong or nil argument panicked the
// process instead of signalling.
func TestGoPointerAndStructArgs(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()
	v.RegisterGoType("Go::Test::Fixture", reflect.TypeOf((*goObjectWrapFixture)(nil)))
	orig := &goObjectWrapFixture{N: 7}
	val := v.GoToValue(orig)

	if got := GoPointerArg[goObjectWrapFixture](v, val); got != orig {
		t.Fatalf("GoPointerArg = %p, want %p", got, orig)
	}
	if got := GoPointerArg[goObjectWrapFixture](v, Nil); got != nil {
		t.Fatalf("GoPointerArg(nil) = %p, want nil", got)
	}
	if got := GoStructArg[goObjectWrapFixture](v, val); got != *orig {
		t.Fatalf("GoStructArg = %+v, want %+v", got, *orig)
	}
	for _, bad := range []func(){
		func() { GoPointerArg[goObjectWrapFixture](v, FromSmallInt(1)) },
		func() { GoPointerArg[int](v, val) },
		func() { GoStructArg[goObjectWrapFixture](v, Nil) },
		func() { GoStructArg[goObjectWrapFixture](v, v.registry.NewStringValue("x")) },
	} {
		expectPrimitiveError(t, v, bad)
	}
}
