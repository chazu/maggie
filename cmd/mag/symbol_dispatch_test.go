package main

import (
	"testing"

	"github.com/chazu/maggie/vm"
)

// A Symbol that happens to spell a class name is still a Symbol: it used to
// be dispatched as that class (class side), so #Object asString, #Array size
// and `Object name == #Object` all silently answered nil.
func TestSymbolNamingAClassIsASymbol(t *testing.T) {
	v := newTestVM(t)
	eval := func(src string) vm.Value {
		m, err := v.CompileExpression(src)
		if err != nil {
			t.Fatalf("%s: %v", src, err)
		}
		r, err := v.ExecuteSafe(m, vm.Nil, nil)
		if err != nil {
			t.Fatalf("%s: %v", src, err)
		}
		return r
	}
	for src, want := range map[string]vm.Value{
		"#Object size":             vm.FromSmallInt(6),
		"Object name == #Object":   vm.True,
		"(#Array asString) size":   vm.FromSmallInt(5),
		"(Object name , 'x') size": vm.FromSmallInt(7),
		"(#foo , 'x') size":        vm.FromSmallInt(4),
	} {
		if got := eval(src); got != want {
			t.Errorf("%s = %v, want %v", src, got, want)
		}
	}
}

// `mag -m Class.method` sends to the class itself; it used to send to a
// Symbol naming the class, which only worked through a dispatch fallback.
func TestRunMainClassMethodEntry(t *testing.T) {
	v := newTestVM(t)
	got, err := runMain(v, "Compiler.isProfiling", false)
	if err != nil {
		t.Fatalf("runMain: %v", err)
	}
	if got != vm.False {
		t.Errorf("Compiler.isProfiling = %v, want false", got)
	}
}
