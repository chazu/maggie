package vm_test

import (
	"strings"
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// evaluate:withLocals: must restore globals even when the evaluated code
// signals, so the locals do not leak into the global namespace.
func TestEvaluateWithLocalsRestoresOnSignal(t *testing.T) {
	_, eval := newEvalVM(t)
	r := eval(`| d | d := Dictionary new. d at: #zork put: 42.
		[Compiler evaluate: 'zork foo' withLocals: d] on: Error do: [:e | nil].
		Compiler getGlobal: #zork`)
	if r != vm.Nil {
		t.Fatalf("local leaked into globals after a signal: getGlobal: #zork = %v", r)
	}
}

// Inside a forked process, locals must shadow the process's own global
// overlay, and new assignments must be written back into the locals dict.
func TestEvaluateWithLocalsInForkedProcess(t *testing.T) {
	_, eval := newEvalVM(t)
	r := eval(`| d | d := Dictionary new. d at: #qq put: 99.
		[Compiler setGlobal: #qq to: 1. Compiler evaluate: 'qq' withLocals: d] fork wait`)
	if !r.IsSmallInt() || r.SmallInt() != 99 {
		t.Fatalf("forked evaluate: 'qq' withLocals: {qq->99} = %v, want 99", r)
	}
	r = eval(`| d | d := Dictionary new. d at: #qq put: 99.
		[Compiler setGlobal: #qq to: 1. Compiler evaluate: 'qq' withLocals: d. Compiler getGlobal: #qq] fork wait`)
	if !r.IsSmallInt() || r.SmallInt() != 1 {
		t.Fatalf("forked process global not restored: got %v, want 1", r)
	}
	r = eval(`| d | d := Dictionary new.
		[Compiler evaluate: 'freshVar := 5' withLocals: d. d at: #freshVar ifAbsent: [nil]] fork wait`)
	if !r.IsSmallInt() || r.SmallInt() != 5 {
		t.Fatalf("forked new variable not written back to locals: got %v, want 5", r)
	}
}

// Main-interpreter behavior: locals shadow globals, assignments write back,
// and globals are restored afterwards.
func TestEvaluateWithLocalsMainInterpreter(t *testing.T) {
	_, eval := newEvalVM(t)
	r := eval(`| d | d := Dictionary new. d at: #aa put: 2.
		Compiler setGlobal: #aa to: 1.
		Compiler evaluate: 'aa := aa + 10. bb := 7' withLocals: d.
		{ d at: #aa. d at: #bb. Compiler getGlobal: #aa. Compiler getGlobal: #bb }`)
	got := vm.ObjectFromValue(r)
	if got == nil || got.NumSlots() != 4 {
		t.Fatalf("unexpected result %v", r)
	}
	want := []vm.Value{vm.FromSmallInt(12), vm.FromSmallInt(7), vm.FromSmallInt(1), vm.Nil}
	for i, w := range want {
		if g := got.GetSlot(i); g != w {
			t.Errorf("element %d = %v, want %v", i+1, g, w)
		}
	}
}

// Concurrent global readers must not race with evaluate:withLocals: on the
// main interpreter (run with -race).
func TestEvaluateWithLocalsConcurrentReaders(t *testing.T) {
	_, eval := newEvalVM(t)
	r := eval(`| p d |
		p := [1 to: 200 do: [:k | Compiler getGlobal: #Object]] fork.
		d := Dictionary new. d at: #rr put: 1.
		1 to: 200 do: [:k | Compiler evaluate: 'rr' withLocals: d].
		p wait.
		3`)
	if !r.IsSmallInt() || r.SmallInt() != 3 {
		t.Fatalf("got %v", r)
	}
}

// fileOut output of methods installed via compileAndInstall: (bare source)
// must parse back with the same instance- and class-side selectors.
func TestFileOutRoundTripsCompileAndInstall(t *testing.T) {
	v, eval := newEvalVM(t)
	cls := vm.NewClass("RoundTrip", v.ObjectClass)
	v.Classes.Register(cls)
	v.SetGlobal("RoundTrip", v.ClassValue(cls))
	eval(`RoundTrip compileAndInstall: 'at: i put: x
    ^i + x'.
		RoundTrip compileAndInstall: 'size ^0'.
		RoundTrip compileAndInstallClassMethod: 'make
    ^self new'`)

	src := vm.FileOutClass(cls, v.Selectors)
	sf, err := compiler.ParseSourceFileFromString(src)
	if err != nil {
		t.Fatalf("fileOut output does not parse: %v\n%s", err, src)
	}
	if len(sf.Classes) != 1 {
		t.Fatalf("expected 1 class, got %d\n%s", len(sf.Classes), src)
	}
	var inst, cm []string
	for _, m := range sf.Classes[0].Methods {
		inst = append(inst, m.Selector)
	}
	for _, m := range sf.Classes[0].ClassMethods {
		cm = append(cm, m.Selector)
	}
	if s := strings.Join(inst, ","); !strings.Contains(s, "at:put:") || !strings.Contains(s, "size") || len(inst) != 2 {
		t.Errorf("instance methods = %v\n%s", inst, src)
	}
	if len(cm) != 1 || cm[0] != "make" {
		t.Errorf("class methods = %v\n%s", cm, src)
	}
}
