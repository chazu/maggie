package cue

import (
	"testing"

	"github.com/chazu/maggie/compiler"
	vm "github.com/chazu/maggie/vm"
)

// newCueEvalVM returns a VM with the image loaded, the Go compiler installed
// and the cue contrib registered, plus an evaluator for Maggie expressions.
func newCueEvalVM(t *testing.T) func(string) vm.Value {
	t.Helper()
	v := vm.NewVM()
	if err := v.LoadImage("../../../maggie.image"); err != nil {
		t.Fatalf("load image: %v", err)
	}
	v.UseGoCompiler(compiler.Compile)
	compilerClass := v.MustGlobal("Compiler")
	return func(src string) vm.Value {
		t.Helper()
		return v.Send(compilerClass, "evaluate:", []vm.Value{v.Registry().NewStringValue(src)})
	}
}

// TestConstraintStoreWatchCallbackRunsIsolated guards the data race where a
// deferred watch:do: callback ran vm.Send on the MAIN interpreter from a raw
// goroutine, concurrently with the main program pushing/popping frames on that
// same interpreter. Run with -race: the old code reports a DATA RACE in
// pushFrame/popFrame.
func TestConstraintStoreWatchCallbackRunsIsolated(t *testing.T) {
	eval := newCueEvalVM(t)
	r := eval(`| store ctx ch sum |
		store := ConstraintStore new.
		ctx := CueContext new.
		ch := Channel new: 1.
		store watch: (ctx compileString: '{x: int}') value do: [
			| s | s := 0. 1 to: 200 do: [:i | s := s + i printString size]. ch send: s].
		store tell: (ctx compileString: '{x: 1}') value.
		sum := 0.
		1 to: 2000 do: [:i | sum := sum + i printString size].
		ch receive`)
	if !r.IsSmallInt() || r.SmallInt() != 492 {
		t.Fatalf("watch:do: callback result: want 492, got %v", r)
	}
}

// TestConstraintStoreWatchCallbackErrorDoesNotCrash: an unhandled error in a
// deferred callback must not take the whole process down.
func TestConstraintStoreWatchCallbackErrorDoesNotCrash(t *testing.T) {
	eval := newCueEvalVM(t)
	r := eval(`| store ctx ch |
		store := ConstraintStore new.
		ctx := CueContext new.
		ch := Channel new: 1.
		store watch: (ctx compileString: '{y: int}') value do: [ch send: 1. nil foo].
		store tell: (ctx compileString: '{y: 1}') value.
		ch receive`)
	if !r.IsSmallInt() || r.SmallInt() != 1 {
		t.Fatalf("want 1, got %v", r)
	}
}
