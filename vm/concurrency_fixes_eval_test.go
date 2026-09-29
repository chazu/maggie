package vm_test

// Source-level regression tests for concurrency primitives. These live in the
// external vm_test package so they can drive the real compiler (compiler
// imports vm, so package vm itself cannot).

import (
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// newEvalVM returns a VM with the image loaded and the Go compiler installed,
// plus an evaluator for Maggie expressions.
func newEvalVM(t *testing.T) (*vm.VM, func(string) vm.Value) {
	t.Helper()
	v := vm.NewVM()
	if err := v.LoadImage("../maggie.image"); err != nil {
		t.Fatalf("load image: %v", err)
	}
	v.UseGoCompiler(compiler.Compile)
	compilerClass := v.MustGlobal("Compiler")
	return v, func(src string) vm.Value {
		t.Helper()
		return v.Send(compilerClass, "evaluate:", []vm.Value{v.Registry().NewStringValue(src)})
	}
}

// TestForkErrorExitResultIsNil guards the regression where a process that died
// from an error left ExitReason.Result as the zero Value, which decodes as the
// Float 0.0 — so `[nil foo] fork wait` answered 0.0 instead of nil.
func TestForkErrorExitResultIsNil(t *testing.T) {
	_, eval := newEvalVM(t)
	if r := eval(`[nil foo] fork wait`); r != vm.Nil {
		t.Fatalf("[nil foo] fork wait: want nil, got %v (IsFloat=%v)", r, r.IsFloat())
	}
}

// TestMutexCriticalUnlockInsideIsCatchable guards the regression where
// `m critical: [m unlock]` made the deferred cleanup Unlock an already-unlocked
// sync.Mutex — a Go *fatal* error no on:do: can catch.
func TestMutexCriticalUnlockInsideIsCatchable(t *testing.T) {
	_, eval := newEvalVM(t)
	r := eval(`| m | m := Mutex new. [m critical: [m unlock. 7]] on: Error do: [:e | 9]`)
	if !r.IsSmallInt() || r.SmallInt() != 7 {
		t.Fatalf("critical: with inner unlock: want 7, got %v", r)
	}
	// The mutex must be usable (unlocked) afterwards.
	r = eval(`| m | m := Mutex new. m critical: [m unlock]. m tryLock`)
	if r != vm.True {
		t.Fatalf("mutex should be unlocked after critical: returns, tryLock got %v", r)
	}
}

// TestWaitGroupWrapWithInnerDoneDoesNotPanic guards the regression where
// wrap:'s deferred decrement skipped the guard `done` uses: an extra `done`
// inside (or racing) the wrapped block drove the counter negative and the
// 'negative WaitGroup counter' panic killed the whole VM.
func TestWaitGroupWrapWithInnerDoneDoesNotPanic(t *testing.T) {
	_, eval := newEvalVM(t)
	r := eval(`| wg p | wg := WaitGroup new.
		p := wg wrap: [[wg done] on: Error do: [:e | nil]. 1].
		p wait.
		wg wait.
		wg count`)
	if !r.IsSmallInt() || r.SmallInt() != 0 {
		t.Fatalf("wg count after wrap: + inner done: want 0, got %v", r)
	}
}

// TestWaitGroupWrapInheritsRestrictions guards the regression where wrap:
// computed the hidden-global set on the NEW goroutine (which resolves to the
// main interpreter), so a forkRestricted: process escaped its sandbox via
// `wg wrap: [...]`.
func TestWaitGroupWrapInheritsRestrictions(t *testing.T) {
	_, eval := newEvalVM(t)
	r := eval(`([ | wg p |
		wg := WaitGroup new.
		p := wg wrap: [[File. #leaked] on: Error do: [:e | #blocked]].
		p wait ] forkRestricted: #('File')) wait`)
	if !r.IsSymbol() {
		t.Fatalf("expected a Symbol result, got %v", r)
	}
	if r == eval(`#leaked`) {
		t.Fatal("wrap: inside forkRestricted: #('File') could see File (sandbox escape)")
	}
	if r != eval(`#blocked`) {
		t.Fatalf("expected #blocked, got %v", r)
	}
}
