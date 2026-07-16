package main

import (
	"strings"
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

func newDoItVM(t *testing.T) *vm.VM {
	t.Helper()
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)
	return vmInst
}

func evalDoIt(t *testing.T, vmInst *vm.VM, source string) vm.Value {
	t.Helper()
	compilerClass := vmInst.MustGlobal("Compiler")
	return vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue(source)})
}

// TestCompileDoIt_StringLiteralWithDot is the regression test for the
// textual dot-splitter (review finding C-3): evaluating `3 + 4. 'x.y'`
// used to splice the return caret INSIDE the string literal, silently
// compiling a method whose string was "x. ^y".
func TestCompileDoIt_StringLiteralWithDot(t *testing.T) {
	vmInst := newDoItVM(t)
	result := evalDoIt(t, vmInst, "3 + 4. 'x.y'")
	got := vmInst.Registry().GetStringContent(result)
	if got != "x.y" {
		t.Fatalf("evaluate '3 + 4. 'x.y'': got %q, want %q", got, "x.y")
	}
}

// Dots inside block bodies must not be mistaken for the last statement
// separator (the old splitter put the ^ inside the block).
func TestCompileDoIt_DotsInsideBlocks(t *testing.T) {
	vmInst := newDoItVM(t)
	result := evalDoIt(t, vmInst, "[ 1. 2 ] value")
	if !result.IsSmallInt() || result.SmallInt() != 2 {
		t.Fatalf("evaluate '[ 1. 2 ] value': got %v, want 2", result)
	}
}

// Multi-statement eval with leading temps returns the final expression.
func TestCompileDoIt_TempsAndStatements(t *testing.T) {
	vmInst := newDoItVM(t)
	result := evalDoIt(t, vmInst, "| x | x := 2. x * 3")
	if !result.IsSmallInt() || result.SmallInt() != 6 {
		t.Fatalf("evaluate '| x | x := 2. x * 3': got %v, want 6", result)
	}
}

// Blocks with mutable captures now go through the full method-compilation
// path (cell analysis included) — the old CompileExpression entry point
// skipped findCellVariables.
func TestCompileDoIt_MutableCaptureInBlock(t *testing.T) {
	vmInst := newDoItVM(t)
	result := evalDoIt(t, vmInst, "| x | x := 1. [ x := x + 41 ] value. x")
	if !result.IsSmallInt() || result.SmallInt() != 42 {
		t.Fatalf("mutable capture: got %v, want 42", result)
	}
}

// Semantic warnings surface through CompileExpressionWithWarnings — the
// first consumer of the analyzer output that used to be discarded.
func TestCompileDoIt_WarningsSurface(t *testing.T) {
	vmInst := newDoItVM(t)
	_, warnings, err := vmInst.CompileExpressionWithWarnings("someUndefinedVariableXyz + 1")
	if err != nil {
		t.Fatalf("compile: %v", err)
	}
	found := false
	for _, w := range warnings {
		if strings.Contains(w, "someUndefinedVariableXyz") {
			found = true
		}
	}
	if !found {
		t.Fatalf("expected undefined-variable warning, got %v", warnings)
	}
}
