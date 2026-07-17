package compiler

import (
	"strings"
	"testing"

	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// Control-flow inlining: semantics
// ---------------------------------------------------------------------------

// compileMethodSrc compiles a full method source on a fresh VM.
func compileMethodSrc(t *testing.T, source string) (*vm.VM, *vm.CompiledMethod) {
	t.Helper()
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(Compile)
	method, err := vmInst.Compile(source, nil)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	return vmInst, method
}

func runMethodSrc(t *testing.T, source string) vm.Value {
	t.Helper()
	vmInst, method := compileMethodSrc(t, source)
	return vmInst.Execute(method, vm.Nil, nil)
}

func wantSmallInt(t *testing.T, got vm.Value, want int64) {
	t.Helper()
	if !got.IsSmallInt() || got.SmallInt() != want {
		t.Errorf("result = %v, want %d", got, want)
	}
}

func TestInline_IfTrue(t *testing.T) {
	wantSmallInt(t, runMethodSrc(t, "test ^true ifTrue: [42]"), 42)
	if got := runMethodSrc(t, "test ^false ifTrue: [42]"); !got.IsNil() {
		t.Errorf("false ifTrue: = %v, want nil", got)
	}
}

func TestInline_IfFalse(t *testing.T) {
	wantSmallInt(t, runMethodSrc(t, "test ^false ifFalse: [7]"), 7)
	if got := runMethodSrc(t, "test ^true ifFalse: [7]"); !got.IsNil() {
		t.Errorf("true ifFalse: = %v, want nil", got)
	}
}

func TestInline_IfTrueIfFalse(t *testing.T) {
	wantSmallInt(t, runMethodSrc(t, "test ^true ifTrue: [1] ifFalse: [2]"), 1)
	wantSmallInt(t, runMethodSrc(t, "test ^false ifTrue: [1] ifFalse: [2]"), 2)
	wantSmallInt(t, runMethodSrc(t, "test ^true ifFalse: [1] ifTrue: [2]"), 2)
	wantSmallInt(t, runMethodSrc(t, "test ^false ifFalse: [1] ifTrue: [2]"), 1)
}

func TestInline_ConditionFromExpression(t *testing.T) {
	wantSmallInt(t, runMethodSrc(t, "test ^3 < 4 ifTrue: [10] ifFalse: [20]"), 10)
	wantSmallInt(t, runMethodSrc(t, "test ^3 > 4 ifTrue: [10] ifFalse: [20]"), 20)
}

func TestInline_MultiStatementBody(t *testing.T) {
	// Intermediate statement results must be popped; last value wins.
	wantSmallInt(t, runMethodSrc(t, "test ^true ifTrue: [1 + 1. 2 + 2. 3 + 3]"), 6)
}

func TestInline_EmptyBlock(t *testing.T) {
	if got := runMethodSrc(t, "test ^true ifTrue: []"); !got.IsNil() {
		t.Errorf("empty ifTrue: body = %v, want nil", got)
	}
}

func TestInline_AndOr(t *testing.T) {
	if got := runMethodSrc(t, "test ^true and: [true]"); got != vm.True {
		t.Errorf("true and: [true] = %v", got)
	}
	if got := runMethodSrc(t, "test ^true and: [false]"); got != vm.False {
		t.Errorf("true and: [false] = %v", got)
	}
	if got := runMethodSrc(t, "test ^false and: [true]"); got != vm.False {
		t.Errorf("false and: [true] = %v", got)
	}
	if got := runMethodSrc(t, "test ^false or: [true]"); got != vm.True {
		t.Errorf("false or: [true] = %v", got)
	}
	if got := runMethodSrc(t, "test ^true or: [false]"); got != vm.True {
		t.Errorf("true or: [false] = %v", got)
	}
}

func TestInline_AndOrShortCircuit(t *testing.T) {
	// The block must NOT be evaluated when the receiver short-circuits.
	src := `test
    | x |
    x := 0.
    false and: [x := 1. true].
    true or: [x := 2. false].
    ^x`
	wantSmallInt(t, runMethodSrc(t, src), 0)
}

func TestInline_WhileTrue(t *testing.T) {
	src := `test
    | sum i |
    sum := 0.
    i := 1.
    [i <= 10] whileTrue: [sum := sum + i. i := i + 1].
    ^sum`
	wantSmallInt(t, runMethodSrc(t, src), 55)
}

func TestInline_WhileFalse(t *testing.T) {
	src := `test
    | i |
    i := 0.
    [i >= 5] whileFalse: [i := i + 1].
    ^i`
	wantSmallInt(t, runMethodSrc(t, src), 5)
}

func TestInline_UnaryWhileTrue(t *testing.T) {
	src := `test
    | i |
    i := 0.
    [i := i + 1. i < 4] whileTrue.
    ^i`
	wantSmallInt(t, runMethodSrc(t, src), 4)
}

func TestInline_WhileResultIsNil(t *testing.T) {
	src := `test
    | i |
    i := 0.
    ^[i < 1] whileTrue: [i := i + 1]`
	if got := runMethodSrc(t, src); !got.IsNil() {
		t.Errorf("whileTrue: result = %v, want nil", got)
	}
}

func TestInline_CaretInInlinedBranch(t *testing.T) {
	// ^ inside an inlined block is a plain method return.
	wantSmallInt(t, runMethodSrc(t, "test true ifTrue: [^42]. ^7"), 42)
	wantSmallInt(t, runMethodSrc(t, "test false ifTrue: [^42]. ^7"), 7)
}

func TestInline_NestedInsideWhile(t *testing.T) {
	src := `test
    | i evens |
    i := 0.
    evens := 0.
    [i < 10] whileTrue: [
        i \\ 2 = 0 ifTrue: [evens := evens + 1].
        i := i + 1].
    ^evens`
	wantSmallInt(t, runMethodSrc(t, src), 5)
}

func TestInline_FallbackBlockInVariable(t *testing.T) {
	// A block held in a variable is not a literal block: the send must
	// dispatch to the Boolean primitive and still work.
	src := `test
    | b |
    b := [3].
    ^true ifTrue: b`
	wantSmallInt(t, runMethodSrc(t, src), 3)
}

func TestInline_FallbackBlockWithTemps(t *testing.T) {
	// Blocks with temps are not inlined; the closure path must still work.
	src := `test
    ^true ifTrue: [ | x | x := 5. x + 1 ]`
	wantSmallInt(t, runMethodSrc(t, src), 6)
}

func TestInline_MustBeBoolean(t *testing.T) {
	vmInst, method := compileMethodSrc(t, "test ^nil ifTrue: [1]")

	defer func() {
		r := recover()
		if r == nil {
			t.Fatal("expected mustBeBoolean signal for nil receiver")
		}
		se, ok := r.(vm.SignaledException)
		if !ok {
			t.Fatalf("expected SignaledException, got %T: %v", r, r)
		}
		msg := vmInst.Registry().GetStringContent(se.Object.MessageText)
		if !strings.Contains(msg, "mustBeBoolean") {
			t.Errorf("message = %q, want mustBeBoolean", msg)
		}
	}()
	vmInst.Execute(method, vm.Nil, nil)
}

// ---------------------------------------------------------------------------
// Control-flow inlining: bytecode shape
// ---------------------------------------------------------------------------

func countOpcode(bc []byte, target vm.Opcode) int {
	n := 0
	for i := 0; i < len(bc); {
		op := vm.Opcode(bc[i])
		if op == target {
			n++
		}
		i += 1 + op.Info().OperandBytes
	}
	return n
}

func TestInline_BytecodeShape_If(t *testing.T) {
	_, method := compileMethodSrc(t, "test ^true ifTrue: [1] ifFalse: [2]")

	if n := countOpcode(method.Bytecode, vm.OpCreateBlock); n != 0 {
		t.Errorf("expected no OpCreateBlock, got %d\n%s", n, vm.Disassemble(method.Bytecode))
	}
	if len(method.Blocks) != 0 {
		t.Errorf("expected no BlockMethods, got %d", len(method.Blocks))
	}
	if n := countOpcode(method.Bytecode, vm.OpJumpFalse); n != 1 {
		t.Errorf("expected 1 OpJumpFalse, got %d\n%s", n, vm.Disassemble(method.Bytecode))
	}
	if n := countOpcode(method.Bytecode, vm.OpSend); n != 0 {
		t.Errorf("expected no OpSend, got %d\n%s", n, vm.Disassemble(method.Bytecode))
	}
}

func TestInline_BytecodeShape_While(t *testing.T) {
	src := `test
    | i |
    i := 0.
    [i < 10] whileTrue: [i := i + 1].
    ^i`
	_, method := compileMethodSrc(t, src)

	if n := countOpcode(method.Bytecode, vm.OpCreateBlock); n != 0 {
		t.Errorf("expected no OpCreateBlock, got %d\n%s", n, vm.Disassemble(method.Bytecode))
	}
	if n := countOpcode(method.Bytecode, vm.OpJump); n != 1 {
		t.Errorf("expected 1 backward OpJump, got %d\n%s", n, vm.Disassemble(method.Bytecode))
	}
	if n := countOpcode(method.Bytecode, vm.OpJumpFalse); n != 1 {
		t.Errorf("expected 1 OpJumpFalse, got %d\n%s", n, vm.Disassemble(method.Bytecode))
	}
}

func TestInline_BytecodeShape_FallbackKeepsSend(t *testing.T) {
	// Non-literal receiver: whileTrue: must stay a send.
	src := `test
    | b |
    b := [true].
    ^b whileTrue: []`
	_, method := compileMethodSrc(t, src)

	if n := countOpcode(method.Bytecode, vm.OpSend); n == 0 {
		t.Errorf("expected OpSend fallback for non-literal receiver\n%s", vm.Disassemble(method.Bytecode))
	}
}

// Inlined conditionals need no VM at all — proof that no dispatch happens.
func TestInline_RunsWithoutVM(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	method, err := CompileExpr("3 < 4 ifTrue: [10] ifFalse: [20]", selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	interp := vm.NewInterpreter()
	interp.Selectors = selectors
	wantSmallInt(t, interp.Execute(method, vm.Nil, nil), 10)
}

// ---------------------------------------------------------------------------
// Benchmarks: inlined control flow vs dispatch fallback
// ---------------------------------------------------------------------------

func benchCompile(b *testing.B, source string) (*vm.VM, *vm.CompiledMethod) {
	b.Helper()
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(Compile)
	method, err := vmInst.Compile(source, nil)
	if err != nil {
		b.Fatalf("compile error: %v", err)
	}
	return vmInst, method
}

// A 1000-iteration counting loop with literal blocks: fully inlined to jumps.
func BenchmarkWhileLoop_Inlined(b *testing.B) {
	vmInst, method := benchCompile(b, `bench
    | i |
    i := 0.
    [i < 1000] whileTrue: [i := i + 1].
    ^i`)
	b.ResetTimer()
	for n := 0; n < b.N; n++ {
		vmInst.Execute(method, vm.Nil, nil)
	}
}

// The same loop with blocks held in variables: falls back to whileTrue:
// dispatch + block evaluation every iteration (the pre-inlining cost).
func BenchmarkWhileLoop_Dispatch(b *testing.B) {
	vmInst, method := benchCompile(b, `bench
    | i c blk |
    i := 0.
    c := [i < 1000].
    blk := [i := i + 1].
    c whileTrue: blk.
    ^i`)
	b.ResetTimer()
	for n := 0; n < b.N; n++ {
		vmInst.Execute(method, vm.Nil, nil)
	}
}

// Conditional in a loop: inlined ifTrue:ifFalse:.
func BenchmarkConditional_Inlined(b *testing.B) {
	vmInst, method := benchCompile(b, `bench
    | i acc |
    i := 0.
    acc := 0.
    [i < 1000] whileTrue: [
        acc := i \\ 2 = 0 ifTrue: [acc + 1] ifFalse: [acc].
        i := i + 1].
    ^acc`)
	b.ResetTimer()
	for n := 0; n < b.N; n++ {
		vmInst.Execute(method, vm.Nil, nil)
	}
}
