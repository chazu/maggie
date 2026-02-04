package compiler

import (
	"testing"

	"github.com/chazu/maggie/vm"
)

// newTestRegistry creates a fresh ObjectRegistry for compiler tests.
func newTestRegistry() *vm.ObjectRegistry {
	return vm.NewObjectRegistry()
}

func TestCompileInteger(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	method, err := CompileExpr("42", selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	// Execute
	interp := vm.NewInterpreter()
	interp.Selectors = selectors
	result := interp.Execute(method, vm.Nil, nil)

	if !result.IsSmallInt() || result.SmallInt() != 42 {
		t.Errorf("result = %v, want 42", result)
	}
}

func TestCompileNegativeInteger(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	method, err := CompileExpr("-5", selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	interp := vm.NewInterpreter()
	interp.Selectors = selectors
	result := interp.Execute(method, vm.Nil, nil)

	if !result.IsSmallInt() || result.SmallInt() != -5 {
		t.Errorf("result = %v, want -5", result)
	}
}

func TestCompileFloat(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	method, err := CompileExpr("3.14", selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	interp := vm.NewInterpreter()
	interp.Selectors = selectors
	result := interp.Execute(method, vm.Nil, nil)

	if !result.IsFloat() || result.Float64() != 3.14 {
		t.Errorf("result = %v, want 3.14", result)
	}
}

func TestCompileNil(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	method, err := CompileExpr("nil", selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	interp := vm.NewInterpreter()
	interp.Selectors = selectors
	result := interp.Execute(method, vm.Nil, nil)

	if result != vm.Nil {
		t.Errorf("result = %v, want nil", result)
	}
}

func TestCompileTrue(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	method, err := CompileExpr("true", selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	interp := vm.NewInterpreter()
	interp.Selectors = selectors
	result := interp.Execute(method, vm.Nil, nil)

	if result != vm.True {
		t.Errorf("result = %v, want true", result)
	}
}

func TestCompileFalse(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	method, err := CompileExpr("false", selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	interp := vm.NewInterpreter()
	interp.Selectors = selectors
	result := interp.Execute(method, vm.Nil, nil)

	if result != vm.False {
		t.Errorf("result = %v, want false", result)
	}
}

func TestCompileBinaryAdd(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	method, err := CompileExpr("1 + 2", selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	// Need VM with primitives for arithmetic
	vmInst := vm.NewVM()
	result := vmInst.Execute(method, vm.Nil, nil)

	if !result.IsSmallInt() || result.SmallInt() != 3 {
		t.Errorf("result = %v, want 3", result)
	}
}

func TestCompileBinarySubtract(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	method, err := CompileExpr("10 - 3", selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	vmInst := vm.NewVM()
	result := vmInst.Execute(method, vm.Nil, nil)

	if !result.IsSmallInt() || result.SmallInt() != 7 {
		t.Errorf("result = %v, want 7", result)
	}
}

func TestCompileBinaryMultiply(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	method, err := CompileExpr("6 * 7", selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	vmInst := vm.NewVM()
	result := vmInst.Execute(method, vm.Nil, nil)

	if !result.IsSmallInt() || result.SmallInt() != 42 {
		t.Errorf("result = %v, want 42", result)
	}
}

func TestCompileBinaryCompare(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	method, err := CompileExpr("5 < 10", selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	vmInst := vm.NewVM()
	result := vmInst.Execute(method, vm.Nil, nil)

	if result != vm.True {
		t.Errorf("5 < 10 = %v, want true", result)
	}
}

func TestCompileBinaryChain(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	// Smalltalk: 1 + 2 * 3 = (1 + 2) * 3 = 9
	method, err := CompileExpr("1 + 2 * 3", selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	vmInst := vm.NewVM()
	result := vmInst.Execute(method, vm.Nil, nil)

	if !result.IsSmallInt() || result.SmallInt() != 9 {
		t.Errorf("result = %v, want 9", result)
	}
}

func TestCompileParenExpr(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	// (1 + 2) * 3 = 9
	method, err := CompileExpr("(1 + 2) * 3", selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	vmInst := vm.NewVM()
	result := vmInst.Execute(method, vm.Nil, nil)

	if !result.IsSmallInt() || result.SmallInt() != 9 {
		t.Errorf("result = %v, want 9", result)
	}
}

func TestCompileMethod(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	method, err := Compile("double ^self * 2", selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	if method.Name() != "double" {
		t.Errorf("selector = %q, want double", method.Name())
	}
	if method.Arity != 0 {
		t.Errorf("arity = %d, want 0", method.Arity)
	}
}

func TestCompileMethodWithArgs(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	method, err := Compile("add: x ^self + x", selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	if method.Name() != "add:" {
		t.Errorf("selector = %q, want add:", method.Name())
	}
	if method.Arity != 1 {
		t.Errorf("arity = %d, want 1", method.Arity)
	}
}

func TestCompileMethodWithTemps(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	source := "compute | result | result := 42. ^result"
	method, err := Compile(source, selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	if method.NumTemps != 1 {
		t.Errorf("numTemps = %d, want 1", method.NumTemps)
	}
}

func TestCompileKeywordMethod(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	method, err := Compile("at: index put: value ^self", selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	if method.Name() != "at:put:" {
		t.Errorf("selector = %q, want at:put:", method.Name())
	}
	if method.Arity != 2 {
		t.Errorf("arity = %d, want 2", method.Arity)
	}
}

func TestCompileSymbol(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	method, err := CompileExpr("#foo", selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	interp := vm.NewInterpreter()
	interp.Selectors = selectors
	result := interp.Execute(method, vm.Nil, nil)

	if !result.IsSymbol() {
		t.Errorf("result is not a symbol")
	}
	if symbols.Name(result.SymbolID()) != "foo" {
		t.Errorf("symbol = %q, want foo", symbols.Name(result.SymbolID()))
	}
}

func TestCompileBlock(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	// Compile a method that creates and returns a block
	method, err := CompileExpr("[42]", selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	// The method should have one block
	if len(method.Blocks) != 1 {
		t.Errorf("blocks count = %d, want 1", len(method.Blocks))
	}
}

func TestCompileComplexExpression(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	// 2 + 3 * 4 = (2 + 3) * 4 = 20 (Smalltalk left-to-right)
	method, err := CompileExpr("2 + 3 * 4", selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	vmInst := vm.NewVM()
	result := vmInst.Execute(method, vm.Nil, nil)

	if !result.IsSmallInt() || result.SmallInt() != 20 {
		t.Errorf("result = %v, want 20", result)
	}
}

func TestCompileAssignment(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	// Method with temp and assignment
	source := "test | x | x := 42. ^x"
	method, err := Compile(source, selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	vmInst := vm.NewVM()
	result := vmInst.Execute(method, vm.Nil, nil)

	if !result.IsSmallInt() || result.SmallInt() != 42 {
		t.Errorf("result = %v, want 42", result)
	}
}

func TestCompileUseArg(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	// Method that uses its argument
	source := "square: n ^n * n"
	method, err := Compile(source, selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	vmInst := vm.NewVM()
	result := vmInst.Execute(method, vm.Nil, []vm.Value{vm.FromSmallInt(5)})

	if !result.IsSmallInt() || result.SmallInt() != 25 {
		t.Errorf("5 squared = %v, want 25", result)
	}
}

// TestMutableCapture verifies that nested blocks can mutate captured variables.
func TestMutableCapture(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(Compile)

	// Test simple capture mutation: x := 10, [x := 30], return x (should be 30)
	source := `test
    true ifTrue: [
        | x |
        x := 10.
        [x := 30] value.
        ^x
    ].
    ^0`

	method, err := vmInst.Compile(source, nil)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	result := vmInst.Execute(method, vm.Nil, nil)
	if !result.IsSmallInt() || result.SmallInt() != 30 {
		t.Errorf("result = %v, want 30", result)
	}
}

// TestMutableCaptureReadWrite verifies captured variables can be read and written.
func TestMutableCaptureReadWrite(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(Compile)

	// x := 10, [x := x + 20], return x (should be 30)
	source := `test
    true ifTrue: [
        | x |
        x := 10.
        [x := x + 20] value.
        ^x
    ].
    ^0`

	method, err := vmInst.Compile(source, nil)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	result := vmInst.Execute(method, vm.Nil, nil)
	if !result.IsSmallInt() || result.SmallInt() != 30 {
		t.Errorf("result = %v, want 30", result)
	}
}

// TestMutableCaptureMultipleVariables verifies multiple captured variables work independently.
func TestMutableCaptureMultipleVariables(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(Compile)

	// a := 10, b := 20, [a := a + b], return a (should be 30)
	source := `test
    true ifTrue: [
        | a b |
        a := 10.
        b := 20.
        [a := a + b] value.
        ^a
    ].
    ^0`

	method, err := vmInst.Compile(source, nil)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	result := vmInst.Execute(method, vm.Nil, nil)
	if !result.IsSmallInt() || result.SmallInt() != 30 {
		t.Errorf("result = %v, want 30", result)
	}
}

// TestMutableCaptureIteration simulates iteration with multiple block evaluations.
func TestMutableCaptureIteration(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(Compile)

	// Simulate summing 1+2+3 = 6
	source := `test
    true ifTrue: [
        | sum i |
        sum := 0.
        i := 1.
        [sum := sum + i. i := i + 1] value.
        [sum := sum + i. i := i + 1] value.
        [sum := sum + i. i := i + 1] value.
        ^sum
    ].
    ^0`

	method, err := vmInst.Compile(source, nil)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	result := vmInst.Execute(method, vm.Nil, nil)
	if !result.IsSmallInt() || result.SmallInt() != 6 {
		t.Errorf("result = %v, want 6", result)
	}
}

// ---------------------------------------------------------------------------
// Tail-call optimization codegen tests
// ---------------------------------------------------------------------------

// TestCompileTailCallEmitsTailSend verifies that the compiler emits
// OpTailSend for a self-recursive keyword send in tail position.
func TestCompileTailCallEmitsTailSend(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	// A self-recursive method: factorial: n acc: acc
	//   n = 0 ifTrue: [^acc].
	//   ^self factorial: n - 1 acc: acc * n
	method := &MethodDef{
		Selector:   "factorial:acc:",
		Parameters: []string{"n", "acc"},
		Statements: []Stmt{
			// n = 0 ifTrue: [^acc]
			&ExprStmt{Expr: &KeywordMessage{
				Receiver: &BinaryMessage{
					Receiver: &Variable{Name: "n"},
					Selector: "=",
					Argument: &IntLiteral{Value: 0},
				},
				Selector: "ifTrue:",
				Keywords: []string{"ifTrue:"},
				Arguments: []Expr{
					&Block{
						Statements: []Stmt{
							&Return{Value: &Variable{Name: "acc"}},
						},
					},
				},
			}},
			// ^self factorial: n - 1 acc: acc * n
			&Return{Value: &KeywordMessage{
				Receiver:  &Self{},
				Selector:  "factorial:acc:",
				Keywords:  []string{"factorial:", "acc:"},
				Arguments: []Expr{
					&BinaryMessage{
						Receiver: &Variable{Name: "n"},
						Selector: "-",
						Argument: &IntLiteral{Value: 1},
					},
					&BinaryMessage{
						Receiver: &Variable{Name: "acc"},
						Selector: "*",
						Argument: &Variable{Name: "n"},
					},
				},
			}},
		},
	}

	compiled, err := CompileMethodDef(method, selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	// Scan bytecode for OpTailSend
	found := false
	for idx := 0; idx < len(compiled.Bytecode); idx++ {
		if vm.Opcode(compiled.Bytecode[idx]) == vm.OpTailSend {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected OpTailSend in bytecode for self-recursive tail call, but not found")
		t.Logf("bytecode: %v", vm.Disassemble(compiled.Bytecode))
	}
}

// TestCompileNonSelfSendNoTailSend verifies that the compiler does NOT
// emit OpTailSend for a send to a non-self receiver, even if it's in
// tail position with the same selector.
func TestCompileNonSelfSendNoTailSend(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	// A method that sends to a variable, not self:
	// forward: other [ ^other forward: other ]
	method := &MethodDef{
		Selector:   "forward:",
		Parameters: []string{"other"},
		Statements: []Stmt{
			&Return{Value: &KeywordMessage{
				Receiver:  &Variable{Name: "other"},
				Selector:  "forward:",
				Keywords:  []string{"forward:"},
				Arguments: []Expr{&Variable{Name: "other"}},
			}},
		},
	}

	compiled, err := CompileMethodDef(method, selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	// Should NOT contain OpTailSend (receiver is not self)
	for idx := 0; idx < len(compiled.Bytecode); idx++ {
		if vm.Opcode(compiled.Bytecode[idx]) == vm.OpTailSend {
			t.Error("expected NO OpTailSend for non-self receiver, but found one")
			t.Logf("bytecode: %v", vm.Disassemble(compiled.Bytecode))
			break
		}
	}
}

// TestCompileDifferentSelectorNoTailSend verifies that the compiler does NOT
// emit OpTailSend for a self send with a different selector than the method.
func TestCompileDifferentSelectorNoTailSend(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	// A method that calls a DIFFERENT method on self:
	// doWork [ ^self cleanup ]
	method := &MethodDef{
		Selector:   "doWork",
		Parameters: nil,
		Statements: []Stmt{
			&Return{Value: &UnaryMessage{
				Receiver: &Self{},
				Selector: "cleanup",
			}},
		},
	}

	compiled, err := CompileMethodDef(method, selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	// Should NOT contain OpTailSend (different selector)
	for idx := 0; idx < len(compiled.Bytecode); idx++ {
		if vm.Opcode(compiled.Bytecode[idx]) == vm.OpTailSend {
			t.Error("expected NO OpTailSend for different selector, but found one")
			t.Logf("bytecode: %v", vm.Disassemble(compiled.Bytecode))
			break
		}
	}
}

// TestCompileUnaryTailCall verifies OpTailSend is emitted for a unary
// self-recursive method in tail position.
func TestCompileUnaryTailCall(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()
	registry := newTestRegistry()

	// loop [ ^self loop ]
	method := &MethodDef{
		Selector:   "loop",
		Parameters: nil,
		Statements: []Stmt{
			&Return{Value: &UnaryMessage{
				Receiver: &Self{},
				Selector: "loop",
			}},
		},
	}

	compiled, err := CompileMethodDef(method, selectors, symbols, registry)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	found := false
	for idx := 0; idx < len(compiled.Bytecode); idx++ {
		if vm.Opcode(compiled.Bytecode[idx]) == vm.OpTailSend {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected OpTailSend for unary self-recursive tail call, but not found")
		t.Logf("bytecode: %v", vm.Disassemble(compiled.Bytecode))
	}
}
