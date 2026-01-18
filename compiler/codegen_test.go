package compiler

import (
	"testing"

	"github.com/chazu/maggie/vm"
)

func TestCompileInteger(t *testing.T) {
	selectors := vm.NewSelectorTable()
	symbols := vm.NewSymbolTable()

	method, err := CompileExpr("42", selectors, symbols)
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

	method, err := CompileExpr("-5", selectors, symbols)
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

	method, err := CompileExpr("3.14", selectors, symbols)
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

	method, err := CompileExpr("nil", selectors, symbols)
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

	method, err := CompileExpr("true", selectors, symbols)
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

	method, err := CompileExpr("false", selectors, symbols)
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

	method, err := CompileExpr("1 + 2", selectors, symbols)
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

	method, err := CompileExpr("10 - 3", selectors, symbols)
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

	method, err := CompileExpr("6 * 7", selectors, symbols)
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

	method, err := CompileExpr("5 < 10", selectors, symbols)
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

	// Smalltalk: 1 + 2 * 3 = (1 + 2) * 3 = 9
	method, err := CompileExpr("1 + 2 * 3", selectors, symbols)
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

	// (1 + 2) * 3 = 9
	method, err := CompileExpr("(1 + 2) * 3", selectors, symbols)
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

	method, err := Compile("double ^self * 2", selectors, symbols)
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

	method, err := Compile("add: x ^self + x", selectors, symbols)
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

	source := "compute | result | result := 42. ^result"
	method, err := Compile(source, selectors, symbols)
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

	method, err := Compile("at: index put: value ^self", selectors, symbols)
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

	method, err := CompileExpr("#foo", selectors, symbols)
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

	// Compile a method that creates and returns a block
	method, err := CompileExpr("[42]", selectors, symbols)
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

	// 2 + 3 * 4 = (2 + 3) * 4 = 20 (Smalltalk left-to-right)
	method, err := CompileExpr("2 + 3 * 4", selectors, symbols)
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

	// Method with temp and assignment
	source := "test | x | x := 42. ^x"
	method, err := Compile(source, selectors, symbols)
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

	// Method that uses its argument
	source := "square: n ^n * n"
	method, err := Compile(source, selectors, symbols)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	vmInst := vm.NewVM()
	result := vmInst.Execute(method, vm.Nil, []vm.Value{vm.FromSmallInt(5)})

	if !result.IsSmallInt() || result.SmallInt() != 25 {
		t.Errorf("5 squared = %v, want 25", result)
	}
}
