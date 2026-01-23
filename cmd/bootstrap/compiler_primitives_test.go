package main

import (
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// Compiler Primitives Tests
// ---------------------------------------------------------------------------

func TestCompilerEvaluateSimpleInteger(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	// Get the Compiler class
	compilerClass, ok := vmInst.Globals["Compiler"]
	if !ok {
		t.Fatal("Compiler global not found")
	}

	// Call Compiler evaluate: '42'
	source := vm.NewStringValue("42")
	result := vmInst.Send(compilerClass, "evaluate:", []vm.Value{source})

	if !result.IsSmallInt() {
		t.Fatalf("Expected SmallInt, got: %v", result)
	}
	if result.SmallInt() != 42 {
		t.Errorf("Expected 42, got: %d", result.SmallInt())
	}
}

func TestCompilerEvaluateArithmetic(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]
	source := vm.NewStringValue("3 + 4")
	result := vmInst.Send(compilerClass, "evaluate:", []vm.Value{source})

	if !result.IsSmallInt() {
		t.Fatalf("Expected SmallInt, got: %v", result)
	}
	if result.SmallInt() != 7 {
		t.Errorf("Expected 7, got: %d", result.SmallInt())
	}
}

func TestCompilerEvaluateBoolean(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Test true
	result := vmInst.Send(compilerClass, "evaluate:", []vm.Value{vm.NewStringValue("true")})
	if result != vm.True {
		t.Errorf("Expected True, got: %v", result)
	}

	// Test false
	result = vmInst.Send(compilerClass, "evaluate:", []vm.Value{vm.NewStringValue("false")})
	if result != vm.False {
		t.Errorf("Expected False, got: %v", result)
	}

	// Test nil
	result = vmInst.Send(compilerClass, "evaluate:", []vm.Value{vm.NewStringValue("nil")})
	if result != vm.Nil {
		t.Errorf("Expected Nil, got: %v", result)
	}
}

func TestCompilerEvaluateComparison(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Test 5 > 3
	result := vmInst.Send(compilerClass, "evaluate:", []vm.Value{vm.NewStringValue("5 > 3")})
	if result != vm.True {
		t.Errorf("Expected True for 5 > 3, got: %v", result)
	}

	// Test 2 > 3
	result = vmInst.Send(compilerClass, "evaluate:", []vm.Value{vm.NewStringValue("2 > 3")})
	if result != vm.False {
		t.Errorf("Expected False for 2 > 3, got: %v", result)
	}
}

func TestCompilerEvaluateMessageSend(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Test negative
	result := vmInst.Send(compilerClass, "evaluate:", []vm.Value{vm.NewStringValue("5 negated")})
	if !result.IsSmallInt() {
		t.Fatalf("Expected SmallInt, got: %v", result)
	}
	if result.SmallInt() != -5 {
		t.Errorf("Expected -5, got: %d", result.SmallInt())
	}
}

func TestCompilerEvaluateArray(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Create an array and get its size
	result := vmInst.Send(compilerClass, "evaluate:", []vm.Value{vm.NewStringValue("(Array new: 3) size")})
	if !result.IsSmallInt() {
		t.Fatalf("Expected SmallInt, got: %v", result)
	}
	if result.SmallInt() != 3 {
		t.Errorf("Expected 3, got: %d", result.SmallInt())
	}
}

func TestCompilerEvaluateInContext(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Create an array to use as context
	arr := vmInst.NewArrayWithElements([]vm.Value{vm.FromSmallInt(10), vm.FromSmallInt(20), vm.FromSmallInt(30)})

	// Evaluate 'self size' in the context of the array
	result := vmInst.Send(compilerClass, "evaluate:in:", []vm.Value{vm.NewStringValue("self size"), arr})
	if !result.IsSmallInt() {
		t.Fatalf("Expected SmallInt, got: %v", result)
	}
	if result.SmallInt() != 3 {
		t.Errorf("Expected 3, got: %d", result.SmallInt())
	}
}
