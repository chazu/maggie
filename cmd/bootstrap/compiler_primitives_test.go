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

func TestCompilerSetGetGlobal(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Set a global using setGlobal:to:
	nameVal := vmInst.Symbols.SymbolValue("testVar")
	valueVal := vm.FromSmallInt(99)
	result := vmInst.Send(compilerClass, "setGlobal:to:", []vm.Value{nameVal, valueVal})

	if !result.IsSmallInt() || result.SmallInt() != 99 {
		t.Fatalf("setGlobal:to: should return the value, got: %v", result)
	}

	// Get the global using getGlobal:
	result = vmInst.Send(compilerClass, "getGlobal:", []vm.Value{nameVal})
	if !result.IsSmallInt() || result.SmallInt() != 99 {
		t.Errorf("getGlobal: expected 99, got: %v", result)
	}

	// Verify it's accessible via evaluate:
	result = vmInst.Send(compilerClass, "evaluate:", []vm.Value{vm.NewStringValue("testVar + 1")})
	if !result.IsSmallInt() || result.SmallInt() != 100 {
		t.Errorf("evaluate: testVar + 1 expected 100, got: %v", result)
	}
}

func TestCompilerEvaluatePersistentGlobals(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// First evaluation: assign x := 42
	result1 := vmInst.Send(compilerClass, "evaluate:", []vm.Value{vm.NewStringValue("x := 42")})
	if !result1.IsSmallInt() {
		t.Fatalf("First evaluation: expected SmallInt, got: %v", result1)
	}
	if result1.SmallInt() != 42 {
		t.Errorf("First evaluation: expected 42, got: %d", result1.SmallInt())
	}

	// Second evaluation: use x (should persist from first evaluation)
	result2 := vmInst.Send(compilerClass, "evaluate:", []vm.Value{vm.NewStringValue("x + 1")})
	if !result2.IsSmallInt() {
		t.Fatalf("Second evaluation: expected SmallInt, got: %v", result2)
	}
	if result2.SmallInt() != 43 {
		t.Errorf("Second evaluation: expected 43, got: %d", result2.SmallInt())
	}

	// Third evaluation: modify x (x is still 42, so 42 * 2 = 84)
	result3 := vmInst.Send(compilerClass, "evaluate:", []vm.Value{vm.NewStringValue("x := x * 2")})
	if !result3.IsSmallInt() {
		t.Fatalf("Third evaluation: expected SmallInt, got: %v", result3)
	}
	if result3.SmallInt() != 84 {
		t.Errorf("Third evaluation: expected 84, got: %d", result3.SmallInt())
	}

	// Fourth evaluation: verify x was updated
	result4 := vmInst.Send(compilerClass, "evaluate:", []vm.Value{vm.NewStringValue("x")})
	if !result4.IsSmallInt() {
		t.Fatalf("Fourth evaluation: expected SmallInt, got: %v", result4)
	}
	if result4.SmallInt() != 84 {
		t.Errorf("Fourth evaluation: expected 84, got: %d", result4.SmallInt())
	}
}

// TestBinaryOperatorFallbackToVTable verifies that optimized binary opcodes
// (OpSendLT, OpSendGT, etc.) fall back to VTable lookup for non-primitive types.
// This was a bug where 'abc' < 'abd' returned nil because OpSendLT only
// handled SmallInt and Float, ignoring methods loaded from .mag files.
func TestBinaryOperatorFallbackToVTable(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	// Add comparison methods to String class (simulating what String.mag does)
	strClass := vmInst.StringClass
	strClass.AddMethod1(vmInst.Selectors, "<", func(_ interface{}, recv vm.Value, other vm.Value) vm.Value {
		s1 := vm.GetStringContent(recv)
		s2 := vm.GetStringContent(other)
		if s1 < s2 {
			return vm.True
		}
		return vm.False
	})
	strClass.AddMethod1(vmInst.Selectors, ">", func(_ interface{}, recv vm.Value, other vm.Value) vm.Value {
		s1 := vm.GetStringContent(recv)
		s2 := vm.GetStringContent(other)
		if s1 > s2 {
			return vm.True
		}
		return vm.False
	})
	strClass.AddMethod1(vmInst.Selectors, "<=", func(_ interface{}, recv vm.Value, other vm.Value) vm.Value {
		s1 := vm.GetStringContent(recv)
		s2 := vm.GetStringContent(other)
		if s1 <= s2 {
			return vm.True
		}
		return vm.False
	})
	strClass.AddMethod1(vmInst.Selectors, ">=", func(_ interface{}, recv vm.Value, other vm.Value) vm.Value {
		s1 := vm.GetStringContent(recv)
		s2 := vm.GetStringContent(other)
		if s1 >= s2 {
			return vm.True
		}
		return vm.False
	})

	compilerClass := vmInst.Globals["Compiler"]

	tests := []struct {
		expr     string
		expected vm.Value
	}{
		{"'abc' < 'abd'", vm.True},
		{"'abd' < 'abc'", vm.False},
		{"'abc' < 'abc'", vm.False},
		{"'xyz' > 'abc'", vm.True},
		{"'abc' > 'xyz'", vm.False},
		{"'abc' <= 'abd'", vm.True},
		{"'abc' <= 'abc'", vm.True},
		{"'abd' <= 'abc'", vm.False},
		{"'abd' >= 'abc'", vm.True},
		{"'abc' >= 'abc'", vm.True},
		{"'abc' >= 'abd'", vm.False},
	}

	for _, tt := range tests {
		result := vmInst.Send(compilerClass, "evaluate:", []vm.Value{vm.NewStringValue(tt.expr)})
		if result != tt.expected {
			t.Errorf("%s: got %v, want %v", tt.expr, result, tt.expected)
		}
	}

	// Also verify numeric operators still work (fast path preserved)
	numTests := []struct {
		expr     string
		expected vm.Value
	}{
		{"3 < 5", vm.True},
		{"5 < 3", vm.False},
		{"5 > 3", vm.True},
		{"3 > 5", vm.False},
	}

	for _, tt := range numTests {
		result := vmInst.Send(compilerClass, "evaluate:", []vm.Value{vm.NewStringValue(tt.expr)})
		if result != tt.expected {
			t.Errorf("%s: got %v, want %v", tt.expr, result, tt.expected)
		}
	}
}
