package main

import (
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// Phase 5: Inspector Window Tests
// Tests for evaluate:in: (mini evaluator) and object introspection.
// Uses built-in types (Array, Dictionary, SmallInteger) since class
// definitions can't be created via evaluate: in the cmd/bootstrap package.
// ---------------------------------------------------------------------------

// Test evaluate:in: with Array as receiver — slot access pattern.
func TestInspector_EvaluateInArray(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	arr := vmInst.NewArrayWithElements([]vm.Value{
		vm.FromSmallInt(10), vm.FromSmallInt(20), vm.FromSmallInt(30),
	})

	// self size
	result := vmInst.Send(compilerClass, "evaluate:in:", []vm.Value{
		vmInst.Registry().NewStringValue("self size"), arr,
	})
	if !result.IsSmallInt() || result.SmallInt() != 3 {
		t.Errorf("Array 'self size' expected 3, got: %v", result)
	}

	// self at: 0 (0-based indexing — Go primitives, no library loaded)
	result = vmInst.Send(compilerClass, "evaluate:in:", []vm.Value{
		vmInst.Registry().NewStringValue("self at: 0"), arr,
	})
	if !result.IsSmallInt() || result.SmallInt() != 10 {
		t.Errorf("Array 'self at: 0' expected 10, got: %v", result)
	}

	// self at: 2
	result = vmInst.Send(compilerClass, "evaluate:in:", []vm.Value{
		vmInst.Registry().NewStringValue("self at: 2"), arr,
	})
	if !result.IsSmallInt() || result.SmallInt() != 30 {
		t.Errorf("Array 'self at: 2' expected 30, got: %v", result)
	}
}

// Test evaluate:in: with Dictionary as receiver — key access pattern.
func TestInspector_EvaluateInDictionary(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Create a dictionary
	dict := vmInst.Registry().NewDictionaryValue()
	dictObj := vmInst.Registry().GetDictionaryObject(dict)

	nameKey := vmInst.Symbols.SymbolValue("name")
	h := vm.HashValue(vmInst.Registry(),nameKey)
	dictObj.Data[h] = vmInst.Registry().NewStringValue("Alice")
	dictObj.Keys[h] = nameKey

	ageKey := vmInst.Symbols.SymbolValue("age")
	h2 := vm.HashValue(vmInst.Registry(),ageKey)
	dictObj.Data[h2] = vm.FromSmallInt(30)
	dictObj.Keys[h2] = ageKey

	// self size
	result := vmInst.Send(compilerClass, "evaluate:in:", []vm.Value{
		vmInst.Registry().NewStringValue("self size"), dict,
	})
	if !result.IsSmallInt() || result.SmallInt() != 2 {
		t.Errorf("Dictionary 'self size' expected 2, got: %v", result)
	}

	// self at: #age
	result = vmInst.Send(compilerClass, "evaluate:in:", []vm.Value{
		vmInst.Registry().NewStringValue("self at: #age"), dict,
	})
	if !result.IsSmallInt() || result.SmallInt() != 30 {
		t.Errorf("Dictionary 'self at: #age' expected 30, got: %v", result)
	}
}

// Test evaluate:in: with SmallInteger as receiver — primitive inspection.
func TestInspector_EvaluateInSmallInteger(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Evaluate 'self + 1' with 42 as receiver
	result := vmInst.Send(compilerClass, "evaluate:in:", []vm.Value{
		vmInst.Registry().NewStringValue("self + 1"), vm.FromSmallInt(42),
	})
	if !result.IsSmallInt() || result.SmallInt() != 43 {
		t.Errorf("SmallInteger 'self + 1' expected 43, got: %v", result)
	}
}

// Test evaluate:in: with nil as receiver.
func TestInspector_EvaluateInNil(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	result := vmInst.Send(compilerClass, "evaluate:in:", []vm.Value{
		vmInst.Registry().NewStringValue("self isNil"), vm.Nil,
	})
	if result != vm.True {
		t.Errorf("Nil 'self isNil' expected true, got: %v", result)
	}
}

// Test className reflection on built-in types.
func TestInspector_ClassName(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	tests := []struct {
		value    vm.Value
		expected string
	}{
		{vm.FromSmallInt(42), "SmallInteger"},
		{vm.True, "True"},
		{vm.False, "False"},
	}

	for _, tt := range tests {
		result := vmInst.Send(tt.value, "className", nil)
		if !result.IsSymbol() {
			t.Errorf("className for %v: expected symbol, got: %v", tt.expected, result)
			continue
		}
		name := vmInst.Symbols.Name(result.SymbolID())
		if name != tt.expected {
			t.Errorf("className expected %q, got %q", tt.expected, name)
		}
	}
}

// Test evaluate:in: with expression that uses self methods — the mini evaluator pattern.
func TestInspector_MiniEvaluator_ArithmeticOnSelf(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	arr := vmInst.NewArrayWithElements([]vm.Value{
		vm.FromSmallInt(10), vm.FromSmallInt(20), vm.FromSmallInt(30),
	})

	// More complex expression: self size * 2
	result := vmInst.Send(compilerClass, "evaluate:in:", []vm.Value{
		vmInst.Registry().NewStringValue("self size * 2"), arr,
	})
	if !result.IsSmallInt() || result.SmallInt() != 6 {
		t.Errorf("'self size * 2' expected 6, got: %v", result)
	}
}

// Test evaluate:in: with className — reflection on receiver type.
func TestInspector_EvaluateInContext_ClassName(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Evaluate 'self className' on a SmallInteger
	result := vmInst.Send(compilerClass, "evaluate:in:", []vm.Value{
		vmInst.Registry().NewStringValue("self className"), vm.FromSmallInt(42),
	})

	if !result.IsSymbol() {
		t.Fatalf("Expected symbol from className, got: %v", result)
	}
	name := vmInst.Symbols.Name(result.SymbolID())
	if name != "SmallInteger" {
		t.Errorf("Expected 'SmallInteger', got: %q", name)
	}
}

// Test evaluate:in: with error — evaluating invalid expression returns a Failure.
func TestInspector_EvaluateInContext_Error(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Evaluate a syntax error
	result := vmInst.Send(compilerClass, "evaluate:in:", []vm.Value{
		vmInst.Registry().NewStringValue("self +++"), vm.FromSmallInt(42),
	})

	// Should get a Failure (Result value), not crash
	// Check it's not nil and not a SmallInt (it should be a Result object)
	if result == vm.Nil {
		t.Error("Expected a Failure result, got nil")
	}
}
