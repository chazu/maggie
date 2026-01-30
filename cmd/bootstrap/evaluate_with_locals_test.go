package main

import (
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// Compiler evaluate:withLocals: Tests
// ---------------------------------------------------------------------------

func TestEvaluateWithLocals_ReadLocal(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Create a locals dictionary with x = 42
	locals := vm.NewDictionaryValue()
	dict := vm.GetDictionaryObject(locals)
	xKey := vmInst.Symbols.SymbolValue("x")
	h := vm.HashValue(xKey)
	dict.Data[h] = vm.FromSmallInt(42)
	dict.Keys[h] = xKey

	// Evaluate: x + 1 (should read x from locals)
	source := vm.NewStringValue("x + 1")
	result := vmInst.Send(compilerClass, "evaluate:withLocals:", []vm.Value{source, locals})

	if !result.IsSmallInt() {
		t.Fatalf("Expected SmallInt, got: %v", result)
	}
	if result.SmallInt() != 43 {
		t.Errorf("Expected 43, got: %d", result.SmallInt())
	}
}

func TestEvaluateWithLocals_WriteLocal(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Create empty locals dictionary
	locals := vm.NewDictionaryValue()

	// Evaluate: y := 99 (should write y to locals)
	source := vm.NewStringValue("y := 99")
	result := vmInst.Send(compilerClass, "evaluate:withLocals:", []vm.Value{source, locals})

	if !result.IsSmallInt() || result.SmallInt() != 99 {
		t.Fatalf("Expected 99 from evaluation, got: %v", result)
	}

	// Verify y is in the locals dictionary
	dict := vm.GetDictionaryObject(locals)
	yKey := vmInst.Symbols.SymbolValue("y")
	h := vm.HashValue(yKey)
	val, ok := dict.Data[h]
	if !ok {
		t.Fatal("Expected y to be in locals dictionary")
	}
	if !val.IsSmallInt() || val.SmallInt() != 99 {
		t.Errorf("Expected y=99 in locals, got: %v", val)
	}

	// Verify y is NOT in globals
	if _, exists := vmInst.Globals["y"]; exists {
		t.Error("y should NOT be in globals after evaluate:withLocals:")
	}
}

func TestEvaluateWithLocals_ModifyExistingLocal(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Create locals with x = 10
	locals := vm.NewDictionaryValue()
	dict := vm.GetDictionaryObject(locals)
	xKey := vmInst.Symbols.SymbolValue("x")
	h := vm.HashValue(xKey)
	dict.Data[h] = vm.FromSmallInt(10)
	dict.Keys[h] = xKey

	// Evaluate: x := x * 5 (should modify x in locals)
	source := vm.NewStringValue("x := x * 5")
	vmInst.Send(compilerClass, "evaluate:withLocals:", []vm.Value{source, locals})

	// Verify x was updated in locals
	val := dict.Data[h]
	if !val.IsSmallInt() || val.SmallInt() != 50 {
		t.Errorf("Expected x=50 in locals, got: %v", val)
	}
}

func TestEvaluateWithLocals_GlobalFallthrough(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Set a global: g := 100
	vmInst.Send(compilerClass, "evaluate:", []vm.Value{vm.NewStringValue("g := 100")})

	// Create empty locals
	locals := vm.NewDictionaryValue()

	// Evaluate: g + 1 (should fall through to globals)
	source := vm.NewStringValue("g + 1")
	result := vmInst.Send(compilerClass, "evaluate:withLocals:", []vm.Value{source, locals})

	if !result.IsSmallInt() || result.SmallInt() != 101 {
		t.Errorf("Expected 101 from global fallthrough, got: %v", result)
	}
}

func TestEvaluateWithLocals_LocalShadowsGlobal(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Set a global: x := 1000
	vmInst.Send(compilerClass, "evaluate:", []vm.Value{vm.NewStringValue("x := 1000")})

	// Create locals with x = 5
	locals := vm.NewDictionaryValue()
	dict := vm.GetDictionaryObject(locals)
	xKey := vmInst.Symbols.SymbolValue("x")
	h := vm.HashValue(xKey)
	dict.Data[h] = vm.FromSmallInt(5)
	dict.Keys[h] = xKey

	// Evaluate: x (should read local x=5, not global x=1000)
	source := vm.NewStringValue("x")
	result := vmInst.Send(compilerClass, "evaluate:withLocals:", []vm.Value{source, locals})

	if !result.IsSmallInt() || result.SmallInt() != 5 {
		t.Errorf("Expected local x=5 to shadow global, got: %v", result)
	}

	// Verify global x is still 1000 (restored)
	globalX, ok := vmInst.Globals["x"]
	if !ok {
		t.Fatal("Global x should still exist")
	}
	if !globalX.IsSmallInt() || globalX.SmallInt() != 1000 {
		t.Errorf("Global x should still be 1000, got: %v", globalX)
	}
}

func TestEvaluateWithLocals_ScopeIsolation(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Create two independent locals dictionaries
	locals1 := vm.NewDictionaryValue()
	locals2 := vm.NewDictionaryValue()

	// Evaluate: x := 42 in scope 1
	vmInst.Send(compilerClass, "evaluate:withLocals:", []vm.Value{vm.NewStringValue("x := 42"), locals1})

	// Evaluate: x := 99 in scope 2
	vmInst.Send(compilerClass, "evaluate:withLocals:", []vm.Value{vm.NewStringValue("x := 99"), locals2})

	// Verify scope 1 still has x = 42
	dict1 := vm.GetDictionaryObject(locals1)
	xKey := vmInst.Symbols.SymbolValue("x")
	h := vm.HashValue(xKey)
	val1 := dict1.Data[h]
	if !val1.IsSmallInt() || val1.SmallInt() != 42 {
		t.Errorf("Scope 1: expected x=42, got: %v", val1)
	}

	// Verify scope 2 has x = 99
	dict2 := vm.GetDictionaryObject(locals2)
	val2 := dict2.Data[h]
	if !val2.IsSmallInt() || val2.SmallInt() != 99 {
		t.Errorf("Scope 2: expected x=99, got: %v", val2)
	}

	// Verify x is NOT in globals
	if _, exists := vmInst.Globals["x"]; exists {
		t.Error("x should NOT leak into globals from evaluate:withLocals:")
	}
}

func TestEvaluateWithLocals_ClassNamesStillAccessible(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Create empty locals
	locals := vm.NewDictionaryValue()

	// Evaluate: Array new: 3 (should still be able to access class names)
	source := vm.NewStringValue("(Array new: 3) size")
	result := vmInst.Send(compilerClass, "evaluate:withLocals:", []vm.Value{source, locals})

	if !result.IsSmallInt() || result.SmallInt() != 3 {
		t.Errorf("Expected 3, got: %v", result)
	}
}

func TestEvaluateWithLocals_MultipleLocals(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Create locals with a=10, b=20
	locals := vm.NewDictionaryValue()
	dict := vm.GetDictionaryObject(locals)

	aKey := vmInst.Symbols.SymbolValue("a")
	aH := vm.HashValue(aKey)
	dict.Data[aH] = vm.FromSmallInt(10)
	dict.Keys[aH] = aKey

	bKey := vmInst.Symbols.SymbolValue("b")
	bH := vm.HashValue(bKey)
	dict.Data[bH] = vm.FromSmallInt(20)
	dict.Keys[bH] = bKey

	// Evaluate: a + b
	source := vm.NewStringValue("a + b")
	result := vmInst.Send(compilerClass, "evaluate:withLocals:", []vm.Value{source, locals})

	if !result.IsSmallInt() || result.SmallInt() != 30 {
		t.Errorf("Expected 30, got: %v", result)
	}
}

func TestEvaluateWithLocals_PersistAcrossEvaluations(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Same locals dictionary used across evaluations
	locals := vm.NewDictionaryValue()

	// First eval: counter := 0
	vmInst.Send(compilerClass, "evaluate:withLocals:", []vm.Value{vm.NewStringValue("counter := 0"), locals})

	// Second eval: counter := counter + 1
	vmInst.Send(compilerClass, "evaluate:withLocals:", []vm.Value{vm.NewStringValue("counter := counter + 1"), locals})

	// Third eval: counter := counter + 1
	vmInst.Send(compilerClass, "evaluate:withLocals:", []vm.Value{vm.NewStringValue("counter := counter + 1"), locals})

	// Fourth eval: read counter (should be 2)
	result := vmInst.Send(compilerClass, "evaluate:withLocals:", []vm.Value{vm.NewStringValue("counter"), locals})

	if !result.IsSmallInt() || result.SmallInt() != 2 {
		t.Errorf("Expected counter=2 after increments, got: %v", result)
	}
}
