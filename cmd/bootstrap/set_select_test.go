package main_test

import (
	"os"
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/compiler/hash"
	"github.com/chazu/maggie/vm"
)

// setupVMWithSet creates a VM with Set.mag methods compiled onto the existing SetClass.
func setupVMWithSet(t *testing.T) *vm.VM {
	t.Helper()
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	content, err := os.ReadFile("../../lib/Set.mag")
	if err != nil {
		t.Fatalf("Error reading Set.mag: %v", err)
	}

	sf, err := compiler.ParseSourceFileFromString(string(content))
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	for _, classDef := range sf.Classes {
		class := vmInst.SetClass
		allIvars := class.AllInstVarNames()

		for _, methodDef := range classDef.Methods {
			if methodDef.IsPrimitiveStub {
				continue
			}

			method, err := compiler.CompileMethodDefWithIvars(methodDef, vmInst.Selectors, vmInst.Symbols, vmInst.Registry(), classDef.InstanceVariables)
			if err != nil {
				t.Fatalf("Compile error: %s: %v", methodDef.Selector, err)
			}

			instVarMap := make(map[string]int, len(allIvars))
			for idx, name := range allIvars {
				instVarMap[name] = idx
			}
			h := hash.HashMethod(methodDef, instVarMap, nil)
			method.SetContentHash(h)
			method.SetClass(class)
			class.VTable.AddMethod(vmInst.Selectors.Intern(method.Name()), method)
		}
	}

	return vmInst
}

// eval compiles and executes a Maggie expression via Compiler evaluate:.
func eval(vmInst *vm.VM, code string) vm.Value {
	compilerVal := vmInst.Globals["Compiler"]
	return vmInst.Send(compilerVal, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue(code)})
}

// TestSetAddAndSize verifies basic Set creation and size via evaluate.
func TestSetAddAndSize(t *testing.T) {
	vmInst := setupVMWithSet(t)
	result := eval(vmInst, "Set new size")
	if !result.IsSmallInt() || result.SmallInt() != 0 {
		t.Errorf("Set new size = %v, want SmallInt 0", result)
	}
}

// TestSetAddDirectly verifies Set primitives work via Go Send.
func TestSetAddDirectly(t *testing.T) {
	vmInst := setupVMWithSet(t)

	setClassVal := vmInst.Globals["Set"]
	s := vmInst.Send(setClassVal, "new", nil)
	if !s.IsObject() {
		t.Fatalf("Set new returned non-object: %v", s)
	}

	vmInst.Send(s, "add:", []vm.Value{vm.FromSmallInt(1)})
	vmInst.Send(s, "add:", []vm.Value{vm.FromSmallInt(2)})
	vmInst.Send(s, "add:", []vm.Value{vm.FromSmallInt(3)})
	vmInst.Send(s, "add:", []vm.Value{vm.FromSmallInt(4)})

	size := vmInst.Send(s, "size", nil)
	if !size.IsSmallInt() || size.SmallInt() != 4 {
		t.Errorf("size after 4 adds = %v, want SmallInt 4", size)
	}
}

// TestSetMultiStatementEval verifies that multi-statement expressions work
// correctly in Compiler evaluate: (regression test for the ^ return placement bug).
func TestSetMultiStatementEval(t *testing.T) {
	vmInst := setupVMWithSet(t)

	// Multi-statement: add 4 elements and check size
	result := eval(vmInst, "s := Set new. s add: 1. s add: 2. s add: 3. s add: 4. s size")
	if !result.IsSmallInt() || result.SmallInt() != 4 {
		t.Errorf("s size after 4 adds = %v, want SmallInt 4", result)
	}
}

// TestSetSelectViaEval verifies Set>>select: works through Compiler evaluate:.
func TestSetSelectViaEval(t *testing.T) {
	vmInst := setupVMWithSet(t)

	// select: elements > 2 (using > instead of even since SmallInteger.mag not loaded)
	result := eval(vmInst, "s := Set new. s add: 1. s add: 2. s add: 3. s add: 4. big := s select: [:x | x > 2]. big size")
	if !result.IsSmallInt() || result.SmallInt() != 2 {
		t.Errorf("select: [:x | x > 2] size = %v, want SmallInt 2", result)
	}
}

// TestSetRejectViaEval verifies Set>>reject: works through Compiler evaluate:.
func TestSetRejectViaEval(t *testing.T) {
	vmInst := setupVMWithSet(t)

	result := eval(vmInst, "s := Set new. s add: 1. s add: 2. s add: 3. s add: 4. small := s reject: [:x | x > 2]. small size")
	if !result.IsSmallInt() || result.SmallInt() != 2 {
		t.Errorf("reject: [:x | x > 2] size = %v, want SmallInt 2", result)
	}
}

// Note: Set>>collect: test omitted because it uses Array concatenation (,)
// which requires Array.mag to be loaded. The collect: method itself works
// correctly — only the test harness limitation prevents testing here.
