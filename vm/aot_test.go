package vm

import (
	"strings"
	"testing"
)

func TestAOTCompilerSimple(t *testing.T) {
	selectors := NewSelectorTable()
	symbols := NewSymbolTable()

	// Create a simple method: ^42
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().EmitInt8(OpPushInt8, 42)
	b.Bytecode().Emit(OpReturnTop)
	method := b.Build()

	aot := NewAOTCompiler(selectors, symbols)
	code := aot.CompileMethod(method, "Test", "answer")

	t.Logf("Generated code:\n%s", code)

	// Verify it contains expected elements
	if !strings.Contains(code, "func aot_Test_answer") {
		t.Error("Missing function declaration")
	}
	if !strings.Contains(code, "FromSmallInt(42)") {
		t.Error("Missing push 42")
	}
	if !strings.Contains(code, "return stack[sp-1]") {
		t.Error("Missing return")
	}
}

func TestAOTCompilerArithmetic(t *testing.T) {
	selectors := NewSelectorTable()
	symbols := NewSymbolTable()

	// Create: | x | x := 3. ^x + 4
	b := NewCompiledMethodBuilder("test", 0)
	b.SetNumTemps(1)
	b.Bytecode().EmitInt8(OpPushInt8, 3)
	b.Bytecode().EmitByte(OpStoreTemp, 0)
	b.Bytecode().Emit(OpPOP)
	b.Bytecode().EmitByte(OpPushTemp, 0)
	b.Bytecode().EmitInt8(OpPushInt8, 4)
	b.Bytecode().Emit(OpSendPlus)
	b.Bytecode().Emit(OpReturnTop)
	method := b.Build()

	aot := NewAOTCompiler(selectors, symbols)
	code := aot.CompileMethod(method, "Test", "arithmetic")

	t.Logf("Generated code:\n%s", code)

	// Verify it contains expected elements
	if !strings.Contains(code, "temps[0] = stack[sp-1]") {
		t.Error("Missing store temp")
	}
	if !strings.Contains(code, "stack[sp] = temps[0]") {
		t.Error("Missing push temp")
	}
	if !strings.Contains(code, "a.SmallInt() + b.SmallInt()") {
		t.Error("Missing optimized addition")
	}
}

func TestAOTCompilerConditional(t *testing.T) {
	selectors := NewSelectorTable()
	symbols := NewSymbolTable()

	// Create: ^true ifTrue: [1] ifFalse: [0]
	// This is roughly: PUSH_TRUE, JUMP_FALSE L1, PUSH 1, JUMP L2, L1: PUSH 0, L2: RETURN
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushTrue)

	// JUMP_FALSE to else branch (offset will be patched)
	b.Bytecode().Emit(OpJumpFalse)
	jumpFalsePos := b.Bytecode().Len()
	b.Bytecode().EmitRaw(0) // placeholder
	b.Bytecode().EmitRaw(0)

	// True branch
	b.Bytecode().EmitInt8(OpPushInt8, 1)
	b.Bytecode().Emit(OpJump)
	jumpEndPos := b.Bytecode().Len()
	b.Bytecode().EmitRaw(0) // placeholder
	b.Bytecode().EmitRaw(0)

	// False branch (patch JUMP_FALSE target)
	elsePos := b.Bytecode().Len()
	offset1 := elsePos - (jumpFalsePos + 2)
	bc := b.Bytecode().Bytes()
	bc[jumpFalsePos] = byte(offset1)
	bc[jumpFalsePos+1] = byte(offset1 >> 8)

	b.Bytecode().EmitInt8(OpPushInt8, 0)

	// End (patch JUMP target)
	endPos := b.Bytecode().Len()
	offset2 := endPos - (jumpEndPos + 2)
	bc[jumpEndPos] = byte(offset2)
	bc[jumpEndPos+1] = byte(offset2 >> 8)

	b.Bytecode().Emit(OpReturnTop)
	method := b.Build()

	aot := NewAOTCompiler(selectors, symbols)
	code := aot.CompileMethod(method, "Test", "conditional")

	t.Logf("Generated code:\n%s", code)

	// Verify it contains jumps and labels
	if !strings.Contains(code, "goto L") {
		t.Error("Missing goto")
	}
	if !strings.Contains(code, "L") && !strings.Contains(code, ":") {
		t.Error("Missing labels")
	}
}

func TestAOTCompilerMessageSend(t *testing.T) {
	selectors := NewSelectorTable()
	symbols := NewSymbolTable()

	// Intern a selector
	sizeSelID := selectors.Intern("size")

	// Create: ^self size
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushSelf)
	b.Bytecode().EmitSend(OpSend, uint16(sizeSelID), 0)
	b.Bytecode().Emit(OpReturnTop)
	method := b.Build()

	aot := NewAOTCompiler(selectors, symbols)
	code := aot.CompileMethod(method, "Test", "sendSize")

	t.Logf("Generated code:\n%s", code)

	// Verify it contains send
	if !strings.Contains(code, "vm.Send") {
		t.Error("Missing vm.Send")
	}
}

func TestAOTDispatchIntegration(t *testing.T) {
	// Test that AOT methods are actually called by the VM
	vmInst := NewVM()

	// Create a test class
	class := vmInst.createClass("TestClass", vmInst.ObjectClass)

	// Add a bytecode method that returns 42
	selectors := vmInst.Selectors
	b := NewCompiledMethodBuilder("answer", 0)
	b.Bytecode().EmitInt8(OpPushInt8, 42)
	b.Bytecode().Emit(OpReturnTop)
	method := b.Build()

	answerSelID := selectors.Intern("answer")
	class.VTable.AddMethod(answerSelID, method)

	// Create an instance
	obj := NewObject(class.VTable, 0)
	objVal := obj.ToValue()

	// Test without AOT - should return 42
	result := vmInst.Send(objVal, "answer", nil)
	if !result.IsSmallInt() || result.SmallInt() != 42 {
		t.Errorf("Without AOT: got %v, want 42", result)
	}

	// Register an AOT method that returns 100 instead
	aotMethods := AOTDispatchTable{
		AOTDispatchKey{"TestClass", "answer"}: func(vm *VM, self Value, args []Value) Value {
			return FromSmallInt(100) // AOT version returns 100
		},
	}
	vmInst.RegisterAOTMethods(aotMethods)

	// Test with AOT - should return 100 (AOT version)
	result = vmInst.Send(objVal, "answer", nil)
	if !result.IsSmallInt() || result.SmallInt() != 100 {
		t.Errorf("With AOT: got %v, want 100", result)
	}
}

// BenchmarkAOTvsInterpreted compares AOT execution to interpreted execution.
func BenchmarkAOTvsInterpreted(b *testing.B) {
	// Setup: create a VM with a simple arithmetic method
	vmInst := NewVM()
	class := vmInst.createClass("Calculator", vmInst.ObjectClass)

	// Create a method that does some computation: ^self + self * 2
	selectors := vmInst.Selectors
	builder := NewCompiledMethodBuilder("compute", 0)
	builder.Bytecode().Emit(OpPushSelf)
	builder.Bytecode().Emit(OpPushSelf)
	builder.Bytecode().EmitInt8(OpPushInt8, 2)
	builder.Bytecode().Emit(OpSendTimes)
	builder.Bytecode().Emit(OpSendPlus)
	builder.Bytecode().Emit(OpReturnTop)
	method := builder.Build()

	computeSelID := selectors.Intern("compute")
	class.VTable.AddMethod(computeSelID, method)

	// Create a numeric receiver (SmallInt)
	receiver := FromSmallInt(10)

	// AOT version of the same method
	aotCompute := func(vm *VM, self Value, args []Value) Value {
		// Direct Go implementation: self + self * 2
		if self.IsSmallInt() {
			val := self.SmallInt()
			return FromSmallInt(val + val*2)
		}
		// Fallback to message sends
		times2 := vm.Send(self, "*", []Value{FromSmallInt(2)})
		return vm.Send(self, "+", []Value{times2})
	}

	b.Run("Interpreted", func(b *testing.B) {
		// Make sure AOT is not registered
		vmInst.aotMethods = nil
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			vmInst.Send(receiver, "compute", nil)
		}
	})

	b.Run("AOT", func(b *testing.B) {
		// Register AOT method
		vmInst.RegisterAOTMethods(AOTDispatchTable{
			AOTDispatchKey{"Calculator", "compute"}: aotCompute,
		})
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			vmInst.Send(receiver, "compute", nil)
		}
	})

	b.Run("DirectGo", func(b *testing.B) {
		// Direct Go function call (no dispatch)
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			_ = aotCompute(vmInst, receiver, nil)
		}
	})
}

func TestAOTCompilerModule(t *testing.T) {
	selectors := NewSelectorTable()
	symbols := NewSymbolTable()

	// Create a simple class with methods
	class := &Class{
		Name: "Counter",
	}
	class.VTable = NewVTable(class, nil)

	// Add a method: value ^42
	b1 := NewCompiledMethodBuilder("value", 0)
	b1.Bytecode().EmitInt8(OpPushInt8, 42)
	b1.Bytecode().Emit(OpReturnTop)
	method1 := b1.Build()

	valueSelID := selectors.Intern("value")
	class.VTable.AddMethod(valueSelID, method1)

	// Add another method: increment ^self + 1
	b2 := NewCompiledMethodBuilder("increment", 0)
	b2.Bytecode().Emit(OpPushSelf)
	b2.Bytecode().EmitInt8(OpPushInt8, 1)
	b2.Bytecode().Emit(OpSendPlus)
	b2.Bytecode().Emit(OpReturnTop)
	method2 := b2.Build()

	incSelID := selectors.Intern("increment")
	class.VTable.AddMethod(incSelID, method2)

	aot := NewAOTCompiler(selectors, symbols)
	code := aot.CompileModule("main", []*Class{class})

	t.Logf("Generated module:\n%s", code)

	// Verify module structure
	if !strings.Contains(code, "package main") {
		t.Error("Missing package declaration")
	}
	if !strings.Contains(code, "import") {
		t.Error("Missing imports")
	}
	if !strings.Contains(code, "aot_Counter_value") {
		t.Error("Missing value method")
	}
	if !strings.Contains(code, "aot_Counter_increment") {
		t.Error("Missing increment method")
	}
	if !strings.Contains(code, "initAOTDispatchTable") {
		t.Error("Missing dispatch table init")
	}
	if !strings.Contains(code, "AOTDispatchKey{\"Counter\", \"value\"}") {
		t.Error("Missing dispatch key for value")
	}
}
