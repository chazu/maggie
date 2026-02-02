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

// ---------------------------------------------------------------------------
// Block Compilation Tests
// ---------------------------------------------------------------------------

func TestAOTCompilerBlockSimple(t *testing.T) {
	selectors := NewSelectorTable()
	symbols := NewSymbolTable()

	// Create a simple block: [^42]
	block := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 0,
		Bytecode:    []byte{byte(OpPushInt8), 42, byte(OpReturnTop)},
		Literals:    nil,
	}

	aot := NewAOTCompiler(selectors, symbols)
	code := aot.CompileBlock(block, "Test", "example", 0)

	t.Logf("Generated block code:\n%s", code)

	// Verify function signature includes captures and homeReturn
	if !strings.Contains(code, "func aot_Test_example_block0") {
		t.Error("Missing block function declaration")
	}
	if !strings.Contains(code, "captures []Value") {
		t.Error("Missing captures parameter")
	}
	if !strings.Contains(code, "homeReturn func(Value)") {
		t.Error("Missing homeReturn parameter")
	}
	if !strings.Contains(code, "FromSmallInt(42)") {
		t.Error("Missing push 42")
	}
}

func TestAOTCompilerBlockWithCaptures(t *testing.T) {
	selectors := NewSelectorTable()
	symbols := NewSymbolTable()

	// Create a block that uses captures: [captures[0] + 1]
	block := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 1,
		Bytecode: []byte{
			byte(OpPushCaptured), 0, // push captures[0]
			byte(OpPushInt8), 1,     // push 1
			byte(OpSendPlus),        // +
			byte(OpReturnTop),
		},
		Literals: nil,
	}

	aot := NewAOTCompiler(selectors, symbols)
	code := aot.CompileBlock(block, "Test", "example", 1)

	t.Logf("Generated block code with captures:\n%s", code)

	// Should access captures array
	if !strings.Contains(code, "captures[0]") {
		t.Error("Missing captures access")
	}
}

func TestAOTCompilerBlockWithArgs(t *testing.T) {
	selectors := NewSelectorTable()
	symbols := NewSymbolTable()

	// Create a block with argument: [:x | x * 2]
	block := &BlockMethod{
		Arity:       1,
		NumTemps:    1, // args count as temps
		NumCaptures: 0,
		Bytecode: []byte{
			byte(OpPushTemp), 0,  // push x (arg)
			byte(OpPushInt8), 2,  // push 2
			byte(OpSendTimes),    // *
			byte(OpReturnTop),
		},
		Literals: nil,
	}

	aot := NewAOTCompiler(selectors, symbols)
	code := aot.CompileBlock(block, "Test", "example", 2)

	t.Logf("Generated block code with args:\n%s", code)

	// Should have temps for args
	if !strings.Contains(code, "temps := make([]Value, 1)") {
		t.Error("Missing temps declaration")
	}
	if !strings.Contains(code, "copy(temps, args)") {
		t.Error("Missing args copy to temps")
	}
}

func TestAOTCompilerBlockReturn(t *testing.T) {
	selectors := NewSelectorTable()
	symbols := NewSymbolTable()

	// Create a block with non-local return: [^99]
	block := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 0,
		Bytecode: []byte{
			byte(OpPushInt8), 99,
			byte(OpBlockReturn), // non-local return
		},
		Literals: nil,
	}

	aot := NewAOTCompiler(selectors, symbols)
	code := aot.CompileBlock(block, "Test", "example", 3)

	t.Logf("Generated block code with non-local return:\n%s", code)

	// Should call homeReturn for non-local return
	if !strings.Contains(code, "homeReturn(") {
		t.Error("Missing homeReturn call for non-local return")
	}
}

func TestAOTCompilerBlockReturnInMethod(t *testing.T) {
	selectors := NewSelectorTable()
	symbols := NewSymbolTable()

	// In a method context, OpBlockReturn should just return normally
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().EmitInt8(OpPushInt8, 77)
	b.Bytecode().Emit(OpBlockReturn)
	method := b.Build()

	aot := NewAOTCompiler(selectors, symbols)
	code := aot.CompileMethod(method, "Test", "blockReturnInMethod")

	t.Logf("Generated method code with BlockReturn:\n%s", code)

	// In method context, should just return (not call homeReturn)
	if strings.Contains(code, "homeReturn") {
		t.Error("Method should not use homeReturn")
	}
	if !strings.Contains(code, "return stack[sp-1]") {
		t.Error("Should return normally in method context")
	}
}

// ---------------------------------------------------------------------------
// Super Send Tests
// ---------------------------------------------------------------------------

func TestAOTCompilerSuperSendCodeGen(t *testing.T) {
	// Test that OpSendSuper generates correct code with vm.SendSuper
	selectors := NewSelectorTable()
	symbols := NewSymbolTable()

	fooSelID := selectors.Intern("foo")

	// Create a class hierarchy: Animal -> Dog
	animalClass := &Class{Name: "Animal"}
	animalClass.VTable = NewVTable(animalClass, nil)

	dogClass := &Class{Name: "Dog", Superclass: animalClass}
	dogClass.VTable = NewVTable(dogClass, animalClass.VTable)

	// Create Dog>>foo that does: ^super foo
	b := NewCompiledMethodBuilder("foo", 0)
	b.Bytecode().Emit(OpPushSelf)
	b.Bytecode().EmitSend(OpSendSuper, uint16(fooSelID), 0)
	b.Bytecode().Emit(OpReturnTop)
	method := b.Build()
	method.SetClass(dogClass) // Set defining class so AOT knows it

	aot := NewAOTCompiler(selectors, symbols)
	code := aot.CompileMethod(method, "Dog", "foo")

	t.Logf("Generated super send code:\n%s", code)

	// Verify it uses vm.SendSuper with the defining class name
	if !strings.Contains(code, "vm.SendSuper") {
		t.Error("Missing vm.SendSuper call")
	}
	if !strings.Contains(code, `"Dog"`) {
		t.Error("Missing defining class name 'Dog'")
	}
	if !strings.Contains(code, "SEND_SUPER") {
		t.Error("Missing SEND_SUPER comment")
	}
	// Should NOT use vm.Send for super sends
	if strings.Contains(code, "vm.Send(self") {
		t.Error("Should not use vm.Send for super send, should use vm.SendSuper")
	}
}

func TestAOTSuperSendDispatchIntegration(t *testing.T) {
	// Integration test: verify super send dispatches to parent class method
	vmInst := NewVM()

	// Create class hierarchy: Animal -> Dog
	animalClass := vmInst.createClass("AnimalAOT", vmInst.ObjectClass)
	dogClass := vmInst.createClass("DogAOT", animalClass)

	selectors := vmInst.Selectors

	// Animal>>speak returns 10
	ab := NewCompiledMethodBuilder("speak", 0)
	ab.Bytecode().EmitInt8(OpPushInt8, 10)
	ab.Bytecode().Emit(OpReturnTop)
	animalSpeak := ab.Build()
	animalSpeak.SetClass(animalClass)
	speakSelID := selectors.Intern("speak")
	animalClass.VTable.AddMethod(speakSelID, animalSpeak)

	// Dog>>speak returns 20
	db := NewCompiledMethodBuilder("speak", 0)
	db.Bytecode().EmitInt8(OpPushInt8, 20)
	db.Bytecode().Emit(OpReturnTop)
	dogSpeak := db.Build()
	dogSpeak.SetClass(dogClass)
	dogClass.VTable.AddMethod(speakSelID, dogSpeak)

	// Dog>>speakSuper uses super send to call Animal>>speak
	dsb := NewCompiledMethodBuilder("speakSuper", 0)
	dsb.Bytecode().Emit(OpPushSelf)
	dsb.Bytecode().EmitSend(OpSendSuper, uint16(speakSelID), 0)
	dsb.Bytecode().Emit(OpReturnTop)
	dogSpeakSuper := dsb.Build()
	dogSpeakSuper.SetClass(dogClass)
	speakSuperSelID := selectors.Intern("speakSuper")
	dogClass.VTable.AddMethod(speakSuperSelID, dogSpeakSuper)

	// Create a Dog instance
	obj := NewObject(dogClass.VTable, 0)
	objVal := obj.ToValue()

	// Register AOT method for Dog>>speakSuper using vm.SendSuper
	vmInst.RegisterAOTMethods(AOTDispatchTable{
		AOTDispatchKey{"DogAOT", "speakSuper"}: func(vm *VM, self Value, args []Value) Value {
			return vm.SendSuper(self, "speak", nil, "DogAOT")
		},
	})

	// Dog>>speak (normal) should return 20
	result := vmInst.Send(objVal, "speak", nil)
	if !result.IsSmallInt() || result.SmallInt() != 20 {
		t.Errorf("Dog>>speak: got %v, want 20", result)
	}

	// Dog>>speakSuper (via AOT with super send) should return 10 (from Animal)
	result = vmInst.Send(objVal, "speakSuper", nil)
	if !result.IsSmallInt() || result.SmallInt() != 10 {
		t.Errorf("Dog>>speakSuper via AOT: got %v, want 10 (from Animal)", result)
	}
}

func TestAOTSuperSendThreeLevelHierarchy(t *testing.T) {
	// Test A -> B -> C, B calls super, should get A's method
	vmInst := NewVM()
	selectors := vmInst.Selectors

	// Create: A -> B -> C
	classA := vmInst.createClass("AClassAOT", vmInst.ObjectClass)
	classB := vmInst.createClass("BClassAOT", classA)
	classC := vmInst.createClass("CClassAOT", classB)

	// A>>greet returns 1
	ab := NewCompiledMethodBuilder("greet", 0)
	ab.Bytecode().EmitInt8(OpPushInt8, 1)
	ab.Bytecode().Emit(OpReturnTop)
	aGreet := ab.Build()
	aGreet.SetClass(classA)
	greetSelID := selectors.Intern("greet")
	classA.VTable.AddMethod(greetSelID, aGreet)

	// B>>greet calls super greet (should reach A>>greet, returning 1)
	bb := NewCompiledMethodBuilder("greet", 0)
	bb.Bytecode().Emit(OpPushSelf)
	bb.Bytecode().EmitSend(OpSendSuper, uint16(greetSelID), 0)
	bb.Bytecode().Emit(OpReturnTop)
	bGreet := bb.Build()
	bGreet.SetClass(classB)
	classB.VTable.AddMethod(greetSelID, bGreet)

	// C does NOT override greet, so it inherits B>>greet

	// Create a C instance
	obj := NewObject(classC.VTable, 0)
	objVal := obj.ToValue()

	// Register AOT method for B>>greet that uses vm.SendSuper
	vmInst.RegisterAOTMethods(AOTDispatchTable{
		AOTDispatchKey{"BClassAOT", "greet"}: func(vm *VM, self Value, args []Value) Value {
			// B>>greet calls super greet -> should go to A>>greet
			return vm.SendSuper(self, "greet", nil, "BClassAOT")
		},
	})

	// C instance calling greet: should hit B>>greet (AOT), which super sends to A>>greet, returning 1
	result := vmInst.Send(objVal, "greet", nil)
	if !result.IsSmallInt() || result.SmallInt() != 1 {
		t.Errorf("C>>greet (via B super -> A): got %v, want 1", result)
	}
}

func TestAOTSelfSendUsesReceiverClass(t *testing.T) {
	// Test that self sends (not super sends) still use the receiver's class
	vmInst := NewVM()
	selectors := vmInst.Selectors

	// Create: Animal -> Dog
	animalClass := vmInst.createClass("AnimalSelfAOT", vmInst.ObjectClass)
	dogClass := vmInst.createClass("DogSelfAOT", animalClass)

	// Animal>>info returns 50
	ab := NewCompiledMethodBuilder("info", 0)
	ab.Bytecode().EmitInt8(OpPushInt8, 50)
	ab.Bytecode().Emit(OpReturnTop)
	animalInfo := ab.Build()
	animalInfo.SetClass(animalClass)
	infoSelID := selectors.Intern("info")
	animalClass.VTable.AddMethod(infoSelID, animalInfo)

	// Dog>>info returns 99
	db := NewCompiledMethodBuilder("info", 0)
	db.Bytecode().EmitInt8(OpPushInt8, 99)
	db.Bytecode().Emit(OpReturnTop)
	dogInfo := db.Build()
	dogInfo.SetClass(dogClass)
	dogClass.VTable.AddMethod(infoSelID, dogInfo)

	// Animal>>callInfo does self info (normal send, not super)
	cb := NewCompiledMethodBuilder("callInfo", 0)
	cb.Bytecode().Emit(OpPushSelf)
	cb.Bytecode().EmitSend(OpSend, uint16(infoSelID), 0)
	cb.Bytecode().Emit(OpReturnTop)
	animalCallInfo := cb.Build()
	animalCallInfo.SetClass(animalClass)
	callInfoSelID := selectors.Intern("callInfo")
	animalClass.VTable.AddMethod(callInfoSelID, animalCallInfo)

	// Register AOT for Animal>>callInfo with self send (not super)
	vmInst.RegisterAOTMethods(AOTDispatchTable{
		AOTDispatchKey{"AnimalSelfAOT", "callInfo"}: func(vm *VM, self Value, args []Value) Value {
			// Normal send on self - should dispatch based on receiver's class
			return vm.Send(self, "info", nil)
		},
	})

	// Dog instance calling callInfo (inherited from Animal) should get Dog>>info (99),
	// because self send dispatches on receiver's class (Dog), not defining class (Animal)
	obj := NewObject(dogClass.VTable, 0)
	objVal := obj.ToValue()

	result := vmInst.Send(objVal, "callInfo", nil)
	if !result.IsSmallInt() || result.SmallInt() != 99 {
		t.Errorf("Dog>>callInfo (self send): got %v, want 99 (Dog>>info)", result)
	}
}

func TestAOTSuperSendMethodNotFound(t *testing.T) {
	// Test that super send returns Nil when the method is not found in superclass
	vmInst := NewVM()
	selectors := vmInst.Selectors

	classA := vmInst.createClass("SuperNotFoundA", vmInst.ObjectClass)
	classB := vmInst.createClass("SuperNotFoundB", classA)

	// B has "unique" method but A does not
	ub := NewCompiledMethodBuilder("unique", 0)
	ub.Bytecode().EmitInt8(OpPushInt8, 42)
	ub.Bytecode().Emit(OpReturnTop)
	bUnique := ub.Build()
	bUnique.SetClass(classB)
	uniqueSelID := selectors.Intern("unique")
	classB.VTable.AddMethod(uniqueSelID, bUnique)

	// B>>callSuperUnique does super unique (A doesn't have it)
	csb := NewCompiledMethodBuilder("callSuperUnique", 0)
	csb.Bytecode().Emit(OpPushSelf)
	csb.Bytecode().EmitSend(OpSendSuper, uint16(uniqueSelID), 0)
	csb.Bytecode().Emit(OpReturnTop)
	bCallSuper := csb.Build()
	bCallSuper.SetClass(classB)
	callSuperSelID := selectors.Intern("callSuperUnique")
	classB.VTable.AddMethod(callSuperSelID, bCallSuper)

	// Use SendSuper directly (as AOT would generate)
	result := vmInst.SendSuper(FromSmallInt(0), "unique", nil, "SuperNotFoundB")
	if result != Nil {
		t.Errorf("Super send to non-existent method: got %v, want Nil", result)
	}
}

// ---------------------------------------------------------------------------
// Block Creation in AOT Tests
// ---------------------------------------------------------------------------

func TestAOTCompilerBlockCreationCodeGen(t *testing.T) {
	// Test that OpCreateBlock generates proper code instead of Nil placeholder
	selectors := NewSelectorTable()
	symbols := NewSymbolTable()

	// Create a method with a block: ^[42] value
	// First create the block
	block := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 0,
		Bytecode:    []byte{byte(OpPushInt8), 42, byte(OpReturnTop)},
		Literals:    nil,
	}

	// Create the method
	b := NewCompiledMethodBuilder("testBlock", 0)
	b.AddBlock(block) // Add block at index 0
	b.Bytecode().EmitCreateBlock(0, 0) // CREATE_BLOCK method=0, captures=0
	b.Bytecode().Emit(OpSendValue)     // value
	b.Bytecode().Emit(OpReturnTop)
	method := b.Build()

	// Set the class so block creation code can reference it
	testClass := &Class{Name: "TestBlockClass"}
	testClass.VTable = NewVTable(testClass, nil)
	method.SetClass(testClass)

	aot := NewAOTCompiler(selectors, symbols)
	code := aot.CompileMethod(method, "TestBlockClass", "testBlock")

	t.Logf("Generated code with block creation:\n%s", code)

	// Verify it generates block creation code, NOT the Nil placeholder
	if strings.Contains(code, "stack[sp] = Nil // placeholder") {
		t.Error("Should NOT have Nil placeholder for block creation")
	}
	if !strings.Contains(code, "CREATE_BLOCK") {
		t.Error("Missing CREATE_BLOCK comment")
	}
	if !strings.Contains(code, "vm.CreateBlock") || !strings.Contains(code, "vm.LookupClass") {
		t.Error("Should generate vm.CreateBlock or vm.LookupClass for block creation")
	}
}

func TestAOTBlockCreationIntegration(t *testing.T) {
	// Integration test: create a block via vm.CreateBlock and evaluate it
	vmInst := NewVM()

	// Create a simple block method
	block := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 0,
		Bytecode:    []byte{byte(OpPushInt8), 77, byte(OpReturnTop)},
		Literals:    nil,
	}

	// Create a block value using the VM's CreateBlock
	blockVal := vmInst.CreateBlock(block, nil, Nil)

	// Should not be Nil
	if blockVal == Nil {
		t.Fatal("CreateBlock returned Nil")
	}

	// Evaluate the block via vm.Send
	result := vmInst.Send(blockVal, "value", nil)
	if !result.IsSmallInt() || result.SmallInt() != 77 {
		t.Errorf("Block value: got %v, want 77", result)
	}
}
