package vm

import (
	"testing"
)

// ---------------------------------------------------------------------------
// Basic execution tests
// ---------------------------------------------------------------------------

func TestInterpreterReturnNil(t *testing.T) {
	interp := NewInterpreter()

	// Method: ^nil
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushNil)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if result != Nil {
		t.Errorf("result = %v, want nil", result)
	}
}

func TestInterpreterReturnTrue(t *testing.T) {
	interp := NewInterpreter()

	// Method: ^true
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushTrue)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if result != True {
		t.Errorf("result = %v, want true", result)
	}
}

func TestInterpreterReturnFalse(t *testing.T) {
	interp := NewInterpreter()

	// Method: ^false
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushFalse)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if result != False {
		t.Errorf("result = %v, want false", result)
	}
}

func TestInterpreterReturnSelf(t *testing.T) {
	interp := NewInterpreter()

	// Method: ^self
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpReturnSelf)
	m := b.Build()

	receiver := FromSmallInt(42)
	result := interp.Execute(m, receiver, nil)
	if result != receiver {
		t.Errorf("result = %v, want receiver", result)
	}
}

func TestInterpreterReturnInt8(t *testing.T) {
	interp := NewInterpreter()

	// Method: ^42
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().EmitInt8(OpPushInt8, 42)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if !result.IsSmallInt() || result.SmallInt() != 42 {
		t.Errorf("result = %v, want 42", result)
	}
}

func TestInterpreterReturnNegativeInt8(t *testing.T) {
	interp := NewInterpreter()

	// Method: ^-1
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().EmitInt8(OpPushInt8, -1)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if !result.IsSmallInt() || result.SmallInt() != -1 {
		t.Errorf("result = %v, want -1", result)
	}
}

func TestInterpreterReturnFloat(t *testing.T) {
	interp := NewInterpreter()

	// Method: ^3.14
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().EmitFloat64(OpPushFloat, 3.14)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if !result.IsFloat() || result.Float64() != 3.14 {
		t.Errorf("result = %v, want 3.14", result)
	}
}

func TestInterpreterReturnLiteral(t *testing.T) {
	interp := NewInterpreter()

	// Method: ^999
	b := NewCompiledMethodBuilder("test", 0)
	litIdx := b.AddLiteral(FromSmallInt(999))
	b.Bytecode().EmitUint16(OpPushLiteral, uint16(litIdx))
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if !result.IsSmallInt() || result.SmallInt() != 999 {
		t.Errorf("result = %v, want 999", result)
	}
}

// ---------------------------------------------------------------------------
// Stack operation tests
// ---------------------------------------------------------------------------

func TestInterpreterPOP(t *testing.T) {
	interp := NewInterpreter()

	// Method: push 1, push 2, pop, ^top (should be 1)
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().EmitInt8(OpPushInt8, 1)
	b.Bytecode().EmitInt8(OpPushInt8, 2)
	b.Bytecode().Emit(OpPOP)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if result.SmallInt() != 1 {
		t.Errorf("result = %v, want 1", result)
	}
}

func TestInterpreterDUP(t *testing.T) {
	interp := NewInterpreter()

	// Method: push 5, dup, add (should be 10)
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().EmitInt8(OpPushInt8, 5)
	b.Bytecode().Emit(OpDUP)
	b.Bytecode().Emit(OpSendPlus)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if result.SmallInt() != 10 {
		t.Errorf("result = %v, want 10", result)
	}
}

// ---------------------------------------------------------------------------
// Arithmetic tests
// ---------------------------------------------------------------------------

func TestInterpreterAddIntegers(t *testing.T) {
	interp := NewInterpreter()

	// Method: ^1 + 2
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().EmitInt8(OpPushInt8, 1)
	b.Bytecode().EmitInt8(OpPushInt8, 2)
	b.Bytecode().Emit(OpSendPlus)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if result.SmallInt() != 3 {
		t.Errorf("result = %v, want 3", result)
	}
}

func TestInterpreterSubtractIntegers(t *testing.T) {
	interp := NewInterpreter()

	// Method: ^10 - 3
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().EmitInt8(OpPushInt8, 10)
	b.Bytecode().EmitInt8(OpPushInt8, 3)
	b.Bytecode().Emit(OpSendMinus)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if result.SmallInt() != 7 {
		t.Errorf("result = %v, want 7", result)
	}
}

func TestInterpreterMultiplyIntegers(t *testing.T) {
	interp := NewInterpreter()

	// Method: ^6 * 7
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().EmitInt8(OpPushInt8, 6)
	b.Bytecode().EmitInt8(OpPushInt8, 7)
	b.Bytecode().Emit(OpSendTimes)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if result.SmallInt() != 42 {
		t.Errorf("result = %v, want 42", result)
	}
}

func TestInterpreterAddFloats(t *testing.T) {
	interp := NewInterpreter()

	// Method: ^1.5 + 2.5
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().EmitFloat64(OpPushFloat, 1.5)
	b.Bytecode().EmitFloat64(OpPushFloat, 2.5)
	b.Bytecode().Emit(OpSendPlus)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if !result.IsFloat() || result.Float64() != 4.0 {
		t.Errorf("result = %v, want 4.0", result)
	}
}

func TestInterpreterMixedArithmetic(t *testing.T) {
	interp := NewInterpreter()

	// Method: ^2 + 3.5
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().EmitInt8(OpPushInt8, 2)
	b.Bytecode().EmitFloat64(OpPushFloat, 3.5)
	b.Bytecode().Emit(OpSendPlus)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if !result.IsFloat() || result.Float64() != 5.5 {
		t.Errorf("result = %v, want 5.5", result)
	}
}

// ---------------------------------------------------------------------------
// Comparison tests
// ---------------------------------------------------------------------------

func TestInterpreterLessThan(t *testing.T) {
	interp := NewInterpreter()

	// Method: ^1 < 2
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().EmitInt8(OpPushInt8, 1)
	b.Bytecode().EmitInt8(OpPushInt8, 2)
	b.Bytecode().Emit(OpSendLT)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if result != True {
		t.Errorf("1 < 2 = %v, want true", result)
	}
}

func TestInterpreterGreaterThan(t *testing.T) {
	interp := NewInterpreter()

	// Method: ^5 > 3
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().EmitInt8(OpPushInt8, 5)
	b.Bytecode().EmitInt8(OpPushInt8, 3)
	b.Bytecode().Emit(OpSendGT)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if result != True {
		t.Errorf("5 > 3 = %v, want true", result)
	}
}

func TestInterpreterEqual(t *testing.T) {
	interp := NewInterpreter()

	// Method: ^42 = 42
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().EmitInt8(OpPushInt8, 42)
	b.Bytecode().EmitInt8(OpPushInt8, 42)
	b.Bytecode().Emit(OpSendEQ)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if result != True {
		t.Errorf("42 = 42 = %v, want true", result)
	}
}

func TestInterpreterNotEqual(t *testing.T) {
	interp := NewInterpreter()

	// Method: ^1 ~= 2
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().EmitInt8(OpPushInt8, 1)
	b.Bytecode().EmitInt8(OpPushInt8, 2)
	b.Bytecode().Emit(OpSendNE)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if result != True {
		t.Errorf("1 ~= 2 = %v, want true", result)
	}
}

// ---------------------------------------------------------------------------
// Temporary variable tests
// ---------------------------------------------------------------------------

func TestInterpreterTempVariable(t *testing.T) {
	interp := NewInterpreter()

	// Method: | temp | temp := 10. ^temp
	b := NewCompiledMethodBuilder("test", 0)
	b.SetNumTemps(1)
	b.Bytecode().EmitInt8(OpPushInt8, 10)
	b.Bytecode().EmitByte(OpStoreTemp, 0)
	b.Bytecode().Emit(OpPOP)
	b.Bytecode().EmitByte(OpPushTemp, 0)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if result.SmallInt() != 10 {
		t.Errorf("result = %v, want 10", result)
	}
}

func TestInterpreterArguments(t *testing.T) {
	interp := NewInterpreter()

	// Method add: x ^x + 1
	b := NewCompiledMethodBuilder("add:", 1)
	b.Bytecode().EmitByte(OpPushTemp, 0) // x is temp 0
	b.Bytecode().EmitInt8(OpPushInt8, 1)
	b.Bytecode().Emit(OpSendPlus)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, []Value{FromSmallInt(5)})
	if result.SmallInt() != 6 {
		t.Errorf("result = %v, want 6", result)
	}
}

func TestInterpreterMultipleArgs(t *testing.T) {
	interp := NewInterpreter()

	// Method add:to: ^arg1 + arg2
	b := NewCompiledMethodBuilder("add:to:", 2)
	b.Bytecode().EmitByte(OpPushTemp, 0) // arg1
	b.Bytecode().EmitByte(OpPushTemp, 1) // arg2
	b.Bytecode().Emit(OpSendPlus)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, []Value{FromSmallInt(3), FromSmallInt(4)})
	if result.SmallInt() != 7 {
		t.Errorf("result = %v, want 7", result)
	}
}

// ---------------------------------------------------------------------------
// Control flow tests
// ---------------------------------------------------------------------------

func TestInterpreterJump(t *testing.T) {
	interp := NewInterpreter()

	// Method: jump over 1, return 2
	b := NewCompiledMethodBuilder("test", 0)
	bc := b.Bytecode()
	label := bc.NewLabel()
	bc.EmitJump(OpJump, label)
	bc.EmitInt8(OpPushInt8, 1) // skipped
	bc.Emit(OpReturnTop)       // skipped
	bc.Mark(label)
	bc.EmitInt8(OpPushInt8, 2)
	bc.Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if result.SmallInt() != 2 {
		t.Errorf("result = %v, want 2", result)
	}
}

func TestInterpreterJumpTrue(t *testing.T) {
	interp := NewInterpreter()

	// Method: true ifTrue: [^1] ifFalse: [^2]
	b := NewCompiledMethodBuilder("test", 0)
	bc := b.Bytecode()
	elseLabel := bc.NewLabel()
	endLabel := bc.NewLabel()

	bc.Emit(OpPushTrue)
	bc.EmitJump(OpJumpFalse, elseLabel)
	bc.EmitInt8(OpPushInt8, 1)
	bc.EmitJump(OpJump, endLabel)
	bc.Mark(elseLabel)
	bc.EmitInt8(OpPushInt8, 2)
	bc.Mark(endLabel)
	bc.Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if result.SmallInt() != 1 {
		t.Errorf("result = %v, want 1 (true branch)", result)
	}
}

func TestInterpreterJumpFalse(t *testing.T) {
	interp := NewInterpreter()

	// Method: false ifTrue: [^1] ifFalse: [^2]
	b := NewCompiledMethodBuilder("test", 0)
	bc := b.Bytecode()
	elseLabel := bc.NewLabel()
	endLabel := bc.NewLabel()

	bc.Emit(OpPushFalse)
	bc.EmitJump(OpJumpFalse, elseLabel)
	bc.EmitInt8(OpPushInt8, 1)
	bc.EmitJump(OpJump, endLabel)
	bc.Mark(elseLabel)
	bc.EmitInt8(OpPushInt8, 2)
	bc.Mark(endLabel)
	bc.Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if result.SmallInt() != 2 {
		t.Errorf("result = %v, want 2 (false branch)", result)
	}
}

func TestInterpreterLoop(t *testing.T) {
	interp := NewInterpreter()

	// Method: | i sum | i := 0. sum := 0. [i < 5] whileTrue: [sum := sum + i. i := i + 1]. ^sum
	// Simplified: count from 0 to 4, return sum (10)
	b := NewCompiledMethodBuilder("test", 0)
	b.SetNumTemps(2) // i=0, sum=1

	bc := b.Bytecode()
	loopStart := bc.NewLabel()
	loopEnd := bc.NewLabel()

	// i := 0
	bc.EmitInt8(OpPushInt8, 0)
	bc.EmitByte(OpStoreTemp, 0)
	bc.Emit(OpPOP)

	// sum := 0
	bc.EmitInt8(OpPushInt8, 0)
	bc.EmitByte(OpStoreTemp, 1)
	bc.Emit(OpPOP)

	// loop:
	bc.Mark(loopStart)

	// i < 5
	bc.EmitByte(OpPushTemp, 0)
	bc.EmitInt8(OpPushInt8, 5)
	bc.Emit(OpSendLT)
	bc.EmitJump(OpJumpFalse, loopEnd)

	// sum := sum + i
	bc.EmitByte(OpPushTemp, 1)
	bc.EmitByte(OpPushTemp, 0)
	bc.Emit(OpSendPlus)
	bc.EmitByte(OpStoreTemp, 1)
	bc.Emit(OpPOP)

	// i := i + 1
	bc.EmitByte(OpPushTemp, 0)
	bc.EmitInt8(OpPushInt8, 1)
	bc.Emit(OpSendPlus)
	bc.EmitByte(OpStoreTemp, 0)
	bc.Emit(OpPOP)

	// goto loop
	bc.EmitJumpAbsolute(OpJump, loopStart.position)

	// loopEnd:
	bc.Mark(loopEnd)
	bc.EmitByte(OpPushTemp, 1) // sum
	bc.Emit(OpReturnTop)

	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	// 0 + 1 + 2 + 3 + 4 = 10
	if result.SmallInt() != 10 {
		t.Errorf("result = %v, want 10", result)
	}
}

// ---------------------------------------------------------------------------
// Instance variable tests
// ---------------------------------------------------------------------------

func TestInterpreterIvarAccess(t *testing.T) {
	interp := NewInterpreter()

	// Create an object with slots
	class := NewClassWithInstVars("Point", nil, []string{"x", "y"})
	obj := class.NewInstance()
	obj.SetSlot(0, FromSmallInt(100))
	obj.SetSlot(1, FromSmallInt(200))

	// Method: ^x (instance variable 0)
	b := NewCompiledMethodBuilder("x", 0)
	b.Bytecode().EmitByte(OpPushIvar, 0)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, obj.ToValue(), nil)
	if result.SmallInt() != 100 {
		t.Errorf("result = %v, want 100", result)
	}
}

func TestInterpreterIvarStore(t *testing.T) {
	interp := NewInterpreter()

	// Create an object
	class := NewClassWithInstVars("Point", nil, []string{"x", "y"})
	obj := class.NewInstance()

	// Method: x := 42
	b := NewCompiledMethodBuilder("x:", 1)
	b.Bytecode().EmitByte(OpPushTemp, 0) // argument
	b.Bytecode().EmitByte(OpStoreIvar, 0)
	b.Bytecode().Emit(OpReturnSelf)
	m := b.Build()

	interp.Execute(m, obj.ToValue(), []Value{FromSmallInt(42)})

	if obj.GetSlot(0).SmallInt() != 42 {
		t.Errorf("ivar = %v, want 42", obj.GetSlot(0))
	}
}

// ---------------------------------------------------------------------------
// Complex expression tests
// ---------------------------------------------------------------------------

func TestInterpreterComplexExpression(t *testing.T) {
	interp := NewInterpreter()

	// Method: ^(1 + 2) * (3 + 4)  = 3 * 7 = 21
	b := NewCompiledMethodBuilder("test", 0)
	bc := b.Bytecode()
	bc.EmitInt8(OpPushInt8, 1)
	bc.EmitInt8(OpPushInt8, 2)
	bc.Emit(OpSendPlus)
	bc.EmitInt8(OpPushInt8, 3)
	bc.EmitInt8(OpPushInt8, 4)
	bc.Emit(OpSendPlus)
	bc.Emit(OpSendTimes)
	bc.Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, Nil, nil)
	if result.SmallInt() != 21 {
		t.Errorf("result = %v, want 21", result)
	}
}

// ---------------------------------------------------------------------------
// Method dispatch tests
// ---------------------------------------------------------------------------

func TestInterpreterMethodDispatch(t *testing.T) {
	interp := NewInterpreter()

	// Create a class with a method
	class := NewClass("Counter", nil)
	class.AddMethod0(interp.Selectors, "value", func(_ interface{}, recv Value) Value {
		return FromSmallInt(99)
	})

	// Create an instance
	obj := class.NewInstance()

	// Method that calls 'value' on self
	b := NewCompiledMethodBuilder("test", 0)
	sel := interp.Selectors.Intern("value")
	bc := b.Bytecode()
	bc.Emit(OpPushSelf)
	bc.EmitSend(OpSend, uint16(sel), 0)
	bc.Emit(OpReturnTop)
	m := b.Build()

	result := interp.Execute(m, obj.ToValue(), nil)
	if result.SmallInt() != 99 {
		t.Errorf("result = %v, want 99", result)
	}
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

func BenchmarkInterpreterSimple(b *testing.B) {
	interp := NewInterpreter()

	mb := NewCompiledMethodBuilder("test", 0)
	mb.Bytecode().EmitInt8(OpPushInt8, 42)
	mb.Bytecode().Emit(OpReturnTop)
	m := mb.Build()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = interp.Execute(m, Nil, nil)
	}
}

func BenchmarkInterpreterArithmetic(b *testing.B) {
	interp := NewInterpreter()

	mb := NewCompiledMethodBuilder("test", 0)
	bc := mb.Bytecode()
	bc.EmitInt8(OpPushInt8, 10)
	bc.EmitInt8(OpPushInt8, 20)
	bc.Emit(OpSendPlus)
	bc.EmitInt8(OpPushInt8, 5)
	bc.Emit(OpSendMinus)
	bc.Emit(OpReturnTop)
	m := mb.Build()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = interp.Execute(m, Nil, nil)
	}
}

func BenchmarkInterpreterLoop(b *testing.B) {
	interp := NewInterpreter()

	// Sum 0 to 99
	mb := NewCompiledMethodBuilder("test", 0)
	mb.SetNumTemps(2)
	bc := mb.Bytecode()
	loopStart := bc.NewLabel()
	loopEnd := bc.NewLabel()

	// i := 0
	bc.EmitInt8(OpPushInt8, 0)
	bc.EmitByte(OpStoreTemp, 0)
	bc.Emit(OpPOP)

	// sum := 0
	bc.EmitInt8(OpPushInt8, 0)
	bc.EmitByte(OpStoreTemp, 1)
	bc.Emit(OpPOP)

	bc.Mark(loopStart)
	bc.EmitByte(OpPushTemp, 0)
	bc.EmitInt8(OpPushInt8, 100)
	bc.Emit(OpSendLT)
	bc.EmitJump(OpJumpFalse, loopEnd)

	bc.EmitByte(OpPushTemp, 1)
	bc.EmitByte(OpPushTemp, 0)
	bc.Emit(OpSendPlus)
	bc.EmitByte(OpStoreTemp, 1)
	bc.Emit(OpPOP)

	bc.EmitByte(OpPushTemp, 0)
	bc.EmitInt8(OpPushInt8, 1)
	bc.Emit(OpSendPlus)
	bc.EmitByte(OpStoreTemp, 0)
	bc.Emit(OpPOP)

	bc.EmitJumpAbsolute(OpJump, loopStart.position)
	bc.Mark(loopEnd)
	bc.EmitByte(OpPushTemp, 1)
	bc.Emit(OpReturnTop)

	m := mb.Build()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = interp.Execute(m, Nil, nil)
	}
}

// ---------------------------------------------------------------------------
// Dynamic stack growth tests
// ---------------------------------------------------------------------------

func TestInterpreterStackGrowth(t *testing.T) {
	interp := NewInterpreter()

	// Verify initial stack size
	initialStackSize := len(interp.stack)
	if initialStackSize != 1024 {
		t.Errorf("initial stack size = %d, want 1024", initialStackSize)
	}

	// Push more values than initial capacity to trigger growth
	// We'll push 1100 values, which exceeds the initial 1024
	for i := 0; i < 1100; i++ {
		interp.push(FromSmallInt(int64(i)))
	}

	// Stack should have grown
	if len(interp.stack) <= initialStackSize {
		t.Errorf("stack did not grow: size = %d, want > %d", len(interp.stack), initialStackSize)
	}

	// Verify the values are correct
	for i := 1099; i >= 0; i-- {
		v := interp.pop()
		if v.SmallInt() != int64(i) {
			t.Errorf("popped value = %d, want %d", v.SmallInt(), i)
			break
		}
	}
}

func TestInterpreterFrameStackGrowth(t *testing.T) {
	interp := NewInterpreter()

	// Verify initial frame stack size
	initialFrameSize := len(interp.frames)
	if initialFrameSize != 256 {
		t.Errorf("initial frame stack size = %d, want 256", initialFrameSize)
	}

	// Create a simple method for frame pushing
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushNil)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	// Push more frames than initial capacity to trigger growth
	// We'll push 300 frames, which exceeds the initial 256
	for i := 0; i < 300; i++ {
		interp.pushFrame(m, Nil, nil)
	}

	// Frame stack should have grown
	if len(interp.frames) <= initialFrameSize {
		t.Errorf("frame stack did not grow: size = %d, want > %d", len(interp.frames), initialFrameSize)
	}

	// Clean up - pop all frames
	for i := 0; i < 300; i++ {
		interp.popFrame()
	}
}

// ---------------------------------------------------------------------------
// Block registry cleanup tests
// ---------------------------------------------------------------------------

func TestBlockRegistryCleanup(t *testing.T) {
	interp := NewInterpreter()

	// Create a simple block method
	blockMethod := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 0,
		Bytecode:    []byte{byte(OpPushNil), byte(OpBlockReturn)},
		Literals:    nil,
	}

	// Record initial registry size
	initialSize := len(blockRegistry)

	// Create a method frame (this will be the home frame for blocks)
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushNil)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	interp.pushFrame(m, Nil, nil)
	homeFrame := interp.fp

	// Create several blocks with this home frame
	numBlocks := 5
	blockIDs := make([]int, numBlocks)
	for i := 0; i < numBlocks; i++ {
		blockVal := interp.createBlockValue(blockMethod, nil)
		blockIDs[i] = int(blockVal.BlockID())
	}

	// Verify blocks were registered
	if len(blockRegistry) != initialSize+numBlocks {
		t.Errorf("block registry size after creation = %d, want %d", len(blockRegistry), initialSize+numBlocks)
	}

	// Verify blocks are tracked by home frame
	if len(blocksByHomeFrame[homeFrame]) != numBlocks {
		t.Errorf("blocks tracked for home frame = %d, want %d", len(blocksByHomeFrame[homeFrame]), numBlocks)
	}

	// Pop the home frame - this should clean up all blocks
	interp.popFrame()

	// Verify blocks were cleaned up
	for _, id := range blockIDs {
		if _, exists := blockRegistry[id]; exists {
			t.Errorf("block %d still in registry after home frame popped", id)
		}
	}

	// Verify home frame tracking was cleaned up
	if _, exists := blocksByHomeFrame[homeFrame]; exists {
		t.Errorf("home frame %d still tracked after being popped", homeFrame)
	}
}

func TestBlockRegistryMultipleFrames(t *testing.T) {
	interp := NewInterpreter()

	// Create a simple block method
	blockMethod := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 0,
		Bytecode:    []byte{byte(OpPushNil), byte(OpBlockReturn)},
		Literals:    nil,
	}

	// Create method for frames
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushNil)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	// Push first frame and create blocks
	interp.pushFrame(m, Nil, nil)
	frame1 := interp.fp
	block1 := interp.createBlockValue(blockMethod, nil)
	block1ID := int(block1.BlockID())

	// Push second frame and create blocks
	interp.pushFrame(m, Nil, nil)
	frame2 := interp.fp
	block2 := interp.createBlockValue(blockMethod, nil)
	block2ID := int(block2.BlockID())

	// Verify both blocks exist
	if _, exists := blockRegistry[block1ID]; !exists {
		t.Error("block1 not in registry")
	}
	if _, exists := blockRegistry[block2ID]; !exists {
		t.Error("block2 not in registry")
	}

	// Pop frame2 - should only clean up block2
	interp.popFrame()

	// block1 should still exist, block2 should be gone
	if _, exists := blockRegistry[block1ID]; !exists {
		t.Error("block1 was incorrectly cleaned up")
	}
	if _, exists := blockRegistry[block2ID]; exists {
		t.Error("block2 was not cleaned up")
	}

	// Pop frame1 - should clean up block1
	interp.popFrame()

	if _, exists := blockRegistry[block1ID]; exists {
		t.Error("block1 was not cleaned up after frame1 popped")
	}

	// Verify both home frames are no longer tracked
	if _, exists := blocksByHomeFrame[frame1]; exists {
		t.Errorf("frame1 still tracked")
	}
	if _, exists := blocksByHomeFrame[frame2]; exists {
		t.Errorf("frame2 still tracked")
	}
}

// TestGarbageCollection verifies that unreachable objects are collected.
func TestGarbageCollection(t *testing.T) {
	vm := NewVM()

	// Create some arrays (which go into keepAlive)
	initialCount := vm.KeepAliveCount()

	arr1 := vm.NewArray(3)
	arr2 := vm.NewArray(5)
	_ = vm.NewArray(2) // arr3 - intentionally unreachable

	if vm.KeepAliveCount() != initialCount+3 {
		t.Errorf("keepAlive count after creation = %d, want %d", vm.KeepAliveCount(), initialCount+3)
	}

	// Push arr1 onto the stack (making it reachable)
	vm.interpreter.push(arr1)

	// Store arr2 in globals (making it reachable)
	vm.Globals["testArray"] = arr2

	// arr3 is not reachable (not on stack, not in globals)

	// Run GC
	collected := vm.CollectGarbage()

	// arr3 should have been collected
	if collected != 1 {
		t.Errorf("collected = %d, want 1", collected)
	}

	// keepAlive should now have 2 fewer (arr3 collected)
	if vm.KeepAliveCount() != initialCount+2 {
		t.Errorf("keepAlive count after GC = %d, want %d", vm.KeepAliveCount(), initialCount+2)
	}

	// Clean up
	vm.interpreter.pop()
	delete(vm.Globals, "testArray")
}

// TestGarbageCollectionNestedObjects verifies that objects referenced by other objects are not collected.
func TestGarbageCollectionNestedObjects(t *testing.T) {
	vm := NewVM()

	initialCount := vm.KeepAliveCount()

	// Create an array that holds another array
	inner := vm.NewArray(2)
	outer := vm.NewArray(1)

	// Store inner in outer's slot
	if outer.IsObject() {
		obj := ObjectFromValue(outer)
		obj.SetSlot(0, inner)
	}

	// Push only outer onto stack
	vm.interpreter.push(outer)

	// inner is reachable through outer, so both should survive GC
	collected := vm.CollectGarbage()

	if collected != 0 {
		t.Errorf("collected = %d, want 0 (both should be reachable)", collected)
	}

	if vm.KeepAliveCount() != initialCount+2 {
		t.Errorf("keepAlive count = %d, want %d", vm.KeepAliveCount(), initialCount+2)
	}

	// Now pop outer from stack - both become unreachable
	vm.interpreter.pop()

	collected = vm.CollectGarbage()

	if collected != 2 {
		t.Errorf("collected = %d, want 2 (both should be unreachable)", collected)
	}
}

// TestInterpreterStoreGlobal verifies that OpStoreGlobal actually stores to globals.
func TestInterpreterStoreGlobal(t *testing.T) {
	interp := NewInterpreter()
	interp.Symbols = NewSymbolTable()

	// Create a symbol for the global name
	globalNameID := interp.Symbols.Intern("myGlobal")
	globalNameValue := interp.Symbols.SymbolValue("myGlobal")

	// Method: myGlobal := 42. ^myGlobal
	b := NewCompiledMethodBuilder("test", 0)

	// Add the global name as a literal
	litIdx := b.AddLiteral(globalNameValue)

	// Push value to store
	b.Bytecode().EmitInt8(OpPushInt8, 42)

	// Store to global (value stays on stack)
	b.Bytecode().EmitUint16(OpStoreGlobal, uint16(litIdx))

	// Pop the stored value
	b.Bytecode().Emit(OpPOP)

	// Push the global back to verify it was stored
	b.Bytecode().EmitUint16(OpPushGlobal, uint16(litIdx))

	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	// Verify global doesn't exist before execution
	if _, exists := interp.Globals["myGlobal"]; exists {
		t.Fatal("global should not exist before execution")
	}

	result := interp.Execute(m, Nil, nil)

	// Verify the global was stored
	if val, exists := interp.Globals["myGlobal"]; !exists {
		t.Error("global 'myGlobal' was not stored")
	} else if !val.IsSmallInt() || val.SmallInt() != 42 {
		t.Errorf("global value = %v, want 42", val)
	}

	// Verify the result (pushed from global) is correct
	if !result.IsSmallInt() || result.SmallInt() != 42 {
		t.Errorf("result = %v, want 42", result)
	}

	_ = globalNameID // silence unused warning
}
