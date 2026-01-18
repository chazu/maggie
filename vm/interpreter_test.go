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
