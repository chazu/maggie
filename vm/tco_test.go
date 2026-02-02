package vm

import (
	"encoding/binary"
	"testing"
)

// ---------------------------------------------------------------------------
// Tail-Call Optimization (TCO) tests
// ---------------------------------------------------------------------------

// buildTCOFactorial creates a class with a tail-recursive factorial method:
//
//	factorial: n acc: acc
//	    n = 0 ifTrue: [^acc].
//	    ^self factorial: n - 1 acc: acc * n
//
// The method uses an accumulator pattern that is in tail position.
func buildTCOFactorial(interp *Interpreter) (*Class, *CompiledMethod) {
	class := NewClass("TCOTest", nil)
	interp.Classes.Register(class)

	selID := interp.Selectors.Intern("factorial:acc:")

	// Build bytecode by hand using OpTailSend
	b := NewCompiledMethodBuilder("factorial:acc:", 2)
	b.SetNumTemps(2) // n=0, acc=1
	bc := b.Bytecode()

	// n = 0?
	endLabel := bc.NewLabel()
	bc.EmitByte(OpPushTemp, 0) // n
	bc.EmitInt8(OpPushInt8, 0)
	bc.Emit(OpSendEQ)
	bc.EmitJump(OpJumpFalse, endLabel)

	// Base case: return acc
	bc.EmitByte(OpPushTemp, 1) // acc
	bc.Emit(OpReturnTop)

	// Recursive case: self factorial: n-1 acc: acc*n
	bc.Mark(endLabel)
	bc.Emit(OpPushSelf) // receiver = self

	// arg1: n - 1
	bc.EmitByte(OpPushTemp, 0) // n
	bc.EmitInt8(OpPushInt8, 1)
	bc.Emit(OpSendMinus) // n - 1

	// arg2: acc * n
	bc.EmitByte(OpPushTemp, 1) // acc
	bc.EmitByte(OpPushTemp, 0) // n
	bc.Emit(OpSendTimes) // acc * n

	// Tail send
	bc.EmitSend(OpTailSend, uint16(selID), 2)
	bc.Emit(OpReturnTop)

	m := b.Build()
	m.SetClass(class)
	class.VTable.AddMethod(selID, m)

	return class, m
}

// TestTCOFactorialCorrectResult verifies that tail-recursive factorial
// produces the correct result.
func TestTCOFactorialCorrectResult(t *testing.T) {
	vm := NewVM()
	class, m := buildTCOFactorial(vm.interpreter)
	obj := class.NewInstance()

	// factorial(5, 1) = 120
	result := vm.interpreter.Execute(m, obj.ToValue(), []Value{
		FromSmallInt(5), FromSmallInt(1),
	})
	if !result.IsSmallInt() || result.SmallInt() != 120 {
		t.Errorf("factorial(5) = %v, want 120", result)
	}

	// factorial(0, 1) = 1 (base case)
	result = vm.interpreter.Execute(m, obj.ToValue(), []Value{
		FromSmallInt(0), FromSmallInt(1),
	})
	if !result.IsSmallInt() || result.SmallInt() != 1 {
		t.Errorf("factorial(0) = %v, want 1", result)
	}

	// factorial(1, 1) = 1
	result = vm.interpreter.Execute(m, obj.ToValue(), []Value{
		FromSmallInt(1), FromSmallInt(1),
	})
	if !result.IsSmallInt() || result.SmallInt() != 1 {
		t.Errorf("factorial(1) = %v, want 1", result)
	}

	// factorial(10, 1) = 3628800
	result = vm.interpreter.Execute(m, obj.ToValue(), []Value{
		FromSmallInt(10), FromSmallInt(1),
	})
	if !result.IsSmallInt() || result.SmallInt() != 3628800 {
		t.Errorf("factorial(10) = %v, want 3628800", result)
	}
}

// TestTCODoesNotGrowStack verifies that tail-recursive calls do not
// increase the frame depth. With a very low MaxFrameDepth, a deeply
// recursive call would overflow without TCO but succeeds with it.
func TestTCODoesNotGrowStack(t *testing.T) {
	vm := NewVM()

	// Set a very low frame depth limit. Without TCO, even sum(30)
	// would need 30+ frames. With TCO, thousands of iterations work.
	vm.interpreter.MaxFrameDepth = 10

	class, m := buildTCOSum(vm.interpreter)
	obj := class.NewInstance()

	// sum(5000, 0) would need 5000+ frames without TCO.
	// With TCO (MaxFrameDepth=10), it should succeed.
	result := vm.interpreter.Execute(m, obj.ToValue(), []Value{
		FromSmallInt(5000), FromSmallInt(0),
	})

	// sum(5000) = 5000 * 5001 / 2 = 12502500
	if !result.IsSmallInt() || result.SmallInt() != 12502500 {
		t.Errorf("sum(5000) = %v, want 12502500", result)
	}
}

// buildTCOSum creates a class with a tail-recursive sum method:
//
//	sum: n acc: acc
//	    n = 0 ifTrue: [^acc].
//	    ^self sum: n - 1 acc: acc + n
func buildTCOSum(interp *Interpreter) (*Class, *CompiledMethod) {
	class := NewClass("TCOSum", nil)
	interp.Classes.Register(class)

	selID := interp.Selectors.Intern("sum:acc:")

	b := NewCompiledMethodBuilder("sum:acc:", 2)
	b.SetNumTemps(2)
	bc := b.Bytecode()

	// n = 0?
	endLabel := bc.NewLabel()
	bc.EmitByte(OpPushTemp, 0) // n
	bc.EmitInt8(OpPushInt8, 0)
	bc.Emit(OpSendEQ)
	bc.EmitJump(OpJumpFalse, endLabel)

	// Base case: return acc
	bc.EmitByte(OpPushTemp, 1) // acc
	bc.Emit(OpReturnTop)

	// Recursive case
	bc.Mark(endLabel)
	bc.Emit(OpPushSelf)

	// n - 1
	bc.EmitByte(OpPushTemp, 0)
	bc.EmitInt8(OpPushInt8, 1)
	bc.Emit(OpSendMinus)

	// acc + n
	bc.EmitByte(OpPushTemp, 1)
	bc.EmitByte(OpPushTemp, 0)
	bc.Emit(OpSendPlus)

	// Tail send
	bc.EmitSend(OpTailSend, uint16(selID), 2)
	bc.Emit(OpReturnTop)

	m := b.Build()
	m.SetClass(class)
	class.VTable.AddMethod(selID, m)

	return class, m
}

// TestTCOSumCorrectResult verifies tail-recursive sum produces correct results.
func TestTCOSumCorrectResult(t *testing.T) {
	vm := NewVM()
	class, m := buildTCOSum(vm.interpreter)
	obj := class.NewInstance()

	// sum(10, 0) = 55
	result := vm.interpreter.Execute(m, obj.ToValue(), []Value{
		FromSmallInt(10), FromSmallInt(0),
	})
	if !result.IsSmallInt() || result.SmallInt() != 55 {
		t.Errorf("sum(10) = %v, want 55", result)
	}

	// sum(100, 0) = 5050
	result = vm.interpreter.Execute(m, obj.ToValue(), []Value{
		FromSmallInt(100), FromSmallInt(0),
	})
	if !result.IsSmallInt() || result.SmallInt() != 5050 {
		t.Errorf("sum(100) = %v, want 5050", result)
	}
}

// TestTCOSumDeepRecursion verifies deep tail recursion with tight frame limit.
func TestTCOSumDeepRecursion(t *testing.T) {
	vm := NewVM()
	vm.interpreter.MaxFrameDepth = 10 // Very tight limit

	class, m := buildTCOSum(vm.interpreter)
	obj := class.NewInstance()

	// sum(10000, 0) = 50005000
	// Without TCO this would overflow with MaxFrameDepth=10.
	result := vm.interpreter.Execute(m, obj.ToValue(), []Value{
		FromSmallInt(10000), FromSmallInt(0),
	})
	if !result.IsSmallInt() || result.SmallInt() != 50005000 {
		t.Errorf("sum(10000) = %v, want 50005000", result)
	}
}

// TestTCONonTailCallStillCreatesFrames verifies that normal (non-tail) calls
// still create frames. A recursive method without TCO should overflow at
// the configured limit.
func TestTCONonTailCallStillCreatesFrames(t *testing.T) {
	vm := NewVM()
	vm.interpreter.MaxFrameDepth = 30

	class := NewClass("NonTail", nil)
	vm.Classes.Register(class)

	// Method: recurse [ ^self recurse ]  (using OpSend, not OpTailSend)
	recurseSelID := vm.Selectors.Intern("recurse")

	b := NewCompiledMethodBuilder("recurse", 0)
	bc := b.Bytecode()
	bc.Emit(OpPushSelf)
	bc.EmitSend(OpSend, uint16(recurseSelID), 0) // regular send, not tail
	bc.Emit(OpReturnTop)
	m := b.Build()
	m.SetClass(class)
	class.VTable.AddMethod(recurseSelID, m)

	obj := class.NewInstance()

	// Should overflow because we're NOT using OpTailSend
	var caught bool
	func() {
		defer func() {
			if r := recover(); r != nil {
				caught = true
			}
		}()
		vm.interpreter.Execute(m, obj.ToValue(), nil)
	}()

	if !caught {
		t.Fatal("expected stack overflow for non-tail recursive call")
	}
}

// TestTCOMutualRecursionNotOptimized verifies that mutual recursion
// (method A calling method B on self) uses OpTailSend but falls back
// to normal send because the target method is different.
func TestTCOMutualRecursionNotOptimized(t *testing.T) {
	vm := NewVM()
	vm.interpreter.MaxFrameDepth = 30

	class := NewClass("MutualRecursion", nil)
	vm.Classes.Register(class)

	// Method A: ping [ ^self pong ]
	// Method B: pong [ ^self ping ]
	pingSelID := vm.Selectors.Intern("ping")
	pongSelID := vm.Selectors.Intern("pong")

	// ping method with OpTailSend to pong
	bPing := NewCompiledMethodBuilder("ping", 0)
	bcPing := bPing.Bytecode()
	bcPing.Emit(OpPushSelf)
	bcPing.EmitSend(OpTailSend, uint16(pongSelID), 0) // tail send to pong
	bcPing.Emit(OpReturnTop)
	mPing := bPing.Build()
	mPing.SetClass(class)
	class.VTable.AddMethod(pingSelID, mPing)

	// pong method with OpTailSend to ping
	bPong := NewCompiledMethodBuilder("pong", 0)
	bcPong := bPong.Bytecode()
	bcPong.Emit(OpPushSelf)
	bcPong.EmitSend(OpTailSend, uint16(pingSelID), 0) // tail send to ping
	bcPong.Emit(OpReturnTop)
	mPong := bPong.Build()
	mPong.SetClass(class)
	class.VTable.AddMethod(pongSelID, mPong)

	obj := class.NewInstance()

	// Should overflow because TCO only optimizes same-method calls
	var caught bool
	func() {
		defer func() {
			if r := recover(); r != nil {
				caught = true
			}
		}()
		vm.interpreter.Execute(mPing, obj.ToValue(), nil)
	}()

	if !caught {
		t.Fatal("expected stack overflow for mutual recursion (TCO should not optimize)")
	}
}

// TestTCODifferentReceiverNotOptimized verifies that when a tail send
// goes to a different receiver (not self), TCO does not reuse the frame.
//
// We set up: ClassA.bounce: other [ ^other bounce: self ]
// This means objA sends to objB, which sends back to objA, etc.
// Because each call dispatches to a DIFFERENT CompiledMethod (mA vs mB),
// and the receiver alternates, TCO never applies and the stack overflows.
func TestTCODifferentReceiverNotOptimized(t *testing.T) {
	vm := NewVM()
	vm.interpreter.MaxFrameDepth = 30

	classA := NewClass("ClassA", nil)
	classB := NewClass("ClassB", nil)
	vm.Classes.Register(classA)
	vm.Classes.Register(classB)

	selID := vm.Selectors.Intern("bounce:")

	// ClassA.bounce: other [ ^other bounce: self ]
	// Receiver is other (different object), argument is self
	bA := NewCompiledMethodBuilder("bounce:", 1)
	bA.SetNumTemps(1)
	bcA := bA.Bytecode()
	bcA.EmitByte(OpPushTemp, 0) // other (receiver)
	bcA.Emit(OpPushSelf)        // self (argument)
	bcA.EmitSend(OpTailSend, uint16(selID), 1)
	bcA.Emit(OpReturnTop)
	mA := bA.Build()
	mA.SetClass(classA)
	classA.VTable.AddMethod(selID, mA)

	// ClassB.bounce: other [ ^other bounce: self ]
	bB := NewCompiledMethodBuilder("bounce:", 1)
	bB.SetNumTemps(1)
	bcB := bB.Bytecode()
	bcB.EmitByte(OpPushTemp, 0) // other (receiver)
	bcB.Emit(OpPushSelf)        // self (argument)
	bcB.EmitSend(OpTailSend, uint16(selID), 1)
	bcB.Emit(OpReturnTop)
	mB := bB.Build()
	mB.SetClass(classB)
	classB.VTable.AddMethod(selID, mB)

	objA := classA.NewInstance()
	objB := classB.NewInstance()

	// objA.bounce:(objB) -> sends to objB (rcvr != self, no TCO) ->
	// objB.bounce:(objA) -> sends to objA (rcvr != self, no TCO) -> ...
	// Each call creates a new frame, so it overflows.
	var caught bool
	func() {
		defer func() {
			if r := recover(); r != nil {
				caught = true
			}
		}()
		vm.interpreter.Execute(mA, objA.ToValue(), []Value{objB.ToValue()})
	}()

	if !caught {
		t.Fatal("expected stack overflow for cross-object bouncing (no TCO)")
	}
}

// TestTCOZeroArgMethod verifies TCO works with a zero-argument
// tail-recursive method.
func TestTCOZeroArgMethod(t *testing.T) {
	vm := NewVM()
	vm.interpreter.MaxFrameDepth = 20

	class := NewClass("Countdown", nil)
	vm.Classes.Register(class)

	// We'll use instance variables to track state
	class = NewClassWithInstVars("Countdown", nil, []string{"count", "result"})
	vm.Classes.Register(class)

	countdownSelID := vm.Selectors.Intern("countdown")

	// countdown
	//   count = 0 ifTrue: [^result].
	//   count := count - 1.
	//   result := result + 1.
	//   ^self countdown
	b := NewCompiledMethodBuilder("countdown", 0)
	bc := b.Bytecode()

	// count (ivar 0) = 0?
	endLabel := bc.NewLabel()
	bc.EmitByte(OpPushIvar, 0) // count
	bc.EmitInt8(OpPushInt8, 0)
	bc.Emit(OpSendEQ)
	bc.EmitJump(OpJumpFalse, endLabel)

	// return result
	bc.EmitByte(OpPushIvar, 1)
	bc.Emit(OpReturnTop)

	// count := count - 1
	bc.Mark(endLabel)
	bc.EmitByte(OpPushIvar, 0)
	bc.EmitInt8(OpPushInt8, 1)
	bc.Emit(OpSendMinus)
	bc.EmitByte(OpStoreIvar, 0)
	bc.Emit(OpPOP)

	// result := result + 1
	bc.EmitByte(OpPushIvar, 1)
	bc.EmitInt8(OpPushInt8, 1)
	bc.Emit(OpSendPlus)
	bc.EmitByte(OpStoreIvar, 1)
	bc.Emit(OpPOP)

	// ^self countdown (tail send)
	bc.Emit(OpPushSelf)
	bc.EmitSend(OpTailSend, uint16(countdownSelID), 0)
	bc.Emit(OpReturnTop)

	m := b.Build()
	m.SetClass(class)
	class.VTable.AddMethod(countdownSelID, m)

	obj := class.NewInstance()
	obj.SetSlot(0, FromSmallInt(1000)) // count = 1000
	obj.SetSlot(1, FromSmallInt(0))    // result = 0

	result := vm.interpreter.Execute(m, obj.ToValue(), nil)
	if !result.IsSmallInt() || result.SmallInt() != 1000 {
		t.Errorf("countdown(1000) = %v, want 1000", result)
	}
}

// TestTCOOpcodeBytecodeEncoding verifies OpTailSend encodes correctly.
func TestTCOOpcodeBytecodeEncoding(t *testing.T) {
	b := NewBytecodeBuilder()

	// Emit a tail send: selector=42, argc=2
	b.EmitSend(OpTailSend, 42, 2)

	bytes := b.Bytes()
	if len(bytes) != 4 {
		t.Fatalf("bytecode length = %d, want 4", len(bytes))
	}
	if Opcode(bytes[0]) != OpTailSend {
		t.Errorf("opcode = %02X, want %02X (OpTailSend)", bytes[0], OpTailSend)
	}
	sel := binary.LittleEndian.Uint16(bytes[1:3])
	if sel != 42 {
		t.Errorf("selector = %d, want 42", sel)
	}
	if bytes[3] != 2 {
		t.Errorf("argc = %d, want 2", bytes[3])
	}
}

// TestTCOOpcodeDisassembly verifies OpTailSend disassembles correctly.
func TestTCOOpcodeDisassembly(t *testing.T) {
	b := NewBytecodeBuilder()
	b.EmitSend(OpTailSend, 42, 2)

	dis := Disassemble(b.Bytes())
	expected := "0000  TAIL_SEND selector=42 argc=2"
	if dis != expected {
		t.Errorf("disassembly = %q, want %q", dis, expected)
	}
}

// TestTCOFibonacci verifies tail-recursive Fibonacci with accumulator pattern.
//
//	fib: n a: a b: b
//	    n = 0 ifTrue: [^a].
//	    ^self fib: n - 1 a: b b: a + b
func TestTCOFibonacci(t *testing.T) {
	vm := NewVM()
	vm.interpreter.MaxFrameDepth = 15 // Very tight limit

	class := NewClass("Fib", nil)
	vm.Classes.Register(class)

	selID := vm.Selectors.Intern("fib:a:b:")

	b := NewCompiledMethodBuilder("fib:a:b:", 3)
	b.SetNumTemps(3) // n=0, a=1, b=2
	bc := b.Bytecode()

	// n = 0?
	endLabel := bc.NewLabel()
	bc.EmitByte(OpPushTemp, 0) // n
	bc.EmitInt8(OpPushInt8, 0)
	bc.Emit(OpSendEQ)
	bc.EmitJump(OpJumpFalse, endLabel)

	// Base: return a
	bc.EmitByte(OpPushTemp, 1)
	bc.Emit(OpReturnTop)

	// Recursive case
	bc.Mark(endLabel)
	bc.Emit(OpPushSelf)

	// arg1: n - 1
	bc.EmitByte(OpPushTemp, 0)
	bc.EmitInt8(OpPushInt8, 1)
	bc.Emit(OpSendMinus)

	// arg2: b
	bc.EmitByte(OpPushTemp, 2)

	// arg3: a + b
	bc.EmitByte(OpPushTemp, 1)
	bc.EmitByte(OpPushTemp, 2)
	bc.Emit(OpSendPlus)

	// Tail send
	bc.EmitSend(OpTailSend, uint16(selID), 3)
	bc.Emit(OpReturnTop)

	m := b.Build()
	m.SetClass(class)
	class.VTable.AddMethod(selID, m)

	obj := class.NewInstance()

	// fib(10, 0, 1) = 55
	result := vm.interpreter.Execute(m, obj.ToValue(), []Value{
		FromSmallInt(10), FromSmallInt(0), FromSmallInt(1),
	})
	if !result.IsSmallInt() || result.SmallInt() != 55 {
		t.Errorf("fib(10) = %v, want 55", result)
	}

	// fib(20, 0, 1) = 6765
	result = vm.interpreter.Execute(m, obj.ToValue(), []Value{
		FromSmallInt(20), FromSmallInt(0), FromSmallInt(1),
	})
	if !result.IsSmallInt() || result.SmallInt() != 6765 {
		t.Errorf("fib(20) = %v, want 6765", result)
	}

	// fib(1000, 0, 1) should NOT overflow (only needs 1 frame)
	// The result will overflow SmallInt but should not crash
	result = vm.interpreter.Execute(m, obj.ToValue(), []Value{
		FromSmallInt(50), FromSmallInt(0), FromSmallInt(1),
	})
	// fib(50) = 12586269025
	if !result.IsSmallInt() || result.SmallInt() != 12586269025 {
		t.Errorf("fib(50) = %v, want 12586269025", result)
	}
}

// TestTCOFrameDepthConstant verifies frame depth stays constant during
// tail-recursive execution by checking i.fp before and during execution.
func TestTCOFrameDepthConstant(t *testing.T) {
	vm := NewVM()

	class := NewClass("DepthCheck", nil)
	vm.Classes.Register(class)

	// Create a method that records the max frame depth via a global variable:
	// countdown: n
	//   n = 0 ifTrue: [^n].
	//   ^self countdown: n - 1
	selID := vm.Selectors.Intern("countdown:")

	b := NewCompiledMethodBuilder("countdown:", 1)
	b.SetNumTemps(1)
	bc := b.Bytecode()

	endLabel := bc.NewLabel()
	bc.EmitByte(OpPushTemp, 0)
	bc.EmitInt8(OpPushInt8, 0)
	bc.Emit(OpSendEQ)
	bc.EmitJump(OpJumpFalse, endLabel)
	bc.EmitByte(OpPushTemp, 0)
	bc.Emit(OpReturnTop)

	bc.Mark(endLabel)
	bc.Emit(OpPushSelf)
	bc.EmitByte(OpPushTemp, 0)
	bc.EmitInt8(OpPushInt8, 1)
	bc.Emit(OpSendMinus)
	bc.EmitSend(OpTailSend, uint16(selID), 1)
	bc.Emit(OpReturnTop)

	m := b.Build()
	m.SetClass(class)
	class.VTable.AddMethod(selID, m)

	obj := class.NewInstance()

	// Record initial fp
	initialFP := vm.interpreter.fp

	// Execute with deep recursion
	result := vm.interpreter.Execute(m, obj.ToValue(), []Value{FromSmallInt(5000)})

	// After execution, fp should be back to initial
	if vm.interpreter.fp != initialFP {
		t.Errorf("fp after execution = %d, want %d (same as before)", vm.interpreter.fp, initialFP)
	}

	if !result.IsSmallInt() || result.SmallInt() != 0 {
		t.Errorf("countdown(5000) = %v, want 0", result)
	}
}
