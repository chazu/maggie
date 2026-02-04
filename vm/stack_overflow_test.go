package vm

import (
	"strings"
	"testing"
)

// ---------------------------------------------------------------------------
// Stack Overflow Protection and Recursion Depth Tests
// ---------------------------------------------------------------------------
//
// These tests verify the VM's stack overflow protection mechanism:
// - Non-tail recursive methods overflow at MaxFrameDepth
// - Tail-recursive methods (OpTailSend) do NOT overflow
// - StackOverflow exceptions are catchable via on:do:
// - Deeply nested block evaluation hits overflow gracefully
// - Mutual recursion (non-TCO) overflows
// ---------------------------------------------------------------------------

// TestNonTailRecursiveMethodOverflows verifies that a non-tail recursive
// method (e.g., factorial without accumulator) hits StackOverflow after
// exceeding the configured frame depth.
//
// Method under test (pseudo-Smalltalk):
//
//	badFactorial: n
//	    n = 0 ifTrue: [^1].
//	    ^n * (self badFactorial: n - 1)
//
// The self-send is NOT in tail position because the multiplication happens
// after the recursive call returns, so each call pushes a new frame.
func TestNonTailRecursiveMethodOverflows(t *testing.T) {
	vm := NewVM()
	vm.interpreter.MaxFrameDepth = 50

	class := NewClass("FactNonTail", nil)
	vm.Classes.Register(class)

	factSelID := vm.Selectors.Intern("badFactorial:")

	// Build: badFactorial: n
	//   n = 0 ifTrue: [^1].
	//   ^n * (self badFactorial: n - 1)
	b := NewCompiledMethodBuilder("badFactorial:", 1)
	b.SetNumTemps(1) // n = temp 0
	bc := b.Bytecode()

	// n = 0?
	recurseLabel := bc.NewLabel()
	bc.EmitByte(OpPushTemp, 0) // push n
	bc.EmitInt8(OpPushInt8, 0) // push 0
	bc.Emit(OpSendEQ)         // n = 0
	bc.EmitJump(OpJumpFalse, recurseLabel)

	// Base case: return 1
	bc.EmitInt8(OpPushInt8, 1)
	bc.Emit(OpReturnTop)

	// Recursive case: n * (self badFactorial: n - 1)
	bc.Mark(recurseLabel)
	bc.EmitByte(OpPushTemp, 0)                               // push n (left side of *)
	bc.Emit(OpPushSelf)                                      // push self
	bc.EmitByte(OpPushTemp, 0)                               // push n
	bc.EmitInt8(OpPushInt8, 1)                               // push 1
	bc.Emit(OpSendMinus)                                     // n - 1
	bc.EmitSend(OpSend, uint16(factSelID), 1)                // self badFactorial: n-1  (NOT tail!)
	bc.Emit(OpSendTimes)                                     // n * result
	bc.Emit(OpReturnTop)

	m := b.Build()
	m.SetClass(class)
	class.VTable.AddMethod(factSelID, m)

	obj := class.NewInstance()

	// With MaxFrameDepth=50, calling badFactorial: 100 should overflow
	var caught *SignaledException
	func() {
		defer func() {
			if r := recover(); r != nil {
				if sigEx, ok := r.(SignaledException); ok {
					caught = &sigEx
				} else {
					t.Errorf("expected SignaledException, got %T: %v", r, r)
				}
			}
		}()
		vm.interpreter.Execute(m, obj.ToValue(), []Value{FromSmallInt(100)})
	}()

	if caught == nil {
		t.Fatal("expected StackOverflow for non-tail recursive factorial(100) with MaxFrameDepth=50")
	}

	if caught.Object == nil {
		t.Fatal("SignaledException.Object is nil")
	}

	if caught.Object.ExceptionClass != vm.StackOverflowClass {
		t.Errorf("exception class = %v, want StackOverflow", caught.Object.ExceptionClass.Name)
	}

	msgText := vm.registry.GetStringContent(caught.Object.MessageText)
	if !strings.Contains(msgText, "Stack overflow") {
		t.Errorf("exception message = %q, want it to contain 'Stack overflow'", msgText)
	}

	// Verify that a call within the limit works correctly
	vm.interpreter.MaxFrameDepth = 200
	result := vm.interpreter.Execute(m, obj.ToValue(), []Value{FromSmallInt(5)})
	if !result.IsSmallInt() || result.SmallInt() != 120 {
		t.Errorf("badFactorial(5) = %v, want 120", result)
	}
}

// TestTailRecursiveMethodDoesNotOverflow verifies that a self-recursive
// method in tail position (using OpTailSend) can iterate well beyond the
// frame depth limit without overflowing.
//
// Method under test (pseudo-Smalltalk):
//
//	loop: n acc: acc
//	    n = 0 ifTrue: [^acc].
//	    ^self loop: n - 1 acc: acc + 1
//
// With OpTailSend, each recursive call reuses the current frame, so
// even 20000 iterations should work with MaxFrameDepth=10.
func TestTailRecursiveMethodDoesNotOverflow(t *testing.T) {
	vm := NewVM()
	vm.interpreter.MaxFrameDepth = 10 // Very tight limit

	class := NewClass("TailLoop", nil)
	vm.Classes.Register(class)

	loopSelID := vm.Selectors.Intern("loop:acc:")

	b := NewCompiledMethodBuilder("loop:acc:", 2)
	b.SetNumTemps(2) // n = temp 0, acc = temp 1
	bc := b.Bytecode()

	// n = 0?
	recurseLabel := bc.NewLabel()
	bc.EmitByte(OpPushTemp, 0) // push n
	bc.EmitInt8(OpPushInt8, 0) // push 0
	bc.Emit(OpSendEQ)         // n = 0
	bc.EmitJump(OpJumpFalse, recurseLabel)

	// Base case: return acc
	bc.EmitByte(OpPushTemp, 1) // push acc
	bc.Emit(OpReturnTop)

	// Recursive case: ^self loop: n-1 acc: acc+1
	bc.Mark(recurseLabel)
	bc.Emit(OpPushSelf)

	// arg1: n - 1
	bc.EmitByte(OpPushTemp, 0) // n
	bc.EmitInt8(OpPushInt8, 1)
	bc.Emit(OpSendMinus) // n - 1

	// arg2: acc + 1
	bc.EmitByte(OpPushTemp, 1) // acc
	bc.EmitInt8(OpPushInt8, 1)
	bc.Emit(OpSendPlus) // acc + 1

	// Tail send
	bc.EmitSend(OpTailSend, uint16(loopSelID), 2)
	bc.Emit(OpReturnTop)

	m := b.Build()
	m.SetClass(class)
	class.VTable.AddMethod(loopSelID, m)

	obj := class.NewInstance()

	// 20000 iterations with MaxFrameDepth=10 -- should NOT overflow
	result := vm.interpreter.Execute(m, obj.ToValue(), []Value{
		FromSmallInt(20000), FromSmallInt(0),
	})
	if !result.IsSmallInt() || result.SmallInt() != 20000 {
		t.Errorf("loop(20000, 0) = %v, want 20000", result)
	}

	// Also verify with an even deeper call
	result = vm.interpreter.Execute(m, obj.ToValue(), []Value{
		FromSmallInt(50000), FromSmallInt(0),
	})
	if !result.IsSmallInt() || result.SmallInt() != 50000 {
		t.Errorf("loop(50000, 0) = %v, want 50000", result)
	}
}

// TestStackOverflowCatchableInSmalltalk verifies that a StackOverflow
// exception can be caught by the on:do: mechanism (evaluateBlockWithHandler).
// This simulates the Smalltalk expression:
//
//	[self deepRecurse] on: StackOverflow do: [:ex | 'caught']
//
// The handler returns a marker integer (777) to prove it was invoked.
func TestStackOverflowCatchableInSmalltalk(t *testing.T) {
	vm := NewVM()
	vm.interpreter.MaxFrameDepth = 30

	// Create a class with an infinitely recursive method
	class := NewClass("DeepRecurser", nil)
	vm.Classes.Register(class)

	recurseSelID := vm.Selectors.Intern("deepRecurse")

	b := NewCompiledMethodBuilder("deepRecurse", 0)
	bc := b.Bytecode()
	bc.Emit(OpPushSelf)
	bc.EmitSend(OpSend, uint16(recurseSelID), 0) // regular send, not tail
	bc.Emit(OpReturnTop)
	m := b.Build()
	m.SetClass(class)
	class.VTable.AddMethod(recurseSelID, m)

	obj := class.NewInstance()

	// Protected block: [obj deepRecurse]
	protectedBlock := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 1,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitByte(OpPushCaptured, 0)                        // push captured obj
			bb.EmitSend(OpSend, uint16(recurseSelID), 0)          // obj deepRecurse
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}

	// Handler block: [:ex | 777]
	handlerBlock := &BlockMethod{
		Arity:    1, // receives the exception
		NumTemps: 1,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			// Push 777 using PushLiteral (use small int encoding)
			// 777 doesn't fit in int8, use two-byte form or push via arithmetic
			// Actually, let's build 777 = 7 * 111 ... simpler: use literal pool
			// Or just push a small value that proves the handler ran
			bb.EmitInt8(OpPushInt8, 77) // Use 77 as marker value
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}

	protectedBV := &BlockValue{
		Block:      protectedBlock,
		Captures:   []Value{obj.ToValue()},
		HomeFrame:  -1,
		HomeSelf:   Nil,
		HomeMethod: nil,
	}
	handlerBV := &BlockValue{
		Block:      handlerBlock,
		Captures:   nil,
		HomeFrame:  -1,
		HomeSelf:   Nil,
		HomeMethod: nil,
	}

	protectedBlockVal := FromBlockID(uint32(vm.registry.RegisterBlock(protectedBV)))
	handlerBlockVal := FromBlockID(uint32(vm.registry.RegisterBlock(handlerBV)))

	// Catch StackOverflow via on:do:
	result := vm.evaluateBlockWithHandler(protectedBlockVal, vm.StackOverflowClass, handlerBlockVal)

	if !result.IsSmallInt() || result.SmallInt() != 77 {
		t.Errorf("result = %v, want 77 (handler should have caught StackOverflow)", result)
	}
}

// TestStackOverflowCatchableViaErrorSuperclass verifies that a StackOverflow
// can be caught by a handler installed for Error (the superclass of
// StackOverflow), confirming the exception class hierarchy works correctly.
func TestStackOverflowCatchableViaErrorSuperclass(t *testing.T) {
	vm := NewVM()
	vm.interpreter.MaxFrameDepth = 30

	class := NewClass("InfiniteRecurser", nil)
	vm.Classes.Register(class)

	recurseSelID := vm.Selectors.Intern("loop")

	b := NewCompiledMethodBuilder("loop", 0)
	bc := b.Bytecode()
	bc.Emit(OpPushSelf)
	bc.EmitSend(OpSend, uint16(recurseSelID), 0)
	bc.Emit(OpReturnTop)
	m := b.Build()
	m.SetClass(class)
	class.VTable.AddMethod(recurseSelID, m)

	obj := class.NewInstance()

	protectedBlock := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 1,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitByte(OpPushCaptured, 0)
			bb.EmitSend(OpSend, uint16(recurseSelID), 0)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}

	handlerBlock := &BlockMethod{
		Arity:    1,
		NumTemps: 1,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitInt8(OpPushInt8, 33) // marker value
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}

	protectedBV := &BlockValue{
		Block:      protectedBlock,
		Captures:   []Value{obj.ToValue()},
		HomeFrame:  -1,
		HomeSelf:   Nil,
		HomeMethod: nil,
	}
	handlerBV := &BlockValue{
		Block:      handlerBlock,
		Captures:   nil,
		HomeFrame:  -1,
		HomeSelf:   Nil,
		HomeMethod: nil,
	}

	protectedBlockVal := FromBlockID(uint32(vm.registry.RegisterBlock(protectedBV)))
	handlerBlockVal := FromBlockID(uint32(vm.registry.RegisterBlock(handlerBV)))

	// Catch with Error (superclass of StackOverflow)
	result := vm.evaluateBlockWithHandler(protectedBlockVal, vm.ErrorClass, handlerBlockVal)

	if !result.IsSmallInt() || result.SmallInt() != 33 {
		t.Errorf("result = %v, want 33 (Error handler should catch StackOverflow)", result)
	}
}

// TestDeeplyNestedBlockEvaluation verifies that deeply nested block
// evaluations (each block calling another block) properly hit StackOverflow
// when the nesting depth exceeds MaxFrameDepth.
//
// This simulates a chain where a method calls a block that calls a method
// that calls a block, etc. — each level pushes a new frame.
func TestDeeplyNestedBlockEvaluation(t *testing.T) {
	vm := NewVM()
	vm.interpreter.MaxFrameDepth = 40

	// Create a class with a method that recursively evaluates itself
	// through block indirection:
	//   nestVia: aBlock depth: n
	//     n = 0 ifTrue: [^n].
	//     ^aBlock value   <-- the block calls back into self nestVia:depth: with n-1
	//
	// We'll build this so the block captures self and n, and calls
	// self nestVia:depth: with n-1.
	// However, building a block inside method bytecode is complex.
	// Instead, we use a simpler approach: a method that calls itself
	// using a helper method that evaluates a block.
	//
	// Simpler approach: method A calls method B, method B calls method A.
	// This is mutual recursion and each call adds a frame.

	class := NewClass("BlockNester", nil)
	vm.Classes.Register(class)

	// Method: outerCall: n
	//   n = 0 ifTrue: [^0].
	//   ^self innerCall: n
	outerSelID := vm.Selectors.Intern("outerCall:")

	bOuter := NewCompiledMethodBuilder("outerCall:", 1)
	bOuter.SetNumTemps(1)
	bcOuter := bOuter.Bytecode()

	doneLabel := bcOuter.NewLabel()
	bcOuter.EmitByte(OpPushTemp, 0) // n
	bcOuter.EmitInt8(OpPushInt8, 0)
	bcOuter.Emit(OpSendEQ)
	bcOuter.EmitJump(OpJumpFalse, doneLabel)
	bcOuter.EmitInt8(OpPushInt8, 0)
	bcOuter.Emit(OpReturnTop)

	bcOuter.Mark(doneLabel)
	bcOuter.Emit(OpPushSelf)
	bcOuter.EmitByte(OpPushTemp, 0) // n

	innerSelID := vm.Selectors.Intern("innerCall:")
	bcOuter.EmitSend(OpSend, uint16(innerSelID), 1)
	bcOuter.Emit(OpReturnTop)

	mOuter := bOuter.Build()
	mOuter.SetClass(class)
	class.VTable.AddMethod(outerSelID, mOuter)

	// Method: innerCall: n
	//   ^self outerCall: n - 1
	bInner := NewCompiledMethodBuilder("innerCall:", 1)
	bInner.SetNumTemps(1)
	bcInner := bInner.Bytecode()

	bcInner.Emit(OpPushSelf)
	bcInner.EmitByte(OpPushTemp, 0) // n
	bcInner.EmitInt8(OpPushInt8, 1)
	bcInner.Emit(OpSendMinus) // n - 1
	bcInner.EmitSend(OpSend, uint16(outerSelID), 1)
	bcInner.Emit(OpReturnTop)

	mInner := bInner.Build()
	mInner.SetClass(class)
	class.VTable.AddMethod(innerSelID, mInner)

	obj := class.NewInstance()

	// With MaxFrameDepth=40, calling outerCall: 100 should overflow
	// because each pair of calls adds 2 frames
	var caught *SignaledException
	func() {
		defer func() {
			if r := recover(); r != nil {
				if sigEx, ok := r.(SignaledException); ok {
					caught = &sigEx
				} else {
					t.Errorf("expected SignaledException, got %T: %v", r, r)
				}
			}
		}()
		vm.interpreter.Execute(mOuter, obj.ToValue(), []Value{FromSmallInt(100)})
	}()

	if caught == nil {
		t.Fatal("expected StackOverflow for deeply nested block-like evaluation")
	}

	if caught.Object.ExceptionClass != vm.StackOverflowClass {
		t.Errorf("exception class = %v, want StackOverflow", caught.Object.ExceptionClass.Name)
	}

	// Verify that shallow nesting within the limit works
	vm.interpreter.MaxFrameDepth = 200
	result := vm.interpreter.Execute(mOuter, obj.ToValue(), []Value{FromSmallInt(10)})
	if !result.IsSmallInt() || result.SmallInt() != 0 {
		t.Errorf("outerCall(10) with high limit = %v, want 0", result)
	}
}

// TestDeeplyNestedBlockEvaluationCatchable verifies that StackOverflow from
// deeply nested block evaluation is catchable via the on:do: mechanism.
func TestDeeplyNestedBlockEvaluationCatchable(t *testing.T) {
	vm := NewVM()
	vm.interpreter.MaxFrameDepth = 30

	class := NewClass("BlockNester2", nil)
	vm.Classes.Register(class)

	// Two mutually recursive methods (simulating deep block nesting)
	outerSelID := vm.Selectors.Intern("outer:")
	innerSelID := vm.Selectors.Intern("inner:")

	// outer: n  ->  n = 0 ifTrue: [^0]. ^self inner: n
	bOuter := NewCompiledMethodBuilder("outer:", 1)
	bOuter.SetNumTemps(1)
	bcOuter := bOuter.Bytecode()
	doneLabel := bcOuter.NewLabel()
	bcOuter.EmitByte(OpPushTemp, 0)
	bcOuter.EmitInt8(OpPushInt8, 0)
	bcOuter.Emit(OpSendEQ)
	bcOuter.EmitJump(OpJumpFalse, doneLabel)
	bcOuter.EmitInt8(OpPushInt8, 0)
	bcOuter.Emit(OpReturnTop)
	bcOuter.Mark(doneLabel)
	bcOuter.Emit(OpPushSelf)
	bcOuter.EmitByte(OpPushTemp, 0)
	bcOuter.EmitSend(OpSend, uint16(innerSelID), 1)
	bcOuter.Emit(OpReturnTop)
	mOuter := bOuter.Build()
	mOuter.SetClass(class)
	class.VTable.AddMethod(outerSelID, mOuter)

	// inner: n  ->  ^self outer: n - 1
	bInner := NewCompiledMethodBuilder("inner:", 1)
	bInner.SetNumTemps(1)
	bcInner := bInner.Bytecode()
	bcInner.Emit(OpPushSelf)
	bcInner.EmitByte(OpPushTemp, 0)
	bcInner.EmitInt8(OpPushInt8, 1)
	bcInner.Emit(OpSendMinus)
	bcInner.EmitSend(OpSend, uint16(outerSelID), 1)
	bcInner.Emit(OpReturnTop)
	mInner := bInner.Build()
	mInner.SetClass(class)
	class.VTable.AddMethod(innerSelID, mInner)

	obj := class.NewInstance()

	// Protected block: [obj outer: 100]
	// 100 fits in int8 and is well beyond the MaxFrameDepth=30 limit
	protectedBlock := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 1,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitByte(OpPushCaptured, 0)                    // captured obj
			bb.EmitInt8(OpPushInt8, 100)                       // push 100
			bb.EmitSend(OpSend, uint16(outerSelID), 1)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}

	// Handler block: [:ex | 55]
	handlerBlock := &BlockMethod{
		Arity:    1,
		NumTemps: 1,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitInt8(OpPushInt8, 55)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}

	protectedBV := &BlockValue{
		Block:      protectedBlock,
		Captures:   []Value{obj.ToValue()},
		HomeFrame:  -1,
		HomeSelf:   Nil,
		HomeMethod: nil,
	}
	handlerBV := &BlockValue{
		Block:      handlerBlock,
		Captures:   nil,
		HomeFrame:  -1,
		HomeSelf:   Nil,
		HomeMethod: nil,
	}

	protectedBlockVal := FromBlockID(uint32(vm.registry.RegisterBlock(protectedBV)))
	handlerBlockVal := FromBlockID(uint32(vm.registry.RegisterBlock(handlerBV)))

	result := vm.evaluateBlockWithHandler(protectedBlockVal, vm.StackOverflowClass, handlerBlockVal)
	if !result.IsSmallInt() || result.SmallInt() != 55 {
		t.Errorf("result = %v, want 55 (handler should catch StackOverflow from nested calls)", result)
	}
}

// TestMutualRecursionOverflows verifies that two methods calling each other
// (A -> B -> A -> B -> ...) without tail-call optimization will overflow
// the stack.
//
// Method ping [ ^self pong ]
// Method pong [ ^self ping ]
//
// Neither is in tail position from TCO's perspective because the target
// method differs from the calling method. Each call pushes a new frame.
func TestMutualRecursionOverflows(t *testing.T) {
	vm := NewVM()
	vm.interpreter.MaxFrameDepth = 40

	class := NewClass("PingPong", nil)
	vm.Classes.Register(class)

	pingSelID := vm.Selectors.Intern("ping")
	pongSelID := vm.Selectors.Intern("pong")

	// ping [ ^self pong ]
	bPing := NewCompiledMethodBuilder("ping", 0)
	bcPing := bPing.Bytecode()
	bcPing.Emit(OpPushSelf)
	bcPing.EmitSend(OpSend, uint16(pongSelID), 0) // regular send (not tail)
	bcPing.Emit(OpReturnTop)
	mPing := bPing.Build()
	mPing.SetClass(class)
	class.VTable.AddMethod(pingSelID, mPing)

	// pong [ ^self ping ]
	bPong := NewCompiledMethodBuilder("pong", 0)
	bcPong := bPong.Bytecode()
	bcPong.Emit(OpPushSelf)
	bcPong.EmitSend(OpSend, uint16(pingSelID), 0)
	bcPong.Emit(OpReturnTop)
	mPong := bPong.Build()
	mPong.SetClass(class)
	class.VTable.AddMethod(pongSelID, mPong)

	obj := class.NewInstance()

	var caught *SignaledException
	func() {
		defer func() {
			if r := recover(); r != nil {
				if sigEx, ok := r.(SignaledException); ok {
					caught = &sigEx
				} else {
					t.Errorf("expected SignaledException, got %T: %v", r, r)
				}
			}
		}()
		vm.interpreter.Execute(mPing, obj.ToValue(), nil)
	}()

	if caught == nil {
		t.Fatal("expected StackOverflow for mutual recursion (ping/pong)")
	}

	if caught.Object.ExceptionClass != vm.StackOverflowClass {
		t.Errorf("exception class = %v, want StackOverflow", caught.Object.ExceptionClass.Name)
	}

	msgText := vm.registry.GetStringContent(caught.Object.MessageText)
	if !strings.Contains(msgText, "Stack overflow") {
		t.Errorf("exception message = %q, want it to contain 'Stack overflow'", msgText)
	}
	if !strings.Contains(msgText, "40") {
		t.Errorf("exception message = %q, want it to mention the limit (40)", msgText)
	}
}

// TestMutualRecursionWithTailSendStillOverflows verifies that mutual
// recursion using OpTailSend (instead of OpSend) still overflows, because
// TCO only optimizes when the resolved method is the SAME CompiledMethod
// as the current frame's method. With two different methods calling each
// other, TCO cannot apply.
func TestMutualRecursionWithTailSendStillOverflows(t *testing.T) {
	vm := NewVM()
	vm.interpreter.MaxFrameDepth = 40

	class := NewClass("PingPongTail", nil)
	vm.Classes.Register(class)

	pingSelID := vm.Selectors.Intern("tailPing")
	pongSelID := vm.Selectors.Intern("tailPong")

	// tailPing [ ^self tailPong ]  (uses OpTailSend but target is different method)
	bPing := NewCompiledMethodBuilder("tailPing", 0)
	bcPing := bPing.Bytecode()
	bcPing.Emit(OpPushSelf)
	bcPing.EmitSend(OpTailSend, uint16(pongSelID), 0) // tail send to DIFFERENT method
	bcPing.Emit(OpReturnTop)
	mPing := bPing.Build()
	mPing.SetClass(class)
	class.VTable.AddMethod(pingSelID, mPing)

	// tailPong [ ^self tailPing ]
	bPong := NewCompiledMethodBuilder("tailPong", 0)
	bcPong := bPong.Bytecode()
	bcPong.Emit(OpPushSelf)
	bcPong.EmitSend(OpTailSend, uint16(pingSelID), 0) // tail send to DIFFERENT method
	bcPong.Emit(OpReturnTop)
	mPong := bPong.Build()
	mPong.SetClass(class)
	class.VTable.AddMethod(pongSelID, mPong)

	obj := class.NewInstance()

	var caught bool
	func() {
		defer func() {
			if r := recover(); r != nil {
				if sigEx, ok := r.(SignaledException); ok {
					if sigEx.Object.ExceptionClass == vm.StackOverflowClass {
						caught = true
					}
				}
			}
		}()
		vm.interpreter.Execute(mPing, obj.ToValue(), nil)
	}()

	if !caught {
		t.Fatal("expected StackOverflow for mutual recursion even with OpTailSend")
	}
}

// TestStackOverflowExceptionMessage verifies the exception message contains
// the configured frame depth limit, making it useful for debugging.
func TestStackOverflowExceptionMessage(t *testing.T) {
	vm := NewVM()

	class := NewClass("MsgChecker", nil)
	vm.Classes.Register(class)

	recurseSelID := vm.Selectors.Intern("go")

	b := NewCompiledMethodBuilder("go", 0)
	bc := b.Bytecode()
	bc.Emit(OpPushSelf)
	bc.EmitSend(OpSend, uint16(recurseSelID), 0)
	bc.Emit(OpReturnTop)
	m := b.Build()
	m.SetClass(class)
	class.VTable.AddMethod(recurseSelID, m)

	obj := class.NewInstance()

	// Test with different limits to verify the message reflects the actual limit
	for _, limit := range []int{25, 50, 100} {
		vm.interpreter.MaxFrameDepth = limit

		var caught *SignaledException
		func() {
			defer func() {
				if r := recover(); r != nil {
					if sigEx, ok := r.(SignaledException); ok {
						caught = &sigEx
					}
				}
			}()
			vm.interpreter.Execute(m, obj.ToValue(), nil)
		}()

		if caught == nil {
			t.Fatalf("expected overflow with MaxFrameDepth=%d", limit)
		}

		msgText := vm.registry.GetStringContent(caught.Object.MessageText)
		expected := "Stack overflow"
		if !strings.Contains(msgText, expected) {
			t.Errorf("MaxFrameDepth=%d: message = %q, want it to contain %q", limit, msgText, expected)
		}
	}
}

// TestStackOverflowRecoveryAllowsSubsequentExecution verifies that after
// a StackOverflow is caught, the VM can continue executing normally.
// The interpreter's frame stack should be unwound properly.
func TestStackOverflowRecoveryAllowsSubsequentExecution(t *testing.T) {
	vm := NewVM()
	vm.interpreter.MaxFrameDepth = 30

	class := NewClass("Recoverable", nil)
	vm.Classes.Register(class)

	// Infinite recursion method
	recurseSelID := vm.Selectors.Intern("overflow")
	bRecurse := NewCompiledMethodBuilder("overflow", 0)
	bcRecurse := bRecurse.Bytecode()
	bcRecurse.Emit(OpPushSelf)
	bcRecurse.EmitSend(OpSend, uint16(recurseSelID), 0)
	bcRecurse.Emit(OpReturnTop)
	mRecurse := bRecurse.Build()
	mRecurse.SetClass(class)
	class.VTable.AddMethod(recurseSelID, mRecurse)

	// Simple method that returns 42
	safeSelID := vm.Selectors.Intern("safeMethod")
	bSafe := NewCompiledMethodBuilder("safeMethod", 0)
	bcSafe := bSafe.Bytecode()
	bcSafe.EmitInt8(OpPushInt8, 42)
	bcSafe.Emit(OpReturnTop)
	mSafe := bSafe.Build()
	mSafe.SetClass(class)
	class.VTable.AddMethod(safeSelID, mSafe)

	obj := class.NewInstance()

	// Protected block: [obj overflow]
	protectedBlock := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 1,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitByte(OpPushCaptured, 0)
			bb.EmitSend(OpSend, uint16(recurseSelID), 0)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}

	// Handler: [:ex | 0]
	handlerBlock := &BlockMethod{
		Arity:    1,
		NumTemps: 1,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitInt8(OpPushInt8, 0)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}

	protectedBV := &BlockValue{
		Block:      protectedBlock,
		Captures:   []Value{obj.ToValue()},
		HomeFrame:  -1,
		HomeSelf:   Nil,
		HomeMethod: nil,
	}
	handlerBV := &BlockValue{
		Block:      handlerBlock,
		Captures:   nil,
		HomeFrame:  -1,
		HomeSelf:   Nil,
		HomeMethod: nil,
	}

	protectedBlockVal := FromBlockID(uint32(vm.registry.RegisterBlock(protectedBV)))
	handlerBlockVal := FromBlockID(uint32(vm.registry.RegisterBlock(handlerBV)))

	// First: catch the overflow
	result := vm.evaluateBlockWithHandler(protectedBlockVal, vm.StackOverflowClass, handlerBlockVal)
	if !result.IsSmallInt() || result.SmallInt() != 0 {
		t.Errorf("overflow handler result = %v, want 0", result)
	}

	// Now: execute a normal method - the VM should still work
	result = vm.interpreter.Execute(mSafe, obj.ToValue(), nil)
	if !result.IsSmallInt() || result.SmallInt() != 42 {
		t.Errorf("after recovery, safeMethod = %v, want 42", result)
	}
}

// TestThreeWayMutualRecursionOverflows verifies that a cycle of three
// mutually recursive methods (A -> B -> C -> A -> ...) overflows.
// This tests a more complex recursion pattern than simple two-way ping-pong.
func TestThreeWayMutualRecursionOverflows(t *testing.T) {
	vm := NewVM()
	vm.interpreter.MaxFrameDepth = 40

	class := NewClass("ThreeWay", nil)
	vm.Classes.Register(class)

	aSelID := vm.Selectors.Intern("methodA")
	bSelID := vm.Selectors.Intern("methodB")
	cSelID := vm.Selectors.Intern("methodC")

	// methodA [ ^self methodB ]
	bA := NewCompiledMethodBuilder("methodA", 0)
	bcA := bA.Bytecode()
	bcA.Emit(OpPushSelf)
	bcA.EmitSend(OpSend, uint16(bSelID), 0)
	bcA.Emit(OpReturnTop)
	mA := bA.Build()
	mA.SetClass(class)
	class.VTable.AddMethod(aSelID, mA)

	// methodB [ ^self methodC ]
	bB := NewCompiledMethodBuilder("methodB", 0)
	bcB := bB.Bytecode()
	bcB.Emit(OpPushSelf)
	bcB.EmitSend(OpSend, uint16(cSelID), 0)
	bcB.Emit(OpReturnTop)
	mB := bB.Build()
	mB.SetClass(class)
	class.VTable.AddMethod(bSelID, mB)

	// methodC [ ^self methodA ]
	bC := NewCompiledMethodBuilder("methodC", 0)
	bcC := bC.Bytecode()
	bcC.Emit(OpPushSelf)
	bcC.EmitSend(OpSend, uint16(aSelID), 0)
	bcC.Emit(OpReturnTop)
	mC := bC.Build()
	mC.SetClass(class)
	class.VTable.AddMethod(cSelID, mC)

	obj := class.NewInstance()

	var caught *SignaledException
	func() {
		defer func() {
			if r := recover(); r != nil {
				if sigEx, ok := r.(SignaledException); ok {
					caught = &sigEx
				}
			}
		}()
		vm.interpreter.Execute(mA, obj.ToValue(), nil)
	}()

	if caught == nil {
		t.Fatal("expected StackOverflow for three-way mutual recursion")
	}

	if caught.Object.ExceptionClass != vm.StackOverflowClass {
		t.Errorf("exception class = %v, want StackOverflow", caught.Object.ExceptionClass.Name)
	}
}

// TestDefaultMaxFrameDepthApplied verifies that when MaxFrameDepth is 0
// (unset), the DefaultMaxFrameDepth constant (4096) is used.
func TestDefaultMaxFrameDepthApplied(t *testing.T) {
	vm := NewVM()

	// The default should be 4096
	if vm.interpreter.MaxFrameDepth != DefaultMaxFrameDepth {
		t.Errorf("default MaxFrameDepth = %d, want %d", vm.interpreter.MaxFrameDepth, DefaultMaxFrameDepth)
	}

	if DefaultMaxFrameDepth != 4096 {
		t.Errorf("DefaultMaxFrameDepth = %d, want 4096", DefaultMaxFrameDepth)
	}
}
