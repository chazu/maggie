package vm

import (
	"strings"
	"testing"
)

// ---------------------------------------------------------------------------
// Exception Value Tests
// ---------------------------------------------------------------------------

func TestExceptionValueCreation(t *testing.T) {
	vm := NewVM()
	ex := &ExceptionObject{
		MessageText: vm.registry.NewStringValue("test error"),
		Resumable:   true,
	}

	id := vm.registry.RegisterException(ex)
	val := FromExceptionID(id)

	if !val.IsException() {
		t.Error("RegisterException should return an exception value")
	}

	retrieved := vm.registry.GetException(val.ExceptionID())
	if retrieved != ex {
		t.Error("GetException should return the original exception")
	}
}

func TestExceptionValueIsException(t *testing.T) {
	vm := NewVM()
	ex := &ExceptionObject{MessageText: Nil}
	id := vm.registry.RegisterException(ex)
	val := FromExceptionID(id)

	if !val.IsException() {
		t.Error("Exception value should be an exception")
	}

	// Other values should not be exceptions
	if FromSmallInt(42).IsException() {
		t.Error("SmallInt should not be an exception")
	}
	if Nil.IsException() {
		t.Error("Nil should not be an exception")
	}
}

// ---------------------------------------------------------------------------
// Exception Class Hierarchy Tests
// ---------------------------------------------------------------------------

func TestExceptionClassesRegistered(t *testing.T) {
	vm := NewVM()

	classes := []struct {
		name string
		ptr  *Class
	}{
		{"Exception", vm.ExceptionClass},
		{"Error", vm.ErrorClass},
		{"MessageNotUnderstood", vm.MessageNotUnderstoodClass},
		{"ZeroDivide", vm.ZeroDivideClass},
		{"SubscriptOutOfBounds", vm.SubscriptOutOfBoundsClass},
		{"StackOverflow", vm.StackOverflowClass},
		{"Warning", vm.WarningClass},
		{"Halt", vm.HaltClass},
		{"Notification", vm.NotificationClass},
	}

	for _, tc := range classes {
		if tc.ptr == nil {
			t.Errorf("%s class should be registered", tc.name)
			continue
		}
		if tc.ptr.Name != tc.name {
			t.Errorf("%s class name should be '%s', got '%s'", tc.name, tc.name, tc.ptr.Name)
		}

		// Check it's in globals
		global := vm.globals[tc.name]
		if global == Nil {
			t.Errorf("%s should be in globals", tc.name)
		}
	}
}

func TestExceptionClassHierarchy(t *testing.T) {
	vm := NewVM()

	// Error should be a subclass of Exception
	if vm.ErrorClass.Superclass != vm.ExceptionClass {
		t.Error("Error should be a subclass of Exception")
	}

	// MessageNotUnderstood should be a subclass of Error
	if vm.MessageNotUnderstoodClass.Superclass != vm.ErrorClass {
		t.Error("MessageNotUnderstood should be a subclass of Error")
	}

	// ZeroDivide should be a subclass of Error
	if vm.ZeroDivideClass.Superclass != vm.ErrorClass {
		t.Error("ZeroDivide should be a subclass of Error")
	}

	// Warning should be a subclass of Exception
	if vm.WarningClass.Superclass != vm.ExceptionClass {
		t.Error("Warning should be a subclass of Exception")
	}
}

// ---------------------------------------------------------------------------
// Exception Signaling Tests
// ---------------------------------------------------------------------------

func TestExceptionSignalWithHandler(t *testing.T) {
	vm := NewVM()

	// Test that the handler infrastructure is set up correctly
	// by verifying handler stack operations

	// Create a mock handler
	handler := &ExceptionHandler{
		ExceptionClass: vm.ErrorClass,
		FrameIndex:     0,
	}

	// Push and verify
	vm.interpreter.PushExceptionHandler(handler)
	if vm.interpreter.exceptionHandlers != handler {
		t.Error("Handler should be installed")
	}

	// Find handler for Error
	found := vm.interpreter.FindHandler(vm.ErrorClass)
	if found != handler {
		t.Error("Should find the installed handler")
	}

	// Find handler for subclass (ZeroDivide)
	found = vm.interpreter.FindHandler(vm.ZeroDivideClass)
	if found != handler {
		t.Error("Should find handler for subclass")
	}

	// Cleanup
	vm.interpreter.PopExceptionHandler()
}

func TestOnDoBasic(t *testing.T) {
	vm := NewVM()

	// Test that on:do: is registered on Block
	selID := vm.Selectors.Intern("on:do:")
	method := vm.BlockClass.VTable.Lookup(selID)
	if method == nil {
		t.Error("Block should have on:do: method")
	}
}

func TestEnsureMethodExists(t *testing.T) {
	vm := NewVM()

	// Test that ensure: is registered on Block
	selID := vm.Selectors.Intern("ensure:")
	method := vm.BlockClass.VTable.Lookup(selID)
	if method == nil {
		t.Error("Block should have ensure: method")
	}
}

func TestIfCurtailedMethodExists(t *testing.T) {
	vm := NewVM()

	// Verify ifCurtailed: exists
	selID := vm.Selectors.Intern("ifCurtailed:")
	method := vm.BlockClass.VTable.Lookup(selID)
	if method == nil {
		t.Error("Block should have ifCurtailed: method")
	}
}

// ---------------------------------------------------------------------------
// Exception Primitives Tests
// ---------------------------------------------------------------------------

func TestExceptionMessageText(t *testing.T) {
	vm := NewVM()

	// Create an exception with a message
	ex := &ExceptionObject{
		ExceptionClass: vm.ErrorClass,
		MessageText:    vm.registry.NewStringValue("Something went wrong"),
	}
	id := vm.registry.RegisterException(ex)
	exVal := FromExceptionID(id)

	// Get messageText
	result := vm.Send(exVal, "messageText", nil)
	if !IsStringValue(result) {
		t.Fatal("messageText should return a string")
	}

	msg := vm.registry.GetStringContent(result)
	if msg != "Something went wrong" {
		t.Errorf("Expected 'Something went wrong', got '%s'", msg)
	}
}

func TestExceptionDescription(t *testing.T) {
	vm := NewVM()

	// Create an exception with a message
	ex := &ExceptionObject{
		ExceptionClass: vm.ErrorClass,
		MessageText:    vm.registry.NewStringValue("test error"),
	}
	id := vm.registry.RegisterException(ex)
	exVal := FromExceptionID(id)

	result := vm.Send(exVal, "description", nil)
	if !IsStringValue(result) {
		t.Fatal("description should return a string")
	}

	desc := vm.registry.GetStringContent(result)
	if !strings.Contains(desc, "Error") {
		t.Errorf("description should contain 'Error', got '%s'", desc)
	}
	if !strings.Contains(desc, "test error") {
		t.Errorf("description should contain message, got '%s'", desc)
	}
}

func TestExceptionIsResumable(t *testing.T) {
	vm := NewVM()

	// Resumable exception
	ex1 := &ExceptionObject{
		ExceptionClass: vm.ExceptionClass,
		Resumable:      true,
	}
	id1 := vm.registry.RegisterException(ex1)
	exVal1 := FromExceptionID(id1)

	result := vm.Send(exVal1, "isResumable", nil)
	if result != True {
		t.Error("Resumable exception should return true for isResumable")
	}

	// Non-resumable exception
	ex2 := &ExceptionObject{
		ExceptionClass: vm.ExceptionClass,
		Resumable:      false,
	}
	id2 := vm.registry.RegisterException(ex2)
	exVal2 := FromExceptionID(id2)

	result = vm.Send(exVal2, "isResumable", nil)
	if result != False {
		t.Error("Non-resumable exception should return false for isResumable")
	}
}

// ---------------------------------------------------------------------------
// Exception Handler Stack Tests
// ---------------------------------------------------------------------------

func TestExceptionHandlerStack(t *testing.T) {
	vm := NewVM()
	i := vm.interpreter

	// Initially empty
	if i.exceptionHandlers != nil {
		t.Error("Handler stack should be initially empty")
	}

	// Push a handler
	h1 := &ExceptionHandler{
		ExceptionClass: vm.ErrorClass,
		FrameIndex:     0,
	}
	i.PushExceptionHandler(h1)

	if i.exceptionHandlers != h1 {
		t.Error("Handler should be on top of stack")
	}

	// Push another handler
	h2 := &ExceptionHandler{
		ExceptionClass: vm.ExceptionClass,
		FrameIndex:     1,
	}
	i.PushExceptionHandler(h2)

	if i.exceptionHandlers != h2 {
		t.Error("New handler should be on top")
	}
	if h2.Prev != h1 {
		t.Error("Previous handler should be linked")
	}

	// Pop handlers
	popped := i.PopExceptionHandler()
	if popped != h2 {
		t.Error("Should pop h2 first")
	}

	popped = i.PopExceptionHandler()
	if popped != h1 {
		t.Error("Should pop h1 second")
	}

	popped = i.PopExceptionHandler()
	if popped != nil {
		t.Error("Should return nil when stack is empty")
	}
}

func TestFindHandler(t *testing.T) {
	vm := NewVM()
	i := vm.interpreter

	// Install handlers for different exception types
	h1 := &ExceptionHandler{
		ExceptionClass: vm.ErrorClass,
		FrameIndex:     0,
	}
	h2 := &ExceptionHandler{
		ExceptionClass: vm.ExceptionClass,
		FrameIndex:     1,
	}
	i.PushExceptionHandler(h1)
	i.PushExceptionHandler(h2)

	// Find handler for ZeroDivide (subclass of Error)
	handler := i.FindHandler(vm.ZeroDivideClass)
	if handler != h2 {
		t.Error("Should find Exception handler for ZeroDivide (it's checked first)")
	}

	// With only h1 installed
	i.PopExceptionHandler()

	handler = i.FindHandler(vm.ZeroDivideClass)
	if handler != h1 {
		t.Error("Should find Error handler for ZeroDivide")
	}

	// Find handler for Warning (not a subclass of Error)
	handler = i.FindHandler(vm.WarningClass)
	if handler != nil {
		t.Error("Should not find Error handler for Warning")
	}
}

func TestIsKindOf(t *testing.T) {
	vm := NewVM()
	i := vm.interpreter

	// ZeroDivide isKindOf: Error -> true
	if !i.isKindOf(vm.ZeroDivideClass, vm.ErrorClass) {
		t.Error("ZeroDivide should be kind of Error")
	}

	// ZeroDivide isKindOf: Exception -> true
	if !i.isKindOf(vm.ZeroDivideClass, vm.ExceptionClass) {
		t.Error("ZeroDivide should be kind of Exception")
	}

	// Error isKindOf: ZeroDivide -> false
	if i.isKindOf(vm.ErrorClass, vm.ZeroDivideClass) {
		t.Error("Error should not be kind of ZeroDivide")
	}

	// Warning isKindOf: Error -> false
	if i.isKindOf(vm.WarningClass, vm.ErrorClass) {
		t.Error("Warning should not be kind of Error")
	}

	// Warning isKindOf: Exception -> true
	if !i.isKindOf(vm.WarningClass, vm.ExceptionClass) {
		t.Error("Warning should be kind of Exception")
	}
}

// ---------------------------------------------------------------------------
// ClassFor Tests
// ---------------------------------------------------------------------------

func TestClassForException(t *testing.T) {
	vm := NewVM()

	// Create an Error exception
	ex := &ExceptionObject{
		ExceptionClass: vm.ErrorClass,
	}
	id := vm.registry.RegisterException(ex)
	exVal := FromExceptionID(id)

	class := vm.ClassFor(exVal)
	if class != vm.ErrorClass {
		t.Errorf("ClassFor should return ErrorClass, got %v", class)
	}
}

// ---------------------------------------------------------------------------
// Pass Tests
// ---------------------------------------------------------------------------

// TestPassForwardsToOuterHandler verifies that calling "pass" inside a handler
// forwards the exception to the next outer handler.
//
// Equivalent Smalltalk:
//   [[Error signal]
//       on: Error do: [:ex | ex pass]]
//       on: Error do: [:ex | 99]
//
// The inner handler passes, so the outer handler should return 99.
func TestPassForwardsToOuterHandler(t *testing.T) {
	vm := NewVM()

	passSelID := vm.Selectors.Intern("pass")
	onDoSelID := vm.Selectors.Intern("on:do:")
	triggerSelID := vm.Selectors.Intern("trigger")
	errorClassVal := vm.classValue(vm.ErrorClass)

	// Helper class that signals Error when "trigger" is sent
	signalerClass := NewClass("PassTestSignaler", nil)
	vm.Classes.Register(signalerClass)
	signalerClass.AddMethod0(vm.Selectors, "trigger", func(v *VM, recv Value) Value {
		return v.signalException(v.ErrorClass, Nil)
	})
	signalerObj := signalerClass.NewInstance()

	// Signal block: [signaler trigger]
	signalBlock := &BlockMethod{
		Arity: 0, NumTemps: 0, NumCaptures: 1,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitByte(OpPushCaptured, 0)
			bb.EmitSend(OpSend, uint16(triggerSelID), 0)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}
	signalBV := &BlockValue{
		Block: signalBlock, Captures: []Value{signalerObj.ToValue()},
		HomeFrame: -1, HomeSelf: Nil, HomeMethod: nil,
	}
	signalBlockVal := FromBlockID(uint32(vm.registry.RegisterBlock(signalBV)))

	// Inner handler: [:ex | ex pass]
	passHandlerBlock := &BlockMethod{
		Arity: 1, NumTemps: 1,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitByte(OpPushTemp, 0)
			bb.EmitSend(OpSend, uint16(passSelID), 0)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}
	passHandlerBV := &BlockValue{
		Block: passHandlerBlock, HomeFrame: -1, HomeSelf: Nil, HomeMethod: nil,
	}
	passHandlerVal := FromBlockID(uint32(vm.registry.RegisterBlock(passHandlerBV)))

	// Outer handler: [:ex | 99]
	outerHandlerBlock := &BlockMethod{
		Arity: 1, NumTemps: 1,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitInt8(OpPushInt8, 99)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}
	outerHandlerBV := &BlockValue{
		Block: outerHandlerBlock, HomeFrame: -1, HomeSelf: Nil, HomeMethod: nil,
	}
	outerHandlerVal := FromBlockID(uint32(vm.registry.RegisterBlock(outerHandlerBV)))

	// Inner on:do: block: [signaler trigger] on: Error do: [:ex | ex pass]
	innerBlock := &BlockMethod{
		Arity: 0, NumTemps: 0, NumCaptures: 3,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitByte(OpPushCaptured, 0) // signalBlockVal
			bb.EmitByte(OpPushCaptured, 1) // errorClassVal
			bb.EmitByte(OpPushCaptured, 2) // passHandlerVal
			bb.EmitSend(OpSend, uint16(onDoSelID), 2)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}
	innerBV := &BlockValue{
		Block: innerBlock, Captures: []Value{signalBlockVal, errorClassVal, passHandlerVal},
		HomeFrame: -1, HomeSelf: Nil, HomeMethod: nil,
	}
	innerBlockVal := FromBlockID(uint32(vm.registry.RegisterBlock(innerBV)))

	// Outer: [inner] on: Error do: [:ex | 99]
	result := vm.evaluateBlockWithHandler(innerBlockVal, vm.ErrorClass, outerHandlerVal)

	if !result.IsSmallInt() || result.SmallInt() != 99 {
		t.Errorf("pass should forward to outer handler, got %v, want 99", result)
	}
}

// TestNestedExceptionHandlersWithPass verifies that pass works correctly
// with multiple levels of nesting.
//
// Equivalent Smalltalk:
//   [[[signaler trigger] on: Error do: [:e | e pass]]
//       on: Error do: [:e | e pass]]
//       on: Error do: [:e | 42]
//
// Both inner handlers pass, so the outermost handler returns 42.
func TestNestedExceptionHandlersWithPass(t *testing.T) {
	vm := NewVM()

	passSelID := vm.Selectors.Intern("pass")
	onDoSelID := vm.Selectors.Intern("on:do:")
	triggerSelID := vm.Selectors.Intern("trigger")
	errorClassVal := vm.classValue(vm.ErrorClass)

	// Helper class to signal Error
	signalerClass := NewClass("NestedPassSignaler", nil)
	vm.Classes.Register(signalerClass)
	signalerClass.AddMethod0(vm.Selectors, "trigger", func(v *VM, recv Value) Value {
		return v.signalException(v.ErrorClass, Nil)
	})
	signalerObj := signalerClass.NewInstance()

	// Signal block: [signaler trigger]
	signalBlock := &BlockMethod{
		Arity: 0, NumTemps: 0, NumCaptures: 1,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitByte(OpPushCaptured, 0)
			bb.EmitSend(OpSend, uint16(triggerSelID), 0)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}
	signalBV := &BlockValue{
		Block: signalBlock, Captures: []Value{signalerObj.ToValue()},
		HomeFrame: -1, HomeSelf: Nil, HomeMethod: nil,
	}
	signalBlockVal := FromBlockID(uint32(vm.registry.RegisterBlock(signalBV)))

	// Pass handler: [:ex | ex pass]
	passHandler := &BlockMethod{
		Arity: 1, NumTemps: 1,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitByte(OpPushTemp, 0)
			bb.EmitSend(OpSend, uint16(passSelID), 0)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}
	passHandlerBV1 := &BlockValue{
		Block: passHandler, HomeFrame: -1, HomeSelf: Nil, HomeMethod: nil,
	}
	passHandlerVal1 := FromBlockID(uint32(vm.registry.RegisterBlock(passHandlerBV1)))

	passHandlerBV2 := &BlockValue{
		Block: passHandler, HomeFrame: -1, HomeSelf: Nil, HomeMethod: nil,
	}
	passHandlerVal2 := FromBlockID(uint32(vm.registry.RegisterBlock(passHandlerBV2)))

	// Outer handler: [:ex | 42]
	outerHandler := &BlockMethod{
		Arity: 1, NumTemps: 1,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitInt8(OpPushInt8, 42)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}
	outerHandlerBV := &BlockValue{
		Block: outerHandler, HomeFrame: -1, HomeSelf: Nil, HomeMethod: nil,
	}
	outerHandlerVal := FromBlockID(uint32(vm.registry.RegisterBlock(outerHandlerBV)))

	// Build inner: [signaler trigger] on: Error do: [:ex | ex pass]
	innerBlock := &BlockMethod{
		Arity: 0, NumTemps: 0, NumCaptures: 3,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitByte(OpPushCaptured, 0)
			bb.EmitByte(OpPushCaptured, 1)
			bb.EmitByte(OpPushCaptured, 2)
			bb.EmitSend(OpSend, uint16(onDoSelID), 2)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}
	innerBV := &BlockValue{
		Block: innerBlock, Captures: []Value{signalBlockVal, errorClassVal, passHandlerVal1},
		HomeFrame: -1, HomeSelf: Nil, HomeMethod: nil,
	}
	innerBlockVal := FromBlockID(uint32(vm.registry.RegisterBlock(innerBV)))

	// Build middle: [inner] on: Error do: [:ex | ex pass]
	middleBlock := &BlockMethod{
		Arity: 0, NumTemps: 0, NumCaptures: 3,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitByte(OpPushCaptured, 0)
			bb.EmitByte(OpPushCaptured, 1)
			bb.EmitByte(OpPushCaptured, 2)
			bb.EmitSend(OpSend, uint16(onDoSelID), 2)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}
	middleBV := &BlockValue{
		Block: middleBlock, Captures: []Value{innerBlockVal, errorClassVal, passHandlerVal2},
		HomeFrame: -1, HomeSelf: Nil, HomeMethod: nil,
	}
	middleBlockVal := FromBlockID(uint32(vm.registry.RegisterBlock(middleBV)))

	// Outermost: [middle] on: Error do: [:ex | 42]
	result := vm.evaluateBlockWithHandler(middleBlockVal, vm.ErrorClass, outerHandlerVal)

	if !result.IsSmallInt() || result.SmallInt() != 42 {
		t.Errorf("double pass should forward to outermost handler, got %v, want 42", result)
	}
}

// TestRetryReexecutesProtectedBlock verifies that calling "retry" inside
// a handler re-executes the protected block from the beginning.
//
// Equivalent Smalltalk:
//   | count |
//   count := 0.
//   [count := count + 1.
//    count < 3 ifTrue: [Error signal]] on: Error do: [:ex | ex retry].
//
// After retry, count should be 3 (block executes 3 times: signals at 1, 2, succeeds at 3).
func TestRetryReexecutesProtectedBlock(t *testing.T) {
	vm := NewVM()

	retrySelID := vm.Selectors.Intern("retry")

	// We'll use a Go-level counter to track retries, since bytecode-level
	// shared state is complex. We'll create a class with a primitive method
	// that increments a counter and signals when count < 3.
	count := 0

	counterClass := NewClass("RetryCounter", nil)
	vm.Classes.Register(counterClass)

	// RetryCounter>>check - increments counter, signals Error if < 3
	checkSelID := vm.Selectors.Intern("check")
	counterClass.AddMethod0(vm.Selectors, "check", func(v *VM, recv Value) Value {
		count++
		if count < 3 {
			v.signalException(v.ErrorClass, Nil)
		}
		return FromSmallInt(int64(count))
	})

	counterObj := counterClass.NewInstance()

	// Protected block: [counter check]
	protectedBlock := &BlockMethod{
		Arity: 0, NumTemps: 0, NumCaptures: 1,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitByte(OpPushCaptured, 0)                    // push counter
			bb.EmitSend(OpSend, uint16(checkSelID), 0)        // counter check
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}

	// Handler: [:ex | ex retry]
	retryHandler := &BlockMethod{
		Arity: 1, NumTemps: 1,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitByte(OpPushTemp, 0)
			bb.EmitSend(OpSend, uint16(retrySelID), 0)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}

	protectedBV := &BlockValue{
		Block: protectedBlock, Captures: []Value{counterObj.ToValue()},
		HomeFrame: -1, HomeSelf: Nil, HomeMethod: nil,
	}
	retryHandlerBV := &BlockValue{
		Block: retryHandler, HomeFrame: -1, HomeSelf: Nil, HomeMethod: nil,
	}

	protectedBlockVal := FromBlockID(uint32(vm.registry.RegisterBlock(protectedBV)))
	retryHandlerVal := FromBlockID(uint32(vm.registry.RegisterBlock(retryHandlerBV)))

	result := vm.evaluateBlockWithHandler(protectedBlockVal, vm.ErrorClass, retryHandlerVal)

	if count != 3 {
		t.Errorf("retry should have caused 3 executions, got %d", count)
	}
	if !result.IsSmallInt() || result.SmallInt() != 3 {
		t.Errorf("result should be 3 (final count), got %v", result)
	}
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

func BenchmarkExceptionCreation(b *testing.B) {
	vm := NewVM()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		ex := &ExceptionObject{
			MessageText: vm.registry.NewStringValue("test"),
			Resumable:   true,
		}
		id := vm.registry.RegisterException(ex)
		vm.registry.UnregisterException(id)
	}
}

func BenchmarkHandlerStackPushPop(b *testing.B) {
	vm := NewVM()
	i := vm.interpreter

	h := &ExceptionHandler{
		ExceptionClass: vm.ExceptionClass,
		FrameIndex:     0,
	}

	b.ResetTimer()
	for n := 0; n < b.N; n++ {
		i.PushExceptionHandler(h)
		i.PopExceptionHandler()
	}
}

func BenchmarkFindHandler(b *testing.B) {
	vm := NewVM()
	i := vm.interpreter

	// Install several handlers
	for j := 0; j < 5; j++ {
		h := &ExceptionHandler{
			ExceptionClass: vm.ExceptionClass,
			FrameIndex:     j,
		}
		i.PushExceptionHandler(h)
	}

	b.ResetTimer()
	for n := 0; n < b.N; n++ {
		i.FindHandler(vm.ErrorClass)
	}
}

// ---------------------------------------------------------------------------
// Captured stack trace tests (review item 4e)
// ---------------------------------------------------------------------------

// buildSignalingHandlerSetup wires up a "signaler trigger" call inside an
// on:do: handler. The handler captures the exception object so the test can
// inspect ex.CapturedFrames after the protected block runs.
//
//	[signaler trigger] on: Error do: [:ex | caught := ex; nil]
//
// Returns the resulting outcome value plus the captured *ExceptionObject.
func runSignalAndCapture(t *testing.T, exClass *Class) *ExceptionObject {
	t.Helper()
	vm := NewVM()

	if exClass == nil {
		exClass = vm.ErrorClass
	}
	triggerSelID := vm.Selectors.Intern("trigger")
	deeperSelID := vm.Selectors.Intern("deeper")

	signalerClass := NewClass("TraceCaptureSignaler", nil)
	vm.Classes.Register(signalerClass)
	signalerClass.AddMethod0(vm.Selectors, "trigger", func(v *VM, recv Value) Value {
		return v.signalException(v.ErrorClass, v.registry.NewStringValue("boom"))
	})
	// "deeper" calls "trigger" so we get at least two interesting frames.
	signalerClass.AddMethod0(vm.Selectors, "deeper", func(v *VM, recv Value) Value {
		return v.Send(recv, "trigger", nil)
	})
	signalerObj := signalerClass.NewInstance()

	// Protected block: [signaler deeper]
	protectedBlock := &BlockMethod{
		Arity: 0, NumTemps: 0, NumCaptures: 1,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitByte(OpPushCaptured, 0)
			bb.EmitSend(OpSend, uint16(deeperSelID), 0)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}
	protectedBV := &BlockValue{
		Block: protectedBlock, Captures: []Value{signalerObj.ToValue()},
		HomeFrame: -1, HomeSelf: Nil, HomeMethod: nil,
	}
	protectedBlockVal := FromBlockID(uint32(vm.registry.RegisterBlock(protectedBV)))
	_ = triggerSelID

	// Handler block [:ex | ex]  -- returns the exception so we can inspect it
	handlerBlock := &BlockMethod{
		Arity: 1, NumTemps: 1,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitByte(OpPushTemp, 0)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}
	handlerBV := &BlockValue{
		Block: handlerBlock, HomeFrame: -1, HomeSelf: Nil, HomeMethod: nil,
	}
	handlerVal := FromBlockID(uint32(vm.registry.RegisterBlock(handlerBV)))

	result := vm.evaluateBlockWithHandler(protectedBlockVal, exClass, handlerVal)
	if !result.IsException() {
		t.Fatalf("handler block should have returned the exception value, got %v", result)
	}
	ex := vm.registry.GetException(result.ExceptionID())
	if ex == nil {
		t.Fatalf("exception object not retrievable from handler result")
	}
	return ex
}

func TestExceptionCapturesStackAtSignal(t *testing.T) {
	ex := runSignalAndCapture(t, nil)

	if len(ex.CapturedFrames) == 0 {
		t.Fatalf("expected CapturedFrames to be populated, got empty slice")
	}
	// The signaling site uses a Go primitive (signalException) and thus the
	// innermost interpreter frame at signal time is the caller of the
	// primitive — i.e. the block that did `signaler deeper`. We just assert
	// the trace has at least the protected block frame.
	found := false
	for _, f := range ex.CapturedFrames {
		if f.Block != nil || f.Selector == "deeper" || f.Selector == "trigger" {
			found = true
			break
		}
	}
	if !found {
		t.Errorf("expected captured trace to include block or deeper/trigger frames, got %+v", ex.CapturedFrames)
	}
}

func TestCapturedTracePreservedAcrossPass(t *testing.T) {
	// First signal captures; pass should NOT re-capture (the original signal
	// site is what matters to the user). We approximate by signaling, then
	// re-signaling the same ExceptionObject and verifying the frame slice
	// pointer is unchanged.
	vm := NewVM()
	ex := &ExceptionObject{
		ExceptionClass: vm.ErrorClass,
		MessageText:    vm.registry.NewStringValue("first"),
		Resumable:      false,
		CapturedFrames: []TraceFrame{{Selector: "preset"}},
	}
	id := vm.registry.RegisterException(ex)
	exVal := FromExceptionID(id)
	originalFrames := ex.CapturedFrames
	defer func() {
		_ = recover()
		if &ex.CapturedFrames[0] != &originalFrames[0] {
			t.Errorf("CapturedFrames was overwritten on re-signal")
		}
	}()
	vm.signalExceptionObject(exVal, ex) // panics
}

func TestFormatCapturedTraceShape(t *testing.T) {
	frames := []TraceFrame{
		{ClassName: "Foo", Selector: "bar", IP: 0},
		{Selector: "<block>", ClassName: "Foo", IP: 0},
	}
	out := FormatCapturedTrace(frames)
	if !strings.Contains(out, "Foo>>bar") {
		t.Errorf("expected 'Foo>>bar' in trace output, got:\n%s", out)
	}
	if !strings.Contains(out, "<block>") {
		t.Errorf("expected '<block>' in trace output, got:\n%s", out)
	}
	if !strings.Contains(out, "[0]") || !strings.Contains(out, "[1]") {
		t.Errorf("expected indexed frame markers, got:\n%s", out)
	}
}

func TestStackTracePrimitiveReturnsString(t *testing.T) {
	ex := runSignalAndCapture(t, nil)
	// Re-register for primitive lookup
	vm := NewVM()
	id := vm.registry.RegisterException(ex)
	exVal := FromExceptionID(id)
	result := vm.Send(exVal, "stackTrace", nil)
	if !IsStringValue(result) {
		t.Fatalf("stackTrace primitive should return a string, got %v", result)
	}
	s := vm.registry.GetStringContent(result)
	if s == "" {
		t.Errorf("stackTrace returned empty string")
	}
}

func TestExecuteSafeIncludesCapturedTrace(t *testing.T) {
	// Build a CompiledMethod that signals. Use the same low-level scaffolding
	// as the other tests in this file.
	vm := NewVM()
	signalSelID := vm.Selectors.Intern("trigger")

	signalerClass := NewClass("ESafeSignaler", nil)
	vm.Classes.Register(signalerClass)
	signalerClass.AddMethod0(vm.Selectors, "trigger", func(v *VM, recv Value) Value {
		return v.signalException(v.ErrorClass, v.registry.NewStringValue("kapow"))
	})
	signalerObj := signalerClass.NewInstance()

	method := &CompiledMethod{
		Arity:    0,
		NumTemps: 0,
		Literals: []Value{signalerObj.ToValue()},
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitUint16(OpPushLiteral, 0)
			bb.EmitSend(OpSend, uint16(signalSelID), 0)
			bb.Emit(OpReturnTop)
			return bb.Bytes()
		}(),
	}

	_, err := vm.ExecuteSafe(method, Nil, nil)
	if err == nil {
		t.Fatalf("expected error from unhandled exception, got nil")
	}
	msg := err.Error()
	if !strings.Contains(msg, "kapow") {
		t.Errorf("expected message text in error, got: %s", msg)
	}
	// The captured trace should be appended on a new line, with at least one
	// frame marker like "[0]".
	if !strings.Contains(msg, "[0]") {
		t.Errorf("expected captured trace frames in error output, got:\n%s", msg)
	}
}

// TestExecuteSafeRestoresStackOnException guards the regression where an
// unhandled exception left interp.fp/sp elevated, so repeated ExecuteSafe calls
// on a shared interpreter (REPL, doctest, LSP) accumulated stale frames and
// eventually threw a spurious StackOverflow.
func TestExecuteSafeRestoresStackOnException(t *testing.T) {
	vm := NewVM()
	signalSelID := vm.Selectors.Intern("trigger")

	signalerClass := NewClass("ESafeStackSignaler", nil)
	vm.Classes.Register(signalerClass)
	signalerClass.AddMethod0(vm.Selectors, "trigger", func(v *VM, recv Value) Value {
		return v.signalException(v.ErrorClass, v.registry.NewStringValue("kapow"))
	})
	signalerObj := signalerClass.NewInstance()

	method := &CompiledMethod{
		Arity:    0,
		NumTemps: 0,
		Literals: []Value{signalerObj.ToValue()},
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitUint16(OpPushLiteral, 0)
			bb.EmitSend(OpSend, uint16(signalSelID), 0)
			bb.Emit(OpReturnTop)
			return bb.Bytes()
		}(),
	}

	interp := vm.currentInterpreter()
	baseFP, baseSP := interp.fp, interp.sp

	for n := 0; n < 50; n++ {
		if _, err := vm.ExecuteSafe(method, Nil, nil); err == nil {
			t.Fatalf("iteration %d: expected error from unhandled exception", n)
		}
		if interp.fp != baseFP || interp.sp != baseSP {
			t.Fatalf("iteration %d: stack not restored after exception: fp %d->%d, sp %d->%d",
				n, baseFP, interp.fp, baseSP, interp.sp)
		}
	}
}

func BenchmarkCaptureTrace(b *testing.B) {
	vm := NewVM()
	i := vm.interpreter
	for d := 0; d < 32; d++ {
		i.fp++
		if i.fp >= len(i.frames) {
			i.frames = append(i.frames, CallFrame{IP: d})
		} else {
			i.frames[i.fp] = CallFrame{IP: d}
		}
	}
	b.ReportAllocs()
	b.ResetTimer()
	for n := 0; n < b.N; n++ {
		_ = i.CaptureTrace(MaxCapturedTraceDepth)
	}
}

func TestCaptureTraceRespectsMaxDepth(t *testing.T) {
	vm := NewVM()
	i := vm.interpreter

	// Synthesize a deep frame stack manually by populating frames.
	depth := 50
	for i.fp+1 < depth {
		i.fp++
		if i.fp >= len(i.frames) {
			i.frames = append(i.frames, CallFrame{IP: i.fp})
		} else {
			i.frames[i.fp] = CallFrame{IP: i.fp}
		}
	}

	// Cap at 10 — should give back exactly 10 frames.
	frames := i.CaptureTrace(10)
	if len(frames) != 10 {
		t.Errorf("expected 10 frames with cap=10, got %d", len(frames))
	}
	// First frame should be the innermost (highest IP).
	if frames[0].IP != depth-1 {
		t.Errorf("expected innermost-first ordering (IP=%d), got IP=%d", depth-1, frames[0].IP)
	}
}

