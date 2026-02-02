package vm

import (
	"strings"
	"testing"
)

// ---------------------------------------------------------------------------
// Exception Value Tests
// ---------------------------------------------------------------------------

func TestExceptionValueCreation(t *testing.T) {
	ex := &ExceptionObject{
		MessageText: NewStringValue("test error"),
		Resumable:   true,
	}

	val := RegisterException(ex)

	if !val.IsException() {
		t.Error("RegisterException should return an exception value")
	}

	retrieved := GetExceptionObject(val)
	if retrieved != ex {
		t.Error("GetExceptionObject should return the original exception")
	}
}

func TestExceptionValueIsException(t *testing.T) {
	ex := &ExceptionObject{MessageText: Nil}
	val := RegisterException(ex)

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
		global := vm.Globals[tc.name]
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
		MessageText:    NewStringValue("Something went wrong"),
	}
	exVal := RegisterException(ex)

	// Get messageText
	result := vm.Send(exVal, "messageText", nil)
	if !IsStringValue(result) {
		t.Fatal("messageText should return a string")
	}

	msg := GetStringContent(result)
	if msg != "Something went wrong" {
		t.Errorf("Expected 'Something went wrong', got '%s'", msg)
	}
}

func TestExceptionDescription(t *testing.T) {
	vm := NewVM()

	// Create an exception with a message
	ex := &ExceptionObject{
		ExceptionClass: vm.ErrorClass,
		MessageText:    NewStringValue("test error"),
	}
	exVal := RegisterException(ex)

	result := vm.Send(exVal, "description", nil)
	if !IsStringValue(result) {
		t.Fatal("description should return a string")
	}

	desc := GetStringContent(result)
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
	exVal1 := RegisterException(ex1)

	result := vm.Send(exVal1, "isResumable", nil)
	if result != True {
		t.Error("Resumable exception should return true for isResumable")
	}

	// Non-resumable exception
	ex2 := &ExceptionObject{
		ExceptionClass: vm.ExceptionClass,
		Resumable:      false,
	}
	exVal2 := RegisterException(ex2)

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
	exVal := RegisterException(ex)

	class := vm.ClassFor(exVal)
	if class != vm.ErrorClass {
		t.Errorf("ClassFor should return ErrorClass, got %v", class)
	}
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

func BenchmarkExceptionCreation(b *testing.B) {
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		ex := &ExceptionObject{
			MessageText: NewStringValue("test"),
			Resumable:   true,
		}
		val := RegisterException(ex)
		UnregisterException(val)
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
