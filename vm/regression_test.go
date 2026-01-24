package vm

import (
	"sync"
	"testing"
	"time"
)

// Regression tests for bugs fixed during Yutani event loop debugging.
// These tests ensure the fixes remain in place.

// TestPrimIdenticalExists tests that the primIdentical: primitive exists
// and works correctly. This was missing, causing `true == true` to return nil.
// Bug: Object.mag's == method delegates to primIdentical: which didn't exist.
func TestPrimIdenticalExists(t *testing.T) {
	vm := NewVM()

	// Test true == true returns true (via primIdentical:)
	result := vm.Send(True, "==", []Value{True})
	if result != True {
		t.Errorf("true == true = %v, want true", result)
	}

	// Test false == false returns true
	result = vm.Send(False, "==", []Value{False})
	if result != True {
		t.Errorf("false == false = %v, want true", result)
	}

	// Test true == false returns false
	result = vm.Send(True, "==", []Value{False})
	if result != False {
		t.Errorf("true == false = %v, want false", result)
	}

	// Test nil == nil returns true
	result = vm.Send(Nil, "==", []Value{Nil})
	if result != True {
		t.Errorf("nil == nil = %v, want true", result)
	}

	// Test integers
	result = vm.Send(FromSmallInt(42), "==", []Value{FromSmallInt(42)})
	if result != True {
		t.Errorf("42 == 42 = %v, want true", result)
	}

	result = vm.Send(FromSmallInt(42), "==", []Value{FromSmallInt(43)})
	if result != False {
		t.Errorf("42 == 43 = %v, want false", result)
	}
}

// TestMarkerNoCollision tests that the marker bytes for different value types
// don't collide with each other. Previous bug: grpcStreamMarker (8 << 24)
// collided with exceptionMarker (8 << 24), causing streams to be misidentified.
func TestMarkerNoCollision(t *testing.T) {
	// Verify all markers are unique
	markers := map[string]uint32{
		"channel":    channelMarker,
		"process":    processMarker,
		"result":     resultMarker,
		"grpcClient": grpcClientMarker,
		"grpcStream": grpcStreamMarker,
		"exception":  exceptionMarker,
	}

	seen := make(map[uint32]string)
	for name, marker := range markers {
		if existing, found := seen[marker]; found {
			t.Errorf("Marker collision: %s and %s both use %#x", name, existing, marker)
		}
		seen[marker] = name
	}

	// Verify detection functions work correctly
	vm := NewVM()

	// Create a channel
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new", nil)
	if !isChannelValue(ch) {
		t.Error("Channel should be identified as channel")
	}
	if isProcessValue(ch) {
		t.Error("Channel should not be identified as process")
	}
	if isResultValue(ch) {
		t.Error("Channel should not be identified as result")
	}
	if isGrpcStreamValue(ch) {
		t.Error("Channel should not be identified as grpc stream")
	}
}

// TestGoroutineLocalInterpreter tests that block evaluation works correctly
// in forked processes. Previous bug: primitives like whileTrue: used vm.interpreter
// which was the main interpreter, not the forked process's interpreter.
func TestGoroutineLocalInterpreter(t *testing.T) {
	vm := NewVM()

	// Test that different goroutines can register different interpreters
	var wg sync.WaitGroup
	results := make([]bool, 3)

	for i := 0; i < 3; i++ {
		wg.Add(1)
		go func(idx int) {
			defer wg.Done()

			// Create a new interpreter for this goroutine
			interp := vm.newInterpreter()
			vm.registerInterpreter(interp)
			defer vm.unregisterInterpreter()

			// Verify we get our own interpreter back
			currentInterp := vm.currentInterpreter()
			results[idx] = (currentInterp == interp)
		}(i)
	}

	wg.Wait()

	for i, ok := range results {
		if !ok {
			t.Errorf("Goroutine %d did not get its own interpreter", i)
		}
	}
}

// TestGoroutineIDRetrieval tests that getGoroutineID works correctly.
func TestGoroutineIDRetrieval(t *testing.T) {
	// Get goroutine ID from main goroutine
	mainID := getGoroutineID()
	if mainID <= 0 {
		t.Errorf("Main goroutine ID = %d, want > 0", mainID)
	}

	// Get ID from a different goroutine
	var otherID int64
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		otherID = getGoroutineID()
	}()
	wg.Wait()

	if otherID <= 0 {
		t.Errorf("Other goroutine ID = %d, want > 0", otherID)
	}

	if mainID == otherID {
		t.Error("Different goroutines should have different IDs")
	}
}

// TestChannelWithCurrentInterpreter tests that channel operations work
// correctly when called from the correct interpreter context.
func TestChannelWithCurrentInterpreter(t *testing.T) {
	vm := NewVM()

	// Create a buffered channel
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(5)})

	// Register an interpreter for this test
	interp := vm.newInterpreter()
	vm.registerInterpreter(interp)
	defer vm.unregisterInterpreter()

	// Verify channel operations work
	vm.Send(ch, "send:", []Value{FromSmallInt(42)})
	val := vm.Send(ch, "receive", nil)

	if !val.IsSmallInt() || val.SmallInt() != 42 {
		t.Errorf("Channel receive = %v, want 42", val)
	}
}

// TestProcessForkedInterpreterRegistration tests that forked processes
// correctly register their interpreters.
func TestProcessForkedInterpreterRegistration(t *testing.T) {
	vm := NewVM()

	// Create a channel to communicate results
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})

	// We can't easily create a block and fork it without the compiler,
	// but we can test the interpreter registration mechanism directly
	done := make(chan bool)

	go func() {
		interp := vm.newInterpreter()
		vm.registerInterpreter(interp)
		defer vm.unregisterInterpreter()

		// Verify we have an interpreter registered
		current := vm.currentInterpreter()
		done <- (current == interp)
	}()

	select {
	case result := <-done:
		if !result {
			t.Error("Forked goroutine should have its own interpreter registered")
		}
	case <-time.After(1 * time.Second):
		t.Error("Timeout waiting for forked goroutine")
	}

	_ = ch // silence unused warning
}

// TestBooleanIdentityComparison tests that boolean identity comparison
// works correctly after loading library files.
func TestBooleanIdentityComparison(t *testing.T) {
	vm := NewVM()

	tests := []struct {
		name   string
		recv   Value
		arg    Value
		expect Value
	}{
		{"true == true", True, True, True},
		{"true == false", True, False, False},
		{"false == true", False, True, False},
		{"false == false", False, False, True},
		{"nil == nil", Nil, Nil, True},
		{"true == nil", True, Nil, False},
		{"nil == true", Nil, True, False},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := vm.Send(tt.recv, "==", []Value{tt.arg})
			if result != tt.expect {
				t.Errorf("%s = %v, want %v", tt.name, result, tt.expect)
			}
		})
	}
}

// TestPrimitiveMethodInheritance tests that primitive methods on Object
// are inherited correctly by subclasses like True, False, etc.
func TestPrimitiveMethodInheritance(t *testing.T) {
	vm := NewVM()

	// True should inherit isNil from Object (returns False for non-nil)
	result := vm.Send(True, "isNil", nil)
	if result != False {
		t.Errorf("true isNil = %v, want false", result)
	}

	// True should inherit notNil from Object
	result = vm.Send(True, "notNil", nil)
	if result != True {
		t.Errorf("true notNil = %v, want true", result)
	}

	// True should inherit yourself from Object
	result = vm.Send(True, "yourself", nil)
	if result != True {
		t.Errorf("true yourself = %v, want true", result)
	}
}
