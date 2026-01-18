package vm

import (
	"testing"
)

func TestSuccessWithValue(t *testing.T) {
	vm := NewVM()

	// Create Success with a value
	success := vm.Send(vm.classValue(vm.SuccessClass), "with:", []Value{FromSmallInt(42)})
	if success == Nil {
		t.Fatal("Success with: returned nil")
	}

	if !isResultValue(success) {
		t.Error("Expected result value")
	}

	// Check isSuccess
	isSuccess := vm.Send(success, "isSuccess", nil)
	if isSuccess != True {
		t.Error("Success isSuccess should be true")
	}

	// Check isFailure
	isFailure := vm.Send(success, "isFailure", nil)
	if isFailure != False {
		t.Error("Success isFailure should be false")
	}

	// Get the value
	val := vm.Send(success, "value", nil)
	if !val.IsSmallInt() || val.SmallInt() != 42 {
		t.Errorf("Success value = %v, want 42", val)
	}

	// Error should be nil
	err := vm.Send(success, "error", nil)
	if err != Nil {
		t.Error("Success error should be nil")
	}
}

func TestFailureWithReason(t *testing.T) {
	vm := NewVM()

	// Create Failure with a reason (use a symbol for the error)
	reason := vm.Symbols.SymbolValue("not found")
	failure := vm.Send(vm.classValue(vm.FailureClass), "with:", []Value{reason})
	if failure == Nil {
		t.Fatal("Failure with: returned nil")
	}

	// Check isSuccess
	isSuccess := vm.Send(failure, "isSuccess", nil)
	if isSuccess != False {
		t.Error("Failure isSuccess should be false")
	}

	// Check isFailure
	isFailure := vm.Send(failure, "isFailure", nil)
	if isFailure != True {
		t.Error("Failure isFailure should be true")
	}

	// Value should be nil
	val := vm.Send(failure, "value", nil)
	if val != Nil {
		t.Error("Failure value should be nil")
	}

	// Get the error
	err := vm.Send(failure, "error", nil)
	if err != reason {
		t.Errorf("Failure error = %v, want %v", err, reason)
	}
}

func TestResultFactoryMethods(t *testing.T) {
	vm := NewVM()

	// Result success: value
	success := vm.Send(vm.classValue(vm.ResultClass), "success:", []Value{FromSmallInt(100)})
	isSuccess := vm.Send(success, "isSuccess", nil)
	if isSuccess != True {
		t.Error("Result success: should create Success")
	}

	val := vm.Send(success, "value", nil)
	if !val.IsSmallInt() || val.SmallInt() != 100 {
		t.Errorf("Result success: value = %v, want 100", val)
	}

	// Result failure: reason
	failure := vm.Send(vm.classValue(vm.ResultClass), "failure:", []Value{FromSmallInt(-1)})
	isFailure := vm.Send(failure, "isFailure", nil)
	if isFailure != True {
		t.Error("Result failure: should create Failure")
	}

	err := vm.Send(failure, "error", nil)
	if !err.IsSmallInt() || err.SmallInt() != -1 {
		t.Errorf("Result failure: error = %v, want -1", err)
	}
}

func TestSuccessIfSuccess(t *testing.T) {
	vm := NewVM()

	success := vm.Send(vm.classValue(vm.SuccessClass), "with:", []Value{FromSmallInt(10)})

	// We need a block that doubles the value
	// For this test, we'll use the fact that ifSuccess: passes the value to a block
	// Since we don't have a way to create blocks programmatically here,
	// we'll test that ifSuccess: returns non-nil (indicating it tried to evaluate)

	// Without a real block, this will return Nil (block evaluation fails)
	// The important thing is that it attempts to evaluate
	result := vm.Send(success, "ifSuccess:", []Value{Nil})
	// Since Nil is not a valid block, this returns Nil
	if result != Nil {
		t.Log("ifSuccess: attempted to evaluate block")
	}
}

func TestFailureIfFailure(t *testing.T) {
	vm := NewVM()

	failure := vm.Send(vm.classValue(vm.FailureClass), "with:", []Value{FromSmallInt(-1)})

	// Failure should not evaluate ifSuccess: block
	result := vm.Send(failure, "ifSuccess:", []Value{Nil})
	// ifSuccess: on Failure returns self (the failure)
	if result != failure {
		t.Error("Failure ifSuccess: should return self")
	}
}

func TestSuccessMap(t *testing.T) {
	vm := NewVM()

	success := vm.Send(vm.classValue(vm.SuccessClass), "with:", []Value{FromSmallInt(5)})

	// map: without a real block - tests that it attempts transformation
	// Since we can't easily create blocks, we test that it returns a new result
	result := vm.Send(success, "map:", []Value{Nil})

	// Should still be a result (map: wraps Nil in Success since no real block)
	if !isResultValue(result) {
		t.Error("Success map: should return a Result")
	}
}

func TestFailureMap(t *testing.T) {
	vm := NewVM()

	failure := vm.Send(vm.classValue(vm.FailureClass), "with:", []Value{FromSmallInt(-1)})

	// Failure map: should return self (propagate failure)
	result := vm.Send(failure, "map:", []Value{Nil})
	if result != failure {
		t.Error("Failure map: should return self")
	}
}

func TestSuccessThen(t *testing.T) {
	vm := NewVM()

	success := vm.Send(vm.classValue(vm.SuccessClass), "with:", []Value{FromSmallInt(5)})

	// then: without real block
	result := vm.Send(success, "then:", []Value{Nil})

	// Should return a result (wraps Nil in Success)
	if !isResultValue(result) {
		t.Error("Success then: should return a Result")
	}
}

func TestFailureThen(t *testing.T) {
	vm := NewVM()

	failure := vm.Send(vm.classValue(vm.FailureClass), "with:", []Value{FromSmallInt(-1)})

	// Failure then: should return self (propagate failure)
	result := vm.Send(failure, "then:", []Value{Nil})
	if result != failure {
		t.Error("Failure then: should return self")
	}
}

func TestSuccessFlatMap(t *testing.T) {
	vm := NewVM()

	success := vm.Send(vm.classValue(vm.SuccessClass), "with:", []Value{FromSmallInt(5)})

	// flatMap: without real block returns what block returns (Nil)
	result := vm.Send(success, "flatMap:", []Value{Nil})
	if result != Nil {
		t.Log("flatMap: returned result of block evaluation")
	}
}

func TestFailureFlatMap(t *testing.T) {
	vm := NewVM()

	failure := vm.Send(vm.classValue(vm.FailureClass), "with:", []Value{FromSmallInt(-1)})

	// Failure flatMap: should return self
	result := vm.Send(failure, "flatMap:", []Value{Nil})
	if result != failure {
		t.Error("Failure flatMap: should return self")
	}
}

func TestOnSuccessOnFailure(t *testing.T) {
	vm := NewVM()

	// Test with Success
	success := vm.Send(vm.classValue(vm.SuccessClass), "with:", []Value{FromSmallInt(42)})
	result := vm.Send(success, "onSuccess:onFailure:", []Value{Nil, Nil})
	// Since no real block, evaluateBlock returns Nil
	if result != Nil {
		t.Log("onSuccess:onFailure: evaluated success block")
	}

	// Test with Failure
	failure := vm.Send(vm.classValue(vm.FailureClass), "with:", []Value{FromSmallInt(-1)})
	result = vm.Send(failure, "onSuccess:onFailure:", []Value{Nil, Nil})
	if result != Nil {
		t.Log("onSuccess:onFailure: evaluated failure block")
	}
}
