package vm

import (
	"fmt"
)

// ---------------------------------------------------------------------------
// Result: Functional error handling pattern
// Result is the abstract base. Success wraps a value, Failure wraps an error.
// ---------------------------------------------------------------------------

// ResultType identifies whether a Result is Success or Failure.
type ResultType int

const (
	ResultSuccess ResultType = iota
	ResultFailure
)

// ResultObject wraps either a success value or failure reason.
type ResultObject struct {
	vtable     *VTable
	resultType ResultType
	value      Value // The success value or failure reason
}

func createResult(rtype ResultType, val Value) *ResultObject {
	return &ResultObject{
		resultType: rtype,
		value:      val,
	}
}

const resultMarker uint32 = 4 << 24

func resultToValue(id int) Value {
	// Use symbol encoding with marker 4 << 24 (channels use 1, processes use 2)
	return FromSymbolID(uint32(id) | resultMarker)
}

func isResultValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return (id & (0xFF << 24)) == resultMarker
}

// ---------------------------------------------------------------------------
// Result primitives registration
// ---------------------------------------------------------------------------

func (vm *VM) registerResultPrimitives() {
	// Success class
	s := vm.SuccessClass

	// Success class>>with: value - create a Success wrapping value (class method)
	s.AddClassMethod1(vm.Selectors, "with:", func(vmPtr interface{}, recv Value, val Value) Value {
		v := vmPtr.(*VM)
		r := createResult(ResultSuccess, val)
		return v.registry.RegisterResultValue(r)
	})

	// Success>>value - return the wrapped value
	s.AddMethod0(vm.Selectors, "value", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		r := v.registry.GetResultFromValue(recv)
		if r == nil || r.resultType != ResultSuccess {
			return Nil
		}
		return r.value
	})

	// Success>>primValue - primitive for value (same as value)
	s.AddMethod0(vm.Selectors, "primValue", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		r := v.registry.GetResultFromValue(recv)
		if r == nil || r.resultType != ResultSuccess {
			return Nil
		}
		return r.value
	})

	// Success>>error - return nil (Success has no error)
	s.AddMethod0(vm.Selectors, "error", func(_ interface{}, recv Value) Value {
		return Nil
	})

	// Success>>isSuccess - return true
	s.AddMethod0(vm.Selectors, "isSuccess", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		fmt.Println("[Result] isSuccess called on Success")
		r := v.registry.GetResultFromValue(recv)
		if r == nil {
			fmt.Println("[Result] isSuccess: r is nil!")
			return False
		}
		if r.resultType == ResultSuccess {
			fmt.Println("[Result] isSuccess: returning True")
			return True
		}
		fmt.Println("[Result] isSuccess: returning False (wrong type)")
		return False
	})

	// Success>>isFailure - return false
	s.AddMethod0(vm.Selectors, "isFailure", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		r := v.registry.GetResultFromValue(recv)
		if r == nil {
			return True
		}
		if r.resultType == ResultFailure {
			return True
		}
		return False
	})

	// Success>>ifSuccess: block - evaluate block with value
	s.AddMethod1(vm.Selectors, "ifSuccess:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		r := v.registry.GetResultFromValue(recv)
		if r == nil || r.resultType != ResultSuccess {
			return Nil
		}
		return v.evaluateBlock(block, []Value{r.value})
	})

	// Success>>ifFailure: block - don't evaluate (we're Success)
	s.AddMethod1(vm.Selectors, "ifFailure:", func(_ interface{}, recv Value, block Value) Value {
		return recv
	})

	// Success>>onSuccess:onFailure: - evaluate success block
	s.AddMethod2(vm.Selectors, "onSuccess:onFailure:", func(vmPtr interface{}, recv Value, successBlock, failureBlock Value) Value {
		v := vmPtr.(*VM)
		r := v.registry.GetResultFromValue(recv)
		if r == nil || r.resultType != ResultSuccess {
			return Nil
		}
		return v.evaluateBlock(successBlock, []Value{r.value})
	})

	// Success>>then: block - chain operations, evaluate block with value, return new Result
	s.AddMethod1(vm.Selectors, "then:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		r := v.registry.GetResultFromValue(recv)
		if r == nil || r.resultType != ResultSuccess {
			return recv
		}
		result := v.evaluateBlock(block, []Value{r.value})
		// If block returns a Result, return it; otherwise wrap in Success
		if isResultValue(result) {
			return result
		}
		newR := createResult(ResultSuccess, result)
		return v.registry.RegisterResultValue(newR)
	})

	// Success>>map: block - transform value, return new Success
	s.AddMethod1(vm.Selectors, "map:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		r := v.registry.GetResultFromValue(recv)
		if r == nil || r.resultType != ResultSuccess {
			return recv
		}
		newVal := v.evaluateBlock(block, []Value{r.value})
		newR := createResult(ResultSuccess, newVal)
		return v.registry.RegisterResultValue(newR)
	})

	// Success>>flatMap: block - transform value, block must return Result
	s.AddMethod1(vm.Selectors, "flatMap:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		r := v.registry.GetResultFromValue(recv)
		if r == nil || r.resultType != ResultSuccess {
			return recv
		}
		return v.evaluateBlock(block, []Value{r.value})
	})

	// Failure class
	f := vm.FailureClass

	// Failure class>>with: reason - create a Failure wrapping reason (class method)
	f.AddClassMethod1(vm.Selectors, "with:", func(vmPtr interface{}, recv Value, reason Value) Value {
		v := vmPtr.(*VM)
		r := createResult(ResultFailure, reason)
		return v.registry.RegisterResultValue(r)
	})

	// Failure>>value - return nil (Failure has no value)
	f.AddMethod0(vm.Selectors, "value", func(_ interface{}, recv Value) Value {
		return Nil
	})

	// Failure>>error - return the error/reason
	f.AddMethod0(vm.Selectors, "error", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		r := v.registry.GetResultFromValue(recv)
		if r == nil || r.resultType != ResultFailure {
			return Nil
		}
		return r.value
	})

	// Failure>>primError - primitive for error (same as error)
	f.AddMethod0(vm.Selectors, "primError", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		r := v.registry.GetResultFromValue(recv)
		if r == nil || r.resultType != ResultFailure {
			return Nil
		}
		return r.value
	})

	// Failure>>isSuccess - return false
	f.AddMethod0(vm.Selectors, "isSuccess", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		r := v.registry.GetResultFromValue(recv)
		if r == nil {
			return False
		}
		if r.resultType == ResultSuccess {
			return True
		}
		return False
	})

	// Failure>>isFailure - return true
	f.AddMethod0(vm.Selectors, "isFailure", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		r := v.registry.GetResultFromValue(recv)
		if r == nil {
			return True
		}
		if r.resultType == ResultFailure {
			return True
		}
		return False
	})

	// Failure>>ifSuccess: block - don't evaluate (we're Failure)
	f.AddMethod1(vm.Selectors, "ifSuccess:", func(_ interface{}, recv Value, block Value) Value {
		return recv
	})

	// Failure>>ifFailure: block - evaluate block with error
	f.AddMethod1(vm.Selectors, "ifFailure:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		r := v.registry.GetResultFromValue(recv)
		if r == nil || r.resultType != ResultFailure {
			return recv
		}
		return v.evaluateBlock(block, []Value{r.value})
	})

	// Failure>>onSuccess:onFailure: - evaluate failure block
	f.AddMethod2(vm.Selectors, "onSuccess:onFailure:", func(vmPtr interface{}, recv Value, successBlock, failureBlock Value) Value {
		v := vmPtr.(*VM)
		r := v.registry.GetResultFromValue(recv)
		if r == nil || r.resultType != ResultFailure {
			return Nil
		}
		return v.evaluateBlock(failureBlock, []Value{r.value})
	})

	// Failure>>then: block - don't evaluate (propagate failure)
	f.AddMethod1(vm.Selectors, "then:", func(_ interface{}, recv Value, block Value) Value {
		return recv
	})

	// Failure>>map: block - don't evaluate (propagate failure)
	f.AddMethod1(vm.Selectors, "map:", func(_ interface{}, recv Value, block Value) Value {
		return recv
	})

	// Failure>>flatMap: block - don't evaluate (propagate failure)
	f.AddMethod1(vm.Selectors, "flatMap:", func(_ interface{}, recv Value, block Value) Value {
		return recv
	})

	// Result base class (abstract)
	r := vm.ResultClass

	// Result class>>success: value - create a Success (class method)
	r.AddClassMethod1(vm.Selectors, "success:", func(vmPtr interface{}, recv Value, val Value) Value {
		v := vmPtr.(*VM)
		result := createResult(ResultSuccess, val)
		return v.registry.RegisterResultValue(result)
	})

	// Result class>>failure: reason - create a Failure (class method)
	r.AddClassMethod1(vm.Selectors, "failure:", func(vmPtr interface{}, recv Value, reason Value) Value {
		v := vmPtr.(*VM)
		result := createResult(ResultFailure, reason)
		return v.registry.RegisterResultValue(result)
	})
}
