package vm

import (
	"sync"
	"sync/atomic"
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

// resultRegistry stores active results
var resultRegistry = make(map[int]*ResultObject)
var resultRegistryMu sync.Mutex
var nextResultID int32 = 1

func createResult(rtype ResultType, val Value) *ResultObject {
	return &ResultObject{
		resultType: rtype,
		value:      val,
	}
}

func registerResult(r *ResultObject) Value {
	resultRegistryMu.Lock()
	defer resultRegistryMu.Unlock()

	id := int(atomic.AddInt32(&nextResultID, 1) - 1)
	resultRegistry[id] = r

	return resultToValue(id)
}

func resultToValue(id int) Value {
	// Use symbol encoding with marker 4 << 24 (channels use 1, processes use 2)
	return FromSymbolID(uint32(id) | (4 << 24))
}

func isResultValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	// Check for marker 4 in upper bits (bit 26)
	return (id & (4 << 24)) != 0
}

func getResult(v Value) *ResultObject {
	if !isResultValue(v) {
		return nil
	}
	id := int(v.SymbolID() & ^uint32(4<<24))

	resultRegistryMu.Lock()
	defer resultRegistryMu.Unlock()
	return resultRegistry[id]
}

// ---------------------------------------------------------------------------
// Result primitives registration
// ---------------------------------------------------------------------------

func (vm *VM) registerResultPrimitives() {
	// Success class
	s := vm.SuccessClass

	// Success class>>with: value - create a Success wrapping value
	s.AddMethod1(vm.Selectors, "with:", func(_ interface{}, recv Value, val Value) Value {
		r := createResult(ResultSuccess, val)
		return registerResult(r)
	})

	// Success>>value - return the wrapped value
	s.AddMethod0(vm.Selectors, "value", func(_ interface{}, recv Value) Value {
		r := getResult(recv)
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
	s.AddMethod0(vm.Selectors, "isSuccess", func(_ interface{}, recv Value) Value {
		r := getResult(recv)
		if r == nil {
			return False
		}
		if r.resultType == ResultSuccess {
			return True
		}
		return False
	})

	// Success>>isFailure - return false
	s.AddMethod0(vm.Selectors, "isFailure", func(_ interface{}, recv Value) Value {
		r := getResult(recv)
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
		r := getResult(recv)
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
		r := getResult(recv)
		if r == nil || r.resultType != ResultSuccess {
			return Nil
		}
		return v.evaluateBlock(successBlock, []Value{r.value})
	})

	// Success>>then: block - chain operations, evaluate block with value, return new Result
	s.AddMethod1(vm.Selectors, "then:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		r := getResult(recv)
		if r == nil || r.resultType != ResultSuccess {
			return recv
		}
		result := v.evaluateBlock(block, []Value{r.value})
		// If block returns a Result, return it; otherwise wrap in Success
		if isResultValue(result) {
			return result
		}
		newR := createResult(ResultSuccess, result)
		return registerResult(newR)
	})

	// Success>>map: block - transform value, return new Success
	s.AddMethod1(vm.Selectors, "map:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		r := getResult(recv)
		if r == nil || r.resultType != ResultSuccess {
			return recv
		}
		newVal := v.evaluateBlock(block, []Value{r.value})
		newR := createResult(ResultSuccess, newVal)
		return registerResult(newR)
	})

	// Success>>flatMap: block - transform value, block must return Result
	s.AddMethod1(vm.Selectors, "flatMap:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		r := getResult(recv)
		if r == nil || r.resultType != ResultSuccess {
			return recv
		}
		return v.evaluateBlock(block, []Value{r.value})
	})

	// Failure class
	f := vm.FailureClass

	// Failure class>>with: reason - create a Failure wrapping reason
	f.AddMethod1(vm.Selectors, "with:", func(_ interface{}, recv Value, reason Value) Value {
		r := createResult(ResultFailure, reason)
		return registerResult(r)
	})

	// Failure>>value - return nil (Failure has no value)
	f.AddMethod0(vm.Selectors, "value", func(_ interface{}, recv Value) Value {
		return Nil
	})

	// Failure>>error - return the error/reason
	f.AddMethod0(vm.Selectors, "error", func(_ interface{}, recv Value) Value {
		r := getResult(recv)
		if r == nil || r.resultType != ResultFailure {
			return Nil
		}
		return r.value
	})

	// Failure>>isSuccess - return false
	f.AddMethod0(vm.Selectors, "isSuccess", func(_ interface{}, recv Value) Value {
		r := getResult(recv)
		if r == nil {
			return False
		}
		if r.resultType == ResultSuccess {
			return True
		}
		return False
	})

	// Failure>>isFailure - return true
	f.AddMethod0(vm.Selectors, "isFailure", func(_ interface{}, recv Value) Value {
		r := getResult(recv)
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
		r := getResult(recv)
		if r == nil || r.resultType != ResultFailure {
			return recv
		}
		return v.evaluateBlock(block, []Value{r.value})
	})

	// Failure>>onSuccess:onFailure: - evaluate failure block
	f.AddMethod2(vm.Selectors, "onSuccess:onFailure:", func(vmPtr interface{}, recv Value, successBlock, failureBlock Value) Value {
		v := vmPtr.(*VM)
		r := getResult(recv)
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

	// Result class>>success: value - create a Success
	r.AddMethod1(vm.Selectors, "success:", func(_ interface{}, recv Value, val Value) Value {
		result := createResult(ResultSuccess, val)
		return registerResult(result)
	})

	// Result class>>failure: reason - create a Failure
	r.AddMethod1(vm.Selectors, "failure:", func(_ interface{}, recv Value, reason Value) Value {
		result := createResult(ResultFailure, reason)
		return registerResult(result)
	})
}
