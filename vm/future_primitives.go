package vm

import (
	"fmt"
	"time"
)

func (vm *VM) registerFuture(f *FutureObject) Value {
	return vm.registry.RegisterFuture(f)
}

func (vm *VM) getFuture(v Value) *FutureObject {
	return vm.registry.GetFuture(v)
}

func (vm *VM) registerFuturePrimitives() {
	c := vm.createClass("Future", vm.ObjectClass)
	vm.FutureClass = c
	vm.globals["Future"] = vm.classValue(c)

	// Futures are pointer-carrying heap Values resolved via classForHeap.

	// Future>>await — blocking, raises the remote exception/error on failure
	c.AddMethod0(vm.Selectors, "await", func(v *VM, recv Value) Value {
		f := v.getFuture(recv)
		if f == nil {
			return Nil
		}
		<-f.ch // block until resolved
		return v.futureResolvedValue(f)
	})

	// Future>>await: timeoutMs — blocking with timeout; nil on timeout.
	// Prefer await:ifTimeout: which disambiguates timeout from a nil result.
	c.AddMethod1(vm.Selectors, "await:", func(v *VM, recv, timeoutVal Value) Value {
		f := v.getFuture(recv)
		if f == nil {
			return Nil
		}
		if !timeoutVal.IsSmallInt() {
			return Nil
		}
		ms := timeoutVal.SmallInt()
		select {
		case <-f.ch:
			return v.futureResolvedValue(f)
		case <-time.After(time.Duration(ms) * time.Millisecond):
			return Nil
		}
	})

	// Future>>await:ifTimeout: — blocking with timeout; evaluates the block
	// on timeout, so a nil result is distinguishable from a deadline miss.
	c.AddMethod2(vm.Selectors, "await:ifTimeout:", func(v *VM, recv, timeoutVal, blockVal Value) Value {
		f := v.getFuture(recv)
		if f == nil {
			return Nil
		}
		if !timeoutVal.IsSmallInt() {
			return Nil
		}
		ms := timeoutVal.SmallInt()
		select {
		case <-f.ch:
			return v.futureResolvedValue(f)
		case <-time.After(time.Duration(ms) * time.Millisecond):
			return v.evaluateBlock(blockVal, nil)
		}
	})

	// Future>>isResolved
	c.AddMethod0(vm.Selectors, "isResolved", func(v *VM, recv Value) Value {
		f := v.getFuture(recv)
		if f == nil {
			return False
		}
		if f.IsResolved() {
			return True
		}
		return False
	})

	// Future>>result — non-blocking, returns nil if not resolved
	c.AddMethod0(vm.Selectors, "result", func(v *VM, recv Value) Value {
		f := v.getFuture(recv)
		if f == nil {
			return Nil
		}
		return f.Result()
	})

	// Future>>exception — returns the typed exception value, or nil
	c.AddMethod0(vm.Selectors, "exception", func(v *VM, recv Value) Value {
		f := v.getFuture(recv)
		if f == nil {
			return Nil
		}
		exVal := f.ExceptionValue()
		if exVal == Nil {
			return Nil
		}
		return exVal
	})

	// Future>>error — returns error string or nil
	c.AddMethod0(vm.Selectors, "error", func(v *VM, recv Value) Value {
		f := v.getFuture(recv)
		if f == nil {
			return Nil
		}
		errMsg := f.Error()
		if errMsg == "" {
			return Nil
		}
		return v.registry.NewStringValue(errMsg)
	})

	// Future>>printString
	c.AddMethod0(vm.Selectors, "printString", func(v *VM, recv Value) Value {
		f := v.getFuture(recv)
		if f == nil {
			return v.registry.NewStringValue("a Future (invalid)")
		}
		if f.IsResolved() {
			if f.Error() != "" {
				return v.registry.NewStringValue(fmt.Sprintf("a Future (error: %s)", f.Error()))
			}
			return v.registry.NewStringValue("a Future (resolved)")
		}
		return v.registry.NewStringValue("a Future (pending)")
	})
}

// futureResolvedValue extracts a resolved Future's value, re-signaling the
// remote typed exception or raising a catchable Error for plain remote
// errors (failure doctrine: await signals, it never answers nil-for-error).
func (vm *VM) futureResolvedValue(f *FutureObject) Value {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.exceptionValue != Nil && f.exceptionValue.IsException() {
		// Re-signal the typed exception from the remote node
		if exObj := vm.registry.GetExceptionFromValue(f.exceptionValue); exObj != nil {
			vm.signalExceptionObject(f.exceptionValue, exObj)
			return Nil // unreachable — signalExceptionObject always panics
		}
	}
	if f.err != "" {
		return vm.signalRemoteError(f.err)
	}
	return f.result
}

// signalRemoteError raises a catchable Error carrying the remote failure
// message (the side-channel `error` accessor remains for non-blocking
// inspection, but await never smuggles errors through nil).
func (vm *VM) signalRemoteError(msg string) Value {
	exObj := &ExceptionObject{
		ExceptionClass: vm.ErrorClass,
		MessageText:    vm.registry.NewStringValue(msg),
		Resumable:      false,
	}
	exVal := vm.registry.RegisterExceptionValue(exObj)
	return vm.signalExceptionObject(exVal, exObj)
}
