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
	vm.Globals["Future"] = vm.classValue(c)

	vm.symbolDispatch.Register(promiseMarker, &SymbolTypeEntry{Class: c})

	// Future>>await — blocking, raises RemoteError on failure
	c.AddMethod0(vm.Selectors, "await", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		f := v.getFuture(recv)
		if f == nil {
			return Nil
		}
		<-f.ch // block until resolved
		f.mu.Lock()
		defer f.mu.Unlock()
		if f.err != "" {
			return v.signalRemoteError(f.err)
		}
		return f.result
	})

	// Future>>await: timeoutMs — blocking with timeout
	c.AddMethod1(vm.Selectors, "await:", func(vmPtr interface{}, recv, timeoutVal Value) Value {
		v := vmPtr.(*VM)
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
			f.mu.Lock()
			defer f.mu.Unlock()
			if f.err != "" {
				return v.signalRemoteError(f.err)
			}
			return f.result
		case <-time.After(time.Duration(ms) * time.Millisecond):
			// Return nil on timeout (raising exceptions requires interpreter context)
			return Nil
		}
	})

	// Future>>isResolved
	c.AddMethod0(vm.Selectors, "isResolved", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod0(vm.Selectors, "result", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		f := v.getFuture(recv)
		if f == nil {
			return Nil
		}
		return f.Result()
	})

	// Future>>error — returns error string or nil
	c.AddMethod0(vm.Selectors, "error", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod0(vm.Selectors, "printString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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

// signalRemoteError returns nil with the error message logged.
// Full exception signaling requires interpreter context; for now,
// the error is accessible via Future>>error.
func (vm *VM) signalRemoteError(msg string) Value {
	// TODO: when exception infrastructure supports raising from primitives,
	// raise a RemoteError here instead of returning Nil
	_ = msg
	return Nil
}
