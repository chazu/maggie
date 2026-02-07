package vm

import (
	"sync"
	"sync/atomic"
)

// ---------------------------------------------------------------------------
// WaitGroup: Wraps Go sync.WaitGroup for Smalltalk
// ---------------------------------------------------------------------------

// WaitGroupObject wraps a Go WaitGroup for use in Smalltalk.
type WaitGroupObject struct {
	vtable  *VTable
	wg      sync.WaitGroup
	counter atomic.Int32 // Track count for inspection
}


func createWaitGroup() *WaitGroupObject {
	return &WaitGroupObject{}
}

func waitGroupToValue(id int) Value {
	return FromSymbolID(uint32(id) | waitGroupMarker)
}

func isWaitGroupValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return (id & (0xFF << 24)) == waitGroupMarker
}

// ---------------------------------------------------------------------------
// WaitGroup primitives registration
// ---------------------------------------------------------------------------

func (vm *VM) registerWaitGroupPrimitives() {
	wg := vm.WaitGroupClass

	// WaitGroup class>>new - create a new wait group
	wg.AddClassMethod0(vm.Selectors, "new", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		waitGroup := createWaitGroup()
		return v.registerWaitGroup(waitGroup)
	})

	// WaitGroup>>add: count - add to the wait group counter
	wg.AddMethod1(vm.Selectors, "add:", func(vmPtr interface{}, recv Value, count Value) Value {
		v := vmPtr.(*VM)
		w := v.getWaitGroup(recv)
		if w == nil {
			return Nil
		}
		if !count.IsSmallInt() {
			return Nil
		}
		n := int(count.SmallInt())
		w.wg.Add(n)
		w.counter.Add(int32(n))
		return recv
	})

	// WaitGroup>>done - decrement the wait group counter by 1
	wg.AddMethod0(vm.Selectors, "done", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		w := v.getWaitGroup(recv)
		if w == nil {
			return Nil
		}
		w.counter.Add(-1)
		w.wg.Done()
		return recv
	})

	// WaitGroup>>wait - block until the counter is zero
	wg.AddMethod0(vm.Selectors, "wait", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		w := v.getWaitGroup(recv)
		if w == nil {
			return Nil
		}
		w.wg.Wait()
		return recv
	})

	// WaitGroup>>count - get the current counter value (for debugging)
	wg.AddMethod0(vm.Selectors, "count", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		w := v.getWaitGroup(recv)
		if w == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(w.counter.Load()))
	})

	// WaitGroup>>wrap: aBlock - convenience: add 1, fork block, done when block completes
	// Returns the forked process
	wg.AddMethod1(vm.Selectors, "wrap:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		w := v.getWaitGroup(recv)
		if w == nil {
			return Nil
		}

		bv := v.currentInterpreter().getBlockValue(block)
		if bv == nil {
			return Nil
		}

		// Add 1 to the wait group
		w.wg.Add(1)
		w.counter.Add(1)

		// Fork the block with automatic done
		proc := v.createProcess()
		procValue := v.registerProcess(proc)

		go func() {
			defer func() {
				// Always call done, even if block panics
				w.counter.Add(-1)
				w.wg.Done()

				if r := recover(); r != nil {
					if nlr, ok := r.(NonLocalReturn); ok {
						proc.markDone(nlr.Value, nil)
					} else {
						proc.markDone(Nil, nil)
					}
				}
				v.unregisterInterpreter()
			}()

			interp := v.newInterpreter()
			v.registerInterpreter(interp)
			result := interp.ExecuteBlockDetached(bv.Block, bv.Captures, nil, bv.HomeSelf, bv.HomeMethod)
			proc.markDone(result, nil)
		}()

		return procValue
	})
}
