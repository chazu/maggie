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

// tryDone decrements the counter by one, returning false (and leaving the
// WaitGroup untouched) if it is already zero. sync.WaitGroup.Done below zero
// panics 'negative WaitGroup counter' and crashes the goroutine, so every
// decrement goes through this guard; the CAS loop keeps the mirror and the
// real wg in lockstep.
func (w *WaitGroupObject) tryDone() bool {
	for {
		cur := w.counter.Load()
		if cur <= 0 {
			return false
		}
		if w.counter.CompareAndSwap(cur, cur-1) {
			w.wg.Done()
			return true
		}
	}
}

func createWaitGroup() *WaitGroupObject {
	return &WaitGroupObject{}
}

func isWaitGroupValue(v Value) bool {
	return v.ptr != nil && v.hi == kindWaitGroup
}

// ---------------------------------------------------------------------------
// WaitGroup primitives registration
// ---------------------------------------------------------------------------

func (vm *VM) registerWaitGroupPrimitives() {
	wg := vm.WaitGroupClass

	// WaitGroup class>>new - create a new wait group
	wg.AddClassMethod0(vm.Selectors, "new", func(v *VM, recv Value) Value {
		waitGroup := createWaitGroup()
		return v.registerWaitGroup(waitGroup)
	})

	wg.AddClassMethod0(vm.Selectors, "primNew", func(v *VM, recv Value) Value {
		waitGroup := createWaitGroup()
		return v.registerWaitGroup(waitGroup)
	})

	// WaitGroup>>add: count - add to the wait group counter
	wg.AddMethod1(vm.Selectors, "add:", func(v *VM, recv Value, count Value) Value {
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

	wg.AddMethod1(vm.Selectors, "primAdd:", func(v *VM, recv Value, count Value) Value {
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
	doneFn := func(v *VM, recv Value) Value {
		w := v.getWaitGroup(recv)
		if w == nil {
			return Nil
		}
		if !w.tryDone() {
			return v.SignalPrimitiveError("done", "WaitGroup counter is already zero")
		}
		return recv
	}
	wg.AddMethod0(vm.Selectors, "done", doneFn)
	wg.AddMethod0(vm.Selectors, "primDone", doneFn)

	// WaitGroup>>wait - block until the counter is zero
	wg.AddMethod0(vm.Selectors, "wait", func(v *VM, recv Value) Value {
		w := v.getWaitGroup(recv)
		if w == nil {
			return Nil
		}
		v.waitGroupKillable(&w.wg)
		return recv
	})

	wg.AddMethod0(vm.Selectors, "primWait", func(v *VM, recv Value) Value {
		w := v.getWaitGroup(recv)
		if w == nil {
			return Nil
		}
		v.waitGroupKillable(&w.wg)
		return recv
	})

	// WaitGroup>>count - get the current counter value (for debugging)
	wg.AddMethod0(vm.Selectors, "count", func(v *VM, recv Value) Value {
		w := v.getWaitGroup(recv)
		if w == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(w.counter.Load()))
	})

	wg.AddMethod0(vm.Selectors, "primCount", func(v *VM, recv Value) Value {
		w := v.getWaitGroup(recv)
		if w == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(w.counter.Load()))
	})

	// WaitGroup>>wrap: aBlock - convenience: add 1, fork block, done when block completes
	// Returns the forked process
	wrapFn := func(v *VM, recv Value, block Value) Value {
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

		// Restrictions MUST be computed here, on the caller's goroutine: on
		// the new goroutine currentInterpreter() resolves to the main
		// interpreter, and a forkRestricted: process would escape its sandbox.
		hidden := v.inheritedHidden(nil)

		go func() {
			defer func() {
				// Always call done, even if block panics. Guarded: an extra
				// `wg done` inside (or racing) the block may already have
				// taken the counter to zero.
				w.tryDone()

				v.HandleForkedPanic(proc, recover())
				v.unregisterInterpreter()
			}()

			// newForkedInterpreter (not newInterpreter) so global writes go to
			// a COW overlay and forkRestricted: hidden-global restrictions are
			// inherited — otherwise a sandboxed process escapes via wrap:.
			interp := v.newForkedInterpreter(hidden)
			interp.bindProcess(proc)
			v.registerInterpreter(interp)
			result := interp.ExecuteBlockDetached(bv.Block, bv.Captures, nil, bv.HomeSelf, bv.HomeMethod)
			// FinishProcess (not markDone) so the live-process index and name
			// registry are cleaned up and links/monitors are notified.
			v.FinishProcess(proc, ExitNormal(result))
		}()

		return procValue
	}
	wg.AddMethod1(vm.Selectors, "wrap:", wrapFn)
	wg.AddMethod1(vm.Selectors, "primWrap:", wrapFn)
}
