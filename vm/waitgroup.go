package vm

import (
	"math"
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
	mu      sync.Mutex   // serializes counter updates with wg.Add
	counter atomic.Int32 // Track count for inspection
}

// tryAdd adjusts the counter by n, returning false (and leaving the WaitGroup
// untouched) if the result would leave the range sync.WaitGroup supports:
// below zero it panics 'negative WaitGroup counter' AFTER corrupting its
// state, and its counter is 32 bits, so a total above MaxInt32 would silently
// wrap. The check and the wg.Add happen under mu so the mirror and the real
// wg can never disagree (a CAS-then-Add would let a racing decrement reach
// the wg before the matching increment and drive it negative).
func (w *WaitGroupObject) tryAdd(n int64) bool {
	w.mu.Lock()
	defer w.mu.Unlock()
	next := int64(w.counter.Load()) + n
	if next < 0 || next > math.MaxInt32 {
		return false
	}
	w.wg.Add(int(n))
	w.counter.Store(int32(next))
	return true
}

// tryDone decrements the counter by one, returning false (and leaving the
// WaitGroup untouched) if it is already zero. Every decrement goes through
// this guard.
func (w *WaitGroupObject) tryDone() bool {
	return w.tryAdd(-1)
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

	// WaitGroup>>add: count - add to the wait group counter. Negative counts
	// are allowed as long as the counter stays >= 0; a result below zero (or
	// beyond the 32-bit counter) is a programmer error and signals without
	// touching the group.
	addFn := func(v *VM, recv Value, count Value) Value {
		w := v.getWaitGroup(recv)
		if w == nil {
			return Nil
		}
		if !count.IsSmallInt() {
			return v.SignalTypeError("add:", 1, "Integer", count)
		}
		if !w.tryAdd(count.SmallInt()) {
			return v.SignalPrimitiveError("add:", "WaitGroup counter would go negative or overflow")
		}
		return recv
	}
	wg.AddMethod1(vm.Selectors, "add:", addFn)
	wg.AddMethod1(vm.Selectors, "primAdd:", addFn)

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
		if !w.tryAdd(1) {
			return v.SignalPrimitiveError("wrap:", "WaitGroup counter overflow")
		}

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
