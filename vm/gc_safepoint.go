package vm

import "time"

// ---------------------------------------------------------------------------
// Stop-the-world safepoint coordination for the string/dictionary collector.
// ---------------------------------------------------------------------------
//
// The collector (heap_gc.go) traces mutable VM state without per-field locks,
// so it must run with no other goroutine mutating that state. Maggie executes
// each process on its own goroutine with no global interpreter lock, so we
// coordinate a cooperative stop-the-world:
//
//   - Every interpreter checks gcRequested at the top of its dispatch loop
//     (one relaxed atomic load on the hot path) and parks when a collection
//     is in progress. At the loop top all live Values are in the operand
//     stack / frames — none are stranded in Go locals — so the frozen state
//     is a complete root set.
//
//   - Blocking primitives bracket their blocking call with enterBlocked /
//     exitBlocked. While blocked an interpreter is "stopped" (the collector
//     need not wait for it) and publishes the Values it holds in Go locals
//     (receiver/args) via blockedRoots so they are still marked.
//
//   - The collector arms gcRequested, waits for every active interpreter to be
//     parked or blocked, traces+sweeps, then disarms and wakes everyone.
//
// SAFETY VS LIVENESS: the barrier wait is bounded by gcBarrierTimeout. If a
// goroutine is stuck in an un-instrumented blocking primitive it will never
// park; rather than hang the VM, the collector ABORTS the cycle (no sweep).
// A missed blocking-primitive instrumentation therefore only costs a skipped
// collection, never a deadlock or a freed-but-live string.
//
// The whole feature is opt-in (EnableStringGC / MAGGIE_GC); when disabled the
// safepoint check is a single always-false atomic load and nothing parks.

const (
	gcStateRunning int32 = 0
	gcStateParked  int32 = 1
	gcStateBlocked int32 = 2
)

// gcBarrierTimeout bounds how long CollectStringGarbage waits for all mutators
// to reach a safepoint before aborting the cycle.
const gcBarrierTimeout = 250 * time.Millisecond

// EnableStringGC turns on the opt-in string/dictionary tracing collector.
func (vm *VM) EnableStringGC() { vm.gcEnabled.Store(true) }

// StringGCEnabled reports whether the collector is enabled.
func (vm *VM) StringGCEnabled() bool { return vm.gcEnabled.Load() }

// safepoint parks the interpreter if a collection is in progress. It is called
// at the top of the dispatch loop; the gcRequested fast-path check is inlined
// at the call site so this is only reached when a collection is pending.
func (i *Interpreter) safepoint() {
	if i.vm == nil {
		return
	}
	i.vm.parkAtSafepoint(i)
}

// parkAtSafepoint marks the interpreter parked and blocks until the in-flight
// collection finishes.
func (vm *VM) parkAtSafepoint(i *Interpreter) {
	vm.gcBarrierMu.Lock()
	i.gcState.Store(gcStateParked)
	vm.gcBarrierCond.Broadcast() // let the collector observe us parked
	for vm.gcRequested.Load() {
		vm.gcBarrierCond.Wait()
	}
	i.gcState.Store(gcStateRunning)
	vm.gcBarrierMu.Unlock()
}

// enterBlocked marks the current interpreter blocked for the duration of a
// blocking primitive and publishes the Values it holds in Go locals so the
// collector still marks them. No-op when the collector is disabled. Must be
// paired with exitBlocked.
func (vm *VM) enterBlocked(roots ...Value) {
	if vm == nil || !vm.gcEnabled.Load() {
		return
	}
	i := vm.currentInterpreter()
	if i == nil {
		return
	}
	vm.gcBarrierMu.Lock()
	i.blockedRoots = append(i.blockedRoots[:0], roots...)
	i.gcState.Store(gcStateBlocked)
	vm.gcBarrierCond.Broadcast()
	vm.gcBarrierMu.Unlock()
}

// exitBlocked clears the blocked state after a blocking primitive returns. If a
// collection is in progress it waits for it to finish first (the collector may
// be reading our published roots).
func (vm *VM) exitBlocked() {
	if vm == nil || !vm.gcEnabled.Load() {
		return
	}
	i := vm.currentInterpreter()
	if i == nil {
		return
	}
	vm.gcBarrierMu.Lock()
	for vm.gcRequested.Load() {
		vm.gcBarrierCond.Wait()
	}
	i.blockedRoots = i.blockedRoots[:0]
	i.gcState.Store(gcStateRunning)
	vm.gcBarrierMu.Unlock()
}

// mutatorStopped reports whether interp is parked or blocked (safe to trace).
func mutatorStopped(interp *Interpreter) bool {
	s := interp.gcState.Load()
	return s == gcStateParked || s == gcStateBlocked
}

// allMutatorsStopped reports whether every active interpreter is parked or
// blocked. Caller must hold gcBarrierMu. The calling goroutine (the collector)
// is assumed not to be a registered mutator.
func (vm *VM) allMutatorsStopped() bool {
	if vm.interpreter != nil && !mutatorStopped(vm.interpreter) {
		// The main interpreter is only a blocker if its goroutine is actively
		// running bytecode. If it is parked/blocked we proceed.
		if vm.mainInterpreterActive() {
			return false
		}
	}
	stopped := true
	vm.interpreters.Range(func(_, v any) bool {
		interp, ok := v.(*Interpreter)
		if !ok || interp == vm.interpreter {
			return true
		}
		if !mutatorStopped(interp) {
			stopped = false
			return false
		}
		return true
	})
	return stopped
}

// mainInterpreterActive reports whether the main interpreter is currently
// executing (has at least one active frame). A main interpreter sitting idle
// with no frames is not a mutator and need not park.
func (vm *VM) mainInterpreterActive() bool {
	mi := vm.interpreter
	return mi != nil && mi.fp >= 0 && mi.sp > 0
}

// CollectStringGarbage runs one stop-the-world string/dictionary collection.
// It returns the number of strings and dictionaries freed and whether the
// cycle actually ran (false if disabled or if the barrier timed out). Safe to
// call only from a goroutine that is NOT a registered Maggie mutator (e.g. the
// RegistryGC background goroutine).
func (vm *VM) CollectStringGarbage() (strings, dicts int, ran bool) {
	if !vm.gcEnabled.Load() {
		return 0, 0, false
	}
	vm.gcRunMu.Lock() // one collection at a time
	defer vm.gcRunMu.Unlock()

	vm.gcBarrierMu.Lock()
	vm.gcRequested.Store(true)

	// Bounded wait for all mutators to reach a safepoint. A timer broadcasts
	// to break the Cond.Wait; on timeout we abort rather than hang.
	timedOut := false
	timer := time.AfterFunc(gcBarrierTimeout, func() {
		vm.gcBarrierMu.Lock()
		timedOut = true
		vm.gcBarrierCond.Broadcast()
		vm.gcBarrierMu.Unlock()
	})
	for !vm.allMutatorsStopped() {
		if timedOut {
			break
		}
		vm.gcBarrierCond.Wait()
	}
	timer.Stop()

	if timedOut && !vm.allMutatorsStopped() {
		// Abort: release everyone, collect nothing.
		vm.gcRequested.Store(false)
		vm.gcBarrierCond.Broadcast()
		vm.gcBarrierMu.Unlock()
		return 0, 0, false
	}

	// Stop-the-world reached: trace + sweep on frozen state.
	strings, dicts = vm.collectHeapGarbageLocked()

	vm.gcRequested.Store(false)
	vm.gcBarrierCond.Broadcast()
	vm.gcBarrierMu.Unlock()
	return strings, dicts, true
}
