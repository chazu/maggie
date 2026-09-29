package vm

import (
	"sync"
	"sync/atomic"
)

// ---------------------------------------------------------------------------
// Mutex: Wraps Go sync.Mutex for Smalltalk
// ---------------------------------------------------------------------------

// MutexObject wraps a Go mutex for use in Smalltalk.
type MutexObject struct {
	vtable *VTable
	mu     sync.Mutex
	locked atomic.Bool // Track if locked (for tryLock and debugging)

	// owner is a token unique to the current acquisition (0 = unlocked). It
	// lets critical:'s cleanup release only the acquisition it made: if the
	// block unlocks the mutex itself (and another process then takes it),
	// the cleanup must neither double-unlock nor release the new holder.
	owner   atomic.Uint64
	nextTok atomic.Uint64
}

// acquired records a fresh acquisition. Caller must hold mu.mu.
func (mu *MutexObject) acquired() uint64 {
	tok := mu.nextTok.Add(1)
	mu.owner.Store(tok)
	mu.locked.Store(true)
	return tok
}

func createMutex() *MutexObject {
	return &MutexObject{}
}

func isMutexValue(v Value) bool {
	return v.ptr != nil && v.hi == kindMutex
}

// ---------------------------------------------------------------------------
// Mutex primitives registration
// ---------------------------------------------------------------------------

func (vm *VM) registerMutexPrimitives() {
	m := vm.MutexClass

	// Mutex class>>new - create a new mutex
	newMutexFn := func(v *VM, recv Value) Value {
		mutex := createMutex()
		return v.registerMutex(mutex)
	}
	m.AddClassMethod0(vm.Selectors, "new", newMutexFn)
	m.AddClassMethod0(vm.Selectors, "primNew", newMutexFn)

	// Mutex>>lock - acquire the mutex (blocks if already held)
	lockFn := func(v *VM, recv Value) Value {
		mu := v.getMutex(recv)
		if mu == nil {
			return Nil
		}
		v.lockKillable(&mu.mu)
		mu.acquired()
		return recv
	}
	m.AddMethod0(vm.Selectors, "lock", lockFn)
	m.AddMethod0(vm.Selectors, "primLock", lockFn)

	// Mutex>>unlock - release the mutex
	unlockFn := func(v *VM, recv Value) Value {
		mu := v.getMutex(recv)
		if mu == nil {
			return Nil
		}
		// Guard: sync.Mutex.Unlock on an unlocked mutex is a Go *fatal* error
		// that no on:do: can catch. The CAS also serializes concurrent
		// unlocks so at most one reaches the real Unlock. Unlocking a mutex
		// that isn't locked is a programmer error → signal a catchable Error.
		if !mu.locked.CompareAndSwap(true, false) {
			return v.SignalPrimitiveError("unlock", "mutex is not locked")
		}
		mu.owner.Store(0)
		mu.mu.Unlock()
		return recv
	}
	m.AddMethod0(vm.Selectors, "unlock", unlockFn)
	m.AddMethod0(vm.Selectors, "primUnlock", unlockFn)

	// Mutex>>tryLock - try to acquire the mutex without blocking
	// Returns true if acquired, false if already held
	tryLockFn := func(v *VM, recv Value) Value {
		mu := v.getMutex(recv)
		if mu == nil {
			return False
		}
		if mu.mu.TryLock() {
			mu.acquired()
			return True
		}
		return False
	}
	m.AddMethod0(vm.Selectors, "tryLock", tryLockFn)
	m.AddMethod0(vm.Selectors, "primTryLock", tryLockFn)

	// Mutex>>isLocked - check if mutex is currently locked
	isLockedFn := func(v *VM, recv Value) Value {
		mu := v.getMutex(recv)
		if mu == nil {
			return False
		}
		if mu.locked.Load() {
			return True
		}
		return False
	}
	m.AddMethod0(vm.Selectors, "isLocked", isLockedFn)
	m.AddMethod0(vm.Selectors, "primIsLocked", isLockedFn)

	// Mutex>>critical: aBlock - execute block while holding the lock
	// Automatically unlocks even if block raises exception
	criticalFn := func(v *VM, recv Value, block Value) Value {
		mu := v.getMutex(recv)
		if mu == nil {
			return Nil
		}

		bv := v.currentInterpreter().getBlockValue(block)
		if bv == nil {
			return Nil
		}

		v.lockKillable(&mu.mu)
		tok := mu.acquired()
		defer func() {
			// Release only our own acquisition: `m critical: [m unlock]`
			// already released it, and an unconditional Unlock here would be
			// Go's uncatchable 'unlock of unlocked mutex' fatal error (or
			// would release another process that has since locked m).
			if mu.owner.CompareAndSwap(tok, 0) {
				mu.locked.Store(false)
				mu.mu.Unlock()
			}
		}()

		// Execute the block using ExecuteBlockDetached to avoid stale
		// HomeFrame references. Blocks passed to critical: should not
		// use non-local returns (^) anyway, so detached mode is safe.
		result := v.currentInterpreter().ExecuteBlockDetached(
			bv.Block, bv.Captures, nil, bv.HomeSelf, bv.HomeMethod,
		)
		return result
	}
	m.AddMethod1(vm.Selectors, "critical:", criticalFn)
	m.AddMethod1(vm.Selectors, "primCritical:", criticalFn)
}
