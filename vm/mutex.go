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
}


func createMutex() *MutexObject {
	return &MutexObject{}
}

func mutexToValue(id int) Value {
	return FromSymbolID(uint32(id) | mutexMarker)
}

func isMutexValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return (id & (0xFF << 24)) == mutexMarker
}

// ---------------------------------------------------------------------------
// Mutex primitives registration
// ---------------------------------------------------------------------------

func (vm *VM) registerMutexPrimitives() {
	m := vm.MutexClass

	// Mutex class>>new - create a new mutex
	newMutexFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		mutex := createMutex()
		return v.registerMutex(mutex)
	}
	m.AddClassMethod0(vm.Selectors, "new", newMutexFn)
	m.AddClassMethod0(vm.Selectors, "primNew", newMutexFn)

	// Mutex>>lock - acquire the mutex (blocks if already held)
	lockFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		mu := v.getMutex(recv)
		if mu == nil {
			return Nil
		}
		mu.mu.Lock()
		mu.locked.Store(true)
		return recv
	}
	m.AddMethod0(vm.Selectors, "lock", lockFn)
	m.AddMethod0(vm.Selectors, "primLock", lockFn)

	// Mutex>>unlock - release the mutex
	unlockFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		mu := v.getMutex(recv)
		if mu == nil {
			return Nil
		}
		mu.locked.Store(false)
		mu.mu.Unlock()
		return recv
	}
	m.AddMethod0(vm.Selectors, "unlock", unlockFn)
	m.AddMethod0(vm.Selectors, "primUnlock", unlockFn)

	// Mutex>>tryLock - try to acquire the mutex without blocking
	// Returns true if acquired, false if already held
	tryLockFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		mu := v.getMutex(recv)
		if mu == nil {
			return False
		}
		if mu.mu.TryLock() {
			mu.locked.Store(true)
			return True
		}
		return False
	}
	m.AddMethod0(vm.Selectors, "tryLock", tryLockFn)
	m.AddMethod0(vm.Selectors, "primTryLock", tryLockFn)

	// Mutex>>isLocked - check if mutex is currently locked
	isLockedFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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
	criticalFn := func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		mu := v.getMutex(recv)
		if mu == nil {
			return Nil
		}

		bv := v.currentInterpreter().getBlockValue(block)
		if bv == nil {
			return Nil
		}

		mu.mu.Lock()
		mu.locked.Store(true)
		defer func() {
			mu.locked.Store(false)
			mu.mu.Unlock()
		}()

		// Execute the block
		return v.currentInterpreter().ExecuteBlock(
			bv.Block, bv.Captures, nil,
			bv.HomeFrame, bv.HomeSelf, bv.HomeMethod,
		)
	}
	m.AddMethod1(vm.Selectors, "critical:", criticalFn)
	m.AddMethod1(vm.Selectors, "primCritical:", criticalFn)
}
