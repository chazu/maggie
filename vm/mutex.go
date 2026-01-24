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

// mutexRegistry stores active mutexes
var mutexRegistry = make(map[int]*MutexObject)
var mutexRegistryMu sync.RWMutex
var nextMutexID atomic.Int32

// Mutex marker for symbol encoding (uses a different bit pattern from channels/processes)
const mutexMarker uint32 = 32 << 24

func init() {
	nextMutexID.Store(1)
}

func createMutex() *MutexObject {
	return &MutexObject{}
}

func registerMutex(m *MutexObject) Value {
	id := int(nextMutexID.Add(1) - 1)

	mutexRegistryMu.Lock()
	mutexRegistry[id] = m
	mutexRegistryMu.Unlock()

	return mutexToValue(id)
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

func getMutex(v Value) *MutexObject {
	if !isMutexValue(v) {
		return nil
	}
	id := int(v.SymbolID() & ^uint32(0xFF<<24))

	mutexRegistryMu.RLock()
	defer mutexRegistryMu.RUnlock()
	return mutexRegistry[id]
}

// ---------------------------------------------------------------------------
// Mutex primitives registration
// ---------------------------------------------------------------------------

func (vm *VM) registerMutexPrimitives() {
	m := vm.MutexClass

	// Mutex class>>new - create a new mutex
	m.AddClassMethod0(vm.Selectors, "new", func(_ interface{}, recv Value) Value {
		mutex := createMutex()
		return registerMutex(mutex)
	})

	// Mutex>>lock - acquire the mutex (blocks if already held)
	m.AddMethod0(vm.Selectors, "lock", func(_ interface{}, recv Value) Value {
		mu := getMutex(recv)
		if mu == nil {
			return Nil
		}
		mu.mu.Lock()
		mu.locked.Store(true)
		return recv
	})

	// Mutex>>unlock - release the mutex
	m.AddMethod0(vm.Selectors, "unlock", func(_ interface{}, recv Value) Value {
		mu := getMutex(recv)
		if mu == nil {
			return Nil
		}
		mu.locked.Store(false)
		mu.mu.Unlock()
		return recv
	})

	// Mutex>>tryLock - try to acquire the mutex without blocking
	// Returns true if acquired, false if already held
	m.AddMethod0(vm.Selectors, "tryLock", func(_ interface{}, recv Value) Value {
		mu := getMutex(recv)
		if mu == nil {
			return False
		}
		if mu.mu.TryLock() {
			mu.locked.Store(true)
			return True
		}
		return False
	})

	// Mutex>>isLocked - check if mutex is currently locked
	m.AddMethod0(vm.Selectors, "isLocked", func(_ interface{}, recv Value) Value {
		mu := getMutex(recv)
		if mu == nil {
			return False
		}
		if mu.locked.Load() {
			return True
		}
		return False
	})

	// Mutex>>critical: aBlock - execute block while holding the lock
	// Automatically unlocks even if block raises exception
	m.AddMethod1(vm.Selectors, "critical:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		mu := getMutex(recv)
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
	})
}
