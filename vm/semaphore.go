package vm

import (
	"sync"
	"sync/atomic"
)

// ---------------------------------------------------------------------------
// Semaphore: Counting semaphore using buffered channel pattern
// ---------------------------------------------------------------------------

// SemaphoreObject implements a counting semaphore for Smalltalk.
// Uses a buffered channel where the capacity is the max permits.
type SemaphoreObject struct {
	vtable   *VTable
	permits  chan struct{}
	capacity int
}

// semaphoreRegistry stores active semaphores
var semaphoreRegistry = make(map[int]*SemaphoreObject)
var semaphoreRegistryMu sync.RWMutex
var nextSemaphoreID atomic.Int32

// Semaphore marker for symbol encoding
const semaphoreMarker uint32 = 34 << 24

func init() {
	nextSemaphoreID.Store(1)
}

func createSemaphore(capacity int) *SemaphoreObject {
	if capacity < 1 {
		capacity = 1
	}
	sem := &SemaphoreObject{
		permits:  make(chan struct{}, capacity),
		capacity: capacity,
	}
	// Pre-fill with available permits
	for i := 0; i < capacity; i++ {
		sem.permits <- struct{}{}
	}
	return sem
}

func registerSemaphore(sem *SemaphoreObject) Value {
	id := int(nextSemaphoreID.Add(1) - 1)

	semaphoreRegistryMu.Lock()
	semaphoreRegistry[id] = sem
	semaphoreRegistryMu.Unlock()

	return semaphoreToValue(id)
}

func semaphoreToValue(id int) Value {
	return FromSymbolID(uint32(id) | semaphoreMarker)
}

func isSemaphoreValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return (id & (0xFF << 24)) == semaphoreMarker
}

func getSemaphore(v Value) *SemaphoreObject {
	if !isSemaphoreValue(v) {
		return nil
	}
	id := int(v.SymbolID() & ^uint32(0xFF<<24))

	semaphoreRegistryMu.RLock()
	defer semaphoreRegistryMu.RUnlock()
	return semaphoreRegistry[id]
}

// ---------------------------------------------------------------------------
// Semaphore primitives registration
// ---------------------------------------------------------------------------

func (vm *VM) registerSemaphorePrimitives() {
	s := vm.SemaphoreClass

	// Semaphore class>>new: capacity - create a semaphore with given capacity
	s.AddClassMethod1(vm.Selectors, "new:", func(vmPtr interface{}, recv Value, capacity Value) Value {
		v := vmPtr.(*VM)
		if !capacity.IsSmallInt() {
			return Nil
		}
		cap := int(capacity.SmallInt())
		sem := createSemaphore(cap)
		return v.registerSemaphore(sem)
	})

	// Semaphore class>>new - create a binary semaphore (capacity 1)
	s.AddClassMethod0(vm.Selectors, "new", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		sem := createSemaphore(1)
		return v.registerSemaphore(sem)
	})

	// Semaphore>>acquire - acquire a permit (blocks if none available)
	s.AddMethod0(vm.Selectors, "acquire", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		sem := v.getSemaphore(recv)
		if sem == nil {
			return Nil
		}
		<-sem.permits
		return recv
	})

	// Semaphore>>release - release a permit
	s.AddMethod0(vm.Selectors, "release", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		sem := v.getSemaphore(recv)
		if sem == nil {
			return Nil
		}
		select {
		case sem.permits <- struct{}{}:
			// Permit released
		default:
			// Semaphore already at capacity, ignore (prevents deadlock)
		}
		return recv
	})

	// Semaphore>>tryAcquire - try to acquire a permit without blocking
	// Returns true if acquired, false otherwise
	s.AddMethod0(vm.Selectors, "tryAcquire", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		sem := v.getSemaphore(recv)
		if sem == nil {
			return False
		}
		select {
		case <-sem.permits:
			return True
		default:
			return False
		}
	})

	// Semaphore>>available - number of available permits
	s.AddMethod0(vm.Selectors, "available", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		sem := v.getSemaphore(recv)
		if sem == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(len(sem.permits)))
	})

	// Semaphore>>capacity - total capacity of the semaphore
	s.AddMethod0(vm.Selectors, "capacity", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		sem := v.getSemaphore(recv)
		if sem == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(sem.capacity))
	})

	// Semaphore>>critical: aBlock - execute block while holding a permit
	// Automatically releases permit when done
	s.AddMethod1(vm.Selectors, "critical:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		sem := v.getSemaphore(recv)
		if sem == nil {
			return Nil
		}

		bv := v.currentInterpreter().getBlockValue(block)
		if bv == nil {
			return Nil
		}

		// Acquire permit
		<-sem.permits
		defer func() {
			// Release permit
			select {
			case sem.permits <- struct{}{}:
			default:
			}
		}()

		// Execute the block
		return v.currentInterpreter().ExecuteBlock(
			bv.Block, bv.Captures, nil,
			bv.HomeFrame, bv.HomeSelf, bv.HomeMethod,
		)
	})

	// Semaphore>>withPermit: aBlock - alias for critical:
	s.AddMethod1(vm.Selectors, "withPermit:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		sem := v.getSemaphore(recv)
		if sem == nil {
			return Nil
		}

		bv := v.currentInterpreter().getBlockValue(block)
		if bv == nil {
			return Nil
		}

		<-sem.permits
		defer func() {
			select {
			case sem.permits <- struct{}{}:
			default:
			}
		}()

		return v.currentInterpreter().ExecuteBlock(
			bv.Block, bv.Captures, nil,
			bv.HomeFrame, bv.HomeSelf, bv.HomeMethod,
		)
	})
}
