package vm

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

// ---------------------------------------------------------------------------
// Semaphore primitives registration
// ---------------------------------------------------------------------------

func (vm *VM) registerSemaphorePrimitives() {
	s := vm.SemaphoreClass

	// Semaphore class>>new: capacity - create a semaphore with given capacity
	newCapFn := func(vmPtr interface{}, recv Value, capacity Value) Value {
		v := vmPtr.(*VM)
		if !capacity.IsSmallInt() {
			return Nil
		}
		cap := int(capacity.SmallInt())
		sem := createSemaphore(cap)
		return v.registerSemaphore(sem)
	}
	s.AddClassMethod1(vm.Selectors, "new:", newCapFn)
	s.AddClassMethod1(vm.Selectors, "primNew:", newCapFn)

	// Semaphore class>>new - create a binary semaphore (capacity 1)
	s.AddClassMethod0(vm.Selectors, "new", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		sem := createSemaphore(1)
		return v.registerSemaphore(sem)
	})

	// Semaphore>>acquire - acquire a permit (blocks if none available)
	acquireFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		sem := v.getSemaphore(recv)
		if sem == nil {
			return Nil
		}
		<-sem.permits
		return recv
	}
	s.AddMethod0(vm.Selectors, "acquire", acquireFn)
	s.AddMethod0(vm.Selectors, "primAcquire", acquireFn)

	// Semaphore>>release - release a permit
	releaseFn := func(vmPtr interface{}, recv Value) Value {
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
	}
	s.AddMethod0(vm.Selectors, "release", releaseFn)
	s.AddMethod0(vm.Selectors, "primRelease", releaseFn)

	// Semaphore>>tryAcquire - try to acquire a permit without blocking
	// Returns true if acquired, false otherwise
	tryAcquireFn := func(vmPtr interface{}, recv Value) Value {
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
	}
	s.AddMethod0(vm.Selectors, "tryAcquire", tryAcquireFn)
	s.AddMethod0(vm.Selectors, "primTryAcquire", tryAcquireFn)

	// Semaphore>>available - number of available permits
	availableFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		sem := v.getSemaphore(recv)
		if sem == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(len(sem.permits)))
	}
	s.AddMethod0(vm.Selectors, "available", availableFn)
	s.AddMethod0(vm.Selectors, "primAvailable", availableFn)

	// Semaphore>>capacity - total capacity of the semaphore
	capacityFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		sem := v.getSemaphore(recv)
		if sem == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(sem.capacity))
	}
	s.AddMethod0(vm.Selectors, "capacity", capacityFn)
	s.AddMethod0(vm.Selectors, "primCapacity", capacityFn)

	// Semaphore>>critical: aBlock - execute block while holding a permit
	// Automatically releases permit when done
	semCriticalFn := func(vmPtr interface{}, recv Value, block Value) Value {
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
	}
	s.AddMethod1(vm.Selectors, "critical:", semCriticalFn)
	s.AddMethod1(vm.Selectors, "primCritical:", semCriticalFn)

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
