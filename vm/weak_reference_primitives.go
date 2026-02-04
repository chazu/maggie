package vm

// ---------------------------------------------------------------------------
// WeakReference Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerWeakReferencePrimitives() {
	c := vm.WeakReferenceClass

	// WeakReference class>>on: anObject
	// Create a new weak reference to the given object
	c.AddClassMethod1(vm.Selectors, "on:", func(vmPtr interface{}, recv Value, target Value) Value {
		v := vmPtr.(*VM)
		if !target.IsObject() {
			// Can only create weak refs to objects
			return Nil
		}
		obj := ObjectFromValueRaw(target) // Don't follow forwarding for the target
		if obj == nil {
			return Nil
		}
		wr := v.NewWeakRef(obj)
		return FromWeakRef(wr)
	})

	// WeakReference>>get
	// Return the referenced object, or nil if it has been collected
	c.AddMethod0(vm.Selectors, "get", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if !recv.IsWeakRef() {
			return Nil
		}
		wr := v.LookupWeakRef(recv.WeakRefID())
		if wr == nil {
			return Nil
		}
		target := wr.Get()
		if target == nil {
			return Nil
		}
		return target.ToValue()
	})

	// WeakReference>>isAlive
	// Return true if the referenced object has not been collected
	c.AddMethod0(vm.Selectors, "isAlive", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if !recv.IsWeakRef() {
			return False
		}
		wr := v.LookupWeakRef(recv.WeakRefID())
		if wr == nil {
			return False
		}
		if wr.IsAlive() {
			return True
		}
		return False
	})

	// WeakReference>>onFinalize:
	// Set a block to be evaluated when the referenced object is collected
	c.AddMethod1(vm.Selectors, "onFinalize:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		if !recv.IsWeakRef() {
			return recv
		}
		wr := v.LookupWeakRef(recv.WeakRefID())
		if wr == nil {
			return recv
		}
		// Set up the finalizer to evaluate the block
		if block.IsBlock() {
			wr.SetFinalizer(func(oldValue Value) {
				// Evaluate the block with the old value (for informational purposes)
				// Note: the object is already collected, so the value is just a reference
				v.evaluateBlock(block, []Value{oldValue})
			})
		}
		return recv
	})

	// WeakReference>>clear
	// Explicitly clear the weak reference (makes get return nil)
	c.AddMethod0(vm.Selectors, "clear", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if !recv.IsWeakRef() {
			return recv
		}
		wr := v.LookupWeakRef(recv.WeakRefID())
		if wr != nil {
			wr.Clear()
		}
		return recv
	})

	// WeakReference>>printString
	c.AddMethod0(vm.Selectors, "printString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if !recv.IsWeakRef() {
			return v.registry.NewStringValue("a WeakReference (invalid)")
		}
		wr := v.LookupWeakRef(recv.WeakRefID())
		if wr == nil {
			return v.registry.NewStringValue("a WeakReference (unregistered)")
		}
		if wr.IsAlive() {
			target := wr.Get()
			if target != nil {
				return v.registry.NewStringValue("a WeakReference to " + target.ClassName())
			}
		}
		return v.registry.NewStringValue("a WeakReference (collected)")
	})
}
