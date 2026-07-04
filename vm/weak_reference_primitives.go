package vm

// ---------------------------------------------------------------------------
// WeakReference Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerWeakReferencePrimitives() {
	c := vm.WeakReferenceClass

	// WeakReference class>>on: anObject
	// Create a new weak reference to the given object
	c.AddClassMethod1(vm.Selectors, "on:", func(v *VM, recv Value, target Value) Value {
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
	c.AddMethod0(vm.Selectors, "get", func(v *VM, recv Value) Value {
		wr := weakRefFromValue(recv)
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
	c.AddMethod0(vm.Selectors, "isAlive", func(v *VM, recv Value) Value {
		wr := weakRefFromValue(recv)
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
	c.AddMethod1(vm.Selectors, "onFinalize:", func(v *VM, recv Value, block Value) Value {
		wr := weakRefFromValue(recv)
		if wr == nil {
			return recv
		}
		// Set up the finalizer to evaluate the block. It runs on a Go GC cleanup
		// goroutine, so guard against a shutting-down VM with recover.
		if block.IsBlock() {
			wr.SetFinalizer(func(oldValue Value) {
				defer func() { _ = recover() }()
				v.evaluateBlock(block, []Value{oldValue})
			})
		}
		return recv
	})

	// WeakReference>>clear
	// Explicitly clear the weak reference (makes get return nil)
	c.AddMethod0(vm.Selectors, "clear", func(v *VM, recv Value) Value {
		if wr := weakRefFromValue(recv); wr != nil {
			wr.Clear()
		}
		return recv
	})

	// WeakReference>>printString
	c.AddMethod0(vm.Selectors, "printString", func(v *VM, recv Value) Value {
		wr := weakRefFromValue(recv)
		if wr == nil {
			return v.registry.NewStringValue("a WeakReference (invalid)")
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
