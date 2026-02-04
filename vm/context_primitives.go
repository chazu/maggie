package vm

// registerContextPrimitives registers primitive methods for the Context class.
// Context represents an execution context (activation record) and provides
// access to method/block, receiver, arguments, temporaries, and sender chain.
func (vm *VM) registerContextPrimitives() {
	c := vm.ContextClass

	// sender - returns the Context that called this one, or nil
	c.AddMethod0(vm.Selectors, "sender", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.registry.GetContextFromValue(recv)
		if ctx == nil || ctx.SenderID < 0 {
			return Nil
		}
		return FromContextID(uint32(ctx.SenderID))
	})

	// receiver - returns self in this context
	c.AddMethod0(vm.Selectors, "receiver", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.registry.GetContextFromValue(recv)
		if ctx == nil {
			return Nil
		}
		return ctx.Receiver
	})

	// method - returns the CompiledMethod for this context (nil for blocks)
	c.AddMethod0(vm.Selectors, "method", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.registry.GetContextFromValue(recv)
		if ctx == nil || ctx.Method == nil {
			return Nil
		}
		// Return the method as a value - for now just return nil
		// since CompiledMethod isn't directly representable as a Value
		// TODO: Consider making methods first-class values
		return Nil
	})

	// selector - returns the selector (method name) as a Symbol
	c.AddMethod0(vm.Selectors, "selector", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.registry.GetContextFromValue(recv)
		if ctx == nil {
			return Nil
		}
		if ctx.Method != nil {
			// Return method name as symbol
			return v.Symbols.SymbolValue(ctx.Method.Name())
		}
		// Blocks don't have selectors
		return Nil
	})

	// isBlockContext - returns true if this is a block context
	c.AddMethod0(vm.Selectors, "isBlockContext", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.registry.GetContextFromValue(recv)
		if ctx == nil {
			return False
		}
		return FromBool(ctx.IsBlockContext())
	})

	// arguments - returns an Array of arguments
	c.AddMethod0(vm.Selectors, "arguments", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.registry.GetContextFromValue(recv)
		if ctx == nil || ctx.Args == nil {
			return v.NewArray(0)
		}
		return v.NewArrayWithElements(ctx.Args)
	})

	// tempAt: - returns the temporary at the given index
	c.AddMethod1(vm.Selectors, "tempAt:", func(vmPtr interface{}, recv Value, idx Value) Value {
		v := vmPtr.(*VM)
		ctx := v.registry.GetContextFromValue(recv)
		if ctx == nil || !idx.IsSmallInt() {
			return Nil
		}
		i := int(idx.SmallInt())
		if i < 0 || i >= len(ctx.Temps) {
			return Nil
		}
		return ctx.Temps[i]
	})

	// tempAt:put: - stores a value in the temporary at the given index
	// Note: This modifies the captured snapshot, not the live execution
	c.AddMethod2(vm.Selectors, "tempAt:put:", func(vmPtr interface{}, recv Value, idx Value, val Value) Value {
		v := vmPtr.(*VM)
		ctx := v.registry.GetContextFromValue(recv)
		if ctx == nil || !idx.IsSmallInt() {
			return Nil
		}
		i := int(idx.SmallInt())
		if i < 0 || i >= len(ctx.Temps) {
			return Nil
		}
		ctx.Temps[i] = val
		return val
	})

	// numTemps - returns the number of temporaries
	c.AddMethod0(vm.Selectors, "numTemps", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.registry.GetContextFromValue(recv)
		if ctx == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(len(ctx.Temps)))
	})

	// numArgs - returns the number of arguments
	c.AddMethod0(vm.Selectors, "numArgs", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.registry.GetContextFromValue(recv)
		if ctx == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(len(ctx.Args)))
	})

	// pc (program counter) - returns the instruction pointer at capture time
	c.AddMethod0(vm.Selectors, "pc", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.registry.GetContextFromValue(recv)
		if ctx == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(ctx.IP))
	})

	// home - for block contexts, returns the home method context
	c.AddMethod0(vm.Selectors, "home", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.registry.GetContextFromValue(recv)
		if ctx == nil || ctx.HomeID < 0 {
			return Nil
		}
		return FromContextID(uint32(ctx.HomeID))
	})

	// printString - return a string representation
	c.AddMethod0(vm.Selectors, "printString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.registry.GetContextFromValue(recv)
		if ctx == nil {
			return v.registry.NewStringValue("a Context (invalid)")
		}
		if ctx.IsBlockContext() {
			return v.registry.NewStringValue("a BlockContext")
		}
		if ctx.Method != nil {
			className := "UnknownClass"
			if ctx.Method.Class() != nil {
				className = ctx.Method.Class().Name
			}
			return v.registry.NewStringValue("a MethodContext(" + className + ">>" + ctx.Method.Name() + ")")
		}
		return v.registry.NewStringValue("a Context")
	})
}
