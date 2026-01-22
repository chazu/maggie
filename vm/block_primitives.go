package vm

// ---------------------------------------------------------------------------
// Block Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerBlockPrimitives() {
	c := vm.BlockClass

	// Primitive evaluation methods (called by Block.mag's value, value:, etc.)
	c.AddMethod0(vm.Selectors, "primValue", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(recv, nil)
	})

	c.AddMethod1(vm.Selectors, "primValue:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(recv, []Value{arg})
	})

	c.AddMethod2(vm.Selectors, "primValue:value:", func(vmPtr interface{}, recv Value, arg1, arg2 Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(recv, []Value{arg1, arg2})
	})

	c.AddMethod3(vm.Selectors, "primValue:value:value:", func(vmPtr interface{}, recv Value, arg1, arg2, arg3 Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(recv, []Value{arg1, arg2, arg3})
	})

	// Direct evaluation methods (for Go code calling blocks directly)
	c.AddMethod0(vm.Selectors, "value", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(recv, nil)
	})

	c.AddMethod1(vm.Selectors, "value:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(recv, []Value{arg})
	})

	c.AddMethod2(vm.Selectors, "value:value:", func(vmPtr interface{}, recv Value, arg1, arg2 Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(recv, []Value{arg1, arg2})
	})

	c.AddMethod3(vm.Selectors, "value:value:value:", func(vmPtr interface{}, recv Value, arg1, arg2, arg3 Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(recv, []Value{arg1, arg2, arg3})
	})

	c.AddMethod0(vm.Selectors, "whileTrue", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		for {
			result := v.evaluateBlock(recv, nil)
			if result != True {
				break
			}
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "whileTrue:", func(vmPtr interface{}, recv Value, body Value) Value {
		v := vmPtr.(*VM)
		for {
			cond := v.evaluateBlock(recv, nil)
			if cond != True {
				break
			}
			v.evaluateBlock(body, nil)
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "whileFalse:", func(vmPtr interface{}, recv Value, body Value) Value {
		v := vmPtr.(*VM)
		for {
			cond := v.evaluateBlock(recv, nil)
			if cond == True {
				break
			}
			v.evaluateBlock(body, nil)
		}
		return Nil
	})

	c.AddMethod0(vm.Selectors, "whileFalse", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		for {
			result := v.evaluateBlock(recv, nil)
			if result == True {
				break
			}
		}
		return Nil
	})
}

// ---------------------------------------------------------------------------
// Block evaluation helper
// ---------------------------------------------------------------------------

func (vm *VM) evaluateBlock(blockVal Value, args []Value) Value {
	// Get block from registry
	bv := vm.interpreter.getBlockValue(blockVal)
	if bv == nil {
		return Nil
	}
	return vm.interpreter.ExecuteBlock(bv.Block, bv.Captures, args, bv.HomeFrame, bv.HomeSelf)
}
