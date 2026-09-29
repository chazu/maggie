package vm

// Block primitives for the Maggie VM

// ---------------------------------------------------------------------------
// Block Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerBlockPrimitives() {
	c := vm.BlockClass

	// Primitive evaluation methods (called by Block.mag's value, value:, etc.)
	c.AddMethod0(vm.Selectors, "primValue", func(v *VM, recv Value) Value {
		return v.evaluateBlock(recv, nil)
	})

	c.AddMethod1(vm.Selectors, "primValue:", func(v *VM, recv Value, arg Value) Value {
		return v.evaluateBlock(recv, []Value{arg})
	})

	c.AddMethod2(vm.Selectors, "primValue:value:", func(v *VM, recv Value, arg1, arg2 Value) Value {
		return v.evaluateBlock(recv, []Value{arg1, arg2})
	})

	c.AddMethod3(vm.Selectors, "primValue:value:value:", func(v *VM, recv Value, arg1, arg2, arg3 Value) Value {
		return v.evaluateBlock(recv, []Value{arg1, arg2, arg3})
	})

	// Direct evaluation methods (for Go code calling blocks directly)
	c.AddMethod0(vm.Selectors, "value", func(v *VM, recv Value) Value {
		return v.evaluateBlock(recv, nil)
	})

	c.AddMethod1(vm.Selectors, "value:", func(v *VM, recv Value, arg Value) Value {
		return v.evaluateBlock(recv, []Value{arg})
	})

	c.AddMethod2(vm.Selectors, "value:value:", func(v *VM, recv Value, arg1, arg2 Value) Value {
		return v.evaluateBlock(recv, []Value{arg1, arg2})
	})

	c.AddMethod3(vm.Selectors, "value:value:value:", func(v *VM, recv Value, arg1, arg2, arg3 Value) Value {
		return v.evaluateBlock(recv, []Value{arg1, arg2, arg3})
	})

	c.AddMethod0(vm.Selectors, "whileTrue", func(v *VM, recv Value) Value {
		for {
			result := v.evaluateBlock(recv, nil)
			if result != True {
				break
			}
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "whileTrue:", func(v *VM, recv Value, body Value) Value {
		for {
			cond := v.evaluateBlock(recv, nil)
			if cond != True {
				break
			}
			v.evaluateBlock(body, nil)
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "whileFalse:", func(v *VM, recv Value, body Value) Value {
		for {
			cond := v.evaluateBlock(recv, nil)
			if cond == True {
				break
			}
			v.evaluateBlock(body, nil)
		}
		return Nil
	})

	c.AddMethod0(vm.Selectors, "whileFalse", func(v *VM, recv Value) Value {
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
	// Get block from registry using the current interpreter for this goroutine
	interp := vm.currentInterpreter()
	if interp == nil {
		return Nil
	}
	bv := interp.getBlockValue(blockVal)
	if bv == nil {
		return Nil
	}
	result := interp.ExecuteBlock(bv.Block, bv.Captures, args, bv.HomeFrame, bv.HomeSelf, bv.HomeMethod)
	return result
}

// valueOf evaluates a "valuable" with args. A real block runs through
// evaluateBlock, so a non-local return (^) inside it unwinds to its home
// method; sending value:… instead dispatches to the lib's compiled
// Block>>value:… wrappers, whose Execute panics on that foreign unwind and
// kills the program. Any other object is sent value/value:/value:value:.
func (vm *VM) valueOf(block Value, args []Value) Value {
	if block.IsBlock() {
		return vm.evaluateBlock(block, args)
	}
	selector := "value"
	switch len(args) {
	case 1:
		selector = "value:"
	case 2:
		selector = "value:value:"
	}
	return vm.Send(block, selector, args)
}
