package vm

import "strconv"

// ---------------------------------------------------------------------------
// SmallInteger Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerSmallIntegerPrimitives() {
	c := vm.SmallIntegerClass

	// Arithmetic
	c.AddMethod1(vm.Selectors, "+", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromSmallInt(recv.SmallInt() + arg.SmallInt())
		}
		if arg.IsFloat() {
			return FromFloat64(float64(recv.SmallInt()) + arg.Float64())
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "-", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromSmallInt(recv.SmallInt() - arg.SmallInt())
		}
		if arg.IsFloat() {
			return FromFloat64(float64(recv.SmallInt()) - arg.Float64())
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "*", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromSmallInt(recv.SmallInt() * arg.SmallInt())
		}
		if arg.IsFloat() {
			return FromFloat64(float64(recv.SmallInt()) * arg.Float64())
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "/", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			if arg.SmallInt() == 0 {
				return Nil // Division by zero
			}
			return FromSmallInt(recv.SmallInt() / arg.SmallInt())
		}
		if arg.IsFloat() {
			return FromFloat64(float64(recv.SmallInt()) / arg.Float64())
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "\\\\", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			if arg.SmallInt() == 0 {
				return Nil
			}
			return FromSmallInt(recv.SmallInt() % arg.SmallInt())
		}
		return Nil
	})

	// // - truncated integer division (floor division)
	c.AddMethod1(vm.Selectors, "//", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			if arg.SmallInt() == 0 {
				return Nil
			}
			// Smalltalk // is floor division
			a, b := recv.SmallInt(), arg.SmallInt()
			result := a / b
			// Adjust for floor behavior with negative numbers
			if (a < 0) != (b < 0) && a%b != 0 {
				result--
			}
			return FromSmallInt(result)
		}
		return Nil
	})

	// Comparisons
	c.AddMethod1(vm.Selectors, "<", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			if recv.SmallInt() < arg.SmallInt() {
				return True
			}
			return False
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, ">", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			if recv.SmallInt() > arg.SmallInt() {
				return True
			}
			return False
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "<=", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			if recv.SmallInt() <= arg.SmallInt() {
				return True
			}
			return False
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, ">=", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			if recv.SmallInt() >= arg.SmallInt() {
				return True
			}
			return False
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "=", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			if recv.SmallInt() == arg.SmallInt() {
				return True
			}
			return False
		}
		return Nil
	})

	// Bit operations
	c.AddMethod1(vm.Selectors, "bitAnd:", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromSmallInt(recv.SmallInt() & arg.SmallInt())
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "bitOr:", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromSmallInt(recv.SmallInt() | arg.SmallInt())
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "bitXor:", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromSmallInt(recv.SmallInt() ^ arg.SmallInt())
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "bitShift:", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			shift := arg.SmallInt()
			if shift >= 0 {
				return FromSmallInt(recv.SmallInt() << uint(shift))
			}
			return FromSmallInt(recv.SmallInt() >> uint(-shift))
		}
		return Nil
	})

	c.AddMethod0(vm.Selectors, "negated", func(_ interface{}, recv Value) Value {
		return FromSmallInt(-recv.SmallInt())
	})

	c.AddMethod0(vm.Selectors, "abs", func(_ interface{}, recv Value) Value {
		n := recv.SmallInt()
		if n < 0 {
			return FromSmallInt(-n)
		}
		return recv
	})

	// Printing
	c.AddMethod0(vm.Selectors, "primPrintString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		return v.registry.NewStringValue(strconv.FormatInt(recv.SmallInt(), 10))
	})

	// Iteration
	c.AddMethod1(vm.Selectors, "timesRepeat:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		n := recv.SmallInt()
		for i := int64(0); i < n; i++ {
			v.evaluateBlock(block, nil)
		}
		return recv
	})

	c.AddMethod2(vm.Selectors, "to:do:", func(vmPtr interface{}, recv Value, stop Value, block Value) Value {
		v := vmPtr.(*VM)
		start := recv.SmallInt()
		end := stop.SmallInt()
		for i := start; i <= end; i++ {
			v.evaluateBlock(block, []Value{FromSmallInt(i)})
		}
		return recv
	})

	c.AddMethod3(vm.Selectors, "to:by:do:", func(vmPtr interface{}, recv Value, stop Value, step Value, block Value) Value {
		v := vmPtr.(*VM)
		start := recv.SmallInt()
		end := stop.SmallInt()
		stepVal := step.SmallInt()
		if stepVal > 0 {
			for i := start; i <= end; i += stepVal {
				v.evaluateBlock(block, []Value{FromSmallInt(i)})
			}
		} else if stepVal < 0 {
			for i := start; i >= end; i += stepVal {
				v.evaluateBlock(block, []Value{FromSmallInt(i)})
			}
		}
		return recv
	})
}
