package vm

// ---------------------------------------------------------------------------
// Float Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerFloatPrimitives() {
	c := vm.FloatClass

	c.AddMethod1(vm.Selectors, "+", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsFloat() {
			return FromFloat64(recv.Float64() + arg.Float64())
		}
		if arg.IsSmallInt() {
			return FromFloat64(recv.Float64() + float64(arg.SmallInt()))
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "-", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsFloat() {
			return FromFloat64(recv.Float64() - arg.Float64())
		}
		if arg.IsSmallInt() {
			return FromFloat64(recv.Float64() - float64(arg.SmallInt()))
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "*", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsFloat() {
			return FromFloat64(recv.Float64() * arg.Float64())
		}
		if arg.IsSmallInt() {
			return FromFloat64(recv.Float64() * float64(arg.SmallInt()))
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "/", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsFloat() {
			return FromFloat64(recv.Float64() / arg.Float64())
		}
		if arg.IsSmallInt() {
			return FromFloat64(recv.Float64() / float64(arg.SmallInt()))
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "<", func(_ interface{}, recv Value, arg Value) Value {
		var other float64
		if arg.IsFloat() {
			other = arg.Float64()
		} else if arg.IsSmallInt() {
			other = float64(arg.SmallInt())
		} else {
			return Nil
		}
		if recv.Float64() < other {
			return True
		}
		return False
	})

	c.AddMethod1(vm.Selectors, ">", func(_ interface{}, recv Value, arg Value) Value {
		var other float64
		if arg.IsFloat() {
			other = arg.Float64()
		} else if arg.IsSmallInt() {
			other = float64(arg.SmallInt())
		} else {
			return Nil
		}
		if recv.Float64() > other {
			return True
		}
		return False
	})

	c.AddMethod0(vm.Selectors, "negated", func(_ interface{}, recv Value) Value {
		return FromFloat64(-recv.Float64())
	})

	c.AddMethod0(vm.Selectors, "abs", func(_ interface{}, recv Value) Value {
		f := recv.Float64()
		if f < 0 {
			return FromFloat64(-f)
		}
		return recv
	})

	c.AddMethod0(vm.Selectors, "truncated", func(_ interface{}, recv Value) Value {
		return FromSmallInt(int64(recv.Float64()))
	})

	c.AddMethod0(vm.Selectors, "rounded", func(_ interface{}, recv Value) Value {
		f := recv.Float64()
		if f >= 0 {
			return FromSmallInt(int64(f + 0.5))
		}
		return FromSmallInt(int64(f - 0.5))
	})
}
