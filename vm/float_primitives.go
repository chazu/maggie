package vm

import (
	"fmt"
	"math"
)

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

	c.AddMethod0(vm.Selectors, "primTruncated", func(_ interface{}, recv Value) Value {
		return FromSmallInt(int64(recv.Float64()))
	})

	c.AddMethod0(vm.Selectors, "rounded", func(_ interface{}, recv Value) Value {
		f := recv.Float64()
		if f >= 0 {
			return FromSmallInt(int64(f + 0.5))
		}
		return FromSmallInt(int64(f - 0.5))
	})

	c.AddMethod0(vm.Selectors, "primRounded", func(_ interface{}, recv Value) Value {
		f := recv.Float64()
		if f >= 0 {
			return FromSmallInt(int64(f + 0.5))
		}
		return FromSmallInt(int64(f - 0.5))
	})

	c.AddMethod0(vm.Selectors, "floor", func(_ interface{}, recv Value) Value {
		return FromSmallInt(int64(math.Floor(recv.Float64())))
	})

	c.AddMethod0(vm.Selectors, "primFloor", func(_ interface{}, recv Value) Value {
		return FromSmallInt(int64(math.Floor(recv.Float64())))
	})

	c.AddMethod0(vm.Selectors, "ceiling", func(_ interface{}, recv Value) Value {
		return FromSmallInt(int64(math.Ceil(recv.Float64())))
	})

	c.AddMethod0(vm.Selectors, "primCeiling", func(_ interface{}, recv Value) Value {
		return FromSmallInt(int64(math.Ceil(recv.Float64())))
	})

	c.AddMethod0(vm.Selectors, "printString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		str := fmt.Sprintf("%g", recv.Float64())
		return v.registry.NewStringValue(str)
	})

	c.AddMethod0(vm.Selectors, "primPrintString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		str := fmt.Sprintf("%g", recv.Float64())
		return v.registry.NewStringValue(str)
	})

	c.AddMethod0(vm.Selectors, "sqrt", func(_ interface{}, recv Value) Value {
		return FromFloat64(math.Sqrt(recv.Float64()))
	})
}
