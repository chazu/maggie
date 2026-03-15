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

	// --- Trigonometric ---

	c.AddMethod0(vm.Selectors, "sin", func(_ interface{}, recv Value) Value {
		return FromFloat64(math.Sin(recv.Float64()))
	})

	c.AddMethod0(vm.Selectors, "cos", func(_ interface{}, recv Value) Value {
		return FromFloat64(math.Cos(recv.Float64()))
	})

	c.AddMethod0(vm.Selectors, "tan", func(_ interface{}, recv Value) Value {
		return FromFloat64(math.Tan(recv.Float64()))
	})

	c.AddMethod0(vm.Selectors, "asin", func(_ interface{}, recv Value) Value {
		return FromFloat64(math.Asin(recv.Float64()))
	})

	c.AddMethod0(vm.Selectors, "acos", func(_ interface{}, recv Value) Value {
		return FromFloat64(math.Acos(recv.Float64()))
	})

	c.AddMethod0(vm.Selectors, "atan", func(_ interface{}, recv Value) Value {
		return FromFloat64(math.Atan(recv.Float64()))
	})

	c.AddMethod1(vm.Selectors, "atan2:", func(_ interface{}, recv Value, arg Value) Value {
		var x float64
		if arg.IsFloat() {
			x = arg.Float64()
		} else if arg.IsSmallInt() {
			x = float64(arg.SmallInt())
		} else {
			return Nil
		}
		return FromFloat64(math.Atan2(recv.Float64(), x))
	})

	// --- Logarithmic / Exponential ---

	c.AddMethod0(vm.Selectors, "ln", func(_ interface{}, recv Value) Value {
		return FromFloat64(math.Log(recv.Float64()))
	})

	c.AddMethod0(vm.Selectors, "log10", func(_ interface{}, recv Value) Value {
		return FromFloat64(math.Log10(recv.Float64()))
	})

	c.AddMethod1(vm.Selectors, "log:", func(_ interface{}, recv Value, arg Value) Value {
		var base float64
		if arg.IsFloat() {
			base = arg.Float64()
		} else if arg.IsSmallInt() {
			base = float64(arg.SmallInt())
		} else {
			return Nil
		}
		return FromFloat64(math.Log(recv.Float64()) / math.Log(base))
	})

	c.AddMethod0(vm.Selectors, "exp", func(_ interface{}, recv Value) Value {
		return FromFloat64(math.Exp(recv.Float64()))
	})

	c.AddMethod1(vm.Selectors, "pow:", func(_ interface{}, recv Value, arg Value) Value {
		var exp float64
		if arg.IsFloat() {
			exp = arg.Float64()
		} else if arg.IsSmallInt() {
			exp = float64(arg.SmallInt())
		} else {
			return Nil
		}
		return FromFloat64(math.Pow(recv.Float64(), exp))
	})

	// --- Class methods: constants ---

	c.AddClassMethod0(vm.Selectors, "pi", func(_ interface{}, recv Value) Value {
		return FromFloat64(math.Pi)
	})

	c.AddClassMethod0(vm.Selectors, "e", func(_ interface{}, recv Value) Value {
		return FromFloat64(math.E)
	})

	c.AddClassMethod0(vm.Selectors, "infinity", func(_ interface{}, recv Value) Value {
		return FromFloat64(math.Inf(1))
	})

	c.AddClassMethod0(vm.Selectors, "nan", func(_ interface{}, recv Value) Value {
		return FromFloat64(math.NaN())
	})
}
