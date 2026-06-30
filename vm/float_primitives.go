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

	c.AddMethod1(vm.Selectors, "+", func(v *VM, recv Value, arg Value) Value {
		if other, ok := numericArgAsFloat(v, arg); ok {
			return FromFloat64(recv.Float64() + other)
		}
		return v.SignalPrimitiveError("+", "argument must be a number")
	})

	c.AddMethod1(vm.Selectors, "-", func(v *VM, recv Value, arg Value) Value {
		if other, ok := numericArgAsFloat(v, arg); ok {
			return FromFloat64(recv.Float64() - other)
		}
		return v.SignalPrimitiveError("-", "argument must be a number")
	})

	c.AddMethod1(vm.Selectors, "*", func(v *VM, recv Value, arg Value) Value {
		if other, ok := numericArgAsFloat(v, arg); ok {
			return FromFloat64(recv.Float64() * other)
		}
		return v.SignalPrimitiveError("*", "argument must be a number")
	})

	c.AddMethod1(vm.Selectors, "/", func(v *VM, recv Value, arg Value) Value {
		if other, ok := numericArgAsFloat(v, arg); ok {
			return FromFloat64(recv.Float64() / other)
		}
		return v.SignalPrimitiveError("/", "argument must be a number")
	})

	c.AddMethod1(vm.Selectors, "<", func(v *VM, recv Value, arg Value) Value {
		other, ok := numericArgAsFloat(v, arg)
		if !ok {
			return v.SignalPrimitiveError("<", "argument must be a number")
		}
		return FromBool(recv.Float64() < other)
	})

	c.AddMethod1(vm.Selectors, ">", func(v *VM, recv Value, arg Value) Value {
		other, ok := numericArgAsFloat(v, arg)
		if !ok {
			return v.SignalPrimitiveError(">", "argument must be a number")
		}
		return FromBool(recv.Float64() > other)
	})

	c.AddMethod1(vm.Selectors, "<=", func(v *VM, recv Value, arg Value) Value {
		other, ok := numericArgAsFloat(v, arg)
		if !ok {
			return v.SignalPrimitiveError("<=", "argument must be a number")
		}
		return FromBool(recv.Float64() <= other)
	})

	c.AddMethod1(vm.Selectors, ">=", func(v *VM, recv Value, arg Value) Value {
		other, ok := numericArgAsFloat(v, arg)
		if !ok {
			return v.SignalPrimitiveError(">=", "argument must be a number")
		}
		return FromBool(recv.Float64() >= other)
	})

	c.AddMethod0(vm.Selectors, "negated", func(_ *VM, recv Value) Value {
		return FromFloat64(-recv.Float64())
	})

	c.AddMethod0(vm.Selectors, "abs", func(_ *VM, recv Value) Value {
		f := recv.Float64()
		if f < 0 {
			return FromFloat64(-f)
		}
		return recv
	})

	c.AddMethod0(vm.Selectors, "truncated", func(_ *VM, recv Value) Value {
		return FromSmallInt(int64(recv.Float64()))
	})

	c.AddMethod0(vm.Selectors, "primTruncated", func(_ *VM, recv Value) Value {
		return FromSmallInt(int64(recv.Float64()))
	})

	c.AddMethod0(vm.Selectors, "rounded", func(_ *VM, recv Value) Value {
		f := recv.Float64()
		if f >= 0 {
			return FromSmallInt(int64(f + 0.5))
		}
		return FromSmallInt(int64(f - 0.5))
	})

	c.AddMethod0(vm.Selectors, "primRounded", func(_ *VM, recv Value) Value {
		f := recv.Float64()
		if f >= 0 {
			return FromSmallInt(int64(f + 0.5))
		}
		return FromSmallInt(int64(f - 0.5))
	})

	c.AddMethod0(vm.Selectors, "floor", func(_ *VM, recv Value) Value {
		return FromSmallInt(int64(math.Floor(recv.Float64())))
	})

	c.AddMethod0(vm.Selectors, "primFloor", func(_ *VM, recv Value) Value {
		return FromSmallInt(int64(math.Floor(recv.Float64())))
	})

	c.AddMethod0(vm.Selectors, "ceiling", func(_ *VM, recv Value) Value {
		return FromSmallInt(int64(math.Ceil(recv.Float64())))
	})

	c.AddMethod0(vm.Selectors, "primCeiling", func(_ *VM, recv Value) Value {
		return FromSmallInt(int64(math.Ceil(recv.Float64())))
	})

	c.AddMethod0(vm.Selectors, "printString", func(v *VM, recv Value) Value {
		str := fmt.Sprintf("%g", recv.Float64())
		return v.registry.NewStringValue(str)
	})

	c.AddMethod0(vm.Selectors, "primPrintString", func(v *VM, recv Value) Value {
		str := fmt.Sprintf("%g", recv.Float64())
		return v.registry.NewStringValue(str)
	})

	c.AddMethod0(vm.Selectors, "sqrt", func(_ *VM, recv Value) Value {
		return FromFloat64(math.Sqrt(recv.Float64()))
	})

	// --- Trigonometric ---

	c.AddMethod0(vm.Selectors, "sin", func(_ *VM, recv Value) Value {
		return FromFloat64(math.Sin(recv.Float64()))
	})

	c.AddMethod0(vm.Selectors, "cos", func(_ *VM, recv Value) Value {
		return FromFloat64(math.Cos(recv.Float64()))
	})

	c.AddMethod0(vm.Selectors, "tan", func(_ *VM, recv Value) Value {
		return FromFloat64(math.Tan(recv.Float64()))
	})

	c.AddMethod0(vm.Selectors, "asin", func(_ *VM, recv Value) Value {
		return FromFloat64(math.Asin(recv.Float64()))
	})

	c.AddMethod0(vm.Selectors, "acos", func(_ *VM, recv Value) Value {
		return FromFloat64(math.Acos(recv.Float64()))
	})

	c.AddMethod0(vm.Selectors, "atan", func(_ *VM, recv Value) Value {
		return FromFloat64(math.Atan(recv.Float64()))
	})

	c.AddMethod1(vm.Selectors, "atan2:", func(v *VM, recv Value, arg Value) Value {
		var x float64
		if arg.IsFloat() {
			x = arg.Float64()
		} else if arg.IsSmallInt() {
			x = float64(arg.SmallInt())
		} else {
			return v.SignalPrimitiveError("atan2:", "argument must be a number")
		}
		return FromFloat64(math.Atan2(recv.Float64(), x))
	})

	// --- Logarithmic / Exponential ---

	c.AddMethod0(vm.Selectors, "ln", func(_ *VM, recv Value) Value {
		return FromFloat64(math.Log(recv.Float64()))
	})

	c.AddMethod0(vm.Selectors, "log10", func(_ *VM, recv Value) Value {
		return FromFloat64(math.Log10(recv.Float64()))
	})

	c.AddMethod1(vm.Selectors, "log:", func(v *VM, recv Value, arg Value) Value {
		var base float64
		if arg.IsFloat() {
			base = arg.Float64()
		} else if arg.IsSmallInt() {
			base = float64(arg.SmallInt())
		} else {
			return v.SignalPrimitiveError("log:", "argument must be a number")
		}
		return FromFloat64(math.Log(recv.Float64()) / math.Log(base))
	})

	c.AddMethod0(vm.Selectors, "exp", func(_ *VM, recv Value) Value {
		return FromFloat64(math.Exp(recv.Float64()))
	})

	c.AddMethod1(vm.Selectors, "pow:", func(v *VM, recv Value, arg Value) Value {
		var exp float64
		if arg.IsFloat() {
			exp = arg.Float64()
		} else if arg.IsSmallInt() {
			exp = float64(arg.SmallInt())
		} else {
			return v.SignalPrimitiveError("pow:", "argument must be a number")
		}
		return FromFloat64(math.Pow(recv.Float64(), exp))
	})

	// --- Class methods: constants ---

	c.AddClassMethod0(vm.Selectors, "pi", func(_ *VM, recv Value) Value {
		return FromFloat64(math.Pi)
	})

	c.AddClassMethod0(vm.Selectors, "e", func(_ *VM, recv Value) Value {
		return FromFloat64(math.E)
	})

	c.AddClassMethod0(vm.Selectors, "infinity", func(_ *VM, recv Value) Value {
		return FromFloat64(math.Inf(1))
	})

	c.AddClassMethod0(vm.Selectors, "nan", func(_ *VM, recv Value) Value {
		return FromFloat64(math.NaN())
	})
}
