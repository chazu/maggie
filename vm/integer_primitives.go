package vm

import (
	"math/big"
	"strconv"
)

// ---------------------------------------------------------------------------
// SmallInteger Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerSmallIntegerPrimitives() {
	c := vm.SmallIntegerClass

	// Arithmetic
	c.AddMethod1(vm.Selectors, "+", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		if arg.IsSmallInt() {
			result := recv.SmallInt() + arg.SmallInt()
			if val, ok := TryFromSmallInt(result); ok {
				return val
			}
			return v.registry.NewBigIntValue(new(big.Int).Add(big.NewInt(recv.SmallInt()), big.NewInt(arg.SmallInt())))
		}
		if IsBigIntValue(arg) {
			a := big.NewInt(recv.SmallInt())
			b := getBigIntOperand(v, arg)
			if b != nil {
				return v.registry.NewBigIntValue(new(big.Int).Add(a, b))
			}
		}
		if arg.IsFloat() {
			return FromFloat64(float64(recv.SmallInt()) + arg.Float64())
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "-", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		if arg.IsSmallInt() {
			result := recv.SmallInt() - arg.SmallInt()
			if val, ok := TryFromSmallInt(result); ok {
				return val
			}
			return v.registry.NewBigIntValue(new(big.Int).Sub(big.NewInt(recv.SmallInt()), big.NewInt(arg.SmallInt())))
		}
		if IsBigIntValue(arg) {
			a := big.NewInt(recv.SmallInt())
			b := getBigIntOperand(v, arg)
			if b != nil {
				return v.registry.NewBigIntValue(new(big.Int).Sub(a, b))
			}
		}
		if arg.IsFloat() {
			return FromFloat64(float64(recv.SmallInt()) - arg.Float64())
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "*", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		if arg.IsSmallInt() {
			result := new(big.Int).Mul(big.NewInt(recv.SmallInt()), big.NewInt(arg.SmallInt()))
			return v.registry.NewBigIntValue(result) // auto-demotes if fits
		}
		if IsBigIntValue(arg) {
			a := big.NewInt(recv.SmallInt())
			b := getBigIntOperand(v, arg)
			if b != nil {
				return v.registry.NewBigIntValue(new(big.Int).Mul(a, b))
			}
		}
		if arg.IsFloat() {
			return FromFloat64(float64(recv.SmallInt()) * arg.Float64())
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "/", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		if arg.IsSmallInt() {
			if arg.SmallInt() == 0 {
				return Nil // Division by zero
			}
			return FromSmallInt(recv.SmallInt() / arg.SmallInt())
		}
		if IsBigIntValue(arg) {
			a := big.NewInt(recv.SmallInt())
			b := getBigIntOperand(v, arg)
			if b != nil && b.Sign() != 0 {
				return v.registry.NewBigIntValue(new(big.Int).Quo(a, b))
			}
			return Nil
		}
		if arg.IsFloat() {
			return FromFloat64(float64(recv.SmallInt()) / arg.Float64())
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "\\\\", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		if arg.IsSmallInt() {
			if arg.SmallInt() == 0 {
				return Nil
			}
			return FromSmallInt(recv.SmallInt() % arg.SmallInt())
		}
		if IsBigIntValue(arg) {
			a := big.NewInt(recv.SmallInt())
			b := getBigIntOperand(v, arg)
			if b != nil && b.Sign() != 0 {
				return v.registry.NewBigIntValue(new(big.Int).Rem(a, b))
			}
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
	c.AddMethod1(vm.Selectors, "<", func(vmPtr interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromBool(recv.SmallInt() < arg.SmallInt())
		}
		if IsBigIntValue(arg) {
			v := vmPtr.(*VM)
			a := big.NewInt(recv.SmallInt())
			b := getBigIntOperand(v, arg)
			if b != nil {
				return FromBool(a.Cmp(b) < 0)
			}
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, ">", func(vmPtr interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromBool(recv.SmallInt() > arg.SmallInt())
		}
		if IsBigIntValue(arg) {
			v := vmPtr.(*VM)
			a := big.NewInt(recv.SmallInt())
			b := getBigIntOperand(v, arg)
			if b != nil {
				return FromBool(a.Cmp(b) > 0)
			}
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "<=", func(vmPtr interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromBool(recv.SmallInt() <= arg.SmallInt())
		}
		if IsBigIntValue(arg) {
			v := vmPtr.(*VM)
			a := big.NewInt(recv.SmallInt())
			b := getBigIntOperand(v, arg)
			if b != nil {
				return FromBool(a.Cmp(b) <= 0)
			}
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, ">=", func(vmPtr interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromBool(recv.SmallInt() >= arg.SmallInt())
		}
		if IsBigIntValue(arg) {
			v := vmPtr.(*VM)
			a := big.NewInt(recv.SmallInt())
			b := getBigIntOperand(v, arg)
			if b != nil {
				return FromBool(a.Cmp(b) >= 0)
			}
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "=", func(vmPtr interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromBool(recv.SmallInt() == arg.SmallInt())
		}
		if IsBigIntValue(arg) {
			v := vmPtr.(*VM)
			a := big.NewInt(recv.SmallInt())
			b := getBigIntOperand(v, arg)
			if b != nil {
				return FromBool(a.Cmp(b) == 0)
			}
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

	c.AddMethod1(vm.Selectors, "bitShift:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		if arg.IsSmallInt() {
			shift := arg.SmallInt()
			if shift >= 0 {
				// Use big.Int for left shifts to avoid overflow
				result := new(big.Int).Lsh(big.NewInt(recv.SmallInt()), uint(shift))
				return v.registry.NewBigIntValue(result)
			}
			return FromSmallInt(recv.SmallInt() >> uint(-shift))
		}
		return Nil
	})

	c.AddMethod0(vm.Selectors, "negated", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		n := recv.SmallInt()
		result := -n
		if val, ok := TryFromSmallInt(result); ok {
			return val
		}
		return v.registry.NewBigIntValue(new(big.Int).Neg(big.NewInt(n)))
	})

	c.AddMethod0(vm.Selectors, "abs", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		n := recv.SmallInt()
		if n < 0 {
			result := -n
			if val, ok := TryFromSmallInt(result); ok {
				return val
			}
			return v.registry.NewBigIntValue(new(big.Int).Abs(big.NewInt(n)))
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
