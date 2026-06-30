package vm

import (
	"math/big"
	"strconv"
)

// ---------------------------------------------------------------------------
// SmallInteger Primitives
// ---------------------------------------------------------------------------

// MaxBitShift bounds the left-shift amount bitShift: will perform. A SmallInt
// shift argument can be up to ~2^47, so `1 bitShift: hugeN` would ask big.Int
// to allocate that many bits and trigger a fatal, uncatchable Go OOM. ~64M bits
// (~8 MiB result) is well beyond any legitimate use while keeping the failure
// a catchable Maggie error rather than a process kill.
const MaxBitShift = 1 << 26

func (vm *VM) registerSmallIntegerPrimitives() {
	c := vm.SmallIntegerClass

	// Arithmetic
	c.AddMethod1(vm.Selectors, "+", func(v *VM, recv Value, arg Value) Value {
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
		return v.SignalPrimitiveError("+", "argument must be a number")
	})

	c.AddMethod1(vm.Selectors, "-", func(v *VM, recv Value, arg Value) Value {
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
		return v.SignalPrimitiveError("-", "argument must be a number")
	})

	c.AddMethod1(vm.Selectors, "*", func(v *VM, recv Value, arg Value) Value {
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
		return v.SignalPrimitiveError("*", "argument must be a number")
	})

	c.AddMethod1(vm.Selectors, "/", func(v *VM, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			if arg.SmallInt() == 0 {
				return v.SignalZeroDivide()
			}
			return FromSmallInt(recv.SmallInt() / arg.SmallInt())
		}
		if IsBigIntValue(arg) {
			a := big.NewInt(recv.SmallInt())
			b := getBigIntOperand(v, arg)
			if b != nil && b.Sign() != 0 {
				return v.registry.NewBigIntValue(new(big.Int).Quo(a, b))
			}
			return v.SignalZeroDivide()
		}
		if arg.IsFloat() {
			return FromFloat64(float64(recv.SmallInt()) / arg.Float64())
		}
		return v.SignalPrimitiveError("/", "argument must be a number")
	})

	c.AddMethod1(vm.Selectors, "\\\\", func(v *VM, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			if arg.SmallInt() == 0 {
				return v.SignalZeroDivide()
			}
			return FromSmallInt(recv.SmallInt() % arg.SmallInt())
		}
		if IsBigIntValue(arg) {
			a := big.NewInt(recv.SmallInt())
			b := getBigIntOperand(v, arg)
			if b != nil && b.Sign() != 0 {
				return v.registry.NewBigIntValue(new(big.Int).Rem(a, b))
			}
			return v.SignalZeroDivide()
		}
		return v.SignalPrimitiveError("\\\\", "argument must be a number")
	})

	// // - truncated integer division (floor division)
	c.AddMethod1(vm.Selectors, "//", func(v *VM, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			if arg.SmallInt() == 0 {
				return v.SignalZeroDivide()
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
		return v.SignalPrimitiveError("//", "argument must be a number")
	})

	// Comparisons
	c.AddMethod1(vm.Selectors, "<", func(v *VM, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromBool(recv.SmallInt() < arg.SmallInt())
		}
		if arg.IsFloat() {
			// Mixed SmallInteger/Float comparison coerces to Float — consistent
			// with SmallInteger arithmetic (7 + 2.5 → 9.5) and Float comparison;
			// previously this raised, so `7 < 2.5` errored instead of answering.
			return FromBool(float64(recv.SmallInt()) < arg.Float64())
		}
		if IsBigIntValue(arg) {
			a := big.NewInt(recv.SmallInt())
			b := getBigIntOperand(v, arg)
			if b != nil {
				return FromBool(a.Cmp(b) < 0)
			}
		}
		return v.SignalPrimitiveError("<", "argument must be a number")
	})

	c.AddMethod1(vm.Selectors, ">", func(v *VM, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromBool(recv.SmallInt() > arg.SmallInt())
		}
		if arg.IsFloat() {
			return FromBool(float64(recv.SmallInt()) > arg.Float64())
		}
		if IsBigIntValue(arg) {
			a := big.NewInt(recv.SmallInt())
			b := getBigIntOperand(v, arg)
			if b != nil {
				return FromBool(a.Cmp(b) > 0)
			}
		}
		return v.SignalPrimitiveError(">", "argument must be a number")
	})

	c.AddMethod1(vm.Selectors, "<=", func(v *VM, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromBool(recv.SmallInt() <= arg.SmallInt())
		}
		if arg.IsFloat() {
			return FromBool(float64(recv.SmallInt()) <= arg.Float64())
		}
		if IsBigIntValue(arg) {
			a := big.NewInt(recv.SmallInt())
			b := getBigIntOperand(v, arg)
			if b != nil {
				return FromBool(a.Cmp(b) <= 0)
			}
		}
		return v.SignalPrimitiveError("<=", "argument must be a number")
	})

	c.AddMethod1(vm.Selectors, ">=", func(v *VM, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromBool(recv.SmallInt() >= arg.SmallInt())
		}
		if arg.IsFloat() {
			return FromBool(float64(recv.SmallInt()) >= arg.Float64())
		}
		if IsBigIntValue(arg) {
			a := big.NewInt(recv.SmallInt())
			b := getBigIntOperand(v, arg)
			if b != nil {
				return FromBool(a.Cmp(b) >= 0)
			}
		}
		return v.SignalPrimitiveError(">=", "argument must be a number")
	})

	c.AddMethod1(vm.Selectors, "=", func(v *VM, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromBool(recv.SmallInt() == arg.SmallInt())
		}
		if IsBigIntValue(arg) {
			a := big.NewInt(recv.SmallInt())
			b := getBigIntOperand(v, arg)
			if b != nil {
				return FromBool(a.Cmp(b) == 0)
			}
		}
		return v.SignalPrimitiveError("=", "argument must be a number")
	})

	// Bit operations
	c.AddMethod1(vm.Selectors, "bitAnd:", func(v *VM, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromSmallInt(recv.SmallInt() & arg.SmallInt())
		}
		return v.SignalTypeError("bitAnd:", 1, "SmallInteger", arg)
	})

	c.AddMethod1(vm.Selectors, "bitOr:", func(v *VM, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromSmallInt(recv.SmallInt() | arg.SmallInt())
		}
		return v.SignalTypeError("bitOr:", 1, "SmallInteger", arg)
	})

	c.AddMethod1(vm.Selectors, "bitXor:", func(v *VM, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromSmallInt(recv.SmallInt() ^ arg.SmallInt())
		}
		return v.SignalTypeError("bitXor:", 1, "SmallInteger", arg)
	})

	c.AddMethod1(vm.Selectors, "bitShift:", func(v *VM, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			shift := arg.SmallInt()
			if shift >= 0 {
				if shift > MaxBitShift {
					return v.SignalPrimitiveError("bitShift:", "shift amount too large")
				}
				result := new(big.Int).Lsh(big.NewInt(recv.SmallInt()), uint(shift))
				return v.registry.NewBigIntValue(result)
			}
			return FromSmallInt(recv.SmallInt() >> uint(-shift))
		}
		return v.SignalTypeError("bitShift:", 1, "SmallInteger", arg)
	})

	c.AddMethod0(vm.Selectors, "negated", func(v *VM, recv Value) Value {
		n := recv.SmallInt()
		result := -n
		if val, ok := TryFromSmallInt(result); ok {
			return val
		}
		return v.registry.NewBigIntValue(new(big.Int).Neg(big.NewInt(n)))
	})

	c.AddMethod0(vm.Selectors, "abs", func(v *VM, recv Value) Value {
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
	c.AddMethod0(vm.Selectors, "primPrintString", func(v *VM, recv Value) Value {
		return v.registry.NewStringValue(strconv.FormatInt(recv.SmallInt(), 10))
	})

	// Iteration
	c.AddMethod1(vm.Selectors, "timesRepeat:", func(v *VM, recv Value, block Value) Value {
		n := recv.SmallInt()
		for i := int64(0); i < n; i++ {
			v.evaluateBlock(block, nil)
		}
		return recv
	})

	c.AddMethod2(vm.Selectors, "to:do:", func(v *VM, recv Value, stop Value, block Value) Value {
		start := recv.SmallInt()
		end := stop.SmallInt()
		for i := start; i <= end; i++ {
			v.evaluateBlock(block, []Value{FromSmallInt(i)})
		}
		return recv
	})

	c.AddMethod3(vm.Selectors, "to:by:do:", func(v *VM, recv Value, stop Value, step Value, block Value) Value {
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
