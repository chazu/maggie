package vm

import (
	"math/big"
)

// ---------------------------------------------------------------------------
// BigInteger Primitives
// ---------------------------------------------------------------------------

// getBigIntOperand extracts a *big.Int from a Value that is either a BigInt
// or a SmallInt. Returns nil if the value is neither.
func getBigIntOperand(vmPtr *VM, v Value) *big.Int {
	if IsBigIntValue(v) {
		obj := vmPtr.registry.GetBigInt(v)
		if obj != nil {
			return obj.Value
		}
		return nil
	}
	if v.IsSmallInt() {
		return big.NewInt(v.SmallInt())
	}
	return nil
}

// numericArgAsFloat coerces a numeric Value (Float, SmallInteger, or
// BigInteger) to float64. ok is false for non-numeric args. Used by Float
// primitives so a Float receiver with a BigInteger argument promotes to a Float
// result (matching the BigInteger-receiver side) instead of erroring.
func numericArgAsFloat(v *VM, arg Value) (float64, bool) {
	if arg.IsFloat() {
		return arg.Float64(), true
	}
	if arg.IsSmallInt() {
		return float64(arg.SmallInt()), true
	}
	if IsBigIntValue(arg) {
		if bi := v.registry.GetBigInt(arg); bi != nil {
			f, _ := new(big.Float).SetInt(bi.Value).Float64()
			return f, true
		}
	}
	return 0, false
}

// bigIntFloatCmp compares a BigInteger magnitude `a` against a float `f`,
// returning -1, 0, or 1 (a<f, a==f, a>f). Used so mixed BigInteger/Float
// ordering promotes to a real comparison (matching mixed arithmetic) instead
// of silently answering nil.
func bigIntFloatCmp(a *big.Int, f float64) int {
	return new(big.Float).SetInt(a).Cmp(big.NewFloat(f))
}

func (vm *VM) registerBigIntegerPrimitives() {
	c := vm.BigIntegerClass

	// Arithmetic: +
	c.AddMethod1(vm.Selectors, "+", func(v *VM, recv Value, arg Value) Value {
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		b := getBigIntOperand(v, arg)
		if b == nil {
			if arg.IsFloat() {
				// BigInt + Float -> Float
				af := new(big.Float).SetInt(a)
				bf := new(big.Float).SetFloat64(arg.Float64())
				result, _ := af.Add(af, bf).Float64()
				return FromFloat64(result)
			}
			return Nil
		}
		result := new(big.Int).Add(a, b)
		return v.registry.NewBigIntValue(result)
	})

	// Arithmetic: -
	c.AddMethod1(vm.Selectors, "-", func(v *VM, recv Value, arg Value) Value {
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		b := getBigIntOperand(v, arg)
		if b == nil {
			if arg.IsFloat() {
				af := new(big.Float).SetInt(a)
				bf := new(big.Float).SetFloat64(arg.Float64())
				result, _ := af.Sub(af, bf).Float64()
				return FromFloat64(result)
			}
			return Nil
		}
		result := new(big.Int).Sub(a, b)
		return v.registry.NewBigIntValue(result)
	})

	// Arithmetic: *
	c.AddMethod1(vm.Selectors, "*", func(v *VM, recv Value, arg Value) Value {
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		b := getBigIntOperand(v, arg)
		if b == nil {
			if arg.IsFloat() {
				af := new(big.Float).SetInt(a)
				bf := new(big.Float).SetFloat64(arg.Float64())
				result, _ := af.Mul(af, bf).Float64()
				return FromFloat64(result)
			}
			return Nil
		}
		result := new(big.Int).Mul(a, b)
		return v.registry.NewBigIntValue(result)
	})

	// Arithmetic: /  (integer division)
	c.AddMethod1(vm.Selectors, "/", func(v *VM, recv Value, arg Value) Value {
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		b := getBigIntOperand(v, arg)
		if b == nil {
			if arg.IsFloat() {
				af := new(big.Float).SetInt(a)
				bf := new(big.Float).SetFloat64(arg.Float64())
				result, _ := af.Quo(af, bf).Float64()
				return FromFloat64(result)
			}
			return Nil
		}
		if b.Sign() == 0 {
			return v.SignalZeroDivide() // catchable, consistent with SmallInt /
		}
		result := new(big.Int).Quo(a, b)
		return v.registry.NewBigIntValue(result)
	})

	// Arithmetic: \\ (modulo)
	c.AddMethod1(vm.Selectors, "\\\\", func(v *VM, recv Value, arg Value) Value {
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		b := getBigIntOperand(v, arg)
		if b == nil {
			return Nil
		}
		if b.Sign() == 0 {
			return v.SignalZeroDivide()
		}
		result := new(big.Int).Rem(a, b)
		return v.registry.NewBigIntValue(result)
	})

	// Arithmetic: // (floor division)
	c.AddMethod1(vm.Selectors, "//", func(v *VM, recv Value, arg Value) Value {
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		b := getBigIntOperand(v, arg)
		if b == nil {
			return Nil
		}
		if b.Sign() == 0 {
			return Nil
		}
		result := new(big.Int).Div(a, b) // Div is Euclidean/floor division in math/big
		return v.registry.NewBigIntValue(result)
	})

	// Comparison: <
	c.AddMethod1(vm.Selectors, "<", func(v *VM, recv Value, arg Value) Value {
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		b := getBigIntOperand(v, arg)
		if b == nil {
			if arg.IsFloat() {
				return FromBool(bigIntFloatCmp(a, arg.Float64()) < 0)
			}
			return v.SignalPrimitiveError("<", "argument must be a number")
		}
		return FromBool(a.Cmp(b) < 0)
	})

	// Comparison: >
	c.AddMethod1(vm.Selectors, ">", func(v *VM, recv Value, arg Value) Value {
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		b := getBigIntOperand(v, arg)
		if b == nil {
			if arg.IsFloat() {
				return FromBool(bigIntFloatCmp(a, arg.Float64()) > 0)
			}
			return v.SignalPrimitiveError(">", "argument must be a number")
		}
		return FromBool(a.Cmp(b) > 0)
	})

	// Comparison: <=
	c.AddMethod1(vm.Selectors, "<=", func(v *VM, recv Value, arg Value) Value {
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		b := getBigIntOperand(v, arg)
		if b == nil {
			if arg.IsFloat() {
				return FromBool(bigIntFloatCmp(a, arg.Float64()) <= 0)
			}
			return v.SignalPrimitiveError("<=", "argument must be a number")
		}
		return FromBool(a.Cmp(b) <= 0)
	})

	// Comparison: >=
	c.AddMethod1(vm.Selectors, ">=", func(v *VM, recv Value, arg Value) Value {
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		b := getBigIntOperand(v, arg)
		if b == nil {
			if arg.IsFloat() {
				return FromBool(bigIntFloatCmp(a, arg.Float64()) >= 0)
			}
			return v.SignalPrimitiveError(">=", "argument must be a number")
		}
		return FromBool(a.Cmp(b) >= 0)
	})

	// Comparison: =
	c.AddMethod1(vm.Selectors, "=", func(v *VM, recv Value, arg Value) Value {
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		// A non-numeric argument is simply not equal (= must answer a boolean,
		// never nil — OpSendEQ now dispatches here for BigInteger receivers).
		b := getBigIntOperand(v, arg)
		if b == nil {
			return False
		}
		return FromBool(a.Cmp(b) == 0)
	})

	// Unary: negated
	c.AddMethod0(vm.Selectors, "negated", func(v *VM, recv Value) Value {
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		result := new(big.Int).Neg(a)
		return v.registry.NewBigIntValue(result)
	})

	// Unary: abs
	c.AddMethod0(vm.Selectors, "abs", func(v *VM, recv Value) Value {
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		result := new(big.Int).Abs(a)
		return v.registry.NewBigIntValue(result)
	})

	// Printing
	c.AddMethod0(vm.Selectors, "primPrintString", func(v *VM, recv Value) Value {
		obj := v.registry.GetBigInt(recv)
		if obj == nil {
			return v.registry.NewStringValue("a BigInteger")
		}
		return v.registry.NewStringValue(obj.Value.String())
	})

	// Hash (for use in dictionaries)
	c.AddMethod0(vm.Selectors, "hash", func(v *VM, recv Value) Value {
		obj := v.registry.GetBigInt(recv)
		if obj == nil {
			return FromSmallInt(0)
		}
		// Use low bits of the big int as a hash
		words := obj.Value.Bits()
		var h uint64
		for _, w := range words {
			h = h*31 + uint64(w)
		}
		// Fit in SmallInt range
		return FromSmallInt(int64(h & uint64(MaxSmallInt)))
	})

	// primAsFloat - convert BigInt to Float
	c.AddMethod0(vm.Selectors, "primAsFloat", func(v *VM, recv Value) Value {
		obj := v.registry.GetBigInt(recv)
		if obj == nil {
			return Nil
		}
		f := new(big.Float).SetInt(obj.Value)
		result, _ := f.Float64()
		return FromFloat64(result)
	})

	// asSmallInt - convert BigInt to SmallInt, or Nil if out of range
	c.AddMethod0(vm.Selectors, "asSmallInt", func(v *VM, recv Value) Value {
		obj := v.registry.GetBigInt(recv)
		if obj == nil {
			return Nil
		}
		if obj.Value.IsInt64() {
			i64 := obj.Value.Int64()
			if i64 >= MinSmallInt && i64 <= MaxSmallInt {
				return FromSmallInt(i64)
			}
		}
		return Nil // Cannot fit
	})

	// Bit operations
	c.AddMethod1(vm.Selectors, "bitAnd:", func(v *VM, recv Value, arg Value) Value {
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		b := getBigIntOperand(v, arg)
		if b == nil {
			return Nil
		}
		result := new(big.Int).And(a, b)
		return v.registry.NewBigIntValue(result)
	})

	c.AddMethod1(vm.Selectors, "bitOr:", func(v *VM, recv Value, arg Value) Value {
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		b := getBigIntOperand(v, arg)
		if b == nil {
			return Nil
		}
		result := new(big.Int).Or(a, b)
		return v.registry.NewBigIntValue(result)
	})

	c.AddMethod1(vm.Selectors, "bitXor:", func(v *VM, recv Value, arg Value) Value {
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		b := getBigIntOperand(v, arg)
		if b == nil {
			return Nil
		}
		result := new(big.Int).Xor(a, b)
		return v.registry.NewBigIntValue(result)
	})

	c.AddMethod1(vm.Selectors, "bitShift:", func(v *VM, recv Value, arg Value) Value {
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		if !arg.IsSmallInt() {
			return v.SignalTypeError("bitShift:", 1, "SmallInteger", arg)
		}
		shift := arg.SmallInt()
		var result *big.Int
		if shift >= 0 {
			if shift > MaxBitShift {
				return v.SignalPrimitiveError("bitShift:", "shift amount too large")
			}
			result = new(big.Int).Lsh(a, uint(shift))
		} else {
			result = new(big.Int).Rsh(a, uint(-shift))
		}
		return v.registry.NewBigIntValue(result)
	})

	// sign - returns -1, 0, or 1
	c.AddMethod0(vm.Selectors, "primSign", func(v *VM, recv Value) Value {
		obj := v.registry.GetBigInt(recv)
		if obj == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(obj.Value.Sign()))
	})

	// isZero
	c.AddMethod0(vm.Selectors, "isZero", func(v *VM, recv Value) Value {
		obj := v.registry.GetBigInt(recv)
		if obj == nil {
			return False
		}
		return FromBool(obj.Value.Sign() == 0)
	})
}
