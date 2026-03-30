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

func (vm *VM) registerBigIntegerPrimitives() {
	c := vm.BigIntegerClass

	// Arithmetic: +
	c.AddMethod1(vm.Selectors, "+", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod1(vm.Selectors, "-", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod1(vm.Selectors, "*", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod1(vm.Selectors, "/", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
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
			return Nil // Division by zero
		}
		result := new(big.Int).Quo(a, b)
		return v.registry.NewBigIntValue(result)
	})

	// Arithmetic: \\ (modulo)
	c.AddMethod1(vm.Selectors, "\\\\", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
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
		result := new(big.Int).Rem(a, b)
		return v.registry.NewBigIntValue(result)
	})

	// Arithmetic: // (floor division)
	c.AddMethod1(vm.Selectors, "//", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod1(vm.Selectors, "<", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		b := getBigIntOperand(v, arg)
		if b == nil {
			return Nil
		}
		return FromBool(a.Cmp(b) < 0)
	})

	// Comparison: >
	c.AddMethod1(vm.Selectors, ">", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		b := getBigIntOperand(v, arg)
		if b == nil {
			return Nil
		}
		return FromBool(a.Cmp(b) > 0)
	})

	// Comparison: <=
	c.AddMethod1(vm.Selectors, "<=", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		b := getBigIntOperand(v, arg)
		if b == nil {
			return Nil
		}
		return FromBool(a.Cmp(b) <= 0)
	})

	// Comparison: >=
	c.AddMethod1(vm.Selectors, ">=", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		b := getBigIntOperand(v, arg)
		if b == nil {
			return Nil
		}
		return FromBool(a.Cmp(b) >= 0)
	})

	// Comparison: =
	c.AddMethod1(vm.Selectors, "=", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		b := getBigIntOperand(v, arg)
		if b == nil {
			return Nil
		}
		return FromBool(a.Cmp(b) == 0)
	})

	// Unary: negated
	c.AddMethod0(vm.Selectors, "negated", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		result := new(big.Int).Neg(a)
		return v.registry.NewBigIntValue(result)
	})

	// Unary: abs
	c.AddMethod0(vm.Selectors, "abs", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		result := new(big.Int).Abs(a)
		return v.registry.NewBigIntValue(result)
	})

	// Printing
	c.AddMethod0(vm.Selectors, "primPrintString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		obj := v.registry.GetBigInt(recv)
		if obj == nil {
			return v.registry.NewStringValue("a BigInteger")
		}
		return v.registry.NewStringValue(obj.Value.String())
	})

	// Hash (for use in dictionaries)
	c.AddMethod0(vm.Selectors, "hash", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod0(vm.Selectors, "primAsFloat", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		obj := v.registry.GetBigInt(recv)
		if obj == nil {
			return Nil
		}
		f := new(big.Float).SetInt(obj.Value)
		result, _ := f.Float64()
		return FromFloat64(result)
	})

	// asSmallInt - convert BigInt to SmallInt (truncates if out of range)
	c.AddMethod0(vm.Selectors, "asSmallInt", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod1(vm.Selectors, "bitAnd:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
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

	c.AddMethod1(vm.Selectors, "bitOr:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
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

	c.AddMethod1(vm.Selectors, "bitXor:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
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

	c.AddMethod1(vm.Selectors, "bitShift:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		a := getBigIntOperand(v, recv)
		if a == nil {
			return Nil
		}
		if !arg.IsSmallInt() {
			return Nil
		}
		shift := arg.SmallInt()
		var result *big.Int
		if shift >= 0 {
			result = new(big.Int).Lsh(a, uint(shift))
		} else {
			result = new(big.Int).Rsh(a, uint(-shift))
		}
		return v.registry.NewBigIntValue(result)
	})

	// sign - returns -1, 0, or 1
	c.AddMethod0(vm.Selectors, "primSign", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		obj := v.registry.GetBigInt(recv)
		if obj == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(obj.Value.Sign()))
	})

	// isZero
	c.AddMethod0(vm.Selectors, "isZero", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		obj := v.registry.GetBigInt(recv)
		if obj == nil {
			return False
		}
		return FromBool(obj.Value.Sign() == 0)
	})
}
