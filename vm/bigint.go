package vm

import (
	"math"
	"math/big"
	"unsafe"
)

// ---------------------------------------------------------------------------
// BigIntObject: Wraps math/big.Int for arbitrary-precision integers
// ---------------------------------------------------------------------------

// BigIntObject wraps a Go math/big.Int for arbitrary-precision integer arithmetic.
// BigInts are promoted from SmallInts when arithmetic overflows the 48-bit range,
// and are demoted back to SmallInts when the result fits.
type BigIntObject struct {
	Value *big.Int
}

// ---------------------------------------------------------------------------
// BigInt heap Values
// ---------------------------------------------------------------------------

// RegisterBigInt wraps a BigIntObject in a heap Value. The name is retained for
// call-site compatibility; the object is now carried by a real pointer traced
// by the Go GC rather than an id registry.
func (or *ObjectRegistry) RegisterBigInt(obj *BigIntObject) Value {
	return makeHeap(kindBigInt, unsafe.Pointer(obj))
}

// GetBigInt retrieves a BigIntObject from a Value.
// Returns nil if the Value is not a BigInt.
func (or *ObjectRegistry) GetBigInt(v Value) *BigIntObject {
	if !IsBigIntValue(v) {
		return nil
	}
	return (*BigIntObject)(v.ptr)
}

// ---------------------------------------------------------------------------
// Value helpers
// ---------------------------------------------------------------------------

// IsBigIntValue returns true if v is a heap BigInt.
func IsBigIntValue(v Value) bool {
	return v.ptr != nil && v.hi == kindBigInt
}

// NewBigIntValue creates a BigInt Value from a *big.Int.
// If the value fits in SmallInt range, returns a SmallInt instead (demotion).
func (or *ObjectRegistry) NewBigIntValue(n *big.Int) Value {
	if n.IsInt64() {
		i64 := n.Int64()
		if i64 >= MinSmallInt && i64 <= MaxSmallInt {
			return FromSmallInt(i64)
		}
	}
	return or.RegisterBigInt(&BigIntObject{Value: new(big.Int).Set(n)})
}

// NewIntegerValue returns n as a SmallInteger when it fits the 48-bit range,
// otherwise as a BigInteger. Use it instead of FromSmallInt for any int64 that
// is not provably small (parsed text, decoded data, arithmetic results):
// FromSmallInt panics out of range, which kills the VM.
func (or *ObjectRegistry) NewIntegerValue(n int64) Value {
	if v, ok := TryFromSmallInt(n); ok {
		return v
	}
	return or.RegisterBigInt(&BigIntObject{Value: big.NewInt(n)})
}

// integerFromFloat converts an integral-valued float (the result of
// truncation, rounding, floor, or ceiling) to an Integer, promoting to
// BigInteger beyond the SmallInteger range. NaN and Infinity have no integer
// value and signal a PrimitiveError naming selector.
func (vm *VM) integerFromFloat(selector string, f float64) Value {
	if math.IsNaN(f) || math.IsInf(f, 0) {
		return vm.SignalPrimitiveError(selector, "cannot convert NaN or Infinity to an Integer")
	}
	// |f| < 2^62 converts to int64 exactly (f is already integral); beyond
	// that int64(f) is implementation-defined, so go through big.Float.
	if f > -(1<<62) && f < 1<<62 {
		return vm.registry.NewIntegerValue(int64(f))
	}
	bi, _ := new(big.Float).SetFloat64(f).Int(nil)
	return vm.registry.NewBigIntValue(bi)
}

// BigIntFromSmallInt creates a *big.Int from a SmallInt Value.
func BigIntFromSmallInt(v Value) *big.Int {
	return big.NewInt(v.SmallInt())
}

