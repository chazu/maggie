package vm

import (
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

// BigIntFromSmallInt creates a *big.Int from a SmallInt Value.
func BigIntFromSmallInt(v Value) *big.Int {
	return big.NewInt(v.SmallInt())
}

