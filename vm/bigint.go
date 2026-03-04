package vm

import (
	"math/big"
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
// BigInt Registry (in ObjectRegistry)
// ---------------------------------------------------------------------------

// RegisterBigInt stores a BigIntObject and returns a symbol-encoded Value.
func (or *ObjectRegistry) RegisterBigInt(obj *BigIntObject) Value {
	id := or.bigIntID.Add(1)
	or.bigIntsMu.Lock()
	or.bigInts[id] = obj
	or.bigIntsMu.Unlock()
	return FromSymbolID(uint32(id) | bigIntMarker)
}

// GetBigInt retrieves a BigIntObject from a symbol-encoded Value.
// Returns nil if the Value is not a BigInt.
func (or *ObjectRegistry) GetBigInt(v Value) *BigIntObject {
	if !v.IsSymbol() {
		return nil
	}
	id := v.SymbolID()
	if id&markerMask != bigIntMarker {
		return nil
	}
	rawID := id & 0x00FFFFFF
	or.bigIntsMu.RLock()
	defer or.bigIntsMu.RUnlock()
	return or.bigInts[rawID]
}

// UnregisterBigInt removes a BigInt from the registry.
func (or *ObjectRegistry) UnregisterBigInt(v Value) {
	if !v.IsSymbol() {
		return
	}
	id := v.SymbolID()
	if id&markerMask != bigIntMarker {
		return
	}
	rawID := id & 0x00FFFFFF
	or.bigIntsMu.Lock()
	delete(or.bigInts, rawID)
	or.bigIntsMu.Unlock()
}

// BigIntCount returns the number of registered BigInts.
func (or *ObjectRegistry) BigIntCount() int {
	or.bigIntsMu.RLock()
	defer or.bigIntsMu.RUnlock()
	return len(or.bigInts)
}

// ---------------------------------------------------------------------------
// Value helpers
// ---------------------------------------------------------------------------

// IsBigIntValue returns true if v is a symbol-encoded BigInt.
func IsBigIntValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	return v.SymbolID()&markerMask == bigIntMarker
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

