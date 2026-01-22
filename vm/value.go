package vm

import (
	"math"
	"unsafe"
)

// Value represents a Maggie value using NaN-boxing.
//
// All values are represented as 64-bit IEEE 754 doubles. Non-float values
// are encoded in the NaN (Not-a-Number) space using the quiet NaN prefix
// and tag bits to distinguish types.
//
// Encoding scheme:
//   - Float: Native IEEE 754 double (if not a NaN, it's a float)
//   - SmallInt: Quiet NaN + tagInt + 48-bit signed payload
//   - Object: Quiet NaN + tagObject + 48-bit pointer
//   - Symbol: Quiet NaN + tagSymbol + symbol ID
//   - Special: Quiet NaN + tagSpecial + special value ID (nil/true/false)
type Value uint64

// NaN-boxing constants
const (
	// Quiet NaN prefix: exponent all 1s, quiet bit set, sign bit 0
	// 0x7FF8_0000_0000_0000
	nanBits uint64 = 0x7FF8000000000000

	// Tag mask: 3 bits within the NaN mantissa space
	// 0x0007_0000_0000_0000
	tagMask uint64 = 0x0007000000000000

	// Payload mask: 48 bits for pointer/int/id
	// 0x0000_FFFF_FFFF_FFFF
	payloadMask uint64 = 0x0000FFFFFFFFFFFF

	// Tag values (shifted into position)
	tagObject  uint64 = 0x0001000000000000 // Heap object pointer
	tagInt     uint64 = 0x0002000000000000 // 48-bit signed integer
	tagSpecial uint64 = 0x0003000000000000 // nil, true, false
	tagSymbol  uint64 = 0x0004000000000000 // Interned symbol ID
	tagBlock   uint64 = 0x0005000000000000 // Block closure ID
	tagCell    uint64 = 0x0006000000000000 // Mutable cell for captured variables

	// Sign bit for 48-bit integer sign extension
	intSignBit uint64 = 0x0000800000000000

	// Mask for sign extension
	intSignExtend uint64 = 0xFFFF000000000000
)

// Special value payloads
const (
	specialNil   uint64 = 0
	specialTrue  uint64 = 1
	specialFalse uint64 = 2
)

// Pre-defined special values
const (
	Nil   Value = Value(nanBits | tagSpecial | specialNil)
	True  Value = Value(nanBits | tagSpecial | specialTrue)
	False Value = Value(nanBits | tagSpecial | specialFalse)
)

// SmallInt range (48-bit signed)
const (
	MaxSmallInt int64 = (1 << 47) - 1  // 140,737,488,355,327
	MinSmallInt int64 = -(1 << 47)     // -140,737,488,355,328
)

// ---------------------------------------------------------------------------
// Type checking
// ---------------------------------------------------------------------------

// IsFloat returns true if v represents a float64 value.
// A value is a float if it's not one of our tagged NaN values.
// This includes regular numbers, infinities, and "real" NaN values.
func (v Value) IsFloat() bool {
	bits := uint64(v)

	// Check if it's a NaN or Infinity (exponent all 1s)
	if (bits & 0x7FF0000000000000) != 0x7FF0000000000000 {
		// Exponent is not all 1s, so it's a regular float
		return true
	}

	// Exponent is all 1s. Could be Infinity or NaN.
	// Infinity has mantissa == 0 (ignoring sign bit)
	mantissa := bits & 0x000FFFFFFFFFFFFF
	if mantissa == 0 {
		// It's +Inf or -Inf, which are valid floats
		return true
	}

	// It's a NaN. Check if it's one of our tagged values.
	// Our tagged values have the quiet NaN bit (0x0008...) set plus a non-zero tag.
	// We check if the quiet bit AND a tag bit are set.
	if (bits & nanBits) != nanBits {
		// Quiet NaN bit not set - it's a signaling NaN, treat as float
		return true
	}

	// It's a quiet NaN. Check tag bits.
	tag := bits & tagMask
	if tag == 0 {
		// No tag bits set - it's a "real" quiet NaN, treat as float
		return true
	}

	// It's one of our tagged non-float values
	return false
}

// IsSmallInt returns true if v represents a small integer.
func (v Value) IsSmallInt() bool {
	return (uint64(v) & (nanBits | tagMask)) == (nanBits | tagInt)
}

// IsObject returns true if v represents a heap object pointer.
func (v Value) IsObject() bool {
	return (uint64(v) & (nanBits | tagMask)) == (nanBits | tagObject)
}

// IsSymbol returns true if v represents an interned symbol.
func (v Value) IsSymbol() bool {
	return (uint64(v) & (nanBits | tagMask)) == (nanBits | tagSymbol)
}

// IsNil returns true if v is the nil value.
func (v Value) IsNil() bool {
	return v == Nil
}

// IsTrue returns true if v is the true value.
func (v Value) IsTrue() bool {
	return v == True
}

// IsFalse returns true if v is the false value.
func (v Value) IsFalse() bool {
	return v == False
}

// IsBool returns true if v is true or false.
func (v Value) IsBool() bool {
	return v == True || v == False
}

// IsSpecial returns true if v is nil, true, or false.
func (v Value) IsSpecial() bool {
	return (uint64(v) & (nanBits | tagMask)) == (nanBits | tagSpecial)
}

// ---------------------------------------------------------------------------
// Float operations
// ---------------------------------------------------------------------------

// Float64 returns v as a float64.
// Panics if v is not a float.
func (v Value) Float64() float64 {
	if !v.IsFloat() {
		panic("Value.Float64: not a float")
	}
	return math.Float64frombits(uint64(v))
}

// FromFloat64 creates a Value from a float64.
func FromFloat64(f float64) Value {
	return Value(math.Float64bits(f))
}

// ---------------------------------------------------------------------------
// SmallInt operations
// ---------------------------------------------------------------------------

// SmallInt returns v as an int64.
// Panics if v is not a small integer.
func (v Value) SmallInt() int64 {
	if !v.IsSmallInt() {
		panic("Value.SmallInt: not a small integer")
	}
	payload := uint64(v) & payloadMask

	// Sign extend from 48 bits to 64 bits
	if (payload & intSignBit) != 0 {
		payload |= intSignExtend
	}
	return int64(payload)
}

// FromSmallInt creates a Value from an int64.
// Panics if n is outside the SmallInt range.
func FromSmallInt(n int64) Value {
	if n > MaxSmallInt || n < MinSmallInt {
		panic("FromSmallInt: value out of range")
	}
	return Value(nanBits | tagInt | (uint64(n) & payloadMask))
}

// TryFromSmallInt creates a Value from an int64, returning false if out of range.
func TryFromSmallInt(n int64) (Value, bool) {
	if n > MaxSmallInt || n < MinSmallInt {
		return Nil, false
	}
	return Value(nanBits | tagInt | (uint64(n) & payloadMask)), true
}

// ---------------------------------------------------------------------------
// Object pointer operations
// ---------------------------------------------------------------------------

// ObjectPtr returns v as an unsafe.Pointer to the heap object.
// Panics if v is not an object.
func (v Value) ObjectPtr() unsafe.Pointer {
	if !v.IsObject() {
		panic("Value.ObjectPtr: not an object")
	}
	ptr := uintptr(uint64(v) & payloadMask)
	return unsafe.Pointer(ptr)
}

// FromObjectPtr creates a Value from an unsafe.Pointer.
// The pointer must fit in 48 bits (true for all current architectures).
func FromObjectPtr(ptr unsafe.Pointer) Value {
	return Value(nanBits | tagObject | uint64(uintptr(ptr)))
}

// ---------------------------------------------------------------------------
// Symbol operations
// ---------------------------------------------------------------------------

// SymbolID returns the symbol ID encoded in v.
// Panics if v is not a symbol.
func (v Value) SymbolID() uint32 {
	if !v.IsSymbol() {
		panic("Value.SymbolID: not a symbol")
	}
	return uint32(uint64(v) & payloadMask)
}

// FromSymbolID creates a Value from a symbol ID.
func FromSymbolID(id uint32) Value {
	return Value(nanBits | tagSymbol | uint64(id))
}

// IsBlock returns true if v represents a block closure.
func (v Value) IsBlock() bool {
	return (uint64(v) & (nanBits | tagMask)) == (nanBits | tagBlock)
}

// BlockID returns the block registry ID.
func (v Value) BlockID() uint32 {
	if !v.IsBlock() {
		panic("Value.BlockID: not a block")
	}
	return uint32(uint64(v) & payloadMask)
}

// FromBlockID creates a Value from a block ID.
func FromBlockID(id uint32) Value {
	return Value(nanBits | tagBlock | uint64(id))
}

// ---------------------------------------------------------------------------
// Cells (mutable boxes for captured variables)
// ---------------------------------------------------------------------------

// Cell is a heap-allocated mutable container for a single Value.
// Used to implement proper Smalltalk semantics where captured variables
// can be mutated and the mutation is visible in all scopes that share the cell.
type Cell struct {
	Value Value
}

// cellRegistry keeps cells alive to prevent Go's GC from collecting them.
// When we convert a Cell pointer to an integer (for NaN-boxing), Go can't
// track the reference anymore. This registry maintains a Go-visible reference.
var cellRegistry = make(map[*Cell]struct{})

// IsCell returns true if v represents a mutable cell.
func (v Value) IsCell() bool {
	bits := uint64(v)
	return (bits & (nanBits | tagMask)) == (nanBits | tagCell)
}

// CellPtr returns the Cell pointer from a cell value.
// Panics if v is not a cell.
func (v Value) CellPtr() *Cell {
	if !v.IsCell() {
		panic("Value.CellPtr: not a cell")
	}
	ptr := uint64(v) & payloadMask
	return (*Cell)(unsafe.Pointer(uintptr(ptr)))
}

// FromCellPtr creates a Value from a Cell pointer.
func FromCellPtr(cell *Cell) Value {
	ptr := uint64(uintptr(unsafe.Pointer(cell)))
	return Value(nanBits | tagCell | (ptr & payloadMask))
}

// NewCell creates a new Cell containing the given value and returns it as a Value.
// The cell is registered in cellRegistry to prevent garbage collection.
func NewCell(v Value) Value {
	cell := &Cell{Value: v}
	cellRegistry[cell] = struct{}{} // Keep alive
	return FromCellPtr(cell)
}

// CellGet returns the value stored in the cell.
// Panics if v is not a cell.
func (v Value) CellGet() Value {
	return v.CellPtr().Value
}

// CellSet stores a value in the cell.
// Panics if v is not a cell.
func (v Value) CellSet(newValue Value) {
	v.CellPtr().Value = newValue
}

// ---------------------------------------------------------------------------
// Boolean operations
// ---------------------------------------------------------------------------

// Bool returns v as a bool.
// Panics if v is not true or false.
func (v Value) Bool() bool {
	switch v {
	case True:
		return true
	case False:
		return false
	default:
		panic("Value.Bool: not a boolean")
	}
}

// FromBool creates a Value from a bool.
func FromBool(b bool) Value {
	if b {
		return True
	}
	return False
}

// ---------------------------------------------------------------------------
// Truthiness
// ---------------------------------------------------------------------------

// IsTruthy returns true if v is considered "truthy" in conditionals.
// In Smalltalk, only false and nil are falsy; everything else is truthy.
func (v Value) IsTruthy() bool {
	return v != False && v != Nil
}

// IsFalsy returns true if v is considered "falsy" in conditionals.
func (v Value) IsFalsy() bool {
	return v == False || v == Nil
}
