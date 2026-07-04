package vm

import (
	"math"
	"sync"
	"unsafe"
)

// Value represents a Maggie value.
//
// Immediates (float, SmallInt, nil/true/false, interned Symbol, Character) are
// NaN-boxed into the `hi` word exactly as before, with `ptr == nil`. Heap
// objects (String, Dictionary, ArrayList, Object, Cell, BigInt, Channel,
// Process, …) carry a real Go pointer in `ptr` — traced by the Go GC — and a
// small `kind` tag in `hi`. A live Value therefore keeps its heap object
// reachable, so the runtime traces the whole object graph and no custom
// collector or id registry is needed.
type Value struct {
	hi  uint64
	ptr unsafe.Pointer
}

// NaN-boxing constants for IMMEDIATES (stored in hi when ptr == nil).
const (
	// Quiet NaN prefix: exponent all 1s, quiet bit set, sign bit 0.
	nanBits uint64 = 0x7FF8000000000000

	// Tag mask: 3 bits within the NaN mantissa space.
	tagMask uint64 = 0x0007000000000000

	// Payload mask: 48 bits for int/id.
	payloadMask uint64 = 0x0000FFFFFFFFFFFF

	// Immediate tags (shifted into position). Heap kinds are NOT tags here —
	// they live in the `kind*` constants below and are only meaningful when
	// ptr != nil.
	tagInt     uint64 = 0x0002000000000000 // 48-bit signed integer
	tagSpecial uint64 = 0x0003000000000000 // nil, true, false
	tagSymbol  uint64 = 0x0004000000000000 // interned symbol id / character marker
	tagContext uint64 = 0x0007000000000000 // execution context id (registry, pre-migration)

	intSignBit    uint64 = 0x0000800000000000
	intSignExtend uint64 = 0xFFFF000000000000
)

// Special value payloads.
const (
	specialNil   uint64 = 0
	specialTrue  uint64 = 1
	specialFalse uint64 = 2
)

// Pre-defined special values. These are immediates (ptr == nil); they are vars
// (not consts) because Value is now a struct.
var (
	Nil   = Value{hi: nanBits | tagSpecial | specialNil}
	True  = Value{hi: nanBits | tagSpecial | specialTrue}
	False = Value{hi: nanBits | tagSpecial | specialFalse}
)

// SmallInt range (48-bit signed).
const (
	MaxSmallInt int64 = (1 << 47) - 1
	MinSmallInt int64 = -(1 << 47)
)

// heapKind tags the object `ptr` points to (only meaningful when ptr != nil).
// These replace the old NaN-box marker bytes. Values are arbitrary small ints.
type heapKind = uint64

const (
	kindNone heapKind = iota
	kindObject
	kindCell
	kindBlock
	kindContext
	kindString
	kindDictionary
	kindArrayList
	kindBigInt
	kindException
	kindResult
	kindClassValue
	kindGoObject
	kindWeakRef
	kindChannel
	kindProcess
	kindMutex
	kindWaitGroup
	kindSemaphore
	kindCancellationContext
	kindFuture
	kindRemoteRef
	kindRemoteChannel
	kindPromise
	// Contrib and extension kinds (http/grpc/cue/json/unix/sse/…) share a
	// single kindExtension; the concrete Go type is recovered from the
	// extension wrapper the pointer references. See markers.go.
	kindExtension
)

// makeHeap builds a heap Value from a kind tag and a pointer.
func makeHeap(kind heapKind, p unsafe.Pointer) Value {
	return Value{hi: kind, ptr: p}
}

// RawBits returns the immediate/kind word of the value. Exported for
// debug/diagnostic formatting only (e.g. a hex last-resort display); it does
// not capture the heap pointer, so it is not a complete identity.
func (v Value) RawBits() uint64 { return v.hi }

// isHeap reports whether v references a heap object.
func (v Value) isHeap() bool { return v.ptr != nil }

// heapKindOf returns the kind tag (only valid when isHeap()).
func (v Value) heapKindOf() heapKind { return v.hi }

// ---------------------------------------------------------------------------
// Type checking — immediates
// ---------------------------------------------------------------------------

// IsFloat returns true if v represents a float64 value.
func (v Value) IsFloat() bool {
	if v.ptr != nil {
		return false
	}
	bits := v.hi
	if (bits & 0x7FF0000000000000) != 0x7FF0000000000000 {
		return true
	}
	mantissa := bits & 0x000FFFFFFFFFFFFF
	if mantissa == 0 {
		return true
	}
	if (bits & nanBits) != nanBits {
		return true
	}
	if (bits & tagMask) == 0 {
		return true
	}
	return false
}

// IsSmallInt returns true if v represents a small integer.
func (v Value) IsSmallInt() bool {
	return v.ptr == nil && (v.hi&(nanBits|tagMask)) == (nanBits|tagInt)
}

// IsSymbolEncoded returns true if v carries the NaN-box symbol tag. In the
// pointer-Value world this is now true only for real interned Symbols and
// Characters (the heap kinds that used to piggy-back on the symbol tag are now
// distinguished by ptr != nil + their kind). Retained for the immediate
// disambiguation still done in markers.go.
func (v Value) IsSymbolEncoded() bool {
	return v.ptr == nil && (v.hi&(nanBits|tagMask)) == (nanBits|tagSymbol)
}

// IsSymbol returns true iff v is an actual interned Smalltalk Symbol.
func (v Value) IsSymbol() bool {
	if !v.IsSymbolEncoded() {
		return false
	}
	id := uint32(v.hi & payloadMask)
	return id>>24 == 0 && id < stringIDOffset
}

// IsNil returns true if v is the nil value.
func (v Value) IsNil() bool { return v == Nil }

// IsTrue returns true if v is the true value.
func (v Value) IsTrue() bool { return v == True }

// IsFalse returns true if v is the false value.
func (v Value) IsFalse() bool { return v == False }

// IsBool returns true if v is true or false.
func (v Value) IsBool() bool { return v == True || v == False }

// IsSpecial returns true if v is nil, true, or false.
func (v Value) IsSpecial() bool {
	return v.ptr == nil && (v.hi&(nanBits|tagMask)) == (nanBits|tagSpecial)
}

// ---------------------------------------------------------------------------
// Float operations
// ---------------------------------------------------------------------------

// Float64 returns v as a float64. Panics if v is not a float.
func (v Value) Float64() float64 {
	if !v.IsFloat() {
		panic("Value.Float64: not a float")
	}
	return math.Float64frombits(v.hi)
}

// FromFloat64 creates a Value from a float64.
func FromFloat64(f float64) Value {
	return Value{hi: math.Float64bits(f)}
}

// ---------------------------------------------------------------------------
// SmallInt operations
// ---------------------------------------------------------------------------

// SmallInt returns v as an int64. Panics if v is not a small integer.
func (v Value) SmallInt() int64 {
	if !v.IsSmallInt() {
		panic("Value.SmallInt: not a small integer")
	}
	payload := v.hi & payloadMask
	if (payload & intSignBit) != 0 {
		payload |= intSignExtend
	}
	return int64(payload)
}

// FromSmallInt creates a Value from an int64. Panics if out of range.
func FromSmallInt(n int64) Value {
	if n > MaxSmallInt || n < MinSmallInt {
		panic("FromSmallInt: value out of range")
	}
	return Value{hi: nanBits | tagInt | (uint64(n) & payloadMask)}
}

// TryFromSmallInt creates a Value from an int64, returning false if out of range.
func TryFromSmallInt(n int64) (Value, bool) {
	if n > MaxSmallInt || n < MinSmallInt {
		return Nil, false
	}
	return Value{hi: nanBits | tagInt | (uint64(n) & payloadMask)}, true
}

// ---------------------------------------------------------------------------
// Object pointer operations (heap)
// ---------------------------------------------------------------------------

// IsObject returns true if v references a heap Object.
func (v Value) IsObject() bool { return v.ptr != nil && v.hi == kindObject }

// ObjectPtr returns v as an unsafe.Pointer to the heap object.
func (v Value) ObjectPtr() unsafe.Pointer {
	if !v.IsObject() {
		panic("Value.ObjectPtr: not an object")
	}
	return v.ptr
}

// FromObjectPtr creates a Value from an *Object pointer.
func FromObjectPtr(ptr unsafe.Pointer) Value {
	return makeHeap(kindObject, ptr)
}

// ---------------------------------------------------------------------------
// Symbol operations (immediate)
// ---------------------------------------------------------------------------

// SymbolID returns the raw 32-bit symbol-tag payload encoded in v.
func (v Value) SymbolID() uint32 {
	if !v.IsSymbolEncoded() {
		panic("Value.SymbolID: not a symbol-encoded value")
	}
	return uint32(v.hi & payloadMask)
}

// FromSymbolID creates a Value from a symbol id (real symbol or character).
func FromSymbolID(id uint32) Value {
	return Value{hi: nanBits | tagSymbol | uint64(id)}
}

// ---------------------------------------------------------------------------
// Blocks (heap)
// ---------------------------------------------------------------------------

// IsBlock returns true if v references a block closure (pointer-carrying).
func (v Value) IsBlock() bool {
	return v.ptr != nil && v.hi == kindBlock
}

// blockPtr returns the *BlockValue behind a block Value, or nil.
func (v Value) blockPtr() *BlockValue {
	if !v.IsBlock() {
		return nil
	}
	return (*BlockValue)(v.ptr)
}

// makeBlockValue wraps a *BlockValue as a pointer-carrying block Value.
func makeBlockValue(bv *BlockValue) Value {
	return makeHeap(kindBlock, unsafe.Pointer(bv))
}

// ---------------------------------------------------------------------------
// Contexts (immediate id form, pre-migration)
// ---------------------------------------------------------------------------

// IsContext returns true if v represents an execution context.
func (v Value) IsContext() bool {
	return v.ptr == nil && (v.hi&(nanBits|tagMask)) == (nanBits|tagContext)
}

// ContextID returns the context registry ID.
func (v Value) ContextID() uint32 {
	if !v.IsContext() {
		panic("Value.ContextID: not a context")
	}
	return uint32(v.hi & payloadMask)
}

// FromContextID creates a Value from a context ID.
func FromContextID(id uint32) Value {
	return Value{hi: nanBits | tagContext | uint64(id)}
}

// ---------------------------------------------------------------------------
// Cells (mutable boxes for captured variables) (heap)
// ---------------------------------------------------------------------------

// Cell is a heap-allocated mutable container for a single Value.
type Cell struct {
	mu    sync.RWMutex
	value Value
}

// IsCell returns true if v references a mutable cell.
func (v Value) IsCell() bool { return v.ptr != nil && v.hi == kindCell }

// CellPtr returns the Cell pointer from a cell value.
func (v Value) CellPtr() *Cell {
	if !v.IsCell() {
		panic("Value.CellPtr: not a cell")
	}
	return (*Cell)(v.ptr)
}

// FromCellPtr creates a Value from a Cell pointer.
func FromCellPtr(cell *Cell) Value {
	return makeHeap(kindCell, unsafe.Pointer(cell))
}

// NewCell creates a new Cell containing the given value and returns it as a
// Value. The registry argument is retained for signature compatibility during
// the migration but is no longer needed for liveness (the Go GC traces the
// cell through the Value's pointer).
func NewCell(registry *ObjectRegistry, v Value) Value {
	cell := &Cell{value: v}
	return FromCellPtr(cell)
}

// CellGet returns the value stored in the cell. Thread-safe.
func (v Value) CellGet() Value {
	cell := v.CellPtr()
	cell.mu.RLock()
	defer cell.mu.RUnlock()
	return cell.value
}

// CellSet stores a value in the cell. Thread-safe.
func (v Value) CellSet(newValue Value) {
	cell := v.CellPtr()
	cell.mu.Lock()
	defer cell.mu.Unlock()
	cell.value = newValue
}

// ---------------------------------------------------------------------------
// Boolean operations
// ---------------------------------------------------------------------------

// Bool returns v as a bool. Panics if v is not true or false.
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
func (v Value) IsTruthy() bool { return v != False && v != Nil }

// IsFalsy returns true if v is considered "falsy" in conditionals.
func (v Value) IsFalsy() bool { return v == False || v == Nil }
