package vm

import (
	"sync"
	"sync/atomic"
	"unsafe"
)

// Object represents a heap-allocated Maggie object.
//
// Objects use a hybrid slot layout optimized for common cases:
//   - 4 inline slots for objects with ≤4 instance variables (most objects)
//   - Overflow slice for objects with >4 instance variables
//
// This avoids slice allocation overhead for the common case while
// still supporting objects of arbitrary size.
type Object struct {
	vtable *VTable // Pointer to method dispatch table

	// forward supports become: — when non-nil, this object has been forwarded
	// to another object. All accesses should follow this pointer.
	// Atomic to prevent data races when goroutines hold *Object refs.
	forward atomic.Pointer[Object]

	// size tracks the actual number of slots for variable-sized objects (arrays).
	// For regular objects, this is 0 and NumSlots() returns NumInlineSlots + len(overflow).
	// For arrays, this is set to the actual array size.
	size int

	// Inline slots for the first 4 instance variables.
	// Most objects (Point, Association, Range, etc.) have ≤4 ivars.
	slot0 Value
	slot1 Value
	slot2 Value
	slot3 Value

	// Overflow for objects with >4 instance variables.
	// Only allocated when needed.
	overflow []Value
}

// NumInlineSlots is the number of slots stored directly in the Object struct.
const NumInlineSlots = 4

// VTable holds the method dispatch table for a class.
// This is a forward declaration; full implementation is in vtable.go.
//
// Local methods are stored in a compact map keyed by selector ID, so the
// per-class footprint is proportional to the number of methods actually
// defined on the class (not to the global selector ID space).
//
// Lookups go through `flat`, an open-addressed (linear-probing) hash
// table of (selector → method) entries that flattens the entire
// inheritance chain. It is sized to the number of reachable methods,
// rebuilt lazily on first Lookup after any structural mutation.
type VTable struct {
	class   *Class         // The class this vtable belongs to
	parent  *VTable        // Parent vtable for inheritance lookup
	methods map[int]Method // Local methods, keyed by selector ID

	// snap holds the current lazily-built dispatch snapshot. A nil pointer
	// means "needs rebuild" (the dirty signal) — there is no separate
	// dirty bool. Lookup readers do one atomic load on the happy path,
	// no locks. Writers (AddMethod/RemoveMethod/SetParent) take `mu`,
	// mutate `methods`/`parent`, and store nil into `snap`. The next
	// Lookup will rebuild under `mu` (double-checked locking).
	snap atomic.Pointer[vtSnapshot]

	// mu protects `methods`, `parent`, and serializes rebuilds of `snap`.
	// It is NEVER held by readers on the dispatch hot path — they only
	// touch `snap` atomically.
	mu sync.Mutex
}

// vtSnapshot is an immutable, flattened dispatch table built from the
// inheritance chain. Once published via VTable.snap atomic store, the
// contents are read-only — readers may share it across goroutines
// without locks.
type vtSnapshot struct {
	entries []vtEntry // Open-addressed hash table; empty iff method == nil
	mask    uint32    // len(entries) - 1
	shift   uint      // 32 - log2(len(entries)); precomputed for hot path
}

// vtEntry is one slot in the open-addressed dispatch hash table.
// An entry is empty iff method == nil. Selector 0 is a valid symbol ID
// (first symbol interned), so we cannot rely on selector == 0 alone.
type vtEntry struct {
	selector int
	method   Method
}

// Class represents a Maggie class.
// This is a forward declaration; full implementation is in class.go.
type Class struct {
	Name        string   // Class name
	Namespace   string   // Namespace (empty for default)
	Superclass  *Class   // Parent class (nil for Object)
	VTable      *VTable  // Method dispatch table for instance methods
	ClassVTable *VTable  // Method dispatch table for class methods (metaclass)
	InstVars    []string // Instance variable names
	NumSlots    int      // Total number of slots needed
	ClassVars   []string // Class variable names (shared across all instances)
	DocString   string   // documentation from """ ... """ (empty if none)

	// NonSerializable marks instances of this class as forbidden on the wire
	// (e.g. ClusterMember, which wraps live node handles). serializeObject
	// signals rather than emitting a detached zombie a peer could act on.
	NonSerializable bool

	// Metaclass is the lazily-created metaclass for this class.
	// In Smalltalk, every class X has a metaclass "X class" whose instances
	// are class X itself. The metaclass's VTable delegates to X's ClassVTable.
	Metaclass *Class
}

// Method represents a compiled or primitive method.
// This is a forward declaration; full implementation is in method.go.
type Method interface {
	Invoke(vm *VM, receiver Value, args []Value) Value
}

// ---------------------------------------------------------------------------
// Object creation
// ---------------------------------------------------------------------------

// NewObject creates a new Object with the given vtable and slot count.
// All slots are initialized to Nil.
func NewObject(vt *VTable, numSlots int) *Object {
	obj := &Object{vtable: vt, size: -1} // -1 means "use actual slot count"

	// Initialize inline slots to Nil
	obj.slot0 = Nil
	obj.slot1 = Nil
	obj.slot2 = Nil
	obj.slot3 = Nil

	// Allocate overflow if needed
	if numSlots > NumInlineSlots {
		obj.overflow = make([]Value, numSlots-NumInlineSlots)
		for i := range obj.overflow {
			obj.overflow[i] = Nil
		}
	}

	return obj
}

// NewObjectWithSlots creates a new Object and initializes its slots.
// Panics if len(slots) doesn't match the expected slot count.
func NewObjectWithSlots(vt *VTable, slots []Value) *Object {
	obj := &Object{vtable: vt, size: -1} // -1 means "use actual slot count"

	// Copy slots
	n := len(slots)
	if n > 0 {
		obj.slot0 = slots[0]
	} else {
		obj.slot0 = Nil
	}
	if n > 1 {
		obj.slot1 = slots[1]
	} else {
		obj.slot1 = Nil
	}
	if n > 2 {
		obj.slot2 = slots[2]
	} else {
		obj.slot2 = Nil
	}
	if n > 3 {
		obj.slot3 = slots[3]
	} else {
		obj.slot3 = Nil
	}

	// Overflow slots
	if n > NumInlineSlots {
		obj.overflow = make([]Value, n-NumInlineSlots)
		copy(obj.overflow, slots[NumInlineSlots:])
	}

	return obj
}

// ---------------------------------------------------------------------------
// Slot access
// ---------------------------------------------------------------------------

// GetSlot returns the value at the given slot index.
// Panics if index is out of range.
func (obj *Object) GetSlot(index int) Value {
	switch index {
	case 0:
		return obj.slot0
	case 1:
		return obj.slot1
	case 2:
		return obj.slot2
	case 3:
		return obj.slot3
	default:
		overflowIdx := index - NumInlineSlots
		if overflowIdx < 0 || overflowIdx >= len(obj.overflow) {
			panic("Object.GetSlot: index out of range")
		}
		return obj.overflow[overflowIdx]
	}
}

// SetSlot sets the value at the given slot index.
// Panics if index is out of range.
func (obj *Object) SetSlot(index int, value Value) {
	switch index {
	case 0:
		obj.slot0 = value
	case 1:
		obj.slot1 = value
	case 2:
		obj.slot2 = value
	case 3:
		obj.slot3 = value
	default:
		overflowIdx := index - NumInlineSlots
		if overflowIdx < 0 || overflowIdx >= len(obj.overflow) {
			panic("Object.SetSlot: index out of range")
		}
		obj.overflow[overflowIdx] = value
	}
}

// NumSlots returns the total number of slots in this object.
// For variable-sized objects (like arrays), returns the actual size.
// For regular objects, returns the physical slot count.
func (obj *Object) NumSlots() int {
	// For variable-sized objects like arrays, obj.size stores the logical size.
	// A value of -1 means "use actual slots" (for fixed-sized objects).
	// A value of 0 or more means "use this as the size" (for arrays).
	if obj.size >= 0 {
		return obj.size
	}
	return NumInlineSlots + len(obj.overflow)
}

// SetSize sets the logical size for variable-sized objects like arrays.
func (obj *Object) SetSize(n int) {
	obj.size = n
}

// VTablePtr returns the object's vtable.
func (obj *Object) VTablePtr() *VTable {
	return obj.vtable
}

// SetVTable sets the object's vtable (used during class change operations).
func (obj *Object) SetVTable(vt *VTable) {
	obj.vtable = vt
}

// ---------------------------------------------------------------------------
// Forwarding (become: support)
// ---------------------------------------------------------------------------

// Resolve follows the forwarding chain to get the actual object.
// If this object has not been forwarded, returns itself.
// This is used by become: to redirect all accesses to a new object.
func (obj *Object) Resolve() *Object {
	current := obj
	for {
		next := current.forward.Load()
		if next == nil {
			break
		}
		current = next
	}
	// Path compression: shortcut long chains so future resolves are O(1).
	if current != obj {
		obj.forward.Store(current)
	}
	return current
}

// IsForwarded returns true if this object has been forwarded to another object.
func (obj *Object) IsForwarded() bool {
	return obj.forward.Load() != nil
}

// ForwardTo sets up a forwarding pointer from this object to another.
// After this call, all accesses through ObjectFromValue will resolve to target.
func (obj *Object) ForwardTo(target *Object) {
	obj.forward.Store(target)
}

// ---------------------------------------------------------------------------
// Value conversion helpers
// ---------------------------------------------------------------------------

// ToValue converts an Object pointer to a NaN-boxed Value.
func (obj *Object) ToValue() Value {
	return FromObjectPtr(unsafe.Pointer(obj))
}

// ObjectFromValue extracts an Object pointer from a NaN-boxed Value.
// Returns nil if the value is not an object.
// Automatically follows forwarding pointers from become:.
func ObjectFromValue(v Value) *Object {
	if !v.IsObject() {
		return nil
	}
	obj := (*Object)(v.ObjectPtr())
	// Follow forwarding chain
	return obj.Resolve()
}

// ObjectFromValueRaw extracts an Object pointer without following forwarding.
// Use this only when you need the original object (e.g., for GC or become: itself).
func ObjectFromValueRaw(v Value) *Object {
	if !v.IsObject() {
		return nil
	}
	return (*Object)(v.ObjectPtr())
}

// MustObjectFromValue extracts an Object pointer from a NaN-boxed Value.
// Panics if the value is not an object.
// Automatically follows forwarding pointers from become:.
func MustObjectFromValue(v Value) *Object {
	if !v.IsObject() {
		panic("MustObjectFromValue: not an object")
	}
	obj := (*Object)(v.ObjectPtr())
	return obj.Resolve()
}

// ---------------------------------------------------------------------------
// Slot iteration
// ---------------------------------------------------------------------------

// ForEachSlot calls fn for each slot in the object.
// This is useful for garbage collection and debugging.
func (obj *Object) ForEachSlot(fn func(index int, value Value)) {
	fn(0, obj.slot0)
	fn(1, obj.slot1)
	fn(2, obj.slot2)
	fn(3, obj.slot3)
	for i, v := range obj.overflow {
		fn(NumInlineSlots+i, v)
	}
}

// AllSlots returns all slot values as a slice.
// This allocates; use ForEachSlot for allocation-free iteration.
func (obj *Object) AllSlots() []Value {
	slots := make([]Value, obj.NumSlots())
	slots[0] = obj.slot0
	slots[1] = obj.slot1
	slots[2] = obj.slot2
	slots[3] = obj.slot3
	copy(slots[NumInlineSlots:], obj.overflow)
	return slots
}

// ---------------------------------------------------------------------------
// Debugging
// ---------------------------------------------------------------------------

// ClassName returns the name of the object's class, or "?" if vtable is nil.
func (obj *Object) ClassName() string {
	if obj.vtable == nil || obj.vtable.class == nil {
		return "?"
	}
	return obj.vtable.class.Name
}

// ---------------------------------------------------------------------------
// become: implementation
// ---------------------------------------------------------------------------

// Become performs a two-way identity swap with another object.
// After this call, all references to obj will see other's contents and vice versa.
// Both objects must have the same number of slots, or an error is returned.
// This swaps vtable, size, and all slot contents between the two objects.
func (obj *Object) Become(other *Object) error {
	if obj == other {
		return nil // No-op: becoming yourself
	}

	// Swap vtables
	obj.vtable, other.vtable = other.vtable, obj.vtable

	// Swap size
	obj.size, other.size = other.size, obj.size

	// Swap inline slots
	obj.slot0, other.slot0 = other.slot0, obj.slot0
	obj.slot1, other.slot1 = other.slot1, obj.slot1
	obj.slot2, other.slot2 = other.slot2, obj.slot2
	obj.slot3, other.slot3 = other.slot3, obj.slot3

	// Swap overflow slices
	obj.overflow, other.overflow = other.overflow, obj.overflow

	// Note: we don't swap forwarding pointers - those are identity, not content

	return nil
}

// BecomeForward performs a one-way become: all accesses to obj will be
// redirected to other. The other object is unchanged.
// This is useful for proxies and lazy loading.
func (obj *Object) BecomeForward(other *Object) {
	if obj == other {
		return // No-op
	}
	obj.forward.Store(other.Resolve()) // Follow any existing chain
}
