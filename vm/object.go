package vm

import (
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
type VTable struct {
	class   *Class   // The class this vtable belongs to
	parent  *VTable  // Parent vtable for inheritance lookup
	methods []Method // Methods indexed by selector ID
}

// Class represents a Maggie class.
// This is a forward declaration; full implementation is in class.go.
type Class struct {
	Name       string   // Class name
	Namespace  string   // Namespace (empty for default)
	Superclass *Class   // Parent class (nil for Object)
	VTable     *VTable  // Method dispatch table
	InstVars   []string // Instance variable names
	NumSlots   int      // Total number of slots needed
}

// Method represents a compiled or primitive method.
// This is a forward declaration; full implementation is in method.go.
type Method interface {
	Invoke(vm interface{}, receiver Value, args []Value) Value
}

// ---------------------------------------------------------------------------
// Object creation
// ---------------------------------------------------------------------------

// NewObject creates a new Object with the given vtable and slot count.
// All slots are initialized to Nil.
func NewObject(vt *VTable, numSlots int) *Object {
	obj := &Object{vtable: vt}

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
	obj := &Object{vtable: vt}

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
func (obj *Object) NumSlots() int {
	return NumInlineSlots + len(obj.overflow)
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
// Value conversion helpers
// ---------------------------------------------------------------------------

// ToValue converts an Object pointer to a NaN-boxed Value.
func (obj *Object) ToValue() Value {
	return FromObjectPtr(unsafe.Pointer(obj))
}

// ObjectFromValue extracts an Object pointer from a NaN-boxed Value.
// Returns nil if the value is not an object.
func ObjectFromValue(v Value) *Object {
	if !v.IsObject() {
		return nil
	}
	return (*Object)(v.ObjectPtr())
}

// MustObjectFromValue extracts an Object pointer from a NaN-boxed Value.
// Panics if the value is not an object.
func MustObjectFromValue(v Value) *Object {
	if !v.IsObject() {
		panic("MustObjectFromValue: not an object")
	}
	return (*Object)(v.ObjectPtr())
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
