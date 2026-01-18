package vm

import (
	"testing"
	"unsafe"
)

// ---------------------------------------------------------------------------
// Object creation tests
// ---------------------------------------------------------------------------

func TestNewObject(t *testing.T) {
	// Create object with 2 slots (inline only)
	obj := NewObject(nil, 2)
	if obj == nil {
		t.Fatal("NewObject returned nil")
	}
	if obj.NumSlots() != NumInlineSlots {
		t.Errorf("NumSlots() = %d, want %d (inline slots)", obj.NumSlots(), NumInlineSlots)
	}

	// All inline slots should be Nil
	for i := 0; i < NumInlineSlots; i++ {
		if obj.GetSlot(i) != Nil {
			t.Errorf("slot %d should be Nil", i)
		}
	}
}

func TestNewObjectWithOverflow(t *testing.T) {
	// Create object with 7 slots (4 inline + 3 overflow)
	obj := NewObject(nil, 7)
	if obj == nil {
		t.Fatal("NewObject returned nil")
	}
	if obj.NumSlots() != 7 {
		t.Errorf("NumSlots() = %d, want 7", obj.NumSlots())
	}

	// All slots should be Nil
	for i := 0; i < 7; i++ {
		if obj.GetSlot(i) != Nil {
			t.Errorf("slot %d should be Nil", i)
		}
	}
}

func TestNewObjectWithSlots(t *testing.T) {
	slots := []Value{
		FromSmallInt(10),
		FromSmallInt(20),
		FromSmallInt(30),
		FromSmallInt(40),
		FromSmallInt(50),
		FromSmallInt(60),
	}

	obj := NewObjectWithSlots(nil, slots)
	if obj.NumSlots() != 6 {
		t.Errorf("NumSlots() = %d, want 6", obj.NumSlots())
	}

	for i, expected := range slots {
		got := obj.GetSlot(i)
		if got != expected {
			t.Errorf("slot %d = %v, want %v", i, got, expected)
		}
	}
}

func TestNewObjectWithFewerSlots(t *testing.T) {
	// Create with fewer than 4 slots
	slots := []Value{FromSmallInt(1), FromSmallInt(2)}
	obj := NewObjectWithSlots(nil, slots)

	if obj.GetSlot(0).SmallInt() != 1 {
		t.Error("slot 0 should be 1")
	}
	if obj.GetSlot(1).SmallInt() != 2 {
		t.Error("slot 1 should be 2")
	}
	if obj.GetSlot(2) != Nil {
		t.Error("slot 2 should be Nil")
	}
	if obj.GetSlot(3) != Nil {
		t.Error("slot 3 should be Nil")
	}
}

// ---------------------------------------------------------------------------
// Slot access tests
// ---------------------------------------------------------------------------

func TestGetSetInlineSlots(t *testing.T) {
	obj := NewObject(nil, 4)

	// Set each inline slot
	for i := 0; i < NumInlineSlots; i++ {
		obj.SetSlot(i, FromSmallInt(int64(i*10)))
	}

	// Verify each slot
	for i := 0; i < NumInlineSlots; i++ {
		got := obj.GetSlot(i).SmallInt()
		want := int64(i * 10)
		if got != want {
			t.Errorf("GetSlot(%d) = %d, want %d", i, got, want)
		}
	}
}

func TestGetSetOverflowSlots(t *testing.T) {
	obj := NewObject(nil, 8)

	// Set all slots including overflow
	for i := 0; i < 8; i++ {
		obj.SetSlot(i, FromSmallInt(int64(i*100)))
	}

	// Verify all slots
	for i := 0; i < 8; i++ {
		got := obj.GetSlot(i).SmallInt()
		want := int64(i * 100)
		if got != want {
			t.Errorf("GetSlot(%d) = %d, want %d", i, got, want)
		}
	}
}

func TestGetSlotPanicOnOutOfRange(t *testing.T) {
	obj := NewObject(nil, 4) // Only inline slots

	defer func() {
		if r := recover(); r == nil {
			t.Error("GetSlot(10) should panic for object with 4 slots")
		}
	}()
	obj.GetSlot(10)
}

func TestSetSlotPanicOnOutOfRange(t *testing.T) {
	obj := NewObject(nil, 4)

	defer func() {
		if r := recover(); r == nil {
			t.Error("SetSlot(10, ...) should panic for object with 4 slots")
		}
	}()
	obj.SetSlot(10, FromSmallInt(42))
}

func TestSlotOverwrite(t *testing.T) {
	obj := NewObject(nil, 4)

	obj.SetSlot(0, FromSmallInt(100))
	if obj.GetSlot(0).SmallInt() != 100 {
		t.Error("first write failed")
	}

	obj.SetSlot(0, FromSmallInt(200))
	if obj.GetSlot(0).SmallInt() != 200 {
		t.Error("overwrite failed")
	}
}

// ---------------------------------------------------------------------------
// Value conversion tests
// ---------------------------------------------------------------------------

func TestObjectToValue(t *testing.T) {
	obj := NewObject(nil, 2)
	obj.SetSlot(0, FromSmallInt(42))

	v := obj.ToValue()
	if !v.IsObject() {
		t.Error("ToValue should return an object value")
	}

	// Round-trip
	obj2 := ObjectFromValue(v)
	if obj2 != obj {
		t.Error("ObjectFromValue should return the same object")
	}
	if obj2.GetSlot(0).SmallInt() != 42 {
		t.Error("slot value should be preserved")
	}
}

func TestObjectFromValueNonObject(t *testing.T) {
	v := FromSmallInt(42)
	obj := ObjectFromValue(v)
	if obj != nil {
		t.Error("ObjectFromValue should return nil for non-object")
	}
}

func TestMustObjectFromValuePanic(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("MustObjectFromValue should panic for non-object")
		}
	}()
	MustObjectFromValue(FromSmallInt(42))
}

// ---------------------------------------------------------------------------
// VTable tests
// ---------------------------------------------------------------------------

func TestObjectVTable(t *testing.T) {
	class := &Class{Name: "Point"}
	vt := &VTable{class: class}
	obj := NewObject(vt, 2)

	if obj.VTablePtr() != vt {
		t.Error("VTablePtr should return the vtable")
	}
	if obj.ClassName() != "Point" {
		t.Errorf("ClassName() = %q, want %q", obj.ClassName(), "Point")
	}
}

func TestObjectSetVTable(t *testing.T) {
	class1 := &Class{Name: "Point"}
	class2 := &Class{Name: "ColorPoint"}
	vt1 := &VTable{class: class1}
	vt2 := &VTable{class: class2}

	obj := NewObject(vt1, 2)
	if obj.ClassName() != "Point" {
		t.Error("initial class should be Point")
	}

	obj.SetVTable(vt2)
	if obj.ClassName() != "ColorPoint" {
		t.Error("class should be ColorPoint after SetVTable")
	}
}

func TestObjectClassNameNilVTable(t *testing.T) {
	obj := NewObject(nil, 2)
	if obj.ClassName() != "?" {
		t.Errorf("ClassName() with nil vtable = %q, want %q", obj.ClassName(), "?")
	}
}

// ---------------------------------------------------------------------------
// Iteration tests
// ---------------------------------------------------------------------------

func TestForEachSlot(t *testing.T) {
	obj := NewObject(nil, 6)
	for i := 0; i < 6; i++ {
		obj.SetSlot(i, FromSmallInt(int64(i)))
	}

	visited := make(map[int]int64)
	obj.ForEachSlot(func(index int, value Value) {
		if value.IsSmallInt() {
			visited[index] = value.SmallInt()
		}
	})

	for i := 0; i < 6; i++ {
		if visited[i] != int64(i) {
			t.Errorf("ForEachSlot didn't visit slot %d correctly", i)
		}
	}
}

func TestAllSlots(t *testing.T) {
	obj := NewObject(nil, 6)
	for i := 0; i < 6; i++ {
		obj.SetSlot(i, FromSmallInt(int64(i*10)))
	}

	slots := obj.AllSlots()
	if len(slots) != 6 {
		t.Errorf("AllSlots() length = %d, want 6", len(slots))
	}

	for i, v := range slots {
		if v.SmallInt() != int64(i*10) {
			t.Errorf("AllSlots()[%d] = %d, want %d", i, v.SmallInt(), i*10)
		}
	}
}

// ---------------------------------------------------------------------------
// Memory layout tests
// ---------------------------------------------------------------------------

func TestObjectSize(t *testing.T) {
	// Object should be reasonably sized
	// vtable (8) + 4 slots (32) + slice header (24) = 64 bytes on 64-bit
	size := unsafe.Sizeof(Object{})
	// Allow some flexibility for alignment
	if size > 80 {
		t.Errorf("Object size = %d bytes, expected <= 80", size)
	}
}

func TestObjectPointerStability(t *testing.T) {
	// Verify that object pointers remain stable after slot operations
	obj := NewObject(nil, 4)
	ptr1 := unsafe.Pointer(obj)

	obj.SetSlot(0, FromSmallInt(100))
	obj.SetSlot(1, FromSmallInt(200))
	obj.SetSlot(2, FromSmallInt(300))
	obj.SetSlot(3, FromSmallInt(400))

	ptr2 := unsafe.Pointer(obj)
	if ptr1 != ptr2 {
		t.Error("object pointer should not change after slot operations")
	}
}

// ---------------------------------------------------------------------------
// Edge cases
// ---------------------------------------------------------------------------

func TestEmptyObjectSlots(t *testing.T) {
	// Even with 0 requested slots, we have 4 inline slots
	obj := NewObject(nil, 0)
	if obj.NumSlots() != NumInlineSlots {
		t.Errorf("NumSlots() = %d, want %d", obj.NumSlots(), NumInlineSlots)
	}
}

func TestMixedValueTypes(t *testing.T) {
	obj := NewObject(nil, 6)

	obj.SetSlot(0, FromSmallInt(42))
	obj.SetSlot(1, FromFloat64(3.14))
	obj.SetSlot(2, True)
	obj.SetSlot(3, Nil)
	obj.SetSlot(4, FromSymbolID(100))

	// Create a nested object
	inner := NewObject(nil, 1)
	inner.SetSlot(0, FromSmallInt(999))
	obj.SetSlot(5, inner.ToValue())

	// Verify all types
	if obj.GetSlot(0).SmallInt() != 42 {
		t.Error("SmallInt slot failed")
	}
	if obj.GetSlot(1).Float64() != 3.14 {
		t.Error("Float slot failed")
	}
	if obj.GetSlot(2) != True {
		t.Error("True slot failed")
	}
	if obj.GetSlot(3) != Nil {
		t.Error("Nil slot failed")
	}
	if obj.GetSlot(4).SymbolID() != 100 {
		t.Error("Symbol slot failed")
	}

	// Verify nested object
	innerFromSlot := ObjectFromValue(obj.GetSlot(5))
	if innerFromSlot == nil {
		t.Fatal("nested object is nil")
	}
	if innerFromSlot.GetSlot(0).SmallInt() != 999 {
		t.Error("nested object slot failed")
	}
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

func BenchmarkGetSlotInline(b *testing.B) {
	obj := NewObject(nil, 4)
	obj.SetSlot(2, FromSmallInt(42))
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = obj.GetSlot(2)
	}
}

func BenchmarkGetSlotOverflow(b *testing.B) {
	obj := NewObject(nil, 8)
	obj.SetSlot(6, FromSmallInt(42))
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = obj.GetSlot(6)
	}
}

func BenchmarkSetSlotInline(b *testing.B) {
	obj := NewObject(nil, 4)
	v := FromSmallInt(42)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		obj.SetSlot(2, v)
	}
}

func BenchmarkSetSlotOverflow(b *testing.B) {
	obj := NewObject(nil, 8)
	v := FromSmallInt(42)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		obj.SetSlot(6, v)
	}
}

func BenchmarkNewObject(b *testing.B) {
	for i := 0; i < b.N; i++ {
		_ = NewObject(nil, 4)
	}
}

func BenchmarkNewObjectWithOverflow(b *testing.B) {
	for i := 0; i < b.N; i++ {
		_ = NewObject(nil, 8)
	}
}

func BenchmarkObjectToValue(b *testing.B) {
	obj := NewObject(nil, 4)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = obj.ToValue()
	}
}

func BenchmarkObjectFromValue(b *testing.B) {
	obj := NewObject(nil, 4)
	v := obj.ToValue()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = ObjectFromValue(v)
	}
}
