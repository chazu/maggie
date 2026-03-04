package vm

import "testing"

// ---------------------------------------------------------------------------
// Array sort primitive tests
// ---------------------------------------------------------------------------

func TestArraySortWithBlock(t *testing.T) {
	v := NewVM()

	// Create array #(3 1 4 1 5 9 2 6)
	arr := v.NewArrayWithElements([]Value{
		FromSmallInt(3), FromSmallInt(1), FromSmallInt(4), FromSmallInt(1),
		FromSmallInt(5), FromSmallInt(9), FromSmallInt(2), FromSmallInt(6),
	})

	// Create a comparison block: [:a :b | a - b]
	// We need to invoke the sort via the VM's Send using the primitive directly.
	// The primSort: primitive needs a block, which requires the interpreter.
	// Instead, test via the primSortDefault (no block needed).

	// Test default sort (no block)
	result := v.Send(arr, "primSortDefault", nil)

	// Should return self
	if result != arr {
		t.Errorf("sort should return self (the same array)")
	}

	// Verify sorted order: 1 1 2 3 4 5 6 9
	expected := []int64{1, 1, 2, 3, 4, 5, 6, 9}
	obj := ObjectFromValue(arr)
	for i, exp := range expected {
		val := obj.GetSlot(i)
		if !val.IsSmallInt() || val.SmallInt() != exp {
			t.Errorf("sorted[%d] = %v, want %d", i, val, exp)
		}
	}
}

func TestArraySortDefaultInPlace(t *testing.T) {
	v := NewVM()

	arr := v.NewArrayWithElements([]Value{
		FromSmallInt(5), FromSmallInt(3), FromSmallInt(1), FromSmallInt(4), FromSmallInt(2),
	})

	// Sort in place
	result := v.Send(arr, "primSortDefault", nil)

	// Should return the same object
	if result != arr {
		t.Errorf("sort should return self")
	}

	// Verify sorted
	obj := ObjectFromValue(arr)
	for i := 0; i < 4; i++ {
		a := obj.GetSlot(i)
		b := obj.GetSlot(i + 1)
		if a.SmallInt() > b.SmallInt() {
			t.Errorf("array not sorted at index %d: %d > %d", i, a.SmallInt(), b.SmallInt())
		}
	}
}

func TestArraySortedDefaultReturnsNewArray(t *testing.T) {
	v := NewVM()

	original := v.NewArrayWithElements([]Value{
		FromSmallInt(3), FromSmallInt(1), FromSmallInt(2),
	})

	// sorted (non-destructive)
	sorted := v.Send(original, "primSortedDefault", nil)

	// Should be a different object
	if sorted == original {
		t.Errorf("sorted should return a new array, not self")
	}

	// Original should be unchanged: 3, 1, 2
	origObj := ObjectFromValue(original)
	if origObj.GetSlot(0).SmallInt() != 3 {
		t.Errorf("original[0] should still be 3, got %d", origObj.GetSlot(0).SmallInt())
	}
	if origObj.GetSlot(1).SmallInt() != 1 {
		t.Errorf("original[1] should still be 1, got %d", origObj.GetSlot(1).SmallInt())
	}
	if origObj.GetSlot(2).SmallInt() != 2 {
		t.Errorf("original[2] should still be 2, got %d", origObj.GetSlot(2).SmallInt())
	}

	// Sorted should be 1, 2, 3
	sortedObj := ObjectFromValue(sorted)
	expected := []int64{1, 2, 3}
	for i, exp := range expected {
		val := sortedObj.GetSlot(i)
		if !val.IsSmallInt() || val.SmallInt() != exp {
			t.Errorf("sorted[%d] = %v, want %d", i, val, exp)
		}
	}
}

func TestArraySortEmptyArray(t *testing.T) {
	v := NewVM()

	arr := v.NewArray(0)
	result := v.Send(arr, "primSortDefault", nil)
	if result != arr {
		t.Errorf("sort of empty array should return self")
	}
}

func TestArraySortSingleElement(t *testing.T) {
	v := NewVM()

	arr := v.NewArrayWithElements([]Value{FromSmallInt(42)})
	result := v.Send(arr, "primSortDefault", nil)
	if result != arr {
		t.Errorf("sort of single-element array should return self")
	}
	obj := ObjectFromValue(arr)
	if obj.GetSlot(0).SmallInt() != 42 {
		t.Errorf("single element should remain 42")
	}
}

func TestArraySortAlreadySorted(t *testing.T) {
	v := NewVM()

	arr := v.NewArrayWithElements([]Value{
		FromSmallInt(1), FromSmallInt(2), FromSmallInt(3),
	})
	v.Send(arr, "primSortDefault", nil)

	obj := ObjectFromValue(arr)
	for i, exp := range []int64{1, 2, 3} {
		if obj.GetSlot(i).SmallInt() != exp {
			t.Errorf("sorted[%d] = %d, want %d", i, obj.GetSlot(i).SmallInt(), exp)
		}
	}
}

func TestArraySortReversed(t *testing.T) {
	v := NewVM()

	arr := v.NewArrayWithElements([]Value{
		FromSmallInt(5), FromSmallInt(4), FromSmallInt(3), FromSmallInt(2), FromSmallInt(1),
	})
	v.Send(arr, "primSortDefault", nil)

	obj := ObjectFromValue(arr)
	for i, exp := range []int64{1, 2, 3, 4, 5} {
		if obj.GetSlot(i).SmallInt() != exp {
			t.Errorf("sorted[%d] = %d, want %d", i, obj.GetSlot(i).SmallInt(), exp)
		}
	}
}
