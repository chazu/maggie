package vm

import (
	"sort"
	"strings"
)

// ---------------------------------------------------------------------------
// Array Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerArrayPrimitives() {
	c := vm.ArrayClass

	// primSize - return array size
	c.AddMethod0(vm.Selectors, "primSize", func(_ interface{}, recv Value) Value {
		if recv.IsObject() {
			obj := ObjectFromValue(recv)
			if obj != nil {
				return FromSmallInt(int64(obj.NumSlots()))
			}
		}
		return FromSmallInt(0)
	})

	// size - alias for primSize for direct calls
	c.AddMethod0(vm.Selectors, "size", func(_ interface{}, recv Value) Value {
		if recv.IsObject() {
			obj := ObjectFromValue(recv)
			if obj != nil {
				return FromSmallInt(int64(obj.NumSlots()))
			}
		}
		return FromSmallInt(0)
	})

	// primAt: - array access (0-based indexing)
	c.AddMethod1(vm.Selectors, "primAt:", func(_ interface{}, recv Value, index Value) Value {
		if !recv.IsObject() || !index.IsSmallInt() {
			return Nil
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		idx := index.SmallInt()
		if idx < 0 || idx >= int64(obj.NumSlots()) {
			return Nil // Bounds error - would raise in full implementation
		}
		return obj.GetSlot(int(idx))
	})

	// at: - alias for primAt: for direct calls
	c.AddMethod1(vm.Selectors, "at:", func(_ interface{}, recv Value, index Value) Value {
		if !recv.IsObject() || !index.IsSmallInt() {
			return Nil
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		idx := index.SmallInt()
		if idx < 0 || idx >= int64(obj.NumSlots()) {
			return Nil // Bounds error - would raise in full implementation
		}
		return obj.GetSlot(int(idx))
	})

	// primAt:put: - array modification (0-based indexing)
	c.AddMethod2(vm.Selectors, "primAt:put:", func(_ interface{}, recv Value, index, value Value) Value {
		if !recv.IsObject() || !index.IsSmallInt() {
			return value
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return value
		}
		idx := index.SmallInt()
		if idx < 0 || idx >= int64(obj.NumSlots()) {
			return value // Bounds error - would raise in full implementation
		}
		obj.SetSlot(int(idx), value)
		return value
	})

	// at:put: - alias for primAt:put: for direct calls
	c.AddMethod2(vm.Selectors, "at:put:", func(_ interface{}, recv Value, index, value Value) Value {
		if !recv.IsObject() || !index.IsSmallInt() {
			return value
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return value
		}
		idx := index.SmallInt()
		if idx < 0 || idx >= int64(obj.NumSlots()) {
			return value // Bounds error - would raise in full implementation
		}
		obj.SetSlot(int(idx), value)
		return value
	})

	// Class-side new - create empty array
	c.AddClassMethod0(vm.Selectors, "new", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		return v.NewArray(0)
	})

	// Class-side new: - create array of given size
	// This is a class method - registered on ClassVTable
	c.AddClassMethod1(vm.Selectors, "new:", func(vmPtr interface{}, recv Value, size Value) Value {
		v := vmPtr.(*VM)
		if !size.IsSmallInt() {
			return Nil
		}
		n := size.SmallInt()
		if n < 0 {
			return Nil
		}
		// Create array object with n slots
		return v.NewArray(int(n))
	})

	// new:withAll: - create array of given size, all elements set to value
	c.AddClassMethod(vm.Selectors, "new:withAll:", NewPrimitiveMethod("new:withAll:", func(vmPtr interface{}, recv Value, args []Value) Value {
		v := vmPtr.(*VM)
		if len(args) < 2 || !args[0].IsSmallInt() {
			return Nil
		}
		n := int(args[0].SmallInt())
		if n < 0 {
			return Nil
		}
		fill := args[1]
		arr := v.NewArray(n)
		if arr.IsObject() {
			obj := ObjectFromValue(arr)
			if obj != nil {
				for i := 0; i < n; i++ {
					obj.SetSlot(i, fill)
				}
			}
		}
		return arr
	}))

	// with: - create single-element array (class side)
	c.AddClassMethod1(vm.Selectors, "with:", func(vmPtr interface{}, recv Value, elem Value) Value {
		v := vmPtr.(*VM)
		arr := v.NewArray(1)
		if arr.IsObject() {
			obj := ObjectFromValue(arr)
			obj.SetSlot(0, elem)
		}
		return arr
	})

	// with:with: - create two-element array (class side)
	c.AddClassMethod2(vm.Selectors, "with:with:", func(vmPtr interface{}, recv Value, elem1, elem2 Value) Value {
		v := vmPtr.(*VM)
		arr := v.NewArray(2)
		if arr.IsObject() {
			obj := ObjectFromValue(arr)
			obj.SetSlot(0, elem1)
			obj.SetSlot(1, elem2)
		}
		return arr
	})

	// sort: - sort array in-place using a comparison block
	// The block receives two elements and should return a negative number (a < b),
	// zero (a = b), or a positive number (a > b).
	c.AddMethod1(vm.Selectors, "primSort:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		if !recv.IsObject() {
			return recv
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return recv
		}
		n := obj.NumSlots()
		// Build an index slice, sort it, then rearrange slots
		indices := make([]int, n)
		for i := range indices {
			indices[i] = i
		}
		// Cache the elements so comparisons use original values
		elems := make([]Value, n)
		for i := 0; i < n; i++ {
			elems[i] = obj.GetSlot(i)
		}
		sort.SliceStable(indices, func(i, j int) bool {
			a := elems[indices[i]]
			b := elems[indices[j]]
			result := v.evaluateBlock(block, []Value{a, b})
			if result.IsSmallInt() {
				return result.SmallInt() < 0
			}
			return false
		})
		// Rearrange slots according to sorted indices
		sorted := make([]Value, n)
		for i, idx := range indices {
			sorted[i] = elems[idx]
		}
		for i, val := range sorted {
			obj.SetSlot(i, val)
		}
		return recv
	})

	// sort - sort array in-place using default < comparison
	c.AddMethod0(vm.Selectors, "primSortDefault", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if !recv.IsObject() {
			return recv
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return recv
		}
		n := obj.NumSlots()
		elems := make([]Value, n)
		for i := 0; i < n; i++ {
			elems[i] = obj.GetSlot(i)
		}
		sort.SliceStable(elems, func(i, j int) bool {
			result := v.Send(elems[i], "<", []Value{elems[j]})
			return result == True
		})
		for i, val := range elems {
			obj.SetSlot(i, val)
		}
		return recv
	})

	// sorted: - return a new sorted array using a comparison block (non-destructive)
	c.AddMethod1(vm.Selectors, "primSorted:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		if !recv.IsObject() {
			return recv
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return recv
		}
		n := obj.NumSlots()
		elems := make([]Value, n)
		for i := 0; i < n; i++ {
			elems[i] = obj.GetSlot(i)
		}
		sort.SliceStable(elems, func(i, j int) bool {
			result := v.evaluateBlock(block, []Value{elems[i], elems[j]})
			if result.IsSmallInt() {
				return result.SmallInt() < 0
			}
			return false
		})
		return v.NewArrayWithElements(elems)
	})

	// primJoinWith: - join array elements into a string with separator
	c.AddMethod1(vm.Selectors, "primJoinWith:", func(vmPtr interface{}, recv Value, sep Value) Value {
		v := vmPtr.(*VM)
		if !recv.IsObject() {
			return v.registry.NewStringValue("")
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return v.registry.NewStringValue("")
		}
		separator := v.getStringLike(sep)
		n := obj.NumSlots()
		parts := make([]string, n)
		for i := 0; i < n; i++ {
			elem := obj.GetSlot(i)
			parts[i] = v.getStringLike(elem)
		}
		return v.registry.NewStringValue(strings.Join(parts, separator))
	})

	// sorted - return a new sorted array using default < comparison (non-destructive)
	c.AddMethod0(vm.Selectors, "primSortedDefault", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if !recv.IsObject() {
			return recv
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return recv
		}
		n := obj.NumSlots()
		elems := make([]Value, n)
		for i := 0; i < n; i++ {
			elems[i] = obj.GetSlot(i)
		}
		sort.SliceStable(elems, func(i, j int) bool {
			result := v.Send(elems[i], "<", []Value{elems[j]})
			return result == True
		})
		return v.NewArrayWithElements(elems)
	})
}

// ---------------------------------------------------------------------------
// Array Helper Functions
// ---------------------------------------------------------------------------

// NewArray creates a new Array object with the given number of slots.
// All slots are initialized to Nil.
func (vm *VM) NewArray(size int) Value {
	if vm.ArrayClass == nil || vm.ArrayClass.VTable == nil {
		return Nil
	}
	obj := NewObject(vm.ArrayClass.VTable, size)
	obj.SetSize(size) // Set the logical size for arrays
	val := obj.ToValue()
	// Keep a reference to prevent GC
	vm.keepAlive[obj] = struct{}{}
	return val
}

// NewArrayWithElements creates a new Array object with the given elements.
func (vm *VM) NewArrayWithElements(elements []Value) Value {
	if vm.ArrayClass == nil || vm.ArrayClass.VTable == nil {
		return Nil
	}
	obj := NewObjectWithSlots(vm.ArrayClass.VTable, elements)
	obj.SetSize(len(elements)) // Set the logical size for arrays
	val := obj.ToValue()
	// Keep a reference to prevent GC
	vm.keepAlive[obj] = struct{}{}
	return val
}

// NewDictionary creates a new empty Dictionary.
func (vm *VM) NewDictionary() Value {
	return vm.registry.NewDictionaryValue()
}

// DictionaryAtPut sets a key-value pair in a dictionary.
func (vm *VM) DictionaryAtPut(dict Value, key Value, value Value) {
	d := vm.registry.GetDictionaryObject(dict)
	if d == nil {
		return
	}
	h := hashValue(vm.registry, key)
	d.Data[h] = value
	d.Keys[h] = key
}

// DictionaryAt gets a value from a dictionary by key.
func (vm *VM) DictionaryAt(dict Value, key Value) Value {
	d := vm.registry.GetDictionaryObject(dict)
	if d == nil {
		return Nil
	}
	h := hashValue(vm.registry, key)
	if val, ok := d.Data[h]; ok {
		return val
	}
	return Nil
}
