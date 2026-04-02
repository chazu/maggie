package vm

import (
	"strings"
)

// ---------------------------------------------------------------------------
// ArrayList Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerArrayListPrimitives() {
	c := vm.ArrayListClass

	// --- Class methods ---

	// ArrayList class>>new - create empty with default capacity (8)
	newFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		al := createArrayList(8)
		return v.registerArrayList(al)
	}
	c.AddClassMethod0(vm.Selectors, "new", newFn)
	c.AddClassMethod0(vm.Selectors, "primNew", newFn)

	// ArrayList class>>new: capacity - create empty with given capacity
	newCapFn := func(vmPtr interface{}, recv Value, cap Value) Value {
		v := vmPtr.(*VM)
		capacity := 8
		if cap.IsSmallInt() {
			capacity = int(cap.SmallInt())
			if capacity < 0 {
				capacity = 0
			}
		}
		al := createArrayList(capacity)
		return v.registerArrayList(al)
	}
	c.AddClassMethod1(vm.Selectors, "new:", newCapFn)
	c.AddClassMethod1(vm.Selectors, "primNew:", newCapFn)

	// ArrayList class>>withAll: anArray - create from existing array
	withAllFn := func(vmPtr interface{}, recv Value, arr Value) Value {
		v := vmPtr.(*VM)
		if !arr.IsObject() {
			al := createArrayList(0)
			return v.registerArrayList(al)
		}
		obj := ObjectFromValue(arr)
		if obj == nil {
			al := createArrayList(0)
			return v.registerArrayList(al)
		}
		n := obj.NumSlots()
		al := createArrayList(n)
		for i := 0; i < n; i++ {
			al.Add(obj.GetSlot(i))
		}
		return v.registerArrayList(al)
	}
	c.AddClassMethod1(vm.Selectors, "withAll:", withAllFn)
	c.AddClassMethod1(vm.Selectors, "primWithAll:", withAllFn)

	// --- Instance methods ---

	// add: element - append, returns self
	addFn := func(vmPtr interface{}, recv Value, elem Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil {
			return recv
		}
		al.Add(elem)
		return recv
	}
	c.AddMethod1(vm.Selectors, "add:", addFn)
	c.AddMethod1(vm.Selectors, "primAdd:", addFn)

	// addAll: collection - append all elements from an Array or ArrayList
	addAllFn := func(vmPtr interface{}, recv Value, coll Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil {
			return recv
		}
		// Try as ArrayList first
		if other := v.getArrayList(coll); other != nil {
			for _, elem := range other.elements {
				al.Add(elem)
			}
			return recv
		}
		// Try as Array (Object with slots)
		if coll.IsObject() {
			obj := ObjectFromValue(coll)
			if obj != nil {
				n := obj.NumSlots()
				for i := 0; i < n; i++ {
					al.Add(obj.GetSlot(i))
				}
			}
		}
		return recv
	}
	c.AddMethod1(vm.Selectors, "addAll:", addAllFn)
	c.AddMethod1(vm.Selectors, "primAddAll:", addAllFn)

	// at: index
	atFn := func(vmPtr interface{}, recv Value, index Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil || !index.IsSmallInt() {
			return Nil
		}
		return al.At(int(index.SmallInt()))
	}
	c.AddMethod1(vm.Selectors, "at:", atFn)
	c.AddMethod1(vm.Selectors, "primAt:", atFn)

	// at:put: - indexed set, returns value
	atPutFn := func(vmPtr interface{}, recv Value, index, value Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil || !index.IsSmallInt() {
			return value
		}
		al.AtPut(int(index.SmallInt()), value)
		return value
	}
	c.AddMethod2(vm.Selectors, "at:put:", atPutFn)
	c.AddMethod2(vm.Selectors, "primAt:put:", atPutFn)

	// size
	sizeFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(al.Size()))
	}
	c.AddMethod0(vm.Selectors, "size", sizeFn)
	c.AddMethod0(vm.Selectors, "primSize", sizeFn)

	// capacity
	capFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(al.Capacity()))
	}
	c.AddMethod0(vm.Selectors, "capacity", capFn)
	c.AddMethod0(vm.Selectors, "primCapacity", capFn)

	// removeLast
	removeLastFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil {
			return Nil
		}
		return al.RemoveLast()
	}
	c.AddMethod0(vm.Selectors, "removeLast", removeLastFn)
	c.AddMethod0(vm.Selectors, "primRemoveLast", removeLastFn)

	// removeAt: index
	removeAtFn := func(vmPtr interface{}, recv Value, index Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil || !index.IsSmallInt() {
			return Nil
		}
		return al.RemoveAt(int(index.SmallInt()))
	}
	c.AddMethod1(vm.Selectors, "removeAt:", removeAtFn)
	c.AddMethod1(vm.Selectors, "primRemoveAt:", removeAtFn)

	// clear - returns self
	clearFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil {
			return recv
		}
		al.Clear()
		return recv
	}
	c.AddMethod0(vm.Selectors, "clear", clearFn)
	c.AddMethod0(vm.Selectors, "primClear", clearFn)

	// first
	firstFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil || al.Size() == 0 {
			return Nil
		}
		return al.At(0)
	}
	c.AddMethod0(vm.Selectors, "first", firstFn)
	c.AddMethod0(vm.Selectors, "primFirst", firstFn)

	// last
	lastFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil || al.Size() == 0 {
			return Nil
		}
		return al.At(al.Size() - 1)
	}
	c.AddMethod0(vm.Selectors, "last", lastFn)
	c.AddMethod0(vm.Selectors, "primLast", lastFn)

	// isEmpty
	isEmptyFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil || al.Size() == 0 {
			return True
		}
		return False
	}
	c.AddMethod0(vm.Selectors, "isEmpty", isEmptyFn)
	c.AddMethod0(vm.Selectors, "primIsEmpty", isEmptyFn)

	// notEmpty
	notEmptyFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil || al.Size() == 0 {
			return False
		}
		return True
	}
	c.AddMethod0(vm.Selectors, "notEmpty", notEmptyFn)
	c.AddMethod0(vm.Selectors, "primNotEmpty", notEmptyFn)

	// includes: element
	includesFn := func(vmPtr interface{}, recv Value, elem Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil {
			return False
		}
		for _, e := range al.elements {
			if v.Send(e, "=", []Value{elem}) == True {
				return True
			}
		}
		return False
	}
	c.AddMethod1(vm.Selectors, "includes:", includesFn)
	c.AddMethod1(vm.Selectors, "primIncludes:", includesFn)

	// indexOf: element
	indexOfFn := func(vmPtr interface{}, recv Value, elem Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil {
			return FromSmallInt(-1)
		}
		for i, e := range al.elements {
			if v.Send(e, "=", []Value{elem}) == True {
				return FromSmallInt(int64(i))
			}
		}
		return FromSmallInt(-1)
	}
	c.AddMethod1(vm.Selectors, "indexOf:", indexOfFn)
	c.AddMethod1(vm.Selectors, "primIndexOf:", indexOfFn)

	// asArray - convert to fixed-size Array
	asArrayFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil {
			return v.NewArray(0)
		}
		// Copy elements to avoid sharing
		elems := make([]Value, al.Size())
		copy(elems, al.elements)
		return v.NewArrayWithElements(elems)
	}
	c.AddMethod0(vm.Selectors, "asArray", asArrayFn)
	c.AddMethod0(vm.Selectors, "primAsArray", asArrayFn)

	// do: block - iterate calling block with each element
	doFn := func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil {
			return Nil
		}
		// Snapshot length — safe if elements are added during iteration
		n := al.Size()
		for i := 0; i < n; i++ {
			v.evaluateBlock(block, []Value{al.elements[i]})
		}
		return Nil
	}
	c.AddMethod1(vm.Selectors, "do:", doFn)
	c.AddMethod1(vm.Selectors, "primDo:", doFn)

	// collect: block - return new ArrayList with transformed elements
	collectFn := func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil {
			return v.registerArrayList(createArrayList(0))
		}
		result := createArrayList(al.Size())
		for i := 0; i < al.Size(); i++ {
			val := v.evaluateBlock(block, []Value{al.elements[i]})
			result.Add(val)
		}
		return v.registerArrayList(result)
	}
	c.AddMethod1(vm.Selectors, "collect:", collectFn)
	c.AddMethod1(vm.Selectors, "primCollect:", collectFn)

	// select: block - return new ArrayList with matching elements
	selectFn := func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil {
			return v.registerArrayList(createArrayList(0))
		}
		result := createArrayList(al.Size() / 2) // estimate half match
		for i := 0; i < al.Size(); i++ {
			elem := al.elements[i]
			if v.evaluateBlock(block, []Value{elem}) == True {
				result.Add(elem)
			}
		}
		return v.registerArrayList(result)
	}
	c.AddMethod1(vm.Selectors, "select:", selectFn)
	c.AddMethod1(vm.Selectors, "primSelect:", selectFn)

	// reject: block - return new ArrayList excluding matching elements
	rejectFn := func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil {
			return v.registerArrayList(createArrayList(0))
		}
		result := createArrayList(al.Size() / 2)
		for i := 0; i < al.Size(); i++ {
			elem := al.elements[i]
			if v.evaluateBlock(block, []Value{elem}) != True {
				result.Add(elem)
			}
		}
		return v.registerArrayList(result)
	}
	c.AddMethod1(vm.Selectors, "reject:", rejectFn)
	c.AddMethod1(vm.Selectors, "primReject:", rejectFn)

	// inject:into: - accumulate
	injectIntoFn := func(vmPtr interface{}, recv Value, initial, block Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil {
			return initial
		}
		result := initial
		for i := 0; i < al.Size(); i++ {
			result = v.evaluateBlock(block, []Value{result, al.elements[i]})
		}
		return result
	}
	c.AddMethod2(vm.Selectors, "inject:into:", injectIntoFn)
	c.AddMethod2(vm.Selectors, "primInject:into:", injectIntoFn)

	// detect: block
	detectFn := func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil {
			return Nil
		}
		for i := 0; i < al.Size(); i++ {
			elem := al.elements[i]
			if v.evaluateBlock(block, []Value{elem}) == True {
				return elem
			}
		}
		return Nil
	}
	c.AddMethod1(vm.Selectors, "detect:", detectFn)
	c.AddMethod1(vm.Selectors, "primDetect:", detectFn)

	// detect:ifNone:
	detectIfNoneFn := func(vmPtr interface{}, recv Value, block, noneBlock Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil {
			return v.evaluateBlock(noneBlock, nil)
		}
		for i := 0; i < al.Size(); i++ {
			elem := al.elements[i]
			if v.evaluateBlock(block, []Value{elem}) == True {
				return elem
			}
		}
		return v.evaluateBlock(noneBlock, nil)
	}
	c.AddMethod2(vm.Selectors, "detect:ifNone:", detectIfNoneFn)
	c.AddMethod2(vm.Selectors, "primDetect:ifNone:", detectIfNoneFn)

	// printString
	printStringFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil {
			return v.registry.NewStringValue("ArrayList()")
		}
		var sb strings.Builder
		sb.WriteString("ArrayList(")
		for i := 0; i < al.Size(); i++ {
			if i > 0 {
				sb.WriteString(" ")
			}
			s := v.Send(al.elements[i], "printString", nil)
			sb.WriteString(v.valueToString(s))
		}
		sb.WriteString(")")
		return v.registry.NewStringValue(sb.String())
	}
	c.AddMethod0(vm.Selectors, "printString", printStringFn)
	c.AddMethod0(vm.Selectors, "primPrintString", printStringFn)

	// copy - return a new ArrayList with the same elements
	copyFn := func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil {
			return v.registerArrayList(createArrayList(0))
		}
		result := createArrayList(al.Size())
		result.elements = append(result.elements, al.elements...)
		return v.registerArrayList(result)
	}
	c.AddMethod0(vm.Selectors, "copy", copyFn)
	c.AddMethod0(vm.Selectors, "primCopy", copyFn)

	// sort: aBlock - in-place sort using comparison block
	sortFn := func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		al := v.getArrayList(recv)
		if al == nil || al.Size() <= 1 {
			return recv
		}
		// Simple insertion sort using the comparison block
		// (for large lists, could use a more efficient sort)
		n := al.Size()
		for i := 1; i < n; i++ {
			key := al.elements[i]
			j := i - 1
			for j >= 0 {
				cmp := v.evaluateBlock(block, []Value{al.elements[j], key})
				if !cmp.IsSmallInt() || cmp.SmallInt() <= 0 {
					break
				}
				al.elements[j+1] = al.elements[j]
				j--
			}
			al.elements[j+1] = key
		}
		return recv
	}
	c.AddMethod1(vm.Selectors, "sort:", sortFn)
	c.AddMethod1(vm.Selectors, "primSort:", sortFn)
}
