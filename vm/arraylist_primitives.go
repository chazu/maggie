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
	newFn := func(v *VM, recv Value) Value {
		al := createArrayList(8)
		return v.registerArrayList(al)
	}
	c.AddClassMethod0(vm.Selectors, "new", newFn)
	c.AddClassMethod0(vm.Selectors, "primNew", newFn)

	// ArrayList class>>new: capacity - create empty with given capacity
	newCapFn := func(v *VM, recv Value, cap Value) Value {
		capacity := 8
		if cap.IsSmallInt() {
			capacity = int(cap.SmallInt())
			if capacity < 0 {
				capacity = 0
			}
			if capacity > MaxArrayElements {
				return v.SignalPrimitiveError("ArrayList new:", "requested capacity exceeds maximum")
			}
		}
		al := createArrayList(capacity)
		return v.registerArrayList(al)
	}
	c.AddClassMethod1(vm.Selectors, "new:", newCapFn)
	c.AddClassMethod1(vm.Selectors, "primNew:", newCapFn)

	// ArrayList class>>withAll: anArray - create from existing array
	withAllFn := func(v *VM, recv Value, arr Value) Value {
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
	addFn := func(v *VM, recv Value, elem Value) Value {
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
	addAllFn := func(v *VM, recv Value, coll Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return recv
		}
		// Try as ArrayList first
		if other := v.getArrayList(coll); other != nil {
			// Snapshot handles the aliasing case (list addAll: list) safely.
			al.AppendSlice(other.Snapshot())
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
	atFn := func(v *VM, recv Value, index Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return v.SignalPrimitiveError("at:", "receiver is not an ArrayList")
		}
		if !index.IsSmallInt() {
			return v.SignalTypeError("at:", 1, "SmallInteger", index)
		}
		// Signal on out-of-bounds like Array>>at:, rather than silently
		// returning nil — a 1-based index must be in [1, size].
		i := index.SmallInt()
		if i < 1 || int(i) > al.Size() {
			return v.SignalSubscriptOutOfBounds("at:", i, al.Size())
		}
		return al.At(int(i - 1))
	}
	c.AddMethod1(vm.Selectors, "at:", atFn)
	c.AddMethod1(vm.Selectors, "primAt:", atFn)

	// at:put: - indexed set, returns value
	atPutFn := func(v *VM, recv Value, index, value Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return v.SignalPrimitiveError("at:put:", "receiver is not an ArrayList")
		}
		if !index.IsSmallInt() {
			return v.SignalTypeError("at:put:", 1, "SmallInteger", index)
		}
		i := index.SmallInt()
		if i < 1 || int(i) > al.Size() {
			return v.SignalSubscriptOutOfBounds("at:put:", i, al.Size())
		}
		al.AtPut(int(i-1), value)
		return value
	}
	c.AddMethod2(vm.Selectors, "at:put:", atPutFn)
	c.AddMethod2(vm.Selectors, "primAt:put:", atPutFn)

	// size
	sizeFn := func(v *VM, recv Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(al.Size()))
	}
	c.AddMethod0(vm.Selectors, "size", sizeFn)
	c.AddMethod0(vm.Selectors, "primSize", sizeFn)

	// capacity
	capFn := func(v *VM, recv Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(al.Capacity()))
	}
	c.AddMethod0(vm.Selectors, "capacity", capFn)
	c.AddMethod0(vm.Selectors, "primCapacity", capFn)

	// removeLast
	removeLastFn := func(v *VM, recv Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return v.SignalPrimitiveError("removeLast", "receiver is not an ArrayList")
		}
		return al.RemoveLast()
	}
	c.AddMethod0(vm.Selectors, "removeLast", removeLastFn)
	c.AddMethod0(vm.Selectors, "primRemoveLast", removeLastFn)

	// removeAt: index
	removeAtFn := func(v *VM, recv Value, index Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return v.SignalPrimitiveError("removeAt:", "receiver is not an ArrayList")
		}
		if !index.IsSmallInt() {
			return v.SignalTypeError("removeAt:", 1, "SmallInteger", index)
		}
		return al.RemoveAt(int(index.SmallInt() - 1))
	}
	c.AddMethod1(vm.Selectors, "removeAt:", removeAtFn)
	c.AddMethod1(vm.Selectors, "primRemoveAt:", removeAtFn)

	// clear - returns self
	clearFn := func(v *VM, recv Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return recv
		}
		al.Clear()
		return recv
	}
	c.AddMethod0(vm.Selectors, "clear", clearFn)
	c.AddMethod0(vm.Selectors, "primClear", clearFn)

	// first — raises on empty, matching Array>>first (a missing first
	// element is a programmer error, not a nil; L-15)
	firstFn := func(v *VM, recv Value) Value {
		al := v.getArrayList(recv)
		if al == nil || al.Size() == 0 {
			return v.SignalSubscriptOutOfBounds("first", 1, 0)
		}
		return al.At(0)
	}
	c.AddMethod0(vm.Selectors, "first", firstFn)
	c.AddMethod0(vm.Selectors, "primFirst", firstFn)

	// last — raises on empty, matching Array>>last
	lastFn := func(v *VM, recv Value) Value {
		al := v.getArrayList(recv)
		if al == nil || al.Size() == 0 {
			return v.SignalSubscriptOutOfBounds("last", 1, 0)
		}
		return al.At(al.Size() - 1)
	}
	c.AddMethod0(vm.Selectors, "last", lastFn)
	c.AddMethod0(vm.Selectors, "primLast", lastFn)

	// isEmpty
	isEmptyFn := func(v *VM, recv Value) Value {
		al := v.getArrayList(recv)
		if al == nil || al.Size() == 0 {
			return True
		}
		return False
	}
	c.AddMethod0(vm.Selectors, "isEmpty", isEmptyFn)
	c.AddMethod0(vm.Selectors, "primIsEmpty", isEmptyFn)

	// notEmpty
	notEmptyFn := func(v *VM, recv Value) Value {
		al := v.getArrayList(recv)
		if al == nil || al.Size() == 0 {
			return False
		}
		return True
	}
	c.AddMethod0(vm.Selectors, "notEmpty", notEmptyFn)
	c.AddMethod0(vm.Selectors, "primNotEmpty", notEmptyFn)

	// includes: element
	includesFn := func(v *VM, recv Value, elem Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return False
		}
		for _, e := range al.Snapshot() {
			if v.Send(e, "=", []Value{elem}) == True {
				return True
			}
		}
		return False
	}
	c.AddMethod1(vm.Selectors, "includes:", includesFn)
	c.AddMethod1(vm.Selectors, "primIncludes:", includesFn)

	// indexOf: element
	indexOfFn := func(v *VM, recv Value, elem Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return FromSmallInt(0)
		}
		for i, e := range al.Snapshot() {
			if v.Send(e, "=", []Value{elem}) == True {
				return FromSmallInt(int64(i + 1))
			}
		}
		return FromSmallInt(0)
	}
	c.AddMethod1(vm.Selectors, "indexOf:", indexOfFn)
	c.AddMethod1(vm.Selectors, "primIndexOf:", indexOfFn)

	// asArray - convert to fixed-size Array
	asArrayFn := func(v *VM, recv Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return v.NewArray(0)
		}
		// Copy elements to avoid sharing
		return v.NewArrayWithElements(al.Snapshot())
	}
	c.AddMethod0(vm.Selectors, "asArray", asArrayFn)
	c.AddMethod0(vm.Selectors, "primAsArray", asArrayFn)

	// do: block - iterate calling block with each element
	doFn := func(v *VM, recv Value, block Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return v.SignalPrimitiveError("do:", "receiver is not an ArrayList")
		}
		// Snapshot — safe if elements are added/removed during iteration
		for _, elem := range al.Snapshot() {
			v.evaluateBlock(block, []Value{elem})
		}
		return Nil
	}
	c.AddMethod1(vm.Selectors, "do:", doFn)
	c.AddMethod1(vm.Selectors, "primDo:", doFn)

	// collect: block - return new ArrayList with transformed elements
	collectFn := func(v *VM, recv Value, block Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return v.registerArrayList(createArrayList(0))
		}
		snapshot := al.Snapshot()
		result := createArrayList(len(snapshot))
		for _, elem := range snapshot {
			val := v.evaluateBlock(block, []Value{elem})
			result.Add(val)
		}
		return v.registerArrayList(result)
	}
	c.AddMethod1(vm.Selectors, "collect:", collectFn)
	c.AddMethod1(vm.Selectors, "primCollect:", collectFn)

	// select: block - return new ArrayList with matching elements
	selectFn := func(v *VM, recv Value, block Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return v.registerArrayList(createArrayList(0))
		}
		snapshot := al.Snapshot()
		result := createArrayList(len(snapshot) / 2) // estimate half match
		for _, elem := range snapshot {
			if v.evaluateBlock(block, []Value{elem}) == True {
				result.Add(elem)
			}
		}
		return v.registerArrayList(result)
	}
	c.AddMethod1(vm.Selectors, "select:", selectFn)
	c.AddMethod1(vm.Selectors, "primSelect:", selectFn)

	// reject: block - return new ArrayList excluding matching elements
	rejectFn := func(v *VM, recv Value, block Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return v.registerArrayList(createArrayList(0))
		}
		snapshot := al.Snapshot()
		result := createArrayList(len(snapshot) / 2)
		for _, elem := range snapshot {
			if v.evaluateBlock(block, []Value{elem}) != True {
				result.Add(elem)
			}
		}
		return v.registerArrayList(result)
	}
	c.AddMethod1(vm.Selectors, "reject:", rejectFn)
	c.AddMethod1(vm.Selectors, "primReject:", rejectFn)

	// inject:into: - accumulate
	injectIntoFn := func(v *VM, recv Value, initial, block Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return initial
		}
		result := initial
		for _, elem := range al.Snapshot() {
			result = v.evaluateBlock(block, []Value{result, elem})
		}
		return result
	}
	c.AddMethod2(vm.Selectors, "inject:into:", injectIntoFn)
	c.AddMethod2(vm.Selectors, "primInject:into:", injectIntoFn)

	// detect: block
	detectFn := func(v *VM, recv Value, block Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return v.SignalPrimitiveError("detect:", "receiver is not an ArrayList")
		}
		for _, elem := range al.Snapshot() {
			if v.evaluateBlock(block, []Value{elem}) == True {
				return elem
			}
		}
		return Nil
	}
	c.AddMethod1(vm.Selectors, "detect:", detectFn)
	c.AddMethod1(vm.Selectors, "primDetect:", detectFn)

	// detect:ifNone:
	detectIfNoneFn := func(v *VM, recv Value, block, noneBlock Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return v.evaluateBlock(noneBlock, nil)
		}
		for _, elem := range al.Snapshot() {
			if v.evaluateBlock(block, []Value{elem}) == True {
				return elem
			}
		}
		return v.evaluateBlock(noneBlock, nil)
	}
	c.AddMethod2(vm.Selectors, "detect:ifNone:", detectIfNoneFn)
	c.AddMethod2(vm.Selectors, "primDetect:ifNone:", detectIfNoneFn)

	// printString
	printStringFn := func(v *VM, recv Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return v.registry.NewStringValue("ArrayList()")
		}
		var sb strings.Builder
		sb.WriteString("ArrayList(")
		for i, elem := range al.Snapshot() {
			if i > 0 {
				sb.WriteString(" ")
			}
			s := v.Send(elem, "printString", nil)
			sb.WriteString(v.valueToString(s))
		}
		sb.WriteString(")")
		return v.registry.NewStringValue(sb.String())
	}
	c.AddMethod0(vm.Selectors, "printString", printStringFn)
	c.AddMethod0(vm.Selectors, "primPrintString", printStringFn)

	// copy - return a new ArrayList with the same elements
	copyFn := func(v *VM, recv Value) Value {
		al := v.getArrayList(recv)
		if al == nil {
			return v.registerArrayList(createArrayList(0))
		}
		snapshot := al.Snapshot()
		result := createArrayList(len(snapshot))
		result.AppendSlice(snapshot)
		return v.registerArrayList(result)
	}
	c.AddMethod0(vm.Selectors, "copy", copyFn)
	c.AddMethod0(vm.Selectors, "primCopy", copyFn)

	// sort: aBlock - in-place sort using comparison block
	sortFn := func(v *VM, recv Value, block Value) Value {
		al := v.getArrayList(recv)
		if al == nil || al.Size() <= 1 {
			return recv
		}
		// Sort a snapshot (the comparison block is arbitrary Maggie code, so
		// the lock cannot be held across it), then write the result back.
		// Simple insertion sort using the comparison block
		// (for large lists, could use a more efficient sort)
		elems := al.Snapshot()
		n := len(elems)
		for i := 1; i < n; i++ {
			key := elems[i]
			j := i - 1
			for j >= 0 {
				cmp := v.evaluateBlock(block, []Value{elems[j], key})
				if !cmp.IsSmallInt() || cmp.SmallInt() <= 0 {
					break
				}
				elems[j+1] = elems[j]
				j--
			}
			elems[j+1] = key
		}
		al.ReplaceAll(elems)
		return recv
	}
	c.AddMethod1(vm.Selectors, "sort:", sortFn)
	c.AddMethod1(vm.Selectors, "primSort:", sortFn)
}
