package vm

// ---------------------------------------------------------------------------
// Set Primitives
// ---------------------------------------------------------------------------
// Set is backed by a Dictionary. The instance variable 'dict' (slot 0)
// holds a Dictionary where keys are the set elements and values are true.

func (vm *VM) registerSetPrimitives() {
	c := vm.SetClass

	// Class-side new - create a new Set with an initialized Dictionary
	c.AddClassMethod0(vm.Selectors, "new", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		var cls *Class
		if IsClassValue(recv) {
			cls = v.GetClassFromValue(recv)
		}
		if cls == nil {
			cls = v.SetClass
		}
		instance := cls.NewInstance()
		v.KeepAlive(instance)
		// Initialize the dict instance variable (slot 0)
		instance.SetSlot(0, v.NewDictionary())
		return instance.ToValue()
	})

	// add: - add an element to the set
	c.AddMethod1(vm.Selectors, "add:", func(vmPtr interface{}, recv Value, elem Value) Value {
		v := vmPtr.(*VM)
		if !recv.IsObject() {
			return elem
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return elem
		}
		dict := obj.GetSlot(0) // dict instance variable
		v.DictionaryAtPut(dict, elem, True)
		return elem
	})

	// remove: - remove an element from the set
	c.AddMethod1(vm.Selectors, "remove:", func(vmPtr interface{}, recv Value, elem Value) Value {
		v := vmPtr.(*VM)
		if !recv.IsObject() {
			return elem
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return elem
		}
		dict := obj.GetSlot(0)
		v.Send(dict, "removeKey:", []Value{elem})
		return elem
	})

	// includes: - check if element is in the set
	c.AddMethod1(vm.Selectors, "includes:", func(vmPtr interface{}, recv Value, elem Value) Value {
		v := vmPtr.(*VM)
		if !recv.IsObject() {
			return False
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return False
		}
		dict := obj.GetSlot(0)
		return v.Send(dict, "includesKey:", []Value{elem})
	})

	// size - number of elements
	c.AddMethod0(vm.Selectors, "size", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if !recv.IsObject() {
			return FromSmallInt(0)
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return FromSmallInt(0)
		}
		dict := obj.GetSlot(0)
		return v.Send(dict, "size", nil)
	})

	// isEmpty - check if set is empty
	c.AddMethod0(vm.Selectors, "isEmpty", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if !recv.IsObject() {
			return True
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return True
		}
		dict := obj.GetSlot(0)
		return v.Send(dict, "isEmpty", nil)
	})

	// do: - iterate over elements (keys of the internal dictionary)
	c.AddMethod1(vm.Selectors, "do:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		if !recv.IsObject() {
			return Nil
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		dict := obj.GetSlot(0)
		// Get keys array from dictionary, iterate over it
		keys := v.Send(dict, "keys", nil)
		if !keys.IsObject() {
			return Nil
		}
		keysObj := ObjectFromValue(keys)
		if keysObj == nil {
			return Nil
		}
		n := keysObj.NumSlots()
		for i := 0; i < n; i++ {
			key := keysObj.GetSlot(i)
			v.evaluateBlock(block, []Value{key})
		}
		return Nil
	})
}
