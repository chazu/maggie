package vm

// ---------------------------------------------------------------------------
// Dictionary Storage: Native Go maps wrapped for the VM
// ---------------------------------------------------------------------------

// DictionaryObject represents a Maggie dictionary (key-value map).
// Uses a Go map internally for O(1) lookup.
type DictionaryObject struct {
	Data map[uint64]Value // Key hash -> Value
	Keys map[uint64]Value // Key hash -> Key (for iteration)
}

// dictionaryIDOffset is the starting offset for dictionary IDs.
const dictionaryIDOffset uint32 = 0xC0000000

// IsDictionaryValue returns true if the value is a dictionary object.
func IsDictionaryValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return id >= dictionaryIDOffset
}

// HashValue computes a hash for a Maggie Value (exported for testing).
func HashValue(or *ObjectRegistry, v Value) uint64 {
	return hashValue(or, v)
}

// hashValue computes a hash for a Maggie Value.
// This is used as the key in the underlying Go map.
func hashValue(or *ObjectRegistry, v Value) uint64 {
	// For strings, hash the content since different string objects
	// with the same content should hash to the same key.
	if IsStringValue(v) {
		content := or.GetStringContent(v)
		// FNV-1a hash for strings
		var h uint64 = 14695981039346656037 // FNV offset basis
		for i := 0; i < len(content); i++ {
			h ^= uint64(content[i])
			h *= 1099511628211 // FNV prime
		}
		return h
	}
	// For other values, use the raw bits as the hash
	// This works because Values are NaN-boxed and unique per value
	return uint64(v)
}

// registerDictionaryPrimitives registers Dictionary primitives on the VM.
func (vm *VM) registerDictionaryPrimitives() {
	c := vm.DictionaryClass

	// Class-side new - create new empty dictionary (class method)
	c.AddClassMethod0(vm.Selectors, "new", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		return v.registry.NewDictionaryValue()
	})

	// at: - get value for key, returns nil if not found
	c.AddMethod1(vm.Selectors, "at:", func(vmPtr interface{}, recv Value, key Value) Value {
		v := vmPtr.(*VM)
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return Nil
		}
		h := hashValue(v.registry, key)
		if val, ok := dict.Data[h]; ok {
			return val
		}
		return Nil
	})

	// at:put: - set value for key, returns the value
	c.AddMethod2(vm.Selectors, "at:put:", func(vmPtr interface{}, recv Value, key, value Value) Value {
		v := vmPtr.(*VM)
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return value
		}
		h := hashValue(v.registry, key)
		dict.Data[h] = value
		dict.Keys[h] = key
		return value
	})

	// at:ifAbsent: - get value or evaluate block if absent
	c.AddMethod2(vm.Selectors, "at:ifAbsent:", func(vmPtr interface{}, recv Value, key, block Value) Value {
		v := vmPtr.(*VM)
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return Nil
		}
		h := hashValue(v.registry, key)
		if val, ok := dict.Data[h]; ok {
			return val
		}
		// Evaluate the block
		return v.Send(block, "value", nil)
	})

	// at:ifPresent: - evaluate block with value if key exists
	c.AddMethod2(vm.Selectors, "at:ifPresent:", func(vmPtr interface{}, recv Value, key, block Value) Value {
		v := vmPtr.(*VM)
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return Nil
		}
		h := hashValue(v.registry, key)
		if val, ok := dict.Data[h]; ok {
			// Evaluate the block with the value
			return v.Send(block, "value:", []Value{val})
		}
		return Nil
	})

	// includesKey: - check if key exists
	c.AddMethod1(vm.Selectors, "includesKey:", func(vmPtr interface{}, recv Value, key Value) Value {
		v := vmPtr.(*VM)
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return False
		}
		h := hashValue(v.registry, key)
		if _, ok := dict.Data[h]; ok {
			return True
		}
		return False
	})

	// size - number of key-value pairs
	c.AddMethod0(vm.Selectors, "size", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(len(dict.Data)))
	})

	// isEmpty - true if dictionary has no entries
	c.AddMethod0(vm.Selectors, "isEmpty", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return True
		}
		if len(dict.Data) == 0 {
			return True
		}
		return False
	})

	// keys - return an array of all keys
	c.AddMethod0(vm.Selectors, "keys", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return v.NewArray(0)
		}
		keys := make([]Value, 0, len(dict.Keys))
		for _, key := range dict.Keys {
			keys = append(keys, key)
		}
		return v.NewArrayWithElements(keys)
	})

	// values - return an array of all values
	c.AddMethod0(vm.Selectors, "values", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return v.NewArray(0)
		}
		values := make([]Value, 0, len(dict.Data))
		for _, val := range dict.Data {
			values = append(values, val)
		}
		return v.NewArrayWithElements(values)
	})

	// do: - iterate over values with a block
	c.AddMethod1(vm.Selectors, "do:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return recv
		}
		for _, val := range dict.Data {
			v.Send(block, "value:", []Value{val})
		}
		return recv
	})

	// keysAndValuesDo: - iterate over key-value pairs with a block
	c.AddMethod1(vm.Selectors, "keysAndValuesDo:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return recv
		}
		for h, val := range dict.Data {
			key := dict.Keys[h]
			v.Send(block, "value:value:", []Value{key, val})
		}
		return recv
	})

	// removeKey: - remove key and return its value, or nil if not found
	c.AddMethod1(vm.Selectors, "removeKey:", func(vmPtr interface{}, recv Value, key Value) Value {
		v := vmPtr.(*VM)
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return Nil
		}
		h := hashValue(v.registry, key)
		if val, ok := dict.Data[h]; ok {
			delete(dict.Data, h)
			delete(dict.Keys, h)
			return val
		}
		return Nil
	})

	// removeKey:ifAbsent: - remove key and return value, or evaluate block if absent
	c.AddMethod2(vm.Selectors, "removeKey:ifAbsent:", func(vmPtr interface{}, recv Value, key, block Value) Value {
		v := vmPtr.(*VM)
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return v.Send(block, "value", nil)
		}
		h := hashValue(v.registry, key)
		if val, ok := dict.Data[h]; ok {
			delete(dict.Data, h)
			delete(dict.Keys, h)
			return val
		}
		return v.Send(block, "value", nil)
	})
}
