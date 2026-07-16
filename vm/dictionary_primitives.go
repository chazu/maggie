package vm

import "sync"

// ---------------------------------------------------------------------------
// Dictionary Storage: Native Go maps wrapped for the VM
// ---------------------------------------------------------------------------

// DictionaryObject represents a Maggie dictionary (key-value map).
// Uses a Go map internally for O(1) lookup.
//
// All access goes through the locked methods below: dictionaries stored in
// globals are reachable from concurrent server requests (RunIsolated
// interpreters) and forked processes, and a bare concurrent map write is an
// uncatchable Go runtime fatal. Iteration helpers return snapshots so
// callers never hold the lock while running Maggie code (blocks).
type DictionaryObject struct {
	mu   sync.RWMutex
	data map[uint64]Value // key hash -> value
	keys map[uint64]Value // key hash -> key (for iteration)
}

// DictEntry is one key-value pair in an iteration snapshot.
type DictEntry struct {
	Hash  uint64
	Key   Value
	Value Value
}

// NewDictionaryObject creates an empty dictionary object.
func NewDictionaryObject() *DictionaryObject {
	return &DictionaryObject{
		data: make(map[uint64]Value),
		keys: make(map[uint64]Value),
	}
}

// GetByHash returns the value stored under a key hash.
func (d *DictionaryObject) GetByHash(h uint64) (Value, bool) {
	d.mu.RLock()
	v, ok := d.data[h]
	d.mu.RUnlock()
	return v, ok
}

// KeyByHash returns the key object stored under a key hash.
func (d *DictionaryObject) KeyByHash(h uint64) (Value, bool) {
	d.mu.RLock()
	k, ok := d.keys[h]
	d.mu.RUnlock()
	return k, ok
}

// SetByHash stores a key-value pair under a key hash.
func (d *DictionaryObject) SetByHash(h uint64, key, value Value) {
	d.mu.Lock()
	d.data[h] = value
	d.keys[h] = key
	d.mu.Unlock()
}

// Put stores a key-value pair, hashing the key with hashValue. Convenience
// for Go code building result dictionaries.
func (d *DictionaryObject) Put(or *ObjectRegistry, key, value Value) {
	d.SetByHash(hashValue(or, key), key, value)
}

// DeleteByHash removes the entry under a key hash, returning the removed
// value and whether it was present.
func (d *DictionaryObject) DeleteByHash(h uint64) (Value, bool) {
	d.mu.Lock()
	v, ok := d.data[h]
	if ok {
		delete(d.data, h)
		delete(d.keys, h)
	}
	d.mu.Unlock()
	return v, ok
}

// Size returns the number of entries.
func (d *DictionaryObject) Size() int {
	d.mu.RLock()
	n := len(d.data)
	d.mu.RUnlock()
	return n
}

// Entries returns a snapshot of all entries. Iterate the snapshot instead
// of the live maps so Maggie blocks can run (and even mutate the receiver)
// without holding the lock.
func (d *DictionaryObject) Entries() []DictEntry {
	d.mu.RLock()
	entries := make([]DictEntry, 0, len(d.data))
	for h, v := range d.data {
		entries = append(entries, DictEntry{Hash: h, Key: d.keys[h], Value: v})
	}
	d.mu.RUnlock()
	return entries
}

// dictionaryIDOffset marks the top of the legacy symbol-id space. Retained so
// the real-symbol id range stays bounded (see stringIDOffset); Dictionaries
// themselves are now pointer-carrying heap Values.
const dictionaryIDOffset uint32 = 0xC0000000

// IsDictionaryValue returns true if the value is a heap dictionary object.
func IsDictionaryValue(v Value) bool {
	return v.ptr != nil && v.hi == kindDictionary
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
	// BigIntegers must hash by value, not by their NaN-boxed handle: two
	// distinct BigInt objects with equal value have different handles, so
	// hashing the raw bits made value-keys (e.g. `d at: 100 factorial put: …`)
	// impossible to look up. FNV-1a over the magnitude bytes plus the sign.
	if IsBigIntValue(v) {
		if bi := or.GetBigInt(v); bi != nil {
			var h uint64 = 14695981039346656037
			if bi.Value.Sign() < 0 {
				h ^= 1
				h *= 1099511628211
			}
			for _, b := range bi.Value.Bytes() {
				h ^= uint64(b)
				h *= 1099511628211
			}
			return h
		}
	}
	// For other values, use the raw bits as the hash
	// This works because Values are NaN-boxed and unique per value
	return v.hi ^ uint64(uintptr(v.ptr))
}

// registerDictionaryPrimitives registers Dictionary primitives on the VM.
func (vm *VM) registerDictionaryPrimitives() {
	c := vm.DictionaryClass

	// Class-side new - create new empty dictionary (class method)
	c.AddClassMethod0(vm.Selectors, "new", func(v *VM, recv Value) Value {
		return v.registry.NewDictionaryValue()
	})

	// at: - get value for key, returns nil if not found
	c.AddMethod1(vm.Selectors, "at:", func(v *VM, recv Value, key Value) Value {
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return v.SignalPrimitiveError("at:", "receiver is not a Dictionary")
		}
		h := hashValue(v.registry, key)
		if val, ok := dict.GetByHash(h); ok {
			return val
		}
		return Nil
	})

	// at:put: - set value for key, returns the value
	c.AddMethod2(vm.Selectors, "at:put:", func(v *VM, recv Value, key, value Value) Value {
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			// Signal like at:/at:ifAbsent: rather than silently returning the
			// value on a non-Dictionary receiver.
			return v.SignalPrimitiveError("at:put:", "receiver is not a Dictionary")
		}
		h := hashValue(v.registry, key)
		dict.SetByHash(h, key, value)
		return value
	})

	// at:ifAbsent: - get value or evaluate block if absent
	c.AddMethod2(vm.Selectors, "at:ifAbsent:", func(v *VM, recv Value, key, block Value) Value {
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return v.SignalPrimitiveError("at:ifAbsent:", "receiver is not a Dictionary")
		}
		h := hashValue(v.registry, key)
		if val, ok := dict.GetByHash(h); ok {
			return val
		}
		// Evaluate the block
		return v.Send(block, "value", nil)
	})

	// at:ifPresent: - evaluate block with value if key exists
	c.AddMethod2(vm.Selectors, "at:ifPresent:", func(v *VM, recv Value, key, block Value) Value {
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return v.SignalPrimitiveError("at:ifPresent:", "receiver is not a Dictionary")
		}
		h := hashValue(v.registry, key)
		if val, ok := dict.GetByHash(h); ok {
			// Evaluate the block with the value
			return v.Send(block, "value:", []Value{val})
		}
		return Nil
	})

	// includesKey: - check if key exists
	c.AddMethod1(vm.Selectors, "includesKey:", func(v *VM, recv Value, key Value) Value {
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return False
		}
		h := hashValue(v.registry, key)
		if _, ok := dict.GetByHash(h); ok {
			return True
		}
		return False
	})

	// size - number of key-value pairs
	c.AddMethod0(vm.Selectors, "size", func(v *VM, recv Value) Value {
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(dict.Size()))
	})

	// isEmpty - true if dictionary has no entries
	c.AddMethod0(vm.Selectors, "isEmpty", func(v *VM, recv Value) Value {
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return True
		}
		if dict.Size() == 0 {
			return True
		}
		return False
	})

	// keys - return an array of all keys
	c.AddMethod0(vm.Selectors, "keys", func(v *VM, recv Value) Value {
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return v.NewArray(0)
		}
		entries := dict.Entries()
		keys := make([]Value, 0, len(entries))
		for _, e := range entries {
			keys = append(keys, e.Key)
		}
		return v.NewArrayWithElements(keys)
	})

	// values - return an array of all values
	c.AddMethod0(vm.Selectors, "values", func(v *VM, recv Value) Value {
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return v.NewArray(0)
		}
		entries := dict.Entries()
		values := make([]Value, 0, len(entries))
		for _, e := range entries {
			values = append(values, e.Value)
		}
		return v.NewArrayWithElements(values)
	})

	// do: - iterate over values with a block (snapshot semantics: mutation
	// from inside the block does not affect this iteration)
	c.AddMethod1(vm.Selectors, "do:", func(v *VM, recv Value, block Value) Value {
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return recv
		}
		for _, e := range dict.Entries() {
			v.Send(block, "value:", []Value{e.Value})
		}
		return recv
	})

	// keysAndValuesDo: - iterate over key-value pairs with a block (snapshot
	// semantics: mutation from inside the block does not affect this iteration)
	c.AddMethod1(vm.Selectors, "keysAndValuesDo:", func(v *VM, recv Value, block Value) Value {
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return recv
		}
		for _, e := range dict.Entries() {
			v.Send(block, "value:value:", []Value{e.Key, e.Value})
		}
		return recv
	})

	// removeKey: - remove key and return its value, or nil if not found
	c.AddMethod1(vm.Selectors, "removeKey:", func(v *VM, recv Value, key Value) Value {
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return v.SignalPrimitiveError("removeKey:", "receiver is not a Dictionary")
		}
		h := hashValue(v.registry, key)
		if val, ok := dict.DeleteByHash(h); ok {
			return val
		}
		return Nil
	})

	// removeKey:ifAbsent: - remove key and return value, or evaluate block if absent
	c.AddMethod2(vm.Selectors, "removeKey:ifAbsent:", func(v *VM, recv Value, key, block Value) Value {
		dict := v.registry.GetDictionaryObject(recv)
		if dict == nil {
			return v.Send(block, "value", nil)
		}
		h := hashValue(v.registry, key)
		if val, ok := dict.DeleteByHash(h); ok {
			return val
		}
		return v.Send(block, "value", nil)
	})
}
