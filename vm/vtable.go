package vm

// VTable holds the method dispatch table for a class.
//
// Methods are stored in a local array indexed by selector ID. A flattened
// array (flat) collapses the entire inheritance chain into a single slice
// for O(1) lookup. The flat array is rebuilt lazily on first Lookup after
// any mutation marks the VTable dirty.
//
// Note: The VTable struct is forward-declared in object.go to break
// the import cycle. This file contains the method implementations.

// Lookup finds a method by selector ID using the flattened dispatch table.
// Returns nil if no method is found (triggers doesNotUnderstand:).
func (vt *VTable) Lookup(selector int) Method {
	if vt.flat == nil || vt.dirty {
		vt.rebuild()
	}
	if selector >= 0 && selector < len(vt.flat) {
		return vt.flat[selector]
	}
	return nil
}

// rebuild collapses the entire inheritance chain into vt.flat.
// Parent methods are added first; child methods override.
func (vt *VTable) rebuild() {
	// Find max selector across the chain
	maxLen := 0
	for v := vt; v != nil; v = v.parent {
		if len(v.methods) > maxLen {
			maxLen = len(v.methods)
		}
	}

	// Collect chain so we can iterate parent-first
	var chain []*VTable
	for v := vt; v != nil; v = v.parent {
		chain = append(chain, v)
	}

	// Fill bottom-up: parent methods first, child overrides
	flat := make([]Method, maxLen)
	for i := len(chain) - 1; i >= 0; i-- {
		for sel, m := range chain[i].methods {
			if m != nil {
				flat[sel] = m
			}
		}
	}

	vt.flat = flat
	vt.dirty = false
}

// markDirty invalidates the flat array so the next Lookup triggers a rebuild.
func (vt *VTable) markDirty() {
	vt.dirty = true
	vt.flat = nil // Release stale flat array for GC
}

// LookupLocal finds a method by selector ID in this vtable only.
// Does not check parent vtables.
func (vt *VTable) LookupLocal(selector int) Method {
	if selector >= 0 && selector < len(vt.methods) {
		return vt.methods[selector]
	}
	return nil
}

// AddMethod adds or replaces a method at the given selector ID.
// The local methods array is grown as needed.
func (vt *VTable) AddMethod(selector int, method Method) {
	if selector >= len(vt.methods) {
		// Grow the methods slice
		newMethods := make([]Method, selector+1)
		copy(newMethods, vt.methods)
		vt.methods = newMethods
	}
	vt.methods[selector] = method
	vt.markDirty()
}

// RemoveMethod removes a method at the given selector ID.
func (vt *VTable) RemoveMethod(selector int) {
	if selector >= 0 && selector < len(vt.methods) {
		vt.methods[selector] = nil
		vt.markDirty()
	}
}

// HasMethod returns true if this vtable (not parents) has a method for selector.
func (vt *VTable) HasMethod(selector int) bool {
	return vt.LookupLocal(selector) != nil
}

// Parent returns the parent vtable (for inheritance).
func (vt *VTable) Parent() *VTable {
	return vt.parent
}

// SetParent sets the parent vtable.
func (vt *VTable) SetParent(parent *VTable) {
	vt.parent = parent
	vt.markDirty()
}

// Class returns the class this vtable belongs to.
func (vt *VTable) Class() *Class {
	return vt.class
}

// MethodCount returns the number of method slots (including nil slots).
func (vt *VTable) MethodCount() int {
	return len(vt.methods)
}

// LocalMethods returns all non-nil methods defined locally in this vtable.
// Returns a map of selector ID to method. Does not include inherited methods.
func (vt *VTable) LocalMethods() map[int]Method {
	result := make(map[int]Method)
	for i, m := range vt.methods {
		if m != nil {
			result[i] = m
		}
	}
	return result
}

// NewVTable creates a new vtable for a class.
func NewVTable(class *Class, parent *VTable) *VTable {
	return &VTable{
		class:   class,
		parent:  parent,
		methods: make([]Method, 0, 32), // Pre-allocate for typical class
	}
}

// NewVTableWithCapacity creates a new vtable with a specific method capacity.
func NewVTableWithCapacity(class *Class, parent *VTable, capacity int) *VTable {
	return &VTable{
		class:   class,
		parent:  parent,
		methods: make([]Method, 0, capacity),
	}
}
