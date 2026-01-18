package vm

// VTable holds the method dispatch table for a class.
//
// Methods are stored in an array indexed by selector ID, allowing O(1)
// lookup for monomorphic call sites. Inheritance is handled by walking
// the parent chain when a method is not found locally.
//
// Note: The VTable struct is forward-declared in object.go to break
// the import cycle. This file contains the method implementations.

// Lookup finds a method by selector ID, walking the inheritance chain.
// Returns nil if no method is found (triggers doesNotUnderstand:).
func (vt *VTable) Lookup(selector int) Method {
	for v := vt; v != nil; v = v.parent {
		if selector >= 0 && selector < len(v.methods) {
			if m := v.methods[selector]; m != nil {
				return m
			}
		}
	}
	return nil
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
// The methods array is grown as needed.
func (vt *VTable) AddMethod(selector int, method Method) {
	if selector >= len(vt.methods) {
		// Grow the methods slice
		newMethods := make([]Method, selector+1)
		copy(newMethods, vt.methods)
		vt.methods = newMethods
	}
	vt.methods[selector] = method
}

// RemoveMethod removes a method at the given selector ID.
func (vt *VTable) RemoveMethod(selector int) {
	if selector >= 0 && selector < len(vt.methods) {
		vt.methods[selector] = nil
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
}

// Class returns the class this vtable belongs to.
func (vt *VTable) Class() *Class {
	return vt.class
}

// MethodCount returns the number of method slots (including nil slots).
func (vt *VTable) MethodCount() int {
	return len(vt.methods)
}

// LocalMethods returns all non-nil methods defined in this vtable.
// Returns a map of selector ID to method.
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
