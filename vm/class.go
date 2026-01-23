package vm

import "sync"

// ---------------------------------------------------------------------------
// Class: Full Maggie class representation
// ---------------------------------------------------------------------------

// Note: The Class struct is forward-declared in object.go.
// This file contains the full implementation and methods.

// InstVarIndex returns the slot index for an instance variable by name.
// Returns -1 if the variable is not found.
func (c *Class) InstVarIndex(name string) int {
	// Check this class's instance variables
	for i, n := range c.InstVars {
		if n == name {
			return c.instVarOffset() + i
		}
	}
	// Check superclass
	if c.Superclass != nil {
		return c.Superclass.InstVarIndex(name)
	}
	return -1
}

// instVarOffset returns the starting slot index for this class's instance variables.
// This accounts for inherited instance variables.
func (c *Class) instVarOffset() int {
	if c.Superclass == nil {
		return 0
	}
	return c.Superclass.NumSlots
}

// AllInstVarNames returns all instance variable names including inherited ones.
func (c *Class) AllInstVarNames() []string {
	if c.Superclass == nil {
		return c.InstVars
	}
	inherited := c.Superclass.AllInstVarNames()
	result := make([]string, len(inherited)+len(c.InstVars))
	copy(result, inherited)
	copy(result[len(inherited):], c.InstVars)
	return result
}

// IsSubclassOf returns true if c is a subclass of other (or is the same class).
func (c *Class) IsSubclassOf(other *Class) bool {
	for current := c; current != nil; current = current.Superclass {
		if current == other {
			return true
		}
	}
	return false
}

// IsSuperclassOf returns true if c is a superclass of other (or is the same class).
func (c *Class) IsSuperclassOf(other *Class) bool {
	return other.IsSubclassOf(c)
}

// ---------------------------------------------------------------------------
// Class Variables
// ---------------------------------------------------------------------------

// classVarStorage holds the actual values for class variables.
// Each class that declares class variables has an entry in this map.
var classVarStorage = make(map[*Class]map[string]Value)
var classVarMu sync.RWMutex

// HasClassVar returns true if this class or any superclass declares the named class variable.
func (c *Class) HasClassVar(name string) bool {
	return c.findClassVarOwner(name) != nil
}

// findClassVarOwner finds the class that declares the named class variable.
// Returns nil if not found.
func (c *Class) findClassVarOwner(name string) *Class {
	for current := c; current != nil; current = current.Superclass {
		for _, cv := range current.ClassVars {
			if cv == name {
				return current
			}
		}
	}
	return nil
}

// GetClassVar returns the value of a class variable.
// Walks up the class hierarchy to find the declaring class.
func (c *Class) GetClassVar(name string) Value {
	owner := c.findClassVarOwner(name)
	if owner == nil {
		return Nil
	}

	classVarMu.RLock()
	defer classVarMu.RUnlock()

	if values, ok := classVarStorage[owner]; ok {
		if val, ok := values[name]; ok {
			return val
		}
	}
	return Nil
}

// SetClassVar sets the value of a class variable.
// Walks up the class hierarchy to find the declaring class.
func (c *Class) SetClassVar(name string, value Value) {
	owner := c.findClassVarOwner(name)
	if owner == nil {
		// Variable not declared - could error, but for now just store on this class
		owner = c
	}

	classVarMu.Lock()
	defer classVarMu.Unlock()

	if classVarStorage[owner] == nil {
		classVarStorage[owner] = make(map[string]Value)
	}
	classVarStorage[owner][name] = value
}

// ClassVarIndex returns the index of a class variable by name.
// Returns -1 if the variable is not found.
func (c *Class) ClassVarIndex(name string) int {
	for current := c; current != nil; current = current.Superclass {
		for i, n := range current.ClassVars {
			if n == name {
				return i
			}
		}
	}
	return -1
}

// AllClassVarNames returns all class variable names including inherited ones.
func (c *Class) AllClassVarNames() []string {
	if c.Superclass == nil {
		return c.ClassVars
	}
	inherited := c.Superclass.AllClassVarNames()
	// Add this class's class vars, avoiding duplicates (shadowing)
	seen := make(map[string]bool)
	for _, name := range inherited {
		seen[name] = true
	}
	result := make([]string, len(inherited))
	copy(result, inherited)
	for _, name := range c.ClassVars {
		if !seen[name] {
			result = append(result, name)
		}
	}
	return result
}

// NewInstance creates a new instance of this class.
func (c *Class) NewInstance() *Object {
	return NewObject(c.VTable, c.NumSlots)
}

// NewInstanceWithSlots creates a new instance with initial slot values.
func (c *Class) NewInstanceWithSlots(slots []Value) *Object {
	return NewObjectWithSlots(c.VTable, slots)
}

// ---------------------------------------------------------------------------
// Method registration on Class
// ---------------------------------------------------------------------------

// AddMethod registers a method on this class.
// The selector will be interned in the given SelectorTable.
func (c *Class) AddMethod(selectors *SelectorTable, name string, method Method) {
	selectorID := selectors.Intern(name)
	c.VTable.AddMethod(selectorID, method)
}

// AddMethod0 registers a zero-argument method on this class.
func (c *Class) AddMethod0(selectors *SelectorTable, name string, fn Method0Func) {
	c.AddMethod(selectors, name, NewMethod0(name, fn))
}

// AddMethod1 registers a one-argument method on this class.
func (c *Class) AddMethod1(selectors *SelectorTable, name string, fn Method1Func) {
	c.AddMethod(selectors, name, NewMethod1(name, fn))
}

// AddMethod2 registers a two-argument method on this class.
func (c *Class) AddMethod2(selectors *SelectorTable, name string, fn Method2Func) {
	c.AddMethod(selectors, name, NewMethod2(name, fn))
}

// AddMethod3 registers a three-argument method on this class.
func (c *Class) AddMethod3(selectors *SelectorTable, name string, fn Method3Func) {
	c.AddMethod(selectors, name, NewMethod3(name, fn))
}

// AddMethod4 registers a four-argument method on this class.
func (c *Class) AddMethod4(selectors *SelectorTable, name string, fn Method4Func) {
	c.AddMethod(selectors, name, NewMethod4(name, fn))
}

// AddMethod8 registers an eight-argument method on this class.
func (c *Class) AddMethod8(selectors *SelectorTable, name string, fn Method8Func) {
	c.AddMethod(selectors, name, NewMethod8(name, fn))
}

// AddPrimitiveMethod registers a variable-arity primitive method on this class.
func (c *Class) AddPrimitiveMethod(selectors *SelectorTable, name string, fn PrimitiveFunc) {
	c.AddMethod(selectors, name, NewPrimitiveMethod(name, fn))
}

// LookupMethod looks up a method by selector name.
func (c *Class) LookupMethod(selectors *SelectorTable, name string) Method {
	selectorID := selectors.Lookup(name)
	if selectorID < 0 {
		return nil
	}
	return c.VTable.Lookup(selectorID)
}

// HasMethod returns true if this class (not superclasses) defines a method.
func (c *Class) HasMethod(selectors *SelectorTable, name string) bool {
	selectorID := selectors.Lookup(name)
	if selectorID < 0 {
		return false
	}
	return c.VTable.HasMethod(selectorID)
}

// ---------------------------------------------------------------------------
// Class method registration (class-side / metaclass methods)
// ---------------------------------------------------------------------------

// AddClassMethod registers a class-side method on this class.
// The selector will be interned in the given SelectorTable.
func (c *Class) AddClassMethod(selectors *SelectorTable, name string, method Method) {
	selectorID := selectors.Intern(name)
	c.ClassVTable.AddMethod(selectorID, method)
}

// AddClassMethod0 registers a zero-argument class-side method.
func (c *Class) AddClassMethod0(selectors *SelectorTable, name string, fn Method0Func) {
	c.AddClassMethod(selectors, name, NewMethod0(name, fn))
}

// AddClassMethod1 registers a one-argument class-side method.
func (c *Class) AddClassMethod1(selectors *SelectorTable, name string, fn Method1Func) {
	c.AddClassMethod(selectors, name, NewMethod1(name, fn))
}

// AddClassMethod2 registers a two-argument class-side method.
func (c *Class) AddClassMethod2(selectors *SelectorTable, name string, fn Method2Func) {
	c.AddClassMethod(selectors, name, NewMethod2(name, fn))
}

// LookupClassMethod looks up a class-side method by selector name.
func (c *Class) LookupClassMethod(selectors *SelectorTable, name string) Method {
	selectorID := selectors.Lookup(name)
	if selectorID < 0 {
		return nil
	}
	return c.ClassVTable.Lookup(selectorID)
}

// ---------------------------------------------------------------------------
// ClassTable: Global class registry
// ---------------------------------------------------------------------------

// ClassTable manages registered classes by name.
// It's thread-safe for concurrent access.
type ClassTable struct {
	mu      sync.RWMutex
	classes map[string]*Class
}

// NewClassTable creates a new empty class table.
func NewClassTable() *ClassTable {
	return &ClassTable{
		classes: make(map[string]*Class),
	}
}

// Register adds a class to the table.
// Returns the previous class with this name, or nil.
func (ct *ClassTable) Register(c *Class) *Class {
	ct.mu.Lock()
	defer ct.mu.Unlock()

	key := ct.classKey(c)
	old := ct.classes[key]
	ct.classes[key] = c
	return old
}

// Lookup finds a class by name.
func (ct *ClassTable) Lookup(name string) *Class {
	ct.mu.RLock()
	defer ct.mu.RUnlock()
	return ct.classes[name]
}

// LookupInNamespace finds a class by name and namespace.
func (ct *ClassTable) LookupInNamespace(namespace, name string) *Class {
	ct.mu.RLock()
	defer ct.mu.RUnlock()

	key := name
	if namespace != "" {
		key = namespace + "::" + name
	}
	return ct.classes[key]
}

// Has returns true if a class with this name is registered.
func (ct *ClassTable) Has(name string) bool {
	ct.mu.RLock()
	defer ct.mu.RUnlock()
	_, ok := ct.classes[name]
	return ok
}

// All returns all registered classes.
func (ct *ClassTable) All() []*Class {
	ct.mu.RLock()
	defer ct.mu.RUnlock()

	result := make([]*Class, 0, len(ct.classes))
	for _, c := range ct.classes {
		result = append(result, c)
	}
	return result
}

// Len returns the number of registered classes.
func (ct *ClassTable) Len() int {
	ct.mu.RLock()
	defer ct.mu.RUnlock()
	return len(ct.classes)
}

// classKey generates the lookup key for a class.
func (ct *ClassTable) classKey(c *Class) string {
	if c.Namespace == "" {
		return c.Name
	}
	return c.Namespace + "::" + c.Name
}

// ---------------------------------------------------------------------------
// Class creation helpers
// ---------------------------------------------------------------------------

// NewClass creates a new class with the given name and superclass.
// The VTable and ClassVTable are automatically created and linked.
func NewClass(name string, superclass *Class) *Class {
	var parentVT *VTable
	var parentClassVT *VTable
	var numSlots int
	if superclass != nil {
		parentVT = superclass.VTable
		parentClassVT = superclass.ClassVTable
		numSlots = superclass.NumSlots
	}

	c := &Class{
		Name:       name,
		Superclass: superclass,
		NumSlots:   numSlots,
	}
	c.VTable = NewVTable(c, parentVT)
	c.ClassVTable = NewVTable(c, parentClassVT)
	return c
}

// NewClassWithInstVars creates a new class with instance variables.
func NewClassWithInstVars(name string, superclass *Class, instVars []string) *Class {
	c := NewClass(name, superclass)
	c.InstVars = instVars
	c.NumSlots += len(instVars)
	return c
}

// NewClassInNamespace creates a new class in a specific namespace.
func NewClassInNamespace(namespace, name string, superclass *Class) *Class {
	c := NewClass(name, superclass)
	c.Namespace = namespace
	return c
}

// ---------------------------------------------------------------------------
// Full qualified name helpers
// ---------------------------------------------------------------------------

// FullName returns the fully qualified class name (namespace::name or just name).
func (c *Class) FullName() string {
	if c.Namespace == "" {
		return c.Name
	}
	return c.Namespace + "::" + c.Name
}

// String implements the Stringer interface.
func (c *Class) String() string {
	return c.FullName()
}

// ---------------------------------------------------------------------------
// Class hierarchy helpers
// ---------------------------------------------------------------------------

// Superclasses returns all superclasses from immediate parent to root.
func (c *Class) Superclasses() []*Class {
	var result []*Class
	for current := c.Superclass; current != nil; current = current.Superclass {
		result = append(result, current)
	}
	return result
}

// Depth returns the inheritance depth (0 for root class).
func (c *Class) Depth() int {
	depth := 0
	for current := c.Superclass; current != nil; current = current.Superclass {
		depth++
	}
	return depth
}
