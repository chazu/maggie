package vm

import "sync"

// ---------------------------------------------------------------------------
// Trait: Composable unit of behavior for Maggie classes
// ---------------------------------------------------------------------------

// Trait represents a collection of methods that can be composed into classes.
// Unlike classes, traits have no inheritance hierarchy and no instance variables.
// Traits are purely about method composition.
type Trait struct {
	Name      string                    // Trait name
	Namespace string                    // Namespace (empty for default)
	Methods   map[int]*CompiledMethod   // Methods indexed by selector ID
	Requires  []int                     // Required method selector IDs (must be provided by class)
	DocString string                    // documentation from """ ... """ (empty if none)
}

// NewTrait creates a new empty trait.
func NewTrait(name string) *Trait {
	return &Trait{
		Name:    name,
		Methods: make(map[int]*CompiledMethod),
	}
}

// AddMethod adds a compiled method to the trait.
func (t *Trait) AddMethod(selectorID int, method *CompiledMethod) {
	t.Methods[selectorID] = method
}

// AddRequires marks a selector as required by the trait.
// Classes that include this trait must provide implementations of required methods.
func (t *Trait) AddRequires(selectorID int) {
	t.Requires = append(t.Requires, selectorID)
}

// HasMethod returns true if the trait provides a method for the given selector.
func (t *Trait) HasMethod(selectorID int) bool {
	_, ok := t.Methods[selectorID]
	return ok
}

// GetMethod returns the method for the given selector, or nil if not found.
func (t *Trait) GetMethod(selectorID int) *CompiledMethod {
	return t.Methods[selectorID]
}

// MethodCount returns the number of methods in the trait.
func (t *Trait) MethodCount() int {
	return len(t.Methods)
}

// ---------------------------------------------------------------------------
// TraitTable: Global trait registry
// ---------------------------------------------------------------------------

// TraitTable manages registered traits by name.
// It's thread-safe for concurrent access.
type TraitTable struct {
	mu     sync.RWMutex
	traits map[string]*Trait
}

// NewTraitTable creates a new empty trait table.
func NewTraitTable() *TraitTable {
	return &TraitTable{
		traits: make(map[string]*Trait),
	}
}

// Register adds a trait to the table.
// Returns the previous trait with this name, or nil.
func (tt *TraitTable) Register(t *Trait) *Trait {
	tt.mu.Lock()
	defer tt.mu.Unlock()

	old := tt.traits[t.Name]
	tt.traits[t.Name] = t
	return old
}

// Lookup finds a trait by name.
func (tt *TraitTable) Lookup(name string) *Trait {
	tt.mu.RLock()
	defer tt.mu.RUnlock()
	return tt.traits[name]
}

// Has returns true if a trait with this name is registered.
func (tt *TraitTable) Has(name string) bool {
	tt.mu.RLock()
	defer tt.mu.RUnlock()
	_, ok := tt.traits[name]
	return ok
}

// All returns all registered traits.
func (tt *TraitTable) All() []*Trait {
	tt.mu.RLock()
	defer tt.mu.RUnlock()

	result := make([]*Trait, 0, len(tt.traits))
	for _, t := range tt.traits {
		result = append(result, t)
	}
	return result
}

// Len returns the number of registered traits.
func (tt *TraitTable) Len() int {
	tt.mu.RLock()
	defer tt.mu.RUnlock()
	return len(tt.traits)
}

// ---------------------------------------------------------------------------
// Class trait composition
// ---------------------------------------------------------------------------

// IncludeTrait composes a trait's methods into this class.
// Trait methods are added to the class's VTable only if the class
// doesn't already define a method with that selector (class wins).
// Returns an error message if required methods are not satisfied, or "" on success.
func (c *Class) IncludeTrait(trait *Trait, selectors *SelectorTable) string {
	// First, check that all required methods are satisfied
	for _, reqSelector := range trait.Requires {
		if c.VTable.Lookup(reqSelector) == nil {
			selectorName := selectors.Name(reqSelector)
			return "class " + c.Name + " does not provide required method " + selectorName + " for trait " + trait.Name
		}
	}

	// Add trait methods (class methods take precedence)
	for selectorID, method := range trait.Methods {
		if c.VTable.Lookup(selectorID) == nil {
			// Class doesn't define this method, add trait's method
			c.VTable.AddMethod(selectorID, method)
		}
		// If class already defines the method, trait method is ignored (class wins)
	}

	return ""
}

// IncludeTraitByName looks up a trait by name and includes it in this class.
// Returns an error message on failure, or "" on success.
func (c *Class) IncludeTraitByName(traitName string, traits *TraitTable, selectors *SelectorTable) string {
	trait := traits.Lookup(traitName)
	if trait == nil {
		return "unknown trait: " + traitName
	}
	return c.IncludeTrait(trait, selectors)
}
