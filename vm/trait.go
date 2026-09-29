package vm

import "sync"

// ---------------------------------------------------------------------------
// Trait: Composable unit of behavior for Maggie classes
// ---------------------------------------------------------------------------

// Trait represents a collection of methods that can be composed into classes.
// Unlike classes, traits have no inheritance hierarchy and no instance variables.
// Traits are purely about method composition.
type Trait struct {
	Name      string                  // Trait name
	Namespace string                  // Namespace (empty for default)
	Methods   map[int]*CompiledMethod // Methods indexed by selector ID
	Requires  []int                   // Required method selector IDs (must be provided by class)
	DocString string                  // documentation from """ ... """ (empty if none)
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

// IncludeTrait composes a trait's methods into this class, following the
// documented resolution order (Guide07): a method defined on the class itself
// wins; otherwise the trait's method is installed — overriding an inherited
// one, and replacing a method installed by an earlier include (the
// last-included trait wins a conflict).
// With a symbol table, the copies are bound to this class's instance
// variables (see bindTraitIvars); nil skips that step.
// Returns an error message if required methods are not satisfied, or "" on success.
func (c *Class) IncludeTrait(trait *Trait, selectors *SelectorTable, symbols *SymbolTable) string {
	// First, check that all required methods are satisfied
	for _, reqSelector := range trait.Requires {
		if c.VTable.Lookup(reqSelector) == nil {
			selectorName := selectors.Name(reqSelector)
			return "class " + c.Name + " does not provide required method " + selectorName + " for trait " + trait.Name
		}
	}

	// Add trait methods (class methods take precedence).
	// Clone so each class owns its own copy with correct class pointer.
	ivarIDs := c.traitIvarSymbolIDs(symbols)
	for selectorID, method := range trait.Methods {
		// LookupLocal, not Lookup: an INHERITED method must not block the
		// trait's (Lookup walks the superclass chain).
		if existing := c.VTable.LookupLocal(selectorID); existing != nil {
			if cm, ok := existing.(*CompiledMethod); !ok || !cm.fromTrait {
				continue // the class's own method (or Go primitive) wins
			}
		}
		c.VTable.AddMethod(selectorID, bindTraitIvars(method, c, ivarIDs))
	}

	return ""
}

// traitIvarSymbolIDs maps the symbol ID of each of this class's instance
// variable names (inherited first, as the compiler numbers them) to its slot.
func (c *Class) traitIvarSymbolIDs(symbols *SymbolTable) map[uint32]int {
	if symbols == nil {
		return nil
	}
	ids := make(map[uint32]int)
	for i, name := range c.AllInstVarNames() {
		if id, ok := symbols.Lookup(name); ok && i <= 0xFF {
			ids[id] = i
		}
	}
	return ids
}

// bindTraitIvars returns a copy of a trait method owned by class c. A trait is
// compiled once, without knowing any host, so a reference to the host's
// instance variable compiles as a global of that name (which reads nil). The
// copy rewrites each PUSH_GLOBAL/STORE_GLOBAL of an instance-variable name to
// PUSH_IVAR/STORE_IVAR on this class's slot — the same resolution the host's
// own methods get, where instance variables shadow globals. The ivar form is
// one byte shorter, so a NOP pads it and no jump offset moves.
func bindTraitIvars(m *CompiledMethod, c *Class, ivarIDs map[uint32]int) *CompiledMethod {
	cloned := m.Clone()
	cloned.SetClass(c)
	cloned.fromTrait = true
	if len(ivarIDs) == 0 {
		return cloned
	}
	if bc, ok := rebindGlobalsToIvars(m.Bytecode, m.Literals, ivarIDs); ok {
		cloned.Bytecode = bc
	}
	var blocks []*BlockMethod
	for i, blk := range m.Blocks {
		bc, ok := rebindGlobalsToIvars(blk.Bytecode, blk.Literals, ivarIDs)
		if !ok {
			continue
		}
		if blocks == nil {
			blocks = append([]*BlockMethod(nil), m.Blocks...)
		}
		copied := *blk
		copied.Bytecode = bc
		copied.Outer = cloned
		blocks[i] = &copied
	}
	if blocks != nil {
		cloned.Blocks = blocks
	}
	return cloned
}

// rebindGlobalsToIvars returns a rewritten copy of bc (and true) if any
// global access names an instance variable in ivarIDs; bc is never mutated.
func rebindGlobalsToIvars(bc []byte, literals []Value, ivarIDs map[uint32]int) ([]byte, bool) {
	var out []byte
	for i := 0; i < len(bc); i += 1 + Opcode(bc[i]).Info().OperandBytes {
		op := Opcode(bc[i])
		if (op != OpPushGlobal && op != OpStoreGlobal) || i+2 >= len(bc) {
			continue
		}
		lit := int(bc[i+1]) | int(bc[i+2])<<8
		if lit >= len(literals) || !literals[lit].IsSymbol() {
			continue
		}
		slot, ok := ivarIDs[literals[lit].SymbolID()]
		if !ok {
			continue
		}
		if out == nil {
			out = append([]byte(nil), bc...)
		}
		out[i] = byte(OpPushIvar)
		if op == OpStoreGlobal {
			out[i] = byte(OpStoreIvar)
		}
		out[i+1] = byte(slot)
		out[i+2] = byte(OpNOP)
	}
	return out, out != nil
}

// IncludeTraitByName looks up a trait by name and includes it in this class.
// Returns an error message on failure, or "" on success.
func (c *Class) IncludeTraitByName(traitName string, traits *TraitTable, selectors *SelectorTable, symbols *SymbolTable) string {
	trait := traits.Lookup(traitName)
	if trait == nil {
		return "unknown trait: " + traitName
	}
	return c.IncludeTrait(trait, selectors, symbols)
}
