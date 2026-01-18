package vm

import "sync"

// ---------------------------------------------------------------------------
// SymbolTable: Interned symbols
// ---------------------------------------------------------------------------

// SymbolTable interns symbol strings to unique IDs.
// Symbols are immutable, unique strings used for identifiers.
type SymbolTable struct {
	mu     sync.RWMutex
	byName map[string]uint32 // name -> ID
	byID   []string          // ID -> name
}

// NewSymbolTable creates a new empty symbol table.
func NewSymbolTable() *SymbolTable {
	return &SymbolTable{
		byName: make(map[string]uint32),
		byID:   make([]string, 0, 256),
	}
}

// Intern returns the ID for a symbol, creating a new one if needed.
func (st *SymbolTable) Intern(name string) uint32 {
	// Fast path: read-only lookup
	st.mu.RLock()
	if id, ok := st.byName[name]; ok {
		st.mu.RUnlock()
		return id
	}
	st.mu.RUnlock()

	// Slow path: need to add new symbol
	st.mu.Lock()
	defer st.mu.Unlock()

	// Double-check after acquiring write lock
	if id, ok := st.byName[name]; ok {
		return id
	}

	id := uint32(len(st.byID))
	st.byName[name] = id
	st.byID = append(st.byID, name)
	return id
}

// Lookup returns the ID for a symbol, or 0 and false if not found.
func (st *SymbolTable) Lookup(name string) (uint32, bool) {
	st.mu.RLock()
	defer st.mu.RUnlock()
	id, ok := st.byName[name]
	return id, ok
}

// Name returns the symbol name for an ID, or "" if invalid.
func (st *SymbolTable) Name(id uint32) string {
	st.mu.RLock()
	defer st.mu.RUnlock()

	if int(id) >= len(st.byID) {
		return ""
	}
	return st.byID[id]
}

// Len returns the number of interned symbols.
func (st *SymbolTable) Len() int {
	st.mu.RLock()
	defer st.mu.RUnlock()
	return len(st.byID)
}

// All returns all symbol names in ID order.
func (st *SymbolTable) All() []string {
	st.mu.RLock()
	defer st.mu.RUnlock()

	result := make([]string, len(st.byID))
	copy(result, st.byID)
	return result
}

// SymbolValue creates a Value from a symbol name.
func (st *SymbolTable) SymbolValue(name string) Value {
	return FromSymbolID(st.Intern(name))
}
