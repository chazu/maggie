package vm

import "sync"

// SelectorTable interns selector names to numeric IDs for fast lookup.
//
// Selectors are method names like "at:", "at:put:", "ifTrue:ifFalse:".
// By converting them to numeric IDs at compile/load time, method dispatch
// can use fast array indexing instead of string comparison.
//
// The table is append-only and thread-safe for concurrent reads after
// initial population. New selectors can be added concurrently.
type SelectorTable struct {
	mu     sync.RWMutex
	byName map[string]int // name -> ID
	byID   []string       // ID -> name
}

// NewSelectorTable creates a new empty selector table.
func NewSelectorTable() *SelectorTable {
	return &SelectorTable{
		byName: make(map[string]int),
		byID:   make([]string, 0, 256), // Pre-allocate for common case
	}
}

// Intern returns the ID for a selector name, creating a new ID if needed.
// This is the primary method for populating the table.
func (st *SelectorTable) Intern(name string) int {
	// Fast path: read-only lookup
	st.mu.RLock()
	if id, ok := st.byName[name]; ok {
		st.mu.RUnlock()
		return id
	}
	st.mu.RUnlock()

	// Slow path: need to add new selector
	st.mu.Lock()
	defer st.mu.Unlock()

	// Double-check after acquiring write lock
	if id, ok := st.byName[name]; ok {
		return id
	}

	id := len(st.byID)
	st.byName[name] = id
	st.byID = append(st.byID, name)
	return id
}

// Lookup returns the ID for a selector name, or -1 if not found.
// Use this when you don't want to create new entries.
func (st *SelectorTable) Lookup(name string) int {
	st.mu.RLock()
	defer st.mu.RUnlock()

	if id, ok := st.byName[name]; ok {
		return id
	}
	return -1
}

// Name returns the selector name for an ID, or "" if invalid.
func (st *SelectorTable) Name(id int) string {
	st.mu.RLock()
	defer st.mu.RUnlock()

	if id < 0 || id >= len(st.byID) {
		return ""
	}
	return st.byID[id]
}

// Len returns the number of interned selectors.
func (st *SelectorTable) Len() int {
	st.mu.RLock()
	defer st.mu.RUnlock()
	return len(st.byID)
}

// All returns all selector names in ID order.
// This allocates a new slice; use for debugging only.
func (st *SelectorTable) All() []string {
	st.mu.RLock()
	defer st.mu.RUnlock()

	result := make([]string, len(st.byID))
	copy(result, st.byID)
	return result
}

// MustIntern is like Intern but returns the ID directly.
// Useful for static initialization.
func (st *SelectorTable) MustIntern(name string) int {
	return st.Intern(name)
}

// InternAll interns multiple selectors and returns their IDs.
func (st *SelectorTable) InternAll(names ...string) []int {
	ids := make([]int, len(names))
	for i, name := range names {
		ids[i] = st.Intern(name)
	}
	return ids
}
