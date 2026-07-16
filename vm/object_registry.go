package vm

import (
	"sync"
	"unsafe"
)

// ---------------------------------------------------------------------------
// ObjectRegistry: Unified registry for ALL VM-local object registries
// ---------------------------------------------------------------------------

// ObjectRegistry manages all VM-local registries. It embeds ConcurrencyRegistry
// so all existing channel/process/mutex/waitgroup/semaphore/cancellation/block
// methods continue to work, and adds registries for exceptions, results,
// contexts, dictionaries, strings, gRPC clients, gRPC streams, HTTP servers,
// HTTP requests, HTTP responses, cells, weak reference IDs, and class variables.
type ObjectRegistry struct {
	*ConcurrencyRegistry

	// Special registries (not suitable for AutoIDRegistry)
	cells       map[*Cell]struct{} // set semantics
	cellsMu     sync.RWMutex
	classVars   map[*Class]map[string]Value // nested map
	classVarsMu sync.RWMutex
}

// NewObjectRegistry creates a new ObjectRegistry with all maps initialized.
func NewObjectRegistry() *ObjectRegistry {
	or := &ObjectRegistry{
		ConcurrencyRegistry: NewConcurrencyRegistry(),

		cells:     make(map[*Cell]struct{}),
		classVars: make(map[*Class]map[string]Value),
	}

	return or
}

// ---------------------------------------------------------------------------
// Exception Registry Methods (delegates to AutoIDRegistry)
// ---------------------------------------------------------------------------

// RegisterExceptionValue wraps an ExceptionObject in a heap Value traced by the
// Go GC. Replaces the old id-registry form.
func (or *ObjectRegistry) RegisterExceptionValue(ex *ExceptionObject) Value {
	return makeHeap(kindException, unsafe.Pointer(ex))
}

// GetExceptionFromValue retrieves the ExceptionObject a Value references, or
// nil if v is not an exception.
func (or *ObjectRegistry) GetExceptionFromValue(v Value) *ExceptionObject {
	if !v.IsException() {
		return nil
	}
	return (*ExceptionObject)(v.ptr)
}

// ---------------------------------------------------------------------------
// Result Registry Methods (delegates to AutoIDRegistry)
// ---------------------------------------------------------------------------

// RegisterResultValue wraps a Result in a heap Value. The name is retained for
// call-site compatibility; the object is now carried by a real pointer traced
// by the Go GC rather than an id registry.
func (or *ObjectRegistry) RegisterResultValue(r *ResultObject) Value {
	return makeHeap(kindResult, unsafe.Pointer(r))
}

// GetResultFromValue retrieves a ResultObject from a Value.
// Returns nil if the Value is not a result.
func (or *ObjectRegistry) GetResultFromValue(v Value) *ResultObject {
	if !isResultValue(v) {
		return nil
	}
	return (*ResultObject)(v.ptr)
}

// ---------------------------------------------------------------------------
// Context Registry Methods (delegates to AutoIDRegistry)
// ---------------------------------------------------------------------------

// RegisterContextValue wraps a ContextValue in a pointer-carrying kindContext
// heap Value traced by Go's GC. The name is retained for call-site
// compatibility; there is no id registry.
func (or *ObjectRegistry) RegisterContextValue(ctx *ContextValue) Value {
	return makeContextValue(ctx)
}

// GetContextFromValue retrieves the ContextValue for a context Value.
// Returns nil if the Value is not a context.
func (or *ObjectRegistry) GetContextFromValue(v Value) *ContextValue {
	return v.contextPtr()
}

// ---------------------------------------------------------------------------
// Dictionary Registry Methods (delegates to AutoIDRegistry)
// ---------------------------------------------------------------------------

// NewDictionaryValue creates a new empty dictionary heap Value.
func (or *ObjectRegistry) NewDictionaryValue() Value {
	return makeHeap(kindDictionary, unsafe.Pointer(NewDictionaryObject()))
}

// GetDictionaryObject returns the DictionaryObject for a Value.
// Returns nil if v is not a dictionary.
func (or *ObjectRegistry) GetDictionaryObject(v Value) *DictionaryObject {
	if !IsDictionaryValue(v) {
		return nil
	}
	return (*DictionaryObject)(v.ptr)
}

// ---------------------------------------------------------------------------
// String heap Values
// ---------------------------------------------------------------------------
//
// Strings are pointer-carrying heap Values (kindString) traced by the Go GC.
// StringObject is immutable-content, so no per-object lock is needed: two
// goroutines reading .Content race-free once the pointer is published.

// NewStringValue creates a heap Value wrapping a Go string.
func (or *ObjectRegistry) NewStringValue(s string) Value {
	return makeHeap(kindString, unsafe.Pointer(&StringObject{Content: s}))
}

// CompareStrings returns true if both values are strings with equal content.
func (or *ObjectRegistry) CompareStrings(a, b Value) bool {
	if !IsStringValue(a) || !IsStringValue(b) {
		return false
	}
	return (*StringObject)(a.ptr).Content == (*StringObject)(b.ptr).Content
}

// GetStringContent returns the Go string content of a string Value.
// Returns empty string if v is not a string.
func (or *ObjectRegistry) GetStringContent(v Value) string {
	if !IsStringValue(v) {
		return ""
	}
	return (*StringObject)(v.ptr).Content
}

// GetStringObject returns the StringObject for a Value.
// Returns nil if v is not a string.
func (or *ObjectRegistry) GetStringObject(v Value) *StringObject {
	if !IsStringValue(v) {
		return nil
	}
	return (*StringObject)(v.ptr)
}

// ---------------------------------------------------------------------------
// Cell Registry Methods
// ---------------------------------------------------------------------------

// RegisterCell adds a cell to the registry.
func (or *ObjectRegistry) RegisterCell(c *Cell) {
	or.cellsMu.Lock()
	or.cells[c] = struct{}{}
	or.cellsMu.Unlock()
}

// UnregisterCell removes a cell from the registry.
func (or *ObjectRegistry) UnregisterCell(c *Cell) {
	or.cellsMu.Lock()
	delete(or.cells, c)
	or.cellsMu.Unlock()
}

// HasCell checks if a cell is in the registry.
func (or *ObjectRegistry) HasCell(c *Cell) bool {
	or.cellsMu.RLock()
	defer or.cellsMu.RUnlock()
	_, exists := or.cells[c]
	return exists
}

// CellCount returns the number of registered cells.
func (or *ObjectRegistry) CellCount() int {
	or.cellsMu.RLock()
	defer or.cellsMu.RUnlock()
	return len(or.cells)
}

// ---------------------------------------------------------------------------
// Class Variable Storage Methods
// ---------------------------------------------------------------------------

// GetClassVar returns the value of a class variable for the given class.
func (or *ObjectRegistry) GetClassVar(c *Class, name string) Value {
	or.classVarsMu.RLock()
	defer or.classVarsMu.RUnlock()

	if values, ok := or.classVars[c]; ok {
		if val, ok := values[name]; ok {
			return val
		}
	}
	return Nil
}

// SetClassVar sets the value of a class variable for the given class.
func (or *ObjectRegistry) SetClassVar(c *Class, name string, value Value) {
	or.classVarsMu.Lock()
	defer or.classVarsMu.Unlock()

	if _, ok := or.classVars[c]; !ok {
		or.classVars[c] = make(map[string]Value)
	}
	or.classVars[c][name] = value
}

// GetClassVarStorage returns the full class variable map for a class.
// Returns nil if no class variables have been set.
func (or *ObjectRegistry) GetClassVarStorage(c *Class) map[string]Value {
	or.classVarsMu.RLock()
	defer or.classVarsMu.RUnlock()
	return or.classVars[c]
}

// ClassVarCount returns the number of classes with class variable storage.
func (or *ObjectRegistry) ClassVarCount() int {
	or.classVarsMu.RLock()
	defer or.classVarsMu.RUnlock()
	return len(or.classVars)
}

// ---------------------------------------------------------------------------
// Class Value Registry Methods
// ---------------------------------------------------------------------------

// RegisterClassValue returns the class as a pointer-carrying heap Value. The
// pointer is the *Class itself, so the result is idempotent (pointer-identical)
// across calls without any id registry.
func (or *ObjectRegistry) RegisterClassValue(c *Class) Value {
	if c == nil {
		return Nil
	}
	return makeHeap(kindClassValue, unsafe.Pointer(c))
}

// GetClassFromValue extracts the *Class from a class value.
// Returns nil if v is not a class value.
func (or *ObjectRegistry) GetClassFromValue(v Value) *Class {
	if !isClassValue(v) {
		return nil
	}
	return (*Class)(v.ptr)
}

// ---------------------------------------------------------------------------
// Extended Stats
// ---------------------------------------------------------------------------

// FullStats returns counts of all registered objects across all registries.
func (or *ObjectRegistry) FullStats() map[string]int {
	stats := or.ConcurrencyRegistry.Stats()
	stats["cells"] = or.CellCount()
	stats["classVarClasses"] = or.ClassVarCount()
	return stats
}
