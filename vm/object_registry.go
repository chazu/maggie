package vm

import (
	"fmt"
	"sync"
	"sync/atomic"
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
	*IORegistry

	// Core VM registries (AutoIDRegistry-backed). Exported so callers can
	// use Register/Get/Delete directly without delegation methods.
	Contexts         *AutoIDRegistry[*ContextValue]
	Dictionaries     *AutoIDRegistry[*DictionaryObject]
	GoObjects        *AutoIDRegistry[*GoObjectWrapper]
	ClassValues      *AutoIDRegistry[*Class]

	// Special registries (not suitable for AutoIDRegistry)
	cells          map[*Cell]struct{} // set semantics
	cellsMu        sync.RWMutex
	weakRefCounter atomic.Uint32       // counter only
	classVars      map[*Class]map[string]Value // nested map
	classVarsMu    sync.RWMutex

	// Extension registries for contrib plugins (keyed by marker constant)
	extensions   map[uint32]*AutoIDRegistry[any]
	extensionsMu sync.RWMutex
}

// NewObjectRegistry creates a new ObjectRegistry with all maps initialized.
func NewObjectRegistry() *ObjectRegistry {
	or := &ObjectRegistry{
		ConcurrencyRegistry: NewConcurrencyRegistry(),
		IORegistry:          NewIORegistry(),

		// Start IDs at 1 unless otherwise noted (0 = nil/uninitialized)
		Contexts:         NewAutoIDRegistry[*ContextValue](1, WithName("contexts")),
		// strings and dictionaries don't carry a marker byte — they use the
		// high bit(s) of the symbol payload as their discriminator. Strings
		// occupy [0x80000000, 0xC0000000), dictionaries [0xC0000000, 0xFFFFFFFF].
		Dictionaries:     NewAutoIDRegistry[*DictionaryObject](dictionaryIDOffset, WithMaxID(0xFFFFFFFF), WithName("dictionaries")),
		GoObjects:        NewAutoIDRegistry[*GoObjectWrapper](0, WithName("goObjects")),
		// ClassValues is monotonic: *Class caches its assigned classValueID,
		// so reusing an ID would create a stale cache on the prior owner.
		// In practice ClassValues entries are never deleted, but we encode
		// the invariant here so it can't regress.
		ClassValues:      NewAutoIDRegistry[*Class](1, WithMonotonic(), WithName("classValues")),

		cells:      make(map[*Cell]struct{}),
		classVars:  make(map[*Class]map[string]Value),
		extensions: make(map[uint32]*AutoIDRegistry[any]),
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

func (or *ObjectRegistry) RegisterContext(ctx *ContextValue) uint32 { return or.Contexts.Register(ctx) }
func (or *ObjectRegistry) GetContext(id uint32) *ContextValue       { return or.Contexts.Get(id) }
func (or *ObjectRegistry) UnregisterContext(id uint32)              { or.Contexts.Delete(id) }
func (or *ObjectRegistry) ClearContexts()                          { or.Contexts.Clear() }
func (or *ObjectRegistry) ContextCount() int                       { return or.Contexts.Count() }

// RegisterContextValue registers a ContextValue and returns a Value representing it.
func (or *ObjectRegistry) RegisterContextValue(ctx *ContextValue) Value {
	id := or.RegisterContext(ctx)
	return FromContextID(id)
}

// GetContextFromValue retrieves the ContextValue for a context Value.
// Returns nil if the Value is not a context.
func (or *ObjectRegistry) GetContextFromValue(v Value) *ContextValue {
	if !v.IsContext() {
		return nil
	}
	return or.GetContext(v.ContextID())
}

// UnregisterContextValue removes a context from the registry by its Value.
func (or *ObjectRegistry) UnregisterContextValue(v Value) {
	if v.IsContext() {
		or.UnregisterContext(v.ContextID())
	}
}

// ---------------------------------------------------------------------------
// Dictionary Registry Methods (delegates to AutoIDRegistry)
// ---------------------------------------------------------------------------

func (or *ObjectRegistry) RegisterDictionary(d *DictionaryObject) uint32 { return or.Dictionaries.Register(d) }
func (or *ObjectRegistry) GetDictionary(id uint32) *DictionaryObject     { return or.Dictionaries.Get(id) }
func (or *ObjectRegistry) UnregisterDictionary(id uint32)                { or.Dictionaries.Delete(id) }
func (or *ObjectRegistry) DictionaryCount() int                          { return or.Dictionaries.Count() }

// NewDictionaryValue creates a new empty dictionary Value, registered in the registry.
func (or *ObjectRegistry) NewDictionaryValue() Value {
	obj := &DictionaryObject{
		Data: make(map[uint64]Value),
		Keys: make(map[uint64]Value),
	}
	id := or.RegisterDictionary(obj)
	return FromSymbolID(id)
}

// GetDictionaryObject returns the DictionaryObject for a Value.
// Returns nil if v is not a dictionary.
func (or *ObjectRegistry) GetDictionaryObject(v Value) *DictionaryObject {
	if !v.IsSymbolEncoded() {
		return nil
	}
	id := v.SymbolID()
	if id < dictionaryIDOffset {
		return nil
	}
	return or.GetDictionary(id)
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
// Weak Reference Counter Methods
// ---------------------------------------------------------------------------

// weakRefIDMax is the largest valid weak reference ID. WeakReference values
// are NaN-boxed via the symbol tag with weakRefMarker in bits 24-31, leaving
// 24 bits for the ID. Wrap-around past this point would alias new weak refs
// to live ones in WeakRegistry.refs and silently corrupt finalizer dispatch
// (see vm/weak_reference.go: WeakRegistry uses the ID as map key).
//
// Weak refs are intentionally append-only: `WeakRegistry.Unregister` exists
// but is not invoked from the VM (the GC clears the target via Clear() but
// keeps the entry around for Lookup). Recycling IDs would require auditing
// every Lookup-after-Clear path, so we treat exhaustion as a fatal error.
const weakRefIDMax uint32 = (1 << 24) - 1

// NextWeakRefID returns the next unique ID for a weak reference. Panics on
// exhaustion rather than silently wrapping (see weakRefIDMax docs).
func (or *ObjectRegistry) NextWeakRefID() uint32 {
	id := or.weakRefCounter.Add(1)
	if id > weakRefIDMax {
		panic("ObjectRegistry: weak ref ID space exhausted (max 2^24-1 weak references)")
	}
	return id
}

// WeakRefCounterValue returns the current value of the weak reference counter.
func (or *ObjectRegistry) WeakRefCounterValue() uint32 {
	return or.weakRefCounter.Load()
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

// RegisterClassValue registers a class and returns its NaN-boxed Value.
// Idempotent — if the class already has a registered ID, the existing value
// is returned without re-registering.
func (or *ObjectRegistry) RegisterClassValue(c *Class) Value {
	if c == nil {
		return Nil
	}

	// Fast path: already registered.
	if id := c.classValueID.Load(); id != 0 {
		return classToValue(id)
	}

	// Slow path: register new. CAS guards against concurrent first-time
	// registrations: the class-value registry is monotonic so a losing
	// goroutine simply discards its (unreferenced) ID and adopts the
	// winner's. This is preferable to a per-Class lock on a hot path.
	id := or.ClassValues.Register(c)
	if !c.classValueID.CompareAndSwap(0, id) {
		id = c.classValueID.Load()
	}
	return classToValue(id)
}

// GetClassFromValue extracts the *Class from a class value.
// Returns nil if v is not a class value or the class is not found.
func (or *ObjectRegistry) GetClassFromValue(v Value) *Class {
	if !isClassValue(v) {
		return nil
	}
	id := classValueIDFromValue(v)
	return or.ClassValues.Get(id)
}

// ClassValueCount returns the number of registered class values.
func (or *ObjectRegistry) ClassValueCount() int {
	return or.ClassValues.Count()
}

// ---------------------------------------------------------------------------
// Extension Registry Methods (for contrib plugins)
// ---------------------------------------------------------------------------

// ExtensionRegistry returns (or lazily creates) a generic AutoIDRegistry for
// the given marker. Contrib plugins use this to store their custom types.
func (or *ObjectRegistry) ExtensionRegistry(marker uint32) *AutoIDRegistry[any] {
	or.extensionsMu.RLock()
	reg := or.extensions[marker]
	or.extensionsMu.RUnlock()
	if reg != nil {
		return reg
	}
	or.extensionsMu.Lock()
	defer or.extensionsMu.Unlock()
	if reg = or.extensions[marker]; reg != nil {
		return reg
	}
	reg = NewAutoIDRegistry[any](1)
	or.extensions[marker] = reg
	return reg
}

// ExtensionStats returns counts for all extension registries.
func (or *ObjectRegistry) ExtensionStats() map[uint32]int {
	or.extensionsMu.RLock()
	defer or.extensionsMu.RUnlock()
	stats := make(map[uint32]int, len(or.extensions))
	for marker, reg := range or.extensions {
		stats[marker] = reg.Count()
	}
	return stats
}

// ---------------------------------------------------------------------------
// Extended Stats
// ---------------------------------------------------------------------------

// FullStats returns counts of all registered objects across all registries.
func (or *ObjectRegistry) FullStats() map[string]int {
	stats := or.ConcurrencyRegistry.Stats()
	for k, v := range or.IORegistry.IOStats() {
		stats[k] = v
	}
	stats["contexts"] = or.ContextCount()
	stats["dictionaries"] = or.DictionaryCount()
	stats["cells"] = or.CellCount()
	stats["classVarClasses"] = or.ClassVarCount()
	stats["goObjects"] = or.GoObjectCount()
	stats["classValues"] = or.ClassValueCount()
	for marker, count := range or.ExtensionStats() {
		stats[fmt.Sprintf("ext_%d", marker>>24)] = count
	}
	return stats
}
