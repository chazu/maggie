package vm

import (
	"sync"
	"sync/atomic"
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

	// Core VM registries (AutoIDRegistry-backed)
	exceptions       *AutoIDRegistry[*ExceptionObject]
	results          *AutoIDRegistry[*ResultObject]
	contexts         *AutoIDRegistry[*ContextValue]
	dictionaries     *AutoIDRegistry[*DictionaryObject]
	strings          *AutoIDRegistry[*StringObject]
	goObjects        *AutoIDRegistry[*GoObjectWrapper]
	bigInts          *AutoIDRegistry[*BigIntObject]
	cueContexts      *AutoIDRegistry[*CueContextObject]
	cueValues        *AutoIDRegistry[*CueValueObject]
	tupleSpaces      *AutoIDRegistry[*TupleSpaceObject]
	constraintStores *AutoIDRegistry[*ConstraintStoreObject]
	classValues      *AutoIDRegistry[*Class]

	// Special registries (not suitable for AutoIDRegistry)
	cells          map[*Cell]struct{} // set semantics
	cellsMu        sync.RWMutex
	weakRefCounter atomic.Uint32       // counter only
	classVars      map[*Class]map[string]Value // nested map
	classVarsMu    sync.RWMutex
}

// NewObjectRegistry creates a new ObjectRegistry with all maps initialized.
func NewObjectRegistry() *ObjectRegistry {
	or := &ObjectRegistry{
		ConcurrencyRegistry: NewConcurrencyRegistry(),
		IORegistry:          NewIORegistry(),

		// Start IDs at 1 unless otherwise noted (0 = nil/uninitialized)
		exceptions:       NewAutoIDRegistry[*ExceptionObject](1, WithName("exceptions")),
		results:          NewAutoIDRegistry[*ResultObject](1, WithName("results")),
		contexts:         NewAutoIDRegistry[*ContextValue](1, WithName("contexts")),
		// strings and dictionaries don't carry a marker byte — they use the
		// high bit(s) of the symbol payload as their discriminator. Strings
		// occupy [0x80000000, 0xC0000000), dictionaries [0xC0000000, 0xFFFFFFFF].
		dictionaries:     NewAutoIDRegistry[*DictionaryObject](dictionaryIDOffset, WithMaxID(0xFFFFFFFF), WithName("dictionaries")),
		strings:          NewAutoIDRegistry[*StringObject](stringIDOffset, WithMaxID(dictionaryIDOffset-1), WithName("strings")),
		goObjects:        NewAutoIDRegistry[*GoObjectWrapper](0, WithName("goObjects")),
		bigInts:          NewAutoIDRegistry[*BigIntObject](0, WithName("bigInts")),
		cueContexts:      NewAutoIDRegistry[*CueContextObject](1, WithName("cueContexts")),
		cueValues:        NewAutoIDRegistry[*CueValueObject](1, WithName("cueValues")),
		tupleSpaces:      NewAutoIDRegistry[*TupleSpaceObject](1, WithName("tupleSpaces")),
		constraintStores: NewAutoIDRegistry[*ConstraintStoreObject](1, WithName("constraintStores")),
		// classValues is monotonic: *Class caches its assigned classValueID,
		// so reusing an ID would create a stale cache on the prior owner.
		// In practice classValues entries are never deleted, but we encode
		// the invariant here so it can't regress.
		classValues:      NewAutoIDRegistry[*Class](1, WithMonotonic(), WithName("classValues")),

		cells:     make(map[*Cell]struct{}),
		classVars: make(map[*Class]map[string]Value),
	}

	return or
}

// ---------------------------------------------------------------------------
// Exception Registry Methods (delegates to AutoIDRegistry)
// ---------------------------------------------------------------------------

func (or *ObjectRegistry) RegisterException(ex *ExceptionObject) uint32 { return or.exceptions.Register(ex) }
func (or *ObjectRegistry) GetException(id uint32) *ExceptionObject      { return or.exceptions.Get(id) }
func (or *ObjectRegistry) UnregisterException(id uint32)                { or.exceptions.Delete(id) }
func (or *ObjectRegistry) ExceptionCount() int                          { return or.exceptions.Count() }

// SweepExceptions removes handled exceptions from the registry.
func (or *ObjectRegistry) SweepExceptions() int {
	return or.exceptions.Sweep(func(_ uint32, ex *ExceptionObject) bool {
		return !ex.Handled
	})
}

// ---------------------------------------------------------------------------
// Result Registry Methods (delegates to AutoIDRegistry)
// ---------------------------------------------------------------------------

func (or *ObjectRegistry) RegisterResult(r *ResultObject) uint32 { return or.results.Register(r) }
func (or *ObjectRegistry) GetResult(id uint32) *ResultObject      { return or.results.Get(id) }
func (or *ObjectRegistry) UnregisterResult(id uint32)             { or.results.Delete(id) }
func (or *ObjectRegistry) ResultCount() int                       { return or.results.Count() }

// RegisterResultValue creates a Result, registers it, and returns a Value.
func (or *ObjectRegistry) RegisterResultValue(r *ResultObject) Value {
	id := or.RegisterResult(r)
	return FromSymbolID(id | resultMarker)
}

// GetResultFromValue retrieves a ResultObject from a Value.
// Returns nil if the Value is not a result.
func (or *ObjectRegistry) GetResultFromValue(v Value) *ResultObject {
	if !isResultValue(v) {
		return nil
	}
	id := v.SymbolID() & ^uint32(4<<24)
	return or.GetResult(id)
}

// ---------------------------------------------------------------------------
// Context Registry Methods (delegates to AutoIDRegistry)
// ---------------------------------------------------------------------------

func (or *ObjectRegistry) RegisterContext(ctx *ContextValue) uint32 { return or.contexts.Register(ctx) }
func (or *ObjectRegistry) GetContext(id uint32) *ContextValue       { return or.contexts.Get(id) }
func (or *ObjectRegistry) UnregisterContext(id uint32)              { or.contexts.Delete(id) }
func (or *ObjectRegistry) ClearContexts()                          { or.contexts.Clear() }
func (or *ObjectRegistry) ContextCount() int                       { return or.contexts.Count() }

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

func (or *ObjectRegistry) RegisterDictionary(d *DictionaryObject) uint32 { return or.dictionaries.Register(d) }
func (or *ObjectRegistry) GetDictionary(id uint32) *DictionaryObject     { return or.dictionaries.Get(id) }
func (or *ObjectRegistry) UnregisterDictionary(id uint32)                { or.dictionaries.Delete(id) }
func (or *ObjectRegistry) DictionaryCount() int                          { return or.dictionaries.Count() }

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
// String Registry Methods (delegates to AutoIDRegistry)
// ---------------------------------------------------------------------------

func (or *ObjectRegistry) RegisterString(s *StringObject) uint32 { return or.strings.Register(s) }
func (or *ObjectRegistry) GetString(id uint32) *StringObject     { return or.strings.Get(id) }
func (or *ObjectRegistry) UnregisterString(id uint32)            { or.strings.Delete(id) }
func (or *ObjectRegistry) StringCount() int                      { return or.strings.Count() }

// NewStringValue creates a Value from a Go string, registering it in the registry.
func (or *ObjectRegistry) NewStringValue(s string) Value {
	obj := &StringObject{Content: s}
	id := or.RegisterString(obj)
	return FromSymbolID(id)
}

// CompareStrings compares two string values under a single read lock.
// Returns true if both are valid strings with equal content.
// This is more efficient than two separate GetStringContent calls
// because it only acquires the lock once instead of twice.
func (or *ObjectRegistry) CompareStrings(a, b Value) bool {
	if !a.IsSymbolEncoded() || !b.IsSymbolEncoded() {
		return false
	}
	idA := a.SymbolID()
	idB := b.SymbolID()
	if idA < stringIDOffset || idB < stringIDOffset {
		return false
	}

	or.strings.RLock()
	defer or.strings.RUnlock()

	objA := or.strings.UnsafeGet(idA)
	objB := or.strings.UnsafeGet(idB)

	if objA == nil || objB == nil {
		return false
	}
	return objA.Content == objB.Content
}

// GetStringContent returns the Go string content of a string Value.
// Returns empty string if v is not a string.
func (or *ObjectRegistry) GetStringContent(v Value) string {
	if !v.IsSymbolEncoded() {
		return ""
	}
	id := v.SymbolID()
	if id < stringIDOffset {
		return "" // It's a regular symbol, not a string
	}

	or.strings.RLock()
	obj := or.strings.UnsafeGet(id)
	if obj == nil {
		or.strings.RUnlock()
		return ""
	}
	s := obj.Content
	or.strings.RUnlock()
	return s
}

// GetStringObject returns the StringObject for a Value.
// Returns nil if v is not a string.
func (or *ObjectRegistry) GetStringObject(v Value) *StringObject {
	if !v.IsSymbolEncoded() {
		return nil
	}
	id := v.SymbolID()
	if id < stringIDOffset {
		return nil
	}
	or.strings.RLock()
	obj := or.strings.UnsafeGet(id)
	or.strings.RUnlock()
	return obj
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
	id := or.classValues.Register(c)
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
	return or.classValues.Get(id)
}

// ClassValueCount returns the number of registered class values.
func (or *ObjectRegistry) ClassValueCount() int {
	return or.classValues.Count()
}

// ---------------------------------------------------------------------------
// CUE Context Registry Methods (delegates to AutoIDRegistry)
// ---------------------------------------------------------------------------

func (or *ObjectRegistry) RegisterCueContext(c *CueContextObject) uint32 { return or.cueContexts.Register(c) }
func (or *ObjectRegistry) GetCueContext(id uint32) *CueContextObject     { return or.cueContexts.Get(id) }
func (or *ObjectRegistry) UnregisterCueContext(id uint32)                { or.cueContexts.Delete(id) }
func (or *ObjectRegistry) CueContextCount() int                         { return or.cueContexts.Count() }

// ---------------------------------------------------------------------------
// CUE Value Registry Methods (delegates to AutoIDRegistry)
// ---------------------------------------------------------------------------

func (or *ObjectRegistry) RegisterCueValue(c *CueValueObject) uint32 { return or.cueValues.Register(c) }
func (or *ObjectRegistry) GetCueValue(id uint32) *CueValueObject     { return or.cueValues.Get(id) }
func (or *ObjectRegistry) UnregisterCueValue(id uint32)              { or.cueValues.Delete(id) }
func (or *ObjectRegistry) CueValueCount() int                        { return or.cueValues.Count() }

// ---------------------------------------------------------------------------
// TupleSpace Registry Methods (delegates to AutoIDRegistry)
// ---------------------------------------------------------------------------

func (or *ObjectRegistry) RegisterTupleSpace(ts *TupleSpaceObject) uint32 { return or.tupleSpaces.Register(ts) }
func (or *ObjectRegistry) GetTupleSpace(id uint32) *TupleSpaceObject      { return or.tupleSpaces.Get(id) }

// ---------------------------------------------------------------------------
// ConstraintStore Registry Methods (delegates to AutoIDRegistry)
// ---------------------------------------------------------------------------

func (or *ObjectRegistry) RegisterConstraintStore(cs *ConstraintStoreObject) uint32 { return or.constraintStores.Register(cs) }
func (or *ObjectRegistry) GetConstraintStore(id uint32) *ConstraintStoreObject      { return or.constraintStores.Get(id) }
func (or *ObjectRegistry) ConstraintStoreCount() int                                { return or.constraintStores.Count() }

// ---------------------------------------------------------------------------
// Extended Stats
// ---------------------------------------------------------------------------

// FullStats returns counts of all registered objects across all registries.
func (or *ObjectRegistry) FullStats() map[string]int {
	stats := or.ConcurrencyRegistry.Stats()
	for k, v := range or.IORegistry.IOStats() {
		stats[k] = v
	}
	stats["exceptions"] = or.ExceptionCount()
	stats["results"] = or.ResultCount()
	stats["contexts"] = or.ContextCount()
	stats["dictionaries"] = or.DictionaryCount()
	stats["strings"] = or.StringCount()
	stats["cells"] = or.CellCount()
	stats["classVarClasses"] = or.ClassVarCount()
	stats["goObjects"] = or.GoObjectCount()
	stats["bigInts"] = or.BigIntCount()
	stats["classValues"] = or.ClassValueCount()
	stats["cueContexts"] = or.CueContextCount()
	stats["cueValues"] = or.CueValueCount()
	stats["constraintStores"] = or.ConstraintStoreCount()
	return stats
}
