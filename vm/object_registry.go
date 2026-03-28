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

	// AutoIDRegistry-backed registries (storage + locking + ID allocation)
	exceptions       *AutoIDRegistry[*ExceptionObject]
	results          *AutoIDRegistry[*ResultObject]
	contexts         *AutoIDRegistry[*ContextValue]
	dictionaries     *AutoIDRegistry[*DictionaryObject]
	strings          *AutoIDRegistry[*StringObject]
	grpcClients      *AutoIDRegistry[*GrpcClientObject]
	grpcStreams      *AutoIDRegistry[*GrpcStreamObject]
	httpServers      *AutoIDRegistry[*HttpServerObject]
	httpClients      *AutoIDRegistry[*HttpClientObject]
	httpRequests     *AutoIDRegistry[*HttpRequestObject]
	httpResponses    *AutoIDRegistry[*HttpResponseObject]
	extProcesses     *AutoIDRegistry[*ExternalProcessObject]
	unixListeners    *AutoIDRegistry[*UnixListenerObject]
	unixConns        *AutoIDRegistry[*UnixConnObject]
	jsonReaders      *AutoIDRegistry[*JsonReaderObject]
	jsonWriters      *AutoIDRegistry[*JsonWriterObject]
	goObjects        *AutoIDRegistry[*GoObjectWrapper]
	bigInts          *AutoIDRegistry[*BigIntObject]
	cueContexts      *AutoIDRegistry[*CueContextObject]
	cueValues        *AutoIDRegistry[*CueValueObject]
	tupleSpaces      *AutoIDRegistry[*TupleSpaceObject]
	constraintStores *AutoIDRegistry[*ConstraintStoreObject]
	classValues      *AutoIDRegistry[*Class]

	// Special registries (not suitable for AutoIDRegistry)
	cells          map[*Cell]struct{} // set semantics
	cellsMu        sync.Mutex
	weakRefCounter atomic.Uint32       // counter only
	classVars      map[*Class]map[string]Value // nested map
	classVarsMu    sync.RWMutex
}

// NewObjectRegistry creates a new ObjectRegistry with all maps initialized.
func NewObjectRegistry() *ObjectRegistry {
	or := &ObjectRegistry{
		ConcurrencyRegistry: NewConcurrencyRegistry(),

		// Start IDs at 1 unless otherwise noted (0 = nil/uninitialized)
		exceptions:       NewAutoIDRegistry[*ExceptionObject](1),
		results:          NewAutoIDRegistry[*ResultObject](1),
		contexts:         NewAutoIDRegistry[*ContextValue](1),
		dictionaries:     NewAutoIDRegistry[*DictionaryObject](dictionaryIDOffset),
		strings:          NewAutoIDRegistry[*StringObject](stringIDOffset),
		grpcClients:      NewAutoIDRegistry[*GrpcClientObject](1),
		grpcStreams:      NewAutoIDRegistry[*GrpcStreamObject](1),
		httpServers:      NewAutoIDRegistry[*HttpServerObject](1),
		httpClients:      NewAutoIDRegistry[*HttpClientObject](1),
		httpRequests:     NewAutoIDRegistry[*HttpRequestObject](1),
		httpResponses:    NewAutoIDRegistry[*HttpResponseObject](1),
		extProcesses:     NewAutoIDRegistry[*ExternalProcessObject](1),
		unixListeners:    NewAutoIDRegistry[*UnixListenerObject](1),
		unixConns:        NewAutoIDRegistry[*UnixConnObject](1),
		jsonReaders:      NewAutoIDRegistry[*JsonReaderObject](1),
		jsonWriters:      NewAutoIDRegistry[*JsonWriterObject](1),
		goObjects:        NewAutoIDRegistry[*GoObjectWrapper](0),
		bigInts:          NewAutoIDRegistry[*BigIntObject](0),
		cueContexts:      NewAutoIDRegistry[*CueContextObject](1),
		cueValues:        NewAutoIDRegistry[*CueValueObject](1),
		tupleSpaces:      NewAutoIDRegistry[*TupleSpaceObject](1),
		constraintStores: NewAutoIDRegistry[*ConstraintStoreObject](1),
		classValues:      NewAutoIDRegistry[*Class](1),

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
	if !v.IsSymbol() {
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
	if !a.IsSymbol() || !b.IsSymbol() {
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
	if !v.IsSymbol() {
		return ""
	}
	id := v.SymbolID()
	if id < stringIDOffset {
		return "" // It's a regular symbol, not a string
	}

	obj := or.GetString(id)
	if obj != nil {
		return obj.Content
	}
	return ""
}

// GetStringObject returns the StringObject for a Value.
// Returns nil if v is not a string.
func (or *ObjectRegistry) GetStringObject(v Value) *StringObject {
	if !v.IsSymbol() {
		return nil
	}
	id := v.SymbolID()
	if id < stringIDOffset {
		return nil
	}
	return or.GetString(id)
}

// ---------------------------------------------------------------------------
// gRPC Registry Methods (delegates to AutoIDRegistry)
// ---------------------------------------------------------------------------

func (or *ObjectRegistry) RegisterGrpcClient(c *GrpcClientObject) uint32 { return or.grpcClients.Register(c) }
func (or *ObjectRegistry) GetGrpcClient(id uint32) *GrpcClientObject     { return or.grpcClients.Get(id) }
func (or *ObjectRegistry) UnregisterGrpcClient(id uint32)                { or.grpcClients.Delete(id) }
func (or *ObjectRegistry) GrpcClientCount() int                          { return or.grpcClients.Count() }

// SweepGrpcClients removes closed gRPC clients from the registry.
func (or *ObjectRegistry) SweepGrpcClients() int {
	return or.grpcClients.Sweep(func(_ uint32, c *GrpcClientObject) bool {
		return !c.closed.Load()
	})
}

func (or *ObjectRegistry) RegisterGrpcStream(s *GrpcStreamObject) uint32 { return or.grpcStreams.Register(s) }
func (or *ObjectRegistry) GetGrpcStream(id uint32) *GrpcStreamObject     { return or.grpcStreams.Get(id) }
func (or *ObjectRegistry) UnregisterGrpcStream(id uint32)                { or.grpcStreams.Delete(id) }
func (or *ObjectRegistry) GrpcStreamCount() int                          { return or.grpcStreams.Count() }

// ---------------------------------------------------------------------------
// HTTP Registry Methods (delegates to AutoIDRegistry)
// ---------------------------------------------------------------------------

func (or *ObjectRegistry) RegisterHttpServer(s *HttpServerObject) uint32 { return or.httpServers.Register(s) }
func (or *ObjectRegistry) GetHttpServer(id uint32) *HttpServerObject     { return or.httpServers.Get(id) }
func (or *ObjectRegistry) UnregisterHttpServer(id uint32)                { or.httpServers.Delete(id) }
func (or *ObjectRegistry) HttpServerCount() int                          { return or.httpServers.Count() }

// SweepHttpServers removes stopped HTTP servers from the registry.
func (or *ObjectRegistry) SweepHttpServers() int {
	return or.httpServers.Sweep(func(_ uint32, s *HttpServerObject) bool {
		return s.running.Load()
	})
}

func (or *ObjectRegistry) RegisterHttpClient(c *HttpClientObject) uint32 { return or.httpClients.Register(c) }
func (or *ObjectRegistry) GetHttpClient(id uint32) *HttpClientObject     { return or.httpClients.Get(id) }
func (or *ObjectRegistry) UnregisterHttpClient(id uint32)                { or.httpClients.Delete(id) }
func (or *ObjectRegistry) HttpClientCount() int                          { return or.httpClients.Count() }

func (or *ObjectRegistry) RegisterHttpRequest(req *HttpRequestObject) uint32 { return or.httpRequests.Register(req) }
func (or *ObjectRegistry) GetHttpRequest(id uint32) *HttpRequestObject       { return or.httpRequests.Get(id) }
func (or *ObjectRegistry) UnregisterHttpRequest(id uint32)                   { or.httpRequests.Delete(id) }
func (or *ObjectRegistry) HttpRequestCount() int                             { return or.httpRequests.Count() }

func (or *ObjectRegistry) RegisterHttpResponse(resp *HttpResponseObject) uint32 { return or.httpResponses.Register(resp) }
func (or *ObjectRegistry) GetHttpResponse(id uint32) *HttpResponseObject        { return or.httpResponses.Get(id) }
func (or *ObjectRegistry) UnregisterHttpResponse(id uint32)                     { or.httpResponses.Delete(id) }
func (or *ObjectRegistry) HttpResponseCount() int                               { return or.httpResponses.Count() }

// ---------------------------------------------------------------------------
// ExternalProcess Registry Methods (delegates to AutoIDRegistry)
// ---------------------------------------------------------------------------

func (or *ObjectRegistry) RegisterExternalProcess(p *ExternalProcessObject) uint32 { return or.extProcesses.Register(p) }
func (or *ObjectRegistry) GetExternalProcess(id uint32) *ExternalProcessObject     { return or.extProcesses.Get(id) }
func (or *ObjectRegistry) UnregisterExternalProcess(id uint32)                     { or.extProcesses.Delete(id) }

// ---------------------------------------------------------------------------
// JSON Registry Methods (delegates to AutoIDRegistry)
// ---------------------------------------------------------------------------

func (or *ObjectRegistry) RegisterJsonReader(r *JsonReaderObject) uint32 { return or.jsonReaders.Register(r) }
func (or *ObjectRegistry) GetJsonReader(id uint32) *JsonReaderObject     { return or.jsonReaders.Get(id) }

func (or *ObjectRegistry) RegisterJsonWriter(w *JsonWriterObject) uint32 { return or.jsonWriters.Register(w) }
func (or *ObjectRegistry) GetJsonWriter(id uint32) *JsonWriterObject     { return or.jsonWriters.Get(id) }

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
	or.cellsMu.Lock()
	defer or.cellsMu.Unlock()
	_, exists := or.cells[c]
	return exists
}

// CellCount returns the number of registered cells.
func (or *ObjectRegistry) CellCount() int {
	or.cellsMu.Lock()
	defer or.cellsMu.Unlock()
	return len(or.cells)
}

// ---------------------------------------------------------------------------
// Weak Reference Counter Methods
// ---------------------------------------------------------------------------

// NextWeakRefID returns the next unique ID for a weak reference.
func (or *ObjectRegistry) NextWeakRefID() uint32 {
	return or.weakRefCounter.Add(1)
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

	// Fast path: already registered
	if c.classValueID != 0 {
		return classToValue(c.classValueID)
	}

	// Slow path: register new
	id := or.classValues.Register(c)
	c.classValueID = id
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
// Unix Socket Registry Methods (delegates to AutoIDRegistry)
// ---------------------------------------------------------------------------

func (or *ObjectRegistry) RegisterUnixListener(l *UnixListenerObject) uint32 { return or.unixListeners.Register(l) }
func (or *ObjectRegistry) GetUnixListener(id uint32) *UnixListenerObject     { return or.unixListeners.Get(id) }
func (or *ObjectRegistry) UnregisterUnixListener(id uint32)                  { or.unixListeners.Delete(id) }
func (or *ObjectRegistry) UnixListenerCount() int                            { return or.unixListeners.Count() }

func (or *ObjectRegistry) RegisterUnixConn(c *UnixConnObject) uint32 { return or.unixConns.Register(c) }
func (or *ObjectRegistry) GetUnixConn(id uint32) *UnixConnObject     { return or.unixConns.Get(id) }
func (or *ObjectRegistry) UnregisterUnixConn(id uint32)              { or.unixConns.Delete(id) }
func (or *ObjectRegistry) UnixConnCount() int                        { return or.unixConns.Count() }

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
	stats["exceptions"] = or.ExceptionCount()
	stats["results"] = or.ResultCount()
	stats["contexts"] = or.ContextCount()
	stats["dictionaries"] = or.DictionaryCount()
	stats["strings"] = or.StringCount()
	stats["grpcClients"] = or.GrpcClientCount()
	stats["grpcStreams"] = or.GrpcStreamCount()
	stats["httpServers"] = or.HttpServerCount()
	stats["httpClients"] = or.HttpClientCount()
	stats["httpRequests"] = or.HttpRequestCount()
	stats["httpResponses"] = or.HttpResponseCount()
	stats["cells"] = or.CellCount()
	stats["classVarClasses"] = or.ClassVarCount()
	stats["goObjects"] = or.GoObjectCount()
	stats["bigInts"] = or.BigIntCount()
	stats["classValues"] = or.ClassValueCount()
	stats["unixListeners"] = or.UnixListenerCount()
	stats["unixConns"] = or.UnixConnCount()
	stats["cueContexts"] = or.CueContextCount()
	stats["cueValues"] = or.CueValueCount()
	stats["constraintStores"] = or.ConstraintStoreCount()
	return stats
}
