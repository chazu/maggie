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

	// TypedRegistry-backed registries (storage + locking delegated)
	exceptions    *TypedRegistry[uint32, *ExceptionObject]
	results       *TypedRegistry[int, *ResultObject]
	contexts      *TypedRegistry[uint32, *ContextValue]
	dictionaries  *TypedRegistry[uint32, *DictionaryObject]
	strings       *TypedRegistry[uint32, *StringObject]
	grpcClients   *TypedRegistry[int, *GrpcClientObject]
	grpcStreams   *TypedRegistry[int, *GrpcStreamObject]
	httpServers   *TypedRegistry[int, *HttpServerObject]
	httpClients   *TypedRegistry[int, *HttpClientObject]
	httpRequests  *TypedRegistry[int, *HttpRequestObject]
	httpResponses *TypedRegistry[int, *HttpResponseObject]
	extProcesses  *TypedRegistry[int, *ExternalProcessObject]
	unixListeners *TypedRegistry[int, *UnixListenerObject]
	unixConns     *TypedRegistry[int, *UnixConnObject]
	jsonReaders   *TypedRegistry[int, *JsonReaderObject]
	jsonWriters   *TypedRegistry[int, *JsonWriterObject]
	goObjects     *TypedRegistry[uint32, *GoObjectWrapper]
	bigInts       *TypedRegistry[uint32, *BigIntObject]
	cueContexts   *TypedRegistry[int, *CueContextObject]
	cueValues     *TypedRegistry[int, *CueValueObject]
	tupleSpaces   *TypedRegistry[int, *TupleSpaceObject]
	classValues   *TypedRegistry[int, *Class]

	// Atomic ID counters (allocation patterns vary per registry)
	exceptionID    atomic.Uint32
	resultID       atomic.Int32
	contextID      atomic.Uint32
	dictionaryID   atomic.Uint32
	stringID       atomic.Uint32
	grpcClientID   atomic.Int32
	grpcStreamID   atomic.Int32
	httpServerID   atomic.Int32
	httpClientID   atomic.Int32
	httpRequestID  atomic.Int32
	httpResponseID atomic.Int32
	extProcessID   atomic.Int32
	unixListenerID atomic.Int32
	unixConnID     atomic.Int32
	jsonReaderID   atomic.Int32
	jsonWriterID   atomic.Int32
	goObjectID     atomic.Uint32
	bigIntID       atomic.Uint32
	cueContextID   atomic.Int32
	cueValueID     atomic.Int32
	tupleSpaceID   atomic.Int32
	classValueID   atomic.Int32

	// Special registries (not suitable for TypedRegistry)
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

		exceptions:    NewTypedRegistry[uint32, *ExceptionObject](),
		results:       NewTypedRegistry[int, *ResultObject](),
		contexts:      NewTypedRegistry[uint32, *ContextValue](),
		dictionaries:  NewTypedRegistry[uint32, *DictionaryObject](),
		strings:       NewTypedRegistry[uint32, *StringObject](),
		grpcClients:   NewTypedRegistry[int, *GrpcClientObject](),
		grpcStreams:   NewTypedRegistry[int, *GrpcStreamObject](),
		httpServers:   NewTypedRegistry[int, *HttpServerObject](),
		httpClients:   NewTypedRegistry[int, *HttpClientObject](),
		httpRequests:  NewTypedRegistry[int, *HttpRequestObject](),
		httpResponses: NewTypedRegistry[int, *HttpResponseObject](),
		extProcesses:  NewTypedRegistry[int, *ExternalProcessObject](),
		unixListeners: NewTypedRegistry[int, *UnixListenerObject](),
		unixConns:     NewTypedRegistry[int, *UnixConnObject](),
		jsonReaders:   NewTypedRegistry[int, *JsonReaderObject](),
		jsonWriters:   NewTypedRegistry[int, *JsonWriterObject](),
		goObjects:     NewTypedRegistry[uint32, *GoObjectWrapper](),
		bigInts:       NewTypedRegistry[uint32, *BigIntObject](),
		cueContexts:   NewTypedRegistry[int, *CueContextObject](),
		cueValues:     NewTypedRegistry[int, *CueValueObject](),
		tupleSpaces:   NewTypedRegistry[int, *TupleSpaceObject](),
		classValues:   NewTypedRegistry[int, *Class](),

		cells:     make(map[*Cell]struct{}),
		classVars: make(map[*Class]map[string]Value),
	}

	// Start IDs at 1 (0 could be confused with nil/uninitialized)
	or.exceptionID.Store(1)
	or.resultID.Store(1)
	or.contextID.Store(1)
	or.dictionaryID.Store(dictionaryIDOffset)
	or.stringID.Store(stringIDOffset)
	or.grpcClientID.Store(1)
	or.grpcStreamID.Store(1)
	or.httpServerID.Store(1)
	or.httpClientID.Store(1)
	or.httpRequestID.Store(1)
	or.httpResponseID.Store(1)
	or.extProcessID.Store(1)
	or.jsonReaderID.Store(1)
	or.jsonWriterID.Store(1)
	or.weakRefCounter.Store(0)
	or.goObjectID.Store(0)
	or.bigIntID.Store(0)
	or.classValueID.Store(1)
	or.unixListenerID.Store(1)
	or.unixConnID.Store(1)
	or.cueContextID.Store(1)
	or.cueValueID.Store(1)
	or.tupleSpaceID.Store(1)

	return or
}

// ---------------------------------------------------------------------------
// Exception Registry Methods
// ---------------------------------------------------------------------------

// RegisterException adds an exception to the registry and returns its ID.
func (or *ObjectRegistry) RegisterException(ex *ExceptionObject) uint32 {
	id := or.exceptionID.Add(1)
	or.exceptions.Put(id, ex)
	return id
}

// GetException retrieves an exception by its ID.
func (or *ObjectRegistry) GetException(id uint32) *ExceptionObject {
	return or.exceptions.Get(id)
}

// UnregisterException removes an exception from the registry.
func (or *ObjectRegistry) UnregisterException(id uint32) {
	or.exceptions.Delete(id)
}

// SweepExceptions removes handled exceptions from the registry.
// Returns the number of exceptions swept.
func (or *ObjectRegistry) SweepExceptions() int {
	return or.exceptions.Sweep(func(_ uint32, ex *ExceptionObject) bool {
		return !ex.Handled
	})
}

// ExceptionCount returns the number of registered exceptions.
func (or *ObjectRegistry) ExceptionCount() int {
	return or.exceptions.Count()
}

// ---------------------------------------------------------------------------
// Result Registry Methods
// ---------------------------------------------------------------------------

// RegisterResult adds a result to the registry and returns its ID.
func (or *ObjectRegistry) RegisterResult(r *ResultObject) int {
	id := int(or.resultID.Add(1) - 1)
	or.results.Put(id, r)
	return id
}

// GetResult retrieves a result by its ID.
func (or *ObjectRegistry) GetResult(id int) *ResultObject {
	return or.results.Get(id)
}

// UnregisterResult removes a result from the registry.
func (or *ObjectRegistry) UnregisterResult(id int) {
	or.results.Delete(id)
}

// ResultCount returns the number of registered results.
func (or *ObjectRegistry) ResultCount() int {
	return or.results.Count()
}

// RegisterResultValue creates a Result, registers it, and returns a Value.
func (or *ObjectRegistry) RegisterResultValue(r *ResultObject) Value {
	id := or.RegisterResult(r)
	return FromSymbolID(uint32(id) | resultMarker)
}

// GetResultFromValue retrieves a ResultObject from a Value.
// Returns nil if the Value is not a result.
func (or *ObjectRegistry) GetResultFromValue(v Value) *ResultObject {
	if !isResultValue(v) {
		return nil
	}
	id := int(v.SymbolID() & ^uint32(4<<24))
	return or.GetResult(id)
}

// ---------------------------------------------------------------------------
// Context Registry Methods
// ---------------------------------------------------------------------------

// RegisterContext adds a context to the registry and returns its ID.
func (or *ObjectRegistry) RegisterContext(ctx *ContextValue) uint32 {
	id := or.contextID.Add(1) - 1
	or.contexts.Put(id, ctx)
	return id
}

// GetContext retrieves a context by its ID.
func (or *ObjectRegistry) GetContext(id uint32) *ContextValue {
	return or.contexts.Get(id)
}

// UnregisterContext removes a context from the registry.
func (or *ObjectRegistry) UnregisterContext(id uint32) {
	or.contexts.Delete(id)
}

// ClearContexts removes all contexts (for testing/reset).
func (or *ObjectRegistry) ClearContexts() {
	or.contexts.Clear()
}

// ContextCount returns the number of registered contexts.
func (or *ObjectRegistry) ContextCount() int {
	return or.contexts.Count()
}

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
// Dictionary Registry Methods
// ---------------------------------------------------------------------------

// RegisterDictionary adds a dictionary to the registry and returns its ID.
func (or *ObjectRegistry) RegisterDictionary(d *DictionaryObject) uint32 {
	id := or.dictionaryID.Add(1) - 1
	or.dictionaries.Put(id, d)
	return id
}

// GetDictionary retrieves a dictionary by its ID.
func (or *ObjectRegistry) GetDictionary(id uint32) *DictionaryObject {
	return or.dictionaries.Get(id)
}

// UnregisterDictionary removes a dictionary from the registry.
func (or *ObjectRegistry) UnregisterDictionary(id uint32) {
	or.dictionaries.Delete(id)
}

// DictionaryCount returns the number of registered dictionaries.
func (or *ObjectRegistry) DictionaryCount() int {
	return or.dictionaries.Count()
}

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
// String Registry Methods
// ---------------------------------------------------------------------------

// RegisterString adds a string to the registry and returns its ID.
func (or *ObjectRegistry) RegisterString(s *StringObject) uint32 {
	id := or.stringID.Add(1) - 1
	or.strings.Put(id, s)
	return id
}

// GetString retrieves a string by its ID.
func (or *ObjectRegistry) GetString(id uint32) *StringObject {
	return or.strings.Get(id)
}

// UnregisterString removes a string from the registry.
func (or *ObjectRegistry) UnregisterString(id uint32) {
	or.strings.Delete(id)
}

// StringCount returns the number of registered strings.
func (or *ObjectRegistry) StringCount() int {
	return or.strings.Count()
}

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
// gRPC Client Registry Methods
// ---------------------------------------------------------------------------

// RegisterGrpcClient adds a gRPC client to the registry and returns its ID.
func (or *ObjectRegistry) RegisterGrpcClient(c *GrpcClientObject) int {
	id := int(or.grpcClientID.Add(1) - 1)
	or.grpcClients.Put(id, c)
	return id
}

// GetGrpcClient retrieves a gRPC client by its ID.
func (or *ObjectRegistry) GetGrpcClient(id int) *GrpcClientObject {
	return or.grpcClients.Get(id)
}

// UnregisterGrpcClient removes a gRPC client from the registry.
func (or *ObjectRegistry) UnregisterGrpcClient(id int) {
	or.grpcClients.Delete(id)
}

// SweepGrpcClients removes closed gRPC clients from the registry.
// Returns the number of clients swept.
func (or *ObjectRegistry) SweepGrpcClients() int {
	return or.grpcClients.Sweep(func(_ int, c *GrpcClientObject) bool {
		return !c.closed.Load()
	})
}

// GrpcClientCount returns the number of registered gRPC clients.
func (or *ObjectRegistry) GrpcClientCount() int {
	return or.grpcClients.Count()
}

// ---------------------------------------------------------------------------
// gRPC Stream Registry Methods
// ---------------------------------------------------------------------------

// RegisterGrpcStream adds a gRPC stream to the registry and returns its ID.
func (or *ObjectRegistry) RegisterGrpcStream(s *GrpcStreamObject) int {
	id := int(or.grpcStreamID.Add(1) - 1)
	or.grpcStreams.Put(id, s)
	return id
}

// GetGrpcStream retrieves a gRPC stream by its ID.
func (or *ObjectRegistry) GetGrpcStream(id int) *GrpcStreamObject {
	return or.grpcStreams.Get(id)
}

// UnregisterGrpcStream removes a gRPC stream from the registry.
func (or *ObjectRegistry) UnregisterGrpcStream(id int) {
	or.grpcStreams.Delete(id)
}

// GrpcStreamCount returns the number of registered gRPC streams.
func (or *ObjectRegistry) GrpcStreamCount() int {
	return or.grpcStreams.Count()
}

// ---------------------------------------------------------------------------
// HTTP Server Registry Methods
// ---------------------------------------------------------------------------

// RegisterHttpServer adds an HTTP server to the registry and returns its ID.
func (or *ObjectRegistry) RegisterHttpServer(s *HttpServerObject) int {
	id := int(or.httpServerID.Add(1) - 1)
	or.httpServers.Put(id, s)
	return id
}

// GetHttpServer retrieves an HTTP server by its ID.
func (or *ObjectRegistry) GetHttpServer(id int) *HttpServerObject {
	return or.httpServers.Get(id)
}

// UnregisterHttpServer removes an HTTP server from the registry.
func (or *ObjectRegistry) UnregisterHttpServer(id int) {
	or.httpServers.Delete(id)
}

// SweepHttpServers removes stopped HTTP servers from the registry.
// Returns the number of servers swept.
func (or *ObjectRegistry) SweepHttpServers() int {
	return or.httpServers.Sweep(func(_ int, s *HttpServerObject) bool {
		return s.running.Load()
	})
}

// HttpServerCount returns the number of registered HTTP servers.
func (or *ObjectRegistry) HttpServerCount() int {
	return or.httpServers.Count()
}

// ---------------------------------------------------------------------------
// HTTP Client Registry Methods
// ---------------------------------------------------------------------------

// RegisterHttpClient adds an HTTP client to the registry and returns its ID.
func (or *ObjectRegistry) RegisterHttpClient(c *HttpClientObject) int {
	id := int(or.httpClientID.Add(1) - 1)
	or.httpClients.Put(id, c)
	return id
}

// GetHttpClient retrieves an HTTP client by its ID.
func (or *ObjectRegistry) GetHttpClient(id int) *HttpClientObject {
	return or.httpClients.Get(id)
}

// UnregisterHttpClient removes an HTTP client from the registry.
func (or *ObjectRegistry) UnregisterHttpClient(id int) {
	or.httpClients.Delete(id)
}

// HttpClientCount returns the number of registered HTTP clients.
func (or *ObjectRegistry) HttpClientCount() int {
	return or.httpClients.Count()
}

// ---------------------------------------------------------------------------
// HTTP Request Registry Methods
// ---------------------------------------------------------------------------

// RegisterHttpRequest adds an HTTP request to the registry and returns its ID.
func (or *ObjectRegistry) RegisterHttpRequest(req *HttpRequestObject) int {
	id := int(or.httpRequestID.Add(1) - 1)
	or.httpRequests.Put(id, req)
	return id
}

// GetHttpRequest retrieves an HTTP request by its ID.
func (or *ObjectRegistry) GetHttpRequest(id int) *HttpRequestObject {
	return or.httpRequests.Get(id)
}

// UnregisterHttpRequest removes an HTTP request from the registry.
func (or *ObjectRegistry) UnregisterHttpRequest(id int) {
	or.httpRequests.Delete(id)
}

// HttpRequestCount returns the number of registered HTTP requests.
func (or *ObjectRegistry) HttpRequestCount() int {
	return or.httpRequests.Count()
}

// ---------------------------------------------------------------------------
// HTTP Response Registry Methods
// ---------------------------------------------------------------------------

// RegisterHttpResponse adds an HTTP response to the registry and returns its ID.
func (or *ObjectRegistry) RegisterHttpResponse(resp *HttpResponseObject) int {
	id := int(or.httpResponseID.Add(1) - 1)
	or.httpResponses.Put(id, resp)
	return id
}

// GetHttpResponse retrieves an HTTP response by its ID.
func (or *ObjectRegistry) GetHttpResponse(id int) *HttpResponseObject {
	return or.httpResponses.Get(id)
}

// UnregisterHttpResponse removes an HTTP response from the registry.
func (or *ObjectRegistry) UnregisterHttpResponse(id int) {
	or.httpResponses.Delete(id)
}

// HttpResponseCount returns the number of registered HTTP responses.
func (or *ObjectRegistry) HttpResponseCount() int {
	return or.httpResponses.Count()
}

// ---------------------------------------------------------------------------
// ExternalProcess Registry Methods
// ---------------------------------------------------------------------------

// RegisterExternalProcess adds an external process to the registry and returns its ID.
func (or *ObjectRegistry) RegisterExternalProcess(p *ExternalProcessObject) int {
	id := int(or.extProcessID.Add(1) - 1)
	or.extProcesses.Put(id, p)
	return id
}

// GetExternalProcess retrieves an external process by its ID.
func (or *ObjectRegistry) GetExternalProcess(id int) *ExternalProcessObject {
	return or.extProcesses.Get(id)
}

// UnregisterExternalProcess removes an external process from the registry.
func (or *ObjectRegistry) UnregisterExternalProcess(id int) {
	or.extProcesses.Delete(id)
}

// ---------------------------------------------------------------------------
// JSON Reader Registry Methods
// ---------------------------------------------------------------------------

// RegisterJsonReader adds a JSON reader to the registry and returns its ID.
func (or *ObjectRegistry) RegisterJsonReader(r *JsonReaderObject) int {
	id := int(or.jsonReaderID.Add(1) - 1)
	or.jsonReaders.Put(id, r)
	return id
}

// GetJsonReader retrieves a JSON reader by its ID.
func (or *ObjectRegistry) GetJsonReader(id int) *JsonReaderObject {
	return or.jsonReaders.Get(id)
}

// ---------------------------------------------------------------------------
// JSON Writer Registry Methods
// ---------------------------------------------------------------------------

// RegisterJsonWriter adds a JSON writer to the registry and returns its ID.
func (or *ObjectRegistry) RegisterJsonWriter(w *JsonWriterObject) int {
	id := int(or.jsonWriterID.Add(1) - 1)
	or.jsonWriters.Put(id, w)
	return id
}

// GetJsonWriter retrieves a JSON writer by its ID.
func (or *ObjectRegistry) GetJsonWriter(id int) *JsonWriterObject {
	return or.jsonWriters.Get(id)
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
	id := int(or.classValueID.Add(1) - 1)
	or.classValues.Put(id, c)
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
// Unix Listener Registry Methods
// ---------------------------------------------------------------------------

// RegisterUnixListener adds a Unix listener to the registry and returns its ID.
func (or *ObjectRegistry) RegisterUnixListener(l *UnixListenerObject) int {
	id := int(or.unixListenerID.Add(1) - 1)
	or.unixListeners.Put(id, l)
	return id
}

// GetUnixListener retrieves a Unix listener by its ID.
func (or *ObjectRegistry) GetUnixListener(id int) *UnixListenerObject {
	return or.unixListeners.Get(id)
}

// UnregisterUnixListener removes a Unix listener from the registry.
func (or *ObjectRegistry) UnregisterUnixListener(id int) {
	or.unixListeners.Delete(id)
}

// UnixListenerCount returns the number of registered Unix listeners.
func (or *ObjectRegistry) UnixListenerCount() int {
	return or.unixListeners.Count()
}

// ---------------------------------------------------------------------------
// Unix Connection Registry Methods
// ---------------------------------------------------------------------------

// RegisterUnixConn adds a Unix connection to the registry and returns its ID.
func (or *ObjectRegistry) RegisterUnixConn(c *UnixConnObject) int {
	id := int(or.unixConnID.Add(1) - 1)
	or.unixConns.Put(id, c)
	return id
}

// GetUnixConn retrieves a Unix connection by its ID.
func (or *ObjectRegistry) GetUnixConn(id int) *UnixConnObject {
	return or.unixConns.Get(id)
}

// UnregisterUnixConn removes a Unix connection from the registry.
func (or *ObjectRegistry) UnregisterUnixConn(id int) {
	or.unixConns.Delete(id)
}

// UnixConnCount returns the number of registered Unix connections.
func (or *ObjectRegistry) UnixConnCount() int {
	return or.unixConns.Count()
}

// ---------------------------------------------------------------------------
// CUE Context Registry Methods
// ---------------------------------------------------------------------------

// RegisterCueContext adds a CUE context to the registry and returns its ID.
func (or *ObjectRegistry) RegisterCueContext(c *CueContextObject) int {
	id := int(or.cueContextID.Add(1) - 1)
	or.cueContexts.Put(id, c)
	return id
}

// GetCueContext retrieves a CUE context by its ID.
func (or *ObjectRegistry) GetCueContext(id int) *CueContextObject {
	return or.cueContexts.Get(id)
}

// UnregisterCueContext removes a CUE context from the registry.
func (or *ObjectRegistry) UnregisterCueContext(id int) {
	or.cueContexts.Delete(id)
}

// CueContextCount returns the number of registered CUE contexts.
func (or *ObjectRegistry) CueContextCount() int {
	return or.cueContexts.Count()
}

// ---------------------------------------------------------------------------
// CUE Value Registry Methods
// ---------------------------------------------------------------------------

// RegisterCueValue adds a CUE value to the registry and returns its ID.
func (or *ObjectRegistry) RegisterCueValue(c *CueValueObject) int {
	id := int(or.cueValueID.Add(1) - 1)
	or.cueValues.Put(id, c)
	return id
}

// GetCueValue retrieves a CUE value by its ID.
func (or *ObjectRegistry) GetCueValue(id int) *CueValueObject {
	return or.cueValues.Get(id)
}

// UnregisterCueValue removes a CUE value from the registry.
func (or *ObjectRegistry) UnregisterCueValue(id int) {
	or.cueValues.Delete(id)
}

// CueValueCount returns the number of registered CUE values.
func (or *ObjectRegistry) CueValueCount() int {
	return or.cueValues.Count()
}

// ---------------------------------------------------------------------------
// TupleSpace Registry Methods
// ---------------------------------------------------------------------------

// RegisterTupleSpace adds a tuple space to the registry and returns its ID.
func (or *ObjectRegistry) RegisterTupleSpace(ts *TupleSpaceObject) int {
	id := int(or.tupleSpaceID.Add(1) - 1)
	or.tupleSpaces.Put(id, ts)
	return id
}

// GetTupleSpace retrieves a tuple space by its ID.
func (or *ObjectRegistry) GetTupleSpace(id int) *TupleSpaceObject {
	return or.tupleSpaces.Get(id)
}

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
	return stats
}
