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

	// Exception registry
	exceptions   map[uint32]*ExceptionObject
	exceptionsMu sync.RWMutex
	exceptionID  atomic.Uint32

	// Result registry
	results   map[int]*ResultObject
	resultsMu sync.RWMutex
	resultID  atomic.Int32

	// Context registry
	contexts   map[uint32]*ContextValue
	contextsMu sync.RWMutex
	contextID  atomic.Uint32

	// Dictionary registry
	dictionaries   map[uint32]*DictionaryObject
	dictionariesMu sync.RWMutex
	dictionaryID   atomic.Uint32

	// String registry
	strings   map[uint32]*StringObject
	stringsMu sync.RWMutex
	stringID  atomic.Uint32

	// gRPC client registry
	grpcClients   map[int]*GrpcClientObject
	grpcClientsMu sync.RWMutex
	grpcClientID  atomic.Int32

	// gRPC stream registry
	grpcStreams   map[int]*GrpcStreamObject
	grpcStreamsMu sync.RWMutex
	grpcStreamID  atomic.Int32

	// HTTP server registry
	httpServers   map[int]*HttpServerObject
	httpServersMu sync.RWMutex
	httpServerID  atomic.Int32

	// HTTP request registry
	httpRequests   map[int]*HttpRequestObject
	httpRequestsMu sync.RWMutex
	httpRequestID  atomic.Int32

	// HTTP response registry
	httpResponses   map[int]*HttpResponseObject
	httpResponsesMu sync.RWMutex
	httpResponseID  atomic.Int32

	// Cell registry (no ID counter; cells register by pointer)
	cells   map[*Cell]struct{}
	cellsMu sync.Mutex

	// Weak reference ID counter
	weakRefCounter atomic.Uint32

	// Class variable storage
	classVars   map[*Class]map[string]Value
	classVarsMu sync.RWMutex
}

// NewObjectRegistry creates a new ObjectRegistry with all maps initialized.
func NewObjectRegistry() *ObjectRegistry {
	or := &ObjectRegistry{
		ConcurrencyRegistry: NewConcurrencyRegistry(),

		exceptions:    make(map[uint32]*ExceptionObject),
		results:       make(map[int]*ResultObject),
		contexts:      make(map[uint32]*ContextValue),
		dictionaries:  make(map[uint32]*DictionaryObject),
		strings:       make(map[uint32]*StringObject),
		grpcClients:   make(map[int]*GrpcClientObject),
		grpcStreams:    make(map[int]*GrpcStreamObject),
		httpServers:   make(map[int]*HttpServerObject),
		httpRequests:  make(map[int]*HttpRequestObject),
		httpResponses: make(map[int]*HttpResponseObject),
		cells:         make(map[*Cell]struct{}),
		classVars:     make(map[*Class]map[string]Value),
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
	or.httpRequestID.Store(1)
	or.httpResponseID.Store(1)
	or.weakRefCounter.Store(0)

	return or
}

// ---------------------------------------------------------------------------
// Exception Registry Methods
// ---------------------------------------------------------------------------

// RegisterException adds an exception to the registry and returns its ID.
func (or *ObjectRegistry) RegisterException(ex *ExceptionObject) uint32 {
	id := or.exceptionID.Add(1)

	or.exceptionsMu.Lock()
	or.exceptions[id] = ex
	or.exceptionsMu.Unlock()

	return id
}

// GetException retrieves an exception by its ID.
func (or *ObjectRegistry) GetException(id uint32) *ExceptionObject {
	or.exceptionsMu.RLock()
	defer or.exceptionsMu.RUnlock()
	return or.exceptions[id]
}

// UnregisterException removes an exception from the registry.
func (or *ObjectRegistry) UnregisterException(id uint32) {
	or.exceptionsMu.Lock()
	defer or.exceptionsMu.Unlock()
	delete(or.exceptions, id)
}

// SweepExceptions removes handled exceptions from the registry.
// Returns the number of exceptions swept.
func (or *ObjectRegistry) SweepExceptions() int {
	or.exceptionsMu.Lock()
	defer or.exceptionsMu.Unlock()

	swept := 0
	for id, ex := range or.exceptions {
		if ex.Handled {
			delete(or.exceptions, id)
			swept++
		}
	}
	return swept
}

// ExceptionCount returns the number of registered exceptions.
func (or *ObjectRegistry) ExceptionCount() int {
	or.exceptionsMu.RLock()
	defer or.exceptionsMu.RUnlock()
	return len(or.exceptions)
}

// ---------------------------------------------------------------------------
// Result Registry Methods
// ---------------------------------------------------------------------------

// RegisterResult adds a result to the registry and returns its ID.
func (or *ObjectRegistry) RegisterResult(r *ResultObject) int {
	id := int(or.resultID.Add(1) - 1)

	or.resultsMu.Lock()
	or.results[id] = r
	or.resultsMu.Unlock()

	return id
}

// GetResult retrieves a result by its ID.
func (or *ObjectRegistry) GetResult(id int) *ResultObject {
	or.resultsMu.RLock()
	defer or.resultsMu.RUnlock()
	return or.results[id]
}

// UnregisterResult removes a result from the registry.
func (or *ObjectRegistry) UnregisterResult(id int) {
	or.resultsMu.Lock()
	defer or.resultsMu.Unlock()
	delete(or.results, id)
}

// ResultCount returns the number of registered results.
func (or *ObjectRegistry) ResultCount() int {
	or.resultsMu.RLock()
	defer or.resultsMu.RUnlock()
	return len(or.results)
}

// ---------------------------------------------------------------------------
// Context Registry Methods
// ---------------------------------------------------------------------------

// RegisterContext adds a context to the registry and returns its ID.
func (or *ObjectRegistry) RegisterContext(ctx *ContextValue) uint32 {
	id := or.contextID.Add(1) - 1

	or.contextsMu.Lock()
	or.contexts[id] = ctx
	or.contextsMu.Unlock()

	return id
}

// GetContext retrieves a context by its ID.
func (or *ObjectRegistry) GetContext(id uint32) *ContextValue {
	or.contextsMu.RLock()
	defer or.contextsMu.RUnlock()
	return or.contexts[id]
}

// UnregisterContext removes a context from the registry.
func (or *ObjectRegistry) UnregisterContext(id uint32) {
	or.contextsMu.Lock()
	defer or.contextsMu.Unlock()
	delete(or.contexts, id)
}

// ClearContexts removes all contexts (for testing/reset).
func (or *ObjectRegistry) ClearContexts() {
	or.contextsMu.Lock()
	defer or.contextsMu.Unlock()
	or.contexts = make(map[uint32]*ContextValue)
}

// ContextCount returns the number of registered contexts.
func (or *ObjectRegistry) ContextCount() int {
	or.contextsMu.RLock()
	defer or.contextsMu.RUnlock()
	return len(or.contexts)
}

// ---------------------------------------------------------------------------
// Dictionary Registry Methods
// ---------------------------------------------------------------------------

// RegisterDictionary adds a dictionary to the registry and returns its ID.
func (or *ObjectRegistry) RegisterDictionary(d *DictionaryObject) uint32 {
	id := or.dictionaryID.Add(1) - 1

	or.dictionariesMu.Lock()
	or.dictionaries[id] = d
	or.dictionariesMu.Unlock()

	return id
}

// GetDictionary retrieves a dictionary by its ID.
func (or *ObjectRegistry) GetDictionary(id uint32) *DictionaryObject {
	or.dictionariesMu.RLock()
	defer or.dictionariesMu.RUnlock()
	return or.dictionaries[id]
}

// UnregisterDictionary removes a dictionary from the registry.
func (or *ObjectRegistry) UnregisterDictionary(id uint32) {
	or.dictionariesMu.Lock()
	defer or.dictionariesMu.Unlock()
	delete(or.dictionaries, id)
}

// DictionaryCount returns the number of registered dictionaries.
func (or *ObjectRegistry) DictionaryCount() int {
	or.dictionariesMu.RLock()
	defer or.dictionariesMu.RUnlock()
	return len(or.dictionaries)
}

// ---------------------------------------------------------------------------
// String Registry Methods
// ---------------------------------------------------------------------------

// RegisterString adds a string to the registry and returns its ID.
func (or *ObjectRegistry) RegisterString(s *StringObject) uint32 {
	id := or.stringID.Add(1) - 1

	or.stringsMu.Lock()
	or.strings[id] = s
	or.stringsMu.Unlock()

	return id
}

// GetString retrieves a string by its ID.
func (or *ObjectRegistry) GetString(id uint32) *StringObject {
	or.stringsMu.RLock()
	defer or.stringsMu.RUnlock()
	return or.strings[id]
}

// UnregisterString removes a string from the registry.
func (or *ObjectRegistry) UnregisterString(id uint32) {
	or.stringsMu.Lock()
	defer or.stringsMu.Unlock()
	delete(or.strings, id)
}

// StringCount returns the number of registered strings.
func (or *ObjectRegistry) StringCount() int {
	or.stringsMu.RLock()
	defer or.stringsMu.RUnlock()
	return len(or.strings)
}

// ---------------------------------------------------------------------------
// gRPC Client Registry Methods
// ---------------------------------------------------------------------------

// RegisterGrpcClient adds a gRPC client to the registry and returns its ID.
func (or *ObjectRegistry) RegisterGrpcClient(c *GrpcClientObject) int {
	id := int(or.grpcClientID.Add(1) - 1)

	or.grpcClientsMu.Lock()
	or.grpcClients[id] = c
	or.grpcClientsMu.Unlock()

	return id
}

// GetGrpcClient retrieves a gRPC client by its ID.
func (or *ObjectRegistry) GetGrpcClient(id int) *GrpcClientObject {
	or.grpcClientsMu.RLock()
	defer or.grpcClientsMu.RUnlock()
	return or.grpcClients[id]
}

// UnregisterGrpcClient removes a gRPC client from the registry.
func (or *ObjectRegistry) UnregisterGrpcClient(id int) {
	or.grpcClientsMu.Lock()
	defer or.grpcClientsMu.Unlock()
	delete(or.grpcClients, id)
}

// SweepGrpcClients removes closed gRPC clients from the registry.
// Returns the number of clients swept.
func (or *ObjectRegistry) SweepGrpcClients() int {
	or.grpcClientsMu.Lock()
	defer or.grpcClientsMu.Unlock()

	swept := 0
	for id, c := range or.grpcClients {
		if c.closed.Load() {
			delete(or.grpcClients, id)
			swept++
		}
	}
	return swept
}

// GrpcClientCount returns the number of registered gRPC clients.
func (or *ObjectRegistry) GrpcClientCount() int {
	or.grpcClientsMu.RLock()
	defer or.grpcClientsMu.RUnlock()
	return len(or.grpcClients)
}

// ---------------------------------------------------------------------------
// gRPC Stream Registry Methods
// ---------------------------------------------------------------------------

// RegisterGrpcStream adds a gRPC stream to the registry and returns its ID.
func (or *ObjectRegistry) RegisterGrpcStream(s *GrpcStreamObject) int {
	id := int(or.grpcStreamID.Add(1) - 1)

	or.grpcStreamsMu.Lock()
	or.grpcStreams[id] = s
	or.grpcStreamsMu.Unlock()

	return id
}

// GetGrpcStream retrieves a gRPC stream by its ID.
func (or *ObjectRegistry) GetGrpcStream(id int) *GrpcStreamObject {
	or.grpcStreamsMu.RLock()
	defer or.grpcStreamsMu.RUnlock()
	return or.grpcStreams[id]
}

// UnregisterGrpcStream removes a gRPC stream from the registry.
func (or *ObjectRegistry) UnregisterGrpcStream(id int) {
	or.grpcStreamsMu.Lock()
	defer or.grpcStreamsMu.Unlock()
	delete(or.grpcStreams, id)
}

// GrpcStreamCount returns the number of registered gRPC streams.
func (or *ObjectRegistry) GrpcStreamCount() int {
	or.grpcStreamsMu.RLock()
	defer or.grpcStreamsMu.RUnlock()
	return len(or.grpcStreams)
}

// ---------------------------------------------------------------------------
// HTTP Server Registry Methods
// ---------------------------------------------------------------------------

// RegisterHttpServer adds an HTTP server to the registry and returns its ID.
func (or *ObjectRegistry) RegisterHttpServer(s *HttpServerObject) int {
	id := int(or.httpServerID.Add(1) - 1)

	or.httpServersMu.Lock()
	or.httpServers[id] = s
	or.httpServersMu.Unlock()

	return id
}

// GetHttpServer retrieves an HTTP server by its ID.
func (or *ObjectRegistry) GetHttpServer(id int) *HttpServerObject {
	or.httpServersMu.RLock()
	defer or.httpServersMu.RUnlock()
	return or.httpServers[id]
}

// UnregisterHttpServer removes an HTTP server from the registry.
func (or *ObjectRegistry) UnregisterHttpServer(id int) {
	or.httpServersMu.Lock()
	defer or.httpServersMu.Unlock()
	delete(or.httpServers, id)
}

// SweepHttpServers removes stopped HTTP servers from the registry.
// Returns the number of servers swept.
func (or *ObjectRegistry) SweepHttpServers() int {
	or.httpServersMu.Lock()
	defer or.httpServersMu.Unlock()

	swept := 0
	for id, s := range or.httpServers {
		if !s.running.Load() {
			delete(or.httpServers, id)
			swept++
		}
	}
	return swept
}

// HttpServerCount returns the number of registered HTTP servers.
func (or *ObjectRegistry) HttpServerCount() int {
	or.httpServersMu.RLock()
	defer or.httpServersMu.RUnlock()
	return len(or.httpServers)
}

// ---------------------------------------------------------------------------
// HTTP Request Registry Methods
// ---------------------------------------------------------------------------

// RegisterHttpRequest adds an HTTP request to the registry and returns its ID.
func (or *ObjectRegistry) RegisterHttpRequest(req *HttpRequestObject) int {
	id := int(or.httpRequestID.Add(1) - 1)

	or.httpRequestsMu.Lock()
	or.httpRequests[id] = req
	or.httpRequestsMu.Unlock()

	return id
}

// GetHttpRequest retrieves an HTTP request by its ID.
func (or *ObjectRegistry) GetHttpRequest(id int) *HttpRequestObject {
	or.httpRequestsMu.RLock()
	defer or.httpRequestsMu.RUnlock()
	return or.httpRequests[id]
}

// UnregisterHttpRequest removes an HTTP request from the registry.
func (or *ObjectRegistry) UnregisterHttpRequest(id int) {
	or.httpRequestsMu.Lock()
	defer or.httpRequestsMu.Unlock()
	delete(or.httpRequests, id)
}

// HttpRequestCount returns the number of registered HTTP requests.
func (or *ObjectRegistry) HttpRequestCount() int {
	or.httpRequestsMu.RLock()
	defer or.httpRequestsMu.RUnlock()
	return len(or.httpRequests)
}

// ---------------------------------------------------------------------------
// HTTP Response Registry Methods
// ---------------------------------------------------------------------------

// RegisterHttpResponse adds an HTTP response to the registry and returns its ID.
func (or *ObjectRegistry) RegisterHttpResponse(resp *HttpResponseObject) int {
	id := int(or.httpResponseID.Add(1) - 1)

	or.httpResponsesMu.Lock()
	or.httpResponses[id] = resp
	or.httpResponsesMu.Unlock()

	return id
}

// GetHttpResponse retrieves an HTTP response by its ID.
func (or *ObjectRegistry) GetHttpResponse(id int) *HttpResponseObject {
	or.httpResponsesMu.RLock()
	defer or.httpResponsesMu.RUnlock()
	return or.httpResponses[id]
}

// UnregisterHttpResponse removes an HTTP response from the registry.
func (or *ObjectRegistry) UnregisterHttpResponse(id int) {
	or.httpResponsesMu.Lock()
	defer or.httpResponsesMu.Unlock()
	delete(or.httpResponses, id)
}

// HttpResponseCount returns the number of registered HTTP responses.
func (or *ObjectRegistry) HttpResponseCount() int {
	or.httpResponsesMu.RLock()
	defer or.httpResponsesMu.RUnlock()
	return len(or.httpResponses)
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
	stats["httpRequests"] = or.HttpRequestCount()
	stats["httpResponses"] = or.HttpResponseCount()
	stats["cells"] = or.CellCount()
	stats["classVarClasses"] = or.ClassVarCount()
	return stats
}
