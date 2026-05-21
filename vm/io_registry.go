package vm

// IORegistry manages registries for external I/O handles: HTTP,
// Unix sockets, JSON streaming, and external processes. Embedded in
// ObjectRegistry so all methods are promoted (zero caller changes).
type IORegistry struct {
	httpServers   *AutoIDRegistry[*HttpServerObject]
	httpClients   *AutoIDRegistry[*HttpClientObject]
	httpRequests  *AutoIDRegistry[*HttpRequestObject]
	httpResponses *AutoIDRegistry[*HttpResponseObject]
	extProcesses  *AutoIDRegistry[*ExternalProcessObject]
	unixListeners *AutoIDRegistry[*UnixListenerObject]
	unixConns     *AutoIDRegistry[*UnixConnObject]
	jsonReaders      *AutoIDRegistry[*JsonReaderObject]
	jsonWriters      *AutoIDRegistry[*JsonWriterObject]
	sseConnections   *AutoIDRegistry[*SSEConnectionObject]
	cliCommands      *AutoIDRegistry[*CliCommandWrapper]
}

// NewIORegistry creates an IORegistry with all sub-registries initialized.
func NewIORegistry() *IORegistry {
	return &IORegistry{
		httpServers:   NewAutoIDRegistry[*HttpServerObject](1, WithName("httpServers")),
		httpClients:   NewAutoIDRegistry[*HttpClientObject](1, WithName("httpClients")),
		httpRequests:  NewAutoIDRegistry[*HttpRequestObject](1, WithName("httpRequests")),
		httpResponses: NewAutoIDRegistry[*HttpResponseObject](1, WithName("httpResponses")),
		extProcesses:  NewAutoIDRegistry[*ExternalProcessObject](1, WithName("externalProcesses")),
		unixListeners: NewAutoIDRegistry[*UnixListenerObject](1, WithName("unixListeners")),
		unixConns:     NewAutoIDRegistry[*UnixConnObject](1, WithName("unixConns")),
		jsonReaders:      NewAutoIDRegistry[*JsonReaderObject](1, WithName("jsonReaders")),
		jsonWriters:      NewAutoIDRegistry[*JsonWriterObject](1, WithName("jsonWriters")),
		sseConnections:   NewAutoIDRegistry[*SSEConnectionObject](1, WithName("sseConnections")),
		cliCommands:      NewAutoIDRegistry[*CliCommandWrapper](1, WithName("cliCommands")),
	}
}

// ---------------------------------------------------------------------------
// HTTP
// ---------------------------------------------------------------------------

func (io *IORegistry) RegisterHttpServer(s *HttpServerObject) uint32 { return io.httpServers.Register(s) }
func (io *IORegistry) GetHttpServer(id uint32) *HttpServerObject     { return io.httpServers.Get(id) }
func (io *IORegistry) UnregisterHttpServer(id uint32)                { io.httpServers.Delete(id) }
func (io *IORegistry) HttpServerCount() int                          { return io.httpServers.Count() }

// SweepHttpServers removes stopped HTTP servers from the registry.
func (io *IORegistry) SweepHttpServers() int {
	return io.httpServers.Sweep(func(_ uint32, s *HttpServerObject) bool {
		return s.running.Load()
	})
}

func (io *IORegistry) RegisterHttpClient(c *HttpClientObject) uint32 { return io.httpClients.Register(c) }
func (io *IORegistry) GetHttpClient(id uint32) *HttpClientObject     { return io.httpClients.Get(id) }
func (io *IORegistry) UnregisterHttpClient(id uint32)                { io.httpClients.Delete(id) }
func (io *IORegistry) HttpClientCount() int                          { return io.httpClients.Count() }

func (io *IORegistry) RegisterHttpRequest(req *HttpRequestObject) uint32 { return io.httpRequests.Register(req) }
func (io *IORegistry) GetHttpRequest(id uint32) *HttpRequestObject       { return io.httpRequests.Get(id) }
func (io *IORegistry) UnregisterHttpRequest(id uint32)                   { io.httpRequests.Delete(id) }
func (io *IORegistry) HttpRequestCount() int                             { return io.httpRequests.Count() }

func (io *IORegistry) RegisterHttpResponse(resp *HttpResponseObject) uint32 { return io.httpResponses.Register(resp) }
func (io *IORegistry) GetHttpResponse(id uint32) *HttpResponseObject        { return io.httpResponses.Get(id) }
func (io *IORegistry) UnregisterHttpResponse(id uint32)                     { io.httpResponses.Delete(id) }
func (io *IORegistry) HttpResponseCount() int                               { return io.httpResponses.Count() }

// ---------------------------------------------------------------------------
// External Processes
// ---------------------------------------------------------------------------

func (io *IORegistry) RegisterExternalProcess(p *ExternalProcessObject) uint32 { return io.extProcesses.Register(p) }
func (io *IORegistry) GetExternalProcess(id uint32) *ExternalProcessObject     { return io.extProcesses.Get(id) }
func (io *IORegistry) UnregisterExternalProcess(id uint32)                     { io.extProcesses.Delete(id) }

// ---------------------------------------------------------------------------
// Unix Sockets
// ---------------------------------------------------------------------------

func (io *IORegistry) RegisterUnixListener(l *UnixListenerObject) uint32 { return io.unixListeners.Register(l) }
func (io *IORegistry) GetUnixListener(id uint32) *UnixListenerObject     { return io.unixListeners.Get(id) }
func (io *IORegistry) UnregisterUnixListener(id uint32)                  { io.unixListeners.Delete(id) }
func (io *IORegistry) UnixListenerCount() int                            { return io.unixListeners.Count() }

func (io *IORegistry) RegisterUnixConn(c *UnixConnObject) uint32 { return io.unixConns.Register(c) }
func (io *IORegistry) GetUnixConn(id uint32) *UnixConnObject     { return io.unixConns.Get(id) }
func (io *IORegistry) UnregisterUnixConn(id uint32)              { io.unixConns.Delete(id) }
func (io *IORegistry) UnixConnCount() int                        { return io.unixConns.Count() }

// ---------------------------------------------------------------------------
// JSON Streaming
// ---------------------------------------------------------------------------

func (io *IORegistry) RegisterJsonReader(r *JsonReaderObject) uint32 { return io.jsonReaders.Register(r) }
func (io *IORegistry) GetJsonReader(id uint32) *JsonReaderObject     { return io.jsonReaders.Get(id) }

func (io *IORegistry) RegisterJsonWriter(w *JsonWriterObject) uint32 { return io.jsonWriters.Register(w) }
func (io *IORegistry) GetJsonWriter(id uint32) *JsonWriterObject     { return io.jsonWriters.Get(id) }

// ---------------------------------------------------------------------------
// SSE Connections
// ---------------------------------------------------------------------------

func (io *IORegistry) RegisterSSEConnection(c *SSEConnectionObject) uint32 { return io.sseConnections.Register(c) }
func (io *IORegistry) GetSSEConnection(id uint32) *SSEConnectionObject     { return io.sseConnections.Get(id) }
func (io *IORegistry) UnregisterSSEConnection(id uint32)                   { io.sseConnections.Delete(id) }
func (io *IORegistry) SSEConnectionCount() int                             { return io.sseConnections.Count() }

// SweepSSEConnections removes closed SSE connections from the registry.
func (io *IORegistry) SweepSSEConnections() int {
	return io.sseConnections.Sweep(func(_ uint32, c *SSEConnectionObject) bool {
		return !c.closed.Load()
	})
}

// ---------------------------------------------------------------------------
// Cli commands (cobra-backed)
// ---------------------------------------------------------------------------

func (io *IORegistry) RegisterCliCommand(w *CliCommandWrapper) uint32 { return io.cliCommands.Register(w) }
func (io *IORegistry) GetCliCommand(id uint32) *CliCommandWrapper     { return io.cliCommands.Get(id) }
func (io *IORegistry) UnregisterCliCommand(id uint32)                 { io.cliCommands.Delete(id) }
func (io *IORegistry) CliCommandCount() int                           { return io.cliCommands.Count() }

// ---------------------------------------------------------------------------
// Stats
// ---------------------------------------------------------------------------

// IOStats returns counts for all I/O registries.
func (io *IORegistry) IOStats() map[string]int {
	return map[string]int{
		"httpServers":   io.HttpServerCount(),
		"httpClients":   io.HttpClientCount(),
		"httpRequests":  io.HttpRequestCount(),
		"httpResponses": io.HttpResponseCount(),
		"unixListeners":  io.UnixListenerCount(),
		"unixConns":      io.UnixConnCount(),
		"sseConnections": io.SSEConnectionCount(),
	}
}
