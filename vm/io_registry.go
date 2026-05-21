package vm

// IORegistry manages registries for external I/O handles: HTTP,
// Unix sockets, JSON streaming, and external processes. Embedded in
// ObjectRegistry so all methods are promoted (zero caller changes).
//
// Fields are exported so primitives can call Register/Get/Delete
// directly on the typed AutoIDRegistry without delegation methods.
// Legacy delegation methods below are retained for existing callers.
type IORegistry struct {
	HttpServers   *AutoIDRegistry[*HttpServerObject]
	HttpClients   *AutoIDRegistry[*HttpClientObject]
	HttpRequests  *AutoIDRegistry[*HttpRequestObject]
	HttpResponses *AutoIDRegistry[*HttpResponseObject]
	ExtProcesses  *AutoIDRegistry[*ExternalProcessObject]
	UnixListeners *AutoIDRegistry[*UnixListenerObject]
	UnixConns     *AutoIDRegistry[*UnixConnObject]
	JsonReaders      *AutoIDRegistry[*JsonReaderObject]
	JsonWriters      *AutoIDRegistry[*JsonWriterObject]
	SSEConnections   *AutoIDRegistry[*SSEConnectionObject]
	CliCommands      *AutoIDRegistry[*CliCommandWrapper]
}

// NewIORegistry creates an IORegistry with all sub-registries initialized.
func NewIORegistry() *IORegistry {
	return &IORegistry{
		HttpServers:   NewAutoIDRegistry[*HttpServerObject](1, WithName("httpServers")),
		HttpClients:   NewAutoIDRegistry[*HttpClientObject](1, WithName("httpClients")),
		HttpRequests:  NewAutoIDRegistry[*HttpRequestObject](1, WithName("httpRequests")),
		HttpResponses: NewAutoIDRegistry[*HttpResponseObject](1, WithName("httpResponses")),
		ExtProcesses:  NewAutoIDRegistry[*ExternalProcessObject](1, WithName("externalProcesses")),
		UnixListeners: NewAutoIDRegistry[*UnixListenerObject](1, WithName("unixListeners")),
		UnixConns:     NewAutoIDRegistry[*UnixConnObject](1, WithName("unixConns")),
		JsonReaders:      NewAutoIDRegistry[*JsonReaderObject](1, WithName("jsonReaders")),
		JsonWriters:      NewAutoIDRegistry[*JsonWriterObject](1, WithName("jsonWriters")),
		SSEConnections:   NewAutoIDRegistry[*SSEConnectionObject](1, WithName("sseConnections")),
		CliCommands:      NewAutoIDRegistry[*CliCommandWrapper](1, WithName("cliCommands")),
	}
}

// ---------------------------------------------------------------------------
// HTTP
// ---------------------------------------------------------------------------

func (io *IORegistry) RegisterHttpServer(s *HttpServerObject) uint32 { return io.HttpServers.Register(s) }
func (io *IORegistry) GetHttpServer(id uint32) *HttpServerObject     { return io.HttpServers.Get(id) }
func (io *IORegistry) UnregisterHttpServer(id uint32)                { io.HttpServers.Delete(id) }
func (io *IORegistry) HttpServerCount() int                          { return io.HttpServers.Count() }

// SweepHttpServers removes stopped HTTP servers from the registry.
func (io *IORegistry) SweepHttpServers() int {
	return io.HttpServers.Sweep(func(_ uint32, s *HttpServerObject) bool {
		return s.running.Load()
	})
}

func (io *IORegistry) RegisterHttpClient(c *HttpClientObject) uint32 { return io.HttpClients.Register(c) }
func (io *IORegistry) GetHttpClient(id uint32) *HttpClientObject     { return io.HttpClients.Get(id) }
func (io *IORegistry) UnregisterHttpClient(id uint32)                { io.HttpClients.Delete(id) }
func (io *IORegistry) HttpClientCount() int                          { return io.HttpClients.Count() }

func (io *IORegistry) RegisterHttpRequest(req *HttpRequestObject) uint32 { return io.HttpRequests.Register(req) }
func (io *IORegistry) GetHttpRequest(id uint32) *HttpRequestObject       { return io.HttpRequests.Get(id) }
func (io *IORegistry) UnregisterHttpRequest(id uint32)                   { io.HttpRequests.Delete(id) }
func (io *IORegistry) HttpRequestCount() int                             { return io.HttpRequests.Count() }

func (io *IORegistry) RegisterHttpResponse(resp *HttpResponseObject) uint32 { return io.HttpResponses.Register(resp) }
func (io *IORegistry) GetHttpResponse(id uint32) *HttpResponseObject        { return io.HttpResponses.Get(id) }
func (io *IORegistry) UnregisterHttpResponse(id uint32)                     { io.HttpResponses.Delete(id) }
func (io *IORegistry) HttpResponseCount() int                               { return io.HttpResponses.Count() }

// ---------------------------------------------------------------------------
// External Processes
// ---------------------------------------------------------------------------

func (io *IORegistry) RegisterExternalProcess(p *ExternalProcessObject) uint32 { return io.ExtProcesses.Register(p) }
func (io *IORegistry) GetExternalProcess(id uint32) *ExternalProcessObject     { return io.ExtProcesses.Get(id) }
func (io *IORegistry) UnregisterExternalProcess(id uint32)                     { io.ExtProcesses.Delete(id) }

// ---------------------------------------------------------------------------
// Unix Sockets
// ---------------------------------------------------------------------------

func (io *IORegistry) RegisterUnixListener(l *UnixListenerObject) uint32 { return io.UnixListeners.Register(l) }
func (io *IORegistry) GetUnixListener(id uint32) *UnixListenerObject     { return io.UnixListeners.Get(id) }
func (io *IORegistry) UnregisterUnixListener(id uint32)                  { io.UnixListeners.Delete(id) }
func (io *IORegistry) UnixListenerCount() int                            { return io.UnixListeners.Count() }

func (io *IORegistry) RegisterUnixConn(c *UnixConnObject) uint32 { return io.UnixConns.Register(c) }
func (io *IORegistry) GetUnixConn(id uint32) *UnixConnObject     { return io.UnixConns.Get(id) }
func (io *IORegistry) UnregisterUnixConn(id uint32)              { io.UnixConns.Delete(id) }
func (io *IORegistry) UnixConnCount() int                        { return io.UnixConns.Count() }

// ---------------------------------------------------------------------------
// JSON Streaming
// ---------------------------------------------------------------------------

func (io *IORegistry) RegisterJsonReader(r *JsonReaderObject) uint32 { return io.JsonReaders.Register(r) }
func (io *IORegistry) GetJsonReader(id uint32) *JsonReaderObject     { return io.JsonReaders.Get(id) }

func (io *IORegistry) RegisterJsonWriter(w *JsonWriterObject) uint32 { return io.JsonWriters.Register(w) }
func (io *IORegistry) GetJsonWriter(id uint32) *JsonWriterObject     { return io.JsonWriters.Get(id) }

// ---------------------------------------------------------------------------
// SSE Connections
// ---------------------------------------------------------------------------

func (io *IORegistry) RegisterSSEConnection(c *SSEConnectionObject) uint32 { return io.SSEConnections.Register(c) }
func (io *IORegistry) GetSSEConnection(id uint32) *SSEConnectionObject     { return io.SSEConnections.Get(id) }
func (io *IORegistry) UnregisterSSEConnection(id uint32)                   { io.SSEConnections.Delete(id) }
func (io *IORegistry) SSEConnectionCount() int                             { return io.SSEConnections.Count() }

// SweepSSEConnections removes closed SSE connections from the registry.
func (io *IORegistry) SweepSSEConnections() int {
	return io.SSEConnections.Sweep(func(_ uint32, c *SSEConnectionObject) bool {
		return !c.closed.Load()
	})
}

// ---------------------------------------------------------------------------
// Cli commands (cobra-backed)
// ---------------------------------------------------------------------------

func (io *IORegistry) RegisterCliCommand(w *CliCommandWrapper) uint32 { return io.CliCommands.Register(w) }
func (io *IORegistry) GetCliCommand(id uint32) *CliCommandWrapper     { return io.CliCommands.Get(id) }
func (io *IORegistry) UnregisterCliCommand(id uint32)                 { io.CliCommands.Delete(id) }
func (io *IORegistry) CliCommandCount() int                           { return io.CliCommands.Count() }

// ---------------------------------------------------------------------------
// Stats
// ---------------------------------------------------------------------------

// IOStats returns counts for all I/O registries.
func (io *IORegistry) IOStats() map[string]int {
	return map[string]int{
		"httpServers":    io.HttpServers.Count(),
		"httpClients":    io.HttpClients.Count(),
		"httpRequests":   io.HttpRequests.Count(),
		"httpResponses":  io.HttpResponses.Count(),
		"unixListeners":  io.UnixListeners.Count(),
		"unixConns":      io.UnixConns.Count(),
		"sseConnections": io.SSEConnections.Count(),
	}
}
