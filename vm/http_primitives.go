package vm

import (
	"context"
	"fmt"
	"io"
	"net/http"
	"strings"
	"sync"
	"sync/atomic"
	"time"
)

// ---------------------------------------------------------------------------
// HttpClient Registry
// ---------------------------------------------------------------------------

// HttpClientObject wraps Go's net/http.Client for use in Maggie.
type HttpClientObject struct {
	client *http.Client
}


func (vm *VM) vmGetHttpClient(v Value) *HttpClientObject {
	if o := ExtensionObject(v, httpClientMarker); o != nil {
		return o.(*HttpClientObject)
	}
	return nil
}

func (vm *VM) vmRegisterHttpClient(c *HttpClientObject) Value {
	return makeExtensionValue(httpClientMarker, c)
}

// ---------------------------------------------------------------------------
// HttpServer Registry
// ---------------------------------------------------------------------------

// HttpServerObject wraps Go's net/http server for use in Maggie.
type HttpServerObject struct {
	mux     *http.ServeMux
	server  *http.Server
	port    int
	running atomic.Bool
	mu      sync.Mutex
}

func isHttpServerValue(v Value) bool {
	return isExtensionValue(v, httpServerMarker)
}

func (vm *VM) vmGetHttpServer(v Value) *HttpServerObject {
	if o := ExtensionObject(v, httpServerMarker); o != nil {
		return o.(*HttpServerObject)
	}
	return nil
}

func (vm *VM) vmRegisterHttpServer(s *HttpServerObject) Value {
	return makeExtensionValue(httpServerMarker, s)
}

// vmUnregisterHttpServer is now a no-op: the server object is reclaimed by Go's
// GC once no Value references it. Retained so existing call sites (the `stop`
// primitive) need not change.
func (vm *VM) vmUnregisterHttpServer(v Value) {}

// ---------------------------------------------------------------------------
// HttpRequest Registry
// ---------------------------------------------------------------------------

type HttpRequestObject struct {
	request  *http.Request
	body     string
	bodyRead bool
}

func isHttpRequestValue(v Value) bool {
	return isExtensionValue(v, httpRequestMarker)
}

func (vm *VM) vmGetHttpRequest(v Value) *HttpRequestObject {
	if o := ExtensionObject(v, httpRequestMarker); o != nil {
		return o.(*HttpRequestObject)
	}
	return nil
}

func (vm *VM) vmRegisterHttpRequest(req *HttpRequestObject) Value {
	return makeExtensionValue(httpRequestMarker, req)
}

// vmUnregisterHttpRequest is now a no-op (Go GC reclaims the request object).
// Retained so the per-request `defer` call sites need not change.
func (vm *VM) vmUnregisterHttpRequest(v Value) {}

// ---------------------------------------------------------------------------
// HttpResponse Registry
// ---------------------------------------------------------------------------

type HttpResponseObject struct {
	status  int
	body    string
	headers map[string]string
}

func isHttpResponseValue(v Value) bool {
	return isExtensionValue(v, httpResponseMarker)
}

func (vm *VM) vmGetHttpResponse(v Value) *HttpResponseObject {
	if o := ExtensionObject(v, httpResponseMarker); o != nil {
		return o.(*HttpResponseObject)
	}
	return nil
}

func (vm *VM) vmRegisterHttpResponse(resp *HttpResponseObject) Value {
	return makeExtensionValue(httpResponseMarker, resp)
}

// httpResult is a fully Go-native snapshot of a handler's response, built by
// extractHTTPResult while the producing interpreter is still registered so
// header/body accessor sends resolve on the right interpreter. (Values are
// Go-GC-traced pointers; no collector can sweep them out from under the
// net/http write path.)
type httpResult struct {
	status  int
	headers map[string]string
	body    []byte
	hasBody bool
}

// extractHTTPResult marshals a handler's return Value into Go-native types
// so the response can be written from any goroutine without touching
// interpreter state.
func (vm *VM) extractHTTPResult(result Value) httpResult {
	if resp := vm.vmGetHttpResponse(result); resp != nil {
		var headers map[string]string
		if len(resp.headers) > 0 {
			headers = make(map[string]string, len(resp.headers))
			for k, hv := range resp.headers {
				headers[k] = hv
			}
		}
		// resp.body is already a Go string (copied at construction); []byte
		// re-copies it into a slice the write path owns outright.
		return httpResult{status: resp.status, headers: headers, body: []byte(resp.body), hasBody: true}
	}
	if IsStringValue(result) {
		return httpResult{status: http.StatusOK, body: []byte(vm.registry.GetStringContent(result)), hasBody: true}
	}
	return httpResult{status: http.StatusOK, hasBody: false}
}

// ---------------------------------------------------------------------------
// SSEConnection Registry
// ---------------------------------------------------------------------------

// SSEConnectionObject represents a Server-Sent Events connection.
// Go holds the HTTP connection open; Maggie sends events via a channel.
type SSEConnectionObject struct {
	eventCh chan sseEvent
	done    <-chan struct{} // r.Context().Done()
	closed  atomic.Bool
}

type sseEvent struct {
	event string // SSE event type (optional)
	data  string // SSE data payload
}

func (vm *VM) vmGetSSEConnection(v Value) *SSEConnectionObject {
	if o := ExtensionObject(v, sseConnectionMarker); o != nil {
		return o.(*SSEConnectionObject)
	}
	return nil
}

func (vm *VM) vmRegisterSSEConnection(c *SSEConnectionObject) Value {
	return makeExtensionValue(sseConnectionMarker, c)
}

// ---------------------------------------------------------------------------
// HttpServer Primitives Registration
// ---------------------------------------------------------------------------

func (vm *VM) registerHttpPrimitives() {
	httpServerClass := vm.createClass("HttpServer", vm.ObjectClass)
	httpRequestClass := vm.createClass("HttpRequest", vm.ObjectClass)
	httpResponseClass := vm.createClass("HttpResponse", vm.ObjectClass)

	vm.globals["HttpServer"] = vm.classValue(httpServerClass)
	vm.globals["HttpRequest"] = vm.classValue(httpRequestClass)
	vm.globals["HttpResponse"] = vm.classValue(httpResponseClass)

	vm.symbolDispatch.Register(httpServerMarker, &SymbolTypeEntry{Class: httpServerClass})
	vm.symbolDispatch.Register(httpRequestMarker, &SymbolTypeEntry{Class: httpRequestClass})
	vm.symbolDispatch.Register(httpResponseMarker, &SymbolTypeEntry{Class: httpResponseClass})

	httpServerClass.AddClassMethod1(vm.Selectors, "new:", func(v *VM, recv Value, portVal Value) Value {
		if !portVal.IsSmallInt() {
			return Nil
		}
		port := int(portVal.SmallInt())
		mux := http.NewServeMux()
		srv := &HttpServerObject{
			mux: mux,
			server: &http.Server{
				Addr:         fmt.Sprintf(":%d", port),
				Handler:      mux,
				ReadTimeout:  30 * time.Second,
				WriteTimeout: 30 * time.Second,
			},
			port: port,
		}
		return v.vmRegisterHttpServer(srv)
	})

	httpServerClass.AddMethod2(vm.Selectors, "serveStatic:from:", func(v *VM, recv Value, urlPathVal, dirPathVal Value) Value {
		srv := v.vmGetHttpServer(recv)
		if srv == nil {
			return Nil
		}
		urlPath := v.valueToString(urlPathVal)
		dirPath := v.valueToString(dirPathVal)
		if urlPath == "" || dirPath == "" {
			return Nil
		}
		if !strings.HasSuffix(urlPath, "/") {
			urlPath = urlPath + "/"
		}
		fileServer := http.FileServer(http.Dir(dirPath))
		srv.mux.Handle(urlPath, http.StripPrefix(urlPath, fileServer))
		return recv
	})

	// sseRoute:handler: — register an SSE endpoint. The handler block receives
	// [:conn :req |] and runs on the HTTP handler goroutine to let Maggie store
	// the connection. The Go-side event loop then streams events to the client.
	httpServerClass.AddMethod2(vm.Selectors, "sseRoute:handler:", func(v *VM, recv Value, pathVal, handlerBlock Value) Value {
		srv := v.vmGetHttpServer(recv)
		if srv == nil {
			return Nil
		}
		path := v.valueToString(pathVal)
		bv := v.currentInterpreter().getBlockValue(handlerBlock)
		if bv == nil {
			return Nil
		}
		// The handler block is retained for the server's lifetime inside the
		// net/http closure below, which is itself a strong (Go-GC-traced)
		// reference to the block Value — no pinning needed.
		block := bv.Block
		captures := bv.Captures
		homeSelf := bv.HomeSelf
		homeMethod := bv.HomeMethod

		srv.mux.HandleFunc(path, func(w http.ResponseWriter, r *http.Request) {
			if r.Method != http.MethodGet {
				http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
				return
			}
			flusher, ok := w.(http.Flusher)
			if !ok {
				http.Error(w, "Streaming not supported", http.StatusInternalServerError)
				return
			}

			// Disable WriteTimeout for this connection only.
			rc := http.NewResponseController(w)
			rc.SetWriteDeadline(time.Time{})

			w.Header().Set("Content-Type", "text/event-stream")
			w.Header().Set("Cache-Control", "no-cache")
			w.Header().Set("Connection", "keep-alive")
			w.WriteHeader(http.StatusOK)
			flusher.Flush()

			conn := &SSEConnectionObject{
				eventCh: make(chan sseEvent, 16),
				done:    r.Context().Done(),
			}

			// Run the Maggie handler block on this request goroutine with its
			// own registered interpreter (the VM's structures are safe for
			// concurrent dispatch; only interpreter stacks are per-goroutine).
			func() {
				interp := v.newRequestInterpreter()
				v.registerInterpreter(interp)
				defer v.unregisterInterpreter()
				connVal := v.vmRegisterSSEConnection(conn)
				reqObj := &HttpRequestObject{request: r}
				reqVal := v.vmRegisterHttpRequest(reqObj)
				interp.ExecuteBlockDetached(block, captures, []Value{connVal, reqVal}, homeSelf, homeMethod)
			}()

			// SSE event loop — runs on the HTTP handler goroutine.
			for {
				select {
				case evt, ok := <-conn.eventCh:
					if !ok {
						return // channel closed via conn close
					}
					if evt.event != "" {
						fmt.Fprintf(w, "event: %s\n", evt.event)
					}
					for _, line := range strings.Split(evt.data, "\n") {
						fmt.Fprintf(w, "data: %s\n", line)
					}
					fmt.Fprint(w, "\n")
					flusher.Flush()
				case <-r.Context().Done():
					conn.closed.Store(true)
					return
				}
			}
		})
		return recv
	})

	registerRoute := func(v *VM, recv, pathVal, methodVal, handlerBlock Value) Value {
		srv := v.vmGetHttpServer(recv)
		if srv == nil {
			return Nil
		}
		path := v.valueToString(pathVal)
		httpMethod := strings.ToUpper(v.valueToString(methodVal))
		bv := v.currentInterpreter().getBlockValue(handlerBlock)
		if bv == nil {
			return Nil
		}
		// The handler block is retained for the server's lifetime inside the
		// net/http closure below, which is itself a strong (Go-GC-traced)
		// reference to the block Value — no pinning needed.
		block := bv.Block
		captures := bv.Captures
		homeSelf := bv.HomeSelf
		homeMethod := bv.HomeMethod
		srv.mux.HandleFunc(path, func(w http.ResponseWriter, r *http.Request) {
			if httpMethod != "" && r.Method != httpMethod {
				http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
				return
			}
			defer func() {
				if rec := recover(); rec != nil {
					http.Error(w, fmt.Sprintf("Internal server error: %v", rec), http.StatusInternalServerError)
				}
			}()

			// Run the Maggie handler on this request goroutine with its own
			// registered interpreter — requests execute concurrently, same
			// pattern as server RPC handlers (RunIsolated).
			var hr httpResult
			func() {
				interp := v.newRequestInterpreter()
				v.registerInterpreter(interp)
				defer v.unregisterInterpreter()
				reqObj := &HttpRequestObject{request: r}
				reqVal := v.vmRegisterHttpRequest(reqObj)
				defer v.vmUnregisterHttpRequest(reqVal)
				result := interp.ExecuteBlockDetached(block, captures, []Value{reqVal}, homeSelf, homeMethod)
				hr = v.extractHTTPResult(result)
			}()

			for k, hv := range hr.headers {
				w.Header().Set(k, hv)
			}
			w.WriteHeader(hr.status)
			if hr.hasBody {
				w.Write(hr.body)
			}
		})
		return recv
	}
	httpServerClass.AddMethod3(vm.Selectors, "route:method:handler:", registerRoute)
	// asyncRoute:method:handler: is a synonym. Its old body forked a goroutine
	// then immediately blocked on the result — no different from running the
	// handler on the request goroutine (route: does that), so it now shares the
	// same registration instead of a divergent forked copy.
	httpServerClass.AddMethod3(vm.Selectors, "asyncRoute:method:handler:", registerRoute)

	httpServerClass.AddMethod0(vm.Selectors, "start", func(v *VM, recv Value) Value {
		srv := v.vmGetHttpServer(recv)
		if srv == nil {
			return Nil
		}
		srv.mu.Lock()
		if srv.running.Load() {
			srv.mu.Unlock()
			return recv
		}
		srv.running.Store(true)
		srv.mu.Unlock()
		// The serving goroutine blocks here indefinitely.
		err := srv.server.ListenAndServe()
		if err != nil && err != http.ErrServerClosed {
			srv.running.Store(false)
			return Nil
		}
		return recv
	})

	httpServerClass.AddMethod0(vm.Selectors, "stop", func(v *VM, recv Value) Value {
		srv := v.vmGetHttpServer(recv)
		if srv == nil {
			return recv
		}
		srv.mu.Lock()
		defer srv.mu.Unlock()
		if !srv.running.Load() {
			return recv
		}
		ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		srv.server.Shutdown(ctx)
		srv.running.Store(false)
		v.vmUnregisterHttpServer(recv)
		return recv
	})

	httpServerClass.AddMethod0(vm.Selectors, "port", func(v *VM, recv Value) Value {
		srv := v.vmGetHttpServer(recv)
		if srv == nil {
			return Nil
		}
		return FromSmallInt(int64(srv.port))
	})

	httpServerClass.AddMethod0(vm.Selectors, "isRunning", func(v *VM, recv Value) Value {
		srv := v.vmGetHttpServer(recv)
		if srv == nil {
			return False
		}
		if srv.running.Load() {
			return True
		}
		return False
	})

	httpRequestClass.AddMethod0(vm.Selectors, "body", func(v *VM, recv Value) Value {
		req := v.vmGetHttpRequest(recv)
		if req == nil {
			return v.registry.NewStringValue("")
		}
		if !req.bodyRead {
			if req.request.Body != nil {
				data, err := io.ReadAll(req.request.Body)
				if err == nil {
					req.body = string(data)
				}
				req.request.Body.Close()
			}
			req.bodyRead = true
		}
		return v.registry.NewStringValue(req.body)
	})

	httpRequestClass.AddMethod0(vm.Selectors, "path", func(v *VM, recv Value) Value {
		req := v.vmGetHttpRequest(recv)
		if req == nil {
			return v.registry.NewStringValue("")
		}
		return v.registry.NewStringValue(req.request.URL.Path)
	})

	httpRequestClass.AddMethod0(vm.Selectors, "method", func(v *VM, recv Value) Value {
		req := v.vmGetHttpRequest(recv)
		if req == nil {
			return v.registry.NewStringValue("")
		}
		return v.registry.NewStringValue(req.request.Method)
	})

	// host returns the request authority. Go promotes the incoming Host header
	// to Request.Host and strips it from the Header map, so `header: 'Host'`
	// always returns "". This exposes the real value.
	httpRequestClass.AddMethod0(vm.Selectors, "host", func(v *VM, recv Value) Value {
		req := v.vmGetHttpRequest(recv)
		if req == nil {
			return v.registry.NewStringValue("")
		}
		return v.registry.NewStringValue(req.request.Host)
	})

	httpRequestClass.AddMethod1(vm.Selectors, "header:", func(v *VM, recv Value, nameVal Value) Value {
		req := v.vmGetHttpRequest(recv)
		if req == nil {
			return v.registry.NewStringValue("")
		}
		name := v.valueToString(nameVal)
		return v.registry.NewStringValue(req.request.Header.Get(name))
	})

	httpRequestClass.AddMethod1(vm.Selectors, "queryParam:", func(v *VM, recv Value, nameVal Value) Value {
		req := v.vmGetHttpRequest(recv)
		if req == nil {
			return v.registry.NewStringValue("")
		}
		name := v.valueToString(nameVal)
		return v.registry.NewStringValue(req.request.URL.Query().Get(name))
	})

	httpResponseClass.AddClassMethod2(vm.Selectors, "new:body:", func(v *VM, recv Value, statusVal, bodyVal Value) Value {
		status := http.StatusOK
		if statusVal.IsSmallInt() {
			status = int(statusVal.SmallInt())
		}
		body := v.valueToString(bodyVal)
		resp := &HttpResponseObject{
			status:  status,
			body:    body,
			headers: make(map[string]string),
		}
		return v.vmRegisterHttpResponse(resp)
	})

	httpResponseClass.AddMethod2(vm.Selectors, "header:value:", func(v *VM, recv Value, nameVal, valueVal Value) Value {
		resp := v.vmGetHttpResponse(recv)
		if resp == nil {
			return recv
		}
		name := v.valueToString(nameVal)
		val := v.valueToString(valueVal)
		resp.headers[name] = val
		return recv
	})

	httpResponseClass.AddMethod1(vm.Selectors, "contentType:", func(v *VM, recv Value, mimeVal Value) Value {
		resp := v.vmGetHttpResponse(recv)
		if resp == nil {
			return recv
		}
		mime := v.valueToString(mimeVal)
		resp.headers["Content-Type"] = mime
		return recv
	})

	httpResponseClass.AddMethod0(vm.Selectors, "status", func(v *VM, recv Value) Value {
		resp := v.vmGetHttpResponse(recv)
		if resp == nil {
			return Nil
		}
		return FromSmallInt(int64(resp.status))
	})

	httpResponseClass.AddMethod0(vm.Selectors, "body", func(v *VM, recv Value) Value {
		resp := v.vmGetHttpResponse(recv)
		if resp == nil {
			return v.registry.NewStringValue("")
		}
		return v.registry.NewStringValue(resp.body)
	})

	// -------------------------------------------------------------------
	// HttpClient Primitives
	// -------------------------------------------------------------------

	httpClientClass := vm.createClass("HttpClient", vm.ObjectClass)
	vm.globals["HttpClient"] = vm.classValue(httpClientClass)
	vm.symbolDispatch.Register(httpClientMarker, &SymbolTypeEntry{Class: httpClientClass})

	// HttpClient new — creates a new client with a 30s timeout
	httpClientClass.AddClassMethod0(vm.Selectors, "new", func(v *VM, recv Value) Value {
		c := &HttpClientObject{
			client: &http.Client{
				Timeout: 30 * time.Second,
			},
		}
		return v.vmRegisterHttpClient(c)
	})

	// All request methods follow the failure doctrine: they return a Result —
	// Success wrapping the response body string, or Failure carrying the
	// reason. nil is never a failure signal.

	// get: url — HTTP GET
	httpClientClass.AddMethod1(vm.Selectors, "get:", func(v *VM, recv Value, urlVal Value) Value {
		c := v.vmGetHttpClient(recv)
		if c == nil {
			return v.newFailureResult("get: receiver is not an HttpClient")
		}
		url := v.valueToString(urlVal)
		if url == "" {
			return v.newFailureResult("get: requires a URL string")
		}
		resp, err := c.client.Get(url)
		if err != nil {
			return v.newFailureResult("GET " + url + ": " + err.Error())
		}
		return v.httpBodyResult(url, resp)
	})

	// post:body: — HTTP POST with string body
	httpClientClass.AddMethod2(vm.Selectors, "post:body:", func(v *VM, recv Value, urlVal, bodyVal Value) Value {
		c := v.vmGetHttpClient(recv)
		if c == nil {
			return v.newFailureResult("post:body: receiver is not an HttpClient")
		}
		url := v.valueToString(urlVal)
		bodyStr := v.valueToString(bodyVal)
		if url == "" {
			return v.newFailureResult("post:body: requires a URL string")
		}
		resp, err := c.client.Post(url, "application/octet-stream", strings.NewReader(bodyStr))
		if err != nil {
			return v.newFailureResult("POST " + url + ": " + err.Error())
		}
		return v.httpBodyResult(url, resp)
	})

	// post:body:contentType: — HTTP POST with explicit content type
	httpClientClass.AddMethod3(vm.Selectors, "post:body:contentType:", func(v *VM, recv Value, urlVal, bodyVal, ctVal Value) Value {
		c := v.vmGetHttpClient(recv)
		if c == nil {
			return v.newFailureResult("post:body:contentType: receiver is not an HttpClient")
		}
		url := v.valueToString(urlVal)
		bodyStr := v.valueToString(bodyVal)
		ct := v.valueToString(ctVal)
		if url == "" {
			return v.newFailureResult("post:body:contentType: requires a URL string")
		}
		if ct == "" {
			ct = "application/octet-stream"
		}
		resp, err := c.client.Post(url, ct, strings.NewReader(bodyStr))
		if err != nil {
			return v.newFailureResult("POST " + url + ": " + err.Error())
		}
		return v.httpBodyResult(url, resp)
	})

	// post:body:contentType:headers: — POST with explicit content type and a
	// Dictionary of extra headers (string keys -> string values).
	httpClientClass.AddMethod4(vm.Selectors, "post:body:contentType:headers:", func(v *VM, recv Value, urlVal, bodyVal, ctVal, headersVal Value) Value {
		c := v.vmGetHttpClient(recv)
		if c == nil {
			return v.newFailureResult("post:body:contentType:headers: receiver is not an HttpClient")
		}
		url := v.valueToString(urlVal)
		bodyStr := v.valueToString(bodyVal)
		ct := v.valueToString(ctVal)
		if url == "" {
			return v.newFailureResult("post:body:contentType:headers: requires a URL string")
		}
		if ct == "" {
			ct = "application/octet-stream"
		}
		req, err := http.NewRequest(http.MethodPost, url, strings.NewReader(bodyStr))
		if err != nil {
			return v.newFailureResult("POST " + url + ": " + err.Error())
		}
		req.Header.Set("Content-Type", ct)
		if dict := v.registry.GetDictionaryObject(headersVal); dict != nil {
			for _, e := range dict.Entries() {
				keyStr := v.valueToString(e.Key)
				valStr := v.valueToString(e.Value)
				if keyStr == "" {
					continue
				}
				req.Header.Set(keyStr, valStr)
			}
		}
		resp, err := c.client.Do(req)
		if err != nil {
			return v.newFailureResult("POST " + url + ": " + err.Error())
		}
		return v.httpBodyResult(url, resp)
	})

	// put:body: — HTTP PUT with string body
	httpClientClass.AddMethod2(vm.Selectors, "put:body:", func(v *VM, recv Value, urlVal, bodyVal Value) Value {
		c := v.vmGetHttpClient(recv)
		if c == nil {
			return v.newFailureResult("put:body: receiver is not an HttpClient")
		}
		url := v.valueToString(urlVal)
		bodyStr := v.valueToString(bodyVal)
		if url == "" {
			return v.newFailureResult("put:body: requires a URL string")
		}
		req, err := http.NewRequest(http.MethodPut, url, strings.NewReader(bodyStr))
		if err != nil {
			return v.newFailureResult("PUT " + url + ": " + err.Error())
		}
		req.Header.Set("Content-Type", "application/octet-stream")
		resp, err := c.client.Do(req)
		if err != nil {
			return v.newFailureResult("PUT " + url + ": " + err.Error())
		}
		return v.httpBodyResult(url, resp)
	})

	// delete: url — HTTP DELETE
	httpClientClass.AddMethod1(vm.Selectors, "delete:", func(v *VM, recv Value, urlVal Value) Value {
		c := v.vmGetHttpClient(recv)
		if c == nil {
			return v.newFailureResult("delete: receiver is not an HttpClient")
		}
		url := v.valueToString(urlVal)
		if url == "" {
			return v.newFailureResult("delete: requires a URL string")
		}
		req, err := http.NewRequest(http.MethodDelete, url, nil)
		if err != nil {
			return v.newFailureResult("DELETE " + url + ": " + err.Error())
		}
		resp, err := c.client.Do(req)
		if err != nil {
			return v.newFailureResult("DELETE " + url + ": " + err.Error())
		}
		return v.httpBodyResult(url, resp)
	})

	// -------------------------------------------------------------------
	// SSEConnection Primitives
	// -------------------------------------------------------------------

	sseConnectionClass := vm.createClass("SSEConnection", vm.ObjectClass)
	vm.globals["SSEConnection"] = vm.classValue(sseConnectionClass)
	vm.symbolDispatch.Register(sseConnectionMarker, &SymbolTypeEntry{Class: sseConnectionClass})

	// send: data — send a data-only SSE event. Returns true/false.
	sseConnectionClass.AddMethod1(vm.Selectors, "send:", func(v *VM, recv Value, dataVal Value) Value {
		conn := v.vmGetSSEConnection(recv)
		if conn == nil || conn.closed.Load() {
			return False
		}
		data := v.valueToString(dataVal)
		select {
		case conn.eventCh <- sseEvent{data: data}:
			return True
		case <-conn.done:
			conn.closed.Store(true)
			return False
		}
	})

	// send:event: — send a named SSE event (e.g. for Datastar). Returns true/false.
	sseConnectionClass.AddMethod2(vm.Selectors, "send:event:", func(v *VM, recv Value, dataVal, eventVal Value) Value {
		conn := v.vmGetSSEConnection(recv)
		if conn == nil || conn.closed.Load() {
			return False
		}
		data := v.valueToString(dataVal)
		event := v.valueToString(eventVal)
		select {
		case conn.eventCh <- sseEvent{event: event, data: data}:
			return True
		case <-conn.done:
			conn.closed.Store(true)
			return False
		}
	})

	// close — close the SSE connection from the server side.
	sseConnectionClass.AddMethod0(vm.Selectors, "close", func(v *VM, recv Value) Value {
		conn := v.vmGetSSEConnection(recv)
		if conn == nil {
			return recv
		}
		if !conn.closed.Swap(true) {
			close(conn.eventCh)
		}
		return recv
	})

	// isOpen — check if the client is still connected.
	sseConnectionClass.AddMethod0(vm.Selectors, "isOpen", func(v *VM, recv Value) Value {
		conn := v.vmGetSSEConnection(recv)
		if conn == nil || conn.closed.Load() {
			return False
		}
		return True
	})
}

// httpBodyResult drains an HTTP response into a Result: Success wrapping the
// body string, or Failure if the body cannot be read.
func (vm *VM) httpBodyResult(url string, resp *http.Response) Value {
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return vm.newFailureResult("reading response from " + url + ": " + err.Error())
	}
	return vm.newSuccessResult(vm.registry.NewStringValue(string(body)))
}
