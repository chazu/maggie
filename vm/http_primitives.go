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

// httpServerRegistry stores active HTTP servers.
var httpServerRegistry = struct {
	sync.RWMutex
	servers map[int]*HttpServerObject
	nextID  int
}{
	servers: make(map[int]*HttpServerObject),
	nextID:  1,
}

const httpServerMarker uint32 = 38 << 24

func httpServerToValue(id int) Value {
	return FromSymbolID(uint32(id) | httpServerMarker)
}

func isHttpServerValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return (id & (0xFF << 24)) == httpServerMarker
}

func getHttpServer(v Value) *HttpServerObject {
	if !isHttpServerValue(v) {
		return nil
	}
	id := int(v.SymbolID() & ^uint32(0xFF<<24))

	httpServerRegistry.RLock()
	defer httpServerRegistry.RUnlock()
	return httpServerRegistry.servers[id]
}

func registerHttpServer(s *HttpServerObject) Value {
	httpServerRegistry.Lock()
	defer httpServerRegistry.Unlock()

	id := httpServerRegistry.nextID
	httpServerRegistry.nextID++
	httpServerRegistry.servers[id] = s

	return httpServerToValue(id)
}

func unregisterHttpServer(v Value) {
	if !isHttpServerValue(v) {
		return
	}
	id := int(v.SymbolID() & ^uint32(0xFF<<24))

	httpServerRegistry.Lock()
	defer httpServerRegistry.Unlock()
	delete(httpServerRegistry.servers, id)
}

// ---------------------------------------------------------------------------
// HttpRequest Registry
// ---------------------------------------------------------------------------

// HttpRequestObject wraps a Go *http.Request for use in Maggie.
type HttpRequestObject struct {
	request  *http.Request
	body     string
	bodyRead bool
}

// httpRequestRegistry stores active HTTP requests.
var httpRequestRegistry = struct {
	sync.RWMutex
	requests map[int]*HttpRequestObject
	nextID   int
}{
	requests: make(map[int]*HttpRequestObject),
	nextID:   1,
}

const httpRequestMarker uint32 = 39 << 24

func httpRequestToValue(id int) Value {
	return FromSymbolID(uint32(id) | httpRequestMarker)
}

func isHttpRequestValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return (id & (0xFF << 24)) == httpRequestMarker
}

func getHttpRequest(v Value) *HttpRequestObject {
	if !isHttpRequestValue(v) {
		return nil
	}
	id := int(v.SymbolID() & ^uint32(0xFF<<24))

	httpRequestRegistry.RLock()
	defer httpRequestRegistry.RUnlock()
	return httpRequestRegistry.requests[id]
}

func registerHttpRequest(req *HttpRequestObject) Value {
	httpRequestRegistry.Lock()
	defer httpRequestRegistry.Unlock()

	id := httpRequestRegistry.nextID
	httpRequestRegistry.nextID++
	httpRequestRegistry.requests[id] = req

	return httpRequestToValue(id)
}

func unregisterHttpRequest(v Value) {
	if !isHttpRequestValue(v) {
		return
	}
	id := int(v.SymbolID() & ^uint32(0xFF<<24))

	httpRequestRegistry.Lock()
	defer httpRequestRegistry.Unlock()
	delete(httpRequestRegistry.requests, id)
}

// ---------------------------------------------------------------------------
// HttpResponse Registry
// ---------------------------------------------------------------------------

// HttpResponseObject holds a response to be sent back from the HTTP handler.
type HttpResponseObject struct {
	status  int
	body    string
	headers map[string]string
}

// httpResponseRegistry stores active HTTP responses.
var httpResponseRegistry = struct {
	sync.RWMutex
	responses map[int]*HttpResponseObject
	nextID    int
}{
	responses: make(map[int]*HttpResponseObject),
	nextID:    1,
}

const httpResponseMarker uint32 = 40 << 24

func httpResponseToValue(id int) Value {
	return FromSymbolID(uint32(id) | httpResponseMarker)
}

func isHttpResponseValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return (id & (0xFF << 24)) == httpResponseMarker
}

func getHttpResponse(v Value) *HttpResponseObject {
	if !isHttpResponseValue(v) {
		return nil
	}
	id := int(v.SymbolID() & ^uint32(0xFF<<24))

	httpResponseRegistry.RLock()
	defer httpResponseRegistry.RUnlock()
	return httpResponseRegistry.responses[id]
}

func registerHttpResponse(resp *HttpResponseObject) Value {
	httpResponseRegistry.Lock()
	defer httpResponseRegistry.Unlock()

	id := httpResponseRegistry.nextID
	httpResponseRegistry.nextID++
	httpResponseRegistry.responses[id] = resp

	return httpResponseToValue(id)
}

// ---------------------------------------------------------------------------
// HttpServer Primitives Registration
// ---------------------------------------------------------------------------

func (vm *VM) registerHttpPrimitives() {
	// Create classes
	httpServerClass := vm.createClass("HttpServer", vm.ObjectClass)
	httpRequestClass := vm.createClass("HttpRequest", vm.ObjectClass)
	httpResponseClass := vm.createClass("HttpResponse", vm.ObjectClass)

	// Register globals
	vm.Globals["HttpServer"] = vm.classValue(httpServerClass)
	vm.Globals["HttpRequest"] = vm.classValue(httpRequestClass)
	vm.Globals["HttpResponse"] = vm.classValue(httpResponseClass)

	// Register symbol dispatch
	vm.symbolDispatch.Register(httpServerMarker, &SymbolTypeEntry{Class: httpServerClass})
	vm.symbolDispatch.Register(httpRequestMarker, &SymbolTypeEntry{Class: httpRequestClass})
	vm.symbolDispatch.Register(httpResponseMarker, &SymbolTypeEntry{Class: httpResponseClass})

	// -----------------------------------------------------------------------
	// HttpServer class methods
	// -----------------------------------------------------------------------

	// HttpServer class>>new: port — create server on the given port
	httpServerClass.AddClassMethod1(vm.Selectors, "new:", func(_ interface{}, recv Value, portVal Value) Value {
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

		return registerHttpServer(srv)
	})

	// -----------------------------------------------------------------------
	// HttpServer instance methods
	// -----------------------------------------------------------------------

	// HttpServer>>serveStatic:from: urlPath dirPath — register a file server
	httpServerClass.AddMethod2(vm.Selectors, "serveStatic:from:", func(vmPtr interface{}, recv Value, urlPathVal, dirPathVal Value) Value {
		v := vmPtr.(*VM)
		srv := getHttpServer(recv)
		if srv == nil {
			return Nil
		}

		urlPath := v.valueToString(urlPathVal)
		dirPath := v.valueToString(dirPathVal)
		if urlPath == "" || dirPath == "" {
			return Nil
		}

		// Ensure URL path ends with /
		if !strings.HasSuffix(urlPath, "/") {
			urlPath = urlPath + "/"
		}

		fileServer := http.FileServer(http.Dir(dirPath))
		srv.mux.Handle(urlPath, http.StripPrefix(urlPath, fileServer))

		return recv
	})

	// HttpServer>>route:method:handler: urlPath httpMethod block — register a route
	httpServerClass.AddMethod3(vm.Selectors, "route:method:handler:", func(vmPtr interface{}, recv Value, pathVal, methodVal, handlerBlock Value) Value {
		v := vmPtr.(*VM)
		srv := getHttpServer(recv)
		if srv == nil {
			return Nil
		}

		path := v.valueToString(pathVal)
		httpMethod := strings.ToUpper(v.valueToString(methodVal))

		// Capture the block value now (while we're on the interpreter goroutine)
		bv := v.currentInterpreter().getBlockValue(handlerBlock)
		if bv == nil {
			return Nil
		}

		// Capture the block's fields so they survive across goroutines
		block := bv.Block
		captures := bv.Captures
		homeSelf := bv.HomeSelf
		homeMethod := bv.HomeMethod

		srv.mux.HandleFunc(path, func(w http.ResponseWriter, r *http.Request) {
			// Filter by HTTP method if specified
			if httpMethod != "" && r.Method != httpMethod {
				http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
				return
			}

			// Recover from panics to prevent server crashes
			defer func() {
				if rec := recover(); rec != nil {
					http.Error(w, fmt.Sprintf("Internal server error: %v", rec), http.StatusInternalServerError)
				}
			}()

			// Create a new interpreter for this goroutine
			interp := v.newInterpreter()
			v.registerInterpreter(interp)
			defer v.unregisterInterpreter()

			// Create HttpRequest value
			reqObj := &HttpRequestObject{request: r}
			reqVal := registerHttpRequest(reqObj)
			defer unregisterHttpRequest(reqVal)

			// Execute handler block with request as argument
			result := interp.ExecuteBlockDetached(
				block, captures, []Value{reqVal},
				homeSelf, homeMethod,
			)

			// Extract response from result
			resp := getHttpResponse(result)
			if resp != nil {
				for k, hv := range resp.headers {
					w.Header().Set(k, hv)
				}
				w.WriteHeader(resp.status)
				w.Write([]byte(resp.body))
			} else {
				// If block didn't return an HttpResponse, convert result to string
				w.WriteHeader(http.StatusOK)
				if IsStringValue(result) {
					w.Write([]byte(GetStringContent(result)))
				}
			}
		})

		return recv
	})

	// HttpServer>>start — start the server (blocking; should be called from a forked process)
	httpServerClass.AddMethod0(vm.Selectors, "start", func(_ interface{}, recv Value) Value {
		srv := getHttpServer(recv)
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

		err := srv.server.ListenAndServe()
		if err != nil && err != http.ErrServerClosed {
			srv.running.Store(false)
			return Nil
		}

		return recv
	})

	// HttpServer>>stop — graceful shutdown
	httpServerClass.AddMethod0(vm.Selectors, "stop", func(_ interface{}, recv Value) Value {
		srv := getHttpServer(recv)
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
		unregisterHttpServer(recv)

		return recv
	})

	// HttpServer>>port — returns the port number
	httpServerClass.AddMethod0(vm.Selectors, "port", func(_ interface{}, recv Value) Value {
		srv := getHttpServer(recv)
		if srv == nil {
			return Nil
		}
		return FromSmallInt(int64(srv.port))
	})

	// HttpServer>>isRunning — returns true/false
	httpServerClass.AddMethod0(vm.Selectors, "isRunning", func(_ interface{}, recv Value) Value {
		srv := getHttpServer(recv)
		if srv == nil {
			return False
		}
		if srv.running.Load() {
			return True
		}
		return False
	})

	// -----------------------------------------------------------------------
	// HttpRequest instance methods
	// -----------------------------------------------------------------------

	// HttpRequest>>body — returns request body as a Maggie string
	httpRequestClass.AddMethod0(vm.Selectors, "body", func(_ interface{}, recv Value) Value {
		req := getHttpRequest(recv)
		if req == nil {
			return NewStringValue("")
		}

		// Lazily read and cache the body
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

		return NewStringValue(req.body)
	})

	// HttpRequest>>path — returns URL path as string
	httpRequestClass.AddMethod0(vm.Selectors, "path", func(_ interface{}, recv Value) Value {
		req := getHttpRequest(recv)
		if req == nil {
			return NewStringValue("")
		}
		return NewStringValue(req.request.URL.Path)
	})

	// HttpRequest>>method — returns HTTP method as string
	httpRequestClass.AddMethod0(vm.Selectors, "method", func(_ interface{}, recv Value) Value {
		req := getHttpRequest(recv)
		if req == nil {
			return NewStringValue("")
		}
		return NewStringValue(req.request.Method)
	})

	// HttpRequest>>header: name — returns header value as string (or empty string)
	httpRequestClass.AddMethod1(vm.Selectors, "header:", func(vmPtr interface{}, recv Value, nameVal Value) Value {
		v := vmPtr.(*VM)
		req := getHttpRequest(recv)
		if req == nil {
			return NewStringValue("")
		}
		name := v.valueToString(nameVal)
		return NewStringValue(req.request.Header.Get(name))
	})

	// HttpRequest>>queryParam: name — returns query parameter value as string
	httpRequestClass.AddMethod1(vm.Selectors, "queryParam:", func(vmPtr interface{}, recv Value, nameVal Value) Value {
		v := vmPtr.(*VM)
		req := getHttpRequest(recv)
		if req == nil {
			return NewStringValue("")
		}
		name := v.valueToString(nameVal)
		return NewStringValue(req.request.URL.Query().Get(name))
	})

	// -----------------------------------------------------------------------
	// HttpResponse class and instance methods
	// -----------------------------------------------------------------------

	// HttpResponse class>>new:body: statusCode bodyString — create response
	httpResponseClass.AddClassMethod2(vm.Selectors, "new:body:", func(vmPtr interface{}, recv Value, statusVal, bodyVal Value) Value {
		v := vmPtr.(*VM)
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

		return registerHttpResponse(resp)
	})

	// HttpResponse>>header:value: name value — sets a response header, returns self
	httpResponseClass.AddMethod2(vm.Selectors, "header:value:", func(vmPtr interface{}, recv Value, nameVal, valueVal Value) Value {
		v := vmPtr.(*VM)
		resp := getHttpResponse(recv)
		if resp == nil {
			return recv
		}

		name := v.valueToString(nameVal)
		val := v.valueToString(valueVal)
		resp.headers[name] = val

		return recv
	})

	// HttpResponse>>contentType: mimeType — sets Content-Type header, returns self
	httpResponseClass.AddMethod1(vm.Selectors, "contentType:", func(vmPtr interface{}, recv Value, mimeVal Value) Value {
		v := vmPtr.(*VM)
		resp := getHttpResponse(recv)
		if resp == nil {
			return recv
		}

		mime := v.valueToString(mimeVal)
		resp.headers["Content-Type"] = mime

		return recv
	})

	// HttpResponse>>status — returns status code as SmallInt
	httpResponseClass.AddMethod0(vm.Selectors, "status", func(_ interface{}, recv Value) Value {
		resp := getHttpResponse(recv)
		if resp == nil {
			return Nil
		}
		return FromSmallInt(int64(resp.status))
	})

	// HttpResponse>>body — returns body string
	httpResponseClass.AddMethod0(vm.Selectors, "body", func(_ interface{}, recv Value) Value {
		resp := getHttpResponse(recv)
		if resp == nil {
			return NewStringValue("")
		}
		return NewStringValue(resp.body)
	})
}
