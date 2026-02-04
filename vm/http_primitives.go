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

func httpServerIDFromValue(v Value) int {
	return int(v.SymbolID() & ^uint32(0xFF<<24))
}

func (vm *VM) vmGetHttpServer(v Value) *HttpServerObject {
	if !isHttpServerValue(v) {
		return nil
	}
	return vm.registry.GetHttpServer(httpServerIDFromValue(v))
}

func (vm *VM) vmRegisterHttpServer(s *HttpServerObject) Value {
	id := vm.registry.RegisterHttpServer(s)
	return httpServerToValue(id)
}

func (vm *VM) vmUnregisterHttpServer(v Value) {
	if !isHttpServerValue(v) {
		return
	}
	vm.registry.UnregisterHttpServer(httpServerIDFromValue(v))
}

// ---------------------------------------------------------------------------
// HttpRequest Registry
// ---------------------------------------------------------------------------

type HttpRequestObject struct {
	request  *http.Request
	body     string
	bodyRead bool
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

func httpRequestIDFromValue(v Value) int {
	return int(v.SymbolID() & ^uint32(0xFF<<24))
}

func (vm *VM) vmGetHttpRequest(v Value) *HttpRequestObject {
	if !isHttpRequestValue(v) {
		return nil
	}
	return vm.registry.GetHttpRequest(httpRequestIDFromValue(v))
}

func (vm *VM) vmRegisterHttpRequest(req *HttpRequestObject) Value {
	id := vm.registry.RegisterHttpRequest(req)
	return httpRequestToValue(id)
}

func (vm *VM) vmUnregisterHttpRequest(v Value) {
	if !isHttpRequestValue(v) {
		return
	}
	vm.registry.UnregisterHttpRequest(httpRequestIDFromValue(v))
}

// ---------------------------------------------------------------------------
// HttpResponse Registry
// ---------------------------------------------------------------------------

type HttpResponseObject struct {
	status  int
	body    string
	headers map[string]string
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

func httpResponseIDFromValue(v Value) int {
	return int(v.SymbolID() & ^uint32(0xFF<<24))
}

func (vm *VM) vmGetHttpResponse(v Value) *HttpResponseObject {
	if !isHttpResponseValue(v) {
		return nil
	}
	return vm.registry.GetHttpResponse(httpResponseIDFromValue(v))
}

func (vm *VM) vmRegisterHttpResponse(resp *HttpResponseObject) Value {
	id := vm.registry.RegisterHttpResponse(resp)
	return httpResponseToValue(id)
}

// ---------------------------------------------------------------------------
// HttpServer Primitives Registration
// ---------------------------------------------------------------------------

func (vm *VM) registerHttpPrimitives() {
	httpServerClass := vm.createClass("HttpServer", vm.ObjectClass)
	httpRequestClass := vm.createClass("HttpRequest", vm.ObjectClass)
	httpResponseClass := vm.createClass("HttpResponse", vm.ObjectClass)

	vm.Globals["HttpServer"] = vm.classValue(httpServerClass)
	vm.Globals["HttpRequest"] = vm.classValue(httpRequestClass)
	vm.Globals["HttpResponse"] = vm.classValue(httpResponseClass)

	vm.symbolDispatch.Register(httpServerMarker, &SymbolTypeEntry{Class: httpServerClass})
	vm.symbolDispatch.Register(httpRequestMarker, &SymbolTypeEntry{Class: httpRequestClass})
	vm.symbolDispatch.Register(httpResponseMarker, &SymbolTypeEntry{Class: httpResponseClass})

	httpServerClass.AddClassMethod1(vm.Selectors, "new:", func(vmPtr interface{}, recv Value, portVal Value) Value {
		v := vmPtr.(*VM)
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

	httpServerClass.AddMethod2(vm.Selectors, "serveStatic:from:", func(vmPtr interface{}, recv Value, urlPathVal, dirPathVal Value) Value {
		v := vmPtr.(*VM)
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

	httpServerClass.AddMethod3(vm.Selectors, "route:method:handler:", func(vmPtr interface{}, recv Value, pathVal, methodVal, handlerBlock Value) Value {
		v := vmPtr.(*VM)
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
			interp := v.newInterpreter()
			v.registerInterpreter(interp)
			defer v.unregisterInterpreter()
			reqObj := &HttpRequestObject{request: r}
			reqVal := v.vmRegisterHttpRequest(reqObj)
			defer v.vmUnregisterHttpRequest(reqVal)
			result := interp.ExecuteBlockDetached(block, captures, []Value{reqVal}, homeSelf, homeMethod)
			resp := v.vmGetHttpResponse(result)
			if resp != nil {
				for k, hv := range resp.headers {
					w.Header().Set(k, hv)
				}
				w.WriteHeader(resp.status)
				w.Write([]byte(resp.body))
			} else {
				w.WriteHeader(http.StatusOK)
				if IsStringValue(result) {
					w.Write([]byte(v.registry.GetStringContent(result)))
				}
			}
		})
		return recv
	})

	httpServerClass.AddMethod0(vm.Selectors, "start", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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
		err := srv.server.ListenAndServe()
		if err != nil && err != http.ErrServerClosed {
			srv.running.Store(false)
			return Nil
		}
		return recv
	})

	httpServerClass.AddMethod0(vm.Selectors, "stop", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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

	httpServerClass.AddMethod0(vm.Selectors, "port", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		srv := v.vmGetHttpServer(recv)
		if srv == nil {
			return Nil
		}
		return FromSmallInt(int64(srv.port))
	})

	httpServerClass.AddMethod0(vm.Selectors, "isRunning", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		srv := v.vmGetHttpServer(recv)
		if srv == nil {
			return False
		}
		if srv.running.Load() {
			return True
		}
		return False
	})

	httpRequestClass.AddMethod0(vm.Selectors, "body", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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

	httpRequestClass.AddMethod0(vm.Selectors, "path", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		req := v.vmGetHttpRequest(recv)
		if req == nil {
			return v.registry.NewStringValue("")
		}
		return v.registry.NewStringValue(req.request.URL.Path)
	})

	httpRequestClass.AddMethod0(vm.Selectors, "method", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		req := v.vmGetHttpRequest(recv)
		if req == nil {
			return v.registry.NewStringValue("")
		}
		return v.registry.NewStringValue(req.request.Method)
	})

	httpRequestClass.AddMethod1(vm.Selectors, "header:", func(vmPtr interface{}, recv Value, nameVal Value) Value {
		v := vmPtr.(*VM)
		req := v.vmGetHttpRequest(recv)
		if req == nil {
			return v.registry.NewStringValue("")
		}
		name := v.valueToString(nameVal)
		return v.registry.NewStringValue(req.request.Header.Get(name))
	})

	httpRequestClass.AddMethod1(vm.Selectors, "queryParam:", func(vmPtr interface{}, recv Value, nameVal Value) Value {
		v := vmPtr.(*VM)
		req := v.vmGetHttpRequest(recv)
		if req == nil {
			return v.registry.NewStringValue("")
		}
		name := v.valueToString(nameVal)
		return v.registry.NewStringValue(req.request.URL.Query().Get(name))
	})

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
		return v.vmRegisterHttpResponse(resp)
	})

	httpResponseClass.AddMethod2(vm.Selectors, "header:value:", func(vmPtr interface{}, recv Value, nameVal, valueVal Value) Value {
		v := vmPtr.(*VM)
		resp := v.vmGetHttpResponse(recv)
		if resp == nil {
			return recv
		}
		name := v.valueToString(nameVal)
		val := v.valueToString(valueVal)
		resp.headers[name] = val
		return recv
	})

	httpResponseClass.AddMethod1(vm.Selectors, "contentType:", func(vmPtr interface{}, recv Value, mimeVal Value) Value {
		v := vmPtr.(*VM)
		resp := v.vmGetHttpResponse(recv)
		if resp == nil {
			return recv
		}
		mime := v.valueToString(mimeVal)
		resp.headers["Content-Type"] = mime
		return recv
	})

	httpResponseClass.AddMethod0(vm.Selectors, "status", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		resp := v.vmGetHttpResponse(recv)
		if resp == nil {
			return Nil
		}
		return FromSmallInt(int64(resp.status))
	})

	httpResponseClass.AddMethod0(vm.Selectors, "body", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		resp := v.vmGetHttpResponse(recv)
		if resp == nil {
			return v.registry.NewStringValue("")
		}
		return v.registry.NewStringValue(resp.body)
	})
}
