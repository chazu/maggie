package vm

import (
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"
)

// ---------------------------------------------------------------------------
// HttpServer Registry Tests
// ---------------------------------------------------------------------------

func TestHttpServerRegistration(t *testing.T) {
	srv := &HttpServerObject{
		mux:  http.NewServeMux(),
		port: 9999,
	}

	val := registerHttpServer(srv)

	if !isHttpServerValue(val) {
		t.Fatal("registerHttpServer should produce an HttpServer value")
	}

	got := getHttpServer(val)
	if got == nil {
		t.Fatal("getHttpServer returned nil for registered server")
	}
	if got.port != 9999 {
		t.Errorf("port = %d, want 9999", got.port)
	}

	// Unregister
	unregisterHttpServer(val)

	got = getHttpServer(val)
	if got != nil {
		t.Error("getHttpServer should return nil after unregister")
	}
}

func TestIsHttpServerValueFalse(t *testing.T) {
	if isHttpServerValue(Nil) {
		t.Error("Nil should not be an HttpServer value")
	}
	if isHttpServerValue(FromSmallInt(42)) {
		t.Error("SmallInt should not be an HttpServer value")
	}
	if isHttpServerValue(NewStringValue("hi")) {
		t.Error("String should not be an HttpServer value")
	}
}

// ---------------------------------------------------------------------------
// HttpRequest Registry Tests
// ---------------------------------------------------------------------------

func TestHttpRequestRegistration(t *testing.T) {
	goReq := httptest.NewRequest("GET", "/test", nil)
	reqObj := &HttpRequestObject{request: goReq}
	val := registerHttpRequest(reqObj)

	if !isHttpRequestValue(val) {
		t.Fatal("registerHttpRequest should produce an HttpRequest value")
	}

	got := getHttpRequest(val)
	if got == nil {
		t.Fatal("getHttpRequest returned nil for registered request")
	}
	if got.request.URL.Path != "/test" {
		t.Errorf("request path = %q, want %q", got.request.URL.Path, "/test")
	}

	// Unregister
	unregisterHttpRequest(val)

	got = getHttpRequest(val)
	if got != nil {
		t.Error("getHttpRequest should return nil after unregister")
	}
}

func TestIsHttpRequestValueFalse(t *testing.T) {
	if isHttpRequestValue(Nil) {
		t.Error("Nil should not be an HttpRequest value")
	}
	if isHttpRequestValue(FromSmallInt(42)) {
		t.Error("SmallInt should not be an HttpRequest value")
	}
}

// ---------------------------------------------------------------------------
// HttpResponse Registry Tests
// ---------------------------------------------------------------------------

func TestHttpResponseRegistration(t *testing.T) {
	resp := &HttpResponseObject{
		status:  200,
		body:    "OK",
		headers: map[string]string{"X-Test": "val"},
	}
	val := registerHttpResponse(resp)

	if !isHttpResponseValue(val) {
		t.Fatal("registerHttpResponse should produce an HttpResponse value")
	}

	got := getHttpResponse(val)
	if got == nil {
		t.Fatal("getHttpResponse returned nil for registered response")
	}
	if got.status != 200 {
		t.Errorf("status = %d, want 200", got.status)
	}
	if got.body != "OK" {
		t.Errorf("body = %q, want %q", got.body, "OK")
	}
	if got.headers["X-Test"] != "val" {
		t.Errorf("header X-Test = %q, want %q", got.headers["X-Test"], "val")
	}
}

func TestIsHttpResponseValueFalse(t *testing.T) {
	if isHttpResponseValue(Nil) {
		t.Error("Nil should not be an HttpResponse value")
	}
	if isHttpResponseValue(FromSmallInt(42)) {
		t.Error("SmallInt should not be an HttpResponse value")
	}
}

// ---------------------------------------------------------------------------
// HttpServer Primitive Tests (via vm.Send)
// ---------------------------------------------------------------------------

func TestHttpServerNewWithPort(t *testing.T) {
	vm := NewVM()

	// HttpServer new: 8080
	httpServerClassVal := vm.Globals["HttpServer"]
	serverVal := vm.Send(httpServerClassVal, "new:", []Value{FromSmallInt(8080)})

	if serverVal == Nil {
		t.Fatal("HttpServer new: 8080 returned Nil")
	}
	if !isHttpServerValue(serverVal) {
		t.Fatal("HttpServer new: should return an HttpServer value")
	}

	// Check port
	port := vm.Send(serverVal, "port", nil)
	if !port.IsSmallInt() || port.SmallInt() != 8080 {
		t.Errorf("port = %v, want 8080", port)
	}
}

func TestHttpServerNewInvalidPort(t *testing.T) {
	vm := NewVM()

	httpServerClassVal := vm.Globals["HttpServer"]

	// Non-integer argument should return Nil
	result := vm.Send(httpServerClassVal, "new:", []Value{NewStringValue("abc")})
	if result != Nil {
		t.Errorf("HttpServer new: with non-integer should return Nil, got %v", result)
	}
}

func TestHttpServerIsRunning(t *testing.T) {
	vm := NewVM()

	httpServerClassVal := vm.Globals["HttpServer"]
	serverVal := vm.Send(httpServerClassVal, "new:", []Value{FromSmallInt(0)})
	if serverVal == Nil {
		t.Fatal("HttpServer new: returned Nil")
	}

	// Before starting, isRunning should be false
	running := vm.Send(serverVal, "isRunning", nil)
	if running != False {
		t.Errorf("isRunning before start should be False, got %v", running)
	}
}

func TestHttpServerStopBeforeStart(t *testing.T) {
	vm := NewVM()

	httpServerClassVal := vm.Globals["HttpServer"]
	serverVal := vm.Send(httpServerClassVal, "new:", []Value{FromSmallInt(0)})
	if serverVal == Nil {
		t.Fatal("HttpServer new: returned Nil")
	}

	// stop on a non-running server should not panic and should return recv
	result := vm.Send(serverVal, "stop", nil)
	if result != serverVal {
		t.Errorf("stop should return receiver, got %v", result)
	}
}

func TestHttpServerPortRetrieval(t *testing.T) {
	vm := NewVM()

	httpServerClassVal := vm.Globals["HttpServer"]

	// Test various ports
	for _, port := range []int64{80, 443, 3000, 8080, 50051} {
		serverVal := vm.Send(httpServerClassVal, "new:", []Value{FromSmallInt(port)})
		if serverVal == Nil {
			t.Fatalf("HttpServer new: %d returned Nil", port)
		}

		got := vm.Send(serverVal, "port", nil)
		if !got.IsSmallInt() || got.SmallInt() != port {
			t.Errorf("port = %v, want %d", got, port)
		}
	}
}

func TestHttpServerPortOnNilServer(t *testing.T) {
	vm := NewVM()

	// Calling port on a non-server value - use a fake value that
	// passes isHttpServerValue but whose registry entry was removed
	srv := &HttpServerObject{mux: http.NewServeMux(), port: 1234}
	val := registerHttpServer(srv)
	unregisterHttpServer(val) // remove from registry

	result := vm.Send(val, "port", nil)
	if result != Nil {
		t.Errorf("port on unregistered server should return Nil, got %v", result)
	}
}

func TestHttpServerIsRunningOnNilServer(t *testing.T) {
	vm := NewVM()

	// Create and immediately unregister
	srv := &HttpServerObject{mux: http.NewServeMux(), port: 1234}
	val := registerHttpServer(srv)
	unregisterHttpServer(val)

	result := vm.Send(val, "isRunning", nil)
	if result != False {
		t.Errorf("isRunning on unregistered server should return False, got %v", result)
	}
}

// ---------------------------------------------------------------------------
// HttpResponse Primitive Tests (via vm.Send)
// ---------------------------------------------------------------------------

func TestHttpResponseNewBodyStatus(t *testing.T) {
	vm := NewVM()

	httpResponseClassVal := vm.Globals["HttpResponse"]

	// HttpResponse new: 200 body: 'Hello'
	respVal := vm.Send(httpResponseClassVal, "new:body:", []Value{
		FromSmallInt(200),
		NewStringValue("Hello"),
	})

	if respVal == Nil {
		t.Fatal("HttpResponse new:body: returned Nil")
	}
	if !isHttpResponseValue(respVal) {
		t.Fatal("HttpResponse new:body: should return an HttpResponse value")
	}

	// Check status
	status := vm.Send(respVal, "status", nil)
	if !status.IsSmallInt() || status.SmallInt() != 200 {
		t.Errorf("status = %v, want 200", status)
	}

	// Check body
	body := vm.Send(respVal, "body", nil)
	if !IsStringValue(body) || GetStringContent(body) != "Hello" {
		t.Errorf("body = %q, want %q", GetStringContent(body), "Hello")
	}
}

func TestHttpResponseStatusCodes(t *testing.T) {
	vm := NewVM()

	httpResponseClassVal := vm.Globals["HttpResponse"]

	codes := []int64{200, 201, 301, 400, 403, 404, 500, 503}
	for _, code := range codes {
		respVal := vm.Send(httpResponseClassVal, "new:body:", []Value{
			FromSmallInt(code),
			NewStringValue("test"),
		})
		status := vm.Send(respVal, "status", nil)
		if !status.IsSmallInt() || status.SmallInt() != code {
			t.Errorf("status = %v, want %d", status, code)
		}
	}
}

func TestHttpResponseContentType(t *testing.T) {
	vm := NewVM()

	httpResponseClassVal := vm.Globals["HttpResponse"]

	respVal := vm.Send(httpResponseClassVal, "new:body:", []Value{
		FromSmallInt(200),
		NewStringValue("{}"),
	})

	// contentType: sets Content-Type header and returns self
	result := vm.Send(respVal, "contentType:", []Value{NewStringValue("application/json")})
	if result != respVal {
		t.Error("contentType: should return self")
	}

	// Verify the header was set on the underlying object
	resp := getHttpResponse(respVal)
	if resp == nil {
		t.Fatal("getHttpResponse returned nil")
	}
	if resp.headers["Content-Type"] != "application/json" {
		t.Errorf("Content-Type = %q, want %q", resp.headers["Content-Type"], "application/json")
	}
}

func TestHttpResponseHeaderValue(t *testing.T) {
	vm := NewVM()

	httpResponseClassVal := vm.Globals["HttpResponse"]

	respVal := vm.Send(httpResponseClassVal, "new:body:", []Value{
		FromSmallInt(200),
		NewStringValue("body"),
	})

	// header:value: sets an arbitrary header
	result := vm.Send(respVal, "header:value:", []Value{
		NewStringValue("X-Custom"),
		NewStringValue("my-value"),
	})
	if result != respVal {
		t.Error("header:value: should return self")
	}

	resp := getHttpResponse(respVal)
	if resp.headers["X-Custom"] != "my-value" {
		t.Errorf("X-Custom = %q, want %q", resp.headers["X-Custom"], "my-value")
	}
}

func TestHttpResponseMultipleHeaders(t *testing.T) {
	vm := NewVM()

	httpResponseClassVal := vm.Globals["HttpResponse"]

	respVal := vm.Send(httpResponseClassVal, "new:body:", []Value{
		FromSmallInt(200),
		NewStringValue("body"),
	})

	// Set multiple headers via chaining (each returns self)
	vm.Send(respVal, "header:value:", []Value{NewStringValue("X-A"), NewStringValue("1")})
	vm.Send(respVal, "header:value:", []Value{NewStringValue("X-B"), NewStringValue("2")})
	vm.Send(respVal, "contentType:", []Value{NewStringValue("text/plain")})

	resp := getHttpResponse(respVal)
	if resp.headers["X-A"] != "1" {
		t.Errorf("X-A = %q, want %q", resp.headers["X-A"], "1")
	}
	if resp.headers["X-B"] != "2" {
		t.Errorf("X-B = %q, want %q", resp.headers["X-B"], "2")
	}
	if resp.headers["Content-Type"] != "text/plain" {
		t.Errorf("Content-Type = %q, want %q", resp.headers["Content-Type"], "text/plain")
	}
}

func TestHttpResponseDefaultStatus(t *testing.T) {
	vm := NewVM()

	httpResponseClassVal := vm.Globals["HttpResponse"]

	// Non-integer status should default to 200
	respVal := vm.Send(httpResponseClassVal, "new:body:", []Value{
		NewStringValue("not-a-number"),
		NewStringValue("body"),
	})

	status := vm.Send(respVal, "status", nil)
	if !status.IsSmallInt() || status.SmallInt() != 200 {
		t.Errorf("default status = %v, want 200", status)
	}
}

func TestHttpResponseStatusOnNil(t *testing.T) {
	vm := NewVM()

	// Create and unregister
	resp := &HttpResponseObject{status: 200, body: "x", headers: map[string]string{}}
	val := registerHttpResponse(resp)

	// We can't easily unregister responses (no unregisterHttpResponse exported),
	// but we can test that status/body work on a fresh response
	status := vm.Send(val, "status", nil)
	if !status.IsSmallInt() || status.SmallInt() != 200 {
		t.Errorf("status = %v, want 200", status)
	}

	body := vm.Send(val, "body", nil)
	if !IsStringValue(body) || GetStringContent(body) != "x" {
		t.Errorf("body = %q, want %q", GetStringContent(body), "x")
	}
}

func TestHttpResponseEmptyBody(t *testing.T) {
	vm := NewVM()

	httpResponseClassVal := vm.Globals["HttpResponse"]

	respVal := vm.Send(httpResponseClassVal, "new:body:", []Value{
		FromSmallInt(204),
		NewStringValue(""),
	})

	body := vm.Send(respVal, "body", nil)
	if !IsStringValue(body) || GetStringContent(body) != "" {
		t.Errorf("body = %q, want empty string", GetStringContent(body))
	}

	status := vm.Send(respVal, "status", nil)
	if status.SmallInt() != 204 {
		t.Errorf("status = %v, want 204", status)
	}
}

// ---------------------------------------------------------------------------
// HttpRequest Primitive Tests (via vm.Send)
// ---------------------------------------------------------------------------

func TestHttpRequestMethod(t *testing.T) {
	vm := NewVM()

	methods := []string{"GET", "POST", "PUT", "DELETE", "PATCH"}
	for _, method := range methods {
		goReq := httptest.NewRequest(method, "/test", nil)
		reqObj := &HttpRequestObject{request: goReq}
		reqVal := registerHttpRequest(reqObj)

		result := vm.Send(reqVal, "method", nil)
		if !IsStringValue(result) || GetStringContent(result) != method {
			t.Errorf("method = %q, want %q", GetStringContent(result), method)
		}

		unregisterHttpRequest(reqVal)
	}
}

func TestHttpRequestPath(t *testing.T) {
	vm := NewVM()

	paths := []string{"/", "/api/users", "/api/users/123", "/search?q=test"}
	for _, path := range paths {
		goReq := httptest.NewRequest("GET", path, nil)
		reqObj := &HttpRequestObject{request: goReq}
		reqVal := registerHttpRequest(reqObj)

		result := vm.Send(reqVal, "path", nil)
		// URL.Path does not include query string
		expectedPath := strings.Split(path, "?")[0]
		if !IsStringValue(result) || GetStringContent(result) != expectedPath {
			t.Errorf("path = %q, want %q", GetStringContent(result), expectedPath)
		}

		unregisterHttpRequest(reqVal)
	}
}

func TestHttpRequestBody(t *testing.T) {
	vm := NewVM()

	bodyContent := `{"name": "test", "value": 42}`
	goReq := httptest.NewRequest("POST", "/api/data", strings.NewReader(bodyContent))
	goReq.Header.Set("Content-Type", "application/json")
	reqObj := &HttpRequestObject{request: goReq}
	reqVal := registerHttpRequest(reqObj)
	defer unregisterHttpRequest(reqVal)

	result := vm.Send(reqVal, "body", nil)
	if !IsStringValue(result) {
		t.Fatal("body did not return a string")
	}
	if GetStringContent(result) != bodyContent {
		t.Errorf("body = %q, want %q", GetStringContent(result), bodyContent)
	}
}

func TestHttpRequestBodyEmpty(t *testing.T) {
	vm := NewVM()

	goReq := httptest.NewRequest("GET", "/test", nil)
	reqObj := &HttpRequestObject{request: goReq}
	reqVal := registerHttpRequest(reqObj)
	defer unregisterHttpRequest(reqVal)

	result := vm.Send(reqVal, "body", nil)
	if !IsStringValue(result) || GetStringContent(result) != "" {
		t.Errorf("body = %q, want empty string", GetStringContent(result))
	}
}

func TestHttpRequestBodyReadCaching(t *testing.T) {
	vm := NewVM()

	goReq := httptest.NewRequest("POST", "/data", strings.NewReader("first-read"))
	reqObj := &HttpRequestObject{request: goReq}
	reqVal := registerHttpRequest(reqObj)
	defer unregisterHttpRequest(reqVal)

	// First read
	result1 := vm.Send(reqVal, "body", nil)
	if GetStringContent(result1) != "first-read" {
		t.Errorf("first body read = %q, want %q", GetStringContent(result1), "first-read")
	}

	// Second read should return the cached value (body was already consumed)
	result2 := vm.Send(reqVal, "body", nil)
	if GetStringContent(result2) != "first-read" {
		t.Errorf("second body read = %q, want %q (should be cached)", GetStringContent(result2), "first-read")
	}
}

func TestHttpRequestHeader(t *testing.T) {
	vm := NewVM()

	goReq := httptest.NewRequest("GET", "/test", nil)
	goReq.Header.Set("Content-Type", "application/json")
	goReq.Header.Set("Authorization", "Bearer token123")
	goReq.Header.Set("X-Custom-Header", "custom-value")

	reqObj := &HttpRequestObject{request: goReq}
	reqVal := registerHttpRequest(reqObj)
	defer unregisterHttpRequest(reqVal)

	tests := []struct {
		header string
		want   string
	}{
		{"Content-Type", "application/json"},
		{"Authorization", "Bearer token123"},
		{"X-Custom-Header", "custom-value"},
		{"Nonexistent", ""},
	}

	for _, tc := range tests {
		result := vm.Send(reqVal, "header:", []Value{NewStringValue(tc.header)})
		if !IsStringValue(result) || GetStringContent(result) != tc.want {
			t.Errorf("header: %q = %q, want %q", tc.header, GetStringContent(result), tc.want)
		}
	}
}

func TestHttpRequestQueryParam(t *testing.T) {
	vm := NewVM()

	goReq := httptest.NewRequest("GET", "/search?q=hello&page=2&limit=10", nil)
	reqObj := &HttpRequestObject{request: goReq}
	reqVal := registerHttpRequest(reqObj)
	defer unregisterHttpRequest(reqVal)

	tests := []struct {
		param string
		want  string
	}{
		{"q", "hello"},
		{"page", "2"},
		{"limit", "10"},
		{"missing", ""},
	}

	for _, tc := range tests {
		result := vm.Send(reqVal, "queryParam:", []Value{NewStringValue(tc.param)})
		if !IsStringValue(result) || GetStringContent(result) != tc.want {
			t.Errorf("queryParam: %q = %q, want %q", tc.param, GetStringContent(result), tc.want)
		}
	}
}

func TestHttpRequestMethodOnNil(t *testing.T) {
	vm := NewVM()

	// Create and unregister a request to get a dead reference
	goReq := httptest.NewRequest("GET", "/test", nil)
	reqObj := &HttpRequestObject{request: goReq}
	reqVal := registerHttpRequest(reqObj)
	unregisterHttpRequest(reqVal)

	// Sending to unregistered request should return empty string
	result := vm.Send(reqVal, "method", nil)
	if !IsStringValue(result) || GetStringContent(result) != "" {
		t.Errorf("method on unregistered request = %q, want empty string", GetStringContent(result))
	}

	result = vm.Send(reqVal, "path", nil)
	if !IsStringValue(result) || GetStringContent(result) != "" {
		t.Errorf("path on unregistered request = %q, want empty string", GetStringContent(result))
	}

	result = vm.Send(reqVal, "body", nil)
	if !IsStringValue(result) || GetStringContent(result) != "" {
		t.Errorf("body on unregistered request = %q, want empty string", GetStringContent(result))
	}
}

// ---------------------------------------------------------------------------
// HttpServer Class Assignment Tests
// ---------------------------------------------------------------------------

func TestHttpServerClassAssignment(t *testing.T) {
	vm := NewVM()

	httpServerClassVal := vm.Globals["HttpServer"]
	serverVal := vm.Send(httpServerClassVal, "new:", []Value{FromSmallInt(8080)})
	if serverVal == Nil {
		t.Fatal("HttpServer new: returned Nil")
	}

	// The symbolDispatch should resolve the class properly
	class := vm.ClassFor(serverVal)
	if class == nil {
		t.Fatal("ClassFor(HttpServer) returned nil")
	}
	if class.Name != "HttpServer" {
		t.Errorf("ClassFor(HttpServer).Name = %q, want %q", class.Name, "HttpServer")
	}
}

func TestHttpResponseClassAssignment(t *testing.T) {
	vm := NewVM()

	httpResponseClassVal := vm.Globals["HttpResponse"]
	respVal := vm.Send(httpResponseClassVal, "new:body:", []Value{
		FromSmallInt(200),
		NewStringValue("test"),
	})

	class := vm.ClassFor(respVal)
	if class == nil {
		t.Fatal("ClassFor(HttpResponse) returned nil")
	}
	if class.Name != "HttpResponse" {
		t.Errorf("ClassFor(HttpResponse).Name = %q, want %q", class.Name, "HttpResponse")
	}
}

func TestHttpRequestClassAssignment(t *testing.T) {
	vm := NewVM()

	goReq := httptest.NewRequest("GET", "/", nil)
	reqObj := &HttpRequestObject{request: goReq}
	reqVal := registerHttpRequest(reqObj)
	defer unregisterHttpRequest(reqVal)

	class := vm.ClassFor(reqVal)
	if class == nil {
		t.Fatal("ClassFor(HttpRequest) returned nil")
	}
	if class.Name != "HttpRequest" {
		t.Errorf("ClassFor(HttpRequest).Name = %q, want %q", class.Name, "HttpRequest")
	}
}

// ---------------------------------------------------------------------------
// Integration: HttpServer with real HTTP using httptest
// ---------------------------------------------------------------------------

func TestHttpServerStartAndStop(t *testing.T) {
	vm := NewVM()

	httpServerClassVal := vm.Globals["HttpServer"]
	// Use port 0 to let the OS assign a free port - but the server object
	// records the requested port, and ListenAndServe needs a real port.
	// We will test start/stop with a real high port.
	serverVal := vm.Send(httpServerClassVal, "new:", []Value{FromSmallInt(0)})
	if serverVal == Nil {
		t.Fatal("HttpServer new: returned Nil")
	}

	// Start in a goroutine since it blocks
	done := make(chan struct{})
	go func() {
		vm.Send(serverVal, "start", nil)
		close(done)
	}()

	// Give it a moment to start
	time.Sleep(50 * time.Millisecond)

	// Stop the server
	vm.Send(serverVal, "stop", nil)

	// Wait for start to return
	select {
	case <-done:
		// good
	case <-time.After(3 * time.Second):
		t.Fatal("server start did not return after stop")
	}
}

func TestHttpServerDoubleStart(t *testing.T) {
	vm := NewVM()

	httpServerClassVal := vm.Globals["HttpServer"]
	serverVal := vm.Send(httpServerClassVal, "new:", []Value{FromSmallInt(0)})

	// Start in goroutine
	go func() {
		vm.Send(serverVal, "start", nil)
	}()

	time.Sleep(50 * time.Millisecond)

	// Second start should return immediately (already running)
	srv := getHttpServer(serverVal)
	if srv != nil {
		srv.running.Store(true)
		result := vm.Send(serverVal, "start", nil)
		if result != serverVal {
			t.Error("second start should return receiver immediately")
		}
	}

	// Cleanup
	vm.Send(serverVal, "stop", nil)
	time.Sleep(50 * time.Millisecond)
}

// ---------------------------------------------------------------------------
// Integration: Full HTTP roundtrip using Go's httptest and the
// underlying HttpServerObject's mux directly (no need to start server)
// ---------------------------------------------------------------------------

func TestHttpServerMuxDirectRoundtrip(t *testing.T) {
	// This test exercises the HTTP server's mux handler directly
	// through httptest, without starting a real TCP listener.
	vm := NewVM()

	httpServerClassVal := vm.Globals["HttpServer"]
	serverVal := vm.Send(httpServerClassVal, "new:", []Value{FromSmallInt(0)})

	srv := getHttpServer(serverVal)
	if srv == nil {
		t.Fatal("could not get HttpServer object")
	}

	// Register a plain handler on the mux (simulates what route:method:handler: does)
	srv.mux.HandleFunc("/hello", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/plain")
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("Hello from Maggie"))
	})

	// Create httptest server with the mux
	ts := httptest.NewServer(srv.mux)
	defer ts.Close()

	// Make a real HTTP request
	resp, err := http.Get(ts.URL + "/hello")
	if err != nil {
		t.Fatalf("HTTP GET failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != 200 {
		t.Errorf("status = %d, want 200", resp.StatusCode)
	}
	if resp.Header.Get("Content-Type") != "text/plain" {
		t.Errorf("Content-Type = %q, want %q", resp.Header.Get("Content-Type"), "text/plain")
	}

	body, _ := io.ReadAll(resp.Body)
	if string(body) != "Hello from Maggie" {
		t.Errorf("body = %q, want %q", string(body), "Hello from Maggie")
	}
}

func TestHttpServerMuxMethodFiltering(t *testing.T) {
	vm := NewVM()

	httpServerClassVal := vm.Globals["HttpServer"]
	serverVal := vm.Send(httpServerClassVal, "new:", []Value{FromSmallInt(0)})

	srv := getHttpServer(serverVal)
	if srv == nil {
		t.Fatal("could not get HttpServer object")
	}

	// Register handler that only allows POST (simulates route:method:handler:)
	srv.mux.HandleFunc("/data", func(w http.ResponseWriter, r *http.Request) {
		if r.Method != "POST" {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}
		body, _ := io.ReadAll(r.Body)
		w.WriteHeader(http.StatusCreated)
		fmt.Fprintf(w, "Received: %s", string(body))
	})

	ts := httptest.NewServer(srv.mux)
	defer ts.Close()

	// GET should be rejected
	resp, err := http.Get(ts.URL + "/data")
	if err != nil {
		t.Fatalf("HTTP GET failed: %v", err)
	}
	resp.Body.Close()
	if resp.StatusCode != http.StatusMethodNotAllowed {
		t.Errorf("GET status = %d, want 405", resp.StatusCode)
	}

	// POST should succeed
	postResp, err := http.Post(ts.URL+"/data", "text/plain", strings.NewReader("hello"))
	if err != nil {
		t.Fatalf("HTTP POST failed: %v", err)
	}
	defer postResp.Body.Close()

	if postResp.StatusCode != http.StatusCreated {
		t.Errorf("POST status = %d, want 201", postResp.StatusCode)
	}
	body, _ := io.ReadAll(postResp.Body)
	if string(body) != "Received: hello" {
		t.Errorf("POST body = %q, want %q", string(body), "Received: hello")
	}
}

func TestHttpServerMuxJSONResponse(t *testing.T) {
	vm := NewVM()

	httpServerClassVal := vm.Globals["HttpServer"]
	serverVal := vm.Send(httpServerClassVal, "new:", []Value{FromSmallInt(0)})

	srv := getHttpServer(serverVal)
	if srv == nil {
		t.Fatal("could not get HttpServer object")
	}

	jsonBody := `{"status":"ok","count":42}`
	srv.mux.HandleFunc("/api/status", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusOK)
		w.Write([]byte(jsonBody))
	})

	ts := httptest.NewServer(srv.mux)
	defer ts.Close()

	resp, err := http.Get(ts.URL + "/api/status")
	if err != nil {
		t.Fatalf("HTTP GET failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.Header.Get("Content-Type") != "application/json" {
		t.Errorf("Content-Type = %q, want %q", resp.Header.Get("Content-Type"), "application/json")
	}

	body, _ := io.ReadAll(resp.Body)
	if string(body) != jsonBody {
		t.Errorf("body = %q, want %q", string(body), jsonBody)
	}
}

func TestHttpServerMuxMultipleRoutes(t *testing.T) {
	vm := NewVM()

	httpServerClassVal := vm.Globals["HttpServer"]
	serverVal := vm.Send(httpServerClassVal, "new:", []Value{FromSmallInt(0)})

	srv := getHttpServer(serverVal)
	if srv == nil {
		t.Fatal("could not get HttpServer object")
	}

	srv.mux.HandleFunc("/route-a", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("A"))
	})
	srv.mux.HandleFunc("/route-b", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("B"))
	})

	ts := httptest.NewServer(srv.mux)
	defer ts.Close()

	for _, tc := range []struct {
		path string
		want string
	}{
		{"/route-a", "A"},
		{"/route-b", "B"},
	} {
		resp, err := http.Get(ts.URL + tc.path)
		if err != nil {
			t.Fatalf("GET %s: %v", tc.path, err)
		}
		body, _ := io.ReadAll(resp.Body)
		resp.Body.Close()
		if string(body) != tc.want {
			t.Errorf("GET %s body = %q, want %q", tc.path, string(body), tc.want)
		}
	}
}

func TestHttpServerMuxRequestQueryParams(t *testing.T) {
	vm := NewVM()

	httpServerClassVal := vm.Globals["HttpServer"]
	serverVal := vm.Send(httpServerClassVal, "new:", []Value{FromSmallInt(0)})

	srv := getHttpServer(serverVal)
	if srv == nil {
		t.Fatal("could not get HttpServer object")
	}

	// Handler that echoes back query params
	srv.mux.HandleFunc("/echo", func(w http.ResponseWriter, r *http.Request) {
		name := r.URL.Query().Get("name")
		age := r.URL.Query().Get("age")
		fmt.Fprintf(w, "name=%s,age=%s", name, age)
	})

	ts := httptest.NewServer(srv.mux)
	defer ts.Close()

	resp, err := http.Get(ts.URL + "/echo?name=Alice&age=30")
	if err != nil {
		t.Fatalf("GET failed: %v", err)
	}
	defer resp.Body.Close()

	body, _ := io.ReadAll(resp.Body)
	if string(body) != "name=Alice,age=30" {
		t.Errorf("body = %q, want %q", string(body), "name=Alice,age=30")
	}
}

func TestHttpServerMuxRequestHeaders(t *testing.T) {
	vm := NewVM()

	httpServerClassVal := vm.Globals["HttpServer"]
	serverVal := vm.Send(httpServerClassVal, "new:", []Value{FromSmallInt(0)})

	srv := getHttpServer(serverVal)
	if srv == nil {
		t.Fatal("could not get HttpServer object")
	}

	// Handler that echoes back an incoming header
	srv.mux.HandleFunc("/headers", func(w http.ResponseWriter, r *http.Request) {
		auth := r.Header.Get("Authorization")
		fmt.Fprintf(w, "auth=%s", auth)
	})

	ts := httptest.NewServer(srv.mux)
	defer ts.Close()

	req, _ := http.NewRequest("GET", ts.URL+"/headers", nil)
	req.Header.Set("Authorization", "Bearer test-token")

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	body, _ := io.ReadAll(resp.Body)
	if string(body) != "auth=Bearer test-token" {
		t.Errorf("body = %q, want %q", string(body), "auth=Bearer test-token")
	}
}

// ---------------------------------------------------------------------------
// Globals Registration Tests
// ---------------------------------------------------------------------------

func TestHttpGlobalsRegistered(t *testing.T) {
	vm := NewVM()

	for _, name := range []string{"HttpServer", "HttpRequest", "HttpResponse"} {
		if _, ok := vm.Globals[name]; !ok {
			t.Errorf("global %q should be registered", name)
		}
	}
}
