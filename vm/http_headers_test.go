package vm

import (
	"io"
	"net/http"
	"net/http/httptest"
	"testing"
)

// TestHttpClientPostWithHeaders verifies post:body:contentType:headers: forwards
// custom headers to the server.
func TestHttpClientPostWithHeaders(t *testing.T) {
	var gotAuth, gotX, gotCT string
	var gotBody []byte
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotAuth = r.Header.Get("Authorization")
		gotX = r.Header.Get("X-Custom")
		gotCT = r.Header.Get("Content-Type")
		gotBody, _ = io.ReadAll(r.Body)
		w.Write([]byte("ok"))
	}))
	defer srv.Close()

	vm := NewVM()
	client := sendClass(t, vm, "HttpClient", "new")

	headers := vm.registry.NewDictionaryValue()
	dict := vm.registry.GetDictionaryObject(headers)
	authKey := vm.registry.NewStringValue("Authorization")
	authVal := vm.registry.NewStringValue("Bearer secret")
	xKey := vm.registry.NewStringValue("X-Custom")
	xVal := vm.registry.NewStringValue("maggie")
	dict.Data[hashValue(vm.registry, authKey)] = authVal
	dict.Keys[hashValue(vm.registry, authKey)] = authKey
	dict.Data[hashValue(vm.registry, xKey)] = xVal
	dict.Keys[hashValue(vm.registry, xKey)] = xKey

	body := vm.Send(client, "post:body:contentType:headers:", []Value{
		vm.registry.NewStringValue(srv.URL),
		vm.registry.NewStringValue("hello"),
		vm.registry.NewStringValue("text/plain"),
		headers,
	})
	if vm.registry.GetStringContent(body) != "ok" {
		t.Fatalf("unexpected body: %q", vm.registry.GetStringContent(body))
	}
	if gotAuth != "Bearer secret" {
		t.Fatalf("auth header = %q", gotAuth)
	}
	if gotX != "maggie" {
		t.Fatalf("x-custom header = %q", gotX)
	}
	if gotCT != "text/plain" {
		t.Fatalf("content-type = %q", gotCT)
	}
	if string(gotBody) != "hello" {
		t.Fatalf("body = %q", gotBody)
	}
}
