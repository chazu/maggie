package vm

import (
	"testing"
)

// ---------------------------------------------------------------------------
// GrpcClient Registry Tests
// ---------------------------------------------------------------------------

func TestGrpcClientRegistration(t *testing.T) {
	vm := NewVM()

	clientObj := &GrpcClientObject{
		target: "localhost:50051",
	}

	val := vm.vmRegisterGrpcClient(clientObj)

	if !isGrpcClientValue(val) {
		t.Fatal("vmRegisterGrpcClient should produce a GrpcClient value")
	}

	got := vm.vmGetGrpcClient(val)
	if got == nil {
		t.Fatal("vmGetGrpcClient returned nil for registered client")
	}
	if got.target != "localhost:50051" {
		t.Errorf("target = %q, want %q", got.target, "localhost:50051")
	}

	// Unregister
	vm.vmUnregisterGrpcClient(val)

	got = vm.vmGetGrpcClient(val)
	if got != nil {
		t.Error("vmGetGrpcClient should return nil after unregister")
	}
}

func TestIsGrpcClientValueFalse(t *testing.T) {
	vm := NewVM()
	_ = vm // used for string creation below

	if isGrpcClientValue(Nil) {
		t.Error("Nil should not be a GrpcClient value")
	}
	if isGrpcClientValue(FromSmallInt(42)) {
		t.Error("SmallInt should not be a GrpcClient value")
	}
	if isGrpcClientValue(vm.registry.NewStringValue("test")) {
		t.Error("String should not be a GrpcClient value")
	}
	if isGrpcClientValue(True) {
		t.Error("True should not be a GrpcClient value")
	}
	if isGrpcClientValue(False) {
		t.Error("False should not be a GrpcClient value")
	}
}

func TestGrpcClientMultipleRegistrations(t *testing.T) {
	vm := NewVM()

	client1 := &GrpcClientObject{target: "host1:50051"}
	client2 := &GrpcClientObject{target: "host2:50051"}
	client3 := &GrpcClientObject{target: "host3:50051"}

	val1 := vm.vmRegisterGrpcClient(client1)
	val2 := vm.vmRegisterGrpcClient(client2)
	val3 := vm.vmRegisterGrpcClient(client3)

	// All should be distinct values
	if val1 == val2 || val2 == val3 || val1 == val3 {
		t.Error("each registration should produce a unique value")
	}

	// Each should resolve to the correct client
	if vm.vmGetGrpcClient(val1).target != "host1:50051" {
		t.Error("val1 should resolve to client1")
	}
	if vm.vmGetGrpcClient(val2).target != "host2:50051" {
		t.Error("val2 should resolve to client2")
	}
	if vm.vmGetGrpcClient(val3).target != "host3:50051" {
		t.Error("val3 should resolve to client3")
	}

	// Unregister one and check others still work
	vm.vmUnregisterGrpcClient(val2)
	if vm.vmGetGrpcClient(val2) != nil {
		t.Error("val2 should be nil after unregister")
	}
	if vm.vmGetGrpcClient(val1) == nil {
		t.Error("val1 should still be valid after unregistering val2")
	}
	if vm.vmGetGrpcClient(val3) == nil {
		t.Error("val3 should still be valid after unregistering val2")
	}

	// Cleanup
	vm.vmUnregisterGrpcClient(val1)
	vm.vmUnregisterGrpcClient(val3)
}

func TestUnregisterGrpcClientInvalidValue(t *testing.T) {
	vm := NewVM()

	// Unregistering non-GrpcClient values should not panic
	vm.vmUnregisterGrpcClient(Nil)
	vm.vmUnregisterGrpcClient(FromSmallInt(42))
	vm.vmUnregisterGrpcClient(vm.registry.NewStringValue("test"))
}

// ---------------------------------------------------------------------------
// GrpcStream Registry Tests
// ---------------------------------------------------------------------------

func TestGrpcStreamRegistration(t *testing.T) {
	vm := NewVM()

	streamObj := &GrpcStreamObject{
		streamType: GrpcStreamServerStreaming,
	}

	val := vm.vmRegisterGrpcStream(streamObj)

	if !isGrpcStreamValue(val) {
		t.Fatal("vmRegisterGrpcStream should produce a GrpcStream value")
	}

	got := vm.vmGetGrpcStream(val)
	if got == nil {
		t.Fatal("vmGetGrpcStream returned nil for registered stream")
	}
	if got.streamType != GrpcStreamServerStreaming {
		t.Errorf("streamType = %v, want GrpcStreamServerStreaming", got.streamType)
	}

	// Unregister
	vm.vmUnregisterGrpcStream(val)

	got = vm.vmGetGrpcStream(val)
	if got != nil {
		t.Error("vmGetGrpcStream should return nil after unregister")
	}
}

func TestIsGrpcStreamValueFalse(t *testing.T) {
	vm := NewVM()
	_ = vm // used for string creation below

	if isGrpcStreamValue(Nil) {
		t.Error("Nil should not be a GrpcStream value")
	}
	if isGrpcStreamValue(FromSmallInt(42)) {
		t.Error("SmallInt should not be a GrpcStream value")
	}
	if isGrpcStreamValue(vm.registry.NewStringValue("test")) {
		t.Error("String should not be a GrpcStream value")
	}
}

func TestGrpcStreamTypes(t *testing.T) {
	vm := NewVM()

	types := []GrpcStreamType{
		GrpcStreamServerStreaming,
		GrpcStreamClientStreaming,
		GrpcStreamBidirectional,
	}

	for _, st := range types {
		streamObj := &GrpcStreamObject{streamType: st}
		val := vm.vmRegisterGrpcStream(streamObj)

		got := vm.vmGetGrpcStream(val)
		if got.streamType != st {
			t.Errorf("streamType = %v, want %v", got.streamType, st)
		}

		vm.vmUnregisterGrpcStream(val)
	}
}

func TestGrpcStreamSendClosedFlag(t *testing.T) {
	vm := NewVM()

	streamObj := &GrpcStreamObject{
		streamType: GrpcStreamServerStreaming,
	}

	val := vm.vmRegisterGrpcStream(streamObj)
	defer vm.vmUnregisterGrpcStream(val)

	got := vm.vmGetGrpcStream(val)
	if got.sendClosed.Load() {
		t.Error("sendClosed should be false initially")
	}

	got.sendClosed.Store(true)
	if !got.sendClosed.Load() {
		t.Error("sendClosed should be true after Store(true)")
	}
}

func TestGrpcStreamRecvClosedFlag(t *testing.T) {
	vm := NewVM()

	streamObj := &GrpcStreamObject{
		streamType: GrpcStreamBidirectional,
	}

	val := vm.vmRegisterGrpcStream(streamObj)
	defer vm.vmUnregisterGrpcStream(val)

	got := vm.vmGetGrpcStream(val)
	if got.recvClosed.Load() {
		t.Error("recvClosed should be false initially")
	}

	got.recvClosed.Store(true)
	if !got.recvClosed.Load() {
		t.Error("recvClosed should be true after Store(true)")
	}
}

func TestUnregisterGrpcStreamInvalidValue(t *testing.T) {
	vm := NewVM()

	// Should not panic
	vm.vmUnregisterGrpcStream(Nil)
	vm.vmUnregisterGrpcStream(FromSmallInt(42))
	vm.vmUnregisterGrpcStream(vm.registry.NewStringValue("test"))
}

// ---------------------------------------------------------------------------
// GrpcClient/GrpcStream marker distinctness
// ---------------------------------------------------------------------------

func TestGrpcMarkersDistinct(t *testing.T) {
	vm := NewVM()

	// grpcClientMarker and grpcStreamMarker should be different
	if grpcClientMarker == grpcStreamMarker {
		t.Error("client and stream markers should be distinct")
	}

	// A GrpcClient value should not be recognized as a GrpcStream value
	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vm.vmRegisterGrpcClient(clientObj)
	defer vm.vmUnregisterGrpcClient(clientVal)

	if isGrpcStreamValue(clientVal) {
		t.Error("GrpcClient value should not be identified as GrpcStream")
	}

	// A GrpcStream value should not be recognized as a GrpcClient value
	streamObj := &GrpcStreamObject{streamType: GrpcStreamBidirectional}
	streamVal := vm.vmRegisterGrpcStream(streamObj)
	defer vm.vmUnregisterGrpcStream(streamVal)

	if isGrpcClientValue(streamVal) {
		t.Error("GrpcStream value should not be identified as GrpcClient")
	}
}

// ---------------------------------------------------------------------------
// Result helper function tests (grpcSuccess / grpcFailure)
// ---------------------------------------------------------------------------

func TestGrpcSuccess(t *testing.T) {
	vm := NewVM()

	inner := FromSmallInt(42)
	result := vm.grpcSuccess(inner)

	if !isResultValue(result) {
		t.Fatal("grpcSuccess should return a Result value")
	}

	// Check it is a success
	isSuccess := vm.Send(result, "isSuccess", nil)
	if isSuccess != True {
		t.Error("grpcSuccess should create a Success result")
	}

	// Unwrap the value
	val := vm.Send(result, "value", nil)
	if !val.IsSmallInt() || val.SmallInt() != 42 {
		t.Errorf("grpcSuccess value = %v, want 42", val)
	}
}

func TestGrpcSuccessWithStringValue(t *testing.T) {
	vm := NewVM()

	inner := vm.registry.NewStringValue("connected")
	result := vm.grpcSuccess(inner)

	isSuccess := vm.Send(result, "isSuccess", nil)
	if isSuccess != True {
		t.Error("grpcSuccess should create a Success result")
	}

	val := vm.Send(result, "value", nil)
	if !IsStringValue(val) || vm.registry.GetStringContent(val) != "connected" {
		t.Errorf("value = %q, want %q", vm.registry.GetStringContent(val), "connected")
	}
}

func TestGrpcFailure(t *testing.T) {
	vm := NewVM()

	result := vm.grpcFailure("connection refused")

	if !isResultValue(result) {
		t.Fatal("grpcFailure should return a Result value")
	}

	// Check it is a failure
	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Error("grpcFailure should create a Failure result")
	}

	// Check error message
	errVal := vm.Send(result, "error", nil)
	if !IsStringValue(errVal) {
		t.Fatal("grpcFailure error should be a string")
	}
	if vm.registry.GetStringContent(errVal) != "connection refused" {
		t.Errorf("error = %q, want %q", vm.registry.GetStringContent(errVal), "connection refused")
	}

	// value should be nil on failure
	val := vm.Send(result, "value", nil)
	if val != Nil {
		t.Errorf("grpcFailure value should be Nil, got %v", val)
	}
}

func TestGrpcFailureVariousMessages(t *testing.T) {
	vm := NewVM()

	messages := []string{
		"connection failed: dial tcp: connection refused",
		"invalid client",
		"method name must be a string",
		"request must be a dictionary",
		"stream send closed",
		"end of stream",
		"",
	}

	for _, msg := range messages {
		result := vm.grpcFailure(msg)
		isFailure := vm.Send(result, "isFailure", nil)
		if isFailure != True {
			t.Errorf("vm.grpcFailure(%q) should be Failure", msg)
		}
		errVal := vm.Send(result, "error", nil)
		if vm.registry.GetStringContent(errVal) != msg {
			t.Errorf("error = %q, want %q", vm.registry.GetStringContent(errVal), msg)
		}
	}
}

// ---------------------------------------------------------------------------
// GrpcClient Primitive Tests (via vm.Send)
// ---------------------------------------------------------------------------

func TestGrpcGlobalsRegistered(t *testing.T) {
	vm := NewVM()

	for _, name := range []string{"GrpcClient", "GrpcStream"} {
		if _, ok := vm.Globals[name]; !ok {
			t.Errorf("global %q should be registered", name)
		}
	}
}

func TestGrpcClientClassAssignment(t *testing.T) {
	vm := NewVM()

	clientObj := &GrpcClientObject{target: "test:50051"}
	clientVal := vm.vmRegisterGrpcClient(clientObj)
	defer vm.vmUnregisterGrpcClient(clientVal)

	class := vm.ClassFor(clientVal)
	if class == nil {
		t.Fatal("ClassFor(GrpcClient) returned nil")
	}
	if class.Name != "GrpcClient" {
		t.Errorf("ClassFor(GrpcClient).Name = %q, want %q", class.Name, "GrpcClient")
	}
}

func TestGrpcStreamClassAssignment(t *testing.T) {
	vm := NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamBidirectional}
	streamVal := vm.vmRegisterGrpcStream(streamObj)
	defer vm.vmUnregisterGrpcStream(streamVal)

	class := vm.ClassFor(streamVal)
	if class == nil {
		t.Fatal("ClassFor(GrpcStream) returned nil")
	}
	if class.Name != "GrpcStream" {
		t.Errorf("ClassFor(GrpcStream).Name = %q, want %q", class.Name, "GrpcStream")
	}
}

func TestGrpcClientConnectToNonString(t *testing.T) {
	vm := NewVM()

	grpcClientClassVal := vm.Globals["GrpcClient"]

	// connectTo: with a non-string argument should return a Failure result
	result := vm.Send(grpcClientClassVal, "connectTo:", []Value{FromSmallInt(12345)})

	if !isResultValue(result) {
		t.Fatal("connectTo: with non-string should return a Result")
	}

	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Error("connectTo: with non-string should be Failure")
	}

	errVal := vm.Send(result, "error", nil)
	if !IsStringValue(errVal) {
		t.Fatal("error should be a string")
	}
	errMsg := vm.registry.GetStringContent(errVal)
	if errMsg != "target must be a string" {
		t.Errorf("error = %q, want %q", errMsg, "target must be a string")
	}
}

func TestGrpcClientConnectToEmptyTarget(t *testing.T) {
	vm := NewVM()

	grpcClientClassVal := vm.Globals["GrpcClient"]

	// connectTo: with an empty string - gRPC dial is lazy so it may still succeed
	// creating the client object. The actual connection failure would happen later.
	result := vm.Send(grpcClientClassVal, "connectTo:", []Value{vm.registry.NewStringValue("")})

	// Should be a Result (either success or failure depending on gRPC behavior)
	if !isResultValue(result) {
		t.Fatal("connectTo: should return a Result")
	}
}

func TestGrpcClientConnectToTarget(t *testing.T) {
	vm := NewVM()

	grpcClientClassVal := vm.Globals["GrpcClient"]

	// Connect to a target - gRPC dial is lazy so this should succeed
	result := vm.Send(grpcClientClassVal, "connectTo:", []Value{vm.registry.NewStringValue("localhost:50051")})

	if !isResultValue(result) {
		t.Fatal("connectTo: should return a Result")
	}

	// gRPC dial is lazy so this should be a success (creates the connection object)
	isSuccess := vm.Send(result, "isSuccess", nil)
	if isSuccess != True {
		errVal := vm.Send(result, "error", nil)
		t.Fatalf("connectTo: failed unexpectedly: %q", vm.registry.GetStringContent(errVal))
	}

	// Unwrap the client value
	clientVal := vm.Send(result, "value", nil)
	if !isGrpcClientValue(clientVal) {
		t.Fatal("connectTo: success value should be a GrpcClient")
	}

	// Test isConnected
	isConnected := vm.Send(clientVal, "isConnected", nil)
	if isConnected != True {
		t.Error("newly created client should be connected")
	}

	// Close the client
	vm.Send(clientVal, "close", nil)

	// After close, isConnected should be false
	isConnected = vm.Send(clientVal, "isConnected", nil)
	if isConnected != False {
		t.Error("client should not be connected after close")
	}
}

func TestGrpcClientCloseIdempotent(t *testing.T) {
	vm := NewVM()

	grpcClientClassVal := vm.Globals["GrpcClient"]

	result := vm.Send(grpcClientClassVal, "connectTo:", []Value{vm.registry.NewStringValue("localhost:50051")})
	clientVal := vm.Send(result, "value", nil)

	// Close once
	closeResult := vm.Send(clientVal, "close", nil)
	if closeResult != clientVal {
		t.Error("close should return receiver")
	}

	// Close again should not panic
	closeResult = vm.Send(clientVal, "close", nil)
	if closeResult != clientVal {
		t.Error("second close should return receiver")
	}
}

func TestGrpcClientIsConnectedOnNilClient(t *testing.T) {
	vm := NewVM()

	// Create and unregister to simulate a dead reference
	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vm.vmRegisterGrpcClient(clientObj)
	vm.vmUnregisterGrpcClient(clientVal)

	isConnected := vm.Send(clientVal, "isConnected", nil)
	if isConnected != False {
		t.Error("isConnected on unregistered client should return False")
	}
}

func TestGrpcClientCloseOnNilClient(t *testing.T) {
	vm := NewVM()

	// Create and unregister
	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vm.vmRegisterGrpcClient(clientObj)
	vm.vmUnregisterGrpcClient(clientVal)

	// close on unregistered client should not panic
	result := vm.Send(clientVal, "close", nil)
	if result != clientVal {
		t.Error("close on unregistered client should return receiver")
	}
}

func TestGrpcClientListServicesNoConnection(t *testing.T) {
	vm := NewVM()

	// Create a client with no real connection (refClient is nil)
	// This simulates calling listServices on a closed/invalid client
	clientObj := &GrpcClientObject{target: "test"}
	clientObj.closed.Store(true)
	clientVal := vm.vmRegisterGrpcClient(clientObj)
	defer vm.vmUnregisterGrpcClient(clientVal)

	// listServices on a closed client should return an empty array
	result := vm.Send(clientVal, "listServices", nil)
	if !result.IsObject() {
		t.Fatal("listServices should return an array")
	}

	size := vm.Send(result, "size", nil)
	if !size.IsSmallInt() || size.SmallInt() != 0 {
		t.Errorf("listServices on closed client should return empty array, got size %v", size)
	}
}

func TestGrpcClientMethodsForServiceNilClient(t *testing.T) {
	vm := NewVM()

	// Unregistered client
	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vm.vmRegisterGrpcClient(clientObj)
	vm.vmUnregisterGrpcClient(clientVal)

	result := vm.Send(clientVal, "methodsForService:", []Value{vm.registry.NewStringValue("test.Service")})
	if !result.IsObject() {
		t.Fatal("methodsForService: should return an array")
	}

	size := vm.Send(result, "size", nil)
	if !size.IsSmallInt() || size.SmallInt() != 0 {
		t.Errorf("methodsForService: on nil client should return empty array, got size %v", size)
	}
}

func TestGrpcClientMethodsForServiceNonString(t *testing.T) {
	vm := NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vm.vmRegisterGrpcClient(clientObj)
	defer vm.vmUnregisterGrpcClient(clientVal)

	// Non-string argument should return empty array
	result := vm.Send(clientVal, "methodsForService:", []Value{FromSmallInt(42)})
	if !result.IsObject() {
		t.Fatal("methodsForService: should return an array")
	}

	size := vm.Send(result, "size", nil)
	if !size.IsSmallInt() || size.SmallInt() != 0 {
		t.Errorf("methodsForService: with non-string should return empty array, got size %v", size)
	}
}

func TestGrpcClientMethodDescriptorNilClient(t *testing.T) {
	vm := NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vm.vmRegisterGrpcClient(clientObj)
	vm.vmUnregisterGrpcClient(clientVal)

	result := vm.Send(clientVal, "methodDescriptor:", []Value{vm.registry.NewStringValue("test.Service/Method")})
	if result != Nil {
		t.Errorf("methodDescriptor: on nil client should return Nil, got %v", result)
	}
}

func TestGrpcClientMethodDescriptorNonString(t *testing.T) {
	vm := NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vm.vmRegisterGrpcClient(clientObj)
	defer vm.vmUnregisterGrpcClient(clientVal)

	result := vm.Send(clientVal, "methodDescriptor:", []Value{FromSmallInt(42)})
	if result != Nil {
		t.Errorf("methodDescriptor: with non-string should return Nil, got %v", result)
	}
}

func TestGrpcClientCallWithInvalidClient(t *testing.T) {
	vm := NewVM()

	// Unregistered client
	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vm.vmRegisterGrpcClient(clientObj)
	vm.vmUnregisterGrpcClient(clientVal)

	dict := vm.registry.NewDictionaryValue()
	result := vm.Send(clientVal, "call:with:", []Value{vm.registry.NewStringValue("test/Method"), dict})

	if !isResultValue(result) {
		t.Fatal("call:with: should return a Result")
	}
	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Error("call:with: on invalid client should return Failure")
	}
}

func TestGrpcClientCallWithNonStringMethod(t *testing.T) {
	vm := NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vm.vmRegisterGrpcClient(clientObj)
	defer vm.vmUnregisterGrpcClient(clientVal)

	dict := vm.registry.NewDictionaryValue()
	result := vm.Send(clientVal, "call:with:", []Value{FromSmallInt(42), dict})

	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Error("call:with: with non-string method should return Failure")
	}

	errVal := vm.Send(result, "error", nil)
	if vm.registry.GetStringContent(errVal) != "method name must be a string" {
		t.Errorf("error = %q, want %q", vm.registry.GetStringContent(errVal), "method name must be a string")
	}
}

func TestGrpcClientCallWithNonDictionaryRequest(t *testing.T) {
	vm := NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vm.vmRegisterGrpcClient(clientObj)
	defer vm.vmUnregisterGrpcClient(clientVal)

	result := vm.Send(clientVal, "call:with:", []Value{vm.registry.NewStringValue("test/Method"), FromSmallInt(42)})

	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Error("call:with: with non-dictionary request should return Failure")
	}

	errVal := vm.Send(result, "error", nil)
	if vm.registry.GetStringContent(errVal) != "request must be a dictionary" {
		t.Errorf("error = %q, want %q", vm.registry.GetStringContent(errVal), "request must be a dictionary")
	}
}

func TestGrpcClientServerStreamInvalidClient(t *testing.T) {
	vm := NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vm.vmRegisterGrpcClient(clientObj)
	vm.vmUnregisterGrpcClient(clientVal)

	dict := vm.registry.NewDictionaryValue()
	result := vm.Send(clientVal, "serverStream:with:", []Value{vm.registry.NewStringValue("test/Method"), dict})

	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Error("serverStream:with: on invalid client should return Failure")
	}
}

func TestGrpcClientServerStreamNonStringMethod(t *testing.T) {
	vm := NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vm.vmRegisterGrpcClient(clientObj)
	defer vm.vmUnregisterGrpcClient(clientVal)

	dict := vm.registry.NewDictionaryValue()
	result := vm.Send(clientVal, "serverStream:with:", []Value{FromSmallInt(42), dict})

	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Error("serverStream:with: with non-string method should return Failure")
	}
}

func TestGrpcClientServerStreamNonDictRequest(t *testing.T) {
	vm := NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vm.vmRegisterGrpcClient(clientObj)
	defer vm.vmUnregisterGrpcClient(clientVal)

	result := vm.Send(clientVal, "serverStream:with:", []Value{vm.registry.NewStringValue("test/Method"), FromSmallInt(42)})

	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Error("serverStream:with: with non-dict request should return Failure")
	}
}

func TestGrpcClientClientStreamInvalidClient(t *testing.T) {
	vm := NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vm.vmRegisterGrpcClient(clientObj)
	vm.vmUnregisterGrpcClient(clientVal)

	result := vm.Send(clientVal, "clientStream:", []Value{vm.registry.NewStringValue("test/Method")})

	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Error("clientStream: on invalid client should return Failure")
	}
}

func TestGrpcClientClientStreamNonString(t *testing.T) {
	vm := NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vm.vmRegisterGrpcClient(clientObj)
	defer vm.vmUnregisterGrpcClient(clientVal)

	result := vm.Send(clientVal, "clientStream:", []Value{FromSmallInt(42)})

	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Error("clientStream: with non-string should return Failure")
	}
}

func TestGrpcClientBidiStreamInvalidClient(t *testing.T) {
	vm := NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vm.vmRegisterGrpcClient(clientObj)
	vm.vmUnregisterGrpcClient(clientVal)

	result := vm.Send(clientVal, "bidiStream:", []Value{vm.registry.NewStringValue("test/Method")})

	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Error("bidiStream: on invalid client should return Failure")
	}
}

func TestGrpcClientBidiStreamNonString(t *testing.T) {
	vm := NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vm.vmRegisterGrpcClient(clientObj)
	defer vm.vmUnregisterGrpcClient(clientVal)

	result := vm.Send(clientVal, "bidiStream:", []Value{FromSmallInt(42)})

	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Error("bidiStream: with non-string should return Failure")
	}
}

// ---------------------------------------------------------------------------
// GrpcStream Primitive Tests (via vm.Send)
// ---------------------------------------------------------------------------

func TestGrpcStreamHasNextOnNilStream(t *testing.T) {
	vm := NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamServerStreaming}
	streamVal := vm.vmRegisterGrpcStream(streamObj)
	vm.vmUnregisterGrpcStream(streamVal)

	// hasNext on unregistered stream should return False
	result := vm.Send(streamVal, "hasNext", nil)
	if result != False {
		t.Error("hasNext on unregistered stream should return False")
	}
}

func TestGrpcStreamHasNextRecvClosed(t *testing.T) {
	vm := NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamServerStreaming}
	streamObj.recvClosed.Store(true)
	streamVal := vm.vmRegisterGrpcStream(streamObj)
	defer vm.vmUnregisterGrpcStream(streamVal)

	result := vm.Send(streamVal, "hasNext", nil)
	if result != False {
		t.Error("hasNext on recv-closed stream should return False")
	}
}

func TestGrpcStreamHasNextOpen(t *testing.T) {
	vm := NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamServerStreaming}
	streamVal := vm.vmRegisterGrpcStream(streamObj)
	defer vm.vmUnregisterGrpcStream(streamVal)

	result := vm.Send(streamVal, "hasNext", nil)
	if result != True {
		t.Error("hasNext on open stream should return True")
	}
}

func TestGrpcStreamCloseOnNilStream(t *testing.T) {
	vm := NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamClientStreaming}
	streamVal := vm.vmRegisterGrpcStream(streamObj)
	vm.vmUnregisterGrpcStream(streamVal)

	// close on nil stream should not panic and should return receiver
	result := vm.Send(streamVal, "close", nil)
	if result != streamVal {
		t.Error("close on unregistered stream should return receiver")
	}
}

func TestGrpcStreamSendOnNilStream(t *testing.T) {
	vm := NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamClientStreaming}
	streamVal := vm.vmRegisterGrpcStream(streamObj)
	vm.vmUnregisterGrpcStream(streamVal)

	dict := vm.registry.NewDictionaryValue()
	result := vm.Send(streamVal, "send:", []Value{dict})

	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Error("send: on nil stream should return Failure")
	}
}

func TestGrpcStreamSendNonDictionary(t *testing.T) {
	vm := NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamClientStreaming}
	streamVal := vm.vmRegisterGrpcStream(streamObj)
	defer vm.vmUnregisterGrpcStream(streamVal)

	result := vm.Send(streamVal, "send:", []Value{FromSmallInt(42)})

	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Error("send: with non-dict should return Failure")
	}

	errVal := vm.Send(result, "error", nil)
	if vm.registry.GetStringContent(errVal) != "message must be a dictionary" {
		t.Errorf("error = %q, want %q", vm.registry.GetStringContent(errVal), "message must be a dictionary")
	}
}

func TestGrpcStreamSendOnClosedSend(t *testing.T) {
	vm := NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamClientStreaming}
	streamObj.sendClosed.Store(true)
	streamVal := vm.vmRegisterGrpcStream(streamObj)
	defer vm.vmUnregisterGrpcStream(streamVal)

	dict := vm.registry.NewDictionaryValue()
	result := vm.Send(streamVal, "send:", []Value{dict})

	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Error("send: on send-closed stream should return Failure")
	}

	errVal := vm.Send(result, "error", nil)
	if vm.registry.GetStringContent(errVal) != "stream send closed" {
		t.Errorf("error = %q, want %q", vm.registry.GetStringContent(errVal), "stream send closed")
	}
}

func TestGrpcStreamReceiveOnNilStream(t *testing.T) {
	vm := NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamServerStreaming}
	streamVal := vm.vmRegisterGrpcStream(streamObj)
	vm.vmUnregisterGrpcStream(streamVal)

	result := vm.Send(streamVal, "receive", nil)

	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Error("receive on nil stream should return Failure")
	}
}

func TestGrpcStreamReceiveOnRecvClosed(t *testing.T) {
	vm := NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamServerStreaming}
	streamObj.recvClosed.Store(true)
	streamVal := vm.vmRegisterGrpcStream(streamObj)
	defer vm.vmUnregisterGrpcStream(streamVal)

	result := vm.Send(streamVal, "receive", nil)

	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Error("receive on recv-closed stream should return Failure")
	}

	errVal := vm.Send(result, "error", nil)
	if vm.registry.GetStringContent(errVal) != "end of stream" {
		t.Errorf("error = %q, want %q", vm.registry.GetStringContent(errVal), "end of stream")
	}
}

func TestGrpcStreamCloseAndReceiveOnNilStream(t *testing.T) {
	vm := NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamClientStreaming}
	streamVal := vm.vmRegisterGrpcStream(streamObj)
	vm.vmUnregisterGrpcStream(streamVal)

	result := vm.Send(streamVal, "closeAndReceive", nil)

	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Error("closeAndReceive on nil stream should return Failure")
	}
}

func TestGrpcStreamStreamTypeOnNilStream(t *testing.T) {
	vm := NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamServerStreaming}
	streamVal := vm.vmRegisterGrpcStream(streamObj)
	vm.vmUnregisterGrpcStream(streamVal)

	result := vm.Send(streamVal, "streamType", nil)
	if result != Nil {
		t.Error("streamType on nil stream should return Nil")
	}
}

func TestGrpcStreamStreamTypeValues(t *testing.T) {
	vm := NewVM()

	tests := []struct {
		stype GrpcStreamType
		want  string
	}{
		{GrpcStreamServerStreaming, "serverStreaming"},
		{GrpcStreamClientStreaming, "clientStreaming"},
		{GrpcStreamBidirectional, "bidirectional"},
	}

	for _, tc := range tests {
		streamObj := &GrpcStreamObject{streamType: tc.stype}
		streamVal := vm.vmRegisterGrpcStream(streamObj)

		result := vm.Send(streamVal, "streamType", nil)
		if !result.IsSymbol() {
			t.Fatalf("streamType should return a symbol for %v", tc.stype)
		}

		name := vm.Symbols.Name(result.SymbolID())
		if name != tc.want {
			t.Errorf("streamType = %q, want %q", name, tc.want)
		}

		vm.vmUnregisterGrpcStream(streamVal)
	}
}

// ---------------------------------------------------------------------------
// resolveMethod helper tests
// ---------------------------------------------------------------------------

func TestResolveMethodInvalidFormat(t *testing.T) {
	// No client needed for format validation - only test cases where
	// strings.Split produces != 2 parts (before refClient is touched)
	client := &GrpcClientObject{target: "test"}

	// Missing "/" separator - Split returns 1 part
	_, err := resolveMethod(client, "no-slash-here")
	if err == nil {
		t.Error("resolveMethod should fail for method name without '/'")
	}

	// Too many "/" separators - Split returns 3 parts
	_, err = resolveMethod(client, "a/b/c")
	if err == nil {
		t.Error("resolveMethod should fail for method name with multiple '/'")
	}

	// Empty string - Split returns 1 part
	_, err = resolveMethod(client, "")
	if err == nil {
		t.Error("resolveMethod should fail for empty method name")
	}
}

// ---------------------------------------------------------------------------
// Marker collision tests
// ---------------------------------------------------------------------------

func TestGrpcMarkersDontCollideWithHttp(t *testing.T) {
	// Ensure gRPC markers don't collide with HTTP markers
	markers := map[string]uint32{
		"httpServer":   httpServerMarker,
		"httpRequest":  httpRequestMarker,
		"httpResponse": httpResponseMarker,
		"grpcClient":   grpcClientMarker,
		"grpcStream":   grpcStreamMarker,
	}

	seen := make(map[uint32]string)
	for name, marker := range markers {
		if existing, ok := seen[marker]; ok {
			t.Errorf("marker collision: %q and %q both use 0x%08X", name, existing, marker)
		}
		seen[marker] = name
	}
}
