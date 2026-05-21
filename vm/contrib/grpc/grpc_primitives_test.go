package grpc

import (
	"testing"

	vm "github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// GrpcClient Registry Tests
// ---------------------------------------------------------------------------

func TestGrpcClientRegistration(t *testing.T) {
	vmInst := vm.NewVM()

	clientObj := &GrpcClientObject{
		target: "localhost:50051",
	}

	val := vmRegisterGrpcClient(vmInst, clientObj)

	if !isGrpcClientValue(val) {
		t.Fatal("vmRegisterGrpcClient should produce a GrpcClient value")
	}

	got := vmGetGrpcClient(vmInst, val)
	if got == nil {
		t.Fatal("vmGetGrpcClient returned nil for registered client")
	}
	if got.target != "localhost:50051" {
		t.Errorf("target = %q, want %q", got.target, "localhost:50051")
	}

	// Unregister
	vmUnregisterGrpcClient(vmInst, val)

	got = vmGetGrpcClient(vmInst, val)
	if got != nil {
		t.Error("vmGetGrpcClient should return nil after unregister")
	}
}

func TestIsGrpcClientValueFalse(t *testing.T) {
	vmInst := vm.NewVM()

	if isGrpcClientValue(vm.Nil) {
		t.Error("Nil should not be a GrpcClient value")
	}
	if isGrpcClientValue(vm.FromSmallInt(42)) {
		t.Error("SmallInt should not be a GrpcClient value")
	}
	if isGrpcClientValue(vmInst.Registry().NewStringValue("test")) {
		t.Error("String should not be a GrpcClient value")
	}
	if isGrpcClientValue(vm.True) {
		t.Error("True should not be a GrpcClient value")
	}
	if isGrpcClientValue(vm.False) {
		t.Error("False should not be a GrpcClient value")
	}
}

func TestGrpcClientMultipleRegistrations(t *testing.T) {
	vmInst := vm.NewVM()

	client1 := &GrpcClientObject{target: "host1:50051"}
	client2 := &GrpcClientObject{target: "host2:50051"}
	client3 := &GrpcClientObject{target: "host3:50051"}

	val1 := vmRegisterGrpcClient(vmInst, client1)
	val2 := vmRegisterGrpcClient(vmInst, client2)
	val3 := vmRegisterGrpcClient(vmInst, client3)

	// All should be distinct values
	if val1 == val2 || val2 == val3 || val1 == val3 {
		t.Error("each registration should produce a unique value")
	}

	// Each should resolve to the correct client
	if vmGetGrpcClient(vmInst, val1).target != "host1:50051" {
		t.Error("val1 should resolve to client1")
	}
	if vmGetGrpcClient(vmInst, val2).target != "host2:50051" {
		t.Error("val2 should resolve to client2")
	}
	if vmGetGrpcClient(vmInst, val3).target != "host3:50051" {
		t.Error("val3 should resolve to client3")
	}

	// Unregister one and check others still work
	vmUnregisterGrpcClient(vmInst, val2)
	if vmGetGrpcClient(vmInst, val2) != nil {
		t.Error("val2 should be nil after unregister")
	}
	if vmGetGrpcClient(vmInst, val1) == nil {
		t.Error("val1 should still be valid after unregistering val2")
	}
	if vmGetGrpcClient(vmInst, val3) == nil {
		t.Error("val3 should still be valid after unregistering val2")
	}

	// Cleanup
	vmUnregisterGrpcClient(vmInst, val1)
	vmUnregisterGrpcClient(vmInst, val3)
}

func TestUnregisterGrpcClientInvalidValue(t *testing.T) {
	vmInst := vm.NewVM()

	// Unregistering non-GrpcClient values should not panic
	vmUnregisterGrpcClient(vmInst, vm.Nil)
	vmUnregisterGrpcClient(vmInst, vm.FromSmallInt(42))
	vmUnregisterGrpcClient(vmInst, vmInst.Registry().NewStringValue("test"))
}

// ---------------------------------------------------------------------------
// GrpcStream Registry Tests
// ---------------------------------------------------------------------------

func TestGrpcStreamRegistration(t *testing.T) {
	vmInst := vm.NewVM()

	streamObj := &GrpcStreamObject{
		streamType: GrpcStreamServerStreaming,
	}

	val := vmRegisterGrpcStream(vmInst, streamObj)

	if !isGrpcStreamValue(val) {
		t.Fatal("vmRegisterGrpcStream should produce a GrpcStream value")
	}

	got := vmGetGrpcStream(vmInst, val)
	if got == nil {
		t.Fatal("vmGetGrpcStream returned nil for registered stream")
	}
	if got.streamType != GrpcStreamServerStreaming {
		t.Errorf("streamType = %v, want GrpcStreamServerStreaming", got.streamType)
	}

	// Unregister
	vmUnregisterGrpcStream(vmInst, val)

	got = vmGetGrpcStream(vmInst, val)
	if got != nil {
		t.Error("vmGetGrpcStream should return nil after unregister")
	}
}

func TestIsGrpcStreamValueFalse(t *testing.T) {
	vmInst := vm.NewVM()

	if isGrpcStreamValue(vm.Nil) {
		t.Error("Nil should not be a GrpcStream value")
	}
	if isGrpcStreamValue(vm.FromSmallInt(42)) {
		t.Error("SmallInt should not be a GrpcStream value")
	}
	if isGrpcStreamValue(vmInst.Registry().NewStringValue("test")) {
		t.Error("String should not be a GrpcStream value")
	}
}

func TestGrpcStreamTypes(t *testing.T) {
	vmInst := vm.NewVM()

	types := []GrpcStreamType{
		GrpcStreamServerStreaming,
		GrpcStreamClientStreaming,
		GrpcStreamBidirectional,
	}

	for _, st := range types {
		streamObj := &GrpcStreamObject{streamType: st}
		val := vmRegisterGrpcStream(vmInst, streamObj)

		got := vmGetGrpcStream(vmInst, val)
		if got.streamType != st {
			t.Errorf("streamType = %v, want %v", got.streamType, st)
		}

		vmUnregisterGrpcStream(vmInst, val)
	}
}

func TestGrpcStreamSendClosedFlag(t *testing.T) {
	vmInst := vm.NewVM()

	streamObj := &GrpcStreamObject{
		streamType: GrpcStreamServerStreaming,
	}

	val := vmRegisterGrpcStream(vmInst, streamObj)
	defer vmUnregisterGrpcStream(vmInst, val)

	got := vmGetGrpcStream(vmInst, val)
	if got.sendClosed.Load() {
		t.Error("sendClosed should be false initially")
	}

	got.sendClosed.Store(true)
	if !got.sendClosed.Load() {
		t.Error("sendClosed should be true after Store(true)")
	}
}

func TestGrpcStreamRecvClosedFlag(t *testing.T) {
	vmInst := vm.NewVM()

	streamObj := &GrpcStreamObject{
		streamType: GrpcStreamBidirectional,
	}

	val := vmRegisterGrpcStream(vmInst, streamObj)
	defer vmUnregisterGrpcStream(vmInst, val)

	got := vmGetGrpcStream(vmInst, val)
	if got.recvClosed.Load() {
		t.Error("recvClosed should be false initially")
	}

	got.recvClosed.Store(true)
	if !got.recvClosed.Load() {
		t.Error("recvClosed should be true after Store(true)")
	}
}

func TestUnregisterGrpcStreamInvalidValue(t *testing.T) {
	vmInst := vm.NewVM()

	// Should not panic
	vmUnregisterGrpcStream(vmInst, vm.Nil)
	vmUnregisterGrpcStream(vmInst, vm.FromSmallInt(42))
	vmUnregisterGrpcStream(vmInst, vmInst.Registry().NewStringValue("test"))
}

// ---------------------------------------------------------------------------
// GrpcClient/GrpcStream marker distinctness
// ---------------------------------------------------------------------------

func TestGrpcMarkersDistinct(t *testing.T) {
	vmInst := vm.NewVM()

	if vm.GrpcClientMarker == vm.GrpcStreamMarker {
		t.Error("client and stream markers should be distinct")
	}

	// A GrpcClient value should not be recognized as a GrpcStream value
	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vmRegisterGrpcClient(vmInst, clientObj)
	defer vmUnregisterGrpcClient(vmInst, clientVal)

	if isGrpcStreamValue(clientVal) {
		t.Error("GrpcClient value should not be identified as GrpcStream")
	}

	// A GrpcStream value should not be recognized as a GrpcClient value
	streamObj := &GrpcStreamObject{streamType: GrpcStreamBidirectional}
	streamVal := vmRegisterGrpcStream(vmInst, streamObj)
	defer vmUnregisterGrpcStream(vmInst, streamVal)

	if isGrpcClientValue(streamVal) {
		t.Error("GrpcStream value should not be identified as GrpcClient")
	}
}

// ---------------------------------------------------------------------------
// Result helper function tests (grpcSuccess / grpcFailure)
// ---------------------------------------------------------------------------

func TestGrpcSuccess(t *testing.T) {
	vmInst := vm.NewVM()

	inner := vm.FromSmallInt(42)
	result := grpcSuccess(vmInst, inner)

	// Check it is a success
	isSuccess := vmInst.Send(result, "isSuccess", nil)
	if isSuccess != vm.True {
		t.Error("grpcSuccess should create a Success result")
	}

	// Unwrap the value
	val := vmInst.Send(result, "value", nil)
	if !val.IsSmallInt() || val.SmallInt() != 42 {
		t.Errorf("grpcSuccess value = %v, want 42", val)
	}
}

func TestGrpcSuccessWithStringValue(t *testing.T) {
	vmInst := vm.NewVM()

	inner := vmInst.Registry().NewStringValue("connected")
	result := grpcSuccess(vmInst, inner)

	isSuccess := vmInst.Send(result, "isSuccess", nil)
	if isSuccess != vm.True {
		t.Error("grpcSuccess should create a Success result")
	}

	val := vmInst.Send(result, "value", nil)
	if !vm.IsStringValue(val) || vmInst.Registry().GetStringContent(val) != "connected" {
		t.Errorf("value = %q, want %q", vmInst.Registry().GetStringContent(val), "connected")
	}
}

func TestGrpcFailure(t *testing.T) {
	vmInst := vm.NewVM()

	result := grpcFailure(vmInst, "connection refused")

	// Check it is a failure
	isFailure := vmInst.Send(result, "isFailure", nil)
	if isFailure != vm.True {
		t.Error("grpcFailure should create a Failure result")
	}

	// Check error message
	errVal := vmInst.Send(result, "error", nil)
	if !vm.IsStringValue(errVal) {
		t.Fatal("grpcFailure error should be a string")
	}
	if vmInst.Registry().GetStringContent(errVal) != "connection refused" {
		t.Errorf("error = %q, want %q", vmInst.Registry().GetStringContent(errVal), "connection refused")
	}

	// value should be nil on failure
	val := vmInst.Send(result, "value", nil)
	if val != vm.Nil {
		t.Errorf("grpcFailure value should be Nil, got %v", val)
	}
}

func TestGrpcFailureVariousMessages(t *testing.T) {
	vmInst := vm.NewVM()

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
		result := grpcFailure(vmInst, msg)
		isFailure := vmInst.Send(result, "isFailure", nil)
		if isFailure != vm.True {
			t.Errorf("grpcFailure(%q) should be Failure", msg)
		}
		errVal := vmInst.Send(result, "error", nil)
		if vmInst.Registry().GetStringContent(errVal) != msg {
			t.Errorf("error = %q, want %q", vmInst.Registry().GetStringContent(errVal), msg)
		}
	}
}

// ---------------------------------------------------------------------------
// GrpcClient Primitive Tests (via vm.Send)
// ---------------------------------------------------------------------------

func TestGrpcGlobalsRegistered(t *testing.T) {
	vmInst := vm.NewVM()

	for _, name := range []string{"GrpcClient", "GrpcStream"} {
		if !vmInst.HasGlobal(name) {
			t.Errorf("global %q should be registered", name)
		}
	}
}

func TestGrpcClientClassAssignment(t *testing.T) {
	vmInst := vm.NewVM()

	clientObj := &GrpcClientObject{target: "test:50051"}
	clientVal := vmRegisterGrpcClient(vmInst, clientObj)
	defer vmUnregisterGrpcClient(vmInst, clientVal)

	class := vmInst.ClassFor(clientVal)
	if class == nil {
		t.Fatal("ClassFor(GrpcClient) returned nil")
	}
	if class.Name != "GrpcClient" {
		t.Errorf("ClassFor(GrpcClient).Name = %q, want %q", class.Name, "GrpcClient")
	}
}

func TestGrpcStreamClassAssignment(t *testing.T) {
	vmInst := vm.NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamBidirectional}
	streamVal := vmRegisterGrpcStream(vmInst, streamObj)
	defer vmUnregisterGrpcStream(vmInst, streamVal)

	class := vmInst.ClassFor(streamVal)
	if class == nil {
		t.Fatal("ClassFor(GrpcStream) returned nil")
	}
	if class.Name != "GrpcStream" {
		t.Errorf("ClassFor(GrpcStream).Name = %q, want %q", class.Name, "GrpcStream")
	}
}

func TestGrpcClientConnectToNonString(t *testing.T) {
	vmInst := vm.NewVM()

	grpcClientClassVal := vmInst.MustGlobal("GrpcClient")

	result := vmInst.Send(grpcClientClassVal, "connectTo:", []vm.Value{vm.FromSmallInt(12345)})

	isFailure := vmInst.Send(result, "isFailure", nil)
	if isFailure != vm.True {
		t.Error("connectTo: with non-string should be Failure")
	}

	errVal := vmInst.Send(result, "error", nil)
	if !vm.IsStringValue(errVal) {
		t.Fatal("error should be a string")
	}
	errMsg := vmInst.Registry().GetStringContent(errVal)
	if errMsg != "target must be a string" {
		t.Errorf("error = %q, want %q", errMsg, "target must be a string")
	}
}

func TestGrpcClientConnectToEmptyTarget(t *testing.T) {
	vmInst := vm.NewVM()

	grpcClientClassVal := vmInst.MustGlobal("GrpcClient")

	result := vmInst.Send(grpcClientClassVal, "connectTo:", []vm.Value{vmInst.Registry().NewStringValue("")})

	// Should be a Result (either success or failure depending on gRPC behavior)
	isSuccess := vmInst.Send(result, "isSuccess", nil)
	isFailure := vmInst.Send(result, "isFailure", nil)
	if isSuccess != vm.True && isFailure != vm.True {
		t.Fatal("connectTo: should return a Result")
	}
}

func TestGrpcClientConnectToTarget(t *testing.T) {
	vmInst := vm.NewVM()

	grpcClientClassVal := vmInst.MustGlobal("GrpcClient")

	result := vmInst.Send(grpcClientClassVal, "connectTo:", []vm.Value{vmInst.Registry().NewStringValue("localhost:50051")})

	// gRPC dial is lazy so this should be a success
	isSuccess := vmInst.Send(result, "isSuccess", nil)
	if isSuccess != vm.True {
		errVal := vmInst.Send(result, "error", nil)
		t.Fatalf("connectTo: failed unexpectedly: %q", vmInst.Registry().GetStringContent(errVal))
	}

	clientVal := vmInst.Send(result, "value", nil)
	if !isGrpcClientValue(clientVal) {
		t.Fatal("connectTo: success value should be a GrpcClient")
	}

	// Test isConnected
	isConnected := vmInst.Send(clientVal, "isConnected", nil)
	if isConnected != vm.True {
		t.Error("newly created client should be connected")
	}

	// Close the client
	vmInst.Send(clientVal, "close", nil)

	// After close, isConnected should be false
	isConnected = vmInst.Send(clientVal, "isConnected", nil)
	if isConnected != vm.False {
		t.Error("client should not be connected after close")
	}
}

func TestGrpcClientCloseIdempotent(t *testing.T) {
	vmInst := vm.NewVM()

	grpcClientClassVal := vmInst.MustGlobal("GrpcClient")

	result := vmInst.Send(grpcClientClassVal, "connectTo:", []vm.Value{vmInst.Registry().NewStringValue("localhost:50051")})
	clientVal := vmInst.Send(result, "value", nil)

	closeResult := vmInst.Send(clientVal, "close", nil)
	if closeResult != clientVal {
		t.Error("close should return receiver")
	}

	closeResult = vmInst.Send(clientVal, "close", nil)
	if closeResult != clientVal {
		t.Error("second close should return receiver")
	}
}

func TestGrpcClientIsConnectedOnNilClient(t *testing.T) {
	vmInst := vm.NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vmRegisterGrpcClient(vmInst, clientObj)
	vmUnregisterGrpcClient(vmInst, clientVal)

	isConnected := vmInst.Send(clientVal, "isConnected", nil)
	if isConnected != vm.False {
		t.Error("isConnected on unregistered client should return False")
	}
}

func TestGrpcClientCloseOnNilClient(t *testing.T) {
	vmInst := vm.NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vmRegisterGrpcClient(vmInst, clientObj)
	vmUnregisterGrpcClient(vmInst, clientVal)

	result := vmInst.Send(clientVal, "close", nil)
	if result != clientVal {
		t.Error("close on unregistered client should return receiver")
	}
}

func TestGrpcClientListServicesNoConnection(t *testing.T) {
	vmInst := vm.NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientObj.closed.Store(true)
	clientVal := vmRegisterGrpcClient(vmInst, clientObj)
	defer vmUnregisterGrpcClient(vmInst, clientVal)

	result := vmInst.Send(clientVal, "listServices", nil)
	if !result.IsObject() {
		t.Fatal("listServices should return an array")
	}

	size := vmInst.Send(result, "size", nil)
	if !size.IsSmallInt() || size.SmallInt() != 0 {
		t.Errorf("listServices on closed client should return empty array, got size %v", size)
	}
}

func TestGrpcClientMethodsForServiceNilClient(t *testing.T) {
	vmInst := vm.NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vmRegisterGrpcClient(vmInst, clientObj)
	vmUnregisterGrpcClient(vmInst, clientVal)

	result := vmInst.Send(clientVal, "methodsForService:", []vm.Value{vmInst.Registry().NewStringValue("test.Service")})
	if !result.IsObject() {
		t.Fatal("methodsForService: should return an array")
	}

	size := vmInst.Send(result, "size", nil)
	if !size.IsSmallInt() || size.SmallInt() != 0 {
		t.Errorf("methodsForService: on nil client should return empty array, got size %v", size)
	}
}

func TestGrpcClientMethodsForServiceNonString(t *testing.T) {
	vmInst := vm.NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vmRegisterGrpcClient(vmInst, clientObj)
	defer vmUnregisterGrpcClient(vmInst, clientVal)

	result := vmInst.Send(clientVal, "methodsForService:", []vm.Value{vm.FromSmallInt(42)})
	if !result.IsObject() {
		t.Fatal("methodsForService: should return an array")
	}

	size := vmInst.Send(result, "size", nil)
	if !size.IsSmallInt() || size.SmallInt() != 0 {
		t.Errorf("methodsForService: with non-string should return empty array, got size %v", size)
	}
}

func TestGrpcClientMethodDescriptorNilClient(t *testing.T) {
	vmInst := vm.NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vmRegisterGrpcClient(vmInst, clientObj)
	vmUnregisterGrpcClient(vmInst, clientVal)

	result := vmInst.Send(clientVal, "methodDescriptor:", []vm.Value{vmInst.Registry().NewStringValue("test.Service/Method")})
	if result != vm.Nil {
		t.Errorf("methodDescriptor: on nil client should return Nil, got %v", result)
	}
}

func TestGrpcClientMethodDescriptorNonString(t *testing.T) {
	vmInst := vm.NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vmRegisterGrpcClient(vmInst, clientObj)
	defer vmUnregisterGrpcClient(vmInst, clientVal)

	result := vmInst.Send(clientVal, "methodDescriptor:", []vm.Value{vm.FromSmallInt(42)})
	if result != vm.Nil {
		t.Errorf("methodDescriptor: with non-string should return Nil, got %v", result)
	}
}

func TestGrpcClientCallWithInvalidClient(t *testing.T) {
	vmInst := vm.NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vmRegisterGrpcClient(vmInst, clientObj)
	vmUnregisterGrpcClient(vmInst, clientVal)

	dict := vmInst.Registry().NewDictionaryValue()
	result := vmInst.Send(clientVal, "call:with:", []vm.Value{vmInst.Registry().NewStringValue("test/Method"), dict})

	isFailure := vmInst.Send(result, "isFailure", nil)
	if isFailure != vm.True {
		t.Error("call:with: on invalid client should return Failure")
	}
}

func TestGrpcClientCallWithNonStringMethod(t *testing.T) {
	vmInst := vm.NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vmRegisterGrpcClient(vmInst, clientObj)
	defer vmUnregisterGrpcClient(vmInst, clientVal)

	dict := vmInst.Registry().NewDictionaryValue()
	result := vmInst.Send(clientVal, "call:with:", []vm.Value{vm.FromSmallInt(42), dict})

	isFailure := vmInst.Send(result, "isFailure", nil)
	if isFailure != vm.True {
		t.Error("call:with: with non-string method should return Failure")
	}

	errVal := vmInst.Send(result, "error", nil)
	if vmInst.Registry().GetStringContent(errVal) != "method name must be a string" {
		t.Errorf("error = %q, want %q", vmInst.Registry().GetStringContent(errVal), "method name must be a string")
	}
}

func TestGrpcClientCallWithNonDictionaryRequest(t *testing.T) {
	vmInst := vm.NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vmRegisterGrpcClient(vmInst, clientObj)
	defer vmUnregisterGrpcClient(vmInst, clientVal)

	result := vmInst.Send(clientVal, "call:with:", []vm.Value{vmInst.Registry().NewStringValue("test/Method"), vm.FromSmallInt(42)})

	isFailure := vmInst.Send(result, "isFailure", nil)
	if isFailure != vm.True {
		t.Error("call:with: with non-dictionary request should return Failure")
	}

	errVal := vmInst.Send(result, "error", nil)
	if vmInst.Registry().GetStringContent(errVal) != "request must be a dictionary" {
		t.Errorf("error = %q, want %q", vmInst.Registry().GetStringContent(errVal), "request must be a dictionary")
	}
}

func TestGrpcClientServerStreamInvalidClient(t *testing.T) {
	vmInst := vm.NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vmRegisterGrpcClient(vmInst, clientObj)
	vmUnregisterGrpcClient(vmInst, clientVal)

	dict := vmInst.Registry().NewDictionaryValue()
	result := vmInst.Send(clientVal, "serverStream:with:", []vm.Value{vmInst.Registry().NewStringValue("test/Method"), dict})

	isFailure := vmInst.Send(result, "isFailure", nil)
	if isFailure != vm.True {
		t.Error("serverStream:with: on invalid client should return Failure")
	}
}

func TestGrpcClientServerStreamNonStringMethod(t *testing.T) {
	vmInst := vm.NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vmRegisterGrpcClient(vmInst, clientObj)
	defer vmUnregisterGrpcClient(vmInst, clientVal)

	dict := vmInst.Registry().NewDictionaryValue()
	result := vmInst.Send(clientVal, "serverStream:with:", []vm.Value{vm.FromSmallInt(42), dict})

	isFailure := vmInst.Send(result, "isFailure", nil)
	if isFailure != vm.True {
		t.Error("serverStream:with: with non-string method should return Failure")
	}
}

func TestGrpcClientServerStreamNonDictRequest(t *testing.T) {
	vmInst := vm.NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vmRegisterGrpcClient(vmInst, clientObj)
	defer vmUnregisterGrpcClient(vmInst, clientVal)

	result := vmInst.Send(clientVal, "serverStream:with:", []vm.Value{vmInst.Registry().NewStringValue("test/Method"), vm.FromSmallInt(42)})

	isFailure := vmInst.Send(result, "isFailure", nil)
	if isFailure != vm.True {
		t.Error("serverStream:with: with non-dict request should return Failure")
	}
}

func TestGrpcClientClientStreamInvalidClient(t *testing.T) {
	vmInst := vm.NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vmRegisterGrpcClient(vmInst, clientObj)
	vmUnregisterGrpcClient(vmInst, clientVal)

	result := vmInst.Send(clientVal, "clientStream:", []vm.Value{vmInst.Registry().NewStringValue("test/Method")})

	isFailure := vmInst.Send(result, "isFailure", nil)
	if isFailure != vm.True {
		t.Error("clientStream: on invalid client should return Failure")
	}
}

func TestGrpcClientClientStreamNonString(t *testing.T) {
	vmInst := vm.NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vmRegisterGrpcClient(vmInst, clientObj)
	defer vmUnregisterGrpcClient(vmInst, clientVal)

	result := vmInst.Send(clientVal, "clientStream:", []vm.Value{vm.FromSmallInt(42)})

	isFailure := vmInst.Send(result, "isFailure", nil)
	if isFailure != vm.True {
		t.Error("clientStream: with non-string should return Failure")
	}
}

func TestGrpcClientBidiStreamInvalidClient(t *testing.T) {
	vmInst := vm.NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vmRegisterGrpcClient(vmInst, clientObj)
	vmUnregisterGrpcClient(vmInst, clientVal)

	result := vmInst.Send(clientVal, "bidiStream:", []vm.Value{vmInst.Registry().NewStringValue("test/Method")})

	isFailure := vmInst.Send(result, "isFailure", nil)
	if isFailure != vm.True {
		t.Error("bidiStream: on invalid client should return Failure")
	}
}

func TestGrpcClientBidiStreamNonString(t *testing.T) {
	vmInst := vm.NewVM()

	clientObj := &GrpcClientObject{target: "test"}
	clientVal := vmRegisterGrpcClient(vmInst, clientObj)
	defer vmUnregisterGrpcClient(vmInst, clientVal)

	result := vmInst.Send(clientVal, "bidiStream:", []vm.Value{vm.FromSmallInt(42)})

	isFailure := vmInst.Send(result, "isFailure", nil)
	if isFailure != vm.True {
		t.Error("bidiStream: with non-string should return Failure")
	}
}

// ---------------------------------------------------------------------------
// GrpcStream Primitive Tests (via vm.Send)
// ---------------------------------------------------------------------------

func TestGrpcStreamHasNextOnNilStream(t *testing.T) {
	vmInst := vm.NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamServerStreaming}
	streamVal := vmRegisterGrpcStream(vmInst, streamObj)
	vmUnregisterGrpcStream(vmInst, streamVal)

	result := vmInst.Send(streamVal, "hasNext", nil)
	if result != vm.False {
		t.Error("hasNext on unregistered stream should return False")
	}
}

func TestGrpcStreamHasNextRecvClosed(t *testing.T) {
	vmInst := vm.NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamServerStreaming}
	streamObj.recvClosed.Store(true)
	streamVal := vmRegisterGrpcStream(vmInst, streamObj)
	defer vmUnregisterGrpcStream(vmInst, streamVal)

	result := vmInst.Send(streamVal, "hasNext", nil)
	if result != vm.False {
		t.Error("hasNext on recv-closed stream should return False")
	}
}

func TestGrpcStreamHasNextOpen(t *testing.T) {
	vmInst := vm.NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamServerStreaming}
	streamVal := vmRegisterGrpcStream(vmInst, streamObj)
	defer vmUnregisterGrpcStream(vmInst, streamVal)

	result := vmInst.Send(streamVal, "hasNext", nil)
	if result != vm.True {
		t.Error("hasNext on open stream should return True")
	}
}

func TestGrpcStreamCloseOnNilStream(t *testing.T) {
	vmInst := vm.NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamClientStreaming}
	streamVal := vmRegisterGrpcStream(vmInst, streamObj)
	vmUnregisterGrpcStream(vmInst, streamVal)

	result := vmInst.Send(streamVal, "close", nil)
	if result != streamVal {
		t.Error("close on unregistered stream should return receiver")
	}
}

func TestGrpcStreamSendOnNilStream(t *testing.T) {
	vmInst := vm.NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamClientStreaming}
	streamVal := vmRegisterGrpcStream(vmInst, streamObj)
	vmUnregisterGrpcStream(vmInst, streamVal)

	dict := vmInst.Registry().NewDictionaryValue()
	result := vmInst.Send(streamVal, "send:", []vm.Value{dict})

	isFailure := vmInst.Send(result, "isFailure", nil)
	if isFailure != vm.True {
		t.Error("send: on nil stream should return Failure")
	}
}

func TestGrpcStreamSendNonDictionary(t *testing.T) {
	vmInst := vm.NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamClientStreaming}
	streamVal := vmRegisterGrpcStream(vmInst, streamObj)
	defer vmUnregisterGrpcStream(vmInst, streamVal)

	result := vmInst.Send(streamVal, "send:", []vm.Value{vm.FromSmallInt(42)})

	isFailure := vmInst.Send(result, "isFailure", nil)
	if isFailure != vm.True {
		t.Error("send: with non-dict should return Failure")
	}

	errVal := vmInst.Send(result, "error", nil)
	if vmInst.Registry().GetStringContent(errVal) != "message must be a dictionary" {
		t.Errorf("error = %q, want %q", vmInst.Registry().GetStringContent(errVal), "message must be a dictionary")
	}
}

func TestGrpcStreamSendOnClosedSend(t *testing.T) {
	vmInst := vm.NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamClientStreaming}
	streamObj.sendClosed.Store(true)
	streamVal := vmRegisterGrpcStream(vmInst, streamObj)
	defer vmUnregisterGrpcStream(vmInst, streamVal)

	dict := vmInst.Registry().NewDictionaryValue()
	result := vmInst.Send(streamVal, "send:", []vm.Value{dict})

	isFailure := vmInst.Send(result, "isFailure", nil)
	if isFailure != vm.True {
		t.Error("send: on send-closed stream should return Failure")
	}

	errVal := vmInst.Send(result, "error", nil)
	if vmInst.Registry().GetStringContent(errVal) != "stream send closed" {
		t.Errorf("error = %q, want %q", vmInst.Registry().GetStringContent(errVal), "stream send closed")
	}
}

func TestGrpcStreamReceiveOnNilStream(t *testing.T) {
	vmInst := vm.NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamServerStreaming}
	streamVal := vmRegisterGrpcStream(vmInst, streamObj)
	vmUnregisterGrpcStream(vmInst, streamVal)

	result := vmInst.Send(streamVal, "receive", nil)

	isFailure := vmInst.Send(result, "isFailure", nil)
	if isFailure != vm.True {
		t.Error("receive on nil stream should return Failure")
	}
}

func TestGrpcStreamReceiveOnRecvClosed(t *testing.T) {
	vmInst := vm.NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamServerStreaming}
	streamObj.recvClosed.Store(true)
	streamVal := vmRegisterGrpcStream(vmInst, streamObj)
	defer vmUnregisterGrpcStream(vmInst, streamVal)

	result := vmInst.Send(streamVal, "receive", nil)

	isFailure := vmInst.Send(result, "isFailure", nil)
	if isFailure != vm.True {
		t.Error("receive on recv-closed stream should return Failure")
	}

	errVal := vmInst.Send(result, "error", nil)
	if vmInst.Registry().GetStringContent(errVal) != "end of stream" {
		t.Errorf("error = %q, want %q", vmInst.Registry().GetStringContent(errVal), "end of stream")
	}
}

func TestGrpcStreamCloseAndReceiveOnNilStream(t *testing.T) {
	vmInst := vm.NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamClientStreaming}
	streamVal := vmRegisterGrpcStream(vmInst, streamObj)
	vmUnregisterGrpcStream(vmInst, streamVal)

	result := vmInst.Send(streamVal, "closeAndReceive", nil)

	isFailure := vmInst.Send(result, "isFailure", nil)
	if isFailure != vm.True {
		t.Error("closeAndReceive on nil stream should return Failure")
	}
}

func TestGrpcStreamStreamTypeOnNilStream(t *testing.T) {
	vmInst := vm.NewVM()

	streamObj := &GrpcStreamObject{streamType: GrpcStreamServerStreaming}
	streamVal := vmRegisterGrpcStream(vmInst, streamObj)
	vmUnregisterGrpcStream(vmInst, streamVal)

	result := vmInst.Send(streamVal, "streamType", nil)
	if result != vm.Nil {
		t.Error("streamType on nil stream should return Nil")
	}
}

func TestGrpcStreamStreamTypeValues(t *testing.T) {
	vmInst := vm.NewVM()

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
		streamVal := vmRegisterGrpcStream(vmInst, streamObj)

		result := vmInst.Send(streamVal, "streamType", nil)
		if !result.IsSymbol() {
			t.Fatalf("streamType should return a symbol for %v", tc.stype)
		}

		name := vmInst.Symbols.Name(result.SymbolID())
		if name != tc.want {
			t.Errorf("streamType = %q, want %q", name, tc.want)
		}

		vmUnregisterGrpcStream(vmInst, streamVal)
	}
}

// ---------------------------------------------------------------------------
// resolveMethod helper tests
// ---------------------------------------------------------------------------

func TestResolveMethodInvalidFormat(t *testing.T) {
	client := &GrpcClientObject{target: "test"}

	_, err := resolveMethod(client, "no-slash-here")
	if err == nil {
		t.Error("resolveMethod should fail for method name without '/'")
	}

	_, err = resolveMethod(client, "a/b/c")
	if err == nil {
		t.Error("resolveMethod should fail for method name with multiple '/'")
	}

	_, err = resolveMethod(client, "")
	if err == nil {
		t.Error("resolveMethod should fail for empty method name")
	}
}

// ---------------------------------------------------------------------------
// Marker collision tests
// ---------------------------------------------------------------------------

func TestGrpcMarkersDontCollideWithOtherMarkers(t *testing.T) {
	// Ensure gRPC markers don't collide with each other or well-known VM markers
	if vm.GrpcClientMarker == vm.GrpcStreamMarker {
		t.Error("client and stream markers should be distinct")
	}

	// Also check against channel marker (a well-known VM marker)
	if vm.GrpcClientMarker == vm.ChannelMarkerValue() {
		t.Error("grpcClient marker should not collide with channel marker")
	}
	if vm.GrpcStreamMarker == vm.ChannelMarkerValue() {
		t.Error("grpcStream marker should not collide with channel marker")
	}
}
