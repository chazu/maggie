package vm

import (
	"crypto/ed25519"
	"crypto/rand"
	"fmt"
	"testing"
	"time"

	"github.com/fxamacker/cbor/v2"
)

func cborUnmarshal(data []byte, v interface{}) error {
	return cbor.Unmarshal(data, v)
}

func testKeys(t *testing.T) (ed25519.PublicKey, ed25519.PrivateKey) {
	t.Helper()
	pub, priv, err := ed25519.GenerateKey(rand.Reader)
	if err != nil {
		t.Fatalf("keygen: %v", err)
	}
	return pub, priv
}

// ---------------------------------------------------------------------------
// Future tests
// ---------------------------------------------------------------------------

func TestFuture_ResolveAndAwait(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	f := NewFuture()
	futureVal := vm.registerFuture(f)

	go func() {
		time.Sleep(10 * time.Millisecond)
		f.Resolve(FromSmallInt(42))
	}()

	// Block on the channel
	<-f.GoChan()
	if !f.IsResolved() {
		t.Fatal("should be resolved")
	}
	if f.Result().SmallInt() != 42 {
		t.Errorf("result: got %d, want 42", f.Result().SmallInt())
	}
	_ = futureVal
}

func TestFuture_ResolveError(t *testing.T) {
	f := NewFuture()
	f.ResolveError("connection refused")

	<-f.GoChan()
	if !f.IsResolved() {
		t.Fatal("should be resolved")
	}
	if f.Error() != "connection refused" {
		t.Errorf("error: got %q, want %q", f.Error(), "connection refused")
	}
}

func TestFuture_Registry(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	f := NewFuture()
	val := vm.registerFuture(f)

	if !isFutureValue(val) {
		t.Fatal("should be a future value")
	}

	got := vm.getFuture(val)
	if got != f {
		t.Error("registry lookup should return same future")
	}
}

func TestFuture_NotResolvedYet(t *testing.T) {
	f := NewFuture()
	if f.IsResolved() {
		t.Error("should not be resolved initially")
	}
	if f.Result() != Nil {
		t.Error("result should be Nil when unresolved")
	}
}

// ---------------------------------------------------------------------------
// NodeRef tests
// ---------------------------------------------------------------------------

func TestNodeRef_CreateAndLookup(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	pub, priv := testKeys(t)
	ref := NewNodeRefData("localhost:8081", pub, priv)
	val := vm.registerNodeRef(ref)

	if !isNodeRefValue(val) {
		t.Fatal("should be a node ref value")
	}

	got := vm.getNodeRef(val)
	if got != ref {
		t.Error("lookup should return same ref")
	}
	if got.Addr != "localhost:8081" {
		t.Errorf("addr: got %q, want %q", got.Addr, "localhost:8081")
	}
}

func TestNodeRef_Nonce(t *testing.T) {
	pub, priv := testKeys(t)
	ref := NewNodeRefData("localhost:8081", pub, priv)

	n1 := ref.NextNonce()
	n2 := ref.NextNonce()
	n3 := ref.NextNonce()

	if n1 >= n2 || n2 >= n3 {
		t.Error("nonces should be monotonically increasing")
	}
}

func TestNodeRef_NodeID(t *testing.T) {
	pub, priv := testKeys(t)
	ref := NewNodeRefData("localhost:8081", pub, priv)

	nid := ref.NodeID()
	if nid == [32]byte{} {
		t.Error("NodeID should not be zero")
	}
	for i := 0; i < 32; i++ {
		if nid[i] != pub[i] {
			t.Errorf("NodeID[%d] should match public key", i)
			break
		}
	}
}

// ---------------------------------------------------------------------------
// RemoteProcess tests
// ---------------------------------------------------------------------------

func TestRemoteProcess_Create(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	pub, priv := testKeys(t)
	ref := NewNodeRefData("localhost:9090", pub, priv)
	nodeVal := vm.registerNodeRef(ref)

	rpVal := vm.createRemoteProcess(nodeVal, "worker-1")
	obj := ObjectFromValue(rpVal)
	if obj == nil {
		t.Fatal("RemoteProcess should be an object")
	}

	// Slot 0 = node ref
	if obj.GetSlot(0) != nodeVal {
		t.Error("slot 0 should be the node ref")
	}

	// Slot 1 = name
	name := vm.registry.GetStringContent(obj.GetSlot(1))
	if name != "worker-1" {
		t.Errorf("name: got %q, want %q", name, "worker-1")
	}
}

func TestRemoteProcess_CastWithSendFunc(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	pub, priv := testKeys(t)
	ref := NewNodeRefData("localhost:9090", pub, priv)

	// Track sends
	var sentEnvelope []byte
	ref.SendFunc = func(envelope []byte) ([]byte, string, string, error) {
		sentEnvelope = envelope
		return nil, "", "", nil
	}

	nodeVal := vm.registerNodeRef(ref)
	rpVal := vm.createRemoteProcess(nodeVal, "worker")

	sel := vm.Symbols.SymbolValue("doWork")
	payload := FromSmallInt(42)

	result := vm.remoteSend(rpVal, sel, payload, false)
	if result != True {
		t.Error("cast should return true")
	}

	// Wait for the background goroutine
	time.Sleep(50 * time.Millisecond)

	if sentEnvelope == nil {
		t.Fatal("SendFunc should have been called")
	}
}

func TestRemoteProcess_AsyncSendWithFuture(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	pub, priv := testKeys(t)
	ref := NewNodeRefData("localhost:9090", pub, priv)

	// Simulate successful response with payload
	ref.SendFunc = func(envelope []byte) ([]byte, string, string, error) {
		// Serialize response value
		respBytes, _ := vm.SerializeValue(FromSmallInt(99))
		return respBytes, "", "", nil
	}

	nodeVal := vm.registerNodeRef(ref)
	rpVal := vm.createRemoteProcess(nodeVal, "worker")

	sel := vm.Symbols.SymbolValue("compute")
	payload := FromSmallInt(10)

	futureVal := vm.remoteSend(rpVal, sel, payload, true)
	if !isFutureValue(futureVal) {
		t.Fatal("asyncSend should return a Future")
	}

	future := vm.getFuture(futureVal)
	// Wait for resolution
	<-future.GoChan()

	if !future.IsResolved() {
		t.Fatal("future should be resolved")
	}
	if future.Error() != "" {
		t.Fatalf("future error: %s", future.Error())
	}
	if future.Result().SmallInt() != 99 {
		t.Errorf("result: got %d, want 99", future.Result().SmallInt())
	}
}

func TestRemoteProcess_AsyncSendNetworkError(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	pub, priv := testKeys(t)
	ref := NewNodeRefData("localhost:9090", pub, priv)

	ref.SendFunc = func(envelope []byte) ([]byte, string, string, error) {
		return nil, "", "", fmt.Errorf("connection refused")
	}

	nodeVal := vm.registerNodeRef(ref)
	rpVal := vm.createRemoteProcess(nodeVal, "worker")

	sel := vm.Symbols.SymbolValue("compute")
	futureVal := vm.remoteSend(rpVal, sel, FromSmallInt(1), true)

	future := vm.getFuture(futureVal)
	<-future.GoChan()

	if future.Error() == "" {
		t.Fatal("should have error on network failure")
	}
	if future.Result() != Nil {
		t.Error("result should be Nil on error")
	}
}

func TestRemoteProcess_AsyncSendRemoteError(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	pub, priv := testKeys(t)
	ref := NewNodeRefData("localhost:9090", pub, priv)

	ref.SendFunc = func(envelope []byte) ([]byte, string, string, error) {
		return nil, "processNotFound", "no such process", nil
	}

	nodeVal := vm.registerNodeRef(ref)
	rpVal := vm.createRemoteProcess(nodeVal, "worker")

	sel := vm.Symbols.SymbolValue("work")
	futureVal := vm.remoteSend(rpVal, sel, FromSmallInt(1), true)

	future := vm.getFuture(futureVal)
	<-future.GoChan()

	if future.Error() != "processNotFound: no such process" {
		t.Errorf("error: got %q", future.Error())
	}
}

func TestRemoteProcess_NilSendFunc(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	pub, priv := testKeys(t)
	ref := NewNodeRefData("localhost:9090", pub, priv)
	// SendFunc is nil — should return Nil

	nodeVal := vm.registerNodeRef(ref)
	rpVal := vm.createRemoteProcess(nodeVal, "worker")

	sel := vm.Symbols.SymbolValue("work")
	result := vm.remoteSend(rpVal, sel, FromSmallInt(1), false)
	if result != Nil {
		t.Error("should return Nil when SendFunc is nil")
	}
}

// ---------------------------------------------------------------------------
// Envelope signing
// ---------------------------------------------------------------------------

func TestBuildSignedEnvelope(t *testing.T) {
	pub, priv := testKeys(t)
	ref := NewNodeRefData("localhost:8081", pub, priv)

	payload := []byte{0x18, 0x2a} // CBOR integer 42

	data, err := buildSignedEnvelope(ref, "worker", "doWork:", payload, true)
	if err != nil {
		t.Fatalf("buildSignedEnvelope: %v", err)
	}

	if len(data) == 0 {
		t.Fatal("envelope should not be empty")
	}

	// Should be parseable CBOR
	var env envelopeData
	if err := cborUnmarshal(data, &env); err != nil {
		t.Fatalf("unmarshal envelope: %v", err)
	}

	if env.TargetName != "worker" {
		t.Errorf("TargetName: got %q, want %q", env.TargetName, "worker")
	}
	if env.Selector != "doWork:" {
		t.Errorf("Selector: got %q, want %q", env.Selector, "doWork:")
	}
	if env.ReplyTo == nil {
		t.Fatal("ReplyTo should be set when wantReply=true")
	}
	if env.Nonce == 0 {
		t.Error("Nonce should be non-zero")
	}
	if len(env.Signature) != ed25519.SignatureSize {
		t.Errorf("Signature length: got %d, want %d", len(env.Signature), ed25519.SignatureSize)
	}
}
