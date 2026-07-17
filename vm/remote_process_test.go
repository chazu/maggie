package vm

import (
	"github.com/chazu/maggie/vm/wire"

	"crypto/ed25519"
	"crypto/rand"
	"fmt"
	"sync"
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

func TestFuture_AwaitIsReEntrant(t *testing.T) {
	// Regression: await did an unconditional <-f.ch, so a second await (or an
	// await after the ch was drained by a select/timeout) blocked forever, and
	// multiple concurrent waiters could not all wake. Awaiting the done channel
	// must be idempotent and broadcast.
	vm := NewVM()
	defer vm.Shutdown()

	f := NewFuture()
	futureVal := vm.registerFuture(f)
	f.Resolve(FromSmallInt(42))

	// Multiple sequential awaits must all return the value, not block.
	for i := 0; i < 3; i++ {
		done := make(chan Value, 1)
		go func() { done <- vm.Send(futureVal, "await", nil) }()
		select {
		case got := <-done:
			if got.SmallInt() != 42 {
				t.Fatalf("await %d: got %d, want 42", i, got.SmallInt())
			}
		case <-time.After(2 * time.Second):
			t.Fatalf("await %d blocked forever", i)
		}
	}

	// Many concurrent waiters must all wake.
	var wg sync.WaitGroup
	for i := 0; i < 8; i++ {
		wg.Add(1)
		go func() { defer wg.Done(); vm.Send(futureVal, "await", nil) }()
	}
	waited := make(chan struct{})
	go func() { wg.Wait(); close(waited) }()
	select {
	case <-waited:
	case <-time.After(2 * time.Second):
		t.Fatal("concurrent awaiters did not all wake")
	}
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

	// Track sends. Use a channel for cross-goroutine handoff so the test
	// observes the envelope under proper synchronization.
	sentCh := make(chan []byte, 1)
	ref.SendFunc = func(envelope []byte) ([]byte, string, string, error) {
		sentCh <- envelope
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

	select {
	case env := <-sentCh:
		if env == nil {
			t.Fatal("SendFunc received nil envelope")
		}
	case <-time.After(2 * time.Second):
		t.Fatal("SendFunc was not called within timeout")
	}
}

func TestRemoteProcess_AsyncSendWithFuture(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	pub, priv := testKeys(t)
	ref := NewNodeRefData("localhost:9090", pub, priv)

	// Simulate a responder: on delivery, decode the correlation id the sender
	// stamped into ReplyTo and resolve the pending reply Future — the same
	// effect the server's __reply__ handler produces when the remote process
	// calls `msg reply:`. Returns only a delivery ack, as a real peer would.
	ref.SendFunc = func(envelope []byte) ([]byte, string, string, error) {
		env, err := wire.Unmarshal(envelope)
		if err != nil || env.ReplyTo == nil {
			return nil, "", "", nil
		}
		go func() {
			if f := vm.ResolvePendingReply(env.ReplyTo.Correlation, [32]byte{}); f != nil {
				f.Resolve(FromSmallInt(99))
			}
		}()
		return nil, "", "", nil
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
	// SendFunc is nil — must SIGNAL a catchable error, not answer nil
	// (failure doctrine: nil never signals; SD-14)

	nodeVal := vm.registerNodeRef(ref)
	rpVal := vm.createRemoteProcess(nodeVal, "worker")

	defer func() {
		r := recover()
		if r == nil {
			t.Fatal("expected a signaled error when SendFunc is nil")
		}
		if _, ok := r.(SignaledException); !ok {
			t.Fatalf("expected SignaledException, got %T: %v", r, r)
		}
	}()
	sel := vm.Symbols.SymbolValue("work")
	vm.remoteSend(rpVal, sel, FromSmallInt(1), false)
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
	env, err := wire.Unmarshal(data)
	if err != nil {
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
