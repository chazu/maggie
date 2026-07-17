package vm

import (
	"crypto/ed25519"
	"strings"
	"sync"
	"testing"
)

// ---------------------------------------------------------------------------
// Node-death cleanup (SD-6 / SD-11)
// ---------------------------------------------------------------------------

func testNodeRef(nodeID [32]byte) *NodeRefData {
	pub := make(ed25519.PublicKey, ed25519.PublicKeySize)
	copy(pub, nodeID[:])
	ref := NewNodeRefData("test:0", pub, nil)
	ref.PingFunc = func() bool { return true }
	return ref
}

func TestHandleNodeDown_ResolvesPendingSpawns(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	nodeA := [32]byte{0xAA}
	nodeB := [32]byte{0xBB}

	fA := NewFuture()
	fB := NewFuture()
	vm.pendingSpawns.register(fA, nodeA)
	idB := vm.pendingSpawns.register(fB, nodeB)

	vm.handleNodeDown(nodeA)

	if !fA.IsResolved() {
		t.Fatal("future against dead node should be resolved")
	}
	if !strings.Contains(fA.Error(), "nodeDown") {
		t.Errorf("future error should mention nodeDown: %q", fA.Error())
	}
	if fB.IsResolved() {
		t.Error("future against live node should still be pending")
	}
	if vm.pendingSpawns.resolve(idB) != fB {
		t.Error("live node's pending spawn should still be registered")
	}
}

func TestHandleNodeDown_ClosesRemoteChannels(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	nodeA := [32]byte{0xAA}
	nodeB := [32]byte{0xBB}

	refA := &RemoteChannelRef{OwnerNode: nodeA, ChannelID: 1}
	refB := &RemoteChannelRef{OwnerNode: nodeB, ChannelID: 2}
	vm.RegisterRemoteChannelValue(refA)
	vm.RegisterRemoteChannelValue(refB)

	vm.handleNodeDown(nodeA)

	if !refA.IsClosed() {
		t.Error("dead node's channel proxy should be closed")
	}
	if refB.IsClosed() {
		t.Error("live node's channel proxy should stay open")
	}
	// Drained proxies leave the tracking set (grow-only leak fix)
	vm.remoteChannels.mu.RLock()
	_, stillTracked := vm.remoteChannels.channels[refA]
	vm.remoteChannels.mu.RUnlock()
	if stillTracked {
		t.Error("drained proxy should be removed from the tracking set")
	}
}

func TestHandleNodeDown_RemovesNodeRefs(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	nodeA := [32]byte{0xAA}
	ref := testNodeRef(nodeA)
	vm.registerNodeRef(ref)

	if vm.findNodeRefByID(nodeA) != ref {
		t.Fatal("node ref should be registered")
	}

	vm.handleNodeDown(nodeA)

	if vm.findNodeRefByID(nodeA) != nil {
		t.Error("dead node's ref should be removed from the reverse-lookup index")
	}
}

func TestHandleNodeDown_UntracksHealthMonitor(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	nodeA := [32]byte{0xAA}
	vm.ensureHealthMonitor(nodeA, testNodeRef(nodeA))

	if vm.healthMonitor.TrackedCount() != 1 {
		t.Fatalf("tracked: got %d, want 1", vm.healthMonitor.TrackedCount())
	}

	vm.handleNodeDown(nodeA)

	if vm.healthMonitor.TrackedCount() != 0 {
		t.Errorf("tracked after node down: got %d, want 0", vm.healthMonitor.TrackedCount())
	}
}

// ensureHealthMonitor is called from interpreter goroutines and server
// handlers concurrently; lazy creation must not race or double-create.
func TestEnsureHealthMonitor_Concurrent(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	nodeA := [32]byte{0xAA}
	ref := testNodeRef(nodeA)

	var wg sync.WaitGroup
	for i := 0; i < 16; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			vm.ensureHealthMonitor(nodeA, ref)
		}()
	}
	wg.Wait()

	if vm.healthMonitor == nil {
		t.Fatal("health monitor should be created")
	}
	if vm.healthMonitor.TrackedCount() != 1 {
		t.Errorf("tracked: got %d, want 1", vm.healthMonitor.TrackedCount())
	}
}

func TestPendingSpawnRegistry_DrainNode(t *testing.T) {
	reg := newPendingSpawnRegistry()

	nodeA := [32]byte{0xAA}
	nodeB := [32]byte{0xBB}

	fA1 := NewFuture()
	fA2 := NewFuture()
	fB := NewFuture()
	reg.register(fA1, nodeA)
	reg.register(fA2, nodeA)
	idB := reg.register(fB, nodeB)

	drained := reg.drainNode(nodeA)
	if len(drained) != 2 {
		t.Fatalf("drained: got %d, want 2", len(drained))
	}
	if reg.drainNode(nodeA) != nil {
		t.Error("second drain should return nothing")
	}
	if reg.resolve(idB) != fB {
		t.Error("other node's spawn should survive the drain")
	}
}

// ---------------------------------------------------------------------------
// Channel export lifecycle (SD-11)
// ---------------------------------------------------------------------------

func TestCloseChannel_UnexportsWhenDrained(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	ch := createChannel(0)
	id := vm.ExportChannel(ch)
	if vm.LookupExportedChannel(id) != ch {
		t.Fatal("channel should be exported")
	}

	vm.CloseChannel(ch)

	if vm.LookupExportedChannel(id) != nil {
		t.Error("closed empty channel should be unexported")
	}
}

func TestCloseChannel_KeepsExportWhileBuffered(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	ch := createChannel(2)
	ch.SafeSend(FromSmallInt(1))
	id := vm.ExportChannel(ch)

	vm.CloseChannel(ch)

	if vm.LookupExportedChannel(id) != ch {
		t.Fatal("closed channel with buffered values must stay exported for draining")
	}

	// Remote drain: final receive observes closed+empty and unexports
	if _, ok := ch.Receive(); !ok {
		t.Fatal("buffered value should be receivable after close")
	}
	if _, ok := ch.Receive(); ok {
		t.Fatal("channel should be drained")
	}
	vm.UnexportChannel(ch)
	if vm.LookupExportedChannel(id) != nil {
		t.Error("drained channel should be unexported")
	}
}
