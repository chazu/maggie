package vm

import (
	"runtime"
	"sync"
	"testing"
	"time"
)

// Regression tests for the distributed-runtime audit fixes (fresh-eyes audit,
// 2026-09). Each test names the bug it guards.

// Envelope nonces are checked per SENDER identity in one replay window, so
// every NodeRefData for the same local identity must draw from one monotonic
// source. Per-ref counters seeded from wall-clock nanos let a newer ref (a
// second connect:, the cluster core's ref) push the receiver's window past the
// older ref's counter, locking the older ref out ("nonce … below replay
// window").
func TestNodeRef_NoncesSharedAcrossRefsForSameIdentity(t *testing.T) {
	pub, priv := testKeys(t)
	older := NewNodeRefData("peer:1", pub, priv)
	time.Sleep(2 * time.Millisecond) // a later ref gets a later wall-clock seed
	newer := NewNodeRefData("peer:1", pub, priv)

	var last uint64
	for i := 0; i < 10; i++ {
		for _, ref := range []*NodeRefData{newer, older} {
			n := ref.NextNonce()
			if n <= last {
				t.Fatalf("nonce went backwards across refs of one identity: %d after %d", n, last)
			}
			last = n
		}
	}

	// A different identity has its own sequence (not required to interleave).
	pub2, priv2 := testKeys(t)
	other := NewNodeRefData("peer:1", pub2, priv2)
	if other.NextNonce() == 0 {
		t.Fatal("nonce for a fresh identity should be seeded, not zero")
	}
}

// asyncSend:with: on a ref whose connect: handshake failed (peer id unknown)
// must still accept the reply from the real peer. Registering the reply under
// peerKey() (our own id as fallback) made handleReply drop it forever.
func TestAsyncSend_UnknownPeerAcceptsRealPeerReply(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	pub, priv := testKeys(t)
	ref := NewNodeRefData("peer:1", pub, priv) // no PingFunc → peer id unknown
	sent := make(chan struct{}, 1)
	ref.SendFunc = func([]byte) ([]byte, string, string, error) {
		sent <- struct{}{}
		return nil, "", "", nil
	}
	nodeVal := vm.registerNodeRef(ref)
	rp := vm.createRemoteProcess(nodeVal, "worker")
	fv := vm.remoteSend(rp, vm.Symbols.SymbolValue("ping"), Nil, true)
	if !isFutureValue(fv) {
		t.Fatalf("asyncSend should answer a Future, got %v", fv)
	}
	<-sent

	var corr uint64
	vm.pendingReplies.mu.RLock()
	for id := range vm.pendingReplies.replies {
		corr = id
	}
	vm.pendingReplies.mu.RUnlock()
	if corr == 0 {
		t.Fatal("no pending reply registered")
	}

	realPeer := [32]byte{0x42}
	if f := vm.ResolvePendingReply(corr, realPeer); f == nil {
		t.Fatal("reply from the real peer was rejected: pending reply keyed on our own id")
	}
}

// asyncSend:with: must start heartbeat coverage for the peer (as forkOn: does)
// so a node death drains the pending reply instead of leaving await blocked.
func TestAsyncSend_NodeDeathResolvesPendingReply(t *testing.T) {
	for _, known := range []bool{true, false} {
		name := "peerKnown"
		if !known {
			name = "peerUnknown"
		}
		t.Run(name, func(t *testing.T) {
			vm := NewVM()
			defer vm.Shutdown()

			pub, priv := testKeys(t)
			vm.SetNodeIdentityKeys(pub, priv) // refs sign as the VM's identity
			ref := NewNodeRefData("peer:1", pub, priv)
			if known {
				ref.SetPeerID([32]byte{0x77})
			}
			ref.SendFunc = func([]byte) ([]byte, string, string, error) { return nil, "", "", nil }
			nodeVal := vm.registerNodeRef(ref)
			rp := vm.createRemoteProcess(nodeVal, "worker")
			fv := vm.remoteSend(rp, vm.Symbols.SymbolValue("ping"), Nil, true)
			f := (*FutureObject)(fv.ptr)

			vm.healthMonitorMu.Lock()
			hm := vm.healthMonitor
			vm.healthMonitorMu.Unlock()
			if hm == nil {
				t.Fatal("asyncSend did not start the node health monitor")
			}
			hm.mu.Lock()
			_, tracked := hm.nodes[ref.peerKey()]
			hm.mu.Unlock()
			if !tracked {
				t.Fatal("asyncSend did not track the peer for heartbeats")
			}

			// Simulate the health monitor declaring the node dead.
			vm.handleNodeDown(ref.peerKey())
			select {
			case <-f.Done():
			case <-time.After(2 * time.Second):
				t.Fatal("pending reply future not resolved on node death")
			}
			if f.Error() == "" {
				t.Fatal("future should resolve with a nodeDown error")
			}
		})
	}
}

// A second resolve must not overwrite the first result: the fields were
// written before publish's already-resolved check.
func TestFuture_SecondResolveDoesNotOverwrite(t *testing.T) {
	f := NewFuture()
	f.Resolve(FromSmallInt(7))
	f.ResolveError("late error")
	f.ResolveException(FromSmallInt(9), "late exception")
	f.Resolve(FromSmallInt(8))
	if got := f.Result(); !got.IsSmallInt() || got.SmallInt() != 7 {
		t.Fatalf("result overwritten: got %v, want 7", got)
	}
	if f.Error() != "" {
		t.Fatalf("error overwritten: %q", f.Error())
	}
	if f.ExceptionValue() != Nil && f.ExceptionValue() != (Value{}) {
		t.Fatalf("exception overwritten: %v", f.ExceptionValue())
	}
}

func TestFuture_ConcurrentResolveRace(t *testing.T) {
	for i := 0; i < 200; i++ {
		f := NewFuture()
		var wg sync.WaitGroup
		wg.Add(3)
		go func() { defer wg.Done(); f.Resolve(FromSmallInt(1)) }()
		go func() { defer wg.Done(); f.ResolveError("e") }()
		go func() { defer wg.Done(); _ = f.Result(); _ = f.Error() }()
		wg.Wait()
		chVal := <-f.GoChan()
		// Whichever won, the cached fields must agree with the published value.
		if f.Error() == "" {
			if f.Result() != chVal {
				t.Fatalf("result %v disagrees with published %v", f.Result(), chVal)
			}
		} else if f.Result() != Nil {
			t.Fatalf("error resolution left result %v", f.Result())
		}
	}
}

// Inbound monitors from two different watcher nodes that happen to pick the
// same refID (every node's allocator starts at 1) must both be kept and both
// get a DOWN; one peer must not be able to cancel the other's.
func TestInboundMonitor_RefIDCollisionAcrossNodes(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	proc := vm.createProcess()
	vm.registerProcess(proc)
	vm.RegisterProcessName("svc", proc.id)

	nodeA := [32]byte{0xA}
	nodeB := [32]byte{0xB}
	if dead, _ := vm.HandleInboundMonitor(1, 10, nodeA, "svc", 0); dead {
		t.Fatal("process should be alive")
	}
	if dead, _ := vm.HandleInboundMonitor(1, 20, nodeB, "svc", 0); dead {
		t.Fatal("process should be alive")
	}
	if got := vm.remoteWatches.InboundCount(); got != 2 {
		t.Fatalf("inbound count: got %d, want 2 (colliding refIDs overwrote)", got)
	}
	proc.mu.Lock()
	n := len(proc.remoteMonitors)
	proc.mu.Unlock()
	if n != 2 {
		t.Fatalf("proc.remoteMonitors: got %d, want 2", n)
	}

	// Node B cancelling refID 1 must not remove node A's monitor.
	vm.CancelInboundMonitor(1, nodeB)
	if got := vm.remoteWatches.InboundCount(); got != 1 {
		t.Fatalf("after B cancels: inbound count %d, want 1", got)
	}
	proc.mu.Lock()
	var remaining *RemoteMonitorRef
	for _, r := range proc.remoteMonitors {
		remaining = r
	}
	n = len(proc.remoteMonitors)
	proc.mu.Unlock()
	if n != 1 || remaining.RemoteNode != nodeA {
		t.Fatalf("after B cancels: proc should keep only A's monitor, got %d", n)
	}

	// Record DOWNs sent to watcher nodes.
	var mu sync.Mutex
	downs := map[[32]byte]int{}
	for _, node := range [][32]byte{nodeA, nodeB} {
		node := node
		pub, priv := testKeys(t)
		ref := NewNodeRefData("watcher:1", pub, priv)
		ref.SetPeerID(node)
		ref.SendFunc = func([]byte) ([]byte, string, string, error) {
			mu.Lock()
			downs[node]++
			mu.Unlock()
			return nil, "", "", nil
		}
		vm.registerNodeRef(ref)
	}
	vm.FinishProcess(proc, ExitNormal(Nil))
	deadline := time.Now().Add(2 * time.Second)
	for time.Now().Before(deadline) {
		mu.Lock()
		a := downs[nodeA]
		mu.Unlock()
		if a == 1 {
			break
		}
		time.Sleep(5 * time.Millisecond)
	}
	time.Sleep(20 * time.Millisecond)
	mu.Lock()
	defer mu.Unlock()
	if downs[nodeA] != 1 {
		t.Errorf("node A should get exactly one DOWN, got %d", downs[nodeA])
	}
	if downs[nodeB] != 0 {
		t.Errorf("node B cancelled its monitor but got %d DOWN(s)", downs[nodeB])
	}
	if got := vm.remoteWatches.InboundCount(); got != 0 {
		t.Errorf("inbound monitors should be cleared on exit, got %d", got)
	}
}

// Removing monitors must prune the per-node index; otherwise it grows forever
// for long-lived peers that monitor/demonitor repeatedly.
func TestRemoteWatchStore_IndexPrunedOnRemove(t *testing.T) {
	rws := NewRemoteWatchStore()
	node := [32]byte{7}
	for i := uint64(1); i <= 100; i++ {
		rws.AddOutboundMonitor(&RemoteMonitorRef{RefID: i, RemoteNode: node, Outbound: true})
		rws.AddInboundMonitor(&RemoteMonitorRef{RefID: i, RemoteNode: node})
		rws.RemoveOutboundMonitor(i)
		rws.RemoveInboundMonitorOwnedBy(i, node)
	}
	rws.mu.Lock()
	defer rws.mu.Unlock()
	if ns, ok := rws.byNode[node]; ok {
		t.Fatalf("per-node index not pruned: %d out, %d in", len(ns.outMonitorRefs), len(ns.inMonitorRefs))
	}
}

// Deserializing the same remote channel repeatedly must reuse one proxy, and
// the tracking set must not pin proxies that are no longer referenced.
func TestRemoteChannelProxy_DedupAndNoLeak(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	owner := [32]byte{0xCC}
	v1 := vm.registerRemoteChannel(&RemoteChannelRef{OwnerNode: owner, ChannelID: 5})
	v2 := vm.registerRemoteChannel(&RemoteChannelRef{OwnerNode: owner, ChannelID: 5})
	if vm.getRemoteChannel(v1) != vm.getRemoteChannel(v2) {
		t.Fatal("same (owner, channel) should dedupe to one proxy")
	}
	runtime.KeepAlive(v1)
	runtime.KeepAlive(v2)

	for i := 0; i < 1000; i++ {
		vm.registerRemoteChannel(&RemoteChannelRef{OwnerNode: owner, ChannelID: uint64(100 + i)})
	}
	for i := 0; i < 5; i++ {
		runtime.GC()
		time.Sleep(5 * time.Millisecond)
	}
	if n := vm.remoteChannels.trackedCount(); n > 100 {
		t.Fatalf("unreferenced proxies still tracked: %d", n)
	}

	// Node death still closes live proxies.
	live := vm.registerRemoteChannel(&RemoteChannelRef{OwnerNode: owner, ChannelID: 9})
	vm.DrainRemoteChannels(owner)
	if !vm.getRemoteChannel(live).IsClosed() {
		t.Fatal("drain should close a live proxy")
	}
	runtime.KeepAlive(live)
}

// A remotely closed + unexported channel must read as closed (nil / false),
// not signal "channel not found".
func TestRemoteChannel_SendOnRemotelyClosedAnswersNil(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	ref := &RemoteChannelRef{OwnerNode: [32]byte{1}, ChannelID: 3}
	ref.SendFunc = func(uint64, []byte) error { return ErrRemoteChannelClosed }
	val := vm.registerRemoteChannel(ref)
	got := vm.Send(val, "send:", []Value{FromSmallInt(1)})
	if got != Nil {
		t.Fatalf("send: on remotely closed channel: want nil, got %v", got)
	}
	if !ref.IsClosed() {
		t.Fatal("proxy should be marked closed")
	}
}
