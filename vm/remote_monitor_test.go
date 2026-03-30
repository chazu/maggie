package vm

import (
	"testing"
	"time"
)

// ---------------------------------------------------------------------------
// RemoteWatchStore tests
// ---------------------------------------------------------------------------

func TestRemoteWatchStore_OutboundMonitor(t *testing.T) {
	rws := NewRemoteWatchStore()
	ref := &RemoteMonitorRef{
		RefID:      1,
		WatcherID:  10,
		RemoteNode: [32]byte{1, 2, 3},
		Outbound:   true,
	}
	rws.AddOutboundMonitor(ref)

	if rws.OutboundCount() != 1 {
		t.Errorf("outbound count: got %d, want 1", rws.OutboundCount())
	}

	got := rws.RemoveOutboundMonitor(1)
	if got != ref {
		t.Error("should return the ref")
	}
	if rws.OutboundCount() != 0 {
		t.Error("should be empty after remove")
	}

	// Double remove returns nil
	if rws.RemoveOutboundMonitor(1) != nil {
		t.Error("double remove should return nil")
	}
}

func TestRemoteWatchStore_InboundMonitor(t *testing.T) {
	rws := NewRemoteWatchStore()
	ref := &RemoteMonitorRef{
		RefID:      2,
		WatcherID:  20,
		RemoteNode: [32]byte{4, 5, 6},
		Outbound:   false,
	}
	rws.AddInboundMonitor(ref)
	if rws.InboundCount() != 1 {
		t.Errorf("inbound count: got %d, want 1", rws.InboundCount())
	}
}

func TestRemoteWatchStore_DrainNode(t *testing.T) {
	rws := NewRemoteWatchStore()
	node := [32]byte{1, 2, 3}

	// Add 2 outbound + 1 inbound for the same node
	rws.AddOutboundMonitor(&RemoteMonitorRef{RefID: 1, WatcherID: 10, RemoteNode: node, Outbound: true})
	rws.AddOutboundMonitor(&RemoteMonitorRef{RefID: 2, WatcherID: 11, RemoteNode: node, Outbound: true})
	rws.AddInboundMonitor(&RemoteMonitorRef{RefID: 3, WatcherID: 30, RemoteNode: node, Outbound: false})

	outMonitors, links := rws.DrainNode(node)
	if len(outMonitors) != 2 {
		t.Errorf("drained outbound monitors: got %d, want 2", len(outMonitors))
	}
	if len(links) != 0 {
		t.Errorf("drained links: got %d, want 0", len(links))
	}

	// Store should be empty for this node
	if rws.OutboundCount() != 0 {
		t.Errorf("outbound count after drain: got %d, want 0", rws.OutboundCount())
	}
	if rws.InboundCount() != 0 {
		t.Errorf("inbound count after drain: got %d, want 0", rws.InboundCount())
	}

	// Double drain returns nothing
	out2, _ := rws.DrainNode(node)
	if len(out2) != 0 {
		t.Error("double drain should return nothing")
	}
}

// ---------------------------------------------------------------------------
// NodeHealthMonitor tests
// ---------------------------------------------------------------------------

func TestNodeHealthMonitor_DetectsNodeDown(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()
	vm.remoteWatches = NewRemoteWatchStore()

	nhm := NewNodeHealthMonitor(vm)
	nhm.interval = 50 * time.Millisecond
	nhm.maxMiss = 2

	nodeID := [32]byte{99}
	pub, priv := testKeys(t)
	ref := NewNodeRefData("dead-node:9090", pub, priv)

	// PingFunc always fails
	ref.PingFunc = func() bool { return false }

	nhm.Track(nodeID, ref)
	if nhm.TrackedCount() != 1 {
		t.Fatal("should be tracking 1 node")
	}

	// Add an outbound monitor so DrainNode has something to return
	vm.remoteWatches.AddOutboundMonitor(&RemoteMonitorRef{
		RefID:      42,
		WatcherID:  vm.MainProcessID(),
		RemoteNode: nodeID,
		Outbound:   true,
	})

	nhm.Start()
	defer nhm.Stop()

	// Wait for 2 missed pings (2 * 50ms + buffer)
	time.Sleep(200 * time.Millisecond)

	// The node should be untracked
	if nhm.TrackedCount() != 0 {
		t.Error("dead node should be untracked after missed heartbeats")
	}

	// The outbound monitor should be drained
	if vm.remoteWatches.OutboundCount() != 0 {
		t.Error("outbound monitors should be drained for dead node")
	}

	// The watcher should have received a nodeDown message
	mainProc := vm.GetProcessByID(vm.MainProcessID())
	msg, ok := mainProc.mailbox.TryReceive()
	if !ok {
		t.Fatal("watcher should receive nodeDown notification")
	}
	msgObj := ObjectFromValue(msg)
	if msgObj == nil {
		t.Fatal("notification should be a MailboxMessage")
	}
	// Check selector is processDown:
	selSlot := msgObj.GetSlot(1)
	if selSlot.IsSymbol() {
		name := vm.Symbols.Name(selSlot.SymbolID())
		if name != "processDown:" {
			t.Errorf("selector: got %q, want %q", name, "processDown:")
		}
	}
}

func TestNodeHealthMonitor_HealthyNodeNotDrained(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	nhm := NewNodeHealthMonitor(vm)
	nhm.interval = 50 * time.Millisecond
	nhm.maxMiss = 3

	nodeID := [32]byte{1}
	pub, priv := testKeys(t)
	ref := NewNodeRefData("healthy:9090", pub, priv)

	// PingFunc always succeeds
	ref.PingFunc = func() bool { return true }

	nhm.Track(nodeID, ref)
	nhm.Start()
	defer nhm.Stop()

	time.Sleep(200 * time.Millisecond)

	if nhm.TrackedCount() != 1 {
		t.Error("healthy node should still be tracked")
	}
}

// ---------------------------------------------------------------------------
// Cross-node monitor integration (local simulation)
// ---------------------------------------------------------------------------

func TestInboundMonitor_RegisterAndNotify(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	// Create a local process to be watched
	watched := vm.createProcess()
	vm.registerProcess(watched)
	vm.RegisterProcessName("watched-proc", watched.id)

	remoteNode := [32]byte{10, 20, 30}

	// Simulate inbound monitor request
	alreadyDead, _ := vm.HandleInboundMonitor(
		100,         // refID
		200,         // remote watcher ID
		remoteNode,  // remote node
		"watched-proc",
		0,
	)

	if alreadyDead {
		t.Fatal("process should not be dead")
	}

	// Check that the monitor was registered
	watched.mu.Lock()
	numRemote := len(watched.remoteMonitors)
	watched.mu.Unlock()
	if numRemote != 1 {
		t.Errorf("remoteMonitors: got %d, want 1", numRemote)
	}

	if vm.remoteWatches.InboundCount() != 1 {
		t.Errorf("inbound count: got %d, want 1", vm.remoteWatches.InboundCount())
	}
}

func TestInboundMonitor_AlreadyDead(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	watched := vm.createProcess()
	vm.registerProcess(watched)
	vm.RegisterProcessName("dead-proc", watched.id)

	// Kill it first
	vm.FinishProcess(watched, ExitError(errForTest("crashed")))

	// Now try to monitor
	alreadyDead, reason := vm.HandleInboundMonitor(
		101, 201, [32]byte{11}, "dead-proc", 0,
	)

	if !alreadyDead {
		t.Error("should report already dead")
	}
	if reason.Normal {
		t.Error("reason should not be normal (was an error)")
	}
}

func TestFinishProcess_NotifiesRemoteMonitors(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	watched := vm.createProcess()
	vm.registerProcess(watched)

	// Manually register a remote monitor (simulating inbound)
	watched.mu.Lock()
	watched.remoteMonitors = map[uint64]*RemoteMonitorRef{
		50: {RefID: 50, WatcherID: 300, RemoteNode: [32]byte{7, 8, 9}, Outbound: false},
	}
	watched.mu.Unlock()
	vm.remoteWatches.AddInboundMonitor(&RemoteMonitorRef{
		RefID: 50, WatcherID: 300, RemoteNode: [32]byte{7, 8, 9}, Outbound: false,
	})

	// Kill the watched process
	vm.FinishProcess(watched, ExitNormal(FromSmallInt(42)))

	// Remote monitors should be cleared
	watched.mu.Lock()
	numRemote := len(watched.remoteMonitors)
	watched.mu.Unlock()
	if numRemote != 0 {
		t.Errorf("remoteMonitors should be cleared, got %d", numRemote)
	}

	// Inbound should be cleaned from store
	if vm.remoteWatches.InboundCount() != 0 {
		t.Errorf("inbound count: got %d, want 0", vm.remoteWatches.InboundCount())
	}
}
