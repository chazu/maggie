package vm

import (
	"fmt"

	"github.com/chazu/maggie/vm/wire"
)

// Reserved DeliverMessage selectors for infrastructure notifications.
// SelectorDown is the wire selector for a monitor DOWN notification. (There is
// no cross-node exit/link selector — process links are local-only.)
const SelectorDown = "__down__"

// MonitorResponse is the Go-side view of MonitorProcessResponse.
type MonitorResponse struct {
	Success      bool
	ErrorKind    string
	ErrorMessage string
	AlreadyDead  bool
	ExitSignal   string
	ExitNormal   bool
}

// ---------------------------------------------------------------------------
// Outbound: this VM wants to monitor a process on a remote node
// ---------------------------------------------------------------------------

// MonitorRemoteProcess sets up a cross-node monitor.
func (vm *VM) MonitorRemoteProcess(watcher *ProcessObject, nodeRef *NodeRefData, targetName string) (*MonitorRef, error) {
	refID := vm.registry.ConcurrencyRegistry.AllocMonitorRefID()
	nodeID := nodeRef.peerKey()

	ref := &MonitorRef{
		ID:      refID,
		Watcher: watcher.id,
	}

	if nodeRef.MonitorFunc == nil {
		return nil, fmt.Errorf("node %s has no MonitorFunc", nodeRef.Addr)
	}

	resp, err := nodeRef.MonitorFunc(watcher.id, refID, targetName)
	if err != nil {
		return nil, err
	}
	if !resp.Success {
		return nil, fmt.Errorf("%s: %s", resp.ErrorKind, resp.ErrorMessage)
	}

	// Register on watcher
	watcher.mu.Lock()
	if watcher.myMonitors == nil {
		watcher.myMonitors = make(map[uint64]*MonitorRef)
	}
	watcher.myMonitors[ref.ID] = ref
	watcher.mu.Unlock()

	if resp.AlreadyDead {
		reason := ExitReason{
			Normal: resp.ExitNormal,
			Signal: resp.ExitSignal,
			Result: Nil,
		}
		vm.deliverDownMessage(watcher, ref, nil, reason)
		return ref, nil
	}

	// Record in remote watch store
	rmRef := &RemoteMonitorRef{
		RefID:       refID,
		WatcherID:   watcher.id,
		WatchedName: targetName,
		RemoteNode:  nodeID,
		Outbound:    true,
	}
	vm.remoteWatches.AddOutboundMonitor(rmRef)
	vm.ensureHealthMonitor(nodeID, nodeRef)

	return ref, nil
}

// DemonitorRemoteProcess cancels a cross-node monitor.
func (vm *VM) DemonitorRemoteProcess(ref *MonitorRef, nodeRef *NodeRefData) {
	if nodeRef.DemonitorFunc != nil {
		_ = nodeRef.DemonitorFunc(ref.ID) // best-effort
	}
	vm.remoteWatches.RemoveOutboundMonitor(ref.ID)
}

// ---------------------------------------------------------------------------
// Inbound: a remote node wants us to watch one of OUR processes
// ---------------------------------------------------------------------------

// HandleInboundMonitor is called when we receive a MonitorProcessRequest.
func (vm *VM) HandleInboundMonitor(refID, watcherID uint64, remoteNode [32]byte, targetName string, targetID uint64) (alreadyDead bool, reason ExitReason) {
	var proc *ProcessObject
	if targetName != "" {
		procVal := vm.LookupProcessName(targetName)
		if procVal != Nil {
			proc = vm.getProcess(procVal)
		}
	} else {
		proc = vm.GetProcessByID(targetID)
	}

	if proc == nil {
		return true, ExitSignal("noproc", Nil)
	}

	rmRef := &RemoteMonitorRef{
		RefID:      refID,
		WatcherID:  watcherID,
		WatchedID:  proc.id,
		RemoteNode: remoteNode,
		Outbound:   false,
		watched:    proc,
	}

	// Register in the store first: it assigns the VM-unique localKey that keys
	// proc.remoteMonitors (the watcher-chosen refID collides across nodes).
	if replaced := vm.remoteWatches.AddInboundMonitor(rmRef); replaced != nil {
		vm.dropFromWatched(replaced)
	}

	proc.mu.Lock()
	dead := proc.state.Load() == int32(ProcessTerminated)
	exitR := proc.exitReason
	if !dead {
		if proc.remoteMonitors == nil {
			proc.remoteMonitors = make(map[uint64]*RemoteMonitorRef)
		}
		proc.remoteMonitors[rmRef.localKey] = rmRef
	}
	proc.mu.Unlock()

	if dead {
		vm.remoteWatches.removeInboundRef(rmRef)
		return true, exitR
	}
	return false, ExitReason{}
}

// CancelInboundMonitor cancels the inbound monitor that `node` (the watcher's
// signature-proven identity) established under refID: it leaves both the watch
// store and the watched process's remoteMonitors, so no DOWN is sent later.
// A refID owned by a different node is left untouched.
func (vm *VM) CancelInboundMonitor(refID uint64, node [32]byte) {
	if rmRef := vm.remoteWatches.RemoveInboundMonitorOwnedBy(refID, node); rmRef != nil {
		vm.dropFromWatched(rmRef)
	}
}

// dropFromWatched removes an inbound monitor ref from its watched process's
// remoteMonitors map (if it is still the entry under its localKey).
func (vm *VM) dropFromWatched(rmRef *RemoteMonitorRef) {
	proc := rmRef.watched
	if proc == nil {
		return
	}
	proc.mu.Lock()
	if cur, ok := proc.remoteMonitors[rmRef.localKey]; ok && cur == rmRef {
		delete(proc.remoteMonitors, rmRef.localKey)
	}
	proc.mu.Unlock()
}

// RemoteWatches returns the VM's remote watch store (for server access).
func (vm *VM) RemoteWatches() *RemoteWatchStore {
	return vm.remoteWatches
}

// ---------------------------------------------------------------------------
// Node failure handling
// ---------------------------------------------------------------------------

// handleNodeDown is called by NodeHealthMonitor when a node is unreachable.
func (vm *VM) handleNodeDown(nodeID [32]byte) {
	outMonitors := vm.remoteWatches.DrainNode(nodeID)
	nodeDownReason := ExitSignal("nodeDown", Nil)

	for _, rm := range outMonitors {
		watcher := vm.GetProcessByID(rm.WatcherID)
		if watcher == nil || watcher.isDone() {
			continue
		}
		ref := &MonitorRef{ID: rm.RefID, Watcher: rm.WatcherID}
		vm.deliverDownMessage(watcher, ref, nil, nodeDownReason)
	}

	// Resolve pending forkOn: futures against the dead node — their results
	// can never arrive, and Future wait would block forever.
	for _, f := range vm.pendingSpawns.drainNode(nodeID) {
		f.ResolveError("nodeDown: remote node died before delivering spawn result")
	}

	// Same for asyncSend:with: request-response futures: no reply will arrive
	// from a dead node.
	for _, f := range vm.pendingReplies.drainNode(nodeID) {
		f.ResolveError("nodeDown: remote node died before replying")
	}
	// Requests to a peer whose id was never learned are registered with a zero
	// expected peer (any replier accepted) and heartbeated under peerKey()'s
	// fallback — our own id. When that fallback key dies, drain them too.
	if local, ok := vm.localNodeID(); ok && local == nodeID {
		for _, f := range vm.pendingReplies.drainNode([32]byte{}) {
			f.ResolveError("nodeDown: remote node died before replying")
		}
	}

	// Mark every remote-channel proxy owned by the dead node closed so
	// blocked/future channel operations fail instead of hanging.
	vm.DrainRemoteChannels(nodeID)

	// Drop the node's reverse-lookup entries; a reconnect registers a fresh ref.
	vm.nodeRefsMu.Lock()
	for ref := range vm.nodeRefs {
		if ref.peerKey() == nodeID {
			delete(vm.nodeRefs, ref)
		}
	}
	vm.nodeRefsMu.Unlock()

	// Stop heartbeating the dead node (no-op when invoked from the monitor's
	// own tick, which already removed it).
	vm.healthMonitorMu.Lock()
	if vm.healthMonitor != nil {
		vm.healthMonitor.Untrack(nodeID)
	}
	vm.healthMonitorMu.Unlock()
}

// ensureHealthMonitor lazily creates and starts the NodeHealthMonitor.
func (vm *VM) ensureHealthMonitor(nodeID [32]byte, ref *NodeRefData) {
	vm.healthMonitorMu.Lock()
	if vm.healthMonitor == nil {
		vm.healthMonitor = NewNodeHealthMonitor(vm)
		vm.healthMonitor.Start()
	}
	hm := vm.healthMonitor
	vm.healthMonitorMu.Unlock()
	hm.Track(nodeID, ref)
}

// ---------------------------------------------------------------------------
// Remote DOWN notification sending (when OUR process dies)
// ---------------------------------------------------------------------------

// sendRemoteDown sends a DOWN notification to a remote watcher node and
// removes the exact (node, refID) inbound monitor from the watch store.
func (vm *VM) sendRemoteDown(rmRef *RemoteMonitorRef, reason ExitReason) {
	vm.remoteWatches.removeInboundRef(rmRef)

	ref := vm.findNodeRefByID(rmRef.RemoteNode)
	if ref == nil || ref.SendFunc == nil {
		return
	}

	payload := vm.buildDownPayload(rmRef.RefID, reason)
	envelope, err := buildSignedEnvelopeForProcess(ref, rmRef.WatcherID, SelectorDown, payload)
	if err != nil {
		return
	}

	go ref.SendFunc(envelope)
}

type downPayloadCBOR struct {
	RefID  uint64 `cbor:"1,keyasint"`
	Signal string `cbor:"2,keyasint"`
	Normal bool   `cbor:"3,keyasint"`
}

func (vm *VM) buildDownPayload(refID uint64, reason ExitReason) []byte {
	p := downPayloadCBOR{
		RefID:  refID,
		Signal: reason.Signal,
		Normal: reason.Normal,
	}
	data, _ := cborSerialEncMode.Marshal(p)
	return data
}

func buildSignedEnvelopeForProcess(ref *NodeRefData, targetPID uint64, selector string, payload []byte) ([]byte, error) {
	env := &wire.Envelope{
		TargetProcess: targetPID,
		Selector:      selector,
		Payload:       payload,
		Nonce:         ref.NextNonce(),
	}
	if err := env.SignWith(ref.NodeID(), ref.Sign); err != nil {
		return nil, err
	}
	return env.Marshal()
}

// findNodeRefByID searches for a NodeRefData by its 32-byte public key.
func (vm *VM) findNodeRefByID(nodeID [32]byte) *NodeRefData {
	vm.nodeRefsMu.RLock()
	defer vm.nodeRefsMu.RUnlock()
	for ref := range vm.nodeRefs {
		if ref.peerKey() == nodeID {
			return ref
		}
	}
	return nil
}

// FindNodeRefByPublicKey finds a NodeRefData by its public key (NodeID).
// Exported for use by cmd/mag spawn result delivery.
func (vm *VM) FindNodeRefByPublicKey(nodeID [32]byte) *NodeRefData {
	return vm.findNodeRefByID(nodeID)
}

// DeliverDownMessage is the exported version for server package access.
// The watched process is remote, so the DOWN payload carries Nil for the
// process slot.
func (vm *VM) DeliverDownMessage(watcher *ProcessObject, ref *MonitorRef, reason ExitReason) {
	vm.deliverDownMessage(watcher, ref, nil, reason)
}

// ExitReason returns the process's exit reason (exported for server access).
func (p *ProcessObject) ExitReason() ExitReason {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.exitReason
}
