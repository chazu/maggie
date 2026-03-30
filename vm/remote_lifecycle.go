package vm

import (
	"encoding/binary"
	"fmt"
)

// Reserved DeliverMessage selectors for infrastructure notifications.
const (
	SelectorDown = "__down__"
	SelectorExit = "__exit__"
)

// MonitorResponse is the Go-side view of MonitorProcessResponse.
type MonitorResponse struct {
	Success      bool
	ErrorKind    string
	ErrorMessage string
	AlreadyDead  bool
	ExitSignal   string
	ExitNormal   bool
}

// LinkResponse is the Go-side view of LinkProcessResponse.
type LinkResponse struct {
	Success      bool
	ErrorKind    string
	ErrorMessage string
	AlreadyDead  bool
	ExitSignal   string
	ExitNormal   bool
	RemoteID     uint64
}

// ---------------------------------------------------------------------------
// Outbound: this VM wants to monitor a process on a remote node
// ---------------------------------------------------------------------------

// MonitorRemoteProcess sets up a cross-node monitor.
func (vm *VM) MonitorRemoteProcess(watcher *ProcessObject, nodeRef *NodeRefData, targetName string) (*MonitorRef, error) {
	refID := vm.registry.ConcurrencyRegistry.monitorRefID.Add(1)
	nodeID := nodeRef.NodeID()

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
		vm.deliverDownMessage(watcher, ref, reason)
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
			pid := uint64(procVal.SymbolID() & ^uint32(0xFF<<24))
			proc = vm.GetProcessByID(pid)
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
		RemoteNode: remoteNode,
		Outbound:   false,
	}

	proc.mu.Lock()
	dead := proc.state.Load() == int32(ProcessTerminated)
	exitR := proc.exitReason
	if !dead {
		if proc.remoteMonitors == nil {
			proc.remoteMonitors = make(map[uint64]*RemoteMonitorRef)
		}
		proc.remoteMonitors[refID] = rmRef
	}
	proc.mu.Unlock()

	if dead {
		return true, exitR
	}

	vm.remoteWatches.AddInboundMonitor(rmRef)
	return false, ExitReason{}
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
	outMonitors, links := vm.remoteWatches.DrainNode(nodeID)
	nodeDownReason := ExitSignal("nodeDown", Nil)

	for _, rm := range outMonitors {
		watcher := vm.GetProcessByID(rm.WatcherID)
		if watcher == nil || watcher.isDone() {
			continue
		}
		ref := &MonitorRef{ID: rm.RefID, Watcher: rm.WatcherID}
		vm.deliverDownMessage(watcher, ref, nodeDownReason)
	}

	for _, lr := range links {
		local := vm.GetProcessByID(lr.LocalID)
		if local == nil || local.isDone() {
			continue
		}
		vm.deliverExitSignal(local, 0, nodeDownReason)
	}
}

// ensureHealthMonitor lazily creates and starts the NodeHealthMonitor.
func (vm *VM) ensureHealthMonitor(nodeID [32]byte, ref *NodeRefData) {
	if vm.healthMonitor == nil {
		vm.healthMonitor = NewNodeHealthMonitor(vm)
		vm.healthMonitor.Start()
	}
	vm.healthMonitor.Track(nodeID, ref)
}

// ---------------------------------------------------------------------------
// Remote DOWN notification sending (when OUR process dies)
// ---------------------------------------------------------------------------

// sendRemoteDown sends a DOWN notification to a remote watcher node.
func (vm *VM) sendRemoteDown(rmRef *RemoteMonitorRef, reason ExitReason) {
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
	env := &envelopeData{
		SenderNode:    ref.NodeID(),
		TargetProcess: targetPID,
		Selector:      selector,
		Payload:       payload,
		Nonce:         ref.NextNonce(),
	}

	var sigBuf []byte
	sigBuf = append(sigBuf, payload...)
	var nonceBuf [8]byte
	binary.BigEndian.PutUint64(nonceBuf[:], env.Nonce)
	sigBuf = append(sigBuf, nonceBuf[:]...)
	var procBuf [8]byte
	binary.BigEndian.PutUint64(procBuf[:], env.TargetProcess)
	sigBuf = append(sigBuf, procBuf[:]...)
	env.Signature = ref.Sign(sigBuf)

	return cborSerialEncMode.Marshal(env)
}

// findNodeRefByID searches for a NodeRefData by its 32-byte public key.
func (vm *VM) findNodeRefByID(nodeID [32]byte) *NodeRefData {
	vm.nodeRefsMu.RLock()
	defer vm.nodeRefsMu.RUnlock()
	for _, ref := range vm.nodeRefs {
		if ref.NodeID() == nodeID {
			return ref
		}
	}
	return nil
}

// DeliverDownMessage is the exported version for server package access.
func (vm *VM) DeliverDownMessage(watcher *ProcessObject, ref *MonitorRef, reason ExitReason) {
	vm.deliverDownMessage(watcher, ref, reason)
}

// ExitReason returns the process's exit reason (exported for server access).
func (p *ProcessObject) ExitReason() ExitReason {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.exitReason
}
