package vm

import (
	"sync"
)

// RemoteMonitorRef tracks a monitor relationship where either the watcher
// or the watched process is on a different node.
type RemoteMonitorRef struct {
	RefID       uint64   // monitor ref ID (allocated by watcher's node)
	WatcherID   uint64   // process ID on the watcher's node
	WatchedID   uint64   // process ID on the watched's node (0 if by-name)
	WatchedName string   // registered name on the watched's node
	RemoteNode  [32]byte // public key of the OTHER node
	Outbound    bool     // true = we are the watcher's node

	// Inbound only: the watched local process and the VM-unique key under
	// which this ref sits in its remoteMonitors map. RefIDs are chosen by the
	// watcher's node (every node's allocator starts at 1), so they are only
	// unique per remote node and cannot key a local map on their own.
	watched  *ProcessObject
	localKey uint64
}

// inboundKey identifies an inbound monitor: the watcher's node plus the refID
// that node allocated.
type inboundKey struct {
	node  [32]byte
	refID uint64
}

// RemoteWatchStore tracks all cross-node monitor relationships for this VM,
// keyed by remote node ID for fast bulk cleanup on node failure. (Cross-node
// process LINKS are not implemented — only local links and cross-node monitors
// exist — so there is no link state here.)
type RemoteWatchStore struct {
	mu sync.Mutex

	// Outbound monitors: we are watching a process on a remote node. Keyed by
	// our own (VM-unique) refID.
	outMonitors map[uint64]*RemoteMonitorRef

	// Inbound monitors: a remote node is watching one of our processes. Keyed
	// by (watcher node, refID) — refIDs alone collide across watcher nodes.
	inMonitors map[inboundKey]*RemoteMonitorRef

	// Index: remoteNode → refIDs for fast node-failure cleanup. Entries are
	// pruned on removal and the node's set is dropped once empty.
	byNode map[[32]byte]*nodeWatchSet

	nextLocalKey uint64 // allocator for RemoteMonitorRef.localKey
}

type nodeWatchSet struct {
	outMonitorRefs map[uint64]struct{}
	inMonitorRefs  map[uint64]struct{}
}

// NewRemoteWatchStore creates an empty remote watch store.
func NewRemoteWatchStore() *RemoteWatchStore {
	return &RemoteWatchStore{
		outMonitors: make(map[uint64]*RemoteMonitorRef),
		inMonitors:  make(map[inboundKey]*RemoteMonitorRef),
		byNode:      make(map[[32]byte]*nodeWatchSet),
	}
}

func (rws *RemoteWatchStore) nodeSet(node [32]byte) *nodeWatchSet {
	s, ok := rws.byNode[node]
	if !ok {
		s = &nodeWatchSet{
			outMonitorRefs: make(map[uint64]struct{}),
			inMonitorRefs:  make(map[uint64]struct{}),
		}
		rws.byNode[node] = s
	}
	return s
}

// unindex removes refID from node's index set and drops the set once empty.
// Caller holds mu.
func (rws *RemoteWatchStore) unindex(node [32]byte, refID uint64, outbound bool) {
	ns, ok := rws.byNode[node]
	if !ok {
		return
	}
	if outbound {
		delete(ns.outMonitorRefs, refID)
	} else {
		delete(ns.inMonitorRefs, refID)
	}
	if len(ns.outMonitorRefs) == 0 && len(ns.inMonitorRefs) == 0 {
		delete(rws.byNode, node)
	}
}

// AddOutboundMonitor records that we are monitoring a remote process.
func (rws *RemoteWatchStore) AddOutboundMonitor(ref *RemoteMonitorRef) {
	rws.mu.Lock()
	defer rws.mu.Unlock()
	if old, ok := rws.outMonitors[ref.RefID]; ok {
		rws.unindex(old.RemoteNode, old.RefID, true)
	}
	rws.outMonitors[ref.RefID] = ref
	rws.nodeSet(ref.RemoteNode).outMonitorRefs[ref.RefID] = struct{}{}
}

// RemoveOutboundMonitor removes an outbound monitor. Returns the ref or nil.
func (rws *RemoteWatchStore) RemoveOutboundMonitor(refID uint64) *RemoteMonitorRef {
	rws.mu.Lock()
	defer rws.mu.Unlock()
	ref, ok := rws.outMonitors[refID]
	if !ok {
		return nil
	}
	delete(rws.outMonitors, refID)
	rws.unindex(ref.RemoteNode, refID, true)
	return ref
}

// RemoveOutboundMonitorFrom removes an outbound monitor only if it watches a
// process on `node` — the signature-proven sender of a DOWN notification — so
// one peer cannot fire (and consume) a monitor we hold on another peer.
func (rws *RemoteWatchStore) RemoveOutboundMonitorFrom(refID uint64, node [32]byte) *RemoteMonitorRef {
	rws.mu.Lock()
	defer rws.mu.Unlock()
	ref, ok := rws.outMonitors[refID]
	if !ok || ref.RemoteNode != node {
		return nil
	}
	delete(rws.outMonitors, refID)
	rws.unindex(ref.RemoteNode, refID, true)
	return ref
}

// AddInboundMonitor records that a remote node is watching one of our
// processes. It assigns ref a VM-unique localKey and returns any ref it
// replaced (the same watcher node re-sending the same refID).
func (rws *RemoteWatchStore) AddInboundMonitor(ref *RemoteMonitorRef) (replaced *RemoteMonitorRef) {
	rws.mu.Lock()
	defer rws.mu.Unlock()
	rws.nextLocalKey++
	ref.localKey = rws.nextLocalKey
	k := inboundKey{node: ref.RemoteNode, refID: ref.RefID}
	replaced = rws.inMonitors[k]
	rws.inMonitors[k] = ref
	rws.nodeSet(ref.RemoteNode).inMonitorRefs[ref.RefID] = struct{}{}
	return replaced
}

// removeInboundRef removes the inbound monitor keyed by ref's (node, refID).
// Used when the watched process exits.
func (rws *RemoteWatchStore) removeInboundRef(ref *RemoteMonitorRef) {
	rws.mu.Lock()
	defer rws.mu.Unlock()
	k := inboundKey{node: ref.RemoteNode, refID: ref.RefID}
	if _, ok := rws.inMonitors[k]; ok {
		delete(rws.inMonitors, k)
		rws.unindex(ref.RemoteNode, ref.RefID, false)
	}
}

// RemoveInboundMonitorOwnedBy removes an inbound monitor only if it was
// established by `node` (the watcher's signature-proven identity). Returns the
// removed ref, or nil if the monitor doesn't exist or belongs to another peer —
// so one peer cannot cancel another peer's monitor by guessing ref IDs.
// It does not touch the watched process's remoteMonitors; use
// VM.CancelInboundMonitor to cancel a monitor completely.
func (rws *RemoteWatchStore) RemoveInboundMonitorOwnedBy(refID uint64, node [32]byte) *RemoteMonitorRef {
	rws.mu.Lock()
	defer rws.mu.Unlock()
	k := inboundKey{node: node, refID: refID}
	ref, ok := rws.inMonitors[k]
	if !ok {
		return nil
	}
	delete(rws.inMonitors, k)
	rws.unindex(node, refID, false)
	return ref
}

// DrainNode removes all monitors for a given remote node. Returns the outbound
// monitors for local DOWN delivery.
// Called by NodeHealthMonitor when a node is declared dead.
func (rws *RemoteWatchStore) DrainNode(node [32]byte) (outMonitors []*RemoteMonitorRef) {
	rws.mu.Lock()
	defer rws.mu.Unlock()

	ns, ok := rws.byNode[node]
	if !ok {
		return nil
	}

	for refID := range ns.outMonitorRefs {
		if ref, exists := rws.outMonitors[refID]; exists {
			outMonitors = append(outMonitors, ref)
			delete(rws.outMonitors, refID)
		}
	}
	for refID := range ns.inMonitorRefs {
		delete(rws.inMonitors, inboundKey{node: node, refID: refID})
	}

	delete(rws.byNode, node)
	return
}

// OutboundCount returns the number of outbound monitors (for testing).
func (rws *RemoteWatchStore) OutboundCount() int {
	rws.mu.Lock()
	defer rws.mu.Unlock()
	return len(rws.outMonitors)
}

// InboundCount returns the number of inbound monitors (for testing).
func (rws *RemoteWatchStore) InboundCount() int {
	rws.mu.Lock()
	defer rws.mu.Unlock()
	return len(rws.inMonitors)
}
