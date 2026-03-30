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
}

// RemoteLinkRef tracks a link relationship across nodes.
type RemoteLinkRef struct {
	LocalID    uint64   // local process ID
	RemoteID   uint64   // remote process ID
	RemoteNode [32]byte // public key of the remote node
}

// RemoteWatchStore tracks all cross-node monitor and link relationships
// for this VM. Keyed by remote node ID for fast bulk cleanup on node failure.
type RemoteWatchStore struct {
	mu sync.Mutex

	// Outbound monitors: we are watching a process on a remote node.
	outMonitors map[uint64]*RemoteMonitorRef

	// Inbound monitors: a remote node is watching one of our processes.
	inMonitors map[uint64]*RemoteMonitorRef

	// Links: bidirectional cross-node links.
	links map[string]*RemoteLinkRef

	// Index: remoteNode → set of refIDs/linkKeys for fast node-failure cleanup.
	byNode map[[32]byte]*nodeWatchSet
}

type nodeWatchSet struct {
	outMonitorRefs []uint64
	inMonitorRefs  []uint64
	linkKeys       []string
}

// NewRemoteWatchStore creates an empty remote watch store.
func NewRemoteWatchStore() *RemoteWatchStore {
	return &RemoteWatchStore{
		outMonitors: make(map[uint64]*RemoteMonitorRef),
		inMonitors:  make(map[uint64]*RemoteMonitorRef),
		links:       make(map[string]*RemoteLinkRef),
		byNode:      make(map[[32]byte]*nodeWatchSet),
	}
}

func (rws *RemoteWatchStore) nodeSet(node [32]byte) *nodeWatchSet {
	s, ok := rws.byNode[node]
	if !ok {
		s = &nodeWatchSet{}
		rws.byNode[node] = s
	}
	return s
}

// AddOutboundMonitor records that we are monitoring a remote process.
func (rws *RemoteWatchStore) AddOutboundMonitor(ref *RemoteMonitorRef) {
	rws.mu.Lock()
	defer rws.mu.Unlock()
	rws.outMonitors[ref.RefID] = ref
	ns := rws.nodeSet(ref.RemoteNode)
	ns.outMonitorRefs = append(ns.outMonitorRefs, ref.RefID)
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
	return ref
}

// AddInboundMonitor records that a remote node is watching one of our processes.
func (rws *RemoteWatchStore) AddInboundMonitor(ref *RemoteMonitorRef) {
	rws.mu.Lock()
	defer rws.mu.Unlock()
	rws.inMonitors[ref.RefID] = ref
	ns := rws.nodeSet(ref.RemoteNode)
	ns.inMonitorRefs = append(ns.inMonitorRefs, ref.RefID)
}

// RemoveInboundMonitor removes an inbound monitor. Returns the ref or nil.
func (rws *RemoteWatchStore) RemoveInboundMonitor(refID uint64) *RemoteMonitorRef {
	rws.mu.Lock()
	defer rws.mu.Unlock()
	ref, ok := rws.inMonitors[refID]
	if !ok {
		return nil
	}
	delete(rws.inMonitors, refID)
	return ref
}

// AddLink records a cross-node link.
func (rws *RemoteWatchStore) AddLink(key string, ref *RemoteLinkRef) {
	rws.mu.Lock()
	defer rws.mu.Unlock()
	rws.links[key] = ref
	ns := rws.nodeSet(ref.RemoteNode)
	ns.linkKeys = append(ns.linkKeys, key)
}

// DrainNode removes all watches for a given remote node. Returns the
// outbound monitors and links for local notification delivery.
// Called by NodeHealthMonitor when a node is declared dead.
func (rws *RemoteWatchStore) DrainNode(node [32]byte) (outMonitors []*RemoteMonitorRef, links []*RemoteLinkRef) {
	rws.mu.Lock()
	defer rws.mu.Unlock()

	ns, ok := rws.byNode[node]
	if !ok {
		return nil, nil
	}

	for _, refID := range ns.outMonitorRefs {
		if ref, exists := rws.outMonitors[refID]; exists {
			outMonitors = append(outMonitors, ref)
			delete(rws.outMonitors, refID)
		}
	}
	for _, refID := range ns.inMonitorRefs {
		delete(rws.inMonitors, refID)
	}
	for _, key := range ns.linkKeys {
		if ref, exists := rws.links[key]; exists {
			links = append(links, ref)
			delete(rws.links, key)
		}
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
