package vm

import (
	"sync"
	"time"
)

const (
	// DefaultHeartbeatInterval is how often we ping remote nodes.
	DefaultHeartbeatInterval = 5 * time.Second
	// DefaultHeartbeatMaxMiss is how many missed pings before declaring dead.
	DefaultHeartbeatMaxMiss = 3
)

// NodeHealthMonitor sends periodic heartbeats to remote nodes that have
// active monitors or links, and synthesizes failure notifications when
// a node becomes unreachable.
type NodeHealthMonitor struct {
	vm       *VM
	interval time.Duration
	maxMiss  int

	mu            sync.Mutex
	nodes         map[[32]byte]*trackedNode
	stopCh        chan struct{}
	stopped       bool
	downObservers []func([32]byte) // notified (in addition to handleNodeDown) when a node dies
}

type trackedNode struct {
	nodeID   [32]byte
	pingFunc func() bool
	sendFunc func(envelope []byte) ([]byte, string, string, error)
	misses   int
	lastSeen time.Time
}

// NewNodeHealthMonitor creates a new health monitor.
func NewNodeHealthMonitor(v *VM) *NodeHealthMonitor {
	return &NodeHealthMonitor{
		vm:       v,
		interval: DefaultHeartbeatInterval,
		maxMiss:  DefaultHeartbeatMaxMiss,
		nodes:    make(map[[32]byte]*trackedNode),
		stopCh:   make(chan struct{}),
	}
}

// Track begins monitoring a remote node. Idempotent.
func (nhm *NodeHealthMonitor) Track(nodeID [32]byte, ref *NodeRefData) {
	nhm.mu.Lock()
	defer nhm.mu.Unlock()
	if _, ok := nhm.nodes[nodeID]; ok {
		return
	}
	nhm.nodes[nodeID] = &trackedNode{
		nodeID:   nodeID,
		pingFunc: ref.PingFunc,
		sendFunc: ref.SendFunc,
		lastSeen: time.Now(),
	}
}

// AddDownObserver registers a callback fired when a tracked node is declared
// dead — in addition to the VM's own handleNodeDown cleanup. The membership
// layer uses this to surface a Dead MemberEvent without the health monitor
// needing to know about it. Observers run on the monitor's tick goroutine and
// must not block.
func (nhm *NodeHealthMonitor) AddDownObserver(fn func([32]byte)) {
	nhm.mu.Lock()
	nhm.downObservers = append(nhm.downObservers, fn)
	nhm.mu.Unlock()
}

// Untrack stops monitoring a remote node.
func (nhm *NodeHealthMonitor) Untrack(nodeID [32]byte) {
	nhm.mu.Lock()
	defer nhm.mu.Unlock()
	delete(nhm.nodes, nodeID)
}

// TrackedCount returns the number of tracked nodes (for testing).
func (nhm *NodeHealthMonitor) TrackedCount() int {
	nhm.mu.Lock()
	defer nhm.mu.Unlock()
	return len(nhm.nodes)
}

// Start begins the heartbeat loop in a goroutine.
func (nhm *NodeHealthMonitor) Start() {
	nhm.mu.Lock()
	if nhm.stopped {
		nhm.mu.Unlock()
		return
	}
	nhm.mu.Unlock()
	go nhm.loop()
}

// Stop terminates the heartbeat loop.
func (nhm *NodeHealthMonitor) Stop() {
	nhm.mu.Lock()
	defer nhm.mu.Unlock()
	if !nhm.stopped {
		nhm.stopped = true
		close(nhm.stopCh)
	}
}

func (nhm *NodeHealthMonitor) loop() {
	ticker := time.NewTicker(nhm.interval)
	defer ticker.Stop()

	for {
		select {
		case <-nhm.stopCh:
			return
		case <-ticker.C:
			nhm.tick()
		}
	}
}

func (nhm *NodeHealthMonitor) tick() {
	nhm.mu.Lock()
	snapshot := make([]*trackedNode, 0, len(nhm.nodes))
	for _, tn := range nhm.nodes {
		snapshot = append(snapshot, tn)
	}
	nhm.mu.Unlock()

	var deadNodes [][32]byte

	for _, tn := range snapshot {
		if tn.pingFunc == nil || !tn.pingFunc() {
			tn.misses++
			if tn.misses >= nhm.maxMiss {
				deadNodes = append(deadNodes, tn.nodeID)
			}
		} else {
			tn.misses = 0
			tn.lastSeen = time.Now()
		}
	}

	for _, nodeID := range deadNodes {
		nhm.mu.Lock()
		delete(nhm.nodes, nodeID)
		observers := make([]func([32]byte), len(nhm.downObservers))
		copy(observers, nhm.downObservers)
		nhm.mu.Unlock()

		nhm.vm.handleNodeDown(nodeID)
		for _, obs := range observers {
			obs(nodeID)
		}
	}
}
