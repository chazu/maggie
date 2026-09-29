package dist

import (
	"sync"

	"github.com/chazu/maggie/vm"
)

// FailureDetector produces liveness transitions for the peers it is told to
// track. The membership core applies the events to its view and gossip spreads
// the result. This is the swap seam: the direct-heartbeat detector below ships
// first; a SWIM detector (probe + indirect probe + suspicion) implements the
// same interface later with no change to the core.
//
// A Phase 1 detector reports failures only (Dead). Positive liveness is inferred
// by the core from successful joins and inbound gossip, so a detector need not
// emit a steady stream of Alive events.
type FailureDetector interface {
	Track(peer NodeID, ref *vm.NodeRefData)
	Untrack(peer NodeID)
	// Events streams membership transitions. Closed by Stop.
	Events() <-chan MemberEvent
	Stop()
}

// DirectHeartbeatDetector wraps the VM's single NodeHealthMonitor: it heartbeats
// tracked peers and reports one Dead event per peer that misses enough pings.
// Reusing the shared monitor means membership heartbeating is unified with
// monitor/link tracking rather than a second ping loop.
type DirectHeartbeatDetector struct {
	hm     *vm.NodeHealthMonitor
	events chan MemberEvent

	// mu guards stopped and serializes onDown's send against Stop's close:
	// the down-observer stays registered on the VM-owned health monitor after
	// Stop, so a later node death must not send on the closed channel.
	mu      sync.Mutex
	stopped bool
}

// NewDirectHeartbeatDetector builds a detector over the VM's health monitor and
// subscribes to its down transitions.
func NewDirectHeartbeatDetector(v *vm.VM) *DirectHeartbeatDetector {
	d := &DirectHeartbeatDetector{
		hm:     v.HealthMonitor(),
		events: make(chan MemberEvent, 64),
	}
	d.hm.AddDownObserver(d.onDown)
	return d
}

func (d *DirectHeartbeatDetector) Track(peer NodeID, ref *vm.NodeRefData) {
	d.hm.Track(peer, ref)
}

func (d *DirectHeartbeatDetector) Untrack(peer NodeID) {
	d.hm.Untrack(peer)
}

func (d *DirectHeartbeatDetector) Events() <-chan MemberEvent { return d.events }

// onDown runs on the health monitor's tick goroutine; it must not block, so the
// event channel is buffered and a full channel drops the (redundant) event.
func (d *DirectHeartbeatDetector) onDown(nodeID [32]byte) {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.stopped {
		return
	}
	select {
	case d.events <- MemberEvent{Peer: NodeID(nodeID), Status: StatusDead}:
	default:
	}
}

// Stop closes the event channel (idempotent). The underlying health monitor is
// owned by the VM and keeps running for monitor/link use; its down-observer
// stays registered but becomes a no-op.
func (d *DirectHeartbeatDetector) Stop() {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.stopped {
		return
	}
	d.stopped = true
	close(d.events)
}
