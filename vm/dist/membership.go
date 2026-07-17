package dist

import (
	"context"
	"crypto/ed25519"
	"math/rand"
	"net"
	"sync"
	"time"

	"github.com/chazu/maggie/vm"
)

// MemberStatus is a peer's liveness in the membership view. Ordering matters for
// same-incarnation merges: a failure is sticky within a generation, so a Dead
// report must not be overwritten by a stale Alive of the same incarnation.
type MemberStatus uint8

const (
	StatusAlive MemberStatus = iota
	StatusSuspect
	StatusDead
	StatusLeft
)

// statusRank orders statuses for same-incarnation conflict resolution
// (Left/Dead beat Suspect beat Alive).
func statusRank(s MemberStatus) int {
	switch s {
	case StatusAlive:
		return 0
	case StatusSuspect:
		return 1
	case StatusDead:
		return 2
	case StatusLeft:
		return 3
	}
	return 0
}

// MemberRecord is one peer's entry in the membership view and the unit of
// gossip. Metadata is an OPEN map: new keys never bump wire.Version, and a node
// forwards metadata keys it does not understand verbatim so labels propagate
// through older nodes. The whole record (including metadata) is replaced
// atomically when the owning node bumps Incarnation.
type MemberRecord struct {
	Peer        NodeID            `cbor:"1,keyasint"`
	Addr        string            `cbor:"2,keyasint"`
	Status      MemberStatus      `cbor:"3,keyasint"`
	Incarnation uint64            `cbor:"4,keyasint"`
	Metadata    map[string]string `cbor:"5,keyasint,omitempty"`
	// Sig is peer X's Ed25519 signature over its own record (all fields above).
	// Because NodeID == the peer's public key, a receiver verifies a record about
	// X against X itself — so a third party cannot forge X's address, metadata, or
	// liveness. Only self-asserted states (Alive/Left) are signed and gossiped;
	// Dead is decided locally by the failure detector and never travels on the wire.
	Sig []byte `cbor:"6,keyasint,omitempty"`
}

// recordSigningBytes is the canonical, deterministic encoding of a record's
// signed fields (everything except Sig). Canonical CBOR sorts metadata keys so
// signer and verifier agree byte-for-byte.
func recordSigningBytes(r MemberRecord) ([]byte, error) {
	payload := struct {
		Peer        NodeID            `cbor:"1,keyasint"`
		Addr        string            `cbor:"2,keyasint"`
		Status      MemberStatus      `cbor:"3,keyasint"`
		Incarnation uint64            `cbor:"4,keyasint"`
		Metadata    map[string]string `cbor:"5,keyasint,omitempty"`
	}{r.Peer, r.Addr, r.Status, r.Incarnation, r.Metadata}
	return cborEncMode.Marshal(payload)
}

// SignMemberRecord self-signs r in place with priv, whose public half must equal
// r.Peer. For nodes or tests that hold the raw private key directly rather than
// going through the VM's node identity.
func SignMemberRecord(r *MemberRecord, priv ed25519.PrivateKey) error {
	msg, err := recordSigningBytes(*r)
	if err != nil {
		return err
	}
	r.Sig = ed25519.Sign(priv, msg)
	return nil
}

// verifyRecordSig checks that r is self-signed by its subject peer (NodeID is the
// Ed25519 public key). A record failing this is unauthenticated and dropped.
func verifyRecordSig(r MemberRecord) bool {
	if len(r.Sig) != ed25519.SignatureSize {
		return false
	}
	msg, err := recordSigningBytes(r)
	if err != nil {
		return false
	}
	return ed25519.Verify(ed25519.PublicKey(r.Peer[:]), msg, r.Sig)
}

// MemberEventKind classifies a membership transition for subscribers (the
// cmd/mag adapter maps it to a Maggie selector).
type MemberEventKind uint8

const (
	EventUp         MemberEventKind = iota // a member is Alive in our view (connected or self-connected)
	EventDown                              // a member was declared Dead by the local detector
	EventDiscovered                        // a gossip-learned member we are NOT connected to (join candidate)
)

// MemberEvent is a membership transition delivered to the failure detector's
// stream and to subscribers. Detectors set only Peer+Status; the core fills
// Kind+Record before fanning out to subscribers.
type MemberEvent struct {
	Peer   NodeID
	Status MemberStatus
	Kind   MemberEventKind
	Record MemberRecord
}

// JoinPolicy governs whether a peer LEARNED VIA GOSSIP (which carries an id and
// address) is auto-connected. Discovery seeds are always connected — that is how
// a node learns peer ids to begin with; the policy applies to the peers gossip
// then reveals.
type JoinPolicy uint8

const (
	JoinEager  JoinPolicy = iota // connect to every gossiped peer
	JoinLazy                     // record gossiped peers, connect on demand (Phase 2)
	JoinManual                   // record only; never auto-connect
)

// MembershipConfig tunes the core. Zero values fall back to sensible defaults.
type MembershipConfig struct {
	GossipInterval   time.Duration
	GossipFanout     int
	JoinPolicy       JoinPolicy
	SelfMetadata     map[string]string
	MaxMetadataBytes int
	MaxMetadataKeys  int
}

func (c MembershipConfig) withDefaults() MembershipConfig {
	if c.GossipInterval <= 0 {
		c.GossipInterval = time.Second
	}
	if c.GossipFanout <= 0 {
		c.GossipFanout = 3
	}
	if c.MaxMetadataBytes <= 0 {
		c.MaxMetadataBytes = 4096
	}
	if c.MaxMetadataKeys <= 0 {
		c.MaxMetadataKeys = 32
	}
	return c
}

// Membership is the Go core: it holds the authoritative view, drives discovery
// and the failure detector, and disseminates the view by gossip. It never
// grants trust — a discovered/gossiped peer stays at the TrustStore's default
// perms until explicitly configured.
type Membership struct {
	vm       *vm.VM
	trust    *TrustStore
	self     NodeID
	selfAddr string
	cfg      MembershipConfig

	mu              sync.RWMutex
	view            map[NodeID]*MemberRecord
	conns           map[NodeID]*vm.NodeRefData
	selfIncarnation uint64
	selfMetadata    map[string]string
	subscribers     []func(MemberEvent)

	detector    FailureDetector
	discoveries []Discovery

	rng     *rand.Rand
	stopCh  chan struct{}
	stopped bool
}

// NewMembership creates a membership core. self/selfAddr identify this node;
// detector is the (pluggable) failure detector; cfg tunes gossip and join policy.
func NewMembership(v *vm.VM, trust *TrustStore, self NodeID, selfAddr string, detector FailureDetector, cfg MembershipConfig) *Membership {
	cfg = cfg.withDefaults()
	m := &Membership{
		vm:           v,
		trust:        trust,
		self:         self,
		selfAddr:     selfAddr,
		cfg:          cfg,
		view:         make(map[NodeID]*MemberRecord),
		conns:        make(map[NodeID]*vm.NodeRefData),
		selfMetadata: cfg.SelfMetadata,
		detector:     detector,
		rng:          rand.New(rand.NewSource(time.Now().UnixNano())),
		stopCh:       make(chan struct{}),
	}
	return m
}

// AddDiscovery registers a discovery provider. Call before Start.
func (m *Membership) AddDiscovery(d Discovery) {
	m.discoveries = append(m.discoveries, d)
}

// Subscribe registers a membership-event callback (the seam Cluster.mag wires in
// Phase 2). Callbacks run outside the core lock and must not block for long.
func (m *Membership) Subscribe(fn func(MemberEvent)) {
	m.mu.Lock()
	m.subscribers = append(m.subscribers, fn)
	m.mu.Unlock()
}

// Start launches the discovery, detector, and gossip loops. Cancelling ctx (or
// calling Stop) tears them down.
func (m *Membership) Start(ctx context.Context) {
	for _, d := range m.discoveries {
		ch, err := d.Discover(ctx)
		if err != nil {
			continue
		}
		go m.discoveryLoop(ctx, ch)
	}
	go m.detectorLoop(ctx)
	go m.gossipLoop(ctx)
}

// Stop tears down the core and its detector.
func (m *Membership) Stop() {
	m.mu.Lock()
	if m.stopped {
		m.mu.Unlock()
		return
	}
	m.stopped = true
	close(m.stopCh)
	m.mu.Unlock()
	m.detector.Stop()
}

func (m *Membership) discoveryLoop(ctx context.Context, ch <-chan PeerAddr) {
	for {
		select {
		case <-ctx.Done():
			return
		case <-m.stopCh:
			return
		case pa, ok := <-ch:
			if !ok {
				return
			}
			// Discovery yields a bare address; connecting is how we learn the
			// peer's id and enter it into the view.
			m.connectPeer(pa.Addr)
		}
	}
}

func (m *Membership) detectorLoop(ctx context.Context) {
	events := m.detector.Events()
	for {
		select {
		case <-ctx.Done():
			return
		case <-m.stopCh:
			return
		case ev, ok := <-events:
			if !ok {
				return
			}
			m.markDead(ev.Peer)
		}
	}
}

func (m *Membership) gossipLoop(ctx context.Context) {
	ticker := time.NewTicker(m.cfg.GossipInterval)
	defer ticker.Stop()
	for {
		select {
		case <-ctx.Done():
			return
		case <-m.stopCh:
			return
		case <-ticker.C:
			m.gossipTick()
		}
	}
}

// connectPeer establishes a connection to addr, learns the peer id via the
// handshake, and records it Alive. Idempotent: if we already hold a connection
// to this address it is returned without re-dialing. A failed handshake leaves
// the peer id unknown, which we skip — a down seed does not pollute the view.
func (m *Membership) connectPeer(addr string) *vm.NodeRefData {
	// Idempotent by address: reuse an existing connection rather than opening a
	// second ref (which would orphan the one the detector already heartbeats).
	m.mu.RLock()
	for _, ref := range m.conns {
		if ref.Addr == addr {
			m.mu.RUnlock()
			return ref
		}
	}
	m.mu.RUnlock()

	ref := m.vm.ConnectNode(addr)
	if ref == nil {
		return nil
	}
	pid, ok := ref.PeerNodeID()
	if !ok {
		return nil // handshake did not complete; peer id unknown, skip
	}
	peer := NodeID(pid)
	if peer == m.self {
		return ref // connected to ourselves
	}
	m.mu.Lock()
	if existing, ok := m.conns[peer]; ok {
		m.mu.Unlock()
		return existing // raced with another connect to the same peer
	}
	m.conns[peer] = ref
	m.mu.Unlock()

	m.detector.Track(peer, ref)

	rec := MemberRecord{Peer: peer, Addr: addr, Status: StatusAlive}
	if ev, changed := m.mergeIncoming(rec); changed {
		ev.Kind = EventUp
		m.fanout(ev)
	}
	return ref
}

// ConnectAsync connects to addr in the background — safe to call from the
// event-loop goroutine because it never blocks on the handshake. The address is
// validated first (it may be attacker-supplied gossip data).
func (m *Membership) ConnectAsync(addr string) {
	if !connectableGossipAddr(addr) {
		return
	}
	go m.connectPeer(addr)
}

// ConnectedRef returns the core's existing NodeRef for peer, or nil if we are
// not connected. Wrapped by ClusterMember>>node so the Maggie Node reuses the
// exact ref the core routes and heartbeats on.
func (m *Membership) ConnectedRef(peer [32]byte) *vm.NodeRefData {
	m.mu.RLock()
	defer m.mu.RUnlock()
	return m.conns[NodeID(peer)]
}

// gossipRecords is the digest we put on the wire: our freshly self-signed Alive
// record plus every Alive peer record we hold that carries a valid signature
// (relayed verbatim). Dead/Left records and anything unsigned are never gossiped,
// so death is decided locally by each node's detector and no forged liveness can
// be injected downstream.
func (m *Membership) gossipRecords() []MemberRecord {
	m.mu.RLock()
	self := MemberRecord{
		Peer:        m.self,
		Addr:        m.selfAddr,
		Status:      StatusAlive,
		Incarnation: m.selfIncarnation,
		Metadata:    m.selfMetadata,
	}
	out := make([]MemberRecord, 0, len(m.view)+1)
	for _, r := range m.view {
		if r.Status == StatusAlive && len(r.Sig) > 0 {
			out = append(out, *r)
		}
	}
	m.mu.RUnlock()

	// Sign the self record outside the lock (pure crypto). Skipped if no identity
	// is installed — we then relay only others' signed records.
	if msg, err := recordSigningBytes(self); err == nil {
		if sig, ok := m.vm.SignWithNodeIdentity(msg); ok {
			self.Sig = sig
			out = append(out, self)
		}
	}
	return out
}

// gossipTick sends our signed Alive digest to a random fanout of connected peers.
func (m *Membership) gossipTick() {
	digest := m.gossipRecords()
	payload, err := EncodeGossip(digest)
	if err != nil {
		return
	}

	m.mu.RLock()
	targets := make([]*vm.NodeRefData, 0, len(m.conns))
	for _, ref := range m.conns {
		targets = append(targets, ref)
	}
	m.mu.RUnlock()

	m.rng.Shuffle(len(targets), func(i, j int) { targets[i], targets[j] = targets[j], targets[i] })
	if len(targets) > m.cfg.GossipFanout {
		targets = targets[:m.cfg.GossipFanout]
	}
	for _, ref := range targets {
		if ref.SendFunc == nil {
			continue
		}
		env, err := vm.BuildSignedEnvelope(ref, "", GossipSelector, payload, false)
		if err != nil {
			continue
		}
		go ref.SendFunc(env)
	}
}

// ApplyGossip merges an inbound gossip digest into the view. fromPeer is the
// signature-proven sender (for future anti-poisoning heuristics). Records about
// ourselves trigger refutation; others merge by incarnation. Newly-alive
// gossiped peers are connected per the join policy.
func (m *Membership) ApplyGossip(records []MemberRecord, fromPeer NodeID) {
	var events []MemberEvent
	var toConnect []MemberRecord

	for _, r := range records {
		if r.Peer == m.self {
			m.refuteIfNeeded(r)
			continue
		}
		// Provenance: a record about peer X must be self-signed by X. This blocks
		// a third party forging X's address, metadata, or liveness. Unsigned or
		// mis-signed records are dropped.
		if !verifyRecordSig(r) {
			continue
		}
		// Nodes never sign their own death. Dead/Suspect are decided locally by
		// the failure detector and never accepted from gossip, so a forged Dead
		// cannot evict a live peer. Only self-asserted Alive/Left propagate.
		if r.Status != StatusAlive && r.Status != StatusLeft {
			continue
		}
		// Reject (do not mutate) an over-cap metadata record: mutating it would
		// invalidate the signature we must relay verbatim, and a legitimate node
		// stays within the caps.
		if !m.metadataWithinCaps(r.Metadata) {
			continue
		}
		ev, changed := m.mergeIncoming(r)
		if changed {
			// A gossip-learned member we are not connected to is a join
			// candidate (EventDiscovered) — the Maggie handler decides. One we
			// already hold a connection to is a plain liveness update (EventUp).
			m.mu.RLock()
			_, connected := m.conns[r.Peer]
			m.mu.RUnlock()
			if connected {
				ev.Kind = EventUp
			} else {
				ev.Kind = EventDiscovered
			}
			events = append(events, ev)
			if r.Status == StatusAlive {
				toConnect = append(toConnect, r)
			}
		}
	}

	for _, ev := range events {
		m.fanout(ev)
	}
	for _, r := range toConnect {
		m.maybeJoin(r)
	}
}

// maybeJoin applies the join policy to a gossiped peer we are not connected to.
func (m *Membership) maybeJoin(r MemberRecord) {
	if m.cfg.JoinPolicy != JoinEager {
		return // lazy/manual: recorded but not auto-connected (Phase 2 extends)
	}
	// Only auto-dial addresses that are safe to connect to. The address came
	// from an untrusted gossip peer; refusing link-local/unspecified/multicast
	// targets blocks the cloud-metadata endpoint (169.254.169.254) and similar
	// SSRF destinations even when an operator has opted into eager join.
	if !connectableGossipAddr(r.Addr) {
		return
	}
	m.mu.RLock()
	_, connected := m.conns[r.Peer]
	m.mu.RUnlock()
	if connected || r.Peer == m.self || r.Addr == "" {
		return
	}
	m.connectPeer(r.Addr)
}

// connectableGossipAddr reports whether addr, learned from an untrusted gossip
// peer, is safe to auto-dial. It rejects the unspecified address, link-local
// unicast/multicast (incl. the 169.254.169.254 cloud-metadata endpoint), and
// multicast. Loopback and private ranges are allowed — real and local-test
// clusters legitimately use them. Hostnames are allowed (their resolution is a
// separate concern); a nil/malformed address is rejected.
func connectableGossipAddr(addr string) bool {
	host, _, err := net.SplitHostPort(addr)
	if err != nil {
		return false
	}
	ip := net.ParseIP(host)
	if ip == nil {
		return host != "" // hostname — allowed (not classifiable here)
	}
	if ip.IsUnspecified() || ip.IsLinkLocalUnicast() || ip.IsLinkLocalMulticast() ||
		ip.IsMulticast() || ip.IsInterfaceLocalMulticast() {
		return false
	}
	return true
}

// mergeIncoming applies one record to the view under the incarnation rules and
// returns the resulting event and whether anything changed.
func (m *Membership) mergeIncoming(r MemberRecord) (MemberEvent, bool) {
	m.mu.Lock()
	defer m.mu.Unlock()

	existing, ok := m.view[r.Peer]
	if !ok {
		cp := r
		m.view[r.Peer] = &cp
		return MemberEvent{Peer: r.Peer, Status: r.Status, Record: cp}, true
	}

	// Higher incarnation always wins (whole-record replace). Equal incarnation:
	// a higher-ranked status (failure) wins; nothing else changes.
	switch {
	case r.Incarnation > existing.Incarnation:
		cp := r
		m.view[r.Peer] = &cp
		return MemberEvent{Peer: r.Peer, Status: r.Status, Record: cp}, true
	case r.Incarnation == existing.Incarnation && statusRank(r.Status) > statusRank(existing.Status):
		existing.Status = r.Status
		return MemberEvent{Peer: r.Peer, Status: r.Status, Record: *existing}, true
	default:
		return MemberEvent{}, false
	}
}

// markDead records a detector-reported failure as Dead at the peer's current
// incarnation (sticky within the generation) and drops its connection.
func (m *Membership) markDead(peer NodeID) {
	m.mu.Lock()
	rec, ok := m.view[peer]
	if !ok {
		m.mu.Unlock()
		return
	}
	if rec.Status == StatusDead || rec.Status == StatusLeft {
		m.mu.Unlock()
		return
	}
	rec.Status = StatusDead
	event := MemberEvent{Peer: peer, Status: StatusDead, Kind: EventDown, Record: *rec}
	delete(m.conns, peer)
	m.mu.Unlock()

	m.fanout(event)
}

// refuteIfNeeded implements SWIM self-defense: if a peer gossips that WE are
// Suspect/Dead, bump our incarnation so the next gossip re-asserts us Alive with
// a higher generation that supersedes the false report cluster-wide.
func (m *Membership) refuteIfNeeded(about MemberRecord) {
	if about.Status == StatusAlive {
		return
	}
	m.mu.Lock()
	if about.Incarnation >= m.selfIncarnation {
		m.selfIncarnation = about.Incarnation + 1
	}
	m.mu.Unlock()
}

// SetMetadata replaces this node's gossiped metadata and bumps our incarnation
// so the new labels win cluster-wide. An over-cap map is ignored (kept unchanged)
// — self-metadata obeys the same guardrails as inbound metadata.
func (m *Membership) SetMetadata(md map[string]string) {
	if !m.metadataWithinCaps(md) {
		return
	}
	m.mu.Lock()
	m.selfMetadata = md
	m.selfIncarnation++
	m.mu.Unlock()
}

// SetJoinPolicy sets the coarse join policy at runtime ("eager" | "lazy" |
// "manual"). Unknown values are ignored.
func (m *Membership) SetJoinPolicy(policy string) {
	m.mu.Lock()
	switch policy {
	case "eager":
		m.cfg.JoinPolicy = JoinEager
	case "lazy":
		m.cfg.JoinPolicy = JoinLazy
	case "manual":
		m.cfg.JoinPolicy = JoinManual
	}
	m.mu.Unlock()
}

// AliveMembers returns the current Alive peers (self excluded, Dead/Left
// filtered). This is the authoritative live view for placement/queries; a
// dropped event self-heals because callers re-read it rather than tracking
// deltas.
func (m *Membership) AliveMembers() []MemberRecord {
	m.mu.RLock()
	defer m.mu.RUnlock()
	out := make([]MemberRecord, 0, len(m.view))
	for _, r := range m.view {
		if r.Status == StatusAlive {
			out = append(out, *r)
		}
	}
	return out
}

// AliveMemberInfos implements vm.ClusterCore: the Alive view as vm-side structs,
// free of dist types.
func (m *Membership) AliveMemberInfos() []vm.ClusterMemberInfo {
	m.mu.RLock()
	defer m.mu.RUnlock()
	out := make([]vm.ClusterMemberInfo, 0, len(m.view))
	for _, r := range m.view {
		if r.Status == StatusAlive {
			out = append(out, vm.ClusterMemberInfo{
				ID:       r.Peer,
				Addr:     r.Addr,
				Status:   uint8(r.Status),
				Metadata: r.Metadata,
			})
		}
	}
	return out
}

// Membership implements vm.ClusterCore.
var _ vm.ClusterCore = (*Membership)(nil)

// Snapshot returns the current view plus a fresh self record. Callers get a copy.
func (m *Membership) Snapshot() []MemberRecord {
	m.mu.RLock()
	defer m.mu.RUnlock()
	out := make([]MemberRecord, 0, len(m.view)+1)
	out = append(out, MemberRecord{
		Peer:        m.self,
		Addr:        m.selfAddr,
		Status:      StatusAlive,
		Incarnation: m.selfIncarnation,
		Metadata:    m.selfMetadata,
	})
	for _, r := range m.view {
		out = append(out, *r)
	}
	return out
}

// Members returns the non-self view (for tests and Cluster.mag).
func (m *Membership) Members() []MemberRecord {
	m.mu.RLock()
	defer m.mu.RUnlock()
	out := make([]MemberRecord, 0, len(m.view))
	for _, r := range m.view {
		out = append(out, *r)
	}
	return out
}

// metadataWithinCaps reports whether md respects the open-map guardrails (key
// count and total size). A record exceeding them is rejected whole rather than
// trimmed, so its signature stays verifiable when relayed.
func (m *Membership) metadataWithinCaps(md map[string]string) bool {
	if len(md) == 0 {
		return true
	}
	if len(md) > m.cfg.MaxMetadataKeys {
		return false
	}
	total := 0
	for k, v := range md {
		total += len(k) + len(v)
	}
	return total <= m.cfg.MaxMetadataBytes
}

func (m *Membership) fanout(ev MemberEvent) {
	m.mu.RLock()
	subs := make([]func(MemberEvent), len(m.subscribers))
	copy(subs, m.subscribers)
	m.mu.RUnlock()
	for _, fn := range subs {
		fn(ev)
	}
}
