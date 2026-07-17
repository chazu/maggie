package dist

import (
	"context"
	"crypto/ed25519"
	"sync/atomic"
	"testing"
	"time"

	"github.com/chazu/maggie/vm"
)

func nid(b byte) NodeID {
	var n NodeID
	n[0] = b
	return n
}

// idNodeID returns the 32-byte NodeID (== public key) for an identity.
func idNodeID(idty *NodeIdentity) NodeID {
	var n NodeID
	copy(n[:], idty.PublicKey)
	return n
}

// signedRecordFor builds a MemberRecord about the subject and self-signs it with
// the subject's key — the only records ApplyGossip now accepts.
func signedRecordFor(t *testing.T, subject *NodeIdentity, addr string, status MemberStatus, inc uint64, md map[string]string) MemberRecord {
	t.Helper()
	r := MemberRecord{Peer: idNodeID(subject), Addr: addr, Status: status, Incarnation: inc, Metadata: md}
	msg, err := recordSigningBytes(r)
	if err != nil {
		t.Fatalf("recordSigningBytes: %v", err)
	}
	r.Sig = ed25519.Sign(subject.PrivateKey, msg)
	return r
}

type fakeDetector struct{ ev chan MemberEvent }

func newFakeDetector() *fakeDetector                  { return &fakeDetector{ev: make(chan MemberEvent, 8)} }
func (f *fakeDetector) Track(NodeID, *vm.NodeRefData) {}
func (f *fakeDetector) Untrack(NodeID)                {}
func (f *fakeDetector) Events() <-chan MemberEvent    { return f.ev }
func (f *fakeDetector) Stop()                         { close(f.ev) }

// newTestMembership builds a membership with JoinManual so ApplyGossip records
// peers without attempting real network connects.
func newTestMembership(t *testing.T, self NodeID) *Membership {
	t.Helper()
	v := vm.NewVM()
	t.Cleanup(v.Shutdown)
	return NewMembership(v, NewPermissiveTrustStore(), self, "self:0", newFakeDetector(),
		MembershipConfig{JoinPolicy: JoinManual})
}

func findRecord(records []MemberRecord, peer NodeID) (MemberRecord, bool) {
	for _, r := range records {
		if r.Peer == peer {
			return r, true
		}
	}
	return MemberRecord{}, false
}

func TestMembership_MergeIncarnationWins(t *testing.T) {
	m := newTestMembership(t, nid(1))
	peer, _ := GenerateIdentity()
	pid := idNodeID(peer)

	m.ApplyGossip([]MemberRecord{signedRecordFor(t, peer, "a:1", StatusAlive, 1, nil)}, nid(9))
	m.ApplyGossip([]MemberRecord{signedRecordFor(t, peer, "a:2", StatusAlive, 3, nil)}, nid(9))

	r, ok := findRecord(m.Members(), pid)
	if !ok {
		t.Fatal("peer not in view")
	}
	if r.Incarnation != 3 || r.Addr != "a:2" {
		t.Errorf("higher incarnation should replace the whole record: got inc=%d addr=%s", r.Incarnation, r.Addr)
	}

	// A stale lower incarnation must be ignored.
	m.ApplyGossip([]MemberRecord{signedRecordFor(t, peer, "a:stale", StatusAlive, 2, nil)}, nid(9))
	r, _ = findRecord(m.Members(), pid)
	if r.Addr != "a:2" {
		t.Errorf("stale incarnation should be ignored, got addr=%s", r.Addr)
	}
}

// TestMembership_DeadNotGossipableAndResurrection verifies that (a) death is set
// only by the local detector, never accepted from gossip, and (b) the merge
// rules still prevent a same-incarnation Alive from resurrecting a Dead peer
// while a higher-incarnation Alive does.
func TestMembership_DeadNotGossipableAndResurrection(t *testing.T) {
	det := newFakeDetector()
	v := vm.NewVM()
	t.Cleanup(v.Shutdown)
	m := NewMembership(v, NewPermissiveTrustStore(), nid(1), "self:0", det, MembershipConfig{JoinPolicy: JoinManual})
	peer, _ := GenerateIdentity()
	pid := idNodeID(peer)

	// Seed Alive @5, then the detector marks it Dead.
	m.ApplyGossip([]MemberRecord{signedRecordFor(t, peer, "a:1", StatusAlive, 5, nil)}, nid(9))
	m.markDead(pid)
	if r, _ := findRecord(m.Members(), pid); r.Status != StatusDead {
		t.Fatalf("detector should mark peer Dead, got %d", r.Status)
	}

	// A (validly signed) same-incarnation Alive from gossip must NOT resurrect.
	m.ApplyGossip([]MemberRecord{signedRecordFor(t, peer, "a:1", StatusAlive, 5, nil)}, nid(9))
	if r, _ := findRecord(m.Members(), pid); r.Status != StatusDead {
		t.Errorf("same-incarnation Alive should not resurrect Dead, got %d", r.Status)
	}

	// A higher-incarnation signed Alive (a real recovery) does resurrect.
	m.ApplyGossip([]MemberRecord{signedRecordFor(t, peer, "a:1", StatusAlive, 6, nil)}, nid(9))
	if r, _ := findRecord(m.Members(), pid); r.Status != StatusAlive {
		t.Errorf("higher-incarnation Alive should resurrect, got %d", r.Status)
	}
}

// TestMembership_RejectsForgedAndUnsigned is the poison-remediation regression:
// a record about peer X is only accepted if self-signed by X, and gossiped
// Dead is never accepted at all.
func TestMembership_RejectsForgedAndUnsigned(t *testing.T) {
	m := newTestMembership(t, nid(1))
	victim, _ := GenerateIdentity()
	attacker, _ := GenerateIdentity()
	vid := idNodeID(victim)

	// 1. Unsigned record about the victim → dropped.
	m.ApplyGossip([]MemberRecord{{Peer: vid, Addr: "a:1", Status: StatusAlive, Incarnation: 1}}, idNodeID(attacker))
	if _, ok := findRecord(m.Members(), vid); ok {
		t.Error("unsigned record must be dropped")
	}

	// 2. Record about the victim signed by the ATTACKER (wrong key) → dropped.
	forged := MemberRecord{Peer: vid, Addr: "evil:1", Status: StatusAlive, Incarnation: 99}
	msg, _ := recordSigningBytes(forged)
	forged.Sig = ed25519.Sign(attacker.PrivateKey, msg) // signed by the wrong identity
	m.ApplyGossip([]MemberRecord{forged}, idNodeID(attacker))
	if _, ok := findRecord(m.Members(), vid); ok {
		t.Error("record signed by a non-subject key must be dropped")
	}

	// 3. Establish the victim as Alive via a genuine self-signed record.
	m.ApplyGossip([]MemberRecord{signedRecordFor(t, victim, "good:1", StatusAlive, 1, nil)}, idNodeID(victim))
	if _, ok := findRecord(m.Members(), vid); !ok {
		t.Fatal("genuine self-signed record should be accepted")
	}

	// 4. A forged high-incarnation Dead about the live victim (the poison attack)
	// must NOT evict it — gossiped Dead is never accepted.
	deadForge := signedRecordFor(t, victim, "good:1", StatusDead, 1000, nil) // even if it WERE signed
	m.ApplyGossip([]MemberRecord{deadForge}, idNodeID(attacker))
	if r, ok := findRecord(m.Members(), vid); !ok || r.Status != StatusAlive {
		t.Error("gossiped Dead must not evict a live peer")
	}
}

func TestMembership_SelfRefutation(t *testing.T) {
	self := nid(1)
	m := newTestMembership(t, self)

	// A peer claims we are Dead at incarnation 7. We must bump past it. (Records
	// about self are handled by refutation before any signature check.)
	m.ApplyGossip([]MemberRecord{{Peer: self, Status: StatusDead, Incarnation: 7}}, nid(9))

	self0, ok := findRecord(m.Snapshot(), self)
	if !ok {
		t.Fatal("self record missing from snapshot")
	}
	if self0.Status != StatusAlive {
		t.Errorf("self should always snapshot as Alive, got %d", self0.Status)
	}
	if self0.Incarnation <= 7 {
		t.Errorf("self incarnation should exceed the false report (7), got %d", self0.Incarnation)
	}
	if _, present := findRecord(m.Members(), self); present {
		t.Error("self must not appear in the non-self member view")
	}
}

func TestMembership_SubscribersFire(t *testing.T) {
	m := newTestMembership(t, nid(1))
	peer, _ := GenerateIdentity()
	var got []MemberEvent
	m.Subscribe(func(ev MemberEvent) { got = append(got, ev) })

	m.ApplyGossip([]MemberRecord{signedRecordFor(t, peer, "a:1", StatusAlive, 1, nil)}, nid(9))
	if len(got) != 1 || got[0].Peer != idNodeID(peer) || got[0].Status != StatusAlive {
		t.Fatalf("expected one Alive event, got %+v", got)
	}
	// A no-op merge (same incarnation, same status) fires nothing.
	m.ApplyGossip([]MemberRecord{signedRecordFor(t, peer, "a:1", StatusAlive, 1, nil)}, nid(9))
	if len(got) != 1 {
		t.Errorf("no-op merge should not fire an event, got %d events", len(got))
	}
}

func TestMembership_MetadataCaps(t *testing.T) {
	v := vm.NewVM()
	t.Cleanup(v.Shutdown)
	m := NewMembership(v, NewPermissiveTrustStore(), nid(1), "self:0", newFakeDetector(),
		MembershipConfig{JoinPolicy: JoinManual, MaxMetadataKeys: 2, MaxMetadataBytes: 64})
	over, _ := GenerateIdentity()
	ok, _ := GenerateIdentity()

	// Too many keys → whole record dropped (can't strip metadata without breaking
	// the signature we'd relay).
	m.ApplyGossip([]MemberRecord{signedRecordFor(t, over, "a:1", StatusAlive, 1, map[string]string{"a": "1", "b": "2", "c": "3"})}, nid(9))
	if _, present := findRecord(m.Members(), idNodeID(over)); present {
		t.Error("over-cap metadata record should be dropped entirely")
	}

	// Within caps → metadata kept verbatim.
	m.ApplyGossip([]MemberRecord{signedRecordFor(t, ok, "a:2", StatusAlive, 1, map[string]string{"zone": "us-1"})}, nid(9))
	r, _ := findRecord(m.Members(), idNodeID(ok))
	if r.Metadata["zone"] != "us-1" {
		t.Errorf("within-cap metadata should be kept, got %v", r.Metadata)
	}
}

func TestMembership_MarkDeadViaDetector(t *testing.T) {
	det := newFakeDetector()
	v := vm.NewVM()
	t.Cleanup(v.Shutdown)
	m := NewMembership(v, NewPermissiveTrustStore(), nid(1), "self:0", det,
		MembershipConfig{JoinPolicy: JoinManual})
	peer, _ := GenerateIdentity()
	pid := idNodeID(peer)

	var downs atomic.Int32
	m.Subscribe(func(ev MemberEvent) {
		if ev.Status == StatusDead {
			downs.Add(1)
		}
	})
	m.ApplyGossip([]MemberRecord{signedRecordFor(t, peer, "a:1", StatusAlive, 1, nil)}, nid(9))

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	m.Start(ctx)
	det.ev <- MemberEvent{Peer: pid, Status: StatusDead}

	deadline := time.After(time.Second)
	for {
		if r, ok := findRecord(m.Members(), pid); ok && r.Status == StatusDead {
			break
		}
		select {
		case <-deadline:
			t.Fatal("detector Dead event did not mark the peer dead")
		case <-time.After(5 * time.Millisecond):
		}
	}
	m.Stop()
	if n := downs.Load(); n != 1 {
		t.Errorf("expected one Down event, got %d", n)
	}
}

// TestMembership_GossipRecordsAreSignedAliveOnly verifies the digest we put on
// the wire: our self record is present and self-signed, and Dead peers are
// excluded.
func TestMembership_GossipRecordsAreSignedAliveOnly(t *testing.T) {
	self, _ := GenerateIdentity()
	selfID := idNodeID(self)
	v := vm.NewVM()
	t.Cleanup(v.Shutdown)
	v.SetNodeIdentityKeys(self.PublicKey, self.PrivateKey)
	m := NewMembership(v, NewPermissiveTrustStore(), selfID, "self:0", newFakeDetector(), MembershipConfig{JoinPolicy: JoinManual})

	peer, _ := GenerateIdentity()
	m.ApplyGossip([]MemberRecord{signedRecordFor(t, peer, "a:1", StatusAlive, 1, nil)}, nid(9))
	m.markDead(idNodeID(peer)) // now Dead — must be excluded from gossip

	digest := m.gossipRecords()
	selfRec, ok := findRecord(digest, selfID)
	if !ok {
		t.Fatal("self record missing from gossip digest")
	}
	if !verifyRecordSig(selfRec) {
		t.Error("self record in digest is not validly self-signed")
	}
	if _, present := findRecord(digest, idNodeID(peer)); present {
		t.Error("Dead peer must not be gossiped")
	}
}

func TestStaticDiscovery_EmitsSeeds(t *testing.T) {
	d := NewStaticDiscovery([]string{"a:1", "b:2"})
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	ch, err := d.Discover(ctx)
	if err != nil {
		t.Fatalf("discover: %v", err)
	}
	got := map[string]bool{}
	for i := 0; i < 2; i++ {
		select {
		case pa := <-ch:
			if pa.Source != "static" {
				t.Errorf("source: got %q", pa.Source)
			}
			got[pa.Addr] = true
		case <-time.After(time.Second):
			t.Fatal("timed out waiting for seed")
		}
	}
	if !got["a:1"] || !got["b:2"] {
		t.Errorf("missing seeds: %v", got)
	}
}

func TestConnectableGossipAddr(t *testing.T) {
	cases := map[string]bool{
		"10.0.0.5:8081":            true,  // private — real clusters
		"203.0.113.7:8081":         true,  // public
		"host.example.com:8081":    true,  // hostname
		"127.0.0.1:8081":           true,  // loopback allowed (local multi-node tests)
		"169.254.169.254:80":       false, // cloud metadata (link-local)
		"0.0.0.0:8081":             false, // unspecified
		"[::]:8081":                false, // unspecified v6
		"224.0.0.1:8081":           false, // multicast
		"notanaddress":             false, // malformed (no port)
	}
	for addr, want := range cases {
		if got := connectableGossipAddr(addr); got != want {
			t.Errorf("connectableGossipAddr(%q) = %v, want %v", addr, got, want)
		}
	}
}

func TestGossipWire_RecordCap(t *testing.T) {
	over := make([]MemberRecord, maxGossipRecords+1)
	data, err := EncodeGossip(over)
	if err != nil {
		t.Fatalf("encode: %v", err)
	}
	if _, err := DecodeGossip(data); err == nil {
		t.Error("DecodeGossip should reject a digest exceeding the record cap")
	}
}

func TestGossipWire_RoundTrip(t *testing.T) {
	records := []MemberRecord{
		{Peer: nid(1), Addr: "a:1", Status: StatusAlive, Incarnation: 2, Metadata: map[string]string{"zone": "eu"}, Sig: []byte{1, 2, 3}},
		{Peer: nid(2), Addr: "b:2", Status: StatusDead, Incarnation: 5},
	}
	data, err := EncodeGossip(records)
	if err != nil {
		t.Fatalf("encode: %v", err)
	}
	got, err := DecodeGossip(data)
	if err != nil {
		t.Fatalf("decode: %v", err)
	}
	if len(got) != 2 {
		t.Fatalf("record count: got %d", len(got))
	}
	r0, _ := findRecord(got, nid(1))
	if r0.Addr != "a:1" || r0.Incarnation != 2 || r0.Metadata["zone"] != "eu" || len(r0.Sig) != 3 {
		t.Errorf("record 1 round-trip mismatch: %+v", r0)
	}
}
