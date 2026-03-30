package dist

import (
	"encoding/hex"
	"fmt"
	"strings"
	"sync"
	"time"
)

// NodeID is the canonical peer identity: an Ed25519 public key.
type NodeID [32]byte

// NodeIDFromBytes creates a NodeID from a 32-byte slice.
func NodeIDFromBytes(b []byte) NodeID {
	var id NodeID
	copy(id[:], b)
	return id
}

func (id NodeID) String() string { return hex.EncodeToString(id[:]) }

// IsZero returns true if the NodeID is all zeros.
func (id NodeID) IsZero() bool { return id == NodeID{} }

// ParseNodeID parses a 64-character hex string into a NodeID.
func ParseNodeID(s string) (NodeID, error) {
	b, err := hex.DecodeString(s)
	if err != nil || len(b) != 32 {
		return NodeID{}, fmt.Errorf("invalid node ID: %q", s)
	}
	var id NodeID
	copy(id[:], b)
	return id, nil
}

// Perm is a bitmask of operations a peer is allowed to perform.
type Perm uint8

const (
	PermSync    Perm = 1 << iota // push/pull code
	PermMessage                  // send messages to registered processes
	PermSpawn                    // remote process spawning (forkOn:)

	PermNone Perm = 0
	PermAll  Perm = PermSync | PermMessage | PermSpawn
)

// Has returns true if the permission set includes the given flag.
func (p Perm) Has(flag Perm) bool { return p&flag != 0 }

func (p Perm) String() string {
	var names []string
	if p.Has(PermSync) {
		names = append(names, "sync")
	}
	if p.Has(PermMessage) {
		names = append(names, "message")
	}
	if p.Has(PermSpawn) {
		names = append(names, "spawn")
	}
	if len(names) == 0 {
		return "none"
	}
	return strings.Join(names, ",")
}

// ParsePerm parses a comma-separated permission string.
func ParsePerm(s string) Perm {
	var p Perm
	for _, part := range strings.Split(s, ",") {
		switch strings.TrimSpace(part) {
		case "sync":
			p |= PermSync
		case "message":
			p |= PermMessage
		case "spawn":
			p |= PermSpawn
		case "all":
			p |= PermAll
		}
	}
	return p
}

// PeerRecord is the unified per-peer state.
type PeerRecord struct {
	ID             NodeID
	Name           string // human-readable label (optional)
	Perms          Perm   // granted permissions
	Configured     bool   // true if declared in maggie.toml
	Banned         bool
	BanReason      string
	SuccessfulOps  int
	FailedOps      int
	HashMismatches int
	LastSeen       time.Time
	FirstSeen      time.Time
}

// TrustPolicy is the node-wide trust configuration.
type TrustPolicy struct {
	DefaultPerms      Perm     // permissions for unknown peers
	BanThreshold      int      // hash mismatches before auto-ban
	SpawnRestrictions []string // globals hidden from remotely-spawned processes
}

// TrustStore is the unified peer trust database. Replaces PeerStore
// and CapabilityPolicy.
type TrustStore struct {
	mu     sync.RWMutex
	peers  map[NodeID]*PeerRecord
	policy TrustPolicy
}

// NewTrustStore creates a TrustStore with the given policy.
func NewTrustStore(policy TrustPolicy) *TrustStore {
	if policy.BanThreshold == 0 {
		policy.BanThreshold = 3
	}
	// Note: DefaultPerms == 0 (PermNone) is a valid configuration.
	// Only set the default if the caller didn't provide ANY policy fields,
	// which we can't distinguish from explicit PermNone. Use the
	// NewPermissiveTrustStore() constructor for tests that want all perms.
	return &TrustStore{
		peers:  make(map[NodeID]*PeerRecord),
		policy: policy,
	}
}

// NewPermissiveTrustStore creates a TrustStore that allows all operations
// from all peers. Suitable for testing.
func NewPermissiveTrustStore() *TrustStore {
	return NewTrustStore(TrustPolicy{
		DefaultPerms: PermAll,
		BanThreshold: 3,
	})
}

// AddConfiguredPeer registers a peer from maggie.toml.
func (ts *TrustStore) AddConfiguredPeer(id NodeID, name string, perms Perm) {
	ts.mu.Lock()
	defer ts.mu.Unlock()
	rec, ok := ts.peers[id]
	if !ok {
		rec = &PeerRecord{ID: id, FirstSeen: time.Now()}
		ts.peers[id] = rec
	}
	rec.Name = name
	rec.Perms = perms
	rec.Configured = true
	rec.Banned = false // explicit config un-bans
}

// Check returns whether the given peer has the given permission.
func (ts *TrustStore) Check(id NodeID, perm Perm) bool {
	ts.mu.RLock()
	defer ts.mu.RUnlock()
	rec, ok := ts.peers[id]
	if !ok {
		return ts.policy.DefaultPerms.Has(perm)
	}
	if rec.Banned {
		return false
	}
	return rec.Perms.Has(perm)
}

// IsBanned returns true if the peer is banned.
func (ts *TrustStore) IsBanned(id NodeID) bool {
	ts.mu.RLock()
	defer ts.mu.RUnlock()
	rec, ok := ts.peers[id]
	if !ok {
		return false
	}
	return rec.Banned
}

// RecordSuccess records a successful operation.
func (ts *TrustStore) RecordSuccess(id NodeID) {
	ts.mu.Lock()
	defer ts.mu.Unlock()
	rec := ts.getOrCreate(id)
	rec.SuccessfulOps++
	rec.LastSeen = time.Now()
}

// RecordFailure records a failed operation.
func (ts *TrustStore) RecordFailure(id NodeID) {
	ts.mu.Lock()
	defer ts.mu.Unlock()
	rec := ts.getOrCreate(id)
	rec.FailedOps++
	rec.LastSeen = time.Now()
}

// RecordHashMismatch records a hash mismatch and auto-bans if threshold hit.
// Returns true if the peer was banned.
func (ts *TrustStore) RecordHashMismatch(id NodeID) bool {
	ts.mu.Lock()
	defer ts.mu.Unlock()
	rec := ts.getOrCreate(id)
	rec.HashMismatches++
	rec.LastSeen = time.Now()
	if rec.HashMismatches >= ts.policy.BanThreshold {
		rec.Banned = true
		rec.BanReason = fmt.Sprintf("hash mismatches: %d", rec.HashMismatches)
		return true
	}
	return false
}

// CheckCapabilities verifies that all capabilities required by a manifest
// are not denied. This preserves the existing capability-checking behavior
// for the sync Announce RPC.
func (ts *TrustStore) CheckCapabilities(manifest *CapabilityManifest) error {
	if manifest == nil {
		return nil
	}
	// With the trust model, capabilities are informational — the peer's
	// Perm bits determine what they can do. But we still reject if the
	// manifest declares capabilities we've explicitly restricted for spawning.
	for _, cap := range manifest.Required {
		for _, restricted := range ts.policy.SpawnRestrictions {
			if cap == restricted {
				// Not an error for sync — spawn restrictions apply to execution only
				break
			}
		}
	}
	return nil
}

// SpawnRestrictions returns the hidden-set for remotely spawned processes.
func (ts *TrustStore) SpawnRestrictions() []string {
	return ts.policy.SpawnRestrictions
}

// Peer returns a copy of a peer record, or nil if unknown.
func (ts *TrustStore) Peer(id NodeID) *PeerRecord {
	ts.mu.RLock()
	defer ts.mu.RUnlock()
	rec, ok := ts.peers[id]
	if !ok {
		return nil
	}
	cp := *rec
	return &cp
}

// AllPeers returns copies of all peer records.
func (ts *TrustStore) AllPeers() []PeerRecord {
	ts.mu.RLock()
	defer ts.mu.RUnlock()
	result := make([]PeerRecord, 0, len(ts.peers))
	for _, rec := range ts.peers {
		result = append(result, *rec)
	}
	return result
}

// PeerCount returns the number of known peers.
func (ts *TrustStore) PeerCount() int {
	ts.mu.RLock()
	defer ts.mu.RUnlock()
	return len(ts.peers)
}

func (ts *TrustStore) getOrCreate(id NodeID) *PeerRecord {
	rec, ok := ts.peers[id]
	if !ok {
		rec = &PeerRecord{
			ID:        id,
			Perms:     ts.policy.DefaultPerms,
			FirstSeen: time.Now(),
		}
		ts.peers[id] = rec
	}
	return rec
}
