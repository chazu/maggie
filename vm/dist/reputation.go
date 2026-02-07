package dist

import (
	"sync"
	"time"
)

const defaultBanThreshold = 3

// PeerReputation tracks the trust level of a single peer.
type PeerReputation struct {
	PeerID          string
	SuccessfulSyncs int
	FailedSyncs     int
	HashMismatches  int
	LastSeen        time.Time
	Banned          bool
}

// PeerStore maintains reputation data for all known peers.
type PeerStore struct {
	mu           sync.RWMutex
	peers        map[string]*PeerReputation
	banThreshold int
}

// NewPeerStore creates a new peer store with default settings.
func NewPeerStore() *PeerStore {
	return &PeerStore{
		peers:        make(map[string]*PeerReputation),
		banThreshold: defaultBanThreshold,
	}
}

// getOrCreate returns the reputation for a peer, creating it if needed.
// Caller must hold the write lock.
func (ps *PeerStore) getOrCreate(peerID string) *PeerReputation {
	p, ok := ps.peers[peerID]
	if !ok {
		p = &PeerReputation{PeerID: peerID}
		ps.peers[peerID] = p
	}
	p.LastSeen = time.Now()
	return p
}

// RecordSuccess records a successful sync with a peer.
func (ps *PeerStore) RecordSuccess(peerID string) {
	ps.mu.Lock()
	defer ps.mu.Unlock()
	p := ps.getOrCreate(peerID)
	p.SuccessfulSyncs++
}

// RecordFailure records a failed sync with a peer.
func (ps *PeerStore) RecordFailure(peerID string) {
	ps.mu.Lock()
	defer ps.mu.Unlock()
	p := ps.getOrCreate(peerID)
	p.FailedSyncs++
}

// RecordHashMismatch records a hash verification failure. The peer is
// automatically banned after reaching the threshold (default: 3).
func (ps *PeerStore) RecordHashMismatch(peerID string) {
	ps.mu.Lock()
	defer ps.mu.Unlock()
	p := ps.getOrCreate(peerID)
	p.HashMismatches++
	if p.HashMismatches >= ps.banThreshold {
		p.Banned = true
	}
}

// IsBanned returns true if the peer has been banned.
func (ps *PeerStore) IsBanned(peerID string) bool {
	ps.mu.RLock()
	defer ps.mu.RUnlock()
	p, ok := ps.peers[peerID]
	if !ok {
		return false
	}
	return p.Banned
}

// GetReputation returns a copy of the peer's reputation data.
// Returns nil if the peer is unknown.
func (ps *PeerStore) GetReputation(peerID string) *PeerReputation {
	ps.mu.RLock()
	defer ps.mu.RUnlock()
	p, ok := ps.peers[peerID]
	if !ok {
		return nil
	}
	copy := *p
	return &copy
}

// PeerCount returns the number of known peers.
func (ps *PeerStore) PeerCount() int {
	ps.mu.RLock()
	defer ps.mu.RUnlock()
	return len(ps.peers)
}
