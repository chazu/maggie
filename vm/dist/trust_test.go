package dist

import (
	"testing"
)

func TestNodeID_ParseAndString(t *testing.T) {
	hex := "a1b2c3d4e5f6a7b8a1b2c3d4e5f6a7b8a1b2c3d4e5f6a7b8a1b2c3d4e5f6a7b8"
	id, err := ParseNodeID(hex)
	if err != nil {
		t.Fatalf("ParseNodeID: %v", err)
	}
	if id.String() != hex {
		t.Errorf("round-trip: got %q, want %q", id.String(), hex)
	}
}

func TestNodeID_ParseInvalid(t *testing.T) {
	_, err := ParseNodeID("short")
	if err == nil {
		t.Error("should reject short hex")
	}
	_, err = ParseNodeID("zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")
	if err == nil {
		t.Error("should reject invalid hex")
	}
}

func TestPerm_ParseAndString(t *testing.T) {
	tests := []struct {
		input string
		perm  Perm
		str   string
	}{
		{"sync", PermSync, "sync"},
		{"message", PermMessage, "message"},
		{"spawn", PermSpawn, "spawn"},
		{"sync,message", PermSync | PermMessage, "sync,message"},
		{"all", PermAll, "sync,message,spawn"},
		{"", PermNone, "none"},
	}
	for _, tt := range tests {
		got := ParsePerm(tt.input)
		if got != tt.perm {
			t.Errorf("ParsePerm(%q): got %d, want %d", tt.input, got, tt.perm)
		}
		if got.String() != tt.str {
			t.Errorf("String(): got %q, want %q", got.String(), tt.str)
		}
	}
}

func TestTrustStore_DefaultPerms(t *testing.T) {
	ts := NewTrustStore(TrustPolicy{DefaultPerms: PermSync})
	unknown := NodeID{1, 2, 3}

	if !ts.Check(unknown, PermSync) {
		t.Error("unknown peer should have sync permission by default")
	}
	if ts.Check(unknown, PermMessage) {
		t.Error("unknown peer should NOT have message permission")
	}
	if ts.Check(unknown, PermSpawn) {
		t.Error("unknown peer should NOT have spawn permission")
	}
}

func TestTrustStore_ConfiguredPeer(t *testing.T) {
	ts := NewTrustStore(TrustPolicy{DefaultPerms: PermNone})
	peer := NodeID{10, 20, 30}
	ts.AddConfiguredPeer(peer, "test-peer", PermSync|PermMessage)

	if !ts.Check(peer, PermSync) {
		t.Error("configured peer should have sync")
	}
	if !ts.Check(peer, PermMessage) {
		t.Error("configured peer should have message")
	}
	if ts.Check(peer, PermSpawn) {
		t.Error("configured peer should NOT have spawn")
	}
}

func TestTrustStore_BanAndUnban(t *testing.T) {
	ts := NewPermissiveTrustStore()
	peer := NodeID{1}

	// Initially allowed
	if !ts.Check(peer, PermSync) {
		t.Error("should be allowed initially")
	}

	// Ban via hash mismatches
	ts.RecordHashMismatch(peer)
	ts.RecordHashMismatch(peer)
	banned := ts.RecordHashMismatch(peer)
	if !banned {
		t.Error("should be banned after 3 mismatches")
	}
	if !ts.IsBanned(peer) {
		t.Error("IsBanned should return true")
	}
	if ts.Check(peer, PermSync) {
		t.Error("banned peer should have no permissions")
	}
}

func TestTrustStore_Permissive(t *testing.T) {
	ts := NewPermissiveTrustStore()
	unknown := NodeID{99}

	if !ts.Check(unknown, PermSync) {
		t.Error("permissive: should allow sync")
	}
	if !ts.Check(unknown, PermMessage) {
		t.Error("permissive: should allow message")
	}
	if !ts.Check(unknown, PermSpawn) {
		t.Error("permissive: should allow spawn")
	}
}

func TestTrustStore_PermNone(t *testing.T) {
	ts := NewTrustStore(TrustPolicy{DefaultPerms: PermNone})
	unknown := NodeID{42}

	if ts.Check(unknown, PermSync) {
		t.Error("PermNone: should deny sync")
	}
	if ts.Check(unknown, PermMessage) {
		t.Error("PermNone: should deny message")
	}
}

func TestTrustStore_RecordSuccess(t *testing.T) {
	ts := NewPermissiveTrustStore()
	peer := NodeID{5}
	ts.RecordSuccess(peer)
	ts.RecordSuccess(peer)

	rec := ts.Peer(peer)
	if rec == nil {
		t.Fatal("peer should exist")
	}
	if rec.SuccessfulOps != 2 {
		t.Errorf("SuccessfulOps: got %d, want 2", rec.SuccessfulOps)
	}
}

func TestTrustStore_SpawnRestrictions(t *testing.T) {
	ts := NewTrustStore(TrustPolicy{
		SpawnRestrictions: []string{"File", "HTTP"},
	})
	restrictions := ts.SpawnRestrictions()
	if len(restrictions) != 2 {
		t.Errorf("restrictions: got %d, want 2", len(restrictions))
	}
}

func TestTrustStore_ConfiguredPeerUnbans(t *testing.T) {
	ts := NewPermissiveTrustStore()
	peer := NodeID{7}

	// Ban
	ts.RecordHashMismatch(peer)
	ts.RecordHashMismatch(peer)
	ts.RecordHashMismatch(peer)
	if !ts.IsBanned(peer) {
		t.Fatal("should be banned")
	}

	// Configure with explicit perms — should un-ban
	ts.AddConfiguredPeer(peer, "forgiven", PermAll)
	if ts.IsBanned(peer) {
		t.Error("configured peer should be un-banned")
	}
	if !ts.Check(peer, PermSync) {
		t.Error("should have sync after reconfiguration")
	}
}
