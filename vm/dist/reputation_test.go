package dist

import "testing"

func TestPeerStore_RecordSuccess(t *testing.T) {
	ps := NewPeerStore()
	ps.RecordSuccess("peer1")
	ps.RecordSuccess("peer1")

	rep := ps.GetReputation("peer1")
	if rep == nil {
		t.Fatal("peer should exist")
	}
	if rep.SuccessfulSyncs != 2 {
		t.Errorf("SuccessfulSyncs: got %d, want 2", rep.SuccessfulSyncs)
	}
}

func TestPeerStore_RecordFailure(t *testing.T) {
	ps := NewPeerStore()
	ps.RecordFailure("peer1")

	rep := ps.GetReputation("peer1")
	if rep.FailedSyncs != 1 {
		t.Errorf("FailedSyncs: got %d, want 1", rep.FailedSyncs)
	}
}

func TestPeerStore_BanAfterHashMismatches(t *testing.T) {
	ps := NewPeerStore()

	for i := 0; i < 2; i++ {
		ps.RecordHashMismatch("peer1")
		if ps.IsBanned("peer1") {
			t.Errorf("peer should not be banned after %d mismatches", i+1)
		}
	}

	ps.RecordHashMismatch("peer1") // 3rd mismatch
	if !ps.IsBanned("peer1") {
		t.Error("peer should be banned after 3 hash mismatches")
	}
}

func TestPeerStore_UnknownPeerNotBanned(t *testing.T) {
	ps := NewPeerStore()
	if ps.IsBanned("unknown") {
		t.Error("unknown peer should not be banned")
	}
}

func TestPeerStore_UnknownPeerReputationNil(t *testing.T) {
	ps := NewPeerStore()
	if ps.GetReputation("unknown") != nil {
		t.Error("unknown peer should return nil reputation")
	}
}

func TestPeerStore_PeerCount(t *testing.T) {
	ps := NewPeerStore()
	ps.RecordSuccess("a")
	ps.RecordSuccess("b")
	ps.RecordFailure("c")

	if ps.PeerCount() != 3 {
		t.Errorf("PeerCount: got %d, want 3", ps.PeerCount())
	}
}

func TestPeerStore_ReputationIsCopy(t *testing.T) {
	ps := NewPeerStore()
	ps.RecordSuccess("peer1")

	rep := ps.GetReputation("peer1")
	rep.SuccessfulSyncs = 999

	rep2 := ps.GetReputation("peer1")
	if rep2.SuccessfulSyncs != 1 {
		t.Error("GetReputation should return a copy, not a reference")
	}
}
