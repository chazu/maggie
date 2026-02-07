package server

import (
	"crypto/sha256"
	"testing"

	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
	"github.com/chazu/maggie/vm"
	"github.com/chazu/maggie/vm/dist"
)

// newTestSyncService creates a SyncService with a fresh content store
// and a permissive capability policy. The compile function simply
// hashes the source text (no real compilation).
func newTestSyncService() (*SyncService, *vm.ContentStore) {
	store := vm.NewContentStore()
	peers := dist.NewPeerStore()
	policy := dist.NewPermissivePolicy()
	compile := func(source string) ([32]byte, error) {
		return sha256.Sum256([]byte(source)), nil
	}
	svc := NewSyncService(testWorker, store, peers, policy, compile)
	return svc, store
}

// ---------------------------------------------------------------------------
// Ping
// ---------------------------------------------------------------------------

func TestSyncPing_Empty(t *testing.T) {
	svc, _ := newTestSyncService()

	resp, err := svc.Ping(bg(), connectReq(&maggiev1.PingRequest{}))
	if err != nil {
		t.Fatalf("Ping returned error: %v", err)
	}
	if resp.Msg.ContentCount != 0 {
		t.Errorf("ContentCount: got %d, want 0", resp.Msg.ContentCount)
	}
}

func TestSyncPing_WithContent(t *testing.T) {
	svc, store := newTestSyncService()

	mh := sha256.Sum256([]byte("m1"))
	m := vm.NewCompiledMethodBuilder("m1", 0).Build()
	m.SetContentHash(mh)
	store.IndexMethod(m)

	resp, err := svc.Ping(bg(), connectReq(&maggiev1.PingRequest{}))
	if err != nil {
		t.Fatalf("Ping returned error: %v", err)
	}
	if resp.Msg.ContentCount != 1 {
		t.Errorf("ContentCount: got %d, want 1", resp.Msg.ContentCount)
	}
}

// ---------------------------------------------------------------------------
// Announce
// ---------------------------------------------------------------------------

func TestSyncAnnounce_WantsAll(t *testing.T) {
	svc, _ := newTestSyncService()

	rootHash := sha256.Sum256([]byte("root"))
	h1 := sha256.Sum256([]byte("h1"))

	resp, err := svc.Announce(bg(), connectReq(&maggiev1.AnnounceRequest{
		RootHash:  rootHash[:],
		AllHashes: [][]byte{rootHash[:], h1[:]},
	}))
	if err != nil {
		t.Fatalf("Announce returned error: %v", err)
	}
	if resp.Msg.Status != maggiev1.AnnounceStatus_ANNOUNCE_ACCEPTED {
		t.Errorf("Status: got %v, want ACCEPTED", resp.Msg.Status)
	}
	if len(resp.Msg.Want) != 2 {
		t.Errorf("Want: got %d hashes, want 2", len(resp.Msg.Want))
	}
}

func TestSyncAnnounce_AlreadyHaveAll(t *testing.T) {
	svc, store := newTestSyncService()

	mh := sha256.Sum256([]byte("m1"))
	m := vm.NewCompiledMethodBuilder("m1", 0).Build()
	m.SetContentHash(mh)
	store.IndexMethod(m)

	resp, err := svc.Announce(bg(), connectReq(&maggiev1.AnnounceRequest{
		RootHash:  mh[:],
		AllHashes: [][]byte{mh[:]},
	}))
	if err != nil {
		t.Fatalf("Announce returned error: %v", err)
	}
	if resp.Msg.Status != maggiev1.AnnounceStatus_ANNOUNCE_ALREADY_HAVE {
		t.Errorf("Status: got %v, want ALREADY_HAVE", resp.Msg.Status)
	}
}

func TestSyncAnnounce_InvalidRootHash(t *testing.T) {
	svc, _ := newTestSyncService()

	_, err := svc.Announce(bg(), connectReq(&maggiev1.AnnounceRequest{
		RootHash: []byte("short"),
	}))
	if err == nil {
		t.Fatal("Announce with short root_hash should return error")
	}
}

func TestSyncAnnounce_RejectedByPolicy(t *testing.T) {
	store := vm.NewContentStore()
	peers := dist.NewPeerStore()
	policy := dist.NewRestrictedPolicy([]string{"File"})
	svc := NewSyncService(testWorker, store, peers, policy, nil)

	rootHash := sha256.Sum256([]byte("root"))

	// Encode a capability manifest requiring "Network" which is not allowed
	capManifest := dist.CapabilityManifest{Required: []string{"Network"}}
	capBytes, err := marshalCBOR(&capManifest)
	if err != nil {
		t.Fatalf("marshal capability: %v", err)
	}

	resp, err := svc.Announce(bg(), connectReq(&maggiev1.AnnounceRequest{
		RootHash:           rootHash[:],
		AllHashes:          [][]byte{rootHash[:]},
		CapabilityManifest: capBytes,
	}))
	if err != nil {
		t.Fatalf("Announce returned error: %v", err)
	}
	if resp.Msg.Status != maggiev1.AnnounceStatus_ANNOUNCE_REJECTED {
		t.Errorf("Status: got %v, want REJECTED", resp.Msg.Status)
	}
	if resp.Msg.RejectReason == "" {
		t.Error("RejectReason should be set")
	}
}

// ---------------------------------------------------------------------------
// Transfer
// ---------------------------------------------------------------------------

func TestSyncTransfer_ValidChunk(t *testing.T) {
	svc, _ := newTestSyncService()

	// Create a chunk whose hash is sha256("source")
	source := "source"
	h := sha256.Sum256([]byte(source))
	chunk := &dist.Chunk{Hash: h, Type: dist.ChunkMethod, Content: source}
	chunkBytes, err := dist.MarshalChunk(chunk)
	if err != nil {
		t.Fatalf("MarshalChunk: %v", err)
	}

	resp, err := svc.Transfer(bg(), connectReq(&maggiev1.TransferRequest{
		Chunks: [][]byte{chunkBytes},
	}))
	if err != nil {
		t.Fatalf("Transfer returned error: %v", err)
	}
	if resp.Msg.Accepted != 1 {
		t.Errorf("Accepted: got %d, want 1", resp.Msg.Accepted)
	}
	if resp.Msg.Rejected != 0 {
		t.Errorf("Rejected: got %d, want 0", resp.Msg.Rejected)
	}
}

func TestSyncTransfer_HashMismatch(t *testing.T) {
	svc, _ := newTestSyncService()

	// Hash doesn't match content
	h := sha256.Sum256([]byte("claimed"))
	chunk := &dist.Chunk{Hash: h, Type: dist.ChunkMethod, Content: "actual"}
	chunkBytes, err := dist.MarshalChunk(chunk)
	if err != nil {
		t.Fatalf("MarshalChunk: %v", err)
	}

	resp, err := svc.Transfer(bg(), connectReq(&maggiev1.TransferRequest{
		Chunks: [][]byte{chunkBytes},
	}))
	if err != nil {
		t.Fatalf("Transfer returned error: %v", err)
	}
	if resp.Msg.Accepted != 0 {
		t.Errorf("Accepted: got %d, want 0", resp.Msg.Accepted)
	}
	if resp.Msg.Rejected != 1 {
		t.Errorf("Rejected: got %d, want 1", resp.Msg.Rejected)
	}
	if len(resp.Msg.FailedHashes) != 1 {
		t.Errorf("FailedHashes: got %d, want 1", len(resp.Msg.FailedHashes))
	}
}

func TestSyncTransfer_InvalidCBOR(t *testing.T) {
	svc, _ := newTestSyncService()

	resp, err := svc.Transfer(bg(), connectReq(&maggiev1.TransferRequest{
		Chunks: [][]byte{[]byte("not cbor")},
	}))
	if err != nil {
		t.Fatalf("Transfer returned error: %v", err)
	}
	if resp.Msg.Rejected != 1 {
		t.Errorf("Rejected: got %d, want 1", resp.Msg.Rejected)
	}
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

// marshalCBOR is a test helper that encodes a value to CBOR.
func marshalCBOR(v interface{}) ([]byte, error) {
	// Use the same CBOR library as the dist package
	return dist.MarshalCapabilityManifest(v.(*dist.CapabilityManifest))
}
