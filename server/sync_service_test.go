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
	svc, store := newTestSyncService()

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
	// Verify the method is now indexed in the store
	if !store.HasHash(h) {
		t.Error("Accepted method should be indexed in content store")
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
// Transfer — nil compile func
// ---------------------------------------------------------------------------

func TestSyncTransfer_NilCompileFunc(t *testing.T) {
	store := vm.NewContentStore()
	peers := dist.NewPeerStore()
	policy := dist.NewPermissivePolicy()
	svc := NewSyncService(testWorker, store, peers, policy, nil) // nil compile

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
	if resp.Msg.Accepted != 0 {
		t.Errorf("Accepted: got %d, want 0 (nil compile func)", resp.Msg.Accepted)
	}
	if resp.Msg.Rejected != 1 {
		t.Errorf("Rejected: got %d, want 1", resp.Msg.Rejected)
	}
}

// ---------------------------------------------------------------------------
// Transfer — class chunk with dependencies
// ---------------------------------------------------------------------------

func TestSyncTransfer_ClassChunk(t *testing.T) {
	svc, store := newTestSyncService()

	// Pre-index method dependencies
	m1h := sha256.Sum256([]byte("method1"))
	m1 := vm.NewCompiledMethodBuilder("m1", 0).Build()
	m1.SetContentHash(m1h)
	store.IndexMethod(m1)

	m2h := sha256.Sum256([]byte("method2"))
	m2 := vm.NewCompiledMethodBuilder("m2", 0).Build()
	m2.SetContentHash(m2h)
	store.IndexMethod(m2)

	// Create class chunk with pre-indexed method deps
	classHash := sha256.Sum256([]byte("MyClass"))
	chunk := &dist.Chunk{
		Hash:         classHash,
		Type:         dist.ChunkClass,
		Content:      "MyClass",
		Dependencies: [][32]byte{m1h, m2h},
	}
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
	if !store.HasHash(classHash) {
		t.Error("Class should be indexed in content store")
	}
}

func TestSyncTransfer_ClassChunk_MissingDep(t *testing.T) {
	svc, _ := newTestSyncService()

	// Class chunk with dependency that's NOT in the store
	missingDep := sha256.Sum256([]byte("not-indexed"))
	classHash := sha256.Sum256([]byte("MyClass"))
	chunk := &dist.Chunk{
		Hash:         classHash,
		Type:         dist.ChunkClass,
		Content:      "MyClass",
		Dependencies: [][32]byte{missingDep},
	}
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
	if resp.Msg.Rejected != 1 {
		t.Errorf("Rejected: got %d, want 1", resp.Msg.Rejected)
	}
}

// ---------------------------------------------------------------------------
// Banned peer
// ---------------------------------------------------------------------------

func TestSyncAnnounce_BannedPeer(t *testing.T) {
	store := vm.NewContentStore()
	peers := dist.NewPeerStore()
	policy := dist.NewPermissivePolicy()
	svc := NewSyncService(testWorker, store, peers, policy, nil)

	// Ban the peer (default threshold = 3 mismatches)
	peers.RecordHashMismatch("unknown")
	peers.RecordHashMismatch("unknown")
	peers.RecordHashMismatch("unknown")

	rootHash := sha256.Sum256([]byte("root"))
	_, err := svc.Announce(bg(), connectReq(&maggiev1.AnnounceRequest{
		RootHash:  rootHash[:],
		AllHashes: [][]byte{rootHash[:]},
	}))
	if err == nil {
		t.Fatal("Announce from banned peer should return error")
	}
}

func TestSyncTransfer_BannedPeer(t *testing.T) {
	store := vm.NewContentStore()
	peers := dist.NewPeerStore()
	policy := dist.NewPermissivePolicy()
	compile := func(source string) ([32]byte, error) {
		return sha256.Sum256([]byte(source)), nil
	}
	svc := NewSyncService(testWorker, store, peers, policy, compile)

	// Ban the peer
	peers.RecordHashMismatch("unknown")
	peers.RecordHashMismatch("unknown")
	peers.RecordHashMismatch("unknown")

	source := "source"
	h := sha256.Sum256([]byte(source))
	chunk := &dist.Chunk{Hash: h, Type: dist.ChunkMethod, Content: source}
	chunkBytes, _ := dist.MarshalChunk(chunk)

	_, err := svc.Transfer(bg(), connectReq(&maggiev1.TransferRequest{
		Chunks: [][]byte{chunkBytes},
	}))
	if err == nil {
		t.Fatal("Transfer from banned peer should return error")
	}
}

// ---------------------------------------------------------------------------
// Serve (pull)
// ---------------------------------------------------------------------------

func TestSyncServe_ReturnsTransitiveClosure(t *testing.T) {
	svc, store := newTestSyncService()

	// Index a class with two method dependencies
	m1h := sha256.Sum256([]byte("method1"))
	m1 := vm.NewCompiledMethodBuilder("m1", 0).Build()
	m1.Source = "method1"
	m1.SetContentHash(m1h)
	store.IndexMethod(m1)

	m2h := sha256.Sum256([]byte("method2"))
	m2 := vm.NewCompiledMethodBuilder("m2", 0).Build()
	m2.Source = "method2"
	m2.SetContentHash(m2h)
	store.IndexMethod(m2)

	classDigest := &vm.ClassDigest{
		Name:         "MyClass",
		MethodHashes: [][32]byte{m1h, m2h},
	}
	classDigest.Hash = vm.HashClass("MyClass", "", "", nil, nil, "", classDigest.MethodHashes)
	store.IndexClass(classDigest)

	resp, err := svc.Serve(bg(), connectReq(&maggiev1.ServeRequest{
		RootHash: classDigest.Hash[:],
	}))
	if err != nil {
		t.Fatalf("Serve returned error: %v", err)
	}

	// Available should include root + both methods (3 total)
	if len(resp.Msg.Available) != 3 {
		t.Errorf("Available: got %d, want 3", len(resp.Msg.Available))
	}
	// Chunks should include method chunks + class chunk (3 total)
	if len(resp.Msg.Chunks) != 3 {
		t.Errorf("Chunks: got %d, want 3", len(resp.Msg.Chunks))
	}
}

func TestSyncServe_FiltersByHave(t *testing.T) {
	svc, store := newTestSyncService()

	m1h := sha256.Sum256([]byte("method1"))
	m1 := vm.NewCompiledMethodBuilder("m1", 0).Build()
	m1.Source = "method1"
	m1.SetContentHash(m1h)
	store.IndexMethod(m1)

	m2h := sha256.Sum256([]byte("method2"))
	m2 := vm.NewCompiledMethodBuilder("m2", 0).Build()
	m2.Source = "method2"
	m2.SetContentHash(m2h)
	store.IndexMethod(m2)

	classDigest := &vm.ClassDigest{
		Name:         "MyClass",
		MethodHashes: [][32]byte{m1h, m2h},
	}
	classDigest.Hash = vm.HashClass("MyClass", "", "", nil, nil, "", classDigest.MethodHashes)
	store.IndexClass(classDigest)

	// Requester already has method1
	resp, err := svc.Serve(bg(), connectReq(&maggiev1.ServeRequest{
		RootHash: classDigest.Hash[:],
		Have:     [][]byte{m1h[:]},
	}))
	if err != nil {
		t.Fatalf("Serve returned error: %v", err)
	}

	// Available should still list all 3
	if len(resp.Msg.Available) != 3 {
		t.Errorf("Available: got %d, want 3", len(resp.Msg.Available))
	}
	// Chunks should only include what's missing (method2 + class = 2)
	if len(resp.Msg.Chunks) != 2 {
		t.Errorf("Chunks: got %d, want 2", len(resp.Msg.Chunks))
	}
}

func TestSyncServe_UnknownRoot(t *testing.T) {
	svc, _ := newTestSyncService()

	unknown := sha256.Sum256([]byte("not-in-store"))
	_, err := svc.Serve(bg(), connectReq(&maggiev1.ServeRequest{
		RootHash: unknown[:],
	}))
	if err == nil {
		t.Fatal("Serve with unknown root should return error")
	}
}

func TestSyncServe_ChunksInOrder(t *testing.T) {
	svc, store := newTestSyncService()

	// Index methods and class
	m1h := sha256.Sum256([]byte("m1"))
	m1 := vm.NewCompiledMethodBuilder("m1", 0).Build()
	m1.Source = "m1"
	m1.SetContentHash(m1h)
	store.IndexMethod(m1)

	classDigest := &vm.ClassDigest{
		Name:         "C",
		MethodHashes: [][32]byte{m1h},
	}
	classDigest.Hash = vm.HashClass("C", "", "", nil, nil, "", classDigest.MethodHashes)
	store.IndexClass(classDigest)

	resp, err := svc.Serve(bg(), connectReq(&maggiev1.ServeRequest{
		RootHash: classDigest.Hash[:],
	}))
	if err != nil {
		t.Fatalf("Serve returned error: %v", err)
	}

	if len(resp.Msg.Chunks) < 2 {
		t.Fatalf("Expected at least 2 chunks, got %d", len(resp.Msg.Chunks))
	}

	// First chunk should be a method, last should be a class
	first, err := dist.UnmarshalChunk(resp.Msg.Chunks[0])
	if err != nil {
		t.Fatalf("Unmarshal first chunk: %v", err)
	}
	last, err := dist.UnmarshalChunk(resp.Msg.Chunks[len(resp.Msg.Chunks)-1])
	if err != nil {
		t.Fatalf("Unmarshal last chunk: %v", err)
	}

	if first.Type != dist.ChunkMethod {
		t.Errorf("First chunk should be method, got type %d", first.Type)
	}
	if last.Type != dist.ChunkClass {
		t.Errorf("Last chunk should be class, got type %d", last.Type)
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
