package dist

import (
	"crypto/sha256"
	"fmt"
	"testing"

	"github.com/chazu/maggie/vm"
)

func TestChunk_CBORRoundTrip(t *testing.T) {
	h := sha256.Sum256([]byte("method-source"))
	dep := sha256.Sum256([]byte("dep"))

	c := &Chunk{
		Hash:         h,
		Type:         ChunkMethod,
		Content:      "method: x [ ^x + 1 ]",
		Dependencies: [][32]byte{dep},
		Capabilities: []string{"File"},
	}

	data, err := MarshalChunk(c)
	if err != nil {
		t.Fatalf("MarshalChunk: %v", err)
	}

	got, err := UnmarshalChunk(data)
	if err != nil {
		t.Fatalf("UnmarshalChunk: %v", err)
	}

	if got.Hash != c.Hash {
		t.Error("Hash mismatch")
	}
	if got.Type != c.Type {
		t.Error("Type mismatch")
	}
	if got.Content != c.Content {
		t.Errorf("Content: got %q, want %q", got.Content, c.Content)
	}
	if len(got.Dependencies) != 1 || got.Dependencies[0] != dep {
		t.Error("Dependencies mismatch")
	}
	if len(got.Capabilities) != 1 || got.Capabilities[0] != "File" {
		t.Error("Capabilities mismatch")
	}
}

func TestSyncAnnouncement_CBORRoundTrip(t *testing.T) {
	root := sha256.Sum256([]byte("root"))
	h1 := sha256.Sum256([]byte("h1"))
	h2 := sha256.Sum256([]byte("h2"))

	a := &SyncAnnouncement{
		RootHash:    root,
		AllHashes:   [][32]byte{h1, h2},
		Capability:  &CapabilityManifest{Required: []string{"HTTP"}},
		HashVersion: 1,
	}

	data, err := MarshalAnnouncement(a)
	if err != nil {
		t.Fatalf("MarshalAnnouncement: %v", err)
	}

	got, err := UnmarshalAnnouncement(data)
	if err != nil {
		t.Fatalf("UnmarshalAnnouncement: %v", err)
	}

	if got.RootHash != root {
		t.Error("RootHash mismatch")
	}
	if len(got.AllHashes) != 2 {
		t.Errorf("AllHashes: got %d, want 2", len(got.AllHashes))
	}
	if got.Capability == nil || len(got.Capability.Required) != 1 {
		t.Error("Capability mismatch")
	}
	if got.HashVersion != 1 {
		t.Errorf("HashVersion: got %d, want 1", got.HashVersion)
	}
}

func TestSyncRequest_CBORRoundTrip(t *testing.T) {
	have := sha256.Sum256([]byte("have"))
	want := sha256.Sum256([]byte("want"))

	r := &SyncRequest{
		Have: [][32]byte{have},
		Want: [][32]byte{want},
	}

	data, err := MarshalSyncRequest(r)
	if err != nil {
		t.Fatalf("MarshalSyncRequest: %v", err)
	}

	got, err := UnmarshalSyncRequest(data)
	if err != nil {
		t.Fatalf("UnmarshalSyncRequest: %v", err)
	}

	if len(got.Have) != 1 || got.Have[0] != have {
		t.Error("Have mismatch")
	}
	if len(got.Want) != 1 || got.Want[0] != want {
		t.Error("Want mismatch")
	}
}

func TestSyncResponse_CBORRoundTrip(t *testing.T) {
	h := sha256.Sum256([]byte("m"))

	r := &SyncResponse{
		Chunks: []Chunk{
			{Hash: h, Type: ChunkMethod, Content: "m"},
		},
	}

	data, err := MarshalSyncResponse(r)
	if err != nil {
		t.Fatalf("MarshalSyncResponse: %v", err)
	}

	got, err := UnmarshalSyncResponse(data)
	if err != nil {
		t.Fatalf("UnmarshalSyncResponse: %v", err)
	}

	if len(got.Chunks) != 1 {
		t.Fatalf("Chunks: got %d, want 1", len(got.Chunks))
	}
	if got.Chunks[0].Hash != h {
		t.Error("Chunk hash mismatch")
	}
}

func TestAnnounceResponse_CBORRoundTrip(t *testing.T) {
	want := sha256.Sum256([]byte("want"))

	r := &AnnounceResponse{
		Status: AnnounceAccepted,
		Want:   [][32]byte{want},
	}

	data, err := MarshalAnnounceResponse(r)
	if err != nil {
		t.Fatalf("MarshalAnnounceResponse: %v", err)
	}

	got, err := UnmarshalAnnounceResponse(data)
	if err != nil {
		t.Fatalf("UnmarshalAnnounceResponse: %v", err)
	}

	if got.Status != AnnounceAccepted {
		t.Errorf("Status: got %d, want %d", got.Status, AnnounceAccepted)
	}
	if len(got.Want) != 1 {
		t.Fatalf("Want: got %d, want 1", len(got.Want))
	}
}

func TestVerifyChunkMethod_Valid(t *testing.T) {
	h := sha256.Sum256([]byte("source"))
	c := &Chunk{Hash: h, Type: ChunkMethod, Content: "source"}

	err := VerifyChunkMethod(c, func(source string) ([32]byte, error) {
		return sha256.Sum256([]byte(source)), nil
	})
	if err != nil {
		t.Errorf("VerifyChunkMethod should succeed: %v", err)
	}
}

func TestVerifyChunkMethod_Mismatch(t *testing.T) {
	h := sha256.Sum256([]byte("claimed"))
	c := &Chunk{Hash: h, Type: ChunkMethod, Content: "actual"}

	err := VerifyChunkMethod(c, func(source string) ([32]byte, error) {
		return sha256.Sum256([]byte(source)), nil
	})
	if err == nil {
		t.Error("VerifyChunkMethod should fail on hash mismatch")
	}
}

func TestVerifyChunkMethod_CompileError(t *testing.T) {
	c := &Chunk{Type: ChunkMethod, Content: "bad"}

	err := VerifyChunkMethod(c, func(source string) ([32]byte, error) {
		return [32]byte{}, fmt.Errorf("syntax error")
	})
	if err == nil {
		t.Error("VerifyChunkMethod should propagate compile errors")
	}
}

func TestVerifyChunkMethod_WrongType(t *testing.T) {
	c := &Chunk{Type: ChunkClass}

	err := VerifyChunkMethod(c, func(source string) ([32]byte, error) {
		return [32]byte{}, nil
	})
	if err == nil {
		t.Error("VerifyChunkMethod should reject non-method chunks")
	}
}

func TestUnmarshalChunk_InvalidData(t *testing.T) {
	_, err := UnmarshalChunk([]byte("not cbor"))
	if err == nil {
		t.Error("UnmarshalChunk should fail on invalid data")
	}
}

// ---------------------------------------------------------------------------
// VerifyChunkClass
// ---------------------------------------------------------------------------

func TestVerifyChunkClass_Valid(t *testing.T) {
	store := vm.NewContentStore()

	// Pre-index method dependencies
	m1h := sha256.Sum256([]byte("m1"))
	m1 := vm.NewCompiledMethodBuilder("m1", 0).Build()
	m1.SetContentHash(m1h)
	store.IndexMethod(m1)

	classHash := sha256.Sum256([]byte("MyClass"))
	c := &Chunk{
		Hash:         classHash,
		Type:         ChunkClass,
		Content:      "MyClass",
		Dependencies: [][32]byte{m1h},
	}

	err := VerifyChunkClass(c, store)
	if err != nil {
		t.Errorf("VerifyChunkClass should succeed: %v", err)
	}
}

func TestVerifyChunkClass_MissingDep(t *testing.T) {
	store := vm.NewContentStore()

	missingDep := sha256.Sum256([]byte("not-in-store"))
	classHash := sha256.Sum256([]byte("MyClass"))
	c := &Chunk{
		Hash:         classHash,
		Type:         ChunkClass,
		Content:      "MyClass",
		Dependencies: [][32]byte{missingDep},
	}

	err := VerifyChunkClass(c, store)
	if err == nil {
		t.Error("VerifyChunkClass should fail on missing dependency")
	}
}

func TestVerifyChunkClass_WrongType(t *testing.T) {
	store := vm.NewContentStore()
	c := &Chunk{Type: ChunkMethod}

	err := VerifyChunkClass(c, store)
	if err == nil {
		t.Error("VerifyChunkClass should reject non-class chunks")
	}
}

// ---------------------------------------------------------------------------
// VerifyChunkModule
// ---------------------------------------------------------------------------

func TestVerifyChunkModule_Valid(t *testing.T) {
	store := vm.NewContentStore()

	// Pre-index class dependency
	ch := sha256.Sum256([]byte("class1"))
	store.IndexClass(&vm.ClassDigest{Name: "C1", Hash: ch})

	moduleHash := sha256.Sum256([]byte("MyModule"))
	c := &Chunk{
		Hash:         moduleHash,
		Type:         ChunkModule,
		Content:      "MyModule",
		Dependencies: [][32]byte{ch},
	}

	err := VerifyChunkModule(c, store)
	if err != nil {
		t.Errorf("VerifyChunkModule should succeed: %v", err)
	}
}

func TestVerifyChunkModule_MissingDep(t *testing.T) {
	store := vm.NewContentStore()

	missingDep := sha256.Sum256([]byte("not-in-store"))
	moduleHash := sha256.Sum256([]byte("MyModule"))
	c := &Chunk{
		Hash:         moduleHash,
		Type:         ChunkModule,
		Content:      "MyModule",
		Dependencies: [][32]byte{missingDep},
	}

	err := VerifyChunkModule(c, store)
	if err == nil {
		t.Error("VerifyChunkModule should fail on missing dependency")
	}
}
