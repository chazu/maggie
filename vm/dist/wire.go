package dist

import (
	"fmt"

	"github.com/chazu/maggie/vm"
	"github.com/fxamacker/cbor/v2"
)

// cborOpts returns CBOR encoding options with canonical mode for
// deterministic encoding.
var cborEncMode cbor.EncMode

func init() {
	em, err := cbor.CanonicalEncOptions().EncMode()
	if err != nil {
		panic(fmt.Sprintf("dist: failed to create CBOR enc mode: %v", err))
	}
	cborEncMode = em
}

// MarshalChunk serializes a Chunk to CBOR bytes.
func MarshalChunk(c *Chunk) ([]byte, error) {
	return cborEncMode.Marshal(c)
}

// UnmarshalChunk deserializes a Chunk from CBOR bytes.
func UnmarshalChunk(data []byte) (*Chunk, error) {
	var c Chunk
	if err := cbor.Unmarshal(data, &c); err != nil {
		return nil, fmt.Errorf("dist: unmarshal chunk: %w", err)
	}
	return &c, nil
}

// MarshalAnnouncement serializes a SyncAnnouncement to CBOR bytes.
func MarshalAnnouncement(a *SyncAnnouncement) ([]byte, error) {
	return cborEncMode.Marshal(a)
}

// UnmarshalAnnouncement deserializes a SyncAnnouncement from CBOR bytes.
func UnmarshalAnnouncement(data []byte) (*SyncAnnouncement, error) {
	var a SyncAnnouncement
	if err := cbor.Unmarshal(data, &a); err != nil {
		return nil, fmt.Errorf("dist: unmarshal announcement: %w", err)
	}
	return &a, nil
}

// MarshalSyncRequest serializes a SyncRequest to CBOR bytes.
func MarshalSyncRequest(r *SyncRequest) ([]byte, error) {
	return cborEncMode.Marshal(r)
}

// UnmarshalSyncRequest deserializes a SyncRequest from CBOR bytes.
func UnmarshalSyncRequest(data []byte) (*SyncRequest, error) {
	var r SyncRequest
	if err := cbor.Unmarshal(data, &r); err != nil {
		return nil, fmt.Errorf("dist: unmarshal sync request: %w", err)
	}
	return &r, nil
}

// MarshalSyncResponse serializes a SyncResponse to CBOR bytes.
func MarshalSyncResponse(r *SyncResponse) ([]byte, error) {
	return cborEncMode.Marshal(r)
}

// UnmarshalSyncResponse deserializes a SyncResponse from CBOR bytes.
func UnmarshalSyncResponse(data []byte) (*SyncResponse, error) {
	var r SyncResponse
	if err := cbor.Unmarshal(data, &r); err != nil {
		return nil, fmt.Errorf("dist: unmarshal sync response: %w", err)
	}
	return &r, nil
}

// MarshalAnnounceResponse serializes an AnnounceResponse to CBOR bytes.
func MarshalAnnounceResponse(r *AnnounceResponse) ([]byte, error) {
	return cborEncMode.Marshal(r)
}

// UnmarshalAnnounceResponse deserializes an AnnounceResponse from CBOR bytes.
func UnmarshalAnnounceResponse(data []byte) (*AnnounceResponse, error) {
	var r AnnounceResponse
	if err := cbor.Unmarshal(data, &r); err != nil {
		return nil, fmt.Errorf("dist: unmarshal announce response: %w", err)
	}
	return &r, nil
}

// MarshalCapabilityManifest serializes a CapabilityManifest to CBOR bytes.
func MarshalCapabilityManifest(m *CapabilityManifest) ([]byte, error) {
	return cborEncMode.Marshal(m)
}

// UnmarshalCapabilityManifest deserializes a CapabilityManifest from CBOR bytes.
func UnmarshalCapabilityManifest(data []byte) (*CapabilityManifest, error) {
	var m CapabilityManifest
	if err := cbor.Unmarshal(data, &m); err != nil {
		return nil, fmt.Errorf("dist: unmarshal capability manifest: %w", err)
	}
	return &m, nil
}

// CompileResult holds both semantic and typed content hashes from compilation.
type CompileResult struct {
	SemanticHash [32]byte
	TypedHash    [32]byte // zero if source has no type annotations
}

// UnmarshalCBOR is a generic CBOR unmarshal helper for the server package.
func UnmarshalCBOR(data []byte, v interface{}) error {
	return cbor.Unmarshal(data, v)
}

// VerifyChunkMethod compiles source from a chunk and verifies that the
// resulting content hash matches the chunk's declared hash.
//
// The compile function is injected to avoid the dist package depending on
// the compiler package. It should compile the source text and return both
// semantic and typed content hashes.
func VerifyChunkMethod(c *Chunk, compile func(source string) (CompileResult, error)) error {
	if c.Type != ChunkMethod {
		return fmt.Errorf("dist: cannot verify non-method chunk (type=%d)", c.Type)
	}
	result, err := compile(c.Content)
	if err != nil {
		return fmt.Errorf("dist: compile failed: %w", err)
	}
	if result.SemanticHash != c.Hash {
		return fmt.Errorf("dist: semantic hash mismatch: declared %x, computed %x", c.Hash, result.SemanticHash)
	}
	// Verify typed hash if both the chunk and compiled result have one
	if c.TypedHash != ([32]byte{}) && result.TypedHash != ([32]byte{}) {
		if result.TypedHash != c.TypedHash {
			return fmt.Errorf("dist: typed hash mismatch: declared %x, computed %x", c.TypedHash, result.TypedHash)
		}
	}
	return nil
}

// VerifyChunkClass verifies that a class chunk's declared dependency hashes
// (method hashes) all exist in the content store. This ensures we have all
// methods before accepting the class digest.
func VerifyChunkClass(c *Chunk, store *vm.ContentStore) error {
	if c.Type != ChunkClass {
		return fmt.Errorf("dist: cannot verify non-class chunk (type=%d)", c.Type)
	}
	for _, dep := range c.Dependencies {
		if !store.HasHash(dep) {
			return fmt.Errorf("dist: class chunk missing dependency %x", dep)
		}
	}
	return nil
}

// VerifyChunkModule verifies that a module chunk's declared dependency hashes
// (class hashes) all exist in the content store.
func VerifyChunkModule(c *Chunk, store *vm.ContentStore) error {
	if c.Type != ChunkModule {
		return fmt.Errorf("dist: cannot verify non-module chunk (type=%d)", c.Type)
	}
	for _, dep := range c.Dependencies {
		if !store.HasHash(dep) {
			return fmt.Errorf("dist: module chunk missing dependency %x", dep)
		}
	}
	return nil
}
