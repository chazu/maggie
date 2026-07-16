package dist

import (
	"crypto/sha256"
	"testing"
)

// These decoders run on raw bytes from remote peers BEFORE any signature or
// trust check (UnmarshalEnvelope in particular gates DeliverMessage), so they
// are an unauthenticated attack surface. The contract under fuzzing is simply:
// never panic, never OOM — malformed input must return an error.

func FuzzUnmarshalEnvelope(f *testing.F) {
	// Seed with a valid signed envelope.
	id, _ := GenerateIdentity()
	env := &MessageEnvelope{
		TargetName: "worker",
		Selector:   "do:",
		Payload:    []byte{1, 2, 3},
		Nonce:      7,
	}
	if id != nil {
		_ = SignEnvelope(env, id)
	}
	if b, err := MarshalEnvelope(env); err == nil {
		f.Add(b)
	}
	f.Add([]byte{})
	f.Add([]byte("not cbor"))

	f.Fuzz(func(t *testing.T, data []byte) {
		if e, err := UnmarshalEnvelope(data); err == nil && e != nil {
			// Verifying arbitrary decoded envelopes must also not panic.
			_ = e.Verify()
		}
	})
}

func FuzzUnmarshalChunk(f *testing.F) {
	h := sha256.Sum256([]byte("src"))
	c := &Chunk{Hash: h, Type: ChunkMethod, Content: "method: x [ ^1 ]"}
	if b, err := MarshalChunk(c); err == nil {
		f.Add(b)
	}
	f.Add([]byte{})
	f.Fuzz(func(t *testing.T, data []byte) {
		_, _ = UnmarshalChunk(data)
	})
}

func FuzzUnmarshalAnnouncement(f *testing.F) {
	root := sha256.Sum256([]byte("root"))
	a := &SyncAnnouncement{RootHash: root, AllHashes: [][32]byte{root}}
	if b, err := MarshalAnnouncement(a); err == nil {
		f.Add(b)
	}
	f.Add([]byte{})
	f.Fuzz(func(t *testing.T, data []byte) {
		_, _ = UnmarshalAnnouncement(data)
	})
}
