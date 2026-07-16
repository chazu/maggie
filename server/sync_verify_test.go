package server

import (
	"crypto/sha256"
	"strings"
	"testing"

	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
	"github.com/chazu/maggie/pipeline"
	"github.com/chazu/maggie/vm"
	"github.com/chazu/maggie/vm/dist"
)

// anonPeerID reproduces peerNodeIDFromRequest's fallback for a request with
// no identifying headers, so tests can inspect the peer's trust record.
func anonPeerID() dist.NodeID {
	h := sha256.Sum256([]byte("ip:unknown"))
	return dist.NodeIDFromBytes(h[:])
}

// newVerifyTestSetup compiles a two-level hierarchy (ivars, inheritance, a
// class-side method) on a sender VM and returns the marshaled chunk batch
// plus a receiving SyncService wired with the real verification compile
// function (pipeline.VerifyCompileFunc) — the production configuration.
func newVerifyTestSetup(t *testing.T) (chunks [][]byte, methodChunks []*dist.Chunk, svc *SyncService, trust *dist.TrustStore) {
	t.Helper()

	vmA := newTestVMForServer(t)
	pipeA := &pipeline.Pipeline{VM: vmA}

	base := `XferVerifyBase subclass: Object
  instanceVars: baseCount

  method: baseBump [ baseCount := baseCount + 1. ^baseCount ]
`
	sub := `XferVerifySub subclass: XferVerifyBase
  instanceVars: subCount

  method: bump [ subCount := (subCount + baseCount) + 1. ^subCount ]
  classMethod: makeOne [ ^XferVerifySub new ]
`
	if _, err := pipeA.CompileSourceFile(base, "xfer_verify_base.mag", ""); err != nil {
		t.Fatalf("compile base: %v", err)
	}
	if _, err := pipeA.CompileSourceFile(sub, "xfer_verify_sub.mag", ""); err != nil {
		t.Fatalf("compile sub: %v", err)
	}

	storeA := vmA.ContentStore()
	for _, className := range []string{"XferVerifyBase", "XferVerifySub"} {
		d := storeA.LookupClassByName(className)
		if d == nil {
			t.Fatalf("digest for %s not found", className)
		}
		for _, mh := range d.MethodHashes {
			m := storeA.LookupMethod(mh)
			if m == nil {
				t.Fatalf("method %x not found", mh[:8])
			}
			chunk := dist.MethodToChunk(m, nil)
			methodChunks = append(methodChunks, chunk)
			data, err := dist.MarshalChunk(chunk)
			if err != nil {
				t.Fatalf("marshal method chunk: %v", err)
			}
			chunks = append(chunks, data)
		}
		data, err := dist.MarshalChunk(dist.ClassToChunk(d, nil))
		if err != nil {
			t.Fatalf("marshal class chunk: %v", err)
		}
		chunks = append(chunks, data)
	}

	// Receiver: empty content store, so class context must be recovered
	// from the batch itself.
	vmB := newTestVMForServer(t)
	worker := NewVMWorker(vmB)
	t.Cleanup(worker.Stop)
	trust = dist.NewPermissiveTrustStore()
	svc = NewSyncService(worker, vm.NewContentStore(), trust, pipeline.VerifyCompileFunc(vmB), nil)
	return chunks, methodChunks, svc, trust
}

// TestTransfer_VerifiesPipelineChunksWithClassContext is the end-to-end
// regression test for the verifier divergence: pipeline-compiled chunks
// (ivar references, inherited ivars, class-side methods) must verify on a
// receiver running the production compile function, with class context
// recovered from digests in the same transfer batch.
func TestTransfer_VerifiesPipelineChunksWithClassContext(t *testing.T) {
	chunks, _, svc, trust := newVerifyTestSetup(t)

	resp, err := svc.Transfer(bg(), connectReq(&maggiev1.TransferRequest{Chunks: chunks}))
	if err != nil {
		t.Fatalf("Transfer: %v", err)
	}
	if resp.Msg.Rejected != 0 {
		t.Fatalf("Rejected: got %d, want 0 (failed hashes: %d)", resp.Msg.Rejected, len(resp.Msg.FailedHashes))
	}
	if int(resp.Msg.Accepted) != len(chunks) {
		t.Errorf("Accepted: got %d, want %d", resp.Msg.Accepted, len(chunks))
	}
	if p := trust.Peer(anonPeerID()); p != nil && p.HashMismatches != 0 {
		t.Errorf("HashMismatches: got %d, want 0", p.HashMismatches)
	}
}

// TestTransfer_TamperedChunkRecordsMismatch: a chunk whose source was altered
// after hashing must be rejected AND counted as a hash-mismatch strike.
func TestTransfer_TamperedChunkRecordsMismatch(t *testing.T) {
	chunks, methodChunks, svc, trust := newVerifyTestSetup(t)

	// Tamper with a method that parses cleanly but hashes differently.
	var tampered *dist.Chunk
	for _, mc := range methodChunks {
		if strings.Contains(mc.Content, "+ 1") {
			cp := *mc
			cp.Content = strings.Replace(cp.Content, "+ 1", "+ 2", 1)
			tampered = &cp
			break
		}
	}
	if tampered == nil {
		t.Fatal("no tamperable method chunk found")
	}
	data, err := dist.MarshalChunk(tampered)
	if err != nil {
		t.Fatalf("marshal tampered chunk: %v", err)
	}
	batch := append([][]byte{data}, chunks...) // class context present in batch

	resp, err := svc.Transfer(bg(), connectReq(&maggiev1.TransferRequest{Chunks: batch}))
	if err != nil {
		t.Fatalf("Transfer: %v", err)
	}
	if resp.Msg.Rejected != 1 {
		t.Errorf("Rejected: got %d, want 1", resp.Msg.Rejected)
	}
	p := trust.Peer(anonPeerID())
	if p == nil || p.HashMismatches != 1 {
		got := -1
		if p != nil {
			got = p.HashMismatches
		}
		t.Errorf("HashMismatches: got %d, want 1", got)
	}
}

// TestTransfer_MissingClassContextRejectsWithoutStrike: a method chunk whose
// owning class is not in the batch, store, or image must be rejected — but
// NOT counted as a hash mismatch, since missing context is not evidence of
// tampering and mismatch strikes lead to bans.
func TestTransfer_MissingClassContextRejectsWithoutStrike(t *testing.T) {
	_, _, svc, trust := newVerifyTestSetup(t)

	orphanHash := sha256.Sum256([]byte("orphan"))
	orphan := &dist.Chunk{
		Hash:      orphanHash,
		Type:      dist.ChunkMethod,
		Content:   "method: foo [ ^1 ]",
		Selector:  "foo",
		ClassName: "NoSuchClassAnywhere",
	}
	data, err := dist.MarshalChunk(orphan)
	if err != nil {
		t.Fatalf("marshal orphan chunk: %v", err)
	}

	resp, err := svc.Transfer(bg(), connectReq(&maggiev1.TransferRequest{Chunks: [][]byte{data}}))
	if err != nil {
		t.Fatalf("Transfer: %v", err)
	}
	if resp.Msg.Rejected != 1 {
		t.Errorf("Rejected: got %d, want 1", resp.Msg.Rejected)
	}
	if p := trust.Peer(anonPeerID()); p != nil && p.HashMismatches != 0 {
		t.Errorf("HashMismatches: got %d, want 0 — missing context must not strike", p.HashMismatches)
	}
}
