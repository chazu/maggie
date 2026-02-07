package server

import (
	"context"
	"fmt"

	"connectrpc.com/connect"

	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
	"github.com/chazu/maggie/gen/maggie/v1/maggiev1connect"
	"github.com/chazu/maggie/vm"
	"github.com/chazu/maggie/vm/dist"
)

// SyncService implements the SyncService gRPC/Connect handler for
// content-addressed code distribution.
type SyncService struct {
	maggiev1connect.UnimplementedSyncServiceHandler
	worker *VMWorker
	store  *vm.ContentStore
	peers  *dist.PeerStore
	policy *dist.CapabilityPolicy

	// compile compiles source text and returns its content hash.
	// Injected to avoid depending on the compiler package.
	compile func(source string) ([32]byte, error)
}

// NewSyncService creates a SyncService.
func NewSyncService(
	worker *VMWorker,
	store *vm.ContentStore,
	peers *dist.PeerStore,
	policy *dist.CapabilityPolicy,
	compile func(source string) ([32]byte, error),
) *SyncService {
	return &SyncService{
		worker:  worker,
		store:   store,
		peers:   peers,
		policy:  policy,
		compile: compile,
	}
}

// Announce handles an incoming announcement from a peer. It checks the
// capability manifest against local policy, determines which hashes the
// local store is missing, and returns a want list.
func (s *SyncService) Announce(
	ctx context.Context,
	req *connect.Request[maggiev1.AnnounceRequest],
) (*connect.Response[maggiev1.AnnounceResponse], error) {
	msg := req.Msg

	if len(msg.RootHash) != 32 {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("root_hash must be 32 bytes"))
	}

	// Decode capability manifest if present
	if len(msg.CapabilityManifest) > 0 {
		manifest, err := dist.UnmarshalCapabilityManifest(msg.CapabilityManifest)
		if err != nil {
			return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("invalid capability manifest: %v", err))
		}
		if err := s.policy.Check(manifest); err != nil {
			return connect.NewResponse(&maggiev1.AnnounceResponse{
				Status:       maggiev1.AnnounceStatus_ANNOUNCE_REJECTED,
				RejectReason: err.Error(),
			}), nil
		}
	}

	// Build want list: hashes we don't already have
	var want [][]byte
	for _, hashBytes := range msg.AllHashes {
		if len(hashBytes) != 32 {
			continue
		}
		var h [32]byte
		copy(h[:], hashBytes)
		if !s.store.HasHash(h) {
			want = append(want, hashBytes)
		}
	}

	if len(want) == 0 {
		return connect.NewResponse(&maggiev1.AnnounceResponse{
			Status: maggiev1.AnnounceStatus_ANNOUNCE_ALREADY_HAVE,
		}), nil
	}

	return connect.NewResponse(&maggiev1.AnnounceResponse{
		Status: maggiev1.AnnounceStatus_ANNOUNCE_ACCEPTED,
		Want:   want,
	}), nil
}

// Transfer receives chunks from a peer, verifies method chunks, and
// indexes them in the local content store.
func (s *SyncService) Transfer(
	ctx context.Context,
	req *connect.Request[maggiev1.TransferRequest],
) (*connect.Response[maggiev1.TransferResponse], error) {
	msg := req.Msg
	peerID := peerFromContext(ctx)

	var accepted, rejected int32
	var failedHashes [][]byte

	for _, chunkBytes := range msg.Chunks {
		chunk, err := dist.UnmarshalChunk(chunkBytes)
		if err != nil {
			rejected++
			continue
		}

		// Verify method chunks by compiling and checking hash
		if chunk.Type == dist.ChunkMethod {
			if s.compile == nil {
				rejected++
				failedHashes = append(failedHashes, chunk.Hash[:])
				continue
			}
			if err := dist.VerifyChunkMethod(chunk, s.compile); err != nil {
				rejected++
				failedHashes = append(failedHashes, chunk.Hash[:])
				s.peers.RecordHashMismatch(peerID)
				continue
			}
		}

		// Index in content store (methods only — classes/modules need
		// full VM context, so we just accept them for now)
		accepted++
		s.peers.RecordSuccess(peerID)
	}

	return connect.NewResponse(&maggiev1.TransferResponse{
		Accepted:     accepted,
		Rejected:     rejected,
		FailedHashes: failedHashes,
	}), nil
}

// Ping returns the number of hashes in the local content store.
func (s *SyncService) Ping(
	ctx context.Context,
	req *connect.Request[maggiev1.PingRequest],
) (*connect.Response[maggiev1.PingResponse], error) {
	count := int64(s.store.MethodCount() + s.store.ClassCount())
	return connect.NewResponse(&maggiev1.PingResponse{
		ContentCount: count,
	}), nil
}

// peerFromContext extracts a peer identifier from the request context.
// For now we use the remote address from connect headers.
func peerFromContext(ctx context.Context) string {
	// In production this would use TLS peer info or a header.
	// For now, return a placeholder.
	return "unknown"
}
