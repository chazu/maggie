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
	peerID := peerFromRequest(req)

	if s.peers.IsBanned(peerID) {
		return nil, connect.NewError(connect.CodePermissionDenied, fmt.Errorf("peer is banned"))
	}

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
	peerID := peerFromRequest(req)

	if s.peers.IsBanned(peerID) {
		return nil, connect.NewError(connect.CodePermissionDenied, fmt.Errorf("peer is banned"))
	}

	var accepted, rejected int32
	var failedHashes [][]byte

	for _, chunkBytes := range msg.Chunks {
		chunk, err := dist.UnmarshalChunk(chunkBytes)
		if err != nil {
			rejected++
			continue
		}

		switch chunk.Type {
		case dist.ChunkMethod:
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
			// Verification passed — index in content store
			s.indexVerifiedMethod(chunk)
			accepted++
			s.peers.RecordSuccess(peerID)

		case dist.ChunkClass:
			if err := dist.VerifyChunkClass(chunk, s.store); err != nil {
				rejected++
				failedHashes = append(failedHashes, chunk.Hash[:])
				s.peers.RecordHashMismatch(peerID)
				continue
			}
			s.indexVerifiedClass(chunk)
			accepted++
			s.peers.RecordSuccess(peerID)

		case dist.ChunkModule:
			if err := dist.VerifyChunkModule(chunk, s.store); err != nil {
				rejected++
				failedHashes = append(failedHashes, chunk.Hash[:])
				s.peers.RecordHashMismatch(peerID)
				continue
			}
			accepted++
			s.peers.RecordSuccess(peerID)

		default:
			rejected++
		}
	}

	return connect.NewResponse(&maggiev1.TransferResponse{
		Accepted:     accepted,
		Rejected:     rejected,
		FailedHashes: failedHashes,
	}), nil
}

// indexVerifiedMethod creates a CompiledMethod stub from a verified chunk
// and indexes it in the content store. The method has source text and hash
// but no compiled bytecode — it serves as proof we received verified content.
func (s *SyncService) indexVerifiedMethod(chunk *dist.Chunk) {
	m := &vm.CompiledMethod{Source: chunk.Content}
	m.SetContentHash(chunk.Hash)
	s.store.IndexMethod(m)
}

// indexVerifiedClass indexes a class digest from a verified chunk.
func (s *SyncService) indexVerifiedClass(chunk *dist.Chunk) {
	d := &vm.ClassDigest{
		Name:         chunk.Content,
		Hash:         chunk.Hash,
		MethodHashes: chunk.Dependencies,
	}
	s.store.IndexClass(d)
}

// Serve returns content reachable from a root hash, minus what the requester
// already has. This is the pull-side counterpart to Announce+Transfer.
func (s *SyncService) Serve(
	ctx context.Context,
	req *connect.Request[maggiev1.ServeRequest],
) (*connect.Response[maggiev1.ServeResponse], error) {
	msg := req.Msg
	peerID := peerFromRequest(req)

	if s.peers.IsBanned(peerID) {
		return nil, connect.NewError(connect.CodePermissionDenied, fmt.Errorf("peer is banned"))
	}

	if len(msg.RootHash) != 32 {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("root_hash must be 32 bytes"))
	}

	var rootHash [32]byte
	copy(rootHash[:], msg.RootHash)

	if !s.store.HasHash(rootHash) {
		return nil, connect.NewError(connect.CodeNotFound, fmt.Errorf("root hash not found"))
	}

	// Compute transitive closure from root
	allHashes := dist.TransitiveClosure(rootHash, s.store)

	// Build set of hashes the requester already has
	haveSet := make(map[[32]byte]bool, len(msg.Have))
	for _, hBytes := range msg.Have {
		if len(hBytes) == 32 {
			var h [32]byte
			copy(h[:], hBytes)
			haveSet[h] = true
		}
	}

	// Return available hashes and build chunks for what they're missing
	var available [][]byte
	var methodChunks [][]byte
	var classChunks [][]byte

	for _, h := range allHashes {
		hCopy := h
		available = append(available, hCopy[:])

		if haveSet[h] {
			continue // requester already has this
		}

		if m := s.store.LookupMethod(h); m != nil {
			chunk := dist.MethodToChunk(m, nil)
			data, err := dist.MarshalChunk(chunk)
			if err == nil {
				methodChunks = append(methodChunks, data)
			}
		} else if d := s.store.LookupClass(h); d != nil {
			chunk := dist.ClassToChunk(d, d.Name, nil)
			data, err := dist.MarshalChunk(chunk)
			if err == nil {
				classChunks = append(classChunks, data)
			}
		}
	}

	// Methods first, then classes (dependency order)
	chunks := append(methodChunks, classChunks...)

	return connect.NewResponse(&maggiev1.ServeResponse{
		Available: available,
		Chunks:    chunks,
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

// peerFromRequest extracts a peer identifier from a Connect request's headers.
// Falls back to "unknown" if no address is available.
func peerFromRequest[T any](req *connect.Request[T]) string {
	// Try X-Forwarded-For (common behind proxies)
	if xff := req.Header().Get("X-Forwarded-For"); xff != "" {
		return xff
	}
	// Try the connect peer header
	if peer := req.Header().Get("X-Real-Ip"); peer != "" {
		return peer
	}
	return "unknown"
}
