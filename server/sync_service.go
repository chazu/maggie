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

	// compile compiles source text and returns both semantic and typed
	// content hashes. Injected to avoid depending on the compiler package.
	compile func(source string) (dist.CompileResult, error)

	// diskCache persists received chunks to disk. May be nil.
	diskCache *dist.DiskCache
}

// NewSyncService creates a SyncService.
func NewSyncService(
	worker *VMWorker,
	store *vm.ContentStore,
	peers *dist.PeerStore,
	policy *dist.CapabilityPolicy,
	compile func(source string) (dist.CompileResult, error),
	diskCache *dist.DiskCache,
) *SyncService {
	return &SyncService{
		worker:    worker,
		store:     store,
		peers:     peers,
		policy:    policy,
		compile:   compile,
		diskCache: diskCache,
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

	// Persist newly received content to disk cache
	if s.diskCache != nil && accepted > 0 {
		// Best-effort: don't fail the RPC if cache write fails
		s.diskCache.SaveFrom(s.store)
	}

	return connect.NewResponse(&maggiev1.TransferResponse{
		Accepted:     accepted,
		Rejected:     rejected,
		FailedHashes: failedHashes,
	}), nil
}

// indexVerifiedMethod creates a CompiledMethod stub from a verified chunk
// and indexes it in the content store. The method has source text, semantic
// hash, and typed hash but no compiled bytecode — it serves as proof we
// received verified content.
func (s *SyncService) indexVerifiedMethod(chunk *dist.Chunk) {
	m := &vm.CompiledMethod{Source: chunk.Content}
	m.SetContentHash(chunk.Hash)
	if chunk.TypedHash != ([32]byte{}) {
		m.SetTypedHash(chunk.TypedHash)
	}
	s.store.IndexMethod(m)
}

// indexVerifiedClass indexes a class digest from a verified chunk.
// Populates both semantic and typed hashes from the chunk.
func (s *SyncService) indexVerifiedClass(chunk *dist.Chunk) {
	d, err := dist.DecodeClassContent(chunk.Content)
	if err != nil {
		// Fallback: treat Content as bare class name for backward compat
		d = &vm.ClassDigest{Name: chunk.Content}
	}
	d.Hash = chunk.Hash
	d.MethodHashes = chunk.Dependencies
	if chunk.TypedHash != ([32]byte{}) {
		d.TypedHash = chunk.TypedHash
	}
	if len(chunk.TypedDependencies) > 0 {
		d.TypedMethodHashes = chunk.TypedDependencies
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
			chunk := dist.ClassToChunk(d, nil)
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

// Resolve maps a class name (FQN) to its content hash.
func (s *SyncService) Resolve(
	ctx context.Context,
	req *connect.Request[maggiev1.ResolveRequest],
) (*connect.Response[maggiev1.ResolveResponse], error) {
	name := req.Msg.ClassName
	if name == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("class_name is required"))
	}

	d := s.store.LookupClassByName(name)
	if d == nil {
		return connect.NewResponse(&maggiev1.ResolveResponse{
			Found: false,
		}), nil
	}

	return connect.NewResponse(&maggiev1.ResolveResponse{
		RootHash: d.Hash[:],
		Found:    true,
	}), nil
}

// List enumerates all content in the local store.
func (s *SyncService) List(
	ctx context.Context,
	req *connect.Request[maggiev1.ListRequest],
) (*connect.Response[maggiev1.ListResponse], error) {
	methodHashes := s.store.AllMethodHashes()
	classDigests := s.store.AllClassDigests()

	mhBytes := make([][]byte, len(methodHashes))
	for i, h := range methodHashes {
		hCopy := h
		mhBytes[i] = hCopy[:]
	}

	chBytes := make([][]byte, len(classDigests))
	classNames := make([]string, len(classDigests))
	for i, d := range classDigests {
		hCopy := d.Hash
		chBytes[i] = hCopy[:]
		classNames[i] = d.Name
	}

	return connect.NewResponse(&maggiev1.ListResponse{
		MethodHashes: mhBytes,
		ClassHashes:  chBytes,
		ClassNames:   classNames,
	}), nil
}

// DeliverMessage handles an incoming message from a remote node. It verifies
// the signature, checks peer reputation, and delivers the message to the
// target process's mailbox. This is the core RPC for distributed messaging.
func (s *SyncService) DeliverMessage(
	ctx context.Context,
	req *connect.Request[maggiev1.DeliverMessageRequest],
) (*connect.Response[maggiev1.DeliverMessageResponse], error) {
	msg := req.Msg

	// Decode the CBOR envelope
	envelope, err := dist.UnmarshalEnvelope(msg.Envelope)
	if err != nil {
		return connect.NewResponse(&maggiev1.DeliverMessageResponse{
			Success:      false,
			ErrorKind:    "deserializationError",
			ErrorMessage: fmt.Sprintf("invalid envelope: %v", err),
		}), nil
	}

	// Derive peer ID from the sender's public key (hex-encoded for PeerStore compat)
	peerID := fmt.Sprintf("%x", envelope.SenderNode)

	// Check if peer is banned
	if s.peers.IsBanned(peerID) {
		return nil, connect.NewError(connect.CodePermissionDenied, fmt.Errorf("peer is banned"))
	}

	// Verify signature
	if err := envelope.Verify(); err != nil {
		s.peers.RecordHashMismatch(peerID) // bad signatures count toward ban
		return connect.NewResponse(&maggiev1.DeliverMessageResponse{
			Success:      false,
			ErrorKind:    "signatureInvalid",
			ErrorMessage: err.Error(),
		}), nil
	}

	s.peers.RecordSuccess(peerID)

	// Resolve target process by name or ID
	var proc *vm.ProcessObject
	if envelope.TargetName != "" {
		procVal := s.worker.vm.LookupProcessName(envelope.TargetName)
		if procVal == vm.Nil {
			return connect.NewResponse(&maggiev1.DeliverMessageResponse{
				Success:      false,
				ErrorKind:    "processNotFound",
				ErrorMessage: fmt.Sprintf("no process registered as %q", envelope.TargetName),
			}), nil
		}
		proc = s.worker.vm.GetProcessByID(uint64(procVal.SymbolID() & ^uint32(0xFF<<24)))
	} else {
		proc = s.worker.vm.GetProcessByID(envelope.TargetProcess)
	}
	if proc == nil || proc.IsDone() {
		return connect.NewResponse(&maggiev1.DeliverMessageResponse{
			Success:      false,
			ErrorKind:    "processNotFound",
			ErrorMessage: "target process not found or terminated",
		}), nil
	}

	// Deserialize payload
	payload, err := s.worker.vm.DeserializeValue(envelope.Payload)
	if err != nil {
		return connect.NewResponse(&maggiev1.DeliverMessageResponse{
			Success:      false,
			ErrorKind:    "deserializationError",
			ErrorMessage: fmt.Sprintf("payload: %v", err),
		}), nil
	}

	// Create MailboxMessage and deliver to mailbox
	mailboxMsg := s.worker.vm.CreateMailboxMessage(vm.Nil, envelope.Selector, payload)
	if !proc.Mailbox().TrySend(mailboxMsg) {
		return connect.NewResponse(&maggiev1.DeliverMessageResponse{
			Success:      false,
			ErrorKind:    "mailboxFull",
			ErrorMessage: "target process mailbox is full",
		}), nil
	}

	return connect.NewResponse(&maggiev1.DeliverMessageResponse{
		Success: true,
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
