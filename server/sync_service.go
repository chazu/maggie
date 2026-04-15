package server

import (
	"context"
	"crypto/sha256"
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
	trust  *dist.TrustStore

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
	trust *dist.TrustStore,
	compile func(source string) (dist.CompileResult, error),
	diskCache *dist.DiskCache,
) *SyncService {
	return &SyncService{
		worker:    worker,
		store:     store,
		trust:     trust,
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
	peerID := peerNodeIDFromRequest(req)

	if s.trust.IsBanned(peerID) {
		return nil, connect.NewError(connect.CodePermissionDenied, fmt.Errorf("peer is banned"))
	}

	if len(msg.RootHash) != 32 {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("root_hash must be 32 bytes"))
	}

	// Check sync permission
	if !s.trust.Check(peerID, dist.PermSync) {
		return connect.NewResponse(&maggiev1.AnnounceResponse{
			Status:       maggiev1.AnnounceStatus_ANNOUNCE_REJECTED,
			RejectReason: fmt.Sprintf("peer %s not permitted to sync", peerID),
		}), nil
	}

	// Decode capability manifest if present (informational — trust perms are authoritative)
	if len(msg.CapabilityManifest) > 0 {
		manifest, err := dist.UnmarshalCapabilityManifest(msg.CapabilityManifest)
		if err != nil {
			return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("invalid capability manifest: %v", err))
		}
		if err := s.trust.CheckCapabilities(manifest); err != nil {
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
	peerID := peerNodeIDFromRequest(req)

	if s.trust.IsBanned(peerID) {
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
				s.trust.RecordHashMismatch(peerID)
				continue
			}
			// Verification passed — index in content store
			s.indexVerifiedMethod(chunk)
			accepted++
			s.trust.RecordSuccess(peerID)

		case dist.ChunkClass:
			if err := dist.VerifyChunkClass(chunk, s.store); err != nil {
				rejected++
				failedHashes = append(failedHashes, chunk.Hash[:])
				s.trust.RecordHashMismatch(peerID)
				continue
			}
			s.indexVerifiedClass(chunk)
			accepted++
			s.trust.RecordSuccess(peerID)

		case dist.ChunkModule:
			if err := dist.VerifyChunkModule(chunk, s.store); err != nil {
				rejected++
				failedHashes = append(failedHashes, chunk.Hash[:])
				s.trust.RecordHashMismatch(peerID)
				continue
			}
			accepted++
			s.trust.RecordSuccess(peerID)

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
	peerID := peerNodeIDFromRequest(req)

	if s.trust.IsBanned(peerID) {
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

	// Derive peer ID from the envelope's Ed25519 public key
	peerID := peerNodeIDFromEnvelope(envelope)

	// Check if peer is banned
	if s.trust.IsBanned(peerID) {
		return nil, connect.NewError(connect.CodePermissionDenied, fmt.Errorf("peer is banned"))
	}

	// Verify signature
	if err := envelope.Verify(); err != nil {
		s.trust.RecordHashMismatch(peerID) // bad signatures count toward ban
		return connect.NewResponse(&maggiev1.DeliverMessageResponse{
			Success:      false,
			ErrorKind:    "signatureInvalid",
			ErrorMessage: err.Error(),
		}), nil
	}

	s.trust.RecordSuccess(peerID)

	// Infrastructure selectors: route through monitor/link system
	if envelope.Selector == vm.SelectorDown {
		return s.handleRemoteDown(envelope)
	}
	if envelope.Selector == vm.SelectorSpawnResult {
		return s.handleSpawnResult(envelope)
	}

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

// handleRemoteDown processes a __down__ notification from a remote node.
func (s *SyncService) handleRemoteDown(envelope *dist.MessageEnvelope) (*connect.Response[maggiev1.DeliverMessageResponse], error) {
	type downPayload struct {
		RefID  uint64 `cbor:"1,keyasint"`
		Signal string `cbor:"2,keyasint"`
		Normal bool   `cbor:"3,keyasint"`
	}
	var dp downPayload
	if err := dist.UnmarshalCBOR(envelope.Payload, &dp); err != nil {
		return connect.NewResponse(&maggiev1.DeliverMessageResponse{
			Success:   false,
			ErrorKind: "deserializationError",
		}), nil
	}

	rmRef := s.worker.vm.RemoteWatches().RemoveOutboundMonitor(dp.RefID)
	if rmRef == nil {
		return connect.NewResponse(&maggiev1.DeliverMessageResponse{Success: true}), nil
	}

	watcher := s.worker.vm.GetProcessByID(rmRef.WatcherID)
	if watcher != nil && !watcher.IsDone() {
		reason := vm.ExitReason{Normal: dp.Normal, Signal: dp.Signal, Result: vm.Nil}
		ref := &vm.MonitorRef{ID: dp.RefID, Watcher: rmRef.WatcherID}
		s.worker.vm.DeliverDownMessage(watcher, ref, reason)
	}

	return connect.NewResponse(&maggiev1.DeliverMessageResponse{Success: true}), nil
}

// handleSpawnResult processes a spawn result delivery from a remote node.
// The payload is a CBOR struct with futureID + result bytes.
func (s *SyncService) handleSpawnResult(envelope *dist.MessageEnvelope) (*connect.Response[maggiev1.DeliverMessageResponse], error) {
	type spawnResult struct {
		FutureID    uint64 `cbor:"1,keyasint"`
		ResultBytes []byte `cbor:"2,keyasint"`
		ErrorMsg    string `cbor:"3,keyasint,omitempty"`
	}
	var sr spawnResult
	if err := dist.UnmarshalCBOR(envelope.Payload, &sr); err != nil {
		return connect.NewResponse(&maggiev1.DeliverMessageResponse{
			Success:   false,
			ErrorKind: "deserializationError",
		}), nil
	}

	future := s.worker.vm.ResolvePendingSpawn(sr.FutureID)
	if future == nil {
		// Already resolved or unknown — not an error
		return connect.NewResponse(&maggiev1.DeliverMessageResponse{Success: true}), nil
	}

	if sr.ErrorMsg != "" {
		future.ResolveError(sr.ErrorMsg)
	} else if len(sr.ResultBytes) > 0 {
		result, err := s.worker.vm.DeserializeValue(sr.ResultBytes)
		if err != nil {
			future.ResolveError(fmt.Sprintf("result deserialize: %v", err))
		} else {
			future.Resolve(result)
		}
	} else {
		future.Resolve(vm.Nil)
	}

	return connect.NewResponse(&maggiev1.DeliverMessageResponse{Success: true}), nil
}

// MonitorProcess handles a remote monitor request.
func (s *SyncService) MonitorProcess(
	ctx context.Context,
	req *connect.Request[maggiev1.MonitorProcessRequest],
) (*connect.Response[maggiev1.MonitorProcessResponse], error) {
	msg := req.Msg

	var senderNode [32]byte
	if len(msg.SenderNode) == 32 {
		copy(senderNode[:], msg.SenderNode)
	}

	alreadyDead, reason := s.worker.vm.HandleInboundMonitor(
		msg.MonitorRefId,
		msg.WatcherId,
		senderNode,
		msg.TargetName,
		msg.TargetId,
	)

	resp := &maggiev1.MonitorProcessResponse{
		Success:     true,
		AlreadyDead: alreadyDead,
	}
	if alreadyDead {
		resp.ExitSignal = reason.Signal
		resp.ExitNormal = reason.Normal
	}
	return connect.NewResponse(resp), nil
}

// DemonitorProcess cancels a remote monitor.
func (s *SyncService) DemonitorProcess(
	ctx context.Context,
	req *connect.Request[maggiev1.DemonitorProcessRequest],
) (*connect.Response[maggiev1.DemonitorProcessResponse], error) {
	s.worker.vm.RemoteWatches().RemoveInboundMonitor(req.Msg.MonitorRefId)
	return connect.NewResponse(&maggiev1.DemonitorProcessResponse{Success: true}), nil
}

// SpawnProcess handles a remote spawn request. It deserializes the
// SpawnBlock from the signed envelope, checks PermSpawn permission,
// resolves the block method (with code-on-demand pull if needed),
// and executes the block in a restricted interpreter.
func (s *SyncService) SpawnProcess(
	ctx context.Context,
	req *connect.Request[maggiev1.SpawnProcessRequest],
) (*connect.Response[maggiev1.SpawnProcessResponse], error) {
	msg := req.Msg

	// Decode the CBOR envelope
	envelope, err := dist.UnmarshalEnvelope(msg.Envelope)
	if err != nil {
		return connect.NewResponse(&maggiev1.SpawnProcessResponse{
			Accepted: false,
			Error:    fmt.Sprintf("invalid envelope: %v", err),
		}), nil
	}

	// Derive peer ID and check permissions
	peerID := peerNodeIDFromEnvelope(envelope)

	// Record the peer's return address for code-on-demand pull-back.
	// The spawning node sends its listen address via X-Maggie-Return-Addr.
	if s.worker.peerAddrs != nil {
		if retAddr := req.Header().Get("X-Maggie-Return-Addr"); retAddr != "" {
			s.worker.peerAddrs.Store(peerID, retAddr)
		}
	}

	if s.trust.IsBanned(peerID) {
		return nil, connect.NewError(connect.CodePermissionDenied, fmt.Errorf("peer is banned"))
	}

	// Verify signature
	if err := envelope.Verify(); err != nil {
		s.trust.RecordHashMismatch(peerID)
		return connect.NewResponse(&maggiev1.SpawnProcessResponse{
			Accepted: false,
			Error:    "invalid signature",
		}), nil
	}

	// Check PermSpawn
	if !s.trust.Check(peerID, dist.PermSpawn) {
		return connect.NewResponse(&maggiev1.SpawnProcessResponse{
			Accepted: false,
			Error:    "spawn not permitted",
		}), nil
	}

	// Deserialize SpawnBlock from envelope payload
	sb, err := vm.DeserializeSpawnBlock(envelope.Payload)
	if err != nil {
		return connect.NewResponse(&maggiev1.SpawnProcessResponse{
			Accepted: false,
			Error:    fmt.Sprintf("invalid spawn block: %v", err),
		}), nil
	}

	// Get spawn restrictions from trust store
	restrictions := s.trust.SpawnRestrictions()

	// Build a pull function for code-on-demand
	// The pull func calls back to the spawning node's Serve RPC
	var pullFunc func(hash [32]byte) error
	if s.worker.pullFunc != nil {
		pullFunc = func(hash [32]byte) error {
			return s.worker.pullFunc(peerID, hash)
		}
	}

	// Execute the spawn block
	procName, err := s.worker.vm.ExecuteSpawnBlock(sb, restrictions, pullFunc)
	if err != nil {
		return connect.NewResponse(&maggiev1.SpawnProcessResponse{
			Accepted: false,
			Error:    err.Error(),
		}), nil
	}

	s.trust.RecordSuccess(peerID)

	// If this is a forkOn: (not spawnOn:), set up result delivery
	if sb.SpawnMode == "fork" && sb.FutureID > 0 {
		s.setupResultDelivery(sb, peerID, procName)
	}

	return connect.NewResponse(&maggiev1.SpawnProcessResponse{
		Accepted:    true,
		ProcessName: procName,
	}), nil
}

// setupResultDelivery monitors the spawned process and delivers its result
// back to the spawning node when it completes.
func (s *SyncService) setupResultDelivery(sb *vm.SpawnBlock, spawnerID dist.NodeID, procName string) {
	procVal := s.worker.vm.LookupProcessName(procName)
	if procVal == vm.Nil {
		return
	}
	procID := uint64(procVal.SymbolID() & ^uint32(0xFF<<24))
	proc := s.worker.vm.GetProcessByID(procID)
	if proc == nil {
		return
	}

	go func() {
		// Wait for the process to complete
		result := proc.Wait()

		// Serialize the result
		resultBytes, err := s.worker.vm.SerializeValue(result)
		if err != nil {
			resultBytes, _ = s.worker.vm.SerializeValue(vm.Nil)
		}

		// Build a spawn-result envelope and deliver back to spawner
		if s.worker.spawnResultFunc != nil {
			s.worker.spawnResultFunc(spawnerID, sb.FutureID, resultBytes, proc.ExitReason().Error)
		}
	}()
}

// ---------------------------------------------------------------------------
// Distributed Channel RPCs
// ---------------------------------------------------------------------------

// ChannelSend sends a value to an exported channel (blocking).
func (s *SyncService) ChannelSend(
	ctx context.Context,
	req *connect.Request[maggiev1.ChannelSendRequest],
) (*connect.Response[maggiev1.ChannelSendResponse], error) {
	ch := s.worker.vm.LookupExportedChannel(req.Msg.ChannelId)
	if ch == nil {
		return connect.NewResponse(&maggiev1.ChannelSendResponse{
			Success: false,
			Error:   "channel not found",
		}), nil
	}

	val, err := s.worker.vm.DeserializeValue(req.Msg.Value)
	if err != nil {
		return connect.NewResponse(&maggiev1.ChannelSendResponse{
			Success: false,
			Error:   fmt.Sprintf("deserialize: %v", err),
		}), nil
	}

	if !ch.SafeSend(val) {
		return connect.NewResponse(&maggiev1.ChannelSendResponse{
			Success: false,
			Error:   "channel closed",
		}), nil
	}

	return connect.NewResponse(&maggiev1.ChannelSendResponse{Success: true}), nil
}

// ChannelReceive receives a value from an exported channel (blocking).
func (s *SyncService) ChannelReceive(
	ctx context.Context,
	req *connect.Request[maggiev1.ChannelReceiveRequest],
) (*connect.Response[maggiev1.ChannelReceiveResponse], error) {
	ch := s.worker.vm.LookupExportedChannel(req.Msg.ChannelId)
	if ch == nil {
		return connect.NewResponse(&maggiev1.ChannelReceiveResponse{
			Success: false,
			Error:   "channel not found",
		}), nil
	}

	val, ok := ch.Receive()
	if !ok {
		return connect.NewResponse(&maggiev1.ChannelReceiveResponse{
			Success:     true,
			ChannelOpen: false,
		}), nil
	}

	data, err := s.worker.vm.SerializeValue(val)
	if err != nil {
		return connect.NewResponse(&maggiev1.ChannelReceiveResponse{
			Success: false,
			Error:   fmt.Sprintf("serialize: %v", err),
		}), nil
	}

	return connect.NewResponse(&maggiev1.ChannelReceiveResponse{
		Success:     true,
		Value:       data,
		ChannelOpen: true,
	}), nil
}

// ChannelTrySend attempts a non-blocking send.
func (s *SyncService) ChannelTrySend(
	ctx context.Context,
	req *connect.Request[maggiev1.ChannelSendRequest],
) (*connect.Response[maggiev1.ChannelTrySendResponse], error) {
	ch := s.worker.vm.LookupExportedChannel(req.Msg.ChannelId)
	if ch == nil {
		return connect.NewResponse(&maggiev1.ChannelTrySendResponse{
			Error: "channel not found",
		}), nil
	}

	val, err := s.worker.vm.DeserializeValue(req.Msg.Value)
	if err != nil {
		return connect.NewResponse(&maggiev1.ChannelTrySendResponse{
			Error: fmt.Sprintf("deserialize: %v", err),
		}), nil
	}

	sent := ch.SafeTrySend(val)
	return connect.NewResponse(&maggiev1.ChannelTrySendResponse{Sent: sent}), nil
}

// ChannelTryReceive attempts a non-blocking receive.
func (s *SyncService) ChannelTryReceive(
	ctx context.Context,
	req *connect.Request[maggiev1.ChannelReceiveRequest],
) (*connect.Response[maggiev1.ChannelTryReceiveResponse], error) {
	ch := s.worker.vm.LookupExportedChannel(req.Msg.ChannelId)
	if ch == nil {
		return connect.NewResponse(&maggiev1.ChannelTryReceiveResponse{
			Error: "channel not found",
		}), nil
	}

	val, gotValue, ok := ch.TryReceive()
	if !gotValue {
		return connect.NewResponse(&maggiev1.ChannelTryReceiveResponse{
			GotValue:    false,
			ChannelOpen: ok,
		}), nil
	}
	data, err := s.worker.vm.SerializeValue(val)
	if err != nil {
		return connect.NewResponse(&maggiev1.ChannelTryReceiveResponse{
			Error: fmt.Sprintf("serialize: %v", err),
		}), nil
	}
	return connect.NewResponse(&maggiev1.ChannelTryReceiveResponse{
		GotValue:    true,
		Value:       data,
		ChannelOpen: ok,
	}), nil
}

// ChannelClose closes an exported channel.
func (s *SyncService) ChannelClose(
	ctx context.Context,
	req *connect.Request[maggiev1.ChannelCloseRequest],
) (*connect.Response[maggiev1.ChannelCloseResponse], error) {
	ch := s.worker.vm.LookupExportedChannel(req.Msg.ChannelId)
	if ch == nil {
		return connect.NewResponse(&maggiev1.ChannelCloseResponse{Success: false}), nil
	}

	ch.Close()
	return connect.NewResponse(&maggiev1.ChannelCloseResponse{Success: true}), nil
}

// ChannelStatus returns the status of an exported channel.
func (s *SyncService) ChannelStatus(
	ctx context.Context,
	req *connect.Request[maggiev1.ChannelStatusRequest],
) (*connect.Response[maggiev1.ChannelStatusResponse], error) {
	ch := s.worker.vm.LookupExportedChannel(req.Msg.ChannelId)
	if ch == nil {
		return connect.NewResponse(&maggiev1.ChannelStatusResponse{Closed: true}), nil
	}

	return connect.NewResponse(&maggiev1.ChannelStatusResponse{
		Size:     int32(ch.Size()),
		Capacity: int32(ch.Cap()),
		Closed:   ch.Closed(),
	}), nil
}

// peerNodeIDFromRequest extracts a NodeID from a Connect request.
// Tries X-Maggie-Node-ID header first (hex-encoded Ed25519 public key),
// falls back to a deterministic hash of the IP address for backward compat.
func peerNodeIDFromRequest[T any](req *connect.Request[T]) dist.NodeID {
	// Best: explicit node ID header
	if idHex := req.Header().Get("X-Maggie-Node-ID"); idHex != "" {
		if id, err := dist.ParseNodeID(idHex); err == nil {
			return id
		}
	}
	// Fallback: hash the IP into a deterministic NodeID
	addr := "unknown"
	if xff := req.Header().Get("X-Forwarded-For"); xff != "" {
		addr = xff
	} else if peer := req.Header().Get("X-Real-Ip"); peer != "" {
		addr = peer
	}
	// Use SHA-256 of the address string as a pseudo-NodeID
	h := sha256.Sum256([]byte("ip:" + addr))
	return dist.NodeIDFromBytes(h[:])
}

// peerNodeIDFromEnvelope extracts a NodeID from a message envelope's sender key.
func peerNodeIDFromEnvelope(envelope *dist.MessageEnvelope) dist.NodeID {
	return dist.NodeIDFromBytes(envelope.SenderNode[:])
}
