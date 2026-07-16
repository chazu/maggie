package server

import (
	"fmt"
	"net/http"
	"sync"
	"time"

	"connectrpc.com/connect"

	"github.com/chazu/maggie/gen/maggie/v1/maggiev1connect"
	"github.com/chazu/maggie/vm"
	"github.com/chazu/maggie/vm/dist"
)

// MaggieServer is the language server wrapping a running VM.
// It serves both gRPC (binary protobuf) and Connect (HTTP/JSON)
// on the same port.
type MaggieServer struct {
	worker   *VMWorker
	handles  *HandleStore
	sessions *SessionStore
	mux      *http.ServeMux

	stopSweeper func()
}

// ServerOption configures a MaggieServer.
type ServerOption func(*serverConfig)

type serverConfig struct {
	compileFunc     dist.CompileFunc
	trustStore      *dist.TrustStore
	diskCache       *dist.DiskCache
	pullFunc        func(peerID dist.NodeID, hash [32]byte) error
	spawnResultFunc func(spawnerID dist.NodeID, futureID uint64, resultBytes []byte, errMsg error, exceptionBytes []byte)
	peerAddrs       *sync.Map // NodeID -> address registry for code-on-demand
	mountSync       bool      // mount the peer-facing SyncService (sync/message/spawn/channel RPCs)
}

// WithCompileFunc sets the compile function used by the sync service to
// verify method chunks. The function should hash the source in the supplied
// class context and return both semantic and typed content hashes
// (pipeline.VerifyCompileFunc is the canonical implementation). Without
// this, method chunk verification is disabled.
func WithCompileFunc(fn dist.CompileFunc) ServerOption {
	return func(c *serverConfig) { c.compileFunc = fn }
}

// WithTrustStore sets the trust store for the sync service.
// If not set, a permissive trust store (allow all) is used.
func WithTrustStore(ts *dist.TrustStore) ServerOption {
	return func(c *serverConfig) { c.trustStore = ts }
}

// WithDiskCache sets the disk cache used by the sync service to persist
// received chunks after successful transfers.
func WithDiskCache(dc *dist.DiskCache) ServerOption {
	return func(c *serverConfig) { c.diskCache = dc }
}

// WithPullFunc sets the code-on-demand pull function for spawn.
func WithPullFunc(fn func(peerID dist.NodeID, hash [32]byte) error) ServerOption {
	return func(c *serverConfig) { c.pullFunc = fn }
}

// WithSpawnResultFunc sets the callback for delivering spawn results.
func WithSpawnResultFunc(fn func(spawnerID dist.NodeID, futureID uint64, resultBytes []byte, errMsg error, exceptionBytes []byte)) ServerOption {
	return func(c *serverConfig) { c.spawnResultFunc = fn }
}

// WithPeerAddrRegistry sets a shared NodeID -> address registry. The
// SpawnProcess handler records the spawning peer's return address here
// (from the X-Maggie-Return-Addr header) so the pull function can call
// back to the peer's Serve RPC for code-on-demand.
func WithPeerAddrRegistry(m *sync.Map) ServerOption {
	return func(c *serverConfig) { c.peerAddrs = m }
}

// WithSyncService mounts the peer-facing SyncService, which exposes the
// sync/announce, message-delivery, remote-spawn, and remote-channel RPCs to
// other nodes. It is OFF by default: a plain server (e.g. the local IDE/
// language server) serves only the developer-facing eval/browse/inspect/
// modify/session services and does NOT expose any peer surface. Enable it only
// for a node acting as a distribution peer, and pair it with a configured
// WithTrustStore — the channel RPCs in particular are reachable by anyone who
// can call the mounted endpoint.
func WithSyncService() ServerOption {
	return func(c *serverConfig) { c.mountSync = true }
}

// New creates a MaggieServer wrapping the given VM.
func New(v *vm.VM, opts ...ServerOption) *MaggieServer {
	// Default to a SECURE trust store: unknown peers get no permissions, so a
	// server constructed without an explicit WithTrustStore (e.g. the local
	// IDE/language server) does not accept remote sync/message/spawn from
	// arbitrary peers. Callers that want to accept peers must pass
	// WithTrustStore with a configured policy.
	cfg := &serverConfig{
		trustStore: dist.NewSecureTrustStore(),
	}
	for _, opt := range opts {
		opt(cfg)
	}

	worker := NewVMWorker(v)
	worker.pullFunc = cfg.pullFunc
	worker.spawnResultFunc = cfg.spawnResultFunc
	worker.peerAddrs = cfg.peerAddrs
	handles := NewHandleStore(worker)
	sessions := NewSessionStore(handles)

	s := &MaggieServer{
		worker:   worker,
		handles:  handles,
		sessions: sessions,
		mux:      http.NewServeMux(),
	}

	// Register Connect/gRPC service handlers
	evalSvc := NewEvalService(worker, handles, sessions)
	sessionSvc := NewSessionServiceImpl(worker, sessions)
	browseSvc := NewBrowseService(worker)
	modifySvc := NewModifyService(worker, handles, sessions)
	inspectSvc := NewInspectService(worker, handles)

	evalPath, evalHandler := maggiev1connect.NewEvaluationServiceHandler(evalSvc)
	sessionPath, sessionHandler := maggiev1connect.NewSessionServiceHandler(sessionSvc)
	browsePath, browseHandler := maggiev1connect.NewBrowsingServiceHandler(browseSvc)
	modifyPath, modifyHandler := maggiev1connect.NewModificationServiceHandler(modifySvc)
	inspectPath, inspectHandler := maggiev1connect.NewInspectionServiceHandler(inspectSvc)

	s.mux.Handle(evalPath, evalHandler)
	s.mux.Handle(sessionPath, sessionHandler)
	s.mux.Handle(browsePath, browseHandler)
	s.mux.Handle(modifyPath, modifyHandler)
	s.mux.Handle(inspectPath, inspectHandler)

	// The peer-facing SyncService (sync/message/spawn/channel RPCs) is mounted
	// only when explicitly enabled via WithSyncService, and always behind the
	// auth interceptor: every RPC requires a signed request from a peer with
	// the right permission (deny by default; Ping is the one anonymous,
	// liveness-only exception). A default server (e.g. the local IDE/language
	// server) exposes no peer surface at all.
	if cfg.mountSync {
		trust := cfg.trustStore
		if trust == nil {
			trust = dist.NewSecureTrustStore()
		}
		syncSvc := NewSyncService(worker, v.ContentStore(), trust, cfg.compileFunc, cfg.diskCache)
		syncPath, syncHandler := maggiev1connect.NewSyncServiceHandler(syncSvc,
			connect.WithInterceptors(NewAuthInterceptor(trust, SyncPermTable())))
		s.mux.Handle(syncPath, syncHandler)
	}

	// Start handle TTL sweeper (sweep every 5 minutes, 30-minute TTL)
	s.stopSweeper = handles.StartSweeper(5*time.Minute, 30*time.Minute)

	return s
}

// ListenAndServe starts the HTTP server on the given address.
// The address should be in the form "host:port" or ":port".
func (s *MaggieServer) ListenAndServe(addr string) error {
	fmt.Printf("Maggie language server listening on %s\n", addr)
	fmt.Printf("  Connect (HTTP/JSON): http://%s/maggie.v1.EvaluationService/Evaluate\n", addr)
	fmt.Printf("  gRPC (binary):       grpc://%s\n", addr)
	return http.ListenAndServe(addr, s.mux)
}

// Stop shuts down the server.
func (s *MaggieServer) Stop() {
	if s.stopSweeper != nil {
		s.stopSweeper()
	}
	s.worker.Stop()
}
