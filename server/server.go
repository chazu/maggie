package server

import (
	"fmt"
	"net/http"
	"sync"
	"time"

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
	compileFunc     func(string) (dist.CompileResult, error)
	trustStore      *dist.TrustStore
	diskCache       *dist.DiskCache
	pullFunc        func(peerID dist.NodeID, hash [32]byte) error
	spawnResultFunc func(spawnerID dist.NodeID, futureID uint64, resultBytes []byte, errMsg error, exceptionBytes []byte)
	peerAddrs       *sync.Map // NodeID -> address registry for code-on-demand
}

// WithCompileFunc sets the compile function used by the sync service to
// verify method chunks. The function should return both semantic and typed
// content hashes. Without this, method chunk verification is disabled.
func WithCompileFunc(fn func(string) (dist.CompileResult, error)) ServerOption {
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

// New creates a MaggieServer wrapping the given VM.
func New(v *vm.VM, opts ...ServerOption) *MaggieServer {
	cfg := &serverConfig{
		trustStore: dist.NewPermissiveTrustStore(),
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
	syncSvc := NewSyncService(worker, v.ContentStore(), cfg.trustStore, cfg.compileFunc, cfg.diskCache)

	evalPath, evalHandler := maggiev1connect.NewEvaluationServiceHandler(evalSvc)
	sessionPath, sessionHandler := maggiev1connect.NewSessionServiceHandler(sessionSvc)
	browsePath, browseHandler := maggiev1connect.NewBrowsingServiceHandler(browseSvc)
	modifyPath, modifyHandler := maggiev1connect.NewModificationServiceHandler(modifySvc)
	inspectPath, inspectHandler := maggiev1connect.NewInspectionServiceHandler(inspectSvc)
	syncPath, syncHandler := maggiev1connect.NewSyncServiceHandler(syncSvc)

	s.mux.Handle(evalPath, evalHandler)
	s.mux.Handle(sessionPath, sessionHandler)
	s.mux.Handle(browsePath, browseHandler)
	s.mux.Handle(modifyPath, modifyHandler)
	s.mux.Handle(inspectPath, inspectHandler)
	s.mux.Handle(syncPath, syncHandler)

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
