package server

import (
	"fmt"
	"net/http"
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
	compileFunc func(string) ([32]byte, error)
	syncPolicy  *dist.CapabilityPolicy
}

// WithCompileFunc sets the compile function used by the sync service to
// verify method chunks. Without this, method chunk verification is disabled.
func WithCompileFunc(fn func(string) ([32]byte, error)) ServerOption {
	return func(c *serverConfig) { c.compileFunc = fn }
}

// WithSyncPolicy sets the capability policy for the sync service.
// If not set, a permissive policy (allow all) is used.
func WithSyncPolicy(policy *dist.CapabilityPolicy) ServerOption {
	return func(c *serverConfig) { c.syncPolicy = policy }
}

// New creates a MaggieServer wrapping the given VM.
func New(v *vm.VM, opts ...ServerOption) *MaggieServer {
	cfg := &serverConfig{
		syncPolicy: dist.NewPermissivePolicy(),
	}
	for _, opt := range opts {
		opt(cfg)
	}

	worker := NewVMWorker(v)
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
	syncSvc := NewSyncService(worker, v.ContentStore(), dist.NewPeerStore(), cfg.syncPolicy, cfg.compileFunc)

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
