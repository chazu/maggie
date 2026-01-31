package server

import (
	"fmt"
	"net/http"
	"time"

	"github.com/chazu/maggie/gen/maggie/v1/maggiev1connect"
	"github.com/chazu/maggie/vm"
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

// New creates a MaggieServer wrapping the given VM.
func New(v *vm.VM) *MaggieServer {
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

	evalPath, evalHandler := maggiev1connect.NewEvaluationServiceHandler(evalSvc)
	sessionPath, sessionHandler := maggiev1connect.NewSessionServiceHandler(sessionSvc)
	browsePath, browseHandler := maggiev1connect.NewBrowsingServiceHandler(browseSvc)
	modifyPath, modifyHandler := maggiev1connect.NewModificationServiceHandler(modifySvc)

	s.mux.Handle(evalPath, evalHandler)
	s.mux.Handle(sessionPath, sessionHandler)
	s.mux.Handle(browsePath, browseHandler)
	s.mux.Handle(modifyPath, modifyHandler)

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
