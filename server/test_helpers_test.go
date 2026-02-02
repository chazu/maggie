package server

import (
	"context"
	"os"
	"testing"

	"connectrpc.com/connect"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// Shared test infrastructure for server package tests.
//
// The VM is expensive to bootstrap, so we create one shared instance via
// TestMain and reuse it across tests. Each test that needs mutation should
// either operate on ephemeral objects or create a local VM.
// ---------------------------------------------------------------------------

var (
	testVM       *vm.VM
	testWorker   *VMWorker
	testHandles  *HandleStore
	testSessions *SessionStore
)

// TestMain bootstraps a single VM with the Go compiler for all server tests.
func TestMain(m *testing.M) {
	testVM = vm.NewVM()
	testVM.UseGoCompiler(compiler.Compile)

	testWorker = NewVMWorker(testVM)
	testHandles = NewHandleStore(testWorker)
	testSessions = NewSessionStore(testHandles)

	code := m.Run()

	testWorker.Stop()
	os.Exit(code)
}

// newTestBrowseService creates a BrowseService backed by the shared VM.
func newTestBrowseService() *BrowseService {
	return NewBrowseService(testWorker)
}

// newTestEvalService creates an EvalService backed by the shared VM.
func newTestEvalService() *EvalService {
	return NewEvalService(testWorker, testHandles, testSessions)
}

// newTestInspectService creates an InspectService backed by the shared VM.
func newTestInspectService() *InspectService {
	return NewInspectService(testWorker, testHandles)
}

// newTestModifyService creates a ModifyService backed by the shared VM.
func newTestModifyService() *ModifyService {
	return NewModifyService(testWorker, testHandles, testSessions)
}

// newTestSessionService creates a SessionServiceImpl backed by the shared VM.
func newTestSessionService() *SessionServiceImpl {
	return NewSessionServiceImpl(testWorker, testSessions)
}

// ---------------------------------------------------------------------------
// Isolated VM helpers — for tests that mutate class tables, globals, etc.
// ---------------------------------------------------------------------------

// testEnv bundles a fresh, isolated VM with its worker and stores.
type testEnv struct {
	VM       *vm.VM
	Worker   *VMWorker
	Handles  *HandleStore
	Sessions *SessionStore
}

// newIsolatedEnv creates a brand-new VM + worker + stores.
// The caller must call env.Stop() when done.
func newIsolatedEnv() *testEnv {
	v := vm.NewVM()
	v.UseGoCompiler(compiler.Compile)
	w := NewVMWorker(v)
	h := NewHandleStore(w)
	s := NewSessionStore(h)
	return &testEnv{VM: v, Worker: w, Handles: h, Sessions: s}
}

func (e *testEnv) Stop() {
	e.Worker.Stop()
}

// ---------------------------------------------------------------------------
// Request builder helpers — reduce boilerplate in tests.
// ---------------------------------------------------------------------------

func connectReq[T any](msg *T) *connect.Request[T] {
	return connect.NewRequest(msg)
}

func bg() context.Context {
	return context.Background()
}
