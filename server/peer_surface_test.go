package server

import (
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chazu/maggie/vm"
)

// A peer-facing server (WithSyncService) must not also expose the
// unauthenticated developer services: the sync listener is reachable by other
// nodes, and EvaluationService there was remote code execution for anyone who
// could connect.
func TestSyncServerDoesNotExposeIDEServices(t *testing.T) {
	v := vm.NewVM()
	defer v.Shutdown()
	s := New(v, WithSyncService())
	defer s.Stop()
	ts := httptest.NewServer(s.mux)
	defer ts.Close()

	for _, path := range []string{
		"/maggie.v1.EvaluationService/Evaluate",
		"/maggie.v1.ModificationService/CompileMethod",
		"/maggie.v1.InspectionService/Inspect",
		"/maggie.v1.BrowsingService/ListClasses",
		"/maggie.v1.SessionService/CreateSession",
	} {
		resp, err := http.Post(ts.URL+path, "application/json", strings.NewReader(`{"source":"3 + 4"}`))
		if err != nil {
			t.Fatal(err)
		}
		resp.Body.Close()
		if resp.StatusCode != http.StatusNotFound {
			t.Errorf("%s on a sync server: status %d, want 404", path, resp.StatusCode)
		}
	}
}

// The developer server (no WithSyncService) keeps serving eval.
func TestIDEServerExposesEvaluation(t *testing.T) {
	v := vm.NewVM()
	defer v.Shutdown()
	s := New(v)
	defer s.Stop()
	ts := httptest.NewServer(s.mux)
	defer ts.Close()

	resp, err := http.Post(ts.URL+"/maggie.v1.EvaluationService/Evaluate", "application/json", strings.NewReader(`{"source":"3 + 4"}`))
	if err != nil {
		t.Fatal(err)
	}
	resp.Body.Close()
	if resp.StatusCode == http.StatusNotFound {
		t.Error("IDE server no longer serves EvaluationService")
	}
}
