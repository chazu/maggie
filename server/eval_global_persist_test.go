package server

import (
	"testing"

	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
)

// TestEvaluateGlobalWritePersists is the regression test for the Stage-5 gate
// bug where per-request interpreters were forked, diverting global writes into a
// throwaway copy-on-write overlay so `Compiler setGlobal:` (and workspace
// variables) were silently lost. A global written by one Evaluate must be
// visible to a later Evaluate — i.e. it must reach the shared VM globals.
func TestEvaluateGlobalWritePersists(t *testing.T) {
	svc := newTestEvalService()

	// Write a global through the eval path. `setGlobal:` is schema-mutating, so
	// DoForSource routes this through the exclusive gate.
	resp, err := svc.Evaluate(bg(), connectReq(&maggiev1.EvaluateRequest{
		Source: "Compiler setGlobal: #StageFivePersistG to: 4242",
	}))
	if err != nil {
		t.Fatalf("setGlobal eval error: %v", err)
	}
	if !resp.Msg.Success {
		t.Fatalf("setGlobal eval failed: %s", resp.Msg.ErrorMessage)
	}

	// A subsequent Evaluate (a different per-request interpreter) must observe
	// the write via the shared global map.
	resp2, err := svc.Evaluate(bg(), connectReq(&maggiev1.EvaluateRequest{
		Source: "Compiler getGlobal: #StageFivePersistG",
	}))
	if err != nil {
		t.Fatalf("getGlobal eval error: %v", err)
	}
	if !resp2.Msg.Success {
		t.Fatalf("getGlobal eval failed: %s", resp2.Msg.ErrorMessage)
	}
	if resp2.Msg.Result != "4242" {
		t.Fatalf("global did not persist across requests: got %q, want %q",
			resp2.Msg.Result, "4242")
	}
}
