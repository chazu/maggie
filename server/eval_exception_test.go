package server

import (
	"strings"
	"testing"

	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
)

// TestEvaluate_UnhandledExceptionRendersReadably guards against the server
// reporting an unhandled Maggie exception as a raw NaN-boxed struct
// (`{9222246137082150913 0x…}`). The Evaluate path is what the IDE and AI
// agents drive, so the error string must be human/agent readable.
func TestEvaluate_UnhandledExceptionRendersReadably(t *testing.T) {
	svc := newTestEvalService()
	resp, err := svc.Evaluate(bg(), connectReq(&maggiev1.EvaluateRequest{Source: "42 fooBarBaz"}))
	if err != nil {
		t.Fatalf("transport error: %v", err)
	}
	if resp.Msg.Success {
		t.Fatal("expected failure for an unknown selector")
	}
	msg := resp.Msg.ErrorMessage
	if strings.HasPrefix(msg, "{") || strings.Contains(msg, "0x") {
		t.Errorf("exception rendered as a raw struct: %q", msg)
	}
	if !strings.Contains(msg, "not understood") {
		t.Errorf("ErrorMessage = %q, want a readable 'not understood' message", msg)
	}
}
