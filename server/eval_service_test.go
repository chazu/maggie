package server

import (
	"testing"

	"connectrpc.com/connect"

	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
)

// ---------------------------------------------------------------------------
// Evaluate — happy paths
// ---------------------------------------------------------------------------

func TestEvaluate_SimpleInteger(t *testing.T) {
	svc := newTestEvalService()

	resp, err := svc.Evaluate(bg(), connectReq(&maggiev1.EvaluateRequest{
		Source: "42",
	}))
	if err != nil {
		t.Fatalf("Evaluate returned error: %v", err)
	}
	if !resp.Msg.Success {
		t.Fatalf("Evaluate was not successful: %s", resp.Msg.ErrorMessage)
	}
	if resp.Msg.Result != "42" {
		t.Errorf("Evaluate result = %q, want %q", resp.Msg.Result, "42")
	}
	if resp.Msg.Handle == nil {
		t.Error("Evaluate should return a handle")
	}
	if resp.Msg.Handle.ClassName != "SmallInteger" {
		t.Errorf("Handle.ClassName = %q, want %q", resp.Msg.Handle.ClassName, "SmallInteger")
	}
}

func TestEvaluate_Arithmetic(t *testing.T) {
	svc := newTestEvalService()

	resp, err := svc.Evaluate(bg(), connectReq(&maggiev1.EvaluateRequest{
		Source: "3 + 4",
	}))
	if err != nil {
		t.Fatalf("Evaluate returned error: %v", err)
	}
	if !resp.Msg.Success {
		t.Fatalf("Evaluate was not successful: %s", resp.Msg.ErrorMessage)
	}
	if resp.Msg.Result != "7" {
		t.Errorf("Evaluate result = %q, want %q", resp.Msg.Result, "7")
	}
}

func TestEvaluate_StringLiteral(t *testing.T) {
	svc := newTestEvalService()

	resp, err := svc.Evaluate(bg(), connectReq(&maggiev1.EvaluateRequest{
		Source: "'hello'",
	}))
	if err != nil {
		t.Fatalf("Evaluate returned error: %v", err)
	}
	if !resp.Msg.Success {
		t.Fatalf("Evaluate was not successful: %s", resp.Msg.ErrorMessage)
	}
	if resp.Msg.Result != "'hello'" {
		t.Errorf("Evaluate result = %q, want %q", resp.Msg.Result, "'hello'")
	}
}

func TestEvaluate_BooleanTrue(t *testing.T) {
	svc := newTestEvalService()

	resp, err := svc.Evaluate(bg(), connectReq(&maggiev1.EvaluateRequest{
		Source: "true",
	}))
	if err != nil {
		t.Fatalf("Evaluate returned error: %v", err)
	}
	if !resp.Msg.Success {
		t.Fatalf("Evaluate was not successful: %s", resp.Msg.ErrorMessage)
	}
	if resp.Msg.Result != "true" {
		t.Errorf("Evaluate result = %q, want %q", resp.Msg.Result, "true")
	}
}

func TestEvaluate_Nil(t *testing.T) {
	svc := newTestEvalService()

	resp, err := svc.Evaluate(bg(), connectReq(&maggiev1.EvaluateRequest{
		Source: "nil",
	}))
	if err != nil {
		t.Fatalf("Evaluate returned error: %v", err)
	}
	if !resp.Msg.Success {
		t.Fatalf("Evaluate was not successful: %s", resp.Msg.ErrorMessage)
	}
	if resp.Msg.Result != "nil" {
		t.Errorf("Evaluate result = %q, want %q", resp.Msg.Result, "nil")
	}
}

func TestEvaluate_MessageSend(t *testing.T) {
	svc := newTestEvalService()

	// Use a well-known message that exists on SmallInteger
	resp, err := svc.Evaluate(bg(), connectReq(&maggiev1.EvaluateRequest{
		Source: "10 negated",
	}))
	if err != nil {
		t.Fatalf("Evaluate returned error: %v", err)
	}
	if !resp.Msg.Success {
		t.Fatalf("Evaluate was not successful: %s", resp.Msg.ErrorMessage)
	}
	if resp.Msg.Result != "-10" {
		t.Errorf("Evaluate result = %q, want %q", resp.Msg.Result, "-10")
	}
}

// ---------------------------------------------------------------------------
// Evaluate — error paths
// ---------------------------------------------------------------------------

func TestEvaluate_EmptySource(t *testing.T) {
	svc := newTestEvalService()

	_, err := svc.Evaluate(bg(), connectReq(&maggiev1.EvaluateRequest{
		Source: "",
	}))
	if err == nil {
		t.Fatal("Evaluate with empty source should return error")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeInvalidArgument {
			t.Errorf("expected CodeInvalidArgument, got %v", connectErr.Code())
		}
	}
}

func TestEvaluate_CompileError(t *testing.T) {
	svc := newTestEvalService()

	resp, err := svc.Evaluate(bg(), connectReq(&maggiev1.EvaluateRequest{
		Source: "[[[",
	}))
	if err != nil {
		t.Fatalf("Evaluate returned transport error: %v", err)
	}
	// Compile errors come back as Success=false, not as gRPC errors
	if resp.Msg.Success {
		t.Error("Evaluate should not succeed with invalid syntax")
	}
	if resp.Msg.ErrorMessage == "" {
		t.Error("ErrorMessage should describe the compile error")
	}
}

// ---------------------------------------------------------------------------
// EvaluateInContext
// ---------------------------------------------------------------------------

func TestEvaluateInContext_WithHandle(t *testing.T) {
	svc := newTestEvalService()

	// First evaluate something to get a handle
	evalResp, err := svc.Evaluate(bg(), connectReq(&maggiev1.EvaluateRequest{
		Source: "42",
	}))
	if err != nil {
		t.Fatalf("Evaluate returned error: %v", err)
	}
	if !evalResp.Msg.Success {
		t.Fatalf("Evaluate was not successful: %s", evalResp.Msg.ErrorMessage)
	}
	handle := evalResp.Msg.Handle

	// Now evaluate in context of that handle
	resp, err := svc.EvaluateInContext(bg(), connectReq(&maggiev1.EvaluateInContextRequest{
		Source: "self + 1",
		Context: &maggiev1.ObjectHandle{
			Id: handle.Id,
		},
	}))
	if err != nil {
		t.Fatalf("EvaluateInContext returned error: %v", err)
	}
	if !resp.Msg.Success {
		t.Fatalf("EvaluateInContext was not successful: %s", resp.Msg.ErrorMessage)
	}
	if resp.Msg.Result != "43" {
		t.Errorf("EvaluateInContext result = %q, want %q", resp.Msg.Result, "43")
	}
}

func TestEvaluateInContext_EmptySource(t *testing.T) {
	svc := newTestEvalService()

	_, err := svc.EvaluateInContext(bg(), connectReq(&maggiev1.EvaluateInContextRequest{
		Source:  "",
		Context: &maggiev1.ObjectHandle{Id: "h-1"},
	}))
	if err == nil {
		t.Fatal("EvaluateInContext with empty source should return error")
	}
}

func TestEvaluateInContext_MissingHandle(t *testing.T) {
	svc := newTestEvalService()

	_, err := svc.EvaluateInContext(bg(), connectReq(&maggiev1.EvaluateInContextRequest{
		Source: "self",
	}))
	if err == nil {
		t.Fatal("EvaluateInContext with no context handle should return error")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeInvalidArgument {
			t.Errorf("expected CodeInvalidArgument, got %v", connectErr.Code())
		}
	}
}

func TestEvaluateInContext_InvalidHandle(t *testing.T) {
	svc := newTestEvalService()

	_, err := svc.EvaluateInContext(bg(), connectReq(&maggiev1.EvaluateInContextRequest{
		Source:  "self",
		Context: &maggiev1.ObjectHandle{Id: "h-nonexistent"},
	}))
	if err == nil {
		t.Fatal("EvaluateInContext with invalid handle should return error")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeNotFound {
			t.Errorf("expected CodeNotFound, got %v", connectErr.Code())
		}
	}
}

// ---------------------------------------------------------------------------
// CheckSyntax
// ---------------------------------------------------------------------------

func TestCheckSyntax_ValidSource(t *testing.T) {
	svc := newTestEvalService()

	resp, err := svc.CheckSyntax(bg(), connectReq(&maggiev1.CheckSyntaxRequest{
		Source: "3 + 4",
	}))
	if err != nil {
		t.Fatalf("CheckSyntax returned error: %v", err)
	}
	if !resp.Msg.Valid {
		t.Error("CheckSyntax should report valid for '3 + 4'")
	}
	if len(resp.Msg.Diagnostics) != 0 {
		t.Errorf("valid source should have no diagnostics, got %d", len(resp.Msg.Diagnostics))
	}
}

func TestCheckSyntax_InvalidSource(t *testing.T) {
	svc := newTestEvalService()

	resp, err := svc.CheckSyntax(bg(), connectReq(&maggiev1.CheckSyntaxRequest{
		Source: "[[[",
	}))
	if err != nil {
		t.Fatalf("CheckSyntax returned error: %v", err)
	}
	if resp.Msg.Valid {
		t.Error("CheckSyntax should report invalid for '[[[' ")
	}
	if len(resp.Msg.Diagnostics) == 0 {
		t.Error("invalid source should have at least one diagnostic")
	}
}

func TestCheckSyntax_EmptySource(t *testing.T) {
	svc := newTestEvalService()

	_, err := svc.CheckSyntax(bg(), connectReq(&maggiev1.CheckSyntaxRequest{
		Source: "",
	}))
	if err == nil {
		t.Fatal("CheckSyntax with empty source should return error")
	}
}
