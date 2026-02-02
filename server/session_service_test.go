package server

import (
	"testing"

	"connectrpc.com/connect"

	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
)

// ---------------------------------------------------------------------------
// CreateSession
// ---------------------------------------------------------------------------

func TestCreateSession_Default(t *testing.T) {
	svc := newTestSessionService()

	resp, err := svc.CreateSession(bg(), connectReq(&maggiev1.CreateSessionRequest{}))
	if err != nil {
		t.Fatalf("CreateSession returned error: %v", err)
	}
	if resp.Msg.SessionId == "" {
		t.Error("CreateSession should return a non-empty session ID")
	}

	// Clean up
	testSessions.Destroy(resp.Msg.SessionId)
}

func TestCreateSession_WithName(t *testing.T) {
	svc := newTestSessionService()

	resp, err := svc.CreateSession(bg(), connectReq(&maggiev1.CreateSessionRequest{
		Name: "my-workspace",
	}))
	if err != nil {
		t.Fatalf("CreateSession returned error: %v", err)
	}
	if resp.Msg.SessionId == "" {
		t.Error("CreateSession should return a non-empty session ID")
	}

	// Verify the session was stored
	session, ok := testSessions.Get(resp.Msg.SessionId)
	if !ok {
		t.Error("session should be retrievable after creation")
	}
	if session.Name != "my-workspace" {
		t.Errorf("Session.Name = %q, want %q", session.Name, "my-workspace")
	}

	// Clean up
	testSessions.Destroy(resp.Msg.SessionId)
}

func TestCreateSession_MultipleSessionsGetUniqueIDs(t *testing.T) {
	svc := newTestSessionService()

	resp1, err := svc.CreateSession(bg(), connectReq(&maggiev1.CreateSessionRequest{Name: "s1"}))
	if err != nil {
		t.Fatalf("CreateSession returned error: %v", err)
	}
	resp2, err := svc.CreateSession(bg(), connectReq(&maggiev1.CreateSessionRequest{Name: "s2"}))
	if err != nil {
		t.Fatalf("CreateSession returned error: %v", err)
	}

	if resp1.Msg.SessionId == resp2.Msg.SessionId {
		t.Error("two sessions should have different IDs")
	}

	// Clean up
	testSessions.Destroy(resp1.Msg.SessionId)
	testSessions.Destroy(resp2.Msg.SessionId)
}

// ---------------------------------------------------------------------------
// DestroySession
// ---------------------------------------------------------------------------

func TestDestroySession_Valid(t *testing.T) {
	svc := newTestSessionService()

	// Create a session first
	createResp, err := svc.CreateSession(bg(), connectReq(&maggiev1.CreateSessionRequest{Name: "to-destroy"}))
	if err != nil {
		t.Fatalf("CreateSession returned error: %v", err)
	}
	sessionID := createResp.Msg.SessionId

	// Destroy it
	_, err = svc.DestroySession(bg(), connectReq(&maggiev1.DestroySessionRequest{
		SessionId: sessionID,
	}))
	if err != nil {
		t.Fatalf("DestroySession returned error: %v", err)
	}

	// Verify it's gone
	_, ok := testSessions.Get(sessionID)
	if ok {
		t.Error("session should not exist after destruction")
	}
}

func TestDestroySession_EmptyId(t *testing.T) {
	svc := newTestSessionService()

	_, err := svc.DestroySession(bg(), connectReq(&maggiev1.DestroySessionRequest{
		SessionId: "",
	}))
	if err == nil {
		t.Fatal("DestroySession with empty session_id should return error")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeInvalidArgument {
			t.Errorf("expected CodeInvalidArgument, got %v", connectErr.Code())
		}
	}
}

func TestDestroySession_NotFound(t *testing.T) {
	svc := newTestSessionService()

	_, err := svc.DestroySession(bg(), connectReq(&maggiev1.DestroySessionRequest{
		SessionId: "s-nonexistent",
	}))
	if err == nil {
		t.Fatal("DestroySession with unknown session_id should return error")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeNotFound {
			t.Errorf("expected CodeNotFound, got %v", connectErr.Code())
		}
	}
}

// ---------------------------------------------------------------------------
// Complete
// ---------------------------------------------------------------------------

func TestComplete_ClassNames(t *testing.T) {
	svc := newTestSessionService()

	resp, err := svc.Complete(bg(), connectReq(&maggiev1.CompleteRequest{
		Prefix: "Obj",
	}))
	if err != nil {
		t.Fatalf("Complete returned error: %v", err)
	}
	if len(resp.Msg.Items) == 0 {
		t.Error("Complete for 'Obj' should return at least Object")
	}
	found := false
	for _, item := range resp.Msg.Items {
		if item.Label == "Object" {
			found = true
			if item.Kind != "class" {
				t.Errorf("Object completion kind = %q, want %q", item.Kind, "class")
			}
			break
		}
	}
	if !found {
		t.Error("Complete for 'Obj' should include 'Object'")
	}
}

func TestComplete_EmptyPrefix(t *testing.T) {
	svc := newTestSessionService()

	resp, err := svc.Complete(bg(), connectReq(&maggiev1.CompleteRequest{
		Prefix: "",
	}))
	if err != nil {
		t.Fatalf("Complete returned error: %v", err)
	}
	// Empty prefix should return no items (as per the implementation)
	if len(resp.Msg.Items) != 0 {
		t.Errorf("Complete with empty prefix should return 0 items, got %d", len(resp.Msg.Items))
	}
}

func TestComplete_Selectors(t *testing.T) {
	svc := newTestSessionService()

	resp, err := svc.Complete(bg(), connectReq(&maggiev1.CompleteRequest{
		Prefix: "isNi",
	}))
	if err != nil {
		t.Fatalf("Complete returned error: %v", err)
	}
	found := false
	for _, item := range resp.Msg.Items {
		if item.Label == "isNil" {
			found = true
			if item.Kind != "selector" {
				t.Errorf("isNil completion kind = %q, want %q", item.Kind, "selector")
			}
			break
		}
	}
	if !found {
		t.Error("Complete for 'isNi' should include 'isNil'")
	}
}

func TestComplete_NoMatches(t *testing.T) {
	svc := newTestSessionService()

	resp, err := svc.Complete(bg(), connectReq(&maggiev1.CompleteRequest{
		Prefix: "xyzZZZnonexistent",
	}))
	if err != nil {
		t.Fatalf("Complete returned error: %v", err)
	}
	if len(resp.Msg.Items) != 0 {
		t.Errorf("Complete for nonsense prefix should return 0 items, got %d", len(resp.Msg.Items))
	}
}

// ---------------------------------------------------------------------------
// SessionStore unit tests (not through gRPC)
// ---------------------------------------------------------------------------

func TestSessionStore_CreateAndGet(t *testing.T) {
	env := newIsolatedEnv()
	defer env.Stop()

	session := env.Sessions.Create("test-session")
	if session.ID == "" {
		t.Error("session should have a non-empty ID")
	}
	if session.Name != "test-session" {
		t.Errorf("session.Name = %q, want %q", session.Name, "test-session")
	}

	got, ok := env.Sessions.Get(session.ID)
	if !ok {
		t.Error("session should be retrievable after creation")
	}
	if got.ID != session.ID {
		t.Errorf("retrieved session ID = %q, want %q", got.ID, session.ID)
	}
}

func TestSessionStore_Destroy(t *testing.T) {
	env := newIsolatedEnv()
	defer env.Stop()

	session := env.Sessions.Create("doomed")
	env.Sessions.Destroy(session.ID)

	_, ok := env.Sessions.Get(session.ID)
	if ok {
		t.Error("session should not be retrievable after destruction")
	}
}

func TestSessionStore_GetMissing(t *testing.T) {
	env := newIsolatedEnv()
	defer env.Stop()

	_, ok := env.Sessions.Get("s-missing")
	if ok {
		t.Error("Get for nonexistent session should return false")
	}
}
