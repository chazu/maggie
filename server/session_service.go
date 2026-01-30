package server

import (
	"context"
	"fmt"
	"strings"

	"connectrpc.com/connect"

	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
	"github.com/chazu/maggie/gen/maggie/v1/maggiev1connect"
	"github.com/chazu/maggie/vm"
)

// SessionServiceImpl implements the SessionService gRPC/Connect handler.
type SessionServiceImpl struct {
	maggiev1connect.UnimplementedSessionServiceHandler
	worker   *VMWorker
	sessions *SessionStore
}

// NewSessionServiceImpl creates a SessionServiceImpl.
func NewSessionServiceImpl(worker *VMWorker, sessions *SessionStore) *SessionServiceImpl {
	return &SessionServiceImpl{
		worker:   worker,
		sessions: sessions,
	}
}

// CreateSession creates a new workspace session.
func (s *SessionServiceImpl) CreateSession(
	ctx context.Context,
	req *connect.Request[maggiev1.CreateSessionRequest],
) (*connect.Response[maggiev1.CreateSessionResponse], error) {
	session := s.sessions.Create(req.Msg.Name)
	return connect.NewResponse(&maggiev1.CreateSessionResponse{
		SessionId: session.ID,
	}), nil
}

// DestroySession destroys a session and releases its handles.
func (s *SessionServiceImpl) DestroySession(
	ctx context.Context,
	req *connect.Request[maggiev1.DestroySessionRequest],
) (*connect.Response[maggiev1.DestroySessionResponse], error) {
	if req.Msg.SessionId == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("session_id is required"))
	}

	_, ok := s.sessions.Get(req.Msg.SessionId)
	if !ok {
		return nil, connect.NewError(connect.CodeNotFound, fmt.Errorf("session %q not found", req.Msg.SessionId))
	}

	s.sessions.Destroy(req.Msg.SessionId)
	return connect.NewResponse(&maggiev1.DestroySessionResponse{}), nil
}

// Complete returns completion candidates matching the given prefix.
func (s *SessionServiceImpl) Complete(
	ctx context.Context,
	req *connect.Request[maggiev1.CompleteRequest],
) (*connect.Response[maggiev1.CompleteResponse], error) {
	prefix := req.Msg.Prefix
	if prefix == "" {
		return connect.NewResponse(&maggiev1.CompleteResponse{}), nil
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		return s.complete(v, prefix)
	})
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}

	return connect.NewResponse(result.(*maggiev1.CompleteResponse)), nil
}

// complete gathers completion candidates from the VM.
// Must be called on the VM worker goroutine.
func (s *SessionServiceImpl) complete(v *vm.VM, prefix string) *maggiev1.CompleteResponse {
	var items []*maggiev1.CompletionItem

	lowerPrefix := strings.ToLower(prefix)

	// Complete class names
	for _, cls := range v.Classes.All() {
		if strings.HasPrefix(strings.ToLower(cls.Name), lowerPrefix) {
			items = append(items, &maggiev1.CompletionItem{
				Label:      cls.Name,
				Kind:       "class",
				Detail:     "class",
				InsertText: cls.Name,
			})
		}
	}

	// Complete global names
	for name := range v.Globals {
		if strings.HasPrefix(strings.ToLower(name), lowerPrefix) {
			// Skip class names already added
			if v.Classes.Lookup(name) != nil {
				continue
			}
			items = append(items, &maggiev1.CompletionItem{
				Label:      name,
				Kind:       "global",
				Detail:     "global",
				InsertText: name,
			})
		}
	}

	// Complete selectors (for message sends)
	for _, name := range v.Selectors.All() {
		if strings.HasPrefix(strings.ToLower(name), lowerPrefix) {
			items = append(items, &maggiev1.CompletionItem{
				Label:      name,
				Kind:       "selector",
				Detail:     "selector",
				InsertText: name,
			})
		}
	}

	// Limit results to prevent oversized responses
	const maxItems = 100
	if len(items) > maxItems {
		items = items[:maxItems]
	}

	return &maggiev1.CompleteResponse{Items: items}
}
