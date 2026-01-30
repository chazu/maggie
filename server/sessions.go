package server

import (
	"fmt"
	"sync"
	"sync/atomic"

	"github.com/chazu/maggie/vm"
)

// Session represents a workspace session with its own globals.
type Session struct {
	ID      string
	Name    string
	Globals map[string]vm.Value
}

// SessionStore manages workspace sessions.
type SessionStore struct {
	mu       sync.RWMutex
	sessions map[string]*Session
	nextID   atomic.Uint64
	handles  *HandleStore
}

// NewSessionStore creates a new session store.
func NewSessionStore(handles *HandleStore) *SessionStore {
	return &SessionStore{
		sessions: make(map[string]*Session),
		handles:  handles,
	}
}

// Create creates a new session with an optional name.
func (s *SessionStore) Create(name string) *Session {
	id := fmt.Sprintf("s-%d", s.nextID.Add(1))

	session := &Session{
		ID:      id,
		Name:    name,
		Globals: make(map[string]vm.Value),
	}

	s.mu.Lock()
	s.sessions[id] = session
	s.mu.Unlock()

	return session
}

// Get retrieves a session by ID.
func (s *SessionStore) Get(id string) (*Session, bool) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	session, ok := s.sessions[id]
	return session, ok
}

// Destroy removes a session and releases all its handles.
func (s *SessionStore) Destroy(id string) {
	s.mu.Lock()
	delete(s.sessions, id)
	s.mu.Unlock()

	s.handles.ReleaseSession(id)
}
