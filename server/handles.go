package server

import (
	"fmt"
	"sync"
	"sync/atomic"
	"time"

	"github.com/chazu/maggie/vm"
)

// handle is a server-side reference to a VM object.
type handle struct {
	id        string
	value     vm.Value
	className string
	display   string
	sessionID string
	created   time.Time
	lastUsed  time.Time
}

// HandleStore maps opaque string IDs to VM values. Each handle holds the
// vm.Value (which carries the heap object's pointer), so a stored handle is
// itself a strong reference — Go's GC keeps the object alive for as long as the
// handle lives; no explicit pinning is needed.
type HandleStore struct {
	mu      sync.RWMutex
	handles map[string]*handle
	nextID  atomic.Uint64
	worker  *VMWorker
}

// NewHandleStore creates a new handle store.
func NewHandleStore(worker *VMWorker) *HandleStore {
	return &HandleStore{
		handles: make(map[string]*handle),
		worker:  worker,
	}
}

// Create registers a value and returns an opaque handle ID. The stored
// vm.Value keeps any heap object it references alive via Go's GC.
func (s *HandleStore) Create(value vm.Value, className, display, sessionID string) string {
	id := fmt.Sprintf("h-%d", s.nextID.Add(1))

	s.mu.Lock()
	defer s.mu.Unlock()

	now := time.Now()
	s.handles[id] = &handle{
		id:        id,
		value:     value,
		className: className,
		display:   display,
		sessionID: sessionID,
		created:   now,
		lastUsed:  now,
	}

	return id
}

// Lookup retrieves the value for a handle. Returns the value and true,
// or Nil and false if the handle doesn't exist.
func (s *HandleStore) Lookup(id string) (vm.Value, bool) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	h, ok := s.handles[id]
	if !ok {
		return vm.Nil, false
	}
	h.lastUsed = time.Now()
	return h.value, true
}

// Release removes a handle. Dropping it from the map releases the strong
// reference; Go's GC reclaims the object once nothing else holds it.
func (s *HandleStore) Release(id string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	delete(s.handles, id)
}

// ReleaseSession releases all handles owned by a session.
func (s *HandleStore) ReleaseSession(sessionID string) {
	s.mu.Lock()
	defer s.mu.Unlock()

	for id, h := range s.handles {
		if h.sessionID == sessionID {
			delete(s.handles, id)
		}
	}
}

// Sweep removes handles that haven't been accessed within the TTL.
func (s *HandleStore) Sweep(ttl time.Duration) int {
	s.mu.Lock()
	defer s.mu.Unlock()

	cutoff := time.Now().Add(-ttl)
	removed := 0
	for id, h := range s.handles {
		if h.lastUsed.Before(cutoff) {
			delete(s.handles, id)
			removed++
		}
	}
	return removed
}

// StartSweeper runs periodic TTL sweeps in the background.
// Returns a stop function.
func (s *HandleStore) StartSweeper(interval, ttl time.Duration) func() {
	ticker := time.NewTicker(interval)
	done := make(chan struct{})
	go func() {
		for {
			select {
			case <-ticker.C:
				s.Sweep(ttl)
			case <-done:
				ticker.Stop()
				return
			}
		}
	}()
	return func() { close(done) }
}
