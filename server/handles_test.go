package server

import (
	"sync"
	"testing"

	"github.com/chazu/maggie/vm"
)

// TestHandleStoreConcurrentLookup exercises many goroutines looking up the same
// handle at once. Lookup updates lastUsed while holding only a read lock, so
// with a plain time.Time field this races (and corrupts) the update; the field
// is an atomic.Int64 for exactly this reason. Run under -race to guard it.
func TestHandleStoreConcurrentLookup(t *testing.T) {
	s := NewHandleStore(nil)
	id := s.Create(vm.Nil, "UndefinedObject", "nil", "")

	const G, N = 16, 2000
	var wg sync.WaitGroup
	wg.Add(G)
	for g := 0; g < G; g++ {
		go func() {
			defer wg.Done()
			for i := 0; i < N; i++ {
				if _, ok := s.Lookup(id); !ok {
					t.Errorf("handle %q vanished", id)
					return
				}
			}
		}()
	}
	wg.Wait()
}
