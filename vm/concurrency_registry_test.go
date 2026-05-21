package vm

import (
	"strings"
	"sync"
	"sync/atomic"
	"testing"
)

func TestAllocConcurrencyIDReturnsErrorOnExhaustion(t *testing.T) {
	var counter atomic.Int32
	counter.Store(concurrencyIDMax + 1)

	_, err := allocConcurrencyID(&counter, "channel")
	if err == nil {
		t.Fatal("expected error on ID exhaustion")
	}
	if !strings.Contains(err.Error(), "exhausted") || !strings.Contains(err.Error(), "channel") {
		t.Fatalf("error missing kind/exhausted markers: %q", err)
	}
}

func TestAllocConcurrencyID64ReturnsErrorOnExhaustion(t *testing.T) {
	var counter atomic.Uint64
	counter.Store(concurrencyIDMaxU64 + 2)

	_, err := allocConcurrencyID64(&counter, "process")
	if err == nil {
		t.Fatal("expected error on ID exhaustion")
	}
}

// TestAllocConcurrencyIDConcurrent verifies the helper allocates unique IDs
// under contention. With N goroutines each making M allocations against a
// fresh counter, we expect exactly N*M distinct IDs.
func TestAllocConcurrencyIDConcurrent(t *testing.T) {
	const goroutines = 32
	const perG = 1000

	var counter atomic.Int32
	counter.Store(1) // matches NewConcurrencyRegistry initial state

	var (
		mu  sync.Mutex
		ids = make(map[int]struct{}, goroutines*perG)
	)

	var wg sync.WaitGroup
	wg.Add(goroutines)
	for g := 0; g < goroutines; g++ {
		go func() {
			defer wg.Done()
			local := make([]int, 0, perG)
			for i := 0; i < perG; i++ {
				id, err := allocConcurrencyID(&counter, "channel")
					if err != nil {
						t.Errorf("unexpected error: %v", err)
						return
					}
					local = append(local, id)
			}
			mu.Lock()
			for _, id := range local {
				if _, dup := ids[id]; dup {
					t.Errorf("duplicate ID %d under contention", id)
				}
				ids[id] = struct{}{}
			}
			mu.Unlock()
		}()
	}
	wg.Wait()

	if got := len(ids); got != goroutines*perG {
		t.Fatalf("expected %d unique IDs, got %d", goroutines*perG, got)
	}
}

// TestWeakRefIDExhaustionPanics verifies the weak-ref counter panics rather
// than wrapping past 2^24-1.
func TestWeakRefIDExhaustionPanics(t *testing.T) {
	or := NewObjectRegistry()
	or.weakRefCounter.Store(weakRefIDMax) // next Add will exceed max

	defer func() {
		r := recover()
		if r == nil {
			t.Fatal("expected panic on weak ref ID exhaustion, got nil")
		}
		msg, ok := r.(string)
		if !ok {
			t.Fatalf("expected string panic, got %T: %v", r, r)
		}
		if !strings.Contains(msg, "weak ref") {
			t.Fatalf("panic message missing 'weak ref': %q", msg)
		}
	}()
	or.NextWeakRefID()
}
