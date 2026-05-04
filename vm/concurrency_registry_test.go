package vm

import (
	"strings"
	"sync"
	"sync/atomic"
	"testing"
)

// TestAllocConcurrencyIDPanicsOnExhaustion verifies the helper panics rather
// than silently wrapping when the 24-bit ID space is exhausted. Exhaustion
// is a real (if pathological) bug class; silent wrap would alias a new
// primitive's ID to a live one and corrupt sends/receives.
func TestAllocConcurrencyIDPanicsOnExhaustion(t *testing.T) {
	var counter atomic.Int32
	// Pre-position the counter so the next Add overflows past concurrencyIDMax.
	counter.Store(concurrencyIDMax + 1)

	defer func() {
		r := recover()
		if r == nil {
			t.Fatal("expected panic on ID exhaustion, got nil")
		}
		msg, ok := r.(string)
		if !ok {
			t.Fatalf("expected string panic, got %T: %v", r, r)
		}
		if !strings.Contains(msg, "exhausted") || !strings.Contains(msg, "channel") {
			t.Fatalf("panic message missing kind/exhausted markers: %q", msg)
		}
	}()
	allocConcurrencyID(&counter, "channel")
}

// TestAllocConcurrencyID64PanicsOnExhaustion: same invariant for the uint64
// helper used by processes.
func TestAllocConcurrencyID64PanicsOnExhaustion(t *testing.T) {
	var counter atomic.Uint64
	counter.Store(concurrencyIDMaxU64 + 2) // counter.Add(1) - 1 lands above max

	defer func() {
		r := recover()
		if r == nil {
			t.Fatal("expected panic on ID exhaustion, got nil")
		}
	}()
	allocConcurrencyID64(&counter, "process")
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
				local = append(local, allocConcurrencyID(&counter, "channel"))
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
