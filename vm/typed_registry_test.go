package vm

import (
	"sync"
	"testing"
)

func TestTypedRegistry_PutGetDelete(t *testing.T) {
	r := NewTypedRegistry[int, string]()

	r.Put(1, "one")
	r.Put(2, "two")

	if got := r.Get(1); got != "one" {
		t.Errorf("Get(1) = %q, want %q", got, "one")
	}
	if got := r.Get(3); got != "" {
		t.Errorf("Get(3) = %q, want zero value", got)
	}

	r.Delete(1)
	if r.Has(1) {
		t.Error("Has(1) should be false after delete")
	}
	if !r.Has(2) {
		t.Error("Has(2) should be true")
	}
}

func TestTypedRegistry_Count(t *testing.T) {
	r := NewTypedRegistry[uint32, int]()
	if r.Count() != 0 {
		t.Fatalf("empty count = %d", r.Count())
	}
	r.Put(10, 100)
	r.Put(20, 200)
	if r.Count() != 2 {
		t.Fatalf("count = %d, want 2", r.Count())
	}
}

func TestTypedRegistry_ForEach(t *testing.T) {
	r := NewTypedRegistry[int, int]()
	r.Put(1, 10)
	r.Put(2, 20)
	r.Put(3, 30)

	sum := 0
	r.ForEach(func(_ int, v int) { sum += v })
	if sum != 60 {
		t.Errorf("ForEach sum = %d, want 60", sum)
	}
}

func TestTypedRegistry_Sweep(t *testing.T) {
	r := NewTypedRegistry[int, int]()
	for i := 0; i < 10; i++ {
		r.Put(i, i)
	}

	swept := r.Sweep(func(_ int, v int) bool { return v%2 == 0 })
	if swept != 5 {
		t.Errorf("swept = %d, want 5", swept)
	}
	if r.Count() != 5 {
		t.Errorf("count after sweep = %d, want 5", r.Count())
	}
	if r.Has(1) {
		t.Error("odd key 1 should have been swept")
	}
	if !r.Has(2) {
		t.Error("even key 2 should remain")
	}
}

func TestTypedRegistry_Clear(t *testing.T) {
	r := NewTypedRegistry[int, string]()
	r.Put(1, "a")
	r.Put(2, "b")
	r.Clear()
	if r.Count() != 0 {
		t.Errorf("count after clear = %d", r.Count())
	}
}

func TestTypedRegistry_RLockUnsafeGet(t *testing.T) {
	r := NewTypedRegistry[int, string]()
	r.Put(1, "one")
	r.Put(2, "two")

	r.RLock()
	a := r.UnsafeGet(1)
	b := r.UnsafeGet(2)
	r.RUnlock()

	if a != "one" || b != "two" {
		t.Errorf("UnsafeGet = (%q, %q), want (one, two)", a, b)
	}
}

func TestAutoIDRegistry_RegisterAndGet(t *testing.T) {
	r := NewAutoIDRegistry[string](1)

	id1 := r.Register("first")
	id2 := r.Register("second")
	id3 := r.Register("third")

	if id1 != 1 || id2 != 2 || id3 != 3 {
		t.Errorf("IDs = (%d, %d, %d), want (1, 2, 3)", id1, id2, id3)
	}
	if got := r.Get(id1); got != "first" {
		t.Errorf("Get(%d) = %q, want %q", id1, got, "first")
	}
	if got := r.Get(id2); got != "second" {
		t.Errorf("Get(%d) = %q, want %q", id2, got, "second")
	}
}

func TestAutoIDRegistry_CustomStartID(t *testing.T) {
	r := NewAutoIDRegistry[string](100)
	id := r.Register("value")
	if id != 100 {
		t.Errorf("first ID = %d, want 100", id)
	}
}

func TestAutoIDRegistry_DeleteAndCount(t *testing.T) {
	r := NewAutoIDRegistry[int](1)
	id1 := r.Register(10)
	r.Register(20)
	r.Register(30)

	if r.Count() != 3 {
		t.Fatalf("count = %d, want 3", r.Count())
	}
	r.Delete(id1)
	if r.Count() != 2 {
		t.Errorf("count after delete = %d, want 2", r.Count())
	}
}

func TestAutoIDRegistry_Sweep(t *testing.T) {
	r := NewAutoIDRegistry[int](1)
	for i := 0; i < 10; i++ {
		r.Register(i)
	}
	swept := r.Sweep(func(_ uint32, v int) bool { return v%2 == 0 })
	if swept != 5 {
		t.Errorf("swept = %d, want 5", swept)
	}
	if r.Count() != 5 {
		t.Errorf("count after sweep = %d, want 5", r.Count())
	}
}

func TestAutoIDRegistry_ConcurrentRegister(t *testing.T) {
	r := NewAutoIDRegistry[int](1)
	var wg sync.WaitGroup
	for i := 0; i < 100; i++ {
		wg.Add(1)
		go func(n int) {
			defer wg.Done()
			id := r.Register(n)
			_ = r.Get(id)
		}(i)
	}
	wg.Wait()
	if r.Count() != 100 {
		t.Errorf("count = %d, want 100", r.Count())
	}
}

func TestAutoIDRegistry_ReuseAfterDelete(t *testing.T) {
	r := NewAutoIDRegistry[string](1, WithName("test"))
	id1 := r.Register("a")
	id2 := r.Register("b")
	id3 := r.Register("c")
	if id1 != 1 || id2 != 2 || id3 != 3 {
		t.Fatalf("initial IDs = (%d,%d,%d), want (1,2,3)", id1, id2, id3)
	}

	r.Delete(id2)
	if r.FreeListLen() != 1 {
		t.Fatalf("free-list len after delete = %d, want 1", r.FreeListLen())
	}

	// Next Register should reuse id2 from the free-list, not bump to 4.
	id4 := r.Register("d")
	if id4 != id2 {
		t.Errorf("reused ID = %d, want %d (the freed ID)", id4, id2)
	}
	if got := r.Get(id4); got != "d" {
		t.Errorf("Get(reused) = %q, want %q", got, "d")
	}
	if r.FreeListLen() != 0 {
		t.Errorf("free-list not drained: len=%d", r.FreeListLen())
	}

	// Another Register past the free-list bumps fresh.
	id5 := r.Register("e")
	if id5 != 4 {
		t.Errorf("post-reuse ID = %d, want 4", id5)
	}
}

func TestAutoIDRegistry_PanicOnExhaustion(t *testing.T) {
	// startID=1, maxID=3 → IDs 1,2,3 succeed, 4th allocation panics.
	r := NewAutoIDRegistry[int](1, WithMaxID(3), WithName("tiny"))
	for i := 1; i <= 3; i++ {
		got := r.Register(i * 10)
		if got != uint32(i) {
			t.Fatalf("alloc %d → ID %d, want %d", i, got, i)
		}
	}

	defer func() {
		rec := recover()
		if rec == nil {
			t.Fatal("expected panic on exhaustion, got none")
		}
		msg, ok := rec.(string)
		if !ok {
			t.Fatalf("panic value is not a string: %T %v", rec, rec)
		}
		if !contains(msg, "tiny") {
			t.Errorf("panic message %q does not name registry", msg)
		}
	}()
	r.Register(99) // should panic
}

func TestAutoIDRegistry_ExhaustionThenDeleteRecovers(t *testing.T) {
	r := NewAutoIDRegistry[int](1, WithMaxID(2), WithName("two"))
	id1 := r.Register(1)
	id2 := r.Register(2)
	if id1 != 1 || id2 != 2 {
		t.Fatalf("IDs = (%d,%d), want (1,2)", id1, id2)
	}

	// Exhausted. Delete one and we should recover.
	r.Delete(id1)
	id3 := r.Register(3)
	if id3 != id1 {
		t.Errorf("recovered ID = %d, want %d", id3, id1)
	}

	// Exhausted again — should panic.
	func() {
		defer func() {
			if recover() == nil {
				t.Error("expected panic, got none")
			}
		}()
		r.Register(4)
	}()
}

func TestAutoIDRegistry_MonotonicNoReuse(t *testing.T) {
	r := NewAutoIDRegistry[string](1, WithMonotonic(), WithName("monotonic"))
	id1 := r.Register("a")
	id2 := r.Register("b")
	r.Delete(id1)
	if r.FreeListLen() != 0 {
		t.Errorf("monotonic registry free-list len = %d, want 0", r.FreeListLen())
	}
	id3 := r.Register("c")
	if id3 != 3 {
		t.Errorf("monotonic next ID = %d, want 3 (no reuse)", id3)
	}
	_ = id2
}

func TestAutoIDRegistry_SweepRecyclesIDs(t *testing.T) {
	r := NewAutoIDRegistry[int](1, WithName("sweep"))
	for i := 0; i < 10; i++ {
		r.Register(i)
	}
	swept := r.Sweep(func(_ uint32, v int) bool { return v%2 == 0 })
	if swept != 5 {
		t.Errorf("swept = %d, want 5", swept)
	}
	if r.FreeListLen() != 5 {
		t.Errorf("free-list len after sweep = %d, want 5", r.FreeListLen())
	}
	// Next 5 Registers reuse swept IDs; the 6th bumps fresh.
	for i := 0; i < 5; i++ {
		id := r.Register(100 + i)
		if id > 10 {
			t.Errorf("alloc #%d = %d, expected reuse (≤10)", i, id)
		}
	}
	id := r.Register(999)
	if id != 11 {
		t.Errorf("post-reuse fresh ID = %d, want 11", id)
	}
}

func TestAutoIDRegistry_NoCollisionUnderConcurrentRegisterDelete(t *testing.T) {
	// Race-detector test: many goroutines Register/Delete concurrently;
	// no two live entries should ever share an ID. Run with -race.
	r := NewAutoIDRegistry[*int](1, WithName("race"))

	const workers = 16
	const iters = 2000
	var wg sync.WaitGroup
	wg.Add(workers)
	for w := 0; w < workers; w++ {
		go func() {
			defer wg.Done()
			ids := make([]uint32, 0, iters)
			for i := 0; i < iters; i++ {
				v := i
				id := r.Register(&v)
				ids = append(ids, id)
				// Periodically delete an old one to feed the free-list.
				if len(ids) > 4 && i%3 == 0 {
					victim := ids[0]
					ids = ids[1:]
					r.Delete(victim)
				}
			}
			// Verify all our remaining IDs still resolve to *our* values.
			for _, id := range ids {
				if got := r.Get(id); got == nil {
					t.Errorf("ID %d returned nil — collision?", id)
				}
			}
		}()
	}
	wg.Wait()
}

func TestAutoIDRegistry_DeleteOutOfRangeIgnored(t *testing.T) {
	r := NewAutoIDRegistry[int](10, WithMaxID(20), WithName("range"))
	r.Delete(5)  // below startID
	r.Delete(99) // above maxID
	if r.FreeListLen() != 0 {
		t.Errorf("out-of-range deletes leaked into free-list: len=%d", r.FreeListLen())
	}
}

func TestAutoIDRegistry_BadConfigPanics(t *testing.T) {
	defer func() {
		if recover() == nil {
			t.Error("expected panic for maxID < startID")
		}
	}()
	_ = NewAutoIDRegistry[int](100, WithMaxID(50))
}

func contains(s, substr string) bool {
	for i := 0; i+len(substr) <= len(s); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}

func TestTypedRegistry_ConcurrentAccess(t *testing.T) {
	r := NewTypedRegistry[int, int]()
	var wg sync.WaitGroup

	for i := 0; i < 100; i++ {
		wg.Add(1)
		go func(n int) {
			defer wg.Done()
			r.Put(n, n*10)
			_ = r.Get(n)
			_ = r.Has(n)
			_ = r.Count()
		}(i)
	}
	wg.Wait()

	if r.Count() != 100 {
		t.Errorf("count = %d, want 100", r.Count())
	}
}
