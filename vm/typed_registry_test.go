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
