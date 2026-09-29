package vm

import (
	"testing"
	"time"
)

// expectSignal runs fn and fails unless it signals a catchable Maggie
// exception (SignaledException panic).
func expectSignal(t *testing.T, what string, fn func()) {
	t.Helper()
	defer func() {
		t.Helper()
		r := recover()
		if r == nil {
			t.Fatalf("%s: expected a signaled Error, got no signal", what)
		}
		if _, ok := r.(SignaledException); !ok {
			t.Fatalf("%s: expected SignaledException, got %T: %v", what, r, r)
		}
	}()
	fn()
}

// TestWaitGroupNegativeAddSignalsWithoutCorrupting guards the regression where
// `add:` called wg.Add(n) before any check: a negative add panicked inside Go's
// WaitGroup AFTER the counter had already gone to -1, so the group was left
// corrupted — later `wait` returned immediately and a subsequent done/fork
// panicked 'negative WaitGroup counter'.
func TestWaitGroupNegativeAddSignalsWithoutCorrupting(t *testing.T) {
	for _, sel := range []string{"add:", "primAdd:"} {
		v := NewVM()
		wg := v.Send(v.classValue(v.WaitGroupClass), "new", nil)

		expectSignal(t, sel+" -1 on empty group", func() {
			v.Send(wg, sel, []Value{FromSmallInt(-1)})
		})
		if c := v.Send(wg, "count", nil); c.SmallInt() != 0 {
			t.Fatalf("%s: count after rejected add: -1 = %v, want 0", sel, c)
		}

		// The group must still work: add 1, done from another goroutine,
		// wait must block until then.
		v.Send(wg, sel, []Value{FromSmallInt(1)})
		expectSignal(t, sel+" -2 with count 1", func() {
			v.Send(wg, sel, []Value{FromSmallInt(-2)})
		})
		if c := v.Send(wg, "count", nil); c.SmallInt() != 1 {
			t.Fatalf("%s: count after rejected add: -2 = %v, want 1", sel, c)
		}

		w := v.getWaitGroup(wg)
		waited := make(chan struct{})
		go func() {
			w.wg.Wait()
			close(waited)
		}()
		select {
		case <-waited:
			t.Fatalf("%s: wait returned while counter is 1", sel)
		case <-time.After(20 * time.Millisecond):
		}

		// A legal negative add (1 + -1 = 0) releases the waiter.
		v.Send(wg, sel, []Value{FromSmallInt(-1)})
		select {
		case <-waited:
		case <-time.After(time.Second):
			t.Fatalf("%s: wait did not return after counter reached 0", sel)
		}
	}
}

// TestWaitGroupAddOverflowSignals guards the int32(n) truncation: an add whose
// total exceeds the 32-bit counter used by sync.WaitGroup must signal rather
// than silently wrap (2^32 would wrap to 0 and desync count from the real wg).
func TestWaitGroupAddOverflowSignals(t *testing.T) {
	v := NewVM()
	wg := v.Send(v.classValue(v.WaitGroupClass), "new", nil)
	expectSignal(t, "add: 2^32", func() {
		v.Send(wg, "add:", []Value{FromSmallInt(1 << 32)})
	})
	if c := v.Send(wg, "count", nil); c.SmallInt() != 0 {
		t.Fatalf("count after rejected huge add = %v, want 0", c)
	}
}

// TestWaitGroupAddNonIntegerSignals: a non-Integer argument is a programmer
// error (failure doctrine) — it must signal, not answer nil.
func TestWaitGroupAddNonIntegerSignals(t *testing.T) {
	v := NewVM()
	wg := v.Send(v.classValue(v.WaitGroupClass), "new", nil)
	expectSignal(t, "add: 'x'", func() {
		v.Send(wg, "add:", []Value{v.registry.NewStringValue("x")})
	})
}
