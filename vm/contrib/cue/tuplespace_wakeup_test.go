package cue

import (
	"testing"
	"time"

	vm "github.com/chazu/maggie/vm"
)

// These tests drive the REAL TupleSpace primitives (via Send) rather than
// re-implementing their logic, so they catch wakeup bugs in TupleSpaceObject.put.

func newPrimTupleSpace(t *testing.T) (*vm.VM, vm.Value, *TupleSpaceObject) {
	t.Helper()
	v := vm.NewVM()
	cls := v.MustGlobal("TupleSpace")
	tsVal := v.Send(cls, "new", nil)
	ts := vmGetTupleSpace(v, tsVal)
	if ts == nil {
		t.Fatal("TupleSpace new did not answer a tuple space")
	}
	return v, tsVal, ts
}

func tmplVal(v *vm.VM, src string) vm.Value {
	return vmRegisterCueValue(v, compileCueTemplate(src))
}

// blockOn runs a (blocking) Send on its own goroutine with its own interpreter
// and delivers the answer on the returned channel.
func blockOn(v *vm.VM, recv vm.Value, sel string, args ...vm.Value) <-chan vm.Value {
	out := make(chan vm.Value, 1)
	go v.RunIsolated(func() {
		out <- v.Send(recv, sel, args)
	})
	return out
}

func waitForWaiters(t *testing.T, ts *TupleSpaceObject, n int) {
	t.Helper()
	deadline := time.Now().Add(2 * time.Second)
	for time.Now().Before(deadline) {
		ts.mu.Lock()
		got := len(ts.waiters)
		ts.mu.Unlock()
		if got >= n {
			return
		}
		time.Sleep(time.Millisecond)
	}
	t.Fatalf("timed out waiting for %d parked waiters", n)
}

func recvOrFail(t *testing.T, what string, ch <-chan vm.Value) vm.Value {
	t.Helper()
	select {
	case r := <-ch:
		return r
	case <-time.After(2 * time.Second):
		t.Fatalf("%s: waiter never woke", what)
		return vm.Nil
	}
}

func tsSize(v *vm.VM, tsVal vm.Value) int64 {
	return v.Send(tsVal, "primSize", nil).SmallInt()
}

// out: 'hello'; [inAll: {int. string}] fork; out: 42 — the compound waiter
// must wake. Waiter notification used to run BEFORE the new tuple was stored, so the
// compound branch only saw 'hello' and the waiter stayed blocked forever.
func TestTupleSpaceInAllWakesOnCompletingOut(t *testing.T) {
	v, tsVal, ts := newPrimTupleSpace(t)
	v.Send(tsVal, "primOut:", []vm.Value{v.Registry().NewStringValue("hello")})

	arr := v.NewArrayWithElements([]vm.Value{tmplVal(v, "int"), tmplVal(v, "string")})
	res := blockOn(v, tsVal, "primInAll:", arr)
	waitForWaiters(t, ts, 1)

	v.Send(tsVal, "primOut:", []vm.Value{vm.FromSmallInt(42)})
	r := recvOrFail(t, "inAll:", res)
	obj := vm.ObjectFromValue(r)
	if obj == nil || obj.NumSlots() != 2 {
		t.Fatalf("inAll: want 2-element array, got %v", r)
	}
	if n := tsSize(v, tsVal); n != 0 {
		t.Fatalf("both tuples should be consumed, size = %d", n)
	}
}

// Same bug for choice (inAny:) waiters and for the affine / withContext outs.
func TestTupleSpaceInAnyWakesOnOut(t *testing.T) {
	for _, sel := range []string{"primOut:", "primOutAffine:ttl:", "primOut:withContext:"} {
		v, tsVal, ts := newPrimTupleSpace(t)
		arr := v.NewArrayWithElements([]vm.Value{tmplVal(v, "string"), tmplVal(v, "int")})
		res := blockOn(v, tsVal, "primInAny:", arr)
		waitForWaiters(t, ts, 1)

		args := []vm.Value{vm.FromSmallInt(7)}
		switch sel {
		case "primOutAffine:ttl:":
			args = append(args, vm.FromSmallInt(60000))
		case "primOut:withContext:":
			ctxCls := v.ClassValue(v.CancellationContextClass)
			args = append(args, v.Send(ctxCls, "background", nil))
		}
		v.Send(tsVal, sel, args)
		if r := recvOrFail(t, sel+" -> inAny:", res); r != vm.FromSmallInt(7) {
			t.Fatalf("%s: inAny: want 7, got %v", sel, r)
		}
		if n := tsSize(v, tsVal); n != 0 {
			t.Fatalf("%s: inAny: should consume the tuple, size = %d", sel, n)
		}
	}
}

// A parked read: (non-consuming) must not swallow the only wakeup: a parked
// in: for the same template must also receive the tuple.
func TestTupleSpaceReadWaiterDoesNotStarveIn(t *testing.T) {
	for _, sel := range []string{"primOut:", "primOutPersistent:"} {
		v, tsVal, ts := newPrimTupleSpace(t)
		reader := blockOn(v, tsVal, "primRead:", tmplVal(v, "int"))
		waitForWaiters(t, ts, 1)
		taker := blockOn(v, tsVal, "primIn:", tmplVal(v, "int"))
		waitForWaiters(t, ts, 2)

		v.Send(tsVal, sel, []vm.Value{vm.FromSmallInt(5)})
		if r := recvOrFail(t, sel+" read:", reader); r != vm.FromSmallInt(5) {
			t.Fatalf("%s: read: want 5, got %v", sel, r)
		}
		if r := recvOrFail(t, sel+" in:", taker); r != vm.FromSmallInt(5) {
			t.Fatalf("%s: in: want 5, got %v", sel, r)
		}
		want := int64(0)
		if sel == "primOutPersistent:" {
			want = 1
		}
		if n := tsSize(v, tsVal); n != want {
			t.Fatalf("%s: size after read+in = %d, want %d", sel, n, want)
		}
	}
}

// A read: waiter woken by an affine tuple must leave the ORIGINAL entry in
// place — it used to be re-put as a Linear tuple, dropping the TTL.
func TestTupleSpaceReadWaiterPreservesAffineTTL(t *testing.T) {
	v, tsVal, ts := newPrimTupleSpace(t)
	reader := blockOn(v, tsVal, "primRead:", tmplVal(v, "int"))
	waitForWaiters(t, ts, 1)

	v.Send(tsVal, "primOutAffine:ttl:", []vm.Value{vm.FromSmallInt(9), vm.FromSmallInt(30)})
	if r := recvOrFail(t, "read:", reader); r != vm.FromSmallInt(9) {
		t.Fatalf("read: want 9, got %v", r)
	}
	time.Sleep(80 * time.Millisecond)
	if r := v.Send(tsVal, "primTryIn:", []vm.Value{tmplVal(v, "int")}); r != vm.Nil {
		t.Fatalf("affine tuple should have expired after its TTL, tryIn: got %v", r)
	}
}

// A persistent tuple satisfies every parked waiter and stays in the space.
func TestTupleSpacePersistentWakesAllWaiters(t *testing.T) {
	v, tsVal, ts := newPrimTupleSpace(t)
	a := blockOn(v, tsVal, "primIn:", tmplVal(v, "int"))
	waitForWaiters(t, ts, 1)
	b := blockOn(v, tsVal, "primIn:", tmplVal(v, "int"))
	waitForWaiters(t, ts, 2)

	v.Send(tsVal, "primOutPersistent:", []vm.Value{vm.FromSmallInt(3)})
	recvOrFail(t, "first in:", a)
	recvOrFail(t, "second in:", b)
	if n := tsSize(v, tsVal); n != 1 {
		t.Fatalf("persistent tuple must remain, size = %d", n)
	}
}
