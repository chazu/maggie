package vm

import (
	"sync"
	"testing"
	"time"

	"cuelang.org/go/cue/cuecontext"
)

func compileCueTemplate(src string) *CueValueObject {
	ctx := cuecontext.New()
	val := ctx.CompileString(src)
	return &CueValueObject{val: val}
}

// ---------------------------------------------------------------------------
// Basic out / tryIn / tryRead
// ---------------------------------------------------------------------------

func TestTupleSpaceOutAndTryIn(t *testing.T) {
	vm := NewVM()
	ts := &TupleSpaceObject{}

	// Empty space
	tmpl := compileCueTemplate("int")
	if vm.matchTuple(tmpl, FromSmallInt(42)) != true {
		t.Fatal("int template should match 42")
	}

	// Out then tryIn
	ts.tuples = append(ts.tuples, FromSmallInt(42))
	if len(ts.tuples) != 1 {
		t.Fatal("expected 1 tuple")
	}

	// Manual tryIn logic
	for i, tuple := range ts.tuples {
		if vm.matchTuple(tmpl, tuple) {
			ts.tuples = append(ts.tuples[:i], ts.tuples[i+1:]...)
			if tuple != FromSmallInt(42) {
				t.Errorf("expected 42, got something else")
			}
			break
		}
	}
	if len(ts.tuples) != 0 {
		t.Error("tuple should have been consumed")
	}
}

func TestTupleSpaceTryReadDoesNotConsume(t *testing.T) {
	vm := NewVM()
	ts := &TupleSpaceObject{}
	ts.tuples = append(ts.tuples, FromSmallInt(42))

	tmpl := compileCueTemplate("int")

	// tryRead should find it but not remove
	found := false
	for _, tuple := range ts.tuples {
		if vm.matchTuple(tmpl, tuple) {
			found = true
			break
		}
	}
	if !found {
		t.Error("tryRead should find 42")
	}
	if len(ts.tuples) != 1 {
		t.Error("tryRead should not consume the tuple")
	}
}

func TestTupleSpaceTemplateSelectsCorrectTuple(t *testing.T) {
	vm := NewVM()
	ts := &TupleSpaceObject{}
	ts.tuples = append(ts.tuples, vm.registry.NewStringValue("hello"))
	ts.tuples = append(ts.tuples, FromSmallInt(42))
	ts.tuples = append(ts.tuples, vm.registry.NewStringValue("world"))

	// int template should match 42, not the strings
	tmpl := compileCueTemplate("int")
	for i, tuple := range ts.tuples {
		if vm.matchTuple(tmpl, tuple) {
			if tuple != FromSmallInt(42) {
				t.Error("int template should match 42")
			}
			ts.tuples = append(ts.tuples[:i], ts.tuples[i+1:]...)
			break
		}
	}
	if len(ts.tuples) != 2 {
		t.Errorf("expected 2 remaining tuples, got %d", len(ts.tuples))
	}
}

func TestTupleSpaceRangeTemplate(t *testing.T) {
	vm := NewVM()

	tmpl := compileCueTemplate(">0 & <100")
	if !vm.matchTuple(tmpl, FromSmallInt(50)) {
		t.Error(">0 & <100 should match 50")
	}
	if vm.matchTuple(tmpl, FromSmallInt(200)) {
		t.Error(">0 & <100 should not match 200")
	}
	if vm.matchTuple(tmpl, FromSmallInt(0)) {
		t.Error(">0 & <100 should not match 0")
	}
}

func TestTupleSpaceStringTemplate(t *testing.T) {
	vm := NewVM()

	tmpl := compileCueTemplate("string")
	if !vm.matchTuple(tmpl, vm.registry.NewStringValue("hello")) {
		t.Error("string template should match 'hello'")
	}
	if vm.matchTuple(tmpl, FromSmallInt(42)) {
		t.Error("string template should not match 42")
	}
}

// ---------------------------------------------------------------------------
// Blocking in: / read: via goroutines
// ---------------------------------------------------------------------------

func TestTupleSpaceBlockingIn(t *testing.T) {
	vm := NewVM()
	ts := &TupleSpaceObject{}
	tsVal := vm.vmRegisterTupleSpace(ts)
	_ = tsVal

	tmpl := compileCueTemplate("int")
	var result Value
	var wg sync.WaitGroup

	wg.Add(1)
	go func() {
		defer wg.Done()
		// Block until an int tuple appears
		ts.mu.Lock()
		for i, tuple := range ts.tuples {
			if vm.matchTuple(tmpl, tuple) {
				result = tuple
				ts.tuples = append(ts.tuples[:i], ts.tuples[i+1:]...)
				ts.mu.Unlock()
				return
			}
		}
		// Not found — register waiter
		w := &tupleWaiter{template: tmpl, ch: make(chan Value, 1), consume: true}
		ts.waiters = append(ts.waiters, w)
		ts.mu.Unlock()
		result = <-w.ch
	}()

	// Give the goroutine time to block
	time.Sleep(10 * time.Millisecond)

	// Now out: a value — should wake the waiter
	ts.mu.Lock()
	for i, w := range ts.waiters {
		if vm.matchTuple(w.template, FromSmallInt(99)) {
			ts.waiters = append(ts.waiters[:i], ts.waiters[i+1:]...)
			ts.mu.Unlock()
			w.ch <- FromSmallInt(99)
			goto done
		}
	}
	ts.tuples = append(ts.tuples, FromSmallInt(99))
	ts.mu.Unlock()
done:
	wg.Wait()

	if result != FromSmallInt(99) {
		t.Errorf("expected 99 from blocked in:, got %v", result)
	}
}

// ---------------------------------------------------------------------------
// Object with ivars template matching
// ---------------------------------------------------------------------------

func TestTupleSpaceStructTemplate(t *testing.T) {
	vm := NewVM()

	pointClass := vm.createClass("Point", vm.ObjectClass)
	pointClass.InstVars = []string{"x", "y"}
	pointClass.NumSlots = 2

	obj := NewObject(pointClass.VTable, 2)
	obj.SetSlot(0, FromSmallInt(3))
	obj.SetSlot(1, FromSmallInt(7))

	// Should match {x: int, y: int}
	tmpl := compileCueTemplate("{x: int, y: int}")
	if !vm.matchTuple(tmpl, obj.ToValue()) {
		t.Error("struct template should match Point(3, 7)")
	}

	// Should NOT match {x: string}
	tmpl2 := compileCueTemplate("{x: string}")
	if vm.matchTuple(tmpl2, obj.ToValue()) {
		t.Error("string template should not match Point(3, 7)")
	}

	// Range template on field
	tmpl3 := compileCueTemplate("{x: >0, y: >5}")
	if !vm.matchTuple(tmpl3, obj.ToValue()) {
		t.Error("{x: >0, y: >5} should match Point(3, 7)")
	}

	tmpl4 := compileCueTemplate("{x: >0, y: >10}")
	if vm.matchTuple(tmpl4, obj.ToValue()) {
		t.Error("{x: >0, y: >10} should not match Point(3, 7)")
	}
}

// ---------------------------------------------------------------------------
// Primitives integration test (via VM)
// ---------------------------------------------------------------------------

func TestTupleSpacePrimitivesIntegration(t *testing.T) {
	vm := NewVM()

	// Register and create a TupleSpace
	ts := &TupleSpaceObject{}
	tsVal := vm.vmRegisterTupleSpace(ts)

	if !isTupleSpaceValue(tsVal) {
		t.Fatal("should be a tuple space value")
	}

	got := vm.vmGetTupleSpace(tsVal)
	if got != ts {
		t.Fatal("should retrieve the same tuple space")
	}
}

func TestTupleSpaceMarkerDistinct(t *testing.T) {
	if tupleSpaceMarker == channelMarker {
		t.Error("tupleSpace marker should not collide with channel")
	}
	if tupleSpaceMarker == cueValueMarker {
		t.Error("tupleSpace marker should not collide with cueValue")
	}
	if tupleSpaceMarker == cueContextMarker {
		t.Error("tupleSpace marker should not collide with cueContext")
	}
}
