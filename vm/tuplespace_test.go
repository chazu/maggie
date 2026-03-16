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
	ts.tuples = append(ts.tuples, TupleEntry{value: FromSmallInt(42), mode: TupleModeLinear})
	if len(ts.tuples) != 1 {
		t.Fatal("expected 1 tuple")
	}

	// Manual tryIn logic
	for i, entry := range ts.tuples {
		if vm.matchTuple(tmpl, entry.value) {
			ts.tuples = append(ts.tuples[:i], ts.tuples[i+1:]...)
			if entry.value != FromSmallInt(42) {
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
	ts.tuples = append(ts.tuples, TupleEntry{value: FromSmallInt(42), mode: TupleModeLinear})

	tmpl := compileCueTemplate("int")

	// tryRead should find it but not remove
	found := false
	for _, entry := range ts.tuples {
		if vm.matchTuple(tmpl, entry.value) {
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
	ts.tuples = append(ts.tuples, TupleEntry{value: vm.registry.NewStringValue("hello"), mode: TupleModeLinear})
	ts.tuples = append(ts.tuples, TupleEntry{value: FromSmallInt(42), mode: TupleModeLinear})
	ts.tuples = append(ts.tuples, TupleEntry{value: vm.registry.NewStringValue("world"), mode: TupleModeLinear})

	// int template should match 42, not the strings
	tmpl := compileCueTemplate("int")
	for i, entry := range ts.tuples {
		if vm.matchTuple(tmpl, entry.value) {
			if entry.value != FromSmallInt(42) {
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
		for i, entry := range ts.tuples {
			if vm.matchTuple(tmpl, entry.value) {
				result = entry.value
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
	ts.tuples = append(ts.tuples, TupleEntry{value: FromSmallInt(99), mode: TupleModeLinear})
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

// ---------------------------------------------------------------------------
// Phase 3: Tuple Modes
// ---------------------------------------------------------------------------

func TestTupleSpacePersistentNotConsumedByIn(t *testing.T) {
	vm := NewVM()
	ts := &TupleSpaceObject{}

	// Add a persistent tuple
	ts.tuples = append(ts.tuples, TupleEntry{
		value: FromSmallInt(42),
		mode:  TupleModePersistent,
	})

	tmpl := compileCueTemplate("int")

	// "in:" on persistent tuple: should return value but NOT remove
	ts.mu.Lock()
	for _, entry := range ts.tuples {
		if vm.matchTuple(tmpl, entry.value) {
			if entry.mode == TupleModePersistent {
				// Return value, don't remove
				ts.mu.Unlock()
				if entry.value != FromSmallInt(42) {
					t.Error("expected 42")
				}
				goto checkSize
			}
		}
	}
	ts.mu.Unlock()
	t.Fatal("should have found the persistent tuple")

checkSize:
	if len(ts.tuples) != 1 {
		t.Errorf("persistent tuple should remain in space, got %d tuples", len(ts.tuples))
	}
}

func TestTupleSpacePersistentMultipleReads(t *testing.T) {
	vm := NewVM()
	ts := &TupleSpaceObject{}

	ts.tuples = append(ts.tuples, TupleEntry{
		value: FromSmallInt(42),
		mode:  TupleModePersistent,
	})

	tmpl := compileCueTemplate("int")

	// Multiple in: calls should all return the same value
	for attempt := 0; attempt < 5; attempt++ {
		ts.mu.Lock()
		found := false
		for _, entry := range ts.tuples {
			if vm.matchTuple(tmpl, entry.value) {
				if entry.value != FromSmallInt(42) {
					t.Errorf("attempt %d: expected 42", attempt)
				}
				found = true
				break
			}
		}
		ts.mu.Unlock()
		if !found {
			t.Errorf("attempt %d: persistent tuple should still be there", attempt)
		}
	}

	if len(ts.tuples) != 1 {
		t.Errorf("persistent tuple should remain after 5 reads, got %d tuples", len(ts.tuples))
	}
}

func TestTupleSpaceAffineWithTTL(t *testing.T) {
	vm := NewVM()
	ts := &TupleSpaceObject{}

	// Add an affine tuple with 50ms TTL
	deadline := time.Now().UnixMilli() + 50
	ts.tuples = append(ts.tuples, TupleEntry{
		value:    FromSmallInt(99),
		mode:     TupleModeAffine,
		deadline: deadline,
	})

	tmpl := compileCueTemplate("int")

	// Before expiry: should be available
	ts.mu.Lock()
	found := false
	for _, entry := range ts.tuples {
		if !entry.isExpired() && vm.matchTuple(tmpl, entry.value) {
			found = true
			break
		}
	}
	ts.mu.Unlock()
	if !found {
		t.Error("affine tuple should be available before expiry")
	}

	// Wait for expiry
	time.Sleep(60 * time.Millisecond)

	// After expiry: should be gone after sweep
	ts.mu.Lock()
	ts.sweepExpired()
	remaining := len(ts.tuples)
	ts.mu.Unlock()

	if remaining != 0 {
		t.Errorf("expired affine tuple should be swept, got %d tuples", remaining)
	}
}

func TestTupleSpaceExpiredAffineSkippedDuringMatch(t *testing.T) {
	vm := NewVM()
	ts := &TupleSpaceObject{}

	// Add an already-expired affine tuple
	ts.tuples = append(ts.tuples, TupleEntry{
		value:    FromSmallInt(42),
		mode:     TupleModeAffine,
		deadline: time.Now().UnixMilli() - 100, // expired 100ms ago
	})

	// Add a valid linear tuple
	ts.tuples = append(ts.tuples, TupleEntry{
		value: FromSmallInt(99),
		mode:  TupleModeLinear,
	})

	tmpl := compileCueTemplate("int")

	// Sweep + match should skip the expired one and find 99
	ts.mu.Lock()
	ts.sweepExpired()
	var matched Value
	for i, entry := range ts.tuples {
		if vm.matchTuple(tmpl, entry.value) {
			matched = entry.value
			ts.tuples = append(ts.tuples[:i], ts.tuples[i+1:]...)
			break
		}
	}
	ts.mu.Unlock()

	if matched != FromSmallInt(99) {
		t.Errorf("should have matched 99 (skipping expired), got %v", matched)
	}
}

func TestTupleSpaceTupleEntryIsExpired(t *testing.T) {
	// Linear tuple never expires
	e1 := TupleEntry{value: FromSmallInt(1), mode: TupleModeLinear}
	if e1.isExpired() {
		t.Error("linear tuple should never be expired")
	}

	// Persistent tuple never expires
	e2 := TupleEntry{value: FromSmallInt(1), mode: TupleModePersistent}
	if e2.isExpired() {
		t.Error("persistent tuple should never be expired")
	}

	// Affine with no deadline never expires
	e3 := TupleEntry{value: FromSmallInt(1), mode: TupleModeAffine, deadline: 0}
	if e3.isExpired() {
		t.Error("affine tuple with no deadline should not be expired")
	}

	// Affine with future deadline not expired
	e4 := TupleEntry{value: FromSmallInt(1), mode: TupleModeAffine, deadline: time.Now().UnixMilli() + 10000}
	if e4.isExpired() {
		t.Error("affine tuple with future deadline should not be expired")
	}

	// Affine with past deadline is expired
	e5 := TupleEntry{value: FromSmallInt(1), mode: TupleModeAffine, deadline: time.Now().UnixMilli() - 100}
	if !e5.isExpired() {
		t.Error("affine tuple with past deadline should be expired")
	}
}

// ---------------------------------------------------------------------------
// Phase 3b: Atomic Multi-Take (inAll:)
// ---------------------------------------------------------------------------

func TestTupleSpaceInAllAtomicTake(t *testing.T) {
	vm := NewVM()
	ts := &TupleSpaceObject{}

	// Store an int and a string tuple
	ts.tuples = append(ts.tuples, TupleEntry{value: FromSmallInt(42), mode: TupleModeLinear})
	ts.tuples = append(ts.tuples, TupleEntry{value: vm.registry.NewStringValue("hello"), mode: TupleModeLinear})

	intTmpl := compileCueTemplate("int")
	strTmpl := compileCueTemplate("string")

	templates := []*CueValueObject{intTmpl, strTmpl}

	results, indices, ok := vm.tryMatchAll(ts, templates)
	if !ok {
		t.Fatal("should match both templates")
	}
	if len(results) != 2 {
		t.Fatalf("expected 2 results, got %d", len(results))
	}
	if results[0] != FromSmallInt(42) {
		t.Error("first result should be 42")
	}
	if len(indices) != 2 {
		t.Fatalf("expected 2 indices, got %d", len(indices))
	}

	// Remove matched tuples
	ts.removeIndices(indices)
	if len(ts.tuples) != 0 {
		t.Errorf("all tuples should be consumed, got %d", len(ts.tuples))
	}
}

func TestTupleSpaceInAllBlocksWhenNotAllSatisfiable(t *testing.T) {
	vm := NewVM()
	ts := &TupleSpaceObject{}

	// Only an int, no string
	ts.tuples = append(ts.tuples, TupleEntry{value: FromSmallInt(42), mode: TupleModeLinear})

	intTmpl := compileCueTemplate("int")
	strTmpl := compileCueTemplate("string")

	templates := []*CueValueObject{intTmpl, strTmpl}

	_, _, ok := vm.tryMatchAll(ts, templates)
	if ok {
		t.Error("should NOT match when string tuple is missing")
	}

	// Verify no tuples were consumed (atomic: all or nothing)
	if len(ts.tuples) != 1 {
		t.Errorf("no tuples should be consumed on failed match, got %d", len(ts.tuples))
	}
}

func TestTupleSpaceInAllDistinctMatches(t *testing.T) {
	vm := NewVM()
	ts := &TupleSpaceObject{}

	// Two int tuples — inAll with two int templates should match DIFFERENT tuples
	ts.tuples = append(ts.tuples, TupleEntry{value: FromSmallInt(1), mode: TupleModeLinear})
	ts.tuples = append(ts.tuples, TupleEntry{value: FromSmallInt(2), mode: TupleModeLinear})

	intTmpl1 := compileCueTemplate("int")
	intTmpl2 := compileCueTemplate("int")

	templates := []*CueValueObject{intTmpl1, intTmpl2}

	results, indices, ok := vm.tryMatchAll(ts, templates)
	if !ok {
		t.Fatal("should match both int templates against two int tuples")
	}
	if results[0] == results[1] {
		t.Error("should match DIFFERENT tuples for each template")
	}
	if indices[0] == indices[1] {
		t.Error("should have different indices")
	}
}

func TestTupleSpaceInAllNotEnoughDistinct(t *testing.T) {
	vm := NewVM()
	ts := &TupleSpaceObject{}

	// Only one int tuple — two int templates should fail
	ts.tuples = append(ts.tuples, TupleEntry{value: FromSmallInt(1), mode: TupleModeLinear})

	intTmpl1 := compileCueTemplate("int")
	intTmpl2 := compileCueTemplate("int")

	templates := []*CueValueObject{intTmpl1, intTmpl2}

	_, _, ok := vm.tryMatchAll(ts, templates)
	if ok {
		t.Error("should NOT match two int templates with only one int tuple")
	}
}

func TestTupleSpaceInAllCompoundWaiterWakes(t *testing.T) {
	vm := NewVM()
	ts := &TupleSpaceObject{}

	intTmpl := compileCueTemplate("int")
	strTmpl := compileCueTemplate("string")

	var wg sync.WaitGroup
	var results []Value

	wg.Add(1)
	go func() {
		defer wg.Done()
		ts.mu.Lock()
		_, _, ok := vm.tryMatchAll(ts, []*CueValueObject{intTmpl, strTmpl})
		if ok {
			t.Error("should not match yet")
			ts.mu.Unlock()
			return
		}
		// Register compound waiter
		w := &tupleWaiter{
			templates: []*CueValueObject{intTmpl, strTmpl},
			chArray:   make(chan []Value, 1),
			consume:   true,
		}
		ts.waiters = append(ts.waiters, w)
		ts.mu.Unlock()
		results = <-w.chArray
	}()

	time.Sleep(10 * time.Millisecond)

	// Add int tuple first
	ts.mu.Lock()
	ts.tuples = append(ts.tuples, TupleEntry{value: FromSmallInt(42), mode: TupleModeLinear})
	ts.mu.Unlock()

	time.Sleep(5 * time.Millisecond)

	// Add string tuple — now both should be satisfiable
	ts.mu.Lock()
	ts.tuples = append(ts.tuples, TupleEntry{value: vm.registry.NewStringValue("hello"), mode: TupleModeLinear})

	// Check compound waiters
	for i, w := range ts.waiters {
		if w.templates != nil && w.chArray != nil {
			matched, indices, ok := vm.tryMatchAll(ts, w.templates)
			if ok {
				ts.waiters = append(ts.waiters[:i], ts.waiters[i+1:]...)
				ts.removeIndices(indices)
				ts.mu.Unlock()
				w.chArray <- matched
				goto waitDone
			}
		}
	}
	ts.mu.Unlock()

waitDone:
	wg.Wait()

	if len(results) != 2 {
		t.Fatalf("expected 2 results from compound waiter, got %d", len(results))
	}
}

// ---------------------------------------------------------------------------
// Phase 3c: Choice (inAny:)
// ---------------------------------------------------------------------------

func TestTupleSpaceInAnyReturnsFirstMatch(t *testing.T) {
	vm := NewVM()
	ts := &TupleSpaceObject{}

	// Only a string tuple
	ts.tuples = append(ts.tuples, TupleEntry{value: vm.registry.NewStringValue("hello"), mode: TupleModeLinear})

	intTmpl := compileCueTemplate("int")
	strTmpl := compileCueTemplate("string")

	// inAny with int and string templates — should match the string
	ts.mu.Lock()
	matched := Nil
	for _, tmpl := range []*CueValueObject{intTmpl, strTmpl} {
		for i, entry := range ts.tuples {
			if vm.matchTuple(tmpl, entry.value) {
				matched = entry.value
				ts.tuples = append(ts.tuples[:i], ts.tuples[i+1:]...)
				goto found
			}
		}
	}
found:
	ts.mu.Unlock()

	if matched == Nil {
		t.Fatal("should have matched the string tuple")
	}
	if len(ts.tuples) != 0 {
		t.Error("matched tuple should be consumed")
	}
}

func TestTupleSpaceInAnyBlocksThenWakes(t *testing.T) {
	vm := NewVM()
	ts := &TupleSpaceObject{}

	intTmpl := compileCueTemplate("int")
	strTmpl := compileCueTemplate("string")

	var wg sync.WaitGroup
	var result Value

	wg.Add(1)
	go func() {
		defer wg.Done()
		ts.mu.Lock()
		// Check for any match
		for _, tmpl := range []*CueValueObject{intTmpl, strTmpl} {
			for _, entry := range ts.tuples {
				if vm.matchTuple(tmpl, entry.value) {
					// should not happen — space is empty
					ts.mu.Unlock()
					return
				}
			}
		}
		// No match — register choice waiter
		w := &tupleWaiter{
			templates: []*CueValueObject{intTmpl, strTmpl},
			ch:        make(chan Value, 1),
			consume:   true,
		}
		ts.waiters = append(ts.waiters, w)
		ts.mu.Unlock()
		result = <-w.ch
	}()

	time.Sleep(10 * time.Millisecond)

	// Add an int tuple — should wake the choice waiter
	ts.mu.Lock()
	tupleVal := FromSmallInt(77)
	ts.tuples = append(ts.tuples, TupleEntry{value: tupleVal, mode: TupleModeLinear})

	// Check choice waiters
	for i, w := range ts.waiters {
		if w.templates != nil && w.ch != nil {
			for _, tmpl := range w.templates {
				for j, entry := range ts.tuples {
					if vm.matchTuple(tmpl, entry.value) {
						ts.waiters = append(ts.waiters[:i], ts.waiters[i+1:]...)
						matched := entry.value
						ts.tuples = append(ts.tuples[:j], ts.tuples[j+1:]...)
						ts.mu.Unlock()
						w.ch <- matched
						goto done
					}
				}
			}
		}
	}
	ts.mu.Unlock()

done:
	wg.Wait()

	if result != FromSmallInt(77) {
		t.Errorf("expected 77 from choice waiter, got %v", result)
	}
}

// ---------------------------------------------------------------------------
// Phase 3d: Context-based expiry
// ---------------------------------------------------------------------------

func TestTupleSpaceContextExpiry(t *testing.T) {
	ts := &TupleSpaceObject{}

	tupleVal := FromSmallInt(42)
	ts.tuples = append(ts.tuples, TupleEntry{value: tupleVal, mode: TupleModeLinear})

	// Create a cancellable context
	ctx := createCancellationContextWithCancel(nil)

	// Start a goroutine that watches for cancellation
	go func() {
		<-ctx.Done()
		ts.mu.Lock()
		defer ts.mu.Unlock()
		for i, e := range ts.tuples {
			if e.value == tupleVal {
				ts.tuples = append(ts.tuples[:i], ts.tuples[i+1:]...)
				return
			}
		}
	}()

	// Tuple should be present before cancellation
	ts.mu.Lock()
	if len(ts.tuples) != 1 {
		t.Error("tuple should be present before cancellation")
	}
	ts.mu.Unlock()

	// Cancel the context
	ctx.Cancel()

	// Wait for goroutine to process
	time.Sleep(20 * time.Millisecond)

	ts.mu.Lock()
	remaining := len(ts.tuples)
	ts.mu.Unlock()

	if remaining != 0 {
		t.Errorf("tuple should be removed after context cancelled, got %d", remaining)
	}
}

func TestTupleSpacePersistentWithInAll(t *testing.T) {
	vm := NewVM()
	ts := &TupleSpaceObject{}

	// Persistent int tuple + linear string tuple
	ts.tuples = append(ts.tuples, TupleEntry{value: FromSmallInt(42), mode: TupleModePersistent})
	ts.tuples = append(ts.tuples, TupleEntry{value: vm.registry.NewStringValue("hello"), mode: TupleModeLinear})

	intTmpl := compileCueTemplate("int")
	strTmpl := compileCueTemplate("string")

	results, indices, ok := vm.tryMatchAll(ts, []*CueValueObject{intTmpl, strTmpl})
	if !ok {
		t.Fatal("should match both templates")
	}
	if len(results) != 2 {
		t.Fatalf("expected 2 results, got %d", len(results))
	}

	// Remove matched — persistent should stay
	ts.removeIndices(indices)
	if len(ts.tuples) != 1 {
		t.Errorf("persistent tuple should remain, got %d tuples", len(ts.tuples))
	}
	if ts.tuples[0].mode != TupleModePersistent {
		t.Error("remaining tuple should be the persistent one")
	}
}

func TestTupleSpaceSweepExpiredKeepsValid(t *testing.T) {
	ts := &TupleSpaceObject{}

	now := time.Now().UnixMilli()

	// Expired
	ts.tuples = append(ts.tuples, TupleEntry{
		value:    FromSmallInt(1),
		mode:     TupleModeAffine,
		deadline: now - 100,
	})
	// Valid affine (far future)
	ts.tuples = append(ts.tuples, TupleEntry{
		value:    FromSmallInt(2),
		mode:     TupleModeAffine,
		deadline: now + 100000,
	})
	// Linear (no deadline)
	ts.tuples = append(ts.tuples, TupleEntry{
		value: FromSmallInt(3),
		mode:  TupleModeLinear,
	})
	// Persistent
	ts.tuples = append(ts.tuples, TupleEntry{
		value: FromSmallInt(4),
		mode:  TupleModePersistent,
	})

	ts.sweepExpired()

	if len(ts.tuples) != 3 {
		t.Errorf("expected 3 tuples after sweep (1 expired removed), got %d", len(ts.tuples))
	}

	// Verify the remaining values
	values := make(map[int64]bool)
	for _, e := range ts.tuples {
		values[e.value.SmallInt()] = true
	}
	if values[1] {
		t.Error("expired tuple (1) should have been removed")
	}
	if !values[2] || !values[3] || !values[4] {
		t.Error("valid tuples should remain")
	}
}

func TestTupleSpaceInAllSkipsExpiredAffine(t *testing.T) {
	vm := NewVM()
	ts := &TupleSpaceObject{}

	now := time.Now().UnixMilli()

	// Expired int tuple
	ts.tuples = append(ts.tuples, TupleEntry{
		value:    FromSmallInt(1),
		mode:     TupleModeAffine,
		deadline: now - 100,
	})
	// Valid int tuple
	ts.tuples = append(ts.tuples, TupleEntry{
		value: FromSmallInt(2),
		mode:  TupleModeLinear,
	})
	// String tuple
	ts.tuples = append(ts.tuples, TupleEntry{
		value: vm.registry.NewStringValue("hello"),
		mode:  TupleModeLinear,
	})

	intTmpl := compileCueTemplate("int")
	strTmpl := compileCueTemplate("string")

	results, _, ok := vm.tryMatchAll(ts, []*CueValueObject{intTmpl, strTmpl})
	if !ok {
		t.Fatal("should match (skipping expired, using valid int)")
	}
	if results[0] != FromSmallInt(2) {
		t.Errorf("should have matched the valid int tuple (2), got %v", results[0])
	}
}
