package vm

import (
	"sync"
	"testing"
	"time"

	"cuelang.org/go/cue"
	"cuelang.org/go/cue/cuecontext"
)

func newTestConstraintStore() *ConstraintStoreObject {
	ctx := cuecontext.New()
	return &ConstraintStoreObject{
		ctx:   ctx,
		store: ctx.CompileString("_"),
	}
}

func compileCueValue(src string) cue.Value {
	ctx := cuecontext.New()
	return ctx.CompileString(src)
}

// ---------------------------------------------------------------------------
// Test 1: Tell then tryAsk
// ---------------------------------------------------------------------------

func TestConstraintStoreTellThenTryAsk(t *testing.T) {
	cs := newTestConstraintStore()

	// Tell {x: 42}
	constraint := compileCueValue("{x: 42}")
	if err := cs.tell(constraint); err != nil {
		t.Fatalf("tell should succeed: %v", err)
	}

	// tryAsk {x: int} should be entailed
	query := compileCueValue("{x: int}")
	if !cs.tryAsk(query) {
		t.Error("store should entail {x: int} after telling {x: 42}")
	}
}

// ---------------------------------------------------------------------------
// Test 2: TryAsk not entailed
// ---------------------------------------------------------------------------

func TestConstraintStoreTryAskNotEntailed(t *testing.T) {
	cs := newTestConstraintStore()

	query := compileCueValue("{x: int}")
	if cs.tryAsk(query) {
		t.Error("empty store should not entail {x: int}")
	}
}

// ---------------------------------------------------------------------------
// Test 3: Multiple tells accumulate
// ---------------------------------------------------------------------------

func TestConstraintStoreMultipleTells(t *testing.T) {
	cs := newTestConstraintStore()

	if err := cs.tell(compileCueValue("{x: 42}")); err != nil {
		t.Fatalf("tell 1 failed: %v", err)
	}
	if err := cs.tell(compileCueValue(`{y: "hello"}`)); err != nil {
		t.Fatalf("tell 2 failed: %v", err)
	}

	query := compileCueValue("{x: int, y: string}")
	if !cs.tryAsk(query) {
		t.Error("store should entail {x: int, y: string} after two tells")
	}
}

// ---------------------------------------------------------------------------
// Test 4: Conflicting tell
// ---------------------------------------------------------------------------

func TestConstraintStoreConflictingTell(t *testing.T) {
	cs := newTestConstraintStore()

	if err := cs.tell(compileCueValue("{x: 42}")); err != nil {
		t.Fatalf("first tell should succeed: %v", err)
	}

	err := cs.tell(compileCueValue(`{x: "hello"}`))
	if err == nil {
		t.Error("conflicting tell should return error")
	}
}

// ---------------------------------------------------------------------------
// Test 5: Store remains consistent after failed tell
// ---------------------------------------------------------------------------

func TestConstraintStoreConsistentAfterFailedTell(t *testing.T) {
	cs := newTestConstraintStore()

	if err := cs.tell(compileCueValue("{x: 42}")); err != nil {
		t.Fatalf("first tell should succeed: %v", err)
	}

	// Conflicting tell — should fail and not change store
	_ = cs.tell(compileCueValue(`{x: "hello"}`))

	// Original constraint should still hold
	query := compileCueValue("{x: int}")
	if !cs.tryAsk(query) {
		t.Error("original constraint should still hold after failed tell")
	}

	if !cs.isConsistent() {
		t.Error("store should remain consistent after failed tell")
	}
}

// ---------------------------------------------------------------------------
// Test 6: Blocking ask
// ---------------------------------------------------------------------------

func TestConstraintStoreBlockingAsk(t *testing.T) {
	cs := newTestConstraintStore()

	var wg sync.WaitGroup
	wg.Add(1)

	done := make(chan struct{})
	go func() {
		defer wg.Done()
		query := compileCueValue("{x: int}")
		cs.ask(query)
		close(done)
	}()

	// Give the goroutine time to block
	time.Sleep(50 * time.Millisecond)

	select {
	case <-done:
		t.Fatal("ask should be blocked before tell")
	default:
		// Good, still blocked
	}

	// Now tell the constraint
	if err := cs.tell(compileCueValue("{x: 42}")); err != nil {
		t.Fatalf("tell failed: %v", err)
	}

	// Wait for ask to unblock
	select {
	case <-done:
		// Good, unblocked
	case <-time.After(2 * time.Second):
		t.Fatal("ask should have unblocked after tell")
	}

	wg.Wait()
}

// ---------------------------------------------------------------------------
// Test 7: Multiple watchers
// ---------------------------------------------------------------------------

func TestConstraintStoreMultipleWatchers(t *testing.T) {
	cs := newTestConstraintStore()

	done1 := make(chan struct{})
	done2 := make(chan struct{})

	go func() {
		cs.ask(compileCueValue("{x: int}"))
		close(done1)
	}()

	go func() {
		cs.ask(compileCueValue(`{y: string}`))
		close(done2)
	}()

	time.Sleep(50 * time.Millisecond)

	// Tell x — should wake watcher 1 only
	if err := cs.tell(compileCueValue("{x: 42}")); err != nil {
		t.Fatalf("tell x failed: %v", err)
	}

	select {
	case <-done1:
		// Good
	case <-time.After(2 * time.Second):
		t.Fatal("watcher 1 should have woken after telling x")
	}

	// Watcher 2 should still be blocked
	select {
	case <-done2:
		t.Fatal("watcher 2 should still be blocked (y not told yet)")
	default:
		// Good
	}

	// Tell y — should wake watcher 2
	if err := cs.tell(compileCueValue(`{y: "hello"}`)); err != nil {
		t.Fatalf("tell y failed: %v", err)
	}

	select {
	case <-done2:
		// Good
	case <-time.After(2 * time.Second):
		t.Fatal("watcher 2 should have woken after telling y")
	}
}

// ---------------------------------------------------------------------------
// Test 8: tryAsk false then true
// ---------------------------------------------------------------------------

func TestConstraintStoreTryAskFalseThenTrue(t *testing.T) {
	cs := newTestConstraintStore()

	query := compileCueValue("{x: int}")

	if cs.tryAsk(query) {
		t.Error("should be false before any tell")
	}

	if err := cs.tell(compileCueValue("{x: 42}")); err != nil {
		t.Fatalf("tell failed: %v", err)
	}

	if !cs.tryAsk(query) {
		t.Error("should be true after tell")
	}
}

// ---------------------------------------------------------------------------
// Test 9: isConsistent
// ---------------------------------------------------------------------------

func TestConstraintStoreIsConsistent(t *testing.T) {
	cs := newTestConstraintStore()

	if !cs.isConsistent() {
		t.Error("new store should be consistent")
	}

	if err := cs.tell(compileCueValue("{x: 42}")); err != nil {
		t.Fatalf("tell failed: %v", err)
	}

	if !cs.isConsistent() {
		t.Error("store with valid tell should be consistent")
	}
}

// ---------------------------------------------------------------------------
// Test 10: value returns current store as CueValue
// ---------------------------------------------------------------------------

func TestConstraintStoreValue(t *testing.T) {
	cs := newTestConstraintStore()

	if err := cs.tell(compileCueValue("{x: 42}")); err != nil {
		t.Fatalf("tell failed: %v", err)
	}

	cs.mu.Lock()
	storeVal := cs.store
	cs.mu.Unlock()

	// Look up x in the store
	result := storeVal.LookupPath(cue.ParsePath("x"))
	if !result.Exists() {
		t.Fatal("store should have field x")
	}

	n, err := result.Int64()
	if err != nil {
		t.Fatalf("x should be an int: %v", err)
	}
	if n != 42 {
		t.Errorf("expected x=42, got x=%d", n)
	}
}

// ---------------------------------------------------------------------------
// Test: NaN-boxing round-trip
// ---------------------------------------------------------------------------

func TestConstraintStoreNaNBoxing(t *testing.T) {
	vm := NewVM()

	ctx := cuecontext.New()
	cs := &ConstraintStoreObject{
		ctx:   ctx,
		store: ctx.CompileString("_"),
	}

	val := vm.vmRegisterConstraintStore(cs)

	if !isConstraintStoreValue(val) {
		t.Fatal("should be recognized as constraint store value")
	}

	retrieved := vm.vmGetConstraintStore(val)
	if retrieved != cs {
		t.Fatal("round-trip should return same object")
	}
}
