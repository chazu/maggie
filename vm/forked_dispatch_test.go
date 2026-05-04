package vm

import (
	"sync"
	"testing"
)

// TestForkedConcurrentDispatch stress-tests the inline cache and the
// per-goroutine interpreter machinery by dispatching the same method
// from many goroutines simultaneously. Run with `go test -race` to
// catch the regressions documented in
// docs/vm-concurrency-audit-2026-05-03.md (concurrent map writes on
// InlineCacheTable, torn reads of InlineCache.State/Count/Entries,
// downstream bad-pointer faults in vtable dispatch).
//
// The test must pass cleanly under -race for the goid speedup
// (replacing runtime.Stack-based getGoroutineID with goid.Get) to be
// re-landable safely.
func TestForkedConcurrentDispatch(t *testing.T) {
	vm := NewVM()

	// A class with two methods:
	//   id     — return self, exercised inline-cached.
	//   callIt — pushes self and sends id to it.
	cls := vm.createClass("BenchTarget", vm.ObjectClass)

	idSel := vm.Selectors.Intern("id")
	idMethod := &CompiledMethod{
		name:     "id",
		Bytecode: []byte{byte(OpReturnSelf)},
	}
	idMethod.SetSelector(idSel)
	idMethod.SetClass(cls)
	cls.VTable.AddMethod(idSel, idMethod)

	callerSel := vm.Selectors.Intern("callIt")
	caller := &CompiledMethod{
		name:     "callIt",
		Arity:    0,
		NumTemps: 0,
		Bytecode: []byte{
			byte(OpPushSelf),
			byte(OpSend), byte(idSel), byte(idSel >> 8), 0,
			byte(OpReturnTop),
		},
	}
	caller.SetSelector(callerSel)
	caller.SetClass(cls)
	cls.VTable.AddMethod(callerSel, caller)

	inst := cls.NewInstance().ToValue()

	const G, N = 32, 500
	var wg sync.WaitGroup
	wg.Add(G)
	start := make(chan struct{})

	for g := 0; g < G; g++ {
		go func() {
			defer wg.Done()
			interp := vm.newForkedInterpreter(nil)
			vm.registerInterpreter(interp)
			defer vm.unregisterInterpreter()
			<-start
			for i := 0; i < N; i++ {
				_ = interp.Execute(caller, inst, nil)
			}
		}()
	}
	close(start)
	wg.Wait()

	// Sanity: the caller's IC should now be populated for BenchTarget.
	tbl := caller.GetInlineCaches()
	if tbl == nil {
		t.Fatal("expected inline cache table to be built")
	}
	// PC of OpSend in caller is 1 (after OpPushSelf).
	ic := tbl.Get(1)
	if ic == nil {
		t.Fatal("expected inline cache entry at PC 1 (OpSend)")
	}
	if ic.State() != CacheMonomorphic {
		t.Errorf("expected monomorphic IC after stress, got %v", ic.State())
	}
}
