package vm

import (
	"fmt"
	"runtime"
	"sync"
	"sync/atomic"
	"testing"
	"time"
)

// TestGCSafepoint_StopTheWorld runs several "mutator" goroutines that park at
// their safepoint while a collector repeatedly stops the world. Validates the
// barrier completes (no deadlock) and is race-free (run with -race).
func TestGCSafepoint_StopTheWorld(t *testing.T) {
	vm := NewVM()
	vm.EnableStringGC()

	var stop atomic.Bool
	var wg sync.WaitGroup
	const mutators = 4
	for k := 0; k < mutators; k++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			interp := vm.newInterpreter()
			vm.registerInterpreter(interp)
			defer vm.unregisterInterpreter()
			for !stop.Load() {
				// Simulate the dispatch-loop safepoint.
				if vm.gcRequested.Load() {
					vm.parkAtSafepoint(interp)
				}
				// Allocate a string each iteration; some kept, some not.
				_ = vm.registry.NewStringValue("churn")
				runtime.Gosched()
			}
		}()
	}

	ranAtLeastOnce := false
	for c := 0; c < 30; c++ {
		if _, _, ran := vm.CollectStringGarbage(); ran {
			ranAtLeastOnce = true
		}
	}
	stop.Store(true)
	wg.Wait()

	if !ranAtLeastOnce {
		t.Fatal("collector never reached stop-the-world across 30 attempts")
	}
}

// TestGCSafepoint_AbortOnStuckMutator verifies the collector aborts (rather
// than hanging) when a mutator never reaches a safepoint.
func TestGCSafepoint_AbortOnStuckMutator(t *testing.T) {
	vm := NewVM()
	vm.EnableStringGC()

	// A registered interpreter that never parks (gcState stays running),
	// simulating a goroutine stuck in an un-instrumented blocking primitive.
	stuck := vm.newInterpreter()
	stuck.gcState.Store(gcStateRunning)
	vm.interpreters.Store(int64(999001), stuck)
	atomic.AddInt32(&vm.interpreterCount, 1)
	defer func() {
		vm.interpreters.Delete(int64(999001))
		atomic.AddInt32(&vm.interpreterCount, -1)
	}()

	start := time.Now()
	_, _, ran := vm.CollectStringGarbage()
	elapsed := time.Since(start)

	if ran {
		t.Fatal("expected collection to abort with a stuck mutator")
	}
	if elapsed < gcBarrierTimeout {
		t.Fatalf("aborted before timeout: %v < %v", elapsed, gcBarrierTimeout)
	}
	if elapsed > gcBarrierTimeout*4 {
		t.Fatalf("abort took far too long: %v", elapsed)
	}
	// gcRequested must be cleared after an abort so the VM keeps running.
	if vm.gcRequested.Load() {
		t.Fatal("gcRequested still set after abort")
	}
}

// TestGCSafepoint_BlockedMutatorIsStopped verifies an interpreter in
// enterBlocked counts as stopped (collection proceeds) and its published roots
// survive the sweep.
func TestGCSafepoint_BlockedMutatorIsStopped(t *testing.T) {
	vm := NewVM()
	vm.EnableStringGC()

	ready := make(chan Value)
	release := make(chan struct{})
	go func() {
		interp := vm.newInterpreter()
		vm.registerInterpreter(interp)
		defer vm.unregisterInterpreter()
		// A string held only as a blocked-root (not on any stack/global).
		s := vm.registry.NewStringValue("\x00blocked-root\x00")
		vm.enterBlocked(s)
		ready <- s
		<-release
		vm.exitBlocked()
	}()

	s := <-ready
	_, _, ran := vm.CollectStringGarbage()
	if !ran {
		t.Fatal("collection should run when the only mutator is blocked")
	}
	if got := vm.registry.GetStringContent(s); got != "\x00blocked-root\x00" {
		t.Fatalf("blocked-root string freed/corrupted: %q", got)
	}
	close(release)
}

// TestGCSafepoint_DisabledIsNoop verifies the safepoint is inert when the
// collector is disabled.
func TestGCSafepoint_DisabledIsNoop(t *testing.T) {
	vm := NewVM()
	vm.gcEnabled.Store(false) // force-disable regardless of MAGGIE_GC env
	if vm.StringGCEnabled() {
		t.Fatal("string GC should be disabled after force-disable")
	}
	_, _, ran := vm.CollectStringGarbage()
	if ran {
		t.Fatal("disabled collector must not run")
	}
}

// TestGCSafepoint_ConcurrentChurnNoCorruption stress-tests kept strings staying
// intact while many goroutines churn allocations and the collector runs.
func TestGCSafepoint_ConcurrentChurnNoCorruption(t *testing.T) {
	vm := NewVM()
	vm.EnableStringGC()

	// Kept strings, reachable via globals.
	const keep = 64
	kept := make([]Value, keep)
	for i := range kept {
		kept[i] = vm.registry.NewStringValue(fmt.Sprintf("\x00keep\x00-%d", i))
		vm.SetGlobal(fmt.Sprintf("Keep%d", i), kept[i])
	}

	var stop atomic.Bool
	var wg sync.WaitGroup
	for k := 0; k < 4; k++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			interp := vm.newInterpreter()
			vm.registerInterpreter(interp)
			defer vm.unregisterInterpreter()
			for !stop.Load() {
				if vm.gcRequested.Load() {
					vm.parkAtSafepoint(interp)
				}
				_ = vm.registry.NewStringValue("ephemeral-churn-string")
				runtime.Gosched()
			}
		}()
	}

	for c := 0; c < 25; c++ {
		vm.CollectStringGarbage()
	}
	stop.Store(true)
	wg.Wait()

	for i := range kept {
		want := fmt.Sprintf("\x00keep\x00-%d", i)
		if got := vm.registry.GetStringContent(kept[i]); got != want {
			t.Fatalf("kept string %d corrupted: got %q want %q", i, got, want)
		}
	}
}
