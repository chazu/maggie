package vm

import (
	"os"
	"sync"
	"sync/atomic"
	"testing"
	"time"
)

// ---------------------------------------------------------------------------
// RegistryGC Unit Tests
// ---------------------------------------------------------------------------

// TestRegistryGCSweepClosedChannels verifies that closed channel entries
// in the VM-local registry are cleaned up after a sweep.
func TestRegistryGCSweepClosedChannels(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	const numChannels = 10

	// Create channels via VM primitives
	channels := make([]Value, numChannels)
	for i := 0; i < numChannels; i++ {
		channels[i] = vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})
	}

	initialCount := vm.Concurrency().ChannelCount()
	if initialCount < numChannels {
		t.Fatalf("Expected at least %d channels, got %d", numChannels, initialCount)
	}

	// Close half the channels
	for i := 0; i < numChannels/2; i++ {
		vm.Send(channels[i], "close", nil)
	}

	// Perform a sweep
	stats := vm.registryGC.SweepNow()

	if stats.Channels != numChannels/2 {
		t.Errorf("Expected %d swept channels, got %d", numChannels/2, stats.Channels)
	}

	// Remaining channels should still be accessible
	afterCount := vm.Concurrency().ChannelCount()
	expectedRemaining := initialCount - numChannels/2
	if afterCount != expectedRemaining {
		t.Errorf("After sweep: %d channels remain, expected %d", afterCount, expectedRemaining)
	}

	// Open channels should still work
	for i := numChannels / 2; i < numChannels; i++ {
		vm.Send(channels[i], "send:", []Value{FromSmallInt(42)})
		val := vm.Send(channels[i], "receive", nil)
		if !val.IsSmallInt() || val.SmallInt() != 42 {
			t.Errorf("Channel %d: received %v, want 42", i, val)
		}
	}
}

// TestRegistryGCSweepTerminatedProcesses verifies that completed process
// entries in the VM-local registry are cleaned up after a sweep.
func TestRegistryGCSweepTerminatedProcesses(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	const numProcesses = 5

	interp := vm.newInterpreter()
	vm.registerInterpreter(interp)

	blockMethod := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 0,
		Bytecode:    []byte{byte(OpPushInt8), 42, byte(OpBlockReturn)},
		Literals:    nil,
	}

	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushNil)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()
	interp.pushFrame(m, Nil, nil)

	processes := make([]Value, numProcesses)
	started := 0
	for i := 0; i < numProcesses; i++ {
		blockVal := interp.createBlockValue(blockMethod, nil)
		processes[i] = vm.Send(blockVal, "fork", nil)
		if processes[i] != Nil {
			started++
		}
	}

	// Wait for all processes to finish
	for i := 0; i < numProcesses; i++ {
		if processes[i] != Nil {
			vm.Send(processes[i], "wait", nil)
		}
	}

	interp.popFrame()
	vm.unregisterInterpreter()

	if started == 0 {
		t.Skip("No processes started")
	}

	beforeCount := vm.Concurrency().ProcessCount()

	// Sweep
	stats := vm.registryGC.SweepNow()

	if stats.Processes < started {
		t.Errorf("Expected at least %d swept processes, got %d", started, stats.Processes)
	}

	afterCount := vm.Concurrency().ProcessCount()
	if afterCount >= beforeCount {
		t.Errorf("Process count should decrease after sweep: before=%d, after=%d", beforeCount, afterCount)
	}

	t.Logf("Swept %d terminated processes (before: %d, after: %d)", stats.Processes, beforeCount, afterCount)
}

// TestRegistryGCActiveChannelsNotSwept verifies that open channels
// are NOT removed during a sweep.
func TestRegistryGCActiveChannelsNotSwept(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	const numChannels = 5

	channels := make([]Value, numChannels)
	for i := 0; i < numChannels; i++ {
		channels[i] = vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})
	}

	countBefore := vm.Concurrency().ChannelCount()

	// Sweep without closing any channels
	stats := vm.registryGC.SweepNow()

	if stats.Channels != 0 {
		t.Errorf("No channels should be swept, but %d were", stats.Channels)
	}

	countAfter := vm.Concurrency().ChannelCount()
	if countAfter != countBefore {
		t.Errorf("Channel count changed: before=%d, after=%d", countBefore, countAfter)
	}

	// All channels should still be usable
	for i := 0; i < numChannels; i++ {
		vm.Send(channels[i], "send:", []Value{FromSmallInt(int64(i))})
		val := vm.Send(channels[i], "receive", nil)
		if !val.IsSmallInt() || val.SmallInt() != int64(i) {
			t.Errorf("Channel %d: received %v, want %d", i, val, i)
		}
	}
}

// TestRegistryGCActiveProcessesNotSwept verifies that running processes
// are NOT removed during a sweep.
func TestRegistryGCActiveProcessesNotSwept(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	// Create a process manually that we control (stays in Running state)
	proc, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(proc); err != nil {
		t.Fatal(err)
	}

	procCountBefore := vm.Concurrency().ProcessCount()

	// Sweep - the running process should NOT be swept
	stats := vm.registryGC.SweepNow()
	if stats.Processes != 0 {
		t.Errorf("Running process should not be swept, but %d were", stats.Processes)
	}

	procCountAfter := vm.Concurrency().ProcessCount()
	if procCountAfter != procCountBefore {
		t.Errorf("Process count changed: before=%d, after=%d", procCountBefore, procCountAfter)
	}

	// Clean up
	proc.markDone(Nil, nil)
}

// CancellationContexts are now pointer-carrying heap Values reclaimed by the Go
// GC; the custom collector no longer sweeps them, so the dedicated sweep test
// was removed. Functional cancel/isCancelled behavior is covered in
// cancellation_test.go.

// TestRegistryGCConfigurableInterval verifies that the sweep interval
// can be configured.
func TestRegistryGCConfigurableInterval(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	// Default interval
	if vm.registryGC.Interval() != DefaultGCInterval {
		t.Errorf("Default interval = %v, want %v", vm.registryGC.Interval(), DefaultGCInterval)
	}

	// Create with custom interval
	gc := NewRegistryGC(vm, 5*time.Second)
	if gc.Interval() != 5*time.Second {
		t.Errorf("Custom interval = %v, want 5s", gc.Interval())
	}

	// Zero interval should use default
	gc2 := NewRegistryGC(vm, 0)
	if gc2.Interval() != DefaultGCInterval {
		t.Errorf("Zero interval should use default, got %v", gc2.Interval())
	}

	// Negative interval should use default
	gc3 := NewRegistryGC(vm, -1*time.Second)
	if gc3.Interval() != DefaultGCInterval {
		t.Errorf("Negative interval should use default, got %v", gc3.Interval())
	}
}

// TestRegistryGCStartStop verifies start/stop lifecycle.
func TestRegistryGCStartStop(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	gc := NewRegistryGC(vm, 50*time.Millisecond)

	// Start
	gc.Start()

	// Wait for at least one sweep
	time.Sleep(120 * time.Millisecond)
	if gc.SweepCount() == 0 {
		t.Error("Expected at least one sweep after starting")
	}

	// Stop
	gc.Stop()
	countAtStop := gc.SweepCount()

	// Wait and verify no more sweeps happen
	time.Sleep(120 * time.Millisecond)
	if gc.SweepCount() != countAtStop {
		t.Errorf("Sweeps continued after Stop: was %d, now %d", countAtStop, gc.SweepCount())
	}
}

// TestRegistryGCDoubleStart verifies that calling Start twice is safe.
func TestRegistryGCDoubleStart(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	gc := NewRegistryGC(vm, 100*time.Millisecond)
	gc.Start()
	gc.Start() // should be no-op

	time.Sleep(50 * time.Millisecond)
	gc.Stop()
}

// TestRegistryGCDoubleStop verifies that calling Stop twice is safe.
func TestRegistryGCDoubleStop(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	gc := NewRegistryGC(vm, 100*time.Millisecond)
	gc.Start()
	gc.Stop()
	gc.Stop() // should be no-op
}

// TestRegistryGCStopWithoutStart verifies that Stop is safe without Start.
func TestRegistryGCStopWithoutStart(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	gc := NewRegistryGC(vm, 100*time.Millisecond)
	gc.Stop() // should be no-op, no panic
}

// TestRegistryGCEnableDisable verifies enable/disable functionality.
func TestRegistryGCEnableDisable(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	gc := NewRegistryGC(vm, 50*time.Millisecond)

	// Disable before start
	gc.SetEnabled(false)
	if gc.IsEnabled() {
		t.Error("GC should be disabled")
	}

	gc.Start()
	defer gc.Stop()

	// Wait for potential sweep cycles
	time.Sleep(120 * time.Millisecond)

	// No sweeps should have occurred while disabled
	if gc.SweepCount() != 0 {
		t.Errorf("Expected 0 sweeps while disabled, got %d", gc.SweepCount())
	}

	// Re-enable
	gc.SetEnabled(true)
	if !gc.IsEnabled() {
		t.Error("GC should be enabled")
	}

	// Wait for a sweep
	time.Sleep(120 * time.Millisecond)

	if gc.SweepCount() == 0 {
		t.Error("Expected at least one sweep after re-enabling")
	}
}

// TestRegistryGCSweepNow verifies that SweepNow performs an immediate sweep.
func TestRegistryGCSweepNow(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	// Create and close a channel
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})
	vm.Send(ch, "close", nil)

	stats := vm.registryGC.SweepNow()

	if stats == nil {
		t.Fatal("SweepNow returned nil stats")
	}

	if stats.TotalSwept < 1 {
		t.Errorf("Expected at least 1 object swept, got %d", stats.TotalSwept)
	}

	if stats.SweepDuration <= 0 {
		t.Error("SweepDuration should be positive")
	}

	if stats.Timestamp.IsZero() {
		t.Error("Timestamp should not be zero")
	}
}

// TestRegistryGCLastStats verifies that LastStats returns the most recent
// sweep statistics.
func TestRegistryGCLastStats(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	// Initially nil
	if vm.registryGC.LastStats() != nil {
		t.Error("LastStats should be nil before any sweep")
	}

	// After manual sweep
	vm.registryGC.SweepNow()
	stats := vm.registryGC.LastStats()
	if stats == nil {
		t.Fatal("LastStats should not be nil after sweep")
	}

	if stats.Timestamp.IsZero() {
		t.Error("LastStats timestamp should not be zero")
	}
}

// TestRegistryGCConcurrentAccessDuringSweep verifies that normal VM operations
// work correctly while a sweep is running.
func TestRegistryGCConcurrentAccessDuringSweep(t *testing.T) {
	// This stress test drives Maggie execution via vm.Send concurrently from
	// many goroutines that share the main interpreter. The string collector's
	// stop-the-world assumes one interpreter per mutator goroutine (Maggie's
	// real model: forked processes / a serialized dispatcher), so this
	// unsupported sharing pattern is incompatible with it. The collector is now
	// on by default, so skip unless it is explicitly disabled (MAGGIE_GC=0),
	// under which this test still exercises RegistryGC concurrency.
	switch os.Getenv("MAGGIE_GC") {
	case "0", "off", "false", "no":
		// collector disabled — run the test
	default:
		t.Skip("concurrent shared-interpreter vm.Send is incompatible with the string GC (default-on); run with MAGGIE_GC=0")
	}
	vm := NewVM()
	defer vm.Shutdown()

	// Use a fast sweep interval
	gc := NewRegistryGC(vm, 5*time.Millisecond)
	gc.Start()
	defer gc.Stop()

	const numGoroutines = 5
	const opsPerGoroutine = 50

	var wg sync.WaitGroup
	errors := int32(0)

	// Concurrently create, use, and close channels while GC runs
	for g := 0; g < numGoroutines; g++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for i := 0; i < opsPerGoroutine; i++ {
				// Create a channel
				ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})
				if ch == Nil {
					atomic.AddInt32(&errors, 1)
					continue
				}

				// Use it
				vm.Send(ch, "send:", []Value{FromSmallInt(int64(i))})
				val := vm.Send(ch, "receive", nil)
				if !val.IsSmallInt() || val.SmallInt() != int64(i) {
					atomic.AddInt32(&errors, 1)
				}

				// Small sleep to ensure GC has time to run
				time.Sleep(time.Millisecond)

				// Close it (will be swept in next cycle)
				vm.Send(ch, "close", nil)
			}
		}()
	}

	// Concurrently create and complete processes while GC runs
	for g := 0; g < numGoroutines; g++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for i := 0; i < opsPerGoroutine/5; i++ {
				proc, err := vm.createProcess()
				if err != nil {
					t.Fatal(err)
				}
				if _, err := vm.registerProcess(proc); err != nil {
					t.Fatal(err)
				}
				proc.markDone(FromSmallInt(int64(i)), nil)
				time.Sleep(time.Millisecond)
			}
		}()
	}

	// Concurrently create and cancel contexts while GC runs
	for g := 0; g < numGoroutines; g++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for i := 0; i < opsPerGoroutine/5; i++ {
				ctx := vm.Send(vm.classValue(vm.CancellationContextClass), "withCancel", nil)
				if ctx != Nil {
					vm.Send(ctx, "cancel", nil)
				}
				time.Sleep(time.Millisecond)
			}
		}()
	}

	wg.Wait()

	if errors > 0 {
		t.Errorf("%d errors during concurrent access", errors)
	}

	// Verify GC ran at least once - if operations were too fast, use SweepNow
	// as a fallback check that sweeping while operations occurred didn't crash
	if gc.SweepCount() == 0 {
		t.Log("GC ticker did not fire during operations (fast execution); verifying manual sweep works")
		gc.SweepNow()
	}

	t.Logf("GC ran %d times during concurrent operations", gc.SweepCount())
}

// TestRegistryGCTotalSwept verifies that TotalSwept is the sum of all
// individual sweep counts.
func TestRegistryGCTotalSwept(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	// Create and close a channel
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})
	vm.Send(ch, "close", nil)

	// Create and complete a process
	proc, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(proc); err != nil {
		t.Fatal(err)
	}
	proc.markDone(Nil, nil)

	stats := vm.registryGC.SweepNow()

	expectedTotal := stats.Channels + stats.Processes

	if stats.TotalSwept != expectedTotal {
		t.Errorf("TotalSwept=%d, expected sum of components=%d", stats.TotalSwept, expectedTotal)
	}

	if stats.TotalSwept < 2 {
		t.Errorf("Expected at least 2 objects swept, got %d", stats.TotalSwept)
	}
}

// TestVMShutdownStopsGC verifies that VM.Shutdown stops the registry GC.
func TestVMShutdownStopsGC(t *testing.T) {
	vm := NewVM()
	gc := vm.RegistryGC()

	if gc == nil {
		t.Fatal("RegistryGC should not be nil after NewVM")
	}

	vm.Shutdown()

	// After shutdown, sweep count should not increase
	countAtShutdown := gc.SweepCount()
	time.Sleep(100 * time.Millisecond)
	if gc.SweepCount() != countAtShutdown {
		t.Error("GC should not run after Shutdown")
	}
}

// TestRegistryGCPeriodicSweep verifies that the GC runs periodically
// at the configured interval.
func TestRegistryGCPeriodicSweep(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	// Stop the default GC
	vm.registryGC.Stop()

	// Create a new one with a short interval
	gc := NewRegistryGC(vm, 30*time.Millisecond)
	gc.Start()
	defer gc.Stop()

	// Wait for several sweep cycles
	time.Sleep(200 * time.Millisecond)

	count := gc.SweepCount()
	if count < 3 {
		t.Errorf("Expected at least 3 periodic sweeps in 200ms with 30ms interval, got %d", count)
	}

	t.Logf("Periodic sweep ran %d times in ~200ms (interval: 30ms)", count)
}

// TestRegistryGCNoDanglingReferences verifies that after sweeping,
// accessing a swept object does not crash the VM.
func TestRegistryGCNoDanglingReferences(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	// Create a channel, close it, sweep it
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})
	vm.Send(ch, "close", nil)
	vm.registryGC.SweepNow()

	// Attempting to use the swept channel should return Nil (not crash)
	result := vm.Send(ch, "isClosed", nil)
	// The channel was removed from registry, so getChannel returns nil,
	// and isClosed returns True for nil channels
	if result != True {
		t.Logf("isClosed on swept channel returned %v (expected True or graceful handling)", result)
	}
}

// ---------------------------------------------------------------------------
// Pressure-Aware Trigger Tests (recommendation 2c)
// ---------------------------------------------------------------------------

// waitForSweepCount polls until SweepCount reaches at least target or
// the deadline elapses. Returns the final count.
func waitForSweepCount(gc *RegistryGC, target uint64, deadline time.Duration) uint64 {
	end := time.Now().Add(deadline)
	for time.Now().Before(end) {
		if c := gc.SweepCount(); c >= target {
			return c
		}
		time.Sleep(2 * time.Millisecond)
	}
	return gc.SweepCount()
}

// (Removed the pressure-driven-sweep tests — TestRegistryGCGrowthTrigger,
// CeilingTrigger, CeilingNoBusyLoop, CoalescingUnderBurst, NoSpuriousTriggers,
// PostSweepBaselineReset — with the AutoIDRegistry pressure hooks. No
// registry-backed heap type remains: strings/dicts/objects/blocks/contexts are
// Go-GC-traced pointer Values. The wall-clock sweep of terminated channels/
// processes is still covered by TestRegistryGCTimerStillFires and the sweep
// tests above.)

// TestRegistryGCTimerStillFires verifies that the wall-clock floor still
// triggers sweeps for idle registries — the safety net for short-lived
// programs and idle daemons.
func TestRegistryGCTimerStillFires(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	vm.registryGC.Stop()
	// 100ms floor — fast enough for a test, long enough that we won't see
	// it fire in the first ~50ms.
	gc := NewRegistryGC(vm, 100*time.Millisecond)
	vm.registryGC = gc
	gc.Start()
	defer gc.Stop()

	startCount := gc.SweepCount()

	// Don't allocate anything — the only path to a sweep should be the timer.
	final := waitForSweepCount(gc, startCount+1, 500*time.Millisecond)
	if final == startCount {
		t.Fatalf("timer did not fire within 500ms with 100ms floor")
	}
	timer, _ := gc.SweepCounts()
	if timer == 0 {
		t.Errorf("expected at least one timer-triggered sweep, got 0")
	}
}
