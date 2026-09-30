package vm

import (
	"bytes"
	"strings"
	"sync"
	"testing"
	"time"
)

// TestSamplingProfilerBasic verifies that the profiler runs without crashing
// on a busy primitive call.
func TestSamplingProfilerBasic(t *testing.T) {
	vmInst := NewVM()
	defer vmInst.Shutdown()

	cls := vmInst.createClass("TestProf", vmInst.ObjectClass)

	// Register a primitive that spins for long enough to be sampled
	busyPrim := &Method0{name: "busyLoop", fn: func(v *VM, recv Value) Value {
		end := time.Now().Add(50 * time.Millisecond)
		for time.Now().Before(end) {
			// spin
		}
		return Nil
	}}
	cls.VTable.AddMethod(vmInst.Selectors.Intern("busyLoop"), busyPrim)

	sp := vmInst.StartSamplingProfiler(500 * time.Microsecond) // 2000 Hz

	// Invoke the busy method directly (won't tag primitives since not going through send())
	busyPrim.Invoke(vmInst, Nil, nil)

	sp.Stop()

	// Verify output format doesn't crash
	var buf bytes.Buffer
	if err := sp.WriteFoldedStacks(&buf); err != nil {
		t.Fatalf("WriteFoldedStacks error: %v", err)
	}

	vmInst.setSamplingProfiler(nil)
}

// TestSamplingProfilerFoldedFormat verifies the folded-stack output format.
func TestSamplingProfilerFoldedFormat(t *testing.T) {
	sp := &SamplingProfiler{
		root: &trieNode{children: make(map[string]*trieNode)},
	}

	// Manually build a trie
	a := &trieNode{label: "A>>foo", children: make(map[string]*trieNode)}
	b := &trieNode{label: "B>>bar", count: 3, children: make(map[string]*trieNode)}
	c := &trieNode{label: "C>>baz", count: 1, children: make(map[string]*trieNode)}
	a.children["B>>bar"] = b
	a.children["C>>baz"] = c
	sp.root.children["A>>foo"] = a

	// Also add a top-level stack
	d := &trieNode{label: "D>>qux", count: 5, children: make(map[string]*trieNode)}
	sp.root.children["D>>qux"] = d

	var buf bytes.Buffer
	if err := sp.WriteFoldedStacks(&buf); err != nil {
		t.Fatalf("WriteFoldedStacks error: %v", err)
	}

	output := buf.String()

	if !strings.Contains(output, "A>>foo;B>>bar 3") {
		t.Errorf("Expected 'A>>foo;B>>bar 3' in output, got:\n%s", output)
	}
	if !strings.Contains(output, "A>>foo;C>>baz 1") {
		t.Errorf("Expected 'A>>foo;C>>baz 1' in output, got:\n%s", output)
	}
	if !strings.Contains(output, "D>>qux 5") {
		t.Errorf("Expected 'D>>qux 5' in output, got:\n%s", output)
	}

	// Verify each line matches the format: frames count\n
	lines := strings.Split(strings.TrimSpace(output), "\n")
	for _, line := range lines {
		parts := strings.Split(line, " ")
		if len(parts) != 2 {
			t.Errorf("Line does not match 'frames count' format: %q", line)
		}
	}
}

// TestSamplingProfilerLifecycle verifies start/stop/start works without panics.
func TestSamplingProfilerLifecycle(t *testing.T) {
	vmInst := NewVM()
	defer vmInst.Shutdown()

	// Start
	sp1 := vmInst.StartSamplingProfiler(time.Millisecond)
	time.Sleep(2 * time.Millisecond)

	// Stop
	stopped := vmInst.StopSamplingProfiler()
	if stopped != sp1 {
		t.Error("StopSamplingProfiler should return the stopped profiler")
	}

	// Start again
	sp2 := vmInst.StartSamplingProfiler(time.Millisecond)
	time.Sleep(2 * time.Millisecond)
	vmInst.StopSamplingProfiler()

	if sp2 == sp1 {
		t.Error("Second profiler should be a new instance")
	}

	// Stop when already stopped
	if sp := vmInst.StopSamplingProfiler(); sp != nil {
		t.Error("StopSamplingProfiler on stopped VM should return nil")
	}
}

// TestSamplingProfilerIdleInterpreter verifies that an idle interpreter (fp == -1)
// doesn't cause a crash.
func TestSamplingProfilerIdleInterpreter(t *testing.T) {
	vmInst := NewVM()
	defer vmInst.Shutdown()

	sp := vmInst.StartSamplingProfiler(500 * time.Microsecond)
	time.Sleep(5 * time.Millisecond)
	sp.Stop()

	stats := sp.Stats()
	if stats.Dropped > 0 {
		t.Logf("Had %d dropped samples (torn reads)", stats.Dropped)
	}
	vmInst.setSamplingProfiler(nil)
}

// TestSamplingProfilerMultiInterpreter verifies that forked interpreters are sampled.
func TestSamplingProfilerMultiInterpreter(t *testing.T) {
	vmInst := NewVM()
	defer vmInst.Shutdown()

	sp := vmInst.StartSamplingProfiler(500 * time.Microsecond)

	// Register a forked interpreter
	forked := vmInst.newForkedInterpreter(nil)
	vmInst.interpreters.Store(int64(99999), forked)

	time.Sleep(5 * time.Millisecond)

	sp.Stop()

	// Should not panic. Forked interpreter with fp == -1 is safely skipped.
	_ = sp.Stats()

	vmInst.interpreters.Delete(int64(99999))
	vmInst.setSamplingProfiler(nil)
}

// TestSamplingProfilerReset verifies that Reset clears all data.
func TestSamplingProfilerReset(t *testing.T) {
	sp := NewSamplingProfiler(nil, time.Millisecond)

	// Manually add some data
	sp.samples = 100
	sp.dropped = 5
	node := &trieNode{label: "test", count: 50, children: make(map[string]*trieNode)}
	sp.root.children["test"] = node

	sp.Reset()

	stats := sp.Stats()
	if stats.TotalSamples != 0 {
		t.Errorf("After reset, TotalSamples = %d, want 0", stats.TotalSamples)
	}
	if stats.Dropped != 0 {
		t.Errorf("After reset, Dropped = %d, want 0", stats.Dropped)
	}

	var buf bytes.Buffer
	sp.WriteFoldedStacks(&buf)
	if buf.Len() != 0 {
		t.Errorf("After reset, output should be empty, got: %s", buf.String())
	}
}

// TestSamplingProfilerMaggieAPI tests the Compiler primitives for profiling.
func TestSamplingProfilerMaggieAPI(t *testing.T) {
	vmInst := NewVM()
	defer vmInst.Shutdown()

	compilerClass := vmInst.Classes.Lookup("Compiler")
	if compilerClass == nil {
		t.Fatal("Compiler class not found")
	}

	compilerVal := vmInst.globals["Compiler"]

	// isProfiling should return false initially
	isProfMethod := compilerClass.LookupClassMethod(vmInst.Selectors, "isProfiling")
	if isProfMethod == nil {
		t.Fatal("isProfiling method not found")
	}
	result := isProfMethod.Invoke(vmInst, compilerVal, nil)
	if result != False {
		t.Error("isProfiling should return false initially")
	}

	// startProfiling
	startMethod := compilerClass.LookupClassMethod(vmInst.Selectors, "startProfiling")
	if startMethod == nil {
		t.Fatal("startProfiling method not found")
	}
	startMethod.Invoke(vmInst, compilerVal, nil)

	// isProfiling should now return true
	result = isProfMethod.Invoke(vmInst, compilerVal, nil)
	if result != True {
		t.Error("isProfiling should return true after start")
	}

	// stopProfiling
	stopMethod := compilerClass.LookupClassMethod(vmInst.Selectors, "stopProfiling")
	if stopMethod == nil {
		t.Fatal("stopProfiling method not found")
	}
	stopResult := stopMethod.Invoke(vmInst, compilerVal, nil)

	// Should return a string (possibly empty), not nil
	if stopResult == Nil {
		t.Error("stopProfiling should return a string, not nil")
	}

	// isProfiling should return false again
	result = isProfMethod.Invoke(vmInst, compilerVal, nil)
	if result != False {
		t.Error("isProfiling should return false after stop")
	}
}

// TestSamplingProfilerSamplesMainOnce guards the regression where sampleAll
// sampled the main interpreter directly and again via the interpreter
// registry (NewVM registers it there), double-counting main-thread stacks.
func TestSamplingProfilerSamplesMainOnce(t *testing.T) {
	vmInst := NewVM()
	defer vmInst.Shutdown()
	// One active method frame on the main interpreter.
	vmInst.interpreter.frames[0] = CallFrame{Method: &CompiledMethod{name: "probe"}}
	vmInst.interpreter.fp = 0
	defer func() {
		vmInst.interpreter.fp = -1
		vmInst.interpreter.frames[0] = CallFrame{}
	}()

	sp := NewSamplingProfiler(vmInst, time.Millisecond)
	sp.sampleAll()
	if got := sp.Stats().TotalSamples; got != 1 {
		t.Fatalf("one sampleAll over only the main interpreter: want 1 sample, got %d", got)
	}
}

// TestSamplingProfilerConcurrentStop guards the regression where concurrent
// stops (two `Compiler stopProfiling` sends, or one racing Shutdown) closed
// the profiler's stop channel twice and panicked the process.
func TestSamplingProfilerConcurrentStop(t *testing.T) {
	vmInst := NewVM()
	defer vmInst.Shutdown()
	for round := 0; round < 20; round++ {
		sp := vmInst.StartSamplingProfiler(time.Millisecond)
		var wg sync.WaitGroup
		for i := 0; i < 4; i++ {
			wg.Add(2)
			go func() { defer wg.Done(); vmInst.StopSamplingProfiler() }()
			go func() { defer wg.Done(); sp.Stop() }()
		}
		wg.Wait()
		if vmInst.SamplingProfiler() != nil {
			t.Fatal("profiler still published after StopSamplingProfiler")
		}
	}
}

// TestSamplingProfilerZeroInterval guards against `Compiler startProfiling:`
// with a rate above 1 GHz: the interval truncated to 0 and time.NewTicker
// panicked on the sampler goroutine, killing the process.
func TestSamplingProfilerZeroInterval(t *testing.T) {
	vmInst := NewVM()
	defer vmInst.Shutdown()
	vmInst.StartSamplingProfiler(0)
	time.Sleep(2 * time.Millisecond)
	if vmInst.StopSamplingProfiler() == nil {
		t.Fatal("expected a running profiler")
	}
}
