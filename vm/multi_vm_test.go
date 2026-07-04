package vm

import (
	"testing"
)

// ---------------------------------------------------------------------------
// Multi-VM Integration Tests
//
// These tests prove that two VMs can run independently in a single Go
// process without interference. Each scenario exercises a different
// registry or subsystem to verify full isolation.
// ---------------------------------------------------------------------------

// TestMultiVM_IndependentChannels verifies that channels created in
// separate VMs are fully isolated: same IDs do not alias, and sending
// on one VM's channel does not affect the other.
func TestMultiVM_IndependentChannels(t *testing.T) {
	vm1 := NewVM()
	defer vm1.Shutdown()
	vm2 := NewVM()
	defer vm2.Shutdown()

	// Create a buffered channel in each VM
	ch1 := vm1.Send(vm1.classValue(vm1.ChannelClass), "new:", []Value{FromSmallInt(1)})
	ch2 := vm2.Send(vm2.classValue(vm2.ChannelClass), "new:", []Value{FromSmallInt(1)})

	if ch1 == Nil || ch2 == Nil {
		t.Fatal("Failed to create channels")
	}

	// Send different values into each VM's channel
	vm1.Send(ch1, "send:", []Value{FromSmallInt(100)})
	vm2.Send(ch2, "send:", []Value{FromSmallInt(200)})

	// Receive from each — must get the value sent to THAT VM's channel
	val1 := vm1.Send(ch1, "receive", nil)
	val2 := vm2.Send(ch2, "receive", nil)

	if !val1.IsSmallInt() || val1.SmallInt() != 100 {
		t.Errorf("VM1 channel: got %v, want 100", val1)
	}
	if !val2.IsSmallInt() || val2.SmallInt() != 200 {
		t.Errorf("VM2 channel: got %v, want 200", val2)
	}

	// Close VM1's channel — VM2's channel must remain open
	vm1.Send(ch1, "close", nil)

	isClosed1 := vm1.Send(ch1, "isClosed", nil)
	isClosed2 := vm2.Send(ch2, "isClosed", nil)

	if isClosed1 != True {
		t.Error("VM1 channel should be closed")
	}
	if isClosed2 != False {
		t.Error("VM2 channel should still be open")
	}

	// Registry counts are independent
	count1 := vm1.Concurrency().ChannelCount()
	count2 := vm2.Concurrency().ChannelCount()

	// VM1 closed its channel but it's still in registry until GC sweep
	// VM2 has exactly one open channel
	if count2 < 1 {
		t.Errorf("VM2 should have at least 1 channel, got %d", count2)
	}
	t.Logf("VM1 channels: %d, VM2 channels: %d", count1, count2)
}

// TestMultiVM_IndependentProcesses verifies that processes created in
// separate VMs are isolated: process IDs may overlap but refer to
// different processes, and one VM's process registry is invisible to
// the other.
func TestMultiVM_IndependentProcesses(t *testing.T) {
	vm1 := NewVM()
	defer vm1.Shutdown()
	vm2 := NewVM()
	defer vm2.Shutdown()

	// Create processes directly in each VM
	proc1, err := vm1.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm1.registerProcess(proc1); err != nil {
		t.Fatal(err)
	}

	proc2, err := vm2.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm2.registerProcess(proc2); err != nil {
		t.Fatal(err)
	}

	// Each VM should see exactly 1 process
	if vm1.Concurrency().ProcessCount() < 1 {
		t.Error("VM1 should have at least 1 process")
	}
	if vm2.Concurrency().ProcessCount() < 1 {
		t.Error("VM2 should have at least 1 process")
	}

	// Completing a process in VM1 should not affect VM2's process count
	countBefore := vm2.Concurrency().ProcessCount()
	proc1.markDone(FromSmallInt(1), nil)
	countAfter := vm2.Concurrency().ProcessCount()

	if countAfter != countBefore {
		t.Errorf("VM2 process count changed when VM1 process completed: before=%d, after=%d",
			countBefore, countAfter)
	}

	// Clean up
	proc2.markDone(FromSmallInt(2), nil)
}

// TestMultiVM_IndependentClassVariables verifies that class variables
// in one VM are invisible to the other, even when both VMs have
// classes with the same name.
func TestMultiVM_IndependentClassVariables(t *testing.T) {
	vm1 := NewVM()
	defer vm1.Shutdown()
	vm2 := NewVM()
	defer vm2.Shutdown()

	// Both VMs have an ObjectClass. Set a class variable on each.
	vm1.Registry().SetClassVar(vm1.ObjectClass, "count", FromSmallInt(42))
	vm2.Registry().SetClassVar(vm2.ObjectClass, "count", FromSmallInt(99))

	// Read back — each VM should see only its own value
	val1 := vm1.Registry().GetClassVar(vm1.ObjectClass, "count")
	val2 := vm2.Registry().GetClassVar(vm2.ObjectClass, "count")

	if !val1.IsSmallInt() || val1.SmallInt() != 42 {
		t.Errorf("VM1 classVar 'count': got %v, want 42", val1)
	}
	if !val2.IsSmallInt() || val2.SmallInt() != 99 {
		t.Errorf("VM2 classVar 'count': got %v, want 99", val2)
	}

	// Updating VM1's variable must not affect VM2
	vm1.Registry().SetClassVar(vm1.ObjectClass, "count", FromSmallInt(0))

	val2After := vm2.Registry().GetClassVar(vm2.ObjectClass, "count")
	if !val2After.IsSmallInt() || val2After.SmallInt() != 99 {
		t.Errorf("VM2 classVar 'count' changed after VM1 update: got %v, want 99", val2After)
	}
}

// (Removed TestMultiVM_IndependentBlocks: blocks are pointer-carrying Values
// owned by Go's GC, not entries in a per-VM block registry, so there is no
// per-VM block table to isolate.)

// TestMultiVM_IndependentGC verifies that running RegistryGC on one VM
// sweeps only that VM's stale objects, leaving the other VM untouched.
func TestMultiVM_IndependentGC(t *testing.T) {
	vm1 := NewVM()
	defer vm1.Shutdown()
	vm2 := NewVM()
	defer vm2.Shutdown()

	// Create channels in both VMs
	ch1 := vm1.Send(vm1.classValue(vm1.ChannelClass), "new:", []Value{FromSmallInt(1)})
	ch2 := vm2.Send(vm2.classValue(vm2.ChannelClass), "new:", []Value{FromSmallInt(1)})

	vm2Count := vm2.Concurrency().ChannelCount()

	// Close VM1's channel
	vm1.Send(ch1, "close", nil)

	// Sweep VM1 — should clean up VM1's closed channel
	stats := vm1.registryGC.SweepNow()
	if stats.Channels < 1 {
		t.Errorf("VM1 sweep should have cleaned at least 1 channel, got %d", stats.Channels)
	}

	// VM2's channel count must be unchanged
	if vm2.Concurrency().ChannelCount() != vm2Count {
		t.Errorf("VM2 channel count changed after VM1 GC: got %d, want %d",
			vm2.Concurrency().ChannelCount(), vm2Count)
	}

	// VM2's channel should still work
	vm2.Send(ch2, "send:", []Value{FromSmallInt(42)})
	val := vm2.Send(ch2, "receive", nil)
	if !val.IsSmallInt() || val.SmallInt() != 42 {
		t.Errorf("VM2 channel after VM1 GC: got %v, want 42", val)
	}
}

// TestMultiVM_IndependentGlobals verifies that global variables set in
// one VM do not leak into another.
func TestMultiVM_IndependentGlobals(t *testing.T) {
	vm1 := NewVM()
	defer vm1.Shutdown()
	vm2 := NewVM()
	defer vm2.Shutdown()

	// Set a global in VM1
	vm1.globals["myVar"] = FromSmallInt(123)

	// VM2 should not see it
	if val, ok := vm2.globals["myVar"]; ok {
		t.Errorf("VM2 should not have 'myVar', got %v", val)
	}

	// Set a different value for the same name in VM2
	vm2.globals["myVar"] = FromSmallInt(456)

	// VM1's value must be unchanged
	if vm1.globals["myVar"].SmallInt() != 123 {
		t.Errorf("VM1 global 'myVar': got %d, want 123", vm1.globals["myVar"].SmallInt())
	}
	if vm2.globals["myVar"].SmallInt() != 456 {
		t.Errorf("VM2 global 'myVar': got %d, want 456", vm2.globals["myVar"].SmallInt())
	}
}
