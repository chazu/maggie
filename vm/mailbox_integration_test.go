package vm

import (
	"testing"
	"time"
)

// TestMailboxPrimitive_SendReceive tests local inter-process messaging
// using the mailbox primitives via direct Go calls.
func TestMailboxPrimitive_SendReceive(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	// Create two processes: sender and receiver
	receiver := vm.createProcess()
	vm.registerProcess(receiver)

	// Send a message to the receiver's mailbox
	payload := FromSmallInt(42)
	msg := vm.CreateMailboxMessage(Nil, "doWork", payload)
	if !receiver.mailbox.TrySend(msg) {
		t.Fatal("TrySend failed")
	}

	// Receive from the mailbox
	got, ok := receiver.mailbox.TryReceive()
	if !ok {
		t.Fatal("TryReceive failed")
	}

	// Verify it's a MailboxMessage with the right payload
	obj := ObjectFromValue(got)
	if obj == nil {
		t.Fatal("received value is not an object")
	}
	// slot 2 = payload
	p := obj.GetSlot(2)
	if !p.IsSmallInt() || p.SmallInt() != 42 {
		t.Errorf("payload: got %v, want 42", p)
	}
	// slot 1 = selector
	sel := obj.GetSlot(1)
	if !sel.IsSymbol() {
		t.Error("selector should be a symbol")
	}
}

// TestMailboxPrimitive_ProcessCurrent tests that Process current returns
// a valid process value from the main goroutine.
func TestMailboxPrimitive_ProcessCurrent(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	proc := vm.currentProcess()
	if proc == nil {
		t.Fatal("currentProcess should not be nil for main goroutine")
	}
	if proc != vm.mainProcess {
		t.Error("currentProcess should be mainProcess for main goroutine")
	}

	procVal := vm.currentProcessValue()
	if procVal == Nil {
		t.Error("currentProcessValue should not be nil")
	}
}

// TestMailboxPrimitive_ForkedProcessCurrent tests that Process current
// returns the correct process from a forked goroutine.
func TestMailboxPrimitive_ForkedProcessCurrent(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	proc := vm.createProcess()
	procVal := vm.registerProcess(proc)

	done := make(chan Value)
	go func() {
		interp := vm.newForkedInterpreter(nil)
		interp.processID = proc.id
		vm.registerInterpreter(interp)
		defer vm.unregisterInterpreter()

		// Check current process from the forked goroutine
		current := vm.currentProcess()
		if current == nil {
			done <- Nil
			return
		}
		done <- processToValue(current.id)
	}()

	got := <-done
	if got == Nil {
		t.Fatal("forked goroutine should have a current process")
	}
	if got != procVal {
		t.Error("forked goroutine's current process should match")
	}
}

// TestMailboxPrimitive_CrossProcessMessaging tests sending between two
// forked processes via mailboxes.
func TestMailboxPrimitive_CrossProcessMessaging(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	// Create receiver process
	receiver := vm.createProcess()
	vm.registerProcess(receiver)

	// Sender goroutine sends a message
	sent := make(chan bool)
	go func() {
		msg := vm.CreateMailboxMessage(Nil, "hello", FromSmallInt(99))
		sent <- receiver.mailbox.TrySend(msg)
	}()

	if !<-sent {
		t.Fatal("send failed")
	}

	// Receiver gets the message
	got, ok := receiver.mailbox.Receive()
	if !ok {
		t.Fatal("receive failed")
	}
	obj := ObjectFromValue(got)
	if obj == nil {
		t.Fatal("not a MailboxMessage")
	}
	if p := obj.GetSlot(2); !p.IsSmallInt() || p.SmallInt() != 99 {
		t.Errorf("payload: got %v, want 99", p)
	}
}

// TestMailboxPrimitive_ProcessRegisteredNames tests name registration
// and lookup.
func TestMailboxPrimitive_ProcessRegisteredNames(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	proc := vm.createProcess()
	vm.registerProcess(proc)

	// Register
	if !vm.RegisterProcessName("worker-1", proc.id) {
		t.Fatal("RegisterProcessName should succeed")
	}

	// Lookup
	found := vm.LookupProcessName("worker-1")
	if found == Nil {
		t.Fatal("LookupProcessName should find registered process")
	}

	// Lookup unknown
	if vm.LookupProcessName("nonexistent") != Nil {
		t.Error("unknown name should return Nil")
	}

	// Duplicate registration fails
	proc2 := vm.createProcess()
	vm.registerProcess(proc2)
	if vm.RegisterProcessName("worker-1", proc2.id) {
		t.Error("duplicate name registration should fail for live process")
	}

	// Unregister
	vm.UnregisterProcessName("worker-1")
	if vm.LookupProcessName("worker-1") != Nil {
		t.Error("unregistered name should return Nil")
	}

	// Re-register to different process
	if !vm.RegisterProcessName("worker-1", proc2.id) {
		t.Error("should be able to register name after unregistration")
	}
}

// TestMailboxPrimitive_DeadProcessNameCleanup tests that names are lazily
// cleaned up when the process dies.
func TestMailboxPrimitive_DeadProcessNameCleanup(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	proc := vm.createProcess()
	vm.registerProcess(proc)
	vm.RegisterProcessName("temp-worker", proc.id)

	// Kill the process
	proc.markDone(Nil, nil)

	// Lookup should clean up and return Nil
	if vm.LookupProcessName("temp-worker") != Nil {
		t.Error("dead process name should be cleaned up on lookup")
	}
}

// TestMailboxPrimitive_ReceiveTimeout tests blocking receive with timeout.
func TestMailboxPrimitive_ReceiveTimeout(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	proc := vm.currentProcess()
	if proc == nil {
		t.Fatal("no current process")
	}

	// Timeout with no messages
	start := time.Now()
	_, ok := proc.mailbox.ReceiveTimeout(50 * time.Millisecond)
	elapsed := time.Since(start)

	if ok {
		t.Error("should timeout on empty mailbox")
	}
	if elapsed < 40*time.Millisecond {
		t.Errorf("timed out too fast: %v", elapsed)
	}
}

// TestMailboxPrimitive_ClosedMailboxRejectsSend tests that dead process
// mailboxes reject new messages.
func TestMailboxPrimitive_ClosedMailboxRejectsSend(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	proc := vm.createProcess()
	vm.registerProcess(proc)

	// Kill process (closes mailbox)
	proc.markDone(FromSmallInt(0), nil)

	// Send should fail
	msg := vm.CreateMailboxMessage(Nil, "", FromSmallInt(1))
	if proc.mailbox.TrySend(msg) {
		t.Error("send to dead process should fail")
	}
}
