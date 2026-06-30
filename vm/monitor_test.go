package vm

import (
	"testing"
	"time"
)

// ---------------------------------------------------------------------------
// Monitor tests
// ---------------------------------------------------------------------------

func TestMonitor_ReceivesDownOnDeath(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	watcher, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(watcher); err != nil {
		t.Fatal(err)
	}

	watched, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(watched); err != nil {
		t.Fatal(err)
	}

	// Watcher monitors watched
	ref, err := vm.MonitorProcess(watcher, watched)
	if err != nil {
		t.Fatal(err)
	}
	if ref == nil {
		t.Fatal("MonitorProcess returned nil")
	}

	// Kill the watched process
	vm.FinishProcess(watched, ExitNormal(FromSmallInt(42)))

	// Watcher should receive a processDown: message
	msg, ok := watcher.mailbox.TryReceive()
	if !ok {
		t.Fatal("watcher should receive a DOWN message")
	}

	msgObj := ObjectFromValue(msg)
	if msgObj == nil {
		t.Fatal("DOWN message should be a MailboxMessage object")
	}

	// selector should be "processDown:"
	selSlot := msgObj.GetSlot(1)
	if selSlot.IsSymbol() {
		selName := vm.Symbols.Name(selSlot.SymbolID())
		if selName != "processDown:" {
			t.Errorf("selector: got %q, want %q", selName, "processDown:")
		}
	} else {
		t.Error("selector should be a symbol")
	}

	// payload should be an array [refID, processValue, reason, result]
	payload := msgObj.GetSlot(2)
	arr := ObjectFromValue(payload)
	if arr == nil {
		t.Fatal("payload should be an array")
	}
	if arr.NumSlots() < 4 {
		t.Fatalf("payload array: got %d slots, want 4", arr.NumSlots())
	}

	// Check ref ID
	if refID := arr.GetSlot(0); !refID.IsSmallInt() || refID.SmallInt() != int64(ref.ID) {
		t.Errorf("refID: got %v, want %d", refID, ref.ID)
	}

	// Check reason is "normal"
	reasonSlot := arr.GetSlot(2)
	if reasonSlot.IsSymbol() {
		name := vm.Symbols.Name(reasonSlot.SymbolID())
		if name != "normal" {
			t.Errorf("reason: got %q, want %q", name, "normal")
		}
	}
}

func TestMonitor_DeadProcessImmediateNotification(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	watcher, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(watcher); err != nil {
		t.Fatal(err)
	}

	watched, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(watched); err != nil {
		t.Fatal(err)
	}

	// Kill watched BEFORE monitoring
	vm.FinishProcess(watched, ExitError(errForTest("crashed")))

	// Now monitor — should get immediate DOWN
	if _, err := vm.MonitorProcess(watcher, watched); err != nil {
		t.Fatal(err)
	}

	msg, ok := watcher.mailbox.TryReceive()
	if !ok {
		t.Fatal("should receive immediate DOWN for already-dead process")
	}

	msgObj := ObjectFromValue(msg)
	if msgObj == nil {
		t.Fatal("message should be an object")
	}
}

func TestMonitor_Demonitor(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	watcher, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(watcher); err != nil {
		t.Fatal(err)
	}

	watched, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(watched); err != nil {
		t.Fatal(err)
	}

	ref, err := vm.MonitorProcess(watcher, watched)
	if err != nil {
		t.Fatal(err)
	}
	vm.DemonitorProcess(ref)

	// Kill watched — watcher should NOT receive DOWN
	vm.FinishProcess(watched, ExitNormal(Nil))

	_, ok := watcher.mailbox.TryReceive()
	if ok {
		t.Error("demonitored process should not receive DOWN")
	}
}

// ---------------------------------------------------------------------------
// Link tests
// ---------------------------------------------------------------------------

func TestLink_PropagatesAbnormalExit(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	procA, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(procA); err != nil {
		t.Fatal(err)
	}

	procB, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(procB); err != nil {
		t.Fatal(err)
	}

	vm.LinkProcesses(procA, procB)

	// Kill A with an error
	vm.FinishProcess(procA, ExitError(errForTest("crash")))

	// B should also be terminated (linked death propagation)
	time.Sleep(10 * time.Millisecond) // allow propagation
	if !procB.isDone() {
		t.Error("linked process B should be terminated after A crashes")
	}
}

func TestLink_NormalExitDoesNotPropagate(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	procA, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(procA); err != nil {
		t.Fatal(err)
	}

	procB, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(procB); err != nil {
		t.Fatal(err)
	}

	vm.LinkProcesses(procA, procB)

	// Kill A normally
	vm.FinishProcess(procA, ExitNormal(FromSmallInt(42)))

	// B should still be alive (normal exits don't propagate)
	time.Sleep(10 * time.Millisecond)
	if procB.isDone() {
		t.Error("linked process B should NOT die on A's normal exit")
	}
}

func TestLink_TrapExit(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	procA, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(procA); err != nil {
		t.Fatal(err)
	}

	procB, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(procB); err != nil {
		t.Fatal(err)
	}

	// B traps exits
	procB.mu.Lock()
	procB.trapExit = true
	procB.mu.Unlock()

	vm.LinkProcesses(procA, procB)

	// Kill A with error
	vm.FinishProcess(procA, ExitError(errForTest("crash")))

	// B should still be alive (trapping exits)
	time.Sleep(10 * time.Millisecond)
	if procB.isDone() {
		t.Error("trapping process B should NOT die")
	}

	// B should receive an exit message in its mailbox
	msg, ok := procB.mailbox.TryReceive()
	if !ok {
		t.Fatal("trapping process should receive exit message")
	}

	msgObj := ObjectFromValue(msg)
	if msgObj == nil {
		t.Fatal("exit message should be an object")
	}

	// Selector should be "exit"
	selSlot := msgObj.GetSlot(1)
	if selSlot.IsSymbol() {
		name := vm.Symbols.Name(selSlot.SymbolID())
		if name != "exit" {
			t.Errorf("exit message selector: got %q, want %q", name, "exit")
		}
	}
}

func TestLink_Unlink(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	procA, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(procA); err != nil {
		t.Fatal(err)
	}

	procB, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(procB); err != nil {
		t.Fatal(err)
	}

	vm.LinkProcesses(procA, procB)
	vm.UnlinkProcesses(procA, procB)

	// Kill A with error — B should NOT die
	vm.FinishProcess(procA, ExitError(errForTest("crash")))

	time.Sleep(10 * time.Millisecond)
	if procB.isDone() {
		t.Error("unlinked process B should NOT die")
	}
}

func TestLink_DeadProcessSendsImmediateSignal(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	procA, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(procA); err != nil {
		t.Fatal(err)
	}

	procB, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(procB); err != nil {
		t.Fatal(err)
	}

	// Kill A first
	vm.FinishProcess(procA, ExitError(errForTest("already dead")))

	// Now link B to dead A — B should be killed immediately
	procB.mu.Lock()
	procB.trapExit = true // trap so we can check the message
	procB.mu.Unlock()

	vm.LinkProcesses(procA, procB)

	msg, ok := procB.mailbox.TryReceive()
	if !ok {
		t.Fatal("linking to dead process should send immediate exit signal")
	}
	_ = msg
}

// ---------------------------------------------------------------------------
// ExitReason tests
// ---------------------------------------------------------------------------

func TestExitReason_Normal(t *testing.T) {
	r := ExitNormal(FromSmallInt(42))
	if !r.Normal {
		t.Error("should be normal")
	}
	if r.Result.SmallInt() != 42 {
		t.Errorf("result: got %d, want 42", r.Result.SmallInt())
	}
}

func TestExitReason_Error(t *testing.T) {
	r := ExitError(errForTest("boom"))
	if r.Normal {
		t.Error("should not be normal")
	}
	if r.Error.Error() != "boom" {
		t.Errorf("error: got %q, want %q", r.Error.Error(), "boom")
	}
}

func TestExitReason_Signal(t *testing.T) {
	r := ExitSignal("linked", Nil)
	if r.Normal {
		t.Error("should not be normal")
	}
	if r.Signal != "linked" {
		t.Errorf("signal: got %q, want %q", r.Signal, "linked")
	}
}

// ---------------------------------------------------------------------------
// FinishProcess tests
// ---------------------------------------------------------------------------

func TestFinishProcess_SetsExitReason(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	proc, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(proc); err != nil {
		t.Fatal(err)
	}

	vm.FinishProcess(proc, ExitNormal(FromSmallInt(99)))

	if !proc.isDone() {
		t.Error("process should be terminated")
	}
	proc.mu.Lock()
	reason := proc.exitReason
	proc.mu.Unlock()

	if !reason.Normal {
		t.Error("exit reason should be normal")
	}
	if reason.Result.SmallInt() != 99 {
		t.Errorf("exit result: got %d, want 99", reason.Result.SmallInt())
	}
}

func TestFinishProcess_ClosesMailbox(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	proc, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(proc); err != nil {
		t.Fatal(err)
	}

	vm.FinishProcess(proc, ExitNormal(Nil))

	if !proc.mailbox.IsClosed() {
		t.Error("mailbox should be closed after finish")
	}
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

type testError struct{ msg string }

func (e *testError) Error() string { return e.msg }
func errForTest(msg string) error  { return &testError{msg} }

// TestLinkProcesses_SelfLinkIsNoOp guards against the self-deadlock regression:
// LinkProcesses(a, a) would otherwise lock the same non-reentrant mutex twice.
// The test must complete well within the timeout; a hang means the bug is back.
func TestLinkProcesses_SelfLinkIsNoOp(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	p, err := vm.createProcess()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := vm.registerProcess(p); err != nil {
		t.Fatal(err)
	}

	done := make(chan struct{})
	go func() {
		vm.LinkProcesses(p, p)
		vm.UnlinkProcesses(p, p)
		close(done)
	}()

	select {
	case <-done:
		// ok — no deadlock, and a process is not linked to itself
		p.mu.Lock()
		_, linked := p.links[p.id]
		p.mu.Unlock()
		if linked {
			t.Error("self-link should be a no-op, but process is linked to itself")
		}
	case <-time.After(5 * time.Second):
		t.Fatal("LinkProcesses(p, p) deadlocked (self-link regression)")
	}
}
