package vm

import (
	"context"
	"errors"
	"fmt"
	"os"
	"runtime/debug"
	"sync"
	"time"
)

// processKilled is the panic value that unwinds a process's goroutine once
// the process has been finished from outside (terminate, a linked exit, node
// death). Go cannot stop a goroutine, so the process stops itself: the
// interpreter checks at every frame push and backward jump, and blocking
// primitives wake on the process's done channel. It is not a Maggie
// exception — on:do: never catches it — but ensure: blocks run as it passes.
type processKilled struct{}

// bindProcess ties a forked interpreter to the process it runs, so that
// finishing the process from outside stops it.
func (i *Interpreter) bindProcess(proc *ProcessObject) {
	i.processID = proc.id
	i.proc = proc
}

// checkKilled raises processKilled once the interpreter's process has been
// finished. After the first raise it is inert, so the ensure: blocks run
// while unwinding are not killed in turn.
func (i *Interpreter) checkKilled() {
	if p := i.proc; p != nil && !i.killDelivered && p.finished.Load() {
		i.killDelivered = true
		panic(processKilled{})
	}
}

// killSignal returns a channel that is closed once the current process has
// been killed, for blocking primitives to select on; nil (never ready) when
// the caller cannot be killed or is already unwinding from a kill.
func (vm *VM) killSignal() <-chan struct{} {
	i := vm.currentInterpreter()
	if i == nil || i.proc == nil || i.killDelivered {
		return nil
	}
	return i.proc.done
}

// abortIfKilled unwinds the current process if it has been killed. Blocking
// primitives call it after waking so a killed process does not carry on.
func (vm *VM) abortIfKilled() {
	if i := vm.currentInterpreter(); i != nil {
		i.checkKilled()
	}
}

// errProcessKilled is the Err of a killContext once its process is killed.
var errProcessKilled = errors.New("process killed")

// killContext adapts a process's kill signal to context.Context, for blocking
// helpers that already take a context (channel send/receive).
type killContext struct{ kill <-chan struct{} }

func (killContext) Deadline() (time.Time, bool) { return time.Time{}, false }
func (c killContext) Done() <-chan struct{}     { return c.kill }
func (killContext) Value(any) any               { return nil }
func (c killContext) Err() error {
	select {
	case <-c.kill:
		return errProcessKilled
	default:
		return nil
	}
}

// killCtx returns a context cancelled when the current process is killed, or
// nil (block without one) when the caller cannot be killed.
func (vm *VM) killCtx() context.Context {
	if kill := vm.killSignal(); kill != nil {
		return killContext{kill}
	}
	return nil
}

// sleepKillable sleeps for d, unwinding early if the process is killed.
func (vm *VM) sleepKillable(d time.Duration) {
	kill := vm.killSignal()
	if kill == nil {
		time.Sleep(d)
		return
	}
	t := time.NewTimer(d)
	defer t.Stop()
	select {
	case <-t.C:
	case <-kill:
		vm.abortIfKilled()
	}
}

// waitKillable blocks until done is closed, unwinding early if the process is
// killed.
func (vm *VM) waitKillable(done <-chan struct{}) {
	select {
	case <-done:
	case <-vm.killSignal():
		vm.abortIfKilled()
	}
}

// waitGroupKillable waits for wg, unwinding early if the process is killed.
// sync.WaitGroup cannot be selected on, so a killable caller waits through a
// helper goroutine that lingers until the group completes.
func (vm *VM) waitGroupKillable(wg *sync.WaitGroup) {
	kill := vm.killSignal()
	if kill == nil {
		wg.Wait()
		return
	}
	done := make(chan struct{})
	go func() {
		wg.Wait()
		close(done)
	}()
	vm.waitKillable(done)
}

// lockKillable acquires mu, unwinding early if the process is killed. A
// contended lock by a killable caller is taken on a helper goroutine; if the
// kill wins, the helper releases the lock as soon as it gets it, so a killed
// process never ends up holding it.
func (vm *VM) lockKillable(mu *sync.Mutex) {
	if mu.TryLock() {
		return
	}
	kill := vm.killSignal()
	if kill == nil {
		mu.Lock()
		return
	}
	acquired := make(chan struct{})
	go func() {
		mu.Lock()
		close(acquired)
	}()
	select {
	case <-acquired:
	case <-kill:
		go func() {
			<-acquired
			mu.Unlock()
		}()
		vm.abortIfKilled()
	}
}

// HandleForkedPanic is the standard recover() handler for goroutines spawned
// by fork primitives. If r is nil it is a no-op. NonLocalReturn panics are
// converted to ExitNormal (matching ExecuteBlockDetached semantics). Any
// other panic is logged to stderr (with stack trace if MAGGIE_DEBUG=1) and
// converted to ExitError so callers of primExitReason can observe it.
func (vm *VM) HandleForkedPanic(proc *ProcessObject, r interface{}) {
	if r == nil {
		return
	}
	if _, ok := r.(processKilled); ok {
		return // already finished by whoever killed it
	}
	if nlr, ok := r.(NonLocalReturn); ok {
		vm.FinishProcess(proc, ExitNormal(nlr.Value))
		return
	}
	// An uncaught Maggie exception (signaled, no on:do: handler in the
	// forked process) terminates the process with a readable error.
	if sig, ok := r.(SignaledException); ok {
		msg := "unhandled exception"
		if sig.Object != nil {
			if sig.Object.ExceptionClass != nil {
				msg = sig.Object.ExceptionClass.Name
			}
			if text := vm.registry.GetStringContent(sig.Object.MessageText); text != "" {
				msg += ": " + text
			}
		}
		vm.FinishProcess(proc, ExitError(fmt.Errorf("%s", msg)))
		return
	}
	stack := debug.Stack()
	if os.Getenv("MAGGIE_DEBUG") == "1" {
		fmt.Fprintf(os.Stderr, "process %d panic: %v\n%s\n", proc.id, r, stack)
	} else {
		fmt.Fprintf(os.Stderr, "process %d panic: %v (set MAGGIE_DEBUG=1 for stack)\n", proc.id, r)
	}
	vm.FinishProcess(proc, ExitError(fmt.Errorf("process panic: %v", r)))
}

// FinishProcess is called when a forked goroutine completes. It sets the
// exit reason, notifies linked processes and monitors, and cleans up.
// This replaces direct calls to proc.markDone().
func (vm *VM) FinishProcess(proc *ProcessObject, reason ExitReason) {
	// One-shot: the goroutine's own completion, an explicit terminate, a
	// linked-process exit signal, and node-death cleanup can all race to
	// finish the same process. Only the first proceeds — a second
	// waitGroup.Done()/close(proc.done) would panic and abort the whole VM.
	if !proc.finished.CompareAndSwap(false, true) {
		return
	}
	proc.mu.Lock()
	proc.result = reason.Result
	if reason.Error != nil {
		proc.err = reason.Error
	}
	proc.exitReason = reason
	proc.state.Store(int32(ProcessTerminated))

	// Snapshot and clear links/monitors under the lock
	var linkedIDs []uint64
	if len(proc.links) > 0 {
		linkedIDs = make([]uint64, 0, len(proc.links))
		for id := range proc.links {
			linkedIDs = append(linkedIDs, id)
		}
		proc.links = nil
	}

	var monitorRefs []*MonitorRef
	if len(proc.monitors) > 0 {
		monitorRefs = make([]*MonitorRef, 0, len(proc.monitors))
		for _, ref := range proc.monitors {
			monitorRefs = append(monitorRefs, ref)
		}
		proc.monitors = nil
	}

	// Snapshot remote monitors
	var remoteRefs []*RemoteMonitorRef
	if len(proc.remoteMonitors) > 0 {
		remoteRefs = make([]*RemoteMonitorRef, 0, len(proc.remoteMonitors))
		for _, ref := range proc.remoteMonitors {
			remoteRefs = append(remoteRefs, ref)
		}
		proc.remoteMonitors = nil
	}

	proc.mu.Unlock()

	// Remove from the live-process index and the name registry BEFORE
	// signaling completion, so anyone who observes the process as done also
	// observes it gone from by-id/by-name resolution. The index stays
	// bounded by live processes; Values holding the process pointer keep
	// working — Go's GC reclaims the object when the last reference drops.
	vm.UnregisterProcessNamesFor(proc.id)
	vm.registry.UnregisterProcess(proc.id)

	proc.waitGroup.Done()
	if proc.mailbox != nil {
		proc.mailbox.Close()
	}
	close(proc.done)

	// Deliver exit signals to linked processes
	for _, linkedID := range linkedIDs {
		linked := vm.GetProcessByID(linkedID)
		if linked != nil && !linked.isDone() {
			// Remove reciprocal link
			linked.mu.Lock()
			delete(linked.links, proc.id)
			linked.mu.Unlock()

			vm.deliverExitSignal(linked, proc, reason)
		}
	}

	// Deliver DOWN messages to local monitors
	for _, ref := range monitorRefs {
		watcher := vm.GetProcessByID(ref.Watcher)
		if watcher != nil && !watcher.isDone() {
			vm.deliverDownMessage(watcher, ref, proc, reason)
		}
	}

	// Send DOWN notifications to remote watchers
	for _, rmRef := range remoteRefs {
		vm.sendRemoteDown(rmRef, reason)
		vm.remoteWatches.RemoveInboundMonitor(rmRef.RefID)
	}
}

// deliverExitSignal handles a linked process death notification.
// Normal exits don't propagate through links (Erlang semantics).
// If the target is trapping exits, it receives a mailbox message.
// Otherwise, the target is killed.
func (vm *VM) deliverExitSignal(target *ProcessObject, from *ProcessObject, reason ExitReason) {
	if reason.Normal {
		return // normal exits don't propagate
	}

	target.mu.Lock()
	trapping := target.trapExit
	target.mu.Unlock()

	if trapping {
		// Deliver as a mailbox message with selector #exit
		msg := vm.createExitMessage(from, reason)
		if target.mailbox != nil {
			target.mailbox.TrySend(msg)
		}
	} else {
		// Kill the target process with a "linked" signal.
		// Go cannot stop a goroutine from outside: the target unwinds
		// itself at its next frame push, backward jump, or blocking
		// primitive (see processKilled).
		linkedReason := ExitSignal("linked", reason.Result)
		vm.FinishProcess(target, linkedReason)
	}
}

// deliverDownMessage sends a processDown: notification to a watcher's
// mailbox. watched is the (just-terminated) process the monitor observed —
// its pointer Value stays valid after termination, so the watcher can still
// inspect it.
func (vm *VM) deliverDownMessage(watcher *ProcessObject, ref *MonitorRef, watched *ProcessObject, reason ExitReason) {
	// Clean up the watcher's myMonitors
	watcher.mu.Lock()
	delete(watcher.myMonitors, ref.ID)
	watcher.mu.Unlock()

	// Payload: [refID, processValue, reasonSymbol, resultValue]
	// watched is nil for remote monitors (the watched process lives on
	// another node) — the payload then carries Nil for the process slot.
	watchedVal := Nil
	if watched != nil {
		watchedVal = processToValue(watched)
	}
	downInfo := vm.createDownInfo(ref, watched, reason)
	msg := vm.CreateMailboxMessage(
		watchedVal,
		"processDown:",
		downInfo,
	)

	if watcher.mailbox != nil {
		watcher.mailbox.TrySend(msg)
	}
}

// createDownInfo builds the payload for a DOWN notification. watched may be
// nil (remote monitor) — the process slot is then Nil.
func (vm *VM) createDownInfo(ref *MonitorRef, watched *ProcessObject, reason ExitReason) Value {
	watchedVal := Nil
	if watched != nil {
		watchedVal = processToValue(watched)
	}
	arr := NewObject(vm.ArrayClass.VTable, 4)
	arr.SetSize(4)
	arr.SetSlot(0, FromSmallInt(int64(ref.ID)))
	arr.SetSlot(1, watchedVal)
	arr.SetSlot(2, vm.exitReasonToValue(reason))
	arr.SetSlot(3, reason.Result)
	return arr.ToValue()
}

// createExitMessage builds a mailbox message for a trapped exit signal.
// from may be nil (remote link severed by node death) — the sender slot is
// then Nil.
func (vm *VM) createExitMessage(from *ProcessObject, reason ExitReason) Value {
	fromVal := Nil
	if from != nil {
		fromVal = processToValue(from)
	}
	// Payload: [senderPID, reasonSymbol, resultValue]
	arr := NewObject(vm.ArrayClass.VTable, 3)
	arr.SetSize(3)
	arr.SetSlot(0, fromVal)
	arr.SetSlot(1, vm.exitReasonToValue(reason))
	arr.SetSlot(2, reason.Result)

	return vm.CreateMailboxMessage(
		fromVal,
		"exit",
		arr.ToValue(),
	)
}

// exitReasonToValue converts an ExitReason to a Maggie symbol value.
func (vm *VM) exitReasonToValue(reason ExitReason) Value {
	if reason.Normal {
		return vm.Symbols.SymbolValue("normal")
	}
	if reason.Signal != "" {
		return vm.Symbols.SymbolValue(reason.Signal)
	}
	if reason.Error != nil {
		return vm.registry.NewStringValue(fmt.Sprintf("error: %s", reason.Error.Error()))
	}
	return vm.Symbols.SymbolValue("error")
}
