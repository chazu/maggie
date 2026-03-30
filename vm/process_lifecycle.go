package vm

import "fmt"

// FinishProcess is called when a forked goroutine completes. It sets the
// exit reason, notifies linked processes and monitors, and cleans up.
// This replaces direct calls to proc.markDone().
func (vm *VM) FinishProcess(proc *ProcessObject, reason ExitReason) {
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

	proc.mu.Unlock()

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

			vm.deliverExitSignal(linked, proc.id, reason)
		}
	}

	// Deliver DOWN messages to monitors
	for _, ref := range monitorRefs {
		watcher := vm.GetProcessByID(ref.Watcher)
		if watcher != nil && !watcher.isDone() {
			vm.deliverDownMessage(watcher, ref, reason)
		}
	}
}

// deliverExitSignal handles a linked process death notification.
// Normal exits don't propagate through links (Erlang semantics).
// If the target is trapping exits, it receives a mailbox message.
// Otherwise, the target is killed.
func (vm *VM) deliverExitSignal(target *ProcessObject, fromID uint64, reason ExitReason) {
	if reason.Normal {
		return // normal exits don't propagate
	}

	target.mu.Lock()
	trapping := target.trapExit
	target.mu.Unlock()

	if trapping {
		// Deliver as a mailbox message with selector #exit
		msg := vm.createExitMessage(fromID, reason)
		if target.mailbox != nil {
			target.mailbox.TrySend(msg)
		}
	} else {
		// Kill the target process with a "linked" signal.
		// Note: Go goroutines can't be forcibly stopped. The goroutine
		// continues until it hits a yield point (receive, sleep, etc.)
		// and notices it's been terminated.
		linkedReason := ExitSignal("linked", reason.Result)
		vm.FinishProcess(target, linkedReason)
	}
}

// deliverDownMessage sends a processDown: notification to a watcher's mailbox.
func (vm *VM) deliverDownMessage(watcher *ProcessObject, ref *MonitorRef, reason ExitReason) {
	// Clean up the watcher's myMonitors
	watcher.mu.Lock()
	delete(watcher.myMonitors, ref.ID)
	watcher.mu.Unlock()

	// Payload: [refID, processValue, reasonSymbol, resultValue]
	downInfo := vm.createDownInfo(ref, reason)
	msg := vm.CreateMailboxMessage(
		processToValue(ref.Watched),
		"processDown:",
		downInfo,
	)

	if watcher.mailbox != nil {
		watcher.mailbox.TrySend(msg)
	}
}

// createDownInfo builds the payload for a DOWN notification.
func (vm *VM) createDownInfo(ref *MonitorRef, reason ExitReason) Value {
	arr := NewObject(vm.ArrayClass.VTable, 4)
	arr.SetSize(4)
	arr.SetSlot(0, FromSmallInt(int64(ref.ID)))
	arr.SetSlot(1, processToValue(ref.Watched))
	arr.SetSlot(2, vm.exitReasonToValue(reason))
	arr.SetSlot(3, reason.Result)
	vm.KeepAlive(arr)
	return arr.ToValue()
}

// createExitMessage builds a mailbox message for a trapped exit signal.
func (vm *VM) createExitMessage(fromID uint64, reason ExitReason) Value {
	// Payload: [senderPID, reasonSymbol, resultValue]
	arr := NewObject(vm.ArrayClass.VTable, 3)
	arr.SetSize(3)
	arr.SetSlot(0, processToValue(fromID))
	arr.SetSlot(1, vm.exitReasonToValue(reason))
	arr.SetSlot(2, reason.Result)
	vm.KeepAlive(arr)

	return vm.CreateMailboxMessage(
		processToValue(fromID),
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
