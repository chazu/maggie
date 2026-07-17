package vm

// MonitorRef identifies a single monitor relationship.
// Immutable after creation — safe to pass between goroutines.
type MonitorRef struct {
	ID      uint64 // unique within the VM
	Watcher uint64 // process ID of the monitoring process
	Watched uint64 // process ID of the monitored process
	// Cross-node extension points (empty = local):
	// WatcherNode string
	// WatchedNode string
}

// ---------------------------------------------------------------------------
// Link operations
// ---------------------------------------------------------------------------

// LinkProcesses creates a bidirectional link between two processes.
// If either process is already dead, the other receives an immediate exit signal.
// Locks are acquired in ID order to prevent ABBA deadlock.
func (vm *VM) LinkProcesses(a, b *ProcessObject) {
	// Linking a process to itself is a no-op (Erlang-style semantics).
	// Without this guard, first==second below would acquire the same
	// non-reentrant mutex twice and self-deadlock the goroutine while
	// holding proc.mu forever.
	if a == b {
		return
	}

	first, second := a, b
	if a.id > b.id {
		first, second = b, a
	}

	// first/second establish lock order only; aliveness and exit reasons are
	// keyed to a/b directly (both locks are held), so the propagation below is
	// not sensitive to which of a/b happened to have the smaller id.
	first.mu.Lock()
	second.mu.Lock()

	aAlive := a.state.Load() != int32(ProcessTerminated)
	bAlive := b.state.Load() != int32(ProcessTerminated)

	if aAlive && bAlive {
		if a.links == nil {
			a.links = make(map[uint64]bool)
		}
		if b.links == nil {
			b.links = make(map[uint64]bool)
		}
		a.links[b.id] = true
		b.links[a.id] = true
	}

	aExit := a.exitReason
	bExit := b.exitReason

	second.mu.Unlock()
	first.mu.Unlock()

	// If exactly one side is already dead, deliver the dead side's exit reason
	// to the live side.
	if !aAlive && bAlive {
		vm.deliverExitSignal(b, a, aExit)
	}
	if !bAlive && aAlive {
		vm.deliverExitSignal(a, b, bExit)
	}
}

// UnlinkProcesses removes a bidirectional link.
func (vm *VM) UnlinkProcesses(a, b *ProcessObject) {
	// Self-unlink is a no-op; guards against the same-mutex self-deadlock
	// described in LinkProcesses.
	if a == b {
		return
	}

	first, second := a, b
	if a.id > b.id {
		first, second = b, a
	}
	first.mu.Lock()
	second.mu.Lock()

	delete(first.links, second.id)
	delete(second.links, first.id)

	second.mu.Unlock()
	first.mu.Unlock()
}

// ---------------------------------------------------------------------------
// Monitor operations
// ---------------------------------------------------------------------------

// MonitorProcess creates a unidirectional monitor: watcher monitors watched.
// Returns a MonitorRef. If watched is already dead, an immediate DOWN message
// is delivered to the watcher's mailbox.
func (vm *VM) MonitorProcess(watcher, watched *ProcessObject) (*MonitorRef, error) {
	refID := vm.registry.ConcurrencyRegistry.AllocMonitorRefID()
	ref := &MonitorRef{
		ID:      refID,
		Watcher: watcher.id,
		Watched: watched.id,
	}

	watched.mu.Lock()
	alreadyDead := watched.state.Load() == int32(ProcessTerminated)
	exitReason := watched.exitReason
	if !alreadyDead {
		if watched.monitors == nil {
			watched.monitors = make(map[uint64]*MonitorRef)
		}
		watched.monitors[ref.ID] = ref
	}
	watched.mu.Unlock()

	watcher.mu.Lock()
	if watcher.myMonitors == nil {
		watcher.myMonitors = make(map[uint64]*MonitorRef)
	}
	watcher.myMonitors[ref.ID] = ref
	watcher.mu.Unlock()

	if alreadyDead {
		vm.deliverDownMessage(watcher, ref, watched, exitReason)
	}

	return ref, nil
}

// DemonitorProcess removes a monitor relationship.
func (vm *VM) DemonitorProcess(ref *MonitorRef) {
	watched := vm.GetProcessByID(ref.Watched)
	watcher := vm.GetProcessByID(ref.Watcher)

	if watched != nil {
		watched.mu.Lock()
		delete(watched.monitors, ref.ID)
		watched.mu.Unlock()
	}
	if watcher != nil {
		watcher.mu.Lock()
		delete(watcher.myMonitors, ref.ID)
		watcher.mu.Unlock()
	}
}
