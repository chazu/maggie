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
	first, second := a, b
	if a.id > b.id {
		first, second = b, a
	}

	first.mu.Lock()
	second.mu.Lock()

	aAlive := first.state.Load() != int32(ProcessTerminated)
	bAlive := second.state.Load() != int32(ProcessTerminated)

	if aAlive && bAlive {
		if first.links == nil {
			first.links = make(map[uint64]bool)
		}
		if second.links == nil {
			second.links = make(map[uint64]bool)
		}
		first.links[second.id] = true
		second.links[first.id] = true
	}

	firstExit := first.exitReason
	secondExit := second.exitReason

	second.mu.Unlock()
	first.mu.Unlock()

	// If one side is already dead, propagate exit signal
	if !aAlive && bAlive {
		vm.deliverExitSignal(b, a.id, firstExit)
	}
	if !bAlive && aAlive {
		vm.deliverExitSignal(a, b.id, secondExit)
	}
}

// UnlinkProcesses removes a bidirectional link.
func (vm *VM) UnlinkProcesses(a, b *ProcessObject) {
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
func (vm *VM) MonitorProcess(watcher, watched *ProcessObject) *MonitorRef {
	refID := vm.registry.ConcurrencyRegistry.monitorRefID.Add(1)
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
		vm.deliverDownMessage(watcher, ref, exitReason)
	}

	return ref
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
