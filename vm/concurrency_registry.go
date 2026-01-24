package vm

import (
	"sync"
	"sync/atomic"
)

// ---------------------------------------------------------------------------
// ConcurrencyRegistry: Holds all concurrency-related registries
// ---------------------------------------------------------------------------

// ConcurrencyRegistry manages channels, processes, mutexes, wait groups,
// semaphores, cancellation contexts, and blocks. It provides VM-local isolation and GC integration.
type ConcurrencyRegistry struct {
	// Channel registry
	channels   map[int]*ChannelObject
	channelsMu sync.RWMutex
	channelID  atomic.Int32

	// Process registry
	processes   map[uint64]*ProcessObject
	processesMu sync.RWMutex
	processID   atomic.Uint64

	// Mutex registry
	mutexes   map[int]*MutexObject
	mutexesMu sync.RWMutex
	mutexID   atomic.Int32

	// WaitGroup registry
	waitGroups   map[int]*WaitGroupObject
	waitGroupsMu sync.RWMutex
	waitGroupID  atomic.Int32

	// Semaphore registry
	semaphores   map[int]*SemaphoreObject
	semaphoresMu sync.RWMutex
	semaphoreID  atomic.Int32

	// CancellationContext registry
	cancellationContexts   map[int]*CancellationContextObject
	cancellationContextsMu sync.RWMutex
	cancellationContextID  atomic.Int32

	// Block registry
	blocks   map[int]*BlockValue
	blocksMu sync.RWMutex
	blockID  atomic.Int32
}

// NewConcurrencyRegistry creates a new concurrency registry.
func NewConcurrencyRegistry() *ConcurrencyRegistry {
	cr := &ConcurrencyRegistry{
		channels:             make(map[int]*ChannelObject),
		processes:            make(map[uint64]*ProcessObject),
		mutexes:              make(map[int]*MutexObject),
		waitGroups:           make(map[int]*WaitGroupObject),
		semaphores:           make(map[int]*SemaphoreObject),
		cancellationContexts: make(map[int]*CancellationContextObject),
		blocks:               make(map[int]*BlockValue),
	}
	// Start IDs at 1 (0 could be confused with nil/uninitialized)
	cr.channelID.Store(1)
	cr.processID.Store(1)
	cr.mutexID.Store(1)
	cr.waitGroupID.Store(1)
	cr.semaphoreID.Store(1)
	cr.cancellationContextID.Store(1)
	cr.blockID.Store(1)
	return cr
}

// ---------------------------------------------------------------------------
// Channel Registry Methods
// ---------------------------------------------------------------------------

// RegisterChannel adds a channel to the registry and returns its Value.
func (cr *ConcurrencyRegistry) RegisterChannel(ch *ChannelObject) Value {
	id := int(cr.channelID.Add(1) - 1)

	cr.channelsMu.Lock()
	cr.channels[id] = ch
	cr.channelsMu.Unlock()

	return channelToValue(id)
}

// GetChannel retrieves a channel by its Value.
func (cr *ConcurrencyRegistry) GetChannel(v Value) *ChannelObject {
	if !isChannelValue(v) {
		return nil
	}
	id := int(v.SymbolID() & ^uint32(0xFF<<24))

	cr.channelsMu.RLock()
	defer cr.channelsMu.RUnlock()
	return cr.channels[id]
}

// SweepChannels removes closed channels from the registry.
// Returns the number of channels swept.
func (cr *ConcurrencyRegistry) SweepChannels() int {
	cr.channelsMu.Lock()
	defer cr.channelsMu.Unlock()

	swept := 0
	for id, ch := range cr.channels {
		if ch.closed.Load() {
			delete(cr.channels, id)
			swept++
		}
	}
	return swept
}

// ChannelCount returns the number of registered channels.
func (cr *ConcurrencyRegistry) ChannelCount() int {
	cr.channelsMu.RLock()
	defer cr.channelsMu.RUnlock()
	return len(cr.channels)
}

// ---------------------------------------------------------------------------
// Process Registry Methods
// ---------------------------------------------------------------------------

// RegisterProcess adds a process to the registry and returns its Value.
func (cr *ConcurrencyRegistry) RegisterProcess(proc *ProcessObject) Value {
	cr.processesMu.Lock()
	cr.processes[proc.id] = proc
	cr.processesMu.Unlock()

	return processToValue(proc.id)
}

// GetProcess retrieves a process by its Value.
func (cr *ConcurrencyRegistry) GetProcess(v Value) *ProcessObject {
	if !isProcessValue(v) {
		return nil
	}
	id := uint64(v.SymbolID() & ^uint32(0xFF<<24))

	cr.processesMu.RLock()
	defer cr.processesMu.RUnlock()
	return cr.processes[id]
}

// CreateProcess creates a new process with a unique ID.
func (cr *ConcurrencyRegistry) CreateProcess() *ProcessObject {
	id := cr.processID.Add(1) - 1
	proc := &ProcessObject{
		id:   id,
		done: make(chan struct{}),
	}
	proc.state.Store(int32(ProcessRunning))
	proc.waitGroup.Add(1)
	return proc
}

// SweepProcesses removes terminated processes from the registry.
// Returns the number of processes swept.
func (cr *ConcurrencyRegistry) SweepProcesses() int {
	cr.processesMu.Lock()
	defer cr.processesMu.Unlock()

	swept := 0
	for id, proc := range cr.processes {
		if proc.isDone() {
			delete(cr.processes, id)
			swept++
		}
	}
	return swept
}

// ProcessCount returns the number of registered processes.
func (cr *ConcurrencyRegistry) ProcessCount() int {
	cr.processesMu.RLock()
	defer cr.processesMu.RUnlock()
	return len(cr.processes)
}

// ---------------------------------------------------------------------------
// Mutex Registry Methods
// ---------------------------------------------------------------------------

// RegisterMutex adds a mutex to the registry and returns its Value.
func (cr *ConcurrencyRegistry) RegisterMutex(mu *MutexObject) Value {
	id := int(cr.mutexID.Add(1) - 1)

	cr.mutexesMu.Lock()
	cr.mutexes[id] = mu
	cr.mutexesMu.Unlock()

	return mutexToValue(id)
}

// GetMutex retrieves a mutex by its Value.
func (cr *ConcurrencyRegistry) GetMutex(v Value) *MutexObject {
	if !isMutexValue(v) {
		return nil
	}
	id := int(v.SymbolID() & ^uint32(0xFF<<24))

	cr.mutexesMu.RLock()
	defer cr.mutexesMu.RUnlock()
	return cr.mutexes[id]
}

// MutexCount returns the number of registered mutexes.
func (cr *ConcurrencyRegistry) MutexCount() int {
	cr.mutexesMu.RLock()
	defer cr.mutexesMu.RUnlock()
	return len(cr.mutexes)
}

// ---------------------------------------------------------------------------
// WaitGroup Registry Methods
// ---------------------------------------------------------------------------

// RegisterWaitGroup adds a wait group to the registry and returns its Value.
func (cr *ConcurrencyRegistry) RegisterWaitGroup(wg *WaitGroupObject) Value {
	id := int(cr.waitGroupID.Add(1) - 1)

	cr.waitGroupsMu.Lock()
	cr.waitGroups[id] = wg
	cr.waitGroupsMu.Unlock()

	return waitGroupToValue(id)
}

// GetWaitGroup retrieves a wait group by its Value.
func (cr *ConcurrencyRegistry) GetWaitGroup(v Value) *WaitGroupObject {
	if !isWaitGroupValue(v) {
		return nil
	}
	id := int(v.SymbolID() & ^uint32(0xFF<<24))

	cr.waitGroupsMu.RLock()
	defer cr.waitGroupsMu.RUnlock()
	return cr.waitGroups[id]
}

// WaitGroupCount returns the number of registered wait groups.
func (cr *ConcurrencyRegistry) WaitGroupCount() int {
	cr.waitGroupsMu.RLock()
	defer cr.waitGroupsMu.RUnlock()
	return len(cr.waitGroups)
}

// ---------------------------------------------------------------------------
// Semaphore Registry Methods
// ---------------------------------------------------------------------------

// RegisterSemaphore adds a semaphore to the registry and returns its Value.
func (cr *ConcurrencyRegistry) RegisterSemaphore(sem *SemaphoreObject) Value {
	id := int(cr.semaphoreID.Add(1) - 1)

	cr.semaphoresMu.Lock()
	cr.semaphores[id] = sem
	cr.semaphoresMu.Unlock()

	return semaphoreToValue(id)
}

// GetSemaphore retrieves a semaphore by its Value.
func (cr *ConcurrencyRegistry) GetSemaphore(v Value) *SemaphoreObject {
	if !isSemaphoreValue(v) {
		return nil
	}
	id := int(v.SymbolID() & ^uint32(0xFF<<24))

	cr.semaphoresMu.RLock()
	defer cr.semaphoresMu.RUnlock()
	return cr.semaphores[id]
}

// SemaphoreCount returns the number of registered semaphores.
func (cr *ConcurrencyRegistry) SemaphoreCount() int {
	cr.semaphoresMu.RLock()
	defer cr.semaphoresMu.RUnlock()
	return len(cr.semaphores)
}

// ---------------------------------------------------------------------------
// CancellationContext Registry Methods
// ---------------------------------------------------------------------------

// RegisterCancellationContext adds a cancellation context to the registry and returns its Value.
func (cr *ConcurrencyRegistry) RegisterCancellationContext(ctx *CancellationContextObject) Value {
	id := int(cr.cancellationContextID.Add(1) - 1)

	cr.cancellationContextsMu.Lock()
	cr.cancellationContexts[id] = ctx
	cr.cancellationContextsMu.Unlock()

	return cancellationContextToValue(id)
}

// GetCancellationContext retrieves a cancellation context by its Value.
func (cr *ConcurrencyRegistry) GetCancellationContext(v Value) *CancellationContextObject {
	if !isCancellationContextValue(v) {
		return nil
	}
	id := int(v.SymbolID() & ^uint32(0xFF<<24))

	cr.cancellationContextsMu.RLock()
	defer cr.cancellationContextsMu.RUnlock()
	return cr.cancellationContexts[id]
}

// SweepCancellationContexts removes cancelled contexts from the registry.
// Returns the number of contexts swept.
func (cr *ConcurrencyRegistry) SweepCancellationContexts() int {
	cr.cancellationContextsMu.Lock()
	defer cr.cancellationContextsMu.Unlock()

	swept := 0
	for id, ctx := range cr.cancellationContexts {
		if ctx.IsCancelled() {
			delete(cr.cancellationContexts, id)
			swept++
		}
	}
	return swept
}

// CancellationContextCount returns the number of registered cancellation contexts.
func (cr *ConcurrencyRegistry) CancellationContextCount() int {
	cr.cancellationContextsMu.RLock()
	defer cr.cancellationContextsMu.RUnlock()
	return len(cr.cancellationContexts)
}

// ---------------------------------------------------------------------------
// Block Registry Methods
// ---------------------------------------------------------------------------

// RegisterBlock adds a block to the registry and returns its ID.
func (cr *ConcurrencyRegistry) RegisterBlock(bv *BlockValue) int {
	id := int(cr.blockID.Add(1) - 1)

	cr.blocksMu.Lock()
	cr.blocks[id] = bv
	cr.blocksMu.Unlock()

	return id
}

// GetBlock retrieves a block by its ID.
func (cr *ConcurrencyRegistry) GetBlock(id int) *BlockValue {
	cr.blocksMu.RLock()
	defer cr.blocksMu.RUnlock()
	return cr.blocks[id]
}

// HasBlock checks if a block ID exists in the registry.
func (cr *ConcurrencyRegistry) HasBlock(id int) bool {
	cr.blocksMu.RLock()
	defer cr.blocksMu.RUnlock()
	_, exists := cr.blocks[id]
	return exists
}

// ReleaseBlock removes a block from the registry.
func (cr *ConcurrencyRegistry) ReleaseBlock(id int) {
	cr.blocksMu.Lock()
	defer cr.blocksMu.Unlock()
	delete(cr.blocks, id)
}

// SweepBlocks removes blocks whose home frame is no longer valid.
// This is a placeholder - actual implementation depends on frame tracking.
// Returns the number of blocks swept.
func (cr *ConcurrencyRegistry) SweepBlocks(validFrames map[int]bool) int {
	cr.blocksMu.Lock()
	defer cr.blocksMu.Unlock()

	swept := 0
	for id, bv := range cr.blocks {
		// Detached blocks (HomeFrame == -1) are always valid
		if bv.HomeFrame == -1 {
			continue
		}
		// If home frame is no longer valid, sweep the block
		if !validFrames[bv.HomeFrame] {
			delete(cr.blocks, id)
			swept++
		}
	}
	return swept
}

// BlockCount returns the number of registered blocks.
func (cr *ConcurrencyRegistry) BlockCount() int {
	cr.blocksMu.RLock()
	defer cr.blocksMu.RUnlock()
	return len(cr.blocks)
}

// ---------------------------------------------------------------------------
// Combined Sweep
// ---------------------------------------------------------------------------

// Sweep runs all sweep operations and returns stats.
func (cr *ConcurrencyRegistry) Sweep() (channels, processes, blocks int) {
	channels = cr.SweepChannels()
	processes = cr.SweepProcesses()
	// Note: blocks need valid frame info, so we skip them here
	// They should be swept by the interpreter when frames are popped
	return
}

// Stats returns counts of all registered objects.
func (cr *ConcurrencyRegistry) Stats() map[string]int {
	return map[string]int{
		"channels":             cr.ChannelCount(),
		"processes":            cr.ProcessCount(),
		"mutexes":              cr.MutexCount(),
		"waitGroups":           cr.WaitGroupCount(),
		"semaphores":           cr.SemaphoreCount(),
		"cancellationContexts": cr.CancellationContextCount(),
		"blocks":               cr.BlockCount(),
	}
}
