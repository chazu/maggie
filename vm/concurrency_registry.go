package vm

import (
	"sync"
	"sync/atomic"
	"unsafe"
)

// ---------------------------------------------------------------------------
// ConcurrencyRegistry
// ---------------------------------------------------------------------------
//
// After the pointer-value migration, every concurrency primitive is a
// pointer-carrying heap Value traced by Go's GC — there are no id maps to
// sweep and no id spaces to exhaust. The single exception is the process
// index below.

// ConcurrencyRegistry holds the live-process index and the monotonic
// counters for process and monitor-ref identity.
type ConcurrencyRegistry struct {
	// processes is the live-process index: id -> process, for every process
	// that has been registered and has not yet terminated. It exists because
	// process identity crosses boundaries as a plain uint64 — links and
	// monitors record peer ids, the wire protocol targets processes by id,
	// and the name registry maps names to ids. Entries are removed
	// deterministically by FinishProcess when a process terminates, so the
	// index is bounded by the number of live processes.
	processes   map[uint64]*ProcessObject
	processesMu sync.RWMutex
	processID   atomic.Uint64

	// Monitor ref ID counter (refs are plain uint64s carried in messages).
	monitorRefID atomic.Uint64
}

// AllocMonitorRefID returns the next monitor reference id.
func (cr *ConcurrencyRegistry) AllocMonitorRefID() uint64 {
	return cr.monitorRefID.Add(1) - 1
}

// NewConcurrencyRegistry creates a new concurrency registry.
func NewConcurrencyRegistry() *ConcurrencyRegistry {
	cr := &ConcurrencyRegistry{
		processes: make(map[uint64]*ProcessObject),
	}
	// Start IDs at 1 (0 could be confused with nil/uninitialized)
	cr.processID.Store(1)
	cr.monitorRefID.Store(1)
	return cr
}

// ---------------------------------------------------------------------------
// Channels
// ---------------------------------------------------------------------------

// RegisterChannel returns a channel as a pointer-carrying heap Value.
func (cr *ConcurrencyRegistry) RegisterChannel(ch *ChannelObject) Value {
	return channelToValue(ch)
}

// GetChannel retrieves a channel from its Value.
func (cr *ConcurrencyRegistry) GetChannel(v Value) *ChannelObject {
	if !isChannelValue(v) {
		return nil
	}
	return (*ChannelObject)(v.ptr)
}

// ---------------------------------------------------------------------------
// Processes
// ---------------------------------------------------------------------------

// RegisterProcess adds a process to the live-process index and returns its
// pointer-carrying Value.
func (cr *ConcurrencyRegistry) RegisterProcess(proc *ProcessObject) Value {
	cr.processesMu.Lock()
	cr.processes[proc.id] = proc
	cr.processesMu.Unlock()
	return processToValue(proc)
}

// UnregisterProcess removes a terminated process from the live-process
// index. Called by FinishProcess; idempotent. Values referencing the
// process keep working (they carry the pointer) — only by-id resolution of
// the dead process stops.
func (cr *ConcurrencyRegistry) UnregisterProcess(id uint64) {
	cr.processesMu.Lock()
	delete(cr.processes, id)
	cr.processesMu.Unlock()
}

// GetProcess retrieves a process from its Value.
func (cr *ConcurrencyRegistry) GetProcess(v Value) *ProcessObject {
	if !isProcessValue(v) {
		return nil
	}
	return (*ProcessObject)(v.ptr)
}

// GetProcessByID retrieves a live process by its raw uint64 ID. Returns nil
// for terminated (or never-registered) processes.
func (cr *ConcurrencyRegistry) GetProcessByID(id uint64) *ProcessObject {
	cr.processesMu.RLock()
	defer cr.processesMu.RUnlock()
	return cr.processes[id]
}

// CreateProcess creates a new process with a unique ID and a mailbox.
func (cr *ConcurrencyRegistry) CreateProcess(mailboxCapacity ...int) *ProcessObject {
	cap := DefaultMailboxCapacity
	if len(mailboxCapacity) > 0 && mailboxCapacity[0] > 0 {
		cap = mailboxCapacity[0]
	}
	proc := &ProcessObject{
		id:      cr.processID.Add(1) - 1,
		done:    make(chan struct{}),
		mailbox: NewMailbox(cap),
	}
	proc.state.Store(int32(ProcessRunning))
	proc.waitGroup.Add(1)
	return proc
}

// ProcessCount returns the number of live registered processes.
func (cr *ConcurrencyRegistry) ProcessCount() int {
	cr.processesMu.RLock()
	defer cr.processesMu.RUnlock()
	return len(cr.processes)
}

// ---------------------------------------------------------------------------
// Mutexes, WaitGroups, Semaphores, CancellationContexts, Futures, ArrayLists
// ---------------------------------------------------------------------------

// RegisterMutex wraps a mutex in a heap Value traced by the Go GC.
func (cr *ConcurrencyRegistry) RegisterMutex(mu *MutexObject) Value {
	return makeHeap(kindMutex, unsafe.Pointer(mu))
}

// GetMutex retrieves a mutex from its Value.
func (cr *ConcurrencyRegistry) GetMutex(v Value) *MutexObject {
	if !isMutexValue(v) {
		return nil
	}
	return (*MutexObject)(v.ptr)
}

// RegisterWaitGroup wraps a wait group in a heap Value traced by the Go GC.
func (cr *ConcurrencyRegistry) RegisterWaitGroup(wg *WaitGroupObject) Value {
	return makeHeap(kindWaitGroup, unsafe.Pointer(wg))
}

// GetWaitGroup retrieves a wait group from its Value.
func (cr *ConcurrencyRegistry) GetWaitGroup(v Value) *WaitGroupObject {
	if !isWaitGroupValue(v) {
		return nil
	}
	return (*WaitGroupObject)(v.ptr)
}

// RegisterSemaphore wraps a semaphore in a heap Value traced by the Go GC.
func (cr *ConcurrencyRegistry) RegisterSemaphore(sem *SemaphoreObject) Value {
	return makeHeap(kindSemaphore, unsafe.Pointer(sem))
}

// GetSemaphore retrieves a semaphore from its Value.
func (cr *ConcurrencyRegistry) GetSemaphore(v Value) *SemaphoreObject {
	if !isSemaphoreValue(v) {
		return nil
	}
	return (*SemaphoreObject)(v.ptr)
}

// RegisterCancellationContext wraps a cancellation context in a heap Value
// traced by the Go GC.
func (cr *ConcurrencyRegistry) RegisterCancellationContext(ctx *CancellationContextObject) Value {
	return makeHeap(kindCancellationContext, unsafe.Pointer(ctx))
}

// GetCancellationContext retrieves a cancellation context from its Value.
func (cr *ConcurrencyRegistry) GetCancellationContext(v Value) *CancellationContextObject {
	if !isCancellationContextValue(v) {
		return nil
	}
	return (*CancellationContextObject)(v.ptr)
}

// RegisterFuture returns a future as a pointer-carrying heap Value.
func (cr *ConcurrencyRegistry) RegisterFuture(f *FutureObject) Value {
	return futureToValue(f)
}

// GetFuture retrieves a future from its Value.
func (cr *ConcurrencyRegistry) GetFuture(v Value) *FutureObject {
	if !isFutureValue(v) {
		return nil
	}
	return (*FutureObject)(v.ptr)
}

// RegisterArrayList wraps an array list in a heap Value traced by the Go GC.
func (cr *ConcurrencyRegistry) RegisterArrayList(al *ArrayListObject) Value {
	return makeHeap(kindArrayList, unsafe.Pointer(al))
}

// GetArrayList retrieves an array list from its Value.
func (cr *ConcurrencyRegistry) GetArrayList(v Value) *ArrayListObject {
	if !isArrayListValue(v) {
		return nil
	}
	return (*ArrayListObject)(v.ptr)
}

// ---------------------------------------------------------------------------
// Blocks
// ---------------------------------------------------------------------------
//
// Blocks are pointer-carrying kindBlock Values (see value.go) traced by Go's
// GC — there is no block registry.

// HasBlock reports whether the Value resolves to a block. Retained for tests.
func (cr *ConcurrencyRegistry) HasBlock(v Value) bool {
	return v.blockPtr() != nil
}
