package vm

import (
	"fmt"
	"sync"
	"sync/atomic"
	"unsafe"
)

// concurrencyIDMax is the largest valid ID for any concurrency primitive whose
// Value encoding shares the 24-bit symbol payload with a marker byte (see
// vm/markers.go: channelMarker, processMarker, mutexMarker, etc.). Wrap-around
// past this would alias a new primitive's ID to a live one and silently route
// sends/receives/locks to the wrong object — a correctness disaster.
//
// We deliberately do NOT recycle IDs even after Sweep removes terminated
// entries: a Value still referencing the swept primitive would resolve to a
// different live primitive after recycling, changing semantics from "nil
// lookup" to "wrong object". The 24-bit space is ~16.7M ids — at a sustained
// 30k allocations/sec it exhausts in ~9 minutes, so a channel-per-request
// server IS a realistic way to hit the cap. When it matters, adopt the block
// registry's slot+generation recycling (see blockSlotGen) rather than widening
// this counter. Exhaustion returns ErrIDSpaceExhausted, surfaced as a Maggie
// exception rather than a silent wrap.
//
// NOTE: Blocks are an exception. They have their own tag (tagBlock) with a
// 48-bit payload split into a 32-bit slot id + 16-bit generation, so they
// do NOT use this 24-bit cap. See blockSlotMaxID and the gen-tagged
// recycle scheme in RegisterBlock/ReleaseBlock.
const concurrencyIDMax int32 = (1 << 24) - 1

// concurrencyIDMaxU64 mirrors concurrencyIDMax for uint64 counters (process,
// monitorRef). Same rationale as concurrencyIDMax.
const concurrencyIDMaxU64 uint64 = (1 << 24) - 1

// ErrIDSpaceExhausted is returned when the 24-bit concurrency ID space
// is full. Callers surface this as a Maggie-level exception.
var ErrIDSpaceExhausted = fmt.Errorf("concurrency ID space exhausted (max 2^24-1)")

func allocConcurrencyID(counter *atomic.Int32, kind string) (int, error) {
	id := counter.Add(1) - 1
	if id < 0 || id > concurrencyIDMax {
		return -1, fmt.Errorf("%s: %w", kind, ErrIDSpaceExhausted)
	}
	return int(id), nil
}

func allocConcurrencyID64(counter *atomic.Uint64, kind string) (uint64, error) {
	id := counter.Add(1) - 1
	if id > concurrencyIDMaxU64 {
		return 0, fmt.Errorf("%s: %w", kind, ErrIDSpaceExhausted)
	}
	return id, nil
}

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

	// Future registry
	futures   map[int]*FutureObject
	futuresMu sync.RWMutex
	futureID  atomic.Int32

	// Monitor ref ID counter
	monitorRefID atomic.Uint64
}

func (cr *ConcurrencyRegistry) AllocMonitorRefID() (uint64, error) {
	return allocConcurrencyID64(&cr.monitorRefID, "monitorRef")
}

// NewConcurrencyRegistry creates a new concurrency registry.
func NewConcurrencyRegistry() *ConcurrencyRegistry {
	cr := &ConcurrencyRegistry{
		channels:  make(map[int]*ChannelObject),
		processes: make(map[uint64]*ProcessObject),
		futures:   make(map[int]*FutureObject),
	}
	// Start IDs at 1 (0 could be confused with nil/uninitialized)
	cr.channelID.Store(1)
	cr.processID.Store(1)
	cr.futureID.Store(1)
	cr.monitorRefID.Store(1)
	return cr
}

// ---------------------------------------------------------------------------
// Channel Registry Methods
// ---------------------------------------------------------------------------

// RegisterChannel records a channel in the id map (for GC root enumeration /
// sweeping) and returns it as a pointer-carrying heap Value. The id space can
// no longer be "exhausted" in a way that aliases live channels (the Value is a
// pointer), but the counter is retained as a map key; err is always nil in
// practice.
func (cr *ConcurrencyRegistry) RegisterChannel(ch *ChannelObject) (Value, error) {
	id, err := allocConcurrencyID(&cr.channelID, "channel")
	if err != nil {
		return Nil, err
	}

	cr.channelsMu.Lock()
	cr.channels[id] = ch
	cr.channelsMu.Unlock()

	return channelToValue(ch), nil
}

// GetChannel retrieves a channel from its Value.
func (cr *ConcurrencyRegistry) GetChannel(v Value) *ChannelObject {
	if !isChannelValue(v) {
		return nil
	}
	return (*ChannelObject)(v.ptr)
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
func (cr *ConcurrencyRegistry) RegisterProcess(proc *ProcessObject) (Value, error) {
	cr.processesMu.Lock()
	cr.processes[proc.id] = proc
	cr.processesMu.Unlock()

	return processToValue(proc.id), nil
}

// GetProcess retrieves a process by its Value.
func (cr *ConcurrencyRegistry) GetProcess(v Value) *ProcessObject {
	if !isProcessValue(v) {
		return nil
	}
	id := uint64(markedIDFromValue(v))

	cr.processesMu.RLock()
	defer cr.processesMu.RUnlock()
	return cr.processes[id]
}

// GetProcessByID retrieves a process by its raw uint64 ID.
func (cr *ConcurrencyRegistry) GetProcessByID(id uint64) *ProcessObject {
	cr.processesMu.RLock()
	defer cr.processesMu.RUnlock()
	return cr.processes[id]
}

// CreateProcess creates a new process with a unique ID and a mailbox.
func (cr *ConcurrencyRegistry) CreateProcess(mailboxCapacity ...int) (*ProcessObject, error) {
	cap := DefaultMailboxCapacity
	if len(mailboxCapacity) > 0 && mailboxCapacity[0] > 0 {
		cap = mailboxCapacity[0]
	}
	id, err := allocConcurrencyID64(&cr.processID, "process")
	if err != nil {
		return nil, err
	}
	proc := &ProcessObject{
		id:      id,
		done:    make(chan struct{}),
		mailbox: NewMailbox(cap),
	}
	proc.state.Store(int32(ProcessRunning))
	proc.waitGroup.Add(1)
	return proc, nil
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

// RegisterMutex wraps a mutex in a heap Value traced by the Go GC. The
// (Value, error) signature is retained for call-site compatibility; err is
// always nil now that there is no id space to exhaust.
func (cr *ConcurrencyRegistry) RegisterMutex(mu *MutexObject) (Value, error) {
	return makeHeap(kindMutex, unsafe.Pointer(mu)), nil
}

// GetMutex retrieves a mutex from its Value.
func (cr *ConcurrencyRegistry) GetMutex(v Value) *MutexObject {
	if !isMutexValue(v) {
		return nil
	}
	return (*MutexObject)(v.ptr)
}

// ---------------------------------------------------------------------------
// WaitGroup Registry Methods
// ---------------------------------------------------------------------------

// RegisterWaitGroup wraps a wait group in a heap Value traced by the Go GC.
func (cr *ConcurrencyRegistry) RegisterWaitGroup(wg *WaitGroupObject) (Value, error) {
	return makeHeap(kindWaitGroup, unsafe.Pointer(wg)), nil
}

// GetWaitGroup retrieves a wait group from its Value.
func (cr *ConcurrencyRegistry) GetWaitGroup(v Value) *WaitGroupObject {
	if !isWaitGroupValue(v) {
		return nil
	}
	return (*WaitGroupObject)(v.ptr)
}

// ---------------------------------------------------------------------------
// Semaphore Registry Methods
// ---------------------------------------------------------------------------

// RegisterSemaphore wraps a semaphore in a heap Value traced by the Go GC.
func (cr *ConcurrencyRegistry) RegisterSemaphore(sem *SemaphoreObject) (Value, error) {
	return makeHeap(kindSemaphore, unsafe.Pointer(sem)), nil
}

// GetSemaphore retrieves a semaphore from its Value.
func (cr *ConcurrencyRegistry) GetSemaphore(v Value) *SemaphoreObject {
	if !isSemaphoreValue(v) {
		return nil
	}
	return (*SemaphoreObject)(v.ptr)
}

// ---------------------------------------------------------------------------
// CancellationContext Registry Methods
// ---------------------------------------------------------------------------

// RegisterCancellationContext wraps a cancellation context in a heap Value
// traced by the Go GC.
func (cr *ConcurrencyRegistry) RegisterCancellationContext(ctx *CancellationContextObject) (Value, error) {
	return makeHeap(kindCancellationContext, unsafe.Pointer(ctx)), nil
}

// GetCancellationContext retrieves a cancellation context from its Value.
func (cr *ConcurrencyRegistry) GetCancellationContext(v Value) *CancellationContextObject {
	if !isCancellationContextValue(v) {
		return nil
	}
	return (*CancellationContextObject)(v.ptr)
}

// ---------------------------------------------------------------------------
// Blocks
// ---------------------------------------------------------------------------
//
// Blocks are pointer-carrying kindBlock Values (see value.go) traced by Go's
// GC — there is no block registry, slot recycling, or generation guard. The
// former RegisterBlock/GetBlock/ReleaseBlock/SweepBlocksLive machinery was
// removed with the custom collector.

// HasBlock reports whether the Value resolves to a block. Retained for tests.
func (cr *ConcurrencyRegistry) HasBlock(v Value) bool {
	return v.blockPtr() != nil
}

// ---------------------------------------------------------------------------
// Combined Sweep
// ---------------------------------------------------------------------------

// Sweep runs all sweep operations and returns stats. Blocks are pointer-carrying
// Values reclaimed by Go's GC — there is nothing to sweep for them here.
func (cr *ConcurrencyRegistry) Sweep() (channels, processes int) {
	channels = cr.SweepChannels()
	processes = cr.SweepProcesses()
	return
}

// ---------------------------------------------------------------------------
// Future Registry Methods
// ---------------------------------------------------------------------------

// RegisterFuture records a future in the id map (for GC root enumeration /
// sweeping) and returns it as a pointer-carrying heap Value.
func (cr *ConcurrencyRegistry) RegisterFuture(f *FutureObject) (Value, error) {
	id, err := allocConcurrencyID(&cr.futureID, "future")
	if err != nil {
		return Nil, err
	}
	cr.futuresMu.Lock()
	cr.futures[id] = f
	cr.futuresMu.Unlock()
	return futureToValue(f), nil
}

// GetFuture retrieves a future from its Value.
func (cr *ConcurrencyRegistry) GetFuture(v Value) *FutureObject {
	if !isFutureValue(v) {
		return nil
	}
	return (*FutureObject)(v.ptr)
}

// FutureCount returns the number of registered futures.
func (cr *ConcurrencyRegistry) FutureCount() int {
	cr.futuresMu.RLock()
	defer cr.futuresMu.RUnlock()
	return len(cr.futures)
}

// SweepFutures removes resolved futures from the registry.
func (cr *ConcurrencyRegistry) SweepFutures() int {
	cr.futuresMu.Lock()
	defer cr.futuresMu.Unlock()
	swept := 0
	for id, f := range cr.futures {
		if f.IsResolved() {
			delete(cr.futures, id)
			swept++
		}
	}
	return swept
}

// ---------------------------------------------------------------------------
// ArrayList Registry Methods
// ---------------------------------------------------------------------------

// RegisterArrayList wraps an array list in a heap Value traced by the Go GC.
// The (Value, error) signature is retained for call-site compatibility; there
// is no longer an id space to exhaust, so err is always nil.
func (cr *ConcurrencyRegistry) RegisterArrayList(al *ArrayListObject) (Value, error) {
	return makeHeap(kindArrayList, unsafe.Pointer(al)), nil
}

// GetArrayList retrieves an array list from its Value.
func (cr *ConcurrencyRegistry) GetArrayList(v Value) *ArrayListObject {
	if !isArrayListValue(v) {
		return nil
	}
	return (*ArrayListObject)(v.ptr)
}

// Stats returns counts of all registered objects.
func (cr *ConcurrencyRegistry) Stats() map[string]int {
	return map[string]int{
		"channels":  cr.ChannelCount(),
		"processes": cr.ProcessCount(),
		"futures":   cr.FutureCount(),
	}
}
