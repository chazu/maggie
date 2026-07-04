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

	// Block registry. Slots are recycled: when a block is released, its
	// slot id is pushed onto freeBlockSlots and its generation in
	// blockSlotGen is bumped. Stale Values pointing at the old (slot, gen)
	// fail the gen check at lookup and resolve to nil instead of aliasing
	// onto whatever block now lives in that slot.
	//
	// blocks and blockSlotGen are slices indexed by slot id (much faster
	// than map[int] for the hot GetBlock path). They grow on demand under
	// blocksMu.
	blocks            []*BlockValue // slot id -> *BlockValue (nil = empty)
	blockSlotGen      []uint16      // slot id -> current generation
	freeBlockSlots    []int         // recycled slot ids waiting to be reused
	blocksMu          sync.RWMutex
	blockID           atomic.Int64 // grows only when no free slot available
	blockLiveCount    int          // number of non-nil entries in blocks
	blockPressureHook func(int32)  // called under blocksMu after each RegisterBlock

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
		channels:             make(map[int]*ChannelObject),
		processes:            make(map[uint64]*ProcessObject),
		blocks:               make([]*BlockValue, 0, 256),
		blockSlotGen:         make([]uint16, 0, 256),
		futures:              make(map[int]*FutureObject),
	}
	// Start IDs at 1 (0 could be confused with nil/uninitialized)
	cr.channelID.Store(1)
	cr.processID.Store(1)
	cr.blockID.Store(1)
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
// Block Registry Methods
// ---------------------------------------------------------------------------

// blockSlotMaxID caps the slot id space to 2^32 - 1, matching the 32-bit
// slot field in the block Value encoding. Steady-state usage is bounded by
// peak live blocks, not cumulative allocations, because slots are recycled
// on release. We only hit this cap if peak live blocks exceeds 4 billion,
// which is far beyond any realistic workload.
const blockSlotMaxID int64 = (1 << 32) - 1

// nextBlockGen advances a generation counter. Generation 0 is reserved for
// "fresh slot, never recycled" so we skip it on wrap to keep that meaning.
func nextBlockGen(g uint16) uint16 {
	g++
	if g == 0 {
		g = 1
	}
	return g
}

// RegisterBlock adds a block to the registry and returns the encoded
// Value (slot id + generation). Reuses a recycled slot if one is
// available; otherwise grows the slot space.
func (cr *ConcurrencyRegistry) RegisterBlock(bv *BlockValue) Value {
	cr.blocksMu.Lock()
	defer cr.blocksMu.Unlock()

	var slot int
	if n := len(cr.freeBlockSlots); n > 0 {
		slot = cr.freeBlockSlots[n-1]
		cr.freeBlockSlots = cr.freeBlockSlots[:n-1]
	} else {
		next := cr.blockID.Add(1) - 1
		if next < 0 || next > blockSlotMaxID {
			panic(fmt.Sprintf("ConcurrencyRegistry: block slot id space exhausted (max %d live blocks)", blockSlotMaxID))
		}
		slot = int(next)
		// Grow slices to accommodate the new slot
		for slot >= len(cr.blocks) {
			newCap := len(cr.blocks) * 2
			if newCap == 0 {
				newCap = 256
			}
			if newCap <= slot {
				newCap = slot + 1
			}
			newBlocks := make([]*BlockValue, newCap)
			copy(newBlocks, cr.blocks)
			cr.blocks = newBlocks
			newGens := make([]uint16, newCap)
			copy(newGens, cr.blockSlotGen)
			cr.blockSlotGen = newGens
		}
	}

	gen := cr.blockSlotGen[slot] // 0 for fresh slot, current gen for recycled
	cr.blocks[slot] = bv
	cr.blockLiveCount++
	if cr.blockPressureHook != nil {
		// Hook only reads the passed count + atomics and does a non-blocking
		// channel send; it must NOT re-acquire blocksMu.
		cr.blockPressureHook(int32(cr.blockLiveCount))
	}

	return FromBlockSlotGen(uint32(slot), gen)
}

// GetBlock retrieves a block by its Value. Returns nil if the slot has
// been recycled (generation mismatch) or never registered.
func (cr *ConcurrencyRegistry) GetBlock(v Value) *BlockValue {
	if !v.IsBlock() {
		return nil
	}
	slot := int(v.BlockID())
	gen := v.BlockGen()

	cr.blocksMu.RLock()
	if slot >= len(cr.blocks) {
		cr.blocksMu.RUnlock()
		return nil
	}
	if cr.blockSlotGen[slot] != gen {
		cr.blocksMu.RUnlock()
		return nil
	}
	bv := cr.blocks[slot]
	cr.blocksMu.RUnlock()
	return bv
}

// HasBlock reports whether the Value still resolves to a live block.
func (cr *ConcurrencyRegistry) HasBlock(v Value) bool {
	return cr.GetBlock(v) != nil
}

// ReleaseBlock releases the slot referenced by v if (and only if) v's
// generation still matches the slot's current generation. Bumps the
// slot's generation and pushes it onto the free list for reuse. Stale
// or already-released Values are no-ops.
func (cr *ConcurrencyRegistry) ReleaseBlock(v Value) {
	if !v.IsBlock() {
		return
	}
	slot := int(v.BlockID())
	gen := v.BlockGen()

	cr.blocksMu.Lock()
	defer cr.blocksMu.Unlock()
	if slot >= len(cr.blocks) || cr.blockSlotGen[slot] != gen {
		return // already released or recycled
	}
	if cr.blocks[slot] == nil {
		return
	}
	cr.blocks[slot] = nil
	cr.blockLiveCount--
	cr.blockSlotGen[slot] = nextBlockGen(gen)
	cr.freeBlockSlots = append(cr.freeBlockSlots, slot)
}

// SweepBlocksLive frees every frame-bound block (HomeFrame >= 0) whose slot
// is not present in the live set, recycling its slot for reuse. Detached
// blocks (HomeFrame == -1, e.g. forked) are never swept here — they are
// reclaimed by the fork goroutine's defer and are treated as unconditional
// GC roots. The live set is the set of slot ids the tracing collector reached
// from the complete root set; the caller MUST run under stop-the-world so no
// other goroutine mutates the registry or holds an unrooted block Value.
// Returns the number of blocks freed.
func (cr *ConcurrencyRegistry) SweepBlocksLive(live map[uint32]struct{}) int {
	cr.blocksMu.Lock()
	defer cr.blocksMu.Unlock()

	swept := 0
	for slot, bv := range cr.blocks {
		if bv == nil {
			continue
		}
		if bv.HomeFrame == -1 {
			continue // detached: reclaimed by the fork goroutine, not here
		}
		if _, ok := live[uint32(slot)]; ok {
			continue // still reachable
		}
		cr.blocks[slot] = nil
		cr.blockLiveCount--
		cr.blockSlotGen[slot] = nextBlockGen(cr.blockSlotGen[slot])
		cr.freeBlockSlots = append(cr.freeBlockSlots, slot)
		swept++
	}
	return swept
}


// BlockCount returns the number of live (non-nil) registered blocks.
func (cr *ConcurrencyRegistry) BlockCount() int {
	cr.blocksMu.RLock()
	defer cr.blocksMu.RUnlock()
	return cr.blockLiveCount
}

// SetBlockPressureHook installs (or clears, with nil) a callback invoked under
// blocksMu after each RegisterBlock with the current live block count. Used by
// RegistryGC to trigger a sweep when frame-bound blocks accumulate.
func (cr *ConcurrencyRegistry) SetBlockPressureHook(h func(int32)) {
	cr.blocksMu.Lock()
	cr.blockPressureHook = h
	cr.blocksMu.Unlock()
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
		"channels":             cr.ChannelCount(),
		"processes":            cr.ProcessCount(),
		"blocks":               cr.BlockCount(),
		"futures":              cr.FutureCount(),
	}
}
