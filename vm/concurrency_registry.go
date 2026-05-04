package vm

import (
	"fmt"
	"sync"
	"sync/atomic"
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
// lookup" to "wrong object". For 4G IDs to actually be exhausted in a single
// VM lifetime, an application would need to allocate ~30k channels/sec for
// 5 days straight without restart — at which point a panic is the right
// failure mode (it's a leak, not a normal workload).
//
// NOTE: Blocks are an exception. They have their own tag (tagBlock) with a
// 48-bit payload split into a 32-bit slot id + 16-bit generation, so they
// do NOT use this 24-bit cap. See blockSlotMaxID and the gen-tagged
// recycle scheme in RegisterBlock/ReleaseBlock.
const concurrencyIDMax int32 = (1 << 24) - 1

// concurrencyIDMaxU64 mirrors concurrencyIDMax for uint64 counters (process,
// monitorRef). Same rationale as concurrencyIDMax.
const concurrencyIDMaxU64 uint64 = (1 << 24) - 1

// allocConcurrencyID atomically bumps a uint32-sized counter and panics on
// exhaustion. The shared helper guarantees identical overflow semantics for
// every concurrency kind so future readers have one invariant to remember.
func allocConcurrencyID(counter *atomic.Int32, kind string) int {
	id := counter.Add(1) - 1
	if id < 0 || id > concurrencyIDMax {
		panic(fmt.Sprintf("ConcurrencyRegistry: %s ID space exhausted (max 2^24-1 live %s)", kind, kind))
	}
	return int(id)
}

// allocConcurrencyID64 is the uint64 counterpart for processes and monitors.
func allocConcurrencyID64(counter *atomic.Uint64, kind string) uint64 {
	id := counter.Add(1) - 1
	if id > concurrencyIDMaxU64 {
		panic(fmt.Sprintf("ConcurrencyRegistry: %s ID space exhausted (max 2^24-1 live %s)", kind, kind))
	}
	return id
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

	// Block registry. Slots are recycled: when a block is released, its
	// slot id is pushed onto freeBlockSlots and its generation in
	// blockSlotGen is bumped. Stale Values pointing at the old (slot, gen)
	// fail the gen check at lookup and resolve to nil instead of aliasing
	// onto whatever block now lives in that slot.
	blocks            map[int]*BlockValue
	blockSlotGen      map[int]uint16  // current generation per slot id
	freeBlockSlots    []int           // recycled slot ids waiting to be reused
	blocksByHomeFrame map[int][]int   // maps frameIndex → list of blockIDs
	blocksMu          sync.RWMutex
	blockID           atomic.Int64    // grows only when no free slot available

	// Future registry
	futures   map[int]*FutureObject
	futuresMu sync.RWMutex
	futureID  atomic.Int32

	// ArrayList registry
	arrayLists   map[int]*ArrayListObject
	arrayListsMu sync.RWMutex
	arrayListID  atomic.Int32

	// Monitor ref ID counter
	monitorRefID atomic.Uint64
}

// AllocMonitorRefID returns a fresh monitor ref ID. Panics on counter exhaustion.
func (cr *ConcurrencyRegistry) AllocMonitorRefID() uint64 {
	return allocConcurrencyID64(&cr.monitorRefID, "monitorRef")
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
		blockSlotGen:         make(map[int]uint16),
		blocksByHomeFrame:    make(map[int][]int),
		futures:              make(map[int]*FutureObject),
		arrayLists:           make(map[int]*ArrayListObject),
	}
	// Start IDs at 1 (0 could be confused with nil/uninitialized)
	cr.channelID.Store(1)
	cr.processID.Store(1)
	cr.mutexID.Store(1)
	cr.waitGroupID.Store(1)
	cr.semaphoreID.Store(1)
	cr.cancellationContextID.Store(1)
	cr.blockID.Store(1)
	cr.futureID.Store(1)
	cr.arrayListID.Store(1)
	cr.monitorRefID.Store(1)
	return cr
}

// ---------------------------------------------------------------------------
// Channel Registry Methods
// ---------------------------------------------------------------------------

// RegisterChannel adds a channel to the registry and returns its Value.
func (cr *ConcurrencyRegistry) RegisterChannel(ch *ChannelObject) Value {
	id := allocConcurrencyID(&cr.channelID, "channel")

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

// GetProcessByID retrieves a process by its raw uint64 ID.
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
	id := allocConcurrencyID64(&cr.processID, "process")
	proc := &ProcessObject{
		id:      id,
		done:    make(chan struct{}),
		mailbox: NewMailbox(cap),
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
	id := allocConcurrencyID(&cr.mutexID, "mutex")

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
	id := allocConcurrencyID(&cr.waitGroupID, "waitGroup")

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
	id := allocConcurrencyID(&cr.semaphoreID, "semaphore")

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
	id := allocConcurrencyID(&cr.cancellationContextID, "cancellationContext")

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
	}

	gen := cr.blockSlotGen[slot] // 0 for fresh slot, current gen for recycled
	cr.blocks[slot] = bv
	cr.blocksByHomeFrame[bv.HomeFrame] = append(cr.blocksByHomeFrame[bv.HomeFrame], slot)

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
	defer cr.blocksMu.RUnlock()
	if cr.blockSlotGen[slot] != gen {
		return nil
	}
	return cr.blocks[slot]
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
	if cr.blockSlotGen[slot] != gen {
		return // already released or recycled
	}
	if _, ok := cr.blocks[slot]; !ok {
		return
	}
	delete(cr.blocks, slot)
	cr.blockSlotGen[slot] = nextBlockGen(gen)
	cr.freeBlockSlots = append(cr.freeBlockSlots, slot)
}

// ReleaseBlocksForFrame releases all blocks whose home frame matches
// frameIndex. Each released slot's generation is bumped and the slot is
// returned to the free list. Blocks with HomeFrame == -1 (detached, e.g.
// forked) are not tracked here.
func (cr *ConcurrencyRegistry) ReleaseBlocksForFrame(frameIndex int) {
	cr.blocksMu.Lock()
	defer cr.blocksMu.Unlock()
	slots, ok := cr.blocksByHomeFrame[frameIndex]
	if !ok {
		return
	}
	for _, slot := range slots {
		bv, present := cr.blocks[slot]
		if !present {
			continue
		}
		// Defensive: if the slot has since been recycled to a block with
		// a different home frame, skip — the per-frame slice may carry
		// stale entries.
		if bv.HomeFrame != frameIndex {
			continue
		}
		delete(cr.blocks, slot)
		cr.blockSlotGen[slot] = nextBlockGen(cr.blockSlotGen[slot])
		cr.freeBlockSlots = append(cr.freeBlockSlots, slot)
	}
	delete(cr.blocksByHomeFrame, frameIndex)
}

// SweepBlocks releases blocks whose home frame is no longer valid.
// Detached blocks (HomeFrame == -1) are unaffected. Returns the number
// of blocks released.
func (cr *ConcurrencyRegistry) SweepBlocks(validFrames map[int]bool) int {
	cr.blocksMu.Lock()
	defer cr.blocksMu.Unlock()

	swept := 0
	for slot, bv := range cr.blocks {
		if bv.HomeFrame == -1 {
			continue
		}
		if !validFrames[bv.HomeFrame] {
			delete(cr.blocks, slot)
			cr.blockSlotGen[slot] = nextBlockGen(cr.blockSlotGen[slot])
			cr.freeBlockSlots = append(cr.freeBlockSlots, slot)
			swept++
		}
	}

	for frame := range cr.blocksByHomeFrame {
		if frame == -1 {
			continue
		}
		if !validFrames[frame] {
			delete(cr.blocksByHomeFrame, frame)
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

// BlocksByHomeFrameCount returns the number of blocks tracked for a given home frame.
func (cr *ConcurrencyRegistry) BlocksByHomeFrameCount(homeFrame int) int {
	cr.blocksMu.RLock()
	defer cr.blocksMu.RUnlock()
	return len(cr.blocksByHomeFrame[homeFrame])
}

// BlocksByHomeFrameHas checks if a home frame is tracked in blocksByHomeFrame.
func (cr *ConcurrencyRegistry) BlocksByHomeFrameHas(homeFrame int) bool {
	cr.blocksMu.RLock()
	defer cr.blocksMu.RUnlock()
	_, exists := cr.blocksByHomeFrame[homeFrame]
	return exists
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

// RegisterFuture adds a future to the registry and returns its Value.
func (cr *ConcurrencyRegistry) RegisterFuture(f *FutureObject) Value {
	id := allocConcurrencyID(&cr.futureID, "future")
	cr.futuresMu.Lock()
	cr.futures[id] = f
	cr.futuresMu.Unlock()
	return futureToValue(uint32(id))
}

// GetFuture retrieves a future by its Value.
func (cr *ConcurrencyRegistry) GetFuture(v Value) *FutureObject {
	if !isFutureValue(v) {
		return nil
	}
	id := int(futureIDFromValue(v))
	cr.futuresMu.RLock()
	defer cr.futuresMu.RUnlock()
	return cr.futures[id]
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

// RegisterArrayList adds an array list to the registry and returns its Value.
func (cr *ConcurrencyRegistry) RegisterArrayList(al *ArrayListObject) Value {
	id := allocConcurrencyID(&cr.arrayListID, "arrayList")

	cr.arrayListsMu.Lock()
	cr.arrayLists[id] = al
	cr.arrayListsMu.Unlock()

	return arrayListToValue(id)
}

// GetArrayList retrieves an array list by its Value.
func (cr *ConcurrencyRegistry) GetArrayList(v Value) *ArrayListObject {
	if !isArrayListValue(v) {
		return nil
	}
	id := int(v.SymbolID() & ^uint32(0xFF<<24))

	cr.arrayListsMu.RLock()
	defer cr.arrayListsMu.RUnlock()
	return cr.arrayLists[id]
}

// ArrayListCount returns the number of registered array lists.
func (cr *ConcurrencyRegistry) ArrayListCount() int {
	cr.arrayListsMu.RLock()
	defer cr.arrayListsMu.RUnlock()
	return len(cr.arrayLists)
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
		"futures":              cr.FutureCount(),
		"arrayLists":           cr.ArrayListCount(),
	}
}
