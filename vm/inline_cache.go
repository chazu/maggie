package vm

import (
	"sync/atomic"
)

// Inline Caching for Method Dispatch
//
// Based on Cog VM's proven approach where:
// - ~90% of call sites are monomorphic (single receiver type)
// - ~9% are polymorphic (2-6 types)
// - ~1% are megamorphic (many types)
//
// The cache is indexed by bytecode PC within a method, so each
// call site has its own cache entry.
//
// Concurrency model:
//   - InlineCacheTable is built once at CompiledMethod finalization by
//     scanning the bytecode for OpSend instructions; the resulting map is
//     immutable thereafter (concurrent reads on a never-written map are
//     race-free per the Go memory model).
//   - Each InlineCache holds an atomic.Pointer[icSnapshot]. Lookup is one
//     atomic load followed by a read of an immutable snapshot. Update
//     builds a fresh snapshot and CAS-publishes; on CAS failure the
//     update is dropped (best-effort cache).
//   - Hits / Misses counters are atomic.Uint64 — mildly racey reads of
//     the pair are tolerated, drift is cosmetic.

// CacheState represents the current state of an inline cache.
type CacheState uint8

const (
	CacheEmpty       CacheState = iota // No cached lookup yet
	CacheMonomorphic                   // Single (class, method) cached
	CachePolymorphic                   // 2-6 entries in PIC
	CacheMegamorphic                   // Too many types, use full lookup
)

// MaxPICEntries is the maximum number of entries in a polymorphic inline cache.
// Cog VM uses 6 entries.
const MaxPICEntries = 6

// InlineCacheEntry holds a single cached method lookup result.
type InlineCacheEntry struct {
	Class  *Class // Receiver class
	Method Method // Resolved method
}

// icSnapshot is an immutable, atomically-published view of an inline cache.
// Every transition (Empty → Mono → Poly → Mega) creates a new snapshot;
// readers see exactly one snapshot per atomic load.
type icSnapshot struct {
	State   CacheState
	Count   int8
	Entries [MaxPICEntries]InlineCacheEntry
}

// emptySnapshot is the canonical zero-state snapshot used after Reset.
// Stored once and shared by all caches; callers must never mutate it.
var emptySnapshot = &icSnapshot{State: CacheEmpty}

// InlineCache represents the cache state for a single call site.
// It progresses through states: Empty -> Monomorphic -> Polymorphic -> Megamorphic.
type InlineCache struct {
	snap atomic.Pointer[icSnapshot]

	// Statistics for profiling. Best-effort; readers may observe drift.
	Hits   atomic.Uint64
	Misses atomic.Uint64
}

// State returns the current cache state (best-effort, atomic load).
func (ic *InlineCache) State() CacheState {
	if s := ic.snap.Load(); s != nil {
		return s.State
	}
	return CacheEmpty
}

// Count returns the current entry count (best-effort, atomic load).
func (ic *InlineCache) Count() int {
	if s := ic.snap.Load(); s != nil {
		return int(s.Count)
	}
	return 0
}

// Entries returns a copy of the current entries (best-effort, atomic load).
// Returns a slice of length Count.
func (ic *InlineCache) Entries() []InlineCacheEntry {
	s := ic.snap.Load()
	if s == nil || s.Count == 0 {
		return nil
	}
	out := make([]InlineCacheEntry, s.Count)
	copy(out, s.Entries[:s.Count])
	return out
}

// Lookup checks the cache for a method matching the given class.
// Returns the cached method on hit, nil on miss.
func (ic *InlineCache) Lookup(class *Class) Method {
	s := ic.snap.Load()
	if s == nil {
		ic.Misses.Add(1)
		return nil
	}
	switch s.State {
	case CacheMonomorphic:
		if s.Entries[0].Class == class {
			ic.Hits.Add(1)
			return s.Entries[0].Method
		}
	case CachePolymorphic:
		// Linear search through entries (typically 2-6 entries).
		for i := int8(0); i < s.Count; i++ {
			if s.Entries[i].Class == class {
				ic.Hits.Add(1)
				return s.Entries[i].Method
			}
		}
	case CacheMegamorphic, CacheEmpty:
		// Always miss.
	}
	ic.Misses.Add(1)
	return nil
}

// Update records a new (class, method) pair, potentially upgrading the
// cache state. Builds a fresh immutable snapshot and CAS-publishes; on
// CAS failure (another goroutine raced), the update is dropped — caches
// are best-effort and the racing goroutine's snapshot will satisfy the
// next Lookup.
func (ic *InlineCache) Update(class *Class, method Method) {
	if method == nil {
		return // Don't cache failed lookups.
	}

	old := ic.snap.Load()
	var next *icSnapshot

	switch {
	case old == nil || old.State == CacheEmpty:
		next = &icSnapshot{State: CacheMonomorphic, Count: 1}
		next.Entries[0] = InlineCacheEntry{Class: class, Method: method}

	case old.State == CacheMonomorphic:
		if old.Entries[0].Class == class {
			return // Already cached this class.
		}
		next = &icSnapshot{State: CachePolymorphic, Count: 2}
		next.Entries[0] = old.Entries[0]
		next.Entries[1] = InlineCacheEntry{Class: class, Method: method}

	case old.State == CachePolymorphic:
		// Check if already present.
		for i := int8(0); i < old.Count; i++ {
			if old.Entries[i].Class == class {
				return
			}
		}
		if old.Count < MaxPICEntries {
			next = &icSnapshot{State: CachePolymorphic, Count: old.Count + 1}
			copy(next.Entries[:], old.Entries[:old.Count])
			next.Entries[old.Count] = InlineCacheEntry{Class: class, Method: method}
		} else {
			// Promote to megamorphic; entries left zeroed.
			next = &icSnapshot{State: CacheMegamorphic}
		}

	case old.State == CacheMegamorphic:
		return // Stay megamorphic.
	}

	// Best-effort publish. CAS failure is acceptable: the winner's
	// snapshot is at least as good as ours for future Lookups.
	ic.snap.CompareAndSwap(old, next)
}

// Reset clears the cache back to empty state.
func (ic *InlineCache) Reset() {
	ic.snap.Store(emptySnapshot)
	ic.Hits.Store(0)
	ic.Misses.Store(0)
}

// HitRate returns the cache hit rate as a percentage (0-100).
func (ic *InlineCache) HitRate() float64 {
	hits := ic.Hits.Load()
	misses := ic.Misses.Load()
	total := hits + misses
	if total == 0 {
		return 0
	}
	return float64(hits) * 100 / float64(total)
}

// ---------------------------------------------------------------------------
// InlineCacheTable: per-method index of call-site caches
// ---------------------------------------------------------------------------

// InlineCacheTable holds one InlineCache per OpSend call site in a
// method's bytecode. Built once at CompiledMethod finalization; the
// `sites` map is never written after publication.
type InlineCacheTable struct {
	sites map[int]*InlineCache // PC of OpSend → cache; immutable after build
}

// NewInlineCacheTable creates an empty inline cache table. Used by tests
// and for methods with no bytecode. Production code should call
// BuildInlineCacheTable to pre-populate the table from bytecode.
func NewInlineCacheTable() *InlineCacheTable {
	return &InlineCacheTable{sites: make(map[int]*InlineCache)}
}

// BuildInlineCacheTable scans bytecode for OpSend instructions and
// allocates one InlineCache per call site. The returned table's `sites`
// map is never mutated afterward, so concurrent reads are race-free.
func BuildInlineCacheTable(bytecode []byte) *InlineCacheTable {
	sites := make(map[int]*InlineCache)
	for pc := 0; pc < len(bytecode); {
		op := Opcode(bytecode[pc])
		// Only OpSend currently goes through the IC path. OpSendSuper
		// and OpTailSend have separate dispatch paths that do not
		// consult the cache (see interpreter.send / execSendSuper).
		if op == OpSend {
			sites[pc] = &InlineCache{}
		}
		// Advance by instruction width = 1 byte opcode + operand bytes.
		operandBytes := 0
		if info, ok := opcodeTable[op]; ok {
			operandBytes = info.OperandBytes
		}
		pc += 1 + operandBytes
	}
	return &InlineCacheTable{sites: sites}
}

// Get returns the cache for a given PC, or nil if no OpSend exists at
// that PC. Safe for concurrent use because `sites` is immutable.
func (t *InlineCacheTable) Get(pc int) *InlineCache {
	if t == nil {
		return nil
	}
	return t.sites[pc]
}

// GetOrCreate is preserved for legacy callers; it returns the
// pre-allocated cache for the PC, or nil if no OpSend exists there.
// (The "create" semantics from the old map-based table no longer apply
// — the table is fully populated at finalization.)
func (t *InlineCacheTable) GetOrCreate(pc int) *InlineCache {
	return t.Get(pc)
}

// Stats returns aggregate statistics for all caches in the table.
func (t *InlineCacheTable) Stats() (mono, poly, mega, empty int, totalHits, totalMisses uint64) {
	if t == nil {
		return
	}
	for _, ic := range t.sites {
		switch ic.State() {
		case CacheMonomorphic:
			mono++
		case CachePolymorphic:
			poly++
		case CacheMegamorphic:
			mega++
		case CacheEmpty:
			empty++
		}
		totalHits += ic.Hits.Load()
		totalMisses += ic.Misses.Load()
	}
	return
}

// HitRate returns the aggregate hit rate for all caches.
func (t *InlineCacheTable) HitRate() float64 {
	_, _, _, _, hits, misses := t.Stats()
	total := hits + misses
	if total == 0 {
		return 0
	}
	return float64(hits) * 100 / float64(total)
}

// Reset clears all caches in the table.
func (t *InlineCacheTable) Reset() {
	if t == nil {
		return
	}
	for _, ic := range t.sites {
		ic.Reset()
	}
}

// InvalidateAllCaches resets every inline cache across all classes in the table.
// This is the safe "sledgehammer" approach: caches are just an optimization and
// will repopulate on next dispatch. Call after rehydration or any bulk VTable update.
func InvalidateAllCaches(ct *ClassTable) {
	ct.mu.RLock()
	defer ct.mu.RUnlock()

	for _, class := range ct.classes {
		if class.VTable != nil {
			invalidateVTableCaches(class.VTable)
		}
		if class.ClassVTable != nil {
			invalidateVTableCaches(class.ClassVTable)
		}
	}
}

func invalidateVTableCaches(vt *VTable) {
	for _, method := range vt.LocalMethods() {
		if cm, ok := method.(*CompiledMethod); ok {
			cm.getInlineCachesIfBuilt().Reset()
		}
	}
}

// ICStats holds aggregate inline cache statistics.
type ICStats struct {
	TotalCallSites  int     // Total number of call sites with caches
	Monomorphic     int     // Call sites in monomorphic state
	Polymorphic     int     // Call sites in polymorphic state
	Megamorphic     int     // Call sites in megamorphic state
	Empty           int     // Call sites never used
	TotalHits       uint64  // Total cache hits
	TotalMisses     uint64  // Total cache misses
	HitRate         float64 // Overall hit rate percentage
	MonomorphicRate float64 // Percentage of call sites that are monomorphic
}

// CollectICStats gathers inline cache statistics from all classes in a ClassTable.
func CollectICStats(ct *ClassTable) ICStats {
	var stats ICStats

	ct.mu.RLock()
	defer ct.mu.RUnlock()

	for _, class := range ct.classes {
		if class.VTable != nil {
			collectFromVTable(class.VTable, &stats)
		}
		if class.ClassVTable != nil {
			collectFromVTable(class.ClassVTable, &stats)
		}
	}

	total := stats.TotalHits + stats.TotalMisses
	if total > 0 {
		stats.HitRate = float64(stats.TotalHits) * 100 / float64(total)
	}

	nonEmpty := stats.TotalCallSites - stats.Empty
	if nonEmpty > 0 {
		stats.MonomorphicRate = float64(stats.Monomorphic) * 100 / float64(nonEmpty)
	}

	return stats
}

func collectFromVTable(vt *VTable, stats *ICStats) {
	for _, method := range vt.LocalMethods() {
		if cm, ok := method.(*CompiledMethod); ok {
			t := cm.getInlineCachesIfBuilt()
			if t == nil {
				continue
			}
			mono, poly, mega, empty, hits, misses := t.Stats()
			stats.Monomorphic += mono
			stats.Polymorphic += poly
			stats.Megamorphic += mega
			stats.Empty += empty
			stats.TotalHits += hits
			stats.TotalMisses += misses
			stats.TotalCallSites += mono + poly + mega + empty
		}
	}
}
