package vm

// Inline Caching for Method Dispatch
//
// Based on Cog VM's proven approach where:
// - ~90% of call sites are monomorphic (single receiver type)
// - ~9% are polymorphic (2-6 types)
// - ~1% are megamorphic (many types)
//
// The cache is indexed by bytecode PC within a method, so each
// call site has its own cache entry.

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

// InlineCache represents the cache state for a single call site.
// It progresses through states: Empty -> Monomorphic -> Polymorphic -> Megamorphic
type InlineCache struct {
	State   CacheState
	Entries [MaxPICEntries]InlineCacheEntry
	Count   int // Number of valid entries (1 for mono, 2-6 for poly)

	// Statistics for profiling
	Hits   uint64
	Misses uint64
}

// Lookup checks the cache for a method matching the given class.
// Returns the cached method on hit, nil on miss.
func (ic *InlineCache) Lookup(class *Class) Method {
	switch ic.State {
	case CacheMonomorphic:
		if ic.Entries[0].Class == class {
			ic.Hits++
			return ic.Entries[0].Method
		}

	case CachePolymorphic:
		// Linear search through entries (typically 2-6 entries)
		for i := 0; i < ic.Count; i++ {
			if ic.Entries[i].Class == class {
				ic.Hits++
				return ic.Entries[i].Method
			}
		}

	case CacheMegamorphic, CacheEmpty:
		// Always miss for megamorphic or empty
	}

	ic.Misses++
	return nil
}

// Update records a new (class, method) pair, potentially upgrading the cache state.
func (ic *InlineCache) Update(class *Class, method Method) {
	if method == nil {
		return // Don't cache failed lookups
	}

	switch ic.State {
	case CacheEmpty:
		// First lookup - become monomorphic
		ic.State = CacheMonomorphic
		ic.Entries[0] = InlineCacheEntry{Class: class, Method: method}
		ic.Count = 1

	case CacheMonomorphic:
		if ic.Entries[0].Class == class {
			return // Already cached this class
		}
		// Second different class - upgrade to polymorphic
		ic.State = CachePolymorphic
		ic.Entries[1] = InlineCacheEntry{Class: class, Method: method}
		ic.Count = 2

	case CachePolymorphic:
		// Check if already present
		for i := 0; i < ic.Count; i++ {
			if ic.Entries[i].Class == class {
				return // Already cached
			}
		}
		// Add new entry if room
		if ic.Count < MaxPICEntries {
			ic.Entries[ic.Count] = InlineCacheEntry{Class: class, Method: method}
			ic.Count++
		} else {
			// Too many types - go megamorphic
			ic.State = CacheMegamorphic
			// Clear entries to free memory (optional)
			for i := range ic.Entries {
				ic.Entries[i] = InlineCacheEntry{}
			}
			ic.Count = 0
		}

	case CacheMegamorphic:
		// Stay megamorphic, don't cache anything
	}
}

// HitRate returns the cache hit rate as a percentage (0-100).
func (ic *InlineCache) HitRate() float64 {
	total := ic.Hits + ic.Misses
	if total == 0 {
		return 0
	}
	return float64(ic.Hits) * 100 / float64(total)
}

// Reset clears the cache back to empty state.
func (ic *InlineCache) Reset() {
	ic.State = CacheEmpty
	ic.Count = 0
	ic.Hits = 0
	ic.Misses = 0
	for i := range ic.Entries {
		ic.Entries[i] = InlineCacheEntry{}
	}
}

// InlineCacheTable manages inline caches for all call sites in a method.
// It maps bytecode PC to cache entry.
type InlineCacheTable struct {
	caches map[int]*InlineCache
}

// NewInlineCacheTable creates a new inline cache table.
func NewInlineCacheTable() *InlineCacheTable {
	return &InlineCacheTable{
		caches: make(map[int]*InlineCache),
	}
}

// GetOrCreate returns the cache for a given PC, creating one if needed.
func (t *InlineCacheTable) GetOrCreate(pc int) *InlineCache {
	if ic := t.caches[pc]; ic != nil {
		return ic
	}
	ic := &InlineCache{State: CacheEmpty}
	t.caches[pc] = ic
	return ic
}

// Get returns the cache for a given PC, or nil if none exists.
func (t *InlineCacheTable) Get(pc int) *InlineCache {
	return t.caches[pc]
}

// Stats returns aggregate statistics for all caches in the table.
func (t *InlineCacheTable) Stats() (mono, poly, mega, empty int, totalHits, totalMisses uint64) {
	for _, ic := range t.caches {
		switch ic.State {
		case CacheMonomorphic:
			mono++
		case CachePolymorphic:
			poly++
		case CacheMegamorphic:
			mega++
		case CacheEmpty:
			empty++
		}
		totalHits += ic.Hits
		totalMisses += ic.Misses
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
	for _, ic := range t.caches {
		ic.Reset()
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
		// Collect from instance methods
		if class.VTable != nil {
			collectFromVTable(class.VTable, &stats)
		}
		// Collect from class methods
		if class.ClassVTable != nil {
			collectFromVTable(class.ClassVTable, &stats)
		}
	}

	// Calculate rates
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
	for _, method := range vt.methods {
		if cm, ok := method.(*CompiledMethod); ok {
			if cm.InlineCaches != nil {
				mono, poly, mega, empty, hits, misses := cm.InlineCaches.Stats()
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
}
