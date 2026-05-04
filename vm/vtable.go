package vm

// VTable holds the method dispatch table for a class.
//
// Local methods are stored in a compact `methods` map keyed by selector ID,
// so per-class memory is proportional to the number of methods actually
// defined on the class — not to the global selector ID space.
//
// Lookups use an immutable `vtSnapshot` published through `snap`
// (atomic.Pointer). The snapshot is an open-addressed (linear-probing) hash
// table that flattens the entire inheritance chain. It is sized to the
// next power of two >= 2 * methodsInChain (load factor <= 0.5), so the
// average probe length is ~1.5 and the worst case stays small. The
// snapshot is rebuilt lazily on the first Lookup after any structural
// mutation (AddMethod / RemoveMethod / SetParent), and rebuilds are
// serialized by `mu` using double-checked locking.
//
// Concurrency model:
//   - Readers (Lookup) do one atomic.Pointer.Load on snap. No mutex on
//     the dispatch hot path. Once published, snapshots are immutable.
//   - Writers take vt.mu, mutate vt.methods / vt.parent, and clear snap
//     by storing nil. The next Lookup will rebuild under mu.
//   - Rebuilds re-check snap == nil under mu (DCL) so concurrent misses
//     don't redundantly build the table.
//
// Note: The VTable struct is forward-declared in object.go to break
// the import cycle. This file contains the method implementations.

// vtSelectorHash is a small mixer for integer selector IDs. Selector IDs
// are dense small integers assigned by the symbol table, so plain modulo
// would cluster badly for sequential allocations; the multiply-and-shift
// step from Knuth's multiplicative hash spreads them out cheaply.
//
// Constant is 2^32 / phi (a 32-bit Fibonacci hashing constant).
func vtSelectorHash(sel int) uint32 {
	return uint32(sel) * 2654435761
}

// Lookup finds a method by selector ID using the flattened dispatch table.
// Returns nil if no method is found (triggers doesNotUnderstand:).
//
// Hot path: one atomic pointer load → Fibonacci hash → indexed load →
// tag compare → return. Lock-free.
func (vt *VTable) Lookup(selector int) Method {
	s := vt.snap.Load()
	if s == nil {
		s = vt.rebuildLocked()
		if s == nil {
			return nil
		}
	}
	idx := (vtSelectorHash(selector) >> s.shift) & s.mask
	for {
		e := &s.entries[idx]
		if e.method == nil {
			return nil
		}
		if e.selector == selector {
			return e.method
		}
		idx = (idx + 1) & s.mask
	}
}

// bitsForMask returns the number of trailing one-bits in `mask`.
// `mask` is always (size - 1) for a power-of-two `size`, so this is
// effectively log2(size). Used to take the high bits of the Fibonacci
// hash (the well-mixed ones) before masking down to the table size.
func bitsForMask(mask uint32) uint {
	n := uint(0)
	for mask != 0 {
		mask >>= 1
		n++
	}
	return n
}

// rebuildLocked acquires vt.mu, double-checks that snap is still nil,
// and (if so) builds and publishes a fresh snapshot. Returns the
// resulting snapshot (which may be nil if there are no methods anywhere
// in the chain).
//
// Called only from Lookup on cache miss. Mutators clear snap to nil but
// do not call this directly — they let the next Lookup pay the cost.
func (vt *VTable) rebuildLocked() *vtSnapshot {
	vt.mu.Lock()
	// Double-checked locking: another goroutine may have rebuilt while
	// we were waiting for the mutex.
	if s := vt.snap.Load(); s != nil {
		vt.mu.Unlock()
		return s
	}
	s := vt.buildSnapshot()
	vt.snap.Store(s)
	vt.mu.Unlock()
	return s
}

// buildSnapshot constructs a fresh dispatch snapshot from the current
// inheritance chain. Caller must hold vt.mu (so vt.methods and vt.parent
// are stable). Returns nil if the chain has no methods.
//
// Walks parent → child so child overrides win.
func (vt *VTable) buildSnapshot() *vtSnapshot {
	// Count distinct selectors reachable in the chain. We over-count if
	// a child overrides a parent (each contributes once), but that just
	// means a marginally larger table — correctness is unaffected.
	//
	// We do not lock parent vtables here. Reading parent.methods races
	// with concurrent AddMethod on the parent only if the parent class
	// is being mutated while a child is being dispatched against — a
	// pattern that is not currently safe in this VM regardless (the
	// child's snapshot would not pick up the parent change without a
	// child-side markDirty). Class-definition is conventionally a
	// quiescent operation w.r.t. that class hierarchy. The alternative
	// (locking every ancestor in order) would complicate teardown and
	// is not warranted by current usage.
	total := 0
	for v := vt; v != nil; v = v.parent {
		total += len(v.methods)
	}

	if total == 0 {
		return nil
	}

	// Size to the next power of two >= 2 * total (load factor <= 0.5).
	size := uint32(1)
	for size < uint32(total)*2 {
		size <<= 1
	}
	if size < 4 {
		size = 4
	}

	entries := make([]vtEntry, size)
	mask := size - 1
	shift := 32 - bitsForMask(mask)

	// Walk parent → child so child overrides win.
	// Collect chain bottom-up first.
	var chain []*VTable
	for v := vt; v != nil; v = v.parent {
		chain = append(chain, v)
	}
	for i := len(chain) - 1; i >= 0; i-- {
		for sel, m := range chain[i].methods {
			if m == nil {
				continue
			}
			idx := (vtSelectorHash(sel) >> shift) & mask
			for {
				e := &entries[idx]
				if e.method == nil || e.selector == sel {
					e.selector = sel
					e.method = m
					break
				}
				idx = (idx + 1) & mask
			}
		}
	}

	return &vtSnapshot{entries: entries, mask: mask, shift: shift}
}

// markDirty invalidates the published snapshot so the next Lookup
// triggers a rebuild. Caller need NOT hold vt.mu — this is a single
// atomic store.
func (vt *VTable) markDirty() {
	vt.snap.Store(nil)
}

// LookupLocal finds a method by selector ID in this vtable only.
// Does not check parent vtables.
//
// Reads vt.methods directly; takes vt.mu to be safe against concurrent
// AddMethod/RemoveMethod. This is not on the dispatch hot path.
func (vt *VTable) LookupLocal(selector int) Method {
	vt.mu.Lock()
	defer vt.mu.Unlock()
	if vt.methods == nil {
		return nil
	}
	return vt.methods[selector]
}

// AddMethod adds or replaces a method at the given selector ID.
func (vt *VTable) AddMethod(selector int, method Method) {
	vt.mu.Lock()
	if vt.methods == nil {
		vt.methods = make(map[int]Method)
	}
	vt.methods[selector] = method
	vt.mu.Unlock()
	vt.markDirty()
}

// RemoveMethod removes a method at the given selector ID.
func (vt *VTable) RemoveMethod(selector int) {
	vt.mu.Lock()
	if vt.methods == nil {
		vt.mu.Unlock()
		return
	}
	if _, ok := vt.methods[selector]; ok {
		delete(vt.methods, selector)
		vt.mu.Unlock()
		vt.markDirty()
		return
	}
	vt.mu.Unlock()
}

// HasMethod returns true if this vtable (not parents) has a method for selector.
func (vt *VTable) HasMethod(selector int) bool {
	return vt.LookupLocal(selector) != nil
}

// Parent returns the parent vtable (for inheritance).
func (vt *VTable) Parent() *VTable {
	return vt.parent
}

// SetParent sets the parent vtable.
func (vt *VTable) SetParent(parent *VTable) {
	vt.mu.Lock()
	vt.parent = parent
	vt.mu.Unlock()
	vt.markDirty()
}

// Class returns the class this vtable belongs to.
func (vt *VTable) Class() *Class {
	return vt.class
}

// MethodCount returns the number of methods defined locally in this vtable.
// (Inherited methods are not counted.)
func (vt *VTable) MethodCount() int {
	vt.mu.Lock()
	defer vt.mu.Unlock()
	return len(vt.methods)
}

// LocalMethods returns all methods defined locally in this vtable.
// Returns a copy keyed by selector ID. Does not include inherited methods.
func (vt *VTable) LocalMethods() map[int]Method {
	vt.mu.Lock()
	defer vt.mu.Unlock()
	result := make(map[int]Method, len(vt.methods))
	for sel, m := range vt.methods {
		if m != nil {
			result[sel] = m
		}
	}
	return result
}

// NewVTable creates a new vtable for a class.
func NewVTable(class *Class, parent *VTable) *VTable {
	return &VTable{
		class:  class,
		parent: parent,
	}
}

// NewVTableWithCapacity creates a new vtable for a class.
//
// The capacity hint is preserved as part of the public API but no longer
// affects layout — the underlying storage is a map and is grown as needed.
func NewVTableWithCapacity(class *Class, parent *VTable, capacity int) *VTable {
	vt := &VTable{
		class:  class,
		parent: parent,
	}
	if capacity > 0 {
		vt.methods = make(map[int]Method, capacity)
	}
	return vt
}
