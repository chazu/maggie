package vm

import (
	"fmt"
	"sync"
	"sync/atomic"
)

// TypedRegistry is a generic, thread-safe registry for mapping keys to values.
// It handles storage and locking only; ID allocation remains with the caller.
type TypedRegistry[K comparable, V any] struct {
	data map[K]V
	mu   sync.RWMutex
}

// NewTypedRegistry creates an empty TypedRegistry.
func NewTypedRegistry[K comparable, V any]() *TypedRegistry[K, V] {
	return &TypedRegistry[K, V]{data: make(map[K]V)}
}

// Put stores a value under the given key.
func (r *TypedRegistry[K, V]) Put(key K, val V) {
	r.mu.Lock()
	r.data[key] = val
	r.mu.Unlock()
}

// Get retrieves a value by key. Returns the zero value if not found.
func (r *TypedRegistry[K, V]) Get(key K) V {
	r.mu.RLock()
	v := r.data[key]
	r.mu.RUnlock()
	return v
}

// Delete removes a key from the registry.
func (r *TypedRegistry[K, V]) Delete(key K) {
	r.mu.Lock()
	delete(r.data, key)
	r.mu.Unlock()
}

// Has returns true if the key exists.
func (r *TypedRegistry[K, V]) Has(key K) bool {
	r.mu.RLock()
	_, ok := r.data[key]
	r.mu.RUnlock()
	return ok
}

// Count returns the number of entries.
func (r *TypedRegistry[K, V]) Count() int {
	r.mu.RLock()
	n := len(r.data)
	r.mu.RUnlock()
	return n
}

// ForEach calls fn for each entry while holding a read lock.
// Do not call other TypedRegistry methods from fn (deadlock risk).
func (r *TypedRegistry[K, V]) ForEach(fn func(K, V)) {
	r.mu.RLock()
	defer r.mu.RUnlock()
	for k, v := range r.data {
		fn(k, v)
	}
}

// Sweep removes entries for which keep returns false.
// Returns the number of entries removed.
func (r *TypedRegistry[K, V]) Sweep(keep func(K, V) bool) int {
	r.mu.Lock()
	defer r.mu.Unlock()
	swept := 0
	for k, v := range r.data {
		if !keep(k, v) {
			delete(r.data, k)
			swept++
		}
	}
	return swept
}

// Clear removes all entries.
func (r *TypedRegistry[K, V]) Clear() {
	r.mu.Lock()
	r.data = make(map[K]V)
	r.mu.Unlock()
}

// RLock acquires a read lock. Caller must call RUnlock.
// Use with UnsafeGet for multi-lookup operations under a single lock.
func (r *TypedRegistry[K, V]) RLock() {
	r.mu.RLock()
}

// RUnlock releases the read lock acquired by RLock.
func (r *TypedRegistry[K, V]) RUnlock() {
	r.mu.RUnlock()
}

// UnsafeGet retrieves a value without acquiring any lock.
// Caller must hold at least a read lock via RLock.
func (r *TypedRegistry[K, V]) UnsafeGet(key K) V {
	return r.data[key]
}

// ---------------------------------------------------------------------------
// AutoIDRegistry: TypedRegistry + auto-incrementing uint32 ID allocation
// ---------------------------------------------------------------------------

// defaultAutoIDMax is the conservative ceiling for AutoIDRegistry: the
// 24-bit symbol-payload limit. Most NaN-boxed kinds carry their type tag in
// bits 24-31, leaving 24 bits (0..2^24-1) for the registry ID. Registries
// that span a wider range (string/dictionary, which use the high payload bit
// as their discriminator) override this via WithMaxID.
const defaultAutoIDMax uint32 = (1 << 24) - 1

// AutoIDRegistry bundles a TypedRegistry[uint32, V] with an atomic ID counter
// and a free-list of recycled IDs. Deleted IDs are returned to the free-list
// and reissued by subsequent Register calls — long-running programs no longer
// exhaust the ID space when entries are routinely deleted.
//
// Register panics on actual exhaustion (free-list empty AND counter past
// maxID). Silent wrap-around is never permitted: a wrap would alias a live
// entry's ID to a new entry and corrupt the registry.
type AutoIDRegistry[V any] struct {
	TypedRegistry[uint32, V]

	// nextID is the next never-yet-allocated ID. It only moves forward.
	// Access via atomic ops in the fast path; mutated under freeMu in the
	// allocation slow path so it stays consistent with maxID checks.
	nextID atomic.Uint32

	// freeMu guards freeList. It is also taken when allocating from nextID
	// past the free-list, to keep the (free-list-empty + bump) sequence
	// atomic relative to concurrent Delete calls.
	freeMu   sync.Mutex
	freeList []uint32

	// startID is the initial ID; freed IDs below startID are ignored
	// (defensive — should never happen in practice).
	startID uint32

	// maxID is the inclusive upper bound on issued IDs.
	maxID uint32

	// monotonic disables ID reuse. When true, Delete does not push to
	// the free-list. Use for registries whose values cache their ID
	// (e.g. *Class.classValueID) or where external systems may hold
	// references that outlive registry membership.
	monotonic bool

	// name labels the registry in panic messages.
	name string

	// liveSize is an atomically maintained count of live entries. It mirrors
	// len(data) but is readable without taking r.mu. Used by the pressure
	// hook to make sub-microsecond decisions on every Register without
	// touching the registry's lock.
	liveSize atomic.Int32

	// pressureHook is consulted on every Register (and after Sweep) when
	// non-nil. It receives the new liveSize and decides whether to wake the
	// RegistryGC. Stored via atomic.Pointer so install/uninstall is race-free
	// without a lock. Nil is the steady state for tests that don't run a GC.
	pressureHook atomic.Pointer[func(liveSize int32)]
}

// AutoIDOption configures an AutoIDRegistry at construction time.
type AutoIDOption func(*autoIDConfig)

type autoIDConfig struct {
	maxID     uint32
	monotonic bool
	name      string
}

// WithMaxID overrides the default 24-bit ceiling. Pass the largest valid ID
// (inclusive). Register panics when an allocation would exceed this.
func WithMaxID(max uint32) AutoIDOption {
	return func(c *autoIDConfig) { c.maxID = max }
}

// WithMonotonic disables ID reuse. Deleted IDs are not recycled.
// Use for registries whose values cache their assigned ID, or where IDs
// double as identity tokens that must never be reissued.
func WithMonotonic() AutoIDOption {
	return func(c *autoIDConfig) { c.monotonic = true }
}

// WithName labels the registry for panic messages on exhaustion.
func WithName(name string) AutoIDOption {
	return func(c *autoIDConfig) { c.name = name }
}

// NewAutoIDRegistry creates an AutoIDRegistry with the given starting ID.
// Use startID=1 for most domains (0 is reserved as nil/uninitialized).
// Use larger offsets for domains that share the symbol ID space (e.g., strings).
func NewAutoIDRegistry[V any](startID uint32, opts ...AutoIDOption) *AutoIDRegistry[V] {
	cfg := autoIDConfig{maxID: defaultAutoIDMax}
	for _, opt := range opts {
		opt(&cfg)
	}
	if cfg.name == "" {
		cfg.name = "AutoIDRegistry"
	}
	if cfg.maxID < startID {
		// Mis-configuration: refuse to create a registry that can never
		// allocate. Better to fail loud at construction.
		panic(fmt.Sprintf("AutoIDRegistry %q: maxID (%d) < startID (%d)", cfg.name, cfg.maxID, startID))
	}
	r := &AutoIDRegistry[V]{
		TypedRegistry: TypedRegistry[uint32, V]{data: make(map[uint32]V)},
		startID:       startID,
		maxID:         cfg.maxID,
		monotonic:     cfg.monotonic,
		name:          cfg.name,
	}
	r.nextID.Store(startID)
	return r
}

// Register stores a value and returns its auto-assigned ID. IDs from the
// free-list are reissued in LIFO order; otherwise a fresh ID is bumped from
// the monotonic counter. Panics if both sources are exhausted.
//
// After storing, Register notifies the pressure hook if one is installed.
// The hook's overhead in the no-hook case is a single atomic-pointer load
// and a nil branch; in the steady-state with a hook it adds two atomic
// loads and a compare. See RegistryGC for how this drives pressure-based
// sweeps.
func (r *AutoIDRegistry[V]) Register(v V) uint32 {
	id, ok := r.allocID()
	if !ok {
		panic(fmt.Sprintf("AutoIDRegistry %q: ID space exhausted (maxID=%d)", r.name, r.maxID))
	}
	r.Put(id, v)
	newSize := r.liveSize.Add(1)
	if hook := r.pressureHook.Load(); hook != nil {
		(*hook)(newSize)
	}
	return id
}

// allocID returns the next ID to issue. It checks the free-list first; if
// empty, it bumps nextID, checking against maxID. Returns (id, false) when
// no ID is available.
//
// freeMu is held during the bump path so a concurrent Delete cannot insert
// into the free-list between our "free-list empty" check and the bump that
// would otherwise needlessly push the counter past maxID.
func (r *AutoIDRegistry[V]) allocID() (uint32, bool) {
	r.freeMu.Lock()
	if n := len(r.freeList); n > 0 {
		id := r.freeList[n-1]
		r.freeList = r.freeList[:n-1]
		r.freeMu.Unlock()
		return id, true
	}
	// Slow path: bump the counter under the lock so the (read-current,
	// check-max, store-next) sequence is atomic. AutoIDRegistry is not a
	// hot-enough path to justify lock-free here; channels/process IDs go
	// through ConcurrencyRegistry, not this code.
	cur := r.nextID.Load()
	if cur > r.maxID {
		r.freeMu.Unlock()
		return 0, false
	}
	r.nextID.Store(cur + 1)
	r.freeMu.Unlock()
	return cur, true
}

// Delete removes a key and (unless the registry is monotonic) returns its
// ID to the free-list for reuse. Overrides TypedRegistry.Delete so callers
// of the existing API automatically get ID recycling.
func (r *AutoIDRegistry[V]) Delete(key uint32) {
	// Remove from the underlying map first. Order matters: Get-after-Delete
	// must see the entry gone before the ID is potentially reissued.
	r.mu.Lock()
	if _, ok := r.data[key]; ok {
		delete(r.data, key)
		r.liveSize.Add(-1)
	}
	r.mu.Unlock()
	if r.monotonic {
		return
	}
	if key < r.startID || key > r.maxID {
		// Defensive: never push out-of-range IDs onto the free-list.
		return
	}
	r.freeMu.Lock()
	r.freeList = append(r.freeList, key)
	r.freeMu.Unlock()
}

// Sweep removes entries for which keep returns false and recycles their IDs
// (unless monotonic). Overrides TypedRegistry.Sweep so the standard API
// continues to work.
func (r *AutoIDRegistry[V]) Sweep(keep func(uint32, V) bool) int {
	// Collect doomed keys under the registry's write lock, then delete via
	// our own Delete to feed the free-list. We can't hold both r.mu and
	// r.freeMu at once without imposing a lock ordering elsewhere, so do
	// it in two passes — Sweep is a cold operation.
	doomed := make([]uint32, 0)
	r.mu.Lock()
	for k, v := range r.data {
		if !keep(k, v) {
			doomed = append(doomed, k)
			delete(r.data, k)
		}
	}
	r.mu.Unlock()
	if n := len(doomed); n > 0 {
		r.liveSize.Add(int32(-n))
	}

	if !r.monotonic {
		r.freeMu.Lock()
		for _, k := range doomed {
			if k >= r.startID && k <= r.maxID {
				r.freeList = append(r.freeList, k)
			}
		}
		r.freeMu.Unlock()
	}
	return len(doomed)
}

// LiveSize returns the current live-entry count via a single atomic load.
// Cheaper than Count() (no RLock); the value is eventually consistent with
// the registry's actual size and may briefly diverge under concurrent
// Register/Delete races, but is exact at quiescence.
func (r *AutoIDRegistry[V]) LiveSize() int32 {
	return r.liveSize.Load()
}

// SetPressureHook installs (or clears, if hook==nil) a callback invoked on
// every successful Register with the new live-size. The callback runs on
// the calling goroutine and MUST be cheap and non-blocking — typical use is
// a non-blocking channel send. Calling SetPressureHook concurrently with
// Register is safe; the previous hook is replaced atomically and may run
// once more from in-flight Register calls.
func (r *AutoIDRegistry[V]) SetPressureHook(hook func(liveSize int32)) {
	if hook == nil {
		r.pressureHook.Store(nil)
		return
	}
	r.pressureHook.Store(&hook)
}

// MaxID returns the inclusive ID ceiling. Useful for GC threshold calc.
func (r *AutoIDRegistry[V]) MaxID() uint32 { return r.maxID }

// Name returns the registry's name (for diagnostics / GC stats).
func (r *AutoIDRegistry[V]) Name() string { return r.name }

// FreeListLen returns the current free-list size. Intended for tests and
// diagnostics.
func (r *AutoIDRegistry[V]) FreeListLen() int {
	r.freeMu.Lock()
	n := len(r.freeList)
	r.freeMu.Unlock()
	return n
}
