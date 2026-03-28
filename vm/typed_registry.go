package vm

import (
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

// AutoIDRegistry bundles a TypedRegistry[uint32, V] with an atomic ID counter.
// It collapses the Register/Get/Delete/Count boilerplate that repeats across
// every domain in ObjectRegistry.
type AutoIDRegistry[V any] struct {
	TypedRegistry[uint32, V]
	nextID atomic.Uint32
}

// NewAutoIDRegistry creates an AutoIDRegistry with the given starting ID.
// Use startID=1 for most domains (0 is reserved as nil/uninitialized).
// Use larger offsets for domains that share the symbol ID space (e.g., strings).
func NewAutoIDRegistry[V any](startID uint32) *AutoIDRegistry[V] {
	r := &AutoIDRegistry[V]{
		TypedRegistry: TypedRegistry[uint32, V]{data: make(map[uint32]V)},
	}
	r.nextID.Store(startID)
	return r
}

// Register stores a value and returns its auto-assigned ID.
func (r *AutoIDRegistry[V]) Register(v V) uint32 {
	id := r.nextID.Add(1) - 1
	r.Put(id, v)
	return id
}
