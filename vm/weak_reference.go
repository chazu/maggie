package vm

import (
	"sync"
)

// ---------------------------------------------------------------------------
// WeakReference: A reference that doesn't prevent garbage collection
// ---------------------------------------------------------------------------

// WeakReference holds a weak reference to an object.
// When the target object is collected by GC, the reference becomes nil.
// Optionally supports finalization callbacks.
type WeakReference struct {
	id       uint32       // Unique ID for Value encoding
	target   *Object      // The weakly-referenced object (nil if collected)
	finalizer func(Value) // Optional callback when target is collected
	mu       sync.RWMutex // Protects target and finalizer
}

// NewWeakReference creates a new weak reference to the given object.
// The registry is used to generate a unique ID; if nil, a zero ID is assigned
// (suitable only for tests that don't need globally unique IDs).
func NewWeakReference(registry *ObjectRegistry, target *Object) *WeakReference {
	var id uint32
	if registry != nil {
		id = registry.NextWeakRefID()
	}
	return &WeakReference{
		id:     id,
		target: target,
	}
}

// ID returns the unique identifier for this weak reference.
func (wr *WeakReference) ID() uint32 {
	return wr.id
}

// Get returns the target object, or nil if it has been collected.
func (wr *WeakReference) Get() *Object {
	wr.mu.RLock()
	defer wr.mu.RUnlock()
	return wr.target
}

// IsAlive returns true if the target object has not been collected.
func (wr *WeakReference) IsAlive() bool {
	wr.mu.RLock()
	defer wr.mu.RUnlock()
	return wr.target != nil
}

// Clear clears the weak reference (called by GC when target is collected).
// Returns the old target for finalization purposes.
func (wr *WeakReference) Clear() *Object {
	wr.mu.Lock()
	defer wr.mu.Unlock()
	old := wr.target
	wr.target = nil
	return old
}

// SetFinalizer sets a callback to be invoked when the target is collected.
// The callback receives the Value that was referenced (for informational purposes).
func (wr *WeakReference) SetFinalizer(fn func(Value)) {
	wr.mu.Lock()
	defer wr.mu.Unlock()
	wr.finalizer = fn
}

// Finalizer returns the finalization callback, if any.
func (wr *WeakReference) Finalizer() func(Value) {
	wr.mu.RLock()
	defer wr.mu.RUnlock()
	return wr.finalizer
}

// ---------------------------------------------------------------------------
// WeakRegistry: Tracks all weak references in the VM
// ---------------------------------------------------------------------------

// WeakRegistry manages all weak references in the VM.
// It integrates with garbage collection to clear references to collected objects.
type WeakRegistry struct {
	refs map[uint32]*WeakReference // ID -> WeakReference
	mu   sync.RWMutex
}

// NewWeakRegistry creates a new weak reference registry.
func NewWeakRegistry() *WeakRegistry {
	return &WeakRegistry{
		refs: make(map[uint32]*WeakReference),
	}
}

// Register adds a weak reference to the registry.
func (r *WeakRegistry) Register(wr *WeakReference) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.refs[wr.id] = wr
}

// Unregister removes a weak reference from the registry.
func (r *WeakRegistry) Unregister(wr *WeakReference) {
	r.mu.Lock()
	defer r.mu.Unlock()
	delete(r.refs, wr.id)
}

// Lookup finds a weak reference by ID.
func (r *WeakRegistry) Lookup(id uint32) *WeakReference {
	r.mu.RLock()
	defer r.mu.RUnlock()
	return r.refs[id]
}

// ProcessGC is called during garbage collection.
// It clears weak references to unmarked (collected) objects and invokes finalizers.
// Returns the number of weak references that were cleared.
func (r *WeakRegistry) ProcessGC(marked map[*Object]struct{}) int {
	r.mu.Lock()
	defer r.mu.Unlock()

	cleared := 0
	var toFinalize []struct {
		wr     *WeakReference
		target *Object
	}

	// Find weak refs pointing to unmarked objects
	for _, wr := range r.refs {
		wr.mu.RLock()
		target := wr.target
		wr.mu.RUnlock()

		if target != nil {
			if _, isMarked := marked[target]; !isMarked {
				// Target is being collected
				toFinalize = append(toFinalize, struct {
					wr     *WeakReference
					target *Object
				}{wr, target})
			}
		}
	}

	// Clear the weak refs and prepare finalizers
	// (done separately to avoid holding locks during finalizer calls)
	for _, item := range toFinalize {
		item.wr.Clear()
		cleared++
	}

	// Run finalizers outside the main lock
	// Note: finalizers run with the target already collected,
	// so they receive the Value for informational purposes only
	r.mu.Unlock()
	for _, item := range toFinalize {
		if fn := item.wr.Finalizer(); fn != nil {
			// Pass the original Value (though the object is now gone)
			fn(item.target.ToValue())
		}
	}
	r.mu.Lock()

	return cleared
}

// Count returns the number of registered weak references.
func (r *WeakRegistry) Count() int {
	r.mu.RLock()
	defer r.mu.RUnlock()
	return len(r.refs)
}

// ---------------------------------------------------------------------------
// Value encoding for WeakReference
// ---------------------------------------------------------------------------

// WeakReference values are encoded using the symbol tag with a special marker.
// Format: symbol tag | (weakRefMarker << 24) | weakRefID
const weakRefMarker uint32 = 16 << 24 // Marker bits for weak references

// FromWeakRef creates a Value from a WeakReference.
func FromWeakRef(wr *WeakReference) Value {
	if wr == nil {
		return Nil
	}
	// Encode as symbol with weak reference marker
	id := weakRefMarker | wr.id
	return FromSymbolID(id)
}

// IsWeakRef returns true if v represents a weak reference.
func (v Value) IsWeakRef() bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return (id & (0xFF << 24)) == weakRefMarker
}

// WeakRefID extracts the weak reference ID from a Value.
// Panics if v is not a weak reference.
func (v Value) WeakRefID() uint32 {
	if !v.IsWeakRef() {
		panic("Value.WeakRefID: not a weak reference")
	}
	return v.SymbolID() & 0x00FFFFFF
}
