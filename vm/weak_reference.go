package vm

import (
	"runtime"
	"sync"
	"sync/atomic"
	"unsafe"
	"weak"
)

// ---------------------------------------------------------------------------
// WeakReference: a reference that doesn't prevent garbage collection
// ---------------------------------------------------------------------------
//
// A WeakReference holds a Go-native weak pointer to a Maggie *Object. When the
// object becomes unreachable through strong references, Go's GC reclaims it and
// Get() begins returning nil — no custom collector, registry, or ProcessGC pass
// is involved. Optional finalization is wired through runtime.AddCleanup on the
// target object.
type WeakReference struct {
	ptr     weak.Pointer[Object] // Go-native weak pointer to the target
	cleared atomic.Bool          // set by an explicit Clear() (makes Get return nil)

	mu               sync.Mutex  // guards finalizer and cleanupScheduled
	finalizer        func(Value) // optional callback when the target is collected
	cleanupScheduled bool        // runtime cleanup already registered on the target
}

// NewWeakReference creates a new weak reference to the given object.
func NewWeakReference(target *Object) *WeakReference {
	return &WeakReference{ptr: weak.Make(target)}
}

// Get returns the target object, or nil if it has been collected or explicitly
// cleared.
func (wr *WeakReference) Get() *Object {
	if wr.cleared.Load() {
		return nil
	}
	return wr.ptr.Value()
}

// IsAlive returns true if the target object has not been collected or cleared.
func (wr *WeakReference) IsAlive() bool {
	return wr.Get() != nil
}

// Clear explicitly clears the weak reference (makes Get return nil). Returns the
// old target for finalization purposes.
func (wr *WeakReference) Clear() *Object {
	old := wr.ptr.Value()
	wr.cleared.Store(true)
	return old
}

// SetFinalizer sets a callback invoked when the target is collected by Go's GC.
// It is wired through runtime.AddCleanup on the (still-live) target; the callback
// receives Nil, since the object is already gone by the time it runs. If the
// target is already collected, no cleanup is scheduled.
//
// Setting a finalizer again replaces the callback; the cleanup is registered
// only once, so the target's collection runs the latest callback exactly once.
func (wr *WeakReference) SetFinalizer(fn func(Value)) {
	wr.mu.Lock()
	wr.finalizer = fn
	obj := wr.ptr.Value()
	if wr.cleanupScheduled || obj == nil {
		wr.mu.Unlock()
		return
	}
	wr.cleanupScheduled = true
	wr.mu.Unlock()
	// The cleanup must NOT capture obj (that would keep it alive). It captures
	// only wr, reading the finalizer under the lock at collection time.
	runtime.AddCleanup(obj, func(struct{}) {
		wr.mu.Lock()
		f := wr.finalizer
		wr.mu.Unlock()
		if f != nil {
			f(Nil)
		}
	}, struct{}{})
}

// Finalizer returns the finalization callback, if any.
func (wr *WeakReference) Finalizer() func(Value) {
	wr.mu.Lock()
	defer wr.mu.Unlock()
	return wr.finalizer
}

// ---------------------------------------------------------------------------
// Value encoding: WeakReference is a pointer-carrying kindWeakRef heap Value.
// ---------------------------------------------------------------------------

// FromWeakRef creates a Value from a WeakReference.
func FromWeakRef(wr *WeakReference) Value {
	if wr == nil {
		return Nil
	}
	return makeHeap(kindWeakRef, unsafe.Pointer(wr))
}

// IsWeakRef returns true if v represents a weak reference.
func (v Value) IsWeakRef() bool {
	return v.ptr != nil && v.hi == kindWeakRef
}

// weakRefFromValue returns the WeakReference behind v, or nil.
func weakRefFromValue(v Value) *WeakReference {
	if !v.IsWeakRef() {
		return nil
	}
	return (*WeakReference)(v.ptr)
}
