package vm

import "sync"

// ---------------------------------------------------------------------------
// ArrayList: Growable array backed by a Go []Value slice
// ---------------------------------------------------------------------------

// ArrayListObject is a growable, ordered collection of Values.
// It wraps a Go slice to provide amortized O(1) append via Go's built-in
// slice growth strategy, O(1) indexed access, and O(1) removeLast.
//
// All access goes through the locked methods below: ArrayLists stored in
// globals are reachable from concurrent server requests (RunIsolated
// interpreters) and forked processes, and racing appends corrupt the slice
// header. Iteration uses Snapshot so callers never hold the lock while
// running Maggie code (blocks).
type ArrayListObject struct {
	mu       sync.RWMutex
	elements []Value
}

func createArrayList(initialCap int) *ArrayListObject {
	if initialCap < 0 {
		initialCap = 0
	}
	return &ArrayListObject{
		elements: make([]Value, 0, initialCap),
	}
}

func isArrayListValue(v Value) bool {
	return v.ptr != nil && v.hi == kindArrayList
}

// Add appends an element. Amortized O(1).
func (al *ArrayListObject) Add(v Value) {
	al.mu.Lock()
	al.elements = append(al.elements, v)
	al.mu.Unlock()
}

// At returns the element at index i, or Nil if out of bounds.
func (al *ArrayListObject) At(i int) Value {
	al.mu.RLock()
	defer al.mu.RUnlock()
	if i < 0 || i >= len(al.elements) {
		return Nil
	}
	return al.elements[i]
}

// AtPut sets the element at index i. Returns false if out of bounds.
func (al *ArrayListObject) AtPut(i int, v Value) bool {
	al.mu.Lock()
	defer al.mu.Unlock()
	if i < 0 || i >= len(al.elements) {
		return false
	}
	al.elements[i] = v
	return true
}

// Size returns the number of elements.
func (al *ArrayListObject) Size() int {
	al.mu.RLock()
	n := len(al.elements)
	al.mu.RUnlock()
	return n
}

// Capacity returns the current backing slice capacity.
func (al *ArrayListObject) Capacity() int {
	al.mu.RLock()
	c := cap(al.elements)
	al.mu.RUnlock()
	return c
}

// RemoveLast removes and returns the last element. Returns Nil if empty.
func (al *ArrayListObject) RemoveLast() Value {
	al.mu.Lock()
	defer al.mu.Unlock()
	n := len(al.elements)
	if n == 0 {
		return Nil
	}
	v := al.elements[n-1]
	al.elements[n-1] = Nil // help GC
	al.elements = al.elements[:n-1]
	return v
}

// RemoveAt removes the element at index i, shifting remaining elements.
// Returns Nil if out of bounds.
func (al *ArrayListObject) RemoveAt(i int) Value {
	al.mu.Lock()
	defer al.mu.Unlock()
	n := len(al.elements)
	if i < 0 || i >= n {
		return Nil
	}
	v := al.elements[i]
	copy(al.elements[i:], al.elements[i+1:])
	al.elements[n-1] = Nil // help GC
	al.elements = al.elements[:n-1]
	return v
}

// Clear removes all elements but keeps the backing capacity.
func (al *ArrayListObject) Clear() {
	al.mu.Lock()
	for i := range al.elements {
		al.elements[i] = Nil // help GC
	}
	al.elements = al.elements[:0]
	al.mu.Unlock()
}

// Snapshot returns a copy of the elements. Iterate the copy instead of the
// live slice so Maggie blocks can run (and even mutate the receiver)
// without holding the lock.
func (al *ArrayListObject) Snapshot() []Value {
	al.mu.RLock()
	out := make([]Value, len(al.elements))
	copy(out, al.elements)
	al.mu.RUnlock()
	return out
}

// AppendSlice appends all values in order.
func (al *ArrayListObject) AppendSlice(vs []Value) {
	al.mu.Lock()
	al.elements = append(al.elements, vs...)
	al.mu.Unlock()
}

// ReplaceAll replaces the entire contents (used by in-place sort).
func (al *ArrayListObject) ReplaceAll(vs []Value) {
	al.mu.Lock()
	al.elements = al.elements[:0]
	al.elements = append(al.elements, vs...)
	al.mu.Unlock()
}

// Elements returns a copy of the backing slice.
func (al *ArrayListObject) Elements() []Value {
	return al.Snapshot()
}
