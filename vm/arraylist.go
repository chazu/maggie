package vm

// ---------------------------------------------------------------------------
// ArrayList: Growable array backed by a Go []Value slice
// ---------------------------------------------------------------------------

// ArrayListObject is a growable, ordered collection of Values.
// It wraps a Go slice to provide amortized O(1) append via Go's built-in
// slice growth strategy, O(1) indexed access, and O(1) removeLast.
type ArrayListObject struct {
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

func arrayListToValue(id int) Value {
	return FromSymbolID(uint32(id) | arrayListMarker)
}

func isArrayListValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return (id & markerMask) == arrayListMarker
}

// Add appends an element. Amortized O(1).
func (al *ArrayListObject) Add(v Value) {
	al.elements = append(al.elements, v)
}

// At returns the element at index i, or Nil if out of bounds.
func (al *ArrayListObject) At(i int) Value {
	if i < 0 || i >= len(al.elements) {
		return Nil
	}
	return al.elements[i]
}

// AtPut sets the element at index i. Returns false if out of bounds.
func (al *ArrayListObject) AtPut(i int, v Value) bool {
	if i < 0 || i >= len(al.elements) {
		return false
	}
	al.elements[i] = v
	return true
}

// Size returns the number of elements.
func (al *ArrayListObject) Size() int {
	return len(al.elements)
}

// Capacity returns the current backing slice capacity.
func (al *ArrayListObject) Capacity() int {
	return cap(al.elements)
}

// RemoveLast removes and returns the last element. Returns Nil if empty.
func (al *ArrayListObject) RemoveLast() Value {
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
	for i := range al.elements {
		al.elements[i] = Nil // help GC
	}
	al.elements = al.elements[:0]
}

// Elements returns the backing slice (read-only use).
func (al *ArrayListObject) Elements() []Value {
	return al.elements
}
