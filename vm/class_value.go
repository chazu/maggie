package vm

// ---------------------------------------------------------------------------
// Class Values: First-class NaN-boxed representation of Class objects
// ---------------------------------------------------------------------------
//
// Class values use the symbol tag with a dedicated marker (36 << 24) to
// distinguish them from regular symbols and other symbol-encoded values
// (channels, processes, etc.).
//
// The lower 24 bits store the class's registration ID, which maps to a
// *Class pointer in the VM-local class value registry (ObjectRegistry).

// classToValue creates a Value from a class registry ID.
func classToValue(id int) Value {
	return FromSymbolID(uint32(id) | classValueMarker)
}

// isClassValue returns true if this value is a class value.
func isClassValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return (id & markerMask) == classValueMarker
}

// classValueIDFromValue extracts the registry ID from a class value.
func classValueIDFromValue(v Value) int {
	return int(v.SymbolID() & ^markerMask)
}

// ---------------------------------------------------------------------------
// Exported accessors
// ---------------------------------------------------------------------------

// IsClassValue returns true if v is a class value.
// This is a pure bit-check and does not require a VM reference.
func IsClassValue(v Value) bool {
	return isClassValue(v)
}
