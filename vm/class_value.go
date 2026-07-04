package vm

// ---------------------------------------------------------------------------
// Class Values: First-class NaN-boxed representation of Class objects
// ---------------------------------------------------------------------------
//
// A class value is a pointer-carrying heap Value (kindClassValue) whose pointer
// is the *Class itself — traced by the Go GC. Two class values for the same
// class are pointer-identical, so they compare equal without an id registry.

// isClassValue returns true if this value is a class value.
func isClassValue(v Value) bool {
	return v.ptr != nil && v.hi == kindClassValue
}

// ---------------------------------------------------------------------------
// Exported accessors
// ---------------------------------------------------------------------------

// IsClassValue returns true if v is a class value.
// This is a pure bit-check and does not require a VM reference.
func IsClassValue(v Value) bool {
	return isClassValue(v)
}
