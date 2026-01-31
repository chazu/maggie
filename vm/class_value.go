package vm

import (
	"sync"
	"sync/atomic"
)

// ---------------------------------------------------------------------------
// Class Values: First-class NaN-boxed representation of Class objects
// ---------------------------------------------------------------------------
//
// Class values use the symbol tag with a dedicated marker (36 << 24) to
// distinguish them from regular symbols and other symbol-encoded values
// (channels, processes, etc.).
//
// The lower 24 bits store the class's registration ID, which maps to a
// *Class pointer in the global class value registry.

const classValueMarker uint32 = 36 << 24

// Global class value registry
var (
	classValueRegistry   = make(map[int]*Class)
	classValueRegistryMu sync.RWMutex
	nextClassValueID     atomic.Int32
)

func init() {
	nextClassValueID.Store(1)
}

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
	return (id & (0xFF << 24)) == classValueMarker
}

// getClassFromValue extracts the Class from a class value.
// Returns nil if the value is not a class value or the class is not found.
func getClassFromValue(v Value) *Class {
	if !isClassValue(v) {
		return nil
	}
	id := int(v.SymbolID() & ^uint32(0xFF<<24))

	classValueRegistryMu.RLock()
	defer classValueRegistryMu.RUnlock()
	return classValueRegistry[id]
}

// registerClassValue registers a class in the class value registry and returns
// its Value representation. This is idempotent — if the class already has a
// registered ID (classValueID != 0), the existing value is returned.
func registerClassValue(c *Class) Value {
	if c == nil {
		return Nil
	}

	// Fast path: already registered
	if c.classValueID != 0 {
		return classToValue(c.classValueID)
	}

	// Slow path: register new
	id := int(nextClassValueID.Add(1) - 1)

	classValueRegistryMu.Lock()
	classValueRegistry[id] = c
	classValueRegistryMu.Unlock()

	c.classValueID = id
	return classToValue(id)
}

// ---------------------------------------------------------------------------
// Exported accessors for use in server/ package
// ---------------------------------------------------------------------------

// IsClassValue returns true if v is a class value.
func IsClassValue(v Value) bool {
	return isClassValue(v)
}

// GetClassFromValue extracts the *Class from a class value.
func GetClassFromValue(v Value) *Class {
	return getClassFromValue(v)
}
