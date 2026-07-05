package vm

import "unsafe"

// ---------------------------------------------------------------------------
// kindExtension: the shared heap representation for contrib/external-IO types
// ---------------------------------------------------------------------------
//
// The many wrapper types that bridge Go libraries into Maggie — HTTP
// client/server/request/response, Unix sockets, JSON streaming, SSE, cobra CLI,
// external OS processes, gRPC client/stream, CUE context/value, TupleSpace,
// ConstraintStore, … — used to be symbol-encoded registry ids, one AutoIDRegistry
// per marker. In the pointer-Value world they all share a single heap kind,
// kindExtension, and carry an extensionObject holding the concrete Go pointer
// plus the marker that discriminates its type.
//
// The marker reuses the existing NaN-box marker constants (markers.go). Each
// primitive set still registers a marker→class entry in symbolDispatch; that
// same table now drives class-of and method dispatch for the heap Value via
// classForHeap's kindExtension case (see vm.go). Keeping one shared kind avoids
// bloating the heapKind enum and classForHeap with ~20 near-identical cases.
type extensionObject struct {
	marker uint32
	obj    any
}

// makeExtensionValue wraps a Go object as a pointer-carrying extension Value.
// Because the returned Value keeps *extensionObject (and through it obj)
// reachable, Go's GC traces the wrapped object for as long as the Value lives —
// no registry, no id, no custom sweep.
func makeExtensionValue(marker uint32, obj any) Value {
	return makeHeap(kindExtension, unsafe.Pointer(&extensionObject{marker: marker, obj: obj}))
}

// extensionObjectOf returns the wrapper behind an extension Value, or nil if v
// is not a kindExtension heap Value.
func (v Value) extensionObjectOf() *extensionObject {
	if v.ptr == nil || v.hi != kindExtension {
		return nil
	}
	return (*extensionObject)(v.ptr)
}

// isExtensionValue reports whether v is an extension Value with the given marker.
func isExtensionValue(v Value, marker uint32) bool {
	e := v.extensionObjectOf()
	return e != nil && e.marker == marker
}

// extensionMarker returns the marker of an extension Value, or 0 if v is not one.
func extensionMarker(v Value) uint32 {
	if e := v.extensionObjectOf(); e != nil {
		return e.marker
	}
	return 0
}

// ---------------------------------------------------------------------------
// Exported API for contrib packages (vm/contrib/*)
// ---------------------------------------------------------------------------
//
// Contrib packages live outside the vm package and cannot touch the unexported
// Value fields or kindExtension. These thin wrappers give them the same
// pointer-carrying representation the in-package IO types use, so a contrib
// value is a real GC-traced heap Value rather than a registry id.

// NewExtensionValue wraps a Go object behind kindExtension with the given marker.
func NewExtensionValue(marker uint32, obj any) Value { return makeExtensionValue(marker, obj) }

// ExtensionObject returns the wrapped Go object if v is an extension Value with
// the given marker, else nil. Callers type-assert the result to their concrete
// wrapper type.
func ExtensionObject(v Value, marker uint32) any {
	e := v.extensionObjectOf()
	if e == nil || e.marker != marker {
		return nil
	}
	return e.obj
}

// IsExtensionValue is the exported form of isExtensionValue.
func IsExtensionValue(v Value, marker uint32) bool { return isExtensionValue(v, marker) }
