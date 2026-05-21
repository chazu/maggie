package vm

import "github.com/fxamacker/cbor/v2"

// Exported API for contrib plugins. These wrap unexported VM methods and fields
// so that vm/contrib/* packages can register classes, primitives, and symbol
// dispatch entries without being in the vm package.

// CreateClass creates a new class with the given name and superclass, registers
// it in the class table, and returns it.
func (vm *VM) CreateClass(name string, superclass *Class) *Class {
	return vm.createClass(name, superclass)
}

// CreateClassWithIvars creates a new class with instance variables.
func (vm *VM) CreateClassWithIvars(name string, superclass *Class, ivars []string) *Class {
	return vm.createClassWithIvars(name, superclass, ivars)
}

// RegisterSymbolDispatchEntry registers a marker→class mapping so that
// the VM knows which class to use for NaN-boxed values with the given marker.
func (vm *VM) RegisterSymbolDispatchEntry(marker uint32, entry *SymbolTypeEntry) {
	vm.symbolDispatch.Register(marker, entry)
}

// ValueToString extracts a Go string from a String or Symbol value.
func (vm *VM) ValueToString(v Value) string {
	return vm.valueToString(v)
}

// GetCancellationContext returns the CancellationContextObject for a value.
func (vm *VM) GetCancellationContext(v Value) *CancellationContextObject {
	return vm.getCancellationContext(v)
}

// CreateCancellationContextWithCancel creates a new cancellable context,
// optionally with a parent. Exported for contrib plugins.
func CreateCancellationContextWithCancel(parent *CancellationContextObject) *CancellationContextObject {
	return createCancellationContextWithCancel(parent)
}

// ChannelMarkerValue returns the unexported channelMarker constant.
// Exported for contrib plugin tests that need to verify marker distinctness.
func ChannelMarkerValue() uint32 {
	return channelMarker
}

// CborSerialEncMode returns the CBOR encoder mode used for serialization.
// Exported for contrib plugins that need to produce CBOR-compatible output.
func CborSerialEncMode() cbor.EncMode {
	return cborSerialEncMode
}

// IsResultValue reports whether a Value is a Result (Success or Failure).
func IsResultValue(v Value) bool {
	return isResultValue(v)
}
