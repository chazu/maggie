package vm

import (
	"fmt"
	"math"
	"math/big"
	"reflect"
	"strconv"
	"sync"
	"unsafe"
)

// ---------------------------------------------------------------------------
// GoObject: Unified wrapper for arbitrary Go values in the VM
// ---------------------------------------------------------------------------

// GoObjectWrapper holds an opaque Go value along with its type registry ID.
type GoObjectWrapper struct {
	TypeID uint16
	Value  interface{}
}

// GoTypeInfo describes a registered Go type and its associated Maggie class.
type GoTypeInfo struct {
	TypeID    uint16
	GoType    reflect.Type
	Class     *Class
	ClassName string
}

// GoTypeRegistry maps Go types to Maggie classes and vice versa.
// Thread-safe for concurrent registration and lookup.
type GoTypeRegistry struct {
	mu     sync.RWMutex
	types  map[uint16]*GoTypeInfo
	byType map[reflect.Type]uint16
	nextID uint16
}

// NewGoTypeRegistry creates an empty type registry.
func NewGoTypeRegistry() *GoTypeRegistry {
	return &GoTypeRegistry{
		types:  make(map[uint16]*GoTypeInfo),
		byType: make(map[reflect.Type]uint16),
		nextID: 1, // 0 means unregistered
	}
}

// Register adds a Go type to the registry and returns its type ID.
// If the type is already registered, returns the existing ID.
func (r *GoTypeRegistry) Register(goType reflect.Type, class *Class, className string) uint16 {
	r.mu.Lock()
	defer r.mu.Unlock()

	if id, ok := r.byType[goType]; ok {
		return id
	}

	id := r.nextID
	r.nextID++

	info := &GoTypeInfo{
		TypeID:    id,
		GoType:    goType,
		Class:     class,
		ClassName: className,
	}
	r.types[id] = info
	r.byType[goType] = id
	return id
}

// Lookup returns the type info for a given type ID.
func (r *GoTypeRegistry) Lookup(id uint16) *GoTypeInfo {
	r.mu.RLock()
	defer r.mu.RUnlock()
	return r.types[id]
}

// LookupByType returns the type info for a given Go reflect.Type.
func (r *GoTypeRegistry) LookupByType(goType reflect.Type) *GoTypeInfo {
	r.mu.RLock()
	defer r.mu.RUnlock()
	id, ok := r.byType[goType]
	if !ok {
		return nil
	}
	return r.types[id]
}

// Count returns the number of registered types.
func (r *GoTypeRegistry) Count() int {
	r.mu.RLock()
	defer r.mu.RUnlock()
	return len(r.types)
}

// ---------------------------------------------------------------------------
// GoObject Registry (in ObjectRegistry)
// ---------------------------------------------------------------------------

// isGoObjectValue reports whether v is a heap GoObject wrapper.
func isGoObjectValue(v Value) bool {
	return v.ptr != nil && v.hi == kindGoObject
}

// RegisterGoObject wraps a GoObjectWrapper in a heap Value traced by the Go GC.
func (or *ObjectRegistry) RegisterGoObject(obj *GoObjectWrapper) Value {
	return makeHeap(kindGoObject, unsafe.Pointer(obj))
}

// GetGoObject retrieves a GoObjectWrapper from a Value.
func (or *ObjectRegistry) GetGoObject(v Value) *GoObjectWrapper {
	if !isGoObjectValue(v) {
		return nil
	}
	return (*GoObjectWrapper)(v.ptr)
}

// ---------------------------------------------------------------------------
// VM-level helpers
// ---------------------------------------------------------------------------

// RegisterGoType registers a Go type with its Maggie class name.
// Creates the class if it doesn't exist, registers it in the type registry,
// and returns the class.
func (vm *VM) RegisterGoType(className string, goType reflect.Type) *Class {
	if vm.goTypeRegistry == nil {
		vm.goTypeRegistry = NewGoTypeRegistry()
	}

	// Skip LookupByType for namespace sentinel types (*struct{}) —
	// all namespace classes share the same Go type, so the first one
	// registered would shadow all subsequent ones.
	isNamespaceSentinel := goType == reflect.TypeOf((*struct{})(nil))
	if !isNamespaceSentinel {
		if info := vm.goTypeRegistry.LookupByType(goType); info != nil {
			return info.Class
		}
	}

	class := vm.Classes.Lookup(className)
	if class == nil {
		class = vm.createClass(className, vm.ObjectClass)
		vm.SetGlobal(className, vm.classValue(class))
	}

	vm.goTypeRegistry.Register(goType, class, className)
	return class
}

// RegisterGoObject wraps a Go value and returns a Maggie Value.
// The Go value's type must be pre-registered via RegisterGoType.
func (vm *VM) RegisterGoObject(goValue interface{}) (Value, error) {
	if vm.goTypeRegistry == nil {
		return Nil, fmt.Errorf("no Go types registered")
	}

	goType := reflect.TypeOf(goValue)
	info := vm.goTypeRegistry.LookupByType(goType)
	if info == nil {
		return Nil, fmt.Errorf("Go type %s not registered", goType)
	}

	wrapper := &GoObjectWrapper{
		TypeID: info.TypeID,
		Value:  goValue,
	}
	return vm.registry.RegisterGoObject(wrapper), nil
}

// GetGoObject extracts the Go value from a Maggie GoObject Value.
func (vm *VM) GetGoObject(v Value) (interface{}, bool) {
	wrapper := vm.registry.GetGoObject(v)
	if wrapper == nil {
		return nil, false
	}
	return wrapper.Value, true
}

// GoObjectClass returns the Maggie class for a GoObject Value.
func (vm *VM) GoObjectClass(v Value) *Class {
	wrapper := vm.registry.GetGoObject(v)
	if wrapper == nil || vm.goTypeRegistry == nil {
		return nil
	}
	info := vm.goTypeRegistry.Lookup(wrapper.TypeID)
	if info == nil {
		return nil
	}
	return info.Class
}

// ---------------------------------------------------------------------------
// Type Marshaling: Go ↔ Maggie Value conversion
// ---------------------------------------------------------------------------

// GoToValue converts a Go value to a Maggie Value.
// Handles basic types (int, float, string, bool, nil) and registered GoObject types.
func (vm *VM) GoToValue(goVal interface{}) Value {
	if goVal == nil {
		return Nil
	}

	v := reflect.ValueOf(goVal)
	switch v.Kind() {
	case reflect.Bool:
		if v.Bool() {
			return True
		}
		return False

	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		// FromSmallInt panics outside the 48-bit range; a wrapped Go function
		// returning a large int64 (ns timestamps, hashes, ids) must promote to
		// BigInteger instead of crashing the VM.
		n := v.Int()
		if val, ok := TryFromSmallInt(n); ok {
			return val
		}
		return vm.registry.NewBigIntValue(big.NewInt(n))

	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		u := v.Uint()
		if u <= math.MaxInt64 {
			if val, ok := TryFromSmallInt(int64(u)); ok {
				return val
			}
		}
		// SetUint64 covers the full uint64 range (int64(u) would wrap negative).
		return vm.registry.NewBigIntValue(new(big.Int).SetUint64(u))

	case reflect.Float32, reflect.Float64:
		return FromFloat64(v.Float())

	case reflect.String:
		return vm.registry.NewStringValue(v.String())

	case reflect.Slice:
		if v.Type().Elem().Kind() == reflect.Uint8 {
			// []byte → String
			return vm.registry.NewStringValue(string(v.Bytes()))
		}
		// []T → Array
		arr := make([]Value, v.Len())
		for i := 0; i < v.Len(); i++ {
			arr[i] = vm.GoToValue(v.Index(i).Interface())
		}
		return vm.NewArrayWithElements(arr)

	case reflect.Map:
		if v.Type().Key().Kind() == reflect.String {
			dict := vm.registry.NewDictionaryValue()
			dictObj := vm.registry.GetDictionaryObject(dict)
			if dictObj != nil {
				iter := v.MapRange()
				for iter.Next() {
					key := vm.registry.NewStringValue(iter.Key().String())
					val := vm.GoToValue(iter.Value().Interface())
					dictObj.Put(vm.registry, key, val)
				}
			}
			return dict
		}

	case reflect.Ptr, reflect.Struct:
		// Try to wrap as GoObject if type is registered
		if vm.goTypeRegistry != nil {
			goType := reflect.TypeOf(goVal)
			if info := vm.goTypeRegistry.LookupByType(goType); info != nil {
				wrapper := &GoObjectWrapper{
					TypeID: info.TypeID,
					Value:  goVal,
				}
				return vm.registry.RegisterGoObject(wrapper)
			}
		}
	}

	return Nil
}

// ValueToGo converts a Maggie Value to a Go interface{}.
// Handles basic types and GoObject unwrapping.
func (vm *VM) ValueToGo(v Value) interface{} {
	switch {
	case v == Nil:
		return nil
	case v == True:
		return true
	case v == False:
		return false
	case v.IsSmallInt():
		return v.SmallInt()
	case IsBigIntValue(v):
		// Mirror GoToValue: int64 when it fits, *big.Int beyond. Returning
		// nil here made large integers silently vanish (e.g. bound as NULL).
		if bi := vm.registry.GetBigInt(v); bi != nil {
			if bi.Value.IsInt64() {
				return bi.Value.Int64()
			}
			return new(big.Int).Set(bi.Value)
		}
		return nil
	case v.IsFloat():
		return v.Float64()
	case IsStringValue(v):
		return vm.registry.GetStringContent(v)
	case v.IsSymbolEncoded():
		// Check GoObject first
		if wrapper := vm.registry.GetGoObject(v); wrapper != nil {
			return wrapper.Value
		}
		// Regular symbol
		return vm.Symbols.Name(v.SymbolID())
	default:
		return nil
	}
}

// GoIntArg converts an Integer argument for a signed Go integer parameter of
// the given bit size (0 means the platform int size). gowrap-generated
// bindings call it; Value.SmallInt panicked on BigIntegers and non-integers,
// and a plain conversion would silently truncate to the parameter's width.
// Anything that is not an Integer within range signals a PrimitiveError.
func (vm *VM) GoIntArg(v Value, bits int) int64 {
	if bits == 0 {
		bits = strconv.IntSize
	}
	n, ok := vm.integerArg(v)
	if ok && n.IsInt64() {
		i := n.Int64()
		if bits == 64 || (i >= -(1<<(bits-1)) && i < 1<<(bits-1)) {
			return i
		}
	}
	vm.SignalPrimitiveError("Go argument", fmt.Sprintf("expected an Integer that fits int%d, got %s", bits, vm.describeIntArg(v, ok)))
	return 0
}

// GoUintArg is GoIntArg for unsigned Go integer parameters.
func (vm *VM) GoUintArg(v Value, bits int) uint64 {
	if bits == 0 {
		bits = strconv.IntSize
	}
	n, ok := vm.integerArg(v)
	if ok && n.Sign() >= 0 && n.BitLen() <= bits {
		return n.Uint64()
	}
	vm.SignalPrimitiveError("Go argument", fmt.Sprintf("expected an Integer that fits uint%d, got %s", bits, vm.describeIntArg(v, ok)))
	return 0
}

// integerArg returns v as a big.Int when it is a SmallInteger or BigInteger.
func (vm *VM) integerArg(v Value) (*big.Int, bool) {
	if v.IsSmallInt() {
		return big.NewInt(v.SmallInt()), true
	}
	if IsBigIntValue(v) {
		if bi := vm.registry.GetBigInt(v); bi != nil {
			return bi.Value, true
		}
	}
	return nil, false
}

func (vm *VM) describeIntArg(v Value, isInt bool) string {
	if isInt {
		n, _ := vm.integerArg(v)
		return n.String()
	}
	return "a non-Integer"
}
