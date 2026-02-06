package vm

import (
	"fmt"
	"reflect"
	"sync"
)

// ---------------------------------------------------------------------------
// GoObject: Unified wrapper for arbitrary Go values in the VM
// ---------------------------------------------------------------------------

// goObjectMarker is the symbol tag for all wrapped Go objects.
// Uses marker 41 (next available after httpResponseMarker = 40).
const goObjectMarker uint32 = 41 << 24

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

// RegisterGoObject stores a GoObjectWrapper and returns a symbol-encoded Value.
func (or *ObjectRegistry) RegisterGoObject(obj *GoObjectWrapper) Value {
	id := or.goObjectID.Add(1)
	or.goObjectsMu.Lock()
	or.goObjects[id] = obj
	or.goObjectsMu.Unlock()
	return FromSymbolID(uint32(id) | goObjectMarker)
}

// GetGoObject retrieves a GoObjectWrapper from a symbol-encoded Value.
func (or *ObjectRegistry) GetGoObject(v Value) *GoObjectWrapper {
	if !v.IsSymbol() {
		return nil
	}
	id := v.SymbolID()
	if id&(0xFF<<24) != goObjectMarker {
		return nil
	}
	rawID := id & 0x00FFFFFF
	or.goObjectsMu.RLock()
	defer or.goObjectsMu.RUnlock()
	return or.goObjects[rawID]
}

// GetGoObjectByID retrieves a GoObjectWrapper by raw registry ID.
func (or *ObjectRegistry) GetGoObjectByID(id uint32) *GoObjectWrapper {
	or.goObjectsMu.RLock()
	defer or.goObjectsMu.RUnlock()
	return or.goObjects[id]
}

// UnregisterGoObject removes a GoObject from the registry.
func (or *ObjectRegistry) UnregisterGoObject(v Value) {
	if !v.IsSymbol() {
		return
	}
	id := v.SymbolID()
	if id&(0xFF<<24) != goObjectMarker {
		return
	}
	rawID := id & 0x00FFFFFF
	or.goObjectsMu.Lock()
	delete(or.goObjects, rawID)
	or.goObjectsMu.Unlock()
}

// GoObjectCount returns the number of registered GoObjects.
func (or *ObjectRegistry) GoObjectCount() int {
	or.goObjectsMu.RLock()
	defer or.goObjectsMu.RUnlock()
	return len(or.goObjects)
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

	if info := vm.goTypeRegistry.LookupByType(goType); info != nil {
		return info.Class
	}

	class := vm.Classes.Lookup(className)
	if class == nil {
		class = vm.createClass(className, vm.ObjectClass)
		vm.Globals[className] = vm.classValue(class)
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
		return FromSmallInt(v.Int())

	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		return FromSmallInt(int64(v.Uint()))

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
					h := hashValue(vm.registry, key)
					dictObj.Keys[h] = key
					dictObj.Data[h] = val
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
	case v.IsFloat():
		return v.Float64()
	case v.IsSymbol():
		// Check GoObject first
		if wrapper := vm.registry.GetGoObject(v); wrapper != nil {
			return wrapper.Value
		}
		// Check string
		if s := vm.registry.GetStringObject(v); s != nil {
			return s.Content
		}
		// Regular symbol
		return vm.Symbols.Name(v.SymbolID())
	default:
		return nil
	}
}
