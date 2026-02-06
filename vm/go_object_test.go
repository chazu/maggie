package vm

import (
	"reflect"
	"testing"
)

func TestGoTypeRegistry_RegisterAndLookup(t *testing.T) {
	reg := NewGoTypeRegistry()

	type Foo struct{ X int }
	fooType := reflect.TypeOf((*Foo)(nil))

	class := NewClass("Go::Foo", nil)
	id := reg.Register(fooType, class, "Go::Foo")

	if id == 0 {
		t.Fatal("expected non-zero type ID")
	}

	// Lookup by ID
	info := reg.Lookup(id)
	if info == nil {
		t.Fatal("expected to find type info by ID")
	}
	if info.ClassName != "Go::Foo" {
		t.Errorf("expected class name Go::Foo, got %s", info.ClassName)
	}
	if info.GoType != fooType {
		t.Error("Go type mismatch")
	}

	// Lookup by reflect.Type
	info2 := reg.LookupByType(fooType)
	if info2 == nil || info2.TypeID != id {
		t.Error("LookupByType failed")
	}

	// Duplicate registration returns same ID
	id2 := reg.Register(fooType, class, "Go::Foo")
	if id2 != id {
		t.Errorf("expected same ID %d on re-register, got %d", id, id2)
	}

	if reg.Count() != 1 {
		t.Errorf("expected count 1, got %d", reg.Count())
	}
}

func TestGoTypeRegistry_MultipleTypes(t *testing.T) {
	reg := NewGoTypeRegistry()

	type A struct{}
	type B struct{}

	classA := NewClass("Go::A", nil)
	classB := NewClass("Go::B", nil)

	idA := reg.Register(reflect.TypeOf((*A)(nil)), classA, "Go::A")
	idB := reg.Register(reflect.TypeOf((*B)(nil)), classB, "Go::B")

	if idA == idB {
		t.Error("expected different IDs for different types")
	}

	if reg.Count() != 2 {
		t.Errorf("expected count 2, got %d", reg.Count())
	}
}

func TestGoObjectRegistry_RegisterAndGet(t *testing.T) {
	or := NewObjectRegistry()

	wrapper := &GoObjectWrapper{TypeID: 1, Value: "hello"}
	v := or.RegisterGoObject(wrapper)

	if !v.IsSymbol() {
		t.Fatal("expected symbol value")
	}

	got := or.GetGoObject(v)
	if got == nil {
		t.Fatal("expected to get GoObject back")
	}
	if got.Value != "hello" {
		t.Errorf("expected 'hello', got %v", got.Value)
	}
	if got.TypeID != 1 {
		t.Errorf("expected TypeID 1, got %d", got.TypeID)
	}

	if or.GoObjectCount() != 1 {
		t.Errorf("expected count 1, got %d", or.GoObjectCount())
	}

	// Unregister
	or.UnregisterGoObject(v)
	if or.GoObjectCount() != 0 {
		t.Errorf("expected count 0 after unregister, got %d", or.GoObjectCount())
	}
}

func TestGoObjectRegistry_GetNonGoObject(t *testing.T) {
	or := NewObjectRegistry()

	// Nil value
	if or.GetGoObject(Nil) != nil {
		t.Error("expected nil for Nil value")
	}

	// SmallInt
	if or.GetGoObject(FromSmallInt(42)) != nil {
		t.Error("expected nil for SmallInt")
	}

	// Random symbol (wrong marker)
	if or.GetGoObject(FromSymbolID(channelMarker | 5)) != nil {
		t.Error("expected nil for channel marker symbol")
	}
}

func TestGoObjectMarker_NoCollision(t *testing.T) {
	markers := map[string]uint32{
		"channel":             channelMarker,
		"process":             processMarker,
		"result":              resultMarker,
		"grpcClient":          grpcClientMarker,
		"exception":           exceptionMarker,
		"grpcStream":          grpcStreamMarker,
		"weakRef":             weakRefMarker,
		"mutex":               mutexMarker,
		"waitGroup":           waitGroupMarker,
		"semaphore":           semaphoreMarker,
		"cancellationContext": cancellationContextMarker,
		"classValue":          classValueMarker,
		"character":           characterMarker,
		"httpServer":          httpServerMarker,
		"httpRequest":         httpRequestMarker,
		"httpResponse":        httpResponseMarker,
		"goObject":            goObjectMarker,
	}

	seen := make(map[uint32]string)
	for name, marker := range markers {
		if prev, exists := seen[marker]; exists {
			t.Errorf("marker collision: %s and %s both use 0x%08X", name, prev, marker)
		}
		seen[marker] = name
	}
}

func TestVM_RegisterGoType(t *testing.T) {
	vmInst := NewVM()
	defer vmInst.Shutdown()

	type MyStruct struct{ Name string }
	ptrType := reflect.TypeOf((*MyStruct)(nil))

	class := vmInst.RegisterGoType("Go::MyStruct", ptrType)
	if class == nil {
		t.Fatal("expected non-nil class")
	}
	if class.Name != "Go::MyStruct" {
		t.Errorf("expected class name Go::MyStruct, got %s", class.Name)
	}

	// Should be in Globals
	if _, ok := vmInst.Globals["Go::MyStruct"]; !ok {
		t.Error("expected Go::MyStruct in Globals")
	}

	// Re-registering returns same class
	class2 := vmInst.RegisterGoType("Go::MyStruct", ptrType)
	if class2 != class {
		t.Error("expected same class on re-register")
	}
}

func TestVM_GoObjectRoundTrip(t *testing.T) {
	vmInst := NewVM()
	defer vmInst.Shutdown()

	type Widget struct{ Label string }
	ptrType := reflect.TypeOf((*Widget)(nil))
	vmInst.RegisterGoType("Go::Widget", ptrType)

	w := &Widget{Label: "OK"}
	v, err := vmInst.RegisterGoObject(w)
	if err != nil {
		t.Fatalf("RegisterGoObject: %v", err)
	}

	got, ok := vmInst.GetGoObject(v)
	if !ok {
		t.Fatal("GetGoObject returned false")
	}
	gotW, ok := got.(*Widget)
	if !ok {
		t.Fatalf("expected *Widget, got %T", got)
	}
	if gotW.Label != "OK" {
		t.Errorf("expected label 'OK', got %s", gotW.Label)
	}

	// Class dispatch
	cls := vmInst.GoObjectClass(v)
	if cls == nil || cls.Name != "Go::Widget" {
		t.Errorf("expected Go::Widget class, got %v", cls)
	}
}

func TestVM_GoToValue_BasicTypes(t *testing.T) {
	vmInst := NewVM()
	defer vmInst.Shutdown()

	tests := []struct {
		name     string
		goVal    interface{}
		checkFn  func(Value) bool
		expected string
	}{
		{"nil", nil, func(v Value) bool { return v == Nil }, "Nil"},
		{"true", true, func(v Value) bool { return v == True }, "True"},
		{"false", false, func(v Value) bool { return v == False }, "False"},
		{"int", 42, func(v Value) bool { return v.IsSmallInt() && v.SmallInt() == 42 }, "SmallInt(42)"},
		{"int64", int64(100), func(v Value) bool { return v.IsSmallInt() && v.SmallInt() == 100 }, "SmallInt(100)"},
		{"float64", 3.14, func(v Value) bool { return v.IsFloat() }, "Float"},
		{"uint", uint(7), func(v Value) bool { return v.IsSmallInt() && v.SmallInt() == 7 }, "SmallInt(7)"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := vmInst.GoToValue(tt.goVal)
			if !tt.checkFn(v) {
				t.Errorf("expected %s for %v", tt.expected, tt.goVal)
			}
		})
	}
}

func TestVM_GoToValue_String(t *testing.T) {
	vmInst := NewVM()
	defer vmInst.Shutdown()

	v := vmInst.GoToValue("hello")
	s := vmInst.registry.GetStringContent(v)
	if s != "hello" {
		t.Errorf("expected 'hello', got '%s'", s)
	}
}

func TestVM_GoToValue_ByteSlice(t *testing.T) {
	vmInst := NewVM()
	defer vmInst.Shutdown()

	v := vmInst.GoToValue([]byte("bytes"))
	s := vmInst.registry.GetStringContent(v)
	if s != "bytes" {
		t.Errorf("expected 'bytes', got '%s'", s)
	}
}

func TestVM_ValueToGo_BasicTypes(t *testing.T) {
	vmInst := NewVM()
	defer vmInst.Shutdown()

	if vmInst.ValueToGo(Nil) != nil {
		t.Error("expected nil for Nil")
	}
	if vmInst.ValueToGo(True) != true {
		t.Error("expected true")
	}
	if vmInst.ValueToGo(False) != false {
		t.Error("expected false")
	}
	if vmInst.ValueToGo(FromSmallInt(42)) != int64(42) {
		t.Error("expected 42")
	}
}

func TestVM_GoToValue_Slice(t *testing.T) {
	vmInst := NewVM()
	defer vmInst.Shutdown()

	input := []int{1, 2, 3}
	v := vmInst.GoToValue(input)
	if !v.IsObject() {
		t.Fatal("expected object (Array)")
	}
	obj := ObjectFromValue(v)
	if obj.NumSlots() != 3 {
		t.Errorf("expected 3 slots, got %d", obj.NumSlots())
	}
	if obj.GetSlot(0).SmallInt() != 1 {
		t.Errorf("expected slot 0 = 1, got %d", obj.GetSlot(0).SmallInt())
	}
}

func TestVM_RegisterGoObject_UnregisteredType(t *testing.T) {
	vmInst := NewVM()
	defer vmInst.Shutdown()

	type Unknown struct{}
	_, err := vmInst.RegisterGoObject(&Unknown{})
	if err == nil {
		t.Error("expected error for unregistered type")
	}
}
