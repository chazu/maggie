package vm

import (
	"math"
	"math/big"
	"testing"

	"cuelang.org/go/cue/cuecontext"
)

// ---------------------------------------------------------------------------
// Primitive types round-trip
// ---------------------------------------------------------------------------

func TestSerial_Nil(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	data, err := vm.SerializeValue(Nil)
	if err != nil {
		t.Fatalf("serialize nil: %v", err)
	}
	got, err := vm.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize nil: %v", err)
	}
	if !got.IsNil() {
		t.Errorf("expected nil, got %v", got)
	}
}

func TestSerial_True(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	data, err := vm.SerializeValue(True)
	if err != nil {
		t.Fatalf("serialize true: %v", err)
	}
	got, err := vm.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize true: %v", err)
	}
	if !got.IsTrue() {
		t.Errorf("expected true, got %v", got)
	}
}

func TestSerial_False(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	data, err := vm.SerializeValue(False)
	if err != nil {
		t.Fatalf("serialize false: %v", err)
	}
	got, err := vm.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize false: %v", err)
	}
	if !got.IsFalse() {
		t.Errorf("expected false, got %v", got)
	}
}

func TestSerial_SmallInt(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	for _, n := range []int64{0, 1, -1, 42, -42, MaxSmallInt, MinSmallInt, 1000000} {
		data, err := vm.SerializeValue(FromSmallInt(n))
		if err != nil {
			t.Fatalf("serialize %d: %v", n, err)
		}
		got, err := vm.DeserializeValue(data)
		if err != nil {
			t.Fatalf("deserialize %d: %v", n, err)
		}
		if !got.IsSmallInt() || got.SmallInt() != n {
			t.Errorf("SmallInt %d: got %v", n, got)
		}
	}
}

func TestSerial_Float(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	for _, f := range []float64{0.0, 1.5, -3.14, math.Inf(1), math.Inf(-1)} {
		data, err := vm.SerializeValue(FromFloat64(f))
		if err != nil {
			t.Fatalf("serialize %f: %v", f, err)
		}
		got, err := vm.DeserializeValue(data)
		if err != nil {
			t.Fatalf("deserialize %f: %v", f, err)
		}
		if !got.IsFloat() || got.Float64() != f {
			t.Errorf("Float %f: got %v (float=%f)", f, got, got.Float64())
		}
	}
}

func TestSerial_String(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	for _, s := range []string{"", "hello", "hello world", "日本語", "🎉"} {
		val := vm.registry.NewStringValue(s)
		data, err := vm.SerializeValue(val)
		if err != nil {
			t.Fatalf("serialize %q: %v", s, err)
		}
		got, err := vm.DeserializeValue(data)
		if err != nil {
			t.Fatalf("deserialize %q: %v", s, err)
		}
		if !IsStringValue(got) {
			t.Errorf("String %q: not a string value", s)
			continue
		}
		content := vm.registry.GetStringContent(got)
		if content != s {
			t.Errorf("String: got %q, want %q", content, s)
		}
	}
}

func TestSerial_Symbol(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	for _, name := range []string{"foo", "bar", "hello:world:"} {
		id := vm.Symbols.Intern(name)
		val := FromSymbolID(id)
		data, err := vm.SerializeValue(val)
		if err != nil {
			t.Fatalf("serialize #%s: %v", name, err)
		}
		got, err := vm.DeserializeValue(data)
		if err != nil {
			t.Fatalf("deserialize #%s: %v", name, err)
		}
		if !got.IsSymbol() {
			t.Errorf("Symbol #%s: not a symbol", name)
			continue
		}
		gotName := vm.Symbols.Name(got.SymbolID())
		if gotName != name {
			t.Errorf("Symbol: got #%s, want #%s", gotName, name)
		}
	}
}

func TestSerial_Character(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	for _, r := range []rune{'A', 'Z', '0', '日', '🎉'} {
		val := FromCharacter(r)
		data, err := vm.SerializeValue(val)
		if err != nil {
			t.Fatalf("serialize char %c: %v", r, err)
		}
		got, err := vm.DeserializeValue(data)
		if err != nil {
			t.Fatalf("deserialize char %c: %v", r, err)
		}
		if !IsCharacterValue(got) {
			t.Errorf("Character %c: not a character", r)
			continue
		}
		if GetCharacterCodePoint(got) != r {
			t.Errorf("Character: got %c, want %c", GetCharacterCodePoint(got), r)
		}
	}
}

func TestSerial_BigInteger(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	// Positive big int
	big1 := new(big.Int).SetInt64(MaxSmallInt)
	big1.Mul(big1, big.NewInt(1000)) // Overflow SmallInt range
	val := vm.registry.NewBigIntValue(big1)
	data, err := vm.SerializeValue(val)
	if err != nil {
		t.Fatalf("serialize BigInt: %v", err)
	}
	got, err := vm.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize BigInt: %v", err)
	}
	if !IsBigIntValue(got) {
		t.Fatal("BigInt: not a big int")
	}
	gotBi := vm.registry.GetBigInt(got)
	if gotBi.Value.Cmp(big1) != 0 {
		t.Errorf("BigInt: got %s, want %s", gotBi.Value, big1)
	}

	// Negative big int
	big2 := new(big.Int).Neg(big1)
	val2 := vm.registry.NewBigIntValue(big2)
	data2, err := vm.SerializeValue(val2)
	if err != nil {
		t.Fatalf("serialize negative BigInt: %v", err)
	}
	got2, err := vm.DeserializeValue(data2)
	if err != nil {
		t.Fatalf("deserialize negative BigInt: %v", err)
	}
	if !IsBigIntValue(got2) {
		t.Fatal("negative BigInt: not a big int")
	}
	gotBi2 := vm.registry.GetBigInt(got2)
	if gotBi2.Value.Cmp(big2) != 0 {
		t.Errorf("negative BigInt: got %s, want %s", gotBi2.Value, big2)
	}
}

// ---------------------------------------------------------------------------
// Collection types
// ---------------------------------------------------------------------------

func TestSerial_Array(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	elems := []Value{
		FromSmallInt(1),
		FromSmallInt(2),
		FromSmallInt(3),
	}
	arr := vm.NewArrayWithElements(elems)

	data, err := vm.SerializeValue(arr)
	if err != nil {
		t.Fatalf("serialize array: %v", err)
	}
	got, err := vm.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize array: %v", err)
	}

	obj := ObjectFromValue(got)
	if obj == nil {
		t.Fatal("deserialized array is not an object")
	}
	if obj.NumSlots() != 3 {
		t.Fatalf("array size: got %d, want 3", obj.NumSlots())
	}
	for i, want := range []int64{1, 2, 3} {
		slot := obj.GetSlot(i)
		if !slot.IsSmallInt() || slot.SmallInt() != want {
			t.Errorf("array[%d]: got %v, want %d", i, slot, want)
		}
	}
}

func TestSerial_NestedArray(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	inner := vm.NewArrayWithElements([]Value{FromSmallInt(10), FromSmallInt(20)})
	outer := vm.NewArrayWithElements([]Value{FromSmallInt(1), inner, FromSmallInt(3)})

	data, err := vm.SerializeValue(outer)
	if err != nil {
		t.Fatalf("serialize nested array: %v", err)
	}
	got, err := vm.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize nested array: %v", err)
	}

	obj := ObjectFromValue(got)
	if obj == nil || obj.NumSlots() != 3 {
		t.Fatal("outer array shape wrong")
	}
	// Check inner array
	innerGot := ObjectFromValue(obj.GetSlot(1))
	if innerGot == nil || innerGot.NumSlots() != 2 {
		t.Fatal("inner array shape wrong")
	}
	if s := innerGot.GetSlot(0); !s.IsSmallInt() || s.SmallInt() != 10 {
		t.Errorf("inner[0]: got %v, want 10", s)
	}
}

func TestSerial_Dictionary(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	dictVal := vm.NewDictionary()
	dict := vm.registry.GetDictionaryObject(dictVal)

	// Add entries
	key1 := vm.registry.NewStringValue("name")
	val1 := vm.registry.NewStringValue("Alice")
	h1 := hashValue(vm.registry, key1)
	dict.Keys[h1] = key1
	dict.Data[h1] = val1

	key2 := vm.registry.NewStringValue("age")
	val2 := FromSmallInt(30)
	h2 := hashValue(vm.registry, key2)
	dict.Keys[h2] = key2
	dict.Data[h2] = val2

	data, err := vm.SerializeValue(dictVal)
	if err != nil {
		t.Fatalf("serialize dict: %v", err)
	}
	got, err := vm.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize dict: %v", err)
	}
	if !IsDictionaryValue(got) {
		t.Fatal("deserialized value is not a dictionary")
	}
	gotDict := vm.registry.GetDictionaryObject(got)
	if len(gotDict.Keys) != 2 {
		t.Errorf("dict entries: got %d, want 2", len(gotDict.Keys))
	}
}

// ---------------------------------------------------------------------------
// Object serialization
// ---------------------------------------------------------------------------

func TestSerial_Object(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	// Create a Point class with x, y
	pointClass := vm.createClass("Point", vm.ObjectClass)
	pointClass.InstVars = []string{"x", "y"}
	pointClass.NumSlots = 2

	// Create instance
	obj := NewObject(pointClass.VTable, 2)
	obj.SetSlot(0, FromSmallInt(10))
	obj.SetSlot(1, FromSmallInt(20))

	data, err := vm.SerializeValue(obj.ToValue())
	if err != nil {
		t.Fatalf("serialize object: %v", err)
	}
	got, err := vm.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize object: %v", err)
	}

	gotObj := ObjectFromValue(got)
	if gotObj == nil {
		t.Fatal("deserialized value is not an object")
	}
	// NumSlots may be >= 2 due to inline slot minimum (4)
	if gotObj.NumSlots() < 2 {
		t.Fatalf("object slots: got %d, want >= 2", gotObj.NumSlots())
	}
	if s := gotObj.GetSlot(0); !s.IsSmallInt() || s.SmallInt() != 10 {
		t.Errorf("slot 0: got %v, want 10", s)
	}
	if s := gotObj.GetSlot(1); !s.IsSmallInt() || s.SmallInt() != 20 {
		t.Errorf("slot 1: got %v, want 20", s)
	}
}

// ---------------------------------------------------------------------------
// CUE value
// ---------------------------------------------------------------------------

func TestSerial_CueValue(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()
	vm.registerCuePrimitives()

	ctx := cuecontext.New()
	cueVal := ctx.CompileString(`{"name": "Alice", "age": 30}`)
	obj := &CueValueObject{val: cueVal}
	val := vm.vmRegisterCueValue(obj)

	data, err := vm.SerializeValue(val)
	if err != nil {
		t.Fatalf("serialize CueValue: %v", err)
	}
	got, err := vm.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize CueValue: %v", err)
	}
	if !isCueValueValue(got) {
		t.Fatal("deserialized value is not a CueValue")
	}
	// Verify the CUE value round-tripped correctly
	gotCv := vm.vmGetCueValue(got)
	if gotCv == nil {
		t.Fatal("CueValue registry miss after deserialization")
	}
	if gotCv.val.Err() != nil {
		t.Fatalf("deserialized CueValue has error: %v", gotCv.val.Err())
	}
}

// ---------------------------------------------------------------------------
// Non-serializable types
// ---------------------------------------------------------------------------

func TestSerial_NonSerializable(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	// Block
	_, err := vm.SerializeValue(FromSymbolID(channelMarker | 1))
	if err == nil {
		t.Error("Channel should not be serializable")
	}

	_, err = vm.SerializeValue(FromSymbolID(processMarker | 1))
	if err == nil {
		t.Error("Process should not be serializable")
	}

	_, err = vm.SerializeValue(FromSymbolID(mutexMarker | 1))
	if err == nil {
		t.Error("Mutex should not be serializable")
	}
}

// ---------------------------------------------------------------------------
// Mixed-type array
// ---------------------------------------------------------------------------

func TestSerial_MixedArray(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	elems := []Value{
		FromSmallInt(42),
		vm.registry.NewStringValue("hello"),
		True,
		Nil,
		FromFloat64(3.14),
	}
	arr := vm.NewArrayWithElements(elems)

	data, err := vm.SerializeValue(arr)
	if err != nil {
		t.Fatalf("serialize mixed array: %v", err)
	}
	got, err := vm.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize mixed array: %v", err)
	}

	obj := ObjectFromValue(got)
	if obj == nil || obj.NumSlots() != 5 {
		t.Fatal("mixed array shape wrong")
	}
	if s := obj.GetSlot(0); !s.IsSmallInt() || s.SmallInt() != 42 {
		t.Errorf("elem 0: got %v, want 42", s)
	}
	if s := obj.GetSlot(1); !IsStringValue(s) {
		t.Error("elem 1: not a string")
	}
	if s := obj.GetSlot(2); !s.IsTrue() {
		t.Error("elem 2: not true")
	}
	if s := obj.GetSlot(3); !s.IsNil() {
		t.Error("elem 3: not nil")
	}
	if s := obj.GetSlot(4); !s.IsFloat() || s.Float64() != 3.14 {
		t.Errorf("elem 4: got %v, want 3.14", s)
	}
}

// ---------------------------------------------------------------------------
// Empty collections
// ---------------------------------------------------------------------------

func TestSerial_EmptyArray(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	arr := vm.NewArrayWithElements(nil)
	data, err := vm.SerializeValue(arr)
	if err != nil {
		t.Fatalf("serialize empty array: %v", err)
	}
	got, err := vm.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize empty array: %v", err)
	}
	obj := ObjectFromValue(got)
	if obj == nil {
		t.Fatal("empty array: not an object")
	}
	if obj.NumSlots() != 0 {
		t.Errorf("empty array: got %d slots, want 0", obj.NumSlots())
	}
}

// ---------------------------------------------------------------------------
// Nested objects (no circularity)
// ---------------------------------------------------------------------------

func TestSerial_NestedObject(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	pairClass := vm.createClass("Pair", vm.ObjectClass)
	pairClass.InstVars = []string{"first", "second"}
	pairClass.NumSlots = 2

	inner := NewObject(pairClass.VTable, 2)
	inner.SetSlot(0, FromSmallInt(10))
	inner.SetSlot(1, FromSmallInt(20))

	outer := NewObject(pairClass.VTable, 2)
	outer.SetSlot(0, FromSmallInt(99))
	outer.SetSlot(1, inner.ToValue())

	data, err := vm.SerializeValue(outer.ToValue())
	if err != nil {
		t.Fatalf("serialize: %v", err)
	}
	got, err := vm.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize: %v", err)
	}

	gotOuter := ObjectFromValue(got)
	if gotOuter == nil {
		t.Fatal("outer not object")
	}
	if s := gotOuter.GetSlot(0); !s.IsSmallInt() || s.SmallInt() != 99 {
		t.Errorf("outer.first: got %v (isSmallInt=%v isObject=%v isFloat=%v), want 99",
			uint64(s), s.IsSmallInt(), s.IsObject(), s.IsFloat())
	}

	gotInner := ObjectFromValue(gotOuter.GetSlot(1))
	if gotInner == nil {
		t.Fatal("inner not object")
	}
	if s := gotInner.GetSlot(0); !s.IsSmallInt() || s.SmallInt() != 10 {
		t.Errorf("inner.first: got %v, want 10", s)
	}
	if s := gotInner.GetSlot(1); !s.IsSmallInt() || s.SmallInt() != 20 {
		t.Errorf("inner.second: got %v, want 20", s)
	}
}

// ---------------------------------------------------------------------------
// Circular references
// ---------------------------------------------------------------------------

func TestSerial_CircularReference(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	// Create a Node class with 'value' and 'next'
	nodeClass := vm.createClass("Node", vm.ObjectClass)
	nodeClass.InstVars = []string{"value", "next"}
	nodeClass.NumSlots = 2

	// Create two nodes that reference each other: A → B → A
	nodeA := NewObject(nodeClass.VTable, 2)
	nodeB := NewObject(nodeClass.VTable, 2)
	nodeA.SetSlot(0, FromSmallInt(1))  // value = 1
	nodeA.SetSlot(1, nodeB.ToValue())  // next = B
	nodeB.SetSlot(0, FromSmallInt(2))  // value = 2
	nodeB.SetSlot(1, nodeA.ToValue())  // next = A (circular!)

	data, err := vm.SerializeValue(nodeA.ToValue())
	if err != nil {
		t.Fatalf("serialize circular: %v", err)
	}

	got, err := vm.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize circular: %v", err)
	}

	// Verify structure: A.value=1, A.next.value=2, A.next.next == A
	objA := ObjectFromValue(got)
	if objA == nil {
		t.Fatal("deserialized A is not an object")
	}
	if s := objA.GetSlot(0); !s.IsSmallInt() || s.SmallInt() != 1 {
		t.Errorf("A.value: got %v, want 1", s)
	}

	objB := ObjectFromValue(objA.GetSlot(1))
	if objB == nil {
		t.Fatal("A.next is not an object")
	}
	if s := objB.GetSlot(0); !s.IsSmallInt() || s.SmallInt() != 2 {
		t.Errorf("B.value: got %v, want 2", s)
	}

	// B.next should be the same object as A (circular reference resolved)
	objABack := ObjectFromValue(objB.GetSlot(1))
	if objABack == nil {
		t.Fatal("B.next is not an object")
	}
	if objABack != objA {
		t.Error("B.next should be the same object as A (circular ref)")
	}
}

func TestSerial_SelfReference(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	nodeClass := vm.createClass("SelfRef", vm.ObjectClass)
	nodeClass.InstVars = []string{"self_ref"}
	nodeClass.NumSlots = 1

	obj := NewObject(nodeClass.VTable, 1)
	obj.SetSlot(0, obj.ToValue()) // self-reference

	data, err := vm.SerializeValue(obj.ToValue())
	if err != nil {
		t.Fatalf("serialize self-ref: %v", err)
	}

	got, err := vm.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize self-ref: %v", err)
	}

	gotObj := ObjectFromValue(got)
	if gotObj == nil {
		t.Fatal("deserialized value is not an object")
	}

	// The self-reference slot should point back to the same object
	selfRef := ObjectFromValue(gotObj.GetSlot(0))
	if selfRef != gotObj {
		t.Error("self-reference should point to same object")
	}
}

func TestSerial_EmptyDictionary(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	dictVal := vm.NewDictionary()
	data, err := vm.SerializeValue(dictVal)
	if err != nil {
		t.Fatalf("serialize empty dict: %v", err)
	}
	got, err := vm.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize empty dict: %v", err)
	}
	if !IsDictionaryValue(got) {
		t.Fatal("not a dictionary")
	}
	gotDict := vm.registry.GetDictionaryObject(got)
	if len(gotDict.Keys) != 0 {
		t.Errorf("empty dict: got %d entries, want 0", len(gotDict.Keys))
	}
}

// ---------------------------------------------------------------------------
// Exception round-trip serialization
// ---------------------------------------------------------------------------

func TestSerial_Exception_Error(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	// Create an Error exception with a message
	exObj := &ExceptionObject{
		ExceptionClass: v.ErrorClass,
		MessageText:    v.registry.NewStringValue("something went wrong"),
		Resumable:      false,
	}
	id := v.registry.RegisterException(exObj)
	exVal := FromExceptionID(id)

	data, err := v.SerializeValue(exVal)
	if err != nil {
		t.Fatalf("serialize exception: %v", err)
	}

	got, err := v.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize exception: %v", err)
	}

	if !got.IsException() {
		t.Fatal("deserialized value is not an exception")
	}

	gotObj := v.registry.GetException(got.ExceptionID())
	if gotObj == nil {
		t.Fatal("deserialized exception not in registry")
	}
	if gotObj.ExceptionClass != v.ErrorClass {
		t.Errorf("expected ErrorClass, got %v", gotObj.ExceptionClass)
	}
	if !IsStringValue(gotObj.MessageText) {
		t.Fatal("message text is not a string")
	}
	msg := v.registry.GetStringContent(gotObj.MessageText)
	if msg != "something went wrong" {
		t.Errorf("message: got %q, want %q", msg, "something went wrong")
	}
}

func TestSerial_Exception_ZeroDivide(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	exObj := &ExceptionObject{
		ExceptionClass: v.ZeroDivideClass,
		MessageText:    v.registry.NewStringValue("division by zero"),
	}
	id := v.registry.RegisterException(exObj)
	exVal := FromExceptionID(id)

	data, err := v.SerializeValue(exVal)
	if err != nil {
		t.Fatalf("serialize: %v", err)
	}

	got, err := v.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize: %v", err)
	}

	if !got.IsException() {
		t.Fatal("not an exception")
	}
	gotObj := v.registry.GetException(got.ExceptionID())
	if gotObj == nil {
		t.Fatal("not in registry")
	}
	if gotObj.ExceptionClass != v.ZeroDivideClass {
		t.Errorf("expected ZeroDivideClass, got %s", gotObj.ExceptionClass.Name)
	}
}

func TestSerial_Exception_WithTag(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	tagSym := FromSymbolID(v.Symbols.Intern("myTag"))
	exObj := &ExceptionObject{
		ExceptionClass: v.ErrorClass,
		MessageText:    v.registry.NewStringValue("tagged error"),
		Tag:            tagSym,
	}
	id := v.registry.RegisterException(exObj)
	exVal := FromExceptionID(id)

	data, err := v.SerializeValue(exVal)
	if err != nil {
		t.Fatalf("serialize: %v", err)
	}

	got, err := v.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize: %v", err)
	}

	gotObj := v.registry.GetException(got.ExceptionID())
	if gotObj == nil {
		t.Fatal("not in registry")
	}
	if !gotObj.Tag.IsSymbol() {
		t.Fatal("tag is not a symbol")
	}
	tagName := v.Symbols.Name(gotObj.Tag.SymbolID())
	if tagName != "myTag" {
		t.Errorf("tag: got %q, want %q", tagName, "myTag")
	}
}

func TestSerial_Exception_NoMessage(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	exObj := &ExceptionObject{
		ExceptionClass: v.ExceptionClass,
		MessageText:    Nil,
		Tag:            Nil,
	}
	id := v.registry.RegisterException(exObj)
	exVal := FromExceptionID(id)

	data, err := v.SerializeValue(exVal)
	if err != nil {
		t.Fatalf("serialize: %v", err)
	}

	got, err := v.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize: %v", err)
	}

	if !got.IsException() {
		t.Fatal("not an exception")
	}
	gotObj := v.registry.GetException(got.ExceptionID())
	if gotObj == nil {
		t.Fatal("not in registry")
	}
	if gotObj.ExceptionClass != v.ExceptionClass {
		t.Errorf("expected ExceptionClass, got %s", gotObj.ExceptionClass.Name)
	}
	if gotObj.MessageText != Nil {
		t.Errorf("expected Nil message, got %v", gotObj.MessageText)
	}
}

func TestSerial_Exception_UnknownClass(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	// Create an exception with a custom class not in the standard hierarchy
	customClass := v.createClass("CustomError", v.ErrorClass)
	// Do NOT register it in Globals — simulate a class unknown to the deserializer

	exObj := &ExceptionObject{
		ExceptionClass: customClass,
		MessageText:    v.registry.NewStringValue("custom error"),
	}
	id := v.registry.RegisterException(exObj)
	exVal := FromExceptionID(id)

	data, err := v.SerializeValue(exVal)
	if err != nil {
		t.Fatalf("serialize: %v", err)
	}

	// Deserialize on a fresh VM where CustomError does not exist
	v2 := NewVM()
	defer v2.Shutdown()

	got, err := v2.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize: %v", err)
	}

	if !got.IsException() {
		t.Fatal("not an exception")
	}
	gotObj := v2.registry.GetException(got.ExceptionID())
	if gotObj == nil {
		t.Fatal("not in registry")
	}
	// Should fall back to ErrorClass since CustomError is unknown
	if gotObj.ExceptionClass != v2.ErrorClass {
		t.Errorf("expected ErrorClass fallback, got %s", gotObj.ExceptionClass.Name)
	}
	msg := v2.registry.GetStringContent(gotObj.MessageText)
	if msg != "custom error" {
		t.Errorf("message preserved: got %q, want %q", msg, "custom error")
	}
}

func TestSerial_Exception_CrossVM(t *testing.T) {
	// Simulate the full cross-node cycle: serialize on VM1, deserialize on VM2
	v1 := NewVM()
	defer v1.Shutdown()
	v2 := NewVM()
	defer v2.Shutdown()

	exObj := &ExceptionObject{
		ExceptionClass: v1.StackOverflowClass,
		MessageText:    v1.registry.NewStringValue("stack overflow at depth 4096"),
	}
	id := v1.registry.RegisterException(exObj)
	exVal := FromExceptionID(id)

	data, err := v1.SerializeValue(exVal)
	if err != nil {
		t.Fatalf("serialize on VM1: %v", err)
	}

	got, err := v2.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize on VM2: %v", err)
	}

	if !got.IsException() {
		t.Fatal("not an exception on VM2")
	}
	gotObj := v2.registry.GetException(got.ExceptionID())
	if gotObj == nil {
		t.Fatal("not in VM2 registry")
	}
	if gotObj.ExceptionClass != v2.StackOverflowClass {
		t.Errorf("class: got %s, want StackOverflow", gotObj.ExceptionClass.Name)
	}
	msg := v2.registry.GetStringContent(gotObj.MessageText)
	if msg != "stack overflow at depth 4096" {
		t.Errorf("message: got %q, want %q", msg, "stack overflow at depth 4096")
	}
}
