package vm

import (
	"strings"
	"testing"
)

// ---------------------------------------------------------------------------
// Json encode: tests
// ---------------------------------------------------------------------------

func TestJsonEncodeSmallInt(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	result := vm.Send(jsonClass, "primEncode:", []Value{FromSmallInt(42)})
	assertStringResult(t, vm, result, "42")
}

func TestJsonEncodeFloat(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	result := vm.Send(jsonClass, "primEncode:", []Value{FromFloat64(3.14)})
	assertStringResult(t, vm, result, "3.14")
}

func TestJsonEncodeString(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	input := vm.registry.NewStringValue("hello world")
	result := vm.Send(jsonClass, "primEncode:", []Value{input})
	assertStringResult(t, vm, result, `"hello world"`)
}

func TestJsonEncodeStringSpecialChars(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	input := vm.registry.NewStringValue("tab\there\nnewline\"quote")
	result := vm.Send(jsonClass, "primEncode:", []Value{input})
	assertStringResult(t, vm, result, `"tab\there\nnewline\"quote"`)
}

func TestJsonEncodeStringUnicode(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	input := vm.registry.NewStringValue("こんにちは 🌍")
	result := vm.Send(jsonClass, "primEncode:", []Value{input})
	assertStringResult(t, vm, result, `"こんにちは 🌍"`)
}

func TestJsonEncodeTrue(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	result := vm.Send(jsonClass, "primEncode:", []Value{True})
	assertStringResult(t, vm, result, "true")
}

func TestJsonEncodeFalse(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	result := vm.Send(jsonClass, "primEncode:", []Value{False})
	assertStringResult(t, vm, result, "false")
}

func TestJsonEncodeNil(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	result := vm.Send(jsonClass, "primEncode:", []Value{Nil})
	assertStringResult(t, vm, result, "null")
}

func TestJsonEncodeArray(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	arr := vm.NewArrayWithElements([]Value{FromSmallInt(1), FromSmallInt(2), FromSmallInt(3)})
	result := vm.Send(jsonClass, "primEncode:", []Value{arr})
	assertStringResult(t, vm, result, "[1,2,3]")
}

func TestJsonEncodeDictionary(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))

	dict := vm.registry.NewDictionaryValue()
	d := vm.registry.GetDictionaryObject(dict)
	key := vm.registry.NewStringValue("name")
	val := vm.registry.NewStringValue("Alice")
	h := hashValue(vm.registry, key)
	d.Data[h] = val
	d.Keys[h] = key

	result := vm.Send(jsonClass, "primEncode:", []Value{dict})
	assertStringResult(t, vm, result, `{"name":"Alice"}`)
}

func TestJsonEncodeNested(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))

	// Build {"items": [1, 2], "ok": true}
	innerArr := vm.NewArrayWithElements([]Value{FromSmallInt(1), FromSmallInt(2)})

	dict := vm.registry.NewDictionaryValue()
	d := vm.registry.GetDictionaryObject(dict)

	key1 := vm.registry.NewStringValue("items")
	h1 := hashValue(vm.registry, key1)
	d.Data[h1] = innerArr
	d.Keys[h1] = key1

	key2 := vm.registry.NewStringValue("ok")
	h2 := hashValue(vm.registry, key2)
	d.Data[h2] = True
	d.Keys[h2] = key2

	result := vm.Send(jsonClass, "primEncode:", []Value{dict})
	if !IsStringValue(result) {
		t.Fatalf("Expected string result, got %v", result)
	}
	got := vm.registry.GetStringContent(result)
	// Dictionary ordering may vary, so check both keys are present
	if !strings.Contains(got, `"items":[1,2]`) {
		t.Errorf("Expected items array in %q", got)
	}
	if !strings.Contains(got, `"ok":true`) {
		t.Errorf("Expected ok:true in %q", got)
	}
}

func TestJsonEncodeEmptyArray(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	arr := vm.NewArrayWithElements([]Value{})
	result := vm.Send(jsonClass, "primEncode:", []Value{arr})
	assertStringResult(t, vm, result, "[]")
}

func TestJsonEncodeEmptyDictionary(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	dict := vm.registry.NewDictionaryValue()
	result := vm.Send(jsonClass, "primEncode:", []Value{dict})
	assertStringResult(t, vm, result, "{}")
}

// ---------------------------------------------------------------------------
// Json encodePretty: tests
// ---------------------------------------------------------------------------

func TestJsonEncodePretty(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))

	dict := vm.registry.NewDictionaryValue()
	d := vm.registry.GetDictionaryObject(dict)
	key := vm.registry.NewStringValue("key")
	val := vm.registry.NewStringValue("value")
	h := hashValue(vm.registry, key)
	d.Data[h] = val
	d.Keys[h] = key

	result := vm.Send(jsonClass, "primEncodePretty:", []Value{dict})
	if !IsStringValue(result) {
		t.Fatalf("Expected string result, got %v", result)
	}
	got := vm.registry.GetStringContent(result)
	if !strings.Contains(got, "  ") {
		t.Errorf("Expected indentation in pretty output, got %q", got)
	}
	if !strings.Contains(got, `"key"`) {
		t.Errorf("Expected key in pretty output, got %q", got)
	}
}

// ---------------------------------------------------------------------------
// Json decode: tests
// ---------------------------------------------------------------------------

func TestJsonDecodeInteger(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	input := vm.registry.NewStringValue("42")
	result := vm.Send(jsonClass, "primDecode:", []Value{input})
	if !result.IsSmallInt() || result.SmallInt() != 42 {
		t.Errorf("decode '42' = %v, want SmallInt(42)", result)
	}
}

func TestJsonDecodeNegativeInteger(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	input := vm.registry.NewStringValue("-99")
	result := vm.Send(jsonClass, "primDecode:", []Value{input})
	if !result.IsSmallInt() || result.SmallInt() != -99 {
		t.Errorf("decode '-99' = %v, want SmallInt(-99)", result)
	}
}

func TestJsonDecodeFloat(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	input := vm.registry.NewStringValue("3.14")
	result := vm.Send(jsonClass, "primDecode:", []Value{input})
	if !result.IsFloat() {
		t.Fatalf("decode '3.14' expected Float, got %v", result)
	}
	if result.Float64() != 3.14 {
		t.Errorf("decode '3.14' = %v, want 3.14", result.Float64())
	}
}

func TestJsonDecodeString(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	input := vm.registry.NewStringValue(`"hello"`)
	result := vm.Send(jsonClass, "primDecode:", []Value{input})
	assertStringResult(t, vm, result, "hello")
}

func TestJsonDecodeTrue(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	input := vm.registry.NewStringValue("true")
	result := vm.Send(jsonClass, "primDecode:", []Value{input})
	if result != True {
		t.Errorf("decode 'true' = %v, want True", result)
	}
}

func TestJsonDecodeFalse(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	input := vm.registry.NewStringValue("false")
	result := vm.Send(jsonClass, "primDecode:", []Value{input})
	if result != False {
		t.Errorf("decode 'false' = %v, want False", result)
	}
}

func TestJsonDecodeNull(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	input := vm.registry.NewStringValue("null")
	result := vm.Send(jsonClass, "primDecode:", []Value{input})
	if result != Nil {
		t.Errorf("decode 'null' = %v, want Nil", result)
	}
}

func TestJsonDecodeArray(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	input := vm.registry.NewStringValue("[1, 2, 3]")
	result := vm.Send(jsonClass, "primDecode:", []Value{input})
	if !result.IsObject() {
		t.Fatalf("decode '[1,2,3]' expected array object, got %v", result)
	}
	obj := ObjectFromValue(result)
	if obj.NumSlots() != 3 {
		t.Fatalf("Expected 3 elements, got %d", obj.NumSlots())
	}
	for i, expected := range []int64{1, 2, 3} {
		el := obj.GetSlot(i)
		if !el.IsSmallInt() || el.SmallInt() != expected {
			t.Errorf("Element %d = %v, want %d", i, el, expected)
		}
	}
}

func TestJsonDecodeDictionary(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	input := vm.registry.NewStringValue(`{"name": "Alice", "age": 30}`)
	result := vm.Send(jsonClass, "primDecode:", []Value{input})
	if !IsDictionaryValue(result) {
		t.Fatalf("Expected dictionary, got %v", result)
	}
	dict := vm.registry.GetDictionaryObject(result)
	if len(dict.Keys) != 2 {
		t.Fatalf("Expected 2 keys, got %d", len(dict.Keys))
	}
	// Check name
	nameKey := vm.registry.NewStringValue("name")
	nameHash := hashValue(vm.registry, nameKey)
	nameVal, ok := dict.Data[nameHash]
	if !ok {
		t.Fatal("Missing 'name' key")
	}
	assertStringResult(t, vm, nameVal, "Alice")

	// Check age
	ageKey := vm.registry.NewStringValue("age")
	ageHash := hashValue(vm.registry, ageKey)
	ageVal, ok := dict.Data[ageHash]
	if !ok {
		t.Fatal("Missing 'age' key")
	}
	if !ageVal.IsSmallInt() || ageVal.SmallInt() != 30 {
		t.Errorf("age = %v, want 30", ageVal)
	}
}

func TestJsonDecodeNested(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	input := vm.registry.NewStringValue(`{"items": [1, {"nested": true}]}`)
	result := vm.Send(jsonClass, "primDecode:", []Value{input})
	if !IsDictionaryValue(result) {
		t.Fatalf("Expected dictionary, got %v", result)
	}
	dict := vm.registry.GetDictionaryObject(result)
	itemsKey := vm.registry.NewStringValue("items")
	itemsHash := hashValue(vm.registry, itemsKey)
	items, ok := dict.Data[itemsHash]
	if !ok {
		t.Fatal("Missing 'items' key")
	}
	if !items.IsObject() {
		t.Fatalf("Expected array for 'items', got %v", items)
	}
	arr := ObjectFromValue(items)
	if arr.NumSlots() != 2 {
		t.Fatalf("Expected 2 items, got %d", arr.NumSlots())
	}
	// First element is integer
	if el := arr.GetSlot(0); !el.IsSmallInt() || el.SmallInt() != 1 {
		t.Errorf("items[0] = %v, want 1", el)
	}
	// Second element is nested dict
	nested := arr.GetSlot(1)
	if !IsDictionaryValue(nested) {
		t.Errorf("items[1] expected dictionary, got %v", nested)
	}
}

func TestJsonDecodeUnicode(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	input := vm.registry.NewStringValue(`"こんにちは 🌍"`)
	result := vm.Send(jsonClass, "primDecode:", []Value{input})
	assertStringResult(t, vm, result, "こんにちは 🌍")
}

func TestJsonDecodeSpecialChars(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	input := vm.registry.NewStringValue(`"line1\nline2\ttab"`)
	result := vm.Send(jsonClass, "primDecode:", []Value{input})
	assertStringResult(t, vm, result, "line1\nline2\ttab")
}

func TestJsonDecodeEmptyArray(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	input := vm.registry.NewStringValue("[]")
	result := vm.Send(jsonClass, "primDecode:", []Value{input})
	if !result.IsObject() {
		t.Fatalf("Expected array object, got %v", result)
	}
	obj := ObjectFromValue(result)
	if obj.NumSlots() != 0 {
		t.Errorf("Expected 0 elements, got %d", obj.NumSlots())
	}
}

func TestJsonDecodeEmptyObject(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	input := vm.registry.NewStringValue("{}")
	result := vm.Send(jsonClass, "primDecode:", []Value{input})
	if !IsDictionaryValue(result) {
		t.Fatalf("Expected dictionary, got %v", result)
	}
	dict := vm.registry.GetDictionaryObject(result)
	if len(dict.Keys) != 0 {
		t.Errorf("Expected 0 keys, got %d", len(dict.Keys))
	}
}

// ---------------------------------------------------------------------------
// Round-trip tests
// ---------------------------------------------------------------------------

func TestJsonRoundTripInteger(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	original := FromSmallInt(12345)
	encoded := vm.Send(jsonClass, "primEncode:", []Value{original})
	decoded := vm.Send(jsonClass, "primDecode:", []Value{encoded})
	if !decoded.IsSmallInt() || decoded.SmallInt() != 12345 {
		t.Errorf("Round-trip 12345: got %v", decoded)
	}
}

func TestJsonRoundTripFloat(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	original := FromFloat64(2.718)
	encoded := vm.Send(jsonClass, "primEncode:", []Value{original})
	decoded := vm.Send(jsonClass, "primDecode:", []Value{encoded})
	if !decoded.IsFloat() || decoded.Float64() != 2.718 {
		t.Errorf("Round-trip 2.718: got %v", decoded)
	}
}

func TestJsonRoundTripString(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	original := vm.registry.NewStringValue("hello \"world\"")
	encoded := vm.Send(jsonClass, "primEncode:", []Value{original})
	decoded := vm.Send(jsonClass, "primDecode:", []Value{encoded})
	assertStringResult(t, vm, decoded, "hello \"world\"")
}

func TestJsonRoundTripNil(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	encoded := vm.Send(jsonClass, "primEncode:", []Value{Nil})
	decoded := vm.Send(jsonClass, "primDecode:", []Value{encoded})
	if decoded != Nil {
		t.Errorf("Round-trip nil: got %v, want Nil", decoded)
	}
}

func TestJsonRoundTripBoolean(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	for _, b := range []Value{True, False} {
		encoded := vm.Send(jsonClass, "primEncode:", []Value{b})
		decoded := vm.Send(jsonClass, "primDecode:", []Value{encoded})
		if decoded != b {
			t.Errorf("Round-trip boolean %v: got %v", b, decoded)
		}
	}
}

func TestJsonRoundTripArray(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	original := vm.NewArrayWithElements([]Value{
		FromSmallInt(1),
		vm.registry.NewStringValue("two"),
		True,
		Nil,
	})
	encoded := vm.Send(jsonClass, "primEncode:", []Value{original})
	decoded := vm.Send(jsonClass, "primDecode:", []Value{encoded})
	if !decoded.IsObject() {
		t.Fatalf("Expected array, got %v", decoded)
	}
	arr := ObjectFromValue(decoded)
	if arr.NumSlots() != 4 {
		t.Fatalf("Expected 4 elements, got %d", arr.NumSlots())
	}
	if el := arr.GetSlot(0); !el.IsSmallInt() || el.SmallInt() != 1 {
		t.Errorf("[0] = %v, want 1", el)
	}
	assertStringResult(t, vm, arr.GetSlot(1), "two")
	if arr.GetSlot(2) != True {
		t.Errorf("[2] = %v, want True", arr.GetSlot(2))
	}
	if arr.GetSlot(3) != Nil {
		t.Errorf("[3] = %v, want Nil", arr.GetSlot(3))
	}
}

// ---------------------------------------------------------------------------
// Error handling tests
// ---------------------------------------------------------------------------

func TestJsonDecodeMalformed(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	input := vm.registry.NewStringValue("{invalid json}")

	defer func() {
		r := recover()
		if r == nil {
			t.Fatal("Expected JsonParseError, got no error")
		}
		sig, ok := r.(SignaledException)
		if !ok {
			t.Fatalf("Expected SignaledException, got %T: %v", r, r)
		}
		if sig.Object.ExceptionClass.Name != "JsonParseError" {
			t.Errorf("Expected JsonParseError, got %s", sig.Object.ExceptionClass.Name)
		}
	}()

	vm.Send(jsonClass, "primDecode:", []Value{input})
}

func TestJsonDecodeNonString(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))

	defer func() {
		r := recover()
		if r == nil {
			t.Fatal("Expected JsonParseError, got no error")
		}
		sig, ok := r.(SignaledException)
		if !ok {
			t.Fatalf("Expected SignaledException, got %T: %v", r, r)
		}
		if sig.Object.ExceptionClass.Name != "JsonParseError" {
			t.Errorf("Expected JsonParseError, got %s", sig.Object.ExceptionClass.Name)
		}
	}()

	vm.Send(jsonClass, "primDecode:", []Value{FromSmallInt(42)})
}

// ---------------------------------------------------------------------------
// JsonReader streaming tests
// ---------------------------------------------------------------------------

func TestJsonReaderMultipleValues(t *testing.T) {
	vm := NewVM()
	readerClass := vm.classValue(vm.Classes.Lookup("JsonReader"))
	input := vm.registry.NewStringValue(`42 "hello" [1,2]`)
	reader := vm.Send(readerClass, "primNew:", []Value{input})
	if reader == Nil {
		t.Fatal("JsonReader new: returned nil")
	}

	// Read first value
	v1 := vm.Send(reader, "primNext", nil)
	if !v1.IsSmallInt() || v1.SmallInt() != 42 {
		t.Errorf("First value = %v, want 42", v1)
	}

	// Read second value
	v2 := vm.Send(reader, "primNext", nil)
	assertStringResult(t, vm, v2, "hello")

	// Read third value
	v3 := vm.Send(reader, "primNext", nil)
	if !v3.IsObject() {
		t.Fatalf("Third value expected array, got %v", v3)
	}

	// Read past end
	v4 := vm.Send(reader, "primNext", nil)
	if v4 != Nil {
		t.Errorf("Past-end read = %v, want Nil", v4)
	}
}

func TestJsonReaderHasMore(t *testing.T) {
	vm := NewVM()
	readerClass := vm.classValue(vm.Classes.Lookup("JsonReader"))
	input := vm.registry.NewStringValue(`1 2`)
	reader := vm.Send(readerClass, "primNew:", []Value{input})

	// Before reading
	if vm.Send(reader, "primHasMore", nil) != True {
		t.Error("Expected hasMore to be true before reading")
	}

	vm.Send(reader, "primNext", nil) // read 1
	vm.Send(reader, "primNext", nil) // read 2

	// After reading all
	if vm.Send(reader, "primHasMore", nil) != False {
		t.Error("Expected hasMore to be false after reading all")
	}
}

// ---------------------------------------------------------------------------
// JsonWriter streaming tests
// ---------------------------------------------------------------------------

func TestJsonWriterBasic(t *testing.T) {
	vm := NewVM()
	writerClass := vm.classValue(vm.Classes.Lookup("JsonWriter"))
	writer := vm.Send(writerClass, "primNew", nil)
	if writer == Nil {
		t.Fatal("JsonWriter new returned nil")
	}

	vm.Send(writer, "primWrite:", []Value{FromSmallInt(42)})
	contents := vm.Send(writer, "primContents", nil)
	assertStringResult(t, vm, contents, "42")
}

func TestJsonWriterMultiple(t *testing.T) {
	vm := NewVM()
	writerClass := vm.classValue(vm.Classes.Lookup("JsonWriter"))
	writer := vm.Send(writerClass, "primNew", nil)

	vm.Send(writer, "primWrite:", []Value{FromSmallInt(1)})
	vm.Send(writer, "primWrite:", []Value{FromSmallInt(2)})

	contents := vm.Send(writer, "primContents", nil)
	if !IsStringValue(contents) {
		t.Fatalf("Expected string, got %v", contents)
	}
	got := vm.registry.GetStringContent(contents)
	// json.Encoder adds newlines between values
	if !strings.Contains(got, "1") || !strings.Contains(got, "2") {
		t.Errorf("Expected both values in output, got %q", got)
	}
}

func TestJsonWriterReset(t *testing.T) {
	vm := NewVM()
	writerClass := vm.classValue(vm.Classes.Lookup("JsonWriter"))
	writer := vm.Send(writerClass, "primNew", nil)

	vm.Send(writer, "primWrite:", []Value{FromSmallInt(42)})
	vm.Send(writer, "primReset", nil)

	contents := vm.Send(writer, "primContents", nil)
	assertStringResult(t, vm, contents, "")
}

func TestJsonWriterPretty(t *testing.T) {
	vm := NewVM()
	writerClass := vm.classValue(vm.Classes.Lookup("JsonWriter"))
	writer := vm.Send(writerClass, "primNewPretty", nil)

	arr := vm.NewArrayWithElements([]Value{FromSmallInt(1), FromSmallInt(2)})
	vm.Send(writer, "primWrite:", []Value{arr})

	contents := vm.Send(writer, "primContents", nil)
	if !IsStringValue(contents) {
		t.Fatalf("Expected string, got %v", contents)
	}
	got := vm.registry.GetStringContent(contents)
	if !strings.Contains(got, "  ") {
		t.Errorf("Expected indentation in pretty output, got %q", got)
	}
}

// ---------------------------------------------------------------------------
// Large payload test
// ---------------------------------------------------------------------------

func TestJsonLargePayload(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))

	// Build an array of 100 dictionaries
	elems := make([]Value, 100)
	for i := 0; i < 100; i++ {
		dict := vm.registry.NewDictionaryValue()
		d := vm.registry.GetDictionaryObject(dict)
		key := vm.registry.NewStringValue("index")
		val := FromSmallInt(int64(i))
		h := hashValue(vm.registry, key)
		d.Data[h] = val
		d.Keys[h] = key
		elems[i] = dict
	}
	arr := vm.NewArrayWithElements(elems)

	// Encode and decode
	encoded := vm.Send(jsonClass, "primEncode:", []Value{arr})
	if !IsStringValue(encoded) {
		t.Fatal("Encode of large payload failed")
	}
	decoded := vm.Send(jsonClass, "primDecode:", []Value{encoded})
	if !decoded.IsObject() {
		t.Fatal("Decode of large payload failed")
	}
	obj := ObjectFromValue(decoded)
	if obj.NumSlots() != 100 {
		t.Errorf("Expected 100 elements, got %d", obj.NumSlots())
	}
}

// ---------------------------------------------------------------------------
// Helper
// ---------------------------------------------------------------------------

func assertStringResult(t *testing.T, vm *VM, v Value, expected string) {
	t.Helper()
	if !IsStringValue(v) {
		t.Fatalf("Expected string value, got %v", v)
	}
	got := vm.registry.GetStringContent(v)
	if got != expected {
		t.Errorf("Got %q, want %q", got, expected)
	}
}
