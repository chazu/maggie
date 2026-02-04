package vm

import (
	"testing"
)

func TestNewDictionaryValue(t *testing.T) {
	vm := NewVM()
	d := vm.registry.NewDictionaryValue()
	if !IsDictionaryValue(d) {
		t.Error("NewDictionaryValue did not create a dictionary value")
	}

	obj := vm.registry.GetDictionaryObject(d)
	if obj == nil {
		t.Fatal("GetDictionaryObject returned nil")
	}
	if len(obj.Data) != 0 {
		t.Errorf("New dictionary has %d entries, want 0", len(obj.Data))
	}
}

func TestIsDictionaryValueFalse(t *testing.T) {
	vm := NewVM()
	_ = vm // used for string creation below

	// Regular symbol should not be a dictionary
	sym := FromSymbolID(42)
	if IsDictionaryValue(sym) {
		t.Error("Regular symbol should not be identified as a dictionary")
	}

	// SmallInt should not be a dictionary
	if IsDictionaryValue(FromSmallInt(42)) {
		t.Error("SmallInt should not be identified as a dictionary")
	}

	// Nil should not be a dictionary
	if IsDictionaryValue(Nil) {
		t.Error("Nil should not be identified as a dictionary")
	}

	// String should not be a dictionary
	if IsDictionaryValue(vm.registry.NewStringValue("hello")) {
		t.Error("String should not be identified as a dictionary")
	}
}

func TestDictionaryNew(t *testing.T) {
	vm := NewVM()

	// Create dictionary via class-side new
	dictClass := vm.Symbols.SymbolValue("Dictionary")
	d := vm.Send(dictClass, "new", nil)

	if !IsDictionaryValue(d) {
		t.Error("Dictionary new did not return a dictionary")
	}
}

func TestDictionaryAtPut(t *testing.T) {
	vm := NewVM()
	d := vm.registry.NewDictionaryValue()

	// Put a value
	key := vm.registry.NewStringValue("foo")
	value := FromSmallInt(42)
	result := vm.Send(d, "at:put:", []Value{key, value})

	if result != value {
		t.Errorf("at:put: returned %v, want %v", result, value)
	}

	// Get the value back
	result = vm.Send(d, "at:", []Value{key})
	if result != value {
		t.Errorf("at: returned %v, want %v", result, value)
	}
}

func TestDictionaryAtMissing(t *testing.T) {
	vm := NewVM()
	d := vm.registry.NewDictionaryValue()

	// Get non-existent key
	result := vm.Send(d, "at:", []Value{vm.registry.NewStringValue("missing")})
	if result != Nil {
		t.Errorf("at: missing key returned %v, want Nil", result)
	}
}

func TestDictionaryAtIfAbsent(t *testing.T) {
	vm := NewVM()
	d := vm.registry.NewDictionaryValue()

	// Put a value
	vm.Send(d, "at:put:", []Value{vm.registry.NewStringValue("key"), FromSmallInt(100)})

	// Create a block that returns 99 (using small number to fit in int8)
	// OpPushInt8(99), OpBlockReturn
	blockMethod := &BlockMethod{
		Arity:    0,
		NumTemps: 0,
		Bytecode: []byte{0x14, 99, 0x73}, // OpPushInt8 99, OpBlockReturn
		Literals: nil,
	}
	block := vm.interpreter.createBlockValue(blockMethod, nil)

	// Get existing key - block should not be evaluated
	result := vm.Send(d, "at:ifAbsent:", []Value{vm.registry.NewStringValue("key"), block})
	if !result.IsSmallInt() || result.SmallInt() != 100 {
		t.Errorf("at:ifAbsent: with existing key returned %v, want 100", result)
	}

	// Get missing key - block should be evaluated
	result = vm.Send(d, "at:ifAbsent:", []Value{vm.registry.NewStringValue("missing"), block})
	if !result.IsSmallInt() || result.SmallInt() != 99 {
		t.Errorf("at:ifAbsent: with missing key returned %v, want 99", result)
	}
}

func TestDictionaryIncludesKey(t *testing.T) {
	vm := NewVM()
	d := vm.registry.NewDictionaryValue()

	key := vm.registry.NewStringValue("myKey")
	vm.Send(d, "at:put:", []Value{key, FromSmallInt(42)})

	// Key exists
	result := vm.Send(d, "includesKey:", []Value{key})
	if result != True {
		t.Errorf("includesKey: for existing key returned %v, want True", result)
	}

	// Key does not exist
	result = vm.Send(d, "includesKey:", []Value{vm.registry.NewStringValue("other")})
	if result != False {
		t.Errorf("includesKey: for missing key returned %v, want False", result)
	}
}

func TestDictionarySize(t *testing.T) {
	vm := NewVM()
	d := vm.registry.NewDictionaryValue()

	// Empty dictionary
	result := vm.Send(d, "size", nil)
	if !result.IsSmallInt() || result.SmallInt() != 0 {
		t.Errorf("size of empty dictionary returned %v, want 0", result)
	}

	// Add some entries
	vm.Send(d, "at:put:", []Value{vm.registry.NewStringValue("a"), FromSmallInt(1)})
	vm.Send(d, "at:put:", []Value{vm.registry.NewStringValue("b"), FromSmallInt(2)})
	vm.Send(d, "at:put:", []Value{vm.registry.NewStringValue("c"), FromSmallInt(3)})

	result = vm.Send(d, "size", nil)
	if !result.IsSmallInt() || result.SmallInt() != 3 {
		t.Errorf("size returned %v, want 3", result)
	}
}

func TestDictionaryIsEmpty(t *testing.T) {
	vm := NewVM()
	d := vm.registry.NewDictionaryValue()

	// Empty dictionary
	result := vm.Send(d, "isEmpty", nil)
	if result != True {
		t.Errorf("isEmpty of empty dictionary returned %v, want True", result)
	}

	// Add an entry
	vm.Send(d, "at:put:", []Value{vm.registry.NewStringValue("key"), FromSmallInt(42)})

	result = vm.Send(d, "isEmpty", nil)
	if result != False {
		t.Errorf("isEmpty of non-empty dictionary returned %v, want False", result)
	}
}

func TestDictionaryKeys(t *testing.T) {
	vm := NewVM()
	d := vm.registry.NewDictionaryValue()

	// Add entries
	vm.Send(d, "at:put:", []Value{vm.registry.NewStringValue("a"), FromSmallInt(1)})
	vm.Send(d, "at:put:", []Value{vm.registry.NewStringValue("b"), FromSmallInt(2)})

	result := vm.Send(d, "keys", nil)
	if !result.IsObject() {
		t.Fatal("keys did not return an object")
	}

	arr := ObjectFromValue(result)
	if arr == nil {
		t.Fatal("keys returned nil object")
	}
	if arr.NumSlots() != 2 {
		t.Errorf("keys returned array with %d elements, want 2", arr.NumSlots())
	}
}

func TestDictionaryValues(t *testing.T) {
	vm := NewVM()
	d := vm.registry.NewDictionaryValue()

	// Add entries
	vm.Send(d, "at:put:", []Value{vm.registry.NewStringValue("a"), FromSmallInt(10)})
	vm.Send(d, "at:put:", []Value{vm.registry.NewStringValue("b"), FromSmallInt(20)})

	result := vm.Send(d, "values", nil)
	if !result.IsObject() {
		t.Fatal("values did not return an object")
	}

	arr := ObjectFromValue(result)
	if arr == nil {
		t.Fatal("values returned nil object")
	}
	if arr.NumSlots() != 2 {
		t.Errorf("values returned array with %d elements, want 2", arr.NumSlots())
	}

	// Check the values are present (order may vary due to map iteration)
	foundValues := make(map[int64]bool)
	for i := 0; i < arr.NumSlots(); i++ {
		v := arr.GetSlot(i)
		if v.IsSmallInt() {
			foundValues[v.SmallInt()] = true
		}
	}
	if !foundValues[10] || !foundValues[20] {
		t.Errorf("values missing expected values 10 and 20, found: %v", foundValues)
	}
}

func TestDictionaryRemoveKey(t *testing.T) {
	vm := NewVM()
	d := vm.registry.NewDictionaryValue()

	key := vm.registry.NewStringValue("toRemove")
	vm.Send(d, "at:put:", []Value{key, FromSmallInt(42)})

	// Verify it exists
	result := vm.Send(d, "size", nil)
	if result.SmallInt() != 1 {
		t.Fatalf("Dictionary should have 1 entry, has %d", result.SmallInt())
	}

	// Remove it
	result = vm.Send(d, "removeKey:", []Value{key})
	if !result.IsSmallInt() || result.SmallInt() != 42 {
		t.Errorf("removeKey: returned %v, want 42", result)
	}

	// Verify it's gone
	result = vm.Send(d, "size", nil)
	if result.SmallInt() != 0 {
		t.Errorf("Dictionary should have 0 entries after removal, has %d", result.SmallInt())
	}

	// Verify at: returns nil
	result = vm.Send(d, "at:", []Value{key})
	if result != Nil {
		t.Errorf("at: after remove returned %v, want Nil", result)
	}
}

func TestDictionaryRemoveKeyMissing(t *testing.T) {
	vm := NewVM()
	d := vm.registry.NewDictionaryValue()

	// Remove non-existent key
	result := vm.Send(d, "removeKey:", []Value{vm.registry.NewStringValue("missing")})
	if result != Nil {
		t.Errorf("removeKey: of missing key returned %v, want Nil", result)
	}
}

func TestDictionaryRemoveKeyIfAbsent(t *testing.T) {
	vm := NewVM()
	d := vm.registry.NewDictionaryValue()

	key := vm.registry.NewStringValue("key")
	vm.Send(d, "at:put:", []Value{key, FromSmallInt(100)})

	// Create a block that returns 77
	blockMethod := &BlockMethod{
		Arity:    0,
		NumTemps: 0,
		Bytecode: []byte{0x14, 77, 0x73}, // OpPushInt8 77, OpBlockReturn
		Literals: nil,
	}
	block := vm.interpreter.createBlockValue(blockMethod, nil)

	// Remove existing key - block should not be evaluated
	result := vm.Send(d, "removeKey:ifAbsent:", []Value{key, block})
	if !result.IsSmallInt() || result.SmallInt() != 100 {
		t.Errorf("removeKey:ifAbsent: with existing key returned %v, want 100", result)
	}

	// Remove missing key - block should be evaluated
	result = vm.Send(d, "removeKey:ifAbsent:", []Value{vm.registry.NewStringValue("missing"), block})
	if !result.IsSmallInt() || result.SmallInt() != 77 {
		t.Errorf("removeKey:ifAbsent: with missing key returned %v, want 77", result)
	}
}

func TestDictionaryDo(t *testing.T) {
	vm := NewVM()
	d := vm.registry.NewDictionaryValue()

	// Add entries
	vm.Send(d, "at:put:", []Value{vm.registry.NewStringValue("a"), FromSmallInt(10)})
	vm.Send(d, "at:put:", []Value{vm.registry.NewStringValue("b"), FromSmallInt(20)})
	vm.Send(d, "at:put:", []Value{vm.registry.NewStringValue("c"), FromSmallInt(30)})

	// Create a block that returns nil: [:x | nil]
	// OpPushNil, OpBlockReturn
	blockMethod := &BlockMethod{
		Arity:    1,
		NumTemps: 0,
		Bytecode: []byte{0x10, 0x73}, // OpPushNil, OpBlockReturn
		Literals: nil,
	}
	block := vm.interpreter.createBlockValue(blockMethod, nil)

	// do: should return the receiver (the dictionary)
	result := vm.Send(d, "do:", []Value{block})
	if result != d {
		t.Errorf("do: returned %v, want the dictionary", result)
	}
}

func TestDictionaryKeysAndValuesDo(t *testing.T) {
	vm := NewVM()
	d := vm.registry.NewDictionaryValue()

	// Add entries
	vm.Send(d, "at:put:", []Value{vm.registry.NewStringValue("x"), FromSmallInt(1)})
	vm.Send(d, "at:put:", []Value{vm.registry.NewStringValue("y"), FromSmallInt(2)})

	// Create a block that takes 2 args and returns nil: [:k :v | nil]
	// OpPushNil, OpBlockReturn
	blockMethod := &BlockMethod{
		Arity:    2,
		NumTemps: 0,
		Bytecode: []byte{0x10, 0x73}, // OpPushNil, OpBlockReturn
		Literals: nil,
	}
	block := vm.interpreter.createBlockValue(blockMethod, nil)

	// keysAndValuesDo: should return the receiver (the dictionary)
	result := vm.Send(d, "keysAndValuesDo:", []Value{block})
	if result != d {
		t.Errorf("keysAndValuesDo: returned %v, want the dictionary", result)
	}
}

func TestDictionaryWithIntegerKeys(t *testing.T) {
	vm := NewVM()
	d := vm.registry.NewDictionaryValue()

	// Use integers as keys
	vm.Send(d, "at:put:", []Value{FromSmallInt(1), vm.registry.NewStringValue("one")})
	vm.Send(d, "at:put:", []Value{FromSmallInt(2), vm.registry.NewStringValue("two")})
	vm.Send(d, "at:put:", []Value{FromSmallInt(3), vm.registry.NewStringValue("three")})

	result := vm.Send(d, "at:", []Value{FromSmallInt(2)})
	if vm.registry.GetStringContent(result) != "two" {
		t.Errorf("at: 2 returned %q, want %q", vm.registry.GetStringContent(result), "two")
	}

	result = vm.Send(d, "size", nil)
	if result.SmallInt() != 3 {
		t.Errorf("size returned %d, want 3", result.SmallInt())
	}
}

func TestDictionaryWithSymbolKeys(t *testing.T) {
	vm := NewVM()
	d := vm.registry.NewDictionaryValue()

	// Use symbols as keys
	sym1 := vm.Symbols.SymbolValue("key1")
	sym2 := vm.Symbols.SymbolValue("key2")

	vm.Send(d, "at:put:", []Value{sym1, FromSmallInt(100)})
	vm.Send(d, "at:put:", []Value{sym2, FromSmallInt(200)})

	result := vm.Send(d, "at:", []Value{sym1})
	if !result.IsSmallInt() || result.SmallInt() != 100 {
		t.Errorf("at: sym1 returned %v, want 100", result)
	}

	result = vm.Send(d, "at:", []Value{sym2})
	if !result.IsSmallInt() || result.SmallInt() != 200 {
		t.Errorf("at: sym2 returned %v, want 200", result)
	}
}

func TestDictionaryOverwriteValue(t *testing.T) {
	vm := NewVM()
	d := vm.registry.NewDictionaryValue()

	key := vm.registry.NewStringValue("key")

	// Set initial value
	vm.Send(d, "at:put:", []Value{key, FromSmallInt(1)})

	// Overwrite with new value
	vm.Send(d, "at:put:", []Value{key, FromSmallInt(2)})

	// Check value was overwritten
	result := vm.Send(d, "at:", []Value{key})
	if !result.IsSmallInt() || result.SmallInt() != 2 {
		t.Errorf("at: returned %v after overwrite, want 2", result)
	}

	// Size should still be 1
	result = vm.Send(d, "size", nil)
	if result.SmallInt() != 1 {
		t.Errorf("size after overwrite is %d, want 1", result.SmallInt())
	}
}

// TestDictionaryNestedProtoStyle tests that nested dictionaries created via
// DictionaryAtPut (simulating protoToDictionary) work with Send-based at:/keys
func TestDictionaryNestedProtoStyle(t *testing.T) {
	vm := NewVM()

	// Simulate what protoToDictionary does for widget_id: {id: "some-uuid"}
	innerDict := vm.registry.NewDictionaryValue()
	idKey := vm.Symbols.SymbolValue("id")
	idVal := vm.registry.NewStringValue("test-uuid-1234")
	vm.DictionaryAtPut(innerDict, idKey, idVal)

	// Verify the inner dict is a proper dictionary
	if !IsDictionaryValue(innerDict) {
		t.Fatal("inner dict is not a dictionary value")
	}

	// Verify ClassFor returns DictionaryClass
	class := vm.ClassFor(innerDict)
	if class != vm.DictionaryClass {
		t.Errorf("ClassFor(innerDict) = %v, want DictionaryClass", class.Name)
	}

	// Test keys via Send
	keysResult := vm.Send(innerDict, "keys", nil)
	if keysResult == Nil {
		t.Fatal("keys returned Nil")
	}
	if !keysResult.IsObject() {
		t.Fatalf("keys returned non-object: IsSmallInt=%v IsSymbol=%v", keysResult.IsSmallInt(), keysResult.IsSymbol())
	}
	keysArr := ObjectFromValue(keysResult)
	if keysArr == nil {
		t.Fatal("keys returned nil object")
	}
	if keysArr.NumSlots() != 1 {
		t.Errorf("keys has %d elements, want 1", keysArr.NumSlots())
	}

	// Test at: with symbol key via Send
	lookupKey := vm.Symbols.SymbolValue("id")
	result := vm.Send(innerDict, "at:", []Value{lookupKey})
	if result == Nil {
		t.Fatal("at: #id returned Nil on nested dict")
	}
	if !IsStringValue(result) {
		t.Fatalf("at: #id returned non-string, IsSymbol=%v IsSmallInt=%v", result.IsSymbol(), result.IsSmallInt())
	}
	if vm.registry.GetStringContent(result) != "test-uuid-1234" {
		t.Errorf("at: #id returned %q, want %q", vm.registry.GetStringContent(result), "test-uuid-1234")
	}

	// Test size
	sizeResult := vm.Send(innerDict, "size", nil)
	if !sizeResult.IsSmallInt() || sizeResult.SmallInt() != 1 {
		t.Errorf("size = %v, want 1", sizeResult)
	}

	// Now simulate full proto structure: outer dict with nested dict as value
	outerDict := vm.registry.NewDictionaryValue()
	vm.DictionaryAtPut(outerDict, vm.Symbols.SymbolValue("widget_id"), innerDict)
	vm.DictionaryAtPut(outerDict, vm.Symbols.SymbolValue("type"), vm.Symbols.SymbolValue("WIDGET_DONE"))

	// Get the nested dict back
	widgetIdKey := vm.Symbols.SymbolValue("widget_id")
	nestedResult := vm.Send(outerDict, "at:", []Value{widgetIdKey})
	if nestedResult == Nil {
		t.Fatal("at: #widget_id returned Nil")
	}
	if !IsDictionaryValue(nestedResult) {
		t.Fatalf("at: #widget_id returned non-dict, IsDictionary=%v IsSymbol=%v", IsDictionaryValue(nestedResult), nestedResult.IsSymbol())
	}

	// Now lookup #id on the retrieved nested dict
	idResult := vm.Send(nestedResult, "at:", []Value{vm.Symbols.SymbolValue("id")})
	if idResult == Nil {
		t.Fatal("at: #id on retrieved nested dict returned Nil")
	}
	if vm.registry.GetStringContent(idResult) != "test-uuid-1234" {
		t.Errorf("at: #id on nested dict = %q, want %q", vm.registry.GetStringContent(idResult), "test-uuid-1234")
	}
}

func TestDictionaryClassAssignment(t *testing.T) {
	vm := NewVM()

	d := vm.registry.NewDictionaryValue()
	class := vm.ClassFor(d)

	// Dictionary values should be classified as DictionaryClass
	if class != vm.DictionaryClass {
		t.Errorf("Dictionary class is %v, want DictionaryClass", class.Name)
	}
}
