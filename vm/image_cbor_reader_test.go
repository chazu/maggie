package vm

import (
	"bytes"
	"math/big"
	"testing"

	"github.com/fxamacker/cbor/v2"
)

// ---------------------------------------------------------------------------
// TestCborDecodeImageValue: subtests for all value types
// ---------------------------------------------------------------------------

func TestCborDecodeImageValue(t *testing.T) {
	vm := NewVM()
	dec := NewImageDecoder()
	dec.registry = vm.registry

	t.Run("nil", func(t *testing.T) {
		raw, _ := cborSerialEncMode.Marshal(nil)
		val, err := decodeImageValue(vm, dec, raw)
		if err != nil {
			t.Fatal(err)
		}
		if val != Nil {
			t.Fatalf("expected Nil, got %v", val)
		}
	})

	t.Run("true", func(t *testing.T) {
		raw, _ := cborSerialEncMode.Marshal(true)
		val, err := decodeImageValue(vm, dec, raw)
		if err != nil {
			t.Fatal(err)
		}
		if val != True {
			t.Fatalf("expected True, got %v", val)
		}
	})

	t.Run("false", func(t *testing.T) {
		raw, _ := cborSerialEncMode.Marshal(false)
		val, err := decodeImageValue(vm, dec, raw)
		if err != nil {
			t.Fatal(err)
		}
		if val != False {
			t.Fatalf("expected False, got %v", val)
		}
	})

	t.Run("positive_int", func(t *testing.T) {
		raw, _ := cborSerialEncMode.Marshal(42)
		val, err := decodeImageValue(vm, dec, raw)
		if err != nil {
			t.Fatal(err)
		}
		if !val.IsSmallInt() || val.SmallInt() != 42 {
			t.Fatalf("expected SmallInt(42), got %v", val)
		}
	})

	t.Run("negative_int", func(t *testing.T) {
		raw, _ := cborSerialEncMode.Marshal(-7)
		val, err := decodeImageValue(vm, dec, raw)
		if err != nil {
			t.Fatal(err)
		}
		if !val.IsSmallInt() || val.SmallInt() != -7 {
			t.Fatalf("expected SmallInt(-7), got %v", val)
		}
	})

	t.Run("float64", func(t *testing.T) {
		raw, _ := cborSerialEncMode.Marshal(3.14)
		val, err := decodeImageValue(vm, dec, raw)
		if err != nil {
			t.Fatal(err)
		}
		if !val.IsFloat() || val.Float64() != 3.14 {
			t.Fatalf("expected Float64(3.14), got %v", val)
		}
	})

	t.Run("string", func(t *testing.T) {
		raw, _ := cborSerialEncMode.Marshal("hello world")
		val, err := decodeImageValue(vm, dec, raw)
		if err != nil {
			t.Fatal(err)
		}
		if !IsStringValue(val) {
			t.Fatal("expected string value")
		}
		content := vm.registry.GetStringContent(val)
		if content != "hello world" {
			t.Fatalf("expected 'hello world', got %q", content)
		}
	})

	t.Run("symbol_ref", func(t *testing.T) {
		symID := vm.Symbols.Intern("testSymbol")
		dec.SetSymbol(0, symID)

		raw, _ := cborSerialEncMode.Marshal(cbor.Tag{Number: imgTagSymbolRef, Content: uint64(0)})
		val, err := decodeImageValue(vm, dec, raw)
		if err != nil {
			t.Fatal(err)
		}
		if !val.IsSymbol() || val.SymbolID() != symID {
			t.Fatalf("expected symbol ID %d, got %v", symID, val)
		}
	})

	t.Run("class_ref", func(t *testing.T) {
		cls := vm.ObjectClass
		dec.AddClass(cls)

		raw, _ := cborSerialEncMode.Marshal(cbor.Tag{Number: imgTagClassRef, Content: uint64(0)})
		val, err := decodeImageValue(vm, dec, raw)
		if err != nil {
			t.Fatal(err)
		}
		if !isClassValue(val) {
			t.Fatal("expected class value")
		}
	})

	t.Run("object_ref", func(t *testing.T) {
		obj := NewObject(vm.ObjectClass.VTable, 0)
		dec.AddObject(obj)

		raw, _ := cborSerialEncMode.Marshal(cbor.Tag{Number: imgTagValueRef, Content: uint64(0)})
		val, err := decodeImageValue(vm, dec, raw)
		if err != nil {
			t.Fatal(err)
		}
		if !val.IsObject() {
			t.Fatal("expected object value")
		}
	})

	t.Run("character", func(t *testing.T) {
		raw, _ := cborSerialEncMode.Marshal(cbor.Tag{Number: cborTagCharacter, Content: uint32('A')})
		val, err := decodeImageValue(vm, dec, raw)
		if err != nil {
			t.Fatal(err)
		}
		if !IsCharacterValue(val) {
			t.Fatal("expected character value")
		}
		if GetCharacterCodePoint(val) != 'A' {
			t.Fatalf("expected 'A', got %c", GetCharacterCodePoint(val))
		}
	})

	t.Run("bigint_positive", func(t *testing.T) {
		n := new(big.Int)
		n.SetString("123456789012345678901234567890", 10)
		raw, _ := cborSerialEncMode.Marshal(cbor.Tag{Number: cborTagBigIntPositive, Content: n.Bytes()})
		val, err := decodeImageValue(vm, dec, raw)
		if err != nil {
			t.Fatal(err)
		}
		if !IsBigIntValue(val) {
			t.Fatal("expected BigInt value")
		}
		obj := vm.registry.GetBigInt(val)
		if obj.Value.Cmp(n) != 0 {
			t.Fatalf("expected %s, got %s", n.String(), obj.Value.String())
		}
	})

	t.Run("bigint_negative", func(t *testing.T) {
		n := new(big.Int)
		n.SetString("-999999999999999999999", 10)
		absN := new(big.Int).Abs(n)
		raw, _ := cborSerialEncMode.Marshal(cbor.Tag{Number: cborTagBigIntNegative, Content: absN.Bytes()})
		val, err := decodeImageValue(vm, dec, raw)
		if err != nil {
			t.Fatal(err)
		}
		if !IsBigIntValue(val) {
			t.Fatal("expected BigInt value")
		}
		obj := vm.registry.GetBigInt(val)
		if obj.Value.Cmp(n) != 0 {
			t.Fatalf("expected %s, got %s", n.String(), obj.Value.String())
		}
	})
}

// ---------------------------------------------------------------------------
// TestCborRoundTrip: write VM to CBOR, load into fresh VM, verify state
// ---------------------------------------------------------------------------

func TestCborRoundTrip(t *testing.T) {
	vm1 := NewVM()

	// Add a custom class with methods
	cls := NewClassWithInstVars("TestRTClass", vm1.ObjectClass, []string{"x", "y"})
	vm1.Classes.Register(cls)

	// Set some globals
	vm1.SetGlobal("TestRTClass", vm1.ClassValue(cls))
	vm1.SetGlobal("testInt", FromSmallInt(42))
	vm1.SetGlobal("testFloat", FromFloat64(2.718))
	vm1.SetGlobal("testTrue", True)
	vm1.SetGlobal("testNil", Nil)

	data, err := vm1.SaveImageCborBytes()
	if err != nil {
		t.Fatalf("SaveImageCborBytes: %v", err)
	}

	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes: %v", err)
	}

	// Verify globals
	if v := vm2.MustGlobal("testInt"); !v.IsSmallInt() || v.SmallInt() != 42 {
		t.Fatalf("testInt: expected 42, got %v", v)
	}
	if v := vm2.MustGlobal("testFloat"); !v.IsFloat() || v.Float64() != 2.718 {
		t.Fatalf("testFloat: expected 2.718, got %v", v)
	}
	if v := vm2.MustGlobal("testTrue"); v != True {
		t.Fatalf("testTrue: expected True, got %v", v)
	}
	if v := vm2.MustGlobal("testNil"); v != Nil {
		t.Fatalf("testNil: expected Nil, got %v", v)
	}

	// Verify class exists
	loaded := vm2.Classes.Lookup("TestRTClass")
	if loaded == nil {
		t.Fatal("TestRTClass not found in loaded VM")
	}
	if loaded.Name != "TestRTClass" {
		t.Fatalf("expected name TestRTClass, got %s", loaded.Name)
	}
}

// ---------------------------------------------------------------------------
// TestCborRoundTripClasses: verify class hierarchy preserved
// ---------------------------------------------------------------------------

func TestCborRoundTripClasses(t *testing.T) {
	vm1 := NewVM()

	parent := NewClass("ParentC", vm1.ObjectClass)
	vm1.Classes.Register(parent)
	child := NewClassWithInstVars("ChildC", parent, []string{"z"})
	vm1.Classes.Register(child)

	vm1.SetGlobal("ParentC", vm1.ClassValue(parent))
	vm1.SetGlobal("ChildC", vm1.ClassValue(child))

	data, err := vm1.SaveImageCborBytes()
	if err != nil {
		t.Fatalf("SaveImageCborBytes: %v", err)
	}

	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes: %v", err)
	}

	loadedChild := vm2.Classes.Lookup("ChildC")
	if loadedChild == nil {
		t.Fatal("ChildC not found")
	}
	if loadedChild.Superclass == nil {
		t.Fatal("ChildC has nil superclass")
	}
	if loadedChild.Superclass.Name != "ParentC" {
		t.Fatalf("expected superclass ParentC, got %s", loadedChild.Superclass.Name)
	}
	if len(loadedChild.InstVars) != 1 || loadedChild.InstVars[0] != "z" {
		t.Fatalf("expected instVars [z], got %v", loadedChild.InstVars)
	}
}

// ---------------------------------------------------------------------------
// TestCborRoundTripMethods: verify methods installed, can be looked up
// ---------------------------------------------------------------------------

func TestCborRoundTripMethods(t *testing.T) {
	vm1 := NewVM()

	cls := NewClass("MethodTestClass", vm1.ObjectClass)
	vm1.Classes.Register(cls)
	vm1.SetGlobal("MethodTestClass", vm1.ClassValue(cls))

	// Create and install a method
	selID := vm1.Selectors.Intern("doSomething")
	method := &CompiledMethod{
		selector: selID,
		name:     "doSomething",
		class:    cls,
		Arity:    0,
		NumTemps: 0,
		Literals: []Value{FromSmallInt(99)},
		Bytecode: []byte{byte(OpPushLiteral), 0, byte(OpReturnTop)},
		Source:   "doSomething ^99",
	}
	cls.VTable.AddMethod(selID, method)

	data, err := vm1.SaveImageCborBytes()
	if err != nil {
		t.Fatalf("SaveImageCborBytes: %v", err)
	}

	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes: %v", err)
	}

	loadedCls := vm2.Classes.Lookup("MethodTestClass")
	if loadedCls == nil {
		t.Fatal("MethodTestClass not found")
	}

	// Look up method
	loadedSelID := vm2.Selectors.Intern("doSomething")
	m := loadedCls.VTable.Lookup(loadedSelID)
	if m == nil {
		t.Fatal("method doSomething not found on loaded class")
	}

	cm, ok := m.(*CompiledMethod)
	if !ok {
		t.Fatal("method is not a CompiledMethod")
	}
	if cm.name != "doSomething" {
		t.Fatalf("expected name doSomething, got %s", cm.name)
	}
	if cm.Source != "doSomething ^99" {
		t.Fatalf("expected source preserved, got %q", cm.Source)
	}
	if len(cm.Literals) != 1 || !cm.Literals[0].IsSmallInt() || cm.Literals[0].SmallInt() != 99 {
		t.Fatal("literal not preserved")
	}
}

// ---------------------------------------------------------------------------
// TestCborRoundTripObjects: verify objects and slot values
// ---------------------------------------------------------------------------

func TestCborRoundTripObjects(t *testing.T) {
	vm1 := NewVM()

	cls := NewClassWithInstVars("SlotObj", vm1.ObjectClass, []string{"a", "b"})
	vm1.Classes.Register(cls)
	vm1.SetGlobal("SlotObj", vm1.ClassValue(cls))

	obj := NewObject(cls.VTable, 2)
	obj.SetSlot(0, FromSmallInt(10))
	obj.SetSlot(1, FromSmallInt(20))
	vm1.SetGlobal("myObj", obj.ToValue())

	data, err := vm1.SaveImageCborBytes()
	if err != nil {
		t.Fatalf("SaveImageCborBytes: %v", err)
	}

	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes: %v", err)
	}

	val := vm2.MustGlobal("myObj")
	if !val.IsObject() {
		t.Fatal("myObj is not an object")
	}
	loadedObj := ObjectFromValue(val)
	slot0 := loadedObj.GetSlot(0)
	slot1 := loadedObj.GetSlot(1)
	if !slot0.IsSmallInt() || slot0.SmallInt() != 10 {
		t.Fatalf("slot 0: expected 10, got %v", slot0)
	}
	if !slot1.IsSmallInt() || slot1.SmallInt() != 20 {
		t.Fatalf("slot 1: expected 20, got %v", slot1)
	}
}

// ---------------------------------------------------------------------------
// TestCborRoundTripObjectCycles: two objects referencing each other
// ---------------------------------------------------------------------------

func TestCborRoundTripObjectCycles(t *testing.T) {
	vm1 := NewVM()

	cls := NewClassWithInstVars("CycleObj", vm1.ObjectClass, []string{"ref"})
	vm1.Classes.Register(cls)
	vm1.SetGlobal("CycleObj", vm1.ClassValue(cls))

	objA := NewObject(cls.VTable, 1)
	objB := NewObject(cls.VTable, 1)
	objA.SetSlot(0, objB.ToValue())
	objB.SetSlot(0, objA.ToValue())
	vm1.SetGlobal("cycleA", objA.ToValue())
	vm1.SetGlobal("cycleB", objB.ToValue())

	data, err := vm1.SaveImageCborBytes()
	if err != nil {
		t.Fatalf("SaveImageCborBytes: %v", err)
	}

	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes: %v", err)
	}

	valA := vm2.MustGlobal("cycleA")
	valB := vm2.MustGlobal("cycleB")
	if !valA.IsObject() || !valB.IsObject() {
		t.Fatal("expected both to be objects")
	}

	loadedA := ObjectFromValue(valA)
	loadedB := ObjectFromValue(valB)

	// A.ref should point to B
	aRef := loadedA.GetSlot(0)
	if !aRef.IsObject() {
		t.Fatal("A.ref is not an object")
	}
	if ObjectFromValue(aRef) != loadedB {
		t.Fatal("A.ref does not point to B")
	}

	// B.ref should point to A
	bRef := loadedB.GetSlot(0)
	if !bRef.IsObject() {
		t.Fatal("B.ref is not an object")
	}
	if ObjectFromValue(bRef) != loadedA {
		t.Fatal("B.ref does not point to A")
	}
}

// ---------------------------------------------------------------------------
// TestCborRoundTripObjectIdentity: same object in two globals
// ---------------------------------------------------------------------------

func TestCborRoundTripObjectIdentity(t *testing.T) {
	vm1 := NewVM()

	cls := NewClass("IdObj", vm1.ObjectClass)
	vm1.Classes.Register(cls)
	vm1.SetGlobal("IdObj", vm1.ClassValue(cls))

	obj := NewObject(cls.VTable, 0)
	// Same object stored in two globals
	vm1.SetGlobal("ref1", obj.ToValue())
	vm1.SetGlobal("ref2", obj.ToValue())

	data, err := vm1.SaveImageCborBytes()
	if err != nil {
		t.Fatalf("SaveImageCborBytes: %v", err)
	}

	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes: %v", err)
	}

	val1 := vm2.MustGlobal("ref1")
	val2 := vm2.MustGlobal("ref2")
	if !val1.IsObject() || !val2.IsObject() {
		t.Fatal("expected both to be objects")
	}

	// Pointer equality: should be the same object
	if ObjectFromValue(val1) != ObjectFromValue(val2) {
		t.Fatal("expected pointer identity to be preserved")
	}
}

// ---------------------------------------------------------------------------
// TestCborRoundTripGlobals: various value types in globals
// ---------------------------------------------------------------------------

func TestCborRoundTripGlobals(t *testing.T) {
	vm1 := NewVM()

	symID := vm1.Symbols.Intern("testSym")

	vm1.SetGlobal("gNil", Nil)
	vm1.SetGlobal("gTrue", True)
	vm1.SetGlobal("gFalse", False)
	vm1.SetGlobal("gInt", FromSmallInt(-123))
	vm1.SetGlobal("gFloat", FromFloat64(1.5))
	vm1.SetGlobal("gSymbol", FromSymbolID(symID))
	vm1.SetGlobal("gChar", FromCharacter('Z'))
	vm1.SetGlobal("gString", vm1.registry.NewStringValue("hello"))

	data, err := vm1.SaveImageCborBytes()
	if err != nil {
		t.Fatalf("SaveImageCborBytes: %v", err)
	}

	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes: %v", err)
	}

	if vm2.MustGlobal("gNil") != Nil {
		t.Fatal("gNil")
	}
	if vm2.MustGlobal("gTrue") != True {
		t.Fatal("gTrue")
	}
	if vm2.MustGlobal("gFalse") != False {
		t.Fatal("gFalse")
	}
	v := vm2.MustGlobal("gInt")
	if !v.IsSmallInt() || v.SmallInt() != -123 {
		t.Fatalf("gInt: %v", v)
	}
	v = vm2.MustGlobal("gFloat")
	if !v.IsFloat() || v.Float64() != 1.5 {
		t.Fatalf("gFloat: %v", v)
	}
	v = vm2.MustGlobal("gSymbol")
	if !v.IsSymbol() {
		t.Fatal("gSymbol is not a symbol")
	}
	v = vm2.MustGlobal("gChar")
	if !IsCharacterValue(v) || GetCharacterCodePoint(v) != 'Z' {
		t.Fatalf("gChar: expected 'Z', got %v", v)
	}
	v = vm2.MustGlobal("gString")
	if !IsStringValue(v) || vm2.registry.GetStringContent(v) != "hello" {
		t.Fatal("gString")
	}
}

// ---------------------------------------------------------------------------
// TestCborRoundTripClassVars: class variables preserved
// ---------------------------------------------------------------------------

func TestCborRoundTripClassVars(t *testing.T) {
	vm1 := NewVM()

	cls := NewClass("CVClass", vm1.ObjectClass)
	cls.ClassVars = []string{"count"}
	vm1.Classes.Register(cls)
	vm1.SetGlobal("CVClass", vm1.ClassValue(cls))

	vm1.registry.SetClassVar(cls, "count", FromSmallInt(42))

	data, err := vm1.SaveImageCborBytes()
	if err != nil {
		t.Fatalf("SaveImageCborBytes: %v", err)
	}

	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes: %v", err)
	}

	loadedCls := vm2.Classes.Lookup("CVClass")
	if loadedCls == nil {
		t.Fatal("CVClass not found")
	}

	val := vm2.registry.GetClassVar(loadedCls, "count")
	if !val.IsSmallInt() || val.SmallInt() != 42 {
		t.Fatalf("class var count: expected 42, got %v", val)
	}
}

// ---------------------------------------------------------------------------
// TestCborRoundTripSelectorRemapping: selector IDs may differ between VMs
// ---------------------------------------------------------------------------

func TestCborRoundTripSelectorRemapping(t *testing.T) {
	vm1 := NewVM()

	cls := NewClass("RemapClass", vm1.ObjectClass)
	vm1.Classes.Register(cls)
	vm1.SetGlobal("RemapClass", vm1.ClassValue(cls))

	// Intern a selector and create a method that uses OpSend with it
	selID := vm1.Selectors.Intern("doWork")
	method := &CompiledMethod{
		selector: selID,
		name:     "doWork",
		class:    cls,
		Arity:    0,
		NumTemps: 0,
		Literals: []Value{},
		Bytecode: []byte{byte(OpPushSelf), byte(OpReturnTop)},
	}
	cls.VTable.AddMethod(selID, method)

	data, err := vm1.SaveImageCborBytes()
	if err != nil {
		t.Fatalf("SaveImageCborBytes: %v", err)
	}

	// Load into a fresh VM where selector IDs may differ
	vm2 := NewVM()
	// Pre-intern some selectors to shift IDs
	vm2.Selectors.Intern("extraSel1")
	vm2.Selectors.Intern("extraSel2")

	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes: %v", err)
	}

	loadedCls := vm2.Classes.Lookup("RemapClass")
	if loadedCls == nil {
		t.Fatal("RemapClass not found")
	}

	// The method should be findable using the vm2 selector ID
	vm2SelID := vm2.Selectors.Intern("doWork")
	m := loadedCls.VTable.Lookup(vm2SelID)
	if m == nil {
		t.Fatal("method doWork not found after selector remapping")
	}
}

// ---------------------------------------------------------------------------
// TestCborRoundTripContentHash: non-zero hashes preserved
// ---------------------------------------------------------------------------

func TestCborRoundTripContentHash(t *testing.T) {
	vm1 := NewVM()

	cls := NewClass("HashClass", vm1.ObjectClass)
	vm1.Classes.Register(cls)
	vm1.SetGlobal("HashClass", vm1.ClassValue(cls))

	selID := vm1.Selectors.Intern("hashMethod")
	method := &CompiledMethod{
		selector: selID,
		name:     "hashMethod",
		class:    cls,
		Arity:    0,
		NumTemps: 0,
		Literals: []Value{},
		Bytecode: []byte{byte(OpPushSelf), byte(OpReturnTop)},
	}
	// Set a non-zero content hash
	for i := range method.ContentHash {
		method.ContentHash[i] = byte(i + 1)
	}
	for i := range method.TypedHash {
		method.TypedHash[i] = byte(i + 100)
	}
	cls.VTable.AddMethod(selID, method)

	data, err := vm1.SaveImageCborBytes()
	if err != nil {
		t.Fatalf("SaveImageCborBytes: %v", err)
	}

	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes: %v", err)
	}

	loadedCls := vm2.Classes.Lookup("HashClass")
	if loadedCls == nil {
		t.Fatal("HashClass not found")
	}

	vm2SelID := vm2.Selectors.Intern("hashMethod")
	m := loadedCls.VTable.Lookup(vm2SelID)
	if m == nil {
		t.Fatal("hashMethod not found")
	}
	cm := m.(*CompiledMethod)

	for i := 0; i < 32; i++ {
		if cm.ContentHash[i] != byte(i+1) {
			t.Fatalf("ContentHash[%d]: expected %d, got %d", i, i+1, cm.ContentHash[i])
		}
		if cm.TypedHash[i] != byte(i+100) {
			t.Fatalf("TypedHash[%d]: expected %d, got %d", i, i+100, cm.TypedHash[i])
		}
	}
}

// ---------------------------------------------------------------------------
// TestCborRoundTripLiteralTypes: method with all literal types
// ---------------------------------------------------------------------------

func TestCborRoundTripLiteralTypes(t *testing.T) {
	vm1 := NewVM()

	cls := NewClass("LitClass", vm1.ObjectClass)
	vm1.Classes.Register(cls)
	vm1.SetGlobal("LitClass", vm1.ClassValue(cls))

	symID := vm1.Symbols.Intern("litSym")

	selID := vm1.Selectors.Intern("litMethod")
	method := &CompiledMethod{
		selector: selID,
		name:     "litMethod",
		class:    cls,
		Arity:    0,
		NumTemps: 0,
		Literals: []Value{
			FromSmallInt(1),
			FromFloat64(2.5),
			vm1.registry.NewStringValue("abc"),
			FromSymbolID(symID),
			FromCharacter('X'),
			True,
			False,
			Nil,
		},
		Bytecode: []byte{byte(OpPushSelf), byte(OpReturnTop)},
	}
	cls.VTable.AddMethod(selID, method)

	data, err := vm1.SaveImageCborBytes()
	if err != nil {
		t.Fatalf("SaveImageCborBytes: %v", err)
	}

	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes: %v", err)
	}

	loadedCls := vm2.Classes.Lookup("LitClass")
	vm2SelID := vm2.Selectors.Intern("litMethod")
	m := loadedCls.VTable.Lookup(vm2SelID)
	cm := m.(*CompiledMethod)

	if len(cm.Literals) != 8 {
		t.Fatalf("expected 8 literals, got %d", len(cm.Literals))
	}

	// SmallInt
	if !cm.Literals[0].IsSmallInt() || cm.Literals[0].SmallInt() != 1 {
		t.Fatal("literal 0")
	}
	// Float
	if !cm.Literals[1].IsFloat() || cm.Literals[1].Float64() != 2.5 {
		t.Fatal("literal 1")
	}
	// String
	if !IsStringValue(cm.Literals[2]) || vm2.registry.GetStringContent(cm.Literals[2]) != "abc" {
		t.Fatal("literal 2")
	}
	// Symbol
	if !cm.Literals[3].IsSymbol() {
		t.Fatal("literal 3 not a symbol")
	}
	// Character
	if !IsCharacterValue(cm.Literals[4]) || GetCharacterCodePoint(cm.Literals[4]) != 'X' {
		t.Fatal("literal 4")
	}
	// True, False, Nil
	if cm.Literals[5] != True {
		t.Fatal("literal 5")
	}
	if cm.Literals[6] != False {
		t.Fatal("literal 6")
	}
	if cm.Literals[7] != Nil {
		t.Fatal("literal 7")
	}
}

// ---------------------------------------------------------------------------
// TestCborFormatAutoDetection: binary vs CBOR routing
// ---------------------------------------------------------------------------

func TestCborFormatAutoDetection(t *testing.T) {
	t.Run("cbor_format", func(t *testing.T) {
		vm1 := NewVM()
		data, err := vm1.SaveImageCborBytes()
		if err != nil {
			t.Fatal(err)
		}

		vm2 := NewVM()
		if err := vm2.LoadImageFromBytes(data); err != nil {
			t.Fatalf("failed to load CBOR image: %v", err)
		}
	})

	t.Run("binary_format", func(t *testing.T) {
		vm1 := NewVM()
		var buf bytes.Buffer
		if err := vm1.SaveImageTo(&buf); err != nil {
			t.Fatal(err)
		}

		vm2 := NewVM()
		if err := vm2.LoadImageFromBytes(buf.Bytes()); err != nil {
			t.Fatalf("failed to load binary image: %v", err)
		}
	})

	t.Run("unrecognized_format", func(t *testing.T) {
		data := []byte{0x00, 0x01, 0x02, 0x03}
		vm1 := NewVM()
		err := vm1.LoadImageFromBytes(data)
		if err == nil {
			t.Fatal("expected error for unrecognized format")
		}
	})

	t.Run("too_short", func(t *testing.T) {
		data := []byte{0x01}
		vm1 := NewVM()
		err := vm1.LoadImageFromBytes(data)
		if err == nil {
			t.Fatal("expected error for data too short")
		}
	})
}

// ---------------------------------------------------------------------------
// TestCborCrossFormatEquivalence: binary and CBOR produce same VM state
// ---------------------------------------------------------------------------

func TestCborCrossFormatEquivalence(t *testing.T) {
	vm1 := NewVM()

	// Set up some state
	cls := NewClassWithInstVars("CrossFmtClass", vm1.ObjectClass, []string{"val"})
	vm1.Classes.Register(cls)
	vm1.SetGlobal("CrossFmtClass", vm1.ClassValue(cls))
	vm1.SetGlobal("testVal", FromSmallInt(999))
	vm1.SetGlobal("testStr", vm1.registry.NewStringValue("cross"))

	selID := vm1.Selectors.Intern("getValue")
	method := &CompiledMethod{
		selector: selID,
		name:     "getValue",
		class:    cls,
		Arity:    0,
		NumTemps: 0,
		Literals: []Value{FromSmallInt(7)},
		Bytecode: []byte{byte(OpPushLiteral), 0, byte(OpReturnTop)},
		Source:   "getValue ^7",
	}
	cls.VTable.AddMethod(selID, method)

	// Save as binary
	var binBuf bytes.Buffer
	if err := vm1.SaveImageTo(&binBuf); err != nil {
		t.Fatalf("SaveImageTo: %v", err)
	}

	// Save as CBOR
	cborData, err := vm1.SaveImageCborBytes()
	if err != nil {
		t.Fatalf("SaveImageCborBytes: %v", err)
	}

	// Load binary
	vmBin := NewVM()
	if err := vmBin.LoadImageFromBytes(binBuf.Bytes()); err != nil {
		t.Fatalf("load binary: %v", err)
	}

	// Load CBOR
	vmCbor := NewVM()
	if err := vmCbor.LoadImageFromBytes(cborData); err != nil {
		t.Fatalf("load CBOR: %v", err)
	}

	// Compare: both should have the same globals
	for _, name := range []string{"testVal", "testStr"} {
		vBin := vmBin.MustGlobal(name)
		vCbor := vmCbor.MustGlobal(name)
		if vBin != vCbor {
			// For strings, compare content
			if IsStringValue(vBin) && IsStringValue(vCbor) {
				sBin := vmBin.registry.GetStringContent(vBin)
				sCbor := vmCbor.registry.GetStringContent(vCbor)
				if sBin != sCbor {
					t.Fatalf("global %q string mismatch: %q vs %q", name, sBin, sCbor)
				}
			} else {
				t.Fatalf("global %q mismatch: %v vs %v", name, vBin, vCbor)
			}
		}
	}

	// Compare: both should have CrossFmtClass
	binCls := vmBin.Classes.Lookup("CrossFmtClass")
	cborCls := vmCbor.Classes.Lookup("CrossFmtClass")
	if binCls == nil || cborCls == nil {
		t.Fatal("CrossFmtClass missing from one of the loaded VMs")
	}
	if binCls.Name != cborCls.Name {
		t.Fatalf("class name mismatch: %s vs %s", binCls.Name, cborCls.Name)
	}
	if len(binCls.InstVars) != len(cborCls.InstVars) {
		t.Fatalf("instVars length mismatch")
	}

	// Compare: both should have getValue method
	binSelID := vmBin.Selectors.Intern("getValue")
	cborSelID := vmCbor.Selectors.Intern("getValue")
	binM := binCls.VTable.Lookup(binSelID)
	cborM := cborCls.VTable.Lookup(cborSelID)
	if binM == nil || cborM == nil {
		t.Fatal("getValue method missing")
	}
	binCM := binM.(*CompiledMethod)
	cborCM := cborM.(*CompiledMethod)
	if binCM.name != cborCM.name {
		t.Fatalf("method name mismatch: %s vs %s", binCM.name, cborCM.name)
	}
	if binCM.Source != cborCM.Source {
		t.Fatalf("method source mismatch: %q vs %q", binCM.Source, cborCM.Source)
	}
	if binCM.Arity != cborCM.Arity {
		t.Fatalf("method arity mismatch: %d vs %d", binCM.Arity, cborCM.Arity)
	}
}

// ---------------------------------------------------------------------------
// TestCborRoundTripNamespacedClass: namespaced class preserved
// ---------------------------------------------------------------------------

func TestCborRoundTripNamespacedClass(t *testing.T) {
	vm1 := NewVM()

	cls := NewClassInNamespace("MyNS", "Widget", vm1.ObjectClass)
	vm1.Classes.Register(cls)
	vm1.SetGlobal("MyNS::Widget", vm1.ClassValue(cls))

	data, err := vm1.SaveImageCborBytes()
	if err != nil {
		t.Fatalf("SaveImageCborBytes: %v", err)
	}

	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes: %v", err)
	}

	loaded := vm2.Classes.Lookup("MyNS::Widget")
	if loaded == nil {
		t.Fatal("MyNS::Widget not found")
	}
	if loaded.Name != "Widget" {
		t.Fatalf("expected name Widget, got %s", loaded.Name)
	}
	if loaded.Namespace != "MyNS" {
		t.Fatalf("expected namespace MyNS, got %s", loaded.Namespace)
	}
}

// ---------------------------------------------------------------------------
// TestCborRoundTripDocString: class and method docstrings
// ---------------------------------------------------------------------------

func TestCborRoundTripDocString(t *testing.T) {
	vm1 := NewVM()

	cls := NewClass("DocClass", vm1.ObjectClass)
	cls.DocString = "This is a documented class."
	vm1.Classes.Register(cls)
	vm1.SetGlobal("DocClass", vm1.ClassValue(cls))

	selID := vm1.Selectors.Intern("documented")
	method := &CompiledMethod{
		selector:  selID,
		name:      "documented",
		class:     cls,
		Arity:     0,
		NumTemps:  0,
		Literals:  []Value{},
		Bytecode:  []byte{byte(OpPushSelf), byte(OpReturnTop)},
		docString: "Returns self.",
	}
	cls.VTable.AddMethod(selID, method)

	data, err := vm1.SaveImageCborBytes()
	if err != nil {
		t.Fatalf("SaveImageCborBytes: %v", err)
	}

	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes: %v", err)
	}

	loaded := vm2.Classes.Lookup("DocClass")
	if loaded == nil {
		t.Fatal("DocClass not found")
	}
	if loaded.DocString != "This is a documented class." {
		t.Fatalf("class docstring: %q", loaded.DocString)
	}

	vm2SelID := vm2.Selectors.Intern("documented")
	m := loaded.VTable.Lookup(vm2SelID)
	cm := m.(*CompiledMethod)
	if cm.docString != "Returns self." {
		t.Fatalf("method docstring: %q", cm.docString)
	}
}

// ---------------------------------------------------------------------------
// TestCborRoundTripBlocks: blocks with literals and source maps
// ---------------------------------------------------------------------------

func TestCborRoundTripBlocks(t *testing.T) {
	vm1 := NewVM()

	cls := NewClass("BlockClass", vm1.ObjectClass)
	vm1.Classes.Register(cls)
	vm1.SetGlobal("BlockClass", vm1.ClassValue(cls))

	selID := vm1.Selectors.Intern("withBlock")
	block := &BlockMethod{
		Arity:       1,
		NumTemps:    2,
		NumCaptures: 1,
		Literals:    []Value{FromSmallInt(5)},
		Bytecode:    []byte{byte(OpPushLiteral), 0, byte(OpReturnTop)},
		SourceMap: []SourceLoc{
			{Offset: 0, Line: 1, Column: 1},
			{Offset: 2, Line: 1, Column: 5},
		},
	}
	method := &CompiledMethod{
		selector: selID,
		name:     "withBlock",
		class:    cls,
		Arity:    0,
		NumTemps: 0,
		Literals: []Value{},
		Bytecode: []byte{byte(OpPushSelf), byte(OpReturnTop)},
		Blocks:   []*BlockMethod{block},
	}
	block.Outer = method
	cls.VTable.AddMethod(selID, method)

	data, err := vm1.SaveImageCborBytes()
	if err != nil {
		t.Fatalf("SaveImageCborBytes: %v", err)
	}

	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes: %v", err)
	}

	loadedCls := vm2.Classes.Lookup("BlockClass")
	vm2SelID := vm2.Selectors.Intern("withBlock")
	m := loadedCls.VTable.Lookup(vm2SelID)
	cm := m.(*CompiledMethod)

	if len(cm.Blocks) != 1 {
		t.Fatalf("expected 1 block, got %d", len(cm.Blocks))
	}

	b := cm.Blocks[0]
	if b.Arity != 1 {
		t.Fatalf("block arity: %d", b.Arity)
	}
	if b.NumTemps != 2 {
		t.Fatalf("block numTemps: %d", b.NumTemps)
	}
	if b.NumCaptures != 1 {
		t.Fatalf("block numCaptures: %d", b.NumCaptures)
	}
	if len(b.Literals) != 1 || !b.Literals[0].IsSmallInt() || b.Literals[0].SmallInt() != 5 {
		t.Fatal("block literal not preserved")
	}
	if len(b.SourceMap) != 2 {
		t.Fatalf("block sourceMap: %d entries", len(b.SourceMap))
	}
	if b.SourceMap[0].Line != 1 || b.SourceMap[0].Column != 1 {
		t.Fatal("block sourceMap[0] mismatch")
	}
	if b.Outer != cm {
		t.Fatal("block Outer not linked to method")
	}
}

// ---------------------------------------------------------------------------
// TestNewCborImageReaderInvalid: error cases for reader construction
// ---------------------------------------------------------------------------

func TestNewCborImageReaderInvalid(t *testing.T) {
	t.Run("empty_data", func(t *testing.T) {
		_, err := NewCborImageReader([]byte{})
		if err == nil {
			t.Fatal("expected error for empty data")
		}
	})

	t.Run("garbage_data", func(t *testing.T) {
		_, err := NewCborImageReader([]byte{0xFF, 0xFF, 0xFF, 0xFF})
		if err == nil {
			t.Fatal("expected error for garbage data")
		}
	})

	t.Run("wrong_tag", func(t *testing.T) {
		data, _ := cborSerialEncMode.Marshal(cbor.Tag{Number: 99999, Content: "hello"})
		_, err := NewCborImageReader(data)
		if err == nil {
			t.Fatal("expected error for wrong tag number")
		}
	})
}

// ---------------------------------------------------------------------------
// TestCborRoundTripSourceMap: source map entries preserved
// ---------------------------------------------------------------------------

func TestCborRoundTripSourceMap(t *testing.T) {
	vm1 := NewVM()

	cls := NewClass("SrcMapClass", vm1.ObjectClass)
	vm1.Classes.Register(cls)
	vm1.SetGlobal("SrcMapClass", vm1.ClassValue(cls))

	selID := vm1.Selectors.Intern("mapped")
	method := &CompiledMethod{
		selector: selID,
		name:     "mapped",
		class:    cls,
		Arity:    0,
		NumTemps: 0,
		Literals: []Value{},
		Bytecode: []byte{byte(OpPushSelf), byte(OpReturnTop)},
		SourceMap: []SourceLoc{
			{Offset: 0, Line: 1, Column: 1},
			{Offset: 1, Line: 2, Column: 3},
		},
	}
	cls.VTable.AddMethod(selID, method)

	data, err := vm1.SaveImageCborBytes()
	if err != nil {
		t.Fatal(err)
	}

	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatal(err)
	}

	loadedCls := vm2.Classes.Lookup("SrcMapClass")
	vm2SelID := vm2.Selectors.Intern("mapped")
	m := loadedCls.VTable.Lookup(vm2SelID)
	cm := m.(*CompiledMethod)

	if len(cm.SourceMap) != 2 {
		t.Fatalf("expected 2 source map entries, got %d", len(cm.SourceMap))
	}
	if cm.SourceMap[0].Offset != 0 || cm.SourceMap[0].Line != 1 || cm.SourceMap[0].Column != 1 {
		t.Fatalf("sourceMap[0]: %+v", cm.SourceMap[0])
	}
	if cm.SourceMap[1].Offset != 1 || cm.SourceMap[1].Line != 2 || cm.SourceMap[1].Column != 3 {
		t.Fatalf("sourceMap[1]: %+v", cm.SourceMap[1])
	}
}
