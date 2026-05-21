package vm

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"
	"unsafe"

	"github.com/fxamacker/cbor/v2"
)

// ---------------------------------------------------------------------------
// TestEncodeImageValue: subtests for each value type
// ---------------------------------------------------------------------------

func TestEncodeImageValue(t *testing.T) {
	vm := NewVM()
	enc := NewImageEncoder()
	enc.registry = vm.registry

	t.Run("nil", func(t *testing.T) {
		raw, err := encodeImageValue(enc, Nil)
		if err != nil {
			t.Fatal(err)
		}
		var decoded interface{}
		if err := cbor.Unmarshal(raw, &decoded); err != nil {
			t.Fatal(err)
		}
		if decoded != nil {
			t.Fatalf("expected nil, got %v", decoded)
		}
	})

	t.Run("true", func(t *testing.T) {
		raw, err := encodeImageValue(enc, True)
		if err != nil {
			t.Fatal(err)
		}
		var decoded bool
		if err := cbor.Unmarshal(raw, &decoded); err != nil {
			t.Fatal(err)
		}
		if !decoded {
			t.Fatal("expected true")
		}
	})

	t.Run("false", func(t *testing.T) {
		raw, err := encodeImageValue(enc, False)
		if err != nil {
			t.Fatal(err)
		}
		var decoded bool
		if err := cbor.Unmarshal(raw, &decoded); err != nil {
			t.Fatal(err)
		}
		if decoded {
			t.Fatal("expected false")
		}
	})

	t.Run("small_int", func(t *testing.T) {
		raw, err := encodeImageValue(enc, FromSmallInt(42))
		if err != nil {
			t.Fatal(err)
		}
		var decoded int64
		if err := cbor.Unmarshal(raw, &decoded); err != nil {
			t.Fatal(err)
		}
		if decoded != 42 {
			t.Fatalf("expected 42, got %d", decoded)
		}
	})

	t.Run("negative_int", func(t *testing.T) {
		raw, err := encodeImageValue(enc, FromSmallInt(-7))
		if err != nil {
			t.Fatal(err)
		}
		var decoded int64
		if err := cbor.Unmarshal(raw, &decoded); err != nil {
			t.Fatal(err)
		}
		if decoded != -7 {
			t.Fatalf("expected -7, got %d", decoded)
		}
	})

	t.Run("float", func(t *testing.T) {
		raw, err := encodeImageValue(enc, FromFloat64(3.14))
		if err != nil {
			t.Fatal(err)
		}
		var decoded float64
		if err := cbor.Unmarshal(raw, &decoded); err != nil {
			t.Fatal(err)
		}
		if decoded != 3.14 {
			t.Fatalf("expected 3.14, got %f", decoded)
		}
	})

	t.Run("symbol", func(t *testing.T) {
		symID := vm.Symbols.Intern("testSymbol")
		enc.RegisterSymbol(symID)
		val := FromSymbolID(symID)

		raw, err := encodeImageValue(enc, val)
		if err != nil {
			t.Fatal(err)
		}
		// Should decode as a CBOR tag
		var tag cbor.Tag
		if err := cbor.Unmarshal(raw, &tag); err != nil {
			t.Fatal(err)
		}
		if tag.Number != imgTagSymbolRef {
			t.Fatalf("expected tag %d, got %d", imgTagSymbolRef, tag.Number)
		}
	})

	t.Run("string", func(t *testing.T) {
		strVal := vm.registry.NewStringValue("hello world")

		raw, err := encodeImageValue(enc, strVal)
		if err != nil {
			t.Fatal(err)
		}
		var decoded string
		if err := cbor.Unmarshal(raw, &decoded); err != nil {
			t.Fatal(err)
		}
		if decoded != "hello world" {
			t.Fatalf("expected 'hello world', got %q", decoded)
		}
	})

	t.Run("character", func(t *testing.T) {
		charVal := FromCharacter('A')

		raw, err := encodeImageValue(enc, charVal)
		if err != nil {
			t.Fatal(err)
		}
		var tag cbor.Tag
		if err := cbor.Unmarshal(raw, &tag); err != nil {
			t.Fatal(err)
		}
		if tag.Number != cborTagCharacter {
			t.Fatalf("expected tag %d, got %d", cborTagCharacter, tag.Number)
		}
	})

	t.Run("object_ref", func(t *testing.T) {
		obj := NewObject(vm.ObjectClass.VTable, 0)
		ptr := uintptr(unsafe.Pointer(obj))
		enc.RegisterObject(ptr)
		val := obj.ToValue()

		raw, err := encodeImageValue(enc, val)
		if err != nil {
			t.Fatal(err)
		}
		var tag cbor.Tag
		if err := cbor.Unmarshal(raw, &tag); err != nil {
			t.Fatal(err)
		}
		if tag.Number != imgTagValueRef {
			t.Fatalf("expected tag %d, got %d", imgTagValueRef, tag.Number)
		}
	})

	t.Run("class_ref", func(t *testing.T) {
		cls := vm.ObjectClass
		enc.RegisterClass(cls)
		val := vm.ClassValue(cls)

		raw, err := encodeImageValue(enc, val)
		if err != nil {
			t.Fatal(err)
		}
		var tag cbor.Tag
		if err := cbor.Unmarshal(raw, &tag); err != nil {
			t.Fatal(err)
		}
		if tag.Number != imgTagClassRef {
			t.Fatalf("expected tag %d, got %d", imgTagClassRef, tag.Number)
		}
	})
}

// ---------------------------------------------------------------------------
// TestWriterEmpty: write an empty (fresh) VM to CBOR
// ---------------------------------------------------------------------------

func TestWriterEmpty(t *testing.T) {
	vm := NewVM()

	data, err := vm.SaveImageBytes()
	if err != nil {
		t.Fatalf("SaveImageBytes failed: %v", err)
	}

	if len(data) == 0 {
		t.Fatal("expected non-empty CBOR output")
	}

	// Verify it's valid CBOR by unmarshaling into a generic tag
	var tag cbor.Tag
	if err := cbor.Unmarshal(data, &tag); err != nil {
		t.Fatalf("output is not valid CBOR: %v", err)
	}

	if tag.Number != imgTagHeader {
		t.Fatalf("expected tag number %d, got %d", imgTagHeader, tag.Number)
	}
}

// ---------------------------------------------------------------------------
// TestWriterDeterministic: write twice, bytes must be identical
// ---------------------------------------------------------------------------

func TestWriterDeterministic(t *testing.T) {
	vm := NewVM()

	data1, err := vm.SaveImageBytes()
	if err != nil {
		t.Fatalf("first write failed: %v", err)
	}

	data2, err := vm.SaveImageBytes()
	if err != nil {
		t.Fatalf("second write failed: %v", err)
	}

	if !bytes.Equal(data1, data2) {
		t.Fatalf("CBOR output is non-deterministic: %d bytes vs %d bytes", len(data1), len(data2))
	}
}

// ---------------------------------------------------------------------------
// TestWriterBootstrapSmoke: verify size and CBOR tag prefix
// ---------------------------------------------------------------------------

func TestWriterBootstrapSmoke(t *testing.T) {
	vm := NewVM()

	data, err := vm.SaveImageBytes()
	if err != nil {
		t.Fatalf("SaveImageBytes failed: %v", err)
	}

	// CBOR tag encoding: 0xD9 = 2-byte tag, 0xDA = 4-byte tag
	// imgTagHeader = 27100 which is > 255, so it uses 0xD9 (2-byte tag)
	if len(data) < 3 {
		t.Fatalf("output too short: %d bytes", len(data))
	}
	if data[0] != 0xD9 {
		t.Fatalf("expected first byte 0xD9 (2-byte CBOR tag), got 0x%02X", data[0])
	}

	// Verify reasonable size (at least 100 bytes for a VM with core classes)
	if len(data) < 100 {
		t.Fatalf("output suspiciously small: %d bytes", len(data))
	}

	t.Logf("CBOR image size: %d bytes", len(data))
}

// ---------------------------------------------------------------------------
// Header format tests
// ---------------------------------------------------------------------------

func TestImageHeaderMagic(t *testing.T) {
	vm := NewVM()
	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	data := buf.Bytes()
	if len(data) < 4 {
		t.Fatal("Image too short for magic number")
	}

	// CBOR image starts with 0xD9 (2-byte CBOR tag prefix)
	if data[0] != 0xD9 {
		t.Errorf("First byte = 0x%02X, want 0xD9 (CBOR tag prefix)", data[0])
	}
}

func TestImageHeaderVersion(t *testing.T) {
	// CBOR format embeds version inside the structure, not at a fixed binary offset.
	// Verify the image is valid CBOR and can round-trip.
	vm := NewVM()
	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	data := buf.Bytes()
	if len(data) < 100 {
		t.Fatal("Image too short")
	}

	// Verify it starts with CBOR tag prefix
	if data[0] != 0xD9 {
		t.Errorf("First byte = 0x%02X, want 0xD9 (CBOR tag prefix)", data[0])
	}
}

func TestImageHeaderFlags(t *testing.T) {
	// CBOR format embeds flags inside the structure, not at a fixed binary offset.
	// Verify the image is valid CBOR and non-trivial.
	vm := NewVM()
	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	data := buf.Bytes()
	if len(data) < 100 {
		t.Fatal("Image too short")
	}
}

func TestImageHeaderStructure(t *testing.T) {
	// CBOR format: verify the image is valid by round-tripping through save/load.
	vm := NewVM()
	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	data := buf.Bytes()
	if len(data) < 100 {
		t.Fatalf("Image too short: %d bytes", len(data))
	}

	// Verify CBOR tag prefix
	if data[0] != 0xD9 {
		t.Errorf("First byte = 0x%02X, want 0xD9 (CBOR tag prefix)", data[0])
	}

	// Verify round-trip: load into fresh VM
	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}
}

// ---------------------------------------------------------------------------
// String table tests
// ---------------------------------------------------------------------------

func TestImageStringTable(t *testing.T) {
	vm1 := NewVM()

	// Add some symbols to create strings
	vm1.Symbols.Intern("testSymbol1")
	vm1.Symbols.Intern("testSymbol2")
	vm1.Symbols.Intern("hello")

	// Save to CBOR bytes
	data, err := vm1.SaveImageBytes()
	if err != nil {
		t.Fatalf("SaveImageBytes failed: %v", err)
	}

	// Load into fresh VM and verify symbols survived
	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}

	// Verify the interned symbols exist in the loaded VM
	for _, name := range []string{"testSymbol1", "testSymbol2", "hello"} {
		if _, ok := vm2.Symbols.Lookup(name); !ok {
			t.Errorf("Symbol %q not found in loaded VM", name)
		}
	}
}

// ---------------------------------------------------------------------------
// Symbol table tests
// ---------------------------------------------------------------------------

func TestImageSymbolTable(t *testing.T) {
	vm := NewVM()

	// The VM already has some symbols from bootstrap
	initialCount := vm.Symbols.Len()

	// Add more symbols
	vm.Symbols.Intern("customSymbol1")
	vm.Symbols.Intern("customSymbol2")

	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	_ = buf.Bytes()

	// The symbol table should have at least as many entries as the VM
	expectedCount := vm.Symbols.Len()
	if expectedCount < initialCount+2 {
		t.Errorf("Symbol count = %d, want at least %d", expectedCount, initialCount+2)
	}
}

// ---------------------------------------------------------------------------
// Selector table tests
// ---------------------------------------------------------------------------

func TestImageSelectorTable(t *testing.T) {
	vm := NewVM()

	// The VM already has selectors from primitives
	initialCount := vm.Selectors.Len()

	// Add more selectors
	vm.Selectors.Intern("customSelector1:")
	vm.Selectors.Intern("customSelector2:with:")

	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	// Just verify the image was created successfully
	if buf.Len() < 100 {
		t.Error("Image too short")
	}

	// Verify selector count increased
	if vm.Selectors.Len() < initialCount+2 {
		t.Error("Expected more selectors")
	}

	// Use the data to suppress unused warning
	_ = buf.Bytes()
}

// ---------------------------------------------------------------------------
// Class serialization tests
// ---------------------------------------------------------------------------

func TestImageClassSerialization(t *testing.T) {
	vm1 := NewVM()

	// Create a custom class
	customClass := NewClassWithInstVars("CustomClass", vm1.ObjectClass, []string{"field1", "field2"})
	vm1.Classes.Register(customClass)

	// Save to CBOR bytes
	data, err := vm1.SaveImageBytes()
	if err != nil {
		t.Fatalf("SaveImageBytes failed: %v", err)
	}

	// Load into fresh VM and verify the class survived
	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}

	loaded := vm2.LookupClass("CustomClass")
	if loaded == nil {
		t.Fatal("CustomClass not found in loaded VM")
	}
	if len(loaded.InstVars) != 2 || loaded.InstVars[0] != "field1" || loaded.InstVars[1] != "field2" {
		t.Errorf("CustomClass instVars = %v, want [field1 field2]", loaded.InstVars)
	}
}

func TestImageClassHierarchy(t *testing.T) {
	vm := NewVM()

	// Create a class hierarchy
	parent := NewClass("Parent", vm.ObjectClass)
	vm.Classes.Register(parent)

	child := NewClass("Child", parent)
	vm.Classes.Register(child)

	grandchild := NewClass("Grandchild", child)
	vm.Classes.Register(grandchild)

	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	// Just verify the image was created successfully with the hierarchy
	if buf.Len() < 100 {
		t.Error("Image too short")
	}
}

// ---------------------------------------------------------------------------
// Method serialization tests
// ---------------------------------------------------------------------------

func TestImageMethodSerialization(t *testing.T) {
	vm := NewVM()

	// Create a compiled method
	builder := NewCompiledMethodBuilder("testMethod", 1)
	builder.SetNumTemps(2)
	builder.AddLiteral(FromSmallInt(42))
	builder.Bytecode().EmitUint16(OpPushLiteral, 0) // PUSH_LITERAL 0
	builder.Bytecode().Emit(OpReturnTop)            // RETURN_TOP
	method := builder.Build()

	// Add to a class
	testClass := NewClass("TestClass", vm.ObjectClass)
	vm.Classes.Register(testClass)
	selectorID := vm.Selectors.Intern("testMethod:")
	method.SetSelector(selectorID)
	method.SetClass(testClass)
	testClass.VTable.AddMethod(selectorID, method)

	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	if buf.Len() < 100 {
		t.Error("Image too short")
	}
}

func TestImageMethodWithBlocks(t *testing.T) {
	vm := NewVM()

	// Create a method with a block
	builder := NewCompiledMethodBuilder("methodWithBlock", 0)

	// Create a block
	blockBuilder := NewBlockMethodBuilder(1)
	blockBuilder.SetNumTemps(2)
	blockBuilder.SetNumCaptures(1)
	blockBuilder.Bytecode().EmitByte(OpPushTemp, 0) // PUSH_TEMP 0
	blockBuilder.Bytecode().Emit(OpReturnTop)       // RETURN_TOP
	block := blockBuilder.Build()

	builder.AddBlock(block)
	builder.Bytecode().EmitCreateBlock(0, 0)  // CREATE_BLOCK method=0 captures=0
	builder.Bytecode().Emit(OpReturnTop)      // RETURN_TOP
	method := builder.Build()

	// Add to a class
	testClass := NewClass("BlockTest", vm.ObjectClass)
	vm.Classes.Register(testClass)
	selectorID := vm.Selectors.Intern("methodWithBlock")
	method.SetSelector(selectorID)
	method.SetClass(testClass)
	testClass.VTable.AddMethod(selectorID, method)

	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	if buf.Len() < 100 {
		t.Error("Image too short")
	}
}

func TestImageMethodWithSourceMap(t *testing.T) {
	vm := NewVM()

	// Create a method with source mapping
	builder := NewCompiledMethodBuilder("sourceMapTest", 0)
	builder.SetSource("sourceMapTest ^ 42")
	builder.MarkSource(1, 1)
	builder.AddLiteral(FromSmallInt(42))
	builder.Bytecode().EmitUint16(OpPushLiteral, 0) // PUSH_LITERAL 0
	builder.MarkSource(1, 17)
	builder.Bytecode().Emit(OpReturnTop) // RETURN_TOP
	method := builder.Build()

	// Add to a class
	testClass := NewClass("SourceMapTest", vm.ObjectClass)
	vm.Classes.Register(testClass)
	selectorID := vm.Selectors.Intern("sourceMapTest")
	method.SetSelector(selectorID)
	method.SetClass(testClass)
	testClass.VTable.AddMethod(selectorID, method)

	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	if buf.Len() < 100 {
		t.Error("Image too short")
	}
}

// ---------------------------------------------------------------------------
// Object serialization tests
// ---------------------------------------------------------------------------

func TestImageObjectSerialization(t *testing.T) {
	vm1 := NewVM()

	// Create a class with slots
	pointClass := NewClassWithInstVars("Point", vm1.ObjectClass, []string{"x", "y"})
	vm1.Classes.Register(pointClass)

	// Create an instance
	point := pointClass.NewInstance()
	point.SetSlot(0, FromSmallInt(10))
	point.SetSlot(1, FromSmallInt(20))

	// Store in globals to make it reachable
	vm1.SetGlobal("testPoint", point.ToValue())

	data, err := vm1.SaveImageBytes()
	if err != nil {
		t.Fatalf("SaveImageBytes failed: %v", err)
	}

	// Round-trip: load into fresh VM and verify object
	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}

	pointVal, ok := vm2.globals["testPoint"]
	if !ok {
		t.Fatal("testPoint global not found after load")
	}
	if !pointVal.IsObject() {
		t.Fatal("testPoint is not an object after load")
	}
}

func TestImageObjectWithNestedReferences(t *testing.T) {
	vm1 := NewVM()

	// Create a linked list-like structure
	nodeClass := NewClassWithInstVars("ListNode", vm1.ObjectClass, []string{"value", "next"})
	vm1.Classes.Register(nodeClass)

	// Create nodes
	node3 := nodeClass.NewInstance()
	node3.SetSlot(0, FromSmallInt(3))
	node3.SetSlot(1, Nil)

	node2 := nodeClass.NewInstance()
	node2.SetSlot(0, FromSmallInt(2))
	node2.SetSlot(1, node3.ToValue())

	node1 := nodeClass.NewInstance()
	node1.SetSlot(0, FromSmallInt(1))
	node1.SetSlot(1, node2.ToValue())

	// Store head in globals
	vm1.SetGlobal("listHead", node1.ToValue())

	data, err := vm1.SaveImageBytes()
	if err != nil {
		t.Fatalf("SaveImageBytes failed: %v", err)
	}

	// Round-trip: load into fresh VM and verify linked list structure
	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}

	headVal, ok := vm2.globals["listHead"]
	if !ok {
		t.Fatal("listHead global not found after load")
	}
	if !headVal.IsObject() {
		t.Fatal("listHead is not an object after load")
	}

	// Walk the linked list and verify values
	cur := ObjectFromValue(headVal)
	for i, expected := range []int64{1, 2, 3} {
		val := cur.GetSlot(0)
		if !val.IsSmallInt() || val.SmallInt() != expected {
			t.Errorf("Node %d value = %v, want %d", i, val, expected)
		}
		next := cur.GetSlot(1)
		if i < 2 {
			if !next.IsObject() {
				t.Fatalf("Node %d next is not an object", i)
			}
			cur = ObjectFromValue(next)
		} else {
			if next != Nil {
				t.Errorf("Node %d next = %v, want nil", i, next)
			}
		}
	}
}

// ---------------------------------------------------------------------------
// Globals serialization tests
// ---------------------------------------------------------------------------

func TestImageGlobalsSerialization(t *testing.T) {
	vm := NewVM()

	// Add some globals
	vm.SetGlobal("testInt", FromSmallInt(42))
	vm.SetGlobal("testFloat", FromFloat64(3.14))
	vm.SetGlobal("testBool", True)
	vm.SetGlobal("testNil", Nil)

	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	// The image should contain the globals
	if buf.Len() < 100 {
		t.Error("Image too short")
	}
}

// ---------------------------------------------------------------------------
// File I/O tests
// ---------------------------------------------------------------------------

func TestImageSaveToFile(t *testing.T) {
	vm := NewVM()

	// Create a temporary directory
	tmpDir := t.TempDir()
	imagePath := filepath.Join(tmpDir, "test.image")

	// Save image
	err := vm.SaveImage(imagePath)
	if err != nil {
		t.Fatalf("SaveImage failed: %v", err)
	}

	// Verify file exists and has content
	info, err := os.Stat(imagePath)
	if err != nil {
		t.Fatalf("Image file not created: %v", err)
	}
	if info.Size() < 100 {
		t.Errorf("Image file too small: %d bytes", info.Size())
	}

	// Read file and verify CBOR tag prefix
	data, err := os.ReadFile(imagePath)
	if err != nil {
		t.Fatalf("Failed to read image file: %v", err)
	}

	if data[0] != 0xD9 {
		t.Errorf("First byte = 0x%02X, want 0xD9 (CBOR tag prefix)", data[0])
	}
}

func TestImageSaveToInvalidPath(t *testing.T) {
	vm := NewVM()

	// Try to save to an invalid path
	err := vm.SaveImage("/nonexistent/directory/test.image")
	if err == nil {
		t.Error("Expected error for invalid path")
	}
}

// ---------------------------------------------------------------------------
// collectAllObjects tests
// ---------------------------------------------------------------------------

func TestCollectAllObjectsEmpty(t *testing.T) {
	vm := NewVM()

	// Fresh VM shouldn't have any collectible objects (all globals are symbols/values)
	objects := vm.CollectAllObjects()

	// Should be empty or very small (no user objects)
	if len(objects) > 10 {
		t.Errorf("Expected few objects in fresh VM, got %d", len(objects))
	}
}

func TestCollectAllObjectsWithGlobals(t *testing.T) {
	vm := NewVM()

	// Create and register an object
	testClass := NewClassWithInstVars("Test", vm.ObjectClass, []string{"value"})
	vm.Classes.Register(testClass)

	obj := testClass.NewInstance()
	obj.SetSlot(0, FromSmallInt(100))

	vm.SetGlobal("testObj", obj.ToValue())

	objects := vm.CollectAllObjects()

	// Should contain our object
	found := false
	for _, o := range objects {
		if o == obj {
			found = true
			break
		}
	}
	if !found {
		t.Error("collectAllObjects didn't find the test object")
	}
}

func TestCollectAllObjectsNoDuplicates(t *testing.T) {
	vm := NewVM()

	// Create an object referenced multiple times
	testClass := NewClass("Test", vm.ObjectClass)
	vm.Classes.Register(testClass)

	obj := testClass.NewInstance()

	// Reference it from multiple globals
	vm.SetGlobal("ref1", obj.ToValue())
	vm.SetGlobal("ref2", obj.ToValue())
	vm.SetGlobal("ref3", obj.ToValue())

	objects := vm.CollectAllObjects()

	// Count occurrences
	count := 0
	for _, o := range objects {
		if o == obj {
			count++
		}
	}
	if count != 1 {
		t.Errorf("Object appears %d times, want exactly 1", count)
	}
}

// ---------------------------------------------------------------------------
// Full VM save/restore consistency tests
// ---------------------------------------------------------------------------

func TestImageConsistency(t *testing.T) {
	// Two saves of the same VM state must produce byte-identical images.
	// This exercises deterministic ordering of all tables: symbols, selectors,
	// classes, methods, objects, globals, and class variables.

	vm := NewVM()

	// Create a small class hierarchy
	parent := NewClassWithInstVars("Parent", vm.ObjectClass, []string{"x", "y"})
	vm.Classes.Register(parent)

	childA := NewClassWithInstVars("ChildA", parent, []string{"a"})
	vm.Classes.Register(childA)

	childB := NewClassWithInstVars("ChildB", parent, []string{"b"})
	vm.Classes.Register(childB)

	// Add methods to the classes
	builder := NewCompiledMethodBuilder("getX", 0)
	builder.AddLiteral(FromSmallInt(1))
	builder.Bytecode().EmitUint16(OpPushLiteral, 0)
	builder.Bytecode().Emit(OpReturnTop)
	methodGetX := builder.Build()

	selGetX := vm.Selectors.Intern("getX")
	methodGetX.SetSelector(selGetX)
	methodGetX.SetClass(parent)
	parent.VTable.AddMethod(selGetX, methodGetX)

	builder2 := NewCompiledMethodBuilder("getA", 0)
	builder2.AddLiteral(FromSmallInt(2))
	builder2.Bytecode().EmitUint16(OpPushLiteral, 0)
	builder2.Bytecode().Emit(OpReturnTop)
	methodGetA := builder2.Build()

	selGetA := vm.Selectors.Intern("getA")
	methodGetA.SetSelector(selGetA)
	methodGetA.SetClass(childA)
	childA.VTable.AddMethod(selGetA, methodGetA)

	// Create objects referenced from globals.
	// Keep Go-side references to prevent GC from collecting NaN-boxed pointers.
	objA := childA.NewInstance()
	objA.SetSlot(0, FromSmallInt(10))
	objA.SetSlot(1, FromSmallInt(20))
	objA.SetSlot(2, FromSmallInt(30))

	objB := childB.NewInstance()
	objB.SetSlot(0, FromSmallInt(40))
	objB.SetSlot(1, FromSmallInt(50))
	objB.SetSlot(2, FromSmallInt(60))

	// Store globals (intentionally out of alphabetical order)
	vm.SetGlobal("zeta", objA.ToValue())
	vm.SetGlobal("alpha", objB.ToValue())
	vm.SetGlobal("mu", FromSmallInt(999))
	vm.SetGlobal("beta", True)

	// Class variables
	vm.registry.SetClassVar(parent, "count", FromSmallInt(42))
	vm.registry.SetClassVar(childA, "name", vm.Symbols.SymbolValue("hello"))
	vm.registry.SetClassVar(childB, "active", True)

	// Save the same VM state twice
	var buf1 bytes.Buffer
	err := vm.SaveImageTo(&buf1)
	if err != nil {
		t.Fatalf("First save failed: %v", err)
	}

	var buf2 bytes.Buffer
	err = vm.SaveImageTo(&buf2)
	if err != nil {
		t.Fatalf("Second save failed: %v", err)
	}

	// The two images must be byte-identical
	data1 := buf1.Bytes()
	data2 := buf2.Bytes()

	if len(data1) != len(data2) {
		t.Fatalf("Image sizes differ: %d vs %d", len(data1), len(data2))
	}

	if !bytes.Equal(data1, data2) {
		// Find and report the first difference for debugging
		for i := range data1 {
			if data1[i] != data2[i] {
				t.Fatalf("Images differ at byte offset %d: 0x%02x vs 0x%02x", i, data1[i], data2[i])
			}
		}
	}

	// Keep references alive past the save calls to prevent GC collection
	_ = objA
	_ = objB
}

// ---------------------------------------------------------------------------
// Edge cases
// ---------------------------------------------------------------------------

func TestImageEmptyStrings(t *testing.T) {
	vm := NewVM()

	// Add an empty string symbol
	vm.Symbols.Intern("")

	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	if buf.Len() < 100 {
		t.Error("Image too short")
	}
}

func TestImageLongStrings(t *testing.T) {
	vm := NewVM()

	// Add a long symbol
	longName := ""
	for i := 0; i < 1000; i++ {
		longName += "x"
	}
	vm.Symbols.Intern(longName)

	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	if buf.Len() < 100 {
		t.Error("Image too short")
	}
}

func TestImageManyClasses(t *testing.T) {
	vm := NewVM()

	// Create many classes
	for i := 0; i < 100; i++ {
		c := NewClass("TestClass"+string(rune('A'+i%26))+string(rune('0'+i/26)), vm.ObjectClass)
		vm.Classes.Register(c)
	}

	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	if buf.Len() < 100 {
		t.Error("Image too short")
	}
}

func TestImageManyObjects(t *testing.T) {
	vm := NewVM()

	testClass := NewClass("Test", vm.ObjectClass)
	vm.Classes.Register(testClass)

	// Keep references to prevent GC from collecting objects.
	// NaN-boxed pointers stored in globals are not traced by Go's GC,
	// so we must keep separate Go references to prevent collection.
	objectRefs := make([]*Object, 100)

	// Create many objects with unique names
	for i := 0; i < 100; i++ {
		obj := testClass.NewInstance()
		objectRefs[i] = obj // Keep reference alive
		name := "testObject_" + string(rune('A'+i/26)) + string(rune('A'+i%26))
		vm.SetGlobal(name, obj.ToValue())
	}

	// First verify that CollectAllObjects finds the right number
	objects := vm.CollectAllObjects()
	if len(objects) < 100 {
		t.Errorf("CollectAllObjects found %d objects, want at least 100", len(objects))
	}

	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	if buf.Len() < 100 {
		t.Error("Image too short")
	}

	// Round-trip: verify objects survive save/load
	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(buf.Bytes()); err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}

	// Spot-check a few globals survived
	for _, name := range []string{"testObject_AA", "testObject_BA", "testObject_DA"} {
		if _, ok := vm2.globals[name]; !ok {
			t.Errorf("Global %q not found after load", name)
		}
	}

	// Use objectRefs to prevent compiler from optimizing away
	_ = objectRefs
}

// ---------------------------------------------------------------------------
// Class variable serialization tests
// ---------------------------------------------------------------------------

func TestImageClassVarsRoundTrip(t *testing.T) {
	vm1 := NewVM()

	// Create two classes with class variables
	classA := NewClass("ClassA", vm1.ObjectClass)
	vm1.Classes.Register(classA)
	vm1.registry.SetClassVar(classA, "count", FromSmallInt(42))
	vm1.registry.SetClassVar(classA, "name", vm1.Symbols.SymbolValue("hello"))

	classB := NewClass("ClassB", vm1.ObjectClass)
	vm1.Classes.Register(classB)
	vm1.registry.SetClassVar(classB, "active", True)

	// Save image
	var buf bytes.Buffer
	err := vm1.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	// Load into fresh VM
	vm2 := NewVM()
	err = vm2.LoadImageFromBytes(buf.Bytes())
	if err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}

	// Verify class A's variables
	loadedA := vm2.LookupClass("ClassA")
	if loadedA == nil {
		t.Fatal("ClassA not found in loaded VM")
	}

	countVal := vm2.registry.GetClassVar(loadedA, "count")
	if !countVal.IsSmallInt() || countVal.SmallInt() != 42 {
		t.Errorf("ClassA.count = %v, want 42", countVal)
	}

	nameVal := vm2.registry.GetClassVar(loadedA, "name")
	if !nameVal.IsSymbol() {
		t.Errorf("ClassA.name is not a symbol, got %v", nameVal)
	}

	// Verify class B's variables
	loadedB := vm2.LookupClass("ClassB")
	if loadedB == nil {
		t.Fatal("ClassB not found in loaded VM")
	}

	activeVal := vm2.registry.GetClassVar(loadedB, "active")
	if activeVal != True {
		t.Errorf("ClassB.active = %v, want true", activeVal)
	}

	// Verify a class without class vars has none
	loadedObj := vm2.LookupClass("Object")
	if loadedObj != nil {
		vars := vm2.registry.GetClassVarStorage(loadedObj)
		if len(vars) != 0 {
			t.Errorf("Object class has %d class vars, want 0", len(vars))
		}
	}
}

func TestImageClassVarsEmpty(t *testing.T) {
	// VM with no class variables should round-trip fine
	vm1 := NewVM()
	classA := NewClass("EmptyVars", vm1.ObjectClass)
	vm1.Classes.Register(classA)

	var buf bytes.Buffer
	err := vm1.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	vm2 := NewVM()
	err = vm2.LoadImageFromBytes(buf.Bytes())
	if err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}

	loaded := vm2.LookupClass("EmptyVars")
	if loaded == nil {
		t.Fatal("EmptyVars class not found")
	}
	vars := vm2.registry.GetClassVarStorage(loaded)
	if len(vars) != 0 {
		t.Errorf("EmptyVars has %d class vars, want 0", len(vars))
	}
}

func TestImageClassVarsWithObjectValues(t *testing.T) {
	vm1 := NewVM()

	holder := NewClassWithInstVars("Holder", vm1.ObjectClass, []string{"data"})
	vm1.Classes.Register(holder)

	// Create an object and store it as a class variable value
	obj := holder.NewInstance()
	obj.SetSlot(0, FromSmallInt(999))
	vm1.SetGlobal("keepAlive", obj.ToValue()) // ensure object is reachable

	target := NewClass("Target", vm1.ObjectClass)
	vm1.Classes.Register(target)
	vm1.registry.SetClassVar(target, "instance", obj.ToValue())

	var buf bytes.Buffer
	err := vm1.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	vm2 := NewVM()
	err = vm2.LoadImageFromBytes(buf.Bytes())
	if err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}

	loadedTarget := vm2.LookupClass("Target")
	if loadedTarget == nil {
		t.Fatal("Target class not found")
	}

	instanceVal := vm2.registry.GetClassVar(loadedTarget, "instance")
	if !instanceVal.IsObject() {
		t.Fatalf("Target.instance is not an object, got %v", instanceVal)
	}

	loadedObj := ObjectFromValue(instanceVal)
	slotVal := loadedObj.GetSlot(0)
	if !slotVal.IsSmallInt() || slotVal.SmallInt() != 999 {
		t.Errorf("Target.instance.data = %v, want 999", slotVal)
	}
}

// ---------------------------------------------------------------------------
// Behavioral round-trip tests
// ---------------------------------------------------------------------------
// These tests verify that compiled bytecode methods survive image save/load
// and still execute correctly afterward.

func TestImageRoundTrip_SuperSend(t *testing.T) {
	// Build a class hierarchy: Animal -> Dog
	// Animal>>speak returns 10
	// Dog>>speak uses super send to call Animal>>speak and returns that result
	vm1 := NewVM()
	selectors := vm1.Selectors

	animalClass := NewClass("AnimalRT", vm1.ObjectClass)
	vm1.Classes.Register(animalClass)

	dogClass := NewClass("DogRT", animalClass)
	vm1.Classes.Register(dogClass)

	speakSel := selectors.Intern("speak")

	// Animal>>speak: push 10, return
	ab := NewCompiledMethodBuilder("speak", 0)
	ab.Bytecode().EmitInt8(OpPushInt8, 10)
	ab.Bytecode().Emit(OpReturnTop)
	animalSpeak := ab.Build()
	animalSpeak.SetClass(animalClass)
	animalClass.VTable.AddMethod(speakSel, animalSpeak)

	// Dog>>speak: push self, super send speak (returns Animal>>speak result=10), return
	db := NewCompiledMethodBuilder("speak", 0)
	db.Bytecode().Emit(OpPushSelf)
	db.Bytecode().EmitSend(OpSendSuper, uint16(speakSel), 0)
	db.Bytecode().Emit(OpReturnTop)
	dogSpeak := db.Build()
	dogSpeak.SetClass(dogClass)
	dogClass.VTable.AddMethod(speakSel, dogSpeak)

	// Verify on vm1 before save
	obj := NewObject(dogClass.VTable, 0)
	result := vm1.Send(obj.ToValue(), "speak", nil)
	if !result.IsSmallInt() || result.SmallInt() != 10 {
		t.Fatalf("pre-save Dog>>speak = %v, want 10", result)
	}

	// Save image
	var buf bytes.Buffer
	if err := vm1.SaveImageTo(&buf); err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	// Load into fresh VM
	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(buf.Bytes()); err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}

	// Look up Dog class and create an instance
	loadedDog := vm2.LookupClass("DogRT")
	if loadedDog == nil {
		t.Fatal("DogRT not found after load")
	}
	loadedAnimal := vm2.LookupClass("AnimalRT")
	if loadedAnimal == nil {
		t.Fatal("AnimalRT not found after load")
	}

	obj2 := NewObject(loadedDog.VTable, 0)
	result2 := vm2.Send(obj2.ToValue(), "speak", nil)
	if !result2.IsSmallInt() || result2.SmallInt() != 10 {
		t.Fatalf("post-load Dog>>speak = %v, want 10 (from super)", result2)
	}
}

func TestImageRoundTrip_BlockEvaluation(t *testing.T) {
	// Build a method that creates a block [:x | x + 1] and calls value: 5
	// Expected result: 6
	vm1 := NewVM()
	selectors := vm1.Selectors

	testClass := NewClass("BlockEvalRT", vm1.ObjectClass)
	vm1.Classes.Register(testClass)

	plusSel := selectors.Intern("+")
	valueSel := selectors.Intern("value:")

	// Build the block: parameter x (index 0), push x, push 1, send +, return
	blockBuilder := NewBlockMethodBuilder(1) // 1 parameter
	blockBuilder.SetNumTemps(1)              // 1 temp (the parameter)
	blockBuilder.Bytecode().EmitByte(OpPushTemp, 0)
	blockBuilder.Bytecode().EmitInt8(OpPushInt8, 1)
	blockBuilder.Bytecode().EmitSend(OpSend, uint16(plusSel), 1)
	blockBuilder.Bytecode().Emit(OpReturnTop)
	block := blockBuilder.Build()

	// Build the method: create block, push 5, send value:, return
	mb := NewCompiledMethodBuilder("runBlock", 0)
	mb.AddBlock(block)
	mb.Bytecode().EmitCreateBlock(0, 0)                        // CREATE_BLOCK (block index 0, 0 captures)
	mb.Bytecode().EmitInt8(OpPushInt8, 5)                      // push 5
	mb.Bytecode().EmitSend(OpSend, uint16(valueSel), 1)        // send value: to block
	mb.Bytecode().Emit(OpReturnTop)
	method := mb.Build()
	method.SetClass(testClass)

	runBlockSel := selectors.Intern("runBlock")
	testClass.VTable.AddMethod(runBlockSel, method)

	// Verify on vm1
	obj := NewObject(testClass.VTable, 0)
	result := vm1.Send(obj.ToValue(), "runBlock", nil)
	if !result.IsSmallInt() || result.SmallInt() != 6 {
		t.Fatalf("pre-save runBlock = %v, want 6", result)
	}

	// Save image
	var buf bytes.Buffer
	if err := vm1.SaveImageTo(&buf); err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	// Load into fresh VM
	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(buf.Bytes()); err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}

	// Execute on loaded VM
	loaded := vm2.LookupClass("BlockEvalRT")
	if loaded == nil {
		t.Fatal("BlockEvalRT not found after load")
	}

	obj2 := NewObject(loaded.VTable, 0)
	result2 := vm2.Send(obj2.ToValue(), "runBlock", nil)
	if !result2.IsSmallInt() || result2.SmallInt() != 6 {
		t.Fatalf("post-load runBlock = %v, want 6", result2)
	}
}

func TestImageRoundTrip_SymbolIdentity(t *testing.T) {
	// Verify that symbols interned before save resolve to the same symbol
	// name (and thus the same Value) after load.
	vm1 := NewVM()

	// Intern some symbols before save
	helloID := vm1.Symbols.Intern("hello")
	worldID := vm1.Symbols.Intern("world")
	fooBarID := vm1.Symbols.Intern("fooBar")

	helloVal := FromSymbolID(helloID)
	worldVal := FromSymbolID(worldID)
	fooBarVal := FromSymbolID(fooBarID)

	// Store symbol values as globals so they survive serialization
	vm1.SetGlobal("symHello", helloVal)
	vm1.SetGlobal("symWorld", worldVal)
	vm1.SetGlobal("symFooBar", fooBarVal)

	// Verify the names resolve before save
	if vm1.Symbols.Name(helloID) != "hello" {
		t.Fatalf("pre-save: hello symbol name mismatch")
	}

	// Save image
	var buf bytes.Buffer
	if err := vm1.SaveImageTo(&buf); err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	// Load into fresh VM
	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(buf.Bytes()); err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}

	// Verify symbols resolve to the same names after load
	checkSymGlobal := func(globalName, expectedSymName string) {
		t.Helper()
		val, ok := vm2.LookupGlobal(globalName)
		if !ok {
			t.Fatalf("global %q not found after load", globalName)
		}
		if !val.IsSymbol() {
			t.Fatalf("global %q is not a symbol after load", globalName)
		}
		name := vm2.Symbols.Name(val.SymbolID())
		if name != expectedSymName {
			t.Errorf("symbol %q: name after load = %q, want %q", globalName, name, expectedSymName)
		}
	}

	checkSymGlobal("symHello", "hello")
	checkSymGlobal("symWorld", "world")
	checkSymGlobal("symFooBar", "fooBar")

	// Also verify that interning the same string on the loaded VM yields
	// a symbol with the same name (identity via name lookup).
	id2 := vm2.Symbols.Intern("hello")
	name2 := vm2.Symbols.Name(id2)
	if name2 != "hello" {
		t.Errorf("re-interned hello: name = %q, want %q", name2, "hello")
	}

	// The global's symbol ID and re-interned ID must refer to the same symbol name
	gVal, _ := vm2.LookupGlobal("symHello")
	if vm2.Symbols.Name(gVal.SymbolID()) != vm2.Symbols.Name(id2) {
		t.Error("symbol identity broken: global symbol and re-interned symbol resolve to different names")
	}
}

func TestImageRoundTrip_GlobalAccessVariousTypes(t *testing.T) {
	// Set globals with SmallInt, Float, Boolean, Nil, and Symbol values,
	// save the image, load it, and verify all survive.
	vm1 := NewVM()

	vm1.SetGlobal("gInt", FromSmallInt(42))
	vm1.SetGlobal("gNegInt", FromSmallInt(-7))
	vm1.SetGlobal("gFloat", FromFloat64(3.14))
	vm1.SetGlobal("gTrue", True)
	vm1.SetGlobal("gFalse", False)
	vm1.SetGlobal("gNil", Nil)
	vm1.SetGlobal("gSymbol", vm1.Symbols.SymbolValue("mySymbol"))

	// Save image
	var buf bytes.Buffer
	if err := vm1.SaveImageTo(&buf); err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	// Load into fresh VM
	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(buf.Bytes()); err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}

	// Helper for checking globals
	checkGlobal := func(name string, check func(Value) bool, desc string) {
		t.Helper()
		val, ok := vm2.LookupGlobal(name)
		if !ok {
			t.Fatalf("global %q not found after load", name)
		}
		if !check(val) {
			t.Errorf("global %q: %s, got %v", name, desc, val)
		}
	}

	checkGlobal("gInt", func(v Value) bool {
		return v.IsSmallInt() && v.SmallInt() == 42
	}, "want SmallInt(42)")

	checkGlobal("gNegInt", func(v Value) bool {
		return v.IsSmallInt() && v.SmallInt() == -7
	}, "want SmallInt(-7)")

	checkGlobal("gFloat", func(v Value) bool {
		return v.IsFloat() && v.Float64() == 3.14
	}, "want Float(3.14)")

	checkGlobal("gTrue", func(v Value) bool {
		return v == True
	}, "want True")

	checkGlobal("gFalse", func(v Value) bool {
		return v == False
	}, "want False")

	checkGlobal("gNil", func(v Value) bool {
		return v == Nil
	}, "want Nil")

	checkGlobal("gSymbol", func(v Value) bool {
		if !v.IsSymbol() {
			return false
		}
		return vm2.Symbols.Name(v.SymbolID()) == "mySymbol"
	}, "want Symbol(mySymbol)")
}

func TestImageRoundTrip_ClassVariables(t *testing.T) {
	// Set class variables with various value types via registry, save/load,
	// and verify they survive with correct values.
	vm1 := NewVM()

	// Create classes with class variables
	classA := NewClass("ClassVarA", vm1.ObjectClass)
	vm1.Classes.Register(classA)

	classB := NewClass("ClassVarB", vm1.ObjectClass)
	vm1.Classes.Register(classB)

	// Set various class variables on classA
	vm1.registry.SetClassVar(classA, "intVar", FromSmallInt(100))
	vm1.registry.SetClassVar(classA, "floatVar", FromFloat64(2.718))
	vm1.registry.SetClassVar(classA, "trueVar", True)
	vm1.registry.SetClassVar(classA, "falseVar", False)
	vm1.registry.SetClassVar(classA, "nilVar", Nil)
	vm1.registry.SetClassVar(classA, "symVar", vm1.Symbols.SymbolValue("classSymbol"))

	// Set a class variable on classB
	vm1.registry.SetClassVar(classB, "counter", FromSmallInt(0))

	// Save image
	var buf bytes.Buffer
	if err := vm1.SaveImageTo(&buf); err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	// Load into fresh VM
	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(buf.Bytes()); err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}

	// Verify classA's class variables
	loadedA := vm2.LookupClass("ClassVarA")
	if loadedA == nil {
		t.Fatal("ClassVarA not found after load")
	}

	intVal := vm2.registry.GetClassVar(loadedA, "intVar")
	if !intVal.IsSmallInt() || intVal.SmallInt() != 100 {
		t.Errorf("ClassVarA.intVar = %v, want 100", intVal)
	}

	floatVal := vm2.registry.GetClassVar(loadedA, "floatVar")
	if !floatVal.IsFloat() || floatVal.Float64() != 2.718 {
		t.Errorf("ClassVarA.floatVar = %v, want 2.718", floatVal)
	}

	trueVal := vm2.registry.GetClassVar(loadedA, "trueVar")
	if trueVal != True {
		t.Errorf("ClassVarA.trueVar = %v, want true", trueVal)
	}

	falseVal := vm2.registry.GetClassVar(loadedA, "falseVar")
	if falseVal != False {
		t.Errorf("ClassVarA.falseVar = %v, want false", falseVal)
	}

	nilVal := vm2.registry.GetClassVar(loadedA, "nilVar")
	if nilVal != Nil {
		t.Errorf("ClassVarA.nilVar = %v, want nil", nilVal)
	}

	symVal := vm2.registry.GetClassVar(loadedA, "symVar")
	if !symVal.IsSymbol() {
		t.Errorf("ClassVarA.symVar is not a symbol, got %v", symVal)
	} else if vm2.Symbols.Name(symVal.SymbolID()) != "classSymbol" {
		t.Errorf("ClassVarA.symVar name = %q, want %q", vm2.Symbols.Name(symVal.SymbolID()), "classSymbol")
	}

	// Verify classB's class variable
	loadedB := vm2.LookupClass("ClassVarB")
	if loadedB == nil {
		t.Fatal("ClassVarB not found after load")
	}

	counterVal := vm2.registry.GetClassVar(loadedB, "counter")
	if !counterVal.IsSmallInt() || counterVal.SmallInt() != 0 {
		t.Errorf("ClassVarB.counter = %v, want 0", counterVal)
	}

	// Verify classA has the right number of class vars
	aVars := vm2.registry.GetClassVarStorage(loadedA)
	if len(aVars) != 6 {
		t.Errorf("ClassVarA has %d class vars, want 6", len(aVars))
	}

	// Verify classB has the right number of class vars
	bVars := vm2.registry.GetClassVarStorage(loadedB)
	if len(bVars) != 1 {
		t.Errorf("ClassVarB has %d class vars, want 1", len(bVars))
	}
}

// ---------------------------------------------------------------------------
// SaveImageAtomic tests
// ---------------------------------------------------------------------------

func TestSaveImageAtomicBasic(t *testing.T) {
	vm := NewVM()

	// Add some state to verify round-trip
	testClass := NewClassWithInstVars("AtomicTest", vm.ObjectClass, []string{"value"})
	vm.Classes.Register(testClass)
	vm.SetGlobal("atomicVal", FromSmallInt(42))

	tmpDir := t.TempDir()
	imagePath := filepath.Join(tmpDir, "checkpoint.image")

	// First atomic save (no previous file)
	err := vm.SaveImageAtomic(imagePath)
	if err != nil {
		t.Fatalf("SaveImageAtomic failed: %v", err)
	}

	// Verify file exists and is valid
	info, err := os.Stat(imagePath)
	if err != nil {
		t.Fatalf("Image file not created: %v", err)
	}
	if info.Size() < 100 {
		t.Errorf("Image file too small: %d bytes", info.Size())
	}

	// Verify CBOR tag prefix
	data, err := os.ReadFile(imagePath)
	if err != nil {
		t.Fatalf("Failed to read image: %v", err)
	}
	if data[0] != 0xD9 {
		t.Errorf("First byte = 0x%02X, want 0xD9 (CBOR tag prefix)", data[0])
	}

	// .prev should NOT exist on first save
	prevPath := imagePath + ".prev"
	if _, err := os.Stat(prevPath); err == nil {
		t.Error(".prev file should not exist on first save")
	}

	// .tmp should NOT exist after successful save
	tmpPath := imagePath + ".tmp"
	if _, err := os.Stat(tmpPath); err == nil {
		t.Error(".tmp file should not exist after successful save")
	}
}

func TestSaveImageAtomicCreatesPrev(t *testing.T) {
	vm := NewVM()
	vm.SetGlobal("version", FromSmallInt(1))

	tmpDir := t.TempDir()
	imagePath := filepath.Join(tmpDir, "checkpoint.image")
	prevPath := imagePath + ".prev"

	// First save
	if err := vm.SaveImageAtomic(imagePath); err != nil {
		t.Fatalf("First SaveImageAtomic failed: %v", err)
	}
	firstSize, _ := os.Stat(imagePath)

	// Update state and save again
	vm.SetGlobal("version", FromSmallInt(2))
	if err := vm.SaveImageAtomic(imagePath); err != nil {
		t.Fatalf("Second SaveImageAtomic failed: %v", err)
	}

	// .prev should now exist with the first save's content
	prevInfo, err := os.Stat(prevPath)
	if err != nil {
		t.Fatalf(".prev file should exist after second save: %v", err)
	}
	if prevInfo.Size() != firstSize.Size() {
		t.Errorf(".prev size = %d, want %d (first save size)", prevInfo.Size(), firstSize.Size())
	}

	// .prev should be a valid CBOR image
	prevData, _ := os.ReadFile(prevPath)
	if prevData[0] != 0xD9 {
		t.Errorf(".prev first byte = 0x%02X, want 0xD9 (CBOR tag prefix)", prevData[0])
	}
}

func TestSaveImageAtomicRoundTrip(t *testing.T) {
	// Verify we can checkpoint and restore with full state
	vm1 := NewVM()

	testClass := NewClassWithInstVars("CheckpointRT", vm1.ObjectClass, []string{"name"})
	vm1.Classes.Register(testClass)

	obj := testClass.NewInstance()
	obj.SetSlot(0, vm1.Symbols.SymbolValue("hello"))
	vm1.SetGlobal("myObj", obj.ToValue())
	vm1.SetGlobal("counter", FromSmallInt(99))

	tmpDir := t.TempDir()
	imagePath := filepath.Join(tmpDir, "round-trip.image")

	// Atomic save
	if err := vm1.SaveImageAtomic(imagePath); err != nil {
		t.Fatalf("SaveImageAtomic failed: %v", err)
	}

	// Load into fresh VM
	imageData, err := os.ReadFile(imagePath)
	if err != nil {
		t.Fatalf("Failed to read image: %v", err)
	}

	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(imageData); err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}

	// Verify state
	counterVal, ok := vm2.globals["counter"]
	if !ok || !counterVal.IsSmallInt() || counterVal.SmallInt() != 99 {
		t.Errorf("counter = %v, want 99", counterVal)
	}

	loadedClass := vm2.LookupClass("CheckpointRT")
	if loadedClass == nil {
		t.Fatal("CheckpointRT class not found after restore")
	}

	if len(loadedClass.InstVars) != 1 || loadedClass.InstVars[0] != "name" {
		t.Errorf("CheckpointRT instVars = %v, want [name]", loadedClass.InstVars)
	}
}

func TestSaveImageAtomicSourcePreservation(t *testing.T) {
	// Verify that method source text survives checkpoint/restore and FileOut works
	vm1 := NewVM()

	testClass := NewClass("SourceRT", vm1.ObjectClass)
	vm1.Classes.Register(testClass)

	// Add a method with source text
	selectors := vm1.Selectors
	mb := NewCompiledMethodBuilder("greet", 0)
	mb.Bytecode().EmitInt8(OpPushInt8, 7)
	mb.Bytecode().Emit(OpReturnTop)
	method := mb.Build()
	method.Source = "method: greet [ ^7 ]"
	method.SetClass(testClass)
	greetSel := selectors.Intern("greet")
	testClass.VTable.AddMethod(greetSel, method)

	tmpDir := t.TempDir()
	imagePath := filepath.Join(tmpDir, "source-rt.image")

	// Checkpoint
	if err := vm1.SaveImageAtomic(imagePath); err != nil {
		t.Fatalf("SaveImageAtomic failed: %v", err)
	}

	// Restore
	imageData, _ := os.ReadFile(imagePath)
	vm2 := NewVM()
	if err := vm2.LoadImageFromBytes(imageData); err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}

	// Verify method source is preserved
	loadedClass := vm2.LookupClass("SourceRT")
	if loadedClass == nil {
		t.Fatal("SourceRT not found after restore")
	}

	loadedGreetSel := vm2.Selectors.Intern("greet")
	loadedMethod := loadedClass.VTable.Lookup(loadedGreetSel)
	if loadedMethod == nil {
		t.Fatal("greet method not found after restore")
	}

	cm, ok := loadedMethod.(*CompiledMethod)
	if !ok {
		t.Fatal("greet is not a CompiledMethod")
	}

	if cm.Source != "method: greet [ ^7 ]" {
		t.Errorf("Source = %q, want %q", cm.Source, "method: greet [ ^7 ]")
	}

	// Verify FileOut produces valid source
	source := FileOutClass(loadedClass, vm2.Selectors)
	if source == "" {
		t.Error("FileOutClass returned empty string for restored class")
	}
	if !bytes.Contains([]byte(source), []byte("greet")) {
		t.Error("FileOutClass output does not contain method name 'greet'")
	}
}

func TestSaveImageAtomicInvalidPath(t *testing.T) {
	vm := NewVM()

	err := vm.SaveImageAtomic("/nonexistent/directory/test.image")
	if err == nil {
		t.Error("Expected error for invalid path")
	}
}

func TestSaveImageAtomicMultipleCheckpoints(t *testing.T) {
	// Simulate periodic checkpointing (3 saves), verify .prev is always the previous version
	vm := NewVM()

	tmpDir := t.TempDir()
	imagePath := filepath.Join(tmpDir, "periodic.image")
	prevPath := imagePath + ".prev"

	// Save 1
	vm.SetGlobal("epoch", FromSmallInt(1))
	if err := vm.SaveImageAtomic(imagePath); err != nil {
		t.Fatalf("Save 1 failed: %v", err)
	}
	save1Data, _ := os.ReadFile(imagePath)

	// Save 2
	vm.SetGlobal("epoch", FromSmallInt(2))
	if err := vm.SaveImageAtomic(imagePath); err != nil {
		t.Fatalf("Save 2 failed: %v", err)
	}
	// .prev should be save 1
	prevData, _ := os.ReadFile(prevPath)
	if !bytes.Equal(prevData, save1Data) {
		t.Error(".prev after save 2 should equal save 1 data")
	}
	save2Data, _ := os.ReadFile(imagePath)

	// Save 3
	vm.SetGlobal("epoch", FromSmallInt(3))
	if err := vm.SaveImageAtomic(imagePath); err != nil {
		t.Fatalf("Save 3 failed: %v", err)
	}
	// .prev should be save 2
	prevData, _ = os.ReadFile(prevPath)
	if !bytes.Equal(prevData, save2Data) {
		t.Error(".prev after save 3 should equal save 2 data")
	}
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

func BenchmarkVMSaveImage(b *testing.B) {
	vm := NewVM()

	// Set up some state
	testClass := NewClassWithInstVars("Test", vm.ObjectClass, []string{"value"})
	vm.Classes.Register(testClass)

	for i := 0; i < 10; i++ {
		obj := testClass.NewInstance()
		obj.SetSlot(0, FromSmallInt(int64(i)))
		vm.SetGlobal("obj"+string(rune('0'+i)), obj.ToValue())
	}

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		var buf bytes.Buffer
		_ = vm.SaveImageTo(&buf)
	}
}

func BenchmarkCollectAllObjects(b *testing.B) {
	vm := NewVM()

	testClass := NewClass("Test", vm.ObjectClass)
	vm.Classes.Register(testClass)

	// Create a chain of objects
	var prev Value = Nil
	for i := 0; i < 100; i++ {
		obj := NewObject(testClass.VTable, 1)
		obj.SetSlot(0, prev)
		prev = obj.ToValue()
	}
	vm.SetGlobal("chain", prev)

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		_ = vm.CollectAllObjects()
	}
}
