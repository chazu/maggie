package vm

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"
)

// ---------------------------------------------------------------------------
// ImageWriter basic tests
// ---------------------------------------------------------------------------

func TestNewImageWriter(t *testing.T) {
	w := NewImageWriter()
	if w == nil {
		t.Fatal("NewImageWriter returned nil")
	}
	if w.buf == nil {
		t.Error("ImageWriter buffer is nil")
	}
	if w.encoder == nil {
		t.Error("ImageWriter encoder is nil")
	}
	if w.flags != ImageFlagNone {
		t.Errorf("ImageWriter flags = %d, want %d", w.flags, ImageFlagNone)
	}
}

func TestImageWriterSetFlags(t *testing.T) {
	w := NewImageWriter()
	w.SetFlags(ImageFlagDebugInfo)
	if w.flags != ImageFlagDebugInfo {
		t.Errorf("SetFlags: got %d, want %d", w.flags, ImageFlagDebugInfo)
	}
}

func TestImageWriterSetEntryPoint(t *testing.T) {
	w := NewImageWriter()
	w.SetEntryPoint(42)
	if w.entryPoint != 42 {
		t.Errorf("SetEntryPoint: got %d, want 42", w.entryPoint)
	}
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

	// Check magic number
	magic := data[0:4]
	if !bytes.Equal(magic, ImageMagic[:]) {
		t.Errorf("Magic number = %v, want %v", magic, ImageMagic)
	}
}

func TestImageHeaderVersion(t *testing.T) {
	vm := NewVM()
	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	data := buf.Bytes()
	if len(data) < 8 {
		t.Fatal("Image too short for version")
	}

	// Check version (at offset 4)
	version := ReadUint32(data[4:])
	if version != ImageVersion {
		t.Errorf("Version = %d, want %d", version, ImageVersion)
	}
}

func TestImageHeaderFlags(t *testing.T) {
	vm := NewVM()
	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	data := buf.Bytes()
	if len(data) < 12 {
		t.Fatal("Image too short for flags")
	}

	// Check flags (at offset 8)
	flags := ReadUint32(data[8:])
	if flags != ImageFlagNone {
		t.Errorf("Flags = %d, want %d", flags, ImageFlagNone)
	}
}

func TestImageHeaderStructure(t *testing.T) {
	vm := NewVM()
	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	data := buf.Bytes()
	if len(data) < ImageHeaderSize {
		t.Fatalf("Image too short: %d bytes, need at least %d", len(data), ImageHeaderSize)
	}

	// Parse header fields
	magic := data[0:4]
	version := ReadUint32(data[4:])
	flags := ReadUint32(data[8:])
	objectCount := ReadUint32(data[12:])
	stringTableOffset := ReadUint64(data[16:])
	classTableOffset := ReadUint64(data[24:])
	entryPoint := ReadUint32(data[32:])

	// Verify magic
	if !bytes.Equal(magic, ImageMagic[:]) {
		t.Errorf("Magic = %v, want %v", magic, ImageMagic)
	}

	// Verify version
	if version != ImageVersion {
		t.Errorf("Version = %d, want %d", version, ImageVersion)
	}

	// Verify flags
	if flags != ImageFlagNone {
		t.Errorf("Flags = %d, want %d", flags, ImageFlagNone)
	}

	// String table offset should be past header
	if stringTableOffset < uint64(ImageHeaderSize) {
		t.Errorf("StringTableOffset = %d, should be >= %d", stringTableOffset, ImageHeaderSize)
	}

	// Class table offset should be past string table
	if classTableOffset < stringTableOffset {
		t.Errorf("ClassTableOffset = %d, should be >= StringTableOffset %d", classTableOffset, stringTableOffset)
	}

	// Entry point should be 0 (no explicit entry point set)
	if entryPoint != 0 {
		t.Errorf("EntryPoint = %d, want 0", entryPoint)
	}

	// Object count can be 0 for a fresh VM
	_ = objectCount // Suppress unused warning
}

// ---------------------------------------------------------------------------
// String table tests
// ---------------------------------------------------------------------------

func TestImageStringTable(t *testing.T) {
	vm := NewVM()

	// Add some symbols to create strings
	vm.Symbols.Intern("testSymbol1")
	vm.Symbols.Intern("testSymbol2")
	vm.Symbols.Intern("hello")

	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	data := buf.Bytes()

	// Find string table offset
	stringTableOffset := ReadUint64(data[16:])
	if stringTableOffset >= uint64(len(data)) {
		t.Fatalf("String table offset %d out of bounds", stringTableOffset)
	}

	// Read string count
	stringCount := ReadUint32(data[stringTableOffset:])
	if stringCount == 0 {
		t.Error("String count should be > 0")
	}

	// Verify we can read strings
	offset := stringTableOffset + 4
	for i := uint32(0); i < stringCount && offset < uint64(len(data)); i++ {
		if offset+4 > uint64(len(data)) {
			t.Fatalf("String %d length out of bounds", i)
		}
		strLen := ReadUint32(data[offset:])
		offset += 4
		if offset+uint64(strLen) > uint64(len(data)) {
			t.Fatalf("String %d data out of bounds", i)
		}
		// Read string bytes
		_ = string(data[offset : offset+uint64(strLen)])
		offset += uint64(strLen)
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
	if buf.Len() < ImageHeaderSize {
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
	vm := NewVM()

	// Create a custom class
	customClass := NewClassWithInstVars("CustomClass", vm.ObjectClass, []string{"field1", "field2"})
	vm.Classes.Register(customClass)

	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	data := buf.Bytes()

	// Find class table offset
	classTableOffset := ReadUint64(data[24:])
	if classTableOffset >= uint64(len(data)) {
		t.Fatalf("Class table offset %d out of bounds", classTableOffset)
	}

	// Read class count
	classCount := ReadUint32(data[classTableOffset:])
	if classCount == 0 {
		t.Error("Class count should be > 0")
	}

	// Should have at least the built-in classes
	if classCount < 10 {
		t.Errorf("Expected at least 10 classes (built-ins), got %d", classCount)
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
	if buf.Len() < ImageHeaderSize {
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

	if buf.Len() < ImageHeaderSize {
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

	if buf.Len() < ImageHeaderSize {
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

	if buf.Len() < ImageHeaderSize {
		t.Error("Image too short")
	}
}

// ---------------------------------------------------------------------------
// Object serialization tests
// ---------------------------------------------------------------------------

func TestImageObjectSerialization(t *testing.T) {
	vm := NewVM()

	// Create a class with slots
	pointClass := NewClassWithInstVars("Point", vm.ObjectClass, []string{"x", "y"})
	vm.Classes.Register(pointClass)

	// Create an instance
	point := pointClass.NewInstance()
	point.SetSlot(0, FromSmallInt(10))
	point.SetSlot(1, FromSmallInt(20))

	// Store in globals to make it reachable
	vm.SetGlobal("testPoint", point.ToValue())

	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	data := buf.Bytes()

	// Check object count in header
	objectCount := ReadUint32(data[12:])
	if objectCount == 0 {
		t.Error("Object count should be > 0")
	}
}

func TestImageObjectWithNestedReferences(t *testing.T) {
	vm := NewVM()

	// Create a linked list-like structure
	nodeClass := NewClassWithInstVars("Node", vm.ObjectClass, []string{"value", "next"})
	vm.Classes.Register(nodeClass)

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
	vm.SetGlobal("listHead", node1.ToValue())

	var buf bytes.Buffer
	err := vm.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	// Check that we collected all 3 nodes
	data := buf.Bytes()
	objectCount := ReadUint32(data[12:])
	if objectCount < 3 {
		t.Errorf("Object count = %d, want at least 3", objectCount)
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
	if buf.Len() < ImageHeaderSize {
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
	if info.Size() < int64(ImageHeaderSize) {
		t.Errorf("Image file too small: %d bytes", info.Size())
	}

	// Read file and verify magic
	data, err := os.ReadFile(imagePath)
	if err != nil {
		t.Fatalf("Failed to read image file: %v", err)
	}

	if !bytes.Equal(data[0:4], ImageMagic[:]) {
		t.Errorf("Magic number mismatch in file")
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

	if buf.Len() < ImageHeaderSize {
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

	if buf.Len() < ImageHeaderSize {
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

	if buf.Len() < ImageHeaderSize {
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

	data := buf.Bytes()
	objectCount := ReadUint32(data[12:])
	if objectCount < 100 {
		t.Errorf("Object count in header = %d, want at least 100", objectCount)
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
	// The global stores the symbol Value; the loaded VM's symbol table must
	// map that symbol ID back to the same name.
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
// Benchmarks
// ---------------------------------------------------------------------------

func BenchmarkImageWriterCreate(b *testing.B) {
	for i := 0; i < b.N; i++ {
		_ = NewImageWriter()
	}
}

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
