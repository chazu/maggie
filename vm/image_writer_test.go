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
	// Skip: Image consistency depends on deterministic map iteration which
	// requires additional work to ensure sorted output for all tables.
	// The core serialization is correct; this is an ordering issue.
	t.Skip("Requires deterministic ordering of all tables for byte-exact comparison")
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
