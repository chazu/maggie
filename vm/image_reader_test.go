package vm

import (
	"bytes"
	"errors"
	"testing"
)

// ---------------------------------------------------------------------------
// Test Helpers: Building test images
// ---------------------------------------------------------------------------

// testImageBuilder helps construct test images for testing the reader.
type testImageBuilder struct {
	buf bytes.Buffer
}

func newTestImageBuilder() *testImageBuilder {
	return &testImageBuilder{}
}

// writeUint32 writes a uint32 in little-endian format.
func (b *testImageBuilder) writeUint32(v uint32) {
	buf := make([]byte, 4)
	WriteUint32(buf, v)
	b.buf.Write(buf)
}

// writeUint64 writes a uint64 in little-endian format.
func (b *testImageBuilder) writeUint64(v uint64) {
	buf := make([]byte, 8)
	WriteUint64(buf, v)
	b.buf.Write(buf)
}

// writeString writes a length-prefixed string (uint32 length + bytes).
// This matches the format used by image_writer.go.
func (b *testImageBuilder) writeString(s string) {
	buf := make([]byte, 4)
	WriteUint32(buf, uint32(len(s)))
	b.buf.Write(buf)
	b.buf.WriteString(s)
}

// writeBytes writes raw bytes.
func (b *testImageBuilder) writeBytes(data []byte) {
	b.buf.Write(data)
}

// writeHeader writes a standard image header.
func (b *testImageBuilder) writeHeader(objectCount, entryPoint uint32) {
	b.buf.Write(ImageMagic[:])
	b.writeUint32(ImageVersion)
	b.writeUint32(ImageFlagNone)
	b.writeUint32(objectCount)
	b.writeUint64(0) // String table offset (placeholder)
	b.writeUint64(0) // Class table offset (placeholder)
	b.writeUint32(entryPoint)
}

// writeValue writes an encoded value.
func (b *testImageBuilder) writeValue(v Value) {
	enc := NewImageEncoder()
	data := enc.EncodeValue(v)
	b.buf.Write(data)
}

// bytes returns the built image data.
func (b *testImageBuilder) bytes() []byte {
	return b.buf.Bytes()
}

// ---------------------------------------------------------------------------
// Header Tests
// ---------------------------------------------------------------------------

func TestImageReaderHeaderValid(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(10, 5)

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	header, err := ir.ReadHeader()
	if err != nil {
		t.Fatalf("ReadHeader failed: %v", err)
	}

	if header.Magic != string(ImageMagic[:]) {
		t.Errorf("Magic = %q, want %q", header.Magic, string(ImageMagic[:]))
	}
	if header.Version != ImageVersion {
		t.Errorf("Version = %d, want %d", header.Version, ImageVersion)
	}
	if header.Flags != ImageFlagNone {
		t.Errorf("Flags = %d, want %d", header.Flags, ImageFlagNone)
	}
	if header.ObjectCount != 10 {
		t.Errorf("ObjectCount = %d, want 10", header.ObjectCount)
	}
	if header.EntryPoint != 5 {
		t.Errorf("EntryPoint = %d, want 5", header.EntryPoint)
	}
}

func TestImageReaderInvalidMagic(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeBytes([]byte("XXXX")) // Invalid magic
	builder.writeUint32(ImageVersion)
	builder.writeUint32(ImageFlagNone)
	builder.writeUint32(0)
	builder.writeUint64(0)
	builder.writeUint64(0)
	builder.writeUint32(0)

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	_, err = ir.ReadHeader()
	if err == nil {
		t.Fatal("Expected error for invalid magic, got nil")
	}
}

func TestImageReaderVersionMismatch(t *testing.T) {
	builder := newTestImageBuilder()
	builder.buf.Write(ImageMagic[:])
	builder.writeUint32(999) // Invalid version
	builder.writeUint32(ImageFlagNone)
	builder.writeUint32(0)
	builder.writeUint64(0)
	builder.writeUint64(0)
	builder.writeUint32(0)

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	_, err = ir.ReadHeader()
	if err == nil {
		t.Fatal("Expected error for version mismatch, got nil")
	}
}

func TestImageReaderCorruptHeader(t *testing.T) {
	// Header too short
	data := []byte("MAGI") // Only magic, missing rest

	_, err := NewImageReaderFromBytes(data)
	if err != ErrCorruptHeader {
		t.Errorf("Expected ErrCorruptHeader, got %v", err)
	}
}

// ---------------------------------------------------------------------------
// String Table Tests
// ---------------------------------------------------------------------------

func TestImageReaderStringTable(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// Write string table: count + strings
	builder.writeUint32(3) // 3 strings
	builder.writeString("hello")
	builder.writeString("world")
	builder.writeString("test")

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	_, err = ir.ReadHeader()
	if err != nil {
		t.Fatalf("ReadHeader failed: %v", err)
	}

	strings, err := ir.ReadStringTable()
	if err != nil {
		t.Fatalf("ReadStringTable failed: %v", err)
	}

	expected := []string{"hello", "world", "test"}
	if len(strings) != len(expected) {
		t.Fatalf("String count = %d, want %d", len(strings), len(expected))
	}

	for i, s := range expected {
		if strings[i] != s {
			t.Errorf("strings[%d] = %q, want %q", i, strings[i], s)
		}
	}
}

func TestImageReaderEmptyStringTable(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)
	builder.writeUint32(0) // 0 strings

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	_, err = ir.ReadHeader()
	if err != nil {
		t.Fatalf("ReadHeader failed: %v", err)
	}

	strings, err := ir.ReadStringTable()
	if err != nil {
		t.Fatalf("ReadStringTable failed: %v", err)
	}

	if len(strings) != 0 {
		t.Errorf("Expected empty string table, got %d strings", len(strings))
	}
}

func TestImageReaderGetString(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)
	builder.writeUint32(2)
	builder.writeString("first")
	builder.writeString("second")

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	ir.ReadHeader()
	ir.ReadStringTable()

	s, err := ir.GetString(0)
	if err != nil || s != "first" {
		t.Errorf("GetString(0) = %q, %v; want 'first', nil", s, err)
	}

	s, err = ir.GetString(1)
	if err != nil || s != "second" {
		t.Errorf("GetString(1) = %q, %v; want 'second', nil", s, err)
	}

	_, err = ir.GetString(99)
	if err == nil {
		t.Error("GetString(99) should return error")
	}
}

// ---------------------------------------------------------------------------
// Symbol Table Tests
// ---------------------------------------------------------------------------

func TestImageReaderSymbolTable(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table
	builder.writeUint32(3)
	builder.writeString("foo")
	builder.writeString("bar")
	builder.writeString("baz")

	// Symbol table (references string indices)
	builder.writeUint32(2)
	builder.writeUint32(0) // Symbol 0 -> string "foo"
	builder.writeUint32(2) // Symbol 1 -> string "baz"

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	ir.ReadHeader()
	ir.ReadStringTable()

	symbols, err := ir.ReadSymbolTable()
	if err != nil {
		t.Fatalf("ReadSymbolTable failed: %v", err)
	}

	if len(symbols) != 2 {
		t.Fatalf("Symbol count = %d, want 2", len(symbols))
	}

	if symbols[0] != "foo" {
		t.Errorf("symbols[0] = %q, want 'foo'", symbols[0])
	}
	if symbols[1] != "baz" {
		t.Errorf("symbols[1] = %q, want 'baz'", symbols[1])
	}
}

func TestImageReaderSymbolTableInvalidStringIndex(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table with only 1 string
	builder.writeUint32(1)
	builder.writeString("only")

	// Symbol table referencing non-existent string
	builder.writeUint32(1)
	builder.writeUint32(99) // Invalid string index

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	ir.ReadHeader()
	ir.ReadStringTable()

	_, err = ir.ReadSymbolTable()
	if err == nil {
		t.Error("Expected error for invalid string index")
	}
}

// ---------------------------------------------------------------------------
// Selector Table Tests
// ---------------------------------------------------------------------------

func TestImageReaderSelectorTable(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table
	builder.writeUint32(2)
	builder.writeString("at:")
	builder.writeString("put:")

	// Symbol table
	builder.writeUint32(2)
	builder.writeUint32(0) // Symbol 0 -> "at:"
	builder.writeUint32(1) // Symbol 1 -> "put:"

	// Selector table
	builder.writeUint32(2)
	builder.writeUint32(0) // Selector 0 -> Symbol 0
	builder.writeUint32(1) // Selector 1 -> Symbol 1

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()

	selectors, err := ir.ReadSelectorTable()
	if err != nil {
		t.Fatalf("ReadSelectorTable failed: %v", err)
	}

	if len(selectors) != 2 {
		t.Fatalf("Selector count = %d, want 2", len(selectors))
	}
}

// ---------------------------------------------------------------------------
// Class Reading Tests
// ---------------------------------------------------------------------------

func TestImageReaderClasses(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table
	builder.writeUint32(4)
	builder.writeString("TestClass")
	builder.writeString("x")
	builder.writeString("y")
	builder.writeString("ChildClass")

	// Symbol table (empty for this test)
	builder.writeUint32(0)

	// Selector table (empty for this test)
	builder.writeUint32(0)

	// Class table (format: name, namespace, superclass, numSlots, instVarCount, instVarNames, methodCount)
	builder.writeUint32(2) // 2 classes

	// Class 0: TestClass
	builder.writeUint32(0)          // name: "TestClass"
	builder.writeUint32(0xFFFFFFFF) // namespace: empty
	builder.writeUint32(0xFFFFFFFF) // superclass: nil (will be Object)
	builder.writeUint32(2)          // numSlots
	builder.writeUint32(2)          // 2 instance variables
	builder.writeUint32(1)          // instvar 0: "x"
	builder.writeUint32(2)          // instvar 1: "y"
	builder.writeUint32(0)          // 0 instance methods
	builder.writeUint32(0)          // 0 class methods
	builder.writeBytes([]byte{0})   // no class docstring (v2+)

	// Class 1: ChildClass extends TestClass (index 0)
	builder.writeUint32(3)          // name: "ChildClass"
	builder.writeUint32(0xFFFFFFFF) // namespace: empty
	builder.writeUint32(0)          // superclass: TestClass (index 0)
	builder.writeUint32(2)          // numSlots (inherited from parent)
	builder.writeUint32(0)          // 0 instance variables
	builder.writeUint32(0)          // 0 instance methods
	builder.writeUint32(0)          // 0 class methods
	builder.writeBytes([]byte{0})   // no class docstring (v2+)

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()

	classes, err := ir.ReadClasses(vm)
	if err != nil {
		t.Fatalf("ReadClasses failed: %v", err)
	}

	if len(classes) != 2 {
		t.Fatalf("Class count = %d, want 2", len(classes))
	}

	// Check TestClass
	tc := classes[0]
	if tc.Name != "TestClass" {
		t.Errorf("classes[0].Name = %q, want 'TestClass'", tc.Name)
	}
	if len(tc.InstVars) != 2 {
		t.Errorf("TestClass.InstVars length = %d, want 2", len(tc.InstVars))
	}
	if tc.InstVars[0] != "x" || tc.InstVars[1] != "y" {
		t.Errorf("TestClass.InstVars = %v, want [x, y]", tc.InstVars)
	}

	// Check ChildClass
	cc := classes[1]
	if cc.Name != "ChildClass" {
		t.Errorf("classes[1].Name = %q, want 'ChildClass'", cc.Name)
	}
	if cc.Superclass != tc {
		t.Error("ChildClass.Superclass should be TestClass")
	}
}

func TestImageReaderClassesUsesExistingVMClass(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table
	builder.writeUint32(1)
	builder.writeString("SmallInteger") // Existing VM class

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// Class table (format: name, namespace, superclass, numSlots, instVarCount, instMethods, classMethods)
	builder.writeUint32(1)
	builder.writeUint32(0)          // name: "SmallInteger"
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)          // numSlots
	builder.writeUint32(0)          // 0 instance variables
	builder.writeUint32(0)          // 0 instance methods
	builder.writeUint32(0)          // 0 class methods
	builder.writeBytes([]byte{0})   // no class docstring (v2+)

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()
	originalSmallInt := vm.SmallIntegerClass

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()

	classes, err := ir.ReadClasses(vm)
	if err != nil {
		t.Fatalf("ReadClasses failed: %v", err)
	}

	// Should use existing VM class, not create new one
	if classes[0] != originalSmallInt {
		t.Error("Expected to reuse existing SmallInteger class")
	}
}

// ---------------------------------------------------------------------------
// Method Reading Tests
// ---------------------------------------------------------------------------

func TestImageReaderMethods(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table
	builder.writeUint32(2)
	builder.writeString("testMethod")
	builder.writeString("TestClass")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// Classes (format: name, namespace, superclass, numSlots, instVarCount, instMethods, classMethods)
	builder.writeUint32(1)
	builder.writeUint32(1)          // name: "TestClass"
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)          // numSlots
	builder.writeUint32(0)          // 0 instance variables
	builder.writeUint32(0)          // 0 instance methods
	builder.writeUint32(0)          // 0 class methods
	builder.writeBytes([]byte{0})   // no class docstring (v2+)

	// Methods (format: selector, class, name, isClassMethod, arity, numTemps, literals, bytecode, blocks, source, sourceMap)
	builder.writeUint32(1) // 1 method

	// Method 0
	builder.writeUint32(42)       // selector ID
	builder.writeUint32(0)        // class index
	builder.writeUint32(0)        // name: "testMethod"
	builder.writeBytes([]byte{0}) // isClassMethod: false
	builder.writeUint32(2)        // arity
	builder.writeUint32(5)        // numTemps

	// Literals
	builder.writeUint32(1) // 1 literal
	builder.writeValue(FromSmallInt(100))

	// Bytecode
	builder.writeUint32(3)   // 3 bytes
	builder.writeBytes([]byte{0x01, 0x02, 0x03})

	// Blocks
	builder.writeUint32(0) // 0 blocks

	// Source (1 byte flag, then optional string index)
	builder.writeBytes([]byte{0}) // no source

	// DocString (1 byte flag, then optional string index, v2+)
	builder.writeBytes([]byte{0}) // no docstring

	// Source map
	builder.writeUint32(1)  // 1 entry
	builder.writeUint32(0)  // offset
	builder.writeUint32(1)  // line
	builder.writeUint32(10) // column

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()
	ir.ReadClasses(vm)

	methods, err := ir.ReadMethods(vm)
	if err != nil {
		t.Fatalf("ReadMethods failed: %v", err)
	}

	if len(methods) != 1 {
		t.Fatalf("Method count = %d, want 1", len(methods))
	}

	m := methods[0]
	if m.Name() != "testMethod" {
		t.Errorf("Method name = %q, want 'testMethod'", m.Name())
	}
	// Selector should match VM's selector ID for "testMethod", not the raw image value
	expectedSelector := vm.Selectors.Lookup("testMethod")
	if m.Selector() != expectedSelector {
		t.Errorf("Method selector = %d, want %d (VM's ID for 'testMethod')", m.Selector(), expectedSelector)
	}
	if m.Arity != 2 {
		t.Errorf("Method arity = %d, want 2", m.Arity)
	}
	if m.NumTemps != 5 {
		t.Errorf("Method numTemps = %d, want 5", m.NumTemps)
	}
	if len(m.Literals) != 1 {
		t.Errorf("Method literals count = %d, want 1", len(m.Literals))
	}
	if !m.Literals[0].IsSmallInt() || m.Literals[0].SmallInt() != 100 {
		t.Errorf("Method literal 0 = %v, want SmallInt(100)", m.Literals[0])
	}
	if len(m.Bytecode) != 3 {
		t.Errorf("Bytecode length = %d, want 3", len(m.Bytecode))
	}
	if len(m.SourceMap) != 1 {
		t.Errorf("SourceMap length = %d, want 1", len(m.SourceMap))
	}
}

func TestImageReaderMethodsWithBlocks(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table
	builder.writeUint32(2)
	builder.writeString("blockMethod")
	builder.writeString("TestClass")

	builder.writeUint32(0)
	builder.writeUint32(0)

	// Classes (format: name, namespace, superclass, numSlots, instVarCount, instMethods, classMethods)
	builder.writeUint32(1)
	builder.writeUint32(1)          // name
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)          // numSlots
	builder.writeUint32(0)          // instVarCount
	builder.writeUint32(0)          // instance methods
	builder.writeUint32(0)          // class methods
	builder.writeBytes([]byte{0})   // no class docstring (v2+)

	// Methods (format: selector, class, name, isClassMethod, arity, numTemps, literals, bytecode, blocks, source, sourceMap)
	builder.writeUint32(1)

	builder.writeUint32(1)        // selector
	builder.writeUint32(0)        // class
	builder.writeUint32(0)        // name
	builder.writeBytes([]byte{0}) // isClassMethod: false
	builder.writeUint32(0)        // arity
	builder.writeUint32(0)        // numTemps
	builder.writeUint32(0)        // literals
	builder.writeUint32(1)        // bytecode length
	builder.writeBytes([]byte{0x00})

	// 1 block
	builder.writeUint32(1)

	// Block
	builder.writeUint32(1) // arity
	builder.writeUint32(2) // numTemps
	builder.writeUint32(1) // numCaptures
	builder.writeUint32(0) // literals
	builder.writeUint32(2) // bytecode length
	builder.writeBytes([]byte{0xAA, 0xBB})
	builder.writeUint32(0) // source map

	builder.writeBytes([]byte{0}) // no source (1 byte flag)
	builder.writeBytes([]byte{0}) // no docstring (1 byte flag, v2+)
	builder.writeUint32(0) // method source map count

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()
	ir.ReadClasses(vm)

	methods, err := ir.ReadMethods(vm)
	if err != nil {
		t.Fatalf("ReadMethods failed: %v", err)
	}

	m := methods[0]
	if len(m.Blocks) != 1 {
		t.Fatalf("Block count = %d, want 1", len(m.Blocks))
	}

	block := m.Blocks[0]
	if block.Arity != 1 {
		t.Errorf("Block arity = %d, want 1", block.Arity)
	}
	if block.NumTemps != 2 {
		t.Errorf("Block numTemps = %d, want 2", block.NumTemps)
	}
	if block.NumCaptures != 1 {
		t.Errorf("Block numCaptures = %d, want 1", block.NumCaptures)
	}
	if block.Outer != m {
		t.Error("Block.Outer should reference containing method")
	}
}

// ---------------------------------------------------------------------------
// Object Reading Tests
// ---------------------------------------------------------------------------

func TestImageReaderObjects(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(2, 0)

	// String table
	builder.writeUint32(3)
	builder.writeString("TestClass")
	builder.writeString("x")
	builder.writeString("y")

	builder.writeUint32(0)
	builder.writeUint32(0)

	// Classes (format: name, namespace, superclass, numSlots, instVarCount, instVarNames, instMethods, classMethods)
	builder.writeUint32(1)
	builder.writeUint32(0)          // name
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(2)          // numSlots
	builder.writeUint32(2)          // 2 instance variables
	builder.writeUint32(1)          // "x"
	builder.writeUint32(2)          // "y"
	builder.writeUint32(0)          // 0 instance methods
	builder.writeUint32(0)          // 0 class methods
	builder.writeBytes([]byte{0})   // no class docstring (v2+)

	// Methods (empty)
	builder.writeUint32(0)

	// Objects
	builder.writeUint32(2) // 2 objects

	// Object 0
	builder.writeUint32(0) // class index
	builder.writeUint32(2) // 2 slots
	builder.writeValue(FromSmallInt(10))
	builder.writeValue(FromSmallInt(20))

	// Object 1
	builder.writeUint32(0) // class index
	builder.writeUint32(2) // 2 slots
	builder.writeValue(FromSmallInt(100))
	builder.writeValue(FromSmallInt(200))

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()
	ir.ReadClasses(vm)
	ir.ReadMethods(vm)

	objects, err := ir.ReadObjectsWithSlots(vm)
	if err != nil {
		t.Fatalf("ReadObjectsWithSlots failed: %v", err)
	}

	if len(objects) != 2 {
		t.Fatalf("Object count = %d, want 2", len(objects))
	}

	obj0 := objects[0]
	if obj0.GetSlot(0).SmallInt() != 10 {
		t.Errorf("obj0.slot0 = %d, want 10", obj0.GetSlot(0).SmallInt())
	}
	if obj0.GetSlot(1).SmallInt() != 20 {
		t.Errorf("obj0.slot1 = %d, want 20", obj0.GetSlot(1).SmallInt())
	}

	obj1 := objects[1]
	if obj1.GetSlot(0).SmallInt() != 100 {
		t.Errorf("obj1.slot0 = %d, want 100", obj1.GetSlot(0).SmallInt())
	}
}

// ---------------------------------------------------------------------------
// Globals Reading Tests
// ---------------------------------------------------------------------------

func TestImageReaderGlobals(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table
	builder.writeUint32(2)
	builder.writeString("MyGlobal")
	builder.writeString("AnotherGlobal")

	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0) // classes
	builder.writeUint32(0) // methods
	builder.writeUint32(0) // objects

	// Globals
	builder.writeUint32(2) // 2 globals

	builder.writeUint32(0) // name: "MyGlobal"
	builder.writeValue(FromSmallInt(42))

	builder.writeUint32(1) // name: "AnotherGlobal"
	builder.writeValue(True)

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()
	ir.ReadClasses(vm)
	ir.ReadMethods(vm)
	ir.ReadObjectsWithSlots(vm)

	err = ir.ReadGlobals(vm)
	if err != nil {
		t.Fatalf("ReadGlobals failed: %v", err)
	}

	val, ok := vm.LookupGlobal("MyGlobal")
	if !ok {
		t.Error("MyGlobal not found in VM globals")
	} else if !val.IsSmallInt() || val.SmallInt() != 42 {
		t.Errorf("MyGlobal = %v, want SmallInt(42)", val)
	}

	val, ok = vm.LookupGlobal("AnotherGlobal")
	if !ok {
		t.Error("AnotherGlobal not found")
	} else if val != True {
		t.Errorf("AnotherGlobal = %v, want True", val)
	}
}

// ---------------------------------------------------------------------------
// Full Image Load Tests
// ---------------------------------------------------------------------------

func TestImageReaderReadAll(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(1, 0)

	// String table
	builder.writeUint32(3)
	builder.writeString("TestClass")
	builder.writeString("value")
	builder.writeString("myGlobal")

	// Symbol table
	builder.writeUint32(1)
	builder.writeUint32(1) // "value"

	// Selector table
	builder.writeUint32(0)

	// Classes (format: name, namespace, superclass, numSlots, instVarCount, instVarNames, instMethods, classMethods)
	builder.writeUint32(1)
	builder.writeUint32(0)          // name: "TestClass"
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(1)          // numSlots
	builder.writeUint32(1)          // 1 instvar
	builder.writeUint32(1)          // instvar "value"
	builder.writeUint32(0)          // 0 instance methods
	builder.writeUint32(0)          // 0 class methods
	builder.writeBytes([]byte{0})   // no class docstring (v2+)

	// Methods
	builder.writeUint32(0)

	// Objects
	builder.writeUint32(1)
	builder.writeUint32(0)
	builder.writeUint32(1)
	builder.writeValue(FromSmallInt(999))

	// Globals
	builder.writeUint32(1)
	builder.writeUint32(2) // "myGlobal"
	builder.writeValue(FromSmallInt(123))

	// Class variables (v3+)
	builder.writeUint32(0) // no class vars

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	err = ir.ReadAll(vm)
	if err != nil {
		t.Fatalf("ReadAll failed: %v", err)
	}

	// Verify class was registered
	cls := vm.LookupClass("TestClass")
	if cls == nil {
		t.Error("TestClass not found in VM")
	}

	// Verify global was set
	val, ok := vm.LookupGlobal("myGlobal")
	if !ok {
		t.Error("myGlobal not found")
	} else if val.SmallInt() != 123 {
		t.Errorf("myGlobal = %v, want 123", val)
	}

	// Verify symbol was interned
	symID, ok := vm.Symbols.Lookup("value")
	if !ok {
		t.Error("Symbol 'value' not interned")
	}
	_ = symID
}

func TestVMLoadImageFromBytes(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	builder.writeUint32(1) // strings
	builder.writeString("test")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors
	builder.writeUint32(0) // classes
	builder.writeUint32(0) // methods
	builder.writeUint32(0) // objects
	builder.writeUint32(0) // globals
	builder.writeUint32(0) // class vars (v3+)

	vm := NewVM()
	err := vm.LoadImageFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}
}

func TestVMLoadImageFrom(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	builder.writeUint32(1)
	builder.writeString("test")

	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0) // class vars (v3+)

	reader := bytes.NewReader(builder.bytes())
	vm := NewVM()
	err := vm.LoadImageFrom(reader)
	if err != nil {
		t.Fatalf("LoadImageFrom failed: %v", err)
	}
}

// ---------------------------------------------------------------------------
// Error Handling Tests
// ---------------------------------------------------------------------------

func TestImageReaderUnexpectedEOF(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)
	builder.writeUint32(100) // Claim 100 strings but don't provide them

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	ir.ReadHeader()
	_, err = ir.ReadStringTable()
	if err == nil {
		t.Error("Expected error for unexpected EOF")
	}
}

func TestImageReaderInvalidClassIndex(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(1, 0)

	builder.writeUint32(1)
	builder.writeString("TestClass")

	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0) // 0 classes

	builder.writeUint32(0) // methods

	// Object referencing non-existent class
	builder.writeUint32(1)
	builder.writeUint32(99) // Invalid class index
	builder.writeUint32(0)

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()
	ir.ReadClasses(vm)
	ir.ReadMethods(vm)

	_, err = ir.ReadObjectsWithSlots(vm)
	if err == nil {
		t.Error("Expected error for invalid class index")
	}
}

// ---------------------------------------------------------------------------
// NewImageReader from io.Reader Test
// ---------------------------------------------------------------------------

func TestNewImageReaderFromReader(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0)

	reader := bytes.NewReader(builder.bytes())
	ir, err := NewImageReader(reader)
	if err != nil {
		t.Fatalf("NewImageReader failed: %v", err)
	}

	header, err := ir.ReadHeader()
	if err != nil {
		t.Fatalf("ReadHeader failed: %v", err)
	}

	if header.Magic != string(ImageMagic[:]) {
		t.Errorf("Magic = %q, want %q", header.Magic, string(ImageMagic[:]))
	}
}

// ---------------------------------------------------------------------------
// Edge Cases
// ---------------------------------------------------------------------------

func TestImageReaderUnicodeStrings(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	builder.writeUint32(3)
	builder.writeString("Hello, World!")
	builder.writeString("Hello, 世界!")
	builder.writeString("こんにちは")

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	ir.ReadHeader()
	strings, err := ir.ReadStringTable()
	if err != nil {
		t.Fatalf("ReadStringTable failed: %v", err)
	}

	if strings[0] != "Hello, World!" {
		t.Errorf("strings[0] = %q, want 'Hello, World!'", strings[0])
	}
	if strings[1] != "Hello, 世界!" {
		t.Errorf("strings[1] = %q, want 'Hello, 世界!'", strings[1])
	}
	if strings[2] != "こんにちは" {
		t.Errorf("strings[2] = %q, want 'こんにちは'", strings[2])
	}
}

func TestImageReaderEmptyImage(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)
	builder.writeUint32(0) // strings
	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors
	builder.writeUint32(0) // classes
	builder.writeUint32(0) // methods
	builder.writeUint32(0) // objects
	builder.writeUint32(0) // globals
	builder.writeUint32(0) // class vars (v3+)

	vm := NewVM()
	err := vm.LoadImageFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("LoadImageFromBytes failed for empty image: %v", err)
	}
}

func TestImageReaderLargeStringTable(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	count := uint32(100)
	builder.writeUint32(count)
	for i := uint32(0); i < count; i++ {
		builder.writeString("string_" + string(rune('a'+i%26)))
	}

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	ir.ReadHeader()
	strings, err := ir.ReadStringTable()
	if err != nil {
		t.Fatalf("ReadStringTable failed: %v", err)
	}

	if uint32(len(strings)) != count {
		t.Errorf("String count = %d, want %d", len(strings), count)
	}
}

// ---------------------------------------------------------------------------
// Decoder Integration Tests
// ---------------------------------------------------------------------------

func TestImageReaderDecoderIntegration(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	builder.writeUint32(2)
	builder.writeString("testSymbol")
	builder.writeString("TestClass")

	// Symbols
	builder.writeUint32(1)
	builder.writeUint32(0) // Symbol 0 -> "testSymbol"

	builder.writeUint32(0) // selectors

	// Classes (format: name, namespace, superclass, numSlots, instVarCount, instMethods, classMethods)
	builder.writeUint32(1)
	builder.writeUint32(1)          // name
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)          // numSlots
	builder.writeUint32(0)          // instVarCount
	builder.writeUint32(0)          // instance methods
	builder.writeUint32(0)          // class methods
	builder.writeBytes([]byte{0})   // no class docstring (v2+)

	builder.writeUint32(0) // methods
	builder.writeUint32(0) // objects
	builder.writeUint32(0) // globals
	builder.writeUint32(0) // class vars (v3+)

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()
	err = ir.ReadAll(vm)
	if err != nil {
		t.Fatalf("ReadAll failed: %v", err)
	}

	// Verify decoder has correct mappings
	decoder := ir.Decoder()
	if decoder.StringCount() != 2 {
		t.Errorf("Decoder string count = %d, want 2", decoder.StringCount())
	}
	if decoder.ClassCount() != 1 {
		t.Errorf("Decoder class count = %d, want 1", decoder.ClassCount())
	}
}

// ---------------------------------------------------------------------------
// Corrupt / Malformed Image Error Handling Tests
// ---------------------------------------------------------------------------

// TestImageReaderBadVersionNumber tests that a very high version number
// (999) is rejected with ErrVersionMismatch.
func TestImageReaderBadVersionNumber(t *testing.T) {
	builder := newTestImageBuilder()
	builder.buf.Write(ImageMagic[:])
	builder.writeUint32(999) // Version far too high
	builder.writeUint32(ImageFlagNone)
	builder.writeUint32(0)
	builder.writeUint64(0)
	builder.writeUint64(0)
	builder.writeUint32(0)

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	_, err = ir.ReadHeader()
	if err == nil {
		t.Fatal("Expected error for version 999, got nil")
	}
	if !errors.Is(err, ErrVersionMismatch) {
		t.Errorf("Expected ErrVersionMismatch, got: %v", err)
	}
}

// TestImageReaderZeroLengthImage tests an image that has only the magic bytes
// but nothing else (too short for a full header).
func TestImageReaderZeroLengthImage(t *testing.T) {
	// Just the magic bytes, no header fields
	data := []byte("MAGI")

	_, err := NewImageReaderFromBytes(data)
	if err != ErrCorruptHeader {
		t.Errorf("Expected ErrCorruptHeader for magic-only image, got: %v", err)
	}
}

// TestImageReaderEmptyBytes tests a completely empty byte slice.
func TestImageReaderEmptyBytes(t *testing.T) {
	_, err := NewImageReaderFromBytes([]byte{})
	if err != ErrCorruptHeader {
		t.Errorf("Expected ErrCorruptHeader for empty bytes, got: %v", err)
	}
}

// TestImageReaderRandomGarbageWithValidMagic tests random garbage data that
// starts with a valid magic number. The header fields will be nonsensical.
func TestImageReaderRandomGarbageWithValidMagic(t *testing.T) {
	// Valid magic + random garbage
	data := make([]byte, ImageHeaderSize+50)
	copy(data[0:4], ImageMagic[:])
	// Fill the rest with garbage
	for i := 4; i < len(data); i++ {
		data[i] = byte(i * 37)
	}
	// Force version to be 0 (valid) to pass header check and exercise later parsing
	WriteUint32(data[4:], 0)

	ir, err := NewImageReaderFromBytes(data)
	if err != nil {
		// ErrCorruptHeader is acceptable
		return
	}

	// ReadHeader might succeed since magic and version are valid
	_, err = ir.ReadHeader()
	if err != nil {
		// Any error is acceptable, as long as no panic
		return
	}

	// Try reading string table from garbage data -- should error, not panic
	_, err = ir.ReadStringTable()
	// We don't care which specific error, just that it doesn't panic
	_ = err
}

// TestImageReaderTruncatedDataMidClassSection tests an image with a valid
// header, valid string table, but truncated in the middle of the class section.
func TestImageReaderTruncatedDataMidClassSection(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// Write a valid string table
	builder.writeUint32(2) // 2 strings
	builder.writeString("TestClass")
	builder.writeString("x")

	// Write valid symbol and selector tables
	builder.writeUint32(0) // 0 symbols
	builder.writeUint32(0) // 0 selectors

	// Start writing class table but truncate mid-way
	builder.writeUint32(2) // Claim 2 classes
	// Write partial first class (name index only, missing the rest)
	builder.writeUint32(0) // name: "TestClass"
	// Truncate here - missing namespace, superclass, numSlots, etc.

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()

	_, err = ir.ReadClasses(vm)
	if err == nil {
		t.Error("Expected error for truncated class section, got nil")
	}
}

// TestImageReaderTruncatedDataMidMethodSection tests truncation in the middle
// of the method section.
func TestImageReaderTruncatedDataMidMethodSection(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table
	builder.writeUint32(2)
	builder.writeString("testMethod")
	builder.writeString("TestClass")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// Classes
	builder.writeUint32(1)
	builder.writeUint32(1)          // name: "TestClass"
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)          // numSlots
	builder.writeUint32(0)          // instVarCount
	builder.writeUint32(0)          // instance methods
	builder.writeUint32(0)          // class methods
	builder.writeBytes([]byte{0})   // no docstring

	// Start method section but truncate mid-way
	builder.writeUint32(1)    // Claim 1 method
	builder.writeUint32(42)   // selector
	builder.writeUint32(0)    // class index
	// Truncate here - missing name, arity, bytecode, etc.

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()
	ir.ReadClasses(vm)

	_, err = ir.ReadMethods(vm)
	if err == nil {
		t.Error("Expected error for truncated method section, got nil")
	}
}

// TestImageReaderObjectOutOfBoundsClassIndex tests an object that references
// a class index exceeding the class count.
func TestImageReaderObjectOutOfBoundsClassIndex(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(1, 0)

	// String table
	builder.writeUint32(1)
	builder.writeString("TestClass")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// 1 class
	builder.writeUint32(1)
	builder.writeUint32(0)          // name: "TestClass"
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)          // numSlots
	builder.writeUint32(0)          // instVarCount
	builder.writeUint32(0)          // instance methods
	builder.writeUint32(0)          // class methods
	builder.writeBytes([]byte{0})   // no docstring

	// 0 methods
	builder.writeUint32(0)

	// Object with out-of-bounds class index
	builder.writeUint32(1)  // 1 object
	builder.writeUint32(50) // class index 50, but only 1 class exists
	builder.writeUint32(0)  // 0 slots

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()
	ir.ReadClasses(vm)
	ir.ReadMethods(vm)

	_, err = ir.ReadObjectsWithSlots(vm)
	if err == nil {
		t.Error("Expected error for object with out-of-bounds class index, got nil")
	}
	if !errors.Is(err, ErrInvalidClassIndex) {
		t.Errorf("Expected ErrInvalidClassIndex, got: %v", err)
	}
}

// TestImageReaderMethodOutOfBoundsClassIndex tests a method that references
// a class index exceeding the class count. Note: The current reader silently
// ignores out-of-bounds class indices in methods (it just skips VTable
// registration), so this test verifies no panic occurs.
func TestImageReaderMethodOutOfBoundsClassIndex(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table
	builder.writeUint32(2)
	builder.writeString("badMethod")
	builder.writeString("TestClass")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// 1 class
	builder.writeUint32(1)
	builder.writeUint32(1)          // name: "TestClass"
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)          // numSlots
	builder.writeUint32(0)          // instVarCount
	builder.writeUint32(0)          // instance methods
	builder.writeUint32(0)          // class methods
	builder.writeBytes([]byte{0})   // no docstring

	// 1 method referencing class index 99 (only 1 class exists)
	builder.writeUint32(1) // 1 method

	builder.writeUint32(1)        // selector ID
	builder.writeUint32(99)       // class index -- out of bounds!
	builder.writeUint32(0)        // name: "badMethod"
	builder.writeBytes([]byte{0}) // isClassMethod: false
	builder.writeUint32(0)        // arity
	builder.writeUint32(0)        // numTemps
	builder.writeUint32(0)        // literal count
	builder.writeUint32(1)        // bytecode length
	builder.writeBytes([]byte{0x00})
	builder.writeUint32(0)        // block count
	builder.writeBytes([]byte{0}) // no source
	builder.writeBytes([]byte{0}) // no docstring
	builder.writeUint32(0)        // source map count

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()
	ir.ReadClasses(vm)

	// NOTE: The current reader does NOT return an error for out-of-bounds class
	// index in methods -- it silently skips VTable registration. This test
	// verifies that at least no panic occurs. A future improvement could make
	// this return ErrInvalidClassIndex.
	methods, err := ir.ReadMethods(vm)
	if err != nil {
		// If the reader does return an error, that is also acceptable behavior
		return
	}

	// Method should still be created, just not linked to any class
	if len(methods) != 1 {
		t.Errorf("Expected 1 method, got %d", len(methods))
	}
	if methods[0].class != nil {
		t.Error("Expected method.class to be nil for out-of-bounds class index")
	}
}

// TestImageReaderSymbolTableOutOfBoundsStringIndex tests a symbol entry that
// references a string index beyond the string table.
func TestImageReaderSymbolTableOutOfBoundsStringIndex(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table with only 2 strings
	builder.writeUint32(2)
	builder.writeString("hello")
	builder.writeString("world")

	// Symbol table: one valid, one out of bounds
	builder.writeUint32(2)
	builder.writeUint32(0)    // Valid: string 0 "hello"
	builder.writeUint32(999)  // Invalid: string index 999

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	ir.ReadHeader()
	ir.ReadStringTable()

	_, err = ir.ReadSymbolTable()
	if err == nil {
		t.Error("Expected error for symbol with out-of-bounds string index, got nil")
	}
	if !errors.Is(err, ErrInvalidStringIndex) {
		t.Errorf("Expected ErrInvalidStringIndex, got: %v", err)
	}
}

// TestImageReaderSelectorTableOutOfBoundsStringIndex tests a selector entry
// that references a string index beyond the string table.
func TestImageReaderSelectorTableOutOfBoundsStringIndex(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table with only 1 string
	builder.writeUint32(1)
	builder.writeString("at:")

	// Symbol table (empty)
	builder.writeUint32(0)

	// Selector table referencing non-existent string
	builder.writeUint32(1)
	builder.writeUint32(100) // Invalid string index

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()

	_, err = ir.ReadSelectorTable()
	if err == nil {
		t.Error("Expected error for selector with out-of-bounds string index, got nil")
	}
	if !errors.Is(err, ErrInvalidStringIndex) {
		t.Errorf("Expected ErrInvalidStringIndex, got: %v", err)
	}
}

// TestImageReaderGlobalsOutOfBoundsStringIndex tests a global that references
// a non-existent string index for its name.
func TestImageReaderGlobalsOutOfBoundsStringIndex(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table with 1 string
	builder.writeUint32(1)
	builder.writeString("onlyString")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors
	builder.writeUint32(0) // classes
	builder.writeUint32(0) // methods
	builder.writeUint32(0) // objects

	// Globals: 1 global with invalid string index for name
	builder.writeUint32(1)
	builder.writeUint32(500) // Invalid string index for name
	builder.writeValue(FromSmallInt(42))

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()
	ir.ReadClasses(vm)
	ir.ReadMethods(vm)
	ir.ReadObjectsWithSlots(vm)

	err = ir.ReadGlobals(vm)
	if err == nil {
		t.Error("Expected error for global with out-of-bounds string index, got nil")
	}
	if !errors.Is(err, ErrInvalidStringIndex) {
		t.Errorf("Expected ErrInvalidStringIndex, got: %v", err)
	}
}

// TestImageReaderHugeSlotCount tests an object that claims a very large number
// of slots. This should cause an error (ErrUnexpectedEOF), not a panic.
//
// NOTE: Using 0xFFFFFFFF would cause an OOM kill because the reader allocates
// slotData before validating. We use a large-but-not-catastrophic value (10000)
// that exceeds the remaining data. A future improvement should validate slot
// counts against remaining data before allocating. That is tracked separately.
func TestImageReaderHugeSlotCount(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(1, 0)

	// String table
	builder.writeUint32(1)
	builder.writeString("TestClass")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// 1 class
	builder.writeUint32(1)
	builder.writeUint32(0)          // name: "TestClass"
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)          // numSlots
	builder.writeUint32(0)          // instVarCount
	builder.writeUint32(0)          // instance methods
	builder.writeUint32(0)          // class methods
	builder.writeBytes([]byte{0})   // no docstring

	// 0 methods
	builder.writeUint32(0)

	// 1 object with excessive slot count (exceeds remaining data)
	builder.writeUint32(1)     // 1 object
	builder.writeUint32(0)     // class index 0
	builder.writeUint32(10000) // 10000 slots -- far more than data available
	// No actual slot data

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()
	ir.ReadClasses(vm)
	ir.ReadMethods(vm)

	// This should return an error (not enough data for slots), not panic
	_, err = ir.ReadObjectsWithSlots(vm)
	if err == nil {
		t.Error("Expected error for object with 10000 slots (no data), got nil")
	}
	if !errors.Is(err, ErrUnexpectedEOF) {
		t.Errorf("Expected ErrUnexpectedEOF, got: %v", err)
	}
}

// TestImageReaderHugeInstVarCount tests a class that claims a very large
// number of instance variables. Should error, not panic.
//
// NOTE: Using 0xFFFFFFFF could cause OOM because the reader allocates
// a slice before validating against remaining data. We use a large-but-safe
// value (10000) that exceeds available data. The underlying issue (no
// pre-allocation bounds check) is tracked separately.
func TestImageReaderHugeInstVarCount(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table
	builder.writeUint32(1)
	builder.writeString("BadClass")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// 1 class with excessive instance variable count
	builder.writeUint32(1)
	builder.writeUint32(0)          // name: "BadClass"
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)          // numSlots
	builder.writeUint32(10000)      // instVarCount = 10000, but no data!

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()

	_, err = ir.ReadClasses(vm)
	if err == nil {
		t.Error("Expected error for class with 10000 instvars (no data), got nil")
	}
}

// TestImageReaderHugeMethodCount tests a class that claims a very large
// number of instance methods. Should error, not panic.
//
// NOTE: This uses a moderate value (5000) rather than 0xFFFFFFFF to avoid
// OOM from pre-allocation. The reader does not bound-check counts against
// remaining data before allocating. That is tracked separately.
func TestImageReaderHugeMethodCount(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table
	builder.writeUint32(1)
	builder.writeString("BadClass")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// 1 class with excessive method count
	builder.writeUint32(1)
	builder.writeUint32(0)          // name: "BadClass"
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)          // numSlots
	builder.writeUint32(0)          // instVarCount
	builder.writeUint32(5000)       // method count = 5000, but no data!
	// No actual method index data

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()

	_, err = ir.ReadClasses(vm)
	if err == nil {
		t.Error("Expected error for class with 5000 methods (no data), got nil")
	}
}

// TestImageReaderValidHeaderWrongStringCount tests an image that claims 100
// strings but provides none.
func TestImageReaderValidHeaderWrongStringCount(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// Claim 100 strings but provide 0
	builder.writeUint32(100)

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	ir.ReadHeader()

	_, err = ir.ReadStringTable()
	if err == nil {
		t.Error("Expected error for 100 claimed strings with 0 provided, got nil")
	}
	if !errors.Is(err, ErrUnexpectedEOF) {
		t.Errorf("Expected ErrUnexpectedEOF, got: %v", err)
	}
}

// TestImageReaderValidHeaderWrongSymbolCount tests an image that claims many
// symbols but provides none.
func TestImageReaderValidHeaderWrongSymbolCount(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// Valid string table
	builder.writeUint32(1)
	builder.writeString("test")

	// Claim 50 symbols but provide none
	builder.writeUint32(50)

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	ir.ReadHeader()
	ir.ReadStringTable()

	_, err = ir.ReadSymbolTable()
	if err == nil {
		t.Error("Expected error for 50 claimed symbols with 0 provided, got nil")
	}
	if !errors.Is(err, ErrUnexpectedEOF) {
		t.Errorf("Expected ErrUnexpectedEOF, got: %v", err)
	}
}

// TestImageReaderValidHeaderWrongClassCount tests an image that claims many
// classes but provides none.
func TestImageReaderValidHeaderWrongClassCount(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	builder.writeUint32(1)
	builder.writeString("test")
	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// Claim 200 classes but provide none
	builder.writeUint32(200)

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()

	_, err = ir.ReadClasses(vm)
	if err == nil {
		t.Error("Expected error for 200 claimed classes with 0 provided, got nil")
	}
}

// TestImageReaderValidHeaderWrongObjectCount tests an image that claims many
// objects but provides none.
func TestImageReaderValidHeaderWrongObjectCount(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	builder.writeUint32(1)
	builder.writeString("TestClass")
	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// 1 class
	builder.writeUint32(1)
	builder.writeUint32(0)          // name
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)          // numSlots
	builder.writeUint32(0)          // instVarCount
	builder.writeUint32(0)          // instance methods
	builder.writeUint32(0)          // class methods
	builder.writeBytes([]byte{0})   // no docstring

	builder.writeUint32(0) // methods

	// Claim 500 objects but provide none
	builder.writeUint32(500)

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()
	ir.ReadClasses(vm)
	ir.ReadMethods(vm)

	_, err = ir.ReadObjectsWithSlots(vm)
	if err == nil {
		t.Error("Expected error for 500 claimed objects with 0 provided, got nil")
	}
}

// TestImageReaderClassVarsSectionCorruption tests a v3 image where everything
// up through globals is valid, but the class vars section is corrupt.
func TestImageReaderClassVarsSectionCorruption(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// Valid string table
	builder.writeUint32(2)
	builder.writeString("TestClass")
	builder.writeString("myGlobal")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// 1 class
	builder.writeUint32(1)
	builder.writeUint32(0)          // name: "TestClass"
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)          // numSlots
	builder.writeUint32(0)          // instVarCount
	builder.writeUint32(0)          // instance methods
	builder.writeUint32(0)          // class methods
	builder.writeBytes([]byte{0})   // no docstring

	builder.writeUint32(0) // methods
	builder.writeUint32(0) // objects

	// Valid globals
	builder.writeUint32(1)
	builder.writeUint32(1) // name: "myGlobal"
	builder.writeValue(FromSmallInt(42))

	// Class vars section: claim 5 entries but provide none
	builder.writeUint32(5)

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	err = ir.ReadAll(vm)
	if err == nil {
		t.Error("Expected error for corrupt class vars section, got nil")
	}
}

// TestImageReaderClassVarsOutOfBoundsClassIndex tests a v3 image where
// the class vars section references a class index beyond the class table.
func TestImageReaderClassVarsOutOfBoundsClassIndex(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table
	builder.writeUint32(2)
	builder.writeString("TestClass")
	builder.writeString("myVar")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// 1 class
	builder.writeUint32(1)
	builder.writeUint32(0)          // name: "TestClass"
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)          // numSlots
	builder.writeUint32(0)          // instVarCount
	builder.writeUint32(0)          // instance methods
	builder.writeUint32(0)          // class methods
	builder.writeBytes([]byte{0})   // no docstring

	builder.writeUint32(0) // methods
	builder.writeUint32(0) // objects
	builder.writeUint32(0) // globals

	// Class vars section: 1 entry referencing invalid class index
	builder.writeUint32(1)
	builder.writeUint32(99) // class index 99, only 1 class exists
	builder.writeUint32(1)  // 1 variable
	builder.writeUint32(1)  // name: "myVar"
	builder.writeValue(FromSmallInt(1))

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	err = ir.ReadAll(vm)
	if err == nil {
		t.Error("Expected error for class var with out-of-bounds class index, got nil")
	}
	if !errors.Is(err, ErrInvalidClassIndex) {
		t.Errorf("Expected ErrInvalidClassIndex, got: %v", err)
	}
}

// TestImageReaderClassVarsOutOfBoundsStringIndex tests a v3 image where
// a class variable name references a non-existent string.
func TestImageReaderClassVarsOutOfBoundsStringIndex(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table with only 1 string
	builder.writeUint32(1)
	builder.writeString("TestClass")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// 1 class
	builder.writeUint32(1)
	builder.writeUint32(0)          // name: "TestClass"
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)          // numSlots
	builder.writeUint32(0)          // instVarCount
	builder.writeUint32(0)          // instance methods
	builder.writeUint32(0)          // class methods
	builder.writeBytes([]byte{0})   // no docstring

	builder.writeUint32(0) // methods
	builder.writeUint32(0) // objects
	builder.writeUint32(0) // globals

	// Class vars section: 1 entry with valid class but invalid var name string
	builder.writeUint32(1)
	builder.writeUint32(0)   // class index 0 (valid)
	builder.writeUint32(1)   // 1 variable
	builder.writeUint32(999) // var name string index 999 (invalid!)
	builder.writeValue(FromSmallInt(1))

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	err = ir.ReadAll(vm)
	if err == nil {
		t.Error("Expected error for class var with out-of-bounds string index, got nil")
	}
	if !errors.Is(err, ErrInvalidStringIndex) {
		t.Errorf("Expected ErrInvalidStringIndex, got: %v", err)
	}
}

// TestImageReaderReadAllTruncatedAfterStringTable tests ReadAll on an image
// that is truncated right after the string table (symbol table is missing).
func TestImageReaderReadAllTruncatedAfterStringTable(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// Valid string table
	builder.writeUint32(2)
	builder.writeString("hello")
	builder.writeString("world")
	// Truncated here -- no symbol table, selector table, etc.

	vm := NewVM()
	err := vm.LoadImageFromBytes(builder.bytes())
	if err == nil {
		t.Error("Expected error for image truncated after string table, got nil")
	}
}

// TestImageReaderReadAllTruncatedAfterSymbolTable tests ReadAll on an image
// truncated after the symbol table.
func TestImageReaderReadAllTruncatedAfterSymbolTable(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	builder.writeUint32(1)
	builder.writeString("test")

	// Symbol table
	builder.writeUint32(1)
	builder.writeUint32(0) // symbol -> "test"
	// Truncated here -- no selector table

	vm := NewVM()
	err := vm.LoadImageFromBytes(builder.bytes())
	if err == nil {
		t.Error("Expected error for image truncated after symbol table, got nil")
	}
}

// TestImageReaderClassSuperclassOutOfBounds tests a class that references
// a superclass index beyond the class count.
func TestImageReaderClassSuperclassOutOfBounds(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table
	builder.writeUint32(1)
	builder.writeString("ChildClass")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// 1 class referencing a superclass index that does not exist
	builder.writeUint32(1)
	builder.writeUint32(0)          // name: "ChildClass"
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(50)         // superclass index 50, but only 1 class
	builder.writeUint32(0)          // numSlots
	builder.writeUint32(0)          // instVarCount
	builder.writeUint32(0)          // instance methods
	builder.writeUint32(0)          // class methods
	builder.writeBytes([]byte{0})   // no docstring

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()

	_, err = ir.ReadClasses(vm)
	if err == nil {
		t.Error("Expected error for superclass out of bounds, got nil")
	}
	if !errors.Is(err, ErrInvalidClassIndex) {
		t.Errorf("Expected ErrInvalidClassIndex, got: %v", err)
	}
}

// TestImageReaderClassNameOutOfBoundsStringIndex tests a class whose name
// index exceeds the string table.
func TestImageReaderClassNameOutOfBoundsStringIndex(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table with only 1 string
	builder.writeUint32(1)
	builder.writeString("x")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// 1 class with name index beyond string table
	builder.writeUint32(1)
	builder.writeUint32(99)         // name index 99 -- out of bounds
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)          // numSlots
	builder.writeUint32(0)          // instVarCount
	builder.writeUint32(0)          // instance methods
	builder.writeUint32(0)          // class methods
	builder.writeBytes([]byte{0})   // no docstring

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()

	_, err = ir.ReadClasses(vm)
	if err == nil {
		t.Error("Expected error for class name with out-of-bounds string index, got nil")
	}
	if !errors.Is(err, ErrInvalidStringIndex) {
		t.Errorf("Expected ErrInvalidStringIndex, got: %v", err)
	}
}

// TestImageReaderHugeStringLength tests a string entry with a huge length
// value (0xFFFFFFFF), which should trigger ErrUnexpectedEOF.
func TestImageReaderHugeStringLength(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// Claim 1 string with absurdly large length
	builder.writeUint32(1)
	builder.writeUint32(0xFFFFFFFF) // string length = 4GB
	// No actual string data

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	ir.ReadHeader()

	_, err = ir.ReadStringTable()
	if err == nil {
		t.Error("Expected error for huge string length, got nil")
	}
	if !errors.Is(err, ErrUnexpectedEOF) {
		t.Errorf("Expected ErrUnexpectedEOF, got: %v", err)
	}
}

// TestImageReaderGlobalsTruncatedValue tests a globals section where the
// value data is truncated.
func TestImageReaderGlobalsTruncatedValue(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	builder.writeUint32(1)
	builder.writeString("myGlobal")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors
	builder.writeUint32(0) // classes
	builder.writeUint32(0) // methods
	builder.writeUint32(0) // objects

	// Globals: claim 1 global, provide name but truncate value data
	builder.writeUint32(1)
	builder.writeUint32(0) // name: "myGlobal"
	// Value data truncated -- EncodedValueSize bytes are expected but not provided

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()
	ir.ReadClasses(vm)
	ir.ReadMethods(vm)
	ir.ReadObjectsWithSlots(vm)

	err = ir.ReadGlobals(vm)
	if err == nil {
		t.Error("Expected error for globals with truncated value, got nil")
	}
	if !errors.Is(err, ErrUnexpectedEOF) {
		t.Errorf("Expected ErrUnexpectedEOF, got: %v", err)
	}
}

// TestImageReaderObjectTruncatedSlotData tests an object whose slot data
// is truncated mid-way.
func TestImageReaderObjectTruncatedSlotData(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(1, 0)

	builder.writeUint32(1)
	builder.writeString("TestClass")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// 1 class
	builder.writeUint32(1)
	builder.writeUint32(0)          // name
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(2)          // numSlots
	builder.writeUint32(0)          // instVarCount
	builder.writeUint32(0)          // instance methods
	builder.writeUint32(0)          // class methods
	builder.writeBytes([]byte{0})   // no docstring

	builder.writeUint32(0) // methods

	// 1 object with 3 slots, but only provide data for 1 slot
	builder.writeUint32(1) // 1 object
	builder.writeUint32(0) // class index 0
	builder.writeUint32(3) // 3 slots
	builder.writeValue(FromSmallInt(42))
	// Missing slot 1 and slot 2 data

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()
	ir.ReadClasses(vm)
	ir.ReadMethods(vm)

	_, err = ir.ReadObjectsWithSlots(vm)
	if err == nil {
		t.Error("Expected error for object with truncated slot data, got nil")
	}
	if !errors.Is(err, ErrUnexpectedEOF) {
		t.Errorf("Expected ErrUnexpectedEOF, got: %v", err)
	}
}

// TestImageReaderClassInstVarOutOfBoundsStringIndex tests a class where an
// instance variable name references a non-existent string.
func TestImageReaderClassInstVarOutOfBoundsStringIndex(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table with only 1 string
	builder.writeUint32(1)
	builder.writeString("MyClass")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// 1 class with 1 instvar referencing invalid string
	builder.writeUint32(1)
	builder.writeUint32(0)          // name: "MyClass"
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(1)          // numSlots
	builder.writeUint32(1)          // 1 instance variable
	builder.writeUint32(200)        // instvar name string index 200 -- invalid
	builder.writeUint32(0)          // 0 instance methods
	builder.writeUint32(0)          // 0 class methods
	builder.writeBytes([]byte{0})   // no docstring

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()

	_, err = ir.ReadClasses(vm)
	if err == nil {
		t.Error("Expected error for instvar with out-of-bounds string index, got nil")
	}
	if !errors.Is(err, ErrInvalidStringIndex) {
		t.Errorf("Expected ErrInvalidStringIndex, got: %v", err)
	}
}

// TestImageReaderMethodNameOutOfBoundsStringIndex tests a method whose name
// index references a non-existent string.
func TestImageReaderMethodNameOutOfBoundsStringIndex(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	// String table with only 1 string
	builder.writeUint32(1)
	builder.writeString("TestClass")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// 1 class
	builder.writeUint32(1)
	builder.writeUint32(0)          // name: "TestClass"
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)          // numSlots
	builder.writeUint32(0)          // instVarCount
	builder.writeUint32(0)          // instance methods
	builder.writeUint32(0)          // class methods
	builder.writeBytes([]byte{0})   // no docstring

	// 1 method with invalid name string index
	builder.writeUint32(1) // 1 method
	builder.writeUint32(0) // selector
	builder.writeUint32(0) // class index
	builder.writeUint32(500) // name string index 500 -- out of bounds!

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()
	ir.ReadClasses(vm)

	_, err = ir.ReadMethods(vm)
	if err == nil {
		t.Error("Expected error for method with out-of-bounds name string index, got nil")
	}
	if !errors.Is(err, ErrInvalidStringIndex) {
		t.Errorf("Expected ErrInvalidStringIndex, got: %v", err)
	}
}

// TestImageReaderHugeLiteralCount tests a method that claims a very large
// number of literals. Should error, not panic.
//
// NOTE: Using 0xFFFFFFFF would cause OOM from pre-allocation. We use a
// moderate value (5000) that exceeds available data. The reader does not
// bound-check literal counts against remaining data before allocating.
// That is tracked separately.
func TestImageReaderHugeLiteralCount(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	builder.writeUint32(2)
	builder.writeString("myMethod")
	builder.writeString("TestClass")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// 1 class
	builder.writeUint32(1)
	builder.writeUint32(1)          // name: "TestClass"
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0) // instance methods
	builder.writeUint32(0) // class methods
	builder.writeBytes([]byte{0})

	// 1 method with excessive literal count
	builder.writeUint32(1)
	builder.writeUint32(0)        // selector
	builder.writeUint32(0)        // class
	builder.writeUint32(0)        // name
	builder.writeBytes([]byte{0}) // isClassMethod
	builder.writeUint32(0)        // arity
	builder.writeUint32(0)        // numTemps
	builder.writeUint32(5000)     // literal count = 5000, no data!

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()
	ir.ReadClasses(vm)

	_, err = ir.ReadMethods(vm)
	if err == nil {
		t.Error("Expected error for method with 5000 literals (no data), got nil")
	}
}

// TestImageReaderHugeBytecodeLength tests a method with a huge bytecode
// length. Should error, not panic.
func TestImageReaderHugeBytecodeLength(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	builder.writeUint32(2)
	builder.writeString("myMethod")
	builder.writeString("TestClass")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// 1 class
	builder.writeUint32(1)
	builder.writeUint32(1)          // name
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeBytes([]byte{0})

	// 1 method with huge bytecode length
	builder.writeUint32(1)
	builder.writeUint32(0)        // selector
	builder.writeUint32(0)        // class
	builder.writeUint32(0)        // name: "myMethod"
	builder.writeBytes([]byte{0}) // isClassMethod
	builder.writeUint32(0)        // arity
	builder.writeUint32(0)        // numTemps
	builder.writeUint32(0)        // 0 literals
	builder.writeUint32(0xFFFFFFFF) // bytecode length = 4GB!

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()
	ir.ReadClasses(vm)

	_, err = ir.ReadMethods(vm)
	if err == nil {
		t.Error("Expected error for method with 0xFFFFFFFF bytecode length, got nil")
	}
	if !errors.Is(err, ErrUnexpectedEOF) {
		t.Errorf("Expected ErrUnexpectedEOF, got: %v", err)
	}
}

// TestImageReaderClassDocstringOutOfBounds tests a v2+ class with a docstring
// index beyond the string table.
func TestImageReaderClassDocstringOutOfBounds(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	builder.writeUint32(1)
	builder.writeString("TestClass")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// 1 class with docstring flag set but invalid string index
	builder.writeUint32(1)
	builder.writeUint32(0)          // name: "TestClass"
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)          // numSlots
	builder.writeUint32(0)          // instVarCount
	builder.writeUint32(0)          // instance methods
	builder.writeUint32(0)          // class methods
	builder.writeBytes([]byte{1})   // has docstring = true
	builder.writeUint32(999)        // docstring index 999 -- out of bounds!

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()

	_, err = ir.ReadClasses(vm)
	if err == nil {
		t.Error("Expected error for class docstring with out-of-bounds string index, got nil")
	}
	if !errors.Is(err, ErrInvalidStringIndex) {
		t.Errorf("Expected ErrInvalidStringIndex, got: %v", err)
	}
}

// TestImageReaderMethodDocstringOutOfBounds tests a v2+ method with a
// docstring index beyond the string table.
func TestImageReaderMethodDocstringOutOfBounds(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	builder.writeUint32(2)
	builder.writeString("badDocMethod")
	builder.writeString("TestClass")

	builder.writeUint32(0) // symbols
	builder.writeUint32(0) // selectors

	// 1 class
	builder.writeUint32(1)
	builder.writeUint32(1)          // name: "TestClass"
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeBytes([]byte{0}) // no class docstring

	// 1 method with invalid docstring
	builder.writeUint32(1)
	builder.writeUint32(0)        // selector
	builder.writeUint32(0)        // class
	builder.writeUint32(0)        // name: "badDocMethod"
	builder.writeBytes([]byte{0}) // isClassMethod
	builder.writeUint32(0)        // arity
	builder.writeUint32(0)        // numTemps
	builder.writeUint32(0)        // 0 literals
	builder.writeUint32(1)        // 1 byte bytecode
	builder.writeBytes([]byte{0x00})
	builder.writeUint32(0)        // 0 blocks
	builder.writeBytes([]byte{0}) // no source
	builder.writeBytes([]byte{1}) // has docstring = true
	builder.writeUint32(777)      // docstring index 777 -- out of bounds!

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()
	ir.ReadClasses(vm)

	_, err = ir.ReadMethods(vm)
	if err == nil {
		t.Error("Expected error for method docstring with out-of-bounds string index, got nil")
	}
	if !errors.Is(err, ErrInvalidStringIndex) {
		t.Errorf("Expected ErrInvalidStringIndex, got: %v", err)
	}
}

// TestImageReaderMethodSourceOutOfBounds tests a method with a source string
// index beyond the string table.
func TestImageReaderMethodSourceOutOfBounds(t *testing.T) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)

	builder.writeUint32(2)
	builder.writeString("myMethod")
	builder.writeString("TestClass")

	builder.writeUint32(0)
	builder.writeUint32(0)

	// 1 class
	builder.writeUint32(1)
	builder.writeUint32(1)          // name
	builder.writeUint32(0xFFFFFFFF) // namespace
	builder.writeUint32(0xFFFFFFFF) // superclass
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeBytes([]byte{0})

	// 1 method with invalid source string
	builder.writeUint32(1)
	builder.writeUint32(0)        // selector
	builder.writeUint32(0)        // class
	builder.writeUint32(0)        // name
	builder.writeBytes([]byte{0}) // isClassMethod
	builder.writeUint32(0)        // arity
	builder.writeUint32(0)        // numTemps
	builder.writeUint32(0)        // 0 literals
	builder.writeUint32(1)        // 1 byte bytecode
	builder.writeBytes([]byte{0x00})
	builder.writeUint32(0)        // 0 blocks
	builder.writeBytes([]byte{1}) // has source = true
	builder.writeUint32(888)      // source index 888 -- out of bounds!

	ir, err := NewImageReaderFromBytes(builder.bytes())
	if err != nil {
		t.Fatalf("NewImageReaderFromBytes failed: %v", err)
	}

	vm := NewVM()

	ir.ReadHeader()
	ir.ReadStringTable()
	ir.ReadSymbolTable()
	ir.ReadSelectorTable()
	ir.ReadClasses(vm)

	_, err = ir.ReadMethods(vm)
	if err == nil {
		t.Error("Expected error for method source with out-of-bounds string index, got nil")
	}
	if !errors.Is(err, ErrInvalidStringIndex) {
		t.Errorf("Expected ErrInvalidStringIndex, got: %v", err)
	}
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

func BenchmarkImageReaderHeader(b *testing.B) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)
	data := builder.bytes()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		ir, _ := NewImageReaderFromBytes(data)
		ir.ReadHeader()
	}
}

func BenchmarkImageReaderStringTable(b *testing.B) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)
	builder.writeUint32(100)
	for i := 0; i < 100; i++ {
		builder.writeString("test_string_name")
	}
	data := builder.bytes()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		ir, _ := NewImageReaderFromBytes(data)
		ir.ReadHeader()
		ir.ReadStringTable()
	}
}

func BenchmarkImageReaderFullLoad(b *testing.B) {
	builder := newTestImageBuilder()
	builder.writeHeader(0, 0)
	builder.writeUint32(10)
	for i := 0; i < 10; i++ {
		builder.writeString("string")
	}
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0)
	builder.writeUint32(0) // class vars (v3+)
	data := builder.bytes()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm := NewVM()
		vm.LoadImageFromBytes(data)
	}
}
