package vm

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
)

// Header size constants for reading
const (
	imageMagicSize       = 4
	imageVersionSize     = 4
	imageFlagsSize       = 4
	imageObjectCountSize = 4
	imageOffsetSize      = 8
	imageEntryPointSize  = 4
)

// ---------------------------------------------------------------------------
// Image Error Types
// ---------------------------------------------------------------------------

var (
	ErrInvalidMagic      = errors.New("invalid magic number: expected MAGI")
	ErrVersionMismatch   = errors.New("image version mismatch")
	ErrCorruptHeader     = errors.New("corrupt image header")
	ErrCorruptData       = errors.New("corrupt image data")
	ErrUnexpectedEOF     = errors.New("unexpected end of image data")
	ErrInvalidClassIndex = errors.New("invalid class index")
	ErrInvalidStringIndex = errors.New("invalid string index")
	ErrInvalidSymbolIndex = errors.New("invalid symbol index")
	ErrInvalidMethodIndex = errors.New("invalid method index")
	ErrInvalidObjectIndex = errors.New("invalid object index")
)

// ---------------------------------------------------------------------------
// ImageHeader: Parsed header information
// ---------------------------------------------------------------------------

// ImageHeader contains the parsed header information from an image file.
type ImageHeader struct {
	Magic            string // Should be "MAGI"
	Version          uint32
	Flags            uint32
	ObjectCount      uint32
	StringTableOffset uint64
	ClassTableOffset  uint64
	EntryPoint       uint32 // Index of the entry point method
}

// ---------------------------------------------------------------------------
// ImageReader: Reads and deserializes VM state from a binary image
// ---------------------------------------------------------------------------

// ImageReader handles reading and deserializing a Maggie VM image.
type ImageReader struct {
	// Input source
	data   []byte // Full image data
	offset int    // Current read position

	// Parsed header
	header ImageHeader

	// Decoder for value deserialization
	decoder *ImageDecoder

	// Loaded tables
	strings   []string            // String table
	symbols   []uint32            // Symbol table (string index -> symbol ID)
	selectors []uint32            // Selector table (selector index -> symbol index)
	classes   []*Class            // Class table
	methods   []*CompiledMethod   // Method table
	objects   []*Object           // Object table

	// Mappings for fixup
	classNameToIndex map[string]uint32 // Class name -> index in classes
}

// NewImageReader creates a new ImageReader from an io.Reader.
func NewImageReader(r io.Reader) (*ImageReader, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, fmt.Errorf("failed to read image data: %w", err)
	}

	return NewImageReaderFromBytes(data)
}

// NewImageReaderFromBytes creates a new ImageReader from a byte slice.
func NewImageReaderFromBytes(data []byte) (*ImageReader, error) {
	if len(data) < ImageHeaderSize {
		return nil, ErrCorruptHeader
	}

	ir := &ImageReader{
		data:             data,
		offset:           0,
		decoder:          NewImageDecoder(),
		strings:          make([]string, 0),
		symbols:          make([]uint32, 0),
		selectors:        make([]uint32, 0),
		classes:          make([]*Class, 0),
		methods:          make([]*CompiledMethod, 0),
		objects:          make([]*Object, 0),
		classNameToIndex: make(map[string]uint32),
	}

	return ir, nil
}

// ---------------------------------------------------------------------------
// Header Reading
// ---------------------------------------------------------------------------

// ReadHeader reads and validates the image header.
func (ir *ImageReader) ReadHeader() (*ImageHeader, error) {
	if len(ir.data) < ImageHeaderSize {
		return nil, ErrCorruptHeader
	}

	ir.offset = 0

	// Read magic number
	magic := string(ir.data[ir.offset : ir.offset+imageMagicSize])
	ir.offset += imageMagicSize

	if magic != string(ImageMagic[:]) {
		return nil, fmt.Errorf("%w: got %q", ErrInvalidMagic, magic)
	}

	// Read version
	version := ReadUint32(ir.data[ir.offset:])
	ir.offset += imageVersionSize

	if version != ImageVersion {
		return nil, fmt.Errorf("%w: expected %d, got %d", ErrVersionMismatch, ImageVersion, version)
	}

	// Read flags
	flags := ReadUint32(ir.data[ir.offset:])
	ir.offset += imageFlagsSize

	// Read object count
	objectCount := ReadUint32(ir.data[ir.offset:])
	ir.offset += imageObjectCountSize

	// Read string table offset
	stringTableOffset := ReadUint64(ir.data[ir.offset:])
	ir.offset += imageOffsetSize

	// Read class table offset
	classTableOffset := ReadUint64(ir.data[ir.offset:])
	ir.offset += imageOffsetSize

	// Read entry point
	entryPoint := ReadUint32(ir.data[ir.offset:])
	ir.offset += imageEntryPointSize

	ir.header = ImageHeader{
		Magic:            magic,
		Version:          version,
		Flags:            flags,
		ObjectCount:      objectCount,
		StringTableOffset: stringTableOffset,
		ClassTableOffset:  classTableOffset,
		EntryPoint:       entryPoint,
	}

	return &ir.header, nil
}

// Header returns the parsed header (must call ReadHeader first).
func (ir *ImageReader) Header() *ImageHeader {
	return &ir.header
}

// ---------------------------------------------------------------------------
// Table Reading
// ---------------------------------------------------------------------------

// readUint32 reads a uint32 from the current position.
func (ir *ImageReader) readUint32() (uint32, error) {
	if ir.offset+4 > len(ir.data) {
		return 0, ErrUnexpectedEOF
	}
	v := ReadUint32(ir.data[ir.offset:])
	ir.offset += 4
	return v, nil
}

// readUint64 reads a uint64 from the current position.
func (ir *ImageReader) readUint64() (uint64, error) {
	if ir.offset+8 > len(ir.data) {
		return 0, ErrUnexpectedEOF
	}
	v := ReadUint64(ir.data[ir.offset:])
	ir.offset += 8
	return v, nil
}

// readBytes reads n bytes from the current position.
func (ir *ImageReader) readBytes(n int) ([]byte, error) {
	if ir.offset+n > len(ir.data) {
		return nil, ErrUnexpectedEOF
	}
	data := ir.data[ir.offset : ir.offset+n]
	ir.offset += n
	return data, nil
}

// readString reads a length-prefixed string from the current position.
// The format is [length:32 | utf8 bytes] (matching image_writer.go format).
func (ir *ImageReader) readString() (string, error) {
	if ir.offset+4 > len(ir.data) {
		return "", ErrUnexpectedEOF
	}
	// Read length as uint32
	length := ReadUint32(ir.data[ir.offset:])
	ir.offset += 4

	// Read string bytes
	if ir.offset+int(length) > len(ir.data) {
		return "", ErrUnexpectedEOF
	}
	s := string(ir.data[ir.offset : ir.offset+int(length)])
	ir.offset += int(length)
	return s, nil
}

// ReadStringTable reads the string table from the image.
func (ir *ImageReader) ReadStringTable() ([]string, error) {
	// Read count
	count, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read string table count: %w", err)
	}

	ir.strings = make([]string, count)
	for i := uint32(0); i < count; i++ {
		s, err := ir.readString()
		if err != nil {
			return nil, fmt.Errorf("failed to read string %d: %w", i, err)
		}
		ir.strings[i] = s
		ir.decoder.AddString(s)
	}

	return ir.strings, nil
}

// ReadSymbolTable reads the symbol table from the image.
// Each entry is a string index that maps to a symbol.
func (ir *ImageReader) ReadSymbolTable() (map[uint32]string, error) {
	// Read count
	count, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read symbol table count: %w", err)
	}

	result := make(map[uint32]string, count)
	ir.symbols = make([]uint32, count)

	for i := uint32(0); i < count; i++ {
		stringIdx, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read symbol %d: %w", i, err)
		}

		if int(stringIdx) >= len(ir.strings) {
			return nil, fmt.Errorf("%w: symbol %d references string %d", ErrInvalidStringIndex, i, stringIdx)
		}

		ir.symbols[i] = stringIdx
		result[i] = ir.strings[stringIdx]
	}

	return result, nil
}

// ReadSelectorTable reads the selector table from the image.
// Each entry is a string index that maps to a selector name.
func (ir *ImageReader) ReadSelectorTable() (map[uint32]uint32, error) {
	// Read count
	count, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read selector table count: %w", err)
	}

	result := make(map[uint32]uint32, count)
	ir.selectors = make([]uint32, count)

	for i := uint32(0); i < count; i++ {
		stringIdx, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read selector %d: %w", i, err)
		}

		// Selectors store string indices directly (like symbols)
		if int(stringIdx) >= len(ir.strings) {
			return nil, fmt.Errorf("%w: selector %d references string %d", ErrInvalidStringIndex, i, stringIdx)
		}

		ir.selectors[i] = stringIdx
		result[i] = stringIdx
	}

	return result, nil
}

// GetString returns the string at the given index.
func (ir *ImageReader) GetString(idx uint32) (string, error) {
	if int(idx) >= len(ir.strings) {
		return "", fmt.Errorf("%w: %d", ErrInvalidStringIndex, idx)
	}
	return ir.strings[idx], nil
}

// GetSymbolName returns the symbol name at the given index.
func (ir *ImageReader) GetSymbolName(idx uint32) (string, error) {
	if int(idx) >= len(ir.symbols) {
		return "", fmt.Errorf("%w: %d", ErrInvalidSymbolIndex, idx)
	}
	stringIdx := ir.symbols[idx]
	return ir.GetString(stringIdx)
}

// ---------------------------------------------------------------------------
// Class Reading
// ---------------------------------------------------------------------------

// ReadClasses reads class definitions from the image and creates Class objects.
// The format matches image_writer.go: name, namespace, superclass, numSlots, instVarCount, instVarNames, methodCount, methodIndices
func (ir *ImageReader) ReadClasses(vm *VM) ([]*Class, error) {
	// Read count
	count, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read class count: %w", err)
	}

	ir.classes = make([]*Class, count)

	// Track superclass indices for second pass linking
	superclassIndices := make([]uint32, count)

	// First pass: create all classes without superclass links
	for i := uint32(0); i < count; i++ {
		// Read class name (string index)
		nameIdx, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read class %d name index: %w", i, err)
		}

		name, err := ir.GetString(nameIdx)
		if err != nil {
			return nil, fmt.Errorf("failed to get class %d name: %w", i, err)
		}

		// Read namespace (string index, 0xFFFFFFFF means empty)
		namespaceIdx, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read class %d namespace index: %w", i, err)
		}

		namespace := ""
		if namespaceIdx != 0xFFFFFFFF {
			namespace, err = ir.GetString(namespaceIdx)
			if err != nil {
				return nil, fmt.Errorf("failed to get class %d namespace: %w", i, err)
			}
		}

		// Read superclass index (0xFFFFFFFF means nil/Object)
		superclassIdx, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read class %d superclass index: %w", i, err)
		}
		superclassIndices[i] = superclassIdx

		// Read numSlots
		numSlots, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read class %d numSlots: %w", i, err)
		}

		// Read instance variable count
		instVarCount, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read class %d instvar count: %w", i, err)
		}

		// Read instance variable names
		instVars := make([]string, instVarCount)
		for j := uint32(0); j < instVarCount; j++ {
			varNameIdx, err := ir.readUint32()
			if err != nil {
				return nil, fmt.Errorf("failed to read class %d instvar %d: %w", i, j, err)
			}
			instVars[j], err = ir.GetString(varNameIdx)
			if err != nil {
				return nil, fmt.Errorf("failed to get class %d instvar %d name: %w", i, j, err)
			}
		}

		// Read instance method count (skip for now, methods are loaded separately)
		methodCount, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read class %d method count: %w", i, err)
		}

		// Read instance method indices (skip them, they'll be linked when methods are loaded)
		for j := uint32(0); j < methodCount; j++ {
			_, err := ir.readUint32()
			if err != nil {
				return nil, fmt.Errorf("failed to read class %d method index %d: %w", i, j, err)
			}
		}

		// Read class method count (skip for now, methods are loaded separately)
		classMethodCount, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read class %d class method count: %w", i, err)
		}

		// Read class method indices (skip them, they'll be linked when methods are loaded)
		for j := uint32(0); j < classMethodCount; j++ {
			_, err := ir.readUint32()
			if err != nil {
				return nil, fmt.Errorf("failed to read class %d class method index %d: %w", i, j, err)
			}
		}

		// Check if class already exists in VM (core classes)
		existingClass := vm.Classes.Lookup(name)
		if existingClass != nil {
			// Use existing class, but update instance variables if needed
			ir.classes[i] = existingClass
			ir.classNameToIndex[name] = i
			ir.decoder.AddClass(existingClass)
			continue
		}

		// Create new class (superclass will be linked in second pass)
		var c *Class
		if len(instVars) > 0 {
			c = NewClassWithInstVars(name, nil, instVars)
		} else {
			c = NewClass(name, nil)
		}
		c.Namespace = namespace
		c.NumSlots = int(numSlots)

		ir.classes[i] = c
		ir.classNameToIndex[name] = i
		ir.decoder.AddClass(c)
	}

	// Second pass: link superclasses
	for i := uint32(0); i < count; i++ {
		c := ir.classes[i]
		if c == nil {
			continue
		}

		// Check if this is an existing VM class (already has superclass set up)
		if vm.Classes.Lookup(c.Name) != nil && c.VTable != nil {
			continue
		}

		superclassIdx := superclassIndices[i]

		if superclassIdx == 0xFFFFFFFF {
			// No superclass (root class) - use Object if not already Object
			if c != vm.ObjectClass {
				c.Superclass = vm.ObjectClass
			}
		} else if int(superclassIdx) < len(ir.classes) {
			c.Superclass = ir.classes[superclassIdx]
		} else {
			return nil, fmt.Errorf("%w: class %d references superclass %d", ErrInvalidClassIndex, i, superclassIdx)
		}

		// Set up VTable parent based on superclass
		var parentVT *VTable
		var parentClassVT *VTable
		if c.Superclass != nil {
			parentVT = c.Superclass.VTable
			parentClassVT = c.Superclass.ClassVTable
		}
		if c.VTable == nil {
			c.VTable = NewVTable(c, parentVT)
		} else {
			// VTable exists but may have nil parent from first pass - update it
			c.VTable.SetParent(parentVT)
		}
		// Set up ClassVTable parent for class-side method inheritance
		if c.ClassVTable == nil {
			c.ClassVTable = NewVTable(c, parentClassVT)
		} else {
			c.ClassVTable.SetParent(parentClassVT)
		}

		// Register class in VM
		vm.Classes.Register(c)
	}

	return ir.classes, nil
}

// GetClass returns the class at the given index.
func (ir *ImageReader) GetClass(idx uint32) (*Class, error) {
	if int(idx) >= len(ir.classes) {
		return nil, fmt.Errorf("%w: %d", ErrInvalidClassIndex, idx)
	}
	return ir.classes[idx], nil
}

// ---------------------------------------------------------------------------
// Method Reading
// ---------------------------------------------------------------------------

// ReadMethods reads compiled methods from the image.
func (ir *ImageReader) ReadMethods(vm *VM) ([]*CompiledMethod, error) {
	// Read count
	count, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read method count: %w", err)
	}

	ir.methods = make([]*CompiledMethod, count)

	for i := uint32(0); i < count; i++ {
		method, err := ir.readMethod(vm)
		if err != nil {
			return nil, fmt.Errorf("failed to read method %d: %w", i, err)
		}
		ir.methods[i] = method
		ir.decoder.AddMethod(method)
	}

	return ir.methods, nil
}

// readMethod reads a single compiled method.
// The format matches image_writer.go: selector, class, name, arity, numTemps, literals, bytecode, blocks, source, sourceMap
func (ir *ImageReader) readMethod(vm *VM) (*CompiledMethod, error) {
	// Read selector ID (int32)
	selectorRaw, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read selector: %w", err)
	}
	selector := int32(selectorRaw)

	// Read class index (0xFFFFFFFF means detached method)
	classIdx, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read class index: %w", err)
	}

	// Read method name (string index)
	nameIdx, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read method name index: %w", err)
	}

	name, err := ir.GetString(nameIdx)
	if err != nil {
		return nil, fmt.Errorf("failed to get method name: %w", err)
	}

	// Read IsClassMethod flag (1 byte)
	if ir.offset >= len(ir.data) {
		return nil, ErrUnexpectedEOF
	}
	isClassMethod := ir.data[ir.offset] != 0
	ir.offset++

	// Read arity
	arity, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read arity: %w", err)
	}

	// Read numTemps
	numTemps, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read numTemps: %w", err)
	}

	// Read literal count
	literalCount, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read literal count: %w", err)
	}

	// Read literals
	literals := make([]Value, literalCount)
	for j := uint32(0); j < literalCount; j++ {
		litData, err := ir.readBytes(EncodedValueSize)
		if err != nil {
			return nil, fmt.Errorf("failed to read literal %d: %w", j, err)
		}
		literals[j] = ir.decoder.DecodeValue(litData)
	}

	// Read bytecode length
	bytecodeLen, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read bytecode length: %w", err)
	}

	// Read bytecode
	bytecode, err := ir.readBytes(int(bytecodeLen))
	if err != nil {
		return nil, fmt.Errorf("failed to read bytecode: %w", err)
	}

	// Read block count
	blockCount, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read block count: %w", err)
	}

	// Read blocks
	blocks := make([]*BlockMethod, blockCount)
	for j := uint32(0); j < blockCount; j++ {
		block, err := ir.readBlock()
		if err != nil {
			return nil, fmt.Errorf("failed to read block %d: %w", j, err)
		}
		blocks[j] = block
	}

	// Read source flag (1 byte)
	if ir.offset >= len(ir.data) {
		return nil, ErrUnexpectedEOF
	}
	hasSource := ir.data[ir.offset]
	ir.offset++

	// Read source (optional, indicated by flag)
	source := ""
	if hasSource != 0 {
		sourceIdx, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read source index: %w", err)
		}
		source, err = ir.GetString(sourceIdx)
		if err != nil {
			return nil, fmt.Errorf("failed to get source: %w", err)
		}
	}

	// Read source map entry count
	sourceMapCount, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read source map count: %w", err)
	}

	// Read source map entries
	sourceMap := make([]SourceLoc, sourceMapCount)
	for j := uint32(0); j < sourceMapCount; j++ {
		offset, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read source map offset: %w", err)
		}
		line, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read source map line: %w", err)
		}
		column, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read source map column: %w", err)
		}
		sourceMap[j] = SourceLoc{
			Offset: int(offset),
			Line:   int(line),
			Column: int(column),
		}
	}

	// Map selector from image space to VM's selector space using the method name
	vmSelectorID := vm.Selectors.Intern(name)

	// Create method with VM's selector ID
	method := &CompiledMethod{
		selector:      vmSelectorID,
		name:          name,
		IsClassMethod: isClassMethod,
		Arity:         int(arity),
		NumTemps:      int(numTemps),
		Literals:      literals,
		Bytecode:      bytecode,
		Blocks:        blocks,
		Source:        source,
		SourceMap:     sourceMap,
	}

	// Ignore the raw selector from image - we use vmSelectorID instead
	_ = selector

	// Link to class and appropriate VTable
	if classIdx != 0xFFFFFFFF {
		if int(classIdx) < len(ir.classes) {
			class := ir.classes[classIdx]
			method.class = class
			// Register method in appropriate vtable using VM's selector ID
			if isClassMethod {
				// Class method goes on ClassVTable
				if class.ClassVTable != nil {
					class.ClassVTable.AddMethod(vmSelectorID, method)
				}
			} else {
				// Instance method goes on VTable
				if class.VTable != nil {
					class.VTable.AddMethod(vmSelectorID, method)
				}
			}
		}
	}

	// Link blocks to outer method
	for _, block := range blocks {
		block.Outer = method
	}

	return method, nil
}

// readBlock reads a single block method.
func (ir *ImageReader) readBlock() (*BlockMethod, error) {
	// Read arity
	arity, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read block arity: %w", err)
	}

	// Read numTemps
	numTemps, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read block numTemps: %w", err)
	}

	// Read numCaptures
	numCaptures, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read block numCaptures: %w", err)
	}

	// Read literal count
	literalCount, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read block literal count: %w", err)
	}

	// Read literals
	literals := make([]Value, literalCount)
	for j := uint32(0); j < literalCount; j++ {
		litData, err := ir.readBytes(EncodedValueSize)
		if err != nil {
			return nil, fmt.Errorf("failed to read block literal %d: %w", j, err)
		}
		literals[j] = ir.decoder.DecodeValue(litData)
	}

	// Read bytecode length
	bytecodeLen, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read block bytecode length: %w", err)
	}

	// Read bytecode
	bytecode, err := ir.readBytes(int(bytecodeLen))
	if err != nil {
		return nil, fmt.Errorf("failed to read block bytecode: %w", err)
	}

	// Read source map entry count
	sourceMapCount, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read block source map count: %w", err)
	}

	// Read source map entries
	sourceMap := make([]SourceLoc, sourceMapCount)
	for j := uint32(0); j < sourceMapCount; j++ {
		offset, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read block source map offset: %w", err)
		}
		line, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read block source map line: %w", err)
		}
		column, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read block source map column: %w", err)
		}
		sourceMap[j] = SourceLoc{
			Offset: int(offset),
			Line:   int(line),
			Column: int(column),
		}
	}

	return &BlockMethod{
		Arity:       int(arity),
		NumTemps:    int(numTemps),
		NumCaptures: int(numCaptures),
		Literals:    literals,
		Bytecode:    bytecode,
		SourceMap:   sourceMap,
	}, nil
}

// GetMethod returns the method at the given index.
func (ir *ImageReader) GetMethod(idx uint32) (*CompiledMethod, error) {
	if int(idx) >= len(ir.methods) {
		return nil, fmt.Errorf("%w: %d", ErrInvalidMethodIndex, idx)
	}
	return ir.methods[idx], nil
}

// ---------------------------------------------------------------------------
// Object Reading
// ---------------------------------------------------------------------------

// ReadObjects reads objects from the image.
func (ir *ImageReader) ReadObjects(vm *VM) ([]*Object, error) {
	// Read count
	count, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read object count: %w", err)
	}

	ir.objects = make([]*Object, count)

	// First pass: create all objects with nil slots
	for i := uint32(0); i < count; i++ {
		// Read class index
		classIdx, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read object %d class index: %w", i, err)
		}

		if int(classIdx) >= len(ir.classes) {
			return nil, fmt.Errorf("%w: object %d references class %d", ErrInvalidClassIndex, i, classIdx)
		}

		class := ir.classes[classIdx]

		// Read slot count
		slotCount, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read object %d slot count: %w", i, err)
		}

		// Create object
		obj := NewObject(class.VTable, int(slotCount))
		ir.objects[i] = obj
		ir.decoder.AddObject(obj)
	}

	// Second pass: read slot values (now that all objects exist for cross-references)
	ir.offset = ir.offset // Continue from current position

	// We need to re-read to get slot data - reset and skip to slot data
	// Actually, we should read slots inline with object creation
	// Let me restructure this...

	return ir.objects, nil
}

// ReadObjectsWithSlots reads objects and their slot values.
func (ir *ImageReader) ReadObjectsWithSlots(vm *VM) ([]*Object, error) {
	// Read count
	count, err := ir.readUint32()
	if err != nil {
		return nil, fmt.Errorf("failed to read object count: %w", err)
	}

	ir.objects = make([]*Object, count)

	// First pass: create all objects (no slots yet)
	objectData := make([]struct {
		classIdx  uint32
		slotCount uint32
		slotData  [][]byte
	}, count)

	startOffset := ir.offset

	for i := uint32(0); i < count; i++ {
		// Read class index
		classIdx, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read object %d class index: %w", i, err)
		}

		// Read slot count
		slotCount, err := ir.readUint32()
		if err != nil {
			return nil, fmt.Errorf("failed to read object %d slot count: %w", i, err)
		}

		// Read slot data
		slotData := make([][]byte, slotCount)
		for j := uint32(0); j < slotCount; j++ {
			data, err := ir.readBytes(EncodedValueSize)
			if err != nil {
				return nil, fmt.Errorf("failed to read object %d slot %d: %w", i, j, err)
			}
			slotData[j] = data
		}

		objectData[i] = struct {
			classIdx  uint32
			slotCount uint32
			slotData  [][]byte
		}{classIdx, slotCount, slotData}
	}

	// Now create objects (so decoder has all objects registered)
	for i := uint32(0); i < count; i++ {
		data := objectData[i]

		if int(data.classIdx) >= len(ir.classes) {
			return nil, fmt.Errorf("%w: object %d references class %d", ErrInvalidClassIndex, i, data.classIdx)
		}

		class := ir.classes[data.classIdx]
		obj := NewObject(class.VTable, int(data.slotCount))
		ir.objects[i] = obj
		ir.decoder.AddObject(obj)
	}

	// Decode slots now that all objects exist
	for i := uint32(0); i < count; i++ {
		obj := ir.objects[i]
		data := objectData[i]

		for j := uint32(0); j < data.slotCount; j++ {
			value := ir.decoder.DecodeValue(data.slotData[j])
			obj.SetSlot(int(j), value)
		}
	}

	// Restore offset position
	_ = startOffset

	return ir.objects, nil
}

// GetObject returns the object at the given index.
func (ir *ImageReader) GetObject(idx uint32) (*Object, error) {
	if int(idx) >= len(ir.objects) {
		return nil, fmt.Errorf("%w: %d", ErrInvalidObjectIndex, idx)
	}
	return ir.objects[idx], nil
}

// ---------------------------------------------------------------------------
// Globals Reading
// ---------------------------------------------------------------------------

// ReadGlobals reads global variables from the image.
func (ir *ImageReader) ReadGlobals(vm *VM) error {
	// Read count
	count, err := ir.readUint32()
	if err != nil {
		return fmt.Errorf("failed to read globals count: %w", err)
	}

	for i := uint32(0); i < count; i++ {
		// Read name (string index)
		nameIdx, err := ir.readUint32()
		if err != nil {
			return fmt.Errorf("failed to read global %d name index: %w", i, err)
		}

		name, err := ir.GetString(nameIdx)
		if err != nil {
			return fmt.Errorf("failed to get global %d name: %w", i, err)
		}

		// Read value
		valueData, err := ir.readBytes(EncodedValueSize)
		if err != nil {
			return fmt.Errorf("failed to read global %d value: %w", i, err)
		}

		value := ir.decoder.DecodeValue(valueData)
		vm.Globals[name] = value
	}

	return nil
}

// ---------------------------------------------------------------------------
// Full Image Loading
// ---------------------------------------------------------------------------

// ReadAll reads the entire image and populates the VM.
func (ir *ImageReader) ReadAll(vm *VM) error {
	// Read header
	_, err := ir.ReadHeader()
	if err != nil {
		return fmt.Errorf("failed to read header: %w", err)
	}

	// Read string table
	_, err = ir.ReadStringTable()
	if err != nil {
		return fmt.Errorf("failed to read string table: %w", err)
	}

	// Read symbol table
	_, err = ir.ReadSymbolTable()
	if err != nil {
		return fmt.Errorf("failed to read symbol table: %w", err)
	}

	// Intern symbols in VM
	for idx, stringIdx := range ir.symbols {
		if int(stringIdx) < len(ir.strings) {
			name := ir.strings[stringIdx]
			symID := vm.Symbols.Intern(name)
			ir.decoder.SetSymbol(uint32(idx), symID)
		}
	}

	// Read selector table
	_, err = ir.ReadSelectorTable()
	if err != nil {
		return fmt.Errorf("failed to read selector table: %w", err)
	}

	// Intern selectors in VM (selectors store string indices directly)
	for _, stringIdx := range ir.selectors {
		if int(stringIdx) < len(ir.strings) {
			name := ir.strings[stringIdx]
			vm.Selectors.Intern(name)
		}
	}

	// Read classes
	_, err = ir.ReadClasses(vm)
	if err != nil {
		return fmt.Errorf("failed to read classes: %w", err)
	}

	// Read methods
	_, err = ir.ReadMethods(vm)
	if err != nil {
		return fmt.Errorf("failed to read methods: %w", err)
	}

	// Read objects
	_, err = ir.ReadObjectsWithSlots(vm)
	if err != nil {
		return fmt.Errorf("failed to read objects: %w", err)
	}

	// Read globals
	err = ir.ReadGlobals(vm)
	if err != nil {
		return fmt.Errorf("failed to read globals: %w", err)
	}

	return nil
}

// Decoder returns the underlying ImageDecoder.
func (ir *ImageReader) Decoder() *ImageDecoder {
	return ir.decoder
}

// ---------------------------------------------------------------------------
// VM Load Methods
// ---------------------------------------------------------------------------

// LoadImage loads a VM state from an image file.
func (vm *VM) LoadImage(path string) error {
	f, err := os.Open(path)
	if err != nil {
		return fmt.Errorf("failed to open image file: %w", err)
	}
	defer f.Close()

	return vm.LoadImageFrom(f)
}

// LoadImageFrom loads a VM state from an io.Reader.
func (vm *VM) LoadImageFrom(r io.Reader) error {
	ir, err := NewImageReader(r)
	if err != nil {
		return err
	}

	return ir.ReadAll(vm)
}

// LoadImageFromBytes loads a VM state from a byte slice.
func (vm *VM) LoadImageFromBytes(data []byte) error {
	return vm.LoadImageFrom(bytes.NewReader(data))
}
