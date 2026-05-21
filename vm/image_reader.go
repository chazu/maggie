package vm

import (
	"fmt"
	"io"
	"math/big"
	"os"

	"github.com/fxamacker/cbor/v2"
)

// ---------------------------------------------------------------------------
// Bytecode Selector Remapping
// ---------------------------------------------------------------------------

// remapBytecodeSelectors scans bytecode and remaps selector IDs from image space
// to VM space. This is necessary because selector IDs in the image may differ
// from VM selector IDs due to primitives being registered before the image is
// loaded. The selectorIDMap maps image selector IDs to VM selector IDs.
func remapBytecodeSelectors(bytecode []byte, selectorIDMap map[int]int) {
	i := 0
	for i < len(bytecode) {
		op := Opcode(bytecode[i])
		switch op {
		case OpSend, OpSendSuper, OpTailSend:
			// Format: opcode (1 byte) + selector (2 bytes little-endian) + argc (1 byte)
			if i+3 < len(bytecode) {
				// Read original selector ID (little-endian 16-bit)
				imageSelectorID := int(bytecode[i+1]) | (int(bytecode[i+2]) << 8)

				// Look up the VM selector ID
				if vmSelectorID, ok := selectorIDMap[imageSelectorID]; ok {
					// Only remap if different
					if vmSelectorID != imageSelectorID {
						// Write remapped selector ID (little-endian)
						bytecode[i+1] = byte(vmSelectorID)
						bytecode[i+2] = byte(vmSelectorID >> 8)
					}
				}
			}
			i += 4 // opcode + 2-byte selector + 1-byte argc
		default:
			// Skip instruction based on operand size
			info := op.Info()
			i += 1 + info.OperandBytes
		}
	}
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
// It reads all bytes into memory and delegates to LoadImageFromBytes.
func (vm *VM) LoadImageFrom(r io.Reader) error {
	data, err := io.ReadAll(r)
	if err != nil {
		return fmt.Errorf("failed to read image data: %w", err)
	}
	return vm.LoadImageFromBytes(data)
}

// LoadImageFromBytes loads a VM state from a byte slice.
// Only CBOR format is supported.
func (vm *VM) LoadImageFromBytes(data []byte) error {
	cr, err := NewImageReader(data)
	if err != nil {
		return fmt.Errorf("failed to parse CBOR image: %w", err)
	}
	return cr.ReadAll(vm)
}

// ---------------------------------------------------------------------------
// decodeImageValue: decode a single CBOR-encoded image value back to a Value
// ---------------------------------------------------------------------------

// decodeImageValue decodes a CBOR-encoded image value back to a Value.
// It handles nil, bool, integers, floats, strings, and tagged values
// (symbols, object refs, class refs, characters, big integers).
func decodeImageValue(vm *VM, decoder *ImageDecoder, raw cbor.RawMessage) (Value, error) {
	// Decode to generic interface first — the cbor library will produce
	// cbor.Tag for tagged values and native Go types for everything else.
	var generic interface{}
	if err := cbor.Unmarshal(raw, &generic); err != nil {
		return Nil, fmt.Errorf("decodeImageValue: CBOR unmarshal failed: %w", err)
	}

	// Check if it decoded as a CBOR tag
	if tag, ok := generic.(cbor.Tag); ok {
		return decodeImageTag(vm, decoder, tag)
	}

	switch v := generic.(type) {
	case nil:
		return Nil, nil
	case bool:
		if v {
			return True, nil
		}
		return False, nil
	case uint64:
		return FromSmallInt(int64(v)), nil
	case int64:
		return FromSmallInt(v), nil
	case float32:
		return FromFloat64(float64(v)), nil
	case float64:
		return FromFloat64(v), nil
	case string:
		return vm.registry.NewStringValue(v), nil
	default:
		return Nil, fmt.Errorf("decodeImageValue: unsupported CBOR type %T", generic)
	}
}

// decodeImageTag handles CBOR-tagged image values.
func decodeImageTag(vm *VM, decoder *ImageDecoder, tag cbor.Tag) (Value, error) {
	switch tag.Number {
	case imgTagSymbolRef:
		// Symbol reference by index into the decoder's symbol table
		idx, err := tagContentUint32(tag.Content)
		if err != nil {
			return Nil, fmt.Errorf("decodeImageValue: SymbolRef: %w", err)
		}
		symID := decoder.GetSymbol(idx)
		return FromSymbolID(symID), nil

	case imgTagValueRef:
		// Object reference by index into the decoder's object table
		idx, err := tagContentUint32(tag.Content)
		if err != nil {
			return Nil, fmt.Errorf("decodeImageValue: ValueRef: %w", err)
		}
		obj := decoder.GetObject(idx)
		if obj == nil {
			return Nil, nil
		}
		return obj.ToValue(), nil

	case imgTagClassRef:
		// Class reference by index into the decoder's class table
		idx, err := tagContentUint32(tag.Content)
		if err != nil {
			return Nil, fmt.Errorf("decodeImageValue: ClassRef: %w", err)
		}
		cls := decoder.GetClass(idx)
		if cls == nil {
			return Nil, nil
		}
		return vm.registry.RegisterClassValue(cls), nil

	case cborTagCharacter:
		cp, err := tagContentUint32(tag.Content)
		if err != nil {
			return Nil, fmt.Errorf("decodeImageValue: Character: %w", err)
		}
		return FromCharacter(rune(cp)), nil

	case cborTagBigIntPositive:
		b, ok := tag.Content.([]byte)
		if !ok {
			return Nil, fmt.Errorf("decodeImageValue: BigIntPositive content not bytes, got %T", tag.Content)
		}
		n := new(big.Int).SetBytes(b)
		return vm.registry.NewBigIntValue(n), nil

	case cborTagBigIntNegative:
		b, ok := tag.Content.([]byte)
		if !ok {
			return Nil, fmt.Errorf("decodeImageValue: BigIntNegative content not bytes, got %T", tag.Content)
		}
		n := new(big.Int).SetBytes(b)
		n.Neg(n)
		return vm.registry.NewBigIntValue(n), nil

	default:
		return Nil, fmt.Errorf("decodeImageValue: unknown CBOR tag %d", tag.Number)
	}
}

// tagContentUint32 extracts a uint32 from a CBOR tag content value,
// handling both uint64 and int64 representations.
func tagContentUint32(content interface{}) (uint32, error) {
	switch v := content.(type) {
	case uint64:
		return uint32(v), nil
	case int64:
		return uint32(v), nil
	case float64:
		return uint32(v), nil
	case float32:
		return uint32(v), nil
	default:
		return 0, fmt.Errorf("expected integer, got %T", content)
	}
}

// ---------------------------------------------------------------------------
// ImageReader: Reads and deserializes VM state from a CBOR image
// ---------------------------------------------------------------------------

// ImageReader deserializes a CBOR-encoded image into a VM.
type ImageReader struct {
	envelope      imageEnvelope
	classes       []*Class
	methods       []*CompiledMethod
	objects       []*Object
	selectorIDMap map[int]int // image selector ordinal -> VM selector ID
	decoder       *ImageDecoder
}

// NewImageReader creates a new ImageReader from CBOR bytes.
// It unmarshals the top-level tagged envelope.
func NewImageReader(data []byte) (*ImageReader, error) {
	var tag cbor.Tag
	if err := cbor.Unmarshal(data, &tag); err != nil {
		return nil, fmt.Errorf("ImageReader: failed to unmarshal CBOR tag: %w", err)
	}
	if tag.Number != imgTagHeader {
		return nil, fmt.Errorf("ImageReader: expected tag %d, got %d", imgTagHeader, tag.Number)
	}

	// Re-marshal the content and unmarshal into the envelope struct.
	// The cbor library decodes tag content as a generic map, so we need
	// to round-trip through CBOR bytes to get proper struct decoding.
	contentBytes, err := cborSerialEncMode.Marshal(tag.Content)
	if err != nil {
		return nil, fmt.Errorf("ImageReader: failed to re-encode envelope: %w", err)
	}

	var envelope imageEnvelope
	if err := cbor.Unmarshal(contentBytes, &envelope); err != nil {
		return nil, fmt.Errorf("ImageReader: failed to decode envelope: %w", err)
	}

	return &ImageReader{
		envelope:      envelope,
		selectorIDMap: make(map[int]int),
		decoder:       NewImageDecoder(),
	}, nil
}

// ReadAll reads the entire CBOR image and populates the VM.
func (cr *ImageReader) ReadAll(vm *VM) error {
	// Set registry on decoder for string/class value creation
	cr.decoder.registry = vm.registry

	// 1. Populate decoder string table from envelope
	for _, s := range cr.envelope.Strings {
		cr.decoder.AddString(s)
	}

	// 2. Intern symbols and track mapping
	for imgIdx, name := range cr.envelope.Symbols {
		vmSymID := vm.Symbols.Intern(name)
		cr.decoder.SetSymbol(uint32(imgIdx), vmSymID)
	}

	// 3. Intern selectors and build remap table
	for imgOrdinal, name := range cr.envelope.Selectors {
		vmSelectorID := vm.Selectors.Intern(name)
		cr.selectorIDMap[imgOrdinal] = vmSelectorID
	}

	// 4. Read classes (two-pass)
	if err := cr.readClasses(vm); err != nil {
		return fmt.Errorf("failed to read classes: %w", err)
	}

	// 5. Read methods
	if err := cr.readMethods(vm); err != nil {
		return fmt.Errorf("failed to read methods: %w", err)
	}

	// 6. Read objects (two-pass)
	if err := cr.readObjects(vm); err != nil {
		return fmt.Errorf("failed to read objects: %w", err)
	}

	// 7. Read globals
	if err := cr.readGlobals(vm); err != nil {
		return fmt.Errorf("failed to read globals: %w", err)
	}

	// 8. Fixup class values: re-register class values in globals
	cr.fixupClassValues(vm)

	// 9. Read class variables
	if err := cr.readClassVars(vm); err != nil {
		return fmt.Errorf("failed to read class vars: %w", err)
	}

	return nil
}

// ---------------------------------------------------------------------------
// Class reading (two-pass)
// ---------------------------------------------------------------------------

func (cr *ImageReader) readClasses(vm *VM) error {
	count := len(cr.envelope.Classes)
	cr.classes = make([]*Class, count)

	// Pass 1: create or find each class
	for i, def := range cr.envelope.Classes {
		name := cr.resolveString(def.Name)
		namespace := ""
		if def.Namespace >= 0 {
			namespace = cr.resolveString(uint32(def.Namespace))
		}

		// Resolve instance variable names
		instVars := make([]string, len(def.InstVars))
		for j, idx := range def.InstVars {
			instVars[j] = cr.resolveString(idx)
		}

		// Check if class already exists in VM (core classes)
		lookupKey := name
		if namespace != "" {
			lookupKey = namespace + "::" + name
		}

		if existing := vm.Classes.Lookup(lookupKey); existing != nil {
			cr.classes[i] = existing
			cr.decoder.AddClass(existing)
			continue
		}

		// Create new class (superclass linked in pass 2)
		var c *Class
		if len(instVars) > 0 {
			c = NewClassWithInstVars(name, nil, instVars)
		} else {
			c = NewClass(name, nil)
		}
		c.Namespace = namespace
		c.NumSlots = int(def.NumSlots)

		// DocString
		if def.HasDocString {
			c.DocString = cr.resolveString(def.DocString)
		}

		cr.classes[i] = c
		cr.decoder.AddClass(c)
	}

	// Pass 2: link superclasses and register in VM
	for i, def := range cr.envelope.Classes {
		c := cr.classes[i]
		if c == nil {
			continue
		}

		// Skip classes that already exist in the VM (already set up)
		if vm.Classes.Lookup(c.FullName()) != nil && c.VTable != nil {
			continue
		}

		if def.Super >= 0 && int(def.Super) < len(cr.classes) {
			c.Superclass = cr.classes[def.Super]
		} else if c != vm.ObjectClass {
			// No superclass specified — default to Object
			c.Superclass = vm.ObjectClass
		}

		// Set up VTables
		var parentVT *VTable
		var parentClassVT *VTable
		if c.Superclass != nil {
			parentVT = c.Superclass.VTable
			parentClassVT = c.Superclass.ClassVTable
		}
		if c.VTable == nil {
			c.VTable = NewVTable(c, parentVT)
		} else {
			c.VTable.SetParent(parentVT)
		}
		if c.ClassVTable == nil {
			c.ClassVTable = NewVTable(c, parentClassVT)
		} else {
			c.ClassVTable.SetParent(parentClassVT)
		}

		vm.Classes.Register(c)
	}

	return nil
}

// ---------------------------------------------------------------------------
// Method reading
// ---------------------------------------------------------------------------

func (cr *ImageReader) readMethods(vm *VM) error {
	count := len(cr.envelope.Methods)
	cr.methods = make([]*CompiledMethod, count)

	for i, def := range cr.envelope.Methods {
		method, err := cr.readMethod(vm, &def)
		if err != nil {
			return fmt.Errorf("method %d: %w", i, err)
		}
		cr.methods[i] = method
		cr.decoder.AddMethod(method)
	}

	return nil
}

func (cr *ImageReader) readMethod(vm *VM, def *methodDef) (*CompiledMethod, error) {
	name := cr.resolveString(def.Name)

	// Decode literals
	literals := make([]Value, len(def.Literals))
	for j, raw := range def.Literals {
		val, err := decodeImageValue(vm, cr.decoder, raw)
		if err != nil {
			return nil, fmt.Errorf("literal %d: %w", j, err)
		}
		literals[j] = val
	}

	// Copy bytecode and remap selectors
	bytecode := make([]byte, len(def.Bytecode))
	copy(bytecode, def.Bytecode)
	remapBytecodeSelectors(bytecode, cr.selectorIDMap)

	// Build blocks
	blocks := make([]*BlockMethod, len(def.Blocks))
	for j := range def.Blocks {
		block, err := cr.readBlock(vm, &def.Blocks[j])
		if err != nil {
			return nil, fmt.Errorf("block %d: %w", j, err)
		}
		blocks[j] = block
	}

	// Build source map
	sourceMap := make([]SourceLoc, len(def.SourceMap))
	for j, entry := range def.SourceMap {
		sourceMap[j] = SourceLoc{
			Offset: int(entry[0]),
			Line:   int(entry[1]),
			Column: int(entry[2]),
		}
	}

	// Content hash
	var contentHash [32]byte
	if len(def.ContentHash) == 32 {
		copy(contentHash[:], def.ContentHash)
	}

	// Typed hash
	var typedHash [32]byte
	if len(def.TypedHash) == 32 {
		copy(typedHash[:], def.TypedHash)
	}

	// Source and docstring
	source := ""
	if def.HasSource {
		source = cr.resolveString(def.Source)
	}
	docString := ""
	if def.HasDocString {
		docString = cr.resolveString(def.DocString)
	}

	// Get VM selector ID from method name
	vmSelectorID := vm.Selectors.Intern(name)

	method := &CompiledMethod{
		selector:      vmSelectorID,
		name:          name,
		IsClassMethod: def.IsClassMethod,
		Arity:         int(def.Arity),
		NumTemps:      int(def.NumTemps),
		Literals:      literals,
		Bytecode:      bytecode,
		Blocks:        blocks,
		ContentHash:   contentHash,
		TypedHash:     typedHash,
		Source:        source,
		docString:     docString,
		SourceMap:     sourceMap,
	}

	// Link to class and VTable
	if def.Class >= 0 && int(def.Class) < len(cr.classes) {
		class := cr.classes[def.Class]
		method.class = class
		if def.IsClassMethod {
			if class.ClassVTable != nil {
				class.ClassVTable.AddMethod(vmSelectorID, method)
			}
		} else {
			if class.VTable != nil {
				class.VTable.AddMethod(vmSelectorID, method)
			}
		}
	}

	// Link blocks to outer method
	for _, block := range blocks {
		block.Outer = method
	}

	return method, nil
}

func (cr *ImageReader) readBlock(vm *VM, def *blockDef) (*BlockMethod, error) {
	// Decode literals
	literals := make([]Value, len(def.Literals))
	for j, raw := range def.Literals {
		val, err := decodeImageValue(vm, cr.decoder, raw)
		if err != nil {
			return nil, fmt.Errorf("literal %d: %w", j, err)
		}
		literals[j] = val
	}

	// Copy bytecode and remap selectors
	bytecode := make([]byte, len(def.Bytecode))
	copy(bytecode, def.Bytecode)
	remapBytecodeSelectors(bytecode, cr.selectorIDMap)

	// Build source map
	sourceMap := make([]SourceLoc, len(def.SourceMap))
	for j, entry := range def.SourceMap {
		sourceMap[j] = SourceLoc{
			Offset: int(entry[0]),
			Line:   int(entry[1]),
			Column: int(entry[2]),
		}
	}

	block := &BlockMethod{
		Arity:       int(def.Arity),
		NumTemps:    int(def.NumTemps),
		NumCaptures: int(def.NumCaptures),
		Literals:    literals,
		Bytecode:    bytecode,
		SourceMap:   sourceMap,
	}

	if def.HasSource {
		block.Source = cr.resolveString(def.Source)
	}

	return block, nil
}

// ---------------------------------------------------------------------------
// Object reading (two-pass)
// ---------------------------------------------------------------------------

func (cr *ImageReader) readObjects(vm *VM) error {
	count := len(cr.envelope.Objects)
	cr.objects = make([]*Object, count)

	// Pass 1: allocate all objects
	for i, def := range cr.envelope.Objects {
		if def.Class < 0 || int(def.Class) >= len(cr.classes) {
			return fmt.Errorf("object %d: invalid class index %d", i, def.Class)
		}
		class := cr.classes[def.Class]
		numSlots := len(def.Slots)
		obj := NewObject(class.VTable, numSlots)
		cr.objects[i] = obj
		cr.decoder.AddObject(obj)
	}

	// Pass 2: fill slots
	for i, def := range cr.envelope.Objects {
		obj := cr.objects[i]
		for j, raw := range def.Slots {
			val, err := decodeImageValue(vm, cr.decoder, raw)
			if err != nil {
				return fmt.Errorf("object %d slot %d: %w", i, j, err)
			}
			obj.SetSlot(j, val)
		}
	}

	return nil
}

// ---------------------------------------------------------------------------
// Globals reading
// ---------------------------------------------------------------------------

func (cr *ImageReader) readGlobals(vm *VM) error {
	for i, entry := range cr.envelope.Globals {
		name := cr.resolveString(entry.Name)
		val, err := decodeImageValue(vm, cr.decoder, entry.Value)
		if err != nil {
			return fmt.Errorf("global %d (%q): %w", i, name, err)
		}
		vm.SetGlobal(name, val)
	}
	return nil
}

// ---------------------------------------------------------------------------
// Class variable reading
// ---------------------------------------------------------------------------

func (cr *ImageReader) readClassVars(vm *VM) error {
	for i, entry := range cr.envelope.ClassVars {
		if entry.Class < 0 || int(entry.Class) >= len(cr.classes) {
			return fmt.Errorf("class var entry %d: invalid class index %d", i, entry.Class)
		}
		class := cr.classes[entry.Class]

		for j, pair := range entry.Vars {
			name := cr.resolveString(pair.Name)
			val, err := decodeImageValue(vm, cr.decoder, pair.Value)
			if err != nil {
				return fmt.Errorf("class var %d for class %d: %w", j, entry.Class, err)
			}
			vm.registry.SetClassVar(class, name, val)
		}
	}
	return nil
}

// ---------------------------------------------------------------------------
// Fixup class values in globals
// ---------------------------------------------------------------------------

func (cr *ImageReader) fixupClassValues(vm *VM) {
	vm.globalsMu.RLock()
	globalsCopy := make(map[string]Value, len(vm.globals))
	for k, v := range vm.globals {
		globalsCopy[k] = v
	}
	vm.globalsMu.RUnlock()

	for name, v := range globalsCopy {
		if !isClassValue(v) {
			continue
		}
		cls := vm.Classes.Lookup(name)
		if cls == nil {
			continue
		}
		newVal := vm.registry.RegisterClassValue(cls)
		if newVal != v {
			vm.SetGlobal(name, newVal)
		}
	}
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

func (cr *ImageReader) resolveString(idx uint32) string {
	if int(idx) < len(cr.envelope.Strings) {
		return cr.envelope.Strings[idx]
	}
	return ""
}
