package vm

import (
	"bytes"
	"io"
	"os"
	"sort"
	"unsafe"
)

// ---------------------------------------------------------------------------
// Image Format Constants
// ---------------------------------------------------------------------------

// ImageMagic is the magic number identifying a Maggie image file.
var ImageMagic = [4]byte{'M', 'A', 'G', 'I'}

// Image format version
const ImageVersion uint32 = 1

// Image header size in bytes
// magic(4) + version(4) + flags(4) + objectCount(4) + stringTableOffset(8) + classTableOffset(8) + entryPoint(4) = 36
const ImageHeaderSize = 36

// Image flags
const (
	ImageFlagNone       uint32 = 0
	ImageFlagDebugInfo  uint32 = 1 << 0 // Includes source maps and debug info
	ImageFlagCompressed uint32 = 1 << 1 // Reserved for future compression
)

// ---------------------------------------------------------------------------
// ImageWriter: Serializes VM state to a binary image
// ---------------------------------------------------------------------------

// ImageWriter handles serialization of VM state to a binary image file.
type ImageWriter struct {
	// Output buffer
	buf *bytes.Buffer

	// Encoder for value serialization and index tracking
	encoder *ImageEncoder

	// Section offsets (for header back-patching)
	stringTableOffset uint64
	symbolTableOffset uint64
	selectorTableOffset uint64
	classTableOffset  uint64
	methodTableOffset uint64
	objectTableOffset uint64
	globalsTableOffset uint64

	// Collected data for serialization
	strings   []string           // Ordered list of strings
	symbols   []uint32           // Symbol IDs in image order
	selectors []int              // Selector IDs in image order
	classes   []*Class           // Classes in image order
	methods   []*CompiledMethod  // Methods in image order
	objects   []*Object          // Objects in image order

	// Flags
	flags uint32

	// Entry point method index (0 if none)
	entryPoint uint32
}

// NewImageWriter creates a new image writer.
func NewImageWriter() *ImageWriter {
	return &ImageWriter{
		buf:     bytes.NewBuffer(nil),
		encoder: NewImageEncoder(),
		flags:   ImageFlagNone,
	}
}

// ---------------------------------------------------------------------------
// Pre-registration phase: Collect and index all data
// ---------------------------------------------------------------------------

// collectFromVM collects all data from the VM for serialization.
func (w *ImageWriter) collectFromVM(vm *VM) {
	// Collect strings from symbols
	allSymbols := vm.Symbols.All()
	for _, name := range allSymbols {
		w.registerString(name)
	}

	// Collect strings from selectors
	allSelectors := vm.Selectors.All()
	for _, name := range allSelectors {
		w.registerString(name)
	}

	// Collect strings from global names (before writing string table!)
	for name := range vm.Globals {
		w.registerString(name)
	}

	// Collect symbols in order
	for i := 0; i < len(allSymbols); i++ {
		w.registerSymbol(uint32(i))
	}

	// Collect selectors in order
	for i := 0; i < len(allSelectors); i++ {
		w.registerSelector(i)
	}

	// Collect classes
	for _, class := range vm.Classes.All() {
		w.collectClass(class)
	}

	// Collect objects from globals and class instances
	w.objects = vm.CollectAllObjects()
	for _, obj := range w.objects {
		w.encoder.RegisterObject(uintptr(unsafe.Pointer(obj)))
	}
}

// collectClass collects a class and its methods for serialization.
func (w *ImageWriter) collectClass(c *Class) {
	// Register class name and namespace as strings
	w.registerString(c.Name)
	if c.Namespace != "" {
		w.registerString(c.Namespace)
	}

	// Register instance variable names
	for _, ivar := range c.InstVars {
		w.registerString(ivar)
	}

	// Register the class
	w.encoder.RegisterClass(c)
	w.classes = append(w.classes, c)

	// Collect compiled methods from VTable (instance methods)
	if c.VTable != nil {
		for _, method := range c.VTable.methods {
			if cm, ok := method.(*CompiledMethod); ok {
				w.collectMethod(cm)
			}
		}
	}

	// Collect compiled methods from ClassVTable (class methods)
	if c.ClassVTable != nil {
		for _, method := range c.ClassVTable.methods {
			if cm, ok := method.(*CompiledMethod); ok {
				w.collectMethod(cm)
			}
		}
	}
}

// collectMethod collects a compiled method for serialization.
func (w *ImageWriter) collectMethod(m *CompiledMethod) {
	// Check if already registered
	if _, ok := w.encoder.LookupMethod(m); ok {
		return
	}

	// Register method name
	w.registerString(m.name)

	// Register source
	if m.Source != "" {
		w.registerString(m.Source)
	}

	// Register the method
	w.encoder.RegisterMethod(m)
	w.methods = append(w.methods, m)

	// Collect literals that are symbols or strings
	for _, lit := range m.Literals {
		if IsStringValue(lit) {
			// String literals - must check before IsSymbol since both use symbol tag
			w.registerString(GetStringContent(lit))
		} else if lit.IsSymbol() {
			w.registerSymbol(lit.SymbolID())
		}
	}

	// Collect nested blocks
	for _, block := range m.Blocks {
		w.collectBlock(block)
	}
}

// collectBlock collects a block method for serialization.
func (w *ImageWriter) collectBlock(b *BlockMethod) {
	if b.Source != "" {
		w.registerString(b.Source)
	}

	// Collect literals that are symbols or strings
	for _, lit := range b.Literals {
		if IsStringValue(lit) {
			// String literals - must check before IsSymbol since both use symbol tag
			w.registerString(GetStringContent(lit))
		} else if lit.IsSymbol() {
			w.registerSymbol(lit.SymbolID())
		}
	}
}

// registerString registers a string and returns its index.
func (w *ImageWriter) registerString(s string) uint32 {
	idx := w.encoder.RegisterString(s)
	// Add to ordered list if new
	if int(idx) >= len(w.strings) {
		w.strings = append(w.strings, s)
	}
	return idx
}

// registerSymbol registers a symbol and returns its index.
func (w *ImageWriter) registerSymbol(symID uint32) uint32 {
	idx := w.encoder.RegisterSymbol(symID)
	// Add to ordered list if new
	if int(idx) >= len(w.symbols) {
		w.symbols = append(w.symbols, symID)
	}
	return idx
}

// registerSelector registers a selector and returns its index.
func (w *ImageWriter) registerSelector(selID int) int {
	// Selectors are tracked separately from symbols
	for i, id := range w.selectors {
		if id == selID {
			return i
		}
	}
	idx := len(w.selectors)
	w.selectors = append(w.selectors, selID)
	return idx
}

// ---------------------------------------------------------------------------
// Header writing
// ---------------------------------------------------------------------------

// writeHeader writes the image header with placeholder offsets.
// The offsets will be back-patched after all sections are written.
func (w *ImageWriter) writeHeader() {
	// Magic number
	w.buf.Write(ImageMagic[:])

	// Version
	buf := make([]byte, 4)
	WriteUint32(buf, ImageVersion)
	w.buf.Write(buf)

	// Flags
	WriteUint32(buf, w.flags)
	w.buf.Write(buf)

	// Object count (placeholder)
	WriteUint32(buf, uint32(len(w.objects)))
	w.buf.Write(buf)

	// String table offset (placeholder - 8 bytes)
	buf8 := make([]byte, 8)
	WriteUint64(buf8, 0)
	w.buf.Write(buf8)

	// Class table offset (placeholder - 8 bytes)
	WriteUint64(buf8, 0)
	w.buf.Write(buf8)

	// Entry point method index
	WriteUint32(buf, w.entryPoint)
	w.buf.Write(buf)
}

// patchHeader updates the header with final section offsets.
func (w *ImageWriter) patchHeader() {
	data := w.buf.Bytes()

	// Patch string table offset at offset 16
	WriteUint64(data[16:], w.stringTableOffset)

	// Patch class table offset at offset 24
	WriteUint64(data[24:], w.classTableOffset)

	// Patch object count at offset 12
	WriteUint32(data[12:], uint32(len(w.objects)))
}

// ---------------------------------------------------------------------------
// String table writing
// ---------------------------------------------------------------------------

// writeStringTable writes the string table section.
func (w *ImageWriter) writeStringTable() {
	w.stringTableOffset = uint64(w.buf.Len())

	// Count
	buf := make([]byte, 4)
	WriteUint32(buf, uint32(len(w.strings)))
	w.buf.Write(buf)

	// Strings: [length:32 | utf8 bytes]...
	for _, s := range w.strings {
		// Length as uint32
		WriteUint32(buf, uint32(len(s)))
		w.buf.Write(buf)
		// UTF8 bytes
		w.buf.WriteString(s)
	}
}

// ---------------------------------------------------------------------------
// Symbol table writing
// ---------------------------------------------------------------------------

// writeSymbolTable writes the symbol table section.
func (w *ImageWriter) writeSymbolTable(symbols *SymbolTable) {
	w.symbolTableOffset = uint64(w.buf.Len())

	// Get all symbol names in order
	allNames := symbols.All()

	// Count
	buf := make([]byte, 4)
	WriteUint32(buf, uint32(len(allNames)))
	w.buf.Write(buf)

	// Symbols: [string-index:32]...
	for _, name := range allNames {
		// Look up string index
		strIdx, _ := w.encoder.LookupString(name)
		WriteUint32(buf, strIdx)
		w.buf.Write(buf)
	}
}

// ---------------------------------------------------------------------------
// Selector table writing
// ---------------------------------------------------------------------------

// writeSelectorTable writes the selector table section.
func (w *ImageWriter) writeSelectorTable(selectors *SelectorTable) {
	w.selectorTableOffset = uint64(w.buf.Len())

	// Get all selector names in order
	allNames := selectors.All()

	// Count
	buf := make([]byte, 4)
	WriteUint32(buf, uint32(len(allNames)))
	w.buf.Write(buf)

	// Selectors: [string-index:32]...
	for _, name := range allNames {
		// Look up string index (selectors share the string table)
		strIdx, _ := w.encoder.LookupString(name)
		WriteUint32(buf, strIdx)
		w.buf.Write(buf)
	}
}

// ---------------------------------------------------------------------------
// Class writing
// ---------------------------------------------------------------------------

// writeClasses writes the class definitions section.
func (w *ImageWriter) writeClasses() {
	w.classTableOffset = uint64(w.buf.Len())

	// Sort classes by dependency (superclasses first)
	sortedClasses := w.sortClassesByDependency()

	// IMPORTANT: Update the encoder's class indices to match the sorted order.
	// Methods reference classes by index, so the indices must match the
	// positions in the written output (sorted order), not the registration order.
	w.updateClassIndicesForSortedOrder(sortedClasses)

	// Count
	buf := make([]byte, 4)
	WriteUint32(buf, uint32(len(sortedClasses)))
	w.buf.Write(buf)

	// Write each class
	for _, c := range sortedClasses {
		w.writeClass(c)
	}
}

// updateClassIndicesForSortedOrder updates the encoder's class mappings
// to reflect the sorted write order instead of the registration order.
func (w *ImageWriter) updateClassIndicesForSortedOrder(sortedClasses []*Class) {
	// Clear existing class mappings
	w.encoder.classIndex = make(map[*Class]uint32)
	w.encoder.nextClass = 0

	// Re-register classes in sorted order
	for i, c := range sortedClasses {
		w.encoder.classIndex[c] = uint32(i)
	}
	w.encoder.nextClass = uint32(len(sortedClasses))
}

// sortClassesByDependency sorts classes so superclasses come before subclasses.
func (w *ImageWriter) sortClassesByDependency() []*Class {
	// Create a copy to sort
	sorted := make([]*Class, len(w.classes))
	copy(sorted, w.classes)

	// Sort by depth (root classes first)
	sort.Slice(sorted, func(i, j int) bool {
		return sorted[i].Depth() < sorted[j].Depth()
	})

	return sorted
}

// writeClass writes a single class definition.
func (w *ImageWriter) writeClass(c *Class) {
	buf := make([]byte, 4)

	// Name string index
	nameIdx, _ := w.encoder.LookupString(c.Name)
	WriteUint32(buf, nameIdx)
	w.buf.Write(buf)

	// Namespace string index (0xFFFFFFFF if none)
	if c.Namespace != "" {
		nsIdx, _ := w.encoder.LookupString(c.Namespace)
		WriteUint32(buf, nsIdx)
	} else {
		WriteUint32(buf, 0xFFFFFFFF)
	}
	w.buf.Write(buf)

	// Superclass index (0xFFFFFFFF if none)
	if c.Superclass != nil {
		superIdx, ok := w.encoder.LookupClass(c.Superclass)
		if ok {
			WriteUint32(buf, superIdx)
		} else {
			WriteUint32(buf, 0xFFFFFFFF)
		}
	} else {
		WriteUint32(buf, 0xFFFFFFFF)
	}
	w.buf.Write(buf)

	// Number of slots
	WriteUint32(buf, uint32(c.NumSlots))
	w.buf.Write(buf)

	// Instance variable count
	WriteUint32(buf, uint32(len(c.InstVars)))
	w.buf.Write(buf)

	// Instance variable names
	for _, ivar := range c.InstVars {
		ivarIdx, _ := w.encoder.LookupString(ivar)
		WriteUint32(buf, ivarIdx)
		w.buf.Write(buf)
	}

	// Instance method count (count CompiledMethods only)
	methodCount := 0
	if c.VTable != nil {
		for _, m := range c.VTable.methods {
			if _, ok := m.(*CompiledMethod); ok {
				methodCount++
			}
		}
	}
	WriteUint32(buf, uint32(methodCount))
	w.buf.Write(buf)

	// Instance method indices
	if c.VTable != nil {
		for _, m := range c.VTable.methods {
			if cm, ok := m.(*CompiledMethod); ok {
				methodIdx, _ := w.encoder.LookupMethod(cm)
				WriteUint32(buf, methodIdx)
				w.buf.Write(buf)
			}
		}
	}

	// Class method count (ClassVTable CompiledMethods)
	classMethodCount := 0
	if c.ClassVTable != nil {
		for _, m := range c.ClassVTable.methods {
			if _, ok := m.(*CompiledMethod); ok {
				classMethodCount++
			}
		}
	}
	WriteUint32(buf, uint32(classMethodCount))
	w.buf.Write(buf)

	// Class method indices
	if c.ClassVTable != nil {
		for _, m := range c.ClassVTable.methods {
			if cm, ok := m.(*CompiledMethod); ok {
				methodIdx, _ := w.encoder.LookupMethod(cm)
				WriteUint32(buf, methodIdx)
				w.buf.Write(buf)
			}
		}
	}
}

// ---------------------------------------------------------------------------
// Method writing
// ---------------------------------------------------------------------------

// writeMethods writes the compiled methods section.
func (w *ImageWriter) writeMethods() {
	w.methodTableOffset = uint64(w.buf.Len())

	// Count
	buf := make([]byte, 4)
	WriteUint32(buf, uint32(len(w.methods)))
	w.buf.Write(buf)

	// Write each method
	for _, m := range w.methods {
		w.writeMethod(m)
	}
}

// writeMethod writes a single compiled method.
func (w *ImageWriter) writeMethod(m *CompiledMethod) {
	buf := make([]byte, 4)
	buf8 := make([]byte, 8)

	// Selector ID
	WriteInt32(buf, int32(m.selector))
	w.buf.Write(buf)

	// Class index (0xFFFFFFFF if none)
	if m.class != nil {
		classIdx, ok := w.encoder.LookupClass(m.class)
		if ok {
			WriteUint32(buf, classIdx)
		} else {
			WriteUint32(buf, 0xFFFFFFFF)
		}
	} else {
		WriteUint32(buf, 0xFFFFFFFF)
	}
	w.buf.Write(buf)

	// Name string index
	nameIdx, _ := w.encoder.LookupString(m.name)
	WriteUint32(buf, nameIdx)
	w.buf.Write(buf)

	// IsClassMethod flag (1 byte: 0 = instance method, 1 = class method)
	if m.IsClassMethod {
		w.buf.WriteByte(1)
	} else {
		w.buf.WriteByte(0)
	}

	// Arity
	WriteUint32(buf, uint32(m.Arity))
	w.buf.Write(buf)

	// NumTemps
	WriteUint32(buf, uint32(m.NumTemps))
	w.buf.Write(buf)

	// Literals count
	WriteUint32(buf, uint32(len(m.Literals)))
	w.buf.Write(buf)

	// Literals (each is EncodedValueSize bytes)
	for _, lit := range m.Literals {
		w.buf.Write(w.encoder.EncodeValue(lit))
	}

	// Bytecode length
	WriteUint32(buf, uint32(len(m.Bytecode)))
	w.buf.Write(buf)

	// Bytecode
	w.buf.Write(m.Bytecode)

	// Blocks count
	WriteUint32(buf, uint32(len(m.Blocks)))
	w.buf.Write(buf)

	// Blocks
	for _, block := range m.Blocks {
		w.writeBlock(block)
	}

	// Source (optional)
	hasSource := m.Source != ""
	if hasSource {
		w.buf.WriteByte(1)
		srcIdx, _ := w.encoder.LookupString(m.Source)
		WriteUint32(buf, srcIdx)
		w.buf.Write(buf)
	} else {
		w.buf.WriteByte(0)
	}

	// Source map count
	WriteUint32(buf, uint32(len(m.SourceMap)))
	w.buf.Write(buf)

	// Source map entries
	for _, loc := range m.SourceMap {
		WriteUint32(buf, uint32(loc.Offset))
		w.buf.Write(buf)
		WriteUint32(buf, uint32(loc.Line))
		w.buf.Write(buf)
		WriteUint32(buf, uint32(loc.Column))
		w.buf.Write(buf)
	}

	// Pad to 8-byte alignment (for future extensibility)
	_ = buf8 // suppress unused warning
}

// writeBlock writes a single block method.
func (w *ImageWriter) writeBlock(b *BlockMethod) {
	buf := make([]byte, 4)

	// Arity
	WriteUint32(buf, uint32(b.Arity))
	w.buf.Write(buf)

	// NumTemps
	WriteUint32(buf, uint32(b.NumTemps))
	w.buf.Write(buf)

	// NumCaptures
	WriteUint32(buf, uint32(b.NumCaptures))
	w.buf.Write(buf)

	// Literals count
	WriteUint32(buf, uint32(len(b.Literals)))
	w.buf.Write(buf)

	// Literals
	for _, lit := range b.Literals {
		w.buf.Write(w.encoder.EncodeValue(lit))
	}

	// Bytecode length
	WriteUint32(buf, uint32(len(b.Bytecode)))
	w.buf.Write(buf)

	// Bytecode
	w.buf.Write(b.Bytecode)

	// Source map count
	WriteUint32(buf, uint32(len(b.SourceMap)))
	w.buf.Write(buf)

	// Source map entries
	for _, loc := range b.SourceMap {
		WriteUint32(buf, uint32(loc.Offset))
		w.buf.Write(buf)
		WriteUint32(buf, uint32(loc.Line))
		w.buf.Write(buf)
		WriteUint32(buf, uint32(loc.Column))
		w.buf.Write(buf)
	}
}

// ---------------------------------------------------------------------------
// Object writing
// ---------------------------------------------------------------------------

// writeObjects writes the objects section.
func (w *ImageWriter) writeObjects() {
	w.objectTableOffset = uint64(w.buf.Len())

	// Count
	buf := make([]byte, 4)
	WriteUint32(buf, uint32(len(w.objects)))
	w.buf.Write(buf)

	// Write each object
	for _, obj := range w.objects {
		w.writeObject(obj)
	}
}

// writeObject writes a single object.
func (w *ImageWriter) writeObject(obj *Object) {
	buf := make([]byte, 4)

	// Class index
	var classIdx uint32 = 0xFFFFFFFF
	if obj.vtable != nil && obj.vtable.class != nil {
		if idx, ok := w.encoder.LookupClass(obj.vtable.class); ok {
			classIdx = idx
		}
	}
	WriteUint32(buf, classIdx)
	w.buf.Write(buf)

	// Slot count
	numSlots := obj.NumSlots()
	WriteUint32(buf, uint32(numSlots))
	w.buf.Write(buf)

	// Slots
	obj.ForEachSlot(func(index int, value Value) {
		w.buf.Write(w.encoder.EncodeValue(value))
	})
}

// ---------------------------------------------------------------------------
// Globals writing
// ---------------------------------------------------------------------------

// writeGlobals writes the globals section.
func (w *ImageWriter) writeGlobals(globals map[string]Value) {
	w.globalsTableOffset = uint64(w.buf.Len())

	// Count
	buf := make([]byte, 4)
	WriteUint32(buf, uint32(len(globals)))
	w.buf.Write(buf)

	// Sort global names for deterministic output
	names := make([]string, 0, len(globals))
	for name := range globals {
		names = append(names, name)
	}
	sort.Strings(names)

	// Write each global
	for _, name := range names {
		value := globals[name]

		// Name string index
		nameIdx := w.registerString(name)
		WriteUint32(buf, nameIdx)
		w.buf.Write(buf)

		// Value
		w.buf.Write(w.encoder.EncodeValue(value))
	}
}

// ---------------------------------------------------------------------------
// Main serialization API
// ---------------------------------------------------------------------------

// WriteTo writes the image to the given writer.
func (w *ImageWriter) WriteTo(out io.Writer) (int64, error) {
	n, err := out.Write(w.buf.Bytes())
	return int64(n), err
}

// Bytes returns the serialized image as a byte slice.
func (w *ImageWriter) Bytes() []byte {
	return w.buf.Bytes()
}

// SetEntryPoint sets the entry point method index.
func (w *ImageWriter) SetEntryPoint(methodIndex uint32) {
	w.entryPoint = methodIndex
}

// SetFlags sets the image flags.
func (w *ImageWriter) SetFlags(flags uint32) {
	w.flags = flags
}

// ---------------------------------------------------------------------------
// VM integration
// ---------------------------------------------------------------------------

// SaveImage saves the VM state to a file.
func (vm *VM) SaveImage(path string) error {
	f, err := os.Create(path)
	if err != nil {
		return err
	}
	defer f.Close()

	return vm.SaveImageTo(f)
}

// SaveImageTo saves the VM state to a writer.
func (vm *VM) SaveImageTo(w io.Writer) error {
	writer := NewImageWriter()

	// Collect all data from VM
	writer.collectFromVM(vm)

	// Write sections in order
	writer.writeHeader()
	writer.writeStringTable()
	writer.writeSymbolTable(vm.Symbols)
	writer.writeSelectorTable(vm.Selectors)
	writer.writeClasses()
	writer.writeMethods()
	writer.writeObjects()
	writer.writeGlobals(vm.Globals)

	// Patch header with final offsets
	writer.patchHeader()

	// Write to output
	_, err := writer.WriteTo(w)
	return err
}

// CollectAllObjects traverses from roots and collects all reachable objects.
func (vm *VM) CollectAllObjects() []*Object {
	visited := make(map[uintptr]bool)
	var objects []*Object

	var visit func(v Value)
	visit = func(v Value) {
		if !v.IsObject() {
			return
		}

		ptr := uintptr(v.ObjectPtr())
		if visited[ptr] {
			return
		}
		visited[ptr] = true

		obj := ObjectFromValue(v)
		if obj == nil {
			return
		}

		objects = append(objects, obj)

		// Visit slots
		obj.ForEachSlot(func(index int, slotVal Value) {
			visit(slotVal)
		})
	}

	// Visit globals
	for _, v := range vm.Globals {
		visit(v)
	}

	// Visit class instances (none for now, but classes are in globals)

	return objects
}
