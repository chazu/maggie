package vm

// ---------------------------------------------------------------------------
// ImageEncoder: Tracks object->index mappings for serialization
// ---------------------------------------------------------------------------

// ImageEncoder handles encoding Values for image serialization.
// It tracks mappings from runtime objects/symbols/classes to indices.
type ImageEncoder struct {
	// Object mappings: pointer -> index
	objectIndex map[uintptr]uint32
	nextObject  uint32

	// Symbol mappings: symbol ID -> image index
	symbolIndex map[uint32]uint32
	nextSymbol  uint32

	// String mappings: string -> image index
	stringIndex map[string]uint32
	nextString  uint32

	// Class mappings: class pointer -> image index
	classIndex map[*Class]uint32
	nextClass  uint32

	// Method mappings: method pointer -> image index
	methodIndex map[*CompiledMethod]uint32
	nextMethod  uint32

	// Registry for string value access (set from VM context)
	registry *ObjectRegistry
}

// NewImageEncoder creates a new encoder for image serialization.
func NewImageEncoder() *ImageEncoder {
	return &ImageEncoder{
		objectIndex: make(map[uintptr]uint32),
		symbolIndex: make(map[uint32]uint32),
		stringIndex: make(map[string]uint32),
		classIndex:  make(map[*Class]uint32),
		methodIndex: make(map[*CompiledMethod]uint32),
	}
}

// RegisterObject assigns an index to an object pointer.
// Returns the assigned index.
func (e *ImageEncoder) RegisterObject(ptr uintptr) uint32 {
	if idx, ok := e.objectIndex[ptr]; ok {
		return idx
	}
	idx := e.nextObject
	e.objectIndex[ptr] = idx
	e.nextObject++
	return idx
}

// LookupObject returns the index for an object pointer, or 0 and false if not found.
func (e *ImageEncoder) LookupObject(ptr uintptr) (uint32, bool) {
	idx, ok := e.objectIndex[ptr]
	return idx, ok
}

// RegisterSymbol assigns an index to a symbol ID.
// Returns the assigned index.
func (e *ImageEncoder) RegisterSymbol(symID uint32) uint32 {
	if idx, ok := e.symbolIndex[symID]; ok {
		return idx
	}
	idx := e.nextSymbol
	e.symbolIndex[symID] = idx
	e.nextSymbol++
	return idx
}

// LookupSymbol returns the index for a symbol ID, or 0 and false if not found.
func (e *ImageEncoder) LookupSymbol(symID uint32) (uint32, bool) {
	idx, ok := e.symbolIndex[symID]
	return idx, ok
}

// RegisterString assigns an index to a string.
// Returns the assigned index.
func (e *ImageEncoder) RegisterString(s string) uint32 {
	if idx, ok := e.stringIndex[s]; ok {
		return idx
	}
	idx := e.nextString
	e.stringIndex[s] = idx
	e.nextString++
	return idx
}

// LookupString returns the index for a string, or 0 and false if not found.
func (e *ImageEncoder) LookupString(s string) (uint32, bool) {
	idx, ok := e.stringIndex[s]
	return idx, ok
}

// RegisterClass assigns an index to a class.
// Returns the assigned index.
func (e *ImageEncoder) RegisterClass(c *Class) uint32 {
	if idx, ok := e.classIndex[c]; ok {
		return idx
	}
	idx := e.nextClass
	e.classIndex[c] = idx
	e.nextClass++
	return idx
}

// LookupClass returns the index for a class, or 0 and false if not found.
func (e *ImageEncoder) LookupClass(c *Class) (uint32, bool) {
	idx, ok := e.classIndex[c]
	return idx, ok
}

// RegisterMethod assigns an index to a compiled method.
// Returns the assigned index.
func (e *ImageEncoder) RegisterMethod(m *CompiledMethod) uint32 {
	if idx, ok := e.methodIndex[m]; ok {
		return idx
	}
	idx := e.nextMethod
	e.methodIndex[m] = idx
	e.nextMethod++
	return idx
}

// LookupMethod returns the index for a method, or 0 and false if not found.
func (e *ImageEncoder) LookupMethod(m *CompiledMethod) (uint32, bool) {
	idx, ok := e.methodIndex[m]
	return idx, ok
}

// ObjectCount returns the number of registered objects.
func (e *ImageEncoder) ObjectCount() int {
	return len(e.objectIndex)
}

// SymbolCount returns the number of registered symbols.
func (e *ImageEncoder) SymbolCount() int {
	return len(e.symbolIndex)
}

// StringCount returns the number of registered strings.
func (e *ImageEncoder) StringCount() int {
	return len(e.stringIndex)
}

// ClassCount returns the number of registered classes.
func (e *ImageEncoder) ClassCount() int {
	return len(e.classIndex)
}

// MethodCount returns the number of registered methods.
func (e *ImageEncoder) MethodCount() int {
	return len(e.methodIndex)
}

// ---------------------------------------------------------------------------
// ImageDecoder: Tracks index->object mappings for deserialization
// ---------------------------------------------------------------------------

// ImageDecoder handles decoding Values from image data.
// It tracks mappings from image indices back to runtime objects.
type ImageDecoder struct {
	// Object mappings: index -> Object
	objects []*Object

	// Symbol mappings: image index -> symbol ID
	symbols []uint32

	// String mappings: image index -> string
	strings []string

	// Class mappings: image index -> Class
	classes []*Class

	// Method mappings: image index -> CompiledMethod
	methods []*CompiledMethod

	// Registry for string value creation (set from VM context)
	registry *ObjectRegistry
}

// NewImageDecoder creates a new decoder for image deserialization.
func NewImageDecoder() *ImageDecoder {
	return &ImageDecoder{
		objects: make([]*Object, 0),
		symbols: make([]uint32, 0),
		strings: make([]string, 0),
		classes: make([]*Class, 0),
		methods: make([]*CompiledMethod, 0),
	}
}

// AddObject registers an object at the next index.
// Returns the assigned index.
func (d *ImageDecoder) AddObject(obj *Object) uint32 {
	idx := uint32(len(d.objects))
	d.objects = append(d.objects, obj)
	return idx
}

// SetObject sets an object at a specific index.
// The objects slice is grown if necessary.
func (d *ImageDecoder) SetObject(idx uint32, obj *Object) {
	for uint32(len(d.objects)) <= idx {
		d.objects = append(d.objects, nil)
	}
	d.objects[idx] = obj
}

// GetObject returns the object at the given index, or nil if out of range.
func (d *ImageDecoder) GetObject(idx uint32) *Object {
	if int(idx) >= len(d.objects) {
		return nil
	}
	return d.objects[idx]
}

// AddSymbol registers a symbol ID at the next index.
// Returns the assigned index.
func (d *ImageDecoder) AddSymbol(symID uint32) uint32 {
	idx := uint32(len(d.symbols))
	d.symbols = append(d.symbols, symID)
	return idx
}

// SetSymbol sets a symbol ID at a specific index.
func (d *ImageDecoder) SetSymbol(idx uint32, symID uint32) {
	for uint32(len(d.symbols)) <= idx {
		d.symbols = append(d.symbols, 0)
	}
	d.symbols[idx] = symID
}

// GetSymbol returns the symbol ID at the given index.
func (d *ImageDecoder) GetSymbol(idx uint32) uint32 {
	if int(idx) >= len(d.symbols) {
		return 0
	}
	return d.symbols[idx]
}

// AddString registers a string at the next index.
// Returns the assigned index.
func (d *ImageDecoder) AddString(s string) uint32 {
	idx := uint32(len(d.strings))
	d.strings = append(d.strings, s)
	return idx
}

// SetString sets a string at a specific index.
func (d *ImageDecoder) SetString(idx uint32, s string) {
	for uint32(len(d.strings)) <= idx {
		d.strings = append(d.strings, "")
	}
	d.strings[idx] = s
}

// GetString returns the string at the given index.
func (d *ImageDecoder) GetString(idx uint32) string {
	if int(idx) >= len(d.strings) {
		return ""
	}
	return d.strings[idx]
}

// AddClass registers a class at the next index.
// Returns the assigned index.
func (d *ImageDecoder) AddClass(c *Class) uint32 {
	idx := uint32(len(d.classes))
	d.classes = append(d.classes, c)
	return idx
}

// SetClass sets a class at a specific index.
func (d *ImageDecoder) SetClass(idx uint32, c *Class) {
	for uint32(len(d.classes)) <= idx {
		d.classes = append(d.classes, nil)
	}
	d.classes[idx] = c
}

// GetClass returns the class at the given index.
func (d *ImageDecoder) GetClass(idx uint32) *Class {
	if int(idx) >= len(d.classes) {
		return nil
	}
	return d.classes[idx]
}

// AddMethod registers a method at the next index.
// Returns the assigned index.
func (d *ImageDecoder) AddMethod(m *CompiledMethod) uint32 {
	idx := uint32(len(d.methods))
	d.methods = append(d.methods, m)
	return idx
}

// SetMethod sets a method at a specific index.
func (d *ImageDecoder) SetMethod(idx uint32, m *CompiledMethod) {
	for uint32(len(d.methods)) <= idx {
		d.methods = append(d.methods, nil)
	}
	d.methods[idx] = m
}

// GetMethod returns the method at the given index.
func (d *ImageDecoder) GetMethod(idx uint32) *CompiledMethod {
	if int(idx) >= len(d.methods) {
		return nil
	}
	return d.methods[idx]
}

// ObjectCount returns the number of registered objects.
func (d *ImageDecoder) ObjectCount() int {
	return len(d.objects)
}

// SymbolCount returns the number of registered symbols.
func (d *ImageDecoder) SymbolCount() int {
	return len(d.symbols)
}

// StringCount returns the number of registered strings.
func (d *ImageDecoder) StringCount() int {
	return len(d.strings)
}

// ClassCount returns the number of registered classes.
func (d *ImageDecoder) ClassCount() int {
	return len(d.classes)
}

// MethodCount returns the number of registered methods.
func (d *ImageDecoder) MethodCount() int {
	return len(d.methods)
}
