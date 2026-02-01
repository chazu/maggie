package vm

import (
	"encoding/binary"
	"math"
)

// ---------------------------------------------------------------------------
// Image Value Encoding: Serialization format for Values in the image
// ---------------------------------------------------------------------------

// Image encoding tags - high 4 bits indicate type, remaining bits are payload
const (
	imageTagFloat     byte = 0x0 // 64-bit float (stored directly)
	imageTagSmallInt  byte = 0x1 // 48-bit signed integer
	imageTagObject    byte = 0x2 // object index
	imageTagNil       byte = 0x3
	imageTagTrue      byte = 0x4
	imageTagFalse     byte = 0x5
	imageTagSymbol    byte = 0x6 // symbol table index
	imageTagString    byte = 0x7 // string table index (for literal strings)
	imageTagClass     byte = 0x8 // class table index
	imageTagMethod    byte = 0x9 // method table index
	imageTagCharacter byte = 0xA // Unicode code point (stored as uint32)
)

// EncodedValueSize is the size of an encoded value in bytes.
// Format: 1 byte tag + up to 8 bytes payload
const EncodedValueSize = 9

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

// EncodeValue serializes a Value to bytes.
// The result is always EncodedValueSize bytes.
func (e *ImageEncoder) EncodeValue(v Value) []byte {
	buf := make([]byte, EncodedValueSize)
	e.EncodeValueTo(v, buf)
	return buf
}

// EncodeValueTo serializes a Value into the provided buffer.
// The buffer must be at least EncodedValueSize bytes.
func (e *ImageEncoder) EncodeValueTo(v Value, buf []byte) {
	switch {
	case v == Nil:
		buf[0] = imageTagNil
		// Remaining bytes are zero (payload unused)

	case v == True:
		buf[0] = imageTagTrue

	case v == False:
		buf[0] = imageTagFalse

	case v.IsSmallInt():
		buf[0] = imageTagSmallInt
		n := v.SmallInt()
		// Store as 8-byte signed integer (little endian)
		binary.LittleEndian.PutUint64(buf[1:], uint64(n))

	case v.IsFloat():
		buf[0] = imageTagFloat
		f := v.Float64()
		bits := math.Float64bits(f)
		binary.LittleEndian.PutUint64(buf[1:], bits)

	case IsStringValue(v):
		// String values must be checked BEFORE symbols since they use the same tag
		buf[0] = imageTagString
		content := GetStringContent(v)
		idx := e.RegisterString(content)
		binary.LittleEndian.PutUint32(buf[1:], idx)

	case IsCharacterValue(v):
		// Character values must be checked BEFORE symbols since they use the same tag
		buf[0] = imageTagCharacter
		cp := GetCharacterCodePoint(v)
		binary.LittleEndian.PutUint32(buf[1:], uint32(cp))

	case isClassValue(v):
		buf[0] = imageTagClass
		cls := getClassFromValue(v)
		if cls != nil {
			idx := e.RegisterClass(cls)
			binary.LittleEndian.PutUint32(buf[1:], idx)
		}

	case v.IsSymbol():
		buf[0] = imageTagSymbol
		symID := v.SymbolID()
		// Register the symbol and store its image index
		idx := e.RegisterSymbol(symID)
		binary.LittleEndian.PutUint32(buf[1:], idx)

	case v.IsObject():
		buf[0] = imageTagObject
		ptr := uintptr(v.ObjectPtr())
		// Register the object and store its image index
		idx := e.RegisterObject(ptr)
		binary.LittleEndian.PutUint32(buf[1:], idx)

	default:
		// Unknown value type - encode as nil
		buf[0] = imageTagNil
	}
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

// DecodeValue deserializes bytes to a Value.
// The data must be at least EncodedValueSize bytes.
func (d *ImageDecoder) DecodeValue(data []byte) Value {
	if len(data) < EncodedValueSize {
		return Nil
	}

	tag := data[0]

	switch tag {
	case imageTagNil:
		return Nil

	case imageTagTrue:
		return True

	case imageTagFalse:
		return False

	case imageTagSmallInt:
		bits := binary.LittleEndian.Uint64(data[1:])
		n := int64(bits)
		return FromSmallInt(n)

	case imageTagFloat:
		bits := binary.LittleEndian.Uint64(data[1:])
		f := math.Float64frombits(bits)
		return FromFloat64(f)

	case imageTagSymbol:
		idx := binary.LittleEndian.Uint32(data[1:])
		symID := d.GetSymbol(idx)
		return FromSymbolID(symID)

	case imageTagCharacter:
		cp := binary.LittleEndian.Uint32(data[1:])
		return FromCharacter(rune(cp))

	case imageTagObject:
		idx := binary.LittleEndian.Uint32(data[1:])
		obj := d.GetObject(idx)
		if obj == nil {
			return Nil
		}
		return obj.ToValue()

	case imageTagString:
		// String values: get string content from table and create a new string value
		idx := binary.LittleEndian.Uint32(data[1:])
		content := d.GetString(idx)
		return NewStringValue(content)

	case imageTagClass:
		idx := binary.LittleEndian.Uint32(data[1:])
		c := d.GetClass(idx)
		if c == nil {
			return Nil
		}
		return registerClassValue(c)

	case imageTagMethod:
		idx := binary.LittleEndian.Uint32(data[1:])
		_ = d.GetMethod(idx)
		// Methods are not directly representable as Values
		return Nil

	default:
		return Nil
	}
}

// ---------------------------------------------------------------------------
// Binary encoding helpers
// ---------------------------------------------------------------------------

// WriteUint64 writes a uint64 in little-endian format.
func WriteUint64(buf []byte, v uint64) {
	binary.LittleEndian.PutUint64(buf, v)
}

// ReadUint64 reads a uint64 in little-endian format.
func ReadUint64(buf []byte) uint64 {
	return binary.LittleEndian.Uint64(buf)
}

// WriteInt64 writes an int64 in little-endian format.
func WriteInt64(buf []byte, v int64) {
	binary.LittleEndian.PutUint64(buf, uint64(v))
}

// ReadInt64 reads an int64 in little-endian format.
func ReadInt64(buf []byte) int64 {
	return int64(binary.LittleEndian.Uint64(buf))
}

// WriteUint32 writes a uint32 in little-endian format.
func WriteUint32(buf []byte, v uint32) {
	binary.LittleEndian.PutUint32(buf, v)
}

// ReadUint32 reads a uint32 in little-endian format.
func ReadUint32(buf []byte) uint32 {
	return binary.LittleEndian.Uint32(buf)
}

// WriteInt32 writes an int32 in little-endian format.
func WriteInt32(buf []byte, v int32) {
	binary.LittleEndian.PutUint32(buf, uint32(v))
}

// ReadInt32 reads an int32 in little-endian format.
func ReadInt32(buf []byte) int32 {
	return int32(binary.LittleEndian.Uint32(buf))
}

// WriteUint16 writes a uint16 in little-endian format.
func WriteUint16(buf []byte, v uint16) {
	binary.LittleEndian.PutUint16(buf, v)
}

// ReadUint16 reads a uint16 in little-endian format.
func ReadUint16(buf []byte) uint16 {
	return binary.LittleEndian.Uint16(buf)
}

// WriteFloat64 writes a float64 in little-endian format.
func WriteFloat64(buf []byte, f float64) {
	binary.LittleEndian.PutUint64(buf, math.Float64bits(f))
}

// ReadFloat64 reads a float64 in little-endian format.
func ReadFloat64(buf []byte) float64 {
	return math.Float64frombits(binary.LittleEndian.Uint64(buf))
}

// ---------------------------------------------------------------------------
// Variable-length integer encoding (for compact representation)
// ---------------------------------------------------------------------------

// WriteVarInt writes a variable-length unsigned integer.
// Uses 7 bits per byte with high bit as continuation flag.
// Returns the number of bytes written.
func WriteVarInt(buf []byte, v uint64) int {
	i := 0
	for v >= 0x80 {
		buf[i] = byte(v) | 0x80
		v >>= 7
		i++
	}
	buf[i] = byte(v)
	return i + 1
}

// ReadVarInt reads a variable-length unsigned integer.
// Returns the value and number of bytes consumed.
func ReadVarInt(buf []byte) (uint64, int) {
	var v uint64
	var shift uint
	for i := 0; i < len(buf); i++ {
		b := buf[i]
		v |= uint64(b&0x7F) << shift
		if b < 0x80 {
			return v, i + 1
		}
		shift += 7
	}
	return v, len(buf)
}

// WriteSignedVarInt writes a variable-length signed integer.
// Uses zigzag encoding to keep small magnitudes small.
func WriteSignedVarInt(buf []byte, v int64) int {
	// Zigzag encode: (v << 1) ^ (v >> 63)
	uv := uint64((v << 1) ^ (v >> 63))
	return WriteVarInt(buf, uv)
}

// ReadSignedVarInt reads a variable-length signed integer.
func ReadSignedVarInt(buf []byte) (int64, int) {
	uv, n := ReadVarInt(buf)
	// Zigzag decode: (uv >> 1) ^ -(uv & 1)
	v := int64((uv >> 1) ^ -(uv & 1))
	return v, n
}

// ---------------------------------------------------------------------------
// String encoding helpers
// ---------------------------------------------------------------------------

// WriteString writes a length-prefixed string.
// Returns the number of bytes written.
func WriteString(buf []byte, s string) int {
	n := WriteVarInt(buf, uint64(len(s)))
	copy(buf[n:], s)
	return n + len(s)
}

// ReadString reads a length-prefixed string.
// Returns the string and number of bytes consumed.
func ReadString(buf []byte) (string, int) {
	length, n := ReadVarInt(buf)
	end := n + int(length)
	if end > len(buf) {
		return "", n
	}
	s := string(buf[n:end])
	return s, end
}

// StringEncodedSize returns the number of bytes needed to encode a string.
func StringEncodedSize(s string) int {
	// VarInt size for length + string bytes
	length := uint64(len(s))
	varIntSize := 1
	for length >= 0x80 {
		varIntSize++
		length >>= 7
	}
	return varIntSize + len(s)
}
