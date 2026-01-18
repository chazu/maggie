package vm

import (
	"math"
	"testing"
)

// ---------------------------------------------------------------------------
// ImageEncoder tests
// ---------------------------------------------------------------------------

func TestImageEncoderNewEmpty(t *testing.T) {
	enc := NewImageEncoder()
	if enc.ObjectCount() != 0 {
		t.Errorf("ObjectCount() = %d, want 0", enc.ObjectCount())
	}
	if enc.SymbolCount() != 0 {
		t.Errorf("SymbolCount() = %d, want 0", enc.SymbolCount())
	}
	if enc.StringCount() != 0 {
		t.Errorf("StringCount() = %d, want 0", enc.StringCount())
	}
	if enc.ClassCount() != 0 {
		t.Errorf("ClassCount() = %d, want 0", enc.ClassCount())
	}
	if enc.MethodCount() != 0 {
		t.Errorf("MethodCount() = %d, want 0", enc.MethodCount())
	}
}

func TestImageEncoderRegisterObject(t *testing.T) {
	enc := NewImageEncoder()

	// Register first object
	ptr1 := uintptr(0x1000)
	idx1 := enc.RegisterObject(ptr1)
	if idx1 != 0 {
		t.Errorf("First object index = %d, want 0", idx1)
	}

	// Register same object again - should return same index
	idx1b := enc.RegisterObject(ptr1)
	if idx1b != 0 {
		t.Errorf("Same object index = %d, want 0", idx1b)
	}

	// Register different object
	ptr2 := uintptr(0x2000)
	idx2 := enc.RegisterObject(ptr2)
	if idx2 != 1 {
		t.Errorf("Second object index = %d, want 1", idx2)
	}

	if enc.ObjectCount() != 2 {
		t.Errorf("ObjectCount() = %d, want 2", enc.ObjectCount())
	}

	// Lookup
	found, ok := enc.LookupObject(ptr1)
	if !ok || found != 0 {
		t.Errorf("LookupObject(ptr1) = (%d, %v), want (0, true)", found, ok)
	}

	_, ok = enc.LookupObject(uintptr(0x9999))
	if ok {
		t.Error("LookupObject(unknown) should return false")
	}
}

func TestImageEncoderRegisterSymbol(t *testing.T) {
	enc := NewImageEncoder()

	idx1 := enc.RegisterSymbol(42)
	if idx1 != 0 {
		t.Errorf("First symbol index = %d, want 0", idx1)
	}

	idx1b := enc.RegisterSymbol(42)
	if idx1b != 0 {
		t.Errorf("Same symbol index = %d, want 0", idx1b)
	}

	idx2 := enc.RegisterSymbol(100)
	if idx2 != 1 {
		t.Errorf("Second symbol index = %d, want 1", idx2)
	}

	if enc.SymbolCount() != 2 {
		t.Errorf("SymbolCount() = %d, want 2", enc.SymbolCount())
	}
}

func TestImageEncoderRegisterString(t *testing.T) {
	enc := NewImageEncoder()

	idx1 := enc.RegisterString("hello")
	if idx1 != 0 {
		t.Errorf("First string index = %d, want 0", idx1)
	}

	idx1b := enc.RegisterString("hello")
	if idx1b != 0 {
		t.Errorf("Same string index = %d, want 0", idx1b)
	}

	idx2 := enc.RegisterString("world")
	if idx2 != 1 {
		t.Errorf("Second string index = %d, want 1", idx2)
	}
}

func TestImageEncoderRegisterClass(t *testing.T) {
	enc := NewImageEncoder()

	c1 := NewClass("Test1", nil)
	c2 := NewClass("Test2", nil)

	idx1 := enc.RegisterClass(c1)
	if idx1 != 0 {
		t.Errorf("First class index = %d, want 0", idx1)
	}

	idx1b := enc.RegisterClass(c1)
	if idx1b != 0 {
		t.Errorf("Same class index = %d, want 0", idx1b)
	}

	idx2 := enc.RegisterClass(c2)
	if idx2 != 1 {
		t.Errorf("Second class index = %d, want 1", idx2)
	}
}

func TestImageEncoderRegisterMethod(t *testing.T) {
	enc := NewImageEncoder()

	m1 := NewCompiledMethod("test1", 0)
	m2 := NewCompiledMethod("test2", 1)

	idx1 := enc.RegisterMethod(m1)
	if idx1 != 0 {
		t.Errorf("First method index = %d, want 0", idx1)
	}

	idx2 := enc.RegisterMethod(m2)
	if idx2 != 1 {
		t.Errorf("Second method index = %d, want 1", idx2)
	}
}

// ---------------------------------------------------------------------------
// Encode/Decode roundtrip tests for basic values
// ---------------------------------------------------------------------------

func TestEncodeDecodeNil(t *testing.T) {
	enc := NewImageEncoder()
	dec := NewImageDecoder()

	data := enc.EncodeValue(Nil)
	if len(data) != EncodedValueSize {
		t.Errorf("Encoded size = %d, want %d", len(data), EncodedValueSize)
	}

	result := dec.DecodeValue(data)
	if result != Nil {
		t.Errorf("Decoded value = %v, want Nil", result)
	}
}

func TestEncodeDecodeTrue(t *testing.T) {
	enc := NewImageEncoder()
	dec := NewImageDecoder()

	data := enc.EncodeValue(True)
	result := dec.DecodeValue(data)
	if result != True {
		t.Errorf("Decoded value = %v, want True", result)
	}
}

func TestEncodeDecodeFalse(t *testing.T) {
	enc := NewImageEncoder()
	dec := NewImageDecoder()

	data := enc.EncodeValue(False)
	result := dec.DecodeValue(data)
	if result != False {
		t.Errorf("Decoded value = %v, want False", result)
	}
}

func TestEncodeDecodeSmallInt(t *testing.T) {
	tests := []int64{
		0, 1, -1, 42, -42,
		1000000, -1000000,
		MaxSmallInt, MinSmallInt,
		MaxSmallInt - 1, MinSmallInt + 1,
	}

	for _, n := range tests {
		enc := NewImageEncoder()
		dec := NewImageDecoder()

		v := FromSmallInt(n)
		data := enc.EncodeValue(v)
		result := dec.DecodeValue(data)

		if !result.IsSmallInt() {
			t.Errorf("For %d: result is not SmallInt", n)
			continue
		}
		if result.SmallInt() != n {
			t.Errorf("For %d: got %d", n, result.SmallInt())
		}
	}
}

func TestEncodeDecodeFloat(t *testing.T) {
	tests := []float64{
		0.0, -0.0, 1.0, -1.0,
		3.14159265358979,
		-3.14159265358979,
		math.MaxFloat64,
		math.SmallestNonzeroFloat64,
		-math.MaxFloat64,
		math.Inf(1),
		math.Inf(-1),
	}

	for _, f := range tests {
		enc := NewImageEncoder()
		dec := NewImageDecoder()

		v := FromFloat64(f)
		data := enc.EncodeValue(v)
		result := dec.DecodeValue(data)

		if !result.IsFloat() {
			t.Errorf("For %v: result is not Float", f)
			continue
		}

		got := result.Float64()
		// Handle -0.0 comparison
		if math.IsInf(f, 1) && !math.IsInf(got, 1) {
			t.Errorf("For +Inf: got %v", got)
		} else if math.IsInf(f, -1) && !math.IsInf(got, -1) {
			t.Errorf("For -Inf: got %v", got)
		} else if !math.IsInf(f, 0) && got != f {
			t.Errorf("For %v: got %v", f, got)
		}
	}
}

func TestEncodeDecodeFloatNaN(t *testing.T) {
	enc := NewImageEncoder()
	dec := NewImageDecoder()

	v := FromFloat64(math.NaN())
	data := enc.EncodeValue(v)
	result := dec.DecodeValue(data)

	if !result.IsFloat() {
		t.Error("NaN result is not Float")
	}
	if !math.IsNaN(result.Float64()) {
		t.Errorf("Expected NaN, got %v", result.Float64())
	}
}

func TestEncodeDecodeSymbol(t *testing.T) {
	enc := NewImageEncoder()
	dec := NewImageDecoder()

	symID := uint32(42)
	v := FromSymbolID(symID)

	data := enc.EncodeValue(v)

	// Set up decoder with the symbol mapping
	idx, _ := enc.LookupSymbol(symID)
	dec.SetSymbol(idx, symID)

	result := dec.DecodeValue(data)

	if !result.IsSymbol() {
		t.Error("Result is not Symbol")
	}
	if result.SymbolID() != symID {
		t.Errorf("Symbol ID = %d, want %d", result.SymbolID(), symID)
	}
}

func TestEncodeDecodeObject(t *testing.T) {
	enc := NewImageEncoder()
	dec := NewImageDecoder()

	// Create an object
	c := NewClass("Test", nil)
	obj := NewObject(c.VTable, 2)
	obj.SetSlot(0, FromSmallInt(42))
	obj.SetSlot(1, True)

	v := obj.ToValue()
	data := enc.EncodeValue(v)

	// Set up decoder with the object mapping
	ptr := uintptr(v.ObjectPtr())
	idx, _ := enc.LookupObject(ptr)
	dec.SetObject(idx, obj)

	result := dec.DecodeValue(data)

	if !result.IsObject() {
		t.Error("Result is not Object")
	}

	resObj := ObjectFromValue(result)
	if resObj != obj {
		t.Error("Object pointer doesn't match")
	}
	if resObj.GetSlot(0).SmallInt() != 42 {
		t.Error("Object slot 0 value incorrect")
	}
}

// ---------------------------------------------------------------------------
// ImageDecoder tests
// ---------------------------------------------------------------------------

func TestImageDecoderNewEmpty(t *testing.T) {
	dec := NewImageDecoder()
	if dec.ObjectCount() != 0 {
		t.Errorf("ObjectCount() = %d, want 0", dec.ObjectCount())
	}
}

func TestImageDecoderAddAndGet(t *testing.T) {
	dec := NewImageDecoder()

	// Add objects
	c := NewClass("Test", nil)
	obj1 := NewObject(c.VTable, 0)
	obj2 := NewObject(c.VTable, 0)

	idx1 := dec.AddObject(obj1)
	idx2 := dec.AddObject(obj2)

	if idx1 != 0 || idx2 != 1 {
		t.Errorf("AddObject indices = (%d, %d), want (0, 1)", idx1, idx2)
	}

	if dec.GetObject(0) != obj1 {
		t.Error("GetObject(0) doesn't match")
	}
	if dec.GetObject(1) != obj2 {
		t.Error("GetObject(1) doesn't match")
	}
	if dec.GetObject(99) != nil {
		t.Error("GetObject(99) should return nil")
	}
}

func TestImageDecoderSetObject(t *testing.T) {
	dec := NewImageDecoder()

	c := NewClass("Test", nil)
	obj := NewObject(c.VTable, 0)

	// Set at index 5 (should grow slice)
	dec.SetObject(5, obj)

	if dec.GetObject(5) != obj {
		t.Error("SetObject didn't work")
	}
	if dec.ObjectCount() != 6 {
		t.Errorf("ObjectCount() = %d, want 6", dec.ObjectCount())
	}

	// Indices 0-4 should be nil
	for i := uint32(0); i < 5; i++ {
		if dec.GetObject(i) != nil {
			t.Errorf("GetObject(%d) should be nil", i)
		}
	}
}

func TestImageDecoderSymbols(t *testing.T) {
	dec := NewImageDecoder()

	dec.AddSymbol(10)
	dec.AddSymbol(20)
	dec.SetSymbol(5, 50)

	if dec.GetSymbol(0) != 10 {
		t.Errorf("GetSymbol(0) = %d, want 10", dec.GetSymbol(0))
	}
	if dec.GetSymbol(1) != 20 {
		t.Errorf("GetSymbol(1) = %d, want 20", dec.GetSymbol(1))
	}
	if dec.GetSymbol(5) != 50 {
		t.Errorf("GetSymbol(5) = %d, want 50", dec.GetSymbol(5))
	}
}

func TestImageDecoderStrings(t *testing.T) {
	dec := NewImageDecoder()

	dec.AddString("hello")
	dec.AddString("world")

	if dec.GetString(0) != "hello" {
		t.Errorf("GetString(0) = %q, want %q", dec.GetString(0), "hello")
	}
	if dec.GetString(1) != "world" {
		t.Errorf("GetString(1) = %q, want %q", dec.GetString(1), "world")
	}
	if dec.GetString(99) != "" {
		t.Error("GetString(99) should return empty string")
	}
}

func TestImageDecoderClasses(t *testing.T) {
	dec := NewImageDecoder()

	c1 := NewClass("Test1", nil)
	c2 := NewClass("Test2", nil)

	dec.AddClass(c1)
	dec.AddClass(c2)

	if dec.GetClass(0) != c1 {
		t.Error("GetClass(0) doesn't match")
	}
	if dec.GetClass(1) != c2 {
		t.Error("GetClass(1) doesn't match")
	}
}

func TestImageDecoderMethods(t *testing.T) {
	dec := NewImageDecoder()

	m1 := NewCompiledMethod("test1", 0)
	m2 := NewCompiledMethod("test2", 1)

	dec.AddMethod(m1)
	dec.AddMethod(m2)

	if dec.GetMethod(0) != m1 {
		t.Error("GetMethod(0) doesn't match")
	}
	if dec.GetMethod(1) != m2 {
		t.Error("GetMethod(1) doesn't match")
	}
}

// ---------------------------------------------------------------------------
// Binary helper tests
// ---------------------------------------------------------------------------

func TestWriteReadUint64(t *testing.T) {
	tests := []uint64{0, 1, 0xFF, 0xFFFF, 0xFFFFFFFF, 0xFFFFFFFFFFFFFFFF}

	buf := make([]byte, 8)
	for _, v := range tests {
		WriteUint64(buf, v)
		got := ReadUint64(buf)
		if got != v {
			t.Errorf("Uint64 roundtrip: got %d, want %d", got, v)
		}
	}
}

func TestWriteReadInt64(t *testing.T) {
	tests := []int64{0, 1, -1, 42, -42, math.MaxInt64, math.MinInt64}

	buf := make([]byte, 8)
	for _, v := range tests {
		WriteInt64(buf, v)
		got := ReadInt64(buf)
		if got != v {
			t.Errorf("Int64 roundtrip: got %d, want %d", got, v)
		}
	}
}

func TestWriteReadUint32(t *testing.T) {
	tests := []uint32{0, 1, 0xFF, 0xFFFF, 0xFFFFFFFF}

	buf := make([]byte, 4)
	for _, v := range tests {
		WriteUint32(buf, v)
		got := ReadUint32(buf)
		if got != v {
			t.Errorf("Uint32 roundtrip: got %d, want %d", got, v)
		}
	}
}

func TestWriteReadInt32(t *testing.T) {
	tests := []int32{0, 1, -1, 42, -42, math.MaxInt32, math.MinInt32}

	buf := make([]byte, 4)
	for _, v := range tests {
		WriteInt32(buf, v)
		got := ReadInt32(buf)
		if got != v {
			t.Errorf("Int32 roundtrip: got %d, want %d", got, v)
		}
	}
}

func TestWriteReadUint16(t *testing.T) {
	tests := []uint16{0, 1, 0xFF, 0xFFFF}

	buf := make([]byte, 2)
	for _, v := range tests {
		WriteUint16(buf, v)
		got := ReadUint16(buf)
		if got != v {
			t.Errorf("Uint16 roundtrip: got %d, want %d", got, v)
		}
	}
}

func TestWriteReadFloat64(t *testing.T) {
	tests := []float64{0.0, 1.0, -1.0, 3.14159, math.MaxFloat64, math.Inf(1), math.Inf(-1)}

	buf := make([]byte, 8)
	for _, v := range tests {
		WriteFloat64(buf, v)
		got := ReadFloat64(buf)
		if got != v {
			t.Errorf("Float64 roundtrip: got %v, want %v", got, v)
		}
	}
}

func TestWriteReadFloat64NaN(t *testing.T) {
	buf := make([]byte, 8)
	WriteFloat64(buf, math.NaN())
	got := ReadFloat64(buf)
	if !math.IsNaN(got) {
		t.Errorf("Float64 NaN roundtrip failed: got %v", got)
	}
}

// ---------------------------------------------------------------------------
// Variable-length integer tests
// ---------------------------------------------------------------------------

func TestWriteReadVarInt(t *testing.T) {
	tests := []uint64{
		0, 1, 127,
		128, 255, 256,
		0x3FFF, 0x4000,
		0x1FFFFF, 0x200000,
		0xFFFFFFF, 0x10000000,
		0xFFFFFFFFFFFFFFFF,
	}

	buf := make([]byte, 10)
	for _, v := range tests {
		n := WriteVarInt(buf, v)
		got, m := ReadVarInt(buf)
		if got != v {
			t.Errorf("VarInt roundtrip: got %d, want %d", got, v)
		}
		if n != m {
			t.Errorf("VarInt length mismatch: wrote %d, read %d", n, m)
		}
	}
}

func TestVarIntSize(t *testing.T) {
	tests := []struct {
		value    uint64
		expected int
	}{
		{0, 1},
		{127, 1},
		{128, 2},
		{0x3FFF, 2},
		{0x4000, 3},
		{0x1FFFFF, 3},
		{0x200000, 4},
	}

	buf := make([]byte, 10)
	for _, tc := range tests {
		n := WriteVarInt(buf, tc.value)
		if n != tc.expected {
			t.Errorf("VarInt(%d) size = %d, want %d", tc.value, n, tc.expected)
		}
	}
}

func TestWriteReadSignedVarInt(t *testing.T) {
	tests := []int64{
		0, 1, -1,
		63, -64,
		64, -65,
		1000, -1000,
		1000000, -1000000,
		math.MaxInt64, math.MinInt64,
	}

	buf := make([]byte, 10)
	for _, v := range tests {
		n := WriteSignedVarInt(buf, v)
		got, m := ReadSignedVarInt(buf)
		if got != v {
			t.Errorf("SignedVarInt roundtrip: got %d, want %d", got, v)
		}
		if n != m {
			t.Errorf("SignedVarInt length mismatch: wrote %d, read %d", n, m)
		}
	}
}

// ---------------------------------------------------------------------------
// String encoding tests
// ---------------------------------------------------------------------------

func TestWriteReadString(t *testing.T) {
	tests := []string{
		"",
		"a",
		"hello",
		"hello world",
		"The quick brown fox jumps over the lazy dog",
		string(make([]byte, 1000)), // 1000 null bytes
	}

	buf := make([]byte, 2000)
	for _, s := range tests {
		n := WriteString(buf, s)
		got, m := ReadString(buf)
		if got != s {
			t.Errorf("String roundtrip failed for len=%d", len(s))
		}
		if n != m {
			t.Errorf("String length mismatch: wrote %d, read %d", n, m)
		}
	}
}

func TestStringEncodedSize(t *testing.T) {
	tests := []struct {
		s        string
		expected int
	}{
		{"", 1},          // 1 byte for length (0)
		{"a", 2},         // 1 byte for length + 1 byte string
		{"hello", 6},     // 1 byte for length + 5 bytes string
		{string(make([]byte, 127)), 128}, // 1 byte for length + 127 bytes
		{string(make([]byte, 128)), 130}, // 2 bytes for length + 128 bytes
	}

	for _, tc := range tests {
		got := StringEncodedSize(tc.s)
		if got != tc.expected {
			t.Errorf("StringEncodedSize(len=%d) = %d, want %d", len(tc.s), got, tc.expected)
		}
	}
}

// ---------------------------------------------------------------------------
// Edge case tests
// ---------------------------------------------------------------------------

func TestDecodeValueTooShort(t *testing.T) {
	dec := NewImageDecoder()

	// Data shorter than EncodedValueSize should return Nil
	result := dec.DecodeValue([]byte{0x00, 0x01})
	if result != Nil {
		t.Error("DecodeValue with short data should return Nil")
	}
}

func TestDecodeUnknownTag(t *testing.T) {
	dec := NewImageDecoder()

	// Unknown tag should return Nil
	data := make([]byte, EncodedValueSize)
	data[0] = 0xFF // Unknown tag
	result := dec.DecodeValue(data)
	if result != Nil {
		t.Error("DecodeValue with unknown tag should return Nil")
	}
}

func TestDecodeObjectNotFound(t *testing.T) {
	dec := NewImageDecoder()

	// Object index not registered should return Nil
	data := make([]byte, EncodedValueSize)
	data[0] = imageTagObject
	WriteUint32(data[1:], 999) // Non-existent index

	result := dec.DecodeValue(data)
	if result != Nil {
		t.Error("DecodeValue for non-existent object should return Nil")
	}
}

func TestEncodeValueTo(t *testing.T) {
	enc := NewImageEncoder()
	buf := make([]byte, EncodedValueSize)

	enc.EncodeValueTo(FromSmallInt(42), buf)

	dec := NewImageDecoder()
	result := dec.DecodeValue(buf)

	if !result.IsSmallInt() || result.SmallInt() != 42 {
		t.Errorf("EncodeValueTo/DecodeValue failed: got %v", result)
	}
}

// ---------------------------------------------------------------------------
// Full encode/decode cycle with multiple values
// ---------------------------------------------------------------------------

func TestFullEncodeDecode(t *testing.T) {
	enc := NewImageEncoder()

	// Create test values
	values := []Value{
		Nil,
		True,
		False,
		FromSmallInt(0),
		FromSmallInt(42),
		FromSmallInt(-42),
		FromSmallInt(MaxSmallInt),
		FromSmallInt(MinSmallInt),
		FromFloat64(0.0),
		FromFloat64(3.14159),
		FromFloat64(-273.15),
		FromSymbolID(0),
		FromSymbolID(100),
	}

	// Encode all values
	encoded := make([][]byte, len(values))
	for i, v := range values {
		encoded[i] = enc.EncodeValue(v)
	}

	// Create decoder with symbol mappings that match the encoder's assignments
	// The encoder assigns image indices sequentially: symbol 0 -> index 0, symbol 100 -> index 1
	dec := NewImageDecoder()
	// Map image index 0 to symbol ID 0, and image index 1 to symbol ID 100
	dec.AddSymbol(0)   // Image index 0 -> Symbol ID 0
	dec.AddSymbol(100) // Image index 1 -> Symbol ID 100

	// Decode and verify
	for i, data := range encoded {
		result := dec.DecodeValue(data)

		original := values[i]
		switch {
		case original == Nil:
			if result != Nil {
				t.Errorf("Value[%d]: expected Nil, got %v", i, result)
			}
		case original == True:
			if result != True {
				t.Errorf("Value[%d]: expected True, got %v", i, result)
			}
		case original == False:
			if result != False {
				t.Errorf("Value[%d]: expected False, got %v", i, result)
			}
		case original.IsSmallInt():
			if !result.IsSmallInt() || result.SmallInt() != original.SmallInt() {
				t.Errorf("Value[%d]: SmallInt mismatch, got %v", i, result)
			}
		case original.IsFloat():
			if !result.IsFloat() {
				t.Errorf("Value[%d]: expected Float, got %v", i, result)
			}
			// Float comparison with tolerance for representation
			if math.Abs(result.Float64()-original.Float64()) > 1e-15 {
				t.Errorf("Value[%d]: Float mismatch, want %v got %v", i, original.Float64(), result.Float64())
			}
		case original.IsSymbol():
			if !result.IsSymbol() || result.SymbolID() != original.SymbolID() {
				t.Errorf("Value[%d]: Symbol mismatch, got %v", i, result)
			}
		}
	}
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

func BenchmarkEncodeSmallInt(b *testing.B) {
	enc := NewImageEncoder()
	v := FromSmallInt(42)
	buf := make([]byte, EncodedValueSize)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		enc.EncodeValueTo(v, buf)
	}
}

func BenchmarkDecodeSmallInt(b *testing.B) {
	enc := NewImageEncoder()
	dec := NewImageDecoder()
	data := enc.EncodeValue(FromSmallInt(42))

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = dec.DecodeValue(data)
	}
}

func BenchmarkEncodeFloat(b *testing.B) {
	enc := NewImageEncoder()
	v := FromFloat64(3.14159)
	buf := make([]byte, EncodedValueSize)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		enc.EncodeValueTo(v, buf)
	}
}

func BenchmarkDecodeFloat(b *testing.B) {
	enc := NewImageEncoder()
	dec := NewImageDecoder()
	data := enc.EncodeValue(FromFloat64(3.14159))

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = dec.DecodeValue(data)
	}
}

func BenchmarkVarIntWrite(b *testing.B) {
	buf := make([]byte, 10)
	v := uint64(1000000)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		WriteVarInt(buf, v)
	}
}

func BenchmarkVarIntRead(b *testing.B) {
	buf := make([]byte, 10)
	WriteVarInt(buf, 1000000)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		ReadVarInt(buf)
	}
}

func BenchmarkStringEncode(b *testing.B) {
	buf := make([]byte, 100)
	s := "hello world"

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		WriteString(buf, s)
	}
}

func BenchmarkStringDecode(b *testing.B) {
	buf := make([]byte, 100)
	WriteString(buf, "hello world")

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		ReadString(buf)
	}
}
