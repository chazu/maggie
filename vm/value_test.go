package vm

import (
	"math"
	"testing"
	"unsafe"
)

// ---------------------------------------------------------------------------
// Float tests
// ---------------------------------------------------------------------------

func TestFloatRoundTrip(t *testing.T) {
	tests := []float64{
		0.0,
		-0.0,
		1.0,
		-1.0,
		3.14159265358979,
		-3.14159265358979,
		math.MaxFloat64,
		math.SmallestNonzeroFloat64,
		-math.MaxFloat64,
		-math.SmallestNonzeroFloat64,
		math.Inf(1),
		math.Inf(-1),
	}

	for _, f := range tests {
		v := FromFloat64(f)
		if !v.IsFloat() {
			t.Errorf("FromFloat64(%v).IsFloat() = false, want true", f)
			continue
		}
		got := v.Float64()
		if got != f && !(math.IsNaN(got) && math.IsNaN(f)) {
			t.Errorf("FromFloat64(%v).Float64() = %v, want %v", f, got, f)
		}
	}
}

func TestFloatNaN(t *testing.T) {
	// Real NaN should be treated as a float
	v := FromFloat64(math.NaN())
	if !v.IsFloat() {
		t.Error("NaN should be treated as float")
	}
	if !math.IsNaN(v.Float64()) {
		t.Error("NaN roundtrip failed")
	}
}

func TestFloatTypeChecks(t *testing.T) {
	v := FromFloat64(42.5)
	if !v.IsFloat() {
		t.Error("IsFloat should be true")
	}
	if v.IsSmallInt() {
		t.Error("IsSmallInt should be false for float")
	}
	if v.IsObject() {
		t.Error("IsObject should be false for float")
	}
	if v.IsSymbol() {
		t.Error("IsSymbol should be false for float")
	}
	if v.IsNil() {
		t.Error("IsNil should be false for float")
	}
	if v.IsBool() {
		t.Error("IsBool should be false for float")
	}
}

// ---------------------------------------------------------------------------
// SmallInt tests
// ---------------------------------------------------------------------------

func TestSmallIntRoundTrip(t *testing.T) {
	tests := []int64{
		0,
		1,
		-1,
		42,
		-42,
		1000000,
		-1000000,
		MaxSmallInt,
		MinSmallInt,
		MaxSmallInt - 1,
		MinSmallInt + 1,
	}

	for _, n := range tests {
		v := FromSmallInt(n)
		if !v.IsSmallInt() {
			t.Errorf("FromSmallInt(%d).IsSmallInt() = false, want true", n)
			continue
		}
		got := v.SmallInt()
		if got != n {
			t.Errorf("FromSmallInt(%d).SmallInt() = %d, want %d", n, got, n)
		}
	}
}

func TestSmallIntSignExtension(t *testing.T) {
	// Test that negative numbers are correctly sign-extended
	tests := []int64{-1, -2, -100, -1000000, MinSmallInt}
	for _, n := range tests {
		v := FromSmallInt(n)
		got := v.SmallInt()
		if got != n {
			t.Errorf("Sign extension failed for %d: got %d", n, got)
		}
		if got >= 0 {
			t.Errorf("Sign extension should produce negative for %d: got %d", n, got)
		}
	}
}

func TestSmallIntOverflow(t *testing.T) {
	// Test that out-of-range values panic
	defer func() {
		if r := recover(); r == nil {
			t.Error("FromSmallInt(MaxSmallInt+1) should panic")
		}
	}()
	FromSmallInt(MaxSmallInt + 1)
}

func TestSmallIntUnderflow(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("FromSmallInt(MinSmallInt-1) should panic")
		}
	}()
	FromSmallInt(MinSmallInt - 1)
}

func TestTryFromSmallInt(t *testing.T) {
	// Valid values
	v, ok := TryFromSmallInt(42)
	if !ok || v.SmallInt() != 42 {
		t.Error("TryFromSmallInt(42) should succeed")
	}

	// Out of range
	_, ok = TryFromSmallInt(MaxSmallInt + 1)
	if ok {
		t.Error("TryFromSmallInt(MaxSmallInt+1) should return false")
	}

	_, ok = TryFromSmallInt(MinSmallInt - 1)
	if ok {
		t.Error("TryFromSmallInt(MinSmallInt-1) should return false")
	}
}

func TestSmallIntTypeChecks(t *testing.T) {
	v := FromSmallInt(42)
	if v.IsFloat() {
		t.Error("IsFloat should be false for SmallInt")
	}
	if !v.IsSmallInt() {
		t.Error("IsSmallInt should be true")
	}
	if v.IsObject() {
		t.Error("IsObject should be false for SmallInt")
	}
	if v.IsSymbol() {
		t.Error("IsSymbol should be false for SmallInt")
	}
	if v.IsNil() {
		t.Error("IsNil should be false for SmallInt")
	}
}

// ---------------------------------------------------------------------------
// Object pointer tests
// ---------------------------------------------------------------------------

func TestObjectPointerRoundTrip(t *testing.T) {
	// Create some test objects on the heap
	type testObj struct {
		x int
		y string
	}

	obj1 := &testObj{x: 42, y: "hello"}
	obj2 := &testObj{x: 100, y: "world"}

	v1 := FromObjectPtr(unsafe.Pointer(obj1))
	v2 := FromObjectPtr(unsafe.Pointer(obj2))

	if !v1.IsObject() {
		t.Error("v1.IsObject() should be true")
	}
	if !v2.IsObject() {
		t.Error("v2.IsObject() should be true")
	}

	// Verify we get the same pointer back
	ptr1 := v1.ObjectPtr()
	ptr2 := v2.ObjectPtr()

	got1 := (*testObj)(ptr1)
	got2 := (*testObj)(ptr2)

	if got1.x != 42 || got1.y != "hello" {
		t.Errorf("Object 1 roundtrip failed: got {%d, %s}", got1.x, got1.y)
	}
	if got2.x != 100 || got2.y != "world" {
		t.Errorf("Object 2 roundtrip failed: got {%d, %s}", got2.x, got2.y)
	}
}

func TestObjectTypeChecks(t *testing.T) {
	obj := &struct{ x int }{x: 1}
	v := FromObjectPtr(unsafe.Pointer(obj))

	if v.IsFloat() {
		t.Error("IsFloat should be false for object")
	}
	if v.IsSmallInt() {
		t.Error("IsSmallInt should be false for object")
	}
	if !v.IsObject() {
		t.Error("IsObject should be true")
	}
	if v.IsSymbol() {
		t.Error("IsSymbol should be false for object")
	}
	if v.IsNil() {
		t.Error("IsNil should be false for object")
	}
}

// ---------------------------------------------------------------------------
// Symbol tests
// ---------------------------------------------------------------------------

func TestSymbolRoundTrip(t *testing.T) {
	// FromSymbolID/SymbolID round-trips ANY 32-bit ID through the symbol
	// NaN-box tag — this is the broad "symbol-encoded" property used for
	// strings, dictionaries, marker-tagged kinds, etc.
	tests := []uint32{0, 1, 100, 1000000, 0xFFFFFFFF}

	for _, id := range tests {
		v := FromSymbolID(id)
		if !v.IsSymbolEncoded() {
			t.Errorf("FromSymbolID(%d).IsSymbolEncoded() = false, want true", id)
			continue
		}
		got := v.SymbolID()
		if got != id {
			t.Errorf("FromSymbolID(%d).SymbolID() = %d, want %d", id, got, id)
		}
	}

	// Strict IsSymbol() must distinguish real symbols (marker byte 0,
	// id < stringIDOffset) from other symbol-encoded kinds.
	realSymbol := FromSymbolID(42)
	if !realSymbol.IsSymbol() {
		t.Errorf("FromSymbolID(42).IsSymbol() = false, want true (real symbol)")
	}
	notRealSymbol := FromSymbolID(0xFFFFFFFF)
	if notRealSymbol.IsSymbol() {
		t.Errorf("FromSymbolID(0xFFFFFFFF).IsSymbol() = true, want false (marker byte 0xFF)")
	}
	stringTagged := FromSymbolID(0x80000000)
	if stringTagged.IsSymbol() {
		t.Errorf("FromSymbolID(0x80000000).IsSymbol() = true, want false (string ID range)")
	}
}

func TestSymbolTypeChecks(t *testing.T) {
	v := FromSymbolID(42)
	if v.IsFloat() {
		t.Error("IsFloat should be false for symbol")
	}
	if v.IsSmallInt() {
		t.Error("IsSmallInt should be false for symbol")
	}
	if v.IsObject() {
		t.Error("IsObject should be false for symbol")
	}
	if !v.IsSymbol() {
		t.Error("IsSymbol should be true")
	}
	if v.IsNil() {
		t.Error("IsNil should be false for symbol")
	}
}

// TestIsSymbolStrictVsEncoded documents the contract between IsSymbol()
// (strict — actual interned Symbol) and IsSymbolEncoded() (broad — anything
// using the symbol NaN-box tag, including strings, dictionaries, marker-
// tagged kinds like channels/processes/mutexes, etc.).
func TestIsSymbolStrictVsEncoded(t *testing.T) {
	cases := []struct {
		name        string
		id          uint32
		wantStrict  bool
		wantEncoded bool
	}{
		{"real symbol (id=0)", 0, true, true},
		{"real symbol (id=42)", 42, true, true},
		{"real symbol (max marker-byte-0 id)", 0x00FFFFFF, true, true},
		{"channel marker", 1 << 24, false, true},
		{"process marker", 2 << 24, false, true},
		{"mutex marker", 32 << 24, false, true},
		{"character marker", 37 << 24, false, true},
		{"goObject marker", 41 << 24, false, true},
		{"remoteRef marker", 43 << 24, false, true},
		{"string range start", 0x80000000, false, true},
		{"string range mid", 0xA0000000, false, true},
		{"dictionary range start", 0xC0000000, false, true},
		{"dictionary range high", 0xFFFFFFFF, false, true},
	}

	for _, tc := range cases {
		v := FromSymbolID(tc.id)
		if got := v.IsSymbol(); got != tc.wantStrict {
			t.Errorf("%s: IsSymbol() = %v, want %v", tc.name, got, tc.wantStrict)
		}
		if got := v.IsSymbolEncoded(); got != tc.wantEncoded {
			t.Errorf("%s: IsSymbolEncoded() = %v, want %v", tc.name, got, tc.wantEncoded)
		}
	}

	// Non-symbol-encoded values must report false for both.
	other := []struct {
		name string
		v    Value
	}{
		{"nil", Nil},
		{"true", True},
		{"false", False},
		{"smallint", FromSmallInt(7)},
		{"float", FromFloat64(1.5)},
	}
	for _, tc := range other {
		if tc.v.IsSymbol() {
			t.Errorf("%s: IsSymbol() = true, want false", tc.name)
		}
		if tc.v.IsSymbolEncoded() {
			t.Errorf("%s: IsSymbolEncoded() = true, want false", tc.name)
		}
	}
}

// ---------------------------------------------------------------------------
// Special value tests
// ---------------------------------------------------------------------------

func TestNil(t *testing.T) {
	if !Nil.IsNil() {
		t.Error("Nil.IsNil() should be true")
	}
	if !Nil.IsSpecial() {
		t.Error("Nil.IsSpecial() should be true")
	}
	if Nil.IsFloat() {
		t.Error("Nil.IsFloat() should be false")
	}
	if Nil.IsSmallInt() {
		t.Error("Nil.IsSmallInt() should be false")
	}
	if Nil.IsBool() {
		t.Error("Nil.IsBool() should be false")
	}
}

func TestTrue(t *testing.T) {
	if !True.IsTrue() {
		t.Error("True.IsTrue() should be true")
	}
	if !True.IsBool() {
		t.Error("True.IsBool() should be true")
	}
	if !True.IsSpecial() {
		t.Error("True.IsSpecial() should be true")
	}
	if True.IsNil() {
		t.Error("True.IsNil() should be false")
	}
	if True.IsFalse() {
		t.Error("True.IsFalse() should be false")
	}

	if True.Bool() != true {
		t.Error("True.Bool() should be true")
	}
}

func TestFalse(t *testing.T) {
	if !False.IsFalse() {
		t.Error("False.IsFalse() should be true")
	}
	if !False.IsBool() {
		t.Error("False.IsBool() should be true")
	}
	if !False.IsSpecial() {
		t.Error("False.IsSpecial() should be true")
	}
	if False.IsNil() {
		t.Error("False.IsNil() should be false")
	}
	if False.IsTrue() {
		t.Error("False.IsTrue() should be false")
	}

	if False.Bool() != false {
		t.Error("False.Bool() should be false")
	}
}

func TestFromBool(t *testing.T) {
	if FromBool(true) != True {
		t.Error("FromBool(true) should equal True")
	}
	if FromBool(false) != False {
		t.Error("FromBool(false) should equal False")
	}
}

// ---------------------------------------------------------------------------
// Truthiness tests
// ---------------------------------------------------------------------------

func TestTruthiness(t *testing.T) {
	// False and nil are falsy
	if Nil.IsTruthy() {
		t.Error("Nil should be falsy")
	}
	if False.IsTruthy() {
		t.Error("False should be falsy")
	}
	if !Nil.IsFalsy() {
		t.Error("Nil.IsFalsy() should be true")
	}
	if !False.IsFalsy() {
		t.Error("False.IsFalsy() should be true")
	}

	// Everything else is truthy
	truthy := []Value{
		True,
		FromSmallInt(0),
		FromSmallInt(1),
		FromSmallInt(-1),
		FromFloat64(0.0),
		FromFloat64(1.0),
		FromSymbolID(0),
	}

	for i, v := range truthy {
		if !v.IsTruthy() {
			t.Errorf("truthy[%d] should be truthy", i)
		}
		if v.IsFalsy() {
			t.Errorf("truthy[%d] should not be falsy", i)
		}
	}
}

// ---------------------------------------------------------------------------
// Panic tests for type mismatches
// ---------------------------------------------------------------------------

func TestFloat64PanicOnNonFloat(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("Float64() on SmallInt should panic")
		}
	}()
	FromSmallInt(42).Float64()
}

func TestSmallIntPanicOnNonInt(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("SmallInt() on Float should panic")
		}
	}()
	FromFloat64(42.0).SmallInt()
}

func TestObjectPtrPanicOnNonObject(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("ObjectPtr() on SmallInt should panic")
		}
	}()
	FromSmallInt(42).ObjectPtr()
}

func TestSymbolIDPanicOnNonSymbol(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("SymbolID() on SmallInt should panic")
		}
	}()
	FromSmallInt(42).SymbolID()
}

func TestBoolPanicOnNonBool(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("Bool() on SmallInt should panic")
		}
	}()
	FromSmallInt(42).Bool()
}

// ---------------------------------------------------------------------------
// Edge case tests
// ---------------------------------------------------------------------------

func TestDistinctTypes(t *testing.T) {
	// Ensure different types with same "payload" are distinct
	v1 := FromSmallInt(42)
	v2 := FromSymbolID(42)
	v3 := FromFloat64(42.0)

	if v1 == v2 {
		t.Error("SmallInt(42) should not equal Symbol(42)")
	}
	if v1 == v3 {
		t.Error("SmallInt(42) should not equal Float(42.0)")
	}
	if v2 == v3 {
		t.Error("Symbol(42) should not equal Float(42.0)")
	}
}

func TestZeroValues(t *testing.T) {
	// Zero should work for all appropriate types
	intZero := FromSmallInt(0)
	floatZero := FromFloat64(0.0)
	symZero := FromSymbolID(0)

	if intZero.SmallInt() != 0 {
		t.Error("SmallInt(0) roundtrip failed")
	}
	if floatZero.Float64() != 0.0 {
		t.Error("Float(0.0) roundtrip failed")
	}
	if symZero.SymbolID() != 0 {
		t.Error("Symbol(0) roundtrip failed")
	}

	// They should all be distinct
	if intZero == floatZero {
		t.Error("SmallInt(0) should not equal Float(0.0)")
	}
	if intZero == symZero {
		t.Error("SmallInt(0) should not equal Symbol(0)")
	}
}

func TestValueSize(t *testing.T) {
	// Post pointer-migration: Value is 16 bytes (uint64 hi + unsafe.Pointer),
	// so heap objects are traced by the Go GC. The copy-cost of the extra word
	// was measured as negligible (see docs/spikes/2026-07-04-value-representation.md).
	if size := unsafe.Sizeof(Value{}); size != 16 {
		t.Errorf("Value size = %d, want 16", size)
	}
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

func BenchmarkIsFloat(b *testing.B) {
	v := FromFloat64(3.14)
	for i := 0; i < b.N; i++ {
		_ = v.IsFloat()
	}
}

func BenchmarkIsSmallInt(b *testing.B) {
	v := FromSmallInt(42)
	for i := 0; i < b.N; i++ {
		_ = v.IsSmallInt()
	}
}

func BenchmarkFloat64Roundtrip(b *testing.B) {
	for i := 0; i < b.N; i++ {
		v := FromFloat64(3.14159)
		_ = v.Float64()
	}
}

func BenchmarkSmallIntRoundtrip(b *testing.B) {
	for i := 0; i < b.N; i++ {
		v := FromSmallInt(42)
		_ = v.SmallInt()
	}
}

func BenchmarkIsTruthy(b *testing.B) {
	v := FromSmallInt(42)
	for i := 0; i < b.N; i++ {
		_ = v.IsTruthy()
	}
}

// ---------------------------------------------------------------------------
// Block tests
// ---------------------------------------------------------------------------

func TestBlockRoundTrip(t *testing.T) {
	bv := &BlockValue{Block: &BlockMethod{Arity: 0}}
	v := makeBlockValue(bv)
	if !v.IsBlock() {
		t.Fatal("makeBlockValue(...).IsBlock() = false, want true")
	}
	if v.blockPtr() != bv {
		t.Error("blockPtr() should resolve to the original *BlockValue")
	}
}

func TestBlockTypeChecks(t *testing.T) {
	v := makeBlockValue(&BlockValue{Block: &BlockMethod{Arity: 0}})
	if v.IsFloat() {
		t.Error("IsFloat should be false for block")
	}
	if v.IsSmallInt() {
		t.Error("IsSmallInt should be false for block")
	}
	if v.IsObject() {
		t.Error("IsObject should be false for block")
	}
	if v.IsSymbol() {
		t.Error("IsSymbol should be false for block")
	}
	if !v.IsBlock() {
		t.Error("IsBlock should be true")
	}
	if v.IsNil() {
		t.Error("IsNil should be false for block")
	}
}

func TestBlockDistinctFromSymbol(t *testing.T) {
	// A pointer-carrying block value is never a symbol (an immediate).
	blockVal := makeBlockValue(&BlockValue{Block: &BlockMethod{Arity: 0}})
	symbolVal := FromSymbolID(123)

	if blockVal == symbolVal {
		t.Error("Block and symbol should not be equal")
	}
	if !blockVal.IsBlock() {
		t.Error("Block value should be a block")
	}
	if blockVal.IsSymbol() {
		t.Error("Block value should not be a symbol")
	}
	if !symbolVal.IsSymbol() {
		t.Error("Symbol value should be a symbol")
	}
	if symbolVal.IsBlock() {
		t.Error("Symbol value should not be a block")
	}
}

func BenchmarkBlockRoundtrip(b *testing.B) {
	bv := &BlockValue{Block: &BlockMethod{Arity: 0}}
	for i := 0; i < b.N; i++ {
		v := makeBlockValue(bv)
		_ = v.blockPtr()
	}
}
