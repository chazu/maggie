package vm

import (
	"math"
	"math/big"
	"testing"

	"github.com/fxamacker/cbor/v2"
)

// Regression tests for int64 values outside the 48-bit SmallInteger range.
// FromSmallInt panics out of range, which killed the VM (an uncaught Go panic,
// not a catchable Maggie error) for large parsed/decoded/computed integers.

// assertBigInt fails unless v is a BigInteger equal to want.
func assertBigInt(t *testing.T, v *VM, got Value, want *big.Int) {
	t.Helper()
	bi := v.registry.GetBigInt(got)
	if bi == nil {
		t.Fatalf("expected BigInteger %s, got non-BigInteger value", want)
	}
	if bi.Value.Cmp(want) != 0 {
		t.Fatalf("expected %s, got %s", want, bi.Value)
	}
}

// expectPrimitiveError runs fn and fails unless it signals a PrimitiveError.
func expectPrimitiveError(t *testing.T, v *VM, fn func()) {
	t.Helper()
	defer func() {
		t.Helper()
		r := recover()
		sig, ok := r.(SignaledException)
		if !ok {
			t.Fatalf("expected SignaledException, got %T: %v", r, r)
		}
		if sig.Object == nil || sig.Object.ExceptionClass != v.PrimitiveErrorClass {
			t.Fatalf("expected PrimitiveError, got %+v", sig.Object)
		}
	}()
	fn()
}

func TestNewIntegerValue(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	if got := v.registry.NewIntegerValue(MaxSmallInt); !got.IsSmallInt() || got.SmallInt() != MaxSmallInt {
		t.Fatalf("MaxSmallInt should stay a SmallInteger")
	}
	assertBigInt(t, v, v.registry.NewIntegerValue(MaxSmallInt+1), big.NewInt(MaxSmallInt+1))
	assertBigInt(t, v, v.registry.NewIntegerValue(math.MinInt64), big.NewInt(math.MinInt64))
}

func TestSmallIntDivisionOverflowPromotes(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	want := new(big.Int).Neg(big.NewInt(MinSmallInt)) // 2^47
	for _, sel := range []string{"/", "//"} {
		got := v.Send(FromSmallInt(MinSmallInt), sel, []Value{FromSmallInt(-1)})
		assertBigInt(t, v, got, want)
	}

	// Bytecode fast path for /.
	interp := v.newInterpreter()
	assertBigInt(t, v, interp.primitiveDiv(FromSmallInt(MinSmallInt), FromSmallInt(-1)), want)
}

func TestFloatToIntegerConversions(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	big20, _ := new(big.Int).SetString("100000000000000000000", 10)
	for _, sel := range []string{"truncated", "rounded", "floor", "ceiling"} {
		assertBigInt(t, v, v.Send(FromFloat64(1e20), sel, nil), big20)
		assertBigInt(t, v, v.Send(FromFloat64(-1e20), sel, nil), new(big.Int).Neg(big20))

		for _, f := range []float64{math.NaN(), math.Inf(1), math.Inf(-1)} {
			expectPrimitiveError(t, v, func() { v.Send(FromFloat64(f), sel, nil) })
		}
	}

	cases := []struct {
		sel  string
		recv float64
		want int64
	}{
		{"rounded", 3.5, 4},
		{"rounded", 3.4, 3},
		{"rounded", -3.5, -4},
		{"rounded", 0.49999999999999994, 0}, // f+0.5 answered 1
		{"truncated", -3.9, -3},
		{"floor", -3.2, -4},
		{"ceiling", 3.2, 4},
	}
	for _, c := range cases {
		got := v.Send(FromFloat64(c.recv), c.sel, nil)
		if !got.IsSmallInt() || got.SmallInt() != c.want {
			t.Errorf("%v %s: want %d", c.recv, c.sel, c.want)
		}
	}
}

func TestStringAsIntegerLarge(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	s := "123456789012345678901234567890"
	want, _ := new(big.Int).SetString(s, 10)
	assertBigInt(t, v, v.Send(v.registry.NewStringValue(s), "asInteger", nil), want)

	if got := v.Send(v.registry.NewStringValue("-42"), "asInteger", nil); !got.IsSmallInt() || got.SmallInt() != -42 {
		t.Errorf("'-42' asInteger: want -42")
	}
	if got := v.Send(v.registry.NewStringValue("12abc"), "asInteger", nil); got != Nil {
		t.Errorf("'12abc' asInteger: want nil")
	}
}

func TestJsonDecodeLargeIntegers(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	jsonClass := v.classValue(v.Classes.Lookup("Json"))
	decode := func(s string) Value {
		return v.Send(jsonClass, "primDecode:", []Value{v.registry.NewStringValue(s)})
	}

	assertBigInt(t, v, decode("9007199254740993"), big.NewInt(9007199254740993))
	beyondInt64, _ := new(big.Int).SetString("123456789012345678901234567890", 10)
	assertBigInt(t, v, decode("123456789012345678901234567890"), beyondInt64)
}

func TestImageDecodeLargeIntegers(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	for _, n := range []int64{MaxSmallInt + 1, math.MinInt64} {
		raw, err := cbor.Marshal(n)
		if err != nil {
			t.Fatal(err)
		}
		got, err := decodeImageValue(v, nil, raw)
		if err != nil {
			t.Fatal(err)
		}
		assertBigInt(t, v, got, big.NewInt(n))
	}
	raw, _ := cbor.Marshal(uint64(math.MaxUint64))
	got, err := decodeImageValue(v, nil, raw)
	if err != nil {
		t.Fatal(err)
	}
	assertBigInt(t, v, got, new(big.Int).SetUint64(math.MaxUint64))
}

func TestSerialExceptionRejectsNonExceptionClass(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	// A peer names a non-exception class; it must not become the exception's
	// class — fall back to Error.
	data, err := cborSerialEncMode.Marshal(cbor.Tag{
		Number:  cborTagException,
		Content: &serializedException{ClassName: "Object"},
	})
	if err != nil {
		t.Fatal(err)
	}
	got, err := v.DeserializeValue(data)
	if err != nil {
		t.Fatal(err)
	}
	exObj := v.registry.GetExceptionFromValue(got)
	if exObj == nil || exObj.ExceptionClass != v.ErrorClass {
		t.Fatalf("expected Error fallback, got %+v", exObj)
	}
}

func TestValueToGoBigInt(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	if got := v.ValueToGo(v.registry.NewIntegerValue(math.MaxInt64)); got != int64(math.MaxInt64) {
		t.Fatalf("BigInteger within int64: got %#v", got)
	}
	huge := new(big.Int).Lsh(big.NewInt(1), 70)
	got, ok := v.ValueToGo(v.registry.NewBigIntValue(huge)).(*big.Int)
	if !ok || got.Cmp(huge) != 0 {
		t.Fatalf("BigInteger beyond int64: got %#v", got)
	}
	got.SetInt64(0) // must be a copy, not the VM's own big.Int
	if v.registry.GetBigInt(v.registry.NewBigIntValue(huge)).Value.Sign() == 0 {
		t.Fatal("ValueToGo aliased the BigInteger")
	}
}

func TestGoIntArgs(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	big50 := v.registry.NewIntegerValue(1 << 50)
	if got := v.GoIntArg(big50, 64); got != 1<<50 {
		t.Fatalf("GoIntArg(2^50, 64) = %d", got)
	}
	if got := v.GoIntArg(FromSmallInt(-128), 8); got != -128 {
		t.Fatalf("GoIntArg(-128, 8) = %d", got)
	}
	maxU64 := v.registry.NewBigIntValue(new(big.Int).SetUint64(math.MaxUint64))
	if got := v.GoUintArg(maxU64, 64); got != math.MaxUint64 {
		t.Fatalf("GoUintArg(MaxUint64, 64) = %d", got)
	}
	if got := v.GoUintArg(FromSmallInt(255), 8); got != 255 {
		t.Fatalf("GoUintArg(255, 8) = %d", got)
	}

	for _, bad := range []func(){
		func() { v.GoIntArg(FromSmallInt(128), 8) },
		func() { v.GoIntArg(big50, 32) },
		func() { v.GoIntArg(maxU64, 64) },
		func() { v.GoUintArg(FromSmallInt(-1), 64) },
		func() { v.GoUintArg(FromSmallInt(256), 8) },
		func() { v.GoIntArg(v.registry.NewStringValue("3"), 0) },
		func() { v.GoIntArg(FromFloat64(1.5), 0) },
	} {
		expectPrimitiveError(t, v, bad)
	}
}
