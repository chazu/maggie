package vm

import (
	"math/big"
	"testing"
)

// ---------------------------------------------------------------------------
// BigInteger Tests
// ---------------------------------------------------------------------------

func TestBigIntOverflowOnPlus(t *testing.T) {
	vm := NewVM()
	// MaxSmallInt + 1 should produce a BigInt, not panic
	a := FromSmallInt(MaxSmallInt)
	b := FromSmallInt(1)
	result := vm.interpreter.primitivePlus(a, b)
	if !IsBigIntValue(result) {
		t.Fatalf("expected BigInt from MaxSmallInt + 1, got SmallInt=%v, IsFloat=%v", result.IsSmallInt(), result.IsFloat())
	}
	obj := vm.registry.GetBigInt(result)
	if obj == nil {
		t.Fatal("expected non-nil BigIntObject")
	}
	expected := new(big.Int).Add(big.NewInt(MaxSmallInt), big.NewInt(1))
	if obj.Value.Cmp(expected) != 0 {
		t.Fatalf("expected %s, got %s", expected.String(), obj.Value.String())
	}
}

func TestBigIntOverflowOnMinus(t *testing.T) {
	vm := NewVM()
	a := FromSmallInt(MinSmallInt)
	b := FromSmallInt(1)
	result := vm.interpreter.primitiveMinus(a, b)
	if !IsBigIntValue(result) {
		t.Fatalf("expected BigInt from MinSmallInt - 1, got SmallInt=%v, IsFloat=%v", result.IsSmallInt(), result.IsFloat())
	}
	obj := vm.registry.GetBigInt(result)
	expected := new(big.Int).Sub(big.NewInt(MinSmallInt), big.NewInt(1))
	if obj.Value.Cmp(expected) != 0 {
		t.Fatalf("expected %s, got %s", expected.String(), obj.Value.String())
	}
}

func TestBigIntOverflowOnTimes(t *testing.T) {
	vm := NewVM()
	a := FromSmallInt(MaxSmallInt)
	b := FromSmallInt(2)
	result := vm.interpreter.primitiveTimes(a, b)
	if !IsBigIntValue(result) {
		t.Fatalf("expected BigInt from MaxSmallInt * 2, got SmallInt=%v", result.IsSmallInt())
	}
	obj := vm.registry.GetBigInt(result)
	expected := new(big.Int).Mul(big.NewInt(MaxSmallInt), big.NewInt(2))
	if obj.Value.Cmp(expected) != 0 {
		t.Fatalf("expected %s, got %s", expected.String(), obj.Value.String())
	}
}

func TestBigIntDemotion(t *testing.T) {
	vm := NewVM()
	// A BigInt that fits in SmallInt range should demote back
	n := big.NewInt(42)
	result := vm.registry.NewBigIntValue(n)
	if !result.IsSmallInt() {
		t.Fatalf("expected SmallInt for value 42, got BigInt")
	}
	if result.SmallInt() != 42 {
		t.Fatalf("expected 42, got %d", result.SmallInt())
	}
}

func TestBigIntDemotionAtBoundary(t *testing.T) {
	vm := NewVM()
	// MaxSmallInt should demote
	result := vm.registry.NewBigIntValue(big.NewInt(MaxSmallInt))
	if !result.IsSmallInt() {
		t.Fatal("expected SmallInt for MaxSmallInt value")
	}
	// MaxSmallInt+1 should stay BigInt
	result = vm.registry.NewBigIntValue(new(big.Int).Add(big.NewInt(MaxSmallInt), big.NewInt(1)))
	if !IsBigIntValue(result) {
		t.Fatal("expected BigInt for MaxSmallInt+1")
	}
}

func TestBigIntArithmeticAdd(t *testing.T) {
	vm := NewVM()
	a := vm.registry.RegisterBigInt(&BigIntObject{Value: big.NewInt(1000000000000000)})
	b := vm.registry.RegisterBigInt(&BigIntObject{Value: big.NewInt(2000000000000000)})

	// Use the BigInteger + primitive
	method := vm.BigIntegerClass.LookupMethod(vm.Selectors, "+")
	if method == nil {
		t.Fatal("BigInteger + method not found")
	}
	result := method.Invoke(vm, a, []Value{b})
	if !IsBigIntValue(result) {
		t.Fatal("expected BigInt result")
	}
	obj := vm.registry.GetBigInt(result)
	expected := big.NewInt(3000000000000000)
	if obj.Value.Cmp(expected) != 0 {
		t.Fatalf("expected %s, got %s", expected.String(), obj.Value.String())
	}
}

func TestBigIntArithmeticSub(t *testing.T) {
	vm := NewVM()
	large := new(big.Int).Add(big.NewInt(MaxSmallInt), big.NewInt(100))
	a := vm.registry.RegisterBigInt(&BigIntObject{Value: new(big.Int).Set(large)})
	b := vm.registry.RegisterBigInt(&BigIntObject{Value: big.NewInt(50)})

	method := vm.BigIntegerClass.LookupMethod(vm.Selectors, "-")
	result := method.Invoke(vm, a, []Value{b})
	if !IsBigIntValue(result) {
		t.Fatal("expected BigInt result")
	}
	obj := vm.registry.GetBigInt(result)
	expected := new(big.Int).Sub(large, big.NewInt(50))
	if obj.Value.Cmp(expected) != 0 {
		t.Fatalf("expected %s, got %s", expected.String(), obj.Value.String())
	}
}

func TestBigIntArithmeticMul(t *testing.T) {
	vm := NewVM()
	large := new(big.Int).Add(big.NewInt(MaxSmallInt), big.NewInt(1))
	a := vm.registry.RegisterBigInt(&BigIntObject{Value: new(big.Int).Set(large)})
	b := vm.registry.RegisterBigInt(&BigIntObject{Value: big.NewInt(3)})

	method := vm.BigIntegerClass.LookupMethod(vm.Selectors, "*")
	result := method.Invoke(vm, a, []Value{b})
	if !IsBigIntValue(result) {
		t.Fatal("expected BigInt result")
	}
	obj := vm.registry.GetBigInt(result)
	expected := new(big.Int).Mul(large, big.NewInt(3))
	if obj.Value.Cmp(expected) != 0 {
		t.Fatalf("expected %s, got %s", expected.String(), obj.Value.String())
	}
}

func TestBigIntArithmeticDiv(t *testing.T) {
	vm := NewVM()
	large := new(big.Int).Mul(big.NewInt(MaxSmallInt), big.NewInt(4))
	a := vm.registry.RegisterBigInt(&BigIntObject{Value: new(big.Int).Set(large)})
	b := vm.registry.RegisterBigInt(&BigIntObject{Value: big.NewInt(2)})

	method := vm.BigIntegerClass.LookupMethod(vm.Selectors, "/")
	result := method.Invoke(vm, a, []Value{b})
	if !IsBigIntValue(result) {
		t.Fatal("expected BigInt result")
	}
	obj := vm.registry.GetBigInt(result)
	expected := new(big.Int).Quo(large, big.NewInt(2))
	if obj.Value.Cmp(expected) != 0 {
		t.Fatalf("expected %s, got %s", expected.String(), obj.Value.String())
	}
}

func TestBigIntArithmeticMod(t *testing.T) {
	vm := NewVM()
	large := new(big.Int).Add(big.NewInt(MaxSmallInt), big.NewInt(7))
	a := vm.registry.RegisterBigInt(&BigIntObject{Value: new(big.Int).Set(large)})
	b := vm.registry.RegisterBigInt(&BigIntObject{Value: big.NewInt(5)})

	method := vm.BigIntegerClass.LookupMethod(vm.Selectors, "\\\\")
	result := method.Invoke(vm, a, []Value{b})
	// Result should demote to SmallInt since mod by 5 is 0-4
	if !result.IsSmallInt() {
		t.Fatal("expected SmallInt result from mod")
	}
	expected := new(big.Int).Rem(large, big.NewInt(5))
	if result.SmallInt() != expected.Int64() {
		t.Fatalf("expected %s, got %d", expected.String(), result.SmallInt())
	}
}

func TestBigIntDivisionByZero(t *testing.T) {
	vm := NewVM()
	a := vm.registry.RegisterBigInt(&BigIntObject{Value: big.NewInt(100)})
	b := vm.registry.RegisterBigInt(&BigIntObject{Value: big.NewInt(0)})

	method := vm.BigIntegerClass.LookupMethod(vm.Selectors, "/")
	result := method.Invoke(vm, a, []Value{b})
	if result != Nil {
		t.Fatal("expected Nil for division by zero")
	}
}

func TestBigIntComparisons(t *testing.T) {
	vm := NewVM()
	big1 := new(big.Int).Add(big.NewInt(MaxSmallInt), big.NewInt(1))
	big2 := new(big.Int).Add(big.NewInt(MaxSmallInt), big.NewInt(2))
	a := vm.registry.RegisterBigInt(&BigIntObject{Value: new(big.Int).Set(big1)})
	b := vm.registry.RegisterBigInt(&BigIntObject{Value: new(big.Int).Set(big2)})
	c := vm.registry.RegisterBigInt(&BigIntObject{Value: new(big.Int).Set(big1)})

	tests := []struct {
		sel    string
		lhs    Value
		rhs    Value
		expect Value
	}{
		{"<", a, b, True},
		{"<", b, a, False},
		{">", b, a, True},
		{">", a, b, False},
		{"<=", a, b, True},
		{"<=", a, c, True},
		{">=", b, a, True},
		{">=", a, c, True},
		{"=", a, c, True},
		{"=", a, b, False},
	}

	for _, tt := range tests {
		method := vm.BigIntegerClass.LookupMethod(vm.Selectors, tt.sel)
		if method == nil {
			t.Fatalf("method %s not found", tt.sel)
		}
		result := method.Invoke(vm, tt.lhs, []Value{tt.rhs})
		if result != tt.expect {
			t.Fatalf("%s: expected %v, got %v", tt.sel, tt.expect, result)
		}
	}
}

func TestBigIntMixedSmallIntBigIntAdd(t *testing.T) {
	vm := NewVM()
	// SmallInt + BigInt
	small := FromSmallInt(100)
	large := new(big.Int).Add(big.NewInt(MaxSmallInt), big.NewInt(1))
	bigVal := vm.registry.RegisterBigInt(&BigIntObject{Value: new(big.Int).Set(large)})

	method := vm.SmallIntegerClass.LookupMethod(vm.Selectors, "+")
	result := method.Invoke(vm, small, []Value{bigVal})
	if !IsBigIntValue(result) {
		t.Fatal("expected BigInt result from SmallInt + BigInt")
	}
	obj := vm.registry.GetBigInt(result)
	expected := new(big.Int).Add(big.NewInt(100), large)
	if obj.Value.Cmp(expected) != 0 {
		t.Fatalf("expected %s, got %s", expected.String(), obj.Value.String())
	}
}

func TestBigIntMixedBigIntSmallIntAdd(t *testing.T) {
	vm := NewVM()
	// BigInt + SmallInt
	large := new(big.Int).Add(big.NewInt(MaxSmallInt), big.NewInt(1))
	bigVal := vm.registry.RegisterBigInt(&BigIntObject{Value: new(big.Int).Set(large)})
	small := FromSmallInt(100)

	method := vm.BigIntegerClass.LookupMethod(vm.Selectors, "+")
	result := method.Invoke(vm, bigVal, []Value{small})
	if !IsBigIntValue(result) {
		t.Fatal("expected BigInt result from BigInt + SmallInt")
	}
	obj := vm.registry.GetBigInt(result)
	expected := new(big.Int).Add(large, big.NewInt(100))
	if obj.Value.Cmp(expected) != 0 {
		t.Fatalf("expected %s, got %s", expected.String(), obj.Value.String())
	}
}

func TestBigIntClassFor(t *testing.T) {
	vm := NewVM()
	large := new(big.Int).Add(big.NewInt(MaxSmallInt), big.NewInt(1))
	bigVal := vm.registry.RegisterBigInt(&BigIntObject{Value: large})

	cls := vm.ClassFor(bigVal)
	if cls != vm.BigIntegerClass {
		t.Fatalf("expected BigIntegerClass, got %v", cls)
	}
}

func TestBigIntNegated(t *testing.T) {
	vm := NewVM()
	// Use MaxSmallInt+2 so its negation (-MaxSmallInt-2) is outside SmallInt range
	large := new(big.Int).Add(big.NewInt(MaxSmallInt), big.NewInt(2))
	bigVal := vm.registry.RegisterBigInt(&BigIntObject{Value: new(big.Int).Set(large)})

	method := vm.BigIntegerClass.LookupMethod(vm.Selectors, "negated")
	result := method.Invoke(vm, bigVal, nil)
	if !IsBigIntValue(result) {
		t.Fatal("expected BigInt result from negated")
	}
	obj := vm.registry.GetBigInt(result)
	expected := new(big.Int).Neg(large)
	if obj.Value.Cmp(expected) != 0 {
		t.Fatalf("expected %s, got %s", expected.String(), obj.Value.String())
	}
}

func TestBigIntPrintString(t *testing.T) {
	vm := NewVM()
	n := new(big.Int).Add(big.NewInt(MaxSmallInt), big.NewInt(1))
	bigVal := vm.registry.RegisterBigInt(&BigIntObject{Value: n})

	method := vm.BigIntegerClass.LookupMethod(vm.Selectors, "primPrintString")
	result := method.Invoke(vm, bigVal, nil)
	str := vm.registry.GetStringContent(result)
	expected := n.String()
	if str != expected {
		t.Fatalf("expected %q, got %q", expected, str)
	}
}

func TestSmallIntTimesNoDemotionPanic(t *testing.T) {
	// Ensure small * small that fits still returns SmallInt (no unnecessary BigInt)
	vm := NewVM()
	a := FromSmallInt(7)
	b := FromSmallInt(6)
	result := vm.interpreter.primitiveTimes(a, b)
	if !result.IsSmallInt() {
		t.Fatal("expected SmallInt for 7*6")
	}
	if result.SmallInt() != 42 {
		t.Fatalf("expected 42, got %d", result.SmallInt())
	}
}

func TestBigIntSubDemotesToSmallInt(t *testing.T) {
	// BigInt - BigInt that results in a value in SmallInt range
	vm := NewVM()
	a := new(big.Int).Add(big.NewInt(MaxSmallInt), big.NewInt(10))
	b := new(big.Int).Add(big.NewInt(MaxSmallInt), big.NewInt(5))
	aVal := vm.registry.RegisterBigInt(&BigIntObject{Value: new(big.Int).Set(a)})
	bVal := vm.registry.RegisterBigInt(&BigIntObject{Value: new(big.Int).Set(b)})

	method := vm.BigIntegerClass.LookupMethod(vm.Selectors, "-")
	result := method.Invoke(vm, aVal, []Value{bVal})
	// a - b = 5, which fits in SmallInt
	if !result.IsSmallInt() {
		t.Fatal("expected SmallInt from BigInt - BigInt that fits")
	}
	if result.SmallInt() != 5 {
		t.Fatalf("expected 5, got %d", result.SmallInt())
	}
}

func TestBigIntMixedComparison(t *testing.T) {
	// SmallInt compared with BigInt
	vm := NewVM()
	small := FromSmallInt(100)
	large := new(big.Int).Add(big.NewInt(MaxSmallInt), big.NewInt(1))
	bigVal := vm.registry.RegisterBigInt(&BigIntObject{Value: large})

	// SmallInt < BigInt
	method := vm.SmallIntegerClass.LookupMethod(vm.Selectors, "<")
	result := method.Invoke(vm, small, []Value{bigVal})
	if result != True {
		t.Fatal("expected 100 < (MaxSmallInt+1) = true")
	}

	// SmallInt > BigInt
	method = vm.SmallIntegerClass.LookupMethod(vm.Selectors, ">")
	result = method.Invoke(vm, small, []Value{bigVal})
	if result != False {
		t.Fatal("expected 100 > (MaxSmallInt+1) = false")
	}
}

func TestBigIntRegistryCount(t *testing.T) {
	vm := NewVM()
	if vm.registry.BigIntCount() != 0 {
		t.Fatal("expected 0 BigInts initially")
	}
	vm.registry.RegisterBigInt(&BigIntObject{Value: big.NewInt(999)})
	if vm.registry.BigIntCount() != 1 {
		t.Fatal("expected 1 BigInt after registration")
	}
}

func TestIsBigIntValue(t *testing.T) {
	vm := NewVM()
	bigVal := vm.registry.RegisterBigInt(&BigIntObject{Value: big.NewInt(999)})
	if !IsBigIntValue(bigVal) {
		t.Fatal("expected IsBigIntValue true")
	}
	if IsBigIntValue(FromSmallInt(42)) {
		t.Fatal("SmallInt should not be BigInt")
	}
	if IsBigIntValue(Nil) {
		t.Fatal("Nil should not be BigInt")
	}
}
