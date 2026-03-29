package vm

import "testing"

func TestProquintEncodeDecode(t *testing.T) {
	vm := NewVM()

	// Encode 0 → "babab-babab" (two quintuplets for 32-bit)
	pqClass := vm.classValue(vm.Classes.Lookup("Proquint"))
	result := vm.Send(pqClass, "encode:", []Value{FromSmallInt(0)})
	s := vm.registry.GetStringContent(result)
	if s != "babab-babab" {
		t.Errorf("encode: 0 → %q, want 'babab-babab'", s)
	}

	// Decode "babab-babab" → 0
	result = vm.Send(pqClass, "decode:", []Value{vm.registry.NewStringValue("babab-babab")})
	if !result.IsSmallInt() || result.SmallInt() != 0 {
		t.Errorf("decode: 'babab-babab' → %v, want 0", result)
	}
}

func TestProquintRoundtrip(t *testing.T) {
	vm := NewVM()
	pqClass := vm.classValue(vm.Classes.Lookup("Proquint"))

	for _, n := range []int64{0, 1, 255, 65535, 1000000, 0xFFFFFFFF} {
		encoded := vm.Send(pqClass, "encode:", []Value{FromSmallInt(n)})
		decoded := vm.Send(pqClass, "decode:", []Value{encoded})
		if !decoded.IsSmallInt() || decoded.SmallInt() != n {
			t.Errorf("roundtrip(%d): got %v", n, decoded)
		}
	}
}

func TestProquintRandom(t *testing.T) {
	vm := NewVM()
	pqClass := vm.classValue(vm.Classes.Lookup("Proquint"))

	r1 := vm.Send(pqClass, "random", nil)
	r2 := vm.Send(pqClass, "random", nil)

	s1 := vm.registry.GetStringContent(r1)
	s2 := vm.registry.GetStringContent(r2)

	if s1 == "" || s2 == "" {
		t.Error("random should return non-empty strings")
	}
	// Not strictly guaranteed to differ, but astronomically unlikely for 32-bit random
	if s1 == s2 {
		t.Logf("Warning: two random proquints were identical: %s", s1)
	}
}

func TestProquint64(t *testing.T) {
	vm := NewVM()
	pqClass := vm.classValue(vm.Classes.Lookup("Proquint"))

	result := vm.Send(pqClass, "encode64:", []Value{FromSmallInt(0)})
	s := vm.registry.GetStringContent(result)
	if s == "" {
		t.Error("encode64: 0 should return non-empty string")
	}

	decoded := vm.Send(pqClass, "decode64:", []Value{result})
	if !decoded.IsSmallInt() || decoded.SmallInt() != 0 {
		t.Errorf("decode64 roundtrip: got %v, want 0", decoded)
	}
}

func TestProquintEncodeHex(t *testing.T) {
	vm := NewVM()
	pqClass := vm.classValue(vm.Classes.Lookup("Proquint"))

	hexVal := vm.registry.NewStringValue("7f000001") // 127.0.0.1
	result := vm.Send(pqClass, "encodeHex:", []Value{hexVal})
	s := vm.registry.GetStringContent(result)
	if s == "" {
		t.Error("encodeHex: should return non-empty string")
	}

	// Decode it back via decode: (it's a 32-bit value)
	decoded := vm.Send(pqClass, "decode:", []Value{result})
	if !decoded.IsSmallInt() || decoded.SmallInt() != 0x7f000001 {
		t.Errorf("encodeHex roundtrip: got %v, want %d", decoded, 0x7f000001)
	}
}
