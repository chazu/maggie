package vm

import "testing"

// Character value: must reject code points outside Unicode (and surrogates):
// out-of-range values used to be OR'd into the marker bits and decode as
// some other immediate (16r3000041 answered an HttpRequest, -1 a Symbol).
func TestCharacterValueRangeChecked(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()
	cls := v.MustGlobal("Character")

	for _, n := range []int64{65, 0, 0x10FFFF, 0xE000} {
		c := v.Send(cls, "value:", []Value{FromSmallInt(n)})
		if !IsCharacterValue(c) || int64(GetCharacterCodePoint(c)) != n {
			t.Errorf("Character value: %#x = %v, want that Character", n, c)
		}
	}

	bad := []Value{
		FromSmallInt(-1),
		FromSmallInt(0x3000041),
		FromSmallInt(0x110000),
		FromSmallInt(0xD800),
		FromSmallInt(0xDFFF),
		Nil,
		FromFloat64(65.0),
	}
	for _, arg := range bad {
		if _, signaled := signalsPrimitiveError(v, func() {
			v.Send(cls, "value:", []Value{arg})
		}); !signaled {
			t.Errorf("Character value: %v should signal", arg)
		}
	}
}
