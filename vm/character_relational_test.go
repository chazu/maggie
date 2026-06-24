package vm

import "testing"

// TestCharacterRelationalComparison covers <= and >= on Character. Before they
// were added, sending them returned nil (doesNotUnderstand), so an expression
// like `(c >= $0) and: [c <= $9]` threw "Message not understood: and:" — the
// pp-serve housekeep crash. They must return real Booleans.
func TestCharacterRelationalComparison(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	send := func(recv Value, sel string, arg Value) Value {
		return vm.Send(recv, sel, []Value{arg})
	}

	a := FromCharacter('a')  // 97
	a2 := FromCharacter('a') // 97
	b := FromCharacter('b')  // 98

	cases := []struct {
		name string
		got  Value
		want Value
	}{
		{"a <= b", send(a, "<=", b), True},
		{"a <= a", send(a, "<=", a2), True},
		{"b <= a", send(b, "<=", a), False},
		{"b >= a", send(b, ">=", a), True},
		{"a >= a", send(a, ">=", a2), True},
		{"a >= b", send(a, ">=", b), False},
	}
	for _, c := range cases {
		if c.got != c.want {
			t.Errorf("%s = %v, want %v", c.name, c.got, c.want)
		}
	}

	// The result must be a Boolean, never nil — the exact failure the housekeep
	// crash exhibited (`(digit >= '0') and: [...]` with `>=` returning nil).
	r := send(FromCharacter('0'), ">=", FromCharacter('0'))
	if r != True {
		t.Fatalf("'0' >= '0' must be True (got %v); Character>= must return a Boolean, not nil", r)
	}
}
