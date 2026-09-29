package vm

import (
	"math/big"
	"testing"
)

func bigIntOf(t *testing.T, v *VM, val Value) *big.Int {
	t.Helper()
	if val.IsSmallInt() {
		return big.NewInt(val.SmallInt())
	}
	if b := v.registry.GetBigInt(val); b != nil {
		return b.Value
	}
	t.Fatalf("expected an Integer, got %v", val)
	return nil
}

// BigInteger // must floor like SmallInteger //. math/big's Div is
// Euclidean, which differs from floor when the divisor is negative.
func TestBigIntFloorDivision(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()
	large := new(big.Int).Add(big.NewInt(MaxSmallInt), big.NewInt(2)) // 2^47+1
	neg := new(big.Int).Neg(large)

	cases := []struct{ a, b, want *big.Int }{
		// 2^47+1 // -2 = floor(-(2^46) - 0.5) = -(2^46) - 1
		{large, big.NewInt(-2), new(big.Int).Sub(new(big.Int).Neg(new(big.Int).Lsh(big.NewInt(1), 46)), big.NewInt(1))},
		// -(2^47+1) // 2 = -(2^46) - 1
		{neg, big.NewInt(2), new(big.Int).Sub(new(big.Int).Neg(new(big.Int).Lsh(big.NewInt(1), 46)), big.NewInt(1))},
		// -(2^47+1) // -2 = 2^46 (floor of 2^46 + 0.5)
		{neg, big.NewInt(-2), new(big.Int).Lsh(big.NewInt(1), 46)},
		// 2^47+1 // 2 = 2^46
		{large, big.NewInt(2), new(big.Int).Lsh(big.NewInt(1), 46)},
	}
	for _, c := range cases {
		a := v.registry.RegisterBigInt(&BigIntObject{Value: new(big.Int).Set(c.a)})
		got := bigIntOf(t, v, v.Send(a, "//", []Value{FromSmallInt(c.b.Int64())}))
		if got.Cmp(c.want) != 0 {
			t.Errorf("%s // %s = %s, want %s", c.a, c.b, got, c.want)
		}
	}
}

func TestBigIntFloorDivisionByZeroSignals(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()
	a := v.registry.RegisterBigInt(&BigIntObject{Value: new(big.Int).Lsh(big.NewInt(1), 60)})

	for _, sel := range []string{"//", "\\\\"} {
		msg, signaled := signalsPrimitiveError(v, func() {
			v.Send(a, sel, []Value{FromSmallInt(0)})
		})
		if !signaled {
			t.Errorf("BigInteger %s 0 should signal ZeroDivide, not answer nil", sel)
		} else if msg == "" {
			t.Errorf("BigInteger %s 0: empty signal message", sel)
		}
	}
}
