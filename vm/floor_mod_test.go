package vm

import (
	"math/big"
	"testing"
)

// \\ is floored (sign of the divisor) and pairs with //; rem: truncates
// (sign of the receiver) and pairs with /. Checked through the primitive,
// the interpreter's OpSendMod fast path and BigInteger.
func TestFloorModPairsWithFloorDivision(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	for _, a := range []int64{-7, -6, -1, 0, 1, 6, 7} {
		for _, b := range []int64{-3, -2, 2, 3} {
			av, bv := FromSmallInt(a), []Value{FromSmallInt(b)}
			q := v.Send(av, "//", bv).SmallInt()
			m := v.Send(av, "\\\\", bv).SmallInt()
			if q*b+m != a || (m != 0 && (m < 0) != (b < 0)) {
				t.Errorf("%d // %d = %d, %d \\\\ %d = %d: not a floored pair", a, b, q, a, b, m)
			}
			if fast := v.interpreter.primitiveMod(av, bv[0]).SmallInt(); fast != m {
				t.Errorf("fast path %d \\\\ %d = %d, primitive = %d", a, b, fast, m)
			}
			tq := v.Send(av, "/", bv).SmallInt()
			r := v.Send(av, "rem:", bv).SmallInt()
			if tq*b+r != a {
				t.Errorf("%d / %d = %d, %d rem: %d = %d: not a truncated pair", a, b, tq, a, b, r)
			}

			big1 := new(big.Int).Lsh(big.NewInt(1), 60)
			bigA := new(big.Int).Add(new(big.Int).Mul(big1, big.NewInt(b)), big.NewInt(a)) // ≡ a (mod b)
			bv2 := v.registry.RegisterBigInt(&BigIntObject{Value: bigA})
			if got := bigIntOf(t, v, v.Send(bv2, "\\\\", bv)).Int64(); got != m {
				t.Errorf("BigInteger %s \\\\ %d = %d, want %d", bigA, b, got, m)
			}
		}
	}
}
