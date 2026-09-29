package vm

import (
	"math/big"
	"testing"

	"github.com/chazu/goquint"
)

// decode64: answers values up to 2^64-1; FromSmallInt panicked above 2^47.
func TestProquintDecode64Large(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()
	pq := v.classValue(v.Classes.Lookup("Proquint"))

	for _, n := range []uint64{1 << 50, 1<<63 + 5, ^uint64(0)} {
		s := v.registry.NewStringValue(goquint.Encode64(n))
		got := bigIntOf(t, v, v.Send(pq, "decode64:", []Value{s}))
		if want := new(big.Int).SetUint64(n); got.Cmp(want) != 0 {
			t.Errorf("decode64: %q = %s, want %s", goquint.Encode64(n), got, want)
		}
		// And encode64: accepts the BigInteger back (round trip).
		enc := v.Send(pq, "encode64:", []Value{v.registry.NewBigIntValue(new(big.Int).SetUint64(n))})
		if s := v.registry.GetStringContent(enc); s != goquint.Encode64(n) {
			t.Errorf("encode64: %d = %q, want %q", n, s, goquint.Encode64(n))
		}
	}
}
