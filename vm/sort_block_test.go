package vm

import (
	"math/big"
	"testing"
)

func TestSortBlockOrder(t *testing.T) {
	vm := NewVM()
	neg, _ := new(big.Int).SetString("-100000000000000000000", 10)
	for _, c := range []struct {
		in   Value
		want int
	}{
		{FromSmallInt(-3), -1}, {FromSmallInt(0), 0}, {FromSmallInt(9), 1},
		{FromFloat64(-0.5), -1}, {FromFloat64(0.25), 1}, {FromFloat64(0), 0},
		{vm.registry.NewBigIntValue(neg), -1},
		{True, -1}, {False, 1},
	} {
		if got := vm.sortBlockOrder(c.in); got != c.want {
			t.Errorf("sortBlockOrder(%v) = %d, want %d", c.in, got, c.want)
		}
	}

	defer func() {
		if _, ok := recover().(SignaledException); !ok {
			t.Error("a nil sort-block answer must signal")
		}
	}()
	vm.sortBlockOrder(Nil)
}
