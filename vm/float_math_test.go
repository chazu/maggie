package vm

import (
	"math"
	"testing"
)

func TestFloatTrig(t *testing.T) {
	vm := NewVM()

	tests := []struct {
		sel  string
		recv float64
		want float64
	}{
		{"sin", 0, 0},
		{"sin", math.Pi / 2, 1},
		{"cos", 0, 1},
		{"cos", math.Pi, -1},
		{"tan", 0, 0},
		{"asin", 0, 0},
		{"asin", 1, math.Pi / 2},
		{"acos", 1, 0},
		{"atan", 0, 0},
		{"atan", 1, math.Pi / 4},
	}

	for _, tt := range tests {
		result := vm.Send(FromFloat64(tt.recv), tt.sel, nil)
		if !result.IsFloat() {
			t.Errorf("%g %s: not a float", tt.recv, tt.sel)
			continue
		}
		if math.Abs(result.Float64()-tt.want) > 1e-10 {
			t.Errorf("%g %s = %g, want %g", tt.recv, tt.sel, result.Float64(), tt.want)
		}
	}
}

func TestFloatAtan2(t *testing.T) {
	vm := NewVM()

	result := vm.Send(FromFloat64(1.0), "atan2:", []Value{FromFloat64(1.0)})
	if math.Abs(result.Float64()-math.Pi/4) > 1e-10 {
		t.Errorf("1.0 atan2: 1.0 = %g, want %g", result.Float64(), math.Pi/4)
	}

	// With SmallInt arg
	result = vm.Send(FromFloat64(0.0), "atan2:", []Value{FromSmallInt(1)})
	if result.Float64() != 0 {
		t.Errorf("0.0 atan2: 1 = %g, want 0", result.Float64())
	}
}

func TestFloatLogExp(t *testing.T) {
	vm := NewVM()

	// ln
	result := vm.Send(FromFloat64(1.0), "ln", nil)
	if result.Float64() != 0 {
		t.Errorf("1.0 ln = %g, want 0", result.Float64())
	}
	result = vm.Send(FromFloat64(math.E), "ln", nil)
	if math.Abs(result.Float64()-1.0) > 1e-10 {
		t.Errorf("e ln = %g, want 1", result.Float64())
	}

	// log10
	result = vm.Send(FromFloat64(100.0), "log10", nil)
	if result.Float64() != 2 {
		t.Errorf("100.0 log10 = %g, want 2", result.Float64())
	}

	// log: (arbitrary base)
	result = vm.Send(FromFloat64(8.0), "log:", []Value{FromFloat64(2.0)})
	if math.Abs(result.Float64()-3.0) > 1e-10 {
		t.Errorf("8.0 log: 2.0 = %g, want 3", result.Float64())
	}

	// log: with SmallInt base
	result = vm.Send(FromFloat64(1000.0), "log:", []Value{FromSmallInt(10)})
	if math.Abs(result.Float64()-3.0) > 1e-10 {
		t.Errorf("1000.0 log: 10 = %g, want 3", result.Float64())
	}

	// exp
	result = vm.Send(FromFloat64(0.0), "exp", nil)
	if result.Float64() != 1 {
		t.Errorf("0.0 exp = %g, want 1", result.Float64())
	}
	result = vm.Send(FromFloat64(1.0), "exp", nil)
	if math.Abs(result.Float64()-math.E) > 1e-10 {
		t.Errorf("1.0 exp = %g, want e", result.Float64())
	}
}

func TestFloatPow(t *testing.T) {
	vm := NewVM()

	result := vm.Send(FromFloat64(2.0), "pow:", []Value{FromFloat64(10.0)})
	if result.Float64() != 1024 {
		t.Errorf("2.0 pow: 10.0 = %g, want 1024", result.Float64())
	}

	// With SmallInt arg
	result = vm.Send(FromFloat64(3.0), "pow:", []Value{FromSmallInt(3)})
	if result.Float64() != 27 {
		t.Errorf("3.0 pow: 3 = %g, want 27", result.Float64())
	}

	// Fractional exponent (sqrt)
	result = vm.Send(FromFloat64(9.0), "pow:", []Value{FromFloat64(0.5)})
	if result.Float64() != 3 {
		t.Errorf("9.0 pow: 0.5 = %g, want 3", result.Float64())
	}
}

func TestFloatClassConstants(t *testing.T) {
	vm := NewVM()

	floatSym := FromSymbolID(vm.Intern("Float"))

	// Float pi
	result := vm.Send(floatSym, "pi", nil)
	if !result.IsFloat() || result.Float64() != math.Pi {
		t.Errorf("Float pi = %v, want %g", result, math.Pi)
	}

	// Float e
	result = vm.Send(floatSym, "e", nil)
	if !result.IsFloat() || result.Float64() != math.E {
		t.Errorf("Float e = %v, want %g", result, math.E)
	}

	// Float infinity
	result = vm.Send(floatSym, "infinity", nil)
	if !result.IsFloat() || !math.IsInf(result.Float64(), 1) {
		t.Errorf("Float infinity = %v, want +Inf", result)
	}

	// Float nan
	result = vm.Send(floatSym, "nan", nil)
	if !result.IsFloat() || !math.IsNaN(result.Float64()) {
		t.Errorf("Float nan = %v, want NaN", result)
	}
}

func TestFloatLessEqualGreaterEqual(t *testing.T) {
	vm := NewVM()
	mk := FromFloat64
	cases := []struct {
		recv, arg Value
		sel       string
		want      Value
	}{
		{mk(2.5), mk(2.5), "<=", True},
		{mk(2.5), mk(3.0), "<=", True},
		{mk(3.0), mk(2.5), "<=", False},
		{mk(2.5), mk(2.5), ">=", True},
		{mk(3.0), mk(2.5), ">=", True},
		{mk(2.5), mk(3.0), ">=", False},
		{mk(5.0), FromSmallInt(5), "<=", True}, // Float vs SmallInt
		{mk(5.0), FromSmallInt(6), ">=", False},
	}
	for _, tc := range cases {
		got := vm.Send(tc.recv, tc.sel, []Value{tc.arg})
		if got != tc.want {
			t.Errorf("%v %s %v = %v, want %v", tc.recv.Float64(), tc.sel, tc.arg, got, tc.want)
		}
		if got == Nil {
			t.Errorf("Float %s returned nil (regression)", tc.sel)
		}
	}
}

// TestSmallIntFloatComparison guards the regression where SmallInteger </>/<=/>=
// raised "argument must be a number" for a Float argument (e.g. 7 < 2.5 errored)
// instead of coercing to a numeric comparison — inconsistent with SmallInteger
// arithmetic (7 + 2.5 → 9.5) and with Float<SmallInt.
func TestSmallIntFloatComparison(t *testing.T) {
	vm := NewVM()
	cases := []struct {
		recv int64
		sel  string
		arg  float64
		want Value
	}{
		{7, "<", 2.5, False},
		{7, ">", 2.5, True},
		{7, "<=", 7.0, True},
		{3, ">=", 2.5, True},
		{2, "<=", 2.0, True},
		{2, "<", 2.0, False},
	}
	for _, tc := range cases {
		got := vm.Send(FromSmallInt(tc.recv), tc.sel, []Value{FromFloat64(tc.arg)})
		if got != tc.want {
			t.Errorf("%d %s %v = %v, want %v", tc.recv, tc.sel, tc.arg, got, tc.want)
		}
		if got == Nil {
			t.Errorf("%d %s %v returned nil", tc.recv, tc.sel, tc.arg)
		}
	}
}
