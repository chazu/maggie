package vm

import (
	"math/big"
	"testing"
	"time"
)

func dateTimeOf(t *testing.T, v *VM, val Value) time.Time {
	t.Helper()
	tp := v.unwrapDateTime(val)
	if tp == nil {
		t.Fatalf("expected a DateTime, got %v", val)
	}
	return *tp
}

// time.Duration overflows past ~292 years, so the add*: methods wrapped
// around (epoch + 10^10 s answered 1702) and differenceFrom: saturated.
func TestDateTimeArithmeticBeyondDurationRange(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()
	dt := getDateTimeClass(v)
	epoch := v.Send(dt, "fromEpoch:", []Value{FromSmallInt(0)})

	const n = 10000000000 // ~317 years in seconds
	want := time.Unix(n, 0).UTC()
	for _, c := range []struct {
		sel string
		arg int64
	}{
		{"addSeconds:", n},
		{"addMinutes:", n / 60},
		{"addHours:", n / 3600},
	} {
		got := dateTimeOf(t, v, v.Send(epoch, c.sel, []Value{FromSmallInt(c.arg)}))
		// addMinutes:/addHours: truncate n, so compare to the scaled value.
		scale := map[string]int64{"addSeconds:": 1, "addMinutes:": 60, "addHours:": 3600}[c.sel]
		exp := time.Unix(c.arg*scale, 0).UTC()
		if !got.Equal(exp) {
			t.Errorf("epoch %s %d = %v, want %v", c.sel, c.arg, got, exp)
		}
		if got.Year() != want.Year() {
			t.Errorf("epoch %s %d year = %d, want %d", c.sel, c.arg, got.Year(), want.Year())
		}
	}

	later := v.Send(dt, "fromEpoch:", []Value{FromSmallInt(n)})
	diff := v.Send(later, "differenceFrom:", []Value{epoch})
	if !diff.IsSmallInt() || diff.SmallInt() != n {
		t.Errorf("differenceFrom: = %v, want %d", diff, int64(n))
	}
	diff = v.Send(epoch, "differenceFrom:", []Value{later})
	if !diff.IsSmallInt() || diff.SmallInt() != -n {
		t.Errorf("reverse differenceFrom: = %v, want %d", diff, int64(-n))
	}
}

// epochMillis for dates past ~6429 AD exceeds the SmallInteger range;
// FromSmallInt panicked (killing the VM) instead of promoting.
func TestDateTimeEpochAccessorsPromote(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()
	dt := getDateTimeClass(v)
	secs := int64(250000000000) // year ~9892
	far := v.Send(dt, "fromEpoch:", []Value{FromSmallInt(secs)})

	got := bigIntOf(t, v, v.Send(far, "epochMillis", nil))
	if want := new(big.Int).Mul(big.NewInt(secs), big.NewInt(1000)); got.Cmp(want) != 0 {
		t.Errorf("epochMillis = %s, want %s", got, want)
	}
	if got := bigIntOf(t, v, v.Send(far, "epochSeconds", nil)); got.Int64() != secs {
		t.Errorf("epochSeconds = %s, want %d", got, secs)
	}
}
