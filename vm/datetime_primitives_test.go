package vm

import (
	"testing"
	"time"
)

func getDateTimeClass(v *VM) Value {
	return v.Globals["DateTime"]
}

func TestDateTimeNow(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	dt := getDateTimeClass(v)
	before := time.Now()
	result := v.Send(dt, "now", nil)
	after := time.Now()

	goVal, ok := v.GetGoObject(result)
	if !ok {
		t.Fatal("DateTime now should return a GoObject")
	}
	tp, ok := goVal.(*time.Time)
	if !ok {
		t.Fatal("DateTime now should wrap *time.Time")
	}
	if tp.Before(before) || tp.After(after) {
		t.Errorf("DateTime now returned %v, expected between %v and %v", tp, before, after)
	}
}

func TestDateTimeFromEpoch(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	dt := getDateTimeClass(v)
	epoch := FromSmallInt(1000)
	result := v.Send(dt, "fromEpoch:", []Value{epoch})

	year := v.Send(result, "year", nil)
	if !year.IsSmallInt() || year.SmallInt() != 1970 {
		t.Errorf("fromEpoch: 1000 year = %d, want 1970", year.SmallInt())
	}

	secs := v.Send(result, "epochSeconds", nil)
	if !secs.IsSmallInt() || secs.SmallInt() != 1000 {
		t.Errorf("fromEpoch: 1000 epochSeconds = %v, want 1000", secs)
	}
}

func TestDateTimeParseFormat(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	dt := getDateTimeClass(v)
	str := v.registry.NewStringValue("2025-06-15 14:30:45")
	layout := v.registry.NewStringValue("2006-01-02 15:04:05")
	result := v.Send(dt, "parse:format:", []Value{str, layout})

	tests := []struct {
		sel  string
		want int64
	}{
		{"year", 2025},
		{"month", 6},
		{"day", 15},
		{"hour", 14},
		{"minute", 30},
		{"second", 45},
	}

	for _, tt := range tests {
		got := v.Send(result, tt.sel, nil)
		if !got.IsSmallInt() || got.SmallInt() != tt.want {
			t.Errorf("%s = %v, want %d", tt.sel, got, tt.want)
		}
	}
}

func TestDateTimeFormat(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	dt := getDateTimeClass(v)
	str := v.registry.NewStringValue("2025-03-14")
	layout := v.registry.NewStringValue("2006-01-02")
	result := v.Send(dt, "parse:format:", []Value{str, layout})

	outLayout := v.registry.NewStringValue("01/02/2006")
	formatted := v.Send(result, "format:", []Value{outLayout})
	s := v.valueToString(formatted)
	if s != "03/14/2025" {
		t.Errorf("format: = %q, want %q", s, "03/14/2025")
	}
}

func TestDateTimeArithmetic(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	dt := getDateTimeClass(v)
	epoch0 := FromSmallInt(0)
	base := v.Send(dt, "fromEpoch:", []Value{epoch0})

	// addSeconds: 60
	r := v.Send(base, "addSeconds:", []Value{FromSmallInt(60)})
	secs := v.Send(r, "epochSeconds", nil)
	if secs.SmallInt() != 60 {
		t.Errorf("addSeconds: 60 epochSeconds = %d, want 60", secs.SmallInt())
	}

	// addMinutes: 5
	r = v.Send(base, "addMinutes:", []Value{FromSmallInt(5)})
	secs = v.Send(r, "epochSeconds", nil)
	if secs.SmallInt() != 300 {
		t.Errorf("addMinutes: 5 epochSeconds = %d, want 300", secs.SmallInt())
	}

	// addHours: 2
	r = v.Send(base, "addHours:", []Value{FromSmallInt(2)})
	secs = v.Send(r, "epochSeconds", nil)
	if secs.SmallInt() != 7200 {
		t.Errorf("addHours: 2 epochSeconds = %d, want 7200", secs.SmallInt())
	}

	// addDays: 1
	r = v.Send(base, "addDays:", []Value{FromSmallInt(1)})
	secs = v.Send(r, "epochSeconds", nil)
	if secs.SmallInt() != 86400 {
		t.Errorf("addDays: 1 epochSeconds = %d, want 86400", secs.SmallInt())
	}
}

func TestDateTimeDifferenceFrom(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	dt := getDateTimeClass(v)
	a := v.Send(dt, "fromEpoch:", []Value{FromSmallInt(100)})
	b := v.Send(dt, "fromEpoch:", []Value{FromSmallInt(0)})

	diff := v.Send(a, "differenceFrom:", []Value{b})
	if !diff.IsSmallInt() || diff.SmallInt() != 100 {
		t.Errorf("differenceFrom: = %v, want 100", diff)
	}

	diff = v.Send(b, "differenceFrom:", []Value{a})
	if !diff.IsSmallInt() || diff.SmallInt() != -100 {
		t.Errorf("reverse differenceFrom: = %v, want -100", diff)
	}
}

func TestDateTimePrintString(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	dt := getDateTimeClass(v)
	epoch0 := FromSmallInt(0)
	result := v.Send(dt, "fromEpoch:", []Value{epoch0})

	ps := v.Send(result, "printString", nil)
	s := v.valueToString(ps)
	if s != "1970-01-01T00:00:00Z" {
		t.Errorf("printString = %q, want %q", s, "1970-01-01T00:00:00Z")
	}
}

func TestDateTimeEpochMillis(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	dt := getDateTimeClass(v)
	epoch0 := FromSmallInt(0)
	result := v.Send(dt, "fromEpoch:", []Value{epoch0})

	millis := v.Send(result, "epochMillis", nil)
	if !millis.IsSmallInt() || millis.SmallInt() != 0 {
		t.Errorf("epochMillis = %v, want 0", millis)
	}
}

func TestDateTimeParseInvalid(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	dt := getDateTimeClass(v)
	str := v.registry.NewStringValue("not-a-date")
	layout := v.registry.NewStringValue("2006-01-02")
	result := v.Send(dt, "parse:format:", []Value{str, layout})

	// Should return a Failure result, not a GoObject
	if !isResultValue(result) {
		t.Fatal("parsing invalid date should return a Result (Failure)")
	}
}
