package vm

import (
	"math/big"
	"reflect"
	"time"
)

// ---------------------------------------------------------------------------
// DateTime Primitives: Wraps Go time.Time via GoObjectWrapper
// ---------------------------------------------------------------------------

// dateTimeType is the reflect.Type for *time.Time pointers stored in GoObjectWrapper.
var dateTimeType = reflect.TypeOf((*time.Time)(nil))

func (vm *VM) registerDateTimePrimitives() {
	dtClass := vm.RegisterGoType("DateTime", dateTimeType)

	// -------------------------------------------------------------------
	// Class methods
	// -------------------------------------------------------------------

	// DateTime now — current time
	dtClass.AddClassMethod0(vm.Selectors, "now", func(v *VM, recv Value) Value {
		now := time.Now()
		return v.wrapDateTime(&now)
	})

	// DateTime parse: str format: fmt — parse a time string. Answers Success
	// wrapping the DateTime, or Failure when the string does not match.
	dtClass.AddClassMethod2(vm.Selectors, "parse:format:", func(v *VM, recv Value, strVal Value, fmtVal Value) Value {
		str := v.valueToString(strVal)
		format := v.valueToString(fmtVal)
		if str == "" || format == "" {
			return v.newFailureResult("DateTime parse:format: requires non-empty string and format")
		}
		t, err := time.Parse(format, str)
		if err != nil {
			return v.newFailureResult("DateTime parse:format: " + err.Error())
		}
		return v.newSuccessResult(v.wrapDateTime(&t))
	})

	// DateTime fromEpoch: seconds — from Unix epoch seconds. A non-number is
	// a programmer error and signals (CONVENTIONS §1).
	dtClass.AddClassMethod1(vm.Selectors, "fromEpoch:", func(v *VM, recv Value, epochVal Value) Value {
		var secs int64
		if epochVal.IsSmallInt() {
			secs = epochVal.SmallInt()
		} else if epochVal.IsFloat() {
			secs = int64(epochVal.Float64())
		} else {
			return v.SignalPrimitiveError("DateTime fromEpoch:", "argument must be a number")
		}
		t := time.Unix(secs, 0).UTC()
		return v.wrapDateTime(&t)
	})

	// -------------------------------------------------------------------
	// Instance methods
	// -------------------------------------------------------------------

	// year
	dtClass.AddMethod0(vm.Selectors, "year", func(v *VM, recv Value) Value {
		t := v.unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		return FromSmallInt(int64(t.Year()))
	})

	// month (1-12)
	dtClass.AddMethod0(vm.Selectors, "month", func(v *VM, recv Value) Value {
		t := v.unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		return FromSmallInt(int64(t.Month()))
	})

	// day (1-31)
	dtClass.AddMethod0(vm.Selectors, "day", func(v *VM, recv Value) Value {
		t := v.unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		return FromSmallInt(int64(t.Day()))
	})

	// hour (0-23)
	dtClass.AddMethod0(vm.Selectors, "hour", func(v *VM, recv Value) Value {
		t := v.unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		return FromSmallInt(int64(t.Hour()))
	})

	// minute (0-59)
	dtClass.AddMethod0(vm.Selectors, "minute", func(v *VM, recv Value) Value {
		t := v.unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		return FromSmallInt(int64(t.Minute()))
	})

	// second (0-59)
	dtClass.AddMethod0(vm.Selectors, "second", func(v *VM, recv Value) Value {
		t := v.unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		return FromSmallInt(int64(t.Second()))
	})

	// format: layoutStr — format using Go layout string. Formatting cannot
	// fail; a missing/non-String layout is a programmer error and signals.
	dtClass.AddMethod1(vm.Selectors, "format:", func(v *VM, recv Value, fmtVal Value) Value {
		t := v.unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		layout := v.valueToString(fmtVal)
		if layout == "" {
			return v.SignalPrimitiveError("DateTime format:", "layout must be a non-empty String")
		}
		return v.registry.NewStringValue(t.Format(layout))
	})

	// epochSeconds — Unix timestamp in seconds
	dtClass.AddMethod0(vm.Selectors, "epochSeconds", func(v *VM, recv Value) Value {
		t := v.unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		return v.registry.NewIntegerValue(t.Unix())
	})

	// epochMillis — Unix timestamp in milliseconds
	dtClass.AddMethod0(vm.Selectors, "epochMillis", func(v *VM, recv Value) Value {
		t := v.unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		return v.registry.NewIntegerValue(t.UnixMilli())
	})

	// addSeconds: n — return new DateTime offset by n seconds
	dtClass.AddMethod1(vm.Selectors, "addSeconds:", func(v *VM, recv Value, nVal Value) Value {
		t := v.unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		return v.dateTimeAddSeconds("addSeconds:", *t, v.valueToInt(nVal), 1)
	})

	// addMinutes: n — return new DateTime offset by n minutes
	dtClass.AddMethod1(vm.Selectors, "addMinutes:", func(v *VM, recv Value, nVal Value) Value {
		t := v.unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		return v.dateTimeAddSeconds("addMinutes:", *t, v.valueToInt(nVal), 60)
	})

	// addHours: n — return new DateTime offset by n hours
	dtClass.AddMethod1(vm.Selectors, "addHours:", func(v *VM, recv Value, nVal Value) Value {
		t := v.unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		return v.dateTimeAddSeconds("addHours:", *t, v.valueToInt(nVal), 3600)
	})

	// addDays: n — return new DateTime offset by n days
	dtClass.AddMethod1(vm.Selectors, "addDays:", func(v *VM, recv Value, nVal Value) Value {
		t := v.unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		n := v.valueToInt(nVal)
		result := t.AddDate(0, 0, int(n))
		return v.wrapDateTime(&result)
	})

	// differenceFrom: other — returns difference in seconds (self - other)
	dtClass.AddMethod1(vm.Selectors, "differenceFrom:", func(v *VM, recv Value, otherVal Value) Value {
		t := v.unwrapDateTime(recv)
		other := v.unwrapDateTime(otherVal)
		if t == nil || other == nil {
			return Nil
		}
		return v.dateTimeDifferenceSeconds(*t, *other)
	})

	// printString — ISO 8601 representation
	dtClass.AddMethod0(vm.Selectors, "printString", func(v *VM, recv Value) Value {
		t := v.unwrapDateTime(recv)
		if t == nil {
			return v.registry.NewStringValue("a DateTime")
		}
		return v.registry.NewStringValue(t.Format(time.RFC3339))
	})
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

// wrapDateTime wraps a *time.Time as a Maggie GoObject Value.
func (vm *VM) wrapDateTime(t *time.Time) Value {
	val, err := vm.RegisterGoObject(t)
	if err != nil {
		// Internal error (DateTime type not registered): not an expected
		// failure, so signal rather than smuggle a Failure into a DateTime slot.
		return vm.SignalPrimitiveError("DateTime", "wrap error: "+err.Error())
	}
	return val
}

// unwrapDateTime extracts *time.Time from a GoObject Value.
func (vm *VM) unwrapDateTime(v Value) *time.Time {
	goVal, ok := vm.GetGoObject(v)
	if !ok {
		return nil
	}
	t, ok := goVal.(*time.Time)
	if !ok {
		return nil
	}
	return t
}

// dateTimeAddSeconds answers t offset by n*unit seconds. It works in Unix
// seconds rather than time.Duration, which overflows past ~292 years (the
// offset silently wrapped around); a result time.Time cannot hold signals.
func (vm *VM) dateTimeAddSeconds(selector string, t time.Time, n, unit int64) Value {
	delta := new(big.Int).Mul(big.NewInt(n), big.NewInt(unit))
	secs := delta.Add(delta, big.NewInt(t.Unix()))
	// time.Time wraps silently near the int64 limits, so check the round trip.
	var result time.Time
	if secs.IsInt64() {
		result = time.Unix(secs.Int64(), int64(t.Nanosecond())).In(t.Location())
	}
	if !secs.IsInt64() || result.Unix() != secs.Int64() {
		return vm.SignalPrimitiveError(selector, "result is out of the representable time range")
	}
	return vm.wrapDateTime(&result)
}

// dateTimeDifferenceSeconds answers (t - other) in whole seconds, truncated
// toward zero. Unlike t.Sub, it does not saturate at ~292 years.
func (vm *VM) dateTimeDifferenceSeconds(t, other time.Time) Value {
	secs := new(big.Int).Sub(big.NewInt(t.Unix()), big.NewInt(other.Unix()))
	nanos := t.Nanosecond() - other.Nanosecond()
	switch {
	case secs.Sign() > 0 && nanos < 0:
		secs.Sub(secs, big.NewInt(1))
	case secs.Sign() < 0 && nanos > 0:
		secs.Add(secs, big.NewInt(1))
	}
	return vm.registry.NewBigIntValue(secs)
}

// valueToInt extracts an integer from a Value (SmallInt or Float).
func (vm *VM) valueToInt(v Value) int64 {
	if v.IsSmallInt() {
		return v.SmallInt()
	}
	if v.IsFloat() {
		return int64(v.Float64())
	}
	return 0
}
