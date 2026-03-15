package vm

import (
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
	dtClass.AddClassMethod0(vm.Selectors, "now", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		now := time.Now()
		return v.wrapDateTime(&now)
	})

	// DateTime parse: str format: fmt — parse a time string
	dtClass.AddClassMethod2(vm.Selectors, "parse:format:", func(vmPtr interface{}, recv Value, strVal Value, fmtVal Value) Value {
		v := vmPtr.(*VM)
		str := v.valueToString(strVal)
		format := v.valueToString(fmtVal)
		if str == "" || format == "" {
			return v.newFailureResult("DateTime parse:format: requires non-empty string and format")
		}
		t, err := time.Parse(format, str)
		if err != nil {
			return v.newFailureResult("DateTime parse:format: " + err.Error())
		}
		return v.wrapDateTime(&t)
	})

	// DateTime fromEpoch: seconds — from Unix epoch seconds
	dtClass.AddClassMethod1(vm.Selectors, "fromEpoch:", func(vmPtr interface{}, recv Value, epochVal Value) Value {
		v := vmPtr.(*VM)
		var secs int64
		if epochVal.IsSmallInt() {
			secs = epochVal.SmallInt()
		} else if epochVal.IsFloat() {
			secs = int64(epochVal.Float64())
		} else {
			return v.newFailureResult("DateTime fromEpoch: requires a number")
		}
		t := time.Unix(secs, 0).UTC()
		return v.wrapDateTime(&t)
	})

	// -------------------------------------------------------------------
	// Instance methods
	// -------------------------------------------------------------------

	// year
	dtClass.AddMethod0(vm.Selectors, "year", func(vmPtr interface{}, recv Value) Value {
		t := vmPtr.(*VM).unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		return FromSmallInt(int64(t.Year()))
	})

	// month (1-12)
	dtClass.AddMethod0(vm.Selectors, "month", func(vmPtr interface{}, recv Value) Value {
		t := vmPtr.(*VM).unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		return FromSmallInt(int64(t.Month()))
	})

	// day (1-31)
	dtClass.AddMethod0(vm.Selectors, "day", func(vmPtr interface{}, recv Value) Value {
		t := vmPtr.(*VM).unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		return FromSmallInt(int64(t.Day()))
	})

	// hour (0-23)
	dtClass.AddMethod0(vm.Selectors, "hour", func(vmPtr interface{}, recv Value) Value {
		t := vmPtr.(*VM).unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		return FromSmallInt(int64(t.Hour()))
	})

	// minute (0-59)
	dtClass.AddMethod0(vm.Selectors, "minute", func(vmPtr interface{}, recv Value) Value {
		t := vmPtr.(*VM).unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		return FromSmallInt(int64(t.Minute()))
	})

	// second (0-59)
	dtClass.AddMethod0(vm.Selectors, "second", func(vmPtr interface{}, recv Value) Value {
		t := vmPtr.(*VM).unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		return FromSmallInt(int64(t.Second()))
	})

	// format: layoutStr — format using Go layout string
	dtClass.AddMethod1(vm.Selectors, "format:", func(vmPtr interface{}, recv Value, fmtVal Value) Value {
		v := vmPtr.(*VM)
		t := v.unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		layout := v.valueToString(fmtVal)
		if layout == "" {
			return v.newFailureResult("DateTime format: requires a non-empty layout string")
		}
		return v.registry.NewStringValue(t.Format(layout))
	})

	// epochSeconds — Unix timestamp in seconds
	dtClass.AddMethod0(vm.Selectors, "epochSeconds", func(vmPtr interface{}, recv Value) Value {
		t := vmPtr.(*VM).unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		return FromSmallInt(t.Unix())
	})

	// epochMillis — Unix timestamp in milliseconds
	dtClass.AddMethod0(vm.Selectors, "epochMillis", func(vmPtr interface{}, recv Value) Value {
		t := vmPtr.(*VM).unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		return FromSmallInt(t.UnixMilli())
	})

	// addSeconds: n — return new DateTime offset by n seconds
	dtClass.AddMethod1(vm.Selectors, "addSeconds:", func(vmPtr interface{}, recv Value, nVal Value) Value {
		v := vmPtr.(*VM)
		t := v.unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		n := v.valueToInt(nVal)
		result := t.Add(time.Duration(n) * time.Second)
		return v.wrapDateTime(&result)
	})

	// addMinutes: n — return new DateTime offset by n minutes
	dtClass.AddMethod1(vm.Selectors, "addMinutes:", func(vmPtr interface{}, recv Value, nVal Value) Value {
		v := vmPtr.(*VM)
		t := v.unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		n := v.valueToInt(nVal)
		result := t.Add(time.Duration(n) * time.Minute)
		return v.wrapDateTime(&result)
	})

	// addHours: n — return new DateTime offset by n hours
	dtClass.AddMethod1(vm.Selectors, "addHours:", func(vmPtr interface{}, recv Value, nVal Value) Value {
		v := vmPtr.(*VM)
		t := v.unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		n := v.valueToInt(nVal)
		result := t.Add(time.Duration(n) * time.Hour)
		return v.wrapDateTime(&result)
	})

	// addDays: n — return new DateTime offset by n days
	dtClass.AddMethod1(vm.Selectors, "addDays:", func(vmPtr interface{}, recv Value, nVal Value) Value {
		v := vmPtr.(*VM)
		t := v.unwrapDateTime(recv)
		if t == nil {
			return Nil
		}
		n := v.valueToInt(nVal)
		result := t.AddDate(0, 0, int(n))
		return v.wrapDateTime(&result)
	})

	// differenceFrom: other — returns difference in seconds (self - other)
	dtClass.AddMethod1(vm.Selectors, "differenceFrom:", func(vmPtr interface{}, recv Value, otherVal Value) Value {
		v := vmPtr.(*VM)
		t := v.unwrapDateTime(recv)
		other := v.unwrapDateTime(otherVal)
		if t == nil || other == nil {
			return Nil
		}
		diff := t.Sub(*other)
		return FromSmallInt(int64(diff.Seconds()))
	})

	// printString — ISO 8601 representation
	dtClass.AddMethod0(vm.Selectors, "printString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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
		return vm.newFailureResult("DateTime wrap error: " + err.Error())
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
