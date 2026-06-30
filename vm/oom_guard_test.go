package vm

import (
	"strings"
	"testing"
)

// signalsPrimitiveError invokes fn and reports the message of a SignaledException
// it raises, if any. Used to assert that allocation guards raise a catchable
// Maggie error rather than triggering a fatal Go OOM.
func signalsPrimitiveError(vm *VM, fn func()) (msg string, signaled bool) {
	defer func() {
		if r := recover(); r != nil {
			if m, ok := FormatUnhandledPanic(vm, r); ok {
				msg = m
				signaled = true
				return
			}
			panic(r)
		}
	}()
	fn()
	return "", false
}

func TestArrayNewRejectsOversize(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	arrayClass := vm.Symbols.SymbolValue("Array")

	msg, signaled := signalsPrimitiveError(vm, func() {
		vm.Send(arrayClass, "new:", []Value{FromSmallInt(MaxArrayElements + 1)})
	})
	if !signaled {
		t.Fatal("Array new: with oversize count should raise a catchable error")
	}
	if !strings.Contains(msg, "maximum") {
		t.Errorf("unexpected error message: %s", msg)
	}

	// A legitimate small size must still succeed.
	if _, ok := signalsPrimitiveError(vm, func() {
		vm.Send(arrayClass, "new:", []Value{FromSmallInt(8)})
	}); ok {
		t.Error("Array new: 8 should not raise")
	}
}

func TestBitShiftRejectsOversize(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	msg, signaled := signalsPrimitiveError(vm, func() {
		vm.Send(FromSmallInt(1), "bitShift:", []Value{FromSmallInt(MaxBitShift + 1)})
	})
	if !signaled {
		t.Fatal("bitShift: with oversize shift should raise a catchable error")
	}
	if !strings.Contains(msg, "too large") {
		t.Errorf("unexpected error message: %s", msg)
	}

	// A modest shift must still produce a value.
	if _, ok := signalsPrimitiveError(vm, func() {
		vm.Send(FromSmallInt(1), "bitShift:", []Value{FromSmallInt(10)})
	}); ok {
		t.Error("1 bitShift: 10 should not raise")
	}
}

func TestSmallIntDivideByZeroSignals(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	if _, signaled := signalsPrimitiveError(vm, func() {
		vm.Send(FromSmallInt(5), "/", []Value{FromSmallInt(0)})
	}); !signaled {
		t.Error("5 / 0 should signal ZeroDivide, not return nil")
	}
	// A normal division still works.
	r := vm.Send(FromSmallInt(10), "/", []Value{FromSmallInt(2)})
	if !r.IsSmallInt() || r.SmallInt() != 5 {
		t.Errorf("10 / 2 = %v, want 5", r)
	}
}

func TestArrayListAndChannelNewRejectOversize(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	alClass := vm.Symbols.SymbolValue("ArrayList")
	if _, signaled := signalsPrimitiveError(vm, func() {
		vm.Send(alClass, "new:", []Value{FromSmallInt(MaxArrayElements + 1)})
	}); !signaled {
		t.Error("ArrayList new: oversize should raise a catchable error")
	}

	chClass := vm.Symbols.SymbolValue("Channel")
	if _, signaled := signalsPrimitiveError(vm, func() {
		vm.Send(chClass, "new:", []Value{FromSmallInt(MaxArrayElements + 1)})
	}); !signaled {
		t.Error("Channel new: oversize should raise a catchable error")
	}
}
