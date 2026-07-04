package vm

import (
	"runtime"
	"testing"
)

// ---------------------------------------------------------------------------
// WeakReference basic tests (Go-native weak.Pointer backing)
// ---------------------------------------------------------------------------

func TestWeakReferenceGet(t *testing.T) {
	obj := NewObject(nil, 2)
	obj.SetSlot(0, FromSmallInt(42))

	wr := NewWeakReference(obj)

	target := wr.Get()
	if target != obj {
		t.Error("Get should return the target object")
	}
	if target.GetSlot(0).SmallInt() != 42 {
		t.Error("Target slot value should be preserved")
	}
	runtime.KeepAlive(obj)
}

func TestWeakReferenceIsAlive(t *testing.T) {
	obj := NewObject(nil, 2)
	wr := NewWeakReference(obj)

	if !wr.IsAlive() {
		t.Error("WeakReference should be alive initially")
	}

	wr.Clear()

	if wr.IsAlive() {
		t.Error("WeakReference should not be alive after Clear")
	}
	runtime.KeepAlive(obj)
}

func TestWeakReferenceClear(t *testing.T) {
	obj := NewObject(nil, 2)
	wr := NewWeakReference(obj)

	old := wr.Clear()
	if old != obj {
		t.Error("Clear should return the old target")
	}

	if wr.Get() != nil {
		t.Error("Get should return nil after Clear")
	}
	runtime.KeepAlive(obj)
}

func TestWeakReferenceFinalizerManualCall(t *testing.T) {
	obj := NewObject(nil, 2)
	wr := NewWeakReference(obj)

	finalizerCalled := false
	wr.SetFinalizer(func(v Value) {
		finalizerCalled = true
	})

	fn := wr.Finalizer()
	if fn == nil {
		t.Error("Finalizer should be set")
	}

	// Invoke manually to verify the callback wiring.
	fn(Nil)
	if !finalizerCalled {
		t.Error("Finalizer should have been called")
	}
	runtime.KeepAlive(obj)
}

// TestWeakReferenceClearedByGC verifies the Go GC clears the weak pointer once
// the target is unreachable — the mechanism that replaces the old ProcessGC pass.
func TestWeakReferenceClearedByGC(t *testing.T) {
	wr := func() *WeakReference {
		obj := NewObject(nil, 2)
		return NewWeakReference(obj)
	}()

	if !wr.IsAlive() {
		t.Fatal("weak ref should be alive before GC")
	}

	// The target is unreachable (no strong reference survives the closure).
	runtime.GC()

	if wr.IsAlive() {
		t.Error("weak ref should be cleared after the target is collected")
	}
	if wr.Get() != nil {
		t.Error("Get should return nil after the target is collected")
	}
}

// ---------------------------------------------------------------------------
// Value encoding tests (pointer-carrying kindWeakRef)
// ---------------------------------------------------------------------------

func TestFromWeakRef(t *testing.T) {
	obj := NewObject(nil, 2)
	wr := NewWeakReference(obj)

	v := FromWeakRef(wr)
	if !v.IsWeakRef() {
		t.Error("Value should be a weak reference")
	}
	if weakRefFromValue(v) != wr {
		t.Error("weakRefFromValue should resolve back to the same WeakReference")
	}
	runtime.KeepAlive(obj)
}

func TestFromWeakRefNil(t *testing.T) {
	v := FromWeakRef(nil)
	if v != Nil {
		t.Error("FromWeakRef(nil) should return Nil")
	}
}

func TestIsWeakRefNegative(t *testing.T) {
	if Nil.IsWeakRef() {
		t.Error("Nil should not be a weak reference")
	}
	if FromSmallInt(42).IsWeakRef() {
		t.Error("SmallInt should not be a weak reference")
	}
}

// ---------------------------------------------------------------------------
// VM integration tests
// ---------------------------------------------------------------------------

func TestVMNewWeakRef(t *testing.T) {
	vm := NewVM()

	obj := NewObject(vm.ObjectClass.VTable, 2)
	obj.SetSlot(0, FromSmallInt(42))

	wr := vm.NewWeakRef(obj)
	if wr == nil {
		t.Fatal("VM.NewWeakRef returned nil")
	}
	if wr.Get() != obj {
		t.Error("weak ref should resolve to the target object")
	}
	runtime.KeepAlive(obj)
}

func TestWeakReferencePrimitiveOn(t *testing.T) {
	vm := NewVM()

	objVal := vm.Send(vm.Symbols.SymbolValue("Object"), "new", nil)
	if !objVal.IsObject() {
		t.Fatal("Failed to create object")
	}

	wrVal := vm.Send(vm.Symbols.SymbolValue("WeakReference"), "on:", []Value{objVal})
	if !wrVal.IsWeakRef() {
		t.Fatal("WeakReference on: should return a weak reference")
	}
}

func TestWeakReferencePrimitiveGet(t *testing.T) {
	vm := NewVM()

	objVal := vm.Send(vm.Symbols.SymbolValue("Object"), "new", nil)
	wrVal := vm.Send(vm.Symbols.SymbolValue("WeakReference"), "on:", []Value{objVal})

	result := vm.Send(wrVal, "get", nil)
	if result != objVal {
		t.Error("get should return the original object")
	}
}

func TestWeakReferencePrimitiveIsAlive(t *testing.T) {
	vm := NewVM()

	objVal := vm.Send(vm.Symbols.SymbolValue("Object"), "new", nil)
	wrVal := vm.Send(vm.Symbols.SymbolValue("WeakReference"), "on:", []Value{objVal})

	result := vm.Send(wrVal, "isAlive", nil)
	if result != True {
		t.Error("isAlive should return true for live reference")
	}

	vm.Send(wrVal, "clear", nil)

	result = vm.Send(wrVal, "isAlive", nil)
	if result != False {
		t.Error("isAlive should return false after clear")
	}
}

func TestWeakReferencePrintString(t *testing.T) {
	vm := NewVM()

	objVal := vm.Send(vm.Symbols.SymbolValue("Object"), "new", nil)
	wrVal := vm.Send(vm.Symbols.SymbolValue("WeakReference"), "on:", []Value{objVal})

	result := vm.Send(wrVal, "printString", nil)
	if !IsStringValue(result) {
		t.Error("printString should return a string")
	}

	str := vm.registry.GetStringContent(result)
	if str != "a WeakReference to Object" {
		t.Errorf("printString = %q, want 'a WeakReference to Object'", str)
	}
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

func BenchmarkWeakReferenceGet(b *testing.B) {
	obj := NewObject(nil, 2)
	wr := NewWeakReference(obj)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = wr.Get()
	}
	runtime.KeepAlive(obj)
}
