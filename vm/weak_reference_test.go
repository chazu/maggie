package vm

import (
	"testing"
)

// ---------------------------------------------------------------------------
// WeakReference basic tests
// ---------------------------------------------------------------------------

func TestNewWeakReference(t *testing.T) {
	reg := NewObjectRegistry()
	obj := NewObject(nil, 2)
	obj.SetSlot(0, FromSmallInt(42))

	wr := NewWeakReference(reg, obj)
	if wr == nil {
		t.Fatal("NewWeakReference returned nil")
	}
	if wr.ID() == 0 {
		t.Error("WeakReference should have non-zero ID")
	}
}

func TestWeakReferenceGet(t *testing.T) {
	obj := NewObject(nil, 2)
	obj.SetSlot(0, FromSmallInt(42))

	wr := NewWeakReference(nil, obj)

	target := wr.Get()
	if target != obj {
		t.Error("Get should return the target object")
	}
	if target.GetSlot(0).SmallInt() != 42 {
		t.Error("Target slot value should be preserved")
	}
}

func TestWeakReferenceIsAlive(t *testing.T) {
	obj := NewObject(nil, 2)
	wr := NewWeakReference(nil, obj)

	if !wr.IsAlive() {
		t.Error("WeakReference should be alive initially")
	}

	wr.Clear()

	if wr.IsAlive() {
		t.Error("WeakReference should not be alive after Clear")
	}
}

func TestWeakReferenceClear(t *testing.T) {
	obj := NewObject(nil, 2)
	wr := NewWeakReference(nil, obj)

	old := wr.Clear()
	if old != obj {
		t.Error("Clear should return the old target")
	}

	if wr.Get() != nil {
		t.Error("Get should return nil after Clear")
	}
}

func TestWeakReferenceFinalizer(t *testing.T) {
	obj := NewObject(nil, 2)
	wr := NewWeakReference(nil, obj)

	finalizerCalled := false
	wr.SetFinalizer(func(v Value) {
		finalizerCalled = true
	})

	fn := wr.Finalizer()
	if fn == nil {
		t.Error("Finalizer should be set")
	}

	// Call it manually to verify it works
	fn(Nil)
	if !finalizerCalled {
		t.Error("Finalizer should have been called")
	}
}

// ---------------------------------------------------------------------------
// WeakRegistry tests
// ---------------------------------------------------------------------------

func TestWeakRegistryRegister(t *testing.T) {
	objReg := NewObjectRegistry()
	registry := NewWeakRegistry()
	obj := NewObject(nil, 2)
	wr := NewWeakReference(objReg, obj)

	registry.Register(wr)

	if registry.Count() != 1 {
		t.Errorf("Count = %d, want 1", registry.Count())
	}

	found := registry.Lookup(wr.ID())
	if found != wr {
		t.Error("Lookup should find the registered weak ref")
	}
}

func TestWeakRegistryUnregister(t *testing.T) {
	objReg := NewObjectRegistry()
	registry := NewWeakRegistry()
	obj := NewObject(nil, 2)
	wr := NewWeakReference(objReg, obj)

	registry.Register(wr)
	registry.Unregister(wr)

	if registry.Count() != 0 {
		t.Errorf("Count = %d, want 0", registry.Count())
	}

	found := registry.Lookup(wr.ID())
	if found != nil {
		t.Error("Lookup should return nil for unregistered weak ref")
	}
}

func TestWeakRegistryProcessGC(t *testing.T) {
	objReg := NewObjectRegistry()
	registry := NewWeakRegistry()

	// Create objects - one will be "collected" (unmarked), one will survive
	obj1 := NewObject(nil, 2)
	obj2 := NewObject(nil, 2)

	wr1 := NewWeakReference(objReg, obj1)
	wr2 := NewWeakReference(objReg, obj2)

	registry.Register(wr1)
	registry.Register(wr2)

	// Simulate GC where only obj2 is marked as reachable
	marked := make(map[*Object]struct{})
	marked[obj2] = struct{}{}

	cleared := registry.ProcessGC(marked)

	if cleared != 1 {
		t.Errorf("ProcessGC cleared = %d, want 1", cleared)
	}

	if wr1.IsAlive() {
		t.Error("wr1 should not be alive after GC (obj1 was not marked)")
	}
	if !wr2.IsAlive() {
		t.Error("wr2 should still be alive after GC (obj2 was marked)")
	}
}

func TestWeakRegistryFinalizerCalledOnGC(t *testing.T) {
	registry := NewWeakRegistry()
	obj := NewObject(nil, 2)
	wr := NewWeakReference(nil, obj)
	registry.Register(wr)

	finalizerCalled := false
	wr.SetFinalizer(func(v Value) {
		finalizerCalled = true
	})

	// Simulate GC where obj is not reachable (not in marked set)
	marked := make(map[*Object]struct{})
	registry.ProcessGC(marked)

	if !finalizerCalled {
		t.Error("Finalizer should be called when object is collected")
	}
}

// ---------------------------------------------------------------------------
// Value encoding tests
// ---------------------------------------------------------------------------

func TestFromWeakRef(t *testing.T) {
	obj := NewObject(nil, 2)
	wr := NewWeakReference(nil, obj)

	v := FromWeakRef(wr)
	if !v.IsWeakRef() {
		t.Error("Value should be a weak reference")
	}
}

func TestFromWeakRefNil(t *testing.T) {
	v := FromWeakRef(nil)
	if v != Nil {
		t.Error("FromWeakRef(nil) should return Nil")
	}
}

func TestWeakRefID(t *testing.T) {
	obj := NewObject(nil, 2)
	wr := NewWeakReference(nil, obj)

	v := FromWeakRef(wr)
	id := v.WeakRefID()

	if id != wr.ID() {
		t.Errorf("WeakRefID = %d, want %d", id, wr.ID())
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

	// Should be registered
	found := vm.LookupWeakRef(wr.ID())
	if found != wr {
		t.Error("Weak ref should be registered with VM")
	}
}

func TestVMWeakRefGCIntegration(t *testing.T) {
	vm := NewVM()

	// Create an object and a weak reference to it
	obj := NewObject(vm.ObjectClass.VTable, 2)
	obj.SetSlot(0, FromSmallInt(42))
	vm.keepAlive[obj] = struct{}{} // Register with keepAlive

	wr := vm.NewWeakRef(obj)

	// Before GC: weak ref should be alive
	if !wr.IsAlive() {
		t.Error("Weak ref should be alive before GC")
	}

	// Simulate object becoming unreachable by removing from keepAlive
	delete(vm.keepAlive, obj)

	// Run GC
	vm.CollectGarbage()

	// After GC: weak ref should be cleared
	if wr.IsAlive() {
		t.Error("Weak ref should not be alive after GC collected the target")
	}
}

func TestWeakReferencePrimitiveOn(t *testing.T) {
	vm := NewVM()

	// Create an object
	objVal := vm.Send(vm.Symbols.SymbolValue("Object"), "new", nil)
	if !objVal.IsObject() {
		t.Fatal("Failed to create object")
	}

	// Create weak reference using primitive
	wrVal := vm.Send(vm.Symbols.SymbolValue("WeakReference"), "on:", []Value{objVal})
	if !wrVal.IsWeakRef() {
		t.Fatal("WeakReference on: should return a weak reference")
	}
}

func TestWeakReferencePrimitiveGet(t *testing.T) {
	vm := NewVM()

	// Create an object
	objVal := vm.Send(vm.Symbols.SymbolValue("Object"), "new", nil)

	// Create weak reference
	wrVal := vm.Send(vm.Symbols.SymbolValue("WeakReference"), "on:", []Value{objVal})

	// Get the target back
	result := vm.Send(wrVal, "get", nil)
	if result != objVal {
		t.Error("get should return the original object")
	}
}

func TestWeakReferencePrimitiveIsAlive(t *testing.T) {
	vm := NewVM()

	// Create an object
	objVal := vm.Send(vm.Symbols.SymbolValue("Object"), "new", nil)

	// Create weak reference
	wrVal := vm.Send(vm.Symbols.SymbolValue("WeakReference"), "on:", []Value{objVal})

	// Check isAlive
	result := vm.Send(wrVal, "isAlive", nil)
	if result != True {
		t.Error("isAlive should return true for live reference")
	}

	// Clear it
	vm.Send(wrVal, "clear", nil)

	// Check isAlive again
	result = vm.Send(wrVal, "isAlive", nil)
	if result != False {
		t.Error("isAlive should return false after clear")
	}
}

func TestWeakReferencePrintString(t *testing.T) {
	vm := NewVM()

	// Create an object
	objVal := vm.Send(vm.Symbols.SymbolValue("Object"), "new", nil)

	// Create weak reference
	wrVal := vm.Send(vm.Symbols.SymbolValue("WeakReference"), "on:", []Value{objVal})

	// Get printString
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

func BenchmarkNewWeakReference(b *testing.B) {
	reg := NewObjectRegistry()
	obj := NewObject(nil, 2)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = NewWeakReference(reg, obj)
	}
}

func BenchmarkWeakReferenceGet(b *testing.B) {
	obj := NewObject(nil, 2)
	wr := NewWeakReference(nil, obj)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = wr.Get()
	}
}

func BenchmarkWeakRegistryLookup(b *testing.B) {
	reg := NewObjectRegistry()
	registry := NewWeakRegistry()
	obj := NewObject(nil, 2)
	wr := NewWeakReference(reg, obj)
	registry.Register(wr)
	id := wr.ID()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = registry.Lookup(id)
	}
}
