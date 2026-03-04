package vm

import (
	"sync"
	"testing"
)

// ---------------------------------------------------------------------------
// SelectorTable tests
// ---------------------------------------------------------------------------

func TestSelectorTableIntern(t *testing.T) {
	st := NewSelectorTable()

	// First intern should get ID 0
	id1 := st.Intern("at:")
	if id1 != 0 {
		t.Errorf("first Intern got ID %d, want 0", id1)
	}

	// Second intern of same name should get same ID
	id2 := st.Intern("at:")
	if id2 != id1 {
		t.Errorf("re-Intern got ID %d, want %d", id2, id1)
	}

	// Different name should get different ID
	id3 := st.Intern("at:put:")
	if id3 == id1 {
		t.Error("different name should get different ID")
	}
	if id3 != 1 {
		t.Errorf("second unique Intern got ID %d, want 1", id3)
	}
}

func TestSelectorTableLookup(t *testing.T) {
	st := NewSelectorTable()
	st.Intern("foo")
	st.Intern("bar")

	// Lookup existing
	if id := st.Lookup("foo"); id != 0 {
		t.Errorf("Lookup(foo) = %d, want 0", id)
	}
	if id := st.Lookup("bar"); id != 1 {
		t.Errorf("Lookup(bar) = %d, want 1", id)
	}

	// Lookup non-existing
	if id := st.Lookup("baz"); id != -1 {
		t.Errorf("Lookup(baz) = %d, want -1", id)
	}
}

func TestSelectorTableName(t *testing.T) {
	st := NewSelectorTable()
	st.Intern("hello")
	st.Intern("world")

	if name := st.Name(0); name != "hello" {
		t.Errorf("Name(0) = %q, want %q", name, "hello")
	}
	if name := st.Name(1); name != "world" {
		t.Errorf("Name(1) = %q, want %q", name, "world")
	}
	if name := st.Name(-1); name != "" {
		t.Errorf("Name(-1) = %q, want empty", name)
	}
	if name := st.Name(100); name != "" {
		t.Errorf("Name(100) = %q, want empty", name)
	}
}

func TestSelectorTableLen(t *testing.T) {
	st := NewSelectorTable()
	if st.Len() != 0 {
		t.Error("empty table should have Len() 0")
	}

	st.Intern("a")
	st.Intern("b")
	st.Intern("c")

	if st.Len() != 3 {
		t.Errorf("Len() = %d, want 3", st.Len())
	}
}

func TestSelectorTableAll(t *testing.T) {
	st := NewSelectorTable()
	st.Intern("x")
	st.Intern("y")
	st.Intern("z")

	all := st.All()
	if len(all) != 3 {
		t.Fatalf("All() has %d elements, want 3", len(all))
	}
	if all[0] != "x" || all[1] != "y" || all[2] != "z" {
		t.Errorf("All() = %v, want [x, y, z]", all)
	}
}

func TestSelectorTableInternAll(t *testing.T) {
	st := NewSelectorTable()
	ids := st.InternAll("a", "b", "c")

	if len(ids) != 3 {
		t.Fatalf("InternAll returned %d IDs, want 3", len(ids))
	}
	if ids[0] != 0 || ids[1] != 1 || ids[2] != 2 {
		t.Errorf("InternAll IDs = %v, want [0, 1, 2]", ids)
	}
}

func TestSelectorTableConcurrency(t *testing.T) {
	st := NewSelectorTable()
	var wg sync.WaitGroup

	// Concurrently intern many selectors
	for i := 0; i < 100; i++ {
		wg.Add(1)
		go func(n int) {
			defer wg.Done()
			for j := 0; j < 100; j++ {
				name := string(rune('a' + (n+j)%26))
				st.Intern(name)
			}
		}(i)
	}
	wg.Wait()

	// Should have exactly 26 unique selectors (a-z)
	if st.Len() != 26 {
		t.Errorf("after concurrent interns, Len() = %d, want 26", st.Len())
	}
}

// ---------------------------------------------------------------------------
// VTable tests
// ---------------------------------------------------------------------------

func TestVTableLookup(t *testing.T) {
	class := &Class{Name: "TestClass"}
	vt := NewVTable(class, nil)

	// Add some methods
	m1 := NewMethod0("method1", func(vm interface{}, receiver Value) Value {
		return FromSmallInt(1)
	})
	m2 := NewMethod1("method2", func(vm interface{}, receiver, arg1 Value) Value {
		return FromSmallInt(2)
	})

	vt.AddMethod(0, m1)
	vt.AddMethod(5, m2) // Sparse - leave gaps

	// Lookup existing methods
	if got := vt.Lookup(0); got != m1 {
		t.Error("Lookup(0) should return m1")
	}
	if got := vt.Lookup(5); got != m2 {
		t.Error("Lookup(5) should return m2")
	}

	// Lookup non-existing
	if got := vt.Lookup(1); got != nil {
		t.Error("Lookup(1) should return nil")
	}
	if got := vt.Lookup(100); got != nil {
		t.Error("Lookup(100) should return nil")
	}
	if got := vt.Lookup(-1); got != nil {
		t.Error("Lookup(-1) should return nil")
	}
}

func TestVTableInheritance(t *testing.T) {
	// Create parent class with a method
	parentClass := &Class{Name: "Parent"}
	parentVT := NewVTable(parentClass, nil)
	parentMethod := NewMethod0("parentMethod", func(vm interface{}, receiver Value) Value {
		return FromSmallInt(100)
	})
	parentVT.AddMethod(0, parentMethod)

	// Create child class that inherits from parent
	childClass := &Class{Name: "Child", Superclass: parentClass}
	childVT := NewVTable(childClass, parentVT)
	childMethod := NewMethod0("childMethod", func(vm interface{}, receiver Value) Value {
		return FromSmallInt(200)
	})
	childVT.AddMethod(1, childMethod)

	// Child should find parent's method through inheritance
	if got := childVT.Lookup(0); got != parentMethod {
		t.Error("child should inherit parent's method at selector 0")
	}

	// Child should find its own method
	if got := childVT.Lookup(1); got != childMethod {
		t.Error("child should have its own method at selector 1")
	}

	// Parent should not see child's method
	if got := parentVT.Lookup(1); got != nil {
		t.Error("parent should not have child's method")
	}
}

func TestVTableOverride(t *testing.T) {
	parentClass := &Class{Name: "Parent"}
	parentVT := NewVTable(parentClass, nil)
	parentMethod := NewMethod0("shared", func(vm interface{}, receiver Value) Value {
		return FromSmallInt(1)
	})
	parentVT.AddMethod(0, parentMethod)

	childClass := &Class{Name: "Child"}
	childVT := NewVTable(childClass, parentVT)
	childMethod := NewMethod0("shared", func(vm interface{}, receiver Value) Value {
		return FromSmallInt(2)
	})
	childVT.AddMethod(0, childMethod) // Override parent's method

	// Child should use its own overridden method
	if got := childVT.Lookup(0); got != childMethod {
		t.Error("child should use overridden method")
	}

	// Parent should still use its own method
	if got := parentVT.Lookup(0); got != parentMethod {
		t.Error("parent should still use its own method")
	}
}

func TestVTableLookupLocal(t *testing.T) {
	parentVT := NewVTable(&Class{Name: "Parent"}, nil)
	parentVT.AddMethod(0, NewMethod0("m", func(vm interface{}, r Value) Value { return Nil }))

	childVT := NewVTable(&Class{Name: "Child"}, parentVT)

	// LookupLocal should not find parent's method
	if got := childVT.LookupLocal(0); got != nil {
		t.Error("LookupLocal should not find parent's method")
	}

	// Lookup should find it through inheritance
	if got := childVT.Lookup(0); got == nil {
		t.Error("Lookup should find parent's method through inheritance")
	}
}

func TestVTableRemoveMethod(t *testing.T) {
	vt := NewVTable(&Class{Name: "Test"}, nil)
	m := NewMethod0("test", func(vm interface{}, r Value) Value { return Nil })
	vt.AddMethod(0, m)

	if vt.Lookup(0) == nil {
		t.Fatal("method should exist before removal")
	}

	vt.RemoveMethod(0)
	if vt.Lookup(0) != nil {
		t.Error("method should be nil after removal")
	}
}

func TestVTableHasMethod(t *testing.T) {
	parentVT := NewVTable(&Class{Name: "Parent"}, nil)
	parentVT.AddMethod(0, NewMethod0("m", func(vm interface{}, r Value) Value { return Nil }))

	childVT := NewVTable(&Class{Name: "Child"}, parentVT)

	// HasMethod only checks local
	if childVT.HasMethod(0) {
		t.Error("HasMethod should be false for inherited method")
	}
	if !parentVT.HasMethod(0) {
		t.Error("HasMethod should be true for local method")
	}
}

func TestVTableMethodCount(t *testing.T) {
	vt := NewVTable(&Class{Name: "Test"}, nil)
	if vt.MethodCount() != 0 {
		t.Error("empty vtable should have 0 method slots")
	}

	vt.AddMethod(5, NewMethod0("m", func(vm interface{}, r Value) Value { return Nil }))
	if vt.MethodCount() != 6 { // 0-5 = 6 slots
		t.Errorf("MethodCount() = %d, want 6", vt.MethodCount())
	}
}

func TestVTableLocalMethods(t *testing.T) {
	vt := NewVTable(&Class{Name: "Test"}, nil)
	m1 := NewMethod0("m1", func(vm interface{}, r Value) Value { return Nil })
	m2 := NewMethod0("m2", func(vm interface{}, r Value) Value { return Nil })
	vt.AddMethod(0, m1)
	vt.AddMethod(5, m2)

	local := vt.LocalMethods()
	if len(local) != 2 {
		t.Errorf("LocalMethods has %d entries, want 2", len(local))
	}
	if local[0] != m1 || local[5] != m2 {
		t.Error("LocalMethods returned wrong methods")
	}
}

// ---------------------------------------------------------------------------
// Method tests
// ---------------------------------------------------------------------------

func TestMethod0(t *testing.T) {
	called := false
	m := NewMethod0("test", func(vm interface{}, receiver Value) Value {
		called = true
		return FromSmallInt(42)
	})

	result := m.Invoke(nil, Nil, nil)
	if !called {
		t.Error("method should have been called")
	}
	if result.SmallInt() != 42 {
		t.Error("wrong return value")
	}
	if MethodName(m) != "test" {
		t.Errorf("Name() = %q, want %q", MethodName(m), "test")
	}
	if MethodArity(m) != 0 {
		t.Errorf("Arity() = %d, want 0", MethodArity(m))
	}
}

func TestMethod1(t *testing.T) {
	m := NewMethod1("add1", func(vm interface{}, receiver, arg1 Value) Value {
		return FromSmallInt(arg1.SmallInt() + 1)
	})

	args := []Value{FromSmallInt(10)}
	result := m.Invoke(nil, Nil, args)
	if result.SmallInt() != 11 {
		t.Errorf("result = %d, want 11", result.SmallInt())
	}
	if MethodArity(m) != 1 {
		t.Errorf("Arity() = %d, want 1", MethodArity(m))
	}
}

func TestMethod2(t *testing.T) {
	m := NewMethod2("add", func(vm interface{}, receiver, arg1, arg2 Value) Value {
		return FromSmallInt(arg1.SmallInt() + arg2.SmallInt())
	})

	args := []Value{FromSmallInt(3), FromSmallInt(4)}
	result := m.Invoke(nil, Nil, args)
	if result.SmallInt() != 7 {
		t.Errorf("result = %d, want 7", result.SmallInt())
	}
	if MethodArity(m) != 2 {
		t.Errorf("Arity() = %d, want 2", MethodArity(m))
	}
}

func TestMethod3(t *testing.T) {
	m := NewMethod3("sum3", func(vm interface{}, receiver, arg1, arg2, arg3 Value) Value {
		return FromSmallInt(arg1.SmallInt() + arg2.SmallInt() + arg3.SmallInt())
	})

	args := []Value{FromSmallInt(1), FromSmallInt(2), FromSmallInt(3)}
	result := m.Invoke(nil, Nil, args)
	if result.SmallInt() != 6 {
		t.Errorf("result = %d, want 6", result.SmallInt())
	}
	if MethodArity(m) != 3 {
		t.Errorf("Arity() = %d, want 3", MethodArity(m))
	}
}

func TestPrimitiveMethod(t *testing.T) {
	m := NewPrimitiveMethod("varargs", func(vm interface{}, receiver Value, args []Value) Value {
		sum := int64(0)
		for _, a := range args {
			sum += a.SmallInt()
		}
		return FromSmallInt(sum)
	})

	args := []Value{FromSmallInt(1), FromSmallInt(2), FromSmallInt(3), FromSmallInt(4)}
	result := m.Invoke(nil, Nil, args)
	if result.SmallInt() != 10 {
		t.Errorf("result = %d, want 10", result.SmallInt())
	}
	if MethodArity(m) != -1 {
		t.Errorf("Arity() = %d, want -1 (varargs)", MethodArity(m))
	}
}

func TestMethodWithReceiver(t *testing.T) {
	// Create an object and a method that accesses its slots
	obj := NewObject(nil, 2)
	obj.SetSlot(0, FromSmallInt(100))

	m := NewMethod0("getValue", func(vm interface{}, receiver Value) Value {
		o := MustObjectFromValue(receiver)
		return o.GetSlot(0)
	})

	result := m.Invoke(nil, obj.ToValue(), nil)
	if result.SmallInt() != 100 {
		t.Errorf("result = %d, want 100", result.SmallInt())
	}
}

// ---------------------------------------------------------------------------
// Integration test: Full dispatch simulation
// ---------------------------------------------------------------------------

func TestFullDispatchSimulation(t *testing.T) {
	// Create a selector table
	st := NewSelectorTable()
	atSelector := st.Intern("at:")
	atPutSelector := st.Intern("at:put:")
	sizeSelector := st.Intern("size")

	// Create a simple "Array" class
	arrayClass := &Class{Name: "Array", NumSlots: 1} // slot 0 = size
	arrayVT := NewVTable(arrayClass, nil)

	// Add methods
	arrayVT.AddMethod(sizeSelector, NewMethod0("size", func(vm interface{}, receiver Value) Value {
		obj := MustObjectFromValue(receiver)
		return obj.GetSlot(0) // Return size from slot 0
	}))

	arrayVT.AddMethod(atSelector, NewMethod1("at:", func(vm interface{}, receiver, index Value) Value {
		// Simplified: just return the index + 1 for testing
		return FromSmallInt(index.SmallInt() + 1)
	}))

	arrayVT.AddMethod(atPutSelector, NewMethod2("at:put:", func(vm interface{}, receiver, index, value Value) Value {
		// Simplified: return the value
		return value
	}))

	// Create an "array" object
	arr := NewObject(arrayVT, 1)
	arr.SetSlot(0, FromSmallInt(10)) // size = 10

	// Simulate dispatch
	sizeMethod := arrayVT.Lookup(sizeSelector)
	if sizeMethod == nil {
		t.Fatal("size method not found")
	}
	sizeResult := sizeMethod.Invoke(nil, arr.ToValue(), nil)
	if sizeResult.SmallInt() != 10 {
		t.Errorf("size = %d, want 10", sizeResult.SmallInt())
	}

	atMethod := arrayVT.Lookup(atSelector)
	if atMethod == nil {
		t.Fatal("at: method not found")
	}
	atResult := atMethod.Invoke(nil, arr.ToValue(), []Value{FromSmallInt(5)})
	if atResult.SmallInt() != 6 {
		t.Errorf("at: 5 = %d, want 6", atResult.SmallInt())
	}
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// VTable flattening tests
// ---------------------------------------------------------------------------

func TestVTableFlattenBasic(t *testing.T) {
	// After Lookup, flat array should be populated
	vt := NewVTable(&Class{Name: "Test"}, nil)
	m := NewMethod0("m", func(vm interface{}, r Value) Value { return Nil })
	vt.AddMethod(3, m)

	// First lookup triggers rebuild
	got := vt.Lookup(3)
	if got != m {
		t.Error("Lookup should return the method after flatten")
	}
	// flat should now be non-nil
	if vt.flat == nil {
		t.Error("flat should be populated after Lookup")
	}
	if vt.dirty {
		t.Error("dirty should be false after rebuild")
	}
}

func TestVTableFlattenInherited(t *testing.T) {
	grandparent := NewVTable(&Class{Name: "GP"}, nil)
	gm := NewMethod0("gm", func(vm interface{}, r Value) Value { return FromSmallInt(1) })
	grandparent.AddMethod(0, gm)

	parent := NewVTable(&Class{Name: "P"}, grandparent)
	pm := NewMethod0("pm", func(vm interface{}, r Value) Value { return FromSmallInt(2) })
	parent.AddMethod(1, pm)

	child := NewVTable(&Class{Name: "C"}, parent)
	cm := NewMethod0("cm", func(vm interface{}, r Value) Value { return FromSmallInt(3) })
	child.AddMethod(2, cm)

	// Child should see all three methods
	if got := child.Lookup(0); got != gm {
		t.Error("child should inherit grandparent method")
	}
	if got := child.Lookup(1); got != pm {
		t.Error("child should inherit parent method")
	}
	if got := child.Lookup(2); got != cm {
		t.Error("child should have its own method")
	}
}

func TestVTableFlattenOverride(t *testing.T) {
	parent := NewVTable(&Class{Name: "P"}, nil)
	pm := NewMethod0("m", func(vm interface{}, r Value) Value { return FromSmallInt(1) })
	parent.AddMethod(0, pm)

	child := NewVTable(&Class{Name: "C"}, parent)
	cm := NewMethod0("m", func(vm interface{}, r Value) Value { return FromSmallInt(2) })
	child.AddMethod(0, cm)

	// Child override should win
	if got := child.Lookup(0); got != cm {
		t.Error("child override should take precedence")
	}
	// Parent unchanged
	if got := parent.Lookup(0); got != pm {
		t.Error("parent should keep its own method")
	}
}

func TestVTableFlattenAddMethodInvalidates(t *testing.T) {
	vt := NewVTable(&Class{Name: "Test"}, nil)
	m1 := NewMethod0("m1", func(vm interface{}, r Value) Value { return FromSmallInt(1) })
	vt.AddMethod(0, m1)

	// Force rebuild
	_ = vt.Lookup(0)
	if vt.flat == nil {
		t.Fatal("flat should be populated")
	}

	// Add a new method — should invalidate
	m2 := NewMethod0("m2", func(vm interface{}, r Value) Value { return FromSmallInt(2) })
	vt.AddMethod(1, m2)
	if vt.flat != nil {
		t.Error("flat should be nil after AddMethod")
	}

	// Next lookup should rebuild and find both
	if got := vt.Lookup(0); got != m1 {
		t.Error("should still find m1 after rebuild")
	}
	if got := vt.Lookup(1); got != m2 {
		t.Error("should find m2 after rebuild")
	}
}

func TestVTableFlattenSetParentInvalidates(t *testing.T) {
	parent := NewVTable(&Class{Name: "P"}, nil)
	pm := NewMethod0("pm", func(vm interface{}, r Value) Value { return FromSmallInt(1) })
	parent.AddMethod(0, pm)

	child := NewVTable(&Class{Name: "C"}, nil)
	cm := NewMethod0("cm", func(vm interface{}, r Value) Value { return FromSmallInt(2) })
	child.AddMethod(1, cm)

	// Force rebuild with no parent
	if got := child.Lookup(0); got != nil {
		t.Error("should not find parent method before SetParent")
	}

	// Set parent — should invalidate
	child.SetParent(parent)
	if vt := child; vt.flat != nil {
		t.Error("flat should be nil after SetParent")
	}

	// Now should find parent's method
	if got := child.Lookup(0); got != pm {
		t.Error("should find parent method after SetParent")
	}
}

func TestVTableFlattenRemoveMethodFallsBack(t *testing.T) {
	parent := NewVTable(&Class{Name: "P"}, nil)
	pm := NewMethod0("pm", func(vm interface{}, r Value) Value { return FromSmallInt(1) })
	parent.AddMethod(0, pm)

	child := NewVTable(&Class{Name: "C"}, parent)
	cm := NewMethod0("cm", func(vm interface{}, r Value) Value { return FromSmallInt(2) })
	child.AddMethod(0, cm) // Override parent

	// Should see child's override
	if got := child.Lookup(0); got != cm {
		t.Error("should see child override")
	}

	// Remove child's override — should fall back to parent
	child.RemoveMethod(0)
	if got := child.Lookup(0); got != pm {
		t.Error("after RemoveMethod, should fall back to parent method")
	}
}

func TestVTableFlattenDeepChain(t *testing.T) {
	// 5-level deep inheritance chain
	var prev *VTable
	methods := make([]Method, 5)
	for i := 0; i < 5; i++ {
		vt := NewVTable(&Class{Name: string(rune('A' + i))}, prev)
		methods[i] = NewMethod0("m", func(vm interface{}, r Value) Value { return Nil })
		vt.AddMethod(i, methods[i])
		prev = vt
	}

	// Bottom of chain should see all methods
	for i := 0; i < 5; i++ {
		if got := prev.Lookup(i); got != methods[i] {
			t.Errorf("deep chain: Lookup(%d) should find method from level %d", i, i)
		}
	}
}

func TestVTableLocalMethodsUnaffectedByFlatten(t *testing.T) {
	parent := NewVTable(&Class{Name: "P"}, nil)
	parent.AddMethod(0, NewMethod0("pm", func(vm interface{}, r Value) Value { return Nil }))

	child := NewVTable(&Class{Name: "C"}, parent)
	cm := NewMethod0("cm", func(vm interface{}, r Value) Value { return Nil })
	child.AddMethod(1, cm)

	// Force flatten
	_ = child.Lookup(0)

	// LocalMethods should only return child's method
	local := child.LocalMethods()
	if len(local) != 1 {
		t.Errorf("LocalMethods should have 1 entry, got %d", len(local))
	}
	if local[1] != cm {
		t.Error("LocalMethods should return child's local method")
	}
	if _, ok := local[0]; ok {
		t.Error("LocalMethods should NOT contain inherited parent method")
	}
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

func BenchmarkSelectorIntern(b *testing.B) {
	st := NewSelectorTable()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		st.Intern("testSelector")
	}
}

func BenchmarkSelectorLookup(b *testing.B) {
	st := NewSelectorTable()
	st.Intern("testSelector")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		st.Lookup("testSelector")
	}
}

func BenchmarkVTableLookupDirect(b *testing.B) {
	vt := NewVTable(&Class{Name: "Test"}, nil)
	vt.AddMethod(5, NewMethod0("m", func(vm interface{}, r Value) Value { return Nil }))
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = vt.Lookup(5)
	}
}

func BenchmarkVTableLookupInherited(b *testing.B) {
	parentVT := NewVTable(&Class{Name: "Parent"}, nil)
	parentVT.AddMethod(5, NewMethod0("m", func(vm interface{}, r Value) Value { return Nil }))
	childVT := NewVTable(&Class{Name: "Child"}, parentVT)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = childVT.Lookup(5)
	}
}

func BenchmarkMethod0Invoke(b *testing.B) {
	m := NewMethod0("test", func(vm interface{}, receiver Value) Value {
		return Nil
	})
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		m.Invoke(nil, Nil, nil)
	}
}

func BenchmarkMethod1Invoke(b *testing.B) {
	m := NewMethod1("test", func(vm interface{}, receiver, arg1 Value) Value {
		return arg1
	})
	args := []Value{FromSmallInt(42)}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		m.Invoke(nil, Nil, args)
	}
}
