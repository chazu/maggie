package vm

import (
	"sync"
	"testing"
)

// ---------------------------------------------------------------------------
// Class creation tests
// ---------------------------------------------------------------------------

func TestNewClass(t *testing.T) {
	c := NewClass("Object", nil)
	if c == nil {
		t.Fatal("NewClass returned nil")
	}
	if c.Name != "Object" {
		t.Errorf("Name = %q, want %q", c.Name, "Object")
	}
	if c.Superclass != nil {
		t.Error("root class should have nil superclass")
	}
	if c.VTable == nil {
		t.Error("VTable should be created")
	}
	if c.VTable.Class() != c {
		t.Error("VTable.Class() should return the class")
	}
	if c.NumSlots != 0 {
		t.Errorf("NumSlots = %d, want 0", c.NumSlots)
	}
}

func TestNewClassWithSuperclass(t *testing.T) {
	object := NewClass("Object", nil)
	point := NewClass("Point", object)

	if point.Superclass != object {
		t.Error("superclass should be Object")
	}
	if point.VTable.Parent() != object.VTable {
		t.Error("VTable parent should be Object's vtable")
	}
}

func TestNewClassWithInstVars(t *testing.T) {
	object := NewClass("Object", nil)
	point := NewClassWithInstVars("Point", object, []string{"x", "y"})

	if len(point.InstVars) != 2 {
		t.Errorf("InstVars count = %d, want 2", len(point.InstVars))
	}
	if point.NumSlots != 2 {
		t.Errorf("NumSlots = %d, want 2", point.NumSlots)
	}

	// Subclass inherits slot count
	colorPoint := NewClassWithInstVars("ColorPoint", point, []string{"color"})
	if colorPoint.NumSlots != 3 {
		t.Errorf("ColorPoint.NumSlots = %d, want 3", colorPoint.NumSlots)
	}
}

func TestNewClassInNamespace(t *testing.T) {
	c := NewClassInNamespace("Graphics", "Point", nil)
	if c.Namespace != "Graphics" {
		t.Errorf("Namespace = %q, want %q", c.Namespace, "Graphics")
	}
	if c.Name != "Point" {
		t.Errorf("Name = %q, want %q", c.Name, "Point")
	}
}

// ---------------------------------------------------------------------------
// Instance variable tests
// ---------------------------------------------------------------------------

func TestInstVarIndex(t *testing.T) {
	object := NewClass("Object", nil)
	point := NewClassWithInstVars("Point", object, []string{"x", "y"})

	if idx := point.InstVarIndex("x"); idx != 0 {
		t.Errorf("InstVarIndex(x) = %d, want 0", idx)
	}
	if idx := point.InstVarIndex("y"); idx != 1 {
		t.Errorf("InstVarIndex(y) = %d, want 1", idx)
	}
	if idx := point.InstVarIndex("z"); idx != -1 {
		t.Errorf("InstVarIndex(z) = %d, want -1", idx)
	}
}

func TestInstVarIndexInherited(t *testing.T) {
	object := NewClass("Object", nil)
	point := NewClassWithInstVars("Point", object, []string{"x", "y"})
	colorPoint := NewClassWithInstVars("ColorPoint", point, []string{"color"})

	// Inherited from Point
	if idx := colorPoint.InstVarIndex("x"); idx != 0 {
		t.Errorf("InstVarIndex(x) = %d, want 0", idx)
	}
	if idx := colorPoint.InstVarIndex("y"); idx != 1 {
		t.Errorf("InstVarIndex(y) = %d, want 1", idx)
	}
	// Own instance variable
	if idx := colorPoint.InstVarIndex("color"); idx != 2 {
		t.Errorf("InstVarIndex(color) = %d, want 2", idx)
	}
}

func TestAllInstVarNames(t *testing.T) {
	object := NewClass("Object", nil)
	point := NewClassWithInstVars("Point", object, []string{"x", "y"})
	colorPoint := NewClassWithInstVars("ColorPoint", point, []string{"color"})

	names := colorPoint.AllInstVarNames()
	expected := []string{"x", "y", "color"}
	if len(names) != len(expected) {
		t.Fatalf("AllInstVarNames() length = %d, want %d", len(names), len(expected))
	}
	for i, name := range names {
		if name != expected[i] {
			t.Errorf("names[%d] = %q, want %q", i, name, expected[i])
		}
	}
}

// ---------------------------------------------------------------------------
// Class hierarchy tests
// ---------------------------------------------------------------------------

func TestIsSubclassOf(t *testing.T) {
	object := NewClass("Object", nil)
	point := NewClass("Point", object)
	colorPoint := NewClass("ColorPoint", point)
	rect := NewClass("Rectangle", object)

	if !point.IsSubclassOf(object) {
		t.Error("Point should be subclass of Object")
	}
	if !colorPoint.IsSubclassOf(object) {
		t.Error("ColorPoint should be subclass of Object")
	}
	if !colorPoint.IsSubclassOf(point) {
		t.Error("ColorPoint should be subclass of Point")
	}
	if !colorPoint.IsSubclassOf(colorPoint) {
		t.Error("ColorPoint should be subclass of itself")
	}
	if colorPoint.IsSubclassOf(rect) {
		t.Error("ColorPoint should not be subclass of Rectangle")
	}
}

func TestIsSuperclassOf(t *testing.T) {
	object := NewClass("Object", nil)
	point := NewClass("Point", object)
	colorPoint := NewClass("ColorPoint", point)

	if !object.IsSuperclassOf(point) {
		t.Error("Object should be superclass of Point")
	}
	if !object.IsSuperclassOf(colorPoint) {
		t.Error("Object should be superclass of ColorPoint")
	}
	if !point.IsSuperclassOf(colorPoint) {
		t.Error("Point should be superclass of ColorPoint")
	}
	if point.IsSuperclassOf(object) {
		t.Error("Point should not be superclass of Object")
	}
}

func TestSuperclasses(t *testing.T) {
	object := NewClass("Object", nil)
	point := NewClass("Point", object)
	colorPoint := NewClass("ColorPoint", point)

	supers := colorPoint.Superclasses()
	if len(supers) != 2 {
		t.Fatalf("Superclasses() length = %d, want 2", len(supers))
	}
	if supers[0] != point {
		t.Error("first superclass should be Point")
	}
	if supers[1] != object {
		t.Error("second superclass should be Object")
	}
}

func TestDepth(t *testing.T) {
	object := NewClass("Object", nil)
	point := NewClass("Point", object)
	colorPoint := NewClass("ColorPoint", point)

	if object.Depth() != 0 {
		t.Errorf("Object.Depth() = %d, want 0", object.Depth())
	}
	if point.Depth() != 1 {
		t.Errorf("Point.Depth() = %d, want 1", point.Depth())
	}
	if colorPoint.Depth() != 2 {
		t.Errorf("ColorPoint.Depth() = %d, want 2", colorPoint.Depth())
	}
}

// ---------------------------------------------------------------------------
// Instance creation tests
// ---------------------------------------------------------------------------

func TestNewInstance(t *testing.T) {
	point := NewClassWithInstVars("Point", nil, []string{"x", "y"})
	obj := point.NewInstance()

	if obj == nil {
		t.Fatal("NewInstance returned nil")
	}
	if obj.VTablePtr() != point.VTable {
		t.Error("instance should have class's vtable")
	}
	if obj.NumSlots() < 2 {
		t.Errorf("NumSlots() = %d, want >= 2", obj.NumSlots())
	}
}

func TestNewInstanceWithSlots(t *testing.T) {
	point := NewClassWithInstVars("Point", nil, []string{"x", "y"})
	slots := []Value{FromSmallInt(10), FromSmallInt(20)}
	obj := point.NewInstanceWithSlots(slots)

	if obj.GetSlot(0).SmallInt() != 10 {
		t.Error("slot 0 should be 10")
	}
	if obj.GetSlot(1).SmallInt() != 20 {
		t.Error("slot 1 should be 20")
	}
}

// ---------------------------------------------------------------------------
// Method registration tests
// ---------------------------------------------------------------------------

func TestAddMethod(t *testing.T) {
	selectors := NewSelectorTable()
	point := NewClass("Point", nil)

	// Add a method
	point.AddMethod0(selectors, "x", func(_ interface{}, recv Value) Value {
		return FromSmallInt(42)
	})

	// Lookup should find it
	method := point.LookupMethod(selectors, "x")
	if method == nil {
		t.Fatal("LookupMethod returned nil")
	}

	// Invoke should work
	result := method.Invoke(nil, Nil, nil)
	if result.SmallInt() != 42 {
		t.Errorf("method result = %d, want 42", result.SmallInt())
	}
}

func TestAddMethodWithArgs(t *testing.T) {
	selectors := NewSelectorTable()
	point := NewClass("Point", nil)

	point.AddMethod2(selectors, "at:put:", func(_ interface{}, recv Value, key, val Value) Value {
		return val // Return the value for testing
	})

	method := point.LookupMethod(selectors, "at:put:")
	if method == nil {
		t.Fatal("LookupMethod returned nil")
	}

	result := method.Invoke(nil, Nil, []Value{FromSmallInt(0), FromSmallInt(99)})
	if result.SmallInt() != 99 {
		t.Errorf("method result = %d, want 99", result.SmallInt())
	}
}

func TestHasMethod(t *testing.T) {
	selectors := NewSelectorTable()
	object := NewClass("Object", nil)
	point := NewClass("Point", object)

	object.AddMethod0(selectors, "class", func(_ interface{}, recv Value) Value {
		return Nil
	})

	if !object.HasMethod(selectors, "class") {
		t.Error("Object should have 'class' method")
	}
	if point.HasMethod(selectors, "class") {
		t.Error("Point should not have 'class' method locally")
	}
	// But lookup should find it via inheritance
	if point.LookupMethod(selectors, "class") == nil {
		t.Error("Point should find 'class' method via inheritance")
	}
}

func TestMethodInheritance(t *testing.T) {
	selectors := NewSelectorTable()
	object := NewClass("Object", nil)
	point := NewClass("Point", object)
	colorPoint := NewClass("ColorPoint", point)

	// Add method on Object
	object.AddMethod0(selectors, "isNil", func(_ interface{}, recv Value) Value {
		return False
	})

	// All classes should find it
	for _, c := range []*Class{object, point, colorPoint} {
		method := c.LookupMethod(selectors, "isNil")
		if method == nil {
			t.Errorf("%s should find isNil method", c.Name)
		}
	}
}

func TestMethodOverride(t *testing.T) {
	selectors := NewSelectorTable()
	object := NewClass("Object", nil)
	point := NewClass("Point", object)

	// Add method on Object
	object.AddMethod0(selectors, "name", func(_ interface{}, recv Value) Value {
		return FromSmallInt(1)
	})

	// Override on Point
	point.AddMethod0(selectors, "name", func(_ interface{}, recv Value) Value {
		return FromSmallInt(2)
	})

	// Object returns 1
	objMethod := object.LookupMethod(selectors, "name")
	if objMethod.Invoke(nil, Nil, nil).SmallInt() != 1 {
		t.Error("Object.name should return 1")
	}

	// Point returns 2
	pointMethod := point.LookupMethod(selectors, "name")
	if pointMethod.Invoke(nil, Nil, nil).SmallInt() != 2 {
		t.Error("Point.name should return 2")
	}
}

// ---------------------------------------------------------------------------
// Full qualified name tests
// ---------------------------------------------------------------------------

func TestFullName(t *testing.T) {
	c := NewClass("Point", nil)
	if c.FullName() != "Point" {
		t.Errorf("FullName() = %q, want %q", c.FullName(), "Point")
	}

	c2 := NewClassInNamespace("Graphics", "Point", nil)
	if c2.FullName() != "Graphics::Point" {
		t.Errorf("FullName() = %q, want %q", c2.FullName(), "Graphics::Point")
	}
}

func TestString(t *testing.T) {
	c := NewClassInNamespace("Graphics", "Point", nil)
	if c.String() != "Graphics::Point" {
		t.Errorf("String() = %q, want %q", c.String(), "Graphics::Point")
	}
}

// ---------------------------------------------------------------------------
// ClassTable tests
// ---------------------------------------------------------------------------

func TestClassTableRegister(t *testing.T) {
	ct := NewClassTable()
	c := NewClass("Object", nil)

	old := ct.Register(c)
	if old != nil {
		t.Error("first registration should return nil")
	}

	if !ct.Has("Object") {
		t.Error("Has should return true")
	}
}

func TestClassTableLookup(t *testing.T) {
	ct := NewClassTable()
	c := NewClass("Point", nil)
	ct.Register(c)

	found := ct.Lookup("Point")
	if found != c {
		t.Error("Lookup should return the registered class")
	}

	notFound := ct.Lookup("NoSuchClass")
	if notFound != nil {
		t.Error("Lookup for missing class should return nil")
	}
}

func TestClassTableLookupInNamespace(t *testing.T) {
	ct := NewClassTable()
	c1 := NewClass("Point", nil)
	c2 := NewClassInNamespace("Graphics", "Point", nil)

	ct.Register(c1)
	ct.Register(c2)

	// Default namespace
	found1 := ct.LookupInNamespace("", "Point")
	if found1 != c1 {
		t.Error("should find Point in default namespace")
	}

	// Specific namespace
	found2 := ct.LookupInNamespace("Graphics", "Point")
	if found2 != c2 {
		t.Error("should find Graphics::Point")
	}
}

func TestClassTableAll(t *testing.T) {
	ct := NewClassTable()
	c1 := NewClass("Object", nil)
	c2 := NewClass("Point", nil)
	c3 := NewClass("Array", nil)

	ct.Register(c1)
	ct.Register(c2)
	ct.Register(c3)

	all := ct.All()
	if len(all) != 3 {
		t.Errorf("All() length = %d, want 3", len(all))
	}
}

func TestClassTableLen(t *testing.T) {
	ct := NewClassTable()
	if ct.Len() != 0 {
		t.Errorf("Len() = %d, want 0", ct.Len())
	}

	ct.Register(NewClass("A", nil))
	ct.Register(NewClass("B", nil))

	if ct.Len() != 2 {
		t.Errorf("Len() = %d, want 2", ct.Len())
	}
}

func TestClassTableReplace(t *testing.T) {
	ct := NewClassTable()
	c1 := NewClass("Point", nil)
	c2 := NewClass("Point", nil) // Same name, different object

	ct.Register(c1)
	old := ct.Register(c2)

	if old != c1 {
		t.Error("replacement should return old class")
	}
	if ct.Lookup("Point") != c2 {
		t.Error("lookup should return new class")
	}
}

func TestClassTableConcurrency(t *testing.T) {
	ct := NewClassTable()
	var wg sync.WaitGroup

	// Concurrent registrations
	for i := 0; i < 100; i++ {
		wg.Add(1)
		go func(n int) {
			defer wg.Done()
			c := NewClass("Class"+string(rune('A'+n%26)), nil)
			ct.Register(c)
		}(i)
	}

	// Concurrent lookups
	for i := 0; i < 100; i++ {
		wg.Add(1)
		go func(n int) {
			defer wg.Done()
			_ = ct.Lookup("Class" + string(rune('A'+n%26)))
		}(i)
	}

	wg.Wait()

	// Just verify no crash and some classes registered
	if ct.Len() == 0 {
		t.Error("should have registered some classes")
	}
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

func BenchmarkNewClass(b *testing.B) {
	for i := 0; i < b.N; i++ {
		_ = NewClass("Point", nil)
	}
}

func BenchmarkNewClassWithInstVars(b *testing.B) {
	instVars := []string{"x", "y", "z"}
	for i := 0; i < b.N; i++ {
		_ = NewClassWithInstVars("Point", nil, instVars)
	}
}

func BenchmarkNewInstance(b *testing.B) {
	point := NewClassWithInstVars("Point", nil, []string{"x", "y"})
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = point.NewInstance()
	}
}

func BenchmarkInstVarIndex(b *testing.B) {
	colorPoint := NewClassWithInstVars("ColorPoint",
		NewClassWithInstVars("Point", nil, []string{"x", "y"}),
		[]string{"color"})
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = colorPoint.InstVarIndex("color")
	}
}

func BenchmarkClassTableLookup(b *testing.B) {
	ct := NewClassTable()
	for i := 0; i < 100; i++ {
		ct.Register(NewClass("Class"+string(rune(i)), nil))
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = ct.Lookup("Class" + string(rune(50)))
	}
}

func BenchmarkMethodLookup(b *testing.B) {
	selectors := NewSelectorTable()
	object := NewClass("Object", nil)
	object.AddMethod0(selectors, "class", func(_ interface{}, recv Value) Value {
		return Nil
	})

	point := NewClass("Point", object)
	selectorID := selectors.Lookup("class")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = point.VTable.Lookup(selectorID)
	}
}

// ---------------------------------------------------------------------------
// Class variable tests
// ---------------------------------------------------------------------------

func TestClassVarBasic(t *testing.T) {
	// Create a class with class variables
	counter := NewClass("Counter", nil)
	counter.ClassVars = []string{"count"}

	// Initially nil
	val := counter.GetClassVar("count")
	if val != Nil {
		t.Errorf("initial class var should be Nil, got %v", val)
	}

	// Set and get
	counter.SetClassVar("count", FromSmallInt(42))
	val = counter.GetClassVar("count")
	if val.SmallInt() != 42 {
		t.Errorf("class var should be 42, got %d", val.SmallInt())
	}

	// Update
	counter.SetClassVar("count", FromSmallInt(100))
	val = counter.GetClassVar("count")
	if val.SmallInt() != 100 {
		t.Errorf("class var should be 100, got %d", val.SmallInt())
	}
}

func TestClassVarInheritance(t *testing.T) {
	// Create class hierarchy with class variables
	object := NewClass("Object", nil)
	animal := NewClass("Animal", object)
	animal.ClassVars = []string{"population"}
	dog := NewClass("Dog", animal)

	// Set via parent class
	animal.SetClassVar("population", FromSmallInt(1000))

	// Subclass should see the same value
	val := dog.GetClassVar("population")
	if val.SmallInt() != 1000 {
		t.Errorf("subclass should see parent's class var, got %d", val.SmallInt())
	}

	// Setting via subclass should update the same storage
	dog.SetClassVar("population", FromSmallInt(2000))
	val = animal.GetClassVar("population")
	if val.SmallInt() != 2000 {
		t.Errorf("parent should see updated class var, got %d", val.SmallInt())
	}
}

func TestClassVarIndex(t *testing.T) {
	object := NewClass("Object", nil)
	counter := NewClass("Counter", object)
	counter.ClassVars = []string{"count", "max"}

	if idx := counter.ClassVarIndex("count"); idx != 0 {
		t.Errorf("ClassVarIndex(count) = %d, want 0", idx)
	}
	if idx := counter.ClassVarIndex("max"); idx != 1 {
		t.Errorf("ClassVarIndex(max) = %d, want 1", idx)
	}
	if idx := counter.ClassVarIndex("notfound"); idx != -1 {
		t.Errorf("ClassVarIndex(notfound) = %d, want -1", idx)
	}
}

func TestHasClassVar(t *testing.T) {
	object := NewClass("Object", nil)
	counter := NewClass("Counter", object)
	counter.ClassVars = []string{"count"}
	subCounter := NewClass("SubCounter", counter)

	if !counter.HasClassVar("count") {
		t.Error("Counter should have 'count' class var")
	}
	if !subCounter.HasClassVar("count") {
		t.Error("SubCounter should have inherited 'count' class var")
	}
	if counter.HasClassVar("notfound") {
		t.Error("Counter should not have 'notfound' class var")
	}
}

func TestAllClassVarNames(t *testing.T) {
	object := NewClass("Object", nil)
	parent := NewClass("Parent", object)
	parent.ClassVars = []string{"a", "b"}
	child := NewClass("Child", parent)
	child.ClassVars = []string{"c"}

	names := child.AllClassVarNames()
	if len(names) != 3 {
		t.Fatalf("AllClassVarNames() length = %d, want 3", len(names))
	}
	// Should be inherited first, then own
	expected := []string{"a", "b", "c"}
	for i, name := range expected {
		if names[i] != name {
			t.Errorf("names[%d] = %q, want %q", i, names[i], name)
		}
	}
}

func TestClassVarOpcode(t *testing.T) {
	// Create interpreter with symbol table
	interp := NewInterpreter()
	interp.Symbols = NewSymbolTable()

	// Create class with class variables
	counter := NewClass("Counter", nil)
	counter.ClassVars = []string{"count"}
	interp.Classes.Register(counter)

	// Create symbol value for the class variable name
	countSymbolValue := interp.Symbols.SymbolValue("count")

	// Create a method that pushes the class variable
	// Bytecode: PUSH_CLASSVAR 0, RETURN_TOP
	b := NewCompiledMethodBuilder("getCount", 0)

	// Add the symbol as a literal
	litIdx := b.AddLiteral(countSymbolValue)

	b.Bytecode().EmitUint16(OpPushClassVar, uint16(litIdx))
	b.Bytecode().Emit(OpReturnTop)

	method := b.Build()
	method.SetClass(counter)

	// Set the class variable
	counter.SetClassVar("count", FromSmallInt(42))

	// Execute
	result := interp.Execute(method, Nil, nil)

	if result.SmallInt() != 42 {
		t.Errorf("expected 42, got %d", result.SmallInt())
	}
}

func TestClassVarStoreOpcode(t *testing.T) {
	// Create interpreter with symbol table
	interp := NewInterpreter()
	interp.Symbols = NewSymbolTable()

	// Create class with class variables
	counter := NewClass("Counter", nil)
	counter.ClassVars = []string{"count"}
	interp.Classes.Register(counter)

	// Create symbol value for the class variable name
	countSymbolValue := interp.Symbols.SymbolValue("count")

	// Create a method that stores into the class variable
	// Bytecode: PUSH_INT8 99, STORE_CLASSVAR 0, RETURN_TOP
	b := NewCompiledMethodBuilder("setCount", 0)

	// Add the symbol as a literal
	litIdx := b.AddLiteral(countSymbolValue)

	b.Bytecode().EmitInt8(OpPushInt8, 99)
	b.Bytecode().EmitUint16(OpStoreClassVar, uint16(litIdx))
	b.Bytecode().Emit(OpReturnTop)

	method := b.Build()
	method.SetClass(counter)

	// Execute
	result := interp.Execute(method, Nil, nil)

	// Return value should be 99 (store leaves value on stack)
	if result.SmallInt() != 99 {
		t.Errorf("expected return 99, got %d", result.SmallInt())
	}

	// Class variable should be updated
	val := counter.GetClassVar("count")
	if val.SmallInt() != 99 {
		t.Errorf("class var should be 99, got %d", val.SmallInt())
	}
}

// ---------------------------------------------------------------------------
// Method categories tests
// ---------------------------------------------------------------------------

func TestMethodCategory(t *testing.T) {
	// Create a method with a category
	method := NewCompiledMethod("add:", 1)
	method.SetCategory("arithmetic")

	if method.Category() != "arithmetic" {
		t.Errorf("Category() = %q, want %q", method.Category(), "arithmetic")
	}
}

func TestCompiledMethodBuilderCategory(t *testing.T) {
	builder := NewCompiledMethodBuilder("subtract:", 1)
	builder.SetCategory("arithmetic")
	method := builder.Build()

	if method.Category() != "arithmetic" {
		t.Errorf("Category() = %q, want %q", method.Category(), "arithmetic")
	}
}

func TestClassMethodCategories(t *testing.T) {
	selectors := NewSelectorTable()
	point := NewClass("Point", nil)

	// Add methods with categories
	method1 := NewCompiledMethod("x", 0)
	method1.SetCategory("accessing")
	method1.SetSelector(selectors.Intern("x"))
	point.VTable.AddMethod(method1.Selector(), method1)

	method2 := NewCompiledMethod("y", 0)
	method2.SetCategory("accessing")
	method2.SetSelector(selectors.Intern("y"))
	point.VTable.AddMethod(method2.Selector(), method2)

	method3 := NewCompiledMethod("isPoint", 0)
	method3.SetCategory("testing")
	method3.SetSelector(selectors.Intern("isPoint"))
	point.VTable.AddMethod(method3.Selector(), method3)

	categories := point.MethodCategories()
	if len(categories) != 2 {
		t.Errorf("MethodCategories() count = %d, want 2", len(categories))
	}

	// Check that both categories are present (order may vary)
	hasAccessing := false
	hasTesting := false
	for _, cat := range categories {
		if cat == "accessing" {
			hasAccessing = true
		}
		if cat == "testing" {
			hasTesting = true
		}
	}
	if !hasAccessing {
		t.Error("should have 'accessing' category")
	}
	if !hasTesting {
		t.Error("should have 'testing' category")
	}
}

func TestClassMethodsInCategory(t *testing.T) {
	selectors := NewSelectorTable()
	point := NewClass("Point", nil)

	// Add methods with categories
	method1 := NewCompiledMethod("x", 0)
	method1.SetCategory("accessing")
	method1.SetSelector(selectors.Intern("x"))
	point.VTable.AddMethod(method1.Selector(), method1)

	method2 := NewCompiledMethod("y", 0)
	method2.SetCategory("accessing")
	method2.SetSelector(selectors.Intern("y"))
	point.VTable.AddMethod(method2.Selector(), method2)

	method3 := NewCompiledMethod("isPoint", 0)
	method3.SetCategory("testing")
	method3.SetSelector(selectors.Intern("isPoint"))
	point.VTable.AddMethod(method3.Selector(), method3)

	// Get methods in 'accessing' category
	methods := point.MethodsInCategory("accessing")
	if len(methods) != 2 {
		t.Errorf("MethodsInCategory('accessing') count = %d, want 2", len(methods))
	}

	// Get methods in 'testing' category
	methods = point.MethodsInCategory("testing")
	if len(methods) != 1 {
		t.Errorf("MethodsInCategory('testing') count = %d, want 1", len(methods))
	}

	// Get methods in non-existent category
	methods = point.MethodsInCategory("nonexistent")
	if len(methods) != 0 {
		t.Errorf("MethodsInCategory('nonexistent') count = %d, want 0", len(methods))
	}
}

func TestClassMethodNamesInCategory(t *testing.T) {
	selectors := NewSelectorTable()
	point := NewClass("Point", nil)

	// Add methods with categories
	method1 := NewCompiledMethod("x", 0)
	method1.SetCategory("accessing")
	method1.SetSelector(selectors.Intern("x"))
	point.VTable.AddMethod(method1.Selector(), method1)

	method2 := NewCompiledMethod("y", 0)
	method2.SetCategory("accessing")
	method2.SetSelector(selectors.Intern("y"))
	point.VTable.AddMethod(method2.Selector(), method2)

	names := point.MethodNamesInCategory("accessing")
	if len(names) != 2 {
		t.Errorf("MethodNamesInCategory('accessing') count = %d, want 2", len(names))
	}

	// Check that both names are present (order may vary)
	hasX := false
	hasY := false
	for _, name := range names {
		if name == "x" {
			hasX = true
		}
		if name == "y" {
			hasY = true
		}
	}
	if !hasX || !hasY {
		t.Errorf("expected 'x' and 'y', got %v", names)
	}
}

func TestClassMethodNamed(t *testing.T) {
	selectors := NewSelectorTable()
	point := NewClass("Point", nil)

	method := NewCompiledMethod("x", 0)
	method.SetCategory("accessing")
	method.SetSelector(selectors.Intern("x"))
	point.VTable.AddMethod(method.Selector(), method)

	found := point.MethodNamed("x")
	if found == nil {
		t.Fatal("MethodNamed('x') should not be nil")
	}
	if found.Category() != "accessing" {
		t.Errorf("found method category = %q, want %q", found.Category(), "accessing")
	}

	notFound := point.MethodNamed("notexist")
	if notFound != nil {
		t.Error("MethodNamed('notexist') should be nil")
	}
}

func TestClassAllCompiledMethods(t *testing.T) {
	selectors := NewSelectorTable()
	point := NewClass("Point", nil)

	// Add compiled methods
	method1 := NewCompiledMethod("x", 0)
	method1.SetSelector(selectors.Intern("x"))
	point.VTable.AddMethod(method1.Selector(), method1)

	method2 := NewCompiledMethod("y", 0)
	method2.SetSelector(selectors.Intern("y"))
	point.VTable.AddMethod(method2.Selector(), method2)

	// Add a primitive method (not compiled)
	point.AddMethod0(selectors, "primMethod", func(_ interface{}, recv Value) Value {
		return Nil
	})

	compiledMethods := point.AllCompiledMethods()
	if len(compiledMethods) != 2 {
		t.Errorf("AllCompiledMethods() count = %d, want 2", len(compiledMethods))
	}
}

func TestClassAllMethodNames(t *testing.T) {
	selectors := NewSelectorTable()
	point := NewClass("Point", nil)

	method := NewCompiledMethod("x", 0)
	method.SetSelector(selectors.Intern("x"))
	point.VTable.AddMethod(method.Selector(), method)

	point.AddMethod0(selectors, "primMethod", func(_ interface{}, recv Value) Value {
		return Nil
	})

	names := point.AllMethodNames()
	if len(names) != 2 {
		t.Errorf("AllMethodNames() count = %d, want 2", len(names))
	}
}

func TestClassMethodCategoriesEmpty(t *testing.T) {
	point := NewClass("Point", nil)

	categories := point.MethodCategories()
	if len(categories) != 0 {
		t.Errorf("MethodCategories() for empty class = %d, want 0", len(categories))
	}
}

func TestClassMethodsWithoutCategory(t *testing.T) {
	selectors := NewSelectorTable()
	point := NewClass("Point", nil)

	// Add method without category
	method := NewCompiledMethod("x", 0)
	// Don't set category
	method.SetSelector(selectors.Intern("x"))
	point.VTable.AddMethod(method.Selector(), method)

	// Categories should not include empty string
	categories := point.MethodCategories()
	for _, cat := range categories {
		if cat == "" {
			t.Error("categories should not include empty string")
		}
	}

	// MethodsInCategory("") should still find the uncategorized method
	methods := point.MethodsInCategory("")
	if len(methods) != 1 {
		t.Errorf("MethodsInCategory('') count = %d, want 1", len(methods))
	}
}
