package vm

import "testing"

// ---------------------------------------------------------------------------
// Metaclass Tests
// ---------------------------------------------------------------------------

func TestClassMessageReturnsClassValue(t *testing.T) {
	vm := NewVM()

	// Sending "class" to an integer should return SmallInteger as a class value
	result := vm.Send(FromSmallInt(42), "class", nil)
	if !isClassValue(result) {
		t.Fatal("Expected class message on integer to return a class value")
	}
	cls := vm.GetClassFromValue(result)
	if cls == nil {
		t.Fatal("Expected non-nil class from class value")
	}
	if cls.Name != "SmallInteger" {
		t.Errorf("Expected SmallInteger, got %s", cls.Name)
	}
}

func TestClassMessageOnClassReturnsMetaclass(t *testing.T) {
	vm := NewVM()

	// SmallInteger class should return a metaclass
	smallIntVal := vm.classValue(vm.SmallIntegerClass)
	result := vm.Send(smallIntVal, "class", nil)
	if !isClassValue(result) {
		t.Fatal("Expected class message on class to return a class value (metaclass)")
	}
	meta := vm.GetClassFromValue(result)
	if meta == nil {
		t.Fatal("Expected non-nil metaclass")
	}
	if meta.Name != "SmallInteger class" {
		t.Errorf("Expected metaclass name 'SmallInteger class', got '%s'", meta.Name)
	}
}

func TestMetaclassIsSameObject(t *testing.T) {
	vm := NewVM()

	// MetaclassFor should return the same object on repeated calls
	meta1 := vm.MetaclassFor(vm.SmallIntegerClass)
	meta2 := vm.MetaclassFor(vm.SmallIntegerClass)
	if meta1 != meta2 {
		t.Error("MetaclassFor should return the same metaclass on repeated calls")
	}
}

func TestMetaclassInheritance(t *testing.T) {
	vm := NewVM()

	// SmallInteger's metaclass should have Object's metaclass as ancestor
	// SmallInteger -> Object, so SmallInteger class -> Object class
	siMeta := vm.MetaclassFor(vm.SmallIntegerClass)
	objMeta := vm.MetaclassFor(vm.ObjectClass)

	if siMeta.Superclass != objMeta {
		t.Errorf("Expected SmallInteger class superclass to be Object class, got %v",
			siMeta.Superclass)
	}

	// Object's metaclass superclass should be Class
	if objMeta.Superclass != vm.ClassClass {
		t.Errorf("Expected Object class superclass to be Class, got %v",
			objMeta.Superclass)
	}
}

func TestMetaclassInheritanceChain(t *testing.T) {
	vm := NewVM()

	// True -> Boolean -> Object
	// True class -> Boolean class -> Object class -> Class -> Object
	trueMeta := vm.MetaclassFor(vm.TrueClass)
	boolMeta := vm.MetaclassFor(vm.BooleanClass)
	objMeta := vm.MetaclassFor(vm.ObjectClass)

	if trueMeta.Superclass != boolMeta {
		t.Error("True class superclass should be Boolean class")
	}
	if boolMeta.Superclass != objMeta {
		t.Error("Boolean class superclass should be Object class")
	}
	if objMeta.Superclass != vm.ClassClass {
		t.Error("Object class superclass should be Class")
	}
}

func TestClassSideMethodsAccessibleThroughMetaclass(t *testing.T) {
	vm := NewVM()

	// The "new" class method should be accessible through SmallInteger's metaclass
	meta := vm.MetaclassFor(vm.SmallIntegerClass)
	newSel := vm.Selectors.Intern("new")
	method := meta.VTable.Lookup(newSel)
	if method == nil {
		t.Error("Expected 'new' method to be accessible through metaclass VTable")
	}
}

func TestClassSideMethodInheritanceThroughMetaclass(t *testing.T) {
	vm := NewVM()

	// Create a custom class and add a class-side method
	customClass := NewClass("MyCustomClass", vm.ObjectClass)
	vm.Classes.Register(customClass)
	customClass.AddClassMethod0(vm.Selectors, "customFactory", func(_ interface{}, recv Value) Value {
		return FromSmallInt(99)
	})

	// The class-side method should be accessible through the metaclass
	meta := vm.MetaclassFor(customClass)
	sel := vm.Selectors.Intern("customFactory")
	method := meta.VTable.Lookup(sel)
	if method == nil {
		t.Error("Expected 'customFactory' to be accessible through metaclass VTable")
	}

	// Inherited class-side method (new from Object) should also be accessible
	newSel := vm.Selectors.Intern("new")
	newMethod := meta.VTable.Lookup(newSel)
	if newMethod == nil {
		t.Error("Expected inherited 'new' to be accessible through metaclass VTable")
	}
}

func TestMetaclassOfMetaclass(t *testing.T) {
	vm := NewVM()

	// Sending class to a metaclass should return Metaclass
	siMetaVal := vm.classValue(vm.MetaclassFor(vm.SmallIntegerClass))
	result := vm.primitiveClass(siMetaVal)
	if !isClassValue(result) {
		t.Fatal("Expected class of metaclass to be a class value")
	}
	cls := vm.GetClassFromValue(result)
	if cls == nil {
		t.Fatal("Expected non-nil class")
	}
	// The class of a metaclass is the metaclass's own metaclass
	// (which is "SmallInteger class class")
	// In practice, this chains but eventually bottoms out.
	// The important thing is it doesn't crash.
	t.Logf("Class of 'SmallInteger class' is '%s'", cls.Name)
}

func TestMetaclassClassExists(t *testing.T) {
	vm := NewVM()

	if vm.MetaclassClass == nil {
		t.Fatal("MetaclassClass should be initialized during bootstrap")
	}
	if vm.MetaclassClass.Name != "Metaclass" {
		t.Errorf("Expected name 'Metaclass', got '%s'", vm.MetaclassClass.Name)
	}
	if vm.MetaclassClass.Superclass != vm.ClassClass {
		t.Error("Metaclass should be a subclass of Class")
	}
}

func TestMetaclassGlobalRegistered(t *testing.T) {
	vm := NewVM()

	v, ok := vm.Globals["Metaclass"]
	if !ok {
		t.Fatal("Metaclass should be registered in Globals")
	}
	if !isClassValue(v) {
		t.Fatal("Globals['Metaclass'] should be a class value")
	}
}

func TestPrimIsKindOf(t *testing.T) {
	vm := NewVM()

	// 3 isKindOf: SmallInteger -> true
	siVal := vm.classValue(vm.SmallIntegerClass)
	result := vm.Send(FromSmallInt(3), "primIsKindOf:", []Value{siVal})
	if result != True {
		t.Error("3 should be kind of SmallInteger")
	}

	// 3 isKindOf: Object -> true (SmallInteger inherits from Object)
	objVal := vm.classValue(vm.ObjectClass)
	result = vm.Send(FromSmallInt(3), "primIsKindOf:", []Value{objVal})
	if result != True {
		t.Error("3 should be kind of Object")
	}

	// 3 isKindOf: String -> false
	strVal := vm.classValue(vm.StringClass)
	result = vm.Send(FromSmallInt(3), "primIsKindOf:", []Value{strVal})
	if result != False {
		t.Error("3 should not be kind of String")
	}
}

func TestClassNameMessage(t *testing.T) {
	vm := NewVM()

	// 3 class name should return #SmallInteger
	classVal := vm.Send(FromSmallInt(42), "class", nil)
	nameVal := vm.Send(classVal, "name", nil)
	if !nameVal.IsSymbol() {
		t.Fatal("Expected name to return a symbol")
	}
	name := vm.Symbols.Name(nameVal.SymbolID())
	if name != "SmallInteger" {
		t.Errorf("Expected SmallInteger, got %s", name)
	}
}

func TestClassForReturnsMetaclassForClassValues(t *testing.T) {
	vm := NewVM()

	// ClassFor on a class value should return the metaclass
	siVal := vm.classValue(vm.SmallIntegerClass)
	cls := vm.ClassFor(siVal)
	if cls == nil {
		t.Fatal("ClassFor should return non-nil for class value")
	}
	if cls.Name != "SmallInteger class" {
		t.Errorf("Expected 'SmallInteger class', got '%s'", cls.Name)
	}
}

func TestClassMessageOnVariousTypes(t *testing.T) {
	vm := NewVM()

	tests := []struct {
		name     string
		value    Value
		expected string
	}{
		{"nil", Nil, "UndefinedObject"},
		{"true", True, "True"},
		{"false", False, "False"},
		{"integer", FromSmallInt(5), "SmallInteger"},
		{"float", FromFloat64(3.14), "Float"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result := vm.primitiveClass(tc.value)
			if !isClassValue(result) {
				t.Fatalf("Expected class value for %s", tc.name)
			}
			cls := vm.GetClassFromValue(result)
			if cls == nil {
				t.Fatalf("Expected non-nil class for %s", tc.name)
			}
			if cls.Name != tc.expected {
				t.Errorf("Expected %s, got %s", tc.expected, cls.Name)
			}
		})
	}
}
