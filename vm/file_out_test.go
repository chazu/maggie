package vm

import (
	"strings"
	"testing"
)

// ---------------------------------------------------------------------------
// FileOutClass namespace tests
// ---------------------------------------------------------------------------

func TestFileOutClassWithNamespace(t *testing.T) {
	selectors := NewSelectorTable()
	parent := NewClass("Object", nil)
	cls := NewClassInNamespace("Graphics", "Point", parent)

	result := FileOutClass(cls, selectors)

	// Should start with namespace declaration
	if !strings.HasPrefix(result, "namespace: 'Graphics'\n") {
		t.Errorf("Expected namespace declaration at top, got:\n%s", result)
	}

	// Should contain the import-not-preserved comment
	if !strings.Contains(result, "import declarations are not preserved by fileOut") {
		t.Errorf("Missing import-not-preserved comment in output:\n%s", result)
	}

	// Namespace declaration should appear before the class header
	nsIdx := strings.Index(result, "namespace:")
	headerIdx := strings.Index(result, "Point subclass: Object")
	if nsIdx < 0 || headerIdx < 0 || nsIdx >= headerIdx {
		t.Errorf("Namespace declaration should appear before class header:\n%s", result)
	}
}

func TestFileOutClassWithNestedNamespace(t *testing.T) {
	selectors := NewSelectorTable()
	parent := NewClass("Object", nil)
	cls := NewClassInNamespace("Yutani::Widgets", "Button", parent)

	result := FileOutClass(cls, selectors)

	if !strings.HasPrefix(result, "namespace: 'Yutani::Widgets'\n") {
		t.Errorf("Expected nested namespace declaration at top, got:\n%s", result)
	}
}

func TestFileOutClassWithoutNamespace(t *testing.T) {
	selectors := NewSelectorTable()
	parent := NewClass("Object", nil)
	cls := NewClass("Simple", parent)

	result := FileOutClass(cls, selectors)

	// Should NOT contain namespace declaration
	if strings.Contains(result, "namespace:") {
		t.Errorf("Root-namespace class should not have namespace declaration:\n%s", result)
	}

	// Should NOT contain the import-not-preserved comment
	if strings.Contains(result, "import declarations are not preserved") {
		t.Errorf("Root-namespace class should not have import comment:\n%s", result)
	}

	// Should start with the class header (or docstring if present)
	if !strings.HasPrefix(result, "Simple subclass: Object") {
		t.Errorf("Root-namespace class should start with class header, got:\n%s", result)
	}
}

func TestFileOutClassNamespaceWithDocString(t *testing.T) {
	selectors := NewSelectorTable()
	parent := NewClass("Object", nil)
	cls := NewClassInNamespace("MyApp", "Controller", parent)
	cls.DocString = "Handles application logic."

	result := FileOutClass(cls, selectors)

	// Namespace should come first
	nsIdx := strings.Index(result, "namespace: 'MyApp'")
	docIdx := strings.Index(result, "Handles application logic.")
	headerIdx := strings.Index(result, "Controller subclass: Object")

	if nsIdx < 0 {
		t.Errorf("Missing namespace declaration:\n%s", result)
	}
	if docIdx < 0 {
		t.Errorf("Missing docstring:\n%s", result)
	}
	if headerIdx < 0 {
		t.Errorf("Missing class header:\n%s", result)
	}

	// Order: namespace < docstring < class header
	if nsIdx >= docIdx {
		t.Errorf("Namespace should appear before docstring:\n%s", result)
	}
	if docIdx >= headerIdx {
		t.Errorf("Docstring should appear before class header:\n%s", result)
	}
}

func TestFileOutClassNamespaceWithMethods(t *testing.T) {
	selectors := NewSelectorTable()
	parent := NewClass("Object", nil)
	cls := NewClassInNamespace("Models", "User", parent)
	cls.InstVars = []string{"name", "email"}

	// Add an instance method
	selID := selectors.Intern("name")
	b := NewCompiledMethodBuilder("name", 0)
	b.SetSource("method: name [ ^name ]")
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()
	m.SetSelector(selID)
	m.SetClass(cls)
	cls.VTable.AddMethod(selID, m)

	result := FileOutClass(cls, selectors)

	// Verify namespace is present
	if !strings.Contains(result, "namespace: 'Models'") {
		t.Errorf("Missing namespace declaration:\n%s", result)
	}

	// Verify instance vars are present
	if !strings.Contains(result, "instanceVars: name email") {
		t.Errorf("Missing instance variables:\n%s", result)
	}

	// Verify method source is present
	if !strings.Contains(result, "method: name [ ^name ]") {
		t.Errorf("Missing method source:\n%s", result)
	}
}

// ---------------------------------------------------------------------------
// FileOutNamespace tests
// ---------------------------------------------------------------------------

func TestFileOutNamespaceEmitsNamespaceDeclaration(t *testing.T) {
	selectors := NewSelectorTable()
	classes := NewClassTable()

	parent := NewClass("Object", nil)
	classes.Register(parent)

	c1 := NewClassInNamespace("Graphics", "Point", parent)
	classes.Register(c1)

	c2 := NewClassInNamespace("Graphics", "Rect", parent)
	classes.Register(c2)

	// Class in different namespace should not be included
	c3 := NewClassInNamespace("Audio", "Sound", parent)
	classes.Register(c3)

	sources := FileOutNamespace("Graphics", classes, selectors)

	// Should have exactly 2 classes
	if len(sources) != 2 {
		t.Errorf("Expected 2 classes, got %d", len(sources))
	}

	// Each should have namespace declaration
	for name, source := range sources {
		if !strings.Contains(source, "namespace: 'Graphics'") {
			t.Errorf("Class %s missing namespace declaration:\n%s", name, source)
		}
	}

	// Should not include Sound
	if _, ok := sources["Sound"]; ok {
		t.Error("FileOutNamespace should not include classes from other namespaces")
	}
}

func TestFileOutNamespaceEmpty(t *testing.T) {
	selectors := NewSelectorTable()
	classes := NewClassTable()

	parent := NewClass("Object", nil)
	classes.Register(parent)

	sources := FileOutNamespace("NonExistent", classes, selectors)

	if len(sources) != 0 {
		t.Errorf("Expected 0 classes for non-existent namespace, got %d", len(sources))
	}
}

func TestFileOutNamespaceNestedNamespace(t *testing.T) {
	selectors := NewSelectorTable()
	classes := NewClassTable()

	parent := NewClass("Object", nil)
	classes.Register(parent)

	cls := NewClassInNamespace("Yutani::Widgets", "Button", parent)
	classes.Register(cls)

	// Class in parent namespace should NOT be included
	cls2 := NewClassInNamespace("Yutani", "App", parent)
	classes.Register(cls2)

	sources := FileOutNamespace("Yutani::Widgets", classes, selectors)

	if len(sources) != 1 {
		t.Errorf("Expected 1 class, got %d", len(sources))
	}

	if src, ok := sources["Button"]; ok {
		if !strings.Contains(src, "namespace: 'Yutani::Widgets'") {
			t.Errorf("Missing nested namespace declaration:\n%s", src)
		}
	} else {
		t.Error("Button not found in FileOutNamespace result")
	}
}
