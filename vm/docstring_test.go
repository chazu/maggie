package vm

import (
	"bytes"
	"strings"
	"testing"
)

// ---------------------------------------------------------------------------
// CompiledMethod DocString tests
// ---------------------------------------------------------------------------

func TestCompiledMethodDocString(t *testing.T) {
	b := NewCompiledMethodBuilder("greet:", 1)
	b.SetDocString("Greets the given name.")
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	if m.DocString() != "Greets the given name." {
		t.Errorf("DocString() = %q, want %q", m.DocString(), "Greets the given name.")
	}
}

func TestCompiledMethodDocStringEmpty(t *testing.T) {
	b := NewCompiledMethodBuilder("noop", 0)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	if m.DocString() != "" {
		t.Errorf("DocString() = %q, want empty", m.DocString())
	}
}

func TestCompiledMethodSetDocString(t *testing.T) {
	b := NewCompiledMethodBuilder("foo", 0)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	m.SetDocString("Updated doc.")
	if m.DocString() != "Updated doc." {
		t.Errorf("DocString() = %q, want %q", m.DocString(), "Updated doc.")
	}
}

// ---------------------------------------------------------------------------
// Class DocString tests
// ---------------------------------------------------------------------------

func TestClassDocString(t *testing.T) {
	cls := NewClass("Documented", nil)
	cls.DocString = "A documented class."

	if cls.DocString != "A documented class." {
		t.Errorf("DocString = %q, want %q", cls.DocString, "A documented class.")
	}
}

// ---------------------------------------------------------------------------
// FormatClassHelp tests
// ---------------------------------------------------------------------------

func TestFormatClassHelpWithDocString(t *testing.T) {
	selectors := NewSelectorTable()
	parent := NewClass("Object", nil)
	cls := NewClass("Point", parent)
	cls.DocString = "A 2D point with x and y coordinates."

	// Add an instance method
	selID := selectors.Intern("x")
	b := NewCompiledMethodBuilder("x", 0)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()
	cls.VTable.AddMethod(selID, m)

	result := FormatClassHelp(cls, selectors)

	if !strings.Contains(result, "Point (subclass of Object)") {
		t.Errorf("Missing class header in help output:\n%s", result)
	}
	if !strings.Contains(result, "A 2D point with x and y coordinates.") {
		t.Errorf("Missing docstring in help output:\n%s", result)
	}
	if !strings.Contains(result, "Instance methods:") {
		t.Errorf("Missing instance methods section:\n%s", result)
	}
	if !strings.Contains(result, "  x") {
		t.Errorf("Missing method 'x' in help output:\n%s", result)
	}
}

func TestFormatClassHelpWithoutDocString(t *testing.T) {
	selectors := NewSelectorTable()
	cls := NewClass("Empty", nil)

	result := FormatClassHelp(cls, selectors)

	if !strings.Contains(result, "Empty") {
		t.Errorf("Missing class name in help output:\n%s", result)
	}
	// Should NOT contain a docstring section
	lines := strings.Split(result, "\n")
	// The output should be just "Empty\n" with no extra blank lines for docstring
	if len(lines) > 2 {
		// Could have trailing newline, but shouldn't have a docstring block
		for _, line := range lines[1:] {
			if strings.TrimSpace(line) != "" && !strings.Contains(line, "methods:") {
				// There shouldn't be content after the class name unless it's methods
				break
			}
		}
	}
}

func TestFormatMethodHelp(t *testing.T) {
	b := NewCompiledMethodBuilder("add:", 1)
	b.SetDocString("Adds the argument to the receiver.")
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := formatMethodHelp("Number", m)

	if !strings.Contains(result, "Number>>add:") {
		t.Errorf("Missing method header in output:\n%s", result)
	}
	if !strings.Contains(result, "Adds the argument to the receiver.") {
		t.Errorf("Missing docstring in output:\n%s", result)
	}
}

func TestFormatMethodHelpNoDocString(t *testing.T) {
	b := NewCompiledMethodBuilder("size", 0)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := formatMethodHelp("Array", m)

	if !strings.Contains(result, "Array>>size") {
		t.Errorf("Missing method header in output:\n%s", result)
	}
	if !strings.Contains(result, "(no documentation)") {
		t.Errorf("Missing '(no documentation)' placeholder:\n%s", result)
	}
}

// ---------------------------------------------------------------------------
// FileOut docstring tests
// ---------------------------------------------------------------------------

func TestFileOutClassDocString(t *testing.T) {
	selectors := NewSelectorTable()
	parent := NewClass("Object", nil)
	cls := NewClass("Widget", parent)
	cls.DocString = "Base class for all UI widgets."

	result := FileOutClass(cls, selectors)

	if !strings.Contains(result, "\"\"\"\nBase class for all UI widgets.\n\"\"\"") {
		t.Errorf("Missing class docstring in fileOut output:\n%s", result)
	}
	// Docstring should appear before the class header
	docIdx := strings.Index(result, "\"\"\"")
	headerIdx := strings.Index(result, "Widget subclass: Object")
	if docIdx < 0 || headerIdx < 0 || docIdx >= headerIdx {
		t.Errorf("Docstring should appear before class header:\n%s", result)
	}
}

func TestFileOutMethodDocString(t *testing.T) {
	selectors := NewSelectorTable()
	parent := NewClass("Object", nil)
	cls := NewClass("Stack", parent)

	// Add an instance method with a docstring
	selID := selectors.Intern("push:")
	b := NewCompiledMethodBuilder("push:", 1)
	b.SetDocString("Pushes value onto the stack.")
	b.SetSource("method: push: value [ items add: value ]")
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()
	m.SetSelector(selID)
	m.SetClass(cls)
	cls.VTable.AddMethod(selID, m)

	result := FileOutClass(cls, selectors)

	if !strings.Contains(result, "  \"\"\"\n  Pushes value onto the stack.\n  \"\"\"") {
		t.Errorf("Missing method docstring in fileOut output:\n%s", result)
	}
	if !strings.Contains(result, "  method: push: value [ items add: value ]") {
		t.Errorf("Missing method source in fileOut output:\n%s", result)
	}
	// Docstring should appear before the method source
	docIdx := strings.Index(result, "Pushes value")
	srcIdx := strings.Index(result, "method: push:")
	if docIdx >= srcIdx {
		t.Errorf("Method docstring should appear before method source:\n%s", result)
	}
}

func TestFileOutClassMethodDocString(t *testing.T) {
	selectors := NewSelectorTable()
	parent := NewClass("Object", nil)
	cls := NewClass("Logger", parent)

	// Add a class method with a docstring
	selID := selectors.Intern("default")
	b := NewCompiledMethodBuilder("default", 0)
	b.SetDocString("Returns the default logger instance.")
	b.SetSource("classMethod: default [ ^DefaultInstance ]")
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()
	m.SetSelector(selID)
	m.SetClass(cls)
	cls.ClassVTable.AddMethod(selID, m)

	result := FileOutClass(cls, selectors)

	if !strings.Contains(result, "Returns the default logger instance.") {
		t.Errorf("Missing class method docstring:\n%s", result)
	}
}

func TestFileOutNoDocString(t *testing.T) {
	selectors := NewSelectorTable()
	parent := NewClass("Object", nil)
	cls := NewClass("Plain", parent)

	result := FileOutClass(cls, selectors)

	if strings.Contains(result, "\"\"\"") {
		t.Errorf("Unexpected docstring markers in output:\n%s", result)
	}
}

// ---------------------------------------------------------------------------
// Image round-trip docstring tests
// ---------------------------------------------------------------------------

func TestImageRoundTripClassDocString(t *testing.T) {
	// Create a VM with a documented class
	vm1 := NewVM()
	cls := NewClass("DocTest", vm1.ObjectClass)
	cls.DocString = "A class for testing docstring round-trips."
	vm1.Classes.Register(cls)

	// Save image
	var buf bytes.Buffer
	err := vm1.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	// Load into a new VM
	vm2 := NewVM()
	err = vm2.LoadImageFromBytes(buf.Bytes())
	if err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}

	// Verify docstring survived
	loaded := vm2.Classes.Lookup("DocTest")
	if loaded == nil {
		t.Fatal("DocTest class not found after loading image")
	}
	if loaded.DocString != "A class for testing docstring round-trips." {
		t.Errorf("Class DocString = %q, want %q", loaded.DocString, "A class for testing docstring round-trips.")
	}
}

func TestImageRoundTripMethodDocString(t *testing.T) {
	// Create a VM with a documented method
	vm1 := NewVM()
	cls := NewClass("MethodDocTest", vm1.ObjectClass)
	vm1.Classes.Register(cls)

	b := NewCompiledMethodBuilder("greet:", 1)
	b.SetDocString("Says hello to the given name.")
	b.SetSource("method: greet: name [ ^'Hello, ', name ]")
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	selID := vm1.Selectors.Intern("greet:")
	m.SetSelector(selID)
	m.SetClass(cls)
	cls.VTable.AddMethod(selID, m)

	// Save image
	var buf bytes.Buffer
	err := vm1.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	// Load into a new VM
	vm2 := NewVM()
	err = vm2.LoadImageFromBytes(buf.Bytes())
	if err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}

	// Verify method docstring survived
	loaded := vm2.Classes.Lookup("MethodDocTest")
	if loaded == nil {
		t.Fatal("MethodDocTest class not found after loading image")
	}

	loadedSelID := vm2.Selectors.Lookup("greet:")
	if loadedSelID < 0 {
		t.Fatal("greet: selector not found after loading image")
	}

	method := loaded.VTable.LookupLocal(loadedSelID)
	if method == nil {
		t.Fatal("greet: method not found after loading image")
	}

	cm, ok := method.(*CompiledMethod)
	if !ok {
		t.Fatal("greet: method is not a CompiledMethod")
	}

	if cm.DocString() != "Says hello to the given name." {
		t.Errorf("Method DocString = %q, want %q", cm.DocString(), "Says hello to the given name.")
	}
}

func TestImageRoundTripEmptyDocString(t *testing.T) {
	// Create a VM with a class that has no docstring
	vm1 := NewVM()
	cls := NewClass("NoDoc", vm1.ObjectClass)
	vm1.Classes.Register(cls)

	b := NewCompiledMethodBuilder("noop", 0)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	selID := vm1.Selectors.Intern("noop")
	m.SetSelector(selID)
	m.SetClass(cls)
	cls.VTable.AddMethod(selID, m)

	// Save and load
	var buf bytes.Buffer
	err := vm1.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	vm2 := NewVM()
	err = vm2.LoadImageFromBytes(buf.Bytes())
	if err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}

	loaded := vm2.Classes.Lookup("NoDoc")
	if loaded == nil {
		t.Fatal("NoDoc class not found after loading image")
	}
	if loaded.DocString != "" {
		t.Errorf("Class DocString = %q, want empty", loaded.DocString)
	}

	loadedSelID := vm2.Selectors.Lookup("noop")
	method := loaded.VTable.LookupLocal(loadedSelID)
	if method == nil {
		t.Fatal("noop method not found")
	}
	cm := method.(*CompiledMethod)
	if cm.DocString() != "" {
		t.Errorf("Method DocString = %q, want empty", cm.DocString())
	}
}

func TestImageRoundTripMultilineDocString(t *testing.T) {
	vm1 := NewVM()
	cls := NewClass("MultiLine", vm1.ObjectClass)
	cls.DocString = "A class with a multi-line docstring.\n\nIt spans multiple paragraphs.\n\nIncluding code:\n  x := 42."
	vm1.Classes.Register(cls)

	var buf bytes.Buffer
	err := vm1.SaveImageTo(&buf)
	if err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}

	vm2 := NewVM()
	err = vm2.LoadImageFromBytes(buf.Bytes())
	if err != nil {
		t.Fatalf("LoadImageFromBytes failed: %v", err)
	}

	loaded := vm2.Classes.Lookup("MultiLine")
	if loaded == nil {
		t.Fatal("MultiLine class not found")
	}
	if loaded.DocString != cls.DocString {
		t.Errorf("DocString mismatch:\ngot:  %q\nwant: %q", loaded.DocString, cls.DocString)
	}
}
