package vm

import (
	"testing"
)

func TestNewStringValue(t *testing.T) {
	vm := NewVM()
	s := vm.registry.NewStringValue("hello")
	if !IsStringValue(s) {
		t.Error("NewStringValue did not create a string value")
	}

	content := vm.registry.GetStringContent(s)
	if content != "hello" {
		t.Errorf("GetStringContent returned %q, want %q", content, "hello")
	}
}

func TestIsStringValueFalse(t *testing.T) {
	// Regular symbol should not be a string
	sym := FromSymbolID(42)
	if IsStringValue(sym) {
		t.Error("Regular symbol should not be identified as a string")
	}

	// SmallInt should not be a string
	if IsStringValue(FromSmallInt(42)) {
		t.Error("SmallInt should not be identified as a string")
	}

	// Nil should not be a string
	if IsStringValue(Nil) {
		t.Error("Nil should not be identified as a string")
	}
}

func TestStringPrimSize(t *testing.T) {
	vm := NewVM()

	s := vm.registry.NewStringValue("hello")
	result := vm.Send(s, "primSize", nil)

	if !result.IsSmallInt() {
		t.Fatalf("primSize did not return a SmallInt")
	}
	if result.SmallInt() != 5 {
		t.Errorf("primSize returned %d, want 5", result.SmallInt())
	}
}

func TestStringPrimSizeEmpty(t *testing.T) {
	vm := NewVM()

	s := vm.registry.NewStringValue("")
	result := vm.Send(s, "primSize", nil)

	if !result.IsSmallInt() || result.SmallInt() != 0 {
		t.Errorf("primSize of empty string returned %v, want 0", result)
	}
}

func TestStringPrimAt(t *testing.T) {
	vm := NewVM()

	s := vm.registry.NewStringValue("hello")

	// Test getting character at index 0 (0-based) — now returns a Character value
	result := vm.Send(s, "primAt:", []Value{FromSmallInt(0)})
	if !IsCharacterValue(result) {
		t.Fatalf("primAt: 0 returned non-Character value")
	}
	if GetCharacterCodePoint(result) != 'h' {
		t.Errorf("primAt: 0 returned %c, want 'h'", GetCharacterCodePoint(result))
	}

	// Test getting character at index 2
	result = vm.Send(s, "primAt:", []Value{FromSmallInt(2)})
	if !IsCharacterValue(result) {
		t.Fatalf("primAt: 2 returned non-Character value")
	}
	if GetCharacterCodePoint(result) != 'l' {
		t.Errorf("primAt: 2 returned %c, want 'l'", GetCharacterCodePoint(result))
	}

	// Test out of bounds (positive)
	result = vm.Send(s, "primAt:", []Value{FromSmallInt(10)})
	if result != Nil {
		t.Errorf("primAt: 10 returned %v, want Nil", result)
	}

	// Test out of bounds (negative)
	result = vm.Send(s, "primAt:", []Value{FromSmallInt(-1)})
	if result != Nil {
		t.Errorf("primAt: -1 returned %v, want Nil", result)
	}
}

func TestStringPrimConcat(t *testing.T) {
	vm := NewVM()

	s1 := vm.registry.NewStringValue("hello")
	s2 := vm.registry.NewStringValue(" world")

	result := vm.Send(s1, "primConcat:", []Value{s2})
	if vm.registry.GetStringContent(result) != "hello world" {
		t.Errorf("primConcat: returned %q, want %q", vm.registry.GetStringContent(result), "hello world")
	}
}

func TestStringPrimEquals(t *testing.T) {
	vm := NewVM()

	s1 := vm.registry.NewStringValue("hello")
	s2 := vm.registry.NewStringValue("hello")
	s3 := vm.registry.NewStringValue("world")

	// Equal strings
	result := vm.Send(s1, "primEquals:", []Value{s2})
	if result != True {
		t.Errorf("primEquals: of equal strings returned %v, want True", result)
	}

	// Unequal strings
	result = vm.Send(s1, "primEquals:", []Value{s3})
	if result != False {
		t.Errorf("primEquals: of unequal strings returned %v, want False", result)
	}
}

func TestStringPrimLessThan(t *testing.T) {
	vm := NewVM()

	s1 := vm.registry.NewStringValue("abc")
	s2 := vm.registry.NewStringValue("abd")
	s3 := vm.registry.NewStringValue("abc")

	// Less than
	result := vm.Send(s1, "primLessThan:", []Value{s2})
	if result != True {
		t.Errorf("primLessThan: 'abc' < 'abd' returned %v, want True", result)
	}

	// Not less than
	result = vm.Send(s2, "primLessThan:", []Value{s1})
	if result != False {
		t.Errorf("primLessThan: 'abd' < 'abc' returned %v, want False", result)
	}

	// Equal (not less than)
	result = vm.Send(s1, "primLessThan:", []Value{s3})
	if result != False {
		t.Errorf("primLessThan: 'abc' < 'abc' returned %v, want False", result)
	}
}

func TestStringPrimIncludes(t *testing.T) {
	vm := NewVM()

	s := vm.registry.NewStringValue("hello world")

	// Contains single char
	result := vm.Send(s, "primIncludes:", []Value{vm.registry.NewStringValue("o")})
	if result != True {
		t.Errorf("primIncludes: 'o' returned %v, want True", result)
	}

	// Contains substring
	result = vm.Send(s, "primIncludes:", []Value{vm.registry.NewStringValue("wor")})
	if result != True {
		t.Errorf("primIncludes: 'wor' returned %v, want True", result)
	}

	// Does not contain
	result = vm.Send(s, "primIncludes:", []Value{vm.registry.NewStringValue("x")})
	if result != False {
		t.Errorf("primIncludes: 'x' returned %v, want False", result)
	}
}

func TestStringPrimIndexOf(t *testing.T) {
	vm := NewVM()

	s := vm.registry.NewStringValue("hello")

	// Find existing character (0-based index)
	result := vm.Send(s, "primIndexOf:", []Value{vm.registry.NewStringValue("e")})
	if !result.IsSmallInt() || result.SmallInt() != 1 {
		t.Errorf("primIndexOf: 'e' returned %v, want 1", result)
	}

	// Find first occurrence of duplicate
	result = vm.Send(s, "primIndexOf:", []Value{vm.registry.NewStringValue("l")})
	if !result.IsSmallInt() || result.SmallInt() != 2 {
		t.Errorf("primIndexOf: 'l' returned %v, want 2", result)
	}

	// Character not found returns -1
	result = vm.Send(s, "primIndexOf:", []Value{vm.registry.NewStringValue("x")})
	if !result.IsSmallInt() || result.SmallInt() != -1 {
		t.Errorf("primIndexOf: 'x' returned %v, want -1", result)
	}
}

func TestStringPrimAsSymbol(t *testing.T) {
	vm := NewVM()

	s := vm.registry.NewStringValue("mySymbol")
	result := vm.Send(s, "primAsSymbol", nil)

	if !result.IsSymbol() {
		t.Fatalf("primAsSymbol did not return a symbol")
	}

	// Verify it's a real symbol (not a string)
	if IsStringValue(result) {
		t.Error("primAsSymbol returned a string instead of a symbol")
	}

	// Verify the symbol name
	name := vm.Symbols.Name(result.SymbolID())
	if name != "mySymbol" {
		t.Errorf("Symbol name is %q, want %q", name, "mySymbol")
	}
}

func TestStringPrimAsUppercase(t *testing.T) {
	vm := NewVM()

	s := vm.registry.NewStringValue("Hello World")
	result := vm.Send(s, "primAsUppercase", nil)

	if vm.registry.GetStringContent(result) != "HELLO WORLD" {
		t.Errorf("primAsUppercase returned %q, want %q", vm.registry.GetStringContent(result), "HELLO WORLD")
	}
}

func TestStringPrimAsLowercase(t *testing.T) {
	vm := NewVM()

	s := vm.registry.NewStringValue("Hello World")
	result := vm.Send(s, "primAsLowercase", nil)

	if vm.registry.GetStringContent(result) != "hello world" {
		t.Errorf("primAsLowercase returned %q, want %q", vm.registry.GetStringContent(result), "hello world")
	}
}

func TestStringPrimCopyFromTo(t *testing.T) {
	vm := NewVM()

	s := vm.registry.NewStringValue("hello world")

	// Copy substring (0-based, exclusive end like Go slices)
	result := vm.Send(s, "primCopyFrom:to:", []Value{FromSmallInt(0), FromSmallInt(5)})
	if vm.registry.GetStringContent(result) != "hello" {
		t.Errorf("primCopyFrom:0 to:5 returned %q, want %q", vm.registry.GetStringContent(result), "hello")
	}

	// Copy middle
	result = vm.Send(s, "primCopyFrom:to:", []Value{FromSmallInt(6), FromSmallInt(11)})
	if vm.registry.GetStringContent(result) != "world" {
		t.Errorf("primCopyFrom:6 to:11 returned %q, want %q", vm.registry.GetStringContent(result), "world")
	}

	// End exceeds string length (clamped)
	result = vm.Send(s, "primCopyFrom:to:", []Value{FromSmallInt(6), FromSmallInt(100)})
	if vm.registry.GetStringContent(result) != "world" {
		t.Errorf("primCopyFrom:6 to:100 returned %q, want %q", vm.registry.GetStringContent(result), "world")
	}

	// Start exceeds end
	result = vm.Send(s, "primCopyFrom:to:", []Value{FromSmallInt(5), FromSmallInt(3)})
	if vm.registry.GetStringContent(result) != "" {
		t.Errorf("primCopyFrom:5 to:3 returned %q, want empty string", vm.registry.GetStringContent(result))
	}
}

func TestStringClassAssignment(t *testing.T) {
	vm := NewVM()

	s := vm.registry.NewStringValue("test")
	class := vm.ClassFor(s)

	// String values should be classified as StringClass
	if class != vm.StringClass {
		t.Errorf("String class is %v, want StringClass", class.Name)
	}
}
