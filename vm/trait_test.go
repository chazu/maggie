package vm

import (
	"testing"
)

func TestNewTrait(t *testing.T) {
	trait := NewTrait("Enumerable")

	if trait.Name != "Enumerable" {
		t.Errorf("expected name 'Enumerable', got '%s'", trait.Name)
	}

	if trait.Methods == nil {
		t.Error("expected Methods map to be initialized")
	}

	if trait.MethodCount() != 0 {
		t.Errorf("expected 0 methods, got %d", trait.MethodCount())
	}
}

func TestTraitAddMethod(t *testing.T) {
	trait := NewTrait("Printable")

	// Create a simple method
	method := &CompiledMethod{}
	method.name = "printString"

	selectorID := 42
	trait.AddMethod(selectorID, method)

	if trait.MethodCount() != 1 {
		t.Errorf("expected 1 method, got %d", trait.MethodCount())
	}

	if !trait.HasMethod(selectorID) {
		t.Error("expected trait to have method for selector 42")
	}

	got := trait.GetMethod(selectorID)
	if got != method {
		t.Error("expected to get the same method back")
	}
}

func TestTraitAddRequires(t *testing.T) {
	trait := NewTrait("Comparable")

	trait.AddRequires(10) // equals
	trait.AddRequires(20) // lessThan

	if len(trait.Requires) != 2 {
		t.Errorf("expected 2 required methods, got %d", len(trait.Requires))
	}

	if trait.Requires[0] != 10 || trait.Requires[1] != 20 {
		t.Error("required selectors not stored correctly")
	}
}

func TestTraitTable(t *testing.T) {
	table := NewTraitTable()

	if table.Len() != 0 {
		t.Errorf("expected empty table, got %d traits", table.Len())
	}

	// Register a trait
	trait := NewTrait("Enumerable")
	old := table.Register(trait)

	if old != nil {
		t.Error("expected nil for first registration")
	}

	if table.Len() != 1 {
		t.Errorf("expected 1 trait, got %d", table.Len())
	}

	// Lookup the trait
	found := table.Lookup("Enumerable")
	if found != trait {
		t.Error("expected to find the registered trait")
	}

	// Has check
	if !table.Has("Enumerable") {
		t.Error("expected Has to return true for registered trait")
	}

	if table.Has("NonExistent") {
		t.Error("expected Has to return false for non-existent trait")
	}

	// All traits
	all := table.All()
	if len(all) != 1 {
		t.Errorf("expected 1 trait in All(), got %d", len(all))
	}

	// Re-register same name (should return old trait)
	trait2 := NewTrait("Enumerable")
	old = table.Register(trait2)
	if old != trait {
		t.Error("expected old trait to be returned on re-registration")
	}
}

func TestClassIncludeTrait(t *testing.T) {
	// Create a VM with selector table
	selectors := NewSelectorTable()

	// Create trait with method
	trait := NewTrait("Printable")
	printSelector := selectors.Intern("print")
	printMethod := &CompiledMethod{}
	printMethod.name = "print"
	trait.AddMethod(printSelector, printMethod)

	// Create class
	class := NewClass("MyClass", nil)

	// Include trait
	errMsg := class.IncludeTrait(trait, selectors)
	if errMsg != "" {
		t.Errorf("unexpected error including trait: %s", errMsg)
	}

	// Check method was added
	found := class.VTable.Lookup(printSelector)
	if found != printMethod {
		t.Error("expected trait method to be added to class")
	}
}

func TestClassMethodTakesPrecedence(t *testing.T) {
	selectors := NewSelectorTable()

	// Create trait with method
	trait := NewTrait("Printable")
	printSelector := selectors.Intern("print")
	traitMethod := &CompiledMethod{}
	traitMethod.name = "print"
	trait.AddMethod(printSelector, traitMethod)

	// Create class with its own method for same selector
	class := NewClass("MyClass", nil)
	classMethod := &CompiledMethod{}
	classMethod.name = "print"
	class.VTable.AddMethod(printSelector, classMethod)

	// Include trait
	errMsg := class.IncludeTrait(trait, selectors)
	if errMsg != "" {
		t.Errorf("unexpected error including trait: %s", errMsg)
	}

	// Check class method was NOT replaced
	found := class.VTable.Lookup(printSelector)
	if found != classMethod {
		t.Error("class method should take precedence over trait method")
	}
}

func TestTraitRequiredMethods(t *testing.T) {
	selectors := NewSelectorTable()

	// Create trait with required method
	trait := NewTrait("Comparable")
	lessThanSelector := selectors.Intern("lessThan:")
	trait.AddRequires(lessThanSelector)

	// Create class WITHOUT the required method
	class := NewClass("MyClass", nil)

	// Include trait should fail
	errMsg := class.IncludeTrait(trait, selectors)
	if errMsg == "" {
		t.Error("expected error for missing required method")
	}

	// Now add the required method
	requiredMethod := &CompiledMethod{}
	requiredMethod.name = "lessThan:"
	class.VTable.AddMethod(lessThanSelector, requiredMethod)

	// Include trait should succeed now
	errMsg = class.IncludeTrait(trait, selectors)
	if errMsg != "" {
		t.Errorf("unexpected error: %s", errMsg)
	}
}

func TestIncludeTraitByName(t *testing.T) {
	selectors := NewSelectorTable()
	traits := NewTraitTable()

	// Create and register trait
	trait := NewTrait("Printable")
	printSelector := selectors.Intern("print")
	printMethod := &CompiledMethod{}
	printMethod.name = "print"
	trait.AddMethod(printSelector, printMethod)
	traits.Register(trait)

	// Create class
	class := NewClass("MyClass", nil)

	// Include by name
	errMsg := class.IncludeTraitByName("Printable", traits, selectors)
	if errMsg != "" {
		t.Errorf("unexpected error: %s", errMsg)
	}

	// Verify method was added
	found := class.VTable.Lookup(printSelector)
	if found != printMethod {
		t.Error("expected trait method to be added via IncludeTraitByName")
	}

	// Try unknown trait
	errMsg = class.IncludeTraitByName("UnknownTrait", traits, selectors)
	if errMsg == "" {
		t.Error("expected error for unknown trait")
	}
}
