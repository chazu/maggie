package compiler

import (
	"testing"

	"github.com/chazu/maggie/vm"
)

// Integration tests: compile and execute real Smalltalk code

func TestIntegrationFactorial(t *testing.T) {
	vmInst := vm.NewVM()

	// Compile factorial method
	source := `factorial
	self = 0 ifTrue: [^1].
	^self * (self - 1) factorial`

	method, err := Compile(source, vmInst.Selectors, vmInst.Symbols)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	// Add method to SmallInteger
	vmInst.SmallIntegerClass.VTable.AddMethod(vmInst.Selectors.Intern("factorial"), method)

	// Test factorial(5) = 120
	result := vmInst.Send(vm.FromSmallInt(5), "factorial", nil)
	if !result.IsSmallInt() || result.SmallInt() != 120 {
		t.Errorf("5 factorial = %v, want 120", result)
	}

	// Test factorial(0) = 1
	result = vmInst.Send(vm.FromSmallInt(0), "factorial", nil)
	if !result.IsSmallInt() || result.SmallInt() != 1 {
		t.Errorf("0 factorial = %v, want 1", result)
	}
}

func TestIntegrationFibonacci(t *testing.T) {
	vmInst := vm.NewVM()

	// Compile fibonacci method
	source := `fib
	self < 2 ifTrue: [^self].
	^(self - 1) fib + (self - 2) fib`

	method, err := Compile(source, vmInst.Selectors, vmInst.Symbols)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	vmInst.SmallIntegerClass.VTable.AddMethod(vmInst.Selectors.Intern("fib"), method)

	// fib(10) = 55
	result := vmInst.Send(vm.FromSmallInt(10), "fib", nil)
	if !result.IsSmallInt() || result.SmallInt() != 55 {
		t.Errorf("10 fib = %v, want 55", result)
	}
}

func TestIntegrationMax(t *testing.T) {
	vmInst := vm.NewVM()

	// Compile max: method
	source := `max: other
	self > other ifTrue: [^self].
	^other`

	method, err := Compile(source, vmInst.Selectors, vmInst.Symbols)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	vmInst.SmallIntegerClass.VTable.AddMethod(vmInst.Selectors.Intern("max:"), method)

	// 5 max: 3 = 5
	result := vmInst.Send(vm.FromSmallInt(5), "max:", []vm.Value{vm.FromSmallInt(3)})
	if !result.IsSmallInt() || result.SmallInt() != 5 {
		t.Errorf("5 max: 3 = %v, want 5", result)
	}

	// 3 max: 5 = 5
	result = vmInst.Send(vm.FromSmallInt(3), "max:", []vm.Value{vm.FromSmallInt(5)})
	if !result.IsSmallInt() || result.SmallInt() != 5 {
		t.Errorf("3 max: 5 = %v, want 5", result)
	}
}

func TestIntegrationBetweenAnd(t *testing.T) {
	vmInst := vm.NewVM()

	// Compile between:and: method
	source := `between: min and: max
	^self >= min & (self <= max)`

	method, err := Compile(source, vmInst.Selectors, vmInst.Symbols)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	vmInst.SmallIntegerClass.VTable.AddMethod(vmInst.Selectors.Intern("between:and:"), method)

	// 5 between: 1 and: 10 = true
	result := vmInst.Send(vm.FromSmallInt(5), "between:and:", []vm.Value{vm.FromSmallInt(1), vm.FromSmallInt(10)})
	if result != vm.True {
		t.Errorf("5 between: 1 and: 10 = %v, want true", result)
	}

	// 15 between: 1 and: 10 = false
	result = vmInst.Send(vm.FromSmallInt(15), "between:and:", []vm.Value{vm.FromSmallInt(1), vm.FromSmallInt(10)})
	if result != vm.False {
		t.Errorf("15 between: 1 and: 10 = %v, want false", result)
	}
}

func TestIntegrationTimesRepeat(t *testing.T) {
	// Skip: requires closure variable capture (blocks accessing outer temps)
	// which is not yet implemented. Blocks currently can't access sum and i
	// from the enclosing method.
	t.Skip("closure variable capture not yet implemented")

	vmInst := vm.NewVM()

	// Test that we can compile and run code that uses blocks
	// Compile sum: method that adds 1 to 10
	source := `sumTo
	| sum i |
	sum := 0.
	i := 1.
	[i <= self] whileTrue: [
		sum := sum + i.
		i := i + 1
	].
	^sum`

	method, err := Compile(source, vmInst.Selectors, vmInst.Symbols)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	vmInst.SmallIntegerClass.VTable.AddMethod(vmInst.Selectors.Intern("sumTo"), method)

	// sumTo(10) = 55 (1+2+3+4+5+6+7+8+9+10)
	result := vmInst.Send(vm.FromSmallInt(10), "sumTo", nil)
	if !result.IsSmallInt() || result.SmallInt() != 55 {
		t.Errorf("10 sumTo = %v, want 55", result)
	}
}

func TestIntegrationEven(t *testing.T) {
	vmInst := vm.NewVM()

	// Compile even method
	source := `even
	^(self \\ 2) = 0`

	method, err := Compile(source, vmInst.Selectors, vmInst.Symbols)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	vmInst.SmallIntegerClass.VTable.AddMethod(vmInst.Selectors.Intern("even"), method)

	// 4 even = true
	result := vmInst.Send(vm.FromSmallInt(4), "even", nil)
	if result != vm.True {
		t.Errorf("4 even = %v, want true", result)
	}

	// 5 even = false
	result = vmInst.Send(vm.FromSmallInt(5), "even", nil)
	if result != vm.False {
		t.Errorf("5 even = %v, want false", result)
	}
}

func TestIntegrationOdd(t *testing.T) {
	vmInst := vm.NewVM()

	// Compile odd method (depends on even)
	evenSource := `even
	^(self \\ 2) = 0`

	oddSource := `odd
	^self even not`

	evenMethod, err := Compile(evenSource, vmInst.Selectors, vmInst.Symbols)
	if err != nil {
		t.Fatalf("compile even error: %v", err)
	}

	oddMethod, err := Compile(oddSource, vmInst.Selectors, vmInst.Symbols)
	if err != nil {
		t.Fatalf("compile odd error: %v", err)
	}

	vmInst.SmallIntegerClass.VTable.AddMethod(vmInst.Selectors.Intern("even"), evenMethod)
	vmInst.SmallIntegerClass.VTable.AddMethod(vmInst.Selectors.Intern("odd"), oddMethod)

	// 5 odd = true
	result := vmInst.Send(vm.FromSmallInt(5), "odd", nil)
	if result != vm.True {
		t.Errorf("5 odd = %v, want true", result)
	}

	// 4 odd = false
	result = vmInst.Send(vm.FromSmallInt(4), "odd", nil)
	if result != vm.False {
		t.Errorf("4 odd = %v, want false", result)
	}
}

func TestIntegrationAbs(t *testing.T) {
	vmInst := vm.NewVM()

	// Compile abs method (different from primitive)
	source := `myAbs
	self < 0 ifTrue: [^self negated].
	^self`

	method, err := Compile(source, vmInst.Selectors, vmInst.Symbols)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	vmInst.SmallIntegerClass.VTable.AddMethod(vmInst.Selectors.Intern("myAbs"), method)

	// -5 myAbs = 5
	result := vmInst.Send(vm.FromSmallInt(-5), "myAbs", nil)
	if !result.IsSmallInt() || result.SmallInt() != 5 {
		t.Errorf("-5 myAbs = %v, want 5", result)
	}

	// 5 myAbs = 5
	result = vmInst.Send(vm.FromSmallInt(5), "myAbs", nil)
	if !result.IsSmallInt() || result.SmallInt() != 5 {
		t.Errorf("5 myAbs = %v, want 5", result)
	}
}
