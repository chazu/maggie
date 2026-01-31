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

// Test method-level cell vars: assign in nested block, read at method level
func TestIntegrationMethodCellVarBasic(t *testing.T) {
	vmInst := vm.NewVM()

	// Method: | r | r := 0. self > 0 ifTrue: [r := self]. ^r
	source := `testMethodCell
	| r |
	r := 0.
	self > 0 ifTrue: [r := self].
	^r`

	method, err := Compile(source, vmInst.Selectors, vmInst.Symbols)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	vmInst.SmallIntegerClass.VTable.AddMethod(vmInst.Selectors.Intern("testMethodCell"), method)

	// 5 testMethodCell → r should be 5
	result := vmInst.Send(vm.FromSmallInt(5), "testMethodCell", nil)
	if !result.IsSmallInt() || result.SmallInt() != 5 {
		t.Errorf("5 testMethodCell = %v, want 5", result)
	}

	// -1 testMethodCell → ifTrue: not taken, r stays 0
	result = vmInst.Send(vm.FromSmallInt(-1), "testMethodCell", nil)
	if !result.IsSmallInt() || result.SmallInt() != 0 {
		t.Errorf("-1 testMethodCell = %v, want 0", result)
	}
}

// Test method-level cell vars with a simple counting loop
func TestIntegrationMethodCellVarSimple(t *testing.T) {
	vmInst := vm.NewVM()

	// Simple: | i | i := 0. [i < 3] whileTrue: [i := i + 1]. ^i
	source := `testCount
	| i |
	i := 0.
	[i < 3] whileTrue: [i := i + 1].
	^i`

	method, err := Compile(source, vmInst.Selectors, vmInst.Symbols)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	vmInst.SmallIntegerClass.VTable.AddMethod(vmInst.Selectors.Intern("testCount"), method)

	result := vmInst.Send(vm.FromSmallInt(1), "testCount", nil)
	if !result.IsSmallInt() || result.SmallInt() != 3 {
		t.Errorf("testCount = %v, want 3", result)
	}
}

func TestIntegrationTimesRepeat(t *testing.T) {
	vmInst := vm.NewVM()

	// Test whileTrue: with method-level variables accessed from blocks
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

// Test that writing to a block-local variable from a nested block works.
// This was broken because cell variables were only created on first assignment
// in the DEFINING scope; when the first assignment was in a nested block,
// no cell existed and the captured value was nil.
func TestIntegrationClosureMutation(t *testing.T) {
	vmInst := vm.NewVM()

	// Method: ^[ | r | self > 0 ifTrue: [ r := self ]. r ] value
	// r is block-local (depth 1), assigned in ifTrue: block (depth 2), read back at depth 1
	source := `testClosureMutation
	^[ | r |
		self > 0 ifTrue: [ r := self ].
		r
	] value`

	method, err := Compile(source, vmInst.Selectors, vmInst.Symbols)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	vmInst.SmallIntegerClass.VTable.AddMethod(vmInst.Selectors.Intern("testClosureMutation"), method)

	// 5 testClosureMutation → r should be 5
	result := vmInst.Send(vm.FromSmallInt(5), "testClosureMutation", nil)
	if !result.IsSmallInt() || result.SmallInt() != 5 {
		t.Errorf("5 testClosureMutation = %v, want 5", result)
	}

	// -1 testClosureMutation → ifTrue: not taken, r stays nil
	result = vmInst.Send(vm.FromSmallInt(-1), "testClosureMutation", nil)
	if result != vm.Nil {
		t.Errorf("-1 testClosureMutation = %v, want nil", result)
	}
}

// Test that writing to a block-local variable from a nested block and reading it
// back also works when the variable is assigned BEFORE and AFTER the nested block.
func TestIntegrationClosureMutationWithLocalAssign(t *testing.T) {
	vmInst := vm.NewVM()

	// r := 0 at depth 1, then r := self inside ifTrue: at depth 2, then read r at depth 1
	source := `testClosureMut2
	^[ | r |
		r := 0.
		self > 0 ifTrue: [ r := self ].
		r
	] value`

	method, err := Compile(source, vmInst.Selectors, vmInst.Symbols)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	vmInst.SmallIntegerClass.VTable.AddMethod(vmInst.Selectors.Intern("testClosureMut2"), method)

	// 5 testClosureMut2 → r should be 5
	result := vmInst.Send(vm.FromSmallInt(5), "testClosureMut2", nil)
	if !result.IsSmallInt() || result.SmallInt() != 5 {
		t.Errorf("5 testClosureMut2 = %v, want 5", result)
	}

	// -1 testClosureMut2 → ifTrue: not taken, r stays 0
	result = vmInst.Send(vm.FromSmallInt(-1), "testClosureMut2", nil)
	if !result.IsSmallInt() || result.SmallInt() != 0 {
		t.Errorf("-1 testClosureMut2 = %v, want 0", result)
	}
}

// Test that a block passed as a method argument can mutate method-level variables.
// This is the core P2 scenario: blocks passed to other methods via arguments
// must capture method vars via cells, not via HomeBP, so they work correctly
// even when the creating method's frame is still active (or has been reclaimed).
func TestIntegrationBlockPassedAsArg(t *testing.T) {
	vmInst := vm.NewVM()

	// Helper method: callBlock: evaluates its argument block
	callBlockSource := `callBlock: aBlock
	^aBlock value`

	callBlockMethod, err := Compile(callBlockSource, vmInst.Selectors, vmInst.Symbols)
	if err != nil {
		t.Fatalf("compile callBlock: error: %v", err)
	}
	vmInst.SmallIntegerClass.VTable.AddMethod(vmInst.Selectors.Intern("callBlock:"), callBlockMethod)

	// Method that passes a closure (capturing method-level var) as argument
	source := `testBlockArg
	| r |
	r := 0.
	self callBlock: [r := self].
	^r`

	method, err := Compile(source, vmInst.Selectors, vmInst.Symbols)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	vmInst.SmallIntegerClass.VTable.AddMethod(vmInst.Selectors.Intern("testBlockArg"), method)

	// 5 testBlockArg → r should be 5 (block mutated r via cell)
	result := vmInst.Send(vm.FromSmallInt(5), "testBlockArg", nil)
	if !result.IsSmallInt() || result.SmallInt() != 5 {
		t.Errorf("5 testBlockArg = %v, want 5", result)
	}

	// 0 testBlockArg → r should be 0 (block sets r := 0)
	result = vmInst.Send(vm.FromSmallInt(0), "testBlockArg", nil)
	if !result.IsSmallInt() || result.SmallInt() != 0 {
		t.Errorf("0 testBlockArg = %v, want 0", result)
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
