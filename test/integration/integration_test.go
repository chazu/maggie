package integration_test

import (
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// Integration test helpers
// ---------------------------------------------------------------------------

// eval compiles and executes a single expression via Compiler evaluate:.
func eval(t *testing.T, vmInst *vm.VM, source string) vm.Value {
	t.Helper()
	vmInst.UseGoCompiler(compiler.Compile)
	compilerClass := vmInst.Globals["Compiler"]
	return vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue(source)})
}

// compileMethod compiles a method definition and installs it on the given class.
func compileMethod(t *testing.T, vmInst *vm.VM, class *vm.Class, source string) {
	t.Helper()
	method, err := compiler.Compile(source, vmInst.Selectors, vmInst.Symbols, vmInst.Registry())
	if err != nil {
		t.Fatalf("compile error for method: %v\nsource: %s", err, source)
	}
	method.SetClass(class)
	selectorID := vmInst.Selectors.Intern(method.Name())
	class.VTable.AddMethod(selectorID, method)
}

// compileMethodWithIvars compiles a method with instance variable context.
func compileMethodWithIvars(t *testing.T, vmInst *vm.VM, class *vm.Class, source string) {
	t.Helper()
	parser := compiler.NewParser(source)
	methodDef := parser.ParseMethod()
	if len(parser.Errors()) > 0 {
		t.Fatalf("parse errors: %v\nsource: %s", parser.Errors(), source)
	}

	allIvars := class.AllInstVarNames()
	method, err := compiler.CompileMethodDefWithIvars(methodDef, vmInst.Selectors, vmInst.Symbols, vmInst.Registry(), allIvars)
	if err != nil {
		t.Fatalf("compile error: %v\nsource: %s", err, source)
	}
	method.SetClass(class)
	selectorID := vmInst.Selectors.Intern(method.Name())
	class.VTable.AddMethod(selectorID, method)
}

// compileClassMethod compiles a class-side method and installs it.
func compileClassMethod(t *testing.T, vmInst *vm.VM, class *vm.Class, source string) {
	t.Helper()
	method, err := compiler.Compile(source, vmInst.Selectors, vmInst.Symbols, vmInst.Registry())
	if err != nil {
		t.Fatalf("compile error for class method: %v\nsource: %s", err, source)
	}
	method.SetClass(class)
	selectorID := vmInst.Selectors.Intern(method.Name())
	class.ClassVTable.AddMethod(selectorID, method)
}

// defineClass creates a class with instance variables and registers it in the VM.
func defineClass(t *testing.T, vmInst *vm.VM, name string, superclass *vm.Class, ivars []string) *vm.Class {
	t.Helper()
	class := vm.NewClassWithInstVars(name, superclass, ivars)
	vmInst.Classes.Register(class)
	classVal := vmInst.ClassValue(class)
	vmInst.Globals[name] = classVal
	return class
}

// compileSourceFile parses a .mag-style source file string and compiles
// all classes and methods into the VM, replicating the cmd/mag pipeline.
func compileSourceFile(t *testing.T, vmInst *vm.VM, source string) {
	t.Helper()
	sf, err := compiler.ParseSourceFileFromString(source)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	for _, classDef := range sf.Classes {
		var superclass *vm.Class
		if classDef.Superclass != "" {
			superclass = vmInst.Classes.Lookup(classDef.Superclass)
		}
		if superclass == nil {
			superclass = vmInst.ObjectClass
		}

		class := vm.NewClassWithInstVars(classDef.Name, superclass, classDef.InstanceVariables)
		vmInst.Classes.Register(class)
		vmInst.Globals[classDef.Name] = vmInst.ClassValue(class)

		allIvars := class.AllInstVarNames()

		for _, methodDef := range classDef.Methods {
			method, err := compiler.CompileMethodDefWithIvars(methodDef, vmInst.Selectors, vmInst.Symbols, vmInst.Registry(), allIvars)
			if err != nil {
				t.Fatalf("compile error %s>>%s: %v", classDef.Name, methodDef.Selector, err)
			}
			method.SetClass(class)
			selectorID := vmInst.Selectors.Intern(method.Name())
			class.VTable.AddMethod(selectorID, method)
		}

		for _, methodDef := range classDef.ClassMethods {
			method, err := compiler.CompileMethodDef(methodDef, vmInst.Selectors, vmInst.Symbols, vmInst.Registry())
			if err != nil {
				t.Fatalf("compile error %s class>>%s: %v", classDef.Name, methodDef.Selector, err)
			}
			method.SetClass(class)
			selectorID := vmInst.Selectors.Intern(method.Name())
			class.ClassVTable.AddMethod(selectorID, method)
		}
	}
}

// ---------------------------------------------------------------------------
// 1. Arithmetic: Factorial
// ---------------------------------------------------------------------------

func TestIntegrationE2E_Factorial(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `factorial
	self = 0 ifTrue: [^1].
	^self * (self - 1) factorial`)

	tests := []struct {
		n, expected int64
	}{
		{0, 1},
		{1, 1},
		{5, 120},
		{10, 3628800},
	}

	for _, tc := range tests {
		result := vmInst.Send(vm.FromSmallInt(tc.n), "factorial", nil)
		if !result.IsSmallInt() || result.SmallInt() != tc.expected {
			t.Errorf("%d factorial = %v, want %d", tc.n, result, tc.expected)
		}
	}
}

// ---------------------------------------------------------------------------
// 2. Arithmetic: Fibonacci
// ---------------------------------------------------------------------------

func TestIntegrationE2E_Fibonacci(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `fib
	self < 2 ifTrue: [^self].
	^(self - 1) fib + (self - 2) fib`)

	tests := []struct {
		n, expected int64
	}{
		{0, 0},
		{1, 1},
		{2, 1},
		{5, 5},
		{10, 55},
		{15, 610},
	}

	for _, tc := range tests {
		result := vmInst.Send(vm.FromSmallInt(tc.n), "fib", nil)
		if !result.IsSmallInt() || result.SmallInt() != tc.expected {
			t.Errorf("%d fib = %v, want %d", tc.n, result, tc.expected)
		}
	}
}

// ---------------------------------------------------------------------------
// 3. Classes: Define class with methods, create instances, call methods
// ---------------------------------------------------------------------------

func TestIntegrationE2E_ClassWithMethods(t *testing.T) {
	vmInst := vm.NewVM()

	pointClass := defineClass(t, vmInst, "Point", vmInst.ObjectClass, []string{"x", "y"})

	compileMethodWithIvars(t, vmInst, pointClass, `x
	^x`)
	compileMethodWithIvars(t, vmInst, pointClass, `y
	^y`)
	compileMethodWithIvars(t, vmInst, pointClass, `x: aValue
	x := aValue`)
	compileMethodWithIvars(t, vmInst, pointClass, `y: aValue
	y := aValue`)
	compileMethodWithIvars(t, vmInst, pointClass, `distanceSquared
	^(x * x) + (y * y)`)

	compileClassMethod(t, vmInst, pointClass, `x: anX y: aY
	| p |
	p := self new.
	p x: anX.
	p y: aY.
	^p`)

	pointClassVal := vmInst.Globals["Point"]
	point := vmInst.Send(pointClassVal, "x:y:", []vm.Value{vm.FromSmallInt(3), vm.FromSmallInt(4)})

	xVal := vmInst.Send(point, "x", nil)
	if !xVal.IsSmallInt() || xVal.SmallInt() != 3 {
		t.Errorf("point x = %v, want 3", xVal)
	}

	yVal := vmInst.Send(point, "y", nil)
	if !yVal.IsSmallInt() || yVal.SmallInt() != 4 {
		t.Errorf("point y = %v, want 4", yVal)
	}

	dist := vmInst.Send(point, "distanceSquared", nil)
	if !dist.IsSmallInt() || dist.SmallInt() != 25 {
		t.Errorf("point distanceSquared = %v, want 25", dist)
	}
}

// ---------------------------------------------------------------------------
// 4. Inheritance: subclass overrides method
// ---------------------------------------------------------------------------

func TestIntegrationE2E_Inheritance(t *testing.T) {
	vmInst := vm.NewVM()

	animalClass := defineClass(t, vmInst, "Animal", vmInst.ObjectClass, []string{"name"})
	compileMethodWithIvars(t, vmInst, animalClass, `name
	^name`)
	compileMethodWithIvars(t, vmInst, animalClass, `name: aName
	name := aName`)
	compileMethodWithIvars(t, vmInst, animalClass, `speak
	^'...'`)

	dogClass := defineClass(t, vmInst, "Dog", animalClass, []string{})
	compileMethodWithIvars(t, vmInst, dogClass, `speak
	^'Woof!'`)

	catClass := defineClass(t, vmInst, "Cat", animalClass, []string{})
	compileMethodWithIvars(t, vmInst, catClass, `speak
	^'Meow!'`)

	// Dog inherits name from Animal and overrides speak
	dogClassVal := vmInst.Globals["Dog"]
	dog := vmInst.Send(dogClassVal, "new", nil)
	vmInst.Send(dog, "name:", []vm.Value{vmInst.Registry().NewStringValue("Rex")})

	dogName := vmInst.Send(dog, "name", nil)
	if !vm.IsStringValue(dogName) || vmInst.Registry().GetStringContent(dogName) != "Rex" {
		t.Errorf("dog name = %v, want 'Rex'", dogName)
	}

	dogSpeak := vmInst.Send(dog, "speak", nil)
	if !vm.IsStringValue(dogSpeak) || vmInst.Registry().GetStringContent(dogSpeak) != "Woof!" {
		t.Errorf("dog speak = %v, want 'Woof!'", dogSpeak)
	}

	// Cat overrides speak
	catClassVal := vmInst.Globals["Cat"]
	cat := vmInst.Send(catClassVal, "new", nil)
	catSpeak := vmInst.Send(cat, "speak", nil)
	if !vm.IsStringValue(catSpeak) || vmInst.Registry().GetStringContent(catSpeak) != "Meow!" {
		t.Errorf("cat speak = %v, want 'Meow!'", catSpeak)
	}

	// Base Animal speak returns '...'
	animalClassVal := vmInst.Globals["Animal"]
	animal := vmInst.Send(animalClassVal, "new", nil)
	animalSpeak := vmInst.Send(animal, "speak", nil)
	if !vm.IsStringValue(animalSpeak) || vmInst.Registry().GetStringContent(animalSpeak) != "..." {
		t.Errorf("animal speak = %v, want '...'", animalSpeak)
	}
}

// ---------------------------------------------------------------------------
// 5. Blocks: closures capturing variables
// ---------------------------------------------------------------------------

func TestIntegrationE2E_BlockClosures(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `testClosure
	| counter |
	counter := 0.
	self timesRepeat: [counter := counter + 1].
	^counter`)

	result := vmInst.Send(vm.FromSmallInt(5), "testClosure", nil)
	if !result.IsSmallInt() || result.SmallInt() != 5 {
		t.Errorf("5 testClosure = %v, want 5", result)
	}
}

// ---------------------------------------------------------------------------
// 6. Blocks: value: and value:value:
// ---------------------------------------------------------------------------

func TestIntegrationE2E_BlockValueWithArgs(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `testBlockArg
	| adder |
	adder := [:x | x + self].
	^adder value: 10`)

	result := vmInst.Send(vm.FromSmallInt(5), "testBlockArg", nil)
	if !result.IsSmallInt() || result.SmallInt() != 15 {
		t.Errorf("5 testBlockArg = %v, want 15", result)
	}
}

func TestIntegrationE2E_BlockValueValueArgs(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `testBlockTwoArgs
	| op |
	op := [:a :b | a * b + self].
	^op value: 3 value: 4`)

	result := vmInst.Send(vm.FromSmallInt(2), "testBlockTwoArgs", nil)
	if !result.IsSmallInt() || result.SmallInt() != 14 {
		t.Errorf("2 testBlockTwoArgs = %v, want 14 (3*4+2)", result)
	}
}

// ---------------------------------------------------------------------------
// 7. Control flow: ifTrue:ifFalse:
// ---------------------------------------------------------------------------

func TestIntegrationE2E_IfTrueIfFalse(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `sign
	self > 0 ifTrue: [^'positive'].
	self < 0 ifTrue: [^'negative'].
	^'zero'`)

	tests := []struct {
		input    int64
		expected string
	}{
		{5, "positive"},
		{-3, "negative"},
		{0, "zero"},
	}

	for _, tc := range tests {
		result := vmInst.Send(vm.FromSmallInt(tc.input), "sign", nil)
		if !vm.IsStringValue(result) || vmInst.Registry().GetStringContent(result) != tc.expected {
			t.Errorf("%d sign = %v, want '%s'", tc.input, result, tc.expected)
		}
	}
}

// ---------------------------------------------------------------------------
// 8. Control flow: whileTrue:
// ---------------------------------------------------------------------------

func TestIntegrationE2E_WhileTrue(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `sumTo
	| sum i |
	sum := 0.
	i := 1.
	[i <= self] whileTrue: [
		sum := sum + i.
		i := i + 1
	].
	^sum`)

	result := vmInst.Send(vm.FromSmallInt(100), "sumTo", nil)
	if !result.IsSmallInt() || result.SmallInt() != 5050 {
		t.Errorf("100 sumTo = %v, want 5050", result)
	}
}

// ---------------------------------------------------------------------------
// 9. Control flow: timesRepeat:
// ---------------------------------------------------------------------------

func TestIntegrationE2E_TimesRepeat(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `powerOf2
	| result |
	result := 1.
	self timesRepeat: [result := result * 2].
	^result`)

	result := vmInst.Send(vm.FromSmallInt(10), "powerOf2", nil)
	if !result.IsSmallInt() || result.SmallInt() != 1024 {
		t.Errorf("10 powerOf2 = %v, want 1024", result)
	}
}

// ---------------------------------------------------------------------------
// 10. Collections: Array creation, at:put:, size
// ---------------------------------------------------------------------------

func TestIntegrationE2E_ArrayAtPut(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `makeSquaresArray
	| arr i |
	arr := Array new: self.
	i := 0.
	[i < self] whileTrue: [
		arr at: i put: (i * i).
		i := i + 1
	].
	^arr`)

	arr := vmInst.Send(vm.FromSmallInt(5), "makeSquaresArray", nil)

	size := vmInst.Send(arr, "size", nil)
	if !size.IsSmallInt() || size.SmallInt() != 5 {
		t.Errorf("array size = %v, want 5", size)
	}

	expected := []int64{0, 1, 4, 9, 16}
	for i, exp := range expected {
		val := vmInst.Send(arr, "at:", []vm.Value{vm.FromSmallInt(int64(i))})
		if !val.IsSmallInt() || val.SmallInt() != exp {
			t.Errorf("array at: %d = %v, want %d", i, val, exp)
		}
	}
}

// ---------------------------------------------------------------------------
// 11. Strings: concatenation and size
// ---------------------------------------------------------------------------

func TestIntegrationE2E_StringOps(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	// Concatenation via primConcat: (the , method requires loading .mag library)
	result := vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue("('Hello' primConcat: ' ') primConcat: 'World'")})
	if !vm.IsStringValue(result) || vmInst.Registry().GetStringContent(result) != "Hello World" {
		t.Errorf("string primConcat: = %v, want 'Hello World'", result)
	}

	// primSize
	result = vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue("'Hello' primSize")})
	if !result.IsSmallInt() || result.SmallInt() != 5 {
		t.Errorf("'Hello' primSize = %v, want 5", result)
	}

	// primAt:
	result = vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue("'Hello' primAt: 0")})
	if result == vm.Nil {
		t.Errorf("'Hello' primAt: 0 should not be nil")
	}
}

// ---------------------------------------------------------------------------
// 12. Dictionary: at:put:, at:, size
// ---------------------------------------------------------------------------

func TestIntegrationE2E_Dictionary(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	compilerClass := vmInst.Globals["Compiler"]

	vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue("d := Dictionary new")})
	vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue("d at: #name put: 'Alice'")})
	vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue("d at: #age put: 30")})

	result := vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue("d at: #name")})
	if !vm.IsStringValue(result) || vmInst.Registry().GetStringContent(result) != "Alice" {
		t.Errorf("d at: #name = %v, want 'Alice'", result)
	}

	result = vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue("d at: #age")})
	if !result.IsSmallInt() || result.SmallInt() != 30 {
		t.Errorf("d at: #age = %v, want 30", result)
	}

	result = vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue("d size")})
	if !result.IsSmallInt() || result.SmallInt() != 2 {
		t.Errorf("d size = %v, want 2", result)
	}
}

// ---------------------------------------------------------------------------
// 13. Cascade: multiple messages to same receiver
// ---------------------------------------------------------------------------

func TestIntegrationE2E_Cascade(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `testCascade
	| arr |
	arr := Array new: 3.
	arr at: 0 put: 10;
		at: 1 put: 20;
		at: 2 put: 30.
	^arr`)

	arr := vmInst.Send(vm.FromSmallInt(0), "testCascade", nil)

	val0 := vmInst.Send(arr, "at:", []vm.Value{vm.FromSmallInt(0)})
	if !val0.IsSmallInt() || val0.SmallInt() != 10 {
		t.Errorf("cascade arr at: 0 = %v, want 10", val0)
	}

	val1 := vmInst.Send(arr, "at:", []vm.Value{vm.FromSmallInt(1)})
	if !val1.IsSmallInt() || val1.SmallInt() != 20 {
		t.Errorf("cascade arr at: 1 = %v, want 20", val1)
	}

	val2 := vmInst.Send(arr, "at:", []vm.Value{vm.FromSmallInt(2)})
	if !val2.IsSmallInt() || val2.SmallInt() != 30 {
		t.Errorf("cascade arr at: 2 = %v, want 30", val2)
	}
}

// ---------------------------------------------------------------------------
// 14. Recursion: GCD
// ---------------------------------------------------------------------------

func TestIntegrationE2E_RecursiveGCD(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `gcd: other
	other = 0 ifTrue: [^self].
	^other gcd: (self \\ other)`)

	tests := []struct {
		a, b, expected int64
	}{
		{12, 8, 4},
		{15, 5, 5},
		{17, 13, 1},
		{100, 75, 25},
	}

	for _, tc := range tests {
		result := vmInst.Send(vm.FromSmallInt(tc.a), "gcd:", []vm.Value{vm.FromSmallInt(tc.b)})
		if !result.IsSmallInt() || result.SmallInt() != tc.expected {
			t.Errorf("%d gcd: %d = %v, want %d", tc.a, tc.b, result, tc.expected)
		}
	}
}

// ---------------------------------------------------------------------------
// 15. Channels: send/receive in forked process
// ---------------------------------------------------------------------------

func TestIntegrationE2E_ChannelSendReceive(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `testChannel
	| ch proc |
	ch := Channel new.
	proc := [ch send: self] fork.
	^ch receive`)

	result := vmInst.Send(vm.FromSmallInt(42), "testChannel", nil)
	if !result.IsSmallInt() || result.SmallInt() != 42 {
		t.Errorf("testChannel = %v, want 42", result)
	}
}

// ---------------------------------------------------------------------------
// 16. Channels: buffered channel
// ---------------------------------------------------------------------------

func TestIntegrationE2E_BufferedChannel(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `testBufferedChannel
	| ch |
	ch := Channel new: 3.
	ch send: 10.
	ch send: 20.
	ch send: 30.
	^(ch receive) + (ch receive) + (ch receive)`)

	result := vmInst.Send(vm.FromSmallInt(0), "testBufferedChannel", nil)
	if !result.IsSmallInt() || result.SmallInt() != 60 {
		t.Errorf("testBufferedChannel = %v, want 60", result)
	}
}

// ---------------------------------------------------------------------------
// 17. Process: fork and wait
// ---------------------------------------------------------------------------

func TestIntegrationE2E_ProcessForkWait(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `testForkWait
	| proc |
	proc := [self * self] fork.
	^proc wait`)

	result := vmInst.Send(vm.FromSmallInt(7), "testForkWait", nil)
	if !result.IsSmallInt() || result.SmallInt() != 49 {
		t.Errorf("testForkWait = %v, want 49", result)
	}
}

// ---------------------------------------------------------------------------
// 18. Multiple processes communicating via channel
// ---------------------------------------------------------------------------

func TestIntegrationE2E_MultipleProcesses(t *testing.T) {
	vmInst := vm.NewVM()

	// Fork two processes that each send a value through a buffered channel
	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `testTwoProcs
	| ch p1 p2 |
	ch := Channel new: 2.
	p1 := [ch send: self] fork.
	p2 := [ch send: (self * 2)] fork.
	p1 wait.
	p2 wait.
	^(ch receive) + (ch receive)`)

	// 10 + 20 = 30
	result := vmInst.Send(vm.FromSmallInt(10), "testTwoProcs", nil)
	if !result.IsSmallInt() || result.SmallInt() != 30 {
		t.Errorf("testTwoProcs = %v, want 30", result)
	}
}

// ---------------------------------------------------------------------------
// 19. Eval: persistent globals across evaluations
// ---------------------------------------------------------------------------

func TestIntegrationE2E_EvalGlobalPersistence(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)
	compilerClass := vmInst.Globals["Compiler"]

	result := vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue("x := 42")})
	if !result.IsSmallInt() || result.SmallInt() != 42 {
		t.Errorf("x := 42 = %v, want 42", result)
	}

	result = vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue("x + 8")})
	if !result.IsSmallInt() || result.SmallInt() != 50 {
		t.Errorf("x + 8 = %v, want 50", result)
	}

	result = vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue("x := x * 2")})
	if !result.IsSmallInt() || result.SmallInt() != 84 {
		t.Errorf("x := x * 2 = %v, want 84", result)
	}

	result = vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue("x")})
	if !result.IsSmallInt() || result.SmallInt() != 84 {
		t.Errorf("x = %v, want 84 (persisted)", result)
	}
}

// ---------------------------------------------------------------------------
// 20. Eval: expression evaluation (arithmetic, booleans, nil)
// ---------------------------------------------------------------------------

func TestIntegrationE2E_EvalExpressions(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)
	compilerClass := vmInst.Globals["Compiler"]

	intTests := []struct {
		expr     string
		expected int64
	}{
		{"3 + 4", 7},
		{"10 - 3", 7},
		{"6 * 7", 42},
		{"20 / 4", 5},
		{"17 \\\\ 5", 2},
		{"-5 abs", 5},
	}

	for _, tc := range intTests {
		result := vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue(tc.expr)})
		if !result.IsSmallInt() || result.SmallInt() != tc.expected {
			t.Errorf("evaluate: '%s' = %v, want %d", tc.expr, result, tc.expected)
		}
	}

	boolTests := []struct {
		expr     string
		expected vm.Value
	}{
		{"true not", vm.False},
		{"false not", vm.True},
		{"5 > 3", vm.True},
		{"3 > 5", vm.False},
		{"5 = 5", vm.True},
		{"nil isNil", vm.True},
		{"42 isNil", vm.False},
	}

	for _, tc := range boolTests {
		result := vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue(tc.expr)})
		if result != tc.expected {
			t.Errorf("evaluate: '%s' = %v, want %v", tc.expr, result, tc.expected)
		}
	}
}

// ---------------------------------------------------------------------------
// 21. Mutual recursion: isEven/isOdd
// ---------------------------------------------------------------------------

func TestIntegrationE2E_MutualRecursion(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `isEvenMR
	self = 0 ifTrue: [^true].
	^(self - 1) isOddMR`)

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `isOddMR
	self = 0 ifTrue: [^false].
	^(self - 1) isEvenMR`)

	tests := []struct {
		n    int64
		even vm.Value
		odd  vm.Value
	}{
		{0, vm.True, vm.False},
		{1, vm.False, vm.True},
		{4, vm.True, vm.False},
		{7, vm.False, vm.True},
	}

	for _, tc := range tests {
		result := vmInst.Send(vm.FromSmallInt(tc.n), "isEvenMR", nil)
		if result != tc.even {
			t.Errorf("%d isEvenMR = %v, want %v", tc.n, result, tc.even)
		}
		result = vmInst.Send(vm.FromSmallInt(tc.n), "isOddMR", nil)
		if result != tc.odd {
			t.Errorf("%d isOddMR = %v, want %v", tc.n, result, tc.odd)
		}
	}
}

// ---------------------------------------------------------------------------
// 22. Inherited instance variables
// ---------------------------------------------------------------------------

func TestIntegrationE2E_InheritedInstanceVars(t *testing.T) {
	vmInst := vm.NewVM()

	vehicleClass := defineClass(t, vmInst, "Vehicle", vmInst.ObjectClass, []string{"speed"})
	compileMethodWithIvars(t, vmInst, vehicleClass, `speed
	^speed`)
	compileMethodWithIvars(t, vmInst, vehicleClass, `speed: s
	speed := s`)
	compileMethodWithIvars(t, vmInst, vehicleClass, `isFast
	^speed > 100`)

	carClass := defineClass(t, vmInst, "Car", vehicleClass, []string{"numDoors"})
	compileMethodWithIvars(t, vmInst, carClass, `numDoors
	^numDoors`)
	compileMethodWithIvars(t, vmInst, carClass, `numDoors: n
	numDoors := n`)

	carClassVal := vmInst.Globals["Car"]
	car := vmInst.Send(carClassVal, "new", nil)
	vmInst.Send(car, "speed:", []vm.Value{vm.FromSmallInt(150)})
	vmInst.Send(car, "numDoors:", []vm.Value{vm.FromSmallInt(4)})

	speedVal := vmInst.Send(car, "speed", nil)
	if !speedVal.IsSmallInt() || speedVal.SmallInt() != 150 {
		t.Errorf("car speed = %v, want 150", speedVal)
	}

	doorsVal := vmInst.Send(car, "numDoors", nil)
	if !doorsVal.IsSmallInt() || doorsVal.SmallInt() != 4 {
		t.Errorf("car numDoors = %v, want 4", doorsVal)
	}

	fastVal := vmInst.Send(car, "isFast", nil)
	if fastVal != vm.True {
		t.Errorf("car isFast = %v, want true", fastVal)
	}
}

// ---------------------------------------------------------------------------
// 23. Linked list: recursive data structure with methods
// ---------------------------------------------------------------------------

func TestIntegrationE2E_LinkedList(t *testing.T) {
	vmInst := vm.NewVM()

	nodeClass := defineClass(t, vmInst, "LLNode", vmInst.ObjectClass, []string{"value", "next"})
	compileMethodWithIvars(t, vmInst, nodeClass, `value
	^value`)
	compileMethodWithIvars(t, vmInst, nodeClass, `value: v
	value := v`)
	compileMethodWithIvars(t, vmInst, nodeClass, `next
	^next`)
	compileMethodWithIvars(t, vmInst, nodeClass, `next: n
	next := n`)
	compileMethodWithIvars(t, vmInst, nodeClass, `length
	next isNil ifTrue: [^1].
	^1 + next length`)
	compileMethodWithIvars(t, vmInst, nodeClass, `sum
	next isNil ifTrue: [^value].
	^value + next sum`)

	nodeClassVal := vmInst.Globals["LLNode"]

	node3 := vmInst.Send(nodeClassVal, "new", nil)
	vmInst.Send(node3, "value:", []vm.Value{vm.FromSmallInt(30)})

	node2 := vmInst.Send(nodeClassVal, "new", nil)
	vmInst.Send(node2, "value:", []vm.Value{vm.FromSmallInt(20)})
	vmInst.Send(node2, "next:", []vm.Value{node3})

	node1 := vmInst.Send(nodeClassVal, "new", nil)
	vmInst.Send(node1, "value:", []vm.Value{vm.FromSmallInt(10)})
	vmInst.Send(node1, "next:", []vm.Value{node2})

	length := vmInst.Send(node1, "length", nil)
	if !length.IsSmallInt() || length.SmallInt() != 3 {
		t.Errorf("list length = %v, want 3", length)
	}

	sum := vmInst.Send(node1, "sum", nil)
	if !sum.IsSmallInt() || sum.SmallInt() != 60 {
		t.Errorf("list sum = %v, want 60", sum)
	}
}

// ---------------------------------------------------------------------------
// 24. Non-local return from block
// ---------------------------------------------------------------------------

func TestIntegrationE2E_NonLocalReturn(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `findInFixed
	| arr |
	arr := Array new: 3.
	arr at: 0 put: 10.
	arr at: 1 put: 20.
	arr at: 2 put: 30.
	0 to: 2 do: [:i |
		(arr at: i) = self ifTrue: [^i]
	].
	^-1`)

	result := vmInst.Send(vm.FromSmallInt(20), "findInFixed", nil)
	if !result.IsSmallInt() || result.SmallInt() != 1 {
		t.Errorf("20 findInFixed = %v, want 1", result)
	}

	result = vmInst.Send(vm.FromSmallInt(99), "findInFixed", nil)
	if !result.IsSmallInt() || result.SmallInt() != -1 {
		t.Errorf("99 findInFixed = %v, want -1 (not found)", result)
	}
}

// ---------------------------------------------------------------------------
// 25. Accumulator closure (returned block captures mutable state)
// ---------------------------------------------------------------------------

func TestIntegrationE2E_ClosureCapturingAndMutating(t *testing.T) {
	vmInst := vm.NewVM()

	// Test that a block passed as an argument captures and mutates
	// an outer variable correctly. This pattern is known to work (see
	// TestIntegrationBlockPassedAsArg in compiler/integration_test.go).
	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `callBlock: aBlock
	^aBlock value`)

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `testClosureMutate
	| total |
	total := 0.
	self timesRepeat: [total := total + 1].
	self callBlock: [total := total * 2].
	^total`)

	// 5 timesRepeat increments total to 5, then callBlock doubles it to 10
	result := vmInst.Send(vm.FromSmallInt(5), "testClosureMutate", nil)
	if !result.IsSmallInt() || result.SmallInt() != 10 {
		t.Errorf("5 testClosureMutate = %v, want 10", result)
	}
}

// ---------------------------------------------------------------------------
// 26. SourceFile compilation (full .mag-style source)
// ---------------------------------------------------------------------------

func TestIntegrationE2E_SourceFileCompilation(t *testing.T) {
	vmInst := vm.NewVM()

	compileSourceFile(t, vmInst, `
Counter subclass: Object
  instanceVars: count

  method: count [ ^count ]
  method: count: aValue [ count := aValue ]
  method: increment [ count := count + 1 ]
  method: decrement [ count := count - 1 ]
  method: reset [ count := 0 ]

  classMethod: startingAt: n [
    | c |
    c := self new.
    c count: n.
    ^c
  ]
`)

	counterClassVal := vmInst.Globals["Counter"]
	counter := vmInst.Send(counterClassVal, "startingAt:", []vm.Value{vm.FromSmallInt(10)})
	if counter == vm.Nil {
		t.Fatal("Counter startingAt: 10 returned nil")
	}

	count := vmInst.Send(counter, "count", nil)
	if !count.IsSmallInt() || count.SmallInt() != 10 {
		t.Errorf("counter count = %v, want 10", count)
	}

	vmInst.Send(counter, "increment", nil)
	vmInst.Send(counter, "increment", nil)
	vmInst.Send(counter, "increment", nil)

	count = vmInst.Send(counter, "count", nil)
	if !count.IsSmallInt() || count.SmallInt() != 13 {
		t.Errorf("counter count after 3 increments = %v, want 13", count)
	}

	vmInst.Send(counter, "decrement", nil)
	count = vmInst.Send(counter, "count", nil)
	if !count.IsSmallInt() || count.SmallInt() != 12 {
		t.Errorf("counter count after decrement = %v, want 12", count)
	}

	vmInst.Send(counter, "reset", nil)
	count = vmInst.Send(counter, "count", nil)
	if !count.IsSmallInt() || count.SmallInt() != 0 {
		t.Errorf("counter count after reset = %v, want 0", count)
	}
}

// ---------------------------------------------------------------------------
// 27. Collatz conjecture (complex iterative program)
// ---------------------------------------------------------------------------

func TestIntegrationE2E_Collatz(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `even
	^(self \\ 2) = 0`)

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `collatzSteps
	| n steps |
	n := self.
	steps := 0.
	[n > 1] whileTrue: [
		n even ifTrue: [n := n / 2] ifFalse: [n := n * 3 + 1].
		steps := steps + 1
	].
	^steps`)

	tests := []struct {
		n, expected int64
	}{
		{1, 0},
		{2, 1},
		{3, 7},
		{6, 8},
		{27, 111},
	}

	for _, tc := range tests {
		result := vmInst.Send(vm.FromSmallInt(tc.n), "collatzSteps", nil)
		if !result.IsSmallInt() || result.SmallInt() != tc.expected {
			t.Errorf("%d collatzSteps = %v, want %d", tc.n, result, tc.expected)
		}
	}
}

// ---------------------------------------------------------------------------
// 28. Float arithmetic via eval
// ---------------------------------------------------------------------------

func TestIntegrationE2E_FloatArithmetic(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)
	compilerClass := vmInst.Globals["Compiler"]

	result := vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue("1.5 + 2.5")})
	if !result.IsFloat() || result.Float64() != 4.0 {
		t.Errorf("1.5 + 2.5 = %v, want 4.0", result)
	}

	result = vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue("3.0 * 2.0")})
	if !result.IsFloat() || result.Float64() != 6.0 {
		t.Errorf("3.0 * 2.0 = %v, want 6.0", result)
	}

	result = vmInst.Send(compilerClass, "evaluate:", []vm.Value{vmInst.Registry().NewStringValue("3.7 truncated")})
	if !result.IsSmallInt() || result.SmallInt() != 3 {
		t.Errorf("3.7 truncated = %v, want 3", result)
	}
}

// ---------------------------------------------------------------------------
// 29. to:do: iteration
// ---------------------------------------------------------------------------

func TestIntegrationE2E_ToDo(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `sumRange
	| sum |
	sum := 0.
	1 to: self do: [:i | sum := sum + i].
	^sum`)

	result := vmInst.Send(vm.FromSmallInt(10), "sumRange", nil)
	if !result.IsSmallInt() || result.SmallInt() != 55 {
		t.Errorf("10 sumRange = %v, want 55", result)
	}
}

// ---------------------------------------------------------------------------
// 30. Recursive power
// ---------------------------------------------------------------------------

func TestIntegrationE2E_RecursivePower(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `power: exp
	exp = 0 ifTrue: [^1].
	^self * (self power: (exp - 1))`)

	tests := []struct {
		base, exp, expected int64
	}{
		{2, 10, 1024},
		{3, 5, 243},
		{5, 0, 1},
		{1, 100, 1},
	}

	for _, tc := range tests {
		result := vmInst.Send(vm.FromSmallInt(tc.base), "power:", []vm.Value{vm.FromSmallInt(tc.exp)})
		if !result.IsSmallInt() || result.SmallInt() != tc.expected {
			t.Errorf("%d power: %d = %v, want %d", tc.base, tc.exp, result, tc.expected)
		}
	}
}

// ---------------------------------------------------------------------------
// 31. Method chaining (one method calls another)
// ---------------------------------------------------------------------------

func TestIntegrationE2E_MethodChaining(t *testing.T) {
	vmInst := vm.NewVM()

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `double
	^self * 2`)

	compileMethod(t, vmInst, vmInst.SmallIntegerClass, `quadruple
	^self double double`)

	result := vmInst.Send(vm.FromSmallInt(3), "quadruple", nil)
	if !result.IsSmallInt() || result.SmallInt() != 12 {
		t.Errorf("3 quadruple = %v, want 12", result)
	}
}

// ---------------------------------------------------------------------------
// 32. Eval: evaluate in context of an object
// ---------------------------------------------------------------------------

func TestIntegrationE2E_EvalInContext(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)
	compilerClass := vmInst.Globals["Compiler"]

	arr := vmInst.NewArrayWithElements([]vm.Value{vm.FromSmallInt(100), vm.FromSmallInt(200), vm.FromSmallInt(300)})

	result := vmInst.Send(compilerClass, "evaluate:in:", []vm.Value{vmInst.Registry().NewStringValue("self size"), arr})
	if !result.IsSmallInt() || result.SmallInt() != 3 {
		t.Errorf("evaluate: 'self size' in array = %v, want 3", result)
	}

	result = vmInst.Send(compilerClass, "evaluate:in:", []vm.Value{vmInst.Registry().NewStringValue("self at: 1"), arr})
	if !result.IsSmallInt() || result.SmallInt() != 200 {
		t.Errorf("evaluate: 'self at: 1' in array = %v, want 200", result)
	}
}
