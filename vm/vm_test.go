package vm

import (
	"testing"
)

// ---------------------------------------------------------------------------
// VM bootstrap tests
// ---------------------------------------------------------------------------

func TestNewVM(t *testing.T) {
	vm := NewVM()
	if vm == nil {
		t.Fatal("NewVM returned nil")
	}
	if vm.Selectors == nil {
		t.Error("Selectors should be initialized")
	}
	if vm.Symbols == nil {
		t.Error("Symbols should be initialized")
	}
	if vm.Classes == nil {
		t.Error("Classes should be initialized")
	}
}

func TestVMBootstrapClasses(t *testing.T) {
	vm := NewVM()

	// Check core classes exist
	classes := []struct {
		name  string
		class **Class
	}{
		{"Object", &vm.ObjectClass},
		{"Class", &vm.ClassClass},
		{"Boolean", &vm.BooleanClass},
		{"True", &vm.TrueClass},
		{"False", &vm.FalseClass},
		{"UndefinedObject", &vm.UndefinedObjectClass},
		{"SmallInteger", &vm.SmallIntegerClass},
		{"Float", &vm.FloatClass},
		{"String", &vm.StringClass},
		{"Symbol", &vm.SymbolClass},
		{"Array", &vm.ArrayClass},
		{"Block", &vm.BlockClass},
	}

	for _, tc := range classes {
		if *tc.class == nil {
			t.Errorf("%s class not bootstrapped", tc.name)
		}
		if (*tc.class).Name != tc.name {
			t.Errorf("%s class has wrong name: %s", tc.name, (*tc.class).Name)
		}
	}
}

func TestVMBootstrapInheritance(t *testing.T) {
	vm := NewVM()

	// True and False should inherit from Boolean
	if vm.TrueClass.Superclass != vm.BooleanClass {
		t.Error("True should inherit from Boolean")
	}
	if vm.FalseClass.Superclass != vm.BooleanClass {
		t.Error("False should inherit from Boolean")
	}

	// Boolean should inherit from Object
	if vm.BooleanClass.Superclass != vm.ObjectClass {
		t.Error("Boolean should inherit from Object")
	}

	// Symbol should inherit from String
	if vm.SymbolClass.Superclass != vm.StringClass {
		t.Error("Symbol should inherit from String")
	}
}

func TestVMBootstrapGlobals(t *testing.T) {
	vm := NewVM()

	// Check that globals are set
	if _, ok := vm.Globals["Object"]; !ok {
		t.Error("Object global should be set")
	}
	if _, ok := vm.Globals["nil"]; !ok {
		t.Error("nil global should be set")
	}
	if _, ok := vm.Globals["true"]; !ok {
		t.Error("true global should be set")
	}
	if _, ok := vm.Globals["false"]; !ok {
		t.Error("false global should be set")
	}
}

// ---------------------------------------------------------------------------
// Class lookup tests
// ---------------------------------------------------------------------------

func TestVMClassFor(t *testing.T) {
	vm := NewVM()

	tests := []struct {
		value    Value
		expected *Class
	}{
		{Nil, vm.UndefinedObjectClass},
		{True, vm.TrueClass},
		{False, vm.FalseClass},
		{FromSmallInt(42), vm.SmallIntegerClass},
		{FromFloat64(3.14), vm.FloatClass},
	}

	for _, tc := range tests {
		got := vm.ClassFor(tc.value)
		if got != tc.expected {
			t.Errorf("ClassFor(%v) = %v, want %v", tc.value, got, tc.expected)
		}
	}
}

func TestVMLookupClass(t *testing.T) {
	vm := NewVM()

	c := vm.LookupClass("SmallInteger")
	if c != vm.SmallIntegerClass {
		t.Error("LookupClass(SmallInteger) should return SmallIntegerClass")
	}

	c = vm.LookupClass("NoSuchClass")
	if c != nil {
		t.Error("LookupClass for missing class should return nil")
	}
}

// ---------------------------------------------------------------------------
// Send tests
// ---------------------------------------------------------------------------

func TestVMSendBasic(t *testing.T) {
	vm := NewVM()

	// Send isNil to nil
	result := vm.Send(Nil, "isNil", nil)
	if result != True {
		t.Errorf("nil isNil = %v, want true", result)
	}

	// Send notNil to nil
	result = vm.Send(Nil, "notNil", nil)
	if result != False {
		t.Errorf("nil notNil = %v, want false", result)
	}

	// Send isNil to an integer
	result = vm.Send(FromSmallInt(42), "isNil", nil)
	if result != False {
		t.Errorf("42 isNil = %v, want false", result)
	}
}

func TestVMSendArithmetic(t *testing.T) {
	vm := NewVM()

	// 10 + 5
	result := vm.Send(FromSmallInt(10), "+", []Value{FromSmallInt(5)})
	if !result.IsSmallInt() || result.SmallInt() != 15 {
		t.Errorf("10 + 5 = %v, want 15", result)
	}

	// 10 - 3
	result = vm.Send(FromSmallInt(10), "-", []Value{FromSmallInt(3)})
	if result.SmallInt() != 7 {
		t.Errorf("10 - 3 = %v, want 7", result)
	}

	// 6 * 7
	result = vm.Send(FromSmallInt(6), "*", []Value{FromSmallInt(7)})
	if result.SmallInt() != 42 {
		t.Errorf("6 * 7 = %v, want 42", result)
	}

	// 20 / 4
	result = vm.Send(FromSmallInt(20), "/", []Value{FromSmallInt(4)})
	if result.SmallInt() != 5 {
		t.Errorf("20 / 4 = %v, want 5", result)
	}

	// 17 \\ 5 (modulo)
	result = vm.Send(FromSmallInt(17), "\\\\", []Value{FromSmallInt(5)})
	if result.SmallInt() != 2 {
		t.Errorf("17 \\\\ 5 = %v, want 2", result)
	}
}

func TestVMSendComparison(t *testing.T) {
	vm := NewVM()

	// 5 < 10
	result := vm.Send(FromSmallInt(5), "<", []Value{FromSmallInt(10)})
	if result != True {
		t.Errorf("5 < 10 = %v, want true", result)
	}

	// 10 > 5
	result = vm.Send(FromSmallInt(10), ">", []Value{FromSmallInt(5)})
	if result != True {
		t.Errorf("10 > 5 = %v, want true", result)
	}

	// 5 = 5
	result = vm.Send(FromSmallInt(5), "=", []Value{FromSmallInt(5)})
	if result != True {
		t.Errorf("5 = 5 = %v, want true", result)
	}

	// 5 <= 5
	result = vm.Send(FromSmallInt(5), "<=", []Value{FromSmallInt(5)})
	if result != True {
		t.Errorf("5 <= 5 = %v, want true", result)
	}
}

func TestVMSendBoolean(t *testing.T) {
	vm := NewVM()

	// true not
	result := vm.Send(True, "not", nil)
	if result != False {
		t.Errorf("true not = %v, want false", result)
	}

	// false not
	result = vm.Send(False, "not", nil)
	if result != True {
		t.Errorf("false not = %v, want true", result)
	}

	// true & false
	result = vm.Send(True, "&", []Value{False})
	if result != False {
		t.Errorf("true & false = %v, want false", result)
	}

	// true | false
	result = vm.Send(True, "|", []Value{False})
	if result != True {
		t.Errorf("true | false = %v, want true", result)
	}
}

func TestVMSendIdentity(t *testing.T) {
	vm := NewVM()

	// x == x
	val := FromSmallInt(42)
	result := vm.Send(val, "==", []Value{val})
	if result != True {
		t.Errorf("x == x = %v, want true", result)
	}

	// x == y (different values)
	result = vm.Send(FromSmallInt(42), "==", []Value{FromSmallInt(43)})
	if result != False {
		t.Errorf("42 == 43 = %v, want false", result)
	}

	// x ~~ y
	result = vm.Send(FromSmallInt(42), "~~", []Value{FromSmallInt(43)})
	if result != True {
		t.Errorf("42 ~~ 43 = %v, want true", result)
	}
}

func TestVMSendFloat(t *testing.T) {
	vm := NewVM()

	// 1.5 + 2.5
	result := vm.Send(FromFloat64(1.5), "+", []Value{FromFloat64(2.5)})
	if !result.IsFloat() || result.Float64() != 4.0 {
		t.Errorf("1.5 + 2.5 = %v, want 4.0", result)
	}

	// 3.14 negated
	result = vm.Send(FromFloat64(3.14), "negated", nil)
	if result.Float64() != -3.14 {
		t.Errorf("3.14 negated = %v, want -3.14", result)
	}

	// 3.7 truncated
	result = vm.Send(FromFloat64(3.7), "truncated", nil)
	if result.SmallInt() != 3 {
		t.Errorf("3.7 truncated = %v, want 3", result)
	}
}

func TestVMSendYourself(t *testing.T) {
	vm := NewVM()

	val := FromSmallInt(42)
	result := vm.Send(val, "yourself", nil)
	if result != val {
		t.Errorf("yourself should return receiver")
	}
}

// ---------------------------------------------------------------------------
// Symbol tests
// ---------------------------------------------------------------------------

func TestVMSymbolIntern(t *testing.T) {
	vm := NewVM()

	id1 := vm.Intern("foo")
	id2 := vm.Intern("foo")
	id3 := vm.Intern("bar")

	if id1 != id2 {
		t.Error("same symbol should return same ID")
	}
	if id1 == id3 {
		t.Error("different symbols should return different IDs")
	}

	if vm.SymbolName(id1) != "foo" {
		t.Errorf("SymbolName(%d) = %q, want %q", id1, vm.SymbolName(id1), "foo")
	}
}

// ---------------------------------------------------------------------------
// Global tests
// ---------------------------------------------------------------------------

func TestVMGlobals(t *testing.T) {
	vm := NewVM()

	vm.SetGlobal("answer", FromSmallInt(42))

	val, ok := vm.LookupGlobal("answer")
	if !ok {
		t.Error("LookupGlobal should find 'answer'")
	}
	if val.SmallInt() != 42 {
		t.Errorf("answer = %v, want 42", val)
	}

	_, ok = vm.LookupGlobal("noSuchGlobal")
	if ok {
		t.Error("LookupGlobal should not find missing global")
	}
}

// ---------------------------------------------------------------------------
// Execute compiled method tests
// ---------------------------------------------------------------------------

func TestVMExecuteMethod(t *testing.T) {
	vm := NewVM()

	// Build a simple method: ^42
	b := NewCompiledMethodBuilder("answer", 0)
	b.Bytecode().EmitInt8(OpPushInt8, 42)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := vm.Execute(m, Nil, nil)
	if result.SmallInt() != 42 {
		t.Errorf("result = %v, want 42", result)
	}
}

func TestVMExecuteWithArgs(t *testing.T) {
	vm := NewVM()

	// Method: add: x ^x + 1
	b := NewCompiledMethodBuilder("add:", 1)
	b.Bytecode().EmitByte(OpPushTemp, 0)
	b.Bytecode().EmitInt8(OpPushInt8, 1)
	b.Bytecode().Emit(OpSendPlus)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	result := vm.Execute(m, Nil, []Value{FromSmallInt(10)})
	if result.SmallInt() != 11 {
		t.Errorf("result = %v, want 11", result)
	}
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

func BenchmarkVMSendArithmetic(b *testing.B) {
	vm := NewVM()
	val := FromSmallInt(10)
	arg := []Value{FromSmallInt(5)}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = vm.Send(val, "+", arg)
	}
}

func BenchmarkVMSendBoolean(b *testing.B) {
	vm := NewVM()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = vm.Send(True, "not", nil)
	}
}

func BenchmarkVMClassFor(b *testing.B) {
	vm := NewVM()
	val := FromSmallInt(42)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = vm.ClassFor(val)
	}
}
