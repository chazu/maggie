package vm

import (
	"math/big"
	"testing"
)

// expectJsonParseError runs fn and requires it to signal JsonParseError.
func expectJsonParseError(t *testing.T, what string, fn func()) {
	t.Helper()
	defer func() {
		r := recover()
		sig, ok := r.(SignaledException)
		if !ok || sig.Object.ExceptionClass.Name != "JsonParseError" {
			t.Errorf("%s: want JsonParseError, got %v", what, r)
		}
	}()
	fn()
}

func TestJsonDecodeRejectsTrailingData(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	for _, in := range []string{`{"a":1} garbage`, `1 2`, `[1,2] ]`} {
		expectJsonParseError(t, in, func() {
			vm.Send(jsonClass, "primDecode:", []Value{vm.registry.NewStringValue(in)})
		})
	}
	// Trailing whitespace is fine.
	got := vm.Send(jsonClass, "primDecode:", []Value{vm.registry.NewStringValue("7 \n")})
	if got != FromSmallInt(7) {
		t.Errorf("decode of '7 \\n' = %v, want 7", got)
	}
}

// A self-containing Array used to recurse until the Go stack overflowed,
// killing the process with an uncatchable fatal error.
func TestJsonEncodeCyclicSignals(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	arr := vm.NewArrayWithElements([]Value{Nil})
	ObjectFromValue(arr).SetSlot(0, arr)
	expectJsonParseError(t, "cyclic array", func() {
		vm.Send(jsonClass, "primEncode:", []Value{arr})
	})
}

func TestJsonEncodeTypes(t *testing.T) {
	vm := NewVM()
	jsonClass := vm.classValue(vm.Classes.Lookup("Json"))
	enc := func(v Value) Value { return vm.Send(jsonClass, "primEncode:", []Value{v}) }

	big1, _ := new(big.Int).SetString("123456789012345678901234567890", 10)
	assertStringResult(t, vm, enc(vm.registry.NewBigIntValue(big1)), "123456789012345678901234567890")

	al := vm.registerArrayList(createArrayList(2))
	vm.Send(al, "add:", []Value{FromSmallInt(1)})
	vm.Send(al, "add:", []Value{FromSmallInt(2)})
	assertStringResult(t, vm, enc(al), "[1,2]")

	d := vm.registry.NewDictionaryValue()
	vm.Send(d, "at:put:", []Value{vm.Symbols.SymbolValue("sym"), FromSmallInt(1)})
	assertStringResult(t, vm, enc(d), `{"sym":1}`)

	bad := vm.registry.NewDictionaryValue()
	vm.Send(bad, "at:put:", []Value{FromFloat64(1.5), FromSmallInt(1)})
	expectJsonParseError(t, "float key", func() { enc(bad) })

	expectJsonParseError(t, "plain object", func() {
		enc(vm.Send(vm.classValue(vm.ObjectClass), "new", nil))
	})
}
