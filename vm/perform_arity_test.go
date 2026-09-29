package vm

import "testing"

func TestSelectorArity(t *testing.T) {
	for sel, want := range map[string]int{
		"printString": 0, "+": 1, "->": 1, "at:": 1, "at:put:": 2, "_x": 0,
	} {
		if got := selectorArity(sel); got != want {
			t.Errorf("selectorArity(%q) = %d, want %d", sel, got, want)
		}
	}
}

// perform: must not pad missing arguments with nil or drop extra ones.
func TestPerformChecksArity(t *testing.T) {
	vm := NewVM()
	three := FromSmallInt(3)
	sym := func(s string) Value { return vm.Symbols.SymbolValue(s) }

	if got := vm.Send(three, "perform:with:", []Value{sym("+"), FromSmallInt(4)}); got != FromSmallInt(7) {
		t.Errorf("3 perform: #+ with: 4 = %v, want 7", got)
	}

	for name, args := range map[string][]Value{
		"missing argument":    {sym("between:and:"), FromSmallInt(1)},
		"extra argument":      {sym("negated"), FromSmallInt(4)},
		"non-symbol selector": {FromSmallInt(42), FromSmallInt(4)},
	} {
		func() {
			defer func() {
				if _, ok := recover().(SignaledException); !ok {
					t.Errorf("%s: perform:with: did not signal", name)
				}
			}()
			vm.Send(three, "perform:with:", args)
		}()
	}
}
