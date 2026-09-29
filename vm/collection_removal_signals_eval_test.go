package vm_test

import (
	"testing"

	"github.com/chazu/maggie/vm"
)

// Failure doctrine (docs/CONVENTIONS.md §1–2): removing something that is not
// there is a programmer error and signals a catchable exception; nil is never
// the failure signal. The tolerant forms (remove:ifAbsent:,
// removeKey:ifAbsent:) are the explicit-absence variants.

func evalSym(t *testing.T, v *vm.VM, r vm.Value) string {
	t.Helper()
	if !r.IsSymbol() {
		t.Fatalf("expected a Symbol result, got %v", r)
	}
	return v.Symbols.Name(r.SymbolID())
}

func TestArrayListRemovalsSignalSubscriptOutOfBounds(t *testing.T) {
	v, eval := newEvalVM(t)
	cases := map[string]string{
		"removeAt: past end": `[(ArrayList withAll: #(1 2)) removeAt: 3. #none] on: SubscriptOutOfBounds do: [:e | #signaled]`,
		"removeAt: zero":     `[(ArrayList withAll: #(1 2)) removeAt: 0. #none] on: SubscriptOutOfBounds do: [:e | #signaled]`,
		"removeLast empty":   `[ArrayList new removeLast. #none] on: SubscriptOutOfBounds do: [:e | #signaled]`,
		"removeFirst empty":  `[ArrayList new removeFirst. #none] on: SubscriptOutOfBounds do: [:e | #signaled]`,
	}
	for name, src := range cases {
		if got := evalSym(t, v, eval(src)); got != "signaled" {
			t.Errorf("%s: want #signaled, got #%s", name, got)
		}
	}
	// A removed nil element is a legitimate answer, not a failure.
	if r := eval(`| l | l := ArrayList new. l add: nil. l removeLast`); r != vm.Nil {
		t.Errorf("removeLast of a stored nil: want nil, got %v", r)
	}
	// The list is untouched by a failed removal.
	r := eval(`| l | l := ArrayList withAll: #(1 2). [l removeAt: 5] on: SubscriptOutOfBounds do: [:e | nil]. l size`)
	if !r.IsSmallInt() || r.SmallInt() != 2 {
		t.Errorf("failed removeAt: must not mutate the list, size = %v", r)
	}
}

func TestDictionaryRemoveKeySignalsKeyNotFound(t *testing.T) {
	v, eval := newEvalVM(t)
	if got := evalSym(t, v, eval(`[Dictionary new removeKey: #nope. #none] on: KeyNotFound do: [:e | #signaled]`)); got != "signaled" {
		t.Errorf("removeKey: absent: want #signaled, got #%s", got)
	}
	// KeyNotFound is a NotFound, which is an Error.
	if got := evalSym(t, v, eval(`[Dictionary new removeKey: #nope. #none] on: NotFound do: [:e | #signaled]`)); got != "signaled" {
		t.Errorf("KeyNotFound should be caught by NotFound, got #%s", got)
	}
	if got := evalSym(t, v, eval(`[Dictionary new removeKey: #nope. #none] on: Error do: [:e | #signaled]`)); got != "signaled" {
		t.Errorf("KeyNotFound should be caught by Error, got #%s", got)
	}
	// Present key still answers the value; a stored nil is answered as nil.
	if r := eval(`| d | d := Dictionary new. d at: #k put: 7. d removeKey: #k`); !r.IsSmallInt() || r.SmallInt() != 7 {
		t.Errorf("removeKey: present: want 7, got %v", r)
	}
	if r := eval(`| d | d := Dictionary new. d at: #k put: nil. d removeKey: #k`); r != vm.Nil {
		t.Errorf("removeKey: of a stored nil: want nil, got %v", r)
	}
	// The tolerant form stays tolerant.
	if got := evalSym(t, v, eval(`Dictionary new removeKey: #nope ifAbsent: [#tolerant]`)); got != "tolerant" {
		t.Errorf("removeKey:ifAbsent: want #tolerant, got #%s", got)
	}
}

func TestSetRemoveSignalsNotFound(t *testing.T) {
	v, eval := newEvalVM(t)
	if got := evalSym(t, v, eval(`[Set new remove: 9. #none] on: NotFound do: [:e | #signaled]`)); got != "signaled" {
		t.Errorf("Set remove: absent: want #signaled, got #%s", got)
	}
	if r := eval(`| s | s := Set new. s add: 3. s remove: 3. s size`); !r.IsSmallInt() || r.SmallInt() != 0 {
		t.Errorf("Set remove: present: size want 0, got %v", r)
	}
	if got := evalSym(t, v, eval(`Set new remove: 9 ifAbsent: [#tolerant]`)); got != "tolerant" {
		t.Errorf("Set remove:ifAbsent: want #tolerant, got #%s", got)
	}
}
