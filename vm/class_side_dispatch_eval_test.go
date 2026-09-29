package vm_test

import (
	"testing"

	"github.com/chazu/maggie/vm"
)

// TestClassSideInheritsObjectProtocol guards the Object class → Class link:
// Object's ClassVTable had no parent, so a class-side send missing every
// ClassVTable (==, hash, isNil, error:, even doesNotUnderstand:) silently
// answered nil instead of reaching Object's instance protocol.
func TestClassSideInheritsObjectProtocol(t *testing.T) {
	_, eval := newEvalVM(t)
	cases := []struct {
		src  string
		want vm.Value
	}{
		{`Object == Object`, vm.True},
		{`Array == Object`, vm.False},
		{`Array hash = Array hash`, vm.True},
		{`Object isNil`, vm.False},
		{`Array notNil`, vm.True},
		{`Array yourself == Array`, vm.True},
		{`[Object fooBarBaz. false] on: MessageNotUnderstood do: [:e | true]`, vm.True},
		{`[Array error: 'boom'. false] on: Error do: [:e | true]`, vm.True},
		// Class-side overrides still win over the inherited instance side.
		{`Object new isNil`, vm.False},
		{`Array printString = 'Array'`, vm.True},
	}
	for _, c := range cases {
		if got := eval(c.src); got != c.want {
			t.Errorf("%s: got %v, want %v", c.src, got, c.want)
		}
	}
}
