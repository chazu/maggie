package vm_test

import (
	"testing"

	"github.com/chazu/maggie/vm"
)

// TestContextTempAtIsOneBased guards Context>>tempAt:/tempAt:put:, which
// indexed 0-based (unlike every other Maggie subscript) and answered nil for
// a bad index instead of signalling.
func TestContextTempAtIsOneBased(t *testing.T) {
	_, eval := newEvalVM(t)
	cases := []struct {
		src  string
		want vm.Value
	}{
		{`| a b c | a := 5. b := 6. c := thisContext. (c tempAt: 1) = 5 and: [(c tempAt: 2) = 6]`, vm.True},
		{`| a c | a := 5. c := thisContext. c tempAt: 1 put: 8. (c tempAt: 1) = 8`, vm.True},
		{`| a c | c := thisContext. [c tempAt: 0. false] on: SubscriptOutOfBounds do: [:e | true]`, vm.True},
		{`| a c | c := thisContext. [c tempAt: c numTemps + 1. false] on: SubscriptOutOfBounds do: [:e | true]`, vm.True},
		{`| a c | c := thisContext. [c tempAt: #x put: 1. false] on: TypeError do: [:e | true]`, vm.True},
	}
	for _, c := range cases {
		if got := eval(c.src); got != c.want {
			t.Errorf("%s: got %v, want %v", c.src, got, c.want)
		}
	}
}
