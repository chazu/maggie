package vm_test

import (
	"testing"

	"github.com/chazu/maggie/vm"
)

// Non-inlined conditionals (the argument is not a literal block) used to run
// their argument through evaluateBlock, which answers nil for anything that
// is not a block: `true ifTrue: x` silently lost x's value. They now evaluate
// the argument as a valuable — blocks run, anything else is sent #value.
func TestConditionalsEvaluateNonLiteralArguments(t *testing.T) {
	v, eval := newEvalVM(t)
	cases := map[string]int64{
		`| b | b := [7]. true ifTrue: b`:                    7,
		`| b | b := [7]. false ifFalse: b`:                  7,
		`| b | b := [7]. true and: b`:                       7,
		`| b | b := [7]. false or: b`:                       7,
		`| b | b := [7]. nil ifNil: b`:                      7,
		`| b | b := [:x | x + 1]. 6 ifNotNil: b`:            7,
		`| b | b := [:x | x + 1]. 6 ifNil: [0] ifNotNil: b`: 7,
	}
	for src, want := range cases {
		if r := eval(src); !r.IsSmallInt() || r.SmallInt() != want {
			t.Errorf("%s: want %d, got %v", src, want, r)
		}
	}
	// A non-valuable argument is a programmer error, not a silent nil.
	for _, src := range []string{
		`| b | b := 5. [true ifTrue: b. #none] on: MessageNotUnderstood do: [:e | #signaled]`,
		`| b | b := 5. [nil ifNil: b. #none] on: MessageNotUnderstood do: [:e | #signaled]`,
	} {
		if got := evalSym(t, v, eval(src)); got != "signaled" {
			t.Errorf("%s: want #signaled, got #%s", src, got)
		}
	}
}

// xor:/eqv: with a non-Boolean argument answered true/false as if it were a
// Boolean (`true xor: nil` was true). A type error signals.
func TestBooleanXorEqvRejectNonBooleans(t *testing.T) {
	v, eval := newEvalVM(t)
	for _, src := range []string{
		`[true xor: nil. #none] on: TypeError do: [:e | #signaled]`,
		`[false xor: 3. #none] on: TypeError do: [:e | #signaled]`,
		`[true eqv: 'x'. #none] on: TypeError do: [:e | #signaled]`,
		`[false eqv: nil. #none] on: TypeError do: [:e | #signaled]`,
	} {
		if got := evalSym(t, v, eval(src)); got != "signaled" {
			t.Errorf("%s: want #signaled, got #%s", src, got)
		}
	}
	truth := map[string]vm.Value{
		`true xor: true`: vm.False, `true xor: false`: vm.True,
		`false xor: true`: vm.True, `false xor: false`: vm.False,
		`true eqv: true`: vm.True, `false eqv: false`: vm.True,
		`true eqv: false`: vm.False, `false eqv: true`: vm.False,
	}
	for src, want := range truth {
		if r := eval(src); r != want {
			t.Errorf("%s: want %v, got %v", src, want, r)
		}
	}
}
