package vm_test

import "testing"

// Failure doctrine (docs/CONVENTIONS.md §1): a non-String argument or a
// malformed literal pattern is a programmer error and signals. Before, the
// regex primitives matched non-Strings as the empty string and answered
// false / the unsplit receiver for bad patterns — indistinguishable from a
// genuine "no match".
func TestRegexBadArgumentsSignal(t *testing.T) {
	v, eval := newEvalVM(t)
	cases := map[string]string{
		"compile: non-String":       `[Regex compile: 42. #none] on: TypeError do: [:e | #signaled]`,
		"matches: non-String":       `[(Regex compile: 'a') value matches: 42. #none] on: TypeError do: [:e | #signaled]`,
		"findIn: non-String":        `[(Regex compile: 'a') value findIn: nil. #none] on: TypeError do: [:e | #signaled]`,
		"findAllIn: non-String":     `[(Regex compile: 'a') value findAllIn: 3. #none] on: TypeError do: [:e | #signaled]`,
		"split: non-String":         `[(Regex compile: 'a') value split: 3. #none] on: TypeError do: [:e | #signaled]`,
		"replaceIn:with: bad arg 2": `[(Regex compile: 'a') value replaceIn: 'abc' with: 7. #none] on: TypeError do: [:e | #signaled]`,
		"matchesRegex: non-String":  `['abc' matchesRegex: 42. #none] on: TypeError do: [:e | #signaled]`,
		"splitRegex: non-String":    `['abc' splitRegex: 42. #none] on: TypeError do: [:e | #signaled]`,
		"matchesRegex: bad pattern": `['abc' matchesRegex: '['. #none] on: Error do: [:e | #signaled]`,
		"splitRegex: bad pattern":   `['abc' splitRegex: '('. #none] on: Error do: [:e | #signaled]`,
	}
	for name, src := range cases {
		if got := evalSym(t, v, eval(src)); got != "signaled" {
			t.Errorf("%s: want #signaled, got #%s", name, got)
		}
	}
	// Symbols are still accepted as text, and valid calls are unaffected.
	if got := evalSym(t, v, eval(`('abc' matchesRegex: #b) ifTrue: [#yes] ifFalse: [#no]`)); got != "yes" {
		t.Errorf("Symbol pattern: want #yes, got #%s", got)
	}
}
