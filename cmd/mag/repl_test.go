package main

import (
	"strings"
	"testing"
)

// REPL input is a statement sequence whose value is the LAST statement; every
// statement must run (a textual ^ prefix used to return after the first).
func TestEvalAndPrintRunsAllStatements(t *testing.T) {
	vmInst := newTestVM(t)
	for input, want := range map[string]string{
		"3. 4.":                "4",
		"| x | x := 5. x * 2.": "10",
		"'a.b' size":           "3",
		"#(1 2 3) inject: 0 into: [:a :b | a + b]": "6",
	} {
		got := strings.TrimSpace(captureStdout(t, func() { evalAndPrint(vmInst, input) }))
		if got != want {
			t.Errorf("evalAndPrint(%q) printed %q, want %q", input, got, want)
		}
	}
}
