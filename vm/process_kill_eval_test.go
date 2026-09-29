package vm_test

import (
	"testing"

	"github.com/chazu/maggie/vm"
)

// terminate (and a linked kill) must actually stop the process: it unwinds,
// running ensure: blocks, whether it is computing or parked in a blocking
// operation. Each case forks `[[<body>] ensure: [ch send: #cleaned]]`,
// terminates it, and expects the ensure block to have run — it can only run
// if the process was really unwound.
func TestTerminateStopsProcess(t *testing.T) {
	cases := map[string]string{
		"inlined busy loop":  `[true] whileTrue: []`,
		"primitive loop":     `| blk | blk := [:i | i]. 1 to: 1000000000000 do: blk`,
		"send loop":          `| c b | c := [true]. b := [nil]. c whileTrue: b`,
		"channel receive":    `(Channel new) receive`,
		"channel send":       `(Channel new) send: 1`,
		"sleep":              `Process sleep: 100000`,
		"wait on a process":  `[Process sleep: 100000] fork wait`,
		"mutex lock":         `| m | m := Mutex new. m lock. [m lock] value`,
		"waitgroup wait":     `| wg | wg := WaitGroup new. wg add: 1. wg wait`,
		"select":             `Channel select: { (Channel new) onReceive: [:v | v] }`,
		"on: Error do: loop": `[[true] whileTrue: []] on: Error do: [:e | ch send: #caught]`,
	}
	for name, body := range cases {
		t.Run(name, func(t *testing.T) {
			_, eval := newEvalVM(t)
			r := eval(`| ch p |
				ch := Channel new: 2.
				p := [[` + body + `] ensure: [ch send: #cleaned]] fork.
				Process sleep: 30.
				p terminate.
				Process sleep: 200.
				{ ch tryReceive. ch tryReceive. p isTerminated }`)
			got := vm.ObjectFromValue(r)
			if got == nil || got.NumSlots() != 3 {
				t.Fatalf("unexpected result %v", r)
			}
			if first := got.GetSlot(0); !first.IsSymbol() {
				t.Fatalf("ensure: block never ran — the process was not stopped (got %v)", first)
			}
			if second := got.GetSlot(1); second != vm.Nil {
				t.Errorf("kill was caught by a handler or ensure ran twice: second message %v", second)
			}
			if got.GetSlot(2) != vm.True {
				t.Error("isTerminated should be true")
			}
		})
	}
}

// A linked process's abnormal exit kills its partner the same way.
func TestLinkedExitStopsPartner(t *testing.T) {
	_, eval := newEvalVM(t)
	r := eval(`| ch p q |
		ch := Channel new: 1.
		p := [[[true] whileTrue: []] ensure: [ch send: #cleaned]] fork.
		q := [Process sleep: 30. nil foo] fork.
		p link: q.
		Process sleep: 250.
		ch tryReceive`)
	if !r.IsSymbol() {
		t.Fatalf("linked partner kept running: ensure: never ran (got %v)", r)
	}
}
