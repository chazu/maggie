package vm_test

import (
	"os"
	"strings"
	"testing"

	"github.com/chazu/maggie/pipeline"
	"github.com/chazu/maggie/vm"
)

// Cluster>>start must be idempotent: `running` used to be set only inside the
// forked loop, so a second start before the loop ran forked a second event
// loop (two __cluster__ inboxes racing for events).
func TestClusterStartIsIdempotent(t *testing.T) {
	v, eval := newEvalVM(t)
	// Compile the current lib/Cluster.mag over the image so the test exercises
	// the source (the image is only rebuilt separately).
	src, err := os.ReadFile("../lib/Cluster.mag")
	if err != nil {
		t.Fatalf("read Cluster.mag: %v", err)
	}
	// Swap the event loop for an inert one: the real loop polls the `running`
	// ivar that stop writes, an unsynchronized-slot race outside this test's
	// concern that -race would flag. start/stop logic is what is under test.
	source := strings.Replace(string(src), "method: runLoop ", "method: realRunLoop ", 1) +
		"\n  method: runLoop [ Process receive: 60000 ]\n"
	p := &pipeline.Pipeline{VM: v}
	if _, err := p.CompileSourceFile(source, "lib/Cluster.mag", ""); err != nil {
		t.Fatalf("compile Cluster.mag: %v", err)
	}

	r := eval(`| c p1 p2 same | c := Cluster current. p1 := c start. p2 := c start. same := p1 == p2. c stop. same`)
	if r != vm.True {
		t.Fatalf("second start forked another event loop (p1 == p2 answered %v)", r)
	}
	// After stop, start runs a fresh loop.
	r = eval(`| c p1 p2 fresh | c := Cluster current. p1 := c start. c stop. p2 := c start. fresh := p1 ~~ p2. c stop. fresh`)
	if r != vm.True {
		t.Fatalf("start after stop should fork a fresh loop, got %v", r)
	}
}
