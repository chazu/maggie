package server

import (
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)


// SD-9 regression: a plain global assignment must classify as EXCLUSIVE even
// though no schema-mutating selector appears in the text.
func TestDoForSource_GlobalAssignmentIsExclusive(t *testing.T) {
	v := vm.NewVM()
	v.UseGoCompiler(compiler.Compile)
	w := NewVMWorker(v)

	classify := func(source string) bool {
		if vm.SourceMayMutateSchema(source) {
			return true
		}
		m, err := v.CompileExpression(source)
		return err == nil && vm.MethodStoresGlobal(m)
	}

	if !classify("GlobalCounterX := 42") {
		t.Error("plain global assignment should classify as exclusive")
	}
	if !classify("true ifTrue: [GlobalCounterX := 42]") {
		t.Error("global assignment inside a block should classify as exclusive")
	}
	if classify("3 + 4") {
		t.Error("pure expression should classify as shared")
	}
	if classify("| x | x := 42. x + 1") {
		t.Error("local assignment should classify as shared")
	}

	// End-to-end: DoForSource with a writing source completes under Do
	// (indirectly verified — it must not deadlock and must execute).
	res, err := w.DoForSource("GlobalCounterY := 7", func(vp *vm.VM) interface{} {
		return "ran"
	})
	if err != nil || res != "ran" {
		t.Fatalf("DoForSource: res=%v err=%v", res, err)
	}
}
