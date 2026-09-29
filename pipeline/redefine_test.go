package pipeline

import (
	"strings"
	"testing"

	"github.com/chazu/maggie/vm"
)

func compileDir(t *testing.T, p *Pipeline, files map[string]string) error {
	t.Helper()
	dir := t.TempDir()
	for name, src := range files {
		writeMagFile(t, dir, name, src)
	}
	_, err := p.CompilePath(dir)
	return err
}

// A method-only extension processed before the class's definition in the
// same load must not freeze the class with no instance variables.
func TestRedefine_ExtensionBeforeDefinitionInOneLoad(t *testing.T) {
	vmInst := newTestVM(t)
	err := compileDir(t, newPipeline(vmInst), map[string]string{
		"A_Ext.mag": "Rd1 subclass: Object\n  method: twice [ ^a * 2 ]\n",
		"B_Def.mag": "Rd1 subclass: Object\n  instanceVars: a\n  method: a: x [ a := x ]\n",
	})
	if err != nil {
		t.Fatalf("CompilePath: %v", err)
	}
	cls := vmInst.LookupClass("Rd1")
	if len(cls.InstVars) != 1 || cls.NumSlots != 1 {
		t.Fatalf("Rd1 ivars = %v, NumSlots = %d; want [a], 1", cls.InstVars, cls.NumSlots)
	}
	obj := vmInst.Send(vmInst.ClassValue(cls), "new", nil)
	vmInst.Send(obj, "a:", []vm.Value{vm.FromSmallInt(5)})
	wantInt(t, "twice", vmInst.Send(obj, "twice", nil), 10)
}

// A later load cannot change an existing class's instance variables: the new
// names would silently compile as globals.
func TestRedefine_ChangedIvarsInLaterLoadIsAnError(t *testing.T) {
	vmInst := newTestVM(t)
	if err := compileDir(t, newPipeline(vmInst), map[string]string{
		"Def.mag": "Rd2 subclass: Object\n  instanceVars: a\n",
	}); err != nil {
		t.Fatalf("first load: %v", err)
	}
	// Same ivars, or none (method-only), is fine.
	if err := compileDir(t, newPipeline(vmInst), map[string]string{
		"Same.mag": "Rd2 subclass: Object\n  instanceVars: a\n  method: a [ ^a ]\n",
		"Ext.mag":  "Rd2 subclass: Object\n  method: b [ ^1 ]\n",
	}); err != nil {
		t.Fatalf("compatible reload: %v", err)
	}
	err := compileDir(t, newPipeline(vmInst), map[string]string{
		"Def.mag": "Rd2 subclass: Object\n  instanceVars: a b\n  method: b: x [ b := x ]\n",
	})
	if err == nil || !strings.Contains(err.Error(), "instance variables") {
		t.Fatalf("want an instance-variable redefinition error, got %v", err)
	}
}

// Re-parenting an existing class under a superclass with instance variables
// would shift its slots; refuse it.
func TestRedefine_ReparentChangingLayoutIsAnError(t *testing.T) {
	vmInst := newTestVM(t)
	if err := compileDir(t, newPipeline(vmInst), map[string]string{
		"Foo.mag": "Rd3 subclass: Object\n  instanceVars: a\n",
		"Bar.mag": "Rd3Base subclass: Object\n  instanceVars: p q\n",
	}); err != nil {
		t.Fatalf("first load: %v", err)
	}
	err := compileDir(t, newPipeline(vmInst), map[string]string{
		"Foo.mag": "Rd3 subclass: Rd3Base\n  instanceVars: a\n",
	})
	if err == nil || !strings.Contains(err.Error(), "superclass") {
		t.Fatalf("want a superclass layout error, got %v", err)
	}
	if got := vmInst.LookupClass("Rd3").Superclass.Name; got != "Object" {
		t.Errorf("Rd3 re-parented to %s despite the error", got)
	}
}
