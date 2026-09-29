package vm_test

import (
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// definePt installs `Pt` (ivar x) whose = and hash are by x, as a user class
// would define them.
func definePt(t *testing.T, v *vm.VM) {
	t.Helper()
	cls := vm.NewClassWithInstVars("Pt", v.ObjectClass, []string{"x"})
	v.Classes.Register(cls)
	v.SetGlobal("Pt", v.ClassValue(cls))
	for _, src := range []string{
		"x ^x",
		"x: a x := a",
		// Reads a dictionary while comparing: must not run under its lock.
		"= other Probe notNil ifTrue: [Probe at: #k ifAbsent: [nil]]. ^x = other x",
		"hash ^x hash",
	} {
		m, err := compiler.Compile(src, v.Selectors, v.Symbols, v.Registry(), []string{"x"})
		if err != nil {
			t.Fatalf("compile %q: %v", src, err)
		}
		m.SetClass(cls)
		cls.VTable.AddMethod(v.Selectors.Intern(m.Name()), m)
	}
}

func TestDictionaryHonorsUserEqualityAndHash(t *testing.T) {
	v, eval := newEvalVM(t)
	definePt(t, v)
	r := eval(`| d a b s |
		a := Pt new x: 3. b := Pt new x: 3.
		d := Dictionary new.
		Probe := d.
		d at: a put: 1.
		d at: b put: 2.
		s := Set new. s add: a; add: b.
		{ d at: b. d size. d includesKey: (Pt new x: 3). d removeKey: b. d size. s size. s includes: (Pt new x: 3) }`)
	got := vm.ObjectFromValue(r)
	if got == nil || got.NumSlots() != 7 {
		t.Fatalf("unexpected result %v", r)
	}
	want := []vm.Value{vm.FromSmallInt(2), vm.FromSmallInt(1), vm.True, vm.FromSmallInt(2), vm.FromSmallInt(0), vm.FromSmallInt(1), vm.True}
	for i, w := range want {
		if g := got.GetSlot(i); g != w {
			t.Errorf("element %d = %v, want %v", i+1, g, w)
		}
	}
}

func TestDictionaryNumericKeysFollowEquality(t *testing.T) {
	_, eval := newEvalVM(t)
	r := eval(`| d | d := Dictionary new.
		d at: 0.0 put: #zero.
		d at: 1 put: #int. d at: 1.0 put: #float.
		{ d at: -0.0. d size. d at: 1. d at: 1.0 }`)
	got := vm.ObjectFromValue(r)
	if got == nil || got.NumSlots() != 4 {
		t.Fatalf("unexpected result %v", r)
	}
	if s := got.GetSlot(0); !s.IsSymbol() {
		t.Errorf("-0.0 should find the 0.0 key (0.0 = -0.0), got %v", s)
	}
	if n := got.GetSlot(1); !n.IsSmallInt() || n.SmallInt() != 3 {
		t.Errorf("size = %v, want 3 (1 and 1.0 are not =, so stay distinct)", n)
	}
	if got.GetSlot(2) == got.GetSlot(3) {
		t.Error("1 and 1.0 collapsed into one key, but 1 = 1.0 is false")
	}
}

func TestDictionaryPlainObjectsStayIdentityKeyed(t *testing.T) {
	_, eval := newEvalVM(t)
	r := eval(`| d | d := Dictionary new. d at: Object new put: 1. d at: Object new put: 2. d size`)
	if !r.IsSmallInt() || r.SmallInt() != 2 {
		t.Fatalf("two distinct plain objects should be two keys, size = %v", r)
	}
}
