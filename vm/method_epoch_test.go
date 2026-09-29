package vm

import "testing"

// A method change on a superclass must reach subclasses: every snapshot
// flattens the whole inheritance chain, so invalidating only the mutated
// class's snapshot leaves subclasses dispatching to the old method.
func TestVTableSubclassSeesSuperclassMethodChanges(t *testing.T) {
	base := NewVTable(&Class{Name: "Base"}, nil)
	sub := NewVTable(&Class{Name: "Sub"}, base)

	const sel = 7
	old := &CompiledMethod{name: "old"}
	base.AddMethod(sel, old)
	if got := sub.Lookup(sel); got != old {
		t.Fatalf("initial lookup = %v, want old", got)
	}

	redefined := &CompiledMethod{name: "new"}
	base.AddMethod(sel, redefined)
	if got := sub.Lookup(sel); got != redefined {
		t.Errorf("after redefinition on superclass, subclass lookup = %v, want redefined", got)
	}

	const added = 9
	extra := &CompiledMethod{name: "extra"}
	base.AddMethod(added, extra)
	if got := sub.Lookup(added); got != extra {
		t.Errorf("method added to superclass not visible in subclass: got %v", got)
	}

	base.RemoveMethod(sel)
	if got := sub.Lookup(sel); got != nil {
		t.Errorf("method removed from superclass still dispatches in subclass: got %v", got)
	}

	other := NewVTable(&Class{Name: "Other"}, nil)
	otherM := &CompiledMethod{name: "other"}
	other.AddMethod(sel, otherM)
	sub.Lookup(added) // warm the snapshot
	base.SetParent(other)
	if got := sub.Lookup(sel); got != otherM {
		t.Errorf("reparenting a superclass not visible in subclass: got %v", got)
	}
}

// An inline cache entry recorded before a method change must not keep
// answering the old method afterwards.
func TestInlineCacheInvalidatedByMethodChange(t *testing.T) {
	cls := &Class{Name: "C"}
	vt := NewVTable(cls, nil)
	old := &CompiledMethod{name: "old"}
	vt.AddMethod(3, old)

	ic := newTestIC()
	ic.Update(cls, old)
	if ic.Lookup(cls) != old {
		t.Fatal("expected cache hit before redefinition")
	}

	vt.AddMethod(3, &CompiledMethod{name: "new"})
	if got := ic.Lookup(cls); got != nil {
		t.Errorf("inline cache answered %v after redefinition, want a miss", got)
	}
}
