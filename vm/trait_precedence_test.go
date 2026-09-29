package vm

import "testing"

// Guide07 "Trait Method Resolution": methods defined on the class win, then
// included traits (the last-included trait winning a conflict), then
// inherited methods.
func TestTraitResolutionOrder(t *testing.T) {
	vm := NewVM()
	sel := vm.Selectors.Intern
	mk := func(name string) *CompiledMethod { return &CompiledMethod{name: name} }

	base := NewClass("TPBase", vm.ObjectClass)
	base.VTable.AddMethod(sel("inherited"), mk("base>>inherited"))
	cls := NewClass("TPSub", base)
	own := mk("sub>>own")
	cls.VTable.AddMethod(sel("own"), own)

	t1 := NewTrait("TP1")
	t1.AddMethod(sel("inherited"), mk("t1>>inherited"))
	t1.AddMethod(sel("own"), mk("t1>>own"))
	t1.AddMethod(sel("conflict"), mk("t1>>conflict"))
	t2 := NewTrait("TP2")
	t2.AddMethod(sel("conflict"), mk("t2>>conflict"))

	for _, tr := range []*Trait{t1, t2} {
		if msg := cls.IncludeTrait(tr, vm.Selectors, vm.Symbols); msg != "" {
			t.Fatal(msg)
		}
	}

	name := func(s string) string {
		if cm, ok := cls.VTable.Lookup(sel(s)).(*CompiledMethod); ok {
			return cm.name
		}
		return "<none>"
	}
	if got := name("own"); got != "sub>>own" {
		t.Errorf("class's own method: got %s, want sub>>own", got)
	}
	if got := name("inherited"); got != "t1>>inherited" {
		t.Errorf("trait vs inherited: got %s, want t1>>inherited", got)
	}
	if got := name("conflict"); got != "t2>>conflict" {
		t.Errorf("two traits: got %s, want the last-included t2>>conflict", got)
	}
}
