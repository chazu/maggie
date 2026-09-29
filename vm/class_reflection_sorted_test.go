package vm

import "testing"

// Same-named classes in different namespaces must each appear once — the
// sort used to key a map by short name, duplicating one and dropping the other.
func TestAllClassesSortedKeepsSameNamedClasses(t *testing.T) {
	vm := NewVM()
	a := NewClassInNamespace("Alpha", "Widget", vm.ObjectClass)
	b := NewClassInNamespace("Beta", "Widget", vm.ObjectClass)
	vm.Classes.Register(a)
	vm.Classes.Register(b)

	result := vm.Send(vm.classValue(vm.ObjectClass), "allClassesSorted", nil)
	arr := ObjectFromValue(result)
	if arr == nil {
		t.Fatal("allClassesSorted did not return an Array")
	}
	seen := map[*Class]int{}
	var prev string
	for i := 0; i < arr.NumSlots(); i++ {
		cls := vm.classFromValue(arr.GetSlot(i))
		if cls == nil {
			t.Fatalf("element %d is not a class", i)
		}
		if cls.Name < prev {
			t.Fatalf("not sorted: %q after %q", cls.Name, prev)
		}
		prev = cls.Name
		seen[cls]++
	}
	if seen[a] != 1 || seen[b] != 1 {
		t.Fatalf("Alpha::Widget seen %d times, Beta::Widget seen %d times; want 1 each", seen[a], seen[b])
	}
}
