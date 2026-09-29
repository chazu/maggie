package vm

import (
	"os"
	"testing"
)

// newImageVM returns a VM with the default image loaded, so lib-defined
// methods (e.g. Enumerable>>asArray) are available. Skips if the image is
// missing.
func newImageVM(t *testing.T) *VM {
	t.Helper()
	data, err := os.ReadFile("../maggie.image")
	if err != nil {
		t.Skipf("maggie.image not available: %v", err)
	}
	v := NewVM()
	if err := v.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes: %v", err)
	}
	return v
}

func arrayListElements(t *testing.T, v *VM, list Value) []Value {
	t.Helper()
	al := v.getArrayList(list)
	if al == nil {
		t.Fatalf("expected an ArrayList, got %v", list)
	}
	return al.Snapshot()
}

func TestArrayListWithAllArrayList(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()
	cls := v.MustGlobal("ArrayList")

	src := v.Send(cls, "withAll:", []Value{v.NewArrayWithElements([]Value{FromSmallInt(7), FromSmallInt(8)})})
	list := v.Send(cls, "withAll:", []Value{src})
	elems := arrayListElements(t, v, list)
	if len(elems) != 2 || elems[0] != FromSmallInt(7) || elems[1] != FromSmallInt(8) {
		t.Fatalf("ArrayList withAll: anArrayList = %v, want [7 8]", elems)
	}
}

func TestArrayListAddAllRejectsNonCollection(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()
	cls := v.MustGlobal("ArrayList")
	list := v.Send(cls, "new", nil)

	for _, arg := range []Value{FromSmallInt(42), Nil} {
		if _, signaled := signalsPrimitiveError(v, func() {
			v.Send(list, "addAll:", []Value{arg})
		}); !signaled {
			t.Errorf("addAll: %v should signal, not silently no-op", arg)
		}
		if _, signaled := signalsPrimitiveError(v, func() {
			v.Send(cls, "withAll:", []Value{arg})
		}); !signaled {
			t.Errorf("withAll: %v should signal, not answer an empty list", arg)
		}
	}
}

// A Set is a slotted object whose slot is its internal Dictionary; addAll:
// used to copy that dictionary (plus nils) instead of the Set's elements.
func TestArrayListAddAllGenericCollections(t *testing.T) {
	v := newImageVM(t)
	defer v.Shutdown()
	cls := v.MustGlobal("ArrayList")

	set := v.Send(v.MustGlobal("Set"), "new", nil)
	v.Send(set, "add:", []Value{FromSmallInt(3)})
	list := v.Send(cls, "withAll:", []Value{v.NewArrayWithElements([]Value{FromSmallInt(1), FromSmallInt(2)})})
	v.Send(list, "addAll:", []Value{set})
	elems := arrayListElements(t, v, list)
	if len(elems) != 3 || elems[2] != FromSmallInt(3) {
		t.Fatalf("addAll: aSet = %v, want [1 2 3]", elems)
	}

	dict := v.NewDictionary()
	v.DictionaryAtPut(dict, v.registry.NewStringValue("k"), FromSmallInt(9))
	list = v.Send(cls, "withAll:", []Value{dict})
	elems = arrayListElements(t, v, list)
	if len(elems) != 1 || elems[0] != FromSmallInt(9) {
		t.Fatalf("withAll: aDictionary = %v, want [9]", elems)
	}

	list = v.Send(cls, "new", nil)
	v.Send(list, "addAll:", []Value{v.registry.NewStringValue("ab")})
	elems = arrayListElements(t, v, list)
	if len(elems) != 2 || elems[0] != FromCharacter('a') || elems[1] != FromCharacter('b') {
		t.Fatalf("addAll: 'ab' = %v, want [$a $b]", elems)
	}
}
