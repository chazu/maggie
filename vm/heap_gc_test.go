package vm

import (
	"fmt"
	"testing"
	"unsafe"
)

// ---------------------------------------------------------------------------
// String/dictionary tracing-GC tests.
//
// Each "Keeps…" test makes a string reachable through EXACTLY ONE root
// category, runs a collection, and asserts the string survives with its
// content intact. If the mark phase misses that root, the string is swept and
// the test fails — this is the safety net for complete root coverage.
//
// All tests are single-threaded: the test goroutine is the only mutator, so
// calling collectHeapGarbageLocked directly satisfies the stop-the-world
// precondition.
// ---------------------------------------------------------------------------

const gcMarker = "\x00gc-probe\x00" // unlikely to collide with bootstrap strings

func probe(i int) string { return fmt.Sprintf("%s-%d", gcMarker, i) }

// stringStillThere asserts that v resolves to want after a collection.
func stringStillThere(t *testing.T, vm *VM, v Value, want string) {
	t.Helper()
	got := vm.registry.GetStringContent(v)
	if got != want {
		t.Fatalf("string freed or corrupted by GC: got %q want %q", got, want)
	}
}

func TestHeapGC_FreesUnreachableStrings(t *testing.T) {
	vm := NewVM()
	base := vm.registry.StringCount()
	const n = 5000
	for i := 0; i < n; i++ {
		_ = vm.registry.NewStringValue(probe(i)) // discarded, unreachable
	}
	if got := vm.registry.StringCount(); got < base+n {
		t.Fatalf("expected at least %d strings before GC, got %d", base+n, got)
	}
	freed, _ := vm.collectHeapGarbageLocked()
	if freed < n {
		t.Fatalf("expected to free >= %d unreachable strings, freed %d", n, freed)
	}
	after := vm.registry.StringCount()
	if after > base+10 {
		t.Fatalf("unreachable strings not reclaimed: base=%d after=%d", base, after)
	}
}

func TestHeapGC_KeepsGlobalString(t *testing.T) {
	vm := NewVM()
	s := vm.registry.NewStringValue(probe(1))
	vm.SetGlobal("GcProbeGlobal", s)
	vm.collectHeapGarbageLocked()
	stringStillThere(t, vm, s, probe(1))
}

func TestHeapGC_KeepsObjectIvarString(t *testing.T) {
	vm := NewVM()
	s := vm.registry.NewStringValue(probe(2))
	obj := NewObject(vm.ObjectClass.VTable, 2)
	obj.SetSlot(0, s)
	vm.KeepAlive(obj)
	vm.SetGlobal("GcProbeObj", FromObjectPtr(unsafe.Pointer(obj)))
	vm.collectHeapGarbageLocked()
	stringStillThere(t, vm, s, probe(2))
}

func TestHeapGC_KeepsNestedObjectString(t *testing.T) {
	vm := NewVM()
	s := vm.registry.NewStringValue(probe(3))
	inner := NewObject(vm.ObjectClass.VTable, 1)
	inner.SetSlot(0, s)
	outer := NewObject(vm.ObjectClass.VTable, 1)
	outer.SetSlot(0, FromObjectPtr(unsafe.Pointer(inner)))
	vm.KeepAlive(inner)
	vm.KeepAlive(outer)
	vm.SetGlobal("GcProbeNested", FromObjectPtr(unsafe.Pointer(outer)))
	vm.collectHeapGarbageLocked()
	stringStillThere(t, vm, s, probe(3))
}

func TestHeapGC_KeepsDictString(t *testing.T) {
	vm := NewVM()
	dv := vm.registry.NewDictionaryValue()
	d := vm.registry.GetDictionaryObject(dv)
	keyStr := vm.registry.NewStringValue(probe(40))
	valStr := vm.registry.NewStringValue(probe(41))
	d.Keys[1] = keyStr
	d.Data[1] = valStr
	vm.SetGlobal("GcProbeDict", dv) // dict reachable via global
	vm.collectHeapGarbageLocked()
	stringStillThere(t, vm, keyStr, probe(40))
	stringStillThere(t, vm, valStr, probe(41))
}

func TestHeapGC_KeepsCellString(t *testing.T) {
	vm := NewVM()
	s := vm.registry.NewStringValue(probe(5))
	cellVal := NewCell(vm.registry, s)
	// Reachable via an object slot holding the cell.
	obj := NewObject(vm.ObjectClass.VTable, 1)
	obj.SetSlot(0, cellVal)
	vm.KeepAlive(obj)
	vm.SetGlobal("GcProbeCell", FromObjectPtr(unsafe.Pointer(obj)))
	vm.collectHeapGarbageLocked()
	stringStillThere(t, vm, s, probe(5))
}

func TestHeapGC_KeepsMethodLiteralString(t *testing.T) {
	vm := NewVM()
	// A string literal embedded in a block's literal pool must survive even
	// though no Value on any stack references it at GC time. The block is
	// reachable as a registered block; its Literals are a root.
	s := vm.registry.NewStringValue(probe(6))
	block := &BlockMethod{
		Arity:    0,
		NumTemps: 0,
		Literals: []Value{s},
	}
	interp := vm.interpreter
	_ = interp.createBlockValue(block, nil) // registers the block; its literals are a root
	vm.collectHeapGarbageLocked()
	stringStillThere(t, vm, s, probe(6))
}

func TestHeapGC_FreesOrphanDictAndString(t *testing.T) {
	// The SSE-leak scenario: a dictionary that holds a big string is built,
	// used transiently, and dropped (never stored in any root). Both the dict
	// and its string must be reclaimed.
	vm := NewVM()
	baseStr := vm.registry.StringCount()
	baseDict := vm.registry.DictionaryCount()
	const n = 2000
	for i := 0; i < n; i++ {
		dv := vm.registry.NewDictionaryValue()
		d := vm.registry.GetDictionaryObject(dv)
		d.Data[1] = vm.registry.NewStringValue(probe(1000 + i))
		// dv discarded — unreachable
	}
	freedStr, freedDict := vm.collectHeapGarbageLocked()
	if freedDict < n {
		t.Fatalf("orphan dicts not reclaimed: freed %d want >= %d", freedDict, n)
	}
	if freedStr < n {
		t.Fatalf("orphan dict strings not reclaimed: freed %d want >= %d", freedStr, n)
	}
	if vm.registry.DictionaryCount() > baseDict+10 {
		t.Fatalf("dicts leaked: base=%d after=%d", baseDict, vm.registry.DictionaryCount())
	}
	if vm.registry.StringCount() > baseStr+10 {
		t.Fatalf("strings leaked: base=%d after=%d", baseStr, vm.registry.StringCount())
	}
}

func TestHeapGC_NoCorruptionMixed(t *testing.T) {
	vm := NewVM()
	// Keep 100 strings reachable via globals; create 5000 ephemerals between
	// them. After GC every kept string must be byte-for-byte intact.
	kept := make([]Value, 100)
	for i := range kept {
		kept[i] = vm.registry.NewStringValue(probe(i))
		vm.SetGlobal(fmt.Sprintf("GcKeep%d", i), kept[i])
		for j := 0; j < 50; j++ {
			_ = vm.registry.NewStringValue(fmt.Sprintf("ephemeral-%d-%d", i, j))
		}
	}
	vm.collectHeapGarbageLocked()
	for i := range kept {
		stringStillThere(t, vm, kept[i], probe(i))
	}
}

func TestHeapGC_Idempotent(t *testing.T) {
	vm := NewVM()
	s := vm.registry.NewStringValue(probe(7))
	vm.SetGlobal("GcIdem", s)
	for i := 0; i < 5; i++ {
		vm.collectHeapGarbageLocked()
		stringStillThere(t, vm, s, probe(7))
	}
}
