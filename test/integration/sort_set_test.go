package integration_test

import (
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// Array sort: with comparison block
// ---------------------------------------------------------------------------

func TestArraySortWithBlock(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	eval(t, vmInst, `arr := Array new: 5`)
	eval(t, vmInst, `arr at: 0 put: 3`)
	eval(t, vmInst, `arr at: 1 put: 1`)
	eval(t, vmInst, `arr at: 2 put: 4`)
	eval(t, vmInst, `arr at: 3 put: 1`)
	eval(t, vmInst, `arr at: 4 put: 5`)
	eval(t, vmInst, `arr primSort: [:a :b | a - b]`)

	result := eval(t, vmInst, `arr`)
	if !result.IsObject() {
		t.Fatalf("expected array object, got %v", result)
	}

	obj := vm.ObjectFromValue(result)
	expected := []int64{1, 1, 3, 4, 5}
	for i, exp := range expected {
		val := obj.GetSlot(i)
		if !val.IsSmallInt() || val.SmallInt() != exp {
			t.Errorf("sorted[%d] = %v, want %d", i, val, exp)
		}
	}
}

func TestArraySortWithBlockDescending(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	eval(t, vmInst, `arr := Array new: 4`)
	eval(t, vmInst, `arr at: 0 put: 1`)
	eval(t, vmInst, `arr at: 1 put: 4`)
	eval(t, vmInst, `arr at: 2 put: 2`)
	eval(t, vmInst, `arr at: 3 put: 3`)
	eval(t, vmInst, `arr primSort: [:a :b | b - a]`)

	result := eval(t, vmInst, `arr`)
	obj := vm.ObjectFromValue(result)
	expected := []int64{4, 3, 2, 1}
	for i, exp := range expected {
		val := obj.GetSlot(i)
		if !val.IsSmallInt() || val.SmallInt() != exp {
			t.Errorf("sorted descending[%d] = %v, want %d", i, val, exp)
		}
	}
}

func TestArraySortDefault(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	eval(t, vmInst, `arr := Array new: 5`)
	eval(t, vmInst, `arr at: 0 put: 5`)
	eval(t, vmInst, `arr at: 1 put: 3`)
	eval(t, vmInst, `arr at: 2 put: 1`)
	eval(t, vmInst, `arr at: 3 put: 4`)
	eval(t, vmInst, `arr at: 4 put: 2`)
	eval(t, vmInst, `arr primSortDefault`)

	result := eval(t, vmInst, `arr`)
	obj := vm.ObjectFromValue(result)
	expected := []int64{1, 2, 3, 4, 5}
	for i, exp := range expected {
		val := obj.GetSlot(i)
		if !val.IsSmallInt() || val.SmallInt() != exp {
			t.Errorf("sorted[%d] = %v, want %d", i, val, exp)
		}
	}
}

func TestArraySortedWithBlockReturnsNewArray(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	eval(t, vmInst, `original := Array new: 3`)
	eval(t, vmInst, `original at: 0 put: 3`)
	eval(t, vmInst, `original at: 1 put: 1`)
	eval(t, vmInst, `original at: 2 put: 2`)
	eval(t, vmInst, `sorted := original primSorted: [:a :b | a - b]`)

	// Check sorted result
	sorted := eval(t, vmInst, `sorted`)
	sortedObj := vm.ObjectFromValue(sorted)
	expectedSorted := []int64{1, 2, 3}
	for i, exp := range expectedSorted {
		val := sortedObj.GetSlot(i)
		if !val.IsSmallInt() || val.SmallInt() != exp {
			t.Errorf("sorted[%d] = %v, want %d", i, val, exp)
		}
	}

	// Check original unchanged
	original := eval(t, vmInst, `original`)
	origObj := vm.ObjectFromValue(original)
	expectedOrig := []int64{3, 1, 2}
	for i, exp := range expectedOrig {
		val := origObj.GetSlot(i)
		if !val.IsSmallInt() || val.SmallInt() != exp {
			t.Errorf("original[%d] = %v, want %d (should be unchanged)", i, val, exp)
		}
	}
}

func TestArraySortedDefaultReturnsNewArray(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	eval(t, vmInst, `original := Array new: 3`)
	eval(t, vmInst, `original at: 0 put: 3`)
	eval(t, vmInst, `original at: 1 put: 1`)
	eval(t, vmInst, `original at: 2 put: 2`)
	eval(t, vmInst, `sorted := original primSortedDefault`)

	// Check sorted
	sorted := eval(t, vmInst, `sorted`)
	sortedObj := vm.ObjectFromValue(sorted)
	for i, exp := range []int64{1, 2, 3} {
		val := sortedObj.GetSlot(i)
		if !val.IsSmallInt() || val.SmallInt() != exp {
			t.Errorf("sorted[%d] = %v, want %d", i, val, exp)
		}
	}

	// Check original unchanged
	original := eval(t, vmInst, `original`)
	origObj := vm.ObjectFromValue(original)
	for i, exp := range []int64{3, 1, 2} {
		val := origObj.GetSlot(i)
		if !val.IsSmallInt() || val.SmallInt() != exp {
			t.Errorf("original[%d] = %v, want %d", i, val, exp)
		}
	}
}

// ---------------------------------------------------------------------------
// Set class tests (using VM-level Set primitives)
// ---------------------------------------------------------------------------

func TestSetAddAndIncludes(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	eval(t, vmInst, `s := Set new`)
	eval(t, vmInst, `s add: 1`)
	eval(t, vmInst, `s add: 2`)
	eval(t, vmInst, `s add: 3`)

	result := eval(t, vmInst, `s includes: 2`)
	if result != vm.True {
		t.Errorf("Set should include 2, got %v", result)
	}

	result = eval(t, vmInst, `s includes: 99`)
	if result != vm.False {
		t.Errorf("Set should not include 99, got %v", result)
	}
}

func TestSetSize(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	eval(t, vmInst, `s := Set new`)
	eval(t, vmInst, `s add: 1`)
	eval(t, vmInst, `s add: 2`)
	eval(t, vmInst, `s add: 1`)

	result := eval(t, vmInst, `s size`)
	if !result.IsSmallInt() || result.SmallInt() != 2 {
		t.Errorf("Set size should be 2 (duplicates ignored), got %v", result)
	}
}

func TestSetRemove(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	eval(t, vmInst, `s := Set new`)
	eval(t, vmInst, `s add: 1`)
	eval(t, vmInst, `s add: 2`)
	eval(t, vmInst, `s add: 3`)
	eval(t, vmInst, `s remove: 2`)

	result := eval(t, vmInst, `s size`)
	if !result.IsSmallInt() || result.SmallInt() != 2 {
		t.Errorf("Set size after remove should be 2, got %v", result)
	}

	result = eval(t, vmInst, `s includes: 2`)
	if result != vm.False {
		t.Errorf("Set should not include removed element 2, got %v", result)
	}
}

func TestSetIsEmpty(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	result := eval(t, vmInst, `Set new isEmpty`)
	if result != vm.True {
		t.Errorf("new Set should be empty, got %v", result)
	}

	eval(t, vmInst, `s := Set new`)
	eval(t, vmInst, `s add: 1`)
	result = eval(t, vmInst, `s isEmpty`)
	if result != vm.False {
		t.Errorf("non-empty Set should not be empty, got %v", result)
	}
}

func TestSetDo(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	eval(t, vmInst, `s := Set new`)
	eval(t, vmInst, `s add: 1`)
	eval(t, vmInst, `s add: 2`)
	eval(t, vmInst, `s add: 3`)
	eval(t, vmInst, `sum := 0`)
	eval(t, vmInst, `s do: [:each | sum := sum + each]`)

	result := eval(t, vmInst, `sum`)
	if !result.IsSmallInt() || result.SmallInt() != 6 {
		t.Errorf("sum of Set(1,2,3) via do: should be 6, got %v", result)
	}
}

func TestSetRemoveNonexistent(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	eval(t, vmInst, `s := Set new`)
	eval(t, vmInst, `s add: 1`)
	eval(t, vmInst, `s remove: 99`)

	result := eval(t, vmInst, `s size`)
	if !result.IsSmallInt() || result.SmallInt() != 1 {
		t.Errorf("removing non-existent element should not change size, got %v", result)
	}
}

func setupSetCollectionMethods(t *testing.T, vmInst *vm.VM) {
	t.Helper()
	// Install select: and reject: compiled methods on Set
	compileMethod(t, vmInst, vmInst.SetClass, `select: aBlock | result | result := Set new. self do: [:each | (aBlock value: each) ifTrue: [result add: each]]. ^result`)
	compileMethod(t, vmInst, vmInst.SetClass, `reject: aBlock | result | result := Set new. self do: [:each | (aBlock value: each) ifFalse: [result add: each]]. ^result`)
}

func TestSetSelect(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)
	setupSetCollectionMethods(t, vmInst)

	eval(t, vmInst, `s := Set new`)
	eval(t, vmInst, `s add: 1`)
	eval(t, vmInst, `s add: 2`)
	eval(t, vmInst, `s add: 3`)
	eval(t, vmInst, `s add: 4`)
	eval(t, vmInst, `big := s select: [:x | x > 2]`)

	result := eval(t, vmInst, `big size`)
	if !result.IsSmallInt() || result.SmallInt() != 2 {
		t.Errorf("select >2 from Set(1,2,3,4) should have size 2, got %v", result)
	}

	result = eval(t, vmInst, `big includes: 3`)
	if result != vm.True {
		t.Errorf("big should include 3")
	}

	result = eval(t, vmInst, `big includes: 4`)
	if result != vm.True {
		t.Errorf("big should include 4")
	}
}

func TestSetReject(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)
	setupSetCollectionMethods(t, vmInst)

	eval(t, vmInst, `s := Set new`)
	eval(t, vmInst, `s add: 1`)
	eval(t, vmInst, `s add: 2`)
	eval(t, vmInst, `s add: 3`)
	eval(t, vmInst, `s add: 4`)
	eval(t, vmInst, `small := s reject: [:x | x > 2]`)

	result := eval(t, vmInst, `small size`)
	if !result.IsSmallInt() || result.SmallInt() != 2 {
		t.Errorf("reject >2 from Set(1,2,3,4) should have size 2, got %v", result)
	}

	result = eval(t, vmInst, `small includes: 1`)
	if result != vm.True {
		t.Errorf("small should include 1")
	}

	result = eval(t, vmInst, `small includes: 2`)
	if result != vm.True {
		t.Errorf("small should include 2")
	}
}

func TestSetDuplicateAdd(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	eval(t, vmInst, `s := Set new`)
	eval(t, vmInst, `s add: 42`)
	eval(t, vmInst, `s add: 42`)
	eval(t, vmInst, `s add: 42`)

	result := eval(t, vmInst, `s size`)
	if !result.IsSmallInt() || result.SmallInt() != 1 {
		t.Errorf("adding same element 3 times should give size 1, got %v", result)
	}
}
