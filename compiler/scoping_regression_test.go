package compiler

import (
	"bytes"
	"os"
	"testing"

	"github.com/chazu/maggie/vm"
)

// runOnSmallInt compiles source as a SmallInteger method, sends it to 0 and
// returns the result. instVars are the compile-time instance variable names.
func runOnSmallInt(t *testing.T, vmInst *vm.VM, source string, instVars []string) vm.Value {
	t.Helper()
	method, err := Compile(source, vmInst.Selectors, vmInst.Symbols, vmInst.Registry(), instVars)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	vmInst.SmallIntegerClass.VTable.AddMethod(vmInst.Selectors.Intern(method.Name()), method)
	return vmInst.Send(vm.FromSmallInt(0), method.Name(), nil)
}

// Cell analysis tracked variables by bare name: a later block reusing a name
// overwrote the earlier variable's capture/assign record, so a mutation from
// an inner block was lost.
func TestCellVarSurvivesSiblingBlockReusingName(t *testing.T) {
	cases := []struct {
		name, src string
		want      int64
	}{
		{"param reused", `siblingParam
	| r |
	r := [:x | [x := 1] value. x] value: 0.
	[:x | x] value: 2.
	^r`, 1},
		{"method temp shadowed later", `shadowLater
	| x |
	[:x | x] value: 1.
	[x := 5] value.
	^x`, 5},
		{"block temp reused", `tempReused
	| r |
	r := [ | t | [t := 1] value. t] value.
	[ | t | t] value.
	^r`, 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			wantSmallInt(t, runOnSmallInt(t, vm.NewVM(), tc.src, nil), tc.want)
		})
	}
}

// A block parameter shadowing a method temp must be captured from the
// enclosing block's own slot, not the method's slot of the same name.
func TestNestedCaptureOfShadowingParam(t *testing.T) {
	src := `shadowCapture
	| a x |
	a := 1.
	x := 10.
	^[:x | | y | y := 7. [x] value] value: 5`
	wantSmallInt(t, runOnSmallInt(t, vm.NewVM(), src, nil), 5)
}

// Inside a block a method temp must win over an instance variable of the same
// name, exactly as it does at method level.
func TestTempShadowsIvarInsideBlock(t *testing.T) {
	src := `tempVsIvar
	| x |
	x := 5.
	^[x] value`
	method, err := Compile(src, vm.NewVM().Selectors, vm.NewVM().Symbols, vm.NewVM().Registry(), []string{"x"})
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	if len(method.Blocks) != 1 {
		t.Fatalf("want 1 block, got %d", len(method.Blocks))
	}
	if bytes.Contains(method.Blocks[0].Bytecode, []byte{byte(vm.OpPushIvar)}) {
		t.Errorf("block reads the instance variable instead of the captured temp: % x", method.Blocks[0].Bytecode)
	}
	src2 := `tempVsIvarAssign
	| x |
	[x := 5] value.
	^x`
	method, err = Compile(src2, vm.NewVM().Selectors, vm.NewVM().Symbols, vm.NewVM().Registry(), []string{"x"})
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	if bytes.Contains(method.Blocks[0].Bytecode, []byte{byte(vm.OpStoreIvar)}) {
		t.Errorf("block assigns the instance variable instead of the captured temp: % x", method.Blocks[0].Bytecode)
	}
}

// Block cell-variable prologues must be emitted in a stable order.
func TestBlockBytecodeDeterministic(t *testing.T) {
	src := `detBlock
	^[ | a b c d | [a := 1. b := 2. c := 3. d := 4] value. a + b + c + d] value`
	v := vm.NewVM()
	var first [][]byte
	for i := 0; i < 30; i++ {
		method, err := Compile(src, v.Selectors, v.Symbols, v.Registry(), nil)
		if err != nil {
			t.Fatalf("compile error: %v", err)
		}
		if first == nil {
			for _, b := range method.Blocks {
				first = append(first, b.Bytecode)
			}
			continue
		}
		for j, b := range method.Blocks {
			if !bytes.Equal(first[j], b.Bytecode) {
				t.Fatalf("block %d bytecode differs between compiles:\n% x\n% x", j, first[j], b.Bytecode)
			}
		}
	}
}

// doItValue compiles source as eval input and runs it.
func doItValue(t *testing.T, source string) vm.Value {
	t.Helper()
	v := vm.NewVM()
	method, _, err := CompileDoIt(source, v.Selectors, v.Symbols, v.Registry())
	if err != nil {
		t.Fatalf("CompileDoIt(%q): %v", source, err)
	}
	return v.Execute(method, vm.Nil, nil)
}

// `x-1` lexes the `-1` as a negative literal; where a binary operator is
// expected it must be read as a minus send, not silently dropped.
func TestMinusWithoutSpaces(t *testing.T) {
	wantSmallInt(t, doItValue(t, "3-1"), 2)
	wantSmallInt(t, doItValue(t, "| x | x := 5. x-1"), 4)
	wantSmallInt(t, doItValue(t, "3 -1"), 2)
	wantSmallInt(t, doItValue(t, "3 - -1"), 4)
	wantSmallInt(t, doItValue(t, "(Array new: 10-1) size"), 9)
	wantSmallInt(t, doItValue(t, "#(1 -2) at: 2"), -2)
	wantSmallInt(t, doItValue(t, "| x | x := -1. x"), -1)
	if got := doItValue(t, "2.5-0.5"); !got.IsFloat() || got.Float64() != 2.0 {
		t.Errorf("2.5-0.5 = %v, want 2.0", got)
	}
}

// Input the parser cannot consume must be an error, not silently ignored.
func TestTrailingTokensRejected(t *testing.T) {
	v := vm.NewVM()
	for _, src := range []string{"3 4", "1 + 2 3 + 4", "3 ]"} {
		if _, _, err := CompileDoIt(src, v.Selectors, v.Symbols, v.Registry()); err == nil {
			t.Errorf("CompileDoIt(%q) succeeded; want a parse error", src)
		}
	}
	if _, err := Compile("foo ^3 4", v.Selectors, v.Symbols, v.Registry(), nil); err == nil {
		t.Error("Compile(\"foo ^3 4\") succeeded; want a parse error")
	}
}

func TestNegativeAndWideRadixLiterals(t *testing.T) {
	wantSmallInt(t, doItValue(t, "-16rFF"), -255)
	wantSmallInt(t, doItValue(t, "36rZZ"), 1295)
	wantSmallInt(t, doItValue(t, "#(-16rFF) at: 1"), -255)
}

// Trait methods are compiled without a host, so a host instance variable
// compiles as a global; including the trait must bind it to the host's slot
// (in method bodies and in blocks, for reads and writes).
func TestTraitMethodsSeeHostInstanceVariables(t *testing.T) {
	v := vm.NewVM()
	trait := vm.NewTrait("HasA")
	for _, src := range []string{
		"peekA ^a",
		"setA: x a := x",
		"peekAInBlock ^[a] value",
		"bumpAInBlock [a := a + 1] value. ^a",
	} {
		m, err := Compile(src, v.Selectors, v.Symbols, v.Registry(), nil)
		if err != nil {
			t.Fatalf("compile %q: %v", src, err)
		}
		trait.AddMethod(v.Selectors.Intern(m.Name()), m)
	}
	// Two hosts with `a` in different slots share the one compiled trait.
	for _, ivars := range [][]string{{"a"}, {"z", "y", "a"}} {
		cls := vm.NewClassWithInstVars("Host", v.ObjectClass, ivars)
		if msg := cls.IncludeTrait(trait, v.Selectors, v.Symbols); msg != "" {
			t.Fatal(msg)
		}
		// Two instances: a global `a` would be shared between them.
		obj := v.Send(v.ClassValue(cls), "new", nil)
		other := v.Send(v.ClassValue(cls), "new", nil)
		v.Send(obj, "setA:", []vm.Value{vm.FromSmallInt(41)})
		v.Send(other, "setA:", []vm.Value{vm.FromSmallInt(7)})
		wantSmallInt(t, v.Send(obj, "peekA", nil), 41)
		wantSmallInt(t, v.Send(obj, "peekAInBlock", nil), 41)
		wantSmallInt(t, v.Send(obj, "bumpAInBlock", nil), 42)
		wantSmallInt(t, v.Send(other, "peekA", nil), 7)
		if got := vm.ObjectFromValue(obj).GetSlot(len(ivars) - 1); !got.IsSmallInt() || got.SmallInt() != 42 {
			t.Errorf("ivar slot = %v, want 42", got)
		}
	}
	// The trait's own compiled methods are left untouched.
	m := trait.Methods[v.Selectors.Intern("peekA")]
	if !bytes.Contains(m.Bytecode, []byte{byte(vm.OpPushGlobal)}) {
		t.Errorf("trait method bytecode was mutated: % x", m.Bytecode)
	}
}

// A non-local return from a block the Dictionary primitives evaluate must
// return from the home method. They used to go through the lib's compiled
// Block>>value:…, whose Execute panicked on the foreign unwind and killed the
// program.
func TestNonLocalReturnFromDictionaryIteration(t *testing.T) {
	data, err := os.ReadFile("../maggie.image")
	if err != nil {
		t.Skipf("maggie.image not available: %v", err)
	}
	v := vm.NewVM()
	if err := v.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes: %v", err)
	}
	for _, src := range []string{
		"findIn: d d keysAndValuesDo: [:k :val | val = 2 ifTrue: [^k]]. ^0",
		"valueIn: d d do: [:val | val = 2 ifTrue: [^val * 10]]. ^0",
		"absentIn: d ^(d at: 99 ifAbsent: [^-1]) + 1000",
	} {
		m, err := Compile(src, v.Selectors, v.Symbols, v.Registry(), nil)
		if err != nil {
			t.Fatalf("compile %q: %v", src, err)
		}
		v.SmallIntegerClass.VTable.AddMethod(v.Selectors.Intern(m.Name()), m)
	}
	d := v.NewDictionary()
	v.Send(d, "at:put:", []vm.Value{vm.FromSmallInt(1), vm.FromSmallInt(1)})
	v.Send(d, "at:put:", []vm.Value{vm.FromSmallInt(7), vm.FromSmallInt(2)})
	wantSmallInt(t, v.Send(vm.FromSmallInt(0), "findIn:", []vm.Value{d}), 7)
	wantSmallInt(t, v.Send(vm.FromSmallInt(0), "valueIn:", []vm.Value{d}), 20)
	wantSmallInt(t, v.Send(vm.FromSmallInt(0), "absentIn:", []vm.Value{d}), -1)
}

// A cascade part is a message chain (ANSI): after `;` the first message goes
// to the cascade receiver and the rest to its result.
func TestCascadeMessageChains(t *testing.T) {
	wantSmallInt(t, doItValue(t, "#(1 2 3) size; size + 1"), 4)
	wantSmallInt(t, doItValue(t, "3 + 4; * 10 + 1"), 31)
	wantSmallInt(t, doItValue(t, "(#(1) size; class new: 3) size"), 3)
	wantSmallInt(t, doItValue(t, "#(1 2) size; size; size * 2 + 1"), 5)
	// Single-message parts are unchanged.
	wantSmallInt(t, doItValue(t, "(Array new: 2) at: 1 put: 9; at: 1"), 9)
}

// A block captures a plain variable by copying it, so a variable reassigned in
// its own scope AFTER a block captured it must be a cell — otherwise the block
// answers the stale value.
func TestBlockSeesOwnScopeAssignmentAfterCapture(t *testing.T) {
	cases := []struct {
		name, src string
		want      int64
	}{
		{"reassigned after capture", `afterCapture
	| x blk |
	x := 1. blk := [x]. x := 2.
	^blk value`, 2},
		{"loop assigns before capture textually", `loopOrder
	| i blk |
	i := 0.
	[i < 3] whileTrue: [i := i + 1. blk isNil ifTrue: [blk := [i]]].
	^blk value`, 3},
		{"loop condition", `loopCond
	| i blk |
	i := 0. blk := [i].
	[(i := i + 1) < 5] whileTrue.
	^blk value`, 5},
		{"block-local temp", `blockLocal
	^[:a | | y blk | y := 10. blk := [y]. y := a. blk value] value: 20`, 20},
		{"value captures its own target", `selfRef
	| x |
	x := [x].
	^(x value == x) ifTrue: [1] ifFalse: [0]`, 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			wantSmallInt(t, runOnSmallInt(t, vm.NewVM(), tc.src, nil), tc.want)
		})
	}
}

// A variable only assigned before any capture keeps the cheaper copy-capture.
func TestFindCellVariables_AssignedOnlyBeforeCapture(t *testing.T) {
	method, err := ParseMethodDef(`method: foo [ | x i |
  x := 1.
  i := 0.
  [i < 3] whileTrue: [i := i + 1].
  ^[x + i] value
]`)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if cells := NewCompiler(nil, nil, nil).findCellVariables(method); len(cells) != 0 {
		t.Errorf("no variable is assigned after a capture; got cells %v", cells)
	}
}
