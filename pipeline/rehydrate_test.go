package pipeline

import (
	"strings"
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/compiler/hash"
	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// Helper: create a method stub in the ContentStore
// ---------------------------------------------------------------------------

// makeMethodStub creates a CompiledMethod stub with source text and a
// content hash, indexes it in the store, and returns its hash.
func makeMethodStub(t *testing.T, store *vm.ContentStore, source string, vmInst *vm.VM) [32]byte {
	t.Helper()

	// Parse the method to compute the real content hash
	methodDef, err := compiler.ParseMethodDef(source)
	if err != nil {
		t.Fatalf("ParseMethodDef(%q): %v", source, err)
	}

	h := hash.HashMethod(methodDef, nil, func(name string) string { return name })

	// Create a stub CompiledMethod with just Source and ContentHash
	stub := vm.NewCompiledMethod(methodDef.Selector, len(methodDef.Parameters))
	stub.Source = source
	stub.SetContentHash(h)

	store.IndexMethod(stub)
	return h
}

// makeMethodStubWithIvars is like makeMethodStub but with instance variable context.
func makeMethodStubWithIvars(t *testing.T, store *vm.ContentStore, source string, ivars []string, vmInst *vm.VM) [32]byte {
	t.Helper()

	methodDef, err := compiler.ParseMethodDef(source)
	if err != nil {
		t.Fatalf("ParseMethodDef(%q): %v", source, err)
	}

	ivarMap := make(map[string]int, len(ivars))
	for i, name := range ivars {
		ivarMap[name] = i
	}

	h := hash.HashMethod(methodDef, ivarMap, func(name string) string { return name })

	stub := vm.NewCompiledMethod(methodDef.Selector, len(methodDef.Parameters))
	stub.Source = source
	stub.SetContentHash(h)

	store.IndexMethod(stub)
	return h
}

// ---------------------------------------------------------------------------
// Test: Basic rehydration
// ---------------------------------------------------------------------------

func TestRehydrateBasic(t *testing.T) {
	vmInst := newTestVM(t)
	store := vmInst.ContentStore()

	// Create a method stub for "method: answer [ ^42 ]"
	mh := makeMethodStub(t, store, "method: answer [ ^42 ]", vmInst)

	// Create a class digest
	d := &vm.ClassDigest{
		Name:           "TestGreeter",
		SuperclassName: "Object",
		MethodHashes:   [][32]byte{mh},
	}
	d.Hash = vm.HashClass(d.Name, d.Namespace, d.SuperclassName, d.InstVars, d.ClassVars, d.DocString, d.MethodHashes)
	store.IndexClass(d)

	// Rehydrate
	compiled, err := RehydrateFromStore(vmInst)
	if err != nil {
		t.Fatalf("RehydrateFromStore: %v", err)
	}
	if compiled != 1 {
		t.Errorf("compiled = %d, want 1", compiled)
	}

	// Verify class exists
	cls := vmInst.Classes.Lookup("TestGreeter")
	if cls == nil {
		t.Fatal("TestGreeter class not found in ClassTable")
	}

	// Verify the class is in Globals
	if _, ok := vmInst.Global("TestGreeter"); !ok {
		t.Error("TestGreeter not found in Globals")
	}

	// Verify the method is callable
	result := vmInst.Send(vmInst.ClassValue(cls), "new", nil)
	result = vmInst.Send(result, "answer", nil)
	if result.IsSmallInt() && result.SmallInt() == 42 {
		// success
	} else {
		t.Errorf("TestGreeter>>answer returned %v, want 42", result)
	}
}

// ---------------------------------------------------------------------------
// Test: Superclass ordering
// ---------------------------------------------------------------------------

func TestRehydrateSuperclassOrdering(t *testing.T) {
	vmInst := newTestVM(t)
	store := vmInst.ContentStore()

	// Class A with a method
	mhA := makeMethodStub(t, store, "method: baseValue [ ^10 ]", vmInst)
	dA := &vm.ClassDigest{
		Name:           "TestBaseA",
		SuperclassName: "Object",
		MethodHashes:   [][32]byte{mhA},
	}
	dA.Hash = vm.HashClass(dA.Name, dA.Namespace, dA.SuperclassName, dA.InstVars, dA.ClassVars, dA.DocString, dA.MethodHashes)
	store.IndexClass(dA)

	// Class B extends A
	mhB := makeMethodStub(t, store, "method: derivedValue [ ^20 ]", vmInst)
	dB := &vm.ClassDigest{
		Name:           "TestDerivedB",
		SuperclassName: "TestBaseA",
		MethodHashes:   [][32]byte{mhB},
	}
	dB.Hash = vm.HashClass(dB.Name, dB.Namespace, dB.SuperclassName, dB.InstVars, dB.ClassVars, dB.DocString, dB.MethodHashes)
	store.IndexClass(dB)

	// Rehydrate (both classes must be created in correct order)
	compiled, err := RehydrateFromStore(vmInst)
	if err != nil {
		t.Fatalf("RehydrateFromStore: %v", err)
	}
	if compiled != 2 {
		t.Errorf("compiled = %d, want 2", compiled)
	}

	// Verify both classes exist
	clsA := vmInst.Classes.Lookup("TestBaseA")
	if clsA == nil {
		t.Fatal("TestBaseA not found")
	}
	clsB := vmInst.Classes.Lookup("TestDerivedB")
	if clsB == nil {
		t.Fatal("TestDerivedB not found")
	}

	// Verify superclass chain
	if clsB.Superclass != clsA {
		t.Errorf("TestDerivedB.Superclass = %v, want TestBaseA", clsB.Superclass)
	}

	// Verify inherited method works
	inst := vmInst.Send(vmInst.ClassValue(clsB), "new", nil)
	base := vmInst.Send(inst, "baseValue", nil)
	if !base.IsSmallInt() || base.SmallInt() != 10 {
		t.Errorf("TestDerivedB>>baseValue = %v, want 10", base)
	}
	derived := vmInst.Send(inst, "derivedValue", nil)
	if !derived.IsSmallInt() || derived.SmallInt() != 20 {
		t.Errorf("TestDerivedB>>derivedValue = %v, want 20", derived)
	}
}

// ---------------------------------------------------------------------------
// Test: Class method
// ---------------------------------------------------------------------------

func TestRehydrateClassMethod(t *testing.T) {
	vmInst := newTestVM(t)
	store := vmInst.ContentStore()

	// Create a class-side method stub
	mh := makeMethodStub(t, store, "classMethod: classAnswer [ ^99 ]", vmInst)

	d := &vm.ClassDigest{
		Name:           "TestClassSide",
		SuperclassName: "Object",
		MethodHashes:   [][32]byte{mh},
	}
	d.Hash = vm.HashClass(d.Name, d.Namespace, d.SuperclassName, d.InstVars, d.ClassVars, d.DocString, d.MethodHashes)
	store.IndexClass(d)

	compiled, err := RehydrateFromStore(vmInst)
	if err != nil {
		t.Fatalf("RehydrateFromStore: %v", err)
	}
	if compiled != 1 {
		t.Errorf("compiled = %d, want 1", compiled)
	}

	cls := vmInst.Classes.Lookup("TestClassSide")
	if cls == nil {
		t.Fatal("TestClassSide not found")
	}

	// Verify class-side method
	result := vmInst.Send(vmInst.ClassValue(cls), "classAnswer", nil)
	if !result.IsSmallInt() || result.SmallInt() != 99 {
		t.Errorf("TestClassSide class>>classAnswer = %v, want 99", result)
	}
}

// ---------------------------------------------------------------------------
// Test: Hash verification (tampered source)
// ---------------------------------------------------------------------------

func TestRehydrateHashMismatch(t *testing.T) {
	vmInst := newTestVM(t)
	store := vmInst.ContentStore()

	// Create a method stub with correct hash
	source := "method: goodMethod [ ^1 ]"
	methodDef, err := compiler.ParseMethodDef(source)
	if err != nil {
		t.Fatalf("ParseMethodDef: %v", err)
	}
	h := hash.HashMethod(methodDef, nil, func(name string) string { return name })

	// Store a stub with the correct hash but tampered source
	stub := vm.NewCompiledMethod("goodMethod", 0)
	stub.Source = "method: goodMethod [ ^999 ]" // different body!
	stub.SetContentHash(h)
	store.IndexMethod(stub)

	d := &vm.ClassDigest{
		Name:           "TestTampered",
		SuperclassName: "Object",
		MethodHashes:   [][32]byte{h},
	}
	d.Hash = vm.HashClass(d.Name, d.Namespace, d.SuperclassName, d.InstVars, d.ClassVars, d.DocString, d.MethodHashes)
	store.IndexClass(d)

	_, err = RehydrateFromStore(vmInst)
	if err == nil {
		t.Fatal("expected hash mismatch error, got nil")
	}
	if !contains(err.Error(), "hash mismatch") {
		t.Errorf("error should mention hash mismatch, got: %v", err)
	}
}

// ---------------------------------------------------------------------------
// Test: Namespaced class
// ---------------------------------------------------------------------------

func TestRehydrateNamespaced(t *testing.T) {
	vmInst := newTestVM(t)
	store := vmInst.ContentStore()

	mh := makeMethodStub(t, store, "method: nsValue [ ^77 ]", vmInst)

	d := &vm.ClassDigest{
		Name:           "Widget",
		Namespace:      "TestNS",
		SuperclassName: "Object",
		MethodHashes:   [][32]byte{mh},
	}
	d.Hash = vm.HashClass(d.Name, d.Namespace, d.SuperclassName, d.InstVars, d.ClassVars, d.DocString, d.MethodHashes)
	store.IndexClass(d)

	compiled, err := RehydrateFromStore(vmInst)
	if err != nil {
		t.Fatalf("RehydrateFromStore: %v", err)
	}
	if compiled != 1 {
		t.Errorf("compiled = %d, want 1", compiled)
	}

	// Should be registered under FQN
	cls := vmInst.Classes.Lookup("TestNS::Widget")
	if cls == nil {
		t.Fatal("TestNS::Widget not found in ClassTable")
	}
	if _, ok := vmInst.Global("TestNS::Widget"); !ok {
		t.Error("TestNS::Widget not found in Globals")
	}
	// Should NOT be registered under bare name
	if _, ok := vmInst.Global("Widget"); ok {
		t.Error("Widget should not be in Globals under bare name")
	}
}

// ---------------------------------------------------------------------------
// Test: Already-compiled classes are skipped
// ---------------------------------------------------------------------------

func TestRehydrateSkipsExisting(t *testing.T) {
	vmInst := newTestVM(t)
	store := vmInst.ContentStore()

	// Create a class digest for "Object" which already exists
	d := &vm.ClassDigest{
		Name:           "Object",
		SuperclassName: "",
	}
	d.Hash = vm.HashClass(d.Name, d.Namespace, d.SuperclassName, d.InstVars, d.ClassVars, d.DocString, d.MethodHashes)
	store.IndexClass(d)

	compiled, err := RehydrateFromStore(vmInst)
	if err != nil {
		t.Fatalf("RehydrateFromStore: %v", err)
	}
	if compiled != 0 {
		t.Errorf("compiled = %d, want 0 (Object already exists)", compiled)
	}
}

func contains(s, sub string) bool {
	return len(s) >= len(sub) && containsStr(s, sub)
}

func containsStr(s, sub string) bool {
	for i := 0; i <= len(s)-len(sub); i++ {
		if s[i:i+len(sub)] == sub {
			return true
		}
	}
	return false
}

// copyStore moves every method and class digest from src's content store into
// dst's, simulating content received over sync.
func copyStore(src, dst *vm.VM) {
	from, to := src.ContentStore(), dst.ContentStore()
	for _, h := range from.MethodHashes() {
		to.IndexMethod(from.LookupMethod(h))
	}
	for _, h := range from.ClassHashes() {
		to.IndexClass(from.LookupClass(h))
	}
}

// ---------------------------------------------------------------------------
// Test: cross-namespace superclass (App::Widget < Lib::Base) and documented
// methods survive compile → store → rehydrate on a fresh VM.
// ---------------------------------------------------------------------------

func TestRehydrateCrossNamespaceSuperclassAndDocstrings(t *testing.T) {
	vmA := newTestVM(t)
	pipe := newPipeline(vmA)

	if _, err := pipe.CompileSourceFile(`RhxBase subclass: Object
  instanceVars: base

  """Answers ten."""
  method: baseValue [ ^10 ]
`, "lib_base.mag", "Lib"); err != nil {
		t.Fatalf("compile Lib::RhxBase: %v", err)
	}
	if _, err := pipe.CompileSourceFile(`import: 'Lib'

RhxWidget subclass: RhxBase
  instanceVars: label

  """Answers twenty."""
  method: widgetValue [ ^20 ]
`, "app_widget.mag", "App"); err != nil {
		t.Fatalf("compile App::RhxWidget: %v", err)
	}

	vmB := newTestVM(t)
	copyStore(vmA, vmB)

	if _, err := RehydrateFromStore(vmB); err != nil {
		t.Fatalf("RehydrateFromStore: %v", err)
	}

	base := vmB.Classes.Lookup("Lib::RhxBase")
	widget := vmB.Classes.Lookup("App::RhxWidget")
	if base == nil || widget == nil {
		t.Fatalf("rehydrated classes missing: base=%v widget=%v", base, widget)
	}
	if widget.Superclass != base {
		t.Errorf("App::RhxWidget superclass = %v, want Lib::RhxBase", widget.Superclass)
	}
	if widget.NumSlots != 2 {
		t.Errorf("App::RhxWidget.NumSlots = %d, want 2", widget.NumSlots)
	}
	inst := vmB.Send(vmB.ClassValue(widget), "new", nil)
	if v := vmB.Send(inst, "baseValue", nil); !v.IsSmallInt() || v.SmallInt() != 10 {
		t.Errorf("baseValue = %v, want 10", v)
	}
	if v := vmB.Send(inst, "widgetValue", nil); !v.IsSmallInt() || v.SmallInt() != 20 {
		t.Errorf("widgetValue = %v, want 20", v)
	}
}

// ---------------------------------------------------------------------------
// Test: a legacy short superclass name resolves in the class's own namespace
// before the root namespace (LookupWithImports order).
// ---------------------------------------------------------------------------

func TestRehydrateShortSuperclassPrefersOwnNamespace(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
	if _, err := pipe.CompileSourceFile("RhpBase subclass: Object\n  method: which [ ^1 ]\n", "root.mag", ""); err != nil {
		t.Fatalf("compile root RhpBase: %v", err)
	}
	if _, err := pipe.CompileSourceFile("RhpBase subclass: Object\n  method: which [ ^2 ]\n", "app.mag", "App"); err != nil {
		t.Fatalf("compile App::RhpBase: %v", err)
	}
	appBase := vmInst.Classes.Lookup("App::RhpBase")
	if appBase == nil || appBase == vmInst.Classes.Lookup("RhpBase") {
		t.Fatal("expected distinct root RhpBase and App::RhpBase")
	}

	store := vmInst.ContentStore()
	d := &vm.ClassDigest{Name: "RhpWidget", Namespace: "App", SuperclassName: "RhpBase"}
	d.Hash = vm.HashClass(d.Name, d.Namespace, d.SuperclassName, d.InstVars, d.ClassVars, d.DocString, d.MethodHashes)
	store.IndexClass(d)

	if _, err := RehydrateFromStore(vmInst); err != nil {
		t.Fatalf("RehydrateFromStore: %v", err)
	}
	widget := vmInst.Classes.Lookup("App::RhpWidget")
	if widget == nil {
		t.Fatal("App::RhpWidget not rehydrated")
	}
	if widget.Superclass != appBase {
		t.Errorf("App::RhpWidget superclass = %s, want App::RhpBase", widget.Superclass.FullName())
	}
}

// ---------------------------------------------------------------------------
// Test: two different digests for the same FQN are a conflict, not a
// nondeterministic overwrite + double compile.
// ---------------------------------------------------------------------------

func TestRehydrateRejectsConflictingDigestsForSameFQN(t *testing.T) {
	vmInst := newTestVM(t)
	store := vmInst.ContentStore()

	mh1 := makeMethodStub(t, store, "method: v [ ^1 ]", vmInst)
	mh2 := makeMethodStub(t, store, "method: v [ ^2 ]", vmInst)
	for _, mh := range [][32]byte{mh1, mh2} {
		d := &vm.ClassDigest{Name: "RhDup", Namespace: "App", SuperclassName: "Object", MethodHashes: [][32]byte{mh}}
		d.Hash = vm.HashClass(d.Name, d.Namespace, d.SuperclassName, d.InstVars, d.ClassVars, d.DocString, d.MethodHashes)
		store.IndexClass(d)
	}

	compiled, err := RehydrateFromStore(vmInst)
	if err == nil || !strings.Contains(err.Error(), "App::RhDup") || !strings.Contains(err.Error(), "conflicting") {
		t.Fatalf("expected conflicting-digest error for App::RhDup, got compiled=%d err=%v", compiled, err)
	}
	if vmInst.Classes.Lookup("App::RhDup") != nil {
		t.Error("App::RhDup should not be registered when its digests conflict")
	}
}

// A digest App::RhsStream whose superclass short name equals its own name
// refers to the root class of that name, not to itself.
func TestRehydrateSuperclassSameShortNameIsRootClass(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
	if _, err := pipe.CompileSourceFile("RhsStream subclass: Object\n  method: which [ ^1 ]\n", "root.mag", ""); err != nil {
		t.Fatalf("compile root RhsStream: %v", err)
	}
	root := vmInst.Classes.Lookup("RhsStream")

	store := vmInst.ContentStore()
	d := &vm.ClassDigest{Name: "RhsStream", Namespace: "App", SuperclassName: "RhsStream"}
	d.Hash = vm.HashClass(d.Name, d.Namespace, d.SuperclassName, d.InstVars, d.ClassVars, d.DocString, d.MethodHashes)
	store.IndexClass(d)

	if _, err := RehydrateFromStore(vmInst); err != nil {
		t.Fatalf("RehydrateFromStore: %v", err)
	}
	app := vmInst.Classes.Lookup("App::RhsStream")
	if app == nil || app.Superclass != root {
		t.Fatalf("App::RhsStream superclass = %v, want root RhsStream", app)
	}
}
