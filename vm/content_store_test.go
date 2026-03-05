package vm

import (
	"crypto/sha256"
	"fmt"
	"strings"
	"testing"
)

func TestContentStore_IndexAndLookupMethod(t *testing.T) {
	cs := NewContentStore()

	m := &CompiledMethod{}
	h := sha256.Sum256([]byte("test-method"))
	m.SetContentHash(h)

	cs.IndexMethod(m)

	if !cs.HasHash(h) {
		t.Error("HasHash should return true for indexed method")
	}

	got := cs.LookupMethod(h)
	if got != m {
		t.Error("LookupMethod should return the indexed method")
	}

	if cs.MethodCount() != 1 {
		t.Errorf("MethodCount: got %d, want 1", cs.MethodCount())
	}
}

func TestContentStore_IgnoresZeroHash(t *testing.T) {
	cs := NewContentStore()

	m := &CompiledMethod{}
	cs.IndexMethod(m)

	if cs.MethodCount() != 0 {
		t.Errorf("Should not index method with zero hash, got count %d", cs.MethodCount())
	}
}

func TestContentStore_IndexAndLookupClass(t *testing.T) {
	cs := NewContentStore()

	d := &ClassDigest{
		Name:           "Foo",
		SuperclassName: "Object",
		Hash:           sha256.Sum256([]byte("class-foo")),
	}
	cs.IndexClass(d)

	if !cs.HasHash(d.Hash) {
		t.Error("HasHash should return true for indexed class")
	}

	got := cs.LookupClass(d.Hash)
	if got != d {
		t.Error("LookupClass should return the indexed class digest")
	}

	if cs.ClassCount() != 1 {
		t.Errorf("ClassCount: got %d, want 1", cs.ClassCount())
	}
}

func TestContentStore_AllHashes(t *testing.T) {
	cs := NewContentStore()

	h1 := sha256.Sum256([]byte("m1"))
	h2 := sha256.Sum256([]byte("c1"))

	m := &CompiledMethod{}
	m.SetContentHash(h1)
	cs.IndexMethod(m)

	d := &ClassDigest{Name: "C1", Hash: h2}
	cs.IndexClass(d)

	all := cs.AllHashes()
	if len(all) != 2 {
		t.Errorf("AllHashes: got %d, want 2", len(all))
	}
}

func TestContentStore_LookupClassByName(t *testing.T) {
	cs := NewContentStore()

	d := &ClassDigest{
		Name: "MyApp::Greeter",
		Hash: sha256.Sum256([]byte("greeter")),
	}
	cs.IndexClass(d)

	got := cs.LookupClassByName("MyApp::Greeter")
	if got != d {
		t.Error("LookupClassByName should find the class")
	}

	got = cs.LookupClassByName("NoSuchClass")
	if got != nil {
		t.Error("LookupClassByName should return nil for missing class")
	}
}

func TestContentStore_AllClassDigests(t *testing.T) {
	cs := NewContentStore()

	d1 := &ClassDigest{Name: "A", Hash: sha256.Sum256([]byte("a"))}
	d2 := &ClassDigest{Name: "B", Hash: sha256.Sum256([]byte("b"))}
	cs.IndexClass(d1)
	cs.IndexClass(d2)

	all := cs.AllClassDigests()
	if len(all) != 2 {
		t.Errorf("AllClassDigests: got %d, want 2", len(all))
	}
}

func TestContentStore_AllMethodHashes(t *testing.T) {
	cs := NewContentStore()

	m1 := &CompiledMethod{}
	h1 := sha256.Sum256([]byte("m1"))
	m1.SetContentHash(h1)
	cs.IndexMethod(m1)

	m2 := &CompiledMethod{}
	h2 := sha256.Sum256([]byte("m2"))
	m2.SetContentHash(h2)
	cs.IndexMethod(m2)

	all := cs.AllMethodHashes()
	if len(all) != 2 {
		t.Errorf("AllMethodHashes: got %d, want 2", len(all))
	}
}

func TestContentStore_LookupByPrefix(t *testing.T) {
	cs := NewContentStore()

	// Add a method
	m := &CompiledMethod{}
	mh := sha256.Sum256([]byte("test-method-for-prefix"))
	m.SetContentHash(mh)
	cs.IndexMethod(m)

	// Add a class
	ch := sha256.Sum256([]byte("test-class-for-prefix"))
	d := &ClassDigest{Name: "TestClass", Hash: ch}
	cs.IndexClass(d)

	mhHex := fmt.Sprintf("%x", mh)
	chHex := fmt.Sprintf("%x", ch)

	// Test: too short prefix
	_, _, err := cs.LookupByPrefix("ab")
	if err == nil {
		t.Error("Should error on prefix shorter than 4 chars")
	}

	// Test: prefix not found
	_, _, err = cs.LookupByPrefix("0000")
	if err == nil || !strings.Contains(err.Error(), "no hash found") {
		// It's possible 0000 matches something, so check carefully
		if err == nil {
			t.Error("Expected error or specific match for 0000 prefix")
		}
	}

	// Test: method lookup by prefix
	hash, typ, err := cs.LookupByPrefix(mhHex[:8])
	if err != nil {
		t.Fatalf("LookupByPrefix method: %v", err)
	}
	if hash != mh {
		t.Error("Hash mismatch for method lookup")
	}
	if typ != "method" {
		t.Errorf("Type: got %q, want %q", typ, "method")
	}

	// Test: class lookup by prefix
	hash, typ, err = cs.LookupByPrefix(chHex[:8])
	if err != nil {
		t.Fatalf("LookupByPrefix class: %v", err)
	}
	if hash != ch {
		t.Error("Hash mismatch for class lookup")
	}
	if typ != "class" {
		t.Errorf("Type: got %q, want %q", typ, "class")
	}
}

func TestHashClass_Deterministic(t *testing.T) {
	mh1 := sha256.Sum256([]byte("method1"))
	mh2 := sha256.Sum256([]byte("method2"))

	h1 := HashClass("Foo", "MyNS", "Object", []string{"a", "b"}, nil, "doc", [][32]byte{mh1, mh2})
	h2 := HashClass("Foo", "MyNS", "Object", []string{"a", "b"}, nil, "doc", [][32]byte{mh2, mh1})

	if h1 != h2 {
		t.Error("HashClass should produce same hash regardless of method hash order")
	}
}

func TestHashClass_DifferentInputsDiffer(t *testing.T) {
	mh := sha256.Sum256([]byte("method1"))

	h1 := HashClass("Foo", "NS", "Object", nil, nil, "", [][32]byte{mh})
	h2 := HashClass("Bar", "NS", "Object", nil, nil, "", [][32]byte{mh})

	if h1 == h2 {
		t.Error("Different class names should produce different hashes")
	}
}

func TestDigestClass(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	c := NewClassWithInstVars("TestDigest", vm.ObjectClass, []string{"x", "y"})
	c.Namespace = "TestNS"
	c.DocString = "A test class"

	d := DigestClass(c)

	if d.Name != "TestDigest" {
		t.Errorf("Name: got %q, want %q", d.Name, "TestDigest")
	}
	if d.Namespace != "TestNS" {
		t.Errorf("Namespace: got %q, want %q", d.Namespace, "TestNS")
	}
	if d.SuperclassName != "Object" {
		t.Errorf("SuperclassName: got %q, want %q", d.SuperclassName, "Object")
	}
	if len(d.InstVars) != 2 {
		t.Errorf("InstVars: got %d, want 2", len(d.InstVars))
	}
	if d.DocString != "A test class" {
		t.Errorf("DocString: got %q, want %q", d.DocString, "A test class")
	}
	if d.Hash == ([32]byte{}) {
		t.Error("Hash should not be zero")
	}
}
