package vm

import (
	"crypto/sha256"
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
