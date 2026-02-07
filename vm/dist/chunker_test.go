package dist

import (
	"crypto/sha256"
	"testing"

	"github.com/chazu/maggie/vm"
)

func TestMethodToChunk(t *testing.T) {
	m := vm.NewCompiledMethodBuilder("test", 0).
		SetSource("method: test [ ^42 ]").
		Build()
	h := sha256.Sum256([]byte("test-hash"))
	m.SetContentHash(h)

	c := MethodToChunk(m, []string{"File"})

	if c.Hash != h {
		t.Error("Hash mismatch")
	}
	if c.Type != ChunkMethod {
		t.Error("Type should be ChunkMethod")
	}
	if c.Content != "method: test [ ^42 ]" {
		t.Errorf("Content: got %q", c.Content)
	}
	if len(c.Capabilities) != 1 || c.Capabilities[0] != "File" {
		t.Error("Capabilities mismatch")
	}
}

func TestClassToChunk(t *testing.T) {
	mh := sha256.Sum256([]byte("method"))
	d := &vm.ClassDigest{
		Name:           "Foo",
		SuperclassName: "Object",
		MethodHashes:   [][32]byte{mh},
		Hash:           sha256.Sum256([]byte("class-foo")),
	}

	c := ClassToChunk(d, "Foo subclass: Object", []string{"HTTP"})

	if c.Hash != d.Hash {
		t.Error("Hash mismatch")
	}
	if c.Type != ChunkClass {
		t.Error("Type should be ChunkClass")
	}
	if len(c.Dependencies) != 1 || c.Dependencies[0] != mh {
		t.Error("Dependencies should contain method hash")
	}
}

func TestModuleToChunk(t *testing.T) {
	ch := sha256.Sum256([]byte("class"))

	c := ModuleToChunk("MyApp::Models", [][32]byte{ch}, nil)

	if c.Type != ChunkModule {
		t.Error("Type should be ChunkModule")
	}
	if c.Content != "MyApp::Models" {
		t.Errorf("Content: got %q", c.Content)
	}
	if len(c.Dependencies) != 1 {
		t.Error("Should have 1 dependency")
	}
	if c.Hash == ([32]byte{}) {
		t.Error("Hash should not be zero")
	}
}

func TestTransitiveClosure(t *testing.T) {
	store := vm.NewContentStore()

	mh1 := sha256.Sum256([]byte("m1"))
	mh2 := sha256.Sum256([]byte("m2"))

	m1 := vm.NewCompiledMethodBuilder("m1", 0).Build()
	m1.SetContentHash(mh1)
	store.IndexMethod(m1)

	m2 := vm.NewCompiledMethodBuilder("m2", 0).Build()
	m2.SetContentHash(mh2)
	store.IndexMethod(m2)

	classHash := sha256.Sum256([]byte("class"))
	d := &vm.ClassDigest{
		Name:         "Foo",
		MethodHashes: [][32]byte{mh1, mh2},
		Hash:         classHash,
	}
	store.IndexClass(d)

	closure := TransitiveClosure(classHash, store)

	if len(closure) != 3 {
		t.Errorf("TransitiveClosure: got %d hashes, want 3 (class + 2 methods)", len(closure))
	}

	found := make(map[[32]byte]bool)
	for _, h := range closure {
		found[h] = true
	}
	if !found[classHash] {
		t.Error("closure should contain class hash")
	}
	if !found[mh1] {
		t.Error("closure should contain method hash 1")
	}
	if !found[mh2] {
		t.Error("closure should contain method hash 2")
	}
}

func TestBuildCapabilityManifest(t *testing.T) {
	store := vm.NewContentStore()

	mh := sha256.Sum256([]byte("m"))
	m := vm.NewCompiledMethodBuilder("m", 0).Build()
	m.SetContentHash(mh)
	store.IndexMethod(m)

	classHash := sha256.Sum256([]byte("class"))
	d := &vm.ClassDigest{
		Name:         "Foo",
		MethodHashes: [][32]byte{mh},
		Hash:         classHash,
	}
	store.IndexClass(d)

	chunks := map[[32]byte]*Chunk{
		mh:        {Hash: mh, Type: ChunkMethod, Capabilities: []string{"File"}},
		classHash: {Hash: classHash, Type: ChunkClass, Capabilities: []string{"HTTP"}},
	}

	manifest := BuildCapabilityManifest(classHash, store, chunks)
	if manifest == nil {
		t.Fatal("manifest should not be nil")
	}

	caps := make(map[string]bool)
	for _, c := range manifest.Required {
		caps[c] = true
	}
	if !caps["File"] || !caps["HTTP"] {
		t.Errorf("manifest should include File and HTTP, got %v", manifest.Required)
	}
}
