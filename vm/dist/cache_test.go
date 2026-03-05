package dist

import (
	"crypto/sha256"
	"testing"

	"github.com/chazu/maggie/vm"
)

func TestDiskCache_PutGet_RoundTrip(t *testing.T) {
	dc, err := NewDiskCache(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}

	h := sha256.Sum256([]byte("hello"))
	chunk := &Chunk{
		Hash:    h,
		Type:    ChunkMethod,
		Content: "method: foo [ ^42 ]",
	}

	if err := dc.Put(chunk); err != nil {
		t.Fatalf("Put: %v", err)
	}

	got, err := dc.Get(h)
	if err != nil {
		t.Fatalf("Get: %v", err)
	}
	if got.Hash != h {
		t.Error("Hash mismatch after round-trip")
	}
	if got.Type != ChunkMethod {
		t.Errorf("Type mismatch: got %d, want %d", got.Type, ChunkMethod)
	}
	if got.Content != chunk.Content {
		t.Errorf("Content mismatch: got %q, want %q", got.Content, chunk.Content)
	}
}

func TestDiskCache_PutGet_ClassChunk(t *testing.T) {
	dc, err := NewDiskCache(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}

	mh := sha256.Sum256([]byte("method1"))
	h := sha256.Sum256([]byte("class-foo"))
	chunk := &Chunk{
		Hash:         h,
		Type:         ChunkClass,
		Content:      "Foo",
		Dependencies: [][32]byte{mh},
	}

	if err := dc.Put(chunk); err != nil {
		t.Fatalf("Put: %v", err)
	}

	got, err := dc.Get(h)
	if err != nil {
		t.Fatalf("Get: %v", err)
	}
	if got.Type != ChunkClass {
		t.Errorf("Type: got %d, want %d", got.Type, ChunkClass)
	}
	if len(got.Dependencies) != 1 || got.Dependencies[0] != mh {
		t.Error("Dependencies mismatch after round-trip")
	}
}

func TestDiskCache_Has(t *testing.T) {
	dc, err := NewDiskCache(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}

	h := sha256.Sum256([]byte("exists"))
	missing := sha256.Sum256([]byte("missing"))

	if dc.Has(h) {
		t.Error("Has should return false for missing hash")
	}

	chunk := &Chunk{Hash: h, Type: ChunkMethod, Content: "x"}
	if err := dc.Put(chunk); err != nil {
		t.Fatal(err)
	}

	if !dc.Has(h) {
		t.Error("Has should return true after Put")
	}
	if dc.Has(missing) {
		t.Error("Has should return false for never-stored hash")
	}
}

func TestDiskCache_Get_Missing(t *testing.T) {
	dc, err := NewDiskCache(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}

	h := sha256.Sum256([]byte("no-such-chunk"))
	_, err = dc.Get(h)
	if err == nil {
		t.Error("Get should return error for missing chunk")
	}
}

func TestDiskCache_AllHashes(t *testing.T) {
	dc, err := NewDiskCache(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}

	h1 := sha256.Sum256([]byte("one"))
	h2 := sha256.Sum256([]byte("two"))
	h3 := sha256.Sum256([]byte("three"))

	for _, h := range [][32]byte{h1, h2, h3} {
		chunk := &Chunk{Hash: h, Type: ChunkMethod, Content: "x"}
		if err := dc.Put(chunk); err != nil {
			t.Fatal(err)
		}
	}

	hashes, err := dc.AllHashes()
	if err != nil {
		t.Fatal(err)
	}
	if len(hashes) != 3 {
		t.Fatalf("AllHashes: got %d, want 3", len(hashes))
	}

	hashSet := make(map[[32]byte]bool)
	for _, h := range hashes {
		hashSet[h] = true
	}
	for _, want := range [][32]byte{h1, h2, h3} {
		if !hashSet[want] {
			t.Errorf("AllHashes missing %x", want)
		}
	}
}

func TestDiskCache_AllHashes_Empty(t *testing.T) {
	dc, err := NewDiskCache(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}

	hashes, err := dc.AllHashes()
	if err != nil {
		t.Fatal(err)
	}
	if len(hashes) != 0 {
		t.Errorf("AllHashes on empty cache: got %d, want 0", len(hashes))
	}
}

func TestDiskCache_LoadInto(t *testing.T) {
	dc, err := NewDiskCache(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}

	// Store a method chunk
	mh := sha256.Sum256([]byte("method-src"))
	mChunk := &Chunk{Hash: mh, Type: ChunkMethod, Content: "method: bar [ ^1 ]"}
	if err := dc.Put(mChunk); err != nil {
		t.Fatal(err)
	}

	// Store a class chunk
	ch := sha256.Sum256([]byte("class-src"))
	cChunk := &Chunk{
		Hash:         ch,
		Type:         ChunkClass,
		Content:      "MyClass",
		Dependencies: [][32]byte{mh},
	}
	if err := dc.Put(cChunk); err != nil {
		t.Fatal(err)
	}

	// Load into a fresh store
	store := vm.NewContentStore()
	loaded, err := dc.LoadInto(store)
	if err != nil {
		t.Fatal(err)
	}
	if loaded != 2 {
		t.Errorf("LoadInto: loaded %d, want 2", loaded)
	}

	// Verify method is present
	m := store.LookupMethod(mh)
	if m == nil {
		t.Error("Method not found in store after LoadInto")
	} else if m.Source != "method: bar [ ^1 ]" {
		t.Errorf("Method source: got %q, want %q", m.Source, "method: bar [ ^1 ]")
	}

	// Verify class is present
	d := store.LookupClass(ch)
	if d == nil {
		t.Error("Class not found in store after LoadInto")
	} else {
		if d.Name != "MyClass" {
			t.Errorf("Class name: got %q, want %q", d.Name, "MyClass")
		}
		if len(d.MethodHashes) != 1 || d.MethodHashes[0] != mh {
			t.Error("Class method hashes mismatch")
		}
	}
}

func TestDiskCache_LoadInto_SkipsExisting(t *testing.T) {
	dc, err := NewDiskCache(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}

	h := sha256.Sum256([]byte("already-there"))
	chunk := &Chunk{Hash: h, Type: ChunkMethod, Content: "method: x [ ^1 ]"}
	if err := dc.Put(chunk); err != nil {
		t.Fatal(err)
	}

	// Pre-populate the store
	store := vm.NewContentStore()
	existing := &vm.CompiledMethod{Source: "original source"}
	existing.SetContentHash(h)
	store.IndexMethod(existing)

	loaded, err := dc.LoadInto(store)
	if err != nil {
		t.Fatal(err)
	}
	if loaded != 0 {
		t.Errorf("LoadInto should skip existing: loaded %d, want 0", loaded)
	}

	// Original should be preserved
	m := store.LookupMethod(h)
	if m.Source != "original source" {
		t.Error("LoadInto should not overwrite existing store entry")
	}
}

func TestDiskCache_SaveFrom(t *testing.T) {
	dc, err := NewDiskCache(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}

	store := vm.NewContentStore()

	// Add a method to the store
	mh := sha256.Sum256([]byte("save-method"))
	m := &vm.CompiledMethod{Source: "method: save [ ^true ]"}
	m.SetContentHash(mh)
	store.IndexMethod(m)

	// Add a class to the store
	ch := sha256.Sum256([]byte("save-class"))
	d := &vm.ClassDigest{
		Name:         "SavedClass",
		Hash:         ch,
		MethodHashes: [][32]byte{mh},
	}
	store.IndexClass(d)

	written, err := dc.SaveFrom(store)
	if err != nil {
		t.Fatal(err)
	}
	if written != 2 {
		t.Errorf("SaveFrom: wrote %d, want 2", written)
	}

	// Verify files exist
	if !dc.Has(mh) {
		t.Error("Method chunk not saved to disk")
	}
	if !dc.Has(ch) {
		t.Error("Class chunk not saved to disk")
	}
}

func TestDiskCache_SaveFrom_SkipsExisting(t *testing.T) {
	dc, err := NewDiskCache(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}

	// Pre-cache a method chunk
	mh := sha256.Sum256([]byte("pre-cached"))
	preChunk := &Chunk{Hash: mh, Type: ChunkMethod, Content: "method: old [ ^0 ]"}
	if err := dc.Put(preChunk); err != nil {
		t.Fatal(err)
	}

	// Add the same hash to the store (different content doesn't matter — same hash)
	store := vm.NewContentStore()
	m := &vm.CompiledMethod{Source: "method: old [ ^0 ]"}
	m.SetContentHash(mh)
	store.IndexMethod(m)

	written, err := dc.SaveFrom(store)
	if err != nil {
		t.Fatal(err)
	}
	if written != 0 {
		t.Errorf("SaveFrom should skip already-cached: wrote %d, want 0", written)
	}
}

func TestDiskCache_SaveFrom_LoadInto_RoundTrip(t *testing.T) {
	dir := t.TempDir()
	dc, err := NewDiskCache(dir)
	if err != nil {
		t.Fatal(err)
	}

	// Build a store with methods and classes
	storeA := vm.NewContentStore()

	mh1 := sha256.Sum256([]byte("m1"))
	m1 := &vm.CompiledMethod{Source: "method: alpha [ ^1 ]"}
	m1.SetContentHash(mh1)
	storeA.IndexMethod(m1)

	mh2 := sha256.Sum256([]byte("m2"))
	m2 := &vm.CompiledMethod{Source: "method: beta [ ^2 ]"}
	m2.SetContentHash(mh2)
	storeA.IndexMethod(m2)

	ch := sha256.Sum256([]byte("cls"))
	d := &vm.ClassDigest{
		Name:         "RoundTripClass",
		Hash:         ch,
		MethodHashes: [][32]byte{mh1, mh2},
	}
	storeA.IndexClass(d)

	// Save to disk
	written, err := dc.SaveFrom(storeA)
	if err != nil {
		t.Fatal(err)
	}
	if written != 3 {
		t.Errorf("SaveFrom: wrote %d, want 3", written)
	}

	// Load into a fresh store
	storeB := vm.NewContentStore()
	loaded, err := dc.LoadInto(storeB)
	if err != nil {
		t.Fatal(err)
	}
	if loaded != 3 {
		t.Errorf("LoadInto: loaded %d, want 3", loaded)
	}

	// Verify methods
	if storeB.LookupMethod(mh1) == nil {
		t.Error("Method mh1 missing after round-trip")
	}
	if storeB.LookupMethod(mh2) == nil {
		t.Error("Method mh2 missing after round-trip")
	}

	// Verify class
	dB := storeB.LookupClass(ch)
	if dB == nil {
		t.Fatal("Class missing after round-trip")
	}
	if dB.Name != "RoundTripClass" {
		t.Errorf("Class name: got %q, want %q", dB.Name, "RoundTripClass")
	}
	if len(dB.MethodHashes) != 2 {
		t.Errorf("Class method hashes: got %d, want 2", len(dB.MethodHashes))
	}

	// Verify method sources
	m1B := storeB.LookupMethod(mh1)
	if m1B.Source != "method: alpha [ ^1 ]" {
		t.Errorf("Method source: got %q, want %q", m1B.Source, "method: alpha [ ^1 ]")
	}
}
