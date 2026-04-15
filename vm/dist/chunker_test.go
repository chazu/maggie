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
	if c.Selector != "test" {
		t.Errorf("Selector: got %q, want %q", c.Selector, "test")
	}
}

func TestMethodToChunkWithClass(t *testing.T) {
	m := vm.NewCompiledMethodBuilder("greet:", 1).
		SetSource("method: greet: name [ ^'Hello ', name ]").
		Build()
	h := sha256.Sum256([]byte("greet-hash"))
	m.SetContentHash(h)

	cls := &vm.Class{Name: "Greeter", Namespace: "MyApp"}
	m.SetClass(cls)
	m.IsClassMethod = true

	c := MethodToChunk(m, nil)

	if c.Selector != "greet:" {
		t.Errorf("Selector: got %q, want %q", c.Selector, "greet:")
	}
	if c.ClassName != "MyApp::Greeter" {
		t.Errorf("ClassName: got %q, want %q", c.ClassName, "MyApp::Greeter")
	}
	if !c.IsClassSide {
		t.Error("IsClassSide should be true for class method")
	}
}

func TestMethodToChunkWithClassNoNamespace(t *testing.T) {
	m := vm.NewCompiledMethodBuilder("hello", 0).
		SetSource("method: hello [ ^'hi' ]").
		Build()
	h := sha256.Sum256([]byte("hello-hash"))
	m.SetContentHash(h)

	cls := &vm.Class{Name: "Greeter"}
	m.SetClass(cls)

	c := MethodToChunk(m, nil)

	if c.ClassName != "Greeter" {
		t.Errorf("ClassName: got %q, want %q", c.ClassName, "Greeter")
	}
	if c.IsClassSide {
		t.Error("IsClassSide should be false for instance method")
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

	c := ClassToChunk(d, []string{"HTTP"})

	if c.Hash != d.Hash {
		t.Error("Hash mismatch")
	}
	if c.Type != ChunkClass {
		t.Error("Type should be ChunkClass")
	}
	if len(c.Dependencies) != 1 || c.Dependencies[0] != mh {
		t.Error("Dependencies should contain method hash")
	}
	// Content should be the encoded class content format
	if c.Content != "CLASS Foo\nSUPER Object" {
		t.Errorf("Content: got %q, want %q", c.Content, "CLASS Foo\nSUPER Object")
	}
}

func TestClassToChunkFullMetadata(t *testing.T) {
	mh := sha256.Sum256([]byte("method"))
	d := &vm.ClassDigest{
		Name:           "Greeter",
		Namespace:      "MyApp",
		SuperclassName: "Object",
		InstVars:       []string{"name", "greeting"},
		ClassVars:      []string{"defaultGreeting"},
		DocString:      "A simple greeter class.",
		MethodHashes:   [][32]byte{mh},
		Hash:           sha256.Sum256([]byte("class-greeter")),
	}

	c := ClassToChunk(d, nil)

	expected := "CLASS Greeter\nNAMESPACE MyApp\nSUPER Object\nIVARS name greeting\nCVARS defaultGreeting\nDOC A simple greeter class."
	if c.Content != expected {
		t.Errorf("Content:\n  got:  %q\n  want: %q", c.Content, expected)
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

// ---------------------------------------------------------------------------
// EncodeClassContent / DecodeClassContent round-trip tests
// ---------------------------------------------------------------------------

func TestEncodeDecodeClassContentRoundTrip(t *testing.T) {
	original := &vm.ClassDigest{
		Name:           "Greeter",
		Namespace:      "MyApp",
		SuperclassName: "Object",
		InstVars:       []string{"name", "greeting"},
		ClassVars:      []string{"defaultGreeting"},
		DocString:      "A simple greeter class.",
	}

	encoded := EncodeClassContent(original)
	decoded, err := DecodeClassContent(encoded)
	if err != nil {
		t.Fatalf("DecodeClassContent: %v", err)
	}

	if decoded.Name != original.Name {
		t.Errorf("Name: got %q, want %q", decoded.Name, original.Name)
	}
	if decoded.Namespace != original.Namespace {
		t.Errorf("Namespace: got %q, want %q", decoded.Namespace, original.Namespace)
	}
	if decoded.SuperclassName != original.SuperclassName {
		t.Errorf("SuperclassName: got %q, want %q", decoded.SuperclassName, original.SuperclassName)
	}
	if len(decoded.InstVars) != len(original.InstVars) {
		t.Errorf("InstVars length: got %d, want %d", len(decoded.InstVars), len(original.InstVars))
	} else {
		for i, v := range decoded.InstVars {
			if v != original.InstVars[i] {
				t.Errorf("InstVars[%d]: got %q, want %q", i, v, original.InstVars[i])
			}
		}
	}
	if len(decoded.ClassVars) != len(original.ClassVars) {
		t.Errorf("ClassVars length: got %d, want %d", len(decoded.ClassVars), len(original.ClassVars))
	} else {
		for i, v := range decoded.ClassVars {
			if v != original.ClassVars[i] {
				t.Errorf("ClassVars[%d]: got %q, want %q", i, v, original.ClassVars[i])
			}
		}
	}
	if decoded.DocString != original.DocString {
		t.Errorf("DocString: got %q, want %q", decoded.DocString, original.DocString)
	}
}

func TestEncodeDecodeClassContentMinimal(t *testing.T) {
	original := &vm.ClassDigest{
		Name: "Foo",
	}

	encoded := EncodeClassContent(original)
	if encoded != "CLASS Foo" {
		t.Errorf("Encoded: got %q, want %q", encoded, "CLASS Foo")
	}

	decoded, err := DecodeClassContent(encoded)
	if err != nil {
		t.Fatalf("DecodeClassContent: %v", err)
	}
	if decoded.Name != "Foo" {
		t.Errorf("Name: got %q, want %q", decoded.Name, "Foo")
	}
	if decoded.Namespace != "" {
		t.Errorf("Namespace should be empty, got %q", decoded.Namespace)
	}
	if decoded.SuperclassName != "" {
		t.Errorf("SuperclassName should be empty, got %q", decoded.SuperclassName)
	}
	if len(decoded.InstVars) != 0 {
		t.Errorf("InstVars should be empty, got %v", decoded.InstVars)
	}
	if len(decoded.ClassVars) != 0 {
		t.Errorf("ClassVars should be empty, got %v", decoded.ClassVars)
	}
}

func TestDecodeClassContentMissingClassTag(t *testing.T) {
	_, err := DecodeClassContent("NAMESPACE MyApp\nSUPER Object")
	if err == nil {
		t.Error("Expected error for missing CLASS tag")
	}
}

func TestDecodeClassContentUnknownTag(t *testing.T) {
	_, err := DecodeClassContent("CLASS Foo\nUNKNOWN stuff")
	if err == nil {
		t.Error("Expected error for unknown tag")
	}
}

func TestDecodeClassContentMalformedLine(t *testing.T) {
	_, err := DecodeClassContent("CLASS Foo\nNAMESPACE")
	if err == nil {
		t.Error("Expected error for line without value")
	}
}

func TestEncodeDecodeClassContentWithNamespace(t *testing.T) {
	// Verify namespace with :: separator round-trips
	original := &vm.ClassDigest{
		Name:           "Button",
		Namespace:      "Yutani::Widgets",
		SuperclassName: "Widget",
	}

	encoded := EncodeClassContent(original)
	decoded, err := DecodeClassContent(encoded)
	if err != nil {
		t.Fatalf("DecodeClassContent: %v", err)
	}
	if decoded.Namespace != "Yutani::Widgets" {
		t.Errorf("Namespace: got %q, want %q", decoded.Namespace, "Yutani::Widgets")
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
