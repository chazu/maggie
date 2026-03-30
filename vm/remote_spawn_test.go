package vm

import (
	"testing"
)

// ---------------------------------------------------------------------------
// SpawnBlock serialization round-trip tests
// ---------------------------------------------------------------------------

func TestSerializeBlock_RoundTrip(t *testing.T) {
	v := NewVM()

	// Create a simple block with captures
	block := &BlockMethod{
		Arity:       0,
		NumTemps:    1,
		NumCaptures: 2,
		Bytecode:    []byte{0x01, 0x02, 0x03},
	}

	// Create a compiled method with a content hash and attach the block
	method := &CompiledMethod{
		Bytecode: []byte{0x10, 0x20},
	}
	method.SetContentHash([32]byte{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32})
	method.Blocks = []*BlockMethod{block}
	block.Outer = method

	// Index in content store
	cs := NewContentStore()
	cs.IndexMethod(method)
	v.SetContentStore(cs)

	// Build a BlockValue
	bv := &BlockValue{
		Block:      block,
		Captures:   []Value{FromSmallInt(42), v.registry.NewStringValue("hello")},
		HomeMethod: method,
	}

	// Serialize
	data, err := v.SerializeBlock(bv, Nil, "fork")
	if err != nil {
		t.Fatalf("SerializeBlock: %v", err)
	}

	// Deserialize
	sb, err := DeserializeSpawnBlock(data)
	if err != nil {
		t.Fatalf("DeserializeSpawnBlock: %v", err)
	}

	if sb.MethodHash != method.GetContentHash() {
		t.Errorf("method hash mismatch")
	}
	if sb.ArgCount != 0 {
		t.Errorf("arg count: got %d, want 0", sb.ArgCount)
	}
	if sb.NumCaptures != 2 {
		t.Errorf("num captures: got %d, want 2", sb.NumCaptures)
	}
	if len(sb.Upvalues) != 2 {
		t.Errorf("upvalues length: got %d, want 2", len(sb.Upvalues))
	}
	if sb.SpawnMode != "fork" {
		t.Errorf("spawn mode: got %q, want %q", sb.SpawnMode, "fork")
	}

	// Deserialize upvalues
	captures, err := v.DeserializeUpvalues(sb)
	if err != nil {
		t.Fatalf("DeserializeUpvalues: %v", err)
	}
	if len(captures) != 2 {
		t.Fatalf("captures length: got %d, want 2", len(captures))
	}
	if captures[0].SmallInt() != 42 {
		t.Errorf("capture 0: got %d, want 42", captures[0].SmallInt())
	}
	if v.registry.GetStringContent(captures[1]) != "hello" {
		t.Errorf("capture 1: got %q, want %q", v.registry.GetStringContent(captures[1]), "hello")
	}
}

func TestSerializeBlock_WithArg(t *testing.T) {
	v := NewVM()

	block := &BlockMethod{
		Arity:       1,
		NumTemps:    2,
		NumCaptures: 0,
	}
	method := &CompiledMethod{Bytecode: []byte{0x01}}
	method.SetContentHash([32]byte{10, 20, 30})
	method.Blocks = []*BlockMethod{block}
	block.Outer = method

	bv := &BlockValue{
		Block:      block,
		Captures:   nil,
		HomeMethod: method,
	}

	data, err := v.SerializeBlock(bv, FromSmallInt(99), "spawn")
	if err != nil {
		t.Fatalf("SerializeBlock with arg: %v", err)
	}

	sb, err := DeserializeSpawnBlock(data)
	if err != nil {
		t.Fatalf("DeserializeSpawnBlock: %v", err)
	}

	if sb.ArgCount != 1 {
		t.Errorf("arg count: got %d, want 1", sb.ArgCount)
	}
	if sb.SpawnMode != "spawn" {
		t.Errorf("spawn mode: got %q, want %q", sb.SpawnMode, "spawn")
	}

	arg, err := v.DeserializeSpawnArg(sb)
	if err != nil {
		t.Fatalf("DeserializeSpawnArg: %v", err)
	}
	if arg.SmallInt() != 99 {
		t.Errorf("arg: got %d, want 99", arg.SmallInt())
	}
}

func TestSerializeBlock_NonSerializableCapture(t *testing.T) {
	v := NewVM()

	block := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 1,
	}
	method := &CompiledMethod{Bytecode: []byte{0x01}}
	method.SetContentHash([32]byte{1, 2, 3})
	method.Blocks = []*BlockMethod{block}
	block.Outer = method

	// Create a mutex — non-serializable
	mutexVal := FromSymbolID(1 | mutexMarker)

	bv := &BlockValue{
		Block:      block,
		Captures:   []Value{mutexVal},
		HomeMethod: method,
	}

	_, err := v.SerializeBlock(bv, Nil, "fork")
	if err == nil {
		t.Error("expected error for non-serializable Mutex capture")
	}
}

func TestSerializeBlock_NoContentHash(t *testing.T) {
	v := NewVM()

	block := &BlockMethod{Arity: 0}
	method := &CompiledMethod{Bytecode: []byte{0x01}}
	// No content hash set
	method.Blocks = []*BlockMethod{block}
	block.Outer = method

	bv := &BlockValue{
		Block:      block,
		HomeMethod: method,
	}

	_, err := v.SerializeBlock(bv, Nil, "fork")
	if err == nil {
		t.Error("expected error for method without content hash")
	}
}

// ---------------------------------------------------------------------------
// Block method resolution tests
// ---------------------------------------------------------------------------

func TestResolveBlockMethod_Found(t *testing.T) {
	v := NewVM()

	block := &BlockMethod{
		Arity:       0,
		NumTemps:    1,
		NumCaptures: 0,
		Bytecode:    []byte{0x01},
	}
	method := &CompiledMethod{Bytecode: []byte{0x10}}
	hash := [32]byte{5, 10, 15, 20}
	method.SetContentHash(hash)
	method.Blocks = []*BlockMethod{block}

	cs := NewContentStore()
	cs.IndexMethod(method)
	v.SetContentStore(cs)

	sb := &SpawnBlock{
		MethodHash:  hash,
		ArgCount:    0,
		NumCaptures: 0,
	}

	found, homeMethod, err := v.ResolveBlockMethod(sb)
	if err != nil {
		t.Fatalf("ResolveBlockMethod: %v", err)
	}
	if found == nil {
		t.Fatal("expected to find block")
	}
	if homeMethod != method {
		t.Error("home method mismatch")
	}
}

func TestResolveBlockMethod_NotFound(t *testing.T) {
	v := NewVM()

	cs := NewContentStore()
	v.SetContentStore(cs)

	sb := &SpawnBlock{
		MethodHash: [32]byte{99, 99, 99},
	}

	found, _, err := v.ResolveBlockMethod(sb)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if found != nil {
		t.Error("expected nil for missing method")
	}
}

// ---------------------------------------------------------------------------
// Pending spawn registry tests
// ---------------------------------------------------------------------------

func TestPendingSpawnRegistry(t *testing.T) {
	reg := newPendingSpawnRegistry()

	f1 := NewFuture()
	f2 := NewFuture()

	id1 := reg.register(f1)
	id2 := reg.register(f2)

	if id1 == id2 {
		t.Error("IDs should be unique")
	}

	// Resolve f1
	got := reg.resolve(id1)
	if got != f1 {
		t.Error("should get back f1")
	}

	// Resolve again — should return nil (already consumed)
	got = reg.resolve(id1)
	if got != nil {
		t.Error("second resolve should return nil")
	}

	// f2 still available
	got = reg.resolve(id2)
	if got != f2 {
		t.Error("should get back f2")
	}
}

// ---------------------------------------------------------------------------
// DeserializeSpawnBlock error cases
// ---------------------------------------------------------------------------

func TestDeserializeSpawnBlock_InvalidData(t *testing.T) {
	_, err := DeserializeSpawnBlock([]byte{0xFF})
	if err == nil {
		t.Error("expected error for invalid CBOR data")
	}
}

func TestDeserializeSpawnBlock_WrongTag(t *testing.T) {
	// Encode a CBOR tag with wrong number
	data, _ := cborSerialEncMode.Marshal("not a spawn block")
	_, err := DeserializeSpawnBlock(data)
	if err == nil {
		t.Error("expected error for non-tagged data")
	}
}

func TestSerializeBlock_NilBlock(t *testing.T) {
	v := NewVM()
	_, err := v.SerializeBlock(nil, Nil, "fork")
	if err == nil {
		t.Error("expected error for nil block")
	}
}
