package vm

import (
	"strings"
	"testing"
)

// ---------------------------------------------------------------------------
// CompiledMethod creation tests
// ---------------------------------------------------------------------------

func TestNewCompiledMethod(t *testing.T) {
	m := NewCompiledMethod("add:", 1)

	if m.Name() != "add:" {
		t.Errorf("Name() = %q, want %q", m.Name(), "add:")
	}
	if m.Arity != 1 {
		t.Errorf("Arity = %d, want 1", m.Arity)
	}
	if m.NumTemps != 1 {
		t.Errorf("NumTemps = %d, want 1", m.NumTemps)
	}
	if m.LiteralCount() != 0 {
		t.Errorf("LiteralCount() = %d, want 0", m.LiteralCount())
	}
}

func TestCompiledMethodSelector(t *testing.T) {
	m := NewCompiledMethod("at:put:", 2)
	m.SetSelector(42)

	if m.Selector() != 42 {
		t.Errorf("Selector() = %d, want 42", m.Selector())
	}
}

func TestCompiledMethodClass(t *testing.T) {
	m := NewCompiledMethod("size", 0)
	c := NewClass("Array", nil)
	m.SetClass(c)

	if m.Class() != c {
		t.Error("Class() should return the set class")
	}
}

// ---------------------------------------------------------------------------
// Literal tests
// ---------------------------------------------------------------------------

func TestCompiledMethodLiterals(t *testing.T) {
	m := NewCompiledMethod("test", 0)
	m.Literals = []Value{FromSmallInt(42), FromFloat64(3.14), True}

	if m.LiteralCount() != 3 {
		t.Errorf("LiteralCount() = %d, want 3", m.LiteralCount())
	}

	if m.GetLiteral(0).SmallInt() != 42 {
		t.Error("literal 0 should be 42")
	}
	if m.GetLiteral(1).Float64() != 3.14 {
		t.Error("literal 1 should be 3.14")
	}
	if m.GetLiteral(2) != True {
		t.Error("literal 2 should be true")
	}
}

func TestCompiledMethodLiteralPanic(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("GetLiteral(-1) should panic")
		}
	}()

	m := NewCompiledMethod("test", 0)
	m.GetLiteral(-1)
}

// ---------------------------------------------------------------------------
// Block tests
// ---------------------------------------------------------------------------

func TestCompiledMethodBlocks(t *testing.T) {
	m := NewCompiledMethod("test", 0)
	b1 := NewBlockMethod(0)
	b2 := NewBlockMethod(1)

	m.Blocks = []*BlockMethod{b1, b2}

	if m.BlockCount() != 2 {
		t.Errorf("BlockCount() = %d, want 2", m.BlockCount())
	}
	if m.GetBlock(0) != b1 {
		t.Error("GetBlock(0) should return b1")
	}
	if m.GetBlock(1) != b2 {
		t.Error("GetBlock(1) should return b2")
	}
}

func TestCompiledMethodBlockPanic(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("GetBlock(0) should panic for empty blocks")
		}
	}()

	m := NewCompiledMethod("test", 0)
	m.GetBlock(0)
}

// ---------------------------------------------------------------------------
// Source mapping tests
// ---------------------------------------------------------------------------

func TestSourceLocation(t *testing.T) {
	m := NewCompiledMethod("test", 0)
	m.AddSourceLocation(0, 1, 1)
	m.AddSourceLocation(5, 2, 5)
	m.AddSourceLocation(10, 3, 1)

	// Exact match
	loc := m.SourceLocation(5)
	if loc == nil || loc.Line != 2 || loc.Column != 5 {
		t.Errorf("SourceLocation(5) = %v, want line 2 col 5", loc)
	}

	// Between entries
	loc = m.SourceLocation(7)
	if loc == nil || loc.Line != 2 || loc.Column != 5 {
		t.Errorf("SourceLocation(7) = %v, want line 2 col 5", loc)
	}

	// Before first entry
	loc = m.SourceLocation(0)
	if loc == nil || loc.Line != 1 {
		t.Errorf("SourceLocation(0) = %v, want line 1", loc)
	}
}

func TestSourceLocationEmpty(t *testing.T) {
	m := NewCompiledMethod("test", 0)
	loc := m.SourceLocation(5)
	if loc != nil {
		t.Error("SourceLocation should return nil for empty map")
	}
}

// ---------------------------------------------------------------------------
// Builder tests
// ---------------------------------------------------------------------------

func TestCompiledMethodBuilder(t *testing.T) {
	b := NewCompiledMethodBuilder("increment", 0)
	b.SetSource("increment\n\t^self + 1")
	b.SetNumTemps(1)

	// Add a literal
	litIdx := b.AddLiteral(FromSmallInt(1))
	if litIdx != 0 {
		t.Errorf("AddLiteral returned %d, want 0", litIdx)
	}

	// Emit some bytecode
	bc := b.Bytecode()
	b.MarkSource(1, 1)
	bc.Emit(OpPushSelf)
	b.MarkSource(2, 2)
	bc.EmitUint16(OpPushLiteral, 0)
	bc.Emit(OpSendPlus)
	bc.Emit(OpReturnTop)

	m := b.Build()

	if m.Name() != "increment" {
		t.Errorf("Name() = %q, want %q", m.Name(), "increment")
	}
	if m.NumTemps != 1 {
		t.Errorf("NumTemps = %d, want 1", m.NumTemps)
	}
	if len(m.Bytecode) != 6 {
		t.Errorf("Bytecode length = %d, want 6", len(m.Bytecode))
	}
	if m.LiteralCount() != 1 {
		t.Errorf("LiteralCount() = %d, want 1", m.LiteralCount())
	}
	if m.Source != "increment\n\t^self + 1" {
		t.Error("Source not set correctly")
	}
	if len(m.SourceMap) != 2 {
		t.Errorf("SourceMap length = %d, want 2", len(m.SourceMap))
	}
}

func TestCompiledMethodBuilderAddLocal(t *testing.T) {
	b := NewCompiledMethodBuilder("test", 1) // 1 argument

	idx1 := b.AddLocal()
	idx2 := b.AddLocal()

	m := b.Build()

	if idx1 != 1 {
		t.Errorf("first local index = %d, want 1", idx1)
	}
	if idx2 != 2 {
		t.Errorf("second local index = %d, want 2", idx2)
	}
	if m.NumTemps != 3 {
		t.Errorf("NumTemps = %d, want 3", m.NumTemps)
	}
}

func TestCompiledMethodBuilderWithBlock(t *testing.T) {
	// Build a method with a nested block
	mb := NewCompiledMethodBuilder("collect:", 1)

	// Build the block
	bb := NewBlockMethodBuilder(1)
	bb.Bytecode().Emit(OpPushSelf)
	bb.Bytecode().Emit(OpReturnTop)
	block := bb.Build()

	// Add block to method
	blockIdx := mb.AddBlock(block)
	if blockIdx != 0 {
		t.Errorf("AddBlock returned %d, want 0", blockIdx)
	}

	mb.Bytecode().EmitCreateBlock(0, 0)
	m := mb.Build()

	if m.BlockCount() != 1 {
		t.Errorf("BlockCount() = %d, want 1", m.BlockCount())
	}
	if m.GetBlock(0).Outer != m {
		t.Error("block's Outer should point to the method")
	}
}

// ---------------------------------------------------------------------------
// BlockMethod tests
// ---------------------------------------------------------------------------

func TestNewBlockMethod(t *testing.T) {
	b := NewBlockMethod(2)

	if b.Arity != 2 {
		t.Errorf("Arity = %d, want 2", b.Arity)
	}
	if b.NumTemps != 2 {
		t.Errorf("NumTemps = %d, want 2", b.NumTemps)
	}
}

func TestBlockMethodLiterals(t *testing.T) {
	b := NewBlockMethod(0)
	b.Literals = []Value{FromSmallInt(99)}

	if b.LiteralCount() != 1 {
		t.Errorf("LiteralCount() = %d, want 1", b.LiteralCount())
	}
	if b.GetLiteral(0).SmallInt() != 99 {
		t.Error("literal should be 99")
	}
}

func TestBlockMethodBuilder(t *testing.T) {
	b := NewBlockMethodBuilder(1)
	b.SetNumTemps(2)
	b.SetNumCaptures(1)

	bc := b.Bytecode()
	bc.EmitByte(OpPushCaptured, 0)
	bc.EmitByte(OpPushTemp, 0)
	bc.Emit(OpSendPlus)
	bc.Emit(OpReturnTop)

	block := b.Build()

	if block.Arity != 1 {
		t.Errorf("Arity = %d, want 1", block.Arity)
	}
	if block.NumTemps != 2 {
		t.Errorf("NumTemps = %d, want 2", block.NumTemps)
	}
	if block.NumCaptures != 1 {
		t.Errorf("NumCaptures = %d, want 1", block.NumCaptures)
	}
	if len(block.Bytecode) != 6 {
		t.Errorf("Bytecode length = %d, want 6", len(block.Bytecode))
	}
}

// ---------------------------------------------------------------------------
// Disassembly tests
// ---------------------------------------------------------------------------

func TestCompiledMethodDisassemble(t *testing.T) {
	b := NewCompiledMethodBuilder("test", 0)
	bc := b.Bytecode()
	bc.Emit(OpPushNil)
	bc.Emit(OpReturnTop)
	m := b.Build()

	dis := m.Disassemble()
	if !strings.Contains(dis, "PUSH_NIL") {
		t.Error("disassembly should contain PUSH_NIL")
	}
	if !strings.Contains(dis, "RETURN_TOP") {
		t.Error("disassembly should contain RETURN_TOP")
	}
}

func TestCompiledMethodString(t *testing.T) {
	m := NewCompiledMethod("at:put:", 2)
	c := NewClass("Dictionary", nil)
	m.SetClass(c)

	s := m.String()
	if s != "Dictionary>>at:put:" {
		t.Errorf("String() = %q, want %q", s, "Dictionary>>at:put:")
	}
}

func TestCompiledMethodStringNoClass(t *testing.T) {
	m := NewCompiledMethod("test", 0)
	s := m.String()
	if s != "?>>test" {
		t.Errorf("String() = %q, want %q", s, "?>>test")
	}
}

// ---------------------------------------------------------------------------
// Method interface tests
// ---------------------------------------------------------------------------

func TestCompiledMethodImplementsMethod(t *testing.T) {
	m := NewCompiledMethod("test", 0)

	// Verify it implements Method interface
	var method Method = m
	_ = method

	// Name() should work
	if method.(interface{ Name() string }).Name() != "test" {
		t.Error("Name() via interface failed")
	}
}

func TestCompiledMethodInvokePanic(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("Invoke should panic")
		}
	}()

	m := NewCompiledMethod("test", 0)
	m.Invoke(nil, Nil, nil)
}

// ---------------------------------------------------------------------------
// Integration test: build a realistic method
// ---------------------------------------------------------------------------

func TestBuildIncrementMethod(t *testing.T) {
	// Build the "increment" method from the design doc:
	// increment
	//     | newValue |
	//     newValue := value + 1.
	//     value := newValue.
	//     ^newValue

	b := NewCompiledMethodBuilder("increment", 0)
	b.SetSource("increment\n\t| newValue |\n\tnewValue := value + 1.\n\tvalue := newValue.\n\t^newValue")
	localIdx := b.AddLocal() // newValue

	bc := b.Bytecode()
	b.MarkSource(3, 2) // newValue := value + 1
	bc.EmitByte(OpPushIvar, 0)    // push 'value' (assuming it's ivar 0)
	bc.EmitInt8(OpPushInt8, 1)    // push 1
	bc.Emit(OpSendPlus)           // value + 1
	bc.EmitByte(OpStoreTemp, byte(localIdx)) // newValue := ...

	b.MarkSource(4, 2) // value := newValue
	bc.EmitByte(OpPushTemp, byte(localIdx)) // push newValue
	bc.EmitByte(OpStoreIvar, 0)             // value := newValue

	b.MarkSource(5, 2) // ^newValue
	bc.EmitByte(OpPushTemp, byte(localIdx)) // push newValue
	bc.Emit(OpReturnTop)                    // ^newValue

	m := b.Build()

	// Verify structure
	if m.Arity != 0 {
		t.Errorf("Arity = %d, want 0", m.Arity)
	}
	if m.NumTemps != 1 {
		t.Errorf("NumTemps = %d, want 1", m.NumTemps)
	}

	// Disassemble and verify
	dis := m.Disassemble()
	expectedOps := []string{"PUSH_IVAR", "PUSH_INT8", "SEND_PLUS", "STORE_TEMP", "PUSH_TEMP", "STORE_IVAR", "PUSH_TEMP", "RETURN_TOP"}
	for _, op := range expectedOps {
		if !strings.Contains(dis, op) {
			t.Errorf("disassembly missing %s:\n%s", op, dis)
		}
	}

	// Check source mapping
	loc := m.SourceLocation(0) // First instruction
	if loc == nil || loc.Line != 3 {
		t.Errorf("SourceLocation(0) = %v, want line 3", loc)
	}
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

func BenchmarkNewCompiledMethod(b *testing.B) {
	for i := 0; i < b.N; i++ {
		_ = NewCompiledMethod("test", 2)
	}
}

func BenchmarkCompiledMethodBuilder(b *testing.B) {
	for i := 0; i < b.N; i++ {
		mb := NewCompiledMethodBuilder("test", 1)
		mb.AddLiteral(FromSmallInt(42))
		bc := mb.Bytecode()
		bc.Emit(OpPushSelf)
		bc.EmitByte(OpPushTemp, 0)
		bc.Emit(OpSendPlus)
		bc.Emit(OpReturnTop)
		_ = mb.Build()
	}
}

func BenchmarkGetLiteral(b *testing.B) {
	m := NewCompiledMethod("test", 0)
	m.Literals = []Value{FromSmallInt(1), FromSmallInt(2), FromSmallInt(3)}
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		_ = m.GetLiteral(1)
	}
}

func BenchmarkSourceLocation(b *testing.B) {
	m := NewCompiledMethod("test", 0)
	for i := 0; i < 100; i++ {
		m.AddSourceLocation(i*5, i+1, 1)
	}
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		_ = m.SourceLocation(250)
	}
}
