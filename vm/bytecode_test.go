package vm

import (
	"math"
	"strings"
	"testing"
)

// ---------------------------------------------------------------------------
// Opcode metadata tests
// ---------------------------------------------------------------------------

func TestOpcodeInfo(t *testing.T) {
	tests := []struct {
		op           Opcode
		name         string
		operandBytes int
	}{
		{OpNOP, "NOP", 0},
		{OpPOP, "POP", 0},
		{OpDUP, "DUP", 0},
		{OpPushNil, "PUSH_NIL", 0},
		{OpPushTrue, "PUSH_TRUE", 0},
		{OpPushFalse, "PUSH_FALSE", 0},
		{OpPushSelf, "PUSH_SELF", 0},
		{OpPushInt8, "PUSH_INT8", 1},
		{OpPushInt32, "PUSH_INT32", 4},
		{OpPushLiteral, "PUSH_LITERAL", 2},
		{OpPushFloat, "PUSH_FLOAT", 8},
		{OpPushTemp, "PUSH_TEMP", 1},
		{OpPushIvar, "PUSH_IVAR", 1},
		{OpPushGlobal, "PUSH_GLOBAL", 2},
		{OpStoreTemp, "STORE_TEMP", 1},
		{OpStoreIvar, "STORE_IVAR", 1},
		{OpStoreGlobal, "STORE_GLOBAL", 2},
		{OpSend, "SEND", 3},
		{OpSendSuper, "SEND_SUPER", 3},
		{OpSendPlus, "SEND_PLUS", 0},
		{OpJump, "JUMP", 2},
		{OpJumpTrue, "JUMP_TRUE", 2},
		{OpJumpFalse, "JUMP_FALSE", 2},
		{OpReturnTop, "RETURN_TOP", 0},
		{OpReturnSelf, "RETURN_SELF", 0},
		{OpCreateBlock, "CREATE_BLOCK", 3},
		{OpCreateArray, "CREATE_ARRAY", 1},
		{OpCreateObject, "CREATE_OBJECT", 3},
	}

	for _, tt := range tests {
		info := tt.op.Info()
		if info.Name != tt.name {
			t.Errorf("%s: Name = %q, want %q", tt.op, info.Name, tt.name)
		}
		if info.OperandBytes != tt.operandBytes {
			t.Errorf("%s: OperandBytes = %d, want %d", tt.op, info.OperandBytes, tt.operandBytes)
		}
	}
}

func TestOpcodeString(t *testing.T) {
	if OpPushNil.String() != "PUSH_NIL" {
		t.Errorf("String() = %q, want %q", OpPushNil.String(), "PUSH_NIL")
	}
}

func TestUnknownOpcode(t *testing.T) {
	op := Opcode(0xFF)
	info := op.Info()
	if !strings.HasPrefix(info.Name, "UNKNOWN_") {
		t.Errorf("unknown opcode should have UNKNOWN_ prefix, got %q", info.Name)
	}
}

// ---------------------------------------------------------------------------
// BytecodeBuilder tests
// ---------------------------------------------------------------------------

func TestBytecodeBuilderEmit(t *testing.T) {
	b := NewBytecodeBuilder()
	b.Emit(OpNOP)
	b.Emit(OpPOP)
	b.Emit(OpDUP)

	bytes := b.Bytes()
	if len(bytes) != 3 {
		t.Fatalf("len = %d, want 3", len(bytes))
	}
	if Opcode(bytes[0]) != OpNOP {
		t.Error("byte 0 should be NOP")
	}
	if Opcode(bytes[1]) != OpPOP {
		t.Error("byte 1 should be POP")
	}
	if Opcode(bytes[2]) != OpDUP {
		t.Error("byte 2 should be DUP")
	}
}

func TestBytecodeBuilderEmitByte(t *testing.T) {
	b := NewBytecodeBuilder()
	b.EmitByte(OpPushTemp, 5)

	bytes := b.Bytes()
	if len(bytes) != 2 {
		t.Fatalf("len = %d, want 2", len(bytes))
	}
	if Opcode(bytes[0]) != OpPushTemp {
		t.Error("byte 0 should be PUSH_TEMP")
	}
	if bytes[1] != 5 {
		t.Errorf("operand = %d, want 5", bytes[1])
	}
}

func TestBytecodeBuilderEmitInt8(t *testing.T) {
	b := NewBytecodeBuilder()
	b.EmitInt8(OpPushInt8, -42)

	bytes := b.Bytes()
	if len(bytes) != 2 {
		t.Fatalf("len = %d, want 2", len(bytes))
	}
	if int8(bytes[1]) != -42 {
		t.Errorf("operand = %d, want -42", int8(bytes[1]))
	}
}

func TestBytecodeBuilderEmitUint16(t *testing.T) {
	b := NewBytecodeBuilder()
	b.EmitUint16(OpPushLiteral, 0x1234)

	bytes := b.Bytes()
	if len(bytes) != 3 {
		t.Fatalf("len = %d, want 3", len(bytes))
	}
	// Little-endian
	if bytes[1] != 0x34 || bytes[2] != 0x12 {
		t.Errorf("operand bytes = [%02X, %02X], want [34, 12]", bytes[1], bytes[2])
	}
}

func TestBytecodeBuilderEmitInt32(t *testing.T) {
	b := NewBytecodeBuilder()
	b.EmitInt32(OpPushInt32, 0x12345678)

	bytes := b.Bytes()
	if len(bytes) != 5 {
		t.Fatalf("len = %d, want 5", len(bytes))
	}
	// Little-endian
	expected := []byte{0x78, 0x56, 0x34, 0x12}
	for i, exp := range expected {
		if bytes[i+1] != exp {
			t.Errorf("byte %d = %02X, want %02X", i+1, bytes[i+1], exp)
		}
	}
}

func TestBytecodeBuilderEmitFloat64(t *testing.T) {
	b := NewBytecodeBuilder()
	b.EmitFloat64(OpPushFloat, 3.14159)

	bytes := b.Bytes()
	if len(bytes) != 9 {
		t.Fatalf("len = %d, want 9", len(bytes))
	}
}

func TestBytecodeBuilderEmitSend(t *testing.T) {
	b := NewBytecodeBuilder()
	b.EmitSend(OpSend, 100, 2)

	bytes := b.Bytes()
	if len(bytes) != 4 {
		t.Fatalf("len = %d, want 4", len(bytes))
	}
	if Opcode(bytes[0]) != OpSend {
		t.Error("byte 0 should be SEND")
	}
	// selector = 100 (little-endian)
	if bytes[1] != 100 || bytes[2] != 0 {
		t.Errorf("selector bytes = [%d, %d], want [100, 0]", bytes[1], bytes[2])
	}
	// argc = 2
	if bytes[3] != 2 {
		t.Errorf("argc = %d, want 2", bytes[3])
	}
}

func TestBytecodeBuilderLen(t *testing.T) {
	b := NewBytecodeBuilder()
	if b.Len() != 0 {
		t.Errorf("initial Len() = %d, want 0", b.Len())
	}
	b.Emit(OpNOP)
	b.Emit(OpPOP)
	if b.Len() != 2 {
		t.Errorf("Len() = %d, want 2", b.Len())
	}
}

// ---------------------------------------------------------------------------
// Label tests
// ---------------------------------------------------------------------------

func TestLabelForwardJump(t *testing.T) {
	b := NewBytecodeBuilder()
	label := b.NewLabel()

	// Emit a forward jump
	b.EmitJump(OpJumpFalse, label) // 3 bytes: op + 2 byte offset
	b.Emit(OpPushNil)              // 1 byte (position 3)
	b.Emit(OpPOP)                  // 1 byte (position 4)
	b.Mark(label)                  // Target position 5
	b.Emit(OpPushTrue)

	bytes := b.Bytes()
	// The jump offset should be 2 (from position 3 to position 5)
	offset := int16(bytes[1]) | (int16(bytes[2]) << 8)
	if offset != 2 {
		t.Errorf("forward jump offset = %d, want 2", offset)
	}
}

func TestLabelBackwardJump(t *testing.T) {
	b := NewBytecodeBuilder()
	label := b.NewLabel()

	b.Mark(label)                 // Target position 0
	b.Emit(OpPushNil)             // 1 byte (position 0)
	b.Emit(OpPOP)                 // 1 byte (position 1)
	b.EmitJump(OpJumpTrue, label) // 3 bytes at position 2

	bytes := b.Bytes()
	// The jump should go back 5 bytes (from position 5 to position 0)
	offset := int16(bytes[3]) | (int16(bytes[4]) << 8)
	if offset != -5 {
		t.Errorf("backward jump offset = %d, want -5", offset)
	}
}

func TestLabelDoubleMark(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("double mark should panic")
		}
	}()

	b := NewBytecodeBuilder()
	label := b.NewLabel()
	b.Mark(label)
	b.Mark(label) // Should panic
}

// ---------------------------------------------------------------------------
// BytecodeReader tests
// ---------------------------------------------------------------------------

func TestBytecodeReaderReadOpcode(t *testing.T) {
	bc := []byte{byte(OpPushNil), byte(OpPOP), byte(OpDUP)}
	r := NewBytecodeReader(bc)

	if !r.HasMore() {
		t.Error("HasMore should be true")
	}
	if r.ReadOpcode() != OpPushNil {
		t.Error("first opcode should be PUSH_NIL")
	}
	if r.ReadOpcode() != OpPOP {
		t.Error("second opcode should be POP")
	}
	if r.ReadOpcode() != OpDUP {
		t.Error("third opcode should be DUP")
	}
	if r.HasMore() {
		t.Error("HasMore should be false")
	}
}

func TestBytecodeReaderReadInt8(t *testing.T) {
	bc := []byte{byte(OpPushInt8), byte(-42 & 0xFF)}
	r := NewBytecodeReader(bc)

	r.ReadOpcode()
	v := r.ReadInt8()
	if v != -42 {
		t.Errorf("ReadInt8() = %d, want -42", v)
	}
}

func TestBytecodeReaderReadUint16(t *testing.T) {
	bc := []byte{byte(OpPushLiteral), 0x34, 0x12}
	r := NewBytecodeReader(bc)

	r.ReadOpcode()
	v := r.ReadUint16()
	if v != 0x1234 {
		t.Errorf("ReadUint16() = %04X, want 1234", v)
	}
}

func TestBytecodeReaderReadInt32(t *testing.T) {
	bc := []byte{byte(OpPushInt32), 0x78, 0x56, 0x34, 0x12}
	r := NewBytecodeReader(bc)

	r.ReadOpcode()
	v := r.ReadInt32()
	if v != 0x12345678 {
		t.Errorf("ReadInt32() = %08X, want 12345678", v)
	}
}

func TestBytecodeReaderReadFloat64(t *testing.T) {
	b := NewBytecodeBuilder()
	b.EmitFloat64(OpPushFloat, 3.14159)

	r := NewBytecodeReader(b.Bytes())
	r.ReadOpcode()
	v := r.ReadFloat64()
	if v != 3.14159 {
		t.Errorf("ReadFloat64() = %f, want 3.14159", v)
	}
}

func TestBytecodeReaderSeek(t *testing.T) {
	bc := []byte{byte(OpNOP), byte(OpPOP), byte(OpDUP)}
	r := NewBytecodeReader(bc)

	r.Seek(2)
	if r.Position() != 2 {
		t.Errorf("Position() = %d, want 2", r.Position())
	}
	if r.ReadOpcode() != OpDUP {
		t.Error("should read DUP")
	}
}

func TestBytecodeReaderSkip(t *testing.T) {
	bc := []byte{byte(OpNOP), byte(OpPOP), byte(OpDUP)}
	r := NewBytecodeReader(bc)

	r.Skip(2)
	if r.Position() != 2 {
		t.Errorf("Position() = %d, want 2", r.Position())
	}
}

// ---------------------------------------------------------------------------
// Disassembly tests
// ---------------------------------------------------------------------------

func TestDisassembleSimple(t *testing.T) {
	b := NewBytecodeBuilder()
	b.Emit(OpPushNil)
	b.Emit(OpPOP)
	b.Emit(OpReturnSelf)

	dis := Disassemble(b.Bytes())
	if !strings.Contains(dis, "PUSH_NIL") {
		t.Error("disassembly should contain PUSH_NIL")
	}
	if !strings.Contains(dis, "POP") {
		t.Error("disassembly should contain POP")
	}
	if !strings.Contains(dis, "RETURN_SELF") {
		t.Error("disassembly should contain RETURN_SELF")
	}
}

func TestDisassembleWithOperands(t *testing.T) {
	b := NewBytecodeBuilder()
	b.EmitInt8(OpPushInt8, 42)
	b.EmitUint16(OpPushLiteral, 100)

	dis := Disassemble(b.Bytes())
	if !strings.Contains(dis, "PUSH_INT8 42") {
		t.Errorf("disassembly should contain 'PUSH_INT8 42', got:\n%s", dis)
	}
	if !strings.Contains(dis, "PUSH_LITERAL 100") {
		t.Errorf("disassembly should contain 'PUSH_LITERAL 100', got:\n%s", dis)
	}
}

func TestDisassembleJump(t *testing.T) {
	b := NewBytecodeBuilder()
	label := b.NewLabel()
	b.EmitJump(OpJumpFalse, label)
	b.Emit(OpPushNil)
	b.Mark(label)
	b.Emit(OpReturnTop)

	dis := Disassemble(b.Bytes())
	if !strings.Contains(dis, "JUMP_FALSE") {
		t.Error("disassembly should contain JUMP_FALSE")
	}
	if !strings.Contains(dis, "->") {
		t.Error("disassembly should contain jump target")
	}
}

func TestDisassembleSend(t *testing.T) {
	b := NewBytecodeBuilder()
	b.EmitSend(OpSend, 50, 3)

	dis := Disassemble(b.Bytes())
	if !strings.Contains(dis, "SEND") {
		t.Error("disassembly should contain SEND")
	}
	if !strings.Contains(dis, "selector=50") {
		t.Errorf("disassembly should contain 'selector=50', got:\n%s", dis)
	}
	if !strings.Contains(dis, "argc=3") {
		t.Errorf("disassembly should contain 'argc=3', got:\n%s", dis)
	}
}

func TestDisassembleFloat(t *testing.T) {
	b := NewBytecodeBuilder()
	b.EmitFloat64(OpPushFloat, 3.14159)

	dis := Disassemble(b.Bytes())
	if !strings.Contains(dis, "PUSH_FLOAT") {
		t.Error("disassembly should contain PUSH_FLOAT")
	}
	if !strings.Contains(dis, "3.14") {
		t.Errorf("disassembly should contain float value, got:\n%s", dis)
	}
}

// ---------------------------------------------------------------------------
// Integration tests: build and read
// ---------------------------------------------------------------------------

func TestBuildAndReadComplete(t *testing.T) {
	b := NewBytecodeBuilder()

	// Build a simple method: push 1, push 2, add, return
	b.Emit(OpPushSelf)                // push receiver
	b.EmitInt8(OpPushInt8, 1)         // push 1
	b.EmitInt8(OpPushInt8, 2)         // push 2
	b.Emit(OpSendPlus)                // send +
	b.Emit(OpReturnTop)               // return result

	// Read it back
	r := NewBytecodeReader(b.Bytes())

	if r.ReadOpcode() != OpPushSelf {
		t.Error("expected PUSH_SELF")
	}

	if r.ReadOpcode() != OpPushInt8 {
		t.Error("expected PUSH_INT8")
	}
	if r.ReadInt8() != 1 {
		t.Error("expected 1")
	}

	if r.ReadOpcode() != OpPushInt8 {
		t.Error("expected PUSH_INT8")
	}
	if r.ReadInt8() != 2 {
		t.Error("expected 2")
	}

	if r.ReadOpcode() != OpSendPlus {
		t.Error("expected SEND_PLUS")
	}

	if r.ReadOpcode() != OpReturnTop {
		t.Error("expected RETURN_TOP")
	}

	if r.HasMore() {
		t.Error("should have no more bytes")
	}
}

func TestBuildConditional(t *testing.T) {
	b := NewBytecodeBuilder()
	elseLabel := b.NewLabel()
	endLabel := b.NewLabel()

	// if condition then trueValue else falseValue
	b.EmitByte(OpPushTemp, 0)        // push condition
	b.EmitJump(OpJumpFalse, elseLabel)
	b.Emit(OpPushTrue)               // true branch
	b.EmitJump(OpJump, endLabel)
	b.Mark(elseLabel)
	b.Emit(OpPushFalse)              // false branch
	b.Mark(endLabel)
	b.Emit(OpReturnTop)

	// Verify we can disassemble it
	dis := Disassemble(b.Bytes())
	if !strings.Contains(dis, "JUMP_FALSE") {
		t.Error("should contain conditional jump")
	}
	if !strings.Contains(dis, "JUMP ") {
		t.Error("should contain unconditional jump")
	}
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

func BenchmarkBytecodeBuilderEmit(b *testing.B) {
	for i := 0; i < b.N; i++ {
		bb := NewBytecodeBuilder()
		bb.Emit(OpPushNil)
		bb.Emit(OpPOP)
		bb.Emit(OpReturnSelf)
	}
}

func BenchmarkBytecodeBuilderComplete(b *testing.B) {
	for i := 0; i < b.N; i++ {
		bb := NewBytecodeBuilder()
		bb.EmitByte(OpPushTemp, 0)
		bb.EmitByte(OpPushIvar, 1)
		bb.Emit(OpSendPlus)
		bb.EmitSend(OpSend, 100, 2)
		bb.Emit(OpReturnTop)
	}
}

func BenchmarkBytecodeReaderRead(b *testing.B) {
	bb := NewBytecodeBuilder()
	for i := 0; i < 100; i++ {
		bb.Emit(OpPushNil)
		bb.EmitByte(OpPushTemp, byte(i%256))
	}
	bc := bb.Bytes()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		r := NewBytecodeReader(bc)
		for r.HasMore() {
			op := r.ReadOpcode()
			r.Skip(op.OperandBytes())
		}
	}
}

func BenchmarkDisassemble(b *testing.B) {
	bb := NewBytecodeBuilder()
	bb.EmitByte(OpPushTemp, 0)
	bb.EmitInt8(OpPushInt8, 42)
	bb.Emit(OpSendPlus)
	bb.EmitSend(OpSend, 100, 2)
	bb.Emit(OpReturnTop)
	bc := bb.Bytes()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		_ = Disassemble(bc)
	}
}

// ---------------------------------------------------------------------------
// Edge cases
// ---------------------------------------------------------------------------

func TestBytecodeReaderUnderflow(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("underflow should panic")
		}
	}()

	r := NewBytecodeReader([]byte{})
	r.ReadOpcode() // Should panic
}

func TestFloat64RoundTrip(t *testing.T) {
	values := []float64{0, 1, -1, 3.14159, math.Pi, math.E, math.Inf(1), math.Inf(-1)}
	for _, v := range values {
		b := NewBytecodeBuilder()
		b.EmitFloat64(OpPushFloat, v)

		r := NewBytecodeReader(b.Bytes())
		r.ReadOpcode()
		got := r.ReadFloat64()

		if got != v && !(math.IsNaN(v) && math.IsNaN(got)) {
			t.Errorf("float64 round-trip failed: %f != %f", got, v)
		}
	}
}

func TestNegativeInt32(t *testing.T) {
	b := NewBytecodeBuilder()
	b.EmitInt32(OpPushInt32, -1000000)

	r := NewBytecodeReader(b.Bytes())
	r.ReadOpcode()
	v := r.ReadInt32()

	if v != -1000000 {
		t.Errorf("negative int32 = %d, want -1000000", v)
	}
}
