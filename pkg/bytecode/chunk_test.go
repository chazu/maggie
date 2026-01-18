package bytecode

import (
	"bytes"
	"testing"
)

func TestNewChunk(t *testing.T) {
	c := NewChunk()

	if c.Version != BytecodeVersion {
		t.Errorf("Version = %d, want %d", c.Version, BytecodeVersion)
	}
	if c.Code == nil {
		t.Error("Code is nil")
	}
	if c.Constants == nil {
		t.Error("Constants is nil")
	}
}

func TestChunkAddConstant(t *testing.T) {
	c := NewChunk()

	// Add first constant
	idx0 := c.AddConstant("hello")
	if idx0 != 0 {
		t.Errorf("First constant index = %d, want 0", idx0)
	}

	// Add second constant
	idx1 := c.AddConstant("world")
	if idx1 != 1 {
		t.Errorf("Second constant index = %d, want 1", idx1)
	}

	// Add duplicate - should return existing index
	idx2 := c.AddConstant("hello")
	if idx2 != 0 {
		t.Errorf("Duplicate constant index = %d, want 0", idx2)
	}

	// Verify count
	if c.ConstantCount() != 2 {
		t.Errorf("ConstantCount() = %d, want 2", c.ConstantCount())
	}

	// Verify retrieval
	if c.GetConstant(0) != "hello" {
		t.Errorf("GetConstant(0) = %q, want %q", c.GetConstant(0), "hello")
	}
	if c.GetConstant(1) != "world" {
		t.Errorf("GetConstant(1) = %q, want %q", c.GetConstant(1), "world")
	}
}

func TestChunkEmit(t *testing.T) {
	c := NewChunk()

	// Emit simple opcode
	off0 := c.Emit(OpNop)
	if off0 != 0 {
		t.Errorf("First emit offset = %d, want 0", off0)
	}

	off1 := c.Emit(OpReturn)
	if off1 != 1 {
		t.Errorf("Second emit offset = %d, want 1", off1)
	}

	if c.CodeLen() != 2 {
		t.Errorf("CodeLen() = %d, want 2", c.CodeLen())
	}

	if Opcode(c.Code[0]) != OpNop {
		t.Errorf("Code[0] = 0x%02X, want OpNop", c.Code[0])
	}
	if Opcode(c.Code[1]) != OpReturn {
		t.Errorf("Code[1] = 0x%02X, want OpReturn", c.Code[1])
	}
}

func TestChunkEmitWithOperand(t *testing.T) {
	c := NewChunk()

	// Emit opcode with operand
	off := c.EmitWithOperand(OpLoadLocal, 5)
	if off != 0 {
		t.Errorf("Emit offset = %d, want 0", off)
	}

	if c.CodeLen() != 2 {
		t.Errorf("CodeLen() = %d, want 2", c.CodeLen())
	}

	if Opcode(c.Code[0]) != OpLoadLocal {
		t.Errorf("Code[0] = 0x%02X, want OpLoadLocal", c.Code[0])
	}
	if c.Code[1] != 5 {
		t.Errorf("Code[1] = %d, want 5", c.Code[1])
	}
}

func TestChunkEmitConstant(t *testing.T) {
	c := NewChunk()

	off := c.EmitConstant("test")

	if off != 0 {
		t.Errorf("Emit offset = %d, want 0", off)
	}

	// Should emit OpConst + 2-byte index
	if c.CodeLen() != 3 {
		t.Errorf("CodeLen() = %d, want 3", c.CodeLen())
	}

	if Opcode(c.Code[0]) != OpConst {
		t.Errorf("Code[0] = 0x%02X, want OpConst", c.Code[0])
	}

	// Index should be 0 (big endian)
	if c.Code[1] != 0 || c.Code[2] != 0 {
		t.Errorf("Constant index = %d,%d, want 0,0", c.Code[1], c.Code[2])
	}

	// Constant should be stored
	if c.GetConstant(0) != "test" {
		t.Errorf("Constant = %q, want %q", c.GetConstant(0), "test")
	}
}

func TestChunkJumpPatch(t *testing.T) {
	c := NewChunk()

	// Emit some code
	c.Emit(OpConstZero) // offset 0, 1 byte

	// Emit jump with placeholder
	placeholderOff := c.EmitJump(OpJumpFalse) // offset 1-3, returns 2 (placeholder offset)

	// Emit body
	c.Emit(OpConstOne) // offset 4, 1 byte
	c.Emit(OpPop)      // offset 5, 1 byte

	// Patch jump to current position (offset 6)
	c.PatchJump(placeholderOff)

	// Emit more code
	c.Emit(OpReturn) // offset 6, 1 byte

	// Verify jump target
	// placeholderOff = 2
	// jumpFrom = placeholderOff + 2 = 4 (after the 2-byte offset)
	// jumpTo = 6 (len when PatchJump was called)
	// delta = 6 - 4 = 2
	delta := int16(c.Code[placeholderOff])<<8 | int16(c.Code[placeholderOff+1])

	if delta != 2 {
		t.Errorf("Jump delta = %d, want 2", delta)
	}
}

func TestChunkEmitLoop(t *testing.T) {
	c := NewChunk()

	// Loop start
	loopStart := c.CurrentOffset()
	c.Emit(OpConstZero)

	// Loop body
	c.Emit(OpConstOne)
	c.Emit(OpAdd)

	// Back edge
	c.EmitLoop(loopStart)

	// Verify backward jump
	// EmitLoop is at offset 3
	// Jump instruction is 3 bytes, so jump "from" is offset 6
	// Target is offset 0, so delta = 0 - 6 = -6
	jumpOffset := c.CodeLen() - 2 // Position of delta bytes
	delta := int16(c.Code[jumpOffset])<<8 | int16(c.Code[jumpOffset+1])

	if delta != -6 {
		t.Errorf("Loop delta = %d, want -6", delta)
	}
}

func TestChunkAddCapture(t *testing.T) {
	c := NewChunk()

	idx0 := c.AddCapture("x", VarSourceLocal, 0)
	idx1 := c.AddCapture("y", VarSourceIVar, 1)

	if idx0 != 0 {
		t.Errorf("First capture index = %d, want 0", idx0)
	}
	if idx1 != 1 {
		t.Errorf("Second capture index = %d, want 1", idx1)
	}
	if c.CaptureCount() != 2 {
		t.Errorf("CaptureCount() = %d, want 2", c.CaptureCount())
	}
	if c.Flags&ChunkFlagHasCaptures == 0 {
		t.Error("ChunkFlagHasCaptures not set")
	}
}

func TestChunkSourceLocation(t *testing.T) {
	c := NewChunk()

	c.AddSourceLocation(0, 10, 5)
	c.AddSourceLocation(5, 12, 10)

	if c.Flags&ChunkFlagDebug == 0 {
		t.Error("ChunkFlagDebug not set")
	}

	// Query locations
	line, col := c.GetSourceLocation(0)
	if line != 10 || col != 5 {
		t.Errorf("Location at 0 = (%d, %d), want (10, 5)", line, col)
	}

	line, col = c.GetSourceLocation(3)
	if line != 10 || col != 5 {
		t.Errorf("Location at 3 = (%d, %d), want (10, 5) (should use nearest before)", line, col)
	}

	line, col = c.GetSourceLocation(5)
	if line != 12 || col != 10 {
		t.Errorf("Location at 5 = (%d, %d), want (12, 10)", line, col)
	}
}

// ============================================================================
// Serialization Tests
// ============================================================================

func TestSerializeDeserializeEmpty(t *testing.T) {
	c := NewChunk()

	data, err := c.Serialize()
	if err != nil {
		t.Fatalf("Serialize error: %v", err)
	}

	// Check magic
	if !bytes.HasPrefix(data, BytecodeMagic) {
		t.Error("Serialized data missing magic header")
	}

	// Deserialize
	c2, err := Deserialize(data)
	if err != nil {
		t.Fatalf("Deserialize error: %v", err)
	}

	if c2.Version != c.Version {
		t.Errorf("Version mismatch: got %d, want %d", c2.Version, c.Version)
	}
	if c2.CodeLen() != 0 {
		t.Errorf("Code length: got %d, want 0", c2.CodeLen())
	}
}

func TestSerializeDeserializeWithCode(t *testing.T) {
	c := NewChunk()

	// Add some code
	c.Emit(OpConstZero)
	c.Emit(OpConstOne)
	c.Emit(OpAdd)
	c.Emit(OpReturn)

	data, err := c.Serialize()
	if err != nil {
		t.Fatalf("Serialize error: %v", err)
	}

	c2, err := Deserialize(data)
	if err != nil {
		t.Fatalf("Deserialize error: %v", err)
	}

	if c2.CodeLen() != c.CodeLen() {
		t.Errorf("Code length: got %d, want %d", c2.CodeLen(), c.CodeLen())
	}

	if !bytes.Equal(c2.Code, c.Code) {
		t.Error("Code mismatch")
	}
}

func TestSerializeDeserializeWithConstants(t *testing.T) {
	c := NewChunk()

	c.EmitConstant("hello")
	c.EmitConstant("world")
	c.EmitConstant("hello") // Duplicate
	c.Emit(OpReturn)

	data, err := c.Serialize()
	if err != nil {
		t.Fatalf("Serialize error: %v", err)
	}

	c2, err := Deserialize(data)
	if err != nil {
		t.Fatalf("Deserialize error: %v", err)
	}

	if c2.ConstantCount() != 2 {
		t.Errorf("Constant count: got %d, want 2", c2.ConstantCount())
	}

	if c2.GetConstant(0) != "hello" {
		t.Errorf("Constant 0: got %q, want %q", c2.GetConstant(0), "hello")
	}
	if c2.GetConstant(1) != "world" {
		t.Errorf("Constant 1: got %q, want %q", c2.GetConstant(1), "world")
	}
}

func TestSerializeDeserializeWithParams(t *testing.T) {
	c := NewChunk()
	c.ParamCount = 2
	c.ParamNames = []string{"x", "y"}

	c.EmitWithOperand(OpLoadParam, 0)
	c.EmitWithOperand(OpLoadParam, 1)
	c.Emit(OpAdd)
	c.Emit(OpReturn)

	data, err := c.Serialize()
	if err != nil {
		t.Fatalf("Serialize error: %v", err)
	}

	c2, err := Deserialize(data)
	if err != nil {
		t.Fatalf("Deserialize error: %v", err)
	}

	if c2.ParamCount != 2 {
		t.Errorf("ParamCount: got %d, want 2", c2.ParamCount)
	}
	if len(c2.ParamNames) != 2 {
		t.Errorf("ParamNames length: got %d, want 2", len(c2.ParamNames))
	}
	if c2.ParamNames[0] != "x" || c2.ParamNames[1] != "y" {
		t.Errorf("ParamNames: got %v, want [x y]", c2.ParamNames)
	}
}

func TestSerializeDeserializeWithCaptures(t *testing.T) {
	c := NewChunk()

	c.AddCapture("outer", VarSourceLocal, 0)
	c.AddCapture("ivar", VarSourceIVar, 5)

	c.EmitWithOperand(OpLoadCapture, 0)
	c.EmitWithOperand(OpLoadCapture, 1)
	c.Emit(OpAdd)
	c.Emit(OpReturn)

	data, err := c.Serialize()
	if err != nil {
		t.Fatalf("Serialize error: %v", err)
	}

	c2, err := Deserialize(data)
	if err != nil {
		t.Fatalf("Deserialize error: %v", err)
	}

	if c2.CaptureCount() != 2 {
		t.Errorf("CaptureCount: got %d, want 2", c2.CaptureCount())
	}

	cap0 := c2.CaptureInfo[0]
	if cap0.Name != "outer" || cap0.Source != VarSourceLocal || cap0.SlotIndex != 0 {
		t.Errorf("Capture 0: got %+v", cap0)
	}

	cap1 := c2.CaptureInfo[1]
	if cap1.Name != "ivar" || cap1.Source != VarSourceIVar || cap1.SlotIndex != 5 {
		t.Errorf("Capture 1: got %+v", cap1)
	}
}

func TestSerializeDeserializeWithDebug(t *testing.T) {
	c := NewChunk()
	c.LocalCount = 2
	c.VarNames = []string{"temp", "result"}

	c.Emit(OpConstZero)
	c.AddSourceLocation(0, 10, 1)

	c.EmitWithOperand(OpStoreLocal, 0)
	c.AddSourceLocation(uint32(c.CodeLen()-2), 10, 5)

	c.Emit(OpReturn)
	c.AddSourceLocation(uint32(c.CodeLen()-1), 11, 1)

	data, err := c.Serialize()
	if err != nil {
		t.Fatalf("Serialize error: %v", err)
	}

	c2, err := Deserialize(data)
	if err != nil {
		t.Fatalf("Deserialize error: %v", err)
	}

	if c2.Flags&ChunkFlagDebug == 0 {
		t.Error("ChunkFlagDebug not preserved")
	}

	if c2.LocalCount != 2 {
		t.Errorf("LocalCount: got %d, want 2", c2.LocalCount)
	}

	if len(c2.VarNames) != 2 {
		t.Errorf("VarNames length: got %d, want 2", len(c2.VarNames))
	}

	if len(c2.SourceMap) != 3 {
		t.Errorf("SourceMap length: got %d, want 3", len(c2.SourceMap))
	}

	line, col := c2.GetSourceLocation(0)
	if line != 10 || col != 1 {
		t.Errorf("Source location at 0: got (%d, %d), want (10, 1)", line, col)
	}
}

func TestSerializeDeserializeRoundTrip(t *testing.T) {
	// Build a more complex chunk
	c := NewChunk()
	c.ParamCount = 1
	c.ParamNames = []string{"n"}
	c.LocalCount = 2
	c.VarNames = []string{"i", "sum"}
	c.AddCapture("multiplier", VarSourceIVar, 0)

	// Emit a simple loop: sum = 0; i = 0; while i < n: sum = sum + i; i = i + 1
	c.EmitConstant("0")     // Push 0
	c.Emit(OpStoreLocal)    // sum = 0
	c.Code = append(c.Code, 1)

	c.EmitConstant("0")     // Push 0
	c.Emit(OpStoreLocal)    // i = 0
	c.Code = append(c.Code, 0)

	loopStart := c.CurrentOffset()
	c.EmitWithOperand(OpLoadLocal, 0)    // Load i
	c.EmitWithOperand(OpLoadParam, 0)    // Load n
	c.Emit(OpLt)                          // i < n

	jumpOff := c.EmitJump(OpJumpFalse)    // Exit if false

	c.EmitWithOperand(OpLoadLocal, 1)    // Load sum
	c.EmitWithOperand(OpLoadLocal, 0)    // Load i
	c.Emit(OpAdd)                         // sum + i
	c.EmitWithOperand(OpStoreLocal, 1)   // Store to sum

	c.EmitWithOperand(OpLoadLocal, 0)    // Load i
	c.Emit(OpConstOne)                    // Push 1
	c.Emit(OpAdd)                         // i + 1
	c.EmitWithOperand(OpStoreLocal, 0)   // Store to i

	c.EmitLoop(loopStart)                 // Back to loop start

	c.PatchJump(jumpOff)
	c.EmitWithOperand(OpLoadLocal, 1)    // Load sum
	c.Emit(OpReturn)

	// Serialize
	data, err := c.Serialize()
	if err != nil {
		t.Fatalf("Serialize error: %v", err)
	}

	// Deserialize
	c2, err := Deserialize(data)
	if err != nil {
		t.Fatalf("Deserialize error: %v", err)
	}

	// Verify structure
	if c2.Version != c.Version {
		t.Errorf("Version: got %d, want %d", c2.Version, c.Version)
	}
	if c2.ParamCount != c.ParamCount {
		t.Errorf("ParamCount: got %d, want %d", c2.ParamCount, c.ParamCount)
	}
	if c2.LocalCount != c.LocalCount {
		t.Errorf("LocalCount: got %d, want %d", c2.LocalCount, c.LocalCount)
	}
	if c2.CaptureCount() != c.CaptureCount() {
		t.Errorf("CaptureCount: got %d, want %d", c2.CaptureCount(), c.CaptureCount())
	}
	if !bytes.Equal(c2.Code, c.Code) {
		t.Error("Code mismatch")
		t.Logf("Original: %v", c.Code)
		t.Logf("Deserialized: %v", c2.Code)
	}

	// Verify we can serialize again and get the same result
	data2, err := c2.Serialize()
	if err != nil {
		t.Fatalf("Second serialize error: %v", err)
	}

	if !bytes.Equal(data, data2) {
		t.Error("Second serialization produced different result")
	}
}

func TestDeserializeErrors(t *testing.T) {
	tests := []struct {
		name string
		data []byte
	}{
		{"too short", []byte{1, 2, 3}},
		{"wrong magic", []byte{'X', 'X', 'X', 'X', 0, 1, 0, 0}},
		{"truncated code", append(BytecodeMagic, 0, 1, 0, 0, 0, 0, 0, 10)}, // Claims 10 bytes of code but has none
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := Deserialize(tt.data)
			if err == nil {
				t.Error("Expected error, got nil")
			}
		})
	}
}

func TestDeserializeFutureVersion(t *testing.T) {
	// Create data with version 999
	data := append(BytecodeMagic, 3, 231, 0, 0) // Version 999, flags 0
	data = append(data, 0, 0, 0, 0)              // Code length 0
	data = append(data, 0, 0)                    // Constant count 0
	data = append(data, 0)                       // Param count 0
	data = append(data, 0)                       // Local count 0
	data = append(data, 0)                       // Capture count 0
	data = append(data, 0)                       // No debug info

	_, err := Deserialize(data)
	if err == nil {
		t.Error("Expected version error, got nil")
	}
}
