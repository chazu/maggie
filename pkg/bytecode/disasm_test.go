package bytecode

import (
	"strings"
	"testing"
)

func TestDisassembleEmpty(t *testing.T) {
	c := NewChunk()

	output := c.Disassemble()

	if !strings.Contains(output, "Trashtalk Bytecode") {
		t.Error("Disassembly missing header")
	}
}

func TestDisassembleSimple(t *testing.T) {
	c := NewChunk()
	c.Emit(OpConstZero)
	c.Emit(OpConstOne)
	c.Emit(OpAdd)
	c.Emit(OpReturn)

	output := c.Disassemble()

	// Should contain the opcodes
	if !strings.Contains(output, "CONST_ZERO") {
		t.Error("Missing CONST_ZERO")
	}
	if !strings.Contains(output, "CONST_ONE") {
		t.Error("Missing CONST_ONE")
	}
	if !strings.Contains(output, "ADD") {
		t.Error("Missing ADD")
	}
	if !strings.Contains(output, "RETURN") {
		t.Error("Missing RETURN")
	}
}

func TestDisassembleWithConstants(t *testing.T) {
	c := NewChunk()
	c.EmitConstant("hello world")
	c.Emit(OpReturn)

	output := c.Disassemble()

	// Should show constants section
	if !strings.Contains(output, "Constants:") {
		t.Error("Missing Constants section")
	}
	if !strings.Contains(output, "hello world") {
		t.Error("Missing constant value")
	}
	// Should show CONST instruction with value
	if !strings.Contains(output, "CONST 0") {
		t.Error("Missing CONST instruction")
	}
}

func TestDisassembleWithParams(t *testing.T) {
	c := NewChunk()
	c.ParamCount = 2
	c.ParamNames = []string{"x", "y"}

	c.EmitWithOperand(OpLoadParam, 0)
	c.EmitWithOperand(OpLoadParam, 1)
	c.Emit(OpAdd)
	c.Emit(OpReturn)

	output := c.Disassemble()

	// Should show parameters
	if !strings.Contains(output, "Parameters (2)") {
		t.Error("Missing Parameters section")
	}
	if !strings.Contains(output, "x") || !strings.Contains(output, "y") {
		t.Error("Missing parameter names")
	}
	// LOAD_PARAM should show parameter name
	if !strings.Contains(output, "LOAD_PARAM 0 ; x") {
		t.Error("LOAD_PARAM should include param name comment")
	}
}

func TestDisassembleWithCaptures(t *testing.T) {
	c := NewChunk()
	c.AddCapture("outer", VarSourceLocal, 0)
	c.AddCapture("counter", VarSourceIVar, 5)

	c.EmitWithOperand(OpLoadCapture, 0)
	c.EmitWithOperand(OpStoreCapture, 1)
	c.Emit(OpReturn)

	output := c.Disassemble()

	// Should show captures section
	if !strings.Contains(output, "Captures:") {
		t.Error("Missing Captures section")
	}
	if !strings.Contains(output, "outer") {
		t.Error("Missing capture name 'outer'")
	}
	if !strings.Contains(output, "counter") {
		t.Error("Missing capture name 'counter'")
	}
	// Should show source type
	if !strings.Contains(output, "local") {
		t.Error("Missing capture source 'local'")
	}
	if !strings.Contains(output, "ivar") {
		t.Error("Missing capture source 'ivar'")
	}
}

func TestDisassembleWithLocals(t *testing.T) {
	c := NewChunk()
	c.LocalCount = 3
	c.VarNames = []string{"temp", "result", "flag"}

	c.EmitWithOperand(OpLoadLocal, 0)
	c.EmitWithOperand(OpStoreLocal, 1)
	c.Emit(OpReturn)

	output := c.Disassemble()

	// Should show locals count
	if !strings.Contains(output, "Locals: 3 slots") {
		t.Error("Missing Locals info")
	}
	// Should show variable names in comments
	if !strings.Contains(output, "LOAD_LOCAL 0 ; temp") {
		t.Error("LOAD_LOCAL should include var name comment")
	}
	if !strings.Contains(output, "STORE_LOCAL 1 ; result") {
		t.Error("STORE_LOCAL should include var name comment")
	}
}

func TestDisassembleJumps(t *testing.T) {
	c := NewChunk()

	c.Emit(OpConstZero)
	jumpOff := c.EmitJump(OpJumpFalse)
	c.Emit(OpConstOne)
	c.PatchJump(jumpOff)
	c.Emit(OpReturn)

	output := c.Disassemble()

	// Should show jump with target
	if !strings.Contains(output, "JUMP_FALSE") {
		t.Error("Missing JUMP_FALSE")
	}
	// Should show target offset
	if !strings.Contains(output, "->") {
		t.Error("Jump should show target offset")
	}
}

func TestDisassembleSends(t *testing.T) {
	c := NewChunk()

	// Add selector constant
	selIdx := c.AddConstant("increment:")

	// Emit send: receiver on stack, then send
	c.Emit(OpConstNil)
	c.EmitWithOperand(OpSend, byte(selIdx>>8), byte(selIdx), 1)
	c.Emit(OpReturn)

	output := c.Disassemble()

	// Should show send with selector
	if !strings.Contains(output, "SEND") {
		t.Error("Missing SEND")
	}
	if !strings.Contains(output, "increment:") {
		t.Error("Missing selector name")
	}
	if !strings.Contains(output, "argc=1") {
		t.Error("Missing argc")
	}
}

func TestDisassembleSendSelf(t *testing.T) {
	c := NewChunk()

	selIdx := c.AddConstant("getValue")
	c.EmitWithOperand(OpSendSelf, byte(selIdx>>8), byte(selIdx), 0)
	c.Emit(OpReturn)

	output := c.Disassemble()

	if !strings.Contains(output, "SEND_SELF") {
		t.Error("Missing SEND_SELF")
	}
	if !strings.Contains(output, "getValue") {
		t.Error("Missing selector name")
	}
}

func TestDisassembleIVars(t *testing.T) {
	c := NewChunk()

	nameIdx := c.AddConstant("counter")
	c.EmitWithOperand(OpLoadIVar, byte(nameIdx>>8), byte(nameIdx))
	c.Emit(OpConstOne)
	c.Emit(OpAdd)
	c.EmitWithOperand(OpStoreIVar, byte(nameIdx>>8), byte(nameIdx))
	c.Emit(OpReturn)

	output := c.Disassemble()

	// Should show ivar name
	if !strings.Contains(output, "LOAD_IVAR") {
		t.Error("Missing LOAD_IVAR")
	}
	if !strings.Contains(output, "STORE_IVAR") {
		t.Error("Missing STORE_IVAR")
	}
	if !strings.Contains(output, "; counter") {
		t.Error("Missing ivar name comment")
	}
}

func TestDisassembleBlocks(t *testing.T) {
	c := NewChunk()

	// Emit block creation
	c.EmitWithOperand(OpMakeBlock, 0, 10, 2) // code at offset 10, 2 captures
	c.EmitWithOperand(OpInvokeBlock, 1)      // invoke with 1 arg
	c.Emit(OpReturn)

	output := c.Disassemble()

	if !strings.Contains(output, "MAKE_BLOCK") {
		t.Error("Missing MAKE_BLOCK")
	}
	if !strings.Contains(output, "code=10") {
		t.Error("Missing code offset")
	}
	if !strings.Contains(output, "captures=2") {
		t.Error("Missing captures count")
	}
	if !strings.Contains(output, "INVOKE_BLOCK") {
		t.Error("Missing INVOKE_BLOCK")
	}
	if !strings.Contains(output, "argc=1") {
		t.Error("Missing argc for INVOKE_BLOCK")
	}
}

func TestDisassembleWithName(t *testing.T) {
	c := NewChunk()
	c.Emit(OpReturn)

	output := c.DisassembleWithName("TestMethod")

	if !strings.Contains(output, "=== TestMethod ===") {
		t.Error("Missing name header")
	}
}

func TestDisassembleWithDebugInfo(t *testing.T) {
	c := NewChunk()
	c.Emit(OpConstZero)
	c.AddSourceLocation(0, 10, 5)
	c.Emit(OpReturn)
	c.AddSourceLocation(1, 11, 1)

	output := c.Disassemble()

	// Should show source locations
	if !strings.Contains(output, "line 10") {
		t.Error("Missing source line info")
	}
}

func TestDisassembleToLines(t *testing.T) {
	c := NewChunk()
	c.Emit(OpConstZero)
	c.Emit(OpConstOne)
	c.Emit(OpAdd)
	c.Emit(OpReturn)

	lines := c.DisassembleToLines()

	if len(lines) != 4 {
		t.Errorf("Expected 4 lines, got %d", len(lines))
	}

	// Each line should have offset prefix
	if !strings.HasPrefix(lines[0], "0000") {
		t.Error("First line should start with 0000")
	}
}

func TestInstructionCount(t *testing.T) {
	c := NewChunk()
	c.Emit(OpConstZero)              // 1 byte
	c.EmitWithOperand(OpLoadLocal, 0) // 2 bytes
	c.EmitConstant("test")            // 3 bytes
	c.Emit(OpReturn)                  // 1 byte

	count := c.InstructionCount()
	if count != 4 {
		t.Errorf("InstructionCount() = %d, want 4", count)
	}
}

func TestDisassembleLongConstant(t *testing.T) {
	c := NewChunk()

	// Add a long constant
	longStr := strings.Repeat("x", 100)
	c.EmitConstant(longStr)
	c.Emit(OpReturn)

	output := c.Disassemble()

	// Should truncate long constants
	if strings.Contains(output, strings.Repeat("x", 50)) {
		t.Error("Long constant should be truncated")
	}
	if !strings.Contains(output, "...") {
		t.Error("Truncated constant should have ellipsis")
	}
}

func TestDisassembleAllOpcodes(t *testing.T) {
	// Test that all opcodes can be disassembled without panicking
	for _, op := range AllOpcodes() {
		c := NewChunk()

		// Emit the opcode with placeholder operands
		operandLen := op.OperandLen()
		operands := make([]byte, operandLen)
		c.EmitWithOperand(op, operands...)

		// Should not panic
		output := c.Disassemble()

		// Should contain the opcode name
		info := GetOpcodeInfo(op)
		if !strings.Contains(output, info.Name) {
			t.Errorf("Disassembly of %s missing opcode name", info.Name)
		}
	}
}
