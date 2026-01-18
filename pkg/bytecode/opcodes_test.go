package bytecode

import (
	"strings"
	"testing"
)

func TestAllOpcodesHaveMetadata(t *testing.T) {
	// Ensure every defined opcode has metadata
	for _, op := range AllOpcodes() {
		info := GetOpcodeInfo(op)
		if info.Name == "" || strings.HasPrefix(info.Name, "UNKNOWN") {
			t.Errorf("Opcode 0x%02X has no metadata", op)
		}
	}
}

func TestOpcodeCount(t *testing.T) {
	count := OpcodeCount()
	if count < 50 {
		t.Errorf("Expected at least 50 opcodes, got %d", count)
	}
}

func TestOpcodeString(t *testing.T) {
	tests := []struct {
		op   Opcode
		want string
	}{
		{OpNop, "NOP"},
		{OpPop, "POP"},
		{OpDup, "DUP"},
		{OpConst, "CONST"},
		{OpAdd, "ADD"},
		{OpSub, "SUB"},
		{OpEq, "EQ"},
		{OpJump, "JUMP"},
		{OpSend, "SEND"},
		{OpReturn, "RETURN"},
		{OpMakeBlock, "MAKE_BLOCK"},
	}

	for _, tt := range tests {
		got := tt.op.String()
		if got != tt.want {
			t.Errorf("Opcode(0x%02X).String() = %q, want %q", tt.op, got, tt.want)
		}
	}
}

func TestUnknownOpcodeString(t *testing.T) {
	// Test an undefined opcode value
	op := Opcode(0xEE) // Not defined
	got := op.String()
	if got[:7] != "UNKNOWN" {
		t.Errorf("Unknown opcode should return UNKNOWN, got %q", got)
	}
}

func TestOpcodeOperandLen(t *testing.T) {
	tests := []struct {
		op   Opcode
		want int
	}{
		{OpNop, 0},
		{OpPop, 0},
		{OpConst, 2},      // u16 index
		{OpLoadLocal, 1},  // u8 slot
		{OpJump, 2},       // i16 offset
		{OpSend, 3},       // u16 selector + u8 argc
		{OpSendClass, 5},  // u16 class + u16 selector + u8 argc
		{OpMakeBlock, 3},  // u16 code + u8 captures
		{OpInvokeBlock, 1}, // u8 argc
	}

	for _, tt := range tests {
		got := tt.op.OperandLen()
		if got != tt.want {
			t.Errorf("%s.OperandLen() = %d, want %d", tt.op, got, tt.want)
		}
	}
}

func TestOpcodeInstructionLen(t *testing.T) {
	tests := []struct {
		op   Opcode
		want int
	}{
		{OpNop, 1},        // Just the opcode
		{OpConst, 3},      // opcode + 2 bytes
		{OpLoadLocal, 2},  // opcode + 1 byte
		{OpJump, 3},       // opcode + 2 bytes
		{OpSend, 4},       // opcode + 3 bytes
	}

	for _, tt := range tests {
		got := tt.op.InstructionLen()
		if got != tt.want {
			t.Errorf("%s.InstructionLen() = %d, want %d", tt.op, got, tt.want)
		}
	}
}

func TestOpcodeIsJump(t *testing.T) {
	jumps := []Opcode{OpJump, OpJumpTrue, OpJumpFalse, OpJumpNil, OpJumpNotNil}
	for _, op := range jumps {
		if !op.IsJump() {
			t.Errorf("%s.IsJump() = false, want true", op)
		}
	}

	nonJumps := []Opcode{OpNop, OpAdd, OpSend, OpReturn}
	for _, op := range nonJumps {
		if op.IsJump() {
			t.Errorf("%s.IsJump() = true, want false", op)
		}
	}
}

func TestOpcodeIsReturn(t *testing.T) {
	returns := []Opcode{OpReturn, OpReturnNil, OpNonLocalRet}
	for _, op := range returns {
		if !op.IsReturn() {
			t.Errorf("%s.IsReturn() = false, want true", op)
		}
	}

	nonReturns := []Opcode{OpNop, OpAdd, OpJump}
	for _, op := range nonReturns {
		if op.IsReturn() {
			t.Errorf("%s.IsReturn() = true, want false", op)
		}
	}
}

func TestOpcodeIsSend(t *testing.T) {
	sends := []Opcode{OpSend, OpSendSelf, OpSendSuper, OpSendClass}
	for _, op := range sends {
		if !op.IsSend() {
			t.Errorf("%s.IsSend() = false, want true", op)
		}
	}

	nonSends := []Opcode{OpNop, OpAdd, OpReturn}
	for _, op := range nonSends {
		if op.IsSend() {
			t.Errorf("%s.IsSend() = true, want false", op)
		}
	}
}

func TestOpcodeIsBlockOp(t *testing.T) {
	blockOps := []Opcode{OpMakeBlock, OpInvokeBlock, OpBlockValue}
	for _, op := range blockOps {
		if !op.IsBlockOp() {
			t.Errorf("%s.IsBlockOp() = false, want true", op)
		}
	}

	nonBlockOps := []Opcode{OpNop, OpAdd, OpSend}
	for _, op := range nonBlockOps {
		if op.IsBlockOp() {
			t.Errorf("%s.IsBlockOp() = true, want false", op)
		}
	}
}

func TestStackEffects(t *testing.T) {
	// Test that stack effects are reasonable
	tests := []struct {
		op      Opcode
		pop     int
		push    int
	}{
		{OpNop, 0, 0},
		{OpPop, 1, 0},
		{OpDup, 1, 2},
		{OpSwap, 2, 2},
		{OpConst, 0, 1},
		{OpAdd, 2, 1},
		{OpEq, 2, 1},
		{OpNot, 1, 1},
		{OpReturn, 1, 0},
		{OpReturnNil, 0, 0},
	}

	for _, tt := range tests {
		info := GetOpcodeInfo(tt.op)
		if info.StackPop != tt.pop {
			t.Errorf("%s.StackPop = %d, want %d", tt.op, info.StackPop, tt.pop)
		}
		if info.StackPush != tt.push {
			t.Errorf("%s.StackPush = %d, want %d", tt.op, info.StackPush, tt.push)
		}
	}
}

func TestOpcodeRanges(t *testing.T) {
	// Verify opcodes are in their expected ranges
	rangeTests := []struct {
		name     string
		ops      []Opcode
		minRange Opcode
		maxRange Opcode
	}{
		{"Stack", []Opcode{OpNop, OpPop, OpDup, OpSwap}, 0x00, 0x0F},
		{"Constants", []Opcode{OpConst, OpConstNil, OpConstTrue}, 0x10, 0x1F},
		{"Locals", []Opcode{OpLoadLocal, OpStoreLocal, OpLoadParam}, 0x20, 0x2F},
		{"Captures", []Opcode{OpLoadCapture, OpStoreCapture}, 0x30, 0x3F},
		{"IVars", []Opcode{OpLoadIVar, OpStoreIVar}, 0x40, 0x4F},
		{"Arithmetic", []Opcode{OpAdd, OpSub, OpMul, OpDiv}, 0x50, 0x5F},
		{"Comparison", []Opcode{OpEq, OpNe, OpLt, OpGt}, 0x60, 0x6F},
		{"Control", []Opcode{OpJump, OpJumpTrue, OpJumpFalse}, 0x80, 0x8F},
		{"Sends", []Opcode{OpSend, OpSendSelf, OpSendSuper}, 0x90, 0x9F},
		{"Blocks", []Opcode{OpMakeBlock, OpInvokeBlock}, 0xA0, 0xAF},
		{"Return", []Opcode{OpReturn, OpReturnNil}, 0xF0, 0xFF},
	}

	for _, tt := range rangeTests {
		for _, op := range tt.ops {
			if op < tt.minRange || op > tt.maxRange {
				t.Errorf("%s opcode %s (0x%02X) is outside range [0x%02X, 0x%02X]",
					tt.name, op, op, tt.minRange, tt.maxRange)
			}
		}
	}
}

func TestVarSourceString(t *testing.T) {
	tests := []struct {
		src  VarSource
		want string
	}{
		{VarSourceLocal, "local"},
		{VarSourceIVar, "ivar"},
		{VarSourceCapture, "capture"},
		{VarSourceParam, "param"},
		{VarSource(99), "VarSource(99)"},
	}

	for _, tt := range tests {
		got := tt.src.String()
		if got != tt.want {
			t.Errorf("VarSource(%d).String() = %q, want %q", tt.src, got, tt.want)
		}
	}
}
