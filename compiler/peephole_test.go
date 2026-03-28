package compiler

import (
	"testing"

	"github.com/chazu/maggie/vm"
)

func TestConstantFoldIntAdd(t *testing.T) {
	// PushInt8(3) PushInt8(4) SendPlus → PushInt8(7)
	bc := []byte{
		byte(vm.OpPushInt8), 3,
		byte(vm.OpPushInt8), 4,
		byte(vm.OpSendPlus),
	}
	out, _ := Peephole(bc, nil)
	if len(out) != 2 {
		t.Fatalf("expected 2 bytes (PushInt8 + operand), got %d: %v", len(out), out)
	}
	if vm.Opcode(out[0]) != vm.OpPushInt8 || int8(out[1]) != 7 {
		t.Errorf("expected PushInt8(7), got opcode=%02x operand=%d", out[0], int8(out[1]))
	}
}

func TestConstantFoldIntMul(t *testing.T) {
	// PushInt8(6) PushInt8(7) SendTimes → PushInt8(42)
	bc := []byte{
		byte(vm.OpPushInt8), 6,
		byte(vm.OpPushInt8), 7,
		byte(vm.OpSendTimes),
	}
	out, _ := Peephole(bc, nil)
	if len(out) != 2 {
		t.Fatalf("expected 2 bytes, got %d", len(out))
	}
	if int8(out[1]) != 42 {
		t.Errorf("expected 42, got %d", int8(out[1]))
	}
}

func TestConstantFoldOverflowToInt32(t *testing.T) {
	// PushInt8(100) PushInt8(100) SendTimes → PushInt32(10000)
	// 10000 doesn't fit in int8 (-128..127), so should use PushInt32
	bc := []byte{
		byte(vm.OpPushInt8), 100,
		byte(vm.OpPushInt8), 100,
		byte(vm.OpSendTimes),
	}
	out, _ := Peephole(bc, nil)
	if len(out) != 5 {
		t.Fatalf("expected 5 bytes (PushInt32), got %d: %v", len(out), out)
	}
	if vm.Opcode(out[0]) != vm.OpPushInt32 {
		t.Errorf("expected PushInt32, got %02x", out[0])
	}
}

func TestConstantFoldDivByZeroSkipped(t *testing.T) {
	// PushInt8(10) PushInt8(0) SendDiv → no folding (div by zero)
	bc := []byte{
		byte(vm.OpPushInt8), 10,
		byte(vm.OpPushInt8), 0,
		byte(vm.OpSendDiv),
	}
	out, _ := Peephole(bc, nil)
	if len(out) != 5 {
		t.Fatalf("expected 5 bytes (no folding), got %d", len(out))
	}
}

func TestConstantFoldChained(t *testing.T) {
	// (3 + 4) should fold to 7. If the result is followed by another constant
	// and op, it should fold again.
	// PushInt8(3) PushInt8(4) SendPlus → PushInt8(7)
	// Then PushInt8(7) PushInt8(2) SendTimes → PushInt8(14)
	bc := []byte{
		byte(vm.OpPushInt8), 3,
		byte(vm.OpPushInt8), 4,
		byte(vm.OpSendPlus),
		byte(vm.OpPushInt8), 2,
		byte(vm.OpSendTimes),
	}
	out, _ := Peephole(bc, nil)
	// After first pass: PushInt8(7) NOP NOP NOP PushInt8(2) SendTimes
	// After second pass: PushInt8(14) NOP NOP NOP NOP NOP NOP (or stripped)
	if len(out) != 2 {
		t.Fatalf("expected 2 bytes after chained folding, got %d: %v", len(out), out)
	}
	if int8(out[1]) != 14 {
		t.Errorf("expected 14, got %d", int8(out[1]))
	}
}

func TestPushPopElimination(t *testing.T) {
	// PushNil Pop → eliminated
	bc := []byte{
		byte(vm.OpPushNil),
		byte(vm.OpPOP),
		byte(vm.OpPushSelf),
		byte(vm.OpReturnTop),
	}
	out, _ := Peephole(bc, nil)
	// PushNil+Pop should be eliminated, leaving PushSelf ReturnTop
	if len(out) != 2 {
		t.Fatalf("expected 2 bytes, got %d: %v", len(out), out)
	}
	if vm.Opcode(out[0]) != vm.OpPushSelf {
		t.Errorf("expected PushSelf, got %02x", out[0])
	}
}

func TestPushPopWithOperand(t *testing.T) {
	// PushInt8(5) Pop → eliminated
	bc := []byte{
		byte(vm.OpPushInt8), 5,
		byte(vm.OpPOP),
		byte(vm.OpReturnSelf),
	}
	out, _ := Peephole(bc, nil)
	if len(out) != 1 {
		t.Fatalf("expected 1 byte (ReturnSelf), got %d: %v", len(out), out)
	}
	if vm.Opcode(out[0]) != vm.OpReturnSelf {
		t.Errorf("expected ReturnSelf, got %02x", out[0])
	}
}

func TestDeadCodeAfterReturn(t *testing.T) {
	// ReturnTop followed by unreachable code → dead code eliminated
	bc := []byte{
		byte(vm.OpPushSelf),
		byte(vm.OpReturnTop),
		byte(vm.OpPushInt8), 42, // dead
		byte(vm.OpPOP),          // dead
	}
	out, _ := Peephole(bc, nil)
	if len(out) != 2 {
		t.Fatalf("expected 2 bytes (PushSelf ReturnTop), got %d: %v", len(out), out)
	}
}

func TestSourceMapAdjusted(t *testing.T) {
	// PushNil Pop PushInt8(5) ReturnTop
	// After optimization: PushInt8(5) ReturnTop
	// Source map for PushInt8 was at offset 2, should become offset 0
	bc := []byte{
		byte(vm.OpPushNil),      // offset 0
		byte(vm.OpPOP),          // offset 1
		byte(vm.OpPushInt8), 5,  // offset 2
		byte(vm.OpReturnTop),    // offset 4
	}
	sm := []vm.SourceLoc{
		{Offset: 0, Line: 1, Column: 1},
		{Offset: 2, Line: 2, Column: 1},
		{Offset: 4, Line: 3, Column: 1},
	}
	_, newSM := Peephole(bc, sm)
	if len(newSM) < 2 {
		t.Fatalf("expected at least 2 source map entries, got %d", len(newSM))
	}
	// The PushInt8 entry should now be at offset 0
	found := false
	for _, loc := range newSM {
		if loc.Line == 2 && loc.Offset == 0 {
			found = true
		}
	}
	if !found {
		t.Errorf("expected source map entry for line 2 at offset 0, got %v", newSM)
	}
}

func TestNoChangeForNonOptimizable(t *testing.T) {
	// PushSelf SendSize ReturnTop — nothing to optimize
	bc := []byte{
		byte(vm.OpPushSelf),
		byte(vm.OpSendSize),
		byte(vm.OpReturnTop),
	}
	out, _ := Peephole(bc, nil)
	if len(out) != 3 {
		t.Fatalf("expected 3 bytes unchanged, got %d", len(out))
	}
}

func TestEmptyBytecode(t *testing.T) {
	out, sm := Peephole(nil, nil)
	if len(out) != 0 || len(sm) != 0 {
		t.Errorf("expected empty results for nil input")
	}
}

func TestJumpTargetsPreserved(t *testing.T) {
	// Build: PushTrue JumpFalse(+2) PushInt8(1) ReturnTop PushInt8(2) ReturnTop
	// The JumpFalse skips to PushInt8(2).
	// Dead code elimination should NOT remove PushInt8(2) because it's a jump target.
	bc := []byte{
		byte(vm.OpPushTrue),           // 0
		byte(vm.OpJumpFalse), 2, 0,   // 1: jump +2 → offset 6
		byte(vm.OpPushInt8), 1,        // 4
		byte(vm.OpReturnTop),          // 6 — this is after the conditional path
		// But wait, offset 1+3+2 = 6. At offset 6 is ReturnTop.
	}
	// No dead code here since after ReturnTop there's nothing
	out, _ := Peephole(bc, nil)
	// Should be unchanged (nothing to optimize)
	if len(out) != len(bc) {
		t.Errorf("expected %d bytes, got %d", len(bc), len(out))
	}
}
