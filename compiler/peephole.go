package compiler

import (
	"encoding/binary"

	"github.com/chazu/maggie/vm"
)

// Peephole performs a bytecode-level optimization pass. It operates on the
// raw bytecode and source map in-place, replacing optimizable patterns with
// NOPs or shorter sequences. A final pass strips the NOPs and adjusts
// source map offsets and jump targets.
//
// Optimizations:
//   - Constant folding: PushInt/PushInt/SendArith → PushInt(result)
//   - Push-pop elimination: Push*/Pop pairs → NOP
//   - Dead code after unconditional jumps → NOP
func Peephole(bytecode []byte, sourceMap []vm.SourceLoc) ([]byte, []vm.SourceLoc) {
	if len(bytecode) == 0 {
		return bytecode, sourceMap
	}

	// Iterate: fold/eliminate → strip NOPs → fold again until stable.
	// Stripping NOPs can create new fold opportunities (e.g., chained arithmetic).
	for {
		changed := false
		changed = foldConstants(bytecode) || changed
		changed = eliminatePushPop(bytecode) || changed

		eliminateDeadCode(bytecode)

		bytecode, sourceMap = stripNOPs(bytecode, sourceMap)
		if !changed {
			break
		}
	}

	return bytecode, sourceMap
}

// ---------------------------------------------------------------------------
// Phase A: Constant folding
// ---------------------------------------------------------------------------

// foldConstants replaces PushInt8/PushInt8/SendArith with PushInt8(result)
// or PushInt32(result) if the result doesn't fit in int8. Returns true if
// any changes were made.
func foldConstants(bc []byte) bool {
	changed := false
	i := 0
	for i+4 < len(bc) {
		// Pattern: PushInt8 <a> PushInt8 <b> SendArith
		if bc[i] == byte(vm.OpPushInt8) && bc[i+2] == byte(vm.OpPushInt8) {
			a := int64(int8(bc[i+1]))
			b := int64(int8(bc[i+3]))
			op := vm.Opcode(bc[i+4])

			var result int64
			var ok bool
			switch op {
			case vm.OpSendPlus:
				result, ok = a+b, true
			case vm.OpSendMinus:
				result, ok = a-b, true
			case vm.OpSendTimes:
				result, ok = a*b, true
			case vm.OpSendDiv:
				if b != 0 {
					result, ok = a/b, true
				}
			case vm.OpSendMod:
				if b != 0 {
					result, ok = a%b, true
				}
			}

			if ok {
				if result >= -128 && result <= 127 {
					// Result fits in int8: PushInt8 <result> + 3 NOPs
					bc[i] = byte(vm.OpPushInt8)
					bc[i+1] = byte(int8(result))
					bc[i+2] = byte(vm.OpNOP)
					bc[i+3] = byte(vm.OpNOP)
					bc[i+4] = byte(vm.OpNOP)
					changed = true
					i += 5
					continue
				}
				// Result needs int32: PushInt32 <result32> but that's 5 bytes
				// and we have exactly 5 bytes (2+2+1) to fill. Perfect fit.
				bc[i] = byte(vm.OpPushInt32)
				binary.LittleEndian.PutUint32(bc[i+1:i+5], uint32(int32(result)))
				changed = true
				i += 5
				continue
			}
		}
		// Advance by instruction size
		i += instrSize(bc, i)
	}
	return changed
}

// ---------------------------------------------------------------------------
// Phase B: Dead code elimination
// ---------------------------------------------------------------------------

// eliminateDeadCode replaces unreachable instructions after unconditional
// jumps with NOPs. Stops at the next jump target (any offset that is the
// target of a jump instruction).
func eliminateDeadCode(bc []byte) {
	// First pass: collect all jump targets
	targets := make(map[int]bool)
	i := 0
	for i < len(bc) {
		op := vm.Opcode(bc[i])
		size := instrSize(bc, i)
		if isJump(op) && i+2 < len(bc) {
			offset := int(int16(binary.LittleEndian.Uint16(bc[i+1 : i+3])))
			target := i + 3 + offset // jump is relative to end of instruction
			if target >= 0 && target < len(bc) {
				targets[target] = true
			}
		}
		i += size
	}

	// Second pass: NOP out instructions after unconditional jumps
	i = 0
	for i < len(bc) {
		op := vm.Opcode(bc[i])
		size := instrSize(bc, i)
		if op == vm.OpJump || op == vm.OpReturnTop || op == vm.OpReturnSelf ||
			op == vm.OpReturnNil || op == vm.OpBlockReturn {
			// Everything after this until a jump target or end is dead
			j := i + size
			for j < len(bc) {
				if targets[j] {
					break // This offset is reachable from elsewhere
				}
				jsize := instrSize(bc, j)
				for k := 0; k < jsize; k++ {
					bc[j+k] = byte(vm.OpNOP)
				}
				j += jsize
			}
		}
		i += size
	}
}

// ---------------------------------------------------------------------------
// Phase C: Push-pop elimination
// ---------------------------------------------------------------------------

// eliminatePushPop replaces side-effect-free Push*/Pop pairs with NOPs.
// Returns true if any changes were made.
func eliminatePushPop(bc []byte) bool {
	changed := false
	i := 0
	for i < len(bc) {
		size := instrSize(bc, i)
		op := vm.Opcode(bc[i])

		// Check if this is a side-effect-free push followed by Pop
		if isPurePush(op) {
			nextI := i + size
			if nextI < len(bc) && vm.Opcode(bc[nextI]) == vm.OpPOP {
				// NOP out both the push and the pop
				for k := i; k < nextI; k++ {
					bc[k] = byte(vm.OpNOP)
				}
				bc[nextI] = byte(vm.OpNOP)
				changed = true
				i = nextI + 1
				continue
			}
		}
		i += size
	}
	return changed
}

// ---------------------------------------------------------------------------
// NOP stripping and offset adjustment
// ---------------------------------------------------------------------------

// stripNOPs removes all NOP instructions, adjusts jump targets, and
// remaps source map offsets.
func stripNOPs(bc []byte, sourceMap []vm.SourceLoc) ([]byte, []vm.SourceLoc) {
	// Build offset mapping: old offset → new offset
	offsetMap := make([]int, len(bc)+1)
	newOffset := 0
	for i := 0; i < len(bc); {
		offsetMap[i] = newOffset
		size := instrSize(bc, i)
		if vm.Opcode(bc[i]) != vm.OpNOP {
			newOffset += size
		}
		i += size
	}
	offsetMap[len(bc)] = newOffset

	if newOffset == len(bc) {
		return bc, sourceMap // Nothing to strip
	}

	// Copy non-NOP instructions to new buffer, adjusting jump offsets
	out := make([]byte, 0, newOffset)
	i := 0
	for i < len(bc) {
		op := vm.Opcode(bc[i])
		size := instrSize(bc, i)
		if op == vm.OpNOP {
			i += size
			continue
		}

		if isJump(op) && size >= 3 {
			// Adjust relative jump offset
			oldTarget := i + 3 + int(int16(binary.LittleEndian.Uint16(bc[i+1:i+3])))
			newTargetPos := offsetMap[oldTarget]
			newInstrEnd := offsetMap[i] + 3 // new position of this jump + 3 bytes
			newRelOffset := int16(newTargetPos - newInstrEnd)
			out = append(out, bc[i])
			out = append(out, byte(newRelOffset), byte(newRelOffset>>8))
			if size > 3 {
				out = append(out, bc[i+3:i+size]...)
			}
		} else {
			out = append(out, bc[i:i+size]...)
		}
		i += size
	}

	// Remap source map offsets
	var newSourceMap []vm.SourceLoc
	for _, loc := range sourceMap {
		if loc.Offset < len(offsetMap) {
			newLoc := vm.SourceLoc{
				Offset: offsetMap[loc.Offset],
				Line:   loc.Line,
				Column: loc.Column,
			}
			// Deduplicate: if same offset as previous entry, replace it
			// (the later entry is more specific for the surviving instruction)
			if len(newSourceMap) > 0 && newSourceMap[len(newSourceMap)-1].Offset == newLoc.Offset {
				newSourceMap[len(newSourceMap)-1] = newLoc
				continue
			}
			newSourceMap = append(newSourceMap, newLoc)
		}
	}

	return out, newSourceMap
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

// instrSize returns the total size (opcode + operands) of the instruction
// at bc[offset]. Falls back to 1 for unknown opcodes.
func instrSize(bc []byte, offset int) int {
	if offset >= len(bc) {
		return 1
	}
	return 1 + vm.Opcode(bc[offset]).Info().OperandBytes
}

// isPurePush returns true if the opcode is a push with no side effects
// (safe to eliminate if immediately followed by Pop).
func isPurePush(op vm.Opcode) bool {
	switch op {
	case vm.OpPushNil, vm.OpPushTrue, vm.OpPushFalse, vm.OpPushSelf,
		vm.OpPushInt8, vm.OpPushInt32, vm.OpPushFloat, vm.OpPushLiteral,
		vm.OpPushTemp, vm.OpPushIvar, vm.OpPushCaptured, vm.OpPushHomeTemp,
		vm.OpPushGlobal, vm.OpPushClassVar, vm.OpDUP:
		return true
	}
	return false
}

// isJump returns true if the opcode is a jump instruction (has a 16-bit
// relative offset operand).
func isJump(op vm.Opcode) bool {
	switch op {
	case vm.OpJump, vm.OpJumpTrue, vm.OpJumpFalse, vm.OpJumpNil, vm.OpJumpNotNil:
		return true
	}
	return false
}
