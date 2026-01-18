package bytecode

import (
	"encoding/binary"
	"fmt"
	"strings"
)

// Disassemble returns a human-readable bytecode listing for the chunk.
func (c *Chunk) Disassemble() string {
	return c.DisassembleWithName("")
}

// DisassembleWithName returns a human-readable bytecode listing with a name header.
func (c *Chunk) DisassembleWithName(name string) string {
	var sb strings.Builder

	// Header
	if name != "" {
		sb.WriteString(fmt.Sprintf("; === %s ===\n", name))
	}
	sb.WriteString(fmt.Sprintf("; Trashtalk Bytecode v%d\n", c.Version))
	sb.WriteString(fmt.Sprintf("; Flags: 0x%04X", c.Flags))
	if c.Flags&ChunkFlagDebug != 0 {
		sb.WriteString(" [DEBUG]")
	}
	if c.Flags&ChunkFlagHasCaptures != 0 {
		sb.WriteString(" [CAPTURES]")
	}
	if c.Flags&ChunkFlagHasNonLocalReturn != 0 {
		sb.WriteString(" [NON_LOCAL_RET]")
	}
	sb.WriteString("\n")

	// Parameters
	if c.ParamCount > 0 {
		sb.WriteString(fmt.Sprintf("; Parameters (%d): ", c.ParamCount))
		for i, name := range c.ParamNames {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(name)
		}
		sb.WriteString("\n")
	}

	// Locals
	if c.LocalCount > 0 {
		sb.WriteString(fmt.Sprintf("; Locals: %d slots\n", c.LocalCount))
	}

	sb.WriteString("\n")

	// Constants
	if len(c.Constants) > 0 {
		sb.WriteString("; Constants:\n")
		for i, s := range c.Constants {
			// Truncate long strings for readability
			display := s
			if len(display) > 40 {
				display = display[:37] + "..."
			}
			// Escape special characters
			display = strings.ReplaceAll(display, "\n", "\\n")
			display = strings.ReplaceAll(display, "\t", "\\t")
			sb.WriteString(fmt.Sprintf(";   [%3d] %q\n", i, display))
		}
		sb.WriteString("\n")
	}

	// Captures
	if len(c.CaptureInfo) > 0 {
		sb.WriteString("; Captures:\n")
		for i, cap := range c.CaptureInfo {
			sb.WriteString(fmt.Sprintf(";   [%3d] %s (%s, slot=%d)\n",
				i, cap.Name, cap.Source.String(), cap.SlotIndex))
		}
		sb.WriteString("\n")
	}

	// Code section
	sb.WriteString("; Code:\n")
	offset := 0
	for offset < len(c.Code) {
		line, instrLen := c.disassembleInstruction(offset)

		// Add source location if available
		if c.Flags&ChunkFlagDebug != 0 {
			if srcLine, srcCol := c.GetSourceLocation(uint32(offset)); srcLine > 0 {
				sb.WriteString(fmt.Sprintf("%04X  %-30s ; line %d:%d\n", offset, line, srcLine, srcCol))
			} else {
				sb.WriteString(fmt.Sprintf("%04X  %s\n", offset, line))
			}
		} else {
			sb.WriteString(fmt.Sprintf("%04X  %s\n", offset, line))
		}

		offset += instrLen
	}

	return sb.String()
}

// disassembleInstruction disassembles a single instruction at the given offset.
// Returns the formatted string and the instruction length.
func (c *Chunk) disassembleInstruction(offset int) (string, int) {
	if offset >= len(c.Code) {
		return "<end of code>", 0
	}

	op := Opcode(c.Code[offset])
	info := GetOpcodeInfo(op)

	switch op {
	// Constants
	case OpConst:
		idx := c.readUint16(offset + 1)
		constVal := ""
		if int(idx) < len(c.Constants) {
			constVal = c.Constants[idx]
			if len(constVal) > 20 {
				constVal = constVal[:17] + "..."
			}
		}
		return fmt.Sprintf("CONST %d ; %q", idx, constVal), 3

	case OpConstNil:
		return "CONST_NIL", 1
	case OpConstTrue:
		return "CONST_TRUE", 1
	case OpConstFalse:
		return "CONST_FALSE", 1
	case OpConstZero:
		return "CONST_ZERO", 1
	case OpConstOne:
		return "CONST_ONE", 1
	case OpConstEmpty:
		return "CONST_EMPTY", 1

	// Local variables
	case OpLoadLocal:
		slot := c.Code[offset+1]
		varName := c.getVarName(int(slot))
		if varName != "" {
			return fmt.Sprintf("LOAD_LOCAL %d ; %s", slot, varName), 2
		}
		return fmt.Sprintf("LOAD_LOCAL %d", slot), 2

	case OpStoreLocal:
		slot := c.Code[offset+1]
		varName := c.getVarName(int(slot))
		if varName != "" {
			return fmt.Sprintf("STORE_LOCAL %d ; %s", slot, varName), 2
		}
		return fmt.Sprintf("STORE_LOCAL %d", slot), 2

	case OpLoadParam:
		idx := c.Code[offset+1]
		paramName := ""
		if int(idx) < len(c.ParamNames) {
			paramName = c.ParamNames[idx]
		}
		if paramName != "" {
			return fmt.Sprintf("LOAD_PARAM %d ; %s", idx, paramName), 2
		}
		return fmt.Sprintf("LOAD_PARAM %d", idx), 2

	// Captures
	case OpLoadCapture:
		idx := c.Code[offset+1]
		capName := ""
		if int(idx) < len(c.CaptureInfo) {
			capName = c.CaptureInfo[idx].Name
		}
		if capName != "" {
			return fmt.Sprintf("LOAD_CAPTURE %d ; %s", idx, capName), 2
		}
		return fmt.Sprintf("LOAD_CAPTURE %d", idx), 2

	case OpStoreCapture:
		idx := c.Code[offset+1]
		capName := ""
		if int(idx) < len(c.CaptureInfo) {
			capName = c.CaptureInfo[idx].Name
		}
		if capName != "" {
			return fmt.Sprintf("STORE_CAPTURE %d ; %s", idx, capName), 2
		}
		return fmt.Sprintf("STORE_CAPTURE %d", idx), 2

	// Instance variables
	case OpLoadIVar:
		nameIdx := c.readUint16(offset + 1)
		ivarName := ""
		if int(nameIdx) < len(c.Constants) {
			ivarName = c.Constants[nameIdx]
		}
		return fmt.Sprintf("LOAD_IVAR %d ; %s", nameIdx, ivarName), 3

	case OpStoreIVar:
		nameIdx := c.readUint16(offset + 1)
		ivarName := ""
		if int(nameIdx) < len(c.Constants) {
			ivarName = c.Constants[nameIdx]
		}
		return fmt.Sprintf("STORE_IVAR %d ; %s", nameIdx, ivarName), 3

	// Jumps
	case OpJump:
		delta := c.readInt16(offset + 1)
		target := offset + 3 + int(delta)
		return fmt.Sprintf("JUMP %+d (-> %04X)", delta, target), 3

	case OpJumpTrue:
		delta := c.readInt16(offset + 1)
		target := offset + 3 + int(delta)
		return fmt.Sprintf("JUMP_TRUE %+d (-> %04X)", delta, target), 3

	case OpJumpFalse:
		delta := c.readInt16(offset + 1)
		target := offset + 3 + int(delta)
		return fmt.Sprintf("JUMP_FALSE %+d (-> %04X)", delta, target), 3

	case OpJumpNil:
		delta := c.readInt16(offset + 1)
		target := offset + 3 + int(delta)
		return fmt.Sprintf("JUMP_NIL %+d (-> %04X)", delta, target), 3

	case OpJumpNotNil:
		delta := c.readInt16(offset + 1)
		target := offset + 3 + int(delta)
		return fmt.Sprintf("JUMP_NOT_NIL %+d (-> %04X)", delta, target), 3

	// Message sends
	case OpSend:
		selIdx := c.readUint16(offset + 1)
		argc := c.Code[offset+3]
		selector := ""
		if int(selIdx) < len(c.Constants) {
			selector = c.Constants[selIdx]
		}
		return fmt.Sprintf("SEND %d (%s) argc=%d", selIdx, selector, argc), 4

	case OpSendSelf:
		selIdx := c.readUint16(offset + 1)
		argc := c.Code[offset+3]
		selector := ""
		if int(selIdx) < len(c.Constants) {
			selector = c.Constants[selIdx]
		}
		return fmt.Sprintf("SEND_SELF %d (%s) argc=%d", selIdx, selector, argc), 4

	case OpSendSuper:
		selIdx := c.readUint16(offset + 1)
		argc := c.Code[offset+3]
		selector := ""
		if int(selIdx) < len(c.Constants) {
			selector = c.Constants[selIdx]
		}
		return fmt.Sprintf("SEND_SUPER %d (%s) argc=%d", selIdx, selector, argc), 4

	case OpSendClass:
		classIdx := c.readUint16(offset + 1)
		selIdx := c.readUint16(offset + 3)
		argc := c.Code[offset+5]
		className := ""
		selector := ""
		if int(classIdx) < len(c.Constants) {
			className = c.Constants[classIdx]
		}
		if int(selIdx) < len(c.Constants) {
			selector = c.Constants[selIdx]
		}
		return fmt.Sprintf("SEND_CLASS %d (%s) %d (%s) argc=%d", classIdx, className, selIdx, selector, argc), 6

	// Blocks
	case OpMakeBlock:
		codeIdx := c.readUint16(offset + 1)
		numCaptures := c.Code[offset+3]
		return fmt.Sprintf("MAKE_BLOCK code=%d captures=%d", codeIdx, numCaptures), 4

	case OpInvokeBlock:
		argc := c.Code[offset+1]
		return fmt.Sprintf("INVOKE_BLOCK argc=%d", argc), 2

	case OpBlockValue:
		return "BLOCK_VALUE", 1

	// Default: use info from table
	default:
		instrLen := 1 + info.OperandLen
		if info.OperandLen == 0 {
			return info.Name, instrLen
		}

		// Format operands generically
		operands := make([]string, 0, info.OperandLen)
		for i := 0; i < info.OperandLen; i++ {
			if offset+1+i < len(c.Code) {
				operands = append(operands, fmt.Sprintf("0x%02X", c.Code[offset+1+i]))
			}
		}
		return fmt.Sprintf("%s %s", info.Name, strings.Join(operands, " ")), instrLen
	}
}

// DisassembleInstruction returns a human-readable representation of a single instruction.
func (c *Chunk) DisassembleInstruction(offset int) string {
	line, _ := c.disassembleInstruction(offset)
	return line
}

// readUint16 reads a big-endian uint16 from the code at the given offset.
func (c *Chunk) readUint16(offset int) uint16 {
	if offset+1 >= len(c.Code) {
		return 0
	}
	return binary.BigEndian.Uint16(c.Code[offset:])
}

// readInt16 reads a big-endian int16 from the code at the given offset.
func (c *Chunk) readInt16(offset int) int16 {
	return int16(c.readUint16(offset))
}

// getVarName returns the variable name for a local slot if available.
func (c *Chunk) getVarName(slot int) string {
	if slot < len(c.VarNames) {
		return c.VarNames[slot]
	}
	return ""
}

// DisassembleToLines returns the disassembly as a slice of lines.
func (c *Chunk) DisassembleToLines() []string {
	var lines []string
	offset := 0
	for offset < len(c.Code) {
		line, instrLen := c.disassembleInstruction(offset)
		lines = append(lines, fmt.Sprintf("%04X  %s", offset, line))
		offset += instrLen
	}
	return lines
}

// InstructionCount returns the number of instructions in the chunk.
// Note: This iterates through all code, so it's O(n).
func (c *Chunk) InstructionCount() int {
	count := 0
	offset := 0
	for offset < len(c.Code) {
		op := Opcode(c.Code[offset])
		offset += op.InstructionLen()
		count++
	}
	return count
}
