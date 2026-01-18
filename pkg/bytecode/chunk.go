package bytecode

import (
	"encoding/binary"
	"fmt"
)

// BytecodeVersion is the current bytecode format version.
// Increment when making incompatible changes to the format.
const BytecodeVersion uint16 = 1

// Magic bytes for bytecode files: "TTBC" (TrashTalk ByteCode)
var BytecodeMagic = []byte{'T', 'T', 'B', 'C'}

// ChunkFlags contains compilation flags for a chunk.
type ChunkFlags uint16

const (
	// ChunkFlagDebug indicates debug information is present.
	ChunkFlagDebug ChunkFlags = 1 << 0

	// ChunkFlagHasCaptures indicates the block captures variables.
	ChunkFlagHasCaptures ChunkFlags = 1 << 1

	// ChunkFlagHasNonLocalReturn indicates the block may perform non-local return.
	ChunkFlagHasNonLocalReturn ChunkFlags = 1 << 2
)

// VarSource indicates where a captured variable originates.
type VarSource uint8

const (
	// VarSourceLocal indicates a local variable in the enclosing scope.
	VarSourceLocal VarSource = 0

	// VarSourceIVar indicates an instance variable.
	VarSourceIVar VarSource = 1

	// VarSourceCapture indicates an already-captured variable from an outer scope.
	VarSourceCapture VarSource = 2

	// VarSourceParam indicates a parameter from the enclosing method.
	VarSourceParam VarSource = 3
)

// String returns a human-readable name for VarSource.
func (v VarSource) String() string {
	switch v {
	case VarSourceLocal:
		return "local"
	case VarSourceIVar:
		return "ivar"
	case VarSourceCapture:
		return "capture"
	case VarSourceParam:
		return "param"
	default:
		return fmt.Sprintf("VarSource(%d)", v)
	}
}

// CaptureDescriptor describes a captured variable in a block.
type CaptureDescriptor struct {
	Name      string    // Variable name
	Source    VarSource // Where the variable comes from
	SlotIndex uint8     // Slot in enclosing scope (for nested blocks)
}

// SourceLocation maps bytecode position to source location for debugging.
type SourceLocation struct {
	BytecodeOffset uint32 // Offset in code section
	Line           uint32 // Source line number (1-based)
	Column         uint16 // Source column number (1-based)
}

// Chunk represents compiled bytecode for a block or method.
// It is the fundamental unit of bytecode that can be serialized and executed.
type Chunk struct {
	// Header
	Version uint16     // Bytecode format version
	Flags   ChunkFlags // Compilation flags

	// Code section
	Code []byte // Bytecode instructions

	// Constant pool - strings referenced by OpConst
	Constants []string

	// Parameter information
	ParamCount uint8    // Number of parameters
	ParamNames []string // Parameter names (for debugging/reflection)

	// Local variables
	LocalCount uint8 // Number of local variable slots needed

	// Capture information for closures
	CaptureInfo []CaptureDescriptor

	// Debug information (optional, present if ChunkFlagDebug is set)
	SourceMap []SourceLocation // Bytecode offset -> source location
	VarNames  []string         // Local variable names for debugging
}

// NewChunk creates a new empty chunk with the current version.
func NewChunk() *Chunk {
	return &Chunk{
		Version:   BytecodeVersion,
		Code:      make([]byte, 0, 64),
		Constants: make([]string, 0, 8),
	}
}

// AddConstant adds a string constant to the pool and returns its index.
// If the constant already exists, returns the existing index.
func (c *Chunk) AddConstant(value string) uint16 {
	// Check if constant already exists
	for i, s := range c.Constants {
		if s == value {
			return uint16(i)
		}
	}
	// Add new constant
	idx := uint16(len(c.Constants))
	c.Constants = append(c.Constants, value)
	return idx
}

// GetConstant returns the constant at the given index.
// Panics if the index is out of bounds.
func (c *Chunk) GetConstant(index uint16) string {
	return c.Constants[index]
}

// Emit appends a single-byte opcode to the code section.
func (c *Chunk) Emit(op Opcode) int {
	offset := len(c.Code)
	c.Code = append(c.Code, byte(op))
	return offset
}

// EmitWithOperand appends an opcode with operand bytes.
func (c *Chunk) EmitWithOperand(op Opcode, operands ...byte) int {
	offset := len(c.Code)
	c.Code = append(c.Code, byte(op))
	c.Code = append(c.Code, operands...)
	return offset
}

// EmitConstant emits an OpConst instruction for the given value.
// Adds the constant to the pool if not already present.
func (c *Chunk) EmitConstant(value string) int {
	idx := c.AddConstant(value)
	return c.EmitWithOperand(OpConst, byte(idx>>8), byte(idx))
}

// EmitJump emits a jump instruction with a placeholder offset.
// Returns the offset of the placeholder for later patching.
func (c *Chunk) EmitJump(op Opcode) int {
	offset := len(c.Code)
	c.Code = append(c.Code, byte(op), 0xFF, 0xFF) // Placeholder
	return offset + 1                              // Return offset of the placeholder bytes
}

// PatchJump patches a jump instruction's offset to jump to the current position.
func (c *Chunk) PatchJump(placeholderOffset int) {
	// Calculate relative jump from after the jump instruction
	jumpFrom := placeholderOffset + 2 // After the 2-byte offset
	jumpTo := len(c.Code)
	delta := jumpTo - jumpFrom

	// Encode as signed 16-bit
	c.Code[placeholderOffset] = byte(delta >> 8)
	c.Code[placeholderOffset+1] = byte(delta)
}

// PatchJumpTo patches a jump to go to a specific offset.
func (c *Chunk) PatchJumpTo(placeholderOffset int, target int) {
	jumpFrom := placeholderOffset + 2
	delta := target - jumpFrom

	c.Code[placeholderOffset] = byte(delta >> 8)
	c.Code[placeholderOffset+1] = byte(delta)
}

// EmitLoop emits a backward jump to the given loop start.
func (c *Chunk) EmitLoop(loopStart int) {
	// Jump goes backward, so delta is negative
	jumpFrom := len(c.Code) + 3 // After this instruction
	delta := loopStart - jumpFrom

	c.Code = append(c.Code, byte(OpJump))
	c.Code = append(c.Code, byte(delta>>8), byte(delta))
}

// CurrentOffset returns the current offset in the code section.
func (c *Chunk) CurrentOffset() int {
	return len(c.Code)
}

// CodeLen returns the length of the code section.
func (c *Chunk) CodeLen() int {
	return len(c.Code)
}

// ConstantCount returns the number of constants in the pool.
func (c *Chunk) ConstantCount() int {
	return len(c.Constants)
}

// AddCapture adds a capture descriptor and returns its index.
func (c *Chunk) AddCapture(name string, source VarSource, slotIndex uint8) uint8 {
	idx := uint8(len(c.CaptureInfo))
	c.CaptureInfo = append(c.CaptureInfo, CaptureDescriptor{
		Name:      name,
		Source:    source,
		SlotIndex: slotIndex,
	})
	c.Flags |= ChunkFlagHasCaptures
	return idx
}

// CaptureCount returns the number of captures.
func (c *Chunk) CaptureCount() int {
	return len(c.CaptureInfo)
}

// AddSourceLocation adds a debug source location mapping.
func (c *Chunk) AddSourceLocation(bytecodeOffset uint32, line uint32, column uint16) {
	c.Flags |= ChunkFlagDebug
	c.SourceMap = append(c.SourceMap, SourceLocation{
		BytecodeOffset: bytecodeOffset,
		Line:           line,
		Column:         column,
	})
}

// GetSourceLocation returns the source location for a bytecode offset.
// Returns line 0, column 0 if no mapping exists.
func (c *Chunk) GetSourceLocation(offset uint32) (line uint32, column uint16) {
	// Find the nearest mapping at or before the offset
	for i := len(c.SourceMap) - 1; i >= 0; i-- {
		if c.SourceMap[i].BytecodeOffset <= offset {
			return c.SourceMap[i].Line, c.SourceMap[i].Column
		}
	}
	return 0, 0
}

// Serialize encodes the chunk to bytes for storage/transport.
// Format:
//
//	[magic:4] [version:2] [flags:2]
//	[code_len:4] [code:...]
//	[const_count:2] [constants:...]
//	[param_count:1] [param_names:...]
//	[local_count:1]
//	[capture_count:1] [captures:...]
//	[debug_present:1] [debug_info:...] (if ChunkFlagDebug)
func (c *Chunk) Serialize() ([]byte, error) {
	// Estimate size: magic + header + code + constants + params + captures + debug
	estimatedSize := 8 + len(c.Code) + len(c.Constants)*32 + 100
	buf := make([]byte, 0, estimatedSize)

	// Magic number: "TTBC"
	buf = append(buf, BytecodeMagic...)

	// Version and flags
	buf = binary.BigEndian.AppendUint16(buf, c.Version)
	buf = binary.BigEndian.AppendUint16(buf, uint16(c.Flags))

	// Code section
	buf = binary.BigEndian.AppendUint32(buf, uint32(len(c.Code)))
	buf = append(buf, c.Code...)

	// Constants
	buf = binary.BigEndian.AppendUint16(buf, uint16(len(c.Constants)))
	for _, s := range c.Constants {
		buf = binary.BigEndian.AppendUint16(buf, uint16(len(s)))
		buf = append(buf, s...)
	}

	// Parameters
	buf = append(buf, c.ParamCount)
	for _, name := range c.ParamNames {
		buf = append(buf, byte(len(name)))
		buf = append(buf, name...)
	}

	// Locals
	buf = append(buf, c.LocalCount)

	// Captures
	buf = append(buf, byte(len(c.CaptureInfo)))
	for _, cap := range c.CaptureInfo {
		buf = append(buf, byte(len(cap.Name)))
		buf = append(buf, cap.Name...)
		buf = append(buf, byte(cap.Source))
		buf = append(buf, cap.SlotIndex)
	}

	// Debug info (if present)
	if c.Flags&ChunkFlagDebug != 0 {
		buf = append(buf, 1) // Debug present marker

		// Source map
		buf = binary.BigEndian.AppendUint16(buf, uint16(len(c.SourceMap)))
		for _, loc := range c.SourceMap {
			buf = binary.BigEndian.AppendUint32(buf, loc.BytecodeOffset)
			buf = binary.BigEndian.AppendUint32(buf, loc.Line)
			buf = binary.BigEndian.AppendUint16(buf, loc.Column)
		}

		// Variable names
		buf = binary.BigEndian.AppendUint16(buf, uint16(len(c.VarNames)))
		for _, name := range c.VarNames {
			buf = append(buf, byte(len(name)))
			buf = append(buf, name...)
		}
	} else {
		buf = append(buf, 0) // No debug info
	}

	return buf, nil
}

// Deserialize decodes a chunk from bytes.
func Deserialize(data []byte) (*Chunk, error) {
	if len(data) < 8 {
		return nil, fmt.Errorf("bytecode too short: need at least 8 bytes, got %d", len(data))
	}

	// Check magic
	if string(data[0:4]) != string(BytecodeMagic) {
		return nil, fmt.Errorf("invalid bytecode magic: expected %q, got %q", BytecodeMagic, data[0:4])
	}

	c := &Chunk{
		Version: binary.BigEndian.Uint16(data[4:6]),
		Flags:   ChunkFlags(binary.BigEndian.Uint16(data[6:8])),
	}

	pos := 8

	// Version check
	if c.Version > BytecodeVersion {
		return nil, fmt.Errorf("bytecode version %d is newer than supported version %d", c.Version, BytecodeVersion)
	}

	// Code section
	if pos+4 > len(data) {
		return nil, fmt.Errorf("unexpected end of bytecode reading code length at pos %d", pos)
	}
	codeLen := binary.BigEndian.Uint32(data[pos:])
	pos += 4

	if pos+int(codeLen) > len(data) {
		return nil, fmt.Errorf("unexpected end of bytecode reading code section: need %d bytes at pos %d", codeLen, pos)
	}
	c.Code = make([]byte, codeLen)
	copy(c.Code, data[pos:pos+int(codeLen)])
	pos += int(codeLen)

	// Constants
	if pos+2 > len(data) {
		return nil, fmt.Errorf("unexpected end of bytecode reading constant count")
	}
	constCount := binary.BigEndian.Uint16(data[pos:])
	pos += 2

	c.Constants = make([]string, constCount)
	for i := range c.Constants {
		if pos+2 > len(data) {
			return nil, fmt.Errorf("unexpected end of bytecode reading constant %d length", i)
		}
		strLen := binary.BigEndian.Uint16(data[pos:])
		pos += 2

		if pos+int(strLen) > len(data) {
			return nil, fmt.Errorf("unexpected end of bytecode reading constant %d", i)
		}
		c.Constants[i] = string(data[pos : pos+int(strLen)])
		pos += int(strLen)
	}

	// Parameters
	if pos >= len(data) {
		return nil, fmt.Errorf("unexpected end of bytecode reading param count")
	}
	c.ParamCount = data[pos]
	pos++

	c.ParamNames = make([]string, c.ParamCount)
	for i := range c.ParamNames {
		if pos >= len(data) {
			return nil, fmt.Errorf("unexpected end of bytecode reading param %d name length", i)
		}
		nameLen := data[pos]
		pos++

		if pos+int(nameLen) > len(data) {
			return nil, fmt.Errorf("unexpected end of bytecode reading param %d name", i)
		}
		c.ParamNames[i] = string(data[pos : pos+int(nameLen)])
		pos += int(nameLen)
	}

	// Locals
	if pos >= len(data) {
		return nil, fmt.Errorf("unexpected end of bytecode reading local count")
	}
	c.LocalCount = data[pos]
	pos++

	// Captures
	if pos >= len(data) {
		return nil, fmt.Errorf("unexpected end of bytecode reading capture count")
	}
	capCount := data[pos]
	pos++

	c.CaptureInfo = make([]CaptureDescriptor, capCount)
	for i := range c.CaptureInfo {
		if pos >= len(data) {
			return nil, fmt.Errorf("unexpected end of bytecode reading capture %d name length", i)
		}
		nameLen := data[pos]
		pos++

		if pos+int(nameLen)+2 > len(data) {
			return nil, fmt.Errorf("unexpected end of bytecode reading capture %d", i)
		}
		c.CaptureInfo[i].Name = string(data[pos : pos+int(nameLen)])
		pos += int(nameLen)

		c.CaptureInfo[i].Source = VarSource(data[pos])
		pos++

		c.CaptureInfo[i].SlotIndex = data[pos]
		pos++
	}

	// Debug info
	if pos >= len(data) {
		return nil, fmt.Errorf("unexpected end of bytecode reading debug marker")
	}
	hasDebug := data[pos]
	pos++

	if hasDebug != 0 {
		// Source map
		if pos+2 > len(data) {
			return nil, fmt.Errorf("unexpected end of bytecode reading source map count")
		}
		sourceMapLen := binary.BigEndian.Uint16(data[pos:])
		pos += 2

		c.SourceMap = make([]SourceLocation, sourceMapLen)
		for i := range c.SourceMap {
			if pos+10 > len(data) {
				return nil, fmt.Errorf("unexpected end of bytecode reading source location %d", i)
			}
			c.SourceMap[i].BytecodeOffset = binary.BigEndian.Uint32(data[pos:])
			pos += 4
			c.SourceMap[i].Line = binary.BigEndian.Uint32(data[pos:])
			pos += 4
			c.SourceMap[i].Column = binary.BigEndian.Uint16(data[pos:])
			pos += 2
		}

		// Variable names
		if pos+2 > len(data) {
			return nil, fmt.Errorf("unexpected end of bytecode reading var names count")
		}
		varNamesLen := binary.BigEndian.Uint16(data[pos:])
		pos += 2

		c.VarNames = make([]string, varNamesLen)
		for i := range c.VarNames {
			if pos >= len(data) {
				return nil, fmt.Errorf("unexpected end of bytecode reading var name %d length", i)
			}
			nameLen := data[pos]
			pos++

			if pos+int(nameLen) > len(data) {
				return nil, fmt.Errorf("unexpected end of bytecode reading var name %d", i)
			}
			c.VarNames[i] = string(data[pos : pos+int(nameLen)])
			pos += int(nameLen)
		}
	}

	return c, nil
}
