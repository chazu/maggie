package vm

import (
	"encoding/binary"
	"fmt"
	"math"
)

// ---------------------------------------------------------------------------
// Opcode definitions
// ---------------------------------------------------------------------------

// Opcode represents a single bytecode instruction.
type Opcode byte

// Stack Operations
const (
	OpNOP Opcode = 0x00 // no operation
	OpPOP Opcode = 0x01 // discard top of stack
	OpDUP Opcode = 0x02 // duplicate top of stack
)

// Push Constants
const (
	OpPushNil     Opcode = 0x10 // push nil
	OpPushTrue    Opcode = 0x11 // push true
	OpPushFalse   Opcode = 0x12 // push false
	OpPushSelf    Opcode = 0x13 // push self
	OpPushInt8    Opcode = 0x14 // push 8-bit signed integer
	OpPushInt32   Opcode = 0x15 // push 32-bit signed integer
	OpPushLiteral Opcode = 0x16 // push literal from literal frame (16-bit index)
	OpPushFloat   Opcode = 0x17 // push inline float64 (8 bytes)
	OpPushContext Opcode = 0x18 // push thisContext (current activation)
)

// Variable Operations
const (
	OpPushTemp      Opcode = 0x20 // push temporary/argument (8-bit index)
	OpPushIvar      Opcode = 0x21 // push instance variable (8-bit index)
	OpPushGlobal    Opcode = 0x22 // push global/class (16-bit index)
	OpStoreTemp     Opcode = 0x23 // store into temporary (8-bit index)
	OpStoreIvar     Opcode = 0x24 // store into instance variable (8-bit index)
	OpStoreGlobal   Opcode = 0x25 // store into global (16-bit index)
	OpPushCaptured  Opcode = 0x26 // push captured variable (8-bit index)
	OpStoreCaptured Opcode = 0x27 // store into captured variable (8-bit index)
)

// Message Sends
const (
	OpSend      Opcode = 0x30 // send message (16-bit selector, 8-bit argc)
	OpSendSuper Opcode = 0x31 // send to super (16-bit selector, 8-bit argc)
)

// Optimized Sends (single-byte, no operands)
const (
	OpSendPlus   Opcode = 0x40 // +
	OpSendMinus  Opcode = 0x41 // -
	OpSendTimes  Opcode = 0x42 // *
	OpSendDiv    Opcode = 0x43 // /
	OpSendMod    Opcode = 0x44 // \\
	OpSendLT     Opcode = 0x45 // <
	OpSendGT     Opcode = 0x46 // >
	OpSendLE     Opcode = 0x47 // <=
	OpSendGE     Opcode = 0x48 // >=
	OpSendEQ     Opcode = 0x49 // =
	OpSendNE     Opcode = 0x4A // ~=
	OpSendAt     Opcode = 0x4B // at:
	OpSendAtPut  Opcode = 0x4C // at:put:
	OpSendSize   Opcode = 0x4D // size
	OpSendValue  Opcode = 0x4E // value
	OpSendValue1 Opcode = 0x4F // value:
	OpSendValue2 Opcode = 0x50 // value:value:
	OpSendNew    Opcode = 0x51 // new
	OpSendClass  Opcode = 0x52 // class
)

// Control Flow
const (
	OpJump       Opcode = 0x60 // unconditional jump (16-bit offset)
	OpJumpTrue   Opcode = 0x61 // pop, jump if true (16-bit offset)
	OpJumpFalse  Opcode = 0x62 // pop, jump if false (16-bit offset)
	OpJumpNil    Opcode = 0x63 // pop, jump if nil (16-bit offset)
	OpJumpNotNil Opcode = 0x64 // pop, jump if not nil (16-bit offset)
)

// Returns
const (
	OpReturnTop   Opcode = 0x70 // return top of stack
	OpReturnSelf  Opcode = 0x71 // return self
	OpReturnNil   Opcode = 0x72 // return nil
	OpBlockReturn Opcode = 0x73 // non-local return from block
)

// Blocks
const (
	OpCreateBlock Opcode = 0x80 // create block (16-bit method index, 8-bit capture count)
	OpCaptureTemp Opcode = 0x81 // capture temporary into block (8-bit index)
	OpCaptureIvar Opcode = 0x82 // capture ivar into block (8-bit index)
)

// Object Creation
const (
	OpCreateArray  Opcode = 0x90 // create array from stack (8-bit size)
	OpCreateObject Opcode = 0x91 // create object (16-bit class, 8-bit slot count)
)

// ---------------------------------------------------------------------------
// Opcode metadata
// ---------------------------------------------------------------------------

// OpcodeInfo holds metadata about an opcode.
type OpcodeInfo struct {
	Name         string // human-readable name
	OperandBytes int    // number of operand bytes
	StackEffect  int    // net effect on stack (-1 = variable)
}

// opcodeTable maps opcodes to their metadata.
var opcodeTable = map[Opcode]OpcodeInfo{
	// Stack operations
	OpNOP: {"NOP", 0, 0},
	OpPOP: {"POP", 0, -1},
	OpDUP: {"DUP", 0, 1},

	// Push constants
	OpPushNil:     {"PUSH_NIL", 0, 1},
	OpPushTrue:    {"PUSH_TRUE", 0, 1},
	OpPushFalse:   {"PUSH_FALSE", 0, 1},
	OpPushSelf:    {"PUSH_SELF", 0, 1},
	OpPushInt8:    {"PUSH_INT8", 1, 1},
	OpPushInt32:   {"PUSH_INT32", 4, 1},
	OpPushLiteral: {"PUSH_LITERAL", 2, 1},
	OpPushFloat:   {"PUSH_FLOAT", 8, 1},

	// Variables
	OpPushTemp:      {"PUSH_TEMP", 1, 1},
	OpPushIvar:      {"PUSH_IVAR", 1, 1},
	OpPushGlobal:    {"PUSH_GLOBAL", 2, 1},
	OpStoreTemp:     {"STORE_TEMP", 1, 0},
	OpStoreIvar:     {"STORE_IVAR", 1, 0},
	OpStoreGlobal:   {"STORE_GLOBAL", 2, 0},
	OpPushCaptured:  {"PUSH_CAPTURED", 1, 1},
	OpStoreCaptured: {"STORE_CAPTURED", 1, 0},

	// Sends
	OpSend:      {"SEND", 3, -1}, // variable: pops receiver + args, pushes result
	OpSendSuper: {"SEND_SUPER", 3, -1},

	// Optimized sends
	OpSendPlus:   {"SEND_PLUS", 0, -1},   // pops 2, pushes 1
	OpSendMinus:  {"SEND_MINUS", 0, -1},  // pops 2, pushes 1
	OpSendTimes:  {"SEND_TIMES", 0, -1},  // pops 2, pushes 1
	OpSendDiv:    {"SEND_DIV", 0, -1},    // pops 2, pushes 1
	OpSendMod:    {"SEND_MOD", 0, -1},    // pops 2, pushes 1
	OpSendLT:     {"SEND_LT", 0, -1},     // pops 2, pushes 1
	OpSendGT:     {"SEND_GT", 0, -1},     // pops 2, pushes 1
	OpSendLE:     {"SEND_LE", 0, -1},     // pops 2, pushes 1
	OpSendGE:     {"SEND_GE", 0, -1},     // pops 2, pushes 1
	OpSendEQ:     {"SEND_EQ", 0, -1},     // pops 2, pushes 1
	OpSendNE:     {"SEND_NE", 0, -1},     // pops 2, pushes 1
	OpSendAt:     {"SEND_AT", 0, -1},     // pops 2, pushes 1
	OpSendAtPut:  {"SEND_AT_PUT", 0, -2}, // pops 3, pushes 1
	OpSendSize:   {"SEND_SIZE", 0, 0},    // pops 1, pushes 1
	OpSendValue:  {"SEND_VALUE", 0, 0},   // pops 1, pushes 1
	OpSendValue1: {"SEND_VALUE1", 0, -1}, // pops 2, pushes 1
	OpSendValue2: {"SEND_VALUE2", 0, -2}, // pops 3, pushes 1
	OpSendNew:    {"SEND_NEW", 0, 0},     // pops 1, pushes 1
	OpSendClass:  {"SEND_CLASS", 0, 0},   // pops 1, pushes 1

	// Control flow
	OpJump:       {"JUMP", 2, 0},
	OpJumpTrue:   {"JUMP_TRUE", 2, -1},
	OpJumpFalse:  {"JUMP_FALSE", 2, -1},
	OpJumpNil:    {"JUMP_NIL", 2, -1},
	OpJumpNotNil: {"JUMP_NOT_NIL", 2, -1},

	// Returns
	OpReturnTop:   {"RETURN_TOP", 0, -1},
	OpReturnSelf:  {"RETURN_SELF", 0, 0},
	OpReturnNil:   {"RETURN_NIL", 0, 0},
	OpBlockReturn: {"BLOCK_RETURN", 0, -1},

	// Blocks
	OpCreateBlock: {"CREATE_BLOCK", 3, 1},
	OpCaptureTemp: {"CAPTURE_TEMP", 1, 0},
	OpCaptureIvar: {"CAPTURE_IVAR", 1, 0},

	// Object creation
	OpCreateArray:  {"CREATE_ARRAY", 1, -1},  // variable: pops N items
	OpCreateObject: {"CREATE_OBJECT", 3, -1}, // variable: pops N items
}

// Info returns the metadata for an opcode.
func (op Opcode) Info() OpcodeInfo {
	if info, ok := opcodeTable[op]; ok {
		return info
	}
	return OpcodeInfo{Name: fmt.Sprintf("UNKNOWN_%02X", byte(op)), OperandBytes: 0, StackEffect: 0}
}

// Name returns the human-readable name for an opcode.
func (op Opcode) Name() string {
	return op.Info().Name
}

// OperandBytes returns the number of operand bytes for an opcode.
func (op Opcode) OperandBytes() int {
	return op.Info().OperandBytes
}

// String implements the Stringer interface.
func (op Opcode) String() string {
	return op.Name()
}

// ---------------------------------------------------------------------------
// BytecodeBuilder: Helper for constructing bytecode
// ---------------------------------------------------------------------------

// BytecodeBuilder helps construct bytecode sequences.
type BytecodeBuilder struct {
	bytes []byte
}

// NewBytecodeBuilder creates a new bytecode builder.
func NewBytecodeBuilder() *BytecodeBuilder {
	return &BytecodeBuilder{
		bytes: make([]byte, 0, 64),
	}
}

// Bytes returns the constructed bytecode.
func (b *BytecodeBuilder) Bytes() []byte {
	return b.bytes
}

// Len returns the current length.
func (b *BytecodeBuilder) Len() int {
	return len(b.bytes)
}

// Emit appends an opcode with no operands.
func (b *BytecodeBuilder) Emit(op Opcode) {
	b.bytes = append(b.bytes, byte(op))
}

// EmitRaw appends a raw byte to the bytecode.
func (b *BytecodeBuilder) EmitRaw(data byte) {
	b.bytes = append(b.bytes, data)
}

// EmitByte appends an opcode with a single byte operand.
func (b *BytecodeBuilder) EmitByte(op Opcode, operand byte) {
	b.bytes = append(b.bytes, byte(op), operand)
}

// EmitInt8 appends an opcode with a signed 8-bit operand.
func (b *BytecodeBuilder) EmitInt8(op Opcode, operand int8) {
	b.bytes = append(b.bytes, byte(op), byte(operand))
}

// EmitUint16 appends an opcode with a 16-bit operand (little-endian).
func (b *BytecodeBuilder) EmitUint16(op Opcode, operand uint16) {
	b.bytes = append(b.bytes, byte(op), byte(operand), byte(operand>>8))
}

// EmitInt32 appends an opcode with a 32-bit operand (little-endian).
func (b *BytecodeBuilder) EmitInt32(op Opcode, operand int32) {
	b.bytes = append(b.bytes, byte(op))
	var buf [4]byte
	binary.LittleEndian.PutUint32(buf[:], uint32(operand))
	b.bytes = append(b.bytes, buf[:]...)
}

// EmitFloat64 appends an opcode with a 64-bit float operand.
func (b *BytecodeBuilder) EmitFloat64(op Opcode, operand float64) {
	b.bytes = append(b.bytes, byte(op))
	var buf [8]byte
	binary.LittleEndian.PutUint64(buf[:], math.Float64bits(operand))
	b.bytes = append(b.bytes, buf[:]...)
}

// EmitSend appends a SEND or SEND_SUPER instruction.
func (b *BytecodeBuilder) EmitSend(op Opcode, selector uint16, argc uint8) {
	b.bytes = append(b.bytes, byte(op), byte(selector), byte(selector>>8), argc)
}

// EmitCreateBlock appends a CREATE_BLOCK instruction.
func (b *BytecodeBuilder) EmitCreateBlock(methodIndex uint16, nCaptures uint8) {
	b.bytes = append(b.bytes, byte(OpCreateBlock), byte(methodIndex), byte(methodIndex>>8), nCaptures)
}

// EmitCreateObject appends a CREATE_OBJECT instruction.
func (b *BytecodeBuilder) EmitCreateObject(classIndex uint16, nSlots uint8) {
	b.bytes = append(b.bytes, byte(OpCreateObject), byte(classIndex), byte(classIndex>>8), nSlots)
}

// ---------------------------------------------------------------------------
// Label management for jumps
// ---------------------------------------------------------------------------

// Label represents a forward reference in bytecode.
type Label struct {
	resolved bool
	position int    // position to patch (if unresolved) or target (if resolved)
	refs     []int  // positions that reference this label
}

// NewLabel creates an unresolved label.
func (b *BytecodeBuilder) NewLabel() *Label {
	return &Label{resolved: false, refs: make([]int, 0, 2)}
}

// Mark resolves a label to the current position.
func (b *BytecodeBuilder) Mark(label *Label) {
	if label.resolved {
		panic("label already resolved")
	}
	label.resolved = true
	label.position = len(b.bytes)

	// Patch all forward references
	for _, ref := range label.refs {
		offset := label.position - (ref + 2) // offset from after the operand
		b.bytes[ref] = byte(offset)
		b.bytes[ref+1] = byte(offset >> 8)
	}
	label.refs = nil
}

// EmitJump emits a jump instruction with a label.
func (b *BytecodeBuilder) EmitJump(op Opcode, label *Label) {
	b.bytes = append(b.bytes, byte(op))
	if label.resolved {
		// Backward jump: calculate offset
		offset := label.position - (len(b.bytes) + 2)
		b.bytes = append(b.bytes, byte(offset), byte(offset>>8))
	} else {
		// Forward jump: record position for later patching
		label.refs = append(label.refs, len(b.bytes))
		b.bytes = append(b.bytes, 0, 0) // placeholder
	}
}

// EmitJumpAbsolute emits a jump to an absolute position (for backward jumps).
func (b *BytecodeBuilder) EmitJumpAbsolute(op Opcode, target int) {
	b.bytes = append(b.bytes, byte(op))
	offset := target - (len(b.bytes) + 2)
	b.bytes = append(b.bytes, byte(offset), byte(offset>>8))
}

// ---------------------------------------------------------------------------
// Bytecode reader for disassembly
// ---------------------------------------------------------------------------

// BytecodeReader reads bytecode for interpretation or disassembly.
type BytecodeReader struct {
	bytes []byte
	pos   int
}

// NewBytecodeReader creates a reader for bytecode.
func NewBytecodeReader(bc []byte) *BytecodeReader {
	return &BytecodeReader{bytes: bc, pos: 0}
}

// Position returns the current read position.
func (r *BytecodeReader) Position() int {
	return r.pos
}

// HasMore returns true if there are more bytes to read.
func (r *BytecodeReader) HasMore() bool {
	return r.pos < len(r.bytes)
}

// ReadOpcode reads and returns the next opcode.
func (r *BytecodeReader) ReadOpcode() Opcode {
	if r.pos >= len(r.bytes) {
		panic("bytecode underflow")
	}
	op := Opcode(r.bytes[r.pos])
	r.pos++
	return op
}

// ReadByte reads a single byte operand.
func (r *BytecodeReader) ReadByte() byte {
	if r.pos >= len(r.bytes) {
		panic("bytecode underflow")
	}
	b := r.bytes[r.pos]
	r.pos++
	return b
}

// ReadInt8 reads a signed 8-bit operand.
func (r *BytecodeReader) ReadInt8() int8 {
	return int8(r.ReadByte())
}

// ReadUint16 reads a 16-bit operand (little-endian).
func (r *BytecodeReader) ReadUint16() uint16 {
	if r.pos+2 > len(r.bytes) {
		panic("bytecode underflow")
	}
	v := binary.LittleEndian.Uint16(r.bytes[r.pos:])
	r.pos += 2
	return v
}

// ReadInt16 reads a signed 16-bit operand (little-endian).
func (r *BytecodeReader) ReadInt16() int16 {
	return int16(r.ReadUint16())
}

// ReadInt32 reads a 32-bit operand (little-endian).
func (r *BytecodeReader) ReadInt32() int32 {
	if r.pos+4 > len(r.bytes) {
		panic("bytecode underflow")
	}
	v := binary.LittleEndian.Uint32(r.bytes[r.pos:])
	r.pos += 4
	return int32(v)
}

// ReadFloat64 reads a 64-bit float operand.
func (r *BytecodeReader) ReadFloat64() float64 {
	if r.pos+8 > len(r.bytes) {
		panic("bytecode underflow")
	}
	bits := binary.LittleEndian.Uint64(r.bytes[r.pos:])
	r.pos += 8
	return math.Float64frombits(bits)
}

// Skip advances the position by n bytes.
func (r *BytecodeReader) Skip(n int) {
	r.pos += n
}

// Seek sets the read position.
func (r *BytecodeReader) Seek(pos int) {
	r.pos = pos
}

// ---------------------------------------------------------------------------
// Disassembly
// ---------------------------------------------------------------------------

// DisassembleInstruction disassembles a single instruction at the reader's position.
// Returns the string representation and advances the reader.
func DisassembleInstruction(r *BytecodeReader) string {
	pos := r.Position()
	op := r.ReadOpcode()
	info := op.Info()

	switch op {
	// No operands
	case OpNOP, OpPOP, OpDUP, OpPushNil, OpPushTrue, OpPushFalse, OpPushSelf:
		return fmt.Sprintf("%04d  %s", pos, info.Name)

	case OpSendPlus, OpSendMinus, OpSendTimes, OpSendDiv, OpSendMod,
		OpSendLT, OpSendGT, OpSendLE, OpSendGE, OpSendEQ, OpSendNE,
		OpSendAt, OpSendAtPut, OpSendSize, OpSendValue, OpSendValue1, OpSendValue2,
		OpSendNew, OpSendClass:
		return fmt.Sprintf("%04d  %s", pos, info.Name)

	case OpReturnTop, OpReturnSelf, OpReturnNil, OpBlockReturn:
		return fmt.Sprintf("%04d  %s", pos, info.Name)

	// 8-bit operand
	case OpPushInt8:
		v := r.ReadInt8()
		return fmt.Sprintf("%04d  %s %d", pos, info.Name, v)

	case OpPushTemp, OpPushIvar, OpStoreTemp, OpStoreIvar, OpPushCaptured, OpStoreCaptured:
		idx := r.ReadByte()
		return fmt.Sprintf("%04d  %s %d", pos, info.Name, idx)

	case OpCaptureTemp, OpCaptureIvar:
		idx := r.ReadByte()
		return fmt.Sprintf("%04d  %s %d", pos, info.Name, idx)

	case OpCreateArray:
		size := r.ReadByte()
		return fmt.Sprintf("%04d  %s %d", pos, info.Name, size)

	// 16-bit operand
	case OpPushLiteral, OpPushGlobal, OpStoreGlobal:
		idx := r.ReadUint16()
		return fmt.Sprintf("%04d  %s %d", pos, info.Name, idx)

	case OpJump, OpJumpTrue, OpJumpFalse, OpJumpNil, OpJumpNotNil:
		offset := r.ReadInt16()
		target := r.Position() + int(offset)
		return fmt.Sprintf("%04d  %s %d (-> %04d)", pos, info.Name, offset, target)

	// 32-bit operand
	case OpPushInt32:
		v := r.ReadInt32()
		return fmt.Sprintf("%04d  %s %d", pos, info.Name, v)

	// 64-bit operand
	case OpPushFloat:
		v := r.ReadFloat64()
		return fmt.Sprintf("%04d  %s %f", pos, info.Name, v)

	// Complex operands
	case OpSend, OpSendSuper:
		selector := r.ReadUint16()
		argc := r.ReadByte()
		return fmt.Sprintf("%04d  %s selector=%d argc=%d", pos, info.Name, selector, argc)

	case OpCreateBlock:
		methodIdx := r.ReadUint16()
		nCaptures := r.ReadByte()
		return fmt.Sprintf("%04d  %s method=%d captures=%d", pos, info.Name, methodIdx, nCaptures)

	case OpCreateObject:
		classIdx := r.ReadUint16()
		nSlots := r.ReadByte()
		return fmt.Sprintf("%04d  %s class=%d slots=%d", pos, info.Name, classIdx, nSlots)

	default:
		// Skip unknown operands
		r.Skip(info.OperandBytes)
		return fmt.Sprintf("%04d  %s", pos, info.Name)
	}
}

// Disassemble returns a full disassembly of bytecode.
func Disassemble(bc []byte) string {
	r := NewBytecodeReader(bc)
	var result string
	for r.HasMore() {
		if result != "" {
			result += "\n"
		}
		result += DisassembleInstruction(r)
	}
	return result
}
