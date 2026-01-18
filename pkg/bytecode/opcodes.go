package bytecode

import "fmt"

// Opcode represents a bytecode instruction.
// Opcodes are organized into ranges by category for easy identification.
type Opcode byte

const (
	// ========================================================================
	// Stack manipulation (0x00-0x0F)
	// ========================================================================

	OpNop  Opcode = 0x00 // No operation
	OpPop  Opcode = 0x01 // Pop top of stack
	OpDup  Opcode = 0x02 // Duplicate top of stack
	OpSwap Opcode = 0x03 // Swap top two stack elements
	OpRot  Opcode = 0x04 // Rotate top three: a b c -> b c a

	// ========================================================================
	// Constants (0x10-0x1F)
	// ========================================================================

	OpConst      Opcode = 0x10 // Push constant from pool: OpConst <index:u16>
	OpConstNil   Opcode = 0x11 // Push nil/empty string
	OpConstTrue  Opcode = 0x12 // Push "true"
	OpConstFalse Opcode = 0x13 // Push "false"
	OpConstZero  Opcode = 0x14 // Push "0"
	OpConstOne   Opcode = 0x15 // Push "1"
	OpConstEmpty Opcode = 0x16 // Push ""

	// ========================================================================
	// Local variables (0x20-0x2F)
	// ========================================================================

	OpLoadLocal  Opcode = 0x20 // Push local variable: OpLoadLocal <slot:u8>
	OpStoreLocal Opcode = 0x21 // Pop and store to local: OpStoreLocal <slot:u8>
	OpLoadParam  Opcode = 0x22 // Push parameter: OpLoadParam <index:u8>

	// ========================================================================
	// Captured variables (0x30-0x3F) - for reference capture
	// ========================================================================

	OpLoadCapture  Opcode = 0x30 // Push captured variable: OpLoadCapture <index:u8>
	OpStoreCapture Opcode = 0x31 // Pop and store to capture: OpStoreCapture <index:u8>
	OpMakeCapture  Opcode = 0x32 // Create capture cell for variable

	// ========================================================================
	// Instance variables (0x40-0x4F)
	// ========================================================================

	OpLoadIVar  Opcode = 0x40 // Push ivar value: OpLoadIVar <name_index:u16>
	OpStoreIVar Opcode = 0x41 // Pop and store to ivar: OpStoreIVar <name_index:u16>

	// ========================================================================
	// Arithmetic (0x50-0x5F)
	// ========================================================================

	OpAdd Opcode = 0x50 // Pop two, push sum
	OpSub Opcode = 0x51 // Pop two, push difference (a - b where b is TOS)
	OpMul Opcode = 0x52 // Pop two, push product
	OpDiv Opcode = 0x53 // Pop two, push quotient
	OpMod Opcode = 0x54 // Pop two, push remainder
	OpNeg Opcode = 0x55 // Negate top of stack

	// ========================================================================
	// Comparison (0x60-0x6F)
	// ========================================================================

	OpEq Opcode = 0x60 // Pop two, push "true" if equal, "false" otherwise
	OpNe Opcode = 0x61 // Pop two, push "true" if not equal
	OpLt Opcode = 0x62 // Pop two, push "true" if a < b
	OpLe Opcode = 0x63 // Pop two, push "true" if a <= b
	OpGt Opcode = 0x64 // Pop two, push "true" if a > b
	OpGe Opcode = 0x65 // Pop two, push "true" if a >= b

	// ========================================================================
	// Logical operations (0x68-0x6F)
	// ========================================================================

	OpNot Opcode = 0x68 // Logical NOT: push "true" if TOS is falsy
	OpAnd Opcode = 0x69 // Logical AND (short-circuit not possible in stack machine)
	OpOr  Opcode = 0x6A // Logical OR

	// ========================================================================
	// String operations (0x70-0x7F)
	// ========================================================================

	OpConcat Opcode = 0x70 // Concatenate top two strings
	OpStrLen Opcode = 0x71 // Get string length (push as string)

	// ========================================================================
	// Control flow (0x80-0x8F)
	// ========================================================================

	OpJump       Opcode = 0x80 // Unconditional jump: OpJump <offset:i16>
	OpJumpTrue   Opcode = 0x81 // Jump if top is truthy: OpJumpTrue <offset:i16>
	OpJumpFalse  Opcode = 0x82 // Jump if top is falsy: OpJumpFalse <offset:i16>
	OpJumpNil    Opcode = 0x83 // Jump if top is nil/empty: OpJumpNil <offset:i16>
	OpJumpNotNil Opcode = 0x84 // Jump if top is not nil/empty

	// ========================================================================
	// Message sends (0x90-0x9F)
	// ========================================================================

	OpSend      Opcode = 0x90 // Send message: OpSend <selector:u16> <argc:u8>
	OpSendSelf  Opcode = 0x91 // Send to self: OpSendSelf <selector:u16> <argc:u8>
	OpSendSuper Opcode = 0x92 // Send to super: OpSendSuper <selector:u16> <argc:u8>
	OpSendClass Opcode = 0x93 // Send to class: OpSendClass <class:u16> <selector:u16> <argc:u8>

	// ========================================================================
	// Block operations (0xA0-0xAF)
	// ========================================================================

	OpMakeBlock   Opcode = 0xA0 // Create block: OpMakeBlock <codeOffset:u16> <numCaptures:u8>
	OpInvokeBlock Opcode = 0xA1 // Invoke block on stack: OpInvokeBlock <argc:u8>
	OpBlockValue  Opcode = 0xA2 // Invoke with 0 args (common case)

	// ========================================================================
	// JSON/Array operations (0xB0-0xB7)
	// ========================================================================

	OpArrayNew    Opcode = 0xB0 // Create empty array, push it
	OpArrayPush   Opcode = 0xB1 // array arrayPush: value -> modified array
	OpArrayAt     Opcode = 0xB2 // array arrayAt: index -> value
	OpArrayAtPut  Opcode = 0xB3 // array arrayAt: index put: value -> modified array
	OpArrayLen    Opcode = 0xB4 // array arrayLength -> length as string
	OpArrayFirst  Opcode = 0xB5 // array first -> first element
	OpArrayLast   Opcode = 0xB6 // array last -> last element
	OpArrayRemove Opcode = 0xB7 // array removeAt: index -> modified array

	// ========================================================================
	// JSON/Object operations (0xB8-0xBF)
	// ========================================================================

	OpObjectNew    Opcode = 0xB8 // Create empty object, push it
	OpObjectAt     Opcode = 0xB9 // object objectAt: key -> value
	OpObjectAtPut  Opcode = 0xBA // object objectAt: key put: value -> modified object
	OpObjectHasKey Opcode = 0xBB // object hasKey: key -> "true"/"false"
	OpObjectKeys   Opcode = 0xBC // object keys -> array of keys
	OpObjectValues Opcode = 0xBD // object values -> array of values
	OpObjectRemove Opcode = 0xBE // object removeKey: key -> modified object
	OpObjectLen    Opcode = 0xBF // object length -> count as string

	// ========================================================================
	// Return (0xF0-0xFF)
	// ========================================================================

	OpReturn      Opcode = 0xF0 // Return top of stack from block
	OpReturnNil   Opcode = 0xF1 // Return nil/empty string
	OpNonLocalRet Opcode = 0xF2 // Non-local return (exit enclosing method)
)

// OpcodeInfo provides metadata about each opcode for debugging and validation.
type OpcodeInfo struct {
	Name       string // Human-readable name
	StackPop   int    // How many values popped from stack (-1 = variable)
	StackPush  int    // How many values pushed to stack
	OperandLen int    // Number of operand bytes following the opcode
}

// opcodeInfoTable maps opcodes to their metadata.
var opcodeInfoTable = map[Opcode]OpcodeInfo{
	// Stack manipulation
	OpNop:  {"NOP", 0, 0, 0},
	OpPop:  {"POP", 1, 0, 0},
	OpDup:  {"DUP", 1, 2, 0},
	OpSwap: {"SWAP", 2, 2, 0},
	OpRot:  {"ROT", 3, 3, 0},

	// Constants
	OpConst:      {"CONST", 0, 1, 2},
	OpConstNil:   {"CONST_NIL", 0, 1, 0},
	OpConstTrue:  {"CONST_TRUE", 0, 1, 0},
	OpConstFalse: {"CONST_FALSE", 0, 1, 0},
	OpConstZero:  {"CONST_ZERO", 0, 1, 0},
	OpConstOne:   {"CONST_ONE", 0, 1, 0},
	OpConstEmpty: {"CONST_EMPTY", 0, 1, 0},

	// Local variables
	OpLoadLocal:  {"LOAD_LOCAL", 0, 1, 1},
	OpStoreLocal: {"STORE_LOCAL", 1, 0, 1},
	OpLoadParam:  {"LOAD_PARAM", 0, 1, 1},

	// Captures
	OpLoadCapture:  {"LOAD_CAPTURE", 0, 1, 1},
	OpStoreCapture: {"STORE_CAPTURE", 1, 0, 1},
	OpMakeCapture:  {"MAKE_CAPTURE", 1, 1, 0},

	// Instance variables
	OpLoadIVar:  {"LOAD_IVAR", 0, 1, 2},
	OpStoreIVar: {"STORE_IVAR", 1, 0, 2},

	// Arithmetic
	OpAdd: {"ADD", 2, 1, 0},
	OpSub: {"SUB", 2, 1, 0},
	OpMul: {"MUL", 2, 1, 0},
	OpDiv: {"DIV", 2, 1, 0},
	OpMod: {"MOD", 2, 1, 0},
	OpNeg: {"NEG", 1, 1, 0},

	// Comparison
	OpEq: {"EQ", 2, 1, 0},
	OpNe: {"NE", 2, 1, 0},
	OpLt: {"LT", 2, 1, 0},
	OpLe: {"LE", 2, 1, 0},
	OpGt: {"GT", 2, 1, 0},
	OpGe: {"GE", 2, 1, 0},

	// Logical
	OpNot: {"NOT", 1, 1, 0},
	OpAnd: {"AND", 2, 1, 0},
	OpOr:  {"OR", 2, 1, 0},

	// String
	OpConcat: {"CONCAT", 2, 1, 0},
	OpStrLen: {"STRLEN", 1, 1, 0},

	// Control flow
	OpJump:       {"JUMP", 0, 0, 2},
	OpJumpTrue:   {"JUMP_TRUE", 1, 0, 2},
	OpJumpFalse:  {"JUMP_FALSE", 1, 0, 2},
	OpJumpNil:    {"JUMP_NIL", 1, 0, 2},
	OpJumpNotNil: {"JUMP_NOT_NIL", 1, 0, 2},

	// Message sends
	OpSend:      {"SEND", -1, 1, 3},      // Pops receiver + argc args
	OpSendSelf:  {"SEND_SELF", -1, 1, 3}, // Pops argc args (self is implicit)
	OpSendSuper: {"SEND_SUPER", -1, 1, 3},
	OpSendClass: {"SEND_CLASS", -1, 1, 5}, // class:u16 + selector:u16 + argc:u8

	// Blocks
	OpMakeBlock:   {"MAKE_BLOCK", -1, 1, 3}, // Pops numCaptures values
	OpInvokeBlock: {"INVOKE_BLOCK", -1, 1, 1},
	OpBlockValue:  {"BLOCK_VALUE", 1, 1, 0},

	// Array operations
	OpArrayNew:    {"ARRAY_NEW", 0, 1, 0},
	OpArrayPush:   {"ARRAY_PUSH", 2, 1, 0},
	OpArrayAt:     {"ARRAY_AT", 2, 1, 0},
	OpArrayAtPut:  {"ARRAY_AT_PUT", 3, 1, 0},
	OpArrayLen:    {"ARRAY_LEN", 1, 1, 0},
	OpArrayFirst:  {"ARRAY_FIRST", 1, 1, 0},
	OpArrayLast:   {"ARRAY_LAST", 1, 1, 0},
	OpArrayRemove: {"ARRAY_REMOVE", 2, 1, 0},

	// Object operations
	OpObjectNew:    {"OBJECT_NEW", 0, 1, 0},
	OpObjectAt:     {"OBJECT_AT", 2, 1, 0},
	OpObjectAtPut:  {"OBJECT_AT_PUT", 3, 1, 0},
	OpObjectHasKey: {"OBJECT_HAS_KEY", 2, 1, 0},
	OpObjectKeys:   {"OBJECT_KEYS", 1, 1, 0},
	OpObjectValues: {"OBJECT_VALUES", 1, 1, 0},
	OpObjectRemove: {"OBJECT_REMOVE", 2, 1, 0},
	OpObjectLen:    {"OBJECT_LEN", 1, 1, 0},

	// Return
	OpReturn:      {"RETURN", 1, 0, 0},
	OpReturnNil:   {"RETURN_NIL", 0, 0, 0},
	OpNonLocalRet: {"NON_LOCAL_RET", 1, 0, 0},
}

// GetOpcodeInfo returns metadata for an opcode.
// Returns a zero OpcodeInfo with name "UNKNOWN" if the opcode is not recognized.
func GetOpcodeInfo(op Opcode) OpcodeInfo {
	if info, ok := opcodeInfoTable[op]; ok {
		return info
	}
	return OpcodeInfo{Name: fmt.Sprintf("UNKNOWN(0x%02X)", byte(op)), StackPop: 0, StackPush: 0, OperandLen: 0}
}

// String returns the human-readable name of an opcode.
func (op Opcode) String() string {
	return GetOpcodeInfo(op).Name
}

// OperandLen returns the number of operand bytes for this opcode.
func (op Opcode) OperandLen() int {
	return GetOpcodeInfo(op).OperandLen
}

// InstructionLen returns the total length of an instruction (1 + operand bytes).
func (op Opcode) InstructionLen() int {
	return 1 + op.OperandLen()
}

// IsJump returns true if this opcode is a jump instruction.
func (op Opcode) IsJump() bool {
	return op >= OpJump && op <= OpJumpNotNil
}

// IsReturn returns true if this opcode terminates execution.
func (op Opcode) IsReturn() bool {
	return op >= OpReturn && op <= OpNonLocalRet
}

// IsSend returns true if this opcode is a message send.
func (op Opcode) IsSend() bool {
	return op >= OpSend && op <= OpSendClass
}

// IsBlockOp returns true if this opcode operates on blocks.
func (op Opcode) IsBlockOp() bool {
	return op >= OpMakeBlock && op <= OpBlockValue
}

// AllOpcodes returns a slice of all defined opcodes.
// Useful for testing that all opcodes have metadata.
func AllOpcodes() []Opcode {
	opcodes := make([]Opcode, 0, len(opcodeInfoTable))
	for op := range opcodeInfoTable {
		opcodes = append(opcodes, op)
	}
	return opcodes
}

// OpcodeCount returns the number of defined opcodes.
func OpcodeCount() int {
	return len(opcodeInfoTable)
}
