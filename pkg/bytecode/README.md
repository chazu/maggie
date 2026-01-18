# pkg/bytecode - Developer Guide

This package provides a stack-based virtual machine for executing Trashtalk blocks natively in Go.

## Architecture Overview

```
parser.BlockExpr → Compiler → Chunk → VM → result string
```

### Components

| Component | File | Purpose |
|-----------|------|---------|
| `Opcode` | opcodes.go | Bytecode instruction set (~50 opcodes) |
| `Chunk` | chunk.go | Compiled bytecode container |
| `Compiler` | compiler.go | AST → bytecode compilation |
| `VM` | vm.go | Stack-based bytecode interpreter |
| `CaptureCell` | vm.go | Reference-captured variable holder |

## Opcode Reference

Opcodes are organized by category with assigned ranges:

| Range | Category | Key Opcodes |
|-------|----------|-------------|
| 0x00-0x0F | Stack | NOP, POP, DUP, SWAP, ROT |
| 0x10-0x1F | Constants | CONST, CONST_NIL, CONST_TRUE, CONST_ZERO |
| 0x20-0x2F | Locals | LOAD_LOCAL, STORE_LOCAL, LOAD_PARAM |
| 0x30-0x3F | Captures | LOAD_CAPTURE, STORE_CAPTURE, MAKE_CAPTURE |
| 0x40-0x4F | Ivars | LOAD_IVAR, STORE_IVAR |
| 0x50-0x5F | Arithmetic | ADD, SUB, MUL, DIV, MOD, NEG |
| 0x60-0x6F | Comparison | EQ, NE, LT, LE, GT, GE, NOT, AND, OR |
| 0x70-0x7F | Strings | CONCAT, STRLEN |
| 0x80-0x8F | Control | JUMP, JUMP_TRUE, JUMP_FALSE, JUMP_NIL |
| 0x90-0x9F | Sends | SEND, SEND_SELF, SEND_SUPER, SEND_CLASS |
| 0xA0-0xAF | Blocks | MAKE_BLOCK, INVOKE_BLOCK, BLOCK_VALUE |
| 0xB0-0xBF | JSON | ARRAY_*, OBJECT_* operations |
| 0xF0-0xFF | Return | RETURN, RETURN_NIL, NON_LOCAL_RET |

### Operand Encoding

Instructions have variable length operands:

```
OpConst <index:u16>          // 3 bytes total
OpLoadLocal <slot:u8>        // 2 bytes total
OpJump <offset:i16>          // 3 bytes total (signed offset)
OpSend <selector:u16> <argc:u8>  // 4 bytes total
```

## Chunk Structure

```go
type Chunk struct {
    Version      uint16               // Format version (currently 1)
    Flags        ChunkFlags           // Debug, HasCaptures, etc.
    Code         []byte               // Bytecode instructions
    Constants    []string             // String constant pool
    ParamCount   uint8                // Number of parameters
    ParamNames   []string             // Parameter names
    LocalCount   uint8                // Local variable slots
    CaptureInfo  []CaptureDescriptor  // Capture metadata
    SourceMap    []SourceLocation     // Debug info (optional)
}
```

### Chunk Flags

- `ChunkFlagDebug` (0x01): Source map is present
- `ChunkFlagHasCaptures` (0x02): Block captures variables
- `ChunkFlagHasNonLocalReturn` (0x04): May perform non-local return

## Serialization Format (TTBC)

Binary format with big-endian encoding:

```
Header:
  [0:4]   Magic "TTBC"
  [4:6]   Version (uint16)
  [6:8]   Flags (uint16)

Code Section:
  [8:12]  Code length (uint32)
  [12:n]  Code bytes

Constants:
  [n:n+2] Count (uint16)
  For each constant:
    [2]   Length (uint16)
    [...]  UTF-8 bytes

Parameters:
  [1]     ParamCount (uint8)
  For each param:
    [1]   Name length (uint8)
    [...] Name bytes

Locals:
  [1]     LocalCount (uint8)

Captures:
  [1]     Count (uint8)
  For each capture:
    [1]   Name length (uint8)
    [...] Name bytes
    [1]   Source (VarSource)
    [1]   SlotIndex (uint8)

Debug (if ChunkFlagDebug):
  [4]     Count (uint32)
  For each entry:
    [4]   BytecodeOffset (uint32)
    [4]   Line (uint32)
    [2]   Column (uint16)
```

## Compiler

### Entry Point

```go
chunk, err := CompileBlock(block *parser.BlockExpr, ctx *CompilerContext)
```

### Capture Analysis

The compiler automatically identifies free variables:

1. Variables used but not declared locally → captured
2. Instance variable access → captured with VarSourceIVar
3. Nested block captures → propagated

### Compilation Context

```go
type CompilerContext struct {
    ClassName   string  // Receiver class name
    MethodName  string  // Enclosing method
    EnableDebug bool    // Include source maps
}
```

## VM Execution

### Basic Execution

```go
vm := NewVM()
result, err := vm.Execute(chunk, instanceID, args)
```

### With Captures

```go
captures := []*CaptureCell{
    {Name: "x", Value: "10", Source: VarSourceLocal},
}
result, err := vm.ExecuteWithCaptures(chunk, instanceID, args, captures)
```

### CaptureCell

Captures are reference cells that enable write-back:

```go
type CaptureCell struct {
    Value   string     // Current value
    Name    string     // Variable name
    Source  VarSource  // Where to persist changes
}
```

When a capture's `Source` is `VarSourceIVar`, changes are written back to SQLite via the instance storage layer.

## Adding New Opcodes

1. Add constant in `opcodes.go`:
   ```go
   OpMyNew Opcode = 0xNN
   ```

2. Add metadata in `opcodeInfoTable`:
   ```go
   OpMyNew: {"MY_NEW", stackPop, stackPush, operandLen},
   ```

3. Add compiler support in `compiler.go`

4. Add VM execution in `vm.go`:
   ```go
   case OpMyNew:
       // Implementation
   ```

5. Add tests in `compiler_test.go` and `vm_test.go`

## Testing

```bash
# Unit tests
go test ./pkg/bytecode/...

# With coverage
go test -cover ./pkg/bytecode/...

# Benchmarks
go test -bench=. ./pkg/bytecode/...
```

## Key Design Decisions

### String-Based Values

All stack values are strings. This matches Bash semantics and simplifies integration with the existing runtime. Type coercion happens implicitly in arithmetic/comparison operations.

### Reference Capture

Variables are captured by reference, not value. Changes made inside a block are visible outside. This matches Smalltalk semantics.

### No Garbage Collection

The VM uses Go's GC for memory management. Stack and local arrays are pre-allocated to minimize allocations during hot loops.

### Parameter Shadowing

Assignment to a parameter creates a new local variable that shadows it. The original parameter value is preserved. Use explicit local copy:
```smalltalk
[ :input | n := input. "now n can be modified" ]
```

## Integration Points

### trashtalk-daemon

The daemon maintains a block registry for cross-process invocation:

- `register`: Store compiled bytecode, return block ID
- `invoke`: Execute block by ID with arguments
- `serialize`: Export block for storage

### Code Generator

Static blocks are pre-compiled and embedded as byte slices in generated Go code:

```go
var myBlock = []byte{0x54, 0x54, 0x42, 0x43, ...}
```

## See Also

- `doc.go` - Package overview and capture semantics
- `../codegen/` - Integration with code generator
- `../../docs/BYTECODE_BLOCKS.md` - User guide
