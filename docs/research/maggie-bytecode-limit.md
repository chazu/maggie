# Maggie Bytecode Limits: Silent Truncation via Narrowing Casts

**Date:** 2026-04-06
**Scout:** research agent
**Status:** Finding confirmed

## Summary

The Maggie compiler has **no explicit bytecode size limit**, but has multiple **implicit limits** from narrowing integer casts that silently truncate values when exceeded. The `Bytecode []byte` field on `CompiledMethod` and `BlockMethod` is an unbounded Go slice, so there is no hard cap on total bytecode length. However, the operand encoding uses fixed-width integers (uint8, uint16, int16) that the compiler casts to **without bounds checking**, causing silent wrap-around/truncation.

## Where Bytecode Is Stored

- **`vm/compiled_method.go:24`** — `Bytecode []byte` (Go slice, no cap)
- **`vm/compiled_method.go:57`** — `BlockMethod.Bytecode []byte` (same)
- **`vm/bytecode.go:240`** — `BytecodeBuilder.bytes []byte` (growable slice, initial cap 64)

There is no fixed-size array. The slice grows without bound.

## The Real Limits: Silent Narrowing Casts in `compiler/codegen.go`

The compiler converts Go `int` values to fixed-width operand types using bare casts like `uint16(idx)` and `byte(idx)` with **zero bounds checking**. When values exceed the representable range, Go silently truncates. This causes the bytecode to reference the **wrong** literal, temp, selector, or array size.

### Limit 1: Literal Pool Index — 65,535 max (uint16)

**Location:** `compiler/codegen.go:418,425,431,436,442,537,628`

```go
idx := c.addLiteral(v)
c.builder.EmitUint16(vm.OpPushLiteral, uint16(idx))  // SILENT TRUNCATION if idx > 65535
```

`addLiteral()` returns `int` (line 470). If a method accumulates >65,535 unique literals, the `uint16()` cast wraps, and bytecode refers to the wrong literal.

### Limit 2: Temp/Arg Slot Index — 255 max (uint8)

**Location:** `compiler/codegen.go:168,170,180,490,499,508,559,587,594,601,871,878`

```go
c.builder.EmitByte(vm.OpPushTemp, byte(idx))  // SILENT TRUNCATION if idx > 255
```

Methods with >255 combined arguments + temporaries silently access wrong slots.

### Limit 3: Selector ID — 65,535 max (uint16)

**Location:** `compiler/codegen.go:346,716,721`

```go
selectorID := c.selectors.Intern(selector)
c.builder.EmitSend(vm.OpSend, uint16(selectorID), byte(numArgs))
```

The `SelectorTable` can grow beyond 65,535 entries across a whole VM session. If it does, message sends dispatch to wrong selectors.

### Limit 4: Argument Count — 255 max (uint8)

**Location:** `compiler/codegen.go:346,716,721`

```go
c.builder.EmitSend(vm.OpSend, uint16(selectorID), byte(numArgs))
```

A keyword message with >255 arguments would silently encode wrong argc. (Extremely unlikely in practice.)

### Limit 5: Array/Dictionary Literal Size — 255 max (uint8)

**Location:** `compiler/codegen.go:450,457,466`

```go
c.builder.EmitByte(vm.OpCreateArray, byte(len(arr.Elements)))  // wraps at 256
c.builder.EmitByte(vm.OpCreateDict, byte(len(dict.Keys)))      // wraps at 256
```

An array literal with >255 elements (`#(a b c ... 256 elements)`) silently wraps to the wrong count, creating a malformed array from whatever happens to be on the stack.

### Limit 6: Jump Offset — -32,768 to +32,767 bytes (int16)

**Location:** `vm/bytecode.go:350-361` (EmitJump)

Jump offsets are encoded as signed 16-bit integers. A method body >32KB of bytecode could have jump targets that overflow, causing jumps to wrong locations. The compiler does not check for this.

### Limit 7: Block Index — 65,535 max (uint16)

**Location:** `compiler/codegen.go:959`

```go
c.builder.EmitCreateBlock(uint16(blockIdx), uint8(numCaptures))
```

### Limit 8: Capture Count — 255 max (uint8)

**Location:** `compiler/codegen.go:959`

A block capturing >255 variables silently truncates the capture count.

## Image Format Is Not the Bottleneck

The image writer (`vm/image_writer.go`) uses `uint32` for all length fields (bytecode length, literal count, etc.), so the image format itself supports up to 4 billion bytes of bytecode per method. The bottleneck is purely in the operand encoding at compile time.

## Most Likely Trigger in Practice

The most practically reachable limits are:

1. **Array literal > 255 elements** — easily hit by data-heavy code
2. **Literal pool > 65,535** — reachable in very large methods with many unique strings/symbols
3. **Jump offset > 32KB** — reachable in very large generated methods

## Recommended Fix: Bounds Checks Before Every Narrowing Cast

The minimal fix is to add bounds checks in `compiler/codegen.go` before each narrowing cast, using the existing `c.errorf()` or `c.errorAt()` error reporting mechanism. Example pattern:

```go
// In addLiteral or at each emit site:
func (c *Compiler) addLiteral(value vm.Value) int {
    key := uint64(value)
    if idx, ok := c.literalMap[key]; ok {
        return idx
    }
    idx := len(c.literals)
    if idx > 0xFFFF {
        c.errorf("literal pool overflow: method has more than 65535 literals")
        return 0 // return safe index to continue compilation
    }
    c.literals = append(c.literals, value)
    c.literalMap[key] = idx
    return idx
}
```

Similar guards needed at:

| Location | Check | Limit |
|----------|-------|-------|
| `addLiteral()` return | `idx > 0xFFFF` | 65,535 |
| `compileVariable` temp/arg emit | `idx > 0xFF` | 255 |
| `compileArrayLiteral` | `len(arr.Elements) > 0xFF` | 255 |
| `compileDynamicArray` | `len(arr.Elements) > 0xFF` | 255 |
| `compileDictionaryLiteral` | `len(dict.Keys) > 0xFF` | 255 |
| `compileBlock` capture count | `numCaptures > 0xFF` | 255 |
| Jump emission (EmitJump) | offset range check | -32768..32767 |
| Selector intern result | `selectorID > 0xFFFF` | 65,535 |
| Arg count in sends | `numArgs > 0xFF` | 255 |

The compiler already has an `errors []string` accumulator and callers check `Errors()` after compilation, so this fits the existing error-reporting pattern with no architectural changes needed.

## Files Referenced

| File | Role |
|------|------|
| `compiler/codegen.go` | Main codegen — all narrowing casts live here |
| `vm/compiled_method.go` | CompiledMethod/BlockMethod structs (Bytecode field) |
| `vm/bytecode.go` | Opcode definitions, BytecodeBuilder, jump encoding |
| `vm/image_writer.go` | Image serialization (uses uint32, not the bottleneck) |
| `compiler/peephole.go` | Post-codegen optimizer (not relevant to limits) |
