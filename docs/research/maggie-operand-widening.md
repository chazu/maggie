# Maggie Compiler Operand Widening Research

**Date:** 2026-04-06
**Scout:** operand-limits investigation

## Summary

The Maggie bytecode format uses fixed-width operands per opcode: **uint8 (1 byte)** for temp/ivar/capture slots, and **uint16 (2 bytes)** for literal/selector/global/class indices. These limits are hardcoded in the opcode encoding (vm/bytecode.go), enforced by bounds checks in the compiler (compiler/codegen.go), and decoded inline in the interpreter (vm/interpreter.go). Widening operands to uint16/uint32 is a **cross-cutting change** affecting at minimum 6 files and roughly 200-350 lines of modifications.

---

## 1. Opcode Encoding Format (vm/bytecode.go)

Opcodes are **not** fixed-width overall. Each opcode is 1 byte, but operand sizes vary per-opcode, defined in the `opcodeTable` via `OperandBytes`:

| Operand type | Width | Used by |
|---|---|---|
| **uint8 (1 byte)** | 1 | OpPushTemp, OpStoreTemp, OpPushIvar, OpStoreIvar, OpPushCaptured, OpStoreCaptured, OpPushHomeTemp, OpStoreHomeTemp, OpCaptureTemp, OpCaptureIvar, OpCreateArray (size), OpCreateDict (pairs), OpCreateObject (nSlots), OpCreateBlock (nCaptures), OpSend/OpSendSuper/OpTailSend (argc) |
| **uint16 (2 bytes)** | 2 | OpPushLiteral, OpPushGlobal, OpStoreGlobal, OpPushClassVar, OpStoreClassVar, OpCreateBlock (methodIndex), OpCreateObject (classIndex), OpSend/OpSendSuper/OpTailSend (selector), OpJump/OpJumpTrue/etc (offset) |
| **int8** | 1 | OpPushInt8 |
| **int32** | 4 | OpPushInt32 |
| **float64** | 8 | OpPushFloat |

The `OperandBytes` field in `opcodeTable` determines how many bytes the disassembler and reader skip. The `BytecodeBuilder` has separate emit methods per width: `EmitByte()`, `EmitUint16()`, `EmitInt32()`, `EmitFloat64()`, `EmitSend()`, `EmitCreateBlock()`, `EmitCreateObject()`.

**Key constraint:** Operand widths are baked into each opcode's definition. Widening requires either new opcodes (e.g., `OpPushTemp16`) or changing existing opcode semantics (breaking image compatibility).

---

## 2. Compiler Bounds Checks (compiler/codegen.go)

The compiler has explicit overflow detection via `checkSlotIndex()` and inline checks:

### uint8-limited (max 255):
- **Temp/arg slots** (lines 109-114): `checkSlotIndex()` validates `idx > 0xFF`
- **Instance variable slots**: same check
- **Captured variable indices**: same check
- **Block captures count** (line 885): `numCaptures > 0xFF`
- **Array literal size** (line 467): `len(arr.Elements) > 0xFF`
- **Dictionary pair count** (line 488): `len(dict.Keys) > 0xFF`
- **Argument count** (line 364, 766): `numArgs > 0xFF`

### uint16-limited (max 65535):
- **Literal pool** (line 508): `idx > 0xFFFF` in `addLiteral()`
- **Selector IDs** (lines 361, 763, 774): `selectorID > 0xFFFF`
- **Block index** (line 1026): `blockIdx > 0xFFFF`

These checks produce error messages but **do not halt compilation** -- they call `c.errorf()` which appends to the error list. The compiler continues and emits truncated `byte()` or `uint16()` casts, resulting in **silently wrong bytecode** if the caller doesn't check the error list.

### Widening temp slots to uint16:

Would require:
1. Change all `EmitByte(OpPushTemp, byte(idx))` calls to `EmitUint16(OpPushTemp, uint16(idx))` -- approximately **25 call sites** in codegen.go
2. Update `checkSlotIndex()` to check against `0xFFFF` instead of `0xFF`
3. Change `opcodeTable` entries: `OpPushTemp` from 1 to 2 operand bytes, etc.
4. Update the interpreter to read `uint16` instead of `byte` for these opcodes

### Widening literals/selectors to uint32:

Would require:
1. Add `EmitUint32()` to BytecodeBuilder (doesn't exist yet; `EmitInt32()` exists but is signed)
2. Change `EmitUint16` calls for literals/selectors to `EmitUint32` -- approximately **12 call sites**
3. Add `ReadUint32()` unsigned reads to BytecodeReader
4. Update interpreter to read 4 bytes instead of 2

---

## 3. Interpreter Operand Decoding (vm/interpreter.go)

The interpreter does **not** use `BytecodeReader`. It reads bytes directly from the `bc` slice for maximum performance:

### uint8 reads (bc[frame.IP] -- 1 byte):
1. **OpPushTemp** (line 658): `idx := bc[frame.IP]`
2. **OpStoreTemp** (line 663): `idx := bc[frame.IP]`
3. **OpPushHomeTemp** (line 668): `idx := bc[frame.IP]`
4. **OpStoreHomeTemp** (line 674): `idx := bc[frame.IP]`
5. **OpPushIvar** (line 680): `idx := bc[frame.IP]`
6. **OpStoreIvar** (line 690): `idx := bc[frame.IP]`
7. **OpPushCaptured** (line 785): `idx := bc[frame.IP]`
8. **OpStoreCaptured** (line 794): `idx := bc[frame.IP]`
9. **OpSend argc** (line 879): `argc := int(bc[frame.IP])`
10. **OpTailSend argc** (line 891): `argc := int(bc[frame.IP])`
11. **OpSendSuper argc** (line 962): `argc := int(bc[frame.IP])`
12. **OpCreateBlock nCaptures** (line 1253): `nCaptures := int(bc[frame.IP])`
13. **OpCreateArray size** (line 1302): `size := int(bc[frame.IP])`
14. **OpCreateDict pairs** (line 1312): `pairs := int(bc[frame.IP])`
15. **OpCreateObject nSlots** (line 1328): `nSlots := int(bc[frame.IP])`
16. **OpPushInt8** (line 628): `val := int8(bc[frame.IP])`

**Total: 16 places** reading single-byte operands.

### uint16 reads (binary.LittleEndian.Uint16 -- 2 bytes):
1. **OpPushLiteral** (line 638)
2. **OpPushGlobal** (line 698)
3. **OpStoreGlobal** (line 751)
4. **OpPushClassVar** (line 832)
5. **OpStoreClassVar** (line 857)
6. **OpSend selector** (line 877)
7. **OpTailSend selector** (line 889)
8. **OpSendSuper selector** (line 960)
9. **OpCreateBlock methodIdx** (line 1251)
10. **OpCreateObject classIdx** (line 1326)
11. **OpJump** (line 1175)
12. **OpJumpTrue** (line 1180)
13. **OpJumpFalse** (line 1188)
14. **OpJumpNil** (line 1196)
15. **OpJumpNotNil** (line 1204)

**Total: 15 places** reading 2-byte operands.

---

## 4. Image Format Impact (vm/image_writer.go, vm/image_reader.go)

The image format stores bytecode as **raw bytes** (`w.buf.Write(m.Bytecode)` at line 656 of image_writer.go, `ir.readBytes(int(bytecodeLen))` at line 703 of image_reader.go). The image does **not** interpret individual opcodes -- it treats bytecode as an opaque byte blob.

**Widening operands would NOT break the image format**, because:
- Bytecode is stored with an explicit length prefix (`WriteUint32(buf, uint32(len(m.Bytecode)))`)
- Literals are stored separately as a counted array
- The reader/writer doesn't parse individual instructions

**However**, widened bytecode is **semantically incompatible** with old interpreters. An image written with 2-byte temp operands would be misinterpreted by an old VM reading 1-byte operands, causing instruction misalignment. This necessitates a **version bump** (currently ImageVersion = 5 -> 6) so old VMs reject new images cleanly.

---

## 5. Class-Level Compilation Limits

There is **no class-level compilation limit** separate from method-level limits. The compilation pipeline processes methods independently:

- In `cmd/bootstrap/main.go`, `compileAllFiles()` iterates over each `ClassDef`, then over each `MethodDef` within it, calling `compiler.CompileMethodDef()` individually (lines 154-173 for traits, similar for classes).
- Each method gets its own fresh `Compiler` instance with its own literal pool, temp map, etc.

**The PP.mag issue (1066 lines, methods at end not compiling) is NOT caused by operand limits.** More likely causes:

1. **Parser error silently dropping trailing methods** -- if the parser encounters a syntax error mid-file, it may stop parsing, causing methods after the error point to be missing from the AST entirely.
2. **Superclass resolution failure** -- in single-pass mode (`fileIn:` vs `fileInAll:`), if PP's superclass isn't registered yet, the class might not be created, and its methods would have nowhere to go.
3. **Selector table or literal pool overflow at the class level** -- if a class has many methods using many unique selectors, the *global* selector table could overflow, but the selector table uses `int` (not uint16), so this is unlikely.
4. **Compilation error in one method halting the rest** -- though the current code continues past individual method errors.

**Recommendation:** Investigate PP.mag separately. Add debug logging to the compilation pipeline to trace which methods are parsed and which are compiled. The operand widening work is orthogonal.

---

## 6. Scope Estimate

### Files requiring changes:

| File | Changes needed | Est. lines |
|---|---|---|
| `vm/bytecode.go` | Update `opcodeTable` OperandBytes entries; add `EmitUint32()` to builder; update `BytecodeReader`; update `DisassembleInstruction` | ~40-60 |
| `compiler/codegen.go` | Change ~25 `EmitByte` to `EmitUint16`, ~12 `EmitUint16` to `EmitUint32`; update bounds checks | ~60-80 |
| `vm/interpreter.go` | Change 16 single-byte reads to 2-byte, 15 two-byte reads to 4-byte | ~50-70 |
| `compiler/peephole.go` | Update operand parsing/rewriting (~27 references to bytecode bytes) | ~30-50 |
| `vm/image_writer.go` | Version bump only (bytecode is stored as opaque blob) | ~2-3 |
| `vm/image_reader.go` | Version compatibility check | ~2-3 |
| `compiler/codegen_test.go` | Update expected bytecode in tests | ~20-40 |

**Additional files potentially affected:**
- `compiler/hash/nodes.go` -- if hash serialization encodes operand widths
- `vm/dist/wire.go` -- if chunker verifies bytecode structure
- `cmd/bootstrap/main.go` -- no changes needed (calls `CompileMethodDef`, not opcode-aware)

### Total estimate:
- **6-8 files** with substantive changes
- **200-350 lines** of modifications
- **Image version bump** required (v5 -> v6)
- **All existing .image files become incompatible** -- must rebuild bootstrap image

### Risk assessment:
- **Medium complexity**: The changes are mechanical (widen reads/writes) but spread across many locations
- **High testing burden**: Every opcode with widened operands needs validation
- **Performance impact**: 2-byte temp reads are slightly slower than 1-byte (extra memory, cache effects), but likely negligible since the interpreter already does uint16 reads for other opcodes
- **Migration**: Existing images must be rebuilt; no in-place migration possible

### Alternative approach: Extended opcodes
Instead of widening all operands, add new "wide" variants (e.g., `OpPushTempW` with uint16 operand) and only use them when indices exceed 255. This preserves backward compatibility and keeps common-case bytecode compact, but doubles the opcode surface area and adds compiler complexity.

---

## Observations

1. The compiler's `errorf()` approach means operand overflow is detected but doesn't prevent emission of truncated bytecode. The caller must check `c.Errors()` -- this is a correctness hazard.
2. Jump offsets use `int16` (signed), limiting jump distances to +/-32767 bytes. Very large methods could also hit this limit. The builder already has overflow detection for jumps.
3. The PP.mag compilation failure is likely a different bug (parser or pipeline issue), not related to operand width limits.
