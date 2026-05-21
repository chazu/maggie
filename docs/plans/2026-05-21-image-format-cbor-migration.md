# Image Format CBOR Migration Plan

**Date:** 2026-05-21
**Priority:** Low (correctness is stable), **Effort:** High (6-8 days)
**Status:** COMPLETE (all 6 phases implemented 2026-05-21)

## Motivation

Maggie's image persistence layer is 3,124 lines of hand-rolled binary
read/write code across three files (`image_reader.go`, `image_writer.go`,
`image_encoding.go`). Every schema change requires coordinating all three
files, manually computing offsets, back-patching headers, and maintaining
backward compatibility shims. The format is now at version 5. Each new version
adds conditional branches in the reader (`if ir.header.Version >= N`), and the
encoding layer uses fixed 9-byte value cells with manual tag/payload
decomposition.

Meanwhile, the project already depends on `fxamacker/cbor/v2` for the
distribution protocol (`vm/serial.go`, CBOR tags 27001-27015) and has
battle-tested CBOR encode/decode paths for Values, Objects, Dictionaries,
BigIntegers, Characters, Symbols, and circular object graphs (backreferences).
The image format duplicates much of this infrastructure in a less maintainable
way.

The goal is to replace the bespoke binary format with CBOR, reusing the
existing serialization infrastructure, to make the image format
self-describing, extensible without version bumps, and significantly less
code to maintain.

## Current Architecture

### What Gets Serialized

The image is the VM's full persistent state. In write order:

1. **Header** (36 bytes): magic `MAGI`, version, flags, object count, string
   table offset, class table offset, entry point method index
2. **String table**: length-prefixed UTF-8 strings (shared pool for all names)
3. **Symbol table**: indices into string table (interned symbol names)
4. **Selector table**: indices into string table (method selector names)
5. **Classes**: name, namespace, superclass index, numSlots, instance variable
   names, instance/class method indices, docstring
6. **Methods**: selector, class index, name, isClassMethod, arity, numTemps,
   literals (9-byte encoded values), bytecode, blocks, source, docstring,
   source map, content hash (32 bytes), typed hash (32 bytes)
7. **Objects**: class index, slot count, slots (9-byte encoded values each)
8. **Globals**: name-value pairs (string index + 9-byte encoded value)
9. **Class variables** (v3+): class index, variable name-value pairs

### The Encoder/Decoder

`ImageEncoder` assigns monotonic indices to objects, symbols, strings, classes,
and methods during the collection phase. Values are encoded as 9-byte cells:
1 tag byte + 8 payload bytes (always fixed size, padding-heavy for small
values like nil/true/false).

`ImageDecoder` reverses the process: index-to-object lookup tables for each
entity type.

### Key Complexities

- **Cross-reference indices**: classes reference superclasses and methods by
  index; methods reference classes by index; objects reference classes by
  index; literals can be symbols, strings, classes, or object references.
- **Dependency ordering**: classes are topologically sorted (superclass before
  subclass) at write time. Reader does a two-pass class load (create all,
  then link superclasses).
- **Selector remapping**: image selector IDs may differ from VM selector IDs
  because the VM pre-interns primitive selectors. The reader builds a remap
  table and patches bytecode in-place.
- **Object graph cycles**: objects can reference each other through slots. The
  writer pre-registers all objects, then encodes slot values by index. The
  reader does a similar two-pass (allocate all objects, then fill slots).
- **Atomic save**: `SaveImageAtomic` uses write-to-tmp + fsync + rename for
  crash safety. This is format-agnostic and survives the migration unchanged.

### Current Image Statistics

- Image size: ~724 KB for 1,175 methods across 68 .mag files
- 9-byte fixed-size value encoding wastes space (nil/true/false use 9 bytes
  each; symbols and object references need only ~5 bytes)
- String table is reasonably compact (varint-prefixed)
- Content hashes add 64 bytes per method (32 content + 32 typed)

## What the Smalltalk World Does

### Pharo/Squeak Image Format

Pharo and Squeak use a fundamentally different image model. The image is a
heap snapshot: the entire object memory is written as a contiguous block,
with raw pointer words adjusted on load. The format is tightly coupled to the
Spur memory manager's object header layout (class index, hash, slot count
packed into 64-bit header words). There is no separate "class table" or
"method table" -- everything is an object in the heap, and the serializer
writes raw memory.

**Lesson:** Pharo's approach works because the VM owns the memory layout.
Maggie's objects are Go structs (`*Object`, `*Class`, `*CompiledMethod`) with
Go-managed memory -- we cannot snapshot raw memory. Our image format is
necessarily a logical serialization, not a physical memory dump.

### Fuel (Pharo's General Serializer)

Fuel is Pharo's graph serializer for packages, code, and arbitrary object
graphs. It uses a two-pass approach:
1. **Trace phase**: walk the object graph, assign cluster IDs, topologically
   sort by class
2. **Serialize phase**: write objects grouped by class ("clusters") for
   efficient deserialization

Fuel handles cycles via forward references and uses class-specific
serializers (materialization strategies) for special objects.

**Lesson:** Cluster-by-class is a good pattern for our CBOR format. Instead
of interleaving heterogeneous objects, we group by type (all strings, then all
symbols, then all classes, etc.). This is what we already do.

### Parcels (VisualWorks)

VisualWorks' Parcels are a code-distribution format (not a full image). They
serialize compiled code + metadata in a portable format that can be loaded
into different images. Parcels handle the "selector remapping" problem similar
to Maggie's approach: selectors are stored by name, remapped to local IDs on
load.

**Lesson:** Store selectors by name (which CBOR text strings give us for
free), and remap to local integer IDs on load. This eliminates the fragile
index-based selector encoding.

### Key Takeaway

None of the Smalltalk systems use a general-purpose format like CBOR for their
image. But Maggie is not a traditional Smalltalk VM -- it uses Go structs, not
a custom heap, so a logical serialization format is the right choice. CBOR
gives us: self-describing types, variable-length integers (compact encoding),
standard tooling for debugging, and the ability to add new fields without
version bumps.

## CBOR Tag Allocation

### Existing Tags (serial.go, 27001-27015)

These tags are used for the distribution protocol (wire format for
inter-node value transfer). The image format should use a **separate tag
range** to avoid ambiguity: distribution tags encode values for transfer
between nodes with potentially different class sets; image tags encode
the full VM state for persistence.

### New Image Tags (27100-27199)

| Tag   | Type              | Content                                                   |
|-------|-------------------|-----------------------------------------------------------|
| 27100 | ImageHeader       | Map: version, flags, stats                                |
| 27101 | StringTable       | Array of text strings                                     |
| 27102 | SymbolEntry       | Text string (symbol name)                                 |
| 27103 | SelectorEntry     | Text string (selector name)                               |
| 27104 | ClassDef          | Map: name, namespace, superIdx, numSlots, instVars, ...   |
| 27105 | MethodDef         | Map: name, isClassMethod, arity, numTemps, ...            |
| 27106 | BlockDef          | Map: arity, numTemps, numCaptures, literals, bytecode, .. |
| 27107 | ObjectDef         | Map: classIdx, slots                                      |
| 27108 | GlobalEntry       | Map: name, value                                          |
| 27109 | ClassVarEntry     | Map: classIdx, vars                                       |
| 27110 | ImageValue        | Tagged union for encoded values (see below)               |
| 27111 | ValueRef          | Integer (back-reference to object by index)               |
| 27112 | ClassRef          | Integer (reference to class by index)                     |
| 27113 | MethodRef         | Integer (reference to method by index)                    |
| 27114 | SymbolRef         | Integer (reference to symbol by index)                    |
| 27115 | SourceMap         | Array of [offset, line, column] triples                   |

### Value Encoding in CBOR

Instead of the current fixed 9-byte cells, values become native CBOR:

| Maggie Type | CBOR Encoding                           | Bytes (typical) |
|-------------|-----------------------------------------|-----------------|
| Nil         | CBOR null                               | 1               |
| True        | CBOR true                               | 1               |
| False       | CBOR false                              | 1               |
| SmallInt    | CBOR integer                            | 1-9             |
| Float       | CBOR float64                            | 9               |
| Symbol      | Tag 27114 + integer (symbol index)      | 3-5             |
| String      | CBOR text string                        | 1+N             |
| Character   | Tag 27006 (reuse) + uint32              | 5-7             |
| Object ref  | Tag 27111 + integer (object index)      | 3-5             |
| Class ref   | Tag 27112 + integer (class index)       | 3-5             |

This saves significant space: the median literal pool entry will shrink from
9 bytes to 1-5 bytes. Nil-heavy slot arrays (common for newly-allocated
objects) compress dramatically.

## Migration Strategy

### Versioning Approach: Dual-Reader, Single-Writer

The cleanest migration path:

1. **Phase 1**: Build the CBOR writer alongside the existing binary writer.
   Add a `--cbor-image` flag to bootstrap that writes the new format.
   Compare outputs for correctness.

2. **Phase 2**: Build the CBOR reader. The image loader auto-detects format
   by checking the first bytes: `MAGI` = legacy binary, `D9` (CBOR tag
   prefix for tag 27100) = CBOR format. Both readers coexist.

3. **Phase 3**: Switch the default writer to CBOR. Bootstrap produces CBOR
   images. The binary reader remains for loading old images.

4. **Phase 4**: Remove the binary writer. The binary reader stays as long as
   old images might exist (one release cycle).

5. **Phase 5**: Remove the binary reader and all legacy code. Clean break.

### No Backward Compatibility Within CBOR

CBOR is inherently extensible: unknown map keys are ignored by the reader,
missing optional keys get defaults. This eliminates the version-conditional
branching problem. The image header will contain a schema version for major
breaking changes, but minor additions (new optional fields) require no version
bump.

## Phase Breakdown

### Phase 1: CBOR Writer (2 days)

**New file:** `vm/image_cbor_writer.go` (~400 lines estimated)

The CBOR writer follows the same collect-then-serialize pattern as the current
writer, but emits CBOR instead of raw bytes.

```
CborImageWriter
  collectFromVM(vm)      -- reuse existing collection logic
  writeCborImage() []byte
    encodeHeader()
    encodeStringTable()
    encodeSymbolTable()
    encodeSelectorTable()
    encodeClasses()
    encodeMethods()
    encodeObjects()
    encodeGlobals()
    encodeClassVars()
```

Key design decisions:

- **Reuse `ImageEncoder` for index tracking.** The existing encoder's
  Register/Lookup methods are format-agnostic. The CBOR writer uses the same
  encoder to assign indices, then emits CBOR tags with those indices.

- **Top-level structure is a CBOR array.** The image is a single CBOR array
  of tagged sections, making it streamable and parseable with standard CBOR
  tools:
  ```
  Tag(27100) [
    header_map,
    Tag(27101) [string_table],
    [symbol_entries],
    [selector_entries],
    [class_defs],
    [method_defs],
    [object_defs],
    [global_entries],
    [class_var_entries]
  ]
  ```

- **Methods encode literals as inline CBOR values**, not as fixed-size cells.
  Bytecode remains as a CBOR byte string (raw bytes, not further encoded).

- **Content hashes**: 32-byte arrays become CBOR byte strings within the
  method map. Zero hashes are omitted (CBOR map key simply absent).

**File changes:**
- `vm/image_cbor_writer.go` (new)
- `vm/image_writer.go` -- add `SaveImageCbor` method on VM, parallel to
  existing `SaveImage`
- `cmd/bootstrap/main.go` -- add `--cbor` flag

**Tests:**
- `vm/image_cbor_writer_test.go` (new): unit tests for each section encoder
- Golden test: bootstrap with CBOR, reload, compare VM state to binary-loaded
  VM state

### Phase 2: CBOR Reader (2 days)

**New file:** `vm/image_cbor_reader.go` (~500 lines estimated)

The CBOR reader uses `fxamacker/cbor/v2` to decode the tagged sections and
populate the VM, following the same multi-pass pattern as the current reader.

```
CborImageReader
  ReadAll(vm)
    decodeHeader()
    decodeStringTable()
    decodeSymbolTable()       -- intern symbols in VM
    decodeSelectorTable()     -- intern selectors, build remap table
    decodeClasses(vm)         -- two-pass: create then link superclasses
    decodeMethods(vm)         -- decode, remap bytecode selectors, install in vtables
    decodeObjects(vm)         -- two-pass: allocate then fill slots
    decodeGlobals(vm)
    decodeClassVars(vm)
    fixupClassValues(vm)      -- re-register class values in registry
```

Key design decisions:

- **Format auto-detection.** `LoadImageFrom` checks the first byte: if it
  starts with `0xD9` (CBOR 2-byte tag prefix) or `0xDA` (4-byte tag prefix),
  route to CBOR reader. If it starts with `M` (from `MAGI`), route to legacy
  binary reader.

- **Selector remapping is identical.** The CBOR reader builds the same
  `selectorIDMap` and runs the same `remapBytecodeSelectors` pass.

- **Object cycle handling.** Same two-pass strategy: allocate all objects
  first (registering them in the decoder), then fill slots. CBOR's
  `Tag(27111)` back-references point to object indices, just like the current
  binary format's `imageTagObject`.

**File changes:**
- `vm/image_cbor_reader.go` (new)
- `vm/image_reader.go` -- modify `LoadImageFrom` for format auto-detection
- `vm/image_encoding.go` -- no changes needed (CBOR reader uses its own
  value decoding, but the `ImageDecoder` index tables are reusable)

**Tests:**
- `vm/image_cbor_reader_test.go` (new): unit tests per section
- Round-trip test: write CBOR image, read it back, verify VM state matches
- Cross-format test: write binary, read binary, save as CBOR, read CBOR,
  compare
- Fuzz test: adapt `image_reader_fuzz_test.go` for CBOR format

### Phase 3: Switch Default to CBOR (0.5 days)

- Change `cmd/bootstrap/main.go` to emit CBOR by default
- Change `vm.SaveImage` to call CBOR writer
- Rebuild images: `go run ./cmd/bootstrap/ && cp maggie.image cmd/mag/`
- Run full test suite to verify
- Keep binary reader active

**File changes:**
- `cmd/bootstrap/main.go`
- `vm/image_writer.go` -- `SaveImageTo` calls CBOR writer

### Phase 4: Remove Binary Writer (0.5 days)

- Delete the binary writing code from `image_writer.go` (lines related to
  `writeHeader`, `writeStringTable`, `writeSymbolTable`, `writeSelectorTable`,
  `writeClasses`, `writeMethods`, `writeObjects`, `writeGlobals`,
  `writeClassVars`, `patchHeader`)
- Keep `CollectAllObjects` (used by both formats)
- Keep `SaveImageAtomic` (format-agnostic)
- Move remaining shared code to appropriate locations

**File changes:**
- `vm/image_writer.go` -- gut binary writing, keep shared infrastructure
- `vm/image_encoding.go` -- remove `EncodeValue`/`EncodeValueTo` and binary
  encoding helpers (WriteUint32, etc.)

### Phase 5: Remove Binary Reader (0.5 days)

- Delete binary reading code from `image_reader.go`
- Remove format auto-detection (only CBOR path)
- Remove legacy error types that are binary-specific
- Remove `ImageHeader` struct (replaced by CBOR header map)

**File changes:**
- `vm/image_reader.go` -- gut binary reading
- `vm/image_encoding.go` -- remove `DecodeValue`, `ImageDecoder` binary
  helpers (most of this file gets deleted)
- Delete `vm/image_reader_fuzz_test.go` (binary fuzz), replace with CBOR
  fuzz test
- Delete binary-specific test cases from `vm/image_reader_test.go` and
  `vm/image_writer_test.go`

### Phase 6: Cleanup (0.5 days)

- Merge `image_cbor_writer.go` into `image_writer.go`
- Merge `image_cbor_reader.go` into `image_reader.go`
- Simplify `image_encoding.go` to only contain the index-tracking
  `ImageEncoder`/`ImageDecoder` (shared between image and distribution)
- Remove `ImageVersion` constant (CBOR format is self-versioning)
- Update `CLAUDE.md` to reflect new format
- Update docs/USER_GUIDE.md if it references image format internals

**Final file structure:**
- `vm/image_writer.go` -- CBOR-based image writer (~500 lines)
- `vm/image_reader.go` -- CBOR-based image reader (~600 lines)
- `vm/image_encoding.go` -- shared index tracking (~200 lines)
- Total: ~1,300 lines (down from 3,124)

## Risks and Mitigations

### 1. Performance: Image Load Time

**Risk:** CBOR decoding may be slower than raw binary reads (no memcpy from
mapped pages; CBOR requires parsing varint lengths, tag bytes, etc.).

**Measurement:** The current image is 724 KB. At Go's CBOR decode throughput
of ~200 MB/s (`fxamacker/cbor` benchmarks), decoding takes ~3.6 ms. The
current binary reader is likely faster (direct `binary.LittleEndian` reads
from a byte slice, effectively zero-copy), probably ~1-2 ms.

**Mitigation:** A ~2 ms difference on a 724 KB image is negligible for a
development tool. If it becomes a problem:
- CBOR supports indefinite-length arrays for streaming
- The CBOR image can be optionally compressed (zstd) for disk, decompressed
  into memory for parsing
- Profile before optimizing; the real bottleneck is likely selector remapping
  and vtable construction, not raw I/O

**Acceptance criterion:** Image load time stays under 50 ms on the standard
bootstrap image.

### 2. Object Identity Preservation

**Risk:** Objects must maintain referential identity through the serialization
round-trip. If object A appears in two different globals, both globals must
point to the same `*Object` after deserialization.

**Current approach:** The binary format assigns each object a unique index.
All references to that object use the same index. On load, objects are
allocated once, and index-based references resolve to the same `*Object`.

**CBOR approach:** Identical. `Tag(27111)` encodes object references as
integer indices. The CBOR reader allocates objects once and resolves
references by index. No change in semantics.

### 3. Circular Object References

**Risk:** Objects can reference each other through slots (e.g., a doubly-linked
list node where A.next = B and B.prev = A).

**Current approach:** Two-pass: allocate all objects (with nil slots), then
fill slots. By the time slot values are decoded, all objects exist and can
be referenced by index.

**CBOR approach:** Same two-pass strategy. Object definitions in the CBOR
array are decoded in order, allocating objects. Then slot values are decoded,
with `Tag(27111)` references resolving to already-allocated objects. The
existing `serial.go` backreference mechanism (`Tag 27010`) is for the
distribution protocol's streaming format; the image format uses its own
index-based scheme which is cleaner for a known-size collection.

### 4. Selector Remapping

**Risk:** Bytecode contains selector IDs that may differ between bootstrap
time (when the image is created) and runtime (when it is loaded), because the
VM pre-interns primitive selectors.

**Mitigation:** Selector remapping is purely a reader concern and is
format-agnostic. The CBOR reader builds the same `selectorIDMap` and runs
the same `remapBytecodeSelectors` pass on raw bytecode bytes. No change
needed.

### 5. Deterministic Output

**Risk:** The current writer produces deterministic output (sorted classes,
sorted globals, sorted class variables). CBOR maps are unordered by default.

**Mitigation:** Use CBOR arrays (not maps) for ordered collections. Use
`cbor.CanonicalEncOptions()` (already used in serial.go) for deterministic
encoding. Sort all collections before encoding, same as today.

Alternatively, use CBOR maps with integer keys (already the pattern in
serial.go structs like `serializedObject`) for the internal structure of
each class/method/object. The outer collection (list of all classes, list of
all methods) is an array in definition order.

### 6. Image Size

**Estimate:** The CBOR image should be **10-20% smaller** than the current
binary format:
- Nil values: 1 byte vs 9 bytes (huge savings in sparse object slots)
- Small integers: 1-3 bytes vs 9 bytes
- Booleans: 1 byte vs 9 bytes
- Symbols/references: 2-5 bytes vs 9 bytes
- Strings: comparable (CBOR length prefix vs current uint32 prefix)
- Bytecode: identical (raw byte string)
- Content hashes: identical (32-byte byte strings)
- CBOR tags add overhead: 2-4 bytes per tagged value

**Counter-factor:** CBOR map keys add overhead (even integer keys cost 1-3
bytes per field). Struct fields that were positional in binary now need keys.

**Net estimate:** ~600-650 KB for the same bootstrap image (vs 724 KB).

## Testing Strategy

### Unit Tests (per phase)

Each section encoder/decoder gets dedicated unit tests:
- Encode a string table, decode it, verify contents
- Encode a class with superclass reference, decode it, verify hierarchy
- Encode a method with literals including all value types, decode, verify
- Encode objects with cross-references, decode, verify referential identity
- Encode/decode class variables

### Round-Trip Test

The most critical test: write the full VM state to CBOR, load it into a fresh
VM, and verify equivalence:
- All classes exist with correct names, namespaces, superclasses
- All methods are installed on correct vtables with correct selectors
- All objects have correct class and slot values
- All globals are present with correct values
- All class variables are present
- A method can be invoked and produces the correct result

### Cross-Format Test (Phase 2-3 only)

Load the binary image, save as CBOR, load the CBOR image, and compare the
two VMs. This catches any differences in what gets serialized.

### Bootstrap Smoke Test

The ultimate end-to-end test:
1. `go run ./cmd/bootstrap/` (produces CBOR image)
2. `cp maggie.image cmd/mag/`
3. `go test ./...` (full test suite passes)
4. `go run ./cmd/mag/ -e '"hello world" printNl'` (sanity check)

### Fuzz Testing

Adapt the existing `image_reader_fuzz_test.go` to generate random CBOR
payloads with the image structure tags. Verify the reader rejects malformed
input gracefully (no panics, no infinite loops, bounded memory allocation).

### Performance Benchmark

Add `BenchmarkLoadImageCBOR` alongside the existing binary load benchmark.
Compare load times on the standard bootstrap image. Accept the migration if
CBOR load time is within 5x of binary (likely within 2x).

## Shared Infrastructure with serial.go

### What Can Be Reused

- `cborSerialEncMode` (canonical CBOR encoding mode) -- shared
- CBOR tag for Character (27006) -- reuse directly
- CBOR tag for BigInt (27011/27012) -- reuse if BigInts appear in literals
- The general pattern of struct-with-integer-keys for CBOR encoding

### What Cannot Be Reused

- `serializedObject` from serial.go uses class content hashes for identity.
  The image format uses class indices (the class is defined earlier in the
  same image, so hash-based lookup is unnecessary overhead).
- Backreference mechanism: serial.go uses `Tag(27010)` for streaming cycle
  detection. The image uses index-based references within a known collection
  size, which is simpler and more efficient.
- `SerializeValue`/`DeserializeValue` are too heavyweight for image use (they
  handle channels, remote processes, exceptions, etc.). The image value
  encoder only needs to handle the types that can appear in literals, slots,
  and globals.

### Recommendation

Create image-specific value encoding functions (`encodeImageValue`,
`decodeImageValue`) that handle the image-relevant subset of types. Keep the
distribution serialization in `serial.go` untouched. Share the CBOR encoding
mode and tag constants for types that overlap (Character, BigInt).

## Non-Goals

- **Compression:** Out of scope. Can be added later as a transparent layer
  (compress before write, decompress before parse). The CBOR format is
  already more compact than the current binary format.

- **Streaming/incremental image updates:** Out of scope. The image is written
  and read as a single unit. Incremental updates would require a log-structured
  format, which is a different project.

- **Human-readable image format:** CBOR is binary, not human-readable. But
  CBOR has excellent tooling (`cbor-diag`, `cbor2json`) for debugging. This
  is a significant improvement over the completely opaque current format.

- **Image compatibility across Maggie versions:** The CBOR format is for
  the current Maggie VM. If the VM's object model changes (e.g., class
  representation), the image format changes too. The CBOR format makes such
  changes cheaper (add a map key) but does not guarantee cross-version
  compatibility.

## Success Criteria

1. All existing tests pass with the CBOR format
2. Bootstrap image round-trips correctly (write CBOR, load, re-save, binary
   compare produces identical output)
3. Image size is within +/- 20% of current binary format
4. Image load time is under 50 ms on standard hardware
5. Total image code is under 1,500 lines (down from 3,124 + 6,692 test lines)
6. No version-conditional branching in the CBOR reader
7. New fields can be added to classes/methods/objects without a format version
   bump (verified by test: write image with extra map key, read with old
   reader, succeeds)

## Appendix: Example CBOR Structure

Pseudocode showing the top-level CBOR image structure:

```
Tag(27100) {
  "version": 1,
  "flags": 0,
  "stats": {
    "classes": 142,
    "methods": 1175,
    "objects": 87,
    "globals": 156
  },

  "strings": ["Object", "SmallInteger", "printNl", ...],

  "symbols": [
    Tag(27102) "Object",
    Tag(27102) "SmallInteger",
    ...
  ],

  "selectors": [
    Tag(27103) "printNl",
    Tag(27103) "+",
    ...
  ],

  "classes": [
    Tag(27104) {
      "name": 0,           // string index
      "namespace": null,
      "super": null,        // no superclass (root)
      "numSlots": 0,
      "instVars": [],
      "instanceMethods": [0, 1, 2],  // method indices
      "classMethods": [3, 4],
      "docString": null
    },
    ...
  ],

  "methods": [
    Tag(27105) {
      "name": "printNl",
      "isClassMethod": false,
      "arity": 0,
      "numTemps": 0,
      "literals": [Tag(27114) 5, 42, null],  // symbol ref, integer, nil
      "bytecode": h'0A01030B...',             // CBOR byte string
      "blocks": [...],
      "source": "printNl\n  <primitive>",
      "sourceMap": [[0, 1, 1], [3, 2, 3]],
      "contentHash": h'A3B4C5...',
      "typedHash": h'D6E7F8...'
    },
    ...
  ],

  "objects": [
    Tag(27107) {
      "class": 5,           // class index
      "slots": [42, Tag(27111) 0, null]  // int, object ref, nil
    },
    ...
  ],

  "globals": [
    Tag(27108) {"name": "Object", "value": Tag(27112) 0},
    Tag(27108) {"name": "counter", "value": 0},
    ...
  ],

  "classVars": [
    Tag(27109) {
      "class": 12,
      "vars": {"Default": null, "Instances": Tag(27111) 3}
    },
    ...
  ]
}
```

## Appendix: File Inventory and Line Counts

### Files Modified

| File | Current Lines | Phase | Change |
|------|--------------|-------|--------|
| `vm/image_writer.go` | 1,100 | 1,4,6 | Add CBOR writer, then remove binary writer, then merge |
| `vm/image_reader.go` | 1,353 | 2,5,6 | Add format detection, then remove binary reader, then merge |
| `vm/image_encoding.go` | 671 | 4,5,6 | Progressively remove binary helpers |
| `cmd/bootstrap/main.go` | ~150 | 1,3 | Add --cbor flag, then switch default |

### Files Created

| File | Est. Lines | Phase |
|------|-----------|-------|
| `vm/image_cbor_writer.go` | ~400 | 1 (merged in phase 6) |
| `vm/image_cbor_reader.go` | ~500 | 2 (merged in phase 6) |
| `vm/image_cbor_writer_test.go` | ~400 | 1 (merged in phase 6) |
| `vm/image_cbor_reader_test.go` | ~500 | 2 (merged in phase 6) |

### Files Deleted (Phase 5-6)

Most of `vm/image_encoding.go` (binary encoding helpers, varint, fixed-width
read/write functions). Approximately 400 lines deleted.

### Test Files

| File | Current Lines | Change |
|------|--------------|--------|
| `vm/image_encoding_test.go` | 883 | Most deleted (binary encoding tests) |
| `vm/image_reader_test.go` | 3,061 | Migrated to CBOR, binary tests deleted |
| `vm/image_writer_test.go` | 1,680 | Migrated to CBOR, binary tests deleted |
| `vm/image_reader_fuzz_test.go` | 252 | Rewritten for CBOR |
| `cmd/mag/image_cli_test.go` | 816 | Format-agnostic, minimal changes |
