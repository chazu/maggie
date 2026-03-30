# Two-Layer Hash Model for Typed Content Addressing

**Date:** 2026-03-29
**Phase:** Type System Phase 3
**Estimated scope:** 1.5-2 days

## Design Decisions

### Q1: How should typed hashing work?

**Decision: Extend the existing normalization pipeline with an optional type layer.**

Do NOT compute a separate hash over just the type signature. Instead, add a
`HashTypedMethod` function that runs the same normalization but includes type
annotations in the serialized output. This means:

- Same De Bruijn indexing, same AST normalization
- Type annotations serialized inline at the point they appear (on method def,
  on block parameters)
- A method with no type annotations produces typed hash == semantic hash (both
  serialize identically since type tags are simply absent)

This avoids the complexity of maintaining two separate serialization paths and
makes the typed hash a strict superset of the semantic hash's information.

### Q2: Where should the typed hash live?

**Decision: On `CompiledMethod` (image format v5) AND on `Chunk`.**

The typed hash needs to survive compilation for it to be useful. If it only
lived in the distribution layer, you could never recompute it from an image
without re-parsing from source. Storing it on `CompiledMethod` makes it
available for local verification, image round-tripping, and distribution.

### Q3: Should type annotations be preserved on CompiledMethod?

**Decision: No, not in this phase.**

The typed hash is computed from the parse-time AST (which has `ParamTypes`,
`ReturnType`, `TempTypes`). Once computed, the hash is stored on
`CompiledMethod`. The type annotations are still available through the `Source`
field if re-parsing is ever needed. Storing structured type info on
`CompiledMethod` is a separate concern (type metadata phase) and would bloat
this change.

### Q4: How does this interact with the content store?

**Decision: Index by semantic hash only. Typed hash is metadata.**

The content store's identity key remains the semantic `ContentHash`. The typed
hash is stored on the method and on the chunk but is NOT a separate index key.
Rationale: execution identity is semantic, not typed. Two methods that differ
only in type annotations are the same method for execution purposes -- you
don't want two entries in the store for the same behavior.

The `ClassDigest` gains a `TypedMethodHashes` field for the typed class digest,
but the class identity hash continues to use semantic method hashes.

### Q5: What about methods with no type annotations?

**Decision: Typed hash equals semantic hash (not zero).**

When a method has no type annotations, the typed serialization is byte-identical
to the semantic serialization, so `TypedHash == ContentHash`. This is a natural
consequence of the "superset serialization" approach. Zero means "not computed"
for both hashes, same as today.

### Q6: Image format implications?

**Decision: Bump to v5. Add 32 bytes per method.**

This is the same pattern as v3->v4 (which added `ContentHash`). The reader
already handles version-conditional field reads. Cost: 32 bytes per method in
the image file. For a system with 2000 methods, that's 64KB -- negligible.

### Q7: What about protocol definitions?

**Decision: Out of scope for this phase.**

Protocol hashing is a good idea but adds complexity (protocols reference types,
which reference other protocols). Defer to a follow-up.

---

## Data Structure Changes

### `compiler/hash/nodes.go` -- Add type annotation fields

```go
// HTypeAnnotation represents a type annotation in the hashing AST.
// Empty Name means untyped/Dynamic.
type HTypeAnnotation struct {
    Name string // FQN-resolved type name, "" = untyped
}

// HMethodDef gains typed fields:
type HMethodDef struct {
    Selector   string
    Arity      int
    NumTemps   int
    Primitive  int
    DocString  string
    Statements []HNode

    // Type annotations (only populated for typed hashing)
    ParamTypes []HTypeAnnotation // parallel to params, nil slice = untyped mode
    TempTypes  []HTypeAnnotation // parallel to temps
    ReturnType HTypeAnnotation   // Name="" means untyped
}

// HBlock gains typed fields:
type HBlock struct {
    Arity      int
    NumTemps   int
    Statements []HNode

    // Type annotations (only populated for typed hashing)
    ParamTypes []HTypeAnnotation // nil slice = untyped mode
}
```

`HTypeAnnotation` is NOT an HNode (no tag byte, not a tree node). It is a
plain struct serialized inline as a string.

### `compiler/hash/tags.go` -- Add typed method tag

```go
// New tags (appended, frozen tags preserved):
TagTypedMethodDef byte = 0x30 // typed variant of TagMethodDef
TagTypedBlock     byte = 0x31 // typed variant of TagBlock
```

The semantic hash uses `TagMethodDef` (0x17) and `TagBlock` (0x16) as today.
The typed hash uses `TagTypedMethodDef` (0x30) and `TagTypedBlock` (0x31).
This means the two serializations produce different byte streams when types are
present, guaranteeing different hashes -- even if the type annotations happen to
be all-Dynamic.

Wait -- that contradicts Q5 (typed hash == semantic hash when no types). Let me
reconsider.

**Revised approach:** Use the SAME tags for both. The difference is whether
type annotation data is appended after the existing fields. The serializer
checks a flag (`includeTypes bool`) and conditionally writes type data after
each node's existing fields. When types are absent (nil slice on HMethodDef),
nothing extra is written, so the byte stream is identical.

This is cleaner: one tag set, one serialization path with a conditional branch,
and the "no types = same hash" property falls out naturally.

### `compiler/hash/tags.go` -- No new tags needed

No tag changes. The type data is appended conditionally after existing fields
using a presence byte:

- `0x00` = no type annotations follow (typed serialization of untyped method)
- `0x01` = type annotations follow

When `includeTypes` is false (semantic hash mode), neither the presence byte
nor any type data is written. When `includeTypes` is true and there are no
types, `0x00` is written. When `includeTypes` is true and there ARE types,
`0x01` is written followed by the type data.

**This means:** semantic hash of untyped method != typed hash of untyped method
(because typed mode writes the `0x00` presence byte). Good -- this is actually
desirable. The typed hash domain should be distinct from the semantic hash
domain to prevent accidental collisions across the two uses.

### `vm/compiled_method.go` -- Add TypedHash field

```go
type CompiledMethod struct {
    // ... existing fields ...
    ContentHash [32]byte // SHA-256 of normalized AST (zero = not computed)
    TypedHash   [32]byte // SHA-256 of normalized AST + type annotations (zero = not computed)
}
```

Add accessors:

```go
func (m *CompiledMethod) GetTypedHash() [32]byte
func (m *CompiledMethod) SetTypedHash(h [32]byte)
```

### `vm/content_store.go` -- No structural changes

The store indexes by `ContentHash` only. No new maps. `ClassDigest` gains an
optional typed hash field for future use but the class hash computation does
not change.

```go
type ClassDigest struct {
    // ... existing fields ...
    TypedMethodHashes [][32]byte // parallel to MethodHashes, zero entries = no types
    TypedHash         [32]byte   // class digest including typed method hashes
}
```

### `vm/dist/chunk.go` -- Add TypedHash to Chunk

```go
type Chunk struct {
    // ... existing fields ...
    TypedHash [32]byte `cbor:"9,keyasint,omitempty"` // typed content hash
}
```

### Image format v5

In `vm/image_writer.go` and `vm/image_reader.go`:

```
// v5: added typed hash (SHA-256) on compiled methods
const ImageVersion uint32 = 5
```

Writer: after writing `ContentHash` (32 bytes), write `TypedHash` (32 bytes).
Reader: if version >= 5, read 32 additional bytes into `TypedHash`.

---

## Hashing Algorithm

### Semantic hash (unchanged)

```
HashVersion(1B) | TagMethodDef | selector | arity | numTemps | primitive | docString | statements...
```

Exactly as today. No changes to existing serialization.

### Typed hash (new)

Same serialization as semantic hash, PLUS after each HMethodDef's existing
fields and after each HBlock's existing fields, write:

For HMethodDef:
```
0x01 (presence byte)
uint32(len(ParamTypes))
for each param type: writeString(name)   // "" for untyped
uint32(len(TempTypes))
for each temp type: writeString(name)
writeString(returnType.Name)             // "" for untyped
```

For HBlock:
```
0x01 (presence byte)
uint32(len(ParamTypes))
for each param type: writeString(name)
```

If the method/block has NO type annotations (all nil/empty), write `0x00`
instead of the above.

Type names are FQN-resolved through `resolveGlobal` (same as global variable
references), so `<Button>` in a file that imports Widgets becomes `Widgets::Button`.

---

## Files to Create

### `compiler/hash/typed.go`

New file containing:

```go
// HashTypedMethod computes the typed content hash of a method definition.
// It includes type annotations in the hash. Methods without type annotations
// produce a distinct hash from HashMethod (the typed domain includes a
// presence byte that the semantic domain does not).
func HashTypedMethod(method *compiler.MethodDef, instVars map[string]int, resolveGlobal func(string) string) [32]byte

// NormalizeTypedMethod transforms a compiler MethodDef into a frozen HMethodDef
// with type annotation fields populated.
func NormalizeTypedMethod(method *compiler.MethodDef, instVars map[string]int, resolveGlobal func(string) string) *HMethodDef
```

Implementation: calls `NormalizeMethod` then populates the type annotation
fields from `method.ParamTypes`, `method.TempTypes`, `method.ReturnType`.
Then calls `SerializeTyped(hm)` which uses the extended serialization.

### `compiler/hash/serialize_typed.go`

New file containing:

```go
// SerializeTyped produces a deterministic byte serialization that includes
// type annotations. The format is a superset of Serialize -- all semantic
// fields are written identically, with type annotation data appended after
// each method/block node.
func SerializeTyped(node HNode) []byte
```

This reuses the `serializer` type. The cleanest approach: embed `serializer`
into a `typedSerializer` that overrides `serializeNode` for HMethodDef and
HBlock cases to append type data.

Alternatively (simpler): add an `includeTypes bool` field to `serializer` and
branch in the HMethodDef/HBlock cases. This avoids a second type.

### `compiler/hash/testdata/typed_*.golden`

New golden files for typed hashing. At minimum:
- `typed_unary_no_types.golden` -- method with no type annotations
- `typed_with_param_types.golden` -- method with `<Integer>` param types
- `typed_with_return_type.golden` -- method with return type
- `typed_mixed.golden` -- some params typed, some not

---

## Files to Modify

### `compiler/hash/nodes.go`
- Add `HTypeAnnotation` struct
- Add `ParamTypes`, `TempTypes`, `ReturnType` fields to `HMethodDef`
- Add `ParamTypes` field to `HBlock`

### `compiler/hash/tags.go`
- Add typed presence bytes as constants (0x00 = absent, 0x01 = present)
- Add to `allTags` for uniqueness test

Actually, the presence bytes are not tags (they don't identify node types).
Define them as:
```go
const (
    TypesAbsent  byte = 0x00
    TypesPresent byte = 0x01
)
```

### `compiler/hash/serialize.go`
- Add `includeTypes bool` field to `serializer`
- In the HMethodDef case, after writing statements, if `includeTypes`:
  write type annotation data
- In the HBlock case, same
- Existing `Serialize()` function unchanged (sets `includeTypes = false`)

### `compiler/hash/hash.go`
- No changes to `HashMethod` (it stays as-is, computing semantic hash)

### `compiler/hash/normalize.go`
- `NormalizeMethod` unchanged
- New `normalizeTyped` helper populates type fields from AST TypeExpr slices

### `compiler/hash/golden_test.go`
- Add typed golden test cases

### `vm/compiled_method.go`
- Add `TypedHash [32]byte` field
- Add `GetTypedHash()`, `SetTypedHash()` methods

### `vm/content_store.go`
- Add `TypedMethodHashes` and `TypedHash` to `ClassDigest`
- `DigestClass` collects typed hashes alongside semantic hashes
- `HashClass` unchanged (class identity = semantic)
- Add `HashTypedClass` that uses typed method hashes

### `vm/image_writer.go`
- Bump `ImageVersion` to 5
- After writing `ContentHash`, write `TypedHash` (32 bytes)

### `vm/image_reader.go`
- Handle version >= 5: read 32 additional bytes for `TypedHash`

### `vm/dist/chunk.go`
- Add `TypedHash` field to `Chunk` struct

### `vm/dist/chunker.go`
- `MethodToChunk`: populate `TypedHash` from compiled method
- `ClassToChunk`: no change (class hash stays semantic)

### `vm/dist/wire.go`
- `VerifyChunkMethod`: optionally verify typed hash (add `compileTyped` param
  or second verify function)

### `cmd/mag/main.go` (or wherever hashing is wired)
- After computing `ContentHash` via `hash.HashMethod`, also compute
  `hash.HashTypedMethod` and set `TypedHash` on the compiled method

### Image test builder
- Update test image construction for v5 (add 32 zero bytes per method for
  typed hash)

---

## Implementation Order

### Step 1: Hashing infrastructure (half day)

1. Add `HTypeAnnotation` and typed fields to `HMethodDef` / `HBlock` in `nodes.go`
2. Add presence byte constants to `tags.go`
3. Add `includeTypes` to serializer, extend HMethodDef/HBlock cases in `serialize.go`
4. Create `typed.go` with `HashTypedMethod` and `NormalizeTypedMethod`
5. Create `serialize_typed.go` (or inline into serialize.go -- prefer inline with flag)
6. Add golden test cases for typed hashing
7. Run `go test ./compiler/hash/...` -- verify existing golden files unchanged

### Step 2: CompiledMethod + image format (half day)

1. Add `TypedHash` field and accessors to `compiled_method.go`
2. Bump image version to 5
3. Update `image_writer.go` to write `TypedHash`
4. Update `image_reader.go` to read `TypedHash` (v5+, zero for v4)
5. Update image test builder for v5
6. Regenerate `maggie.image` and `cmd/mag/maggie.image`
7. Run `go test ./vm/...`

### Step 3: Wire into compilation pipeline (quarter day)

1. In `cmd/mag/main.go` `compileAll` (or wherever `HashMethod` is called),
   also call `HashTypedMethod` and set `TypedHash`
2. Same for `cmd/bootstrap/main.go` if applicable
3. Run full test suite

### Step 4: Distribution integration (quarter day)

1. Add `TypedHash` to `Chunk` struct
2. Update `MethodToChunk` to populate `TypedHash`
3. Add `TypedMethodHashes` / `TypedHash` to `ClassDigest`
4. Update `DigestClass` to collect typed hashes
5. Add `VerifyChunkTypedMethod` (or extend `VerifyChunkMethod` with typed compile func)
6. Run `go test ./vm/dist/...`

### Step 5: Tests and cleanup (quarter day)

1. Integration test: compile a typed method, verify both hashes differ
2. Integration test: compile an untyped method, verify typed hash != semantic hash
   (presence byte difference)
3. Round-trip test: save image with typed hashes, reload, verify hashes preserved
4. Distribution test: chunk a typed method, verify TypedHash on chunk
5. Update golden files if any changed

---

## What This Does NOT Include

- Storing structured type annotations on `CompiledMethod` (separate phase)
- Protocol content hashing
- Type-aware class digests (class identity stays semantic)
- Any changes to the type checker itself
- Any changes to the runtime or bytecode
- Negotiation of typed vs untyped verification in the sync protocol (that's a
  policy decision for the sync service, not a data model change)

## Risk Assessment

**Low risk items:**
- New fields on structs (zero-value backward compat)
- New hash function alongside existing one (no changes to semantic hash)
- CBOR `omitempty` on new Chunk field (backward compat with older peers)

**Medium risk items:**
- Image format v5 requires regenerating 2 image files
- Image test builder needs updating (same pattern as v3->v4, well-understood)

**No risk items:**
- Existing `ContentHash` behavior is completely unchanged
- Existing golden files must not change (regression test)
- Existing distribution protocol still works (new field is optional/omitempty)
