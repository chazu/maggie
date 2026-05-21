# Image Format CBOR Migration: Implementation Plan

**Date:** 2026-05-21
**Architecture plan:** `docs/plans/2026-05-21-image-format-cbor-migration.md`
**Estimated effort:** 6-8 days across 6 phases

---

## Phase 1: CBOR Writer (2 days)

### 1.1 Define CBOR Tag Constants and Struct Types

**File:** `vm/image_cbor_tags.go` (new, ~100 lines)

```go
package vm

import "github.com/fxamacker/cbor/v2"

// Image CBOR tag range: 27100-27115 (separate from serial.go's 27001-27015)
const (
    imgTagHeader      = 27100
    imgTagStringTable = 27101
    imgTagSymbolEntry = 27102
    imgTagSelectorEntry = 27103
    imgTagClassDef    = 27104
    imgTagMethodDef   = 27105
    imgTagBlockDef    = 27106
    imgTagObjectDef   = 27107
    imgTagGlobalEntry = 27108
    imgTagClassVarEntry = 27109
    imgTagImageValue  = 27110
    imgTagValueRef    = 27111  // object back-reference by index
    imgTagClassRef    = 27112  // class reference by index
    imgTagMethodRef   = 27113  // method reference by index
    imgTagSymbolRef   = 27114  // symbol reference by index
    imgTagSourceMap   = 27115
)

// cborImageEncMode is the canonical CBOR encoding mode for images.
// Initialized in init(), shared across writer and reader.
var cborImageEncMode cbor.EncMode

func init() {
    em, err := cbor.CanonicalEncOptions().EncMode()
    if err != nil {
        panic("image_cbor: failed to create CBOR enc mode: " + err.Error())
    }
    cborImageEncMode = em
}
```

Wait -- `cborSerialEncMode` already exists in `serial.go` and is package-scoped. Reuse it directly instead of creating a duplicate. Define only the tag constants in this file, and reference `cborSerialEncMode` from the writer/reader.

**Revised `vm/image_cbor_tags.go`:**

```go
package vm

// Image CBOR tags (27100-27115). Separate range from serial.go (27001-27015).
const (
    imgTagHeader        uint64 = 27100
    imgTagStringTable   uint64 = 27101
    imgTagSymbolEntry   uint64 = 27102
    imgTagSelectorEntry uint64 = 27103
    imgTagClassDef      uint64 = 27104
    imgTagMethodDef     uint64 = 27105
    imgTagBlockDef      uint64 = 27106
    imgTagObjectDef     uint64 = 27107
    imgTagGlobalEntry   uint64 = 27108
    imgTagClassVarEntry uint64 = 27109
    imgTagValueRef      uint64 = 27111
    imgTagClassRef      uint64 = 27112
    imgTagMethodRef     uint64 = 27113
    imgTagSymbolRef     uint64 = 27114
    imgTagSourceMap     uint64 = 27115
)
```

### 1.2 Define CBOR Struct Types for Sections

**File:** `vm/image_cbor_types.go` (new, ~120 lines)

These structs use integer keys (`keyasint`) for compactness and `omitempty` for optional fields.

```go
package vm

// cborImageEnvelope is the top-level tagged structure.
// Wrapped in cbor.Tag{Number: imgTagHeader, Content: cborImageEnvelope{...}}
type cborImageEnvelope struct {
    Version    uint32            `cbor:"1,keyasint"`
    Flags      uint32            `cbor:"2,keyasint"`
    Stats      cborImageStats    `cbor:"3,keyasint"`
    Strings    []string          `cbor:"4,keyasint"`
    Symbols    []string          `cbor:"5,keyasint"`            // symbol names
    Selectors  []string          `cbor:"6,keyasint"`            // selector names
    Classes    []cborClassDef    `cbor:"7,keyasint"`
    Methods    []cborMethodDef   `cbor:"8,keyasint"`
    Objects    []cborObjectDef   `cbor:"9,keyasint"`
    Globals    []cborGlobalEntry `cbor:"10,keyasint"`
    ClassVars  []cborClassVarEntry `cbor:"11,keyasint,omitempty"`
    EntryPoint uint32            `cbor:"12,keyasint,omitempty"` // method index
}

type cborImageStats struct {
    Classes  uint32 `cbor:"1,keyasint"`
    Methods  uint32 `cbor:"2,keyasint"`
    Objects  uint32 `cbor:"3,keyasint"`
    Globals  uint32 `cbor:"4,keyasint"`
}

type cborClassDef struct {
    Name            uint32   `cbor:"1,keyasint"`            // string table index
    Namespace       int64    `cbor:"2,keyasint"`            // string index or -1
    Super           int64    `cbor:"3,keyasint"`            // class index or -1
    NumSlots        uint32   `cbor:"4,keyasint"`
    InstVars        []uint32 `cbor:"5,keyasint,omitempty"`  // string indices
    InstanceMethods []uint32 `cbor:"6,keyasint,omitempty"`  // method indices
    ClassMethods    []uint32 `cbor:"7,keyasint,omitempty"`  // method indices
    DocString       uint32   `cbor:"8,keyasint,omitempty"`  // string index
    HasDocString    bool     `cbor:"9,keyasint,omitempty"`  // disambiguates 0 vs absent
}

type cborMethodDef struct {
    Selector      int32            `cbor:"1,keyasint"`
    Class         int64            `cbor:"2,keyasint"`           // class index or -1
    Name          uint32           `cbor:"3,keyasint"`           // string index
    IsClassMethod bool             `cbor:"4,keyasint,omitempty"`
    Arity         uint32           `cbor:"5,keyasint"`
    NumTemps      uint32           `cbor:"6,keyasint"`
    Literals      []cbor.RawMessage `cbor:"7,keyasint"`          // each is an encoded image value
    Bytecode      []byte           `cbor:"8,keyasint"`
    Blocks        []cborBlockDef   `cbor:"9,keyasint,omitempty"`
    Source        uint32           `cbor:"10,keyasint,omitempty"` // string index
    HasSource     bool             `cbor:"11,keyasint,omitempty"`
    DocString     uint32           `cbor:"12,keyasint,omitempty"` // string index
    HasDocString  bool             `cbor:"13,keyasint,omitempty"`
    SourceMap     [][3]uint32      `cbor:"14,keyasint,omitempty"` // [offset, line, column]
    ContentHash   []byte           `cbor:"15,keyasint,omitempty"` // 32 bytes or empty
    TypedHash     []byte           `cbor:"16,keyasint,omitempty"` // 32 bytes or empty
}

type cborBlockDef struct {
    Arity       uint32             `cbor:"1,keyasint"`
    NumTemps    uint32             `cbor:"2,keyasint"`
    NumCaptures uint32             `cbor:"3,keyasint"`
    Literals    []cbor.RawMessage  `cbor:"4,keyasint,omitempty"`
    Bytecode    []byte             `cbor:"5,keyasint"`
    SourceMap   [][3]uint32        `cbor:"6,keyasint,omitempty"`
    Source      uint32             `cbor:"7,keyasint,omitempty"` // string index
    HasSource   bool               `cbor:"8,keyasint,omitempty"`
}

type cborObjectDef struct {
    Class int64              `cbor:"1,keyasint"`           // class index or -1
    Slots []cbor.RawMessage  `cbor:"2,keyasint,omitempty"` // encoded values
}

type cborGlobalEntry struct {
    Name  uint32           `cbor:"1,keyasint"` // string index
    Value cbor.RawMessage  `cbor:"2,keyasint"` // encoded value
}

type cborClassVarEntry struct {
    Class int64                      `cbor:"1,keyasint"` // class index
    Vars  []cborClassVarPair         `cbor:"2,keyasint"`
}

type cborClassVarPair struct {
    Name  uint32           `cbor:"1,keyasint"` // string index
    Value cbor.RawMessage  `cbor:"2,keyasint"` // encoded value
}
```

### 1.3 Image Value CBOR Encoder

**File:** `vm/image_cbor_writer.go` (new, ~400 lines)

**Key function:** `encodeImageValue` -- encodes a single `Value` to CBOR bytes using the image tag scheme. This is **not** the same as `serial.go`'s `SerializeValue` (which handles channels, remote processes, etc.). The image value encoder handles only the subset of types that appear in literals, slots, and globals.

```go
// encodeImageValue encodes a Value into CBOR bytes for image serialization.
// Uses the ImageEncoder for index lookups (objects, symbols, classes, strings).
func encodeImageValue(enc *ImageEncoder, v Value) (cbor.RawMessage, error)
```

Implementation logic (matches `EncodeValueTo` cases but produces CBOR):

| Value type | CBOR output |
|---|---|
| Nil | `cborSerialEncMode.Marshal(nil)` |
| True | `cborSerialEncMode.Marshal(true)` |
| False | `cborSerialEncMode.Marshal(false)` |
| SmallInt | `cborSerialEncMode.Marshal(v.SmallInt())` |
| Float | `cborSerialEncMode.Marshal(v.Float64())` |
| String | `cborSerialEncMode.Marshal(v)` as CBOR text (inline string, NOT index) |
| Character | `cborSerialEncMode.Marshal(cbor.Tag{Number: cborTagCharacter, Content: uint32(cp)})` -- reuse serial.go tag 27006 |
| Symbol | `cborSerialEncMode.Marshal(cbor.Tag{Number: imgTagSymbolRef, Content: enc.LookupSymbol(symID)})` |
| Object ref | `cborSerialEncMode.Marshal(cbor.Tag{Number: imgTagValueRef, Content: enc.LookupObject(ptr)})` |
| Class ref | `cborSerialEncMode.Marshal(cbor.Tag{Number: imgTagClassRef, Content: enc.LookupClass(cls)})` |
| BigInt | `cborSerialEncMode.Marshal(cbor.Tag{Number: cborTagBigIntPositive/Negative, Content: bi.Bytes()})` -- reuse serial.go tags |
| Unknown | Return error (fail loudly, not silently nil) |

**Key design decision for strings in literals:** Literal string values are stored as inline CBOR text strings (not string table indices). This is simpler and CBOR already handles deduplication at the format level. The string table is only used for names (class names, method names, variable names, selector names). This differs from the binary format where string literals used string table indices.

**Alternative considered:** Use string table indices for string literals too. Rejected because: (a) adds complexity with no significant size benefit (CBOR text strings are already compact), (b) the string table is built during `collectFromVM` which already handles literal strings -- we'd just be adding indirection.

**Actual decision:** Keep string table indices for consistency with the binary format's approach. String values in literals, slots, and globals use `imgTagSymbolRef`-style indirection via the string table. This keeps the envelope self-contained and allows the reader to decode values without the full string table loaded. Actually, the reader DOES have the string table loaded by the time it decodes values, so inline strings are fine. Use inline CBOR text strings for simplicity.

**Final decision:** String literals and string slot values are encoded as inline CBOR text strings. Symbol references use `imgTagSymbolRef + index`. This is the simplest correct approach.

### 1.4 CborImageWriter Implementation

**File:** `vm/image_cbor_writer.go` (continued)

```go
// CborImageWriter serializes VM state to a CBOR image.
type CborImageWriter struct {
    encoder *ImageEncoder  // reuse existing index tracking

    // Collected data (same as ImageWriter)
    strings     []string
    symbols     []uint32
    selectors   []int
    classes     []*Class
    methods     []*CompiledMethod
    objects     []*Object
    classVarData map[*Class]map[string]Value

    flags      uint32
    entryPoint uint32
}

// NewCborImageWriter creates a new CBOR image writer.
func NewCborImageWriter() *CborImageWriter

// collectFromVM collects all data from the VM for serialization.
// Reuses the exact same logic as ImageWriter.collectFromVM.
func (w *CborImageWriter) collectFromVM(vm *VM)
```

The `collectFromVM` method on `CborImageWriter` is a near-copy of `ImageWriter.collectFromVM`. To avoid duplication, extract the collection logic into a shared helper:

**Refactoring step:** Extract from `ImageWriter`:

```go
// imageCollector holds data collected from a VM for serialization.
// Shared between binary ImageWriter and CborImageWriter.
type imageCollector struct {
    encoder      *ImageEncoder
    strings      []string
    symbols      []uint32
    selectors    []int
    classes      []*Class
    methods      []*CompiledMethod
    objects      []*Object
    classVarData map[*Class]map[string]Value
}

// collectFromVM populates the collector from the VM.
// This is the format-agnostic collection phase.
func (c *imageCollector) collectFromVM(vm *VM)

// sortClassesByDependency returns classes in dependency order.
func (c *imageCollector) sortClassesByDependency() []*Class

// updateClassIndicesForSortedOrder re-indexes classes in sorted order.
func (c *imageCollector) updateClassIndicesForSortedOrder(sorted []*Class)
```

Move `collectFromVM`, `collectClass`, `collectMethod`, `collectBlock`, `registerString`, `registerSymbol`, `registerSelector`, `sortClassesByDependency`, and `updateClassIndicesForSortedOrder` from `ImageWriter` to `imageCollector`. The `ImageWriter` embeds `imageCollector` and delegates. `CborImageWriter` also embeds `imageCollector`.

**Files changed:**
- `vm/image_encoding.go` -- add `imageCollector` struct and methods (extracted from `image_writer.go`)
- `vm/image_writer.go` -- refactor `ImageWriter` to embed `imageCollector`
- `vm/image_cbor_writer.go` -- `CborImageWriter` embeds `imageCollector`

### 1.5 CBOR Image Serialization

**File:** `vm/image_cbor_writer.go`

```go
// WriteCborImage serializes the collected data to CBOR bytes.
func (w *CborImageWriter) WriteCborImage() ([]byte, error)
```

Step-by-step implementation:

1. Sort classes by dependency (call `w.sortClassesByDependency()`)
2. Update class indices for sorted order (call `w.updateClassIndicesForSortedOrder(sorted)`)
3. Build `cborImageEnvelope` struct:
   - `Version: 1` (CBOR format version, independent of binary version)
   - `Flags: w.flags`
   - `Stats`: counts from collected data
   - `Strings`: copy of `w.strings`
   - `Symbols`: for each `w.symbols[i]`, look up the symbol name via `vm.Symbols.Name(symID)`
     - **Problem:** The writer doesn't hold a `*VM` reference after collection. Solution: store `vm.Symbols` and `vm.Selectors` during collection, or store the resolved names during collection.
     - **Solution:** During `collectFromVM`, also build `symbolNames []string` and `selectorNames []string` lists. Add these fields to `imageCollector`.
   - `Selectors`: for each `w.selectors[i]`, look up selector name
   - `Classes`: for each class in sorted order, build `cborClassDef`
   - `Methods`: for each method, build `cborMethodDef`
   - `Objects`: for each object, build `cborObjectDef`
   - `Globals`: sorted by name, build `cborGlobalEntry`
   - `ClassVars`: sorted by class name, build `cborClassVarEntry`
4. Marshal the envelope as `cbor.Tag{Number: imgTagHeader, Content: envelope}`
5. Return the bytes

**Build cborClassDef (for each class in sorted order):**

```go
func (w *CborImageWriter) buildClassDef(c *Class) cborClassDef {
    nameIdx, _ := w.encoder.LookupString(c.Name)
    var nsIdx int64 = -1
    if c.Namespace != "" {
        idx, _ := w.encoder.LookupString(c.Namespace)
        nsIdx = int64(idx)
    }
    var superIdx int64 = -1
    if c.Superclass != nil {
        if idx, ok := w.encoder.LookupClass(c.Superclass); ok {
            superIdx = int64(idx)
        }
    }
    // ... build instVars, instanceMethods, classMethods lists
    // ... handle docstring
    return cborClassDef{...}
}
```

**Build cborMethodDef (for each method):**

```go
func (w *CborImageWriter) buildMethodDef(m *CompiledMethod) (cborMethodDef, error) {
    // Encode each literal via encodeImageValue
    literals := make([]cbor.RawMessage, len(m.Literals))
    for i, lit := range m.Literals {
        enc, err := encodeImageValue(w.encoder, lit)
        if err != nil {
            return cborMethodDef{}, fmt.Errorf("method %s literal %d: %w", m.name, i, err)
        }
        literals[i] = enc
    }
    // Build source map as [][3]uint32
    var sourceMap [][3]uint32
    for _, loc := range m.SourceMap {
        sourceMap = append(sourceMap, [3]uint32{uint32(loc.Offset), uint32(loc.Line), uint32(loc.Column)})
    }
    // Content hash: only include if non-zero
    var contentHash, typedHash []byte
    if m.ContentHash != [32]byte{} {
        contentHash = m.ContentHash[:]
    }
    if m.TypedHash != [32]byte{} {
        typedHash = m.TypedHash[:]
    }
    // ... fill remaining fields
    return cborMethodDef{...}, nil
}
```

**Build cborBlockDef:**

```go
func (w *CborImageWriter) buildBlockDef(b *BlockMethod) (cborBlockDef, error)
```

Same pattern as `buildMethodDef` but for `BlockMethod` fields.

**Build cborObjectDef:**

```go
func (w *CborImageWriter) buildObjectDef(obj *Object) (cborObjectDef, error) {
    var classIdx int64 = -1
    if obj.vtable != nil && obj.vtable.class != nil {
        if idx, ok := w.encoder.LookupClass(obj.vtable.class); ok {
            classIdx = int64(idx)
        }
    }
    slots := make([]cbor.RawMessage, obj.NumSlots())
    obj.ForEachSlot(func(index int, value Value) {
        enc, err := encodeImageValue(w.encoder, value)
        // handle error via captured error variable
        slots[index] = enc
    })
    return cborObjectDef{Class: classIdx, Slots: slots}, nil
}
```

### 1.6 VM Integration for CBOR Writer

**File:** `vm/image_writer.go` (modify)

Add:

```go
// SaveImageCbor saves the VM state to a CBOR image file.
func (vm *VM) SaveImageCbor(path string) error {
    data, err := vm.SaveImageCborBytes()
    if err != nil {
        return err
    }
    return os.WriteFile(path, data, 0644)
}

// SaveImageCborBytes returns the VM state as CBOR image bytes.
func (vm *VM) SaveImageCborBytes() ([]byte, error) {
    writer := NewCborImageWriter()
    writer.collectFromVM(vm)
    return writer.WriteCborImage()
}
```

### 1.7 Bootstrap CBOR Flag

**File:** `cmd/bootstrap/main.go` (modify)

Add `--cbor` flag:

```go
cborFlag := flag.Bool("cbor", false, "Write CBOR format image")
```

After compilation, branch on the flag:

```go
if *cborFlag {
    if err := vmInst.SaveImageCbor(*output); err != nil {
        fmt.Fprintf(os.Stderr, "Error saving CBOR image: %v\n", err)
        os.Exit(1)
    }
} else {
    if err := vmInst.SaveImage(*output); err != nil {
        fmt.Fprintf(os.Stderr, "Error saving image: %v\n", err)
        os.Exit(1)
    }
}
```

### 1.8 Phase 1 Tests

**File:** `vm/image_cbor_writer_test.go` (new, ~400 lines)

```go
func TestCborImageWriterEmpty(t *testing.T)
    // Write a fresh VM to CBOR, verify it produces valid CBOR, can be unmarshaled

func TestCborEncodeImageValueNil(t *testing.T)
func TestCborEncodeImageValueTrue(t *testing.T)
func TestCborEncodeImageValueFalse(t *testing.T)
func TestCborEncodeImageValueSmallInt(t *testing.T)
func TestCborEncodeImageValueFloat(t *testing.T)
func TestCborEncodeImageValueSymbol(t *testing.T)
func TestCborEncodeImageValueString(t *testing.T)
func TestCborEncodeImageValueCharacter(t *testing.T)
func TestCborEncodeImageValueObjectRef(t *testing.T)
func TestCborEncodeImageValueClassRef(t *testing.T)

func TestCborImageWriterStrings(t *testing.T)
    // Verify string table round-trips

func TestCborImageWriterClassDef(t *testing.T)
    // Create VM with a class, serialize, unmarshal envelope, check class fields

func TestCborImageWriterMethodDef(t *testing.T)
    // Create VM with compiled method, serialize, unmarshal, check method fields

func TestCborImageWriterObjectDef(t *testing.T)
    // Create VM with objects, serialize, unmarshal, check slots

func TestCborImageWriterGlobals(t *testing.T)

func TestCborImageWriterClassVars(t *testing.T)

func TestCborImageWriterDeterministic(t *testing.T)
    // Write twice, compare bytes — must be identical

func TestCborImageWriterBootstrapSmokeTest(t *testing.T)
    // Full VM write, verify no errors, verify output starts with CBOR tag bytes
```

**Test pattern:** Use `cbor.Unmarshal` to decode the raw CBOR output into `cborImageEnvelope` and inspect fields directly. This validates the CBOR structure without needing the reader.

### 1.9 Phase 1 Verification

After completing Phase 1:

1. `go build ./...` -- compiles
2. `go test ./vm/ -run TestCbor` -- all CBOR writer tests pass
3. `go run ./cmd/bootstrap/ --cbor -o /tmp/test.cbor.image` -- produces a CBOR image file
4. Verify the CBOR image file starts with `0xD9` or `0xDA` (CBOR tag prefix)
5. Verify file size is within 20% of binary image size (~580-870 KB)
6. Use `cbor-diag` or similar tool to inspect the CBOR structure (optional)

---

## Phase 2: CBOR Reader (2 days)

### 2.1 Image Value CBOR Decoder

**File:** `vm/image_cbor_reader.go` (new, ~500 lines)

```go
// decodeImageValue decodes a CBOR-encoded image value back to a Value.
// Uses the ImageDecoder for index lookups.
func decodeImageValue(dec *ImageDecoder, raw cbor.RawMessage) (Value, error)
```

Implementation logic:

1. Unmarshal the raw CBOR message into `interface{}` (or use `cbor.RawTag`)
2. Switch on the type:
   - `nil` -> `Nil`
   - `bool(true)` -> `True`
   - `bool(false)` -> `False`
   - `int64` / `uint64` -> `FromSmallInt(n)`
   - `float64` -> `FromFloat64(f)`
   - `string` -> `dec.registry.NewStringValue(s)` (inline string)
   - `cbor.Tag` -> switch on `tag.Number`:
     - `imgTagSymbolRef` -> `FromSymbolID(dec.GetSymbol(idx))`
     - `imgTagValueRef` -> `dec.GetObject(idx).ToValue()`
     - `imgTagClassRef` -> `dec.registry.RegisterClassValue(dec.GetClass(idx))`
     - `cborTagCharacter` (27006) -> `FromCharacter(rune(cp))`
     - `cborTagBigIntPositive` (27011) -> reconstruct BigInt
     - `cborTagBigIntNegative` (27012) -> reconstruct negative BigInt
   - default -> return error

**Critical subtlety:** When unmarshaling CBOR into `interface{}`, `fxamacker/cbor/v2` represents integers as `uint64` or `int64` depending on sign. The decoder must handle both. For floats, CBOR float64 maps to Go `float64`. CBOR integers in the range that overlaps with float must be correctly distinguished -- `fxamacker/cbor/v2` handles this correctly by type.

### 2.2 CborImageReader Implementation

```go
// CborImageReader reads a CBOR-encoded image and populates a VM.
type CborImageReader struct {
    envelope cborImageEnvelope
    decoder  *ImageDecoder

    // State built during reading
    classes       []*Class
    methods       []*CompiledMethod
    objects       []*Object
    selectorIDMap map[int]int  // image selector ID -> VM selector ID

    classNameToIndex map[string]uint32
}

// NewCborImageReader creates a reader from raw CBOR bytes.
func NewCborImageReader(data []byte) (*CborImageReader, error) {
    // Verify CBOR tag prefix
    // Unmarshal into cbor.Tag, verify Number == imgTagHeader
    // Unmarshal Content into cborImageEnvelope
    return &CborImageReader{...}, nil
}

// ReadAll reads the entire CBOR image and populates the VM.
func (cr *CborImageReader) ReadAll(vm *VM) error
```

### 2.3 ReadAll Implementation (step by step)

```go
func (cr *CborImageReader) ReadAll(vm *VM) error {
    cr.decoder = NewImageDecoder()
    cr.decoder.registry = vm.registry

    // 1. String table is already loaded (cr.envelope.Strings)
    for _, s := range cr.envelope.Strings {
        cr.decoder.AddString(s)
    }

    // 2. Intern symbols, build symbol mapping
    for imgIdx, name := range cr.envelope.Symbols {
        vmSymID := vm.Symbols.Intern(name)
        cr.decoder.SetSymbol(uint32(imgIdx), vmSymID)
    }

    // 3. Intern selectors, build remap table
    cr.selectorIDMap = make(map[int]int)
    for imgIdx, name := range cr.envelope.Selectors {
        vmSelectorID := vm.Selectors.Intern(name)
        cr.selectorIDMap[imgIdx] = vmSelectorID
    }

    // 4. Read classes (two-pass: create then link superclasses)
    if err := cr.readClasses(vm); err != nil {
        return fmt.Errorf("reading classes: %w", err)
    }

    // 5. Read methods
    if err := cr.readMethods(vm); err != nil {
        return fmt.Errorf("reading methods: %w", err)
    }

    // 6. Read objects (two-pass: allocate then fill slots)
    if err := cr.readObjects(vm); err != nil {
        return fmt.Errorf("reading objects: %w", err)
    }

    // 7. Read globals
    if err := cr.readGlobals(vm); err != nil {
        return fmt.Errorf("reading globals: %w", err)
    }

    // 8. Fix up class values (same as binary reader)
    cr.fixupClassValues(vm)

    // 9. Read class variables
    if err := cr.readClassVars(vm); err != nil {
        return fmt.Errorf("reading class vars: %w", err)
    }

    return nil
}
```

### 2.4 readClasses Implementation

```go
func (cr *CborImageReader) readClasses(vm *VM) error
```

- First pass: iterate `cr.envelope.Classes`, create or look up each class
  - For each `cborClassDef`:
    - Resolve name from string table: `cr.envelope.Strings[def.Name]`
    - Resolve namespace: if `def.Namespace >= 0`, `cr.envelope.Strings[def.Namespace]`
    - Check if class exists in VM (`vm.Classes.Lookup(lookupKey)`)
    - If exists: reuse it. If not: create with `NewClassWithInstVars` or `NewClass`
    - Register in `cr.decoder.AddClass(c)`
- Second pass: link superclasses
  - For each class where `def.Super >= 0`: set `c.Superclass = cr.classes[def.Super]`
  - Set up VTable/ClassVTable parent chains
  - Register class in `vm.Classes.Register(c)`

This is structurally identical to `ImageReader.ReadClasses` but reads from `cborClassDef` structs instead of binary bytes.

### 2.5 readMethods Implementation

```go
func (cr *CborImageReader) readMethods(vm *VM) error
```

For each `cborMethodDef` in `cr.envelope.Methods`:

1. Decode literals: for each `def.Literals[i]`, call `decodeImageValue(cr.decoder, raw)`
2. Copy bytecode: `def.Bytecode` (already a `[]byte`)
3. Remap selectors in bytecode: reuse the same `remapBytecodeSelectors` logic
   - **Refactoring needed:** Extract `remapBytecodeSelectors` from `ImageReader` into a standalone function:
     ```go
     func remapBytecodeSelectors(bytecode []byte, selectorIDMap map[int]int)
     ```
   - Both `ImageReader` and `CborImageReader` call this function.
4. Decode blocks: for each `cborBlockDef`, decode literals, copy bytecode, remap selectors
5. Reconstruct `SourceMap` from `[][3]uint32`
6. Reconstruct `ContentHash` and `TypedHash` from `[]byte` (copy into `[32]byte`)
7. Resolve method's VM selector ID: `vm.Selectors.Intern(methodName)`
8. Link to class and install in VTable (same as binary reader)

### 2.6 readObjects Implementation

```go
func (cr *CborImageReader) readObjects(vm *VM) error
```

Two-pass (same as binary):

- Pass 1: Allocate all objects
  ```go
  for i, def := range cr.envelope.Objects {
      class := cr.classes[def.Class]
      obj := NewObject(class.VTable, len(def.Slots))
      cr.objects[i] = obj
      cr.decoder.AddObject(obj)
  }
  ```

- Pass 2: Fill slots
  ```go
  for i, def := range cr.envelope.Objects {
      obj := cr.objects[i]
      for j, raw := range def.Slots {
          value, err := decodeImageValue(cr.decoder, raw)
          obj.SetSlot(j, value)
      }
  }
  ```

### 2.7 readGlobals, readClassVars, fixupClassValues

```go
func (cr *CborImageReader) readGlobals(vm *VM) error
```

For each `cborGlobalEntry`:
1. Resolve name: `cr.envelope.Strings[entry.Name]`
2. Decode value: `decodeImageValue(cr.decoder, entry.Value)`
3. `vm.SetGlobal(name, value)`

```go
func (cr *CborImageReader) readClassVars(vm *VM) error
```

For each `cborClassVarEntry`:
1. Resolve class: `cr.classes[entry.Class]`
2. For each `cborClassVarPair`:
   - Resolve name from string table
   - Decode value
   - `vm.registry.SetClassVar(class, name, value)`

```go
func (cr *CborImageReader) fixupClassValues(vm *VM)
```

Same logic as `ImageReader.ReadAll` lines 1291-1307 -- iterate globals, re-register class values.

### 2.8 Format Auto-Detection

**File:** `vm/image_reader.go` (modify `LoadImageFrom`)

```go
func (vm *VM) LoadImageFrom(r io.Reader) error {
    data, err := io.ReadAll(r)
    if err != nil {
        return fmt.Errorf("failed to read image data: %w", err)
    }

    if len(data) < 4 {
        return ErrCorruptHeader
    }

    // Auto-detect format:
    // CBOR tagged value starts with 0xD9 (2-byte tag) or 0xDA (4-byte tag)
    // Binary image starts with 'M' (0x4D) from "MAGI"
    switch {
    case data[0] == 'M':
        // Legacy binary format
        return vm.LoadImageFromBytes(data)
    case data[0] == 0xD9 || data[0] == 0xDA:
        // CBOR format
        return vm.loadImageFromCborBytes(data)
    default:
        return fmt.Errorf("unrecognized image format: first byte 0x%02X", data[0])
    }
}

// loadImageFromCborBytes loads a CBOR image from bytes.
func (vm *VM) loadImageFromCborBytes(data []byte) error {
    cr, err := NewCborImageReader(data)
    if err != nil {
        return fmt.Errorf("failed to parse CBOR image: %w", err)
    }
    return cr.ReadAll(vm)
}
```

Also update `LoadImageFromBytes`:
```go
func (vm *VM) LoadImageFromBytes(data []byte) error {
    // For backward compat: if called directly with binary data, use binary reader
    // If called with CBOR data, use CBOR reader
    if len(data) > 0 && (data[0] == 0xD9 || data[0] == 0xDA) {
        return vm.loadImageFromCborBytes(data)
    }
    return vm.LoadImageFrom(bytes.NewReader(data))
}
```

Wait -- this creates a recursion issue. Better approach: keep `LoadImageFromBytes` simple, have `LoadImageFrom` call it:

```go
func (vm *VM) LoadImageFrom(r io.Reader) error {
    data, err := io.ReadAll(r)
    if err != nil {
        return fmt.Errorf("failed to read image data: %w", err)
    }
    return vm.LoadImageFromBytes(data)
}

func (vm *VM) LoadImageFromBytes(data []byte) error {
    if len(data) < 4 {
        return ErrCorruptHeader
    }
    switch {
    case data[0] == 'M':
        ir, err := NewImageReaderFromBytes(data)
        if err != nil {
            return err
        }
        return ir.ReadAll(vm)
    case data[0] == 0xD9 || data[0] == 0xDA:
        cr, err := NewCborImageReader(data)
        if err != nil {
            return err
        }
        return cr.ReadAll(vm)
    default:
        return fmt.Errorf("unrecognized image format: first byte 0x%02X", data[0])
    }
}
```

### 2.9 Extract remapBytecodeSelectors as Standalone Function

**File:** `vm/image_reader.go` (modify)

Change `(ir *ImageReader) remapBytecodeSelectors` to a package-level function:

```go
// remapBytecodeSelectors scans bytecode and remaps selector IDs from image space to VM space.
func remapBytecodeSelectors(bytecode []byte, selectorIDMap map[int]int) {
    // ... same body, but uses selectorIDMap parameter instead of ir.selectorIDMap
}
```

Update the one call site in `ImageReader.readMethod` to pass `ir.selectorIDMap`.

### 2.10 Phase 2 Tests

**File:** `vm/image_cbor_reader_test.go` (new, ~500 lines)

```go
func TestCborImageReaderEmpty(t *testing.T)
    // Write empty VM to CBOR, read it back, verify VM state

func TestCborRoundTripStrings(t *testing.T)
    // Write VM with symbols, read back, verify symbols interned

func TestCborRoundTripClasses(t *testing.T)
    // Write VM with custom class hierarchy, read back, verify hierarchy

func TestCborRoundTripMethods(t *testing.T)
    // Write VM with compiled methods, read back, invoke a method

func TestCborRoundTripObjects(t *testing.T)
    // Write VM with objects, read back, verify slot values

func TestCborRoundTripObjectCycles(t *testing.T)
    // Create two objects referencing each other, round-trip, verify identity

func TestCborRoundTripObjectIdentity(t *testing.T)
    // Same object in two globals, round-trip, verify pointer identity

func TestCborRoundTripGlobals(t *testing.T)
    // Write globals with various value types, read back, verify

func TestCborRoundTripClassVars(t *testing.T)

func TestCborRoundTripSelectorRemapping(t *testing.T)
    // Pre-intern selectors in target VM to create ID mismatch, verify remap works

func TestCborRoundTripContentHash(t *testing.T)
    // Method with non-zero content hash, verify preserved

func TestCborRoundTripLiteralTypes(t *testing.T)
    // Method with all literal types: int, float, symbol, string, nil, true, false, char

func TestCborFormatAutoDetection(t *testing.T)
    // Write binary image, load with auto-detect (should use binary reader)
    // Write CBOR image, load with auto-detect (should use CBOR reader)

func TestCborCrossFormatEquivalence(t *testing.T)
    // Bootstrap VM, save as binary, save as CBOR
    // Load each into fresh VMs
    // Compare: all classes, all methods, all globals, all class vars match
```

This last test is the most critical -- it proves the CBOR format is equivalent to binary.

### 2.11 Phase 2 Verification

After completing Phase 2:

1. `go test ./vm/ -run TestCbor` -- all CBOR reader and writer tests pass
2. `go test ./vm/ -run TestCborCrossFormat` -- cross-format equivalence verified
3. `go run ./cmd/bootstrap/ --cbor -o /tmp/test.cbor.image && go run ./cmd/mag/ -image /tmp/test.cbor.image -e '"hello" printNl'` -- loads and runs
4. Full test suite still passes with binary images: `go test ./...`

---

## Phase 3: Switch Default to CBOR (0.5 days)

### 3.1 Bootstrap Default

**File:** `cmd/bootstrap/main.go` (modify)

Change the default format to CBOR. Replace the binary `SaveImage` call:

```go
// Replace:
//   if err := vmInst.SaveImage(*output); err != nil {
// With:
if err := vmInst.SaveImageCbor(*output); err != nil {
    fmt.Fprintf(os.Stderr, "Error saving image: %v\n", err)
    os.Exit(1)
}
```

Remove the `--cbor` flag (no longer needed; could add `--legacy-binary` for escape hatch).

### 3.2 SaveImageTo Default

**File:** `vm/image_writer.go` (modify `SaveImageTo`)

```go
func (vm *VM) SaveImageTo(w io.Writer) error {
    data, err := vm.SaveImageCborBytes()
    if err != nil {
        return err
    }
    _, err = w.Write(data)
    return err
}
```

This means `SaveImage`, `SaveImageAtomic`, and all callers now produce CBOR.

### 3.3 Rebuild Images

```bash
go run ./cmd/bootstrap/ && cp maggie.image cmd/mag/
```

### 3.4 Phase 3 Verification

1. `go test ./...` -- full test suite passes with CBOR images
2. `go run ./cmd/mag/ -e '"hello world" printNl'` -- works
3. Verify `maggie.image` starts with CBOR tag bytes, not `MAGI`
4. Run any existing integration tests / example scripts

---

## Phase 4: Remove Binary Writer (0.5 days)

### 4.1 Delete Binary Write Methods from ImageWriter

**File:** `vm/image_writer.go`

Remove these methods:
- `writeHeader()`
- `patchHeader()`
- `writeStringTable()`
- `writeSymbolTable()`
- `writeSelectorTable()`
- `writeClasses()`
- `writeClass()`
- `writeMethods()`
- `writeMethod()`
- `writeBlock()`
- `writeObjects()`
- `writeObject()`
- `writeGlobals()`
- `writeClassVars()`

Remove the `ImageWriter` struct (its role is replaced by `CborImageWriter`).

Keep:
- `SaveImage()`, `SaveImageAtomic()`, `SaveImageTo()` -- now delegate to CBOR
- `CollectAllObjects()` -- used by both formats and possibly elsewhere
- `SaveImageCbor()`, `SaveImageCborBytes()` -- the active writer

Remove:
- `ImageWriter.buf`, `ImageWriter.stringTableOffset` and all offset fields
- All binary format constants: `ImageMagic`, `ImageVersion`, `ImageHeaderSize`, `ImageFlagNone`, `ImageFlagDebugInfo`, `ImageFlagCompressed`
  - **Wait:** `ImageMagic` is still needed by the binary READER for auto-detection. Keep it until Phase 5.

### 4.2 Remove Binary Encoding from image_encoding.go

**File:** `vm/image_encoding.go`

Remove:
- `EncodeValue()` and `EncodeValueTo()` methods on `ImageEncoder`
- `DecodeValue()` method on `ImageDecoder` -- **Wait:** still needed by binary reader. Keep until Phase 5.
- All `imageTag*` constants (`imageTagFloat`, `imageTagSmallInt`, etc.) -- keep until Phase 5
- `EncodedValueSize` constant -- keep until Phase 5

Keep:
- `ImageEncoder` struct and all Register/Lookup methods
- `ImageDecoder` struct and all Add/Set/Get methods
- `imageCollector` (extracted in Phase 1)
- Binary encoding helpers (`WriteUint32`, `ReadUint32`, etc.) -- still used by binary reader

### 4.3 Phase 4 Verification

1. `go build ./...` -- compiles (no binary write code referenced)
2. `go test ./...` -- all tests pass
3. Verify no test uses `ImageWriter` directly (search for `NewImageWriter()` in tests)

---

## Phase 5: Remove Binary Reader (0.5 days)

### 5.1 Delete Binary Reader

**File:** `vm/image_reader.go`

Remove:
- `ImageReader` struct and all its methods
- `NewImageReader()`, `NewImageReaderFromBytes()`
- `ReadHeader()`, `ReadStringTable()`, `ReadSymbolTable()`, `ReadSelectorTable()`
- `ReadClasses()`, `ReadMethods()`, `ReadObjectsWithSlots()`, `ReadGlobals()`, `ReadClassVars()`
- `readMethod()`, `readBlock()`
- `readUint32()`, `readUint64()`, `readBytes()`, `readString()`, `remaining()`, `validateAllocation()`
- `ImageHeader` struct

Keep:
- `LoadImage()`, `LoadImageFrom()`, `LoadImageFromBytes()` -- now only route to CBOR reader
- Error variables that the CBOR reader uses (or remove and use new ones)
- `remapBytecodeSelectors()` standalone function (used by CBOR reader)

Update `LoadImageFromBytes` to remove the binary path:
```go
func (vm *VM) LoadImageFromBytes(data []byte) error {
    cr, err := NewCborImageReader(data)
    if err != nil {
        return fmt.Errorf("failed to parse CBOR image: %w", err)
    }
    return cr.ReadAll(vm)
}
```

### 5.2 Delete Binary Encoding Infrastructure

**File:** `vm/image_encoding.go`

Remove:
- All `imageTag*` constants
- `EncodedValueSize`
- `EncodeValue()`, `EncodeValueTo()` (if not already removed)
- `DecodeValue()`
- Binary helpers: `WriteUint64`, `ReadUint64`, `WriteInt64`, `ReadInt64`, `WriteUint32`, `ReadUint32`, `WriteInt32`, `ReadInt32`, `WriteUint16`, `ReadUint16`, `WriteFloat64`, `ReadFloat64`
  - **Check:** Are these used elsewhere? Search codebase.
  - `WriteUint32`/`ReadUint32` are used in `image_writer_test.go` test builders and potentially in other packages. Search before deleting.
  - If used elsewhere, keep them.
- `WriteVarInt`, `ReadVarInt`, `WriteSignedVarInt`, `ReadSignedVarInt`, `WriteString`, `ReadString`, `StringEncodedSize` -- only used by binary image format
- `ImageMagic`, `ImageVersion`, `ImageHeaderSize`, image flag constants

### 5.3 Remove Binary Format Constants

**File:** `vm/image_writer.go`

Remove:
- `ImageMagic`
- `ImageVersion`
- `ImageHeaderSize`
- `ImageFlagNone`, `ImageFlagDebugInfo`, `ImageFlagCompressed`

### 5.4 Delete Binary Test Infrastructure

**Files to modify:**
- `vm/image_reader_test.go` -- delete `testImageBuilder` and all binary-format tests. Keep any CBOR tests added here.
- `vm/image_writer_test.go` -- delete tests that assert binary format structure (magic bytes, header layout, etc.)
- `vm/image_encoding_test.go` -- delete `EncodeValue`/`DecodeValue` round-trip tests, keep `ImageEncoder` registration tests (Register/Lookup are still used by CBOR writer)
- `vm/image_reader_fuzz_test.go` -- delete (binary fuzz test)

**New fuzz test:** `vm/image_cbor_fuzz_test.go`

```go
func FuzzCborImageReader(f *testing.F) {
    // Seed with a valid CBOR image
    vm := NewVM()
    validImage, _ := vm.SaveImageCborBytes()
    f.Add(validImage)

    f.Fuzz(func(t *testing.T, data []byte) {
        vm := NewVM()
        // Must not panic
        _ = vm.LoadImageFromBytes(data)
    })
}
```

### 5.5 Phase 5 Verification

1. `go build ./...` -- compiles with no binary reader/writer code
2. `go test ./...` -- all tests pass
3. `grep -r "ImageReader\|testImageBuilder\|ImageMagic\|ReadUint32\|WriteUint32" vm/` -- verify no stale references
4. Verify no other packages import the removed helpers

---

## Phase 6: Cleanup and Consolidation (0.5 days)

### 6.1 Merge CBOR Writer into image_writer.go

**File operations:**
1. Move contents of `vm/image_cbor_writer.go` into `vm/image_writer.go`
2. Delete `vm/image_cbor_writer.go`
3. Rename `CborImageWriter` to `ImageWriter` (it's the only writer now)
4. Rename `SaveImageCbor` to `SaveImage`, `SaveImageCborBytes` to `SaveImageBytes`

### 6.2 Merge CBOR Reader into image_reader.go

**File operations:**
1. Move contents of `vm/image_cbor_reader.go` into `vm/image_reader.go`
2. Delete `vm/image_cbor_reader.go`
3. Rename `CborImageReader` to `ImageReader` (it's the only reader now)
4. Rename `loadImageFromCborBytes` to an internal function or inline it

### 6.3 Merge CBOR Types and Tags

**File operations:**
1. Move `vm/image_cbor_tags.go` contents into `vm/image_encoding.go`
2. Move `vm/image_cbor_types.go` contents into `vm/image_encoding.go`
3. Delete the standalone files

### 6.4 Simplify image_encoding.go

**File:** `vm/image_encoding.go`

After merging, this file contains:
- CBOR image tag constants
- CBOR struct types for sections
- `ImageEncoder` (index tracking, used by writer)
- `ImageDecoder` (index tracking, used by reader)
- `imageCollector` (collection phase, used by writer)
- `encodeImageValue` function
- `decodeImageValue` function

Remove:
- Any leftover binary encoding helpers
- `ImageDecoder.DecodeValue` (replaced by `decodeImageValue`)
- `ImageEncoder.EncodeValue` / `EncodeValueTo` (replaced by `encodeImageValue`)

### 6.5 Merge Test Files

**File operations:**
1. Move `vm/image_cbor_writer_test.go` tests into `vm/image_writer_test.go`
2. Move `vm/image_cbor_reader_test.go` tests into `vm/image_reader_test.go`
3. Delete the standalone test files
4. Move `vm/image_cbor_fuzz_test.go` into `vm/image_reader_fuzz_test.go`

### 6.6 Update Documentation

**File:** `CLAUDE.md`

Update the image format section:
- Remove references to binary format version numbers (v1-v5)
- Update `Image format is v4` note to `Image format is CBOR-based (self-versioning)`
- Update any references to `ImageVersion` constant

**File:** `docs/USER_GUIDE.md` (if it references image internals)

### 6.7 Final File Structure

After cleanup:

| File | Contents | Est. Lines |
|---|---|---|
| `vm/image_writer.go` | `ImageWriter`, `SaveImage*`, `CollectAllObjects`, CBOR encoding | ~500 |
| `vm/image_reader.go` | `ImageReader`, `LoadImage*`, `remapBytecodeSelectors`, CBOR decoding | ~600 |
| `vm/image_encoding.go` | Tags, types, `ImageEncoder`, `ImageDecoder`, `imageCollector`, value encode/decode | ~350 |
| **Total** | | **~1,450** |

Down from 3,124 lines (image_writer: 1,100 + image_reader: 1,353 + image_encoding: 671).

### 6.8 Phase 6 Verification

1. `go build ./...`
2. `go test ./...`
3. `go run ./cmd/bootstrap/ && cp maggie.image cmd/mag/`
4. `go run ./cmd/mag/ -e '"hello world" printNl'`
5. `go test ./... -count=1` -- all tests pass from clean state
6. Line count check: `wc -l vm/image_writer.go vm/image_reader.go vm/image_encoding.go` -- under 1,500

---

## Cross-Cutting Concerns

### Determinism

The binary writer sorts classes by dependency (depth then name), globals by name, class variables by class name then variable name. The CBOR writer must maintain all of these sort orders. This is handled by `imageCollector.sortClassesByDependency()` and the sorted iteration in `WriteCborImage()`.

CBOR maps with integer keys are ordered by `cbor.CanonicalEncOptions()` (CTAP2 canonical CBOR: keys sorted by length then bytewise). Since we use `keyasint` struct tags, key order is deterministic.

### Selector Remapping Invariant

Selector remapping is critical for correctness. The invariant is:

1. Image stores selectors by **name** (string)
2. On load, each selector name is interned in the target VM's `SelectorTable`
3. The resulting VM selector ID may differ from the image's ordinal position
4. A remap table maps `imageOrdinal -> vmSelectorID`
5. All bytecode `OpSend`/`OpSendSuper`/`OpTailSend` instructions are patched in-place

The CBOR format makes this cleaner: selectors are stored as strings in `cborImageEnvelope.Selectors`, and the reader builds the remap table from ordinal position to `vm.Selectors.Intern(name)`.

### Object Identity Guarantee

The round-trip test `TestCborRoundTripObjectIdentity` must verify:

```go
// Create an object, reference it from two globals
obj := NewObject(someClass.VTable, 1)
vm.SetGlobal("ref1", obj.ToValue())
vm.SetGlobal("ref2", obj.ToValue())

// Save and reload
data, _ := vm.SaveImageCborBytes()
vm2 := NewVM()
vm2.LoadImageFromBytes(data)

// Both globals must point to the same *Object
ref1, _ := vm2.Global("ref1")
ref2, _ := vm2.Global("ref2")
if ref1 != ref2 {
    t.Error("object identity not preserved")
}
```

### Atomic Save Compatibility

`SaveImageAtomic` calls `SaveImageTo` which now writes CBOR. No changes needed -- the atomic write protocol (tmp + fsync + rename) is format-agnostic.

### Content Store Interaction

`ContentStore` indexes methods by content hash. The CBOR format preserves content hashes (32-byte `ContentHash` and `TypedHash` fields). After loading a CBOR image, the content store should be populated by iterating all loaded methods and calling `store.IndexMethod(cm)`, same as the bootstrap does. This is currently NOT done by the binary reader either -- verify whether it's needed at load time or only at bootstrap time.

### Performance Benchmark

Add in Phase 2:

```go
func BenchmarkLoadImageBinary(b *testing.B) {
    // Bootstrap a VM, save as binary
    vm := setupBenchmarkVM()
    var buf bytes.Buffer
    vm.SaveImageTo(&buf)  // binary
    data := buf.Bytes()

    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        vm := NewVM()
        vm.LoadImageFromBytes(data)
    }
}

func BenchmarkLoadImageCBOR(b *testing.B) {
    // Bootstrap a VM, save as CBOR
    vm := setupBenchmarkVM()
    data, _ := vm.SaveImageCborBytes()

    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        vm := NewVM()
        vm.LoadImageFromBytes(data)
    }
}
```

Acceptance: CBOR load time within 5x of binary (likely within 2x).

---

## Risk Checklist

| Risk | Mitigation | Verified by |
|---|---|---|
| Object identity lost | Two-pass object allocation + index-based refs | `TestCborRoundTripObjectIdentity` |
| Circular references | Two-pass: allocate all, then fill slots | `TestCborRoundTripObjectCycles` |
| Selector remap broken | Standalone `remapBytecodeSelectors` + test with pre-interned selectors | `TestCborRoundTripSelectorRemapping` |
| Non-deterministic output | `CanonicalEncOptions`, sorted collections | `TestCborImageWriterDeterministic` |
| Class hierarchy order | `sortClassesByDependency` (depth + name sort) | `TestCborRoundTripClasses` |
| Content hash preservation | `ContentHash`/`TypedHash` fields in `cborMethodDef` | `TestCborRoundTripContentHash` |
| Format auto-detect fails | First-byte check (`M` vs `0xD9`/`0xDA`) | `TestCborFormatAutoDetection` |
| CBOR load too slow | Benchmark, accept if < 50ms | `BenchmarkLoadImageCBOR` |
| BigInt in literals | Reuse serial.go tags 27011/27012 | `TestCborEncodeImageValueBigInt` |
| String literal encoding | Inline CBOR text strings | `TestCborRoundTripLiteralTypes` |

---

## File Change Summary

### New Files (temporary, merged in Phase 6)
- `vm/image_cbor_tags.go` (~30 lines) -- Phase 1
- `vm/image_cbor_types.go` (~120 lines) -- Phase 1
- `vm/image_cbor_writer.go` (~400 lines) -- Phase 1
- `vm/image_cbor_reader.go` (~500 lines) -- Phase 2
- `vm/image_cbor_writer_test.go` (~400 lines) -- Phase 1
- `vm/image_cbor_reader_test.go` (~500 lines) -- Phase 2
- `vm/image_cbor_fuzz_test.go` (~30 lines) -- Phase 5

### Modified Files
- `vm/image_encoding.go` -- Phase 1: extract `imageCollector`; Phase 4-6: remove binary helpers
- `vm/image_writer.go` -- Phase 1: add CBOR entry points; Phase 3: switch default; Phase 4: gut binary; Phase 6: absorb CBOR writer
- `vm/image_reader.go` -- Phase 2: format auto-detect, extract `remapBytecodeSelectors`; Phase 5: gut binary; Phase 6: absorb CBOR reader
- `cmd/bootstrap/main.go` -- Phase 1: add `--cbor` flag; Phase 3: switch default

### Deleted Files (Phase 5-6)
- Binary-specific test helpers in `vm/image_reader_test.go`
- Binary-specific test helpers in `vm/image_writer_test.go`
- Binary encoding tests in `vm/image_encoding_test.go`
- `vm/image_reader_fuzz_test.go` (replaced by CBOR fuzz)
