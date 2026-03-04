# Architecture Research: Maggie VM Improvements

Date: 2026-03-03

---

## TODO 9: Break Up ObjectRegistry

### Current Implementation

The `ObjectRegistry` struct (`vm/object_registry.go`, 1071 lines) is a monolithic type that holds **every** VM-local registry. It embeds `*ConcurrencyRegistry` (`vm/concurrency_registry.go`, 472 lines) and directly declares 20 additional registry groups, each following an identical pattern:

| Registry | Key Type | Value Type | Mutex | Sweep? |
|----------|----------|------------|-------|--------|
| **ConcurrencyRegistry (embedded):** | | | | |
| channels | `int` | `*ChannelObject` | `sync.RWMutex` | Yes (closed) |
| processes | `uint64` | `*ProcessObject` | `sync.RWMutex` | Yes (done) |
| mutexes | `int` | `*MutexObject` | `sync.RWMutex` | No |
| waitGroups | `int` | `*WaitGroupObject` | `sync.RWMutex` | No |
| semaphores | `int` | `*SemaphoreObject` | `sync.RWMutex` | No |
| cancellationContexts | `int` | `*CancellationContextObject` | `sync.RWMutex` | Yes (cancelled) |
| blocks | `int` | `*BlockValue` | `sync.RWMutex` | Yes (frame validity) |
| **ObjectRegistry (direct):** | | | | |
| exceptions | `uint32` | `*ExceptionObject` | `sync.RWMutex` | Yes (handled) |
| results | `int` | `*ResultObject` | `sync.RWMutex` | No |
| contexts | `uint32` | `*ContextValue` | `sync.RWMutex` | No |
| dictionaries | `uint32` | `*DictionaryObject` | `sync.RWMutex` | No |
| strings | `uint32` | `*StringObject` | `sync.RWMutex` | No |
| grpcClients | `int` | `*GrpcClientObject` | `sync.RWMutex` | Yes (closed) |
| grpcStreams | `int` | `*GrpcStreamObject` | `sync.RWMutex` | No |
| httpServers | `int` | `*HttpServerObject` | `sync.RWMutex` | Yes (stopped) |
| httpRequests | `int` | `*HttpRequestObject` | `sync.RWMutex` | No |
| httpResponses | `int` | `*HttpResponseObject` | `sync.RWMutex` | No |
| extProcesses | `int` | `*ExternalProcessObject` | `sync.RWMutex` | No |
| unixListeners | `int` | `*UnixListenerObject` | `sync.RWMutex` | No |
| unixConns | `int` | `*UnixConnObject` | `sync.RWMutex` | No |
| jsonReaders | `int` | `*JsonReaderObject` | `sync.RWMutex` | No |
| jsonWriters | `int` | `*JsonWriterObject` | `sync.RWMutex` | No |
| goObjects | `uint32` | `*GoObjectWrapper` | `sync.RWMutex` | No |
| cueContexts | `int` | `*CueContextObject` | `sync.RWMutex` | No |
| cueValues | `int` | `*CueValueObject` | `sync.RWMutex` | No |
| classValues | `int` | `*Class` | `sync.RWMutex` | No |
| **Special registries:** | | | | |
| cells | `*Cell` (pointer) | `struct{}` (set) | `sync.Mutex` | No |
| classVars | `*Class` | `map[string]Value` | `sync.RWMutex` | No |
| weakRefCounter | N/A (atomic only) | N/A | N/A | N/A |

Every standard registry follows an identical pattern:
1. A `map[K]*V` field
2. A `sync.RWMutex` field
3. An `atomic.Int32` or `atomic.Uint32` ID counter
4. `Register(obj) -> id`, `Get(id) -> obj`, `Unregister(id)`, `Count() -> int`
5. Optional `Sweep*()` for GC-eligible registries

The `NewObjectRegistry()` constructor (line 128) initializes all 22+ maps and sets all ID counters to their starting values, producing a ~200-line function.

#### Cross-Registry Operations

There are **no** cross-registry operations within `ObjectRegistry` itself. Each registry group is fully independent -- `Register*` and `Get*` methods only touch their own map and mutex. The only cross-registry coordination is:

1. `FullStats()` (line 1050) calls `Count()` on every registry to build a stats map.
2. `RegistryGC.sweep()` (`vm/registry_gc.go:152`) calls `SweepChannels`, `SweepProcesses`, `SweepCancellationContexts`, and `SweepExceptions` in sequence. Each sweep is independent; there is no transactional coordination.
3. `ConcurrencyRegistry.Sweep()` (line 452) sweeps channels, processes (but not blocks, which need frame info).

#### GC Coordination

The `RegistryGC` (`vm/registry_gc.go`) runs on a 30-second timer and sweeps only 4 registries (channels, processes, cancellation contexts, exceptions). It does not need to coordinate across registries; each sweep is self-contained. Block sweeping is delegated to the interpreter via `ReleaseBlocksForFrame`.

### Analysis

The core problem is purely structural: the file is 1071 lines of mechanically repeated code. Each registry group is ~30-40 lines of boilerplate (Register/Get/Unregister/Count/optional Sweep). The lack of cross-registry dependencies means extraction is safe.

Go 1.18+ generics make this a natural candidate for a generic `Registry[K, V]` type. The three special registries (cells, classVars, weakRefCounter) do not fit the generic pattern and should remain as-is.

The mixed key types (`int`, `uint32`, `uint64`) mean the generic type needs `K comparable`. The ID counter type variation (`atomic.Int32`, `atomic.Uint32`, `atomic.Uint64`) can be abstracted via an `IDGenerator` interface or by standardizing on `uint64` internally and narrowing at the boundary.

### Recommended Fix

#### Step 1: Define the Generic Registry Type

Create `vm/typed_registry.go`:

```go
package vm

import (
    "sync"
    "sync/atomic"
)

// TypedRegistry is a concurrent, ID-keyed registry for a single object type.
// K is the key type (typically int or uint32), V is the value type (pointer).
type TypedRegistry[K comparable, V any] struct {
    items map[K]V
    mu    sync.RWMutex
    nextID atomic.Uint64  // unified counter, narrowed at Register()
}

func NewTypedRegistry[K comparable, V any]() *TypedRegistry[K, V] {
    return &TypedRegistry[K, V]{
        items: make(map[K]V),
    }
}

// SetStartID sets the starting ID. Call before first Register.
func (r *TypedRegistry[K, V]) SetStartID(start uint64) {
    r.nextID.Store(start)
}

// Register adds an item and returns the auto-generated key.
// The keyFn converts the uint64 counter to the concrete key type.
func (r *TypedRegistry[K, V]) Put(key K, val V) {
    r.mu.Lock()
    r.items[key] = val
    r.mu.Unlock()
}

func (r *TypedRegistry[K, V]) Get(key K) (V, bool) {
    r.mu.RLock()
    defer r.mu.RUnlock()
    v, ok := r.items[key]
    return v, ok
}

func (r *TypedRegistry[K, V]) Delete(key K) {
    r.mu.Lock()
    delete(r.items, key)
    r.mu.Unlock()
}

func (r *TypedRegistry[K, V]) Count() int {
    r.mu.RLock()
    defer r.mu.RUnlock()
    return len(r.items)
}

func (r *TypedRegistry[K, V]) NextID() uint64 {
    return r.nextID.Add(1)
}

// Sweep removes items matching the predicate. Returns count removed.
func (r *TypedRegistry[K, V]) Sweep(shouldRemove func(K, V) bool) int {
    r.mu.Lock()
    defer r.mu.Unlock()
    swept := 0
    for k, v := range r.items {
        if shouldRemove(k, v) {
            delete(r.items, k)
            swept++
        }
    }
    return swept
}

// ForEach iterates over all items under a read lock.
func (r *TypedRegistry[K, V]) ForEach(fn func(K, V)) {
    r.mu.RLock()
    defer r.mu.RUnlock()
    for k, v := range r.items {
        fn(k, v)
    }
}
```

#### Step 2: Migration Order

Migrate in order of simplest/least-used to most complex/most-used:

1. **CUE registries** (cueContexts, cueValues) -- least used, fewest callers
2. **JSON registries** (jsonReaders, jsonWriters)
3. **Unix registries** (unixListeners, unixConns)
4. **External processes** (extProcesses)
5. **gRPC registries** (grpcClients, grpcStreams)
6. **HTTP registries** (httpServers, httpRequests, httpResponses)
7. **Results and contexts**
8. **GoObjects**
9. **Exceptions** (has Sweep with custom predicate)
10. **Class values**
11. **Dictionaries and strings** (most heavily used, ID offset logic)

For each migration:
- Replace the 3 fields (map + mutex + counter) with `*TypedRegistry[K, *V]`
- Replace `Register*`, `Get*`, `Unregister*`, `*Count` methods with delegates to the generic type
- Run tests after each migration

#### Step 3: Flatten ConcurrencyRegistry

After all ObjectRegistry groups are migrated, consider inlining ConcurrencyRegistry fields directly into ObjectRegistry (or dissolving both into a bag of `*TypedRegistry` instances). The blocks registry has extra complexity (`blocksByHomeFrame` secondary index) and should remain as a specialized type.

#### Step 4: Update NewObjectRegistry and FullStats

`NewObjectRegistry()` becomes a series of `NewTypedRegistry[K,V]()` calls. `FullStats()` iterates each registry calling `.Count()`.

### Risk/Complexity

**Medium.** The refactoring is mechanical but touches a type used throughout the VM. The key risks are:
- **Build breakage from method signature changes.** Mitigate by keeping the old method signatures as thin wrappers initially, then removing them once all callers are updated.
- **Mixed key types.** The dictionary/string registries use `uint32` keys with special offset ranges (`dictionaryIDOffset`, `stringIDOffset`). These need careful handling.
- **Blocks registry.** The `blocksByHomeFrame` secondary index does not fit the generic pattern; this registry should remain specialized.
- No runtime behavior changes, so correctness risk is low.

---

## TODO 10: Extract Compilation Pipeline from main.go

### Current Implementation

`cmd/mag/main.go` is 1523 lines. Here is a function-by-function catalog with dependency analysis:

| Function | Lines | Pure Logic? | CLI Deps | VM Deps | Notes |
|----------|-------|-------------|----------|---------|-------|
| `main()` | 25-331 | No | flags, os.Exit, fmt.Fprintf | vmInst | Entry point, flag parsing, dispatch |
| `loadRC()` | 333-351 | No | os.UserHomeDir, os.Stat | vmInst | Loads ~/.maggierc |
| `runMain()` | 354-413 | Partially | None | vmInst | Resolves class/method, sends message |
| `runREPL()` | 416-478 | No | bufio.Scanner, fmt.Print, os.Stdin | vmInst | Interactive loop |
| `handleREPLCommand()` | 481-513 | No | fmt.Println | vmInst | :help, :compiler, etc. |
| `handleHelpLookup()` | 516-551 | No | fmt.Printf | vmInst | Docstring display |
| `evalAndPrint()` | 554-578 | No | fmt.Printf | vmInst | Compile + execute + print |
| `looksLikeMethodDef()` | 581-605 | Yes | None | None | Pure string analysis |
| `printValue()` | 608-647 | No | fmt.Println | vmInst | Value formatting |
| `parsedFile` (struct) | 650-656 | Yes | None | compiler, vm | Data type |
| `collectFiles()` | 660-753 | Partially | os.ReadFile, filepath, os.Stat | compiler | File I/O + parsing |
| `compileAll()` | 760-1058 | Yes | None | vm, compiler, hash | Core two-pass pipeline |
| `buildCompileFunc()` | 1063-1073 | Yes | None | compiler, hash | Factory for sync service |
| `resolveGlobalForHash()` | 1077-1086 | Yes | None | vm.ClassTable | FQN resolution |
| `hashAndSetMethod()` | 1089-1094 | Yes | None | hash, vm | Content hashing helper |
| `compilePath()` | 1099-1105 | Partially | None | vm | Thin wrapper |
| `deriveNamespace()` | 1111-1131 | Yes | filepath.Rel | manifest | Pure path computation |
| `compileFile()` | 1134-1141 | No | os.ReadFile | vm | Single file compilation |
| `compileSourceFile()` | 1148-1173 | Yes | None | compiler, vm | Single source text pipeline |
| `runYutaniIDE()` | 1177-1238 | No | fmt.Printf, findYutaniLib | vm, compiler | IDE startup |
| `findYutaniLib()` | 1241-1276 | No | os.Executable, os.Getwd, os.Stat | None | Path discovery |
| `checkNamespaceCollisions()` | 1280-1303 | Yes | None | manifest | Pure validation |
| `prefixDepNamespaces()` | 1308-1343 | Yes | None | None | Pure data transform |
| `remapImport()` | 1349-1357 | Yes | None | None | Pure string transform |
| `loadProject()` | 1362-1449 | Partially | os.Stat | vm, manifest | Dep resolution + compile |
| `handleDepsCommand()` | 1452-1523 | No | fmt.Fprintf, os.Exit | manifest | CLI subcommand |

Functions defined in other files in `cmd/mag/` but relevant:
- `handleFmtCommand()` in `cmd/mag/format.go`
- `handleWrapCommand()` in `cmd/mag/wrap.go`
- `handleBuildCommand()` in `cmd/mag/build.go`
- `handleSyncCommand()` in `cmd/mag/sync.go`
- `handleDocCommand()`, `handleDoctestCommand()` in `cmd/mag/doc.go`

### Analysis

The functions fall into three clear categories:

**1. Pure compilation pipeline** (no CLI or I/O dependencies):
- `compileAll`, `compileSourceFile`, `compilePath`, `collectFiles`
- `parsedFile` struct
- `deriveNamespace`, `resolveGlobalForHash`, `hashAndSetMethod`, `buildCompileFunc`
- `checkNamespaceCollisions`, `prefixDepNamespaces`, `remapImport`
- `loadProject`
- `looksLikeMethodDef`

These are the extraction candidates. `collectFiles` does file I/O but is integral to the pipeline (reads source files, parses them). It should move with the pipeline but accept an `fs.FS` or file-reading function for testability.

**2. REPL/CLI interaction:**
- `runREPL`, `handleREPLCommand`, `handleHelpLookup`, `evalAndPrint`, `printValue`
- `runMain`, `loadRC`
- `runYutaniIDE`, `findYutaniLib`

**3. Subcommand handlers:**
- `handleDepsCommand` -- already in main.go but could be its own file
- Other handlers already in separate files

### Recommended Fix

#### Package: `pipeline/`

Create `pipeline/pipeline.go` with the following public API:

```go
package pipeline

import (
    "github.com/chazu/maggie/compiler"
    "github.com/chazu/maggie/compiler/hash"
    "github.com/chazu/maggie/manifest"
    "github.com/chazu/maggie/vm"
)

// ParsedFile holds a parsed source file with resolved metadata.
type ParsedFile struct {
    SF        *compiler.SourceFile
    Namespace string
    Imports   []string
    Path      string
    BasePath  string
}

// CollectFiles resolves a path pattern into parsed files.
// Supports ./... recursive syntax and directory-as-namespace convention.
func CollectFiles(path string) ([]ParsedFile, error)

// CompileAll implements two-pass compilation over a set of parsed files.
// Pass 1a: skeleton registration, Pass 1b: superclass resolution, Pass 2: method compilation.
func CompileAll(files []ParsedFile, vmInst *vm.VM, verbose bool) (int, error)

// CompilePath is a convenience wrapper: collectFiles + compileAll.
func CompilePath(path string, vmInst *vm.VM, verbose bool) (int, error)

// CompileSourceFile compiles a single source text into the VM.
func CompileSourceFile(vmInst *vm.VM, source, sourcePath, nsOverride string, verbose bool) (int, error)

// LoadProject resolves dependencies and loads source directories from a manifest.
func LoadProject(vmInst *vm.VM, m *manifest.Manifest, verbose bool) (int, error)

// DeriveNamespace computes a namespace from a file's relative directory path.
func DeriveNamespace(filePath, basePath string) string

// BuildCompileFunc creates a compile function for sync service verification.
func BuildCompileFunc(vmInst *vm.VM) func(string) ([32]byte, error)

// CheckNamespaceCollisions validates that no two deps map to the same namespace.
func CheckNamespaceCollisions(deps []manifest.ResolvedDep) error

// PrefixDepNamespaces prefixes file namespaces with a dependency's resolved namespace.
func PrefixDepNamespaces(files []ParsedFile, dep manifest.ResolvedDep, verbose bool)

// RemapImport replaces an import's old prefix with a new prefix.
func RemapImport(imp, oldPrefix, newPrefix string) string
```

#### What stays in `cmd/mag/main.go`:
- `main()` (flag parsing, dispatch)
- `loadRC()`
- `runMain()`

#### Package: `repl/`

Create `repl/repl.go`:

```go
package repl

import (
    "github.com/chazu/maggie/vm"
    "io"
)

// Config holds REPL configuration.
type Config struct {
    In      io.Reader
    Out     io.Writer
    ErrOut  io.Writer
    Verbose bool
}

// Run starts the interactive REPL.
func Run(vmInst *vm.VM, cfg Config)

// EvalAndPrint compiles and executes an expression, printing the result.
func EvalAndPrint(vmInst *vm.VM, input string, out io.Writer)

// PrintValue formats a value for display.
func PrintValue(vmInst *vm.VM, v vm.Value, out io.Writer)

// LooksLikeMethodDef checks if input appears to be a method definition.
func LooksLikeMethodDef(input string) bool
```

#### Migration Steps

1. Create `pipeline/` package with the types and functions listed above. Initially call the `pipeline` functions from `main.go` wrappers so everything still compiles.
2. Create `repl/` package. Move REPL functions. Wire `main.go` to call `repl.Run()`.
3. Move `handleDepsCommand` into `cmd/mag/deps.go` (separate file in same package).
4. Move `runYutaniIDE` and `findYutaniLib` into `cmd/mag/yutani.go`.
5. Remove old functions from `main.go`. Target: `main.go` should be ~200 lines (flag setup, image loading, dispatch).

### Risk/Complexity

**Medium.** The extraction is straightforward but involves many function moves. Key risks:
- **Import cycles.** The `pipeline` package needs `vm`, `compiler`, `manifest`, and `hash`. These have no circular dependencies today, so this is safe.
- **Test coverage.** Currently tested indirectly through integration tests in `cmd/mag/`. New packages should get unit tests, particularly for `CompileAll` and `LoadProject`.
- **FileIn/FileInBatch wiring.** The `vmInst.SetFileInFunc` and `vmInst.SetFileInBatchFunc` calls in `main()` reference `compileSourceFile` and `compilePath`. After extraction these become `pipeline.CompileSourceFile` and `pipeline.CompilePath`. This is a simple rename.

---

## TODO 11: Bytecode-to-Source-Position Mapping

### Current Implementation

#### AST Position Info

All AST nodes implement the `Node` interface which requires `Span() Span` (`compiler/ast.go:21`). `Span` contains `Start Position` and `End Position`, where `Position` has `Offset`, `Line`, and `Column` fields (all `int`, 1-based for line/column). Every expression and statement node stores a `SpanVal Span` field. The parser populates these from the lexer's `position()` method.

#### Lexer Position Tracking

The lexer (`compiler/lexer.go:16`) tracks `line`, `col`, and `lineStart` fields. The `position()` method (line 68) returns a `Position{Offset, Line, Column}`. Every token gets a `Pos Position` from the lexer at the start of token recognition. Positions are accurate and 1-based.

#### CompiledMethod SourceMap Support

The `CompiledMethod` struct (`vm/compiled_method.go:9`) already has:
```go
SourceMap []SourceLoc // bytecode offset -> source position
```

And `SourceLoc` is already defined (line 41):
```go
type SourceLoc struct {
    Offset int // bytecode offset
    Line   int // 1-based line number
    Column int // 1-based column number
}
```

`AddSourceLocation(offset, line, column int)` and `SourceLocation(offset int) *SourceLoc` are already implemented. The `CompiledMethodBuilder` has `MarkSource(line, column int)` (line 345) which calls `AddSourceLocation` with the current bytecode position.

`BlockMethod` also has `SourceMap []SourceLoc` and `SourceLocation()`.

#### Image Persistence

The image writer (`vm/image_writer.go:687`) already serializes `SourceMap`:
```go
WriteUint32(buf, uint32(len(m.SourceMap)))
for _, loc := range m.SourceMap {
    WriteUint32(buf, uint32(loc.Offset))
    WriteUint32(buf, uint32(loc.Line))
    WriteUint32(buf, uint32(loc.Column))
}
```

This uses 12 bytes per entry (3x uint32). The image reader deserializes it symmetrically.

#### Current Usage

The `SourceMap` is **never populated** during compilation. The `Compiler` struct in `compiler/codegen.go` does not call `MarkSource` or `AddSourceLocation` anywhere. The infrastructure exists end-to-end (struct, serialization, lookup) but the emission step is missing.

The `StackTrace()` method in `vm/interpreter.go:217` shows only frame index, class name, method name, and IP. It does **not** use `SourceLocation()` to show line numbers, because the source map is always empty.

### Analysis

#### What Needs to Change

1. **Compiler emission:** The `Compiler` needs to emit source locations during `compileExpr` and `compileStmt`. Since every AST node has a `Span`, this is straightforward.

2. **Stack trace enrichment:** `StackTrace()` should call `method.SourceLocation(frame.IP)` and include `file:line:col` when available.

3. **Compact encoding for images:** The current 12-byte-per-entry format is wasteful. Most methods have sequential line numbers. A more compact encoding would reduce image size.

#### Research: Compact Source Map Encodings

**CPython co_linetable (PEP 626, Python 3.11+):** Uses variable-length entries. Each entry encodes a (bytecodeIncrement, lineIncrement) pair. Small deltas use 1-2 bytes; large jumps use escape sequences. Very compact for typical code (1-2 bytes per source line).

**Lua lineinfo:** Simple array indexed by instruction number. Each instruction gets a `uint16` or `uint32` line number. Zero overhead at decode time but no column info and wastes space when many instructions map to the same line.

**JVM LineNumberTable:** Array of `(start_pc: u2, line_number: u2)` entries. Only records changes (new entries when the line number changes). 4 bytes per transition. No column info.

**Source Map v3 (JavaScript):** Uses VLQ (Variable-Length Quantity) encoding with base64. Highly compact for the web use case but complex to decode and overkill for a bytecode VM.

#### Recommended Encoding for Maggie

The JVM approach (record only transitions) fits Maggie best. Most bytecode instructions within a single expression map to the same source line. Recording only when line/column changes minimizes entries.

For image persistence, use a **delta-encoded** format:
- First entry: full (offset: varint, line: varint, column: varint)
- Subsequent entries: (deltaOffset: varint, deltaLine: svarint, deltaColumn: svarint)
- Signed varints for line/column since they can decrease (e.g., cascades)

This achieves ~3-6 bytes per entry vs 12 bytes currently.

### Recommended Fix

#### Step 1: Emit Source Locations During Compilation

Add a helper to the `Compiler` struct in `compiler/codegen.go`:

```go
// emitSourcePos records the source position for the current bytecode offset.
// Deduplicates: only emits if line or column changed since last emission.
func (c *Compiler) emitSourcePos(node Node) {
    span := node.Span()
    pos := span.Start
    if pos.Line == 0 {
        return // no position info
    }

    offset := c.builder.Len()

    // Deduplicate: skip if same as last entry
    if c.lastSourceLine == pos.Line && c.lastSourceCol == pos.Column {
        return
    }
    c.lastSourceLine = pos.Line
    c.lastSourceCol = pos.Column

    c.sourceMap = append(c.sourceMap, vm.SourceLoc{
        Offset: offset,
        Line:   pos.Line,
        Column: pos.Column,
    })
}
```

Add `lastSourceLine`, `lastSourceCol int` and `sourceMap []vm.SourceLoc` fields to the `Compiler` struct. Call `c.emitSourcePos(node)` at the top of `compileExpr` (before any bytecode emission) and at the top of `compileStmt`. After compilation, assign `c.sourceMap` to the `CompiledMethod.SourceMap` field.

For blocks, save/restore the source map state the same way the builder state is saved/restored, and assign the block's source map to `BlockMethod.SourceMap`.

#### Step 2: Enrich Stack Traces

In `vm/interpreter.go`, update `StackTrace()`:

```go
// In the method frame branch:
if frame.Method != nil {
    methodName := frame.Method.Name()
    className := "<unknown>"
    if frame.Method.Class() != nil {
        className = frame.Method.Class().Name
    }

    // Try to resolve source position
    loc := frame.Method.SourceLocation(frame.IP)
    if loc != nil {
        location = fmt.Sprintf("  [%d] %s>>%s (line %d, col %d)",
            j, className, methodName, loc.Line, loc.Column)
    } else {
        location = fmt.Sprintf("  [%d] %s>>%s at IP %d",
            j, className, methodName, frame.IP)
    }
}
```

#### Step 3: Compact Image Encoding (optional, follow-up)

Bump image version to v5 and use delta-encoded varints for `SourceMap` entries. Keep backward compatibility by reading v4 format (12 bytes per entry) and writing v5 format. This is lower priority since the current format works; it just wastes some space.

Estimated savings: a method with 20 source map entries uses 240 bytes in v4 format vs ~60-80 bytes with delta encoding.

#### Step 4: File Name Tracking (optional, follow-up)

Currently there is no file path in `CompiledMethod`. For multi-file stack traces, add a `SourceFile string` field and populate it during `compileAll`. This enables "file:line" format in stack traces.

### Risk/Complexity

**Low to Medium.**
- Step 1 (emission) is low risk: additive change, no behavior change for existing code.
- Step 2 (stack traces) is low risk: purely display improvement.
- Step 3 (compact encoding) is medium risk: image format change requires version bump and backward compatibility.
- The main risk is performance: emitting source positions adds overhead during compilation. Deduplication (only emit on line/column change) keeps this minimal. Runtime lookup (`SourceLocation`) is only called during error reporting, not in the hot path.

---

## TODO 12: Peephole Optimizer Pass

### Current Implementation

#### Opcode Catalog

The full opcode set is defined in `vm/bytecode.go`. There are 55 opcodes across 7 categories:

**Stack Operations (3):** NOP, POP, DUP

**Push Constants (9):** PushNil, PushTrue, PushFalse, PushSelf, PushInt8, PushInt32, PushLiteral, PushFloat, PushContext

**Variable Operations (15):** PushTemp, PushIvar, PushGlobal, StoreTemp, StoreIvar, StoreGlobal, PushCaptured, StoreCaptured, PushHomeTemp, StoreHomeTemp, MakeCell, CellGet, CellSet, PushClassVar, StoreClassVar

**Message Sends (3 generic + 16 specialized = 19):** Send, SendSuper, TailSend, SendPlus through SendClass

**Control Flow (5):** Jump, JumpTrue, JumpFalse, JumpNil, JumpNotNil

**Returns (4):** ReturnTop, ReturnSelf, ReturnNil, BlockReturn

**Blocks/Objects (5):** CreateBlock, CaptureTemp, CaptureIvar, CreateArray, CreateObject

#### Bytecode Emission Pipeline

The compiler (`compiler/codegen.go`) emits bytecode directly via `c.builder.Emit*()` calls during AST traversal. There is **no** intermediate representation between the AST and the final bytecode. The `CompileMethod` function:

1. Sets up compiler state (args, temps, cell vars)
2. Calls `compileStatements` which recursively walks the AST
3. Each AST node directly emits bytecode via the builder
4. Calls `b.Build()` to finalize the method

The builder (`vm/bytecode.go:237`) appends bytes to a `[]byte` slice. Labels use forward-reference patching for jumps.

#### Existing Optimizations

The compiler already performs two optimizations at the AST level:
- **Specialized sends** (`compiler/codegen.go:608`): Binary messages for `+`, `-`, `*`, `/`, `<`, `>`, `<=`, `>=`, `=` emit single-byte opcodes (OpSendPlus, etc.) instead of the 4-byte OpSend instruction.
- **Tail-call optimization** (`compiler/codegen.go:250`): Self-recursive calls in return position emit OpTailSend instead of OpSend.

### Analysis

#### Candidate Peephole Patterns

Analyzing the bytecode emission patterns from `codegen.go`, here are the most impactful optimization targets:

**1. Push-Pop Elimination (high impact)**

Pattern: `PUSH_* ... POP` where the push has no side effects.

This occurs in `compileStatements` (line 234): expression statements that are not the last statement get a `POP` appended. If the expression is a simple variable read or literal, the entire push-pop sequence is dead code.

```
Before: PUSH_TEMP 0 / POP
After:  (removed)
```

Caution: Only safe when the push operation has no side effects. `PUSH_GLOBAL` could trigger lazy initialization in theory, but currently does not.

**2. Store-Push Coalescing (medium impact)**

Pattern: `STORE_TEMP n / PUSH_TEMP n` (assignment leaves value on stack).

This appears in every assignment (`compileAssignment`, line 537):
```go
c.builder.EmitByte(vm.OpStoreTemp, byte(idx))
c.builder.EmitByte(vm.OpPushTemp, byte(idx)) // Leave value on stack
```

A combined `STORE_AND_PUSH_TEMP` opcode would save 2 bytes per assignment. However, this requires adding a new opcode, which increases interpreter complexity. A simpler approach: if the assignment result is immediately popped (statement context), eliminate the push.

**3. Redundant Self-Push Elimination (low-medium impact)**

Pattern: `PUSH_SELF / RETURN_TOP` -> `RETURN_SELF`

This appears at the end of methods with no explicit return (`codegen.go:167`):
```go
c.builder.Emit(vm.OpPushSelf)
c.builder.Emit(vm.OpReturnTop)
```

The compiler could emit `RETURN_SELF` directly here (it already exists as an opcode). However, this is better fixed at the emission site in `CompileMethod` rather than in a peephole pass.

**4. Constant Folding (medium impact for math-heavy code)**

Pattern: `PUSH_INT8 X / PUSH_INT8 Y / SEND_PLUS` -> `PUSH_INT8 (X+Y)` (when result fits in int8)

More generally:
```
PUSH_INT8 X / PUSH_INT8 Y / SEND_PLUS  -> PUSH_INT8 (X+Y)  [if fits]
PUSH_INT8 X / PUSH_INT8 Y / SEND_TIMES -> PUSH_INT8 (X*Y)  [if fits]
PUSH_INT8 X / PUSH_INT8 Y / SEND_LT    -> PUSH_TRUE/PUSH_FALSE
```

This is only safe for integer arithmetic where the result is known at compile time and the receiver is definitely a SmallInt (not a user class that overrides `+`). Since we cannot guarantee the receiver type at compile time in Smalltalk, this optimization is **unsound** in general. It could be applied only when both operands are literal integers pushed immediately before the send, but even then, a custom Number subclass could override `+`. **Not recommended** unless we add a "trusted primitives" annotation.

**5. Dead Code After Unconditional Jump/Return (low impact)**

Pattern: After `JUMP`, `RETURN_TOP`, `RETURN_SELF`, or `RETURN_NIL`, any code before the next jump target is unreachable and can be removed.

This rarely occurs in practice since the compiler does not generate unreachable code.

**6. Jump Threading (low impact)**

Pattern: `JUMP target` where `target` is another `JUMP target2` -> `JUMP target2`

This can occur with nested conditionals but is rare in Smalltalk code where `ifTrue:ifFalse:` generates simple conditional jumps.

#### Where to Hook In

A bytecode-level peephole optimizer is simpler and more practical than an AST-level pass:

- **AST-level:** Would require transforming the AST before codegen. More powerful (can restructure control flow) but invasive and risks breaking the semantic analysis step.
- **Bytecode-level:** Operates on the emitted byte slice after `Build()`. Simpler, can be added as an optional post-processing step. Cannot restructure control flow easily but handles the common patterns above.

The best hook point is between `c.builder.Bytes()` and `b.Build()` in `CompileMethod`, or as a post-pass on the `CompiledMethod.Bytecode` slice.

### Recommended Fix

#### Step 1: Implement a Bytecode Peephole Optimizer

Create `compiler/peephole.go`:

```go
package compiler

import "github.com/chazu/maggie/vm"

// PeepholeOptimize applies local bytecode optimizations to a compiled method.
// Returns the optimized bytecode and adjusted source map.
// The optimization is conservative: it only removes clearly redundant sequences.
func PeepholeOptimize(bytecode []byte, sourceMap []vm.SourceLoc) ([]byte, []vm.SourceLoc) {
    opt := &peepholeOptimizer{
        input:     bytecode,
        output:    make([]byte, 0, len(bytecode)),
        sourceMap: sourceMap,
    }
    opt.run()
    return opt.output, opt.newSourceMap
}

type peepholeOptimizer struct {
    input     []byte
    output    []byte
    sourceMap []vm.SourceLoc
    newSourceMap []vm.SourceLoc

    // Offset mapping: input offset -> output offset (for jump fixup)
    offsetMap map[int]int
}

func (p *peepholeOptimizer) run() {
    // Phase 1: Identify optimization opportunities (mark deletions)
    // Phase 2: Copy bytes, skipping deleted regions, building offset map
    // Phase 3: Fix up jump targets using offset map
    // Phase 4: Remap source map offsets

    p.offsetMap = make(map[int]int)

    // Simple single-pass approach: scan for patterns, emit optimized output
    pos := 0
    for pos < len(p.input) {
        outPos := len(p.output)
        p.offsetMap[pos] = outPos

        op := vm.Opcode(p.input[pos])
        info := op.Info()
        instrLen := 1 + info.OperandBytes

        // Pattern: PUSH_SELF + RETURN_TOP -> RETURN_SELF
        if op == vm.OpPushSelf && pos+1 < len(p.input) &&
           vm.Opcode(p.input[pos+1]) == vm.OpReturnTop {
            p.output = append(p.output, byte(vm.OpReturnSelf))
            pos += 2
            continue
        }

        // Pattern: PUSH_NIL + RETURN_TOP -> RETURN_NIL
        if op == vm.OpPushNil && pos+1 < len(p.input) &&
           vm.Opcode(p.input[pos+1]) == vm.OpReturnTop {
            p.output = append(p.output, byte(vm.OpReturnNil))
            pos += 2
            continue
        }

        // Pattern: STORE_TEMP n + PUSH_TEMP n -> STORE_TEMP n + DUP
        // (saves 1 byte: DUP is 1 byte vs PUSH_TEMP which is 2 bytes)
        // Note: only valid if the store is immediately followed by push of same slot
        if (op == vm.OpStoreTemp || op == vm.OpStoreIvar) &&
           pos+instrLen < len(p.input) {
            nextOp := vm.Opcode(p.input[pos+instrLen])
            pushOp := vm.OpPushTemp
            if op == vm.OpStoreIvar {
                pushOp = vm.OpPushIvar
            }
            if nextOp == pushOp &&
               pos+instrLen+1 < len(p.input) &&
               p.input[pos+1] == p.input[pos+instrLen+1] {
                // Emit: STORE + DUP instead of STORE + PUSH
                // Actually, STORE does not leave value on stack, so this does not work.
                // The current codegen pattern is: value on stack -> STORE_TEMP (pops) -> PUSH_TEMP (re-reads).
                // We cannot replace this with DUP because STORE_TEMP consumes the value.
                // Skip this optimization.
            }
        }

        // Pattern: side-effect-free PUSH + POP -> remove both
        if isPurePush(op) && pos+instrLen < len(p.input) &&
           vm.Opcode(p.input[pos+instrLen]) == vm.OpPOP {
            // Skip both instructions
            pos += instrLen + 1
            continue
        }

        // No optimization: copy instruction verbatim
        p.output = append(p.output, p.input[pos:pos+instrLen]...)
        pos += instrLen
    }

    // Record final offset for any remaining references
    p.offsetMap[pos] = len(p.output)

    // Fix up jump targets
    p.fixJumps()

    // Remap source map
    p.remapSourceMap()
}

// isPurePush returns true if the opcode is a push with no side effects.
func isPurePush(op vm.Opcode) bool {
    switch op {
    case vm.OpPushNil, vm.OpPushTrue, vm.OpPushFalse, vm.OpPushSelf,
         vm.OpPushInt8, vm.OpPushTemp, vm.OpPushIvar, vm.OpPushCaptured,
         vm.OpPushHomeTemp, vm.OpPushLiteral, vm.OpPushContext:
        return true
    }
    return false
}
```

The jump fixup phase needs to scan the output for jump instructions and adjust their relative offsets based on the offset map. This is the trickiest part: relative jump offsets change when instructions are removed between the jump and its target.

#### Step 2: Wire Into the Compilation Pipeline

In `compiler/codegen.go`, after `b.Build()`:

```go
func (c *Compiler) CompileMethod(method *MethodDef) *vm.CompiledMethod {
    // ... existing compilation ...
    result := b.Build()

    // Apply peephole optimizations
    result.Bytecode, result.SourceMap = PeepholeOptimize(result.Bytecode, result.SourceMap)

    return result
}
```

#### Step 3: Measure Impact

Add benchmarks comparing compilation with and without peephole optimization. The key metrics:
- Compilation time overhead (should be < 5% since peephole is a single linear pass)
- Bytecode size reduction (expect 5-15% for typical Smalltalk code)
- Execution speed improvement (expect minimal: the interpreter already fast-paths specialized sends)

#### Patterns to Implement (Priority Order)

1. **PUSH_SELF + RETURN_TOP -> RETURN_SELF** -- simple, safe, saves 1 byte per method without explicit return
2. **PUSH_NIL + RETURN_TOP -> RETURN_NIL** -- same pattern, less common
3. **Pure PUSH + POP elimination** -- removes dead expression statements, saves 2-3 bytes each
4. **NOP removal** -- trivial
5. **Jump threading** -- follow-up, only if profiling shows benefit

Patterns deliberately NOT implemented:
- **Constant folding** -- unsound without type information (Smalltalk semantics)
- **Store+Push coalescing** -- requires new opcode, marginal benefit
- **Inline caching improvements** -- these are runtime, not compile-time

### Risk/Complexity

**Medium.**
- **Jump fixup correctness** is the primary risk. Removing bytes between a jump instruction and its target changes the relative offset. The offset map approach handles this but must be tested thoroughly, especially with backward jumps (loops via `whileTrue:`) and forward jumps (conditionals via `ifTrue:ifFalse:`).
- **Source map adjustment** must track offset changes so that debugger source locations remain correct after optimization.
- **Block bytecode** must be optimized separately since blocks have their own bytecode arrays. The optimizer function should be called on each `BlockMethod.Bytecode` as well.
- **Testing strategy:** Compile a corpus of Maggie code (the standard library in `lib/`) with and without peephole, verify identical execution results. Add golden-file tests for specific optimization patterns.
- The impact on execution speed is expected to be minimal since the hot path is interpreter dispatch, not bytecode density. The primary benefit is reduced image size and cleaner bytecode for debugging/inspection.
