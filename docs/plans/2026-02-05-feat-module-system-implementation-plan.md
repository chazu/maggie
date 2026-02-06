---
title: "feat: Module System Implementation (Two-Pass Loading, FQN Resolution, Content-Addressing)"
type: feat
date: 2026-02-05
brainstorm: docs/brainstorms/2026-02-04-module-system-design-brainstorm.md
epics: maggie-vc64, maggie-gu5d
---

# Module System Implementation

## Enhancement Summary

**Deepened on:** 2026-02-05
**Sections enhanced:** 6 (all phases + architecture + acceptance criteria)
**Research agents used:** architecture-strategist, performance-oracle, security-sentinel, code-simplicity-reviewer, language-architect, golang-hpc-architect, best-practices-researcher (×2), pattern-recognition-specialist, learnings-researcher

### Key Improvements
1. **Bug fix in ProcessEnv.Lookup** — double map lookup and Nil conflation identified and corrected
2. **De Bruijn contradiction resolved** — Decision 4 ("names significant") superseded by Decision 11 ("de Bruijn from day one"); plan follows Decision 11
3. **Security hardening** — Compiler evaluate: escalation path, Object allClasses bypass, and git command injection identified with mitigations
4. **Simplification opportunities** — Pass 1a/1b merge candidate, ClassLookupFunc callback may be unnecessary, ProcessEnv COW can use `maps.Clone`
5. **Instance variable hashing fix** — use relative ivar index (position within declaring class's own InstVars) for class-independent method hashing

### New Considerations Discovered
- REPL should auto-import all loaded namespaces (not just provide FQN access)
- `compileAssignment` also needs FQN resolution (not just `compileVariable`)
- `newInterpreter()` has wasteful allocations on fork path (creates then discards SelectorTable, ClassTable, Globals)
- Content store section ordering in image format should be specified
- Pending buffer must be cleared between files during two-pass loading (namespace leaking risk, per docstring learning)
- Golden-file testing strategy essential for content-addressing (per Unison research)
- Effect ordering must be normalized (sort before hash) to avoid non-determinism

---

## Overview

Implement the module system designed in the [2026-02-04 brainstorm](../brainstorms/2026-02-04-module-system-design-brainstorm.md). This is a 6-phase effort spanning the compiler, loader, image format, VM runtime, CLI tooling, and distribution protocol. The brainstorm contains 14 design decisions — all are ratified and ready for implementation.

**Implementation order:** Two-pass loading + FQN resolution (foundation) → Content-addressing → Dependency namespace mapping → Process-level restriction → Go interop → Distribution protocol. Each phase is independently valuable and shippable.

This plan covers **Phases 1–4** in detail (the core module system). Phases 5–6 (Go interop, distribution) are sketched for sequencing but will get their own plans when ready.

## Problem Statement

Three bugs in the current loading/compilation pipeline prevent correct module semantics:

1. **Compiler does not resolve class refs to FQN in method bodies** (`compiler/codegen.go:458-461`). `import:` has no effect on method bodies — unqualified names work only by accident via short-name Globals registration.
2. **Short-name Globals registration causes silent collisions** (`cmd/mag/main.go:820`). Every class is registered under both bare name and FQN. Last-loaded wins silently when two namespaces define the same class name.
3. **Loading pipeline is single-pass** (`cmd/mag/main.go:761`, `compileSourceFile`). Classes and methods compile in one pass per file. Forward references across files fail (superclass defaults to Object silently). Circular imports are impossible.

Until these are fixed, the namespace/import system is decorative — it parses correctly but does not enforce correct resolution.

## Technical Approach

### Architecture

The implementation touches four layers:

```
┌─────────────────────────────────────────────────────────┐
│  CLI / Loader (cmd/mag/main.go)                         │
│  - Two-pass loading pipeline                            │
│  - Dependency namespace mapping                         │
│  - Collision detection                                  │
├─────────────────────────────────────────────────────────┤
│  Compiler (compiler/codegen.go, compiler/ast.go)        │
│  - FQN resolution during method compilation             │
│  - Hashing AST generation + content-addressing          │
│  - Namespace/import context threading                   │
├─────────────────────────────────────────────────────────┤
│  VM Runtime (vm/interpreter.go, vm/class.go)            │
│  - Per-process Globals map (COW overlay)                │
│  - OpPushGlobal unchanged (map lookup)                  │
├─────────────────────────────────────────────────────────┤
│  Image (vm/image_writer.go, vm/image_reader.go)         │
│  - Content store serialization                          │
│  - Version bump for new fields                          │
└─────────────────────────────────────────────────────────┘
```

### Architecture Research Insights

**Architecture Strategist:**
- Plan is architecturally sound — ClassLookupFunc callback is a correct boundary pattern
- The four-layer architecture (CLI/Loader → Compiler → VM Runtime → Image) has clean separation
- Pattern-recognition-specialist confirms ClassLookupFunc is consistent with existing callback patterns in the codebase
- Two-pass loading aligns the runtime pipeline with the bootstrap pattern already used in `cmd/bootstrap/main.go`

**Simplification Candidates (code-simplicity-reviewer):**
- ClassLookupFunc callback may be unnecessary — the compiler package already imports `vm`. Consider passing `*vm.ClassTable` directly or defining a proper Go interface. Counter-argument: callback provides testability via mock. **Decision: evaluate during implementation — if a simple interface works, prefer it over callback.**
- `CompileMethodWithContext` on the interface may be unnecessary — a `*CompileContext` parameter to the existing method is simpler. **Recommendation: use `*CompileContext` parameter.**

**Pattern Recognition:**
- God Object tendency in `cmd/mag/main.go` — the two-pass refactor is an opportunity to extract a `Loader` type that encapsulates the loading pipeline
- Recommend extracting a shared `compileClassDef` helper rather than duplicating logic between Pass 1 skeleton registration and Pass 2 compilation
- Hash AST files should go in `compiler/` not `vm/` — they are compiler concerns

**Go HPC Architect:**
- Keep direct map reference on Interpreter for hot path (`OpPushGlobal`) — avoid ProcessEnv indirection in the fast path
- Use `maps.Clone` (Go 1.21+) for COW copy instead of manual range loop
- Use `slices.Sort` over `sort.Strings` for modern Go idioms

### Implementation Phases

#### Phase 1: Two-Pass Loading + Compiler FQN Resolution

The foundation. Everything else depends on this.

##### Phase 1a: Two-Pass Loading

**Goal:** All class skeletons are registered before any method bodies compile. Forward references and circular imports work.

**Key design decision:** Two-pass loading is batch-level, not per-file. Pass 1 covers ALL files (deps + project) before ANY pass 2 compilation begins. This is the only correct approach — per-file two-pass still breaks on cross-file forward references.

**Superclass resolution within Pass 1 requires sub-phases:**
- **Pass 1a:** Parse all `.mag` files. Register class names in ClassTable with placeholder (nil) superclass. Store `(SourceFile, namespace, imports)` tuples for replay.
- **Pass 1b:** Resolve superclass pointers. Walk all registered skeletons, resolve each superclass name via `LookupWithImports` against the now-complete ClassTable. Error if superclass not found (instead of silently defaulting to Object).
- **Pass 2:** Compile all method bodies. ClassTable is fully populated with resolved superclass chains. Compiler has FQN resolution context.

**Files to modify:**

| File | Change |
|------|--------|
| `cmd/mag/main.go` | Split `compileSourceFile` into `registerClassSkeleton` + `compileClassMethods`. New `compileAll(files []parsedFile)` orchestrates two-pass. `compilePath` collects parsed files instead of compiling inline. `loadProject` collects all dep + project files before calling `compileAll`. |
| `cmd/mag/main.go` | Remove silent Object fallback for unresolved superclasses (line ~799). Emit clear error: `"class Foo: superclass Bar not found (namespace: X, imports: [Y, Z])"`. |
| `cmd/mag/main.go` | Store `parsedFile` struct: `{sf *compiler.SourceFile, namespace string, imports []string, path string}`. |

**Trait resolution:** Traits also need two-pass treatment. Currently `Trait` struct has no `Namespace` field (`vm/trait.go:12-17`). Add `Namespace string` to `Trait`. Apply same skeleton-first pattern: register trait names in pass 1a, resolve trait references in pass 1b.

**fileIn at runtime:** `Compiler fileIn:` (`vm/file_in.go`) remains single-pass. This is acceptable — runtime fileIn operates on an already-populated ClassTable. Classes being filed in can reference any class already in the system. If a filed-in class references an unknown class, the compiler falls through to raw-name emission (runtime resolution via Globals). Document this behavioral difference.

**fileInAll at runtime:** `Compiler fileInAll:` should use two-pass loading within its batch. Modify `FileInAll` to collect all parsed files first, then do skeleton registration, then compile. This is a small lift since the function already walks a directory.

**Acceptance criteria:**
- [ ] Forward reference across files works: File A references class from File B, loaded in any order
- [ ] Circular imports work: File A imports File B, File B imports File A
- [ ] Unresolved superclass produces clear error (not silent Object fallback)
- [ ] Traits are namespaced and participate in two-pass loading
- [ ] `fileInAll` uses two-pass within its batch
- [ ] All existing tests pass (no regression from restructuring)

**Phase 1a Research Insights:**

**Two-Pass Compilation Patterns (best-practices-researcher):**
- **Go compiler** uses a 4-phase approach: `collectObjects` → `packageObjects` → `processDelayed` → compile. Our Pass 1a/1b/2 maps to collect → resolve → compile.
- **Java** uses lazy completion with a `halfcompleted` queue for circular references. Not needed here since Maggie doesn't have Java-style generics that create circular completion.
- **Superclass resolution options:** batch (Go/Roslyn), on-demand lazy (Java), placeholder proxies (Pharo). Our batch approach (Pass 1b) is the simplest and correct choice.
- **Rust-style error messages** recommended — include the file/line where the unresolved superclass was declared, and list what namespaces were searched.

**Simplification (code-simplicity-reviewer):**
- Pass 1a/1b split can potentially be merged — resolve superclass at the start of Pass 2 instead of as a separate sub-phase. The ClassTable is already fully populated after 1a. **Counter-argument:** keeping 1b separate makes the error messages clearer (all skeleton errors before any compilation errors). **Decision: keep separate for now, merge if implementation proves awkward.**

**Institutional Learning (docstrings solution):**
- The compiler-native-docstrings solution is a highly relevant pipeline template for flowing new constructs through lexer→parser→compiler→image
- **Critical gotcha:** Pending buffer must be cleared between files during batch loading — namespace/import state from File A must not leak into File B. Ensure `parsedFile` tuples carry their own namespace/import state.
- Image version bumps require updating 80+ test images — plan for a batch update script
- `FileOut` needs to emit `namespace:` and `import:` declarations when reconstructing source

**Architecture Strategist:**
- Specify `fileInAll` batch behavior explicitly: does it do its own two-pass within the batch, or does it register skeletons into the already-populated ClassTable? **Recommendation:** `fileInAll` does its own mini two-pass (register all skeletons from the directory, resolve supers, then compile). This prevents ordering sensitivity within the batch.

##### Phase 1b: Compiler FQN Resolution

**Goal:** The compiler resolves unqualified class names to FQNs during method body compilation. Bytecode contains FQNs, not raw names.

**Key design decision — distinguishing class refs from other globals:** The compiler attempts `LookupWithImports` first. If a class is found, emit the FQN. If not found, emit the raw name (for user globals like `x := 42`). This is "best effort" resolution — class references that can be resolved at compile time are; everything else falls through to runtime.

**Compiler context threading:**

The `Compiler` struct (`compiler/codegen.go:14-49`) needs new fields:

```go
type Compiler struct {
    // ... existing fields ...
    namespace   string          // current file's namespace
    imports     []string        // current file's import list
    classLookup ClassLookupFunc // callback to resolve names via ClassTable
}

// ClassLookupFunc resolves an unqualified name using namespace + imports.
// Returns FQN if found, empty string if not.
type ClassLookupFunc func(name, namespace string, imports []string) string
```

**Why a callback, not a ClassTable reference:** The compiler package imports `vm`, but the ClassTable lives in `vm`. Using a callback avoids adding a direct dependency. The caller (`cmd/mag/main.go`) provides a closure that calls `vmInst.ClassTable.LookupWithImports`.

**The CompileFunc signature change:** The `CompileFunc` type (`vm/compiler_dispatch.go:33`) and `CompilerBackend` interface need optional namespace/import context. Add a new method to `CompilerBackend`:

```go
CompileMethodWithContext(source string, namespace string, imports []string,
    classLookup ClassLookupFunc, ...) (*CompiledMethod, error)
```

The existing `CompileMethod` remains for backward compatibility (REPL, tests). Internally it calls `CompileMethodWithContext` with empty namespace/imports and nil classLookup.

**Change in `compileVariable`** (`compiler/codegen.go:458-461`):

```go
// Current: unconditionally emits raw name
idx := c.addLiteral(c.symbols.SymbolValue(name))
c.builder.EmitUint16(vm.OpPushGlobal, uint16(idx))

// New: try FQN resolution first
resolvedName := name
if c.classLookup != nil {
    if fqn := c.classLookup(name, c.namespace, c.imports); fqn != "" {
        resolvedName = fqn
    }
}
idx := c.addLiteral(c.symbols.SymbolValue(resolvedName))
c.builder.EmitUint16(vm.OpPushGlobal, uint16(idx))
```

**Self-hosting compiler:** The `MaggieCompilerBackend` will continue to emit raw names until the self-hosting compiler is updated separately. This is acceptable — the self-hosting compiler is experimental and falls back to Go.

**Files to modify:**

| File | Change |
|------|--------|
| `compiler/codegen.go` | Add `namespace`, `imports`, `classLookup` fields to `Compiler`. Modify `compileVariable` for FQN resolution. Add `NewCompilerWithContext` constructor. |
| `compiler/compile.go` | Add `CompileWithContext` function that threads namespace/import context through to `Compiler`. |
| `vm/compiler_dispatch.go` | Add `ClassLookupFunc` type. Add `CompileMethodWithContext` to `CompilerBackend` interface. Update `GoCompilerBackend`. |
| `cmd/mag/main.go` | Pass namespace, imports, and classLookup closure when compiling method bodies in pass 2. |

**Acceptance criteria:**
- [ ] `Button` in a file with `import: 'Yutani::Widgets'` compiles to `OpPushGlobal("Yutani::Widgets::Button")`
- [ ] FQN `Yutani::Widgets::Button` passes through unchanged
- [ ] User globals (`x` from `Compiler evaluate: 'x := 42'`) fall through correctly (no false FQN resolution)
- [ ] Bare/root classes (`Array`, `Object`) resolve by short name (FQN = short name)
- [ ] `CompileFunc` backward compatibility preserved for REPL and tests

**Phase 1b Research Insights:**

**Architecture Strategist:**
- `compileAssignment` also needs FQN resolution, not just `compileVariable` — when assigning to a global that is a class name, the target name should be resolved to FQN
- Consider a deprecation phase for short-name Globals: log warnings before hard-removing in Phase 1c

**Language Architect:**
- Hybrid FQN resolution (try ClassTable, fall through to raw name) is fine for Smalltalk family languages
- **REPL should auto-import all loaded namespaces** — requiring FQN access only (e.g., `Yutani::Widgets::Button new`) is hostile to interactive use. Recommendation: REPL's import list = all known namespaces. This makes exploration natural while keeping compiled code explicit.
- The Nil fallback for missing globals (push Nil on not-found) should eventually become a `NameError`. For Phase 1 the current behavior is acceptable, but track as future work.

**Code Simplicity:**
- `CompileMethodWithContext` on the interface is unnecessary — use a `*CompileContext` struct parameter instead:
```go
type CompileContext struct {
    Namespace   string
    Imports     []string
    ClassLookup ClassLookupFunc
}
// Pass nil for backward compatibility (REPL, tests)
```

**Pattern Recognition:**
- Consider extracting `resolveClassName(name string) string` as a method on Compiler to centralize FQN resolution logic used in both `compileVariable` and `compileAssignment`

##### Phase 1c: Remove Short-Name Globals Registration

**Goal:** Only register classes under their FQN in Globals. Eliminate silent last-loaded-wins collisions.

**Breaking change:** Any existing `.mag` code that references a namespaced class by short name (without an import) will break. This is intentional — the old behavior was a bug, not a feature.

**Migration path:** Since Phases 1a and 1b land first, the compiler resolves unqualified names to FQNs via imports. Users must add `import:` declarations to their source files for any cross-namespace reference.

**REPL context:** The REPL starts with no namespace and no imports. Bare/root classes (`Array`, `String`, `Object`, etc.) work fine (their FQN = short name). Namespaced classes must be referenced by FQN in the REPL, or a new REPL command (`:import Yutani::Widgets`) sets the import context for subsequent expressions. The `:import` command is a nice-to-have for Phase 1, not a blocker.

**Files to modify:**

| File | Change |
|------|--------|
| `cmd/mag/main.go` ~line 820 | Remove `vmInst.Globals[classDef.Name] = classVal`. Keep only FQN registration at ~line 824. |
| `lib/yutani/**/*.mag` | Audit and add `import:` declarations where needed. |

**Acceptance criteria:**
- [ ] Classes are only findable in Globals by FQN
- [ ] Two namespaces defining the same short class name coexist without collision
- [ ] All lib/ and test .mag files updated with necessary imports
- [ ] REPL works for bare/root classes without import

---

#### Phase 2: Content-Addressing

**Goal:** Class and method definitions are content-addressed by hash. Names are a mutable mapping layer on top of immutable, content-addressed definitions.

##### Phase 2a: Frozen Hashing AST + Tag Byte Assignments

**Goal:** Define the stable, frozen AST node types used only for hashing. Enumerate all node types and assign uint8 tag bytes.

**Node types to enumerate** (from `compiler/ast.go`):

| Tag | Node Type | Notes |
|-----|-----------|-------|
| 0x01 | IntLiteral | Canonical form (no radix prefix) |
| 0x02 | FloatLiteral | IEEE 754 |
| 0x03 | StringLiteral | UTF-8 |
| 0x04 | SymbolLiteral | |
| 0x05 | CharacterLiteral | |
| 0x06 | ArrayLiteral | |
| 0x07 | BoolLiteral | |
| 0x08 | NilLiteral | |
| 0x09 | SelfRef | |
| 0x0A | SuperRef | Node type, not a class reference |
| 0x0B | LocalVarRef | De Bruijn: (scope_depth, slot_index) |
| 0x0C | InstanceVarRef | Slot index only (name dropped) |
| 0x0D | GlobalRef | FQN string |
| 0x10 | UnaryMessage | |
| 0x11 | BinaryMessage | |
| 0x12 | KeywordMessage | |
| 0x13 | Cascade | |
| 0x14 | Assignment | |
| 0x15 | Return | |
| 0x16 | Block | Params + body |
| 0x17 | MethodDef | Selector + params + body + docstring |
| 0x18 | ClassDef | Name + super + ivars + sorted method hashes |
| 0x19 | TraitDef | Name + sorted method hashes |
| 0x1A | PrimitiveCall | |
| 0x1B | DynamicArray | |

Reserve 0x00 for "version prefix" and 0xFE-0xFF for future use.

**New files:**

| File | Purpose |
|------|---------|
| `vm/hash_ast.go` | Frozen hashing AST node type definitions |
| `vm/hash_tags.go` | Tag byte constants |
| `vm/hash_encode.go` | Token stream encoder (AST → bytes) |
| `vm/hash.go` | SHA-256 computation, Merkle tree assembly |
| `vm/hash_debruijn.go` | De Bruijn index transformation |
| `vm/hash_test.go` | Round-trip and alpha-equivalence tests |

##### Phase 2b: AST Normalization + De Bruijn Indexing

**Goal:** Transform working AST to hashing AST. Normalize variable references.

**What gets de Bruijn indexed:**
- Method args → `(0, slot_index)`
- Method temps → `(0, args_count + slot_index)`
- Block params → `(scope_depth, slot_index)`
- Block temps → `(scope_depth, params_count + slot_index)`
- Captured vars → resolved to original scope's `(depth, slot)`

**What does NOT get indexed:**
- Instance variables → slot index only (name dropped)
- `self`, `super` → special node types (no index)
- Globals → FQN string (semantically meaningful)

**Normalization rules:**
- Method ordering: methods sorted by selector before computing class hash
- Docstrings: included in hash (documentation is content)
- Literal values: canonical form (`16rFF` and `255` → same integer node)
- Spans/positions: stripped (not content)

##### Phase 2c: Compiler Integration + Content Store

**Goal:** Compute hashes at compile time. Store in content store. Add to image format.

**Compiler integration:** After compiling a method, the compiler transforms its AST to hashing AST, serializes, and computes SHA-256. The hash is stored on `CompiledMethod`.

```go
// vm/compiled_method.go
type CompiledMethod struct {
    // ... existing fields ...
    ContentHash [32]byte // SHA-256 of canonical AST
}
```

**Content store:** Initially a `map[[32]byte][]byte` on the VM — hash → serialized hashing AST. Embedded in image.

**Image format:** Version bump (3 → 4). New section for content store after existing sections. Old reader rejects version 4 with clear error. New reader accepts version 3 (no content store, zero hashes) and version 4.

**Acceptance criteria:**
- [ ] Same source → same hash (deterministic)
- [ ] Alpha-equivalent code → same hash (de Bruijn working)
- [ ] Different source → different hash (no false collisions in test suite)
- [ ] Method hash changes → class hash changes → module hash changes
- [ ] Image round-trip preserves hashes
- [ ] Image version 3 still loadable

**Phase 2 Research Insights:**

**Language Architect — De Bruijn Contradiction Resolution:**
- **CRITICAL:** Decision 4 says "variable names are significant" but Decision 11 says "de Bruijn from day one." These contradict. **Resolution: Follow Decision 11.** Decision 4 was superseded during the brainstorm but not edited. De Bruijn indices provide alpha-equivalence which is essential for content-addressing.
- **Instance variable hashing fix:** Plan says "slot index only (name dropped)" for ivar refs, but absolute slot index depends on the inheritance chain (superclass ivar count). A method compiled for `SubClass` would get different ivar indices than the same method compiled for `BaseClass`. **Fix: Use relative ivar index** — position within the declaring class's own `InstVars` array, not the absolute offset. This makes method hashes class-independent.

**Unison Content-Addressing Research (best-practices-researcher):**
- **Tag byte scheme:** Unison uses leading tag prefixes for cross-domain collision prevention — terms start with Tag 1, types start with Tag 0. Our 0x01+ scheme is fine but consider reserving a prefix bit to distinguish expression nodes from declaration nodes.
- **Golden-file testing strategy is essential:** Store known AST→bytes→hash triples in testdata files. Run `go test -update` to regenerate. This catches unintentional hash format changes.
- **Effect ordering must be normalized** — sort effects/side-effect annotations alphabetically before hashing to ensure determinism
- **Quadratic hashing risk in mutual recursion:** If class A references class B and B references A, naively computing recursive hashes creates infinite loops. **Mitigation:** Use a "hash in progress" sentinel. Classes being hashed get a placeholder; circular references hash to the placeholder. This matches Unison's approach.
- **Keep hashing code concentrated in one package** — our plan puts it in `vm/hash_*.go` which is good, but consider `compiler/hash/` since it's a compiler concern

**Go HPC Architect — SHA-256 Performance:**
- `sha256.Sum256` is ~60ns for 100 bytes on Apple M3 (uses ARM SHA2 hardware acceleration via `crypto/sha256`)
- `map[[32]byte]` lookup is ~10ns with Go's Swiss Tables implementation
- No Merkle tree library needed — recursive AST hashing is sufficient (walk tree, hash leaves, hash interior nodes from child hashes)
- Use `encoding/binary` for deterministic serialization of integer/float values (not `fmt.Sprintf`)
- 32 bytes per `CompiledMethod` for `ContentHash` is perfectly acceptable memory overhead

**Architecture Strategist:**
- Content store section ordering in image format should be specified — write content store after class table but before method source (dependencies flow: classes → content store → sources)
- Document ProcessEnv.Set COW copy cost explicitly: ~35-55μs for 1000 entries, acceptable for the rare case when a process first writes

**Code Simplicity:**
- Frozen hashing AST as separate Go types is YAGNI for Phase 2a — consider reusing the existing AST types and stripping position/name info during serialization rather than maintaining parallel type hierarchies. **Counter-argument:** separate types enforce the invariant that hashed ASTs are normalized. **Decision: evaluate complexity during implementation.**

---

#### Phase 3: Dependency Namespace Mapping

**Goal:** Dependencies map to importable namespaces. Producer-declared, consumer-overridable. Collisions are hard errors.

**Manifest changes:**

Add `Namespace` field to `Dependency` struct (`manifest/manifest.go:37-41`):

```go
type Dependency struct {
    Git       string `toml:"git"`
    Tag       string `toml:"tag"`
    Path      string `toml:"path"`
    Namespace string `toml:"namespace"` // consumer override
}
```

**Namespace resolution order for a dependency:**
1. Consumer override (`namespace` key in `[dependencies]`)
2. Producer's `maggie.toml` `[project] namespace`
3. PascalCase of dependency key (fallback)
4. Hard error if none of the above produces a namespace

**Override semantics:** The override replaces the root segment. If dependency declares `namespace = "Yutani"` and a file within it declares `namespace: 'Yutani::Widgets'`, a consumer override of `namespace = "ThirdParty::Yutani"` remaps `Yutani::Widgets` to `ThirdParty::Yutani::Widgets`. The override is a prefix substitution.

**Collision detection:** After all dependencies are resolved and namespace-mapped, but before loading, scan for FQN collisions. If two dependencies produce the same FQN, fail with:

```
error: namespace collision: class "Yutani::Widget" defined by both
  dependency "yutani" (git: github.com/acme/yutani, tag: v0.5.0)
  dependency "yutani-fork" (git: github.com/other/yutani, tag: v1.0.0)
hint: add namespace override to one dependency in maggie.toml:
  [dependencies]
  yutani-fork = { git = "...", namespace = "YutaniFork" }
```

**Diamond dependency:** When dep A and dep B both depend on dep C, dep C is loaded once. If the consumer overrides C's namespace, that override applies globally. If A or B internally override C's namespace, those overrides are local to their own compilation context — but this is a future concern (transitive namespace overrides are not supported in this phase).

**Files to modify:**

| File | Change |
|------|--------|
| `manifest/manifest.go` | Add `Namespace` field to `Dependency`. |
| `manifest/resolver.go` | Compute effective namespace per dependency. |
| `cmd/mag/main.go` | Add collision detection pass before loading. Apply namespace overrides when loading dependency files. |

**Acceptance criteria:**
- [ ] Dependency with `[project] namespace` loads under declared namespace
- [ ] Consumer `namespace` override remaps all classes (prefix substitution)
- [ ] Missing namespace falls back to PascalCase of dependency key
- [ ] FQN collision between two dependencies produces clear error with fix suggestion
- [ ] `mag deps list` shows effective namespace per dependency

**Phase 3 Research Insights:**

**Security Sentinel — Dependency Loading:**
- **HIGH: Command injection via git dependency loading** — if dependency URLs or tags are not sanitized, a malicious `maggie.toml` could inject shell commands via crafted git URLs (e.g., `git = "$(malicious command)"`). **Mitigation:** Use `exec.Command("git", "clone", "--", url, dest)` with explicit argument separation. Never pass URLs through shell interpolation. Validate URLs match `https://` or `git://` schemes only.
- **MEDIUM: Namespace injection via malicious dependencies** — a dependency could declare `namespace = "Object"` or `namespace = "Array"` to shadow core classes. **Mitigation:** Maintain a reserved namespace list (all built-in class names). Error if a dependency attempts to use a reserved namespace as its root.

**Architecture Strategist:**
- Collision detection should happen as a distinct pass between dependency resolution and loading — this keeps the error UX clean (all collisions reported at once, not one at a time)
- Consider reporting namespace collisions with a `diff`-style output showing which classes clash and where they come from

**Python Circular Import Cautionary Tale (best-practices-researcher):**
- Python's circular import problems stem from mixing registration with definition — a module that imports during class body execution can see half-initialized modules. Our two-pass design avoids this entirely (all registration before any definition), but the same principle applies to dependency loading: register all dependency namespaces before loading any dependency code.

---

#### Phase 4: Process-Level Restriction

**Goal:** Each process gets its own Globals map. Restricted processes see a filtered view where certain names don't exist.

**Prerequisite — thread safety:** The current `Globals` map (`map[string]Value`) is not thread-safe. Before implementing COW overlays, add `sync.RWMutex` protection or migrate to a concurrent-safe structure. Options:
- `sync.RWMutex` wrapper with `LookupGlobal`/`SetGlobal` methods on VM
- Use the COW overlay itself as the safety mechanism: the shared "root" map is immutable after loading; only per-process copies are writable

**Decision:** Use the COW overlay as the safety mechanism. After the two-pass loading pipeline completes, the root Globals map is frozen (no writes). Processes that need to write (e.g., REPL process, `Compiler evaluate:`) get a COW copy. This is simpler than adding a mutex to every `OpPushGlobal`.

**Implementation:**

```go
// vm/process_env.go
type ProcessEnv struct {
    parent  *ProcessEnv       // nil for root
    globals map[string]Value  // COW: nil until first write or restriction
    frozen  map[string]Value  // read-only reference to parent's map
}

func (e *ProcessEnv) Lookup(name string) (Value, bool) {
    if e.globals != nil {
        v, ok := e.globals[name]
        return v, ok
    }
    v, ok := e.frozen[name]  // single lookup, proper ok check
    return v, ok
}

func (e *ProcessEnv) Set(name string, val Value) {
    if e.globals == nil {
        e.globals = make(map[string]Value, len(e.frozen))
        for k, v := range e.frozen {
            e.globals[k] = v
        }
    }
    e.globals[name] = val
}

func (e *ProcessEnv) Restrict(remove []string) *ProcessEnv {
    child := &ProcessEnv{parent: e}
    // Shallow copy and remove
    src := e.globals
    if src == nil {
        src = e.frozen
    }
    child.globals = make(map[string]Value, len(src)-len(remove))
    removeSet := make(map[string]bool, len(remove))
    for _, r := range remove {
        removeSet[r] = true
    }
    for k, v := range src {
        if !removeSet[k] {
            child.globals[k] = v
        }
    }
    return child
}
```

**Interpreter change:** Replace `Globals map[string]Value` field on `Interpreter` with `Env *ProcessEnv`. Modify `OpPushGlobal` and `OpStoreGlobal` to use `i.Env.Lookup` / `i.Env.Set`.

**Fork path:** When `[expr] fork` creates a new interpreter, it gets a `ProcessEnv` pointing to the parent's frozen map. Zero allocation unless the child writes or is restricted.

**Maggie-side API (minimal):**

```smalltalk
"Fork with restricted environment — remove File and HTTP classes"
proc := [expr] forkRestricted: #('File' 'HTTP' 'TCPSocket').

"Or via Process class"
proc := Process forkWithout: #('File' 'HTTP') do: [dangerous code].
```

This requires a new primitive on `Block` or `Process`. The primitive creates a `ProcessEnv` with the specified names removed.

**OpPushGlobal behavior for missing names:** Keep current behavior (push Nil) for now. Add a `--strict-globals` flag or future `NameError` exception as a separate enhancement. The restricted process model works with Nil — `Nil new` sends `new` to nil, which produces a clear `doesNotUnderstand:` error.

**Files to modify:**

| File | Change |
|------|--------|
| `vm/process_env.go` | New file: `ProcessEnv` type with Lookup, Set, Restrict |
| `vm/interpreter.go` | Replace `Globals` with `Env *ProcessEnv`. Update `OpPushGlobal`, `OpStoreGlobal`. |
| `vm/vm.go` | After loading, freeze root Globals into `ProcessEnv`. |
| `vm/process_primitives.go` | Add `forkRestricted:` / `forkWithout:do:` primitives. |

**Acceptance criteria:**
- [ ] Default forked process inherits parent's globals (zero allocation)
- [ ] Restricted process cannot see removed names
- [ ] Restricted process's child inherits restriction
- [ ] `Compiler evaluate:` works in processes with writable COW copy
- [ ] No data races under `go test -race`

**Phase 4 Research Insights:**

**Performance Oracle — ProcessEnv.Lookup Bug (CRITICAL):**
```go
// BUGGY (from plan):
return e.frozen[name], e.frozen[name] != Nil  // two map lookups, Nil conflation

// FIXED:
v, ok := e.frozen[name]
return v, ok
```
The original code: (1) performs two map lookups instead of one, and (2) conflates a Nil-valued global with a missing key. A global explicitly set to `Nil` would report as "not found." Value(0) (float 0.0) would be incorrectly returned for missing keys instead of Nil.

**Performance Oracle — Hot Path Preservation:**
- `OpPushGlobal` is the single hottest path for global access. The plan says "OpPushGlobal unchanged (map lookup)" but Phase 4 changes it to `i.Env.Lookup()`. **Recommendation:** Keep a direct `map[string]Value` reference on Interpreter alongside `Env`. On fork, point this reference at the ProcessEnv's active map. This avoids a method call + nil check on every global lookup.
- **Missing benchmark:** Add `BenchmarkHotPath_GlobalLookup` to the benchmark suite before starting Phase 4. This is the regression canary.
- `newInterpreter()` has wasteful allocations on the fork path — it creates then discards SelectorTable, ClassTable, and Globals map. Fix this during Phase 4 refactor.

**Security Sentinel — Privilege Escalation Paths (CRITICAL for Phase 4):**
- **Compiler evaluate: bypasses restriction** — `Compiler evaluate:` operates on `vm.interpreter.Globals` (the main/root interpreter), not the calling process's restricted Globals. A restricted process running `Compiler evaluate: 'File new'` would bypass its own restriction. **Mitigation:** `Compiler evaluate:` must use the calling process's `ProcessEnv`, not the root VM's Globals. Thread the calling interpreter's Env through to the evaluation path.
- **Object allClasses bypasses restriction** — `Object allClasses` reads the shared ClassTable, not Globals. A restricted process that can't see `File` in Globals can still do `Object allClasses detect: [:c | c name = 'File']` to get the class. **Mitigation:** Filter `allClasses` through the calling process's ProcessEnv — only return classes whose FQN exists in the process's Globals.
- **thisContext sender chain** can walk beyond process boundary — a restricted process could potentially inspect parent process state via `thisContext sender`. **Mitigation:** Truncate sender chain at process boundary (return nil sender for the process's entry frame).
- **Shared mutable state:** ClassTable and VTable are shared across all processes. A restricted process that has a class reference can still modify its methods (if any method-modification primitives exist). **Mitigation:** Audit primitives for class/method mutation; restrict or copy-on-write as needed.

**Go HPC Architect — COW Implementation:**
- Use `maps.Clone` (Go 1.21+) for COW copy — this is optimized and clearer than manual range loop
- COW shallow copy cost: ~35-55μs for 1000 entries, ~350-550μs for 10000 entries. Acceptable since copy only happens on first write.
- For the `Restrict` method, building the remove set and filtering is O(n+m). Consider pre-sorting remove list and using binary search if the remove list is large (unlikely in practice — most restrictions are <10 names).

**Code Simplicity:**
- ProcessEnv COW is the right pattern, but the manual copy loop should use `maps.Clone`. The `parent` field may be unnecessary if we don't need to walk up the chain — evaluate whether parent tracking adds value beyond debugging.

---

### Phases 5–6: Sketched (Separate Plans)

#### Phase 5: Go Interop (Future Plan)

- `mag wrap` CLI subcommand: introspect Go package, generate stub files
- `[go-wrap]` section in `maggie.toml`
- `mag build` CLI subcommand: compile custom binary with wrappers
- Type mapping: Go primitives ↔ Maggie primitives, structs ↔ opaque handles, `(T, error)` ↔ Result monad
- Phased extension: custom binary (Phase 5a), Go plugins (Phase 5b), Yaegi interpreted (Phase 5c)

#### Phase 6: Distribution Protocol (Future Plan)

- CBOR wire format for content-addressed chunks (requires Phase 2)
- HTTP/2 sync endpoint with `have`/`want` protocol
- Hash verification on receive
- Peer reputation tracking
- Capability manifest negotiation
- Depends on: distributed runtime roadmap Phase 0 (registry unification, Globals thread safety)

---

## Alternative Approaches Considered

See brainstorm document, section "Approaches Considered & Rejected":
- **Pure Object-Capability (Newspeak-style):** Rejected as ergonomically painful
- **Capability-Aware Imports (Gatekeeper Layer):** Rejected as "security theater"
- **Capability Objects (Authority from Objects, Not Names):** Rejected as too costly for the benefit

---

## Acceptance Criteria

### Functional Requirements

- [ ] Forward references across files work in any load order
- [ ] Circular imports work (`A imports B`, `B imports A`)
- [ ] `import:` affects method body class resolution (not just superclass)
- [ ] Qualified and unqualified access both work; bytecode contains FQN
- [ ] No silent superclass-defaults-to-Object
- [ ] No silent short-name collisions
- [ ] Content-addressed hashing: same source → same hash, alpha-equivalent → same hash
- [ ] Image format versioned; old images loadable
- [ ] Dependencies map to namespaces; collisions are hard errors
- [ ] Per-process Globals with COW overlay; restricted processes see filtered view

### Non-Functional Requirements

- [ ] No performance regression on `BenchmarkHotPath` (run `./scripts/bench-compare.sh`)
- [ ] `OpPushGlobal` remains a single map lookup (no added indirection in hot path)
- [ ] Zero allocation for default (non-restricted) forked processes
- [ ] All existing tests pass at each phase boundary

### Quality Gates

- [ ] `go test ./...` passes at each phase
- [ ] `go test -race ./...` passes (especially after Phase 4)
- [ ] Each phase has dedicated tests (not just regression)
- [ ] Alpha-equivalence tests for content-addressing

### Acceptance Criteria Research Insights

**Additional test categories discovered by research agents:**
- [ ] Golden-file tests for content-addressing hash stability (Phase 2) — store known AST→bytes→hash triples in `testdata/`, use `go test -update` to regenerate
- [ ] `BenchmarkHotPath_GlobalLookup` benchmark added before Phase 4 (performance oracle: missing from current suite)
- [ ] Benchmark comparison before/after Phase 4 ProcessEnv change (`./scripts/bench-compare.sh`)
- [ ] Security: `Compiler evaluate:` in a restricted process cannot access restricted names
- [ ] Security: `Object allClasses` in a restricted process returns only visible classes
- [ ] Security: Git dependency URLs validated against injection (no shell metacharacters)
- [ ] Security: Reserved namespace list prevents dependencies from shadowing core classes
- [ ] Pending buffer isolation: namespace/import state does not leak between files in batch loading

---

## Dependencies & Prerequisites

| Dependency | Phase | Notes |
|------------|-------|-------|
| Two-pass loading | Phase 1a | Prerequisite for compiler FQN resolution |
| Compiler FQN resolution | Phase 1b | Prerequisite for removing short-name Globals |
| Phase 1 complete | Phase 2 | Content-addressing needs stable FQNs in AST |
| Phase 1 complete | Phase 3 | Dependency namespaces need FQN resolution |
| Phase 2 complete | Phase 6 | Distribution needs content-addressed chunks |
| Globals thread safety | Phase 4 | COW overlay is the safety mechanism |

---

## Risk Analysis & Mitigation

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Breaking change when removing short-name Globals | High | Medium | Land Phase 1a+1b first, giving users time to add `import:` declarations. Audit all `lib/` files. |
| CompileFunc API change breaks callers | Medium | Medium | Add new function alongside existing one. Existing callers continue to work with no-context compilation. |
| fileIn at runtime behaves differently from batch loading | High | Low | Document the difference. fileIn operates on populated ClassTable, so most references resolve. |
| Image format version bump breaks existing images | Medium | Low | New reader accepts old format. Old reader rejects new format with clear error. |
| Performance regression from ProcessEnv indirection | Low | Medium | Benchmark before/after. ProcessEnv.Lookup is one extra pointer dereference, comparable to current map lookup. |
| Tag byte assignment conflicts in future | Low | High | Reserve ranges. Document allocation table in code. Use version prefix for format evolution. |
| Circular class hashing (mutual recursion) | Medium | High | Use "hash in progress" sentinel per Unison pattern. Classes being hashed get placeholder; circular refs hash to placeholder. |
| Compiler evaluate: privilege escalation | High | High | Thread calling process's ProcessEnv through evaluation path. Must be fixed before Phase 4 ships. |
| Object allClasses bypass of restriction | Medium | Medium | Filter allClasses through ProcessEnv visibility. |
| Git dependency URL injection | Low | High | Use exec.Command with explicit args, validate URL schemes (https://, git:// only). |
| 80+ test images need updating on version bump | High | Low | Write batch update script. Run once when Phase 2c lands. |

---

## Open Questions (Resolved with Defaults)

These were identified during SpecFlow analysis. Defaults are documented; revisit if they cause friction.

| # | Question | Default Decision |
|---|----------|-----------------|
| Q1 | Is two-pass loading across ALL files or per-directory? | ALL files (deps + project) before any compilation |
| Q2 | How does superclass resolution work in pass 1? | Two sub-phases: 1a register names, 1b resolve superclasses |
| Q3 | How distinguish class refs from user globals? | LookupWithImports first; if not found, fall through to raw name |
| Q4 | What about `fileIn` at runtime? | Single-pass, operates on populated ClassTable. Document difference. |
| Q5 | Globals map thread safety? | COW overlay is the mechanism. Root map frozen after loading. |
| Q6 | REPL namespace/import context? | No namespace, no imports. Bare classes work. `:import` command is future nice-to-have. |
| Q7 | Namespace override: root replacement or full? | Root segment replacement (prefix substitution) |
| Q8 | OpPushGlobal for missing name: Nil or error? | Keep Nil for now. NameError is future enhancement. |
| Q9 | Should traits be namespaced? | Yes. Add `Namespace` field to `Trait`. |
| Q10 | Image format version bump? | Yes, version 3 → 4 in Phase 2c. Reader accepts both. |

---

## References & Research

### Internal References

- **Brainstorm:** `docs/brainstorms/2026-02-04-module-system-design-brainstorm.md` (14 design decisions)
- **Distributed runtime roadmap:** `docs/roadmaps/2026-02-03-distributed-runtime-roadmap.md`
- **Compiler docstring implementation:** `docs/solutions/language-features/compiler-native-docstrings.md` (reference for flowing new constructs through pipeline)
- **Compiler codegen (class ref emission):** `compiler/codegen.go:458-461`
- **Loader (single-pass):** `cmd/mag/main.go:761` (`compileSourceFile`)
- **LookupWithImports:** `vm/class.go:333`
- **Short-name Globals registration:** `cmd/mag/main.go:820`
- **Bootstrap two-pass pattern:** `cmd/bootstrap/main.go`
- **Trait struct (no namespace):** `vm/trait.go:12-17`
- **Dependency struct (no namespace field):** `manifest/manifest.go:37-41`
- **OpPushGlobal runtime lookup:** `vm/interpreter.go:567-584`
- **CompileFunc type:** `vm/compiler_dispatch.go:33`

### External References

- **Unison hashing architecture:** [unison-lang.org](https://www.unison-lang.org/) — Content-addressed code, frozen hashing types, version-prefixed serialization
- **Lua `_ENV` mechanism:** [lua.org/manual/5.4](https://www.lua.org/manual/5.4/) — Per-chunk environment tables for sandboxing
- **CBOR deterministic encoding:** [RFC 8949 §4.2](https://www.rfc-editor.org/rfc/rfc8949#section-4.2)
- **De Bruijn indices:** [Wikipedia](https://en.wikipedia.org/wiki/De_Bruijn_index) — Positional variable naming for alpha-equivalence
- **Go compiler multi-phase loading:** `cmd/compile/internal/noder` — collectObjects → packageObjects → processDelayed pattern
- **Java lazy completion:** `javac` halfcompleted queue for circular class completion
- **Roslyn batch superclass resolution:** Batch-resolves all base types before member compilation
- **Go maps.Clone:** `golang.org/x/exp/maps` / Go 1.21+ stdlib — optimized shallow map copy for COW
- **Go crypto/sha256 ARM acceleration:** Uses hardware SHA2 instructions on Apple M-series (60ns per 100B)
- **Unison cross-domain tag prefixes:** Terms start Tag 1, types start Tag 0 — prevents cross-domain hash collisions
