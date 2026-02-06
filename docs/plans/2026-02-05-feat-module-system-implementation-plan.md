---
title: "feat: Module System Phase 1 — Two-Pass Loading + FQN Resolution"
type: feat
date: 2026-02-05
brainstorm: docs/brainstorms/2026-02-04-module-system-design-brainstorm.md
epics: maggie-vc64, maggie-gu5d
---

# Module System Phase 1: Two-Pass Loading + FQN Resolution

## Overview

Fix the three bugs that make `namespace:` and `import:` decorative. After this work, the module system actually enforces correct name resolution.

This plan covers Phase 1 only. Subsequent phases (content-addressing, dependency namespace mapping, process-level restriction, Go interop, distribution) are preserved as future work in a [companion document](2026-02-05-module-system-future-phases.md) based on the [brainstorm](../brainstorms/2026-02-04-module-system-design-brainstorm.md).

## Problem Statement

Three bugs in the current loading/compilation pipeline prevent correct module semantics:

1. **Compiler does not resolve class refs to FQN in method bodies** (`compiler/codegen.go:458-461`). `import:` has no effect on method bodies — unqualified names work only by accident via short-name Globals registration.
2. **Short-name Globals registration causes silent collisions** (`cmd/mag/main.go:820`). Every class is registered under both bare name and FQN. Last-loaded wins silently when two namespaces define the same class name.
3. **Loading pipeline is single-pass** (`cmd/mag/main.go:761`, `compileSourceFile`). Classes and methods compile in one pass per file. Forward references across files fail (superclass defaults to Object silently). Circular imports are impossible.

Until these are fixed, the namespace/import system parses correctly but does not enforce correct resolution.

## Technical Approach

The implementation touches two primary files:

| File | Changes |
|------|---------|
| `cmd/mag/main.go` | Split single-pass loading into two-pass. Remove short-name Globals registration. |
| `compiler/codegen.go` | Add FQN resolution during method compilation. |

Supporting changes in `vm/trait.go` (add Namespace field) and `vm/compiler_dispatch.go` (thread compile context).

### Phase 1a: Two-Pass Loading

**Goal:** All class skeletons are registered before any method bodies compile. Forward references and circular imports work.

**Key design decision:** Two-pass loading is batch-level, not per-file. Pass 1 covers ALL files (deps + project) before ANY pass 2 compilation begins. Per-file two-pass still breaks on cross-file forward references.

**Superclass resolution within Pass 1 requires sub-phases:**
- **Pass 1a:** Parse all `.mag` files. Register class names in ClassTable with placeholder (nil) superclass. Store `(SourceFile, namespace, imports)` tuples for replay.
- **Pass 1b:** Resolve superclass pointers. Walk all registered skeletons, resolve each superclass name via `LookupWithImports` against the now-complete ClassTable. Error if superclass not found (instead of silently defaulting to Object).
- **Pass 2:** Compile all method bodies. ClassTable is fully populated with resolved superclass chains. Compiler has FQN resolution context.

**Changes to `cmd/mag/main.go`:**

Split `compileSourceFile` into `registerClassSkeleton` + `compileClassMethods`. New `compileAll(files []parsedFile)` orchestrates two-pass. `compilePath` collects parsed files instead of compiling inline. `loadProject` collects all dep + project files before calling `compileAll`.

Remove silent Object fallback for unresolved superclasses (line ~799). Emit clear error:
```
class Foo: superclass Bar not found
  declared in: src/myapp/foo.mag:3
  namespace: MyApp
  imports searched: [Yutani, Yutani::Widgets]
```

**Namespace/import state isolation:** Each `parsedFile` carries its own namespace and imports. State from File A must not leak into File B during batch loading.

**Trait resolution:** Traits also need two-pass treatment. Add `Namespace string` to `Trait` struct (`vm/trait.go:12-17`). Register trait names in pass 1a, resolve trait references in pass 1b.

**fileIn at runtime:** `Compiler fileIn:` remains single-pass. Runtime fileIn operates on an already-populated ClassTable — this is acceptable and should be documented.

**fileInAll at runtime:** `Compiler fileInAll:` should use two-pass within its batch. It already walks a directory; modify it to collect all parsed files first, register skeletons, then compile.

**FileOut:** `Compiler fileOut:` needs to emit `namespace:` and `import:` declarations when reconstructing source.

**Acceptance criteria:**
- [ ] Forward reference across files works: File A references class from File B, loaded in any order
- [ ] Circular imports work: File A imports File B, File B imports File A
- [ ] Unresolved superclass produces clear error with file, namespace, and imports searched
- [ ] Traits are namespaced and participate in two-pass loading
- [ ] Trait declared in File B, used in File A, loaded in any order (trait forward references)
- [ ] `fileInAll` uses two-pass within its batch
- [ ] `fileInAll` handles classes that already exist in ClassTable (specify: merge methods? error? skip?)
- [ ] Namespace/import state does not leak between files in batch loading
- [ ] `fileOut` emits `namespace:` and `import:` declarations
- [ ] All existing tests pass (no regression from restructuring)

---

### Phase 1b: Compiler FQN Resolution

**Goal:** The compiler resolves unqualified class names to FQNs during method body compilation. Bytecode contains FQNs, not raw names.

**Key design decision — distinguishing class refs from other globals:** The compiler attempts `LookupWithImports` first. If a class is found, emit the FQN. If not found, emit the raw name (for user globals like `x := 42`). This is "best effort" resolution — class references that can be resolved at compile time are; everything else falls through to runtime.

**Compiler context threading:**

The compiler already imports `vm` (`compiler/codegen.go:6`). Add fields to the `Compiler` struct for namespace context and a `ClassTable` reference:

```go
type Compiler struct {
    // ... existing fields ...
    namespace  string          // current file's namespace
    imports    []string        // current file's import list
    classTable *vm.ClassTable  // for FQN resolution (nil = no resolution)
}
```

When `classTable` is nil (REPL, tests, backward compat), FQN resolution is skipped — existing behavior preserved with zero changes to callers.

**FQN resolution method** on Compiler to centralize logic:

```go
func (c *Compiler) resolveClassName(name string) string {
    if c.classTable == nil {
        return name
    }
    if fqn := c.classTable.LookupWithImports(name, c.namespace, c.imports); fqn != "" {
        return fqn
    }
    return name
}
```

**Change in `compileVariable`** (`compiler/codegen.go:458-461`):

```go
// Current: unconditionally emits raw name
idx := c.addLiteral(c.symbols.SymbolValue(name))
c.builder.EmitUint16(vm.OpPushGlobal, uint16(idx))

// New: resolve class names to FQN
resolvedName := c.resolveClassName(name)
idx := c.addLiteral(c.symbols.SymbolValue(resolvedName))
c.builder.EmitUint16(vm.OpPushGlobal, uint16(idx))
```

**Change in `compileAssignment`** (global assignment path): Same FQN resolution. Without this fix, a global assignment to a class name would store under the short name, reintroducing the collision bug.

**Block bodies:** Blocks share the same `Compiler` instance, so FQN resolution applies automatically inside blocks.

**Files to modify:**

| File | Change |
|------|--------|
| `compiler/codegen.go` | Add `namespace`, `imports`, `classTable` fields. Add `resolveClassName` method. Modify `compileVariable` and `compileAssignment` for FQN resolution. |
| `vm/compiler_dispatch.go` | Thread compile context (namespace, imports, classTable) through `CompilerBackend`. |
| `cmd/mag/main.go` | Pass namespace, imports, and classTable when compiling method bodies in pass 2. |

**Acceptance criteria:**
- [ ] `Button` in a file with `import: 'Yutani::Widgets'` compiles to `OpPushGlobal("Yutani::Widgets::Button")`
- [ ] FQN `Yutani::Widgets::Button` passes through unchanged
- [ ] User globals (`x` from `Compiler evaluate: 'x := 42'`) fall through correctly (no false FQN resolution)
- [ ] Bare/root classes (`Array`, `Object`) resolve by short name (FQN = short name)
- [ ] Global assignment to a class name resolves to FQN in emitted bytecode
- [ ] Block bodies within methods resolve class names via import context
- [ ] Backward compatibility: nil classTable = no resolution (REPL, tests unchanged)

---

### Phase 1c: Remove Short-Name Globals Registration

**Goal:** Only register classes under their FQN in Globals. Eliminate silent last-loaded-wins collisions.

**Breaking change:** Any existing `.mag` code that references a namespaced class by short name (without an import) will break. This is intentional — the old behavior was a bug, not a feature.

**Migration path:** Since Phases 1a and 1b land first, the compiler resolves unqualified names to FQNs via imports. Users must add `import:` declarations to their source files for any cross-namespace reference.

**REPL context:** Bare/root classes (`Array`, `String`, `Object`, etc.) work fine (their FQN = short name). Namespaced classes must be referenced by FQN in the REPL.

**Error behavior after 1c:** Referencing a namespaced class by short name without an import causes the compiler to emit the raw short name (FQN resolution finds nothing). At runtime, `OpPushGlobal` with the short name finds nothing in Globals and pushes Nil. The subsequent message send produces `doesNotUnderstand:`. This is not ideal UX but is acceptable for Phase 1 — a future `NameError` exception would improve it.

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

## Acceptance Criteria

### Functional Requirements

- [ ] Forward references across files work in any load order
- [ ] Circular imports work (`A imports B`, `B imports A`)
- [ ] `import:` affects method body class resolution (not just superclass)
- [ ] Qualified and unqualified access both work; bytecode contains FQN
- [ ] No silent superclass-defaults-to-Object
- [ ] No silent short-name collisions

### Non-Functional Requirements

- [ ] No performance regression on `BenchmarkHotPath` (run `./scripts/bench-compare.sh`)
- [ ] `OpPushGlobal` remains a single map lookup (no added indirection in hot path)
- [ ] All existing tests pass at each phase boundary

### Quality Gates

- [ ] `go test ./...` passes after each sub-phase (1a, 1b, 1c)
- [ ] Each sub-phase has dedicated tests (not just regression)

---

## Dependencies & Prerequisites

| Dependency | Notes |
|------------|-------|
| Phase 1a (two-pass loading) | Prerequisite for Phase 1b (FQN resolution needs populated ClassTable) |
| Phase 1b (FQN resolution) | Prerequisite for Phase 1c (removing short-name Globals requires FQN resolution to be working) |

---

## Risk Analysis & Mitigation

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Breaking change when removing short-name Globals | High | Medium | Land 1a+1b first, giving time to add `import:` declarations. Audit all `lib/` files. |
| Namespace/import state leaking between files in batch | Medium | High | Each `parsedFile` carries own namespace/imports. Clear pending state between files. |
| fileIn at runtime behaves differently from batch loading | High | Low | Document the difference. fileIn operates on populated ClassTable, so most references resolve. |

---

## Open Questions (Resolved with Defaults)

| # | Question | Default Decision |
|---|----------|-----------------|
| Q1 | Is two-pass loading across ALL files or per-directory? | ALL files (deps + project) before any compilation |
| Q2 | How does superclass resolution work in pass 1? | Two sub-phases: 1a register names, 1b resolve superclasses |
| Q3 | How distinguish class refs from user globals? | LookupWithImports first; if not found, fall through to raw name |
| Q4 | What about `fileIn` at runtime? | Single-pass, operates on populated ClassTable. Document difference. |
| Q5 | REPL namespace/import context? | No namespace, no imports. Bare classes work. |
| Q6 | Should traits be namespaced? | Yes. Add `Namespace` field to `Trait`. |

---

## References

- **Brainstorm:** `docs/brainstorms/2026-02-04-module-system-design-brainstorm.md` (14 design decisions)
- **Compiler docstring implementation:** `docs/solutions/language-features/compiler-native-docstrings.md` (reference for flowing new constructs through pipeline)
- **Compiler codegen (class ref emission):** `compiler/codegen.go:458-461`
- **Loader (single-pass):** `cmd/mag/main.go:761` (`compileSourceFile`)
- **LookupWithImports:** `vm/class.go:333`
- **Short-name Globals registration:** `cmd/mag/main.go:820`
- **Bootstrap two-pass pattern:** `cmd/bootstrap/main.go`
- **Trait struct (no namespace):** `vm/trait.go:12-17`
- **CompileFunc type:** `vm/compiler_dispatch.go:33`

## Future Phases

The following phases are deferred to separate plans. The original detailed designs are preserved in [2026-02-05-module-system-future-phases.md](2026-02-05-module-system-future-phases.md) as reference material.

- **Phase 2: Content-Addressing** — Frozen hashing AST, de Bruijn indexing, SHA-256 content store, image format v4. Prerequisite for distribution protocol.
- **Phase 3: Dependency Namespace Mapping** — Producer-declared/consumer-overridable namespaces, collision detection. Depends on Phase 1c.
- **Phase 4: Process-Level Restriction** — Per-process Globals with COW overlay, `forkRestricted:` primitive. Note: ProcessEnv.Lookup must use `v, ok := map[name]` (not double lookup). `Compiler evaluate:` must thread calling process's env to prevent privilege escalation.
- **Phase 5: Go Interop** — `mag wrap` / `mag build` CLI subcommands.
- **Phase 6: Distribution Protocol** — CBOR wire format, HTTP/2 sync, hash verification.
