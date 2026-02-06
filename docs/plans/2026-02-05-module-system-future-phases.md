---
title: "Module System — Future Phases Reference (Phases 2–6)"
type: reference
date: 2026-02-05
brainstorm: docs/brainstorms/2026-02-04-module-system-design-brainstorm.md
status: deferred
---

# Module System — Future Phases Reference

This document preserves the detailed designs for Phases 2–6 of the module system. These are deferred until Phase 1 ships and real usage demonstrates the need. Each phase should get its own focused plan when the time comes.

See the [brainstorm](../brainstorms/2026-02-04-module-system-design-brainstorm.md) for the original 14 design decisions.

---

## Phase 2: Content-Addressing

**Goal:** Class and method definitions are content-addressed by hash. Names are a mutable mapping layer on top of immutable, content-addressed definitions.

**Prerequisite:** Phase 1 complete (stable FQNs in AST).

### Phase 2a: Frozen Hashing AST + Tag Byte Assignments

Define stable, frozen AST node types used only for hashing. Enumerate all node types and assign uint8 tag bytes.

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
| 0x0C | InstanceVarRef | Relative ivar index (position within declaring class's own InstVars, not absolute offset) |
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

Reserve 0x00 for "version prefix" and 0xFE-0xFF for future use. Gap at 0x0E-0x0F is intentional (reserved for future literal types).

### Phase 2b: AST Normalization + De Bruijn Indexing

Transform working AST to hashing AST. Normalize variable references.

**De Bruijn indexing:**
- Method args → `(0, slot_index)`
- Method temps → `(0, args_count + slot_index)`
- Block params → `(scope_depth, slot_index)`
- Block temps → `(scope_depth, params_count + slot_index)`
- Captured vars → resolved to original scope's `(depth, slot)`
- Instance variables → relative ivar index (NOT absolute slot)
- `self`, `super` → special node types
- Globals → FQN string

**Note:** Decision 4 ("variable names are significant") and Decision 11 ("de Bruijn from day one") contradict. Follow Decision 11 — de Bruijn indices provide alpha-equivalence essential for content-addressing.

**Normalization rules:**
- Methods sorted by selector before computing class hash
- Docstrings included in hash
- Literal values in canonical form
- Spans/positions stripped
- Effects/side-effect annotations sorted alphabetically before hashing

**Circular hashing:** Use "hash in progress" sentinel for mutual recursion (per Unison pattern).

### Phase 2c: Compiler Integration + Content Store

Add `ContentHash [32]byte` to `CompiledMethod`. Content store as `map[[32]byte][]byte` on VM. Image format version bump (3 → 4). Use `encoding/binary` for deterministic serialization.

**Testing:** Golden-file tests essential — store known AST→bytes→hash triples in `testdata/`.

**Performance:** SHA-256 is ~60ns per 100 bytes on Apple M3 (hardware accelerated). 32 bytes per method is negligible.

**Image version bump:** Will require updating 80+ test images — write a batch update script.

---

## Phase 3: Dependency Namespace Mapping

**Goal:** Dependencies map to importable namespaces. Producer-declared, consumer-overridable. Collisions are hard errors.

**Prerequisite:** Phase 1c complete (classes registered only by FQN).

**Manifest changes:** Add `Namespace string` to `Dependency` struct (`manifest/manifest.go:37-41`).

**Namespace resolution order:**
1. Consumer override (`namespace` key in `[dependencies]`)
2. Producer's `maggie.toml` `[project] namespace`
3. PascalCase of dependency key (fallback)
4. Hard error if none produces a namespace

**Override semantics:** Root segment replacement (prefix substitution).

**Collision detection:** Distinct pass between dependency resolution and loading. Report all collisions at once with fix suggestions.

**Security considerations:**
- Sanitize git dependency URLs — use `exec.Command` with explicit args, validate schemes (https://, git:// only)
- Maintain reserved namespace list to prevent dependencies from shadowing core classes (Object, Array, etc.)

---

## Phase 4: Process-Level Restriction

**Goal:** Each process gets its own Globals map. Restricted processes see a filtered view.

**Prerequisite:** Phase 1 complete.

**Design:** COW overlay as thread safety mechanism. Root Globals frozen after loading. Per-process copies only on write or restriction.

**Known issues to address when planning this phase:**
- `ProcessEnv.Lookup` must use `v, ok := map[name]` — original design had a double-lookup bug that conflated Nil-valued globals with missing keys
- `ProcessEnv.Set` should use two-level overlay (local writes map + frozen reads) rather than eager full copy — the REPL triggers a copy on first `x := 42`
- `Compiler evaluate:` must thread calling process's ProcessEnv to prevent privilege escalation
- `Object allClasses` must filter through ProcessEnv visibility
- `thisContext sender` chain should truncate at process boundary
- Keep direct `map[string]Value` reference on Interpreter for OpPushGlobal hot path
- Add `BenchmarkHotPath_GlobalLookup` before starting this phase
- Fix `newInterpreter()` wasteful allocations on fork path

**Maggie-side API:**
```smalltalk
proc := [expr] forkRestricted: #('File' 'HTTP' 'TCPSocket').
proc := Process forkWithout: #('File' 'HTTP') do: [dangerous code].
```

---

## Phase 5: Go Interop (Sketch)

- `mag wrap` CLI subcommand: introspect Go package, generate stub files
- `[go-wrap]` section in `maggie.toml`
- `mag build` CLI subcommand: compile custom binary with wrappers
- Type mapping: Go primitives <-> Maggie primitives, structs <-> opaque handles, `(T, error)` <-> Result monad
- Phased extension: custom binary (5a), Go plugins (5b), Yaegi interpreted (5c)

---

## Phase 6: Distribution Protocol (Sketch)

- CBOR wire format for content-addressed chunks (requires Phase 2)
- HTTP/2 sync endpoint with `have`/`want` protocol
- Hash verification on receive
- Peer reputation tracking
- Capability manifest negotiation
- Depends on: distributed runtime roadmap Phase 0
