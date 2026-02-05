# Adaptive Compilation: Implementation Status

This document tracks the implementation status of Maggie's adaptive compilation system. For architectural context, see the **Execution Architecture** section in [MAGGIE_DESIGN.md](MAGGIE_DESIGN.md).

## Architecture Overview

Maggie uses a tiered adaptive compilation system inspired by Cog VM (Pharo Smalltalk), adapted for Go's constraints. The "JIT" is not a true JIT — it generates Go source code for hot methods, not machine code. The generated code must be compiled by `go build` to take effect.

```
┌──────────────────────────────────────────────────────────────┐
│  Tier 0: Interpreter + Inline Cache                          │
│    - All code starts here                                    │
│    - Monomorphic IC on first call (cache class+method)       │
│    - Upgrade to PIC after 2nd different class                │
│    - Profiling counters increment                            │
├──────────────────────────────────────────────────────────────┤
│  Tier 1: Polymorphic Inline Cache (PIC)                      │
│    - Up to 6 cached (class, method) pairs per call site      │
│    - Falls back to megamorphic (full lookup) if exceeded     │
├──────────────────────────────────────────────────────────────┤
│  Tier 2: AOT Compiled (hot methods & blocks)                 │
│    - Methods/blocks exceeding invocation threshold           │
│    - Generated Go code via AOT compiler                      │
│    - Written to file or image, loaded on next startup        │
└──────────────────────────────────────────────────────────────┘
```

---

## Phase 1: Inline Caching — DONE

**Files:** `vm/inline_cache.go` (265 lines), `vm/inline_cache_test.go`

Monomorphic, polymorphic (up to 6 entries), and megamorphic inline caches. Integrated into the interpreter's send dispatch path. Each `CompiledMethod` holds an `InlineCacheTable` keyed by bytecode PC.

Statistics collection via `CollectICStats()` reports per-state counts and aggregate hit rates.

**Integrated into interpreter:** Yes — the interpreter checks IC before vtable lookup on every send.

---

## Phase 2: Method & Block Profiling — DONE

**Files:** `vm/profiler.go` (289 lines)

Tracks invocation counts for methods (threshold: 100) and blocks (threshold: 500) using atomic counters. Fires `OnHot` callback when threshold exceeded. Thread-safe via `sync.Map`.

Provides `TopMethods(n)` / `TopBlocks(n)` for diagnostics and `Stats()` for aggregate profiling data.

**Integrated into interpreter:** Yes — the profiler records invocations, and its `OnHot` callback connects to the JIT controller.

---

## Phase 3: Adaptive AOT Compilation — IMPLEMENTED, NOT DEPLOYED

**Files:** `vm/jit.go` (496 lines), `vm/aot.go` (714 lines)

### JIT Controller (`vm/jit.go`)
- Background compilation worker goroutine processes hot methods/blocks
- Connected to profiler via `OnHot` callback
- Deduplicates work via `compiledKeys` map
- `GenerateAOTPackage()` produces a complete Go package with dispatch table registration

### AOT Compiler (`vm/aot.go`)
- Translates bytecode → Go source for methods and blocks
- Handles full instruction set (97 opcodes) including `OpSendSuper`, `OpCreateBlock`, `OpTailSend`
- Generated code uses type-specialized fast paths (SmallInt, Float) with message-send fallback

### What works
- `vm.EnableJIT()` creates the JIT compiler and starts the background worker
- Hot method detection → Go code generation → file output works end-to-end
- AOT dispatch table integrated into VM (`aotMethods` checked before interpreter on every send)

### What doesn't work yet
- **Block compilation disabled by default** — `CompileBlocks: false` with comment "Blocks need more work for captures"
- **No CLI flag** — `EnableJIT()` / `DisableJIT()` exist in Go API but aren't wired to `cmd/mag/main.go`
- **Block registration TBD** — blocks have a different signature (captures, homeReturn) so need a separate dispatch table
- **No transparent runtime speedup** — generated Go source requires `go build` to become executable

---

## Phase 4: Compiled Code Persistence — IMPLEMENTED, NOT DEPLOYED

**Files:** `vm/jit_persistence.go` (560 lines), `vm/jit_persistence_test.go`

Three persistence modes:

| Mode | How it works | Pros | Cons |
|------|-------------|------|------|
| **Static** | Writes Go files for `go build` | Zero runtime overhead | Requires rebuild |
| **Plugin** | Compiles to `.so`, loads via `plugin.Open` | No rebuild needed | Linux/macOS only, same Go version required |
| **Image** | Appends AOT section to image file (gzip compressed) | Self-contained, portable | Stores Go source, not compiled code |

The image format uses an `AOT!` magic number appended after the regular `MAGI` image data. Entries are length-prefixed strings (class name, method name, Go source, checksum).

### What doesn't work yet
- **Bytecode checksum validation** — `Checksum` field exists but is always 0 (TODO in source)
- **Image mode doesn't auto-compile** — loads Go source into memory but doesn't compile it to executable form at runtime
- **No user-facing workflow** — all persistence requires Go API calls

---

## Performance Expectations

These are estimates from the original design, not measured benchmarks.

| Component | Expected Impact | Based On |
|-----------|----------------|----------|
| Inline caching | 1.5–2x faster dispatch | Cog VM: ~90% monomorphic sites |
| Polymorphic IC | +10% over monomorphic only | Cog benchmarks |
| Profiling | ~0% (small overhead) | — |
| Hot method AOT | 5–10x on hot methods | Eliminates interpreter dispatch loop |
| Hot block AOT | 5–10x on loops | Critical for real workloads |
| Preloaded AOT | Instant warm start | Like Sista snapshots |

Only inline caching and profiling are active in production. AOT performance gains require completing the deployment story.

---

## What's Needed to Make This User-Facing

1. **CLI flag** — Add `--jit` flag to `cmd/mag/main.go` that calls `vm.EnableJIT()`
2. **Block compilation** — Fix capture handling so `CompileBlocks` can be enabled
3. **Block dispatch table** — Separate table for block functions (different signature)
4. **Bytecode checksums** — Validate that AOT code matches current bytecode before dispatch
5. **Image auto-load** — When loading an image with an AOT section, register methods in the dispatch table automatically
6. **Benchmark validation** — Measure actual speedup on real workloads before advertising performance claims

---

*Originally created as a pre-implementation plan. Updated 2026-02-04 to reflect implementation status.*
