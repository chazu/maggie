# Maggie Language Guide — Brainstorm

**Date:** 2026-02-07
**Status:** Approved

## What We're Building

A comprehensive, practical guide to the Maggie language — from first expression to distributed runtime. The guide targets mainstream developers (Go, JS, Python background) who need Smalltalk concepts explained.

### Guide-as-Code Format

Each chapter is a `.mag` file in `lib/guide/` containing:
- A class whose **class-level docstring** is the chapter prose (rendered as the guide page)
- Methods whose **docstrings** contain subsection text
- Method **bodies** are runnable examples
- `>>>` assertions validated by `mag doctest`

Example structure:
```smalltalk
"""
# Getting Started with Maggie

Maggie is a Smalltalk-family language...
"""
Guide01GettingStarted subclass: Object

"""
## Your First Expression

Everything in Maggie is an expression that returns a value.

```test
3 + 4 >>> 7
'hello' , ' world' >>> 'hello world'
```
"""
method: basics [
    3 + 4.
    'hello' , ' world'
]
```

### Integration with `mag doc`

The doc generator will be extended to:
1. Recognize `lib/guide/` classes as guide pages (not API reference)
2. Render them in a sidebar "Guide" section, ordered by numeric prefix
3. Share CSS/JS with the existing API reference pages

### Replaces / Reworks

The existing `docs/USER_GUIDE.md` (~26KB, 1197 lines) will be superseded by this guide. The Markdown file can be kept as a legacy reference during transition.

## Why This Approach

1. **Guide-as-code eliminates hallucination**: Every code example in the guide is actual Maggie code that compiles and runs. `mag doctest` validates all `>>>` assertions.
2. **Piecewise authoring**: Each chapter is an independent file with a clear spec. An agent reads specific source files, writes one chapter, verifies via REPL.
3. **Single doc system**: Guide pages use the same rendering pipeline as API docs — no separate tooling.
4. **Living documentation**: Guide code runs against the actual VM, so breakage from language changes is caught by `mag doctest`.

## Key Decisions

1. **Audience**: Mainstream developers (Go/JS/Python). Smalltalk concepts explained, not assumed.
2. **Scope**: Full language — syntax, stdlib, concurrency, modules, projects, Go interop, distribution, tooling.
3. **Format**: `.mag` files in `lib/guide/`, rendered via extended `mag doc`.
4. **Grounding**: Source-verified specs + runnable example validation (both).
5. **Chapter naming**: `GuideNN<Topic>` class names (e.g., `Guide01GettingStarted`).
6. **Integration**: Sidebar navigation in `mag doc` output, alongside API reference.

## Chapter Plan

Each chapter lists the source files the writing agent must read before authoring, ensuring grounded, accurate content.

### 01 — Getting Started
**File:** `lib/guide/Guide01GettingStarted.mag`
**Sources:** `cmd/mag/main.go`, `README.md`
**Topics:** Installation, running mag, REPL basics, loading files, CLI overview

### 02 — Language Basics
**File:** `lib/guide/Guide02Basics.mag`
**Sources:** `examples/hello.mag`, `lib/Object.mag`
**Topics:** Expressions, message passing (unary/binary/keyword), cascades, variables, assignment, comments, nil/true/false, identity vs equality

### 03 — Numbers & Math
**File:** `lib/guide/Guide03Numbers.mag`
**Sources:** `lib/SmallInteger.mag`, `lib/Float.mag`
**Topics:** SmallInteger, Float, arithmetic, comparison, iteration (timesRepeat:, to:do:), math methods (factorial, gcd:), type testing

### 04 — Strings & Symbols
**File:** `lib/guide/Guide04Strings.mag`
**Sources:** `lib/String.mag`, `lib/Symbol.mag`, `lib/Character.mag`
**Topics:** String creation, concatenation, slicing, searching, case conversion, characters, symbols vs strings, character testing

### 05 — Collections
**File:** `lib/guide/Guide05Collections.mag`
**Sources:** `lib/Array.mag`, `lib/Dictionary.mag`, `examples/collections.mag`
**Topics:** Array creation, access, iteration (do:, collect:, select:, reject:, detect:, inject:into:), Dictionary (at:, at:put:, keys, values, iteration), 0-based indexing

### 06 — Blocks & Control Flow
**File:** `lib/guide/Guide06Blocks.mag`
**Sources:** `lib/Block.mag`, `lib/Boolean.mag`, `lib/True.mag`, `lib/False.mag`, `examples/blocks.mag`
**Topics:** Block syntax, arguments, closures, value/value:, conditionals (ifTrue:ifFalse:), loops (whileTrue:, whileFalse:, timesRepeat:), non-local return (^)

### 07 — Classes & Traits
**File:** `lib/guide/Guide07Classes.mag`
**Sources:** `lib/Printable.mag`, `lib/Result.mag`, `lib/Success.mag`, `lib/Failure.mag`
**Topics:** Class definition syntax, subclassing, instance variables, methods, class methods, traits (definition, inclusion), docstrings

### 08 — Error Handling
**File:** `lib/guide/Guide08ErrorHandling.mag`
**Sources:** `lib/Block.mag` (on:do:, ensure:), `examples/results.mag`
**Topics:** Exception handling (on:do:), ensure:, Result type pattern (Success/Failure), then:/map:/flatMap:, stack overflow protection

### 09 — Concurrency
**File:** `lib/guide/Guide09Concurrency.mag`
**Sources:** `lib/Channel.mag`, `lib/Process.mag`, `lib/Mutex.mag`, `lib/WaitGroup.mag`, `lib/Semaphore.mag`, `lib/CancellationContext.mag` (if exists), `examples/concurrency.mag`
**Topics:** fork/forkWith:, channels (send:/receive, buffered, select:), mutex (lock/unlock/critical:), WaitGroup, Semaphore, CancellationContext (timeouts, child contexts), patterns (producer-consumer, fan-in, worker pool)

### 10 — Modules & Namespaces
**File:** `lib/guide/Guide10Modules.mag`
**Sources:** `CLAUDE.md` (module system section), compiler namespace handling
**Topics:** namespace: declaration, import:, FQN syntax (::), resolution order, directory-as-namespace convention, fileIn/fileOut

### 11 — Project Structure
**File:** `lib/guide/Guide11Projects.mag`
**Sources:** `manifest/manifest.go`, `CLAUDE.md` (manifest + dependency sections)
**Topics:** maggie.toml structure, source dirs, entry points, dependencies (git/path), lock file, mag deps subcommand, dependency namespace mapping

### 12 — Supervisors
**File:** `lib/guide/Guide12Supervisors.mag`
**Topics:** ChildSpec, Supervisor strategies (oneForOne, oneForAll, restForOne), restart policies, DynamicSupervisor

### 13 — Clustering
**File:** `lib/guide/Guide13Clustering.mag`
**Topics:** Cluster seeds, membership events, HashRing, consistent hashing

### 14 — Go Interop
**File:** `lib/guide/Guide14GoInterop.mag`
**Sources:** `gowrap/`, `cmd/mag/wrap.go`, `cmd/mag/build.go`, `vm/go_object.go`
**Topics:** mag wrap, mag build, GoObject wrapper, type marshaling, generated glue code, custom binaries

### 15 — Distributed Runtime
**File:** `lib/guide/Guide15Distribution.mag`
**Sources:** `vm/dist/`, `vm/content_store.go`, `server/`, `cmd/mag/sync.go`, `compiler/hash/`
**Topics:** Content addressing, content hashes, ContentStore, chunks (method/class/module), push/pull, reputation, capabilities, sync configuration

### 16 — Tooling & IDE
**File:** `lib/guide/Guide16Tooling.mag`
**Sources:** `cmd/mag/docgen.go`, `cmd/mag/format.go`, `cmd/mag/doctest.go`, `docs/lsp.md`
**Topics:** mag fmt, mag doc, mag doctest, LSP server, REPL commands, image save/load, Yutani IDE overview

## Implementation: Two Work Streams

### Stream 1: Extend `mag doc` (prerequisite)
- Detect `lib/guide/` classes during doc generation
- Render guide pages with chapter ordering (numeric prefix)
- Add sidebar navigation with "Guide" and "API Reference" sections
- Shared CSS/JS, guide-specific styling for long-form prose

### Stream 2: Write Chapters (parallelizable after Stream 1)
- Each chapter is an independent task
- Agent reads specified source files, writes the `.mag` file, runs `mag doctest` to verify
- Chapters can be written in any order, though 01-06 form a natural progression

## Open Questions

1. **Self-hosting compiler chapter?** The `lib/compiler/` directory contains a full Maggie compiler written in Maggie. Could be an advanced chapter (15) or appendix.
2. **Yutani deep-dive?** The TUI framework (`lib/yutani/`) is substantial. Separate guide or just an overview in the Tooling chapter?
3. **Chapter length target?** Suggest ~500-1000 lines per `.mag` file (prose + code). Too long and the agent context gets strained.
4. **Do guide classes need to avoid polluting the class namespace?** They could use a `Guide::` namespace prefix to stay out of the way.
