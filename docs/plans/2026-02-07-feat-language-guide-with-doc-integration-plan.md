---
title: "feat: Language guide integrated into mag doc"
type: feat
date: 2026-02-07
brainstorm: docs/brainstorms/2026-02-07-language-guide-brainstorm.md
---

# Language Guide Integrated into `mag doc`

## Overview

A 14-chapter practical guide to the Maggie language, written as `.mag` source files in `lib/guide/`. Each chapter is a Maggie class whose docstrings contain prose and runnable `>>>` assertions validated by `mag doctest`. The `mag doc` generator is extended with sidebar navigation and a guide-specific template so guide pages render alongside the existing API reference.

## Problem Statement

Maggie lacks a structured, validated tutorial for mainstream developers. The existing `docs/USER_GUIDE.md` is a static Markdown file that drifts from the actual implementation. Documentation examples cannot be tested, and there is no integration with the `mag doc` HTML system.

## Proposed Solution

**Guide-as-code**: chapters are `.mag` files that compile and whose doctests run, eliminating stale examples. The doc generator renders them as long-form prose pages with sidebar navigation.

## Technical Approach

### Design Decisions (from brainstorm)

1. **Guide class detection**: Classes whose bare name matches `^Guide\d{2}` are guide classes. No namespace needed — they live in the root namespace like `lib/compiler/` classes.
2. **Guide page titles**: Extracted from the first `# Heading` line in the class docstring. Fallback: strip `Guide\d+` prefix and insert spaces before uppercase letters.
3. **Method ordering on guide pages**: Methods use selector names with numeric prefixes for ordering (e.g., `s01_intro`, `s02_variables`). The guide template sorts by selector name (which is already alphabetical for this naming convention).
4. **Cross-references**: Minimal — auto-link bare class names in prose to their API pages. Full Markdown support in docstrings is out of scope; rely on sidebar navigation for cross-chapter links.
5. **Guide output path**: `docs/api/guide/01-getting-started.html` etc., separate from `classes/`.
6. **Sidebar**: Embedded in each HTML page (no shared JS). Two sections: "Guide" (always expanded, ordered by chapter number) and "API Reference" (namespace groups, collapsible).

### Architecture

```
lib/guide/
  Guide01GettingStarted.mag     ← chapter .mag files (Stream 2)
  Guide02Basics.mag
  ...

cmd/mag/docgen.go               ← extended (Stream 1)
  - isGuideClass()              ← detection: name matches ^Guide\d{2}
  - guideDoc struct             ← chapter number, title, slug, classDoc
  - guideTemplate               ← prose-first layout, method sections
  - sidebarData struct          ← guide entries + API namespace groups
  - All templates get sidebar   ← index, class, guide pages

cmd/mag/doctest.go              ← fix: also test class-level docstrings
cmd/bootstrap/main.go           ← add lib/guide/*.mag glob
```

### Implementation Phases

#### Phase 1: Foundation (prerequisite for all other phases)

These changes unblock guide chapter writing and doc generation.

##### 1a. Fix class docstring doctest gap

**File:** `cmd/mag/doctest.go`

The `collectAndRunDoctests` function (line 118) only walks method docstrings. It never checks `cls.DocString`. This is a pre-existing bug — e.g., `Array.mag` has `(Array new: 3) size >>> 3` in its class docstring that is never tested.

- [x] In `collectAndRunDoctests`, after the class filter check (line 128), parse `cls.DocString` for `DocTest` sections
- [x] Run any `>>>` assertions found, grouping results under `ClassName` / `(class docstring)` selector
- [x] Verify existing lib class docstring tests pass (especially Array, Dictionary)

##### 1b. Extend bootstrap to compile guide files

**File:** `cmd/bootstrap/main.go`

- [x] Add a third glob for `lib/guide/*.mag` (lines 58-64 show the pattern for `lib/compiler/`)
- [x] Append guide files after compiler files in `allFiles`
- [x] Rebuild image: `go run cmd/bootstrap/main.go -lib lib -o cmd/mag/maggie.image -v`
- [x] Copy to `maggie.image` at root (or wherever the embedded image lives)

##### 1c. Create `lib/guide/` directory and pilot chapter

**File:** `lib/guide/Guide01GettingStarted.mag`

Write a minimal pilot chapter (~50 lines) to validate the full pipeline:

```smalltalk
"""
# Getting Started

Maggie is a modern Smalltalk-family language built on a Go runtime.

```test
3 + 4 >>> 7
```
"""
Guide01GettingStarted subclass: Object

"""
## Running the REPL

Start an interactive session with `mag -i`.

```test
'hello' , ' world' >>> 'hello world'
```
"""
method: s01_repl [
    'hello' , ' world'
]
```

- [x] Verify `mag doctest --class Guide01GettingStarted` passes (both class and method doctests)
- [x] Verify the class appears in `vmInst.Classes.All()`

#### Phase 2: Doc Generator — Guide Detection and Sidebar

**File:** `cmd/mag/docgen.go`

##### 2a. Guide class detection and data model

- [x] Add `isGuideClass(name string) bool` — returns true if name matches `^Guide\d{2}`
- [ ] Add `guideDoc` struct:

```go
type guideDoc struct {
    Number  int      // extracted from class name (e.g., 1 from Guide01)
    Title   string   // from first # heading in class docstring
    Slug    string   // kebab-case for filename (e.g., "01-getting-started")
    Class   classDoc // reuse existing classDoc
    RelPath string   // "guide/01-getting-started.html"
}
```

- [x] Add `extractGuideTitle(docString string) string` — find first `# ` line, fallback to name munging
- [x] Add `extractGuideNumber(name string) int` — parse digits after "Guide"
- [x] Add `guideSlug(number int, title string) string` — e.g., `"01-getting-started"`

##### 2b. Sidebar data structure

- [ ] Add `sidebarData` struct:

```go
type sidebarData struct {
    GuideEntries []sidebarGuideEntry
    APIGroups    []namespaceGroup
}

type sidebarGuideEntry struct {
    Number  int
    Title   string
    RelPath string
}
```

- [x] In `handleDocCommand`, after collecting all `classDocs`, partition into guide docs and API docs
- [x] Build `sidebarData` from both sets
- [x] Pass `sidebarData` to all page-writing functions

##### 2c. Guide page template

- [x] Add `guideTemplate` — a new HTML template for guide pages:
  - Sidebar (shared)
  - Class docstring rendered as main content (prose sections)
  - Each method rendered as a subsection: method docstring as prose, method body as a code example block
  - Method subsections ordered by selector name (numeric prefix convention)
  - No superclass chain, no instance vars, no "Source" collapse — this is prose, not API reference
  - Playground JS included for `example` blocks

##### 2d. Sidebar in all templates

- [x] Modify `indexTemplate` to include sidebar
- [x] Modify `classTemplate` to include sidebar with active-page highlighting
- [x] Create shared sidebar HTML (can be a Go template `define` block or a helper function)
- [x] Active page: the current page's entry in the sidebar gets a CSS class `active`

##### 2e. CSS updates

- [x] Change body layout from centered `max-width: 960px` to flex layout: sidebar (250px fixed) + content (fluid)
- [x] Sidebar styling: fixed position or sticky, scrollable, collapsible namespace groups
- [x] Guide page prose styling: wider line length for readability, paragraph spacing
- [x] Responsive: hide sidebar on narrow screens, add hamburger toggle
- [x] Active page highlight in sidebar

##### 2f. Output directory and writing

- [x] Create `docs/api/guide/` directory
- [x] `writeGuidePage(outputDir, siteTitle string, guide guideDoc, sidebar sidebarData) error`
- [x] Guide pages exclude from API index; API index page links to guide index
- [x] Guide index: either the first guide page or a dedicated `guide/index.html`

##### 2g. Verify end-to-end

- [ ] Run `mag doc` — pilot chapter appears in guide section of sidebar
- [ ] Open generated HTML — sidebar navigates between guide and API pages
- [ ] Run `mag doc --serve` — playground "Run" buttons work on guide examples
- [ ] Relative paths work from guide pages to CSS/JS and from sidebar links

#### Phase 3: Write Guide Chapters (parallelizable)

Each chapter is an independent task. An agent writing a chapter must:

1. Read the source files listed in the spec below
2. Write the `.mag` file following the pilot chapter's structure
3. Run `mag doctest --class GuideNN<Topic>` to verify all assertions pass
4. Keep chapters to ~500-800 lines (prose + code)

**Method naming convention**: `s01_sectionName`, `s02_sectionName`, etc. for ordered subsections.

**Chapter specs:**

##### Ch 01 — Getting Started (pilot, done in Phase 1c)
`lib/guide/Guide01GettingStarted.mag`
**Read:** `cmd/mag/main.go` (CLI flags/subcommands), `README.md`
**Cover:** Installation, `mag -i` REPL, loading files, CLI overview, `.maggierc`

##### Ch 02 — Language Basics
`lib/guide/Guide02Basics.mag`
**Read:** `examples/hello.mag`, `lib/Object.mag`
**Cover:** Expressions, message types (unary `size`, binary `+ 3`, keyword `at: 0`), cascades (`;`), variables/assignment (`:=`), comments (`"..."`), nil/true/false, identity (`==`) vs equality (`=`), self/super

##### Ch 03 — Numbers & Math
`lib/guide/Guide03Numbers.mag`
**Read:** `lib/SmallInteger.mag`, `lib/Float.mag`
**Cover:** SmallInteger, Float, arithmetic, comparison, iteration (`timesRepeat:`, `to:do:`, `to:by:do:`), math methods (`factorial`, `gcd:`, `lcm:`), type testing (`isZero`, `isPositive`)

##### Ch 04 — Strings & Symbols
`lib/guide/Guide04Strings.mag`
**Read:** `lib/String.mag`, `lib/Symbol.mag`, `lib/Character.mag`
**Cover:** String literals, concatenation (`,`), `size`, `at:`, `copyFrom:to:`, `includes:`, `indexOf:`, case conversion, `split:`, character literals (`$a`), symbols (`#foo`), identity comparison for symbols

##### Ch 05 — Collections
`lib/guide/Guide05Collections.mag`
**Read:** `lib/Array.mag`, `lib/Dictionary.mag`, `examples/collections.mag`
**Cover:** Array literals (`#(1 2 3)`), `new:`, `at:`, `at:put:`, iteration (`do:`, `collect:`, `select:`, `reject:`, `detect:`, `inject:into:`), `includes:`, concatenation, Dictionary (`at:`, `at:put:`, `keys`, `values`, `keysAndValuesDo:`), 0-based indexing

##### Ch 06 — Blocks & Control Flow
`lib/guide/Guide06Blocks.mag`
**Read:** `lib/Block.mag`, `lib/Boolean.mag`, `lib/True.mag`, `lib/False.mag`, `examples/blocks.mag`
**Cover:** Block syntax `[...]`, arguments `[:x | ...]`, `value`/`value:`, closures, conditionals (`ifTrue:ifFalse:`), loops (`whileTrue:`, `whileFalse:`), non-local return (`^`), `ensure:`

##### Ch 07 — Classes & Traits
`lib/guide/Guide07Classes.mag`
**Read:** `lib/Printable.mag`, `lib/Result.mag`, `lib/Success.mag`, `lib/Failure.mag`
**Cover:** `subclass:` syntax, `instanceVars:`, `method:`, `classMethod:`, traits (`trait` definition, `include:`), docstrings (`"""`), class hierarchy, method categories

##### Ch 08 — Error Handling
`lib/guide/Guide08ErrorHandling.mag`
**Read:** `lib/Block.mag` (`on:do:`, `ensure:`), `examples/results.mag`
**Cover:** `on:do:` exception handling, `ensure:` cleanup, `StackOverflow` exception, Result pattern (`Success`/`Failure`), `then:`/`map:`/`flatMap:`, safe division example

##### Ch 09 — Concurrency
`lib/guide/Guide09Concurrency.mag`
**Read:** `lib/Channel.mag`, `lib/Process.mag`, `lib/Mutex.mag`, `lib/WaitGroup.mag`, `lib/Semaphore.mag`, `examples/concurrency.mag`
**Cover:** `fork`/`forkWith:`, `wait`, channels (`send:`/`receive`, buffered `new:`, `select:`), mutex (`lock`/`unlock`/`critical:`), WaitGroup (`add:`/`done`/`wait`/`wrap:`), Semaphore, CancellationContext (timeouts, child contexts), patterns (producer-consumer, fan-in)

##### Ch 10 — Modules & Namespaces
`lib/guide/Guide10Modules.mag`
**Read:** `CLAUDE.md` (module system section)
**Cover:** `namespace:` declaration, `import:`, FQN syntax (`Widgets::Button`), resolution order, directory-as-namespace convention, `fileIn:`/`fileOut:to:`/`fileOutNamespace:to:`

##### Ch 11 — Project Structure
`lib/guide/Guide11Projects.mag`
**Read:** `manifest/manifest.go`, `CLAUDE.md` (manifest + dependency sections)
**Cover:** `maggie.toml` format (`[project]`, `[source]`, `[dependencies]`, `[image]`), `mag deps`/`deps update`/`deps list`, git and path dependencies, lock file, dependency namespace mapping, reserved namespaces

##### Ch 12 — Go Interop
`lib/guide/Guide12GoInterop.mag`
**Read:** `gowrap/gen_go.go`, `gowrap/gen_mag.go`, `cmd/mag/wrap.go`, `cmd/mag/build.go`, `vm/go_object.go`
**Cover:** `mag wrap` (introspect Go packages, generate bindings), `mag build` (compile custom binary), GoObject wrapper, type marshaling (Go↔Maggie), generated glue code walkthrough, `[gowrap]` manifest config

##### Ch 13 — Distributed Runtime
`lib/guide/Guide13Distribution.mag`
**Read:** `vm/dist/chunk.go`, `vm/dist/wire.go`, `vm/content_store.go`, `server/server.go`, `cmd/mag/sync.go`, `compiler/hash/nodes.go`
**Cover:** Content addressing (method hashing, class digests), ContentStore, chunks (method/class/module), `mag sync push`/`pull`/`status`, reputation & capabilities, `[sync]` manifest config, peer networking

##### Ch 14 — Tooling & IDE
`lib/guide/Guide14Tooling.mag`
**Read:** `cmd/mag/format.go`, `cmd/mag/docgen.go`, `cmd/mag/doctest.go`, `docs/lsp.md`
**Cover:** `mag fmt` (formatting), `mag doc` (HTML generation), `mag doctest` (testing examples), `saveImage:`/`--image` (image persistence), LSP server (`--lsp`), REPL commands (`:help`, `:use-go`/`:use-maggie`), Yutani IDE overview (`--yutani`)

#### Phase 4: Polish

- [x] Update `README.md` to mention the guide and link to it
- [x] Remove or add deprecation note to `docs/USER_GUIDE.md`
- [ ] Add `mag doc --no-guide` flag (nice-to-have, skip if unnecessary)
- [ ] Review all generated guide pages for visual consistency
- [x] Run full `mag doctest` to verify all 14 chapters pass (807/807)

## Acceptance Criteria

### Functional Requirements

- [ ] `mag doc` generates guide pages alongside API reference
- [ ] Sidebar navigation appears on all pages (guide, API, index)
- [ ] Guide chapters are ordered by numeric prefix in sidebar
- [ ] `mag doctest` tests assertions in both class docstrings and method docstrings
- [ ] All 14 guide chapters compile and their doctests pass
- [ ] `mag doc --serve` serves guide pages with working playground

### Non-Functional Requirements

- [ ] Guide chapters are independently authorable (no cross-chapter dependencies in doctests)
- [ ] Each chapter is 500-800 lines (manageable for agent context)
- [ ] Sidebar is responsive (usable on mobile)
- [ ] Generated HTML works as static files (no server required for reading)

### Quality Gates

- [ ] `go test ./cmd/mag/...` passes (existing tests not broken)
- [ ] `go test ./cmd/bootstrap/...` passes
- [ ] `mag doctest` passes with 0 failures
- [ ] Pilot chapter validates full pipeline before remaining chapters are written

## Dependencies & Prerequisites

- Phase 2 (doc generator) depends on Phase 1 (foundation)
- Phase 3 (chapters) depends on Phase 1c (pilot chapter validates the structure) and Phase 2 (for visual verification)
- Phase 3 chapters are independent of each other — parallelizable
- No external dependencies

## Risk Analysis & Mitigation

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Guide docstrings are too large for image | Low | Medium | Monitor image size; can lazy-load guide from source if needed |
| Method ordering convention is confusing | Medium | Low | Document clearly; pilot chapter sets the example |
| Sidebar layout breaks existing API pages | Medium | High | Phase 2 is one atomic change; test existing pages before/after |
| Agent hallucination in chapter content | High | Medium | Source-verified specs + mandatory doctest validation |
| CSS responsiveness is tricky | Medium | Low | Start with desktop-only sidebar; responsive polish in Phase 4 |

## References

### Internal
- Brainstorm: `docs/brainstorms/2026-02-07-language-guide-brainstorm.md`
- Docstring pipeline: `docs/solutions/language-features/compiler-native-docstrings.md`
- Doc generator: `cmd/mag/docgen.go`
- Doctest runner: `cmd/mag/doctest.go`
- Bootstrap: `cmd/bootstrap/main.go`
- Existing guide: `docs/USER_GUIDE.md`

### Key Source Files per Chapter
See Phase 3 chapter specs above — each chapter lists the exact files the writing agent must read.
