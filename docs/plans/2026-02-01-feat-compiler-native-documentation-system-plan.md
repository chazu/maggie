---
title: "feat: Compiler-Native Documentation System"
type: feat
date: 2026-02-01
brainstorm: docs/brainstorms/2026-02-01-documentation-system-brainstorm.md
---

# feat: Compiler-Native Documentation System

## Overview

Add first-class docstrings (`"""..."""`) to the Maggie language. Docstrings flow through the entire compiler pipeline — lexer, parser, AST, compiled method, image serialization, runtime reflection — making documentation available everywhere: in the REPL, in IDE tools, in saved images, and on a generated reference site with interactive examples.

## Problem Statement

Maggie has no documentation system. Comments (`#` and `"..."`) are stripped by the lexer and never reach the runtime. There is no way to query documentation from the REPL, inspect it in IDE tools, or generate reference material. This makes the language harder to learn and use.

## Proposed Solution

A nine-layer implementation, broken into four phases:

1. **Core pipeline** (lexer → parser → AST → compiler → image) — docstrings become data
2. **Runtime access** (help messages, REPL commands, reflection primitives, browsing API) — developers can query docs
3. **Doc generator** (static HTML reference site) — publishable documentation
4. **Web server + interactive examples** (HTTP primitives, eval endpoint) — live playground

---

## Technical Approach

### Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Docstring delimiter | `"""..."""` (triple double-quote) | Distinct from `"..."` comments and `#` line comments. No ambiguity. |
| Storage | Compiler-native (on AST, CompiledMethod, Class) | Survives image save/load. Available everywhere at runtime. |
| Executable docs | ` ```test ``` ` with `>>>` assertions, ` ```example ``` ` for playground | Familiar Markdown-inspired syntax. Tests are runnable specs. |
| `>>>` semantics | Parsed by doc test runner only, NOT a Maggie operator | Test block content is raw text split on `>>>`. Each side compiled separately. |
| Runtime API | Both `Class help` message + `:help` REPL command | Smalltalk idiom for programmatic use, convenience command for REPL. |
| Image compatibility | Version 2 reader loads version 1 images (docstrings default to empty) | No breaking change for existing images. |
| Orphan docstrings | Compiler warning, silently discarded | Not an error — doesn't block compilation. |
| Trait docstrings | Yes, `TraitDef` gets `DocString` field | Traits are core abstractions; they should be documentable. |
| Whitespace handling | Strip common leading indent from docstring content | Class-body docstrings are indented; raw content should be clean. |
| `fileOut` | Preserves docstrings | Documentation must survive code export/import cycles. |
| Self-hosting compiler | Deferred to follow-up | Go compiler is primary; self-hosting compiler updated later. |

### Architecture

```
.mag source file
    │
    ▼
┌─────────────────────┐
│ Lexer                │  TOKEN_DOCSTRING for """..."""
│ compiler/lexer.go    │  Intercepts before comment handler
└─────────┬───────────┘
          │
          ▼
┌─────────────────────┐
│ Parser               │  Attaches docstrings to next ClassDef/MethodDef/TraitDef
│ compiler/parser.go   │  Buffers pending docstring, warns on orphans
└─────────┬───────────┘
          │
          ▼
┌─────────────────────┐
│ AST                  │  ClassDef.DocString, MethodDef.DocString, TraitDef.DocString
│ compiler/ast.go      │
└─────────┬───────────┘
          │
          ▼
┌─────────────────────┐
│ Compiler/Codegen     │  Copies docstrings to CompiledMethod.DocString and Class.DocString
│ cmd/mag/main.go      │
└─────────┬───────────┘
          │
          ▼
┌─────────────────────┐
│ Image Writer/Reader  │  Serializes docstrings (flag byte + string table index)
│ vm/image_writer.go   │  Version bump: 1 → 2 (reader accepts both)
│ vm/image_reader.go   │
└─────────┬───────────┘
          │
          ▼
┌─────────────────────┐
│ Runtime              │  Object>>help, Class>>help:, :help REPL command
│ Reflection prims     │  docStringFor:, methodDocFor: primitives
│ Browsing API         │  Populates existing comment field + adds method docstring
└─────────┬───────────┘
          │
          ▼
┌─────────────────────┐
│ Doc Generator        │  Walks classes, parses docstrings, generates HTML
│ cmd/mag/docgen.go    │  Syntax highlighting, cross-references
└─────────┬───────────┘
          │
          ▼
┌─────────────────────┐
│ HTTP Server          │  New Maggie-native HTTP primitives
│ vm/http_primitives   │  Serves static files + eval endpoint
└─────────────────────┘
```

---

## Implementation Phases

### Phase 1: Core Pipeline (Lexer → Parser → AST → Compiler → Image)

This is the foundation. All subsequent features depend on docstrings being stored in the image.

#### 1.1 Lexer: `"""` Tokenization

**Files:** `compiler/token.go`, `compiler/lexer.go`, `compiler/lexer_test.go`

**Changes to `compiler/token.go`:**
- Add `TokenDocstring` to the token type enum (after `TokenString`)
- Add entry in `tokenNames` map

**Changes to `compiler/lexer.go`:**

The key challenge: `"` is currently the comment delimiter. `"""` must be detected BEFORE the comment handler consumes it.

In `skipWhitespaceAndComments()` (line 171), when `l.ch == '"'`:
- Peek at `l.input[l.readPos]` and `l.input[l.readPos+1]` (bounds-checked)
- If both are `"`, this is a triple-quote — **break out of the skip loop** without consuming anything, letting `NextToken()` handle it
- If not, consume as a regular comment (current behavior)

In `NextToken()`, add a new case before the default path:
- When `l.ch == '"'` and the next two characters are also `"`, call `readDocstring(pos)`
- `readDocstring()` consumes the opening `"""`, reads content until closing `"""`, returns `TokenDocstring` with content as `Literal`
- Strip common leading whitespace (dedent) from content
- Handle unterminated docstring (EOF before closing `"""`) as `TokenError`

**Edge cases:**
- `""` (empty comment) — unchanged, handled by comment path (peek finds non-`"`)
- `"""` followed by no closing `"""` — error: unterminated docstring
- `""""""` (six quotes) — empty docstring (open `"""`, immediate close `"""`)
- Docstring content containing `"` or `""` — fine, only `"""` closes

**Tests (`compiler/lexer_test.go`):**
- Basic docstring: `"""Hello world"""`
- Multi-line docstring with content
- Empty docstring: `""""""`
- Docstring with embedded `"` and `""`
- Unterminated docstring (error case)
- Regular `"comment"` still works after change
- Empty comment `""` still works
- Docstring followed by identifier
- Docstring with fenced blocks inside (just lexed as raw text)

#### 1.2 Parser: Docstring Attachment

**Files:** `compiler/ast.go`, `compiler/parser.go`, `compiler/parser_test.go`

**Changes to `compiler/ast.go`:**
- Add `DocString string` field to `MethodDef` (after `SourceText`)
- Add `DocString string` field to `ClassDef` (after `Category`)
- Add `DocString string` field to `TraitDef` (after `Requires`)

**Changes to `compiler/parser.go`:**

In `ParseSourceFile()` (line 795):
- Before the class/trait detection loop, check for `TokenDocstring`
- If found, store as `pendingDocString` variable
- When the next `ClassDef` or `TraitDef` is parsed, attach the pending docstring to it
- If a second docstring is encountered before a class/trait, replace the pending one (last wins) and emit a warning
- If `pendingDocString` is non-empty at end of file with no class/trait following, emit a warning (orphan)

In `parseClassDefBody()` (line 933):
- Add handling for `TokenDocstring` tokens encountered in the class body loop
- Store as `pendingMethodDoc`
- When the next `method:` or `classMethod:` is parsed, attach the pending docstring to the `MethodDef`
- If a docstring is followed by `instanceVars:`, `include:`, or another docstring, warn and discard

In `parseTraitDefBody()` (line 987):
- Same pattern as `parseClassDefBody` — buffer pending docstring, attach to next method

In `parseMethodInBrackets()` (line 1065):
- Accept an optional `docString string` parameter (passed from the caller)
- Set `methodDef.DocString = docString`

**Tests (`compiler/parser_test.go`):**
- Class with docstring
- Method with docstring inside class body
- Class with docstring + methods with docstrings
- Trait with docstring
- Orphan docstring (not followed by class/method) — warning
- Consecutive docstrings (last wins)
- Docstring preservation through `MethodDef.DocString` field
- Class method with docstring

#### 1.3 Compiler: Docstring Storage

**Files:** `vm/compiled_method.go`, `vm/object.go`, `cmd/mag/main.go`

**Changes to `vm/compiled_method.go`:**
- Add `docString string` field (private, following `category` pattern)
- Add `DocString() string` accessor
- Add `SetDocString(s string)` mutator

**Changes to `vm/object.go` (Class struct):**
- Add `DocString string` field

**Changes to `cmd/mag/main.go` (`compileSourceFile`):**
- After line 739 (where `method.Source = methodDef.SourceText`), add:
  ```
  if methodDef.DocString != "" {
      method.SetDocString(methodDef.DocString)
  }
  ```
- During class creation/lookup (around line 699), copy `classDef.DocString` to `class.DocString`

#### 1.4 Image Serialization

**Files:** `vm/image_writer.go`, `vm/image_reader.go`, `vm/image_writer_test.go`, `vm/image_reader_test.go`

**Changes to `vm/image_writer.go`:**
- Bump `ImageVersion` from 1 to 2
- In `writeMethod()` (after source serialization at line 610): add docstring serialization using the same flag-byte + string-index pattern
- In `writeClass()` (after existing class fields): add class docstring serialization

**Changes to `vm/image_reader.go`:**
- In `LoadImage()`: accept both version 1 and version 2
- In `readMethod()`: if version >= 2, read docstring after source; if version 1, set docstring to ""
- In `readClass()`: if version >= 2, read class docstring; if version 1, set docstring to ""

**Binary layout for methods (version 2 additions after existing source block):**
```
[existing source block: hasSource(1 byte) + sourceIdx(4 bytes if present)]
hasDocString  (1 byte: 0 or 1)
docStringIdx  (4 bytes uint32, only if hasDocString == 1)
[existing source map block]
```

**Binary layout for classes (version 2 additions):**
```
[existing class fields]
hasDocString  (1 byte: 0 or 1)
docStringIdx  (4 bytes uint32, only if hasDocString == 1)
```

**Tests:**
- Round-trip: write method with docstring → read back → docstring preserved
- Round-trip: write class with docstring → read back → docstring preserved
- Version 1 image loaded by version 2 reader → docstrings are empty (backward compat)
- Method with no docstring → flag byte is 0, no string index written

#### 1.5 FileOut Preservation

**Files:** `vm/file_out.go`

**Changes to `FileOutClass()`:**
- Before emitting the class definition line, check `class.DocString` — if non-empty, emit `"""` + docstring + `"""`
- Before emitting each method, check `method.DocString()` — if non-empty, emit indented `"""` + docstring + `"""`

---

### Phase 2: Runtime Access

#### 2.1 Reflection Primitives

**Files:** `vm/class_reflection_primitives.go`

**New primitives on Object class:**
- `docString` — returns the class docstring as a Maggie string (or empty string)
- `methodDocFor: aSymbol` — returns the docstring for the named instance method
- `classMethodDocFor: aSymbol` — returns the docstring for the named class method

**New primitive on Object (instance-level):**
- `help` — prints the class docstring to stdout, returns self

**New primitives on Class (class-level):**
- `help` — prints the class docstring to stdout, returns self
- `help: aSymbol` — prints the method docstring to stdout, returns self

Implementation follows the exact pattern of `methodSourceFor:` at line 244: look up the method by selector, access the docstring field, return as a Maggie string value.

#### 2.2 REPL `:help` Command

**Files:** `cmd/mag/main.go`

**Changes to `handleREPLCommand()`:**
- Change from exact string `switch` to prefix-based dispatch
- Add `strings.HasPrefix(cmd, ":help ")` (note trailing space) for the doc lookup command
- Keep bare `:help` as the existing REPL help text

**`:help` argument parsing:**
```
:help                      → existing REPL help (unchanged)
:help Array                → print Array class docstring
:help Array>>at:           → print Array instance method at: docstring
:help Array class>>new:    → print Array class method new: docstring
:help MyApp::Models::User  → namespaced class lookup
```

Parsing logic:
1. Extract argument after `:help `
2. Split on first `>>` — left is class name, right is selector
3. If left contains ` class>>`, it is a class method lookup
4. Look up class via `vm.Classes.Lookup(className)` (with namespace resolution)
5. Look up method via class VTable
6. Print docstring or "No documentation available for ..."

#### 2.3 Browsing API Updates

**Files:** `server/browse_service.go`, `proto/maggie/v1/common.proto`

**Changes to `proto/maggie/v1/common.proto`:**
- Add `string docstring = 7;` field to `MethodInfo` message

**Changes to `server/browse_service.go`:**
- In `GetClass()` handler: populate the existing `resp.Comment` field with `class.DocString`
- In `methodToInfo()` helper: populate the new `info.Docstring` field from `method.DocString()`

**Proto regeneration:** Run `buf generate` (or whatever the project's proto generation command is) after changing `.proto` files.

#### 2.4 LSP Hover Documentation

**Files:** `server/lsp.go`

**Changes to hover handler:**
- When hovering over a class name, include the class docstring in the hover content
- When hovering over a method selector, include the method docstring in the hover content
- Format as Markdown in the hover response

---

### Phase 3: Doc Generator

#### 3.1 Doc Generator Core

**Files:** New file `cmd/mag/docgen.go` (or `docgen/` package)

**CLI integration:**
- Add `mag doc` subcommand (or `mag --doc`) that generates HTML docs
- Flags: `--output <dir>` (default `docs/api/`), `--title <string>`

**Implementation:**
1. Load the image (or compile source files)
2. Walk all classes via `vm.Classes.All()`
3. For each class, collect: name, namespace, superclass, instance vars, docstring, methods with docstrings
4. Parse each docstring to extract: prose sections, `test` blocks, `example` blocks
5. Generate HTML using Go `html/template`

**Docstring parser (internal):**
- Split content on ` ```test ` / ` ```example ` / ` ``` ` fences
- Return structured data: `[]DocSection` where each is `{Type: "prose"|"test"|"example", Content: string}`
- For test blocks, further parse on `>>>` to extract expression/expected pairs

**HTML output structure:**
```
output/
  index.html              # class listing, grouped by namespace
  classes/
    Array.html            # one page per class
    Dictionary.html
    MyApp/
      Models/
        User.html         # namespaced classes in subdirectories
  css/
    style.css
  js/
    highlight.js          # syntax highlighting (client-side)
    playground.js         # "Run" button handler (Phase 4)
```

**Per-class page content:**
- Class name, namespace, superclass chain
- Class docstring (prose)
- Instance variables
- Method listing grouped by category (if categories exist, otherwise alphabetical)
- Each method: selector, docstring, source, test examples, interactive examples

#### 3.2 Docstring Test Runner

**Files:** New file `cmd/mag/doctest.go` (or integrated into `mag doc --test`)

**CLI:** `mag doctest [files...]` or `mag doc --test`

**Implementation:**
1. Walk all methods, extract `test` blocks from docstrings
2. For each `>>>` assertion line:
   - Split on `>>>`
   - Compile left side as a Maggie expression
   - Compile right side as a Maggie expression
   - Evaluate both
   - Compare results (using `=` message or `printString` equality)
3. Report pass/fail counts

---

### Phase 4: Web Server + Interactive Examples

#### 4.1 HTTP Server Primitives

**Files:** New file `vm/http_primitives.go`

**Scope:** Minimal HTTP server sufficient to serve the doc site. NOT a general-purpose HTTP framework (that can come later).

**Maggie API:**
```smalltalk
server := HttpServer new: 8080.
server serveStatic: '/docs' from: './output'.
server route: '/api/eval' method: 'POST' handler: [:req |
    result := Compiler evaluate: req body.
    HttpResponse new: 200 body: result printString
].
server start.
```

**Go implementation:**
- Register `HttpServer` class with primitives wrapping Go's `net/http`
- `new:` — creates server on port
- `serveStatic:from:` — registers `http.FileServer` for a path prefix
- `route:method:handler:` — registers a route with a Maggie block as handler
- `start` — calls `http.ListenAndServe` (blocking, should be forked)
- `HttpRequest` class — wraps request with `body`, `path`, `method`, `header:` accessors
- `HttpResponse` class — wraps response with `new:body:`, `header:value:`, `contentType:`

#### 4.2 Doc Server in Maggie

**Files:** New file `lib/DocServer.mag`

A Maggie program that:
1. Serves the generated HTML from Phase 3
2. Provides a `/api/eval` POST endpoint for running examples
3. Sandboxes eval: timeout (5 seconds), no file access, no network from eval context

#### 4.3 Interactive Examples (Client-Side)

**Files:** JS file in doc generator output (`js/playground.js`)

- "Run" buttons on `example` blocks send POST to `/api/eval`
- Display result inline below the example
- Handle errors gracefully (timeout, syntax errors)
- Loading state while waiting for response

---

## Acceptance Criteria

### Phase 1: Core Pipeline
- [x] `"""Hello"""` lexes as `TokenDocstring` with literal `"Hello"`
- [x] Multi-line docstrings preserve content and strip common indent
- [x] Unterminated docstrings produce lexer error
- [x] Existing `"comments"` and `#` comments are unaffected
- [x] Parser attaches docstrings to ClassDef, MethodDef, TraitDef
- [x] Orphan docstrings produce a compiler warning
- [x] `CompiledMethod.DocString()` returns the docstring after compilation
- [x] `Class.DocString` is populated after compilation
- [x] Image version 2 round-trips docstrings correctly
- [x] Image version 1 loads in version 2 reader with empty docstrings
- [x] `fileOut` preserves docstrings

### Phase 2: Runtime Access
- [x] `Array help` prints the Array class docstring in the REPL
- [x] `Array help: #at:` prints the at: method docstring
- [x] `:help Array` works as a REPL command
- [x] `:help Array>>at:` works for method lookup
- [x] `:help NonexistentClass` prints "No documentation available"
- [x] Browsing API `GetClass` returns class docstring in `comment` field
- [x] Browsing API `GetMethod` returns method docstring in new `docstring` field
- [x] LSP hover shows docstrings

### Phase 3: Doc Generator
- [x] `mag doc --output ./site` generates HTML reference
- [x] Each class has its own page with docstring, methods, examples
- [x] Index page lists all classes grouped by namespace
- [x] `mag doctest` runs all `>>>` assertions and reports results
- [x] Test failures produce clear error messages with file/method context

### Phase 4: Web Server + Interactive
- [ ] `HttpServer` class can serve static files and handle routes
- [ ] Doc server starts and serves generated HTML
- [ ] "Run" buttons execute examples and display results
- [ ] Eval endpoint has timeout protection (5 seconds)

---

## Dependencies & Risks

### Dependencies
- Phase 2 depends on Phase 1 (docstrings must be stored before they can be queried)
- Phase 3 depends on Phase 2 (doc generator uses the same reflection primitives)
- Phase 4 depends on Phase 3 (web server serves generated HTML)

### Risks
- **Lexer `"` disambiguation** — The `"""` vs `"..."` detection is the trickiest part. Thorough lexer tests are critical.
- **Image version bump** — Must handle backward compatibility or users lose existing saved images.
- **HTTP primitives scope creep** — Keep it minimal for docs. General-purpose HTTP is a separate feature.
- **Security of eval endpoint** — Unrestricted eval is an RCE vector. Must sandbox with timeouts and restricted capabilities.

---

## Open Questions (Deferred)

These are explicitly deferred to future iterations:

1. **WASM compilation** — Client-side example execution via WASM (alternative to server-side eval)
2. **Doc inheritance** — Subclasses inheriting superclass docstrings for undocumented methods
3. **Category grouping** — Metadata in docstrings for organizing methods by protocol/category
4. **Multi-version docs** — Serving documentation for different Maggie versions
5. **Self-hosting compiler** — Updating the Maggie-in-Maggie compiler's lexer for `"""`
6. **General-purpose HTTP** — Full HTTP client/server beyond doc serving needs

---

## References

### Internal
- Brainstorm: `docs/brainstorms/2026-02-01-documentation-system-brainstorm.md`
- Lexer: `compiler/lexer.go:162` (comment handling), `compiler/token.go:12` (token types)
- Parser: `compiler/parser.go:795` (ParseSourceFile), `:910` (parseClassDefBody), `:1065` (parseMethodInBrackets)
- AST: `compiler/ast.go:292` (MethodDef), `:306` (ClassDef), `:323` (TraitDef)
- CompiledMethod: `vm/compiled_method.go:9`
- Class struct: `vm/object.go:52`
- Image writer: `vm/image_writer.go:535` (writeMethod), `:423` (writeClass)
- Image reader: `vm/image_reader.go:549` (readMethod)
- Reflection primitives: `vm/class_reflection_primitives.go:244` (methodSourceFor: pattern)
- FileOut: `vm/file_out.go:43`
- REPL: `cmd/mag/main.go:308` (runREPL), `:373` (handleREPLCommand)
- Browsing API: `server/browse_service.go:51` (GetClass), `proto/maggie/v1/browsing.proto:57` (unused comment field)
- LSP: `server/lsp.go`
- Design doc: `docs/MAGGIE_DESIGN.md`
