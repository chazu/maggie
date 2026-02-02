---
title: "Compiler-Native Docstring System (Triple-Quote)"
category: language-features
tags:
  - compiler
  - lexer
  - parser
  - ast
  - image-format
  - docstrings
  - reflection
  - runtime
  - repl
  - browsing-api
  - fileout
  - serialization
  - backward-compatibility
module: compiler/vm
severity: high
date: 2026-02-01
symptoms:
  - "No way to document classes or methods in Maggie source"
  - "Comments stripped by lexer, never reach runtime"
  - "No help command in REPL"
  - "Browsing API returns empty comment field for classes"
  - "No docstring reflection primitives"
  - "fileOut loses all documentation"
---

# Compiler-Native Docstring System (Triple-Quote)

## Problem

Maggie had no documentation system. Comments (`#` line comments and `"..."` inline comments) were stripped by the lexer and never reached the runtime. There was no way to:

- Attach documentation to classes, methods, or traits
- Query documentation from the REPL
- Inspect documentation via IDE tools or the browsing API
- Preserve documentation through image save/load cycles
- Reconstruct documentation via `fileOut`

## Solution Overview

Added `"""..."""` triple-quote docstrings as a first-class language construct that flows through the entire compiler pipeline:

```
Source → Lexer (tokenize) → Parser (attach to AST) → Compiler (store on CompiledMethod/Class)
  → Image Writer (serialize) → Image Reader (deserialize) → Runtime (reflection + REPL)
```

## Key Technical Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Delimiter | `"""..."""` | Distinct from `"..."` comments and `#` line comments |
| Lexer strategy | Peek-ahead in `skipWhitespaceAndComments` | Must detect `"""` before comment handler consumes first `"` |
| Parser pattern | Pending docstring buffer | Simple, unambiguous; docstring attaches to next definition |
| Image compat | Version 1 → 2, reader accepts both | No breaking change for existing images |
| Serialization | Flag byte + string table index | Same pattern as method source; deduplication via string table |
| Whitespace | Strip common leading indent | Class-body docstrings are indented; content should be clean |

## Implementation Details

### Layer 1: Lexer (`compiler/lexer.go`, `compiler/token.go`)

Added `TokenDocstring` token type. The critical challenge is disambiguating `"""` from `"..."` comments.

In `skipWhitespaceAndComments()`, when `l.ch == '"'`, peek ahead two characters. If both are `"`, break out of the skip loop without consuming — let `NextToken()` handle the triple-quote. Otherwise, consume as a regular comment.

In `NextToken()`, a new case detects triple-quote and calls `readDocstring()`, which consumes content until closing `"""` and applies `dedentDocstring()` to strip common leading whitespace.

**Gotcha:** The peek-ahead must happen *inside* the whitespace/comment skip loop, before the comment handler runs. If the comment handler sees the first `"`, it will consume through the second `"` and break the triple-quote detection.

### Layer 2: Parser (`compiler/parser.go`, `compiler/ast.go`)

Added `DocString string` fields to `ClassDef`, `MethodDef`, and `TraitDef` AST nodes.

Parser uses a **pending docstring buffer** pattern:

```go
var pendingDocString string
for !p.curTokenIs(TokenEOF) {
    if p.curTokenIs(TokenDocstring) {
        pendingDocString = p.curToken.Literal
        p.nextToken()
        continue
    }
    // When next class/method/trait is parsed:
    classDef.DocString = pendingDocString
    pendingDocString = ""
}
```

This pattern repeats in `ParseSourceFile()` (for class/trait docstrings) and `parseClassDefBody()` / `parseTraitDefBody()` (for method docstrings).

### Layer 3: Compiler (`vm/compiled_method.go`, `vm/object.go`)

- `CompiledMethod`: private `docString string` field with `DocString()` getter, `SetDocString()` setter, and builder support via `CompiledMethodBuilder.SetDocString()`
- `Class`: public `DocString string` field
- `Trait`: public `DocString string` field

Wiring in `cmd/mag/main.go` and `cmd/bootstrap/main.go` copies docstrings from AST to compiled objects during compilation.

### Layer 4: Image Serialization (`vm/image_writer.go`, `vm/image_reader.go`)

Bumped `ImageVersion` from 1 to 2. Both methods and classes serialize docstrings using:

```
hasDocString  (1 byte: 0 or 1)
docStringIdx  (4 bytes uint32, only if hasDocString == 1)
```

The reader checks `ir.header.Version >= 2` before attempting to read docstring fields. Version 1 images load with empty docstrings.

**Gotcha:** After bumping ImageVersion, all manually-constructed test images in `image_reader_test.go` needed docstring flag bytes added (8 class definitions + 2 method definitions). The tests build raw byte images, so they must match the exact binary format.

### Layer 5: FileOut (`vm/file_out.go`)

`FileOutClass()` emits class docstrings before the class header and method docstrings before each method's source text:

```smalltalk
"""
A documented class.
"""
MyClass subclass: Object
  """
  A documented method.
  """
  method: greet: name [ ^'Hello, ', name ]
```

### Layer 6: Runtime (`vm/docstring_primitives.go`, `cmd/mag/main.go`)

**Reflection primitives** on Object (instance + class side):
- `help` — prints class help to stdout, returns receiver
- `docString` — returns class docstring as string (or nil)
- `help: aSymbol` — prints method docstring (class-side only)
- `methodDocFor: aSymbol` — returns method docstring as string (class-side only)

**REPL command** `:help` with argument parsing:
- `:help` — existing REPL help text
- `:help ClassName` — class docstring + method list
- `:help ClassName>>methodName` — specific method docstring

**Browsing API** (`server/browse_service.go`, `proto/maggie/v1/common.proto`):
- `GetClassResponse.Comment` populated from `cls.DocString`
- `MethodInfo.doc_string` field added and populated from `cm.DocString()`

### Layer 7: Formatting Helpers

`FormatClassHelp(cls, selectors)` produces human-readable output:

```
Point (subclass of Object)

A 2D point with x and y coordinates.

Instance methods:
  x
  y
  distanceTo:
```

`formatMethodHelp(className, cm)` produces:

```
Point>>distanceTo:

Computes the Euclidean distance to another point.
```

## Tests

- **Lexer**: 10 tests covering basic, multi-line, empty, embedded quotes, unterminated, and interaction with regular comments
- **Parser**: 6 tests covering class, method, class+methods, class methods, traits, and absent docstrings
- **VM**: 17 tests in `vm/docstring_test.go` covering CompiledMethod docstring get/set, Class docstring, FormatClassHelp (with/without), formatMethodHelp (with/without), FileOut (class/method/class-method/none), image round-trip (class, method, empty, multiline)

## Prevention / Best Practices

1. **Image version bumps require test updates.** Any manually-constructed test images must include new fields. Grep for `writeHeader` in test files to find them all.
2. **Lexer disambiguation is fragile.** Any future changes to comment handling must preserve the triple-quote peek-ahead in `skipWhitespaceAndComments()`. Test with both `"regular comment"` and `"""docstring"""` in the same source file.
3. **Pending docstring buffer must be cleared.** After attaching to a definition, always reset `pendingDocString = ""`. Forgetting this causes docstrings to "leak" to subsequent definitions.

## What Was Deferred

- **LSP hover**: Docstrings accessible at runtime but not yet wired to LSP hover provider
- **Orphan docstring warnings**: Parser does not warn when a docstring is not followed by a definition
- **Phase 3**: Doc generator (`mag doc` subcommand, HTML output, `>>>` test runner)
- **Phase 4**: HTTP server primitives, interactive playground, doc server in Maggie

## Related Files

- Brainstorm: `docs/brainstorms/2026-02-01-documentation-system-brainstorm.md`
- Plan: `docs/plans/2026-02-01-feat-compiler-native-documentation-system-plan.md`
- Lexer: `compiler/lexer.go`, `compiler/token.go`
- Parser: `compiler/parser.go`, `compiler/ast.go`
- Compiler: `vm/compiled_method.go`, `vm/object.go`, `vm/trait.go`
- Image: `vm/image_writer.go`, `vm/image_reader.go`
- FileOut: `vm/file_out.go`
- Runtime: `vm/docstring_primitives.go`
- Tests: `vm/docstring_test.go`, `compiler/lexer_test.go`, `compiler/parser_test.go`
- REPL: `cmd/mag/main.go`
- Browsing API: `server/browse_service.go`, `proto/maggie/v1/common.proto`
