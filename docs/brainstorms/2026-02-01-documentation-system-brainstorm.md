# Maggie Documentation System

**Date:** 2026-02-01
**Status:** Brainstorm complete, ready for planning

## What We're Building

A compiler-native documentation system for Maggie where:

1. **Docstrings** (`"""..."""`) are a first-class part of the language — captured by the lexer, stored on AST nodes, compiled into method/class metadata, serialized in images, and accessible at runtime.

2. **Executable examples** live inside docstrings using fenced code blocks:
   - ` ```test ``` ` — spec tests with `>>>` assertion syntax (e.g., `3 + 4 >>> 7`)
   - ` ```example ``` ` — runnable snippets that become interactive playground examples on the web

3. **REPL access** via both `Class help` / `Class help: #method` messages and a `:help ClassName` REPL command.

4. **Generated reference site** — a static HTML site auto-generated from docstrings, served by a web server written in Maggie. Interactive examples run live.

## Doc Syntax

```smalltalk
"""
Array is an ordered, indexable collection of objects.
Arrays have a fixed size set at creation time.

```test
(Array new: 3) size >>> 3
(Array with: 1 with: 2) first >>> 1
```

```example
arr := Array new: 5.
arr at: 1 put: 'hello'.
arr at: 2 put: 'world'.
arr collect: [:each | each size]
```
"""
Array subclass: SequenceableCollection
  instanceVars: storage

  """
  Returns the element at the given index.
  Raises an error if the index is out of bounds.

  ```test
  #(10 20 30) at: 2 >>> 20
  ```
  """
  method: at: index [
      ^storage at: index
  ]
```

### Rules

- `"""..."""` (triple double-quote) delimits docstrings
- `#` comments remain as-is (informal, not captured)
- `"..."` single double-quote comments remain as-is (informal, not captured)
- A docstring immediately before a class definition documents the class
- A docstring immediately before a `method:` or `classMethod:` documents that method
- Inside docstrings, ` ```test ``` ` blocks contain assertions using `>>>` (expression >>> expected-result)
- Inside docstrings, ` ```example ``` ` blocks contain runnable code for the web docs playground

## Architecture

### Pipeline

```
.mag source
    |
    v
Lexer (new TOKEN_DOCSTRING for """)
    |
    v
Parser (attaches docstrings to ClassDef / MethodDef AST nodes)
    |
    v
Compiler (stores docstring on CompiledMethod.DocString and class metadata)
    |
    v
Image Writer (serializes docstrings alongside method source)
    |
    v
Runtime (reflection API: help messages, :help REPL command)
    |
    v
Doc Generator (walks classes/methods, parses docstring fenced blocks)
    |
    v
Static HTML site (reference docs with live examples)
    |
    v
Maggie Web Server (serves the site, runs examples server-side or via WASM)
```

### Layer Details

**Lexer changes:**
- New token type `TOKEN_DOCSTRING`
- `"""` opens, next `"""` closes
- Content between delimiters is the token value (raw string, preserving newlines)

**Parser changes:**
- When a `TOKEN_DOCSTRING` is encountered at class body level or file level, attach it to the next class/method definition
- New fields: `ClassDef.DocString`, `MethodDef.DocString`

**CompiledMethod changes:**
- New `DocString string` field (alongside existing `Source`)

**Class metadata changes:**
- Store docstring on the class object (accessible via reflection)

**Image format changes:**
- Extend method serialization to include docstring (similar to how Source is already serialized)
- Extend class serialization to include class-level docstring

**Runtime API:**
- `Object>>help` — prints docstring for the receiver's class
- `Class>>help` — prints the class docstring
- `Class>>help: aSymbol` — prints docstring for the named method
- `:help ClassName` REPL command — convenience wrapper
- `:help ClassName>>methodName` REPL command variant

**Doc generator:**
- Walks all classes in the image (or source files)
- Parses docstrings: extracts prose, test blocks, example blocks
- Generates static HTML with syntax highlighting
- Test blocks rendered as code + expected output
- Example blocks rendered with a "Run" button for interactivity

**Web server (Maggie-native):**
- Serves static HTML files
- Provides an eval endpoint for running example code
- Requires HTTP server primitives (new capability for Maggie)

## Key Decisions

1. **Triple-quote syntax (`"""`)** — distinct from existing `"..."` comments and `#` comments. No ambiguity.
2. **Compiler-native** — docstrings flow through the full pipeline (lexer -> image -> runtime). Not a sidecar tool.
3. **Fenced blocks inside docstrings** — Markdown-inspired ` ```test ``` ` and ` ```example ``` ` for structured content.
4. **`>>>` assertion syntax** — readable, distinct from any Maggie operator, familiar from Python doctests.
5. **API reference first** — initial web site is auto-generated reference. Guides and tutorials come later.
6. **Both message and REPL command** — `Class help` as a Smalltalk message + `:help` as a REPL convenience.

## Open Questions

1. **HTTP primitives scope** — What HTTP server capabilities does Maggie need? Minimal (serve static + one eval endpoint) or general-purpose?
2. **WASM vs server-side eval** — Should interactive examples run client-side (WASM compilation of Maggie) or server-side (POST to eval endpoint)?
3. **Doc inheritance** — Should subclasses inherit superclass docstrings for undocumented methods?
4. **Category/protocol grouping** — Should docstrings support a way to declare method categories for organizing the reference?
5. **Versioning** — How to handle docs for different Maggie versions on the static site?

## Implementation Order (suggested)

1. Lexer: `"""` tokenization
2. Parser: docstring attachment to AST nodes
3. Compiler: docstring storage on CompiledMethod and class metadata
4. Image: docstring serialization
5. Runtime: `help` message and `:help` REPL command
6. Doc generator: extract and render to HTML
7. HTTP primitives for Maggie
8. Web server in Maggie serving the doc site
9. Interactive examples (eval endpoint or WASM)
