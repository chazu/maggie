# Maggie

A late-bound Smalltalk dialect implemented in Go.

**Documentation:**
- [User Guide](docs/USER_GUIDE.md) - Getting started, syntax, core classes
- [Design Document](docs/MAGGIE_DESIGN.md) - Architecture and implementation details

## Status

Phase 3: Image System - Complete

- Core VM with NaN-boxed values, bytecode interpreter
- 16 core classes: Object, Boolean, True, False, UndefinedObject, SmallInteger, Float, String, Symbol, Array, Block, Channel, Process, Result, Success, Failure
- Compiler: Smalltalk-like syntax to bytecode
- Image system: Binary save/load of VM state
- Development tools: Inspector, Debugger

## Quick Start

```bash
# Build the bootstrap image
go run ./cmd/bootstrap/

# Run tests
go test ./...
```

## Project Structure

- `vm/` - Virtual machine implementation (values, classes, interpreter, image I/O)
- `compiler/` - Lexer, parser, and bytecode compiler
- `cmd/bootstrap/` - Bootstrap tool that compiles core library to binary image
- `lib/` - Core class library in Maggie source (`.mag` files)
- `docs/` - Design documentation

## Maggie Source Files (.mag)

Maggie source files use `.mag` extension. Method boundaries are detected automatically by looking for unindented method headers:

```smalltalk
-- This is a comment
factorial
    self = 0 ifTrue: [^1].
    ^self * (self - 1) factorial

even
    ^(self \\ 2) = 0

max: other
    self > other ifTrue: [^self].
    ^other
```

The bootstrap tool compiles these into bytecode and saves them in a binary image (`maggie.image`).
