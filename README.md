# Maggie

A late-bound Smalltalk dialect implemented in Go.

**Documentation:**
- [User Guide](docs/USER_GUIDE.md) - Getting started, syntax, core classes
- [Design Document](docs/MAGGIE_DESIGN.md) - Architecture and implementation details

## Status

Phase 4: JIT Compilation - In Progress

- Core VM with NaN-boxed values, bytecode interpreter
- 16 core classes: Object, Boolean, True, False, UndefinedObject, SmallInteger, Float, String, Symbol, Array, Block, Channel, Process, Result, Success, Failure
- Compiler: Smalltalk-like syntax to bytecode
- Image system: Binary save/load of VM state
- Development tools: Inspector, Debugger
- JIT: Inline caching, method profiling, adaptive AOT compilation
- Persistence: Static build, plugin loading, image-embedded AOT

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

## JIT Compilation

Maggie includes an adaptive JIT compilation system that automatically compiles frequently-executed methods to optimized Go code. This follows traditional Smalltalk VM patterns (similar to Cog VM).

### Enabling JIT

```go
vm := vm.NewVM()
jit := vm.EnableJIT()

// Optional configuration
jit.LogCompilation = true  // Log when methods are compiled
```

### How It Works

1. **Profiling**: The VM tracks method invocation counts
2. **Hot Detection**: Methods exceeding the threshold (default: 100 invocations) are marked "hot"
3. **Background Compilation**: Hot methods are compiled to Go code asynchronously
4. **Inline Caching**: Call sites cache method lookups for faster dispatch (~90% monomorphic hit rate)

### Persistence Modes

Compiled code can be persisted across restarts using three modes:

**Static Build** - Generates Go files for compilation into your binary:
```go
jit.WriteForStaticBuild("./generated/aot", "aot")
// Then rebuild your application to include the generated code
```

**Plugin Loading** - Generates `.so` plugins for runtime loading (Linux/macOS only):
```go
jit.WriteAsPlugin("./plugins", "maggie_aot")
pluginPath, _ := jit.BuildPlugin("./plugins", "maggie_aot")
jit.LoadPlugin(pluginPath)
```

**Image Persistence** - Stores compiled code in the image file:
```go
// Save compiled methods to image (with gzip compression)
jit.WriteToImage("maggie.image", true)

// Load compiled methods from image on next startup
count, _ := jit.LoadFromImage("maggie.image")
```

### JIT Statistics

```go
stats := jit.Stats()
fmt.Printf("Methods compiled: %d\n", stats.MethodsCompiled)
fmt.Printf("Hot methods: %d\n", stats.HotMethodCount)
fmt.Printf("Queue length: %d\n", stats.QueueLength)
```

### Inline Cache Statistics

```go
icStats := vm.GetInterpreter().CollectICStats()
fmt.Printf("Monomorphic: %d (%.1f%%)\n", icStats.Monomorphic,
    float64(icStats.Monomorphic)/float64(icStats.Total)*100)
fmt.Printf("Cache hit rate: %.2f%%\n", icStats.HitRate*100)
```
