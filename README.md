<p align="center">
  <img src="https://github.com/chazu/procyon/blob/main/img/logo.png">
</p>

# Maggie

A Smalltalk dialect implemented in Go. Named for [Margaret Hamilton](https://en.wikipedia.org/wiki/Margaret_Hamilton_\(software_engineer\))

**Documentation:**
- [User Guide](docs/USER_GUIDE.md) - Getting started, syntax, core classes
- [Design Document](docs/MAGGIE_DESIGN.md) - Architecture and implementation details
- [Language Server](docs/lsp.md) - LSP features and editor integration


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
- `manifest/` - Project manifest (`maggie.toml`) and dependency resolution
- `cmd/bootstrap/` - Bootstrap tool that compiles core library to binary image
- `cmd/mag/` - Main CLI entry point
- `lib/` - Core class library in Maggie source (`.mag` files)
- `docs/` - Design documentation

## Maggie Source Files (.mag)

Maggie source files use `.mag` extension. Files can define classes with Trashtalk-style declarations, or add methods to existing classes with unindented method headers:

```smalltalk
-- Define a class with instance variables and methods
Counter subclass: Object
  instanceVars: value

  method: increment [
    value := value + 1.
    ^value
  ]

  method: value [ ^value ]
```

Files can also declare a namespace and imports:

```smalltalk
namespace: 'MyApp::Models'
import: 'MyApp::Core'

User subclass: Object
  instanceVars: name email

  method: name [ ^name ]
  method: email [ ^email ]
```

The bootstrap tool compiles core library files into bytecode and saves them in a binary image (`maggie.image`).

## Module System

Maggie supports class-level namespaces using `::` as the separator:

- **Namespace declaration**: `namespace: 'MyApp::Models'` at the top of a `.mag` file
- **Imports**: `import: 'MyApp::Core'` to reference classes from another namespace without qualification
- **Directory convention**: Directory structure maps to namespaces automatically (`src/myapp/models/` becomes `Myapp::Models`)
- **Class resolution order**: Current namespace, then imports, then bare name

## Project Manifest (maggie.toml)

Projects can be managed with a `maggie.toml` manifest:

```toml
[project]
name = "my-app"
namespace = "MyApp"
version = "0.1.0"

[source]
dirs = ["src"]
entry = "Main.start"

[dependencies]
yutani = { git = "https://github.com/chazu/yutani-mag", tag = "v0.5.0" }
helper = { path = "../helper" }

[image]
output = "my-app.image"
include-source = true
```

When `mag` is invoked in a directory with `maggie.toml`, it automatically resolves dependencies, loads source directories, and runs the entry point.

### Dependency Management

```bash
mag deps              # Resolve and fetch dependencies
mag deps update       # Re-resolve ignoring lock file
mag deps list         # Show resolved dependency tree
```

Dependencies are stored in `.maggie/deps/` and locked in `.maggie/lock.toml`.

## Images

Maggie uses binary images to snapshot VM state (classes, methods, globals).

```bash
# Save an image after loading source
mag ./src/... --save-image my-app.image

# Run from a custom image
mag --image my-app.image -m Main.start

# Save from within Maggie code
Compiler saveImage: 'my-app.image'.
```

## Adaptive Compilation

The interpreter uses **inline caching** (monomorphic → polymorphic → megamorphic) on all call sites for faster method dispatch, and a **profiler** that tracks method/block invocation counts to identify hot code.

An **AOT compiler** (`vm/aot.go`) can translate bytecode methods to Go source code. This infrastructure is implemented but **not yet user-facing** — there is no CLI flag, and the generated Go source requires `go build` to become executable.

See [docs/MAGGIE_DESIGN.md](docs/MAGGIE_DESIGN.md) (Execution Architecture) for details.

## Formatting

Maggie includes a built-in source formatter:

```bash
mag fmt                    # Format all .mag files in current directory
mag fmt src/ lib/          # Format specific directories
mag fmt --check            # Check without modifying (exit 1 if unformatted)
```

## Documentation Generation

Generate HTML API documentation from docstrings, and run inline doctests:

```bash
mag ./src doc              # Generate docs for loaded classes
mag ./src doc --output docs/api  # Specify output directory
mag ./src doctest          # Run doctest assertions from docstrings
mag ./src doctest --verbose      # Show each test as it runs
mag ./src doctest --class Array  # Run tests for a specific class
```

Docstrings use triple-quote syntax (`"""`), and doctest assertions use `>>>` to compare expressions.

## Benchmarking

```bash
# Compare against stored baseline
./scripts/bench-compare.sh

# Generate a new baseline
go test -bench=BenchmarkHotPath -run='^$' -count=10 -benchmem ./vm/ > benchmarks/baseline.txt
```

Requires [benchstat](https://pkg.go.dev/golang.org/x/perf/cmd/benchstat).

## Language Server

Maggie provides an LSP server for editor integration:

```bash
mag lsp          # Start LSP server on stdio
mag --lsp        # Alternative flag form
```

See [docs/lsp.md](docs/lsp.md) for supported features and editor configuration.
