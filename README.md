<p align="center">
  <img src="https://github.com/chazu/procyon/blob/main/img/logo.png">
</p>

# Maggie

A Smalltalk dialect implemented in Go. Named for [Margaret Hamilton](https://en.wikipedia.org/wiki/Margaret_Hamilton_\(software_engineer\))

**Documentation:**
- **Language Guide** — Run `mag doc --serve` and open the Guide tab, or browse `lib/guide/` source files directly. 16 chapters from Getting Started through TupleSpace & Constraint Programming, with runnable examples validated by `mag doctest`.
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

Maggie has literal syntax for arrays and dictionaries:

```smalltalk
-- Array literal
#(1 2 3)

-- Dictionary literal (keys -> values, period-separated)
#{#name -> 'Alice'. #age -> 30. #active -> true}
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
description = "A web app with admin tools"
license = "MIT"
authors = ["Alice <alice@example.com>"]
repository = "https://github.com/example/my-app"
maggie = ">=0.10.0"          # minimum Maggie version required

[source]
dirs = ["src"]               # source directories to compile (default: ["src"])
entry = "Main.start"         # entry point (resolved relative to project namespace)
exclude = ["*_scratch.mag"]  # glob patterns to exclude from compilation

[dependencies]
yutani = { git = "https://github.com/chazu/yutani-mag", tag = "v0.5.0" }
helper = { path = "../helper" }
bleeding = { git = "https://github.com/example/lib", branch = "main" }
pinned = { git = "https://github.com/example/exact", commit = "abc123" }
ui-toolkit = { path = "../ui-toolkit", namespace = "CustomUI" }  # namespace override

[dev-dependencies]
test-helpers = { path = "../test-helpers" }

[image]
output = "my-app.image"
include-source = true         # preserve method source in image for fileOut

[test]
dirs = ["test"]               # test source directories
entry = "TestRunner.run"      # test entry point
timeout = 30000               # timeout in milliseconds (0 = no timeout)
exclude = ["*_slow.mag"]      # glob patterns to exclude from test sources

[scripts]
prebuild = "mag fmt --check"  # before compilation
postbuild = "echo done"       # after binary is written
pretest = "mag build"         # before test execution
posttest = "echo done"        # after tests complete

[go-wrap]
output = ".maggie/wrap"       # output directory for generated wrappers

  [[go-wrap.packages]]
  import = "net/http"
  include = ["ListenAndServe", "Get"]

[sync]
capabilities = ["File", "HTTP"]  # capabilities advertised to peers
listen = ":8081"                 # address for sync server
peers = ["localhost:8082"]       # peers to push/pull from

[trust]
default = "sync"                        # default perms for unknown peers
ban-threshold = 3                       # hash mismatches before auto-ban
spawn-restrictions = ["File", "ExternalProcess", "HTTP"]  # globals hidden from remote spawns

  [[trust.peer]]
  id = "a1b2c3..."                      # Ed25519 public key, hex-encoded
  name = "build-farm"
  perms = "sync,spawn"                  # comma-separated: sync, message, spawn, all
```

When `mag` is invoked in a directory with `maggie.toml`, it automatically resolves dependencies, loads source directories, and runs the entry point.

**Project metadata:** `description`, `license`, `authors`, and `repository` are informational. The `maggie` field is a version constraint — if the running `mag` version is too old, loading fails with a clear error. Supported constraint forms: `>=X.Y.Z`, `<X.Y.Z`, `>=X.Y.Z <A.B.C`, `~X.Y.Z` (pessimistic).

**Dev-dependencies:** Same format as `[dependencies]` but only loaded for `mag test`, `mag -i` (REPL), and other development commands. Excluded from `mag build` production binaries.

**Dependency refs:** Each git dependency uses exactly one of `tag`, `branch`, or `commit` (mutually exclusive). The lock file pins exact commit hashes regardless.

### Multi-Target Builds

Projects can declare multiple build targets using `[[target]]` array tables:

```toml
[[target]]
name = "server"
entry = "MyApp::Server.start"
output = "my-server"
full = true                    # full mag CLI with project baked in

  [[target.go-wrap]]           # per-target Go package wrapping
  import = "net/http"
  include = ["ListenAndServe"]

  [target.image]
  output = "server.image"
  include-source = false

[[target]]
name = "cli"
entry = "MyApp::CLI.main"
output = "my-cli"
extra-dirs = ["tools"]         # additional source dirs
exclude-dirs = ["admin"]       # dirs to remove from base source.dirs
exclude = ["*_debug.mag"]      # additional file exclude patterns
```

Source composition per target: `source.dirs + target.extra-dirs - target.exclude-dirs`, filtered by `source.exclude + target.exclude`. Go-wrap packages are additive (top-level + target-specific). If no `[[target]]` sections exist, the project behaves as a single target from top-level config.

```bash
mag build               # build the default (first) target
mag build -t server     # build a specific target
mag build --all         # build all targets
mag build -t cli -o /usr/local/bin/mycli  # -o overrides target output
```

### Dependency Management

```bash
mag deps              # Resolve and fetch dependencies
mag deps update       # Re-resolve ignoring lock file
mag deps list         # Show resolved dependency tree
```

Dependencies are stored in `.maggie/deps/` and locked in `.maggie/lock.toml`.

### Test Configuration

```bash
mag test                    # run tests from [test] config
mag test --timeout 5000     # override timeout
mag test --entry CustomRunner.run  # override entry point
```

Test dirs are compiled in addition to source dirs. Dev-dependencies are loaded automatically. The test entry point should return a small integer exit code (0 = pass).

## Building Custom Binaries

Build a standalone binary from your project:

```bash
# Entry-point-only binary (just runs your app)
mag build -o myapp

# Full-system binary (complete mag CLI with your project baked in)
mag build --full -o myapp
```

With `--full`, the resulting binary is a complete Maggie system — REPL, `fmt`, `doctest`, `help`, LSP, and all other `mag` subcommands work. When invoked with no arguments, it runs your project's entry point. Use `-i` to drop into a REPL with all your classes pre-loaded.

```bash
./myapp                  # Runs your entry point (e.g., Main.start)
./myapp -i               # REPL with your classes loaded
./myapp fmt src/          # Format your source files
./myapp doctest           # Run your docstring tests
./myapp help MyClass      # Show help for your classes
```

Without `--full`, the binary only runs the entry point (smaller, simpler).

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

## CUE Integration

Maggie integrates [CUE](https://cuelang.org) as a first-class citizen for constraint-based validation and pattern matching. Any object can be projected into the CUE value lattice, and CUE schemas can be used for structural template matching.

```smalltalk
"Project an object into CUE and check its kind"
42 asCueValue kind          "=> 'int'"

"Match objects against CUE schemas"
ctx := CueContext new.
schema := (ctx compileString: 'int & >0 & <100') value.
schema matchesObject: 50    "=> true"
schema matchesObject: 200   "=> false"

"Validate data against a schema"
ctx validate: 'name: "Alice", age: 30' against: 'name: string, age: int & >0'
```

See Guide 15 (CUE Integration) for full coverage, and [ROADMAP.md](ROADMAP.md) for planned layers (class schemas, method guards/predicate dispatch).

## TupleSpace & Constraint Store

Maggie provides Linda-style tuplespace coordination with linear logic semantics and a concurrent constraint programming (CCP) constraint store.

```smalltalk
"TupleSpace: publish, take, and match tuples via CUE templates"
ts := TupleSpace new.
ctx := CueContext new.
ts out: 42.
ts out: 'hello'.
ts tryIn: (ctx compileString: 'int') value   "=> 42 (consumed)"

"Persistent tuples (never consumed — shared facts)"
ts outPersistent: 'config-value'.

"Atomic multi-take (tensor product — all or nothing)"
ts inAll: {intTemplate. stringTemplate}

"Constraint Store: monotonic knowledge with ask/tell"
store := ConstraintStore new.
store tell: (ctx compileString: '{ready: true}') value.
store tryAsk: (ctx compileString: '{ready: true}') value  "=> true"
```

See Guide 16 (TupleSpace & Constraint Programming) for full coverage.

## Adaptive Compilation

The interpreter uses **inline caching** (monomorphic → polymorphic → megamorphic) on all call sites for faster method dispatch, and a **profiler** that tracks method/block invocation counts to identify hot code.

An **AOT compiler** (`vm/aot.go`) can translate bytecode methods to Go source code. This infrastructure is implemented but **not yet user-facing** — there is no CLI flag, and the generated Go source requires `go build` to become executable.

See [docs/MAGGIE_DESIGN.md](docs/MAGGIE_DESIGN.md) (Execution Architecture) for details.

## Content-Addressed Code

Every compiled method has a **semantic content hash** (SHA-256 of its normalized AST with De Bruijn indexed variables). Methods with identical behavior produce identical hashes regardless of variable naming, whitespace, or comments. This enables Unison-style code sharing: if two nodes have the same hash, they have the same code.

Methods also carry a **typed content hash** that includes type annotations (parameter types, return types) alongside the semantic content. This creates a two-layer identity system:

- **Semantic hash** — identifies behavior. Same code = same hash, even if type annotations differ. Used for execution identity and content-addressed distribution.
- **Typed hash** — identifies behavior + type contract. Used for distributed type verification: a node can check that received code has been type-checked without re-running the checker.

Both hashes are computed at compile time, persisted in images, and carried in the distribution protocol. The `ContentStore` indexes primarily by semantic hash (used for have/want negotiation between nodes) and maintains a reverse index from typed hash to semantic hash for local verification. When receiving code from a peer, the sync service verifies both hashes — semantic for identity, typed for type annotation integrity.

```bash
mag sync push localhost:9090    # Push code by content hash
mag sync pull localhost:9090    # Pull and verify by hash
```

## Distributed Messaging

Maggie processes on different nodes can exchange messages over HTTP/2. Each node has an Ed25519 identity (stored in `.maggie/node.key`) and all messages are cryptographically signed.

```smalltalk
"Connect to a remote node"
node := Node connect: 'localhost:8081'.

"Get a reference to a named process"
worker := node processNamed: 'counter'.

"Fire-and-forget (delivers to remote mailbox)"
worker cast: #increment: with: 42.

"Request-response (returns a Future)"
future := worker asyncSend: #compute: with: data.
result := future await.
```

On the receiving side, processes read from their mailbox:

```smalltalk
"Register and receive"
Process current registerAs: 'counter'.
msg := Process receive.          "Blocks until message arrives"
msg selector.                    "=> #increment:"
msg payload.                     "=> 42"
```

All message payloads are serialized with CBOR. Supported types include integers, floats, strings, symbols, arrays, dictionaries, objects (with circular reference support), and CUE values. Non-serializable types (Process, Channel, Mutex) raise errors at send time.

Process monitors and links work across nodes. A `NodeHealthMonitor` pings remote nodes with active watches and synthesizes `nodeDown` notifications on failure:

```smalltalk
"Monitor a remote process"
ref := Process current monitor: remoteWorker.
msg := Process receive.
msg selector = #processDown:
    ifTrue: ['Worker died: ', msg payload printString].
```

See [`examples/distributed-counter/`](examples/distributed-counter/) for a complete runnable example.

## Formatting

Maggie includes a built-in source formatter:

```bash
mag fmt                    # Format all .mag files in current directory
mag fmt src/ lib/          # Format specific directories
mag fmt --check            # Check without modifying (exit 1 if unformatted)
```

## Documentation Generation

Generate HTML documentation from docstrings, including the language guide and API reference:

```bash
mag doc                        # Generate docs (guide + API reference)
mag doc --serve                # Generate and serve on :8080
mag doc --output docs/site     # Specify output directory
mag doctest                    # Run doctest assertions from docstrings
mag doctest --verbose          # Show each test as it runs
mag doctest --class Array      # Run tests for a specific class
```

The guide lives in `lib/guide/` as `.mag` source files. Each chapter is a Maggie class with docstring prose and `>>>` test assertions validated by `mag doctest`. Docstrings use triple-quote syntax (`"""`).

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
