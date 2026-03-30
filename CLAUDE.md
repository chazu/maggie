## Concurrency Primitives

Maggie provides Go-style concurrency primitives. All are fully implemented.

### Channel (Go-style communication)

```smalltalk
ch := Channel new.          "Unbuffered channel"
ch := Channel new: 5.       "Buffered channel with capacity 5"

ch send: value.             "Blocking send"
ch receive.                 "Blocking receive"
ch trySend: value.          "Non-blocking send (returns true/false)"
ch tryReceive.              "Non-blocking receive (returns value or nil)"
ch close.                   "Close channel"
ch isClosed.                "Check if closed"
ch isEmpty.                 "Check if empty"
ch size.                    "Number of buffered items"
ch capacity.                "Buffer capacity"
```

### Channel Select (multiplexed channel operations)

```smalltalk
"Wait on multiple channels"
result := Channel select: {
    ch1 onReceive: [:v | 'Got from ch1: ', v].
    ch2 onReceive: [:v | 'Got from ch2: ', v]
}.

"Non-blocking with default"
result := Channel select: {
    ch onReceive: [:v | 'Received: ', v]
} ifNone: [
    'No channel ready'
].
```

### Process (lightweight goroutines)

```smalltalk
proc := [expression] fork.         "Fork block, returns Process"
proc := [expr] forkWith: arg.      "Fork with argument"
proc := [:ctx | ...] forkWithContext: ctx.  "Fork with cancellation context"
proc := [expr] forkRestricted: #('File' 'HTTP').  "Fork with restricted globals"

proc wait.                         "Block until complete, returns result"
proc isDone.                       "Check if finished"
proc result.                       "Get result (nil if not done)"

Process current.                   "Current process"
Process yield.                     "Yield to other goroutines"
Process sleep: milliseconds.       "Sleep"
Process forkWithout: #('File' 'HTTP') do: [dangerous code].  "Fork with restrictions"
```

**fork vs forkWithResult**: `fork` treats non-local returns (^) as local returns within the forked process—they don't escape. This prevents crashes from NLRs crossing goroutine boundaries.

**Process-level restriction**: Forked processes can be restricted from seeing certain globals (classes, modules). Restricted names simply resolve to `nil` — there is no permission error. Restrictions are inherited by child forks. `Compiler evaluate:` and `Object allClasses` respect process restrictions. Forked process writes to globals go to a process-local overlay and don't affect the shared VM state.

### Mutex (mutual exclusion)

```smalltalk
mutex := Mutex new.
mutex lock.                        "Acquire lock (blocks)"
mutex unlock.                      "Release lock"
mutex tryLock.                     "Non-blocking (returns true/false)"
mutex isLocked.                    "Check if locked"
mutex critical: [protected code]. "Execute block while holding lock"
```

### WaitGroup (synchronization barrier)

```smalltalk
wg := WaitGroup new.
wg add: count.                     "Add to counter"
wg done.                           "Decrement counter"
wg wait.                           "Block until counter reaches zero"
wg count.                          "Current counter value"
wg wrap: [block].                  "Convenience: add 1, fork, auto-done"
```

### Semaphore (counting permits)

```smalltalk
sem := Semaphore new.              "Binary semaphore (1 permit)"
sem := Semaphore new: 3.           "Semaphore with 3 permits"

sem acquire.                       "Acquire permit (blocks)"
sem release.                       "Release permit"
sem tryAcquire.                    "Non-blocking (returns true/false)"
sem available.                     "Number of available permits"
sem capacity.                      "Total capacity"
sem critical: [block].             "Execute while holding permit"
```

### CancellationContext (timeouts and cancellation)

```smalltalk
ctx := CancellationContext background.      "Never-cancelled base"
ctx := CancellationContext withCancel.      "Cancellable context"
ctx := CancellationContext withTimeout: ms. "Timeout in milliseconds"

ctx cancel.                        "Cancel this context"
ctx isCancelled.                   "Check if cancelled"
ctx isDone.                        "Alias for isCancelled"
ctx hasDeadline.                   "Check if has timeout"
ctx deadline.                      "Deadline in milliseconds"
ctx remainingTime.                 "Milliseconds until deadline"
ctx wait.                          "Block until cancelled"
ctx doneChannel.                   "Channel that closes on cancel"

"Child contexts inherit parent cancellation"
child := parent withCancel.
child := parent withTimeout: 500.

"Fork with context"
[:context |
    [context isCancelled not] whileTrue: [work]
] forkWithContext: ctx.
```

---

## Compiler Primitives

The Compiler class provides methods for dynamic code evaluation with persistent globals.

### Dynamic Evaluation

```smalltalk
"Evaluate expressions at runtime"
Compiler evaluate: '3 + 4'.           "→ 7"
Compiler evaluate: 'Array new: 5'.    "→ #(nil nil nil nil nil)"

"Evaluate with a specific receiver context"
arr := Array new: 3.
Compiler evaluate: 'self size' in: arr.  "→ 3"
```

### Global Variable Persistence

Variables assigned in evaluated expressions persist across evaluations:

```smalltalk
Compiler evaluate: 'x := 42'.         "→ 42, x is now a global"
Compiler evaluate: 'x + 1'.           "→ 43"
Compiler evaluate: 'x := x * 2'.      "→ 84"
```

### Direct Global Access

For REPL tools and IDE integration:

```smalltalk
"Set a global directly"
Compiler setGlobal: #it to: someObject.

"Get a global (returns nil if not found)"
value := Compiler getGlobal: #myVar.
```

This is used by the IDE to bind `it` to the last evaluated result, making it available in subsequent expressions.

### File Loading (fileIn/fileOut)

```smalltalk
"Load a .mag source file into the running VM"
Compiler fileIn: 'path/to/file.mag'.

"Recursively load all .mag files from a directory"
Compiler fileInAll: 'path/to/dir'.

"Write a class back to a .mag file (reconstructs source)"
Compiler fileOut: 'ClassName' to: '/tmp/ClassName.mag'.

"Write all classes in a namespace to a directory (one file per class)"
Compiler fileOutNamespace: 'MyApp::Models' to: '/tmp/models/'.
```

fileOut reconstructs source from stored method text. Methods compiled from `.mag` files preserve their original source text; methods defined via `evaluate:` or primitives produce a stub comment instead. For namespaced classes, fileOut emits a `namespace:` declaration at the top of the file. Import declarations are not preserved by fileOut (classes store their namespace but not per-file import lists).

**Loading pipeline:** Files are loaded in a two-pass batch pipeline. Pass 1a registers class/trait skeletons, pass 1b resolves superclass pointers via `LookupWithImports`, and pass 2 compiles method bodies with FQN resolution context. `fileInAll:` uses this two-pass pipeline; `fileIn:` uses single-pass (acceptable since it operates on an already-populated ClassTable).

### Image Persistence

```smalltalk
"Save current VM state (all classes, methods, globals) to an image file"
Compiler saveImage: 'my-app.image'.
```

From the CLI:

```bash
# Build image from source
mag ./src/... --save-image my-app.image

# Run from a custom image instead of the embedded default
mag --image my-app.image -m Main.start
```

### Stack Overflow Protection

The VM enforces a configurable maximum call frame depth (default: 4096). When exceeded, a `StackOverflow` exception is raised. This exception is a subclass of `Error` and is catchable with `on:do:`:

```smalltalk
[self deeplyRecursiveMethod]
    on: StackOverflow
    do: [:ex | 'Stack overflow caught' printString].
```

The limit is configurable via the interpreter's `MaxFrameDepth` field (Go-level). The constant `DefaultMaxFrameDepth` (4096) applies when no custom value is set.

### Tail-Call Optimization

Self-recursive methods in tail position are automatically optimized by the compiler. The compiler detects `^self selector: args` patterns at the end of a method and emits `OpTailSend` instead of `OpSend`. At runtime, `OpTailSend` reuses the current call frame rather than pushing a new one, allowing unbounded self-recursion without stack overflow.

No code changes are needed — TCO is transparent. It works alongside stack overflow protection (non-tail-recursive methods still hit the depth limit).

### Peephole Bytecode Optimizer

The compiler runs a peephole optimization pass after codegen (`compiler/peephole.go`):

- **Constant folding:** `3 + 4` compiles to `PushInt8(7)` instead of `PushInt8(3)/PushInt8(4)/SendPlus`. Chains fold completely. Overflow promotes to `PushInt32`.
- **Push-pop elimination:** Side-effect-free `Push*/Pop` pairs (from statement separators, cell init, etc.) are stripped.
- **Dead code elimination:** Unreachable instructions after unconditional jumps/returns are removed, respecting jump targets.

The pass iterates until stable, adjusts jump offsets, and remaps source map entries. Transparent — no user action needed.

### Source Position Mapping

Compiled methods and blocks carry a `SourceMap []SourceLoc` that maps bytecode offsets to `(line, column)` positions. The compiler emits entries during codegen; the interpreter uses them for stack traces:

```
  [0] MyClass>>myMethod at line 5, column 3
  [1] <block> at line 12, column 7
```

Source maps are persisted in the image format and survive save/load cycles.

### Call Stack Capture

```smalltalk
Compiler captureCallStack.   "Returns an Array of Dictionaries"
```

Each dictionary contains `#class`, `#method`, `#line`, `#column`, and `#ip` keys. Useful for post-mortem debugging and custom error reporting.

### BigInteger

When SmallInteger arithmetic overflows the 48-bit range, values are automatically promoted to BigInteger (backed by Go's `math/big.Int`). Demotion back to SmallInteger happens when results fit. All arithmetic, comparison, and bitwise operations work transparently across SmallInteger and BigInteger.

```smalltalk
20 factorial.              "→ BigInteger: 2432902008176640000"
20 factorial class name.   "→ #BigInteger"
```

### Type Annotations and Protocols

Maggie supports optional structural type annotations. Types are checked by `mag typecheck` — they never affect compilation or runtime behavior (zero cost, Strongtalk model).

#### Type Annotations

```smalltalk
"Parameter types"
method: at: index <Integer> put: value <Object> [ ... ]

"Return types (caret + angle brackets)"
method: size ^<Integer> [ ^items size ]

"Binary method"
method: + other <Number> ^<Number> [ ^self primPlus: other ]

"Typed temporaries"
| count <Integer> name <String> |

"Typed instance variables"
instanceVars: name <String> age <Integer>
```

Special types: `<Self>` (receiver type), `<Dynamic>` (compatible with everything, the default for untyped code).

#### Protocols

Protocols define structural types — a set of message signatures that a class must respond to:

```smalltalk
Sizeable protocol
  size ^<Integer>.
  isEmpty ^<Boolean>.

Indexable protocol
  includes: Sizeable.
  at: <Integer> ^<Object>.
  at: <Integer> put: <Object> ^<Object>.
```

Protocol inclusion (`includes:`) imports all signatures from another protocol. Protocols live in `.mag` files and participate in the namespace/import system.

#### Type Checking

```bash
mag typecheck                  # check all .mag files in current dir
mag typecheck src/             # check specific directory
mag typecheck --verbose        # show all checks
```

Reports warnings to stderr, never blocks compilation. Checks:
- Type annotations reference known types/protocols
- Classes satisfy protocols used as parameter types
- **Type inference:** infers types from literals and assignments, warns when a message is sent to an incompatible type (e.g., `42 isEmpty` warns "SmallInteger does not understand #isEmpty")
- Return type annotations are cross-checked against inferred return types

Type inference works without any annotations — it tracks types from literals (`42` → SmallInteger, `'hello'` → String) through assignments and message send chains. Untyped code produces `<Dynamic>` which suppresses warnings.

### Source Formatting

Format Maggie source files to a canonical style:

```bash
# Format all .mag files in the current directory
mag fmt

# Format specific files or directories
mag fmt src/ lib/MyClass.mag

# Check formatting without modifying files (exit code 1 if changes needed)
mag fmt --check
```

The `Format()` Go function in `cmd/mag/format.go` can also be used as a library.

### Building Custom Binaries

```bash
# Entry-point-only binary (minimal, just runs your app)
mag build -o myapp

# Full-system binary (complete mag CLI with project baked in)
mag build --full -o myapp

# Build a specific target
mag build -t server

# Build all targets
mag build --all
```

With `--full`, the binary is a complete `mag` CLI with the project's image embedded. When invoked with no arguments, it runs the project's entry point; all `mag` subcommands (REPL, `fmt`, `doctest`, `help`, LSP, etc.) still work. Without `--full`, the binary only runs the entry point.

When `[[target]]` sections are defined in `maggie.toml`, `-t <name>` selects a specific target and `--all` builds all of them. CLI flags (`-o`, `--full`) override the target's configuration.

**Implementation:** `cmd/mag/build.go` resolves targets via `manifest.ResolveTarget()` / `ResolveAllTargets()`, then dispatches to `gowrap.BuildFullSystem()` or `gowrap.BuildEmbedded()` per target. The `projectEntryPoint`, `projectNamespace`, and `projectWrapperRegistrars` package-level vars in `main.go` are set by the generated `project_config.go` via `init()`.

---

## Module System

Maggie has a module system built on namespaces, source file conventions, and a project manifest.

### Namespaces and Imports

Source files can declare a namespace and import other namespaces. These must appear before any class or trait definitions.

```smalltalk
namespace: 'Yutani::Widgets'

import: 'Yutani'
import: 'Yutani::Events'

YutaniButton subclass: YutaniWidget
  instanceVars: label onClick
  method: label [ ^label ]
```

- `namespace:` — optional, one per file, sets the namespace for all classes defined in the file
- `import:` — zero or more, allows unqualified references to classes in imported namespaces
- `::` is the namespace separator (e.g., `Yutani::Widgets::Button`)
- Files without `namespace:`/`import:` work exactly as before (backward compatible)

**Class resolution order** (when looking up a class name at compile time):
1. Current namespace (`MyNS::ClassName`)
2. Each imported namespace in order (`Import1::ClassName`, `Import2::ClassName`, ...)
3. Bare name (`ClassName` — the root/default namespace)

**FQN resolution in method bodies:** The compiler resolves bare class names to fully-qualified names at compile time. When a method body references `Button` and the file imports `Widgets`, the compiler emits `OpPushGlobal("Widgets::Button")`. This is transparent — no runtime cost, no new opcodes. Explicit FQN syntax also works in expressions: `Widgets::Button new`.

**Globals registration:** Namespaced classes are registered in Globals only under their FQN (e.g., `Globals["Widgets::Button"]`). Root-namespace classes use the bare name. Two namespaces defining the same class name coexist without collision.

### Directory-as-Namespace Convention

When loading files from directories, namespaces are derived automatically from the directory structure:

```
src/myapp/models/User.mag  -> MyApp::Models
src/myapp/Main.mag          -> MyApp
src/Helper.mag              -> (no namespace, root)
```

- Path segments relative to the base directory become PascalCase namespace segments joined by `::`
- An explicit `namespace:` declaration in the file overrides the directory-derived namespace
- Root-level files (directly in the base directory) have no namespace

### Project Manifest (maggie.toml)

A `maggie.toml` file in the project root configures the project:

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
dirs = ["src"]               # source directories to compile
entry = "Main.start"         # entry point (resolved relative to project namespace)
exclude = ["*_scratch.mag"]  # glob patterns to exclude from compilation

[dependencies]
yutani = { git = "https://github.com/chazu/yutani-mag", tag = "v0.5.0" }
local-lib = { path = "../shared-lib" }
bleeding = { git = "https://github.com/example/lib", branch = "main" }
pinned = { git = "https://github.com/example/exact", commit = "abc123" }

[dev-dependencies]
test-helpers = { path = "../test-helpers" }

[image]
output = "my-app.image"
include-source = true         # preserve method source in image for fileOut

[test]
dirs = ["test"]               # test source directories
entry = "TestRunner.run"      # test entry point
timeout = 30000               # timeout in milliseconds (0 = no timeout)

[scripts]
prebuild = "mag fmt --check"
postbuild = "echo done"
pretest = "echo running tests"
posttest = "echo tests done"
```

**Project metadata:** `description`, `license`, `authors`, and `repository` are informational. The `maggie` field is a version constraint — if the running `mag` version is too old, loading fails with a clear error. Supported constraint forms: `>=X.Y.Z`, `<X.Y.Z`, `>=X.Y.Z <A.B.C`, `~X.Y.Z` (pessimistic).

**Source exclude:** Glob patterns matched against filenames and relative paths. Uses `filepath.Match` syntax (single-level globs).

**Dev-dependencies:** Same format as `[dependencies]` but only loaded for `mag test`, `mag -i` (REPL), and other development commands. Excluded from `mag build` production binaries.

**Dependency refs:** Each git dependency uses exactly one of `tag`, `branch`, or `commit` (mutually exclusive). The lock file pins exact commit hashes regardless.

When `mag` is invoked without explicit paths and a `maggie.toml` exists, the project is loaded automatically:

```bash
mag -m Main.start       # Detects maggie.toml, resolves deps, loads src/, runs Main.start
mag run                 # Run the default entry point
mag run -t server       # Run a named target
mag --save-image app.image  # Same, but also saves image
```

### Multi-Target Builds

Projects can declare multiple build targets using `[[target]]` array tables:

```toml
[source]
dirs = ["src"]
entry = "Main.start"    # default entry (used when no targets declared)

[[target]]
name = "server"
entry = "MyApp::Server.start"
output = "my-server"
full = true              # full mag CLI with project baked in

  [[target.go-wrap]]     # per-target Go package wrapping
  import = "net/http"
  include = ["ListenAndServe"]

  [target.image]
  output = "server.image"
  include-source = false

[[target]]
name = "cli"
entry = "MyApp::CLI.main"
output = "my-cli"
extra-dirs = ["tools"]   # additional source dirs
exclude-dirs = ["admin"] # dirs to remove from base source.dirs
exclude = ["*_debug.mag"] # additional file exclude patterns
```

**Source composition per target:** `project.source.dirs + target.extra-dirs - target.exclude-dirs`, filtered by `source.exclude + target.exclude`.

**Go-wrap composition:** Top-level `go-wrap.packages` + `target.go-wrap.packages` (additive).

**CLI:**

```bash
mag build               # build the default (first) target
mag build -t server     # build a specific target
mag build --all         # build all targets
mag build -t cli -o /usr/local/bin/mycli  # -o overrides target output
```

If no `[[target]]` sections exist, the project behaves as before (single target from top-level config). Backward compatible.

### Test Configuration

```bash
mag test                    # run tests from [test] config
mag test --timeout 5000     # override timeout
mag test --entry CustomRunner.run  # override entry point
```

Test dirs are compiled in addition to source dirs. Dev-dependencies are loaded automatically. The test entry point should return a small integer exit code (0 = pass).

### Script Hooks

Lifecycle hooks run shell commands at build/test boundaries. Empty strings are no-ops.

```toml
[scripts]
prebuild = "mag fmt --check"  # before compilation
postbuild = "echo done"       # after binary is written
pretest = "mag build"         # before test execution
posttest = "echo done"        # after tests complete
```

### Dependency Management

Dependencies are declared in `maggie.toml` and managed via the `mag deps` subcommand.

```bash
mag deps          # Resolve and fetch all dependencies
mag deps update   # Re-resolve ignoring lock file
mag deps list     # Show dependency tree
```

**Dependency types:**
- `git` — cloned to `.maggie/deps/<name>/`, checked out to the specified tag, branch, or commit
- `path` — local filesystem path (relative to project root)

**Ref specifiers** (mutually exclusive — pick one per dependency):
- `tag = "v1.0.0"` — exact tag
- `branch = "main"` — track a branch (lock file pins the commit)
- `commit = "abc123"` — exact commit hash

**Dependency storage layout:**

```
my-app/
  maggie.toml
  .maggie/
    deps/
      yutani/           # git clone
      local-lib/        # resolved path
    lock.toml           # locked versions (auto-generated)
  src/
    Main.mag
```

Dependencies are loaded in topological order (transitive deps before direct deps, deps before the project itself). Each dependency's own `maggie.toml` is read for transitive dependency resolution.

The lock file (`.maggie/lock.toml`) records exact commit hashes for reproducible builds. It is auto-generated — do not edit by hand.

### Dependency Namespace Mapping

Each dependency maps to an importable namespace. The namespace is resolved in this order:

1. **Consumer override** — explicit `namespace` in `[dependencies]`
2. **Producer manifest** — the dependency's own `maggie.toml` `[project].namespace`
3. **PascalCase fallback** — the dep name converted to PascalCase (e.g., `my-lib` → `MyLib`)

```toml
[dependencies]
yutani = { git = "https://github.com/chazu/yutani-mag", tag = "v0.5.0" }
# Uses Yutani's own maggie.toml namespace (e.g., "Yutani")

ui-toolkit = { path = "../ui-toolkit", namespace = "CustomUI" }
# Consumer override: maps to "CustomUI" regardless of producer manifest
```

**Namespace prefixing:** All classes from a dependency are prefixed with its resolved namespace. A dep with namespace `Yutani` containing `src/widgets/Button.mag` registers `Button` under `Yutani::Widgets::Button` in Globals.

**Import remapping:** When a consumer overrides a dependency's namespace, internal imports within that dependency are automatically remapped. For example, if Yutani internally imports `Yutani::Events` and the consumer overrides to `ThirdParty::Yutani`, that import becomes `ThirdParty::Yutani::Events`.

**Collision detection:** Two dependencies mapping to the same namespace cause a hard error before loading. The error lists all collisions and suggests adding `namespace` overrides.

**Reserved namespaces:** Core VM class names (Object, Array, String, Channel, Process, etc.) cannot be used as the root segment of a dependency namespace. `namespace = "MyLib::Array"` is fine; `namespace = "Array"` is rejected.

---

## Debugging Yutani TUI Applications

When debugging MaggieDesktop or other Yutani-based TUI applications, use Yutani's DebugService to inspect screen state and widget properties.

### Quick Commands

```bash
# See what's on screen (ASCII dump with widget markers)
yutani debug screen -s <session-id> --bounds --legend

# Inspect a specific widget's state
yutani debug widget -s <session-id> -w <widget-id>

# List all widgets with positions
yutani debug bounds -s <session-id>

# JSON output for detailed analysis
yutani debug screen -s <session-id> --format json
```

### Finding the Session ID

Look for this in the Maggie output when starting a Yutani app:
```
YutaniSession: session created with ID: f1e6eb39-...
```

### Common Debugging Scenarios

**Widget not responding to clicks:**
1. Check if widget has focus: `yutani debug bounds -s <id>` (look for `*` in Focused column)
2. Check recent events: `yutani debug widget -s <id> -w <widget-id>` (see Recent Events)
3. Verify widget bounds vs click coordinates

**List selection not working:**
```bash
yutani debug widget -s <session-id> -w <list-widget-id>
```
Look for `selectedIndex` in Properties - if it's -1, nothing is selected.

**Can't see what's on screen (LLM debugging):**
```bash
yutani debug screen -s <session-id> --bounds --legend
```
This gives an ASCII representation with widget overlay that LLMs can reason about.

### Full Documentation

See `~/dev/go/yutani/DEBUG_GUIDE.md` for complete debugging documentation.

---

## Profiling

Maggie includes a wall-clock sampling profiler that periodically snapshots interpreter call stacks and produces flamegraph-compatible folded-stack output.

### CLI Flags

```bash
mag --profile -m Main.start                    # Profile at 1000 Hz → profile.folded
mag --profile --profile-rate 500 -m Main.start  # 500 Hz
mag --profile --profile-output out.folded -m Main.start  # Custom output path
mag --pprof -m Main.start                      # Go pprof CPU profiler → cpu.pprof
mag --profile --pprof -m Main.start            # Both profilers
```

### Maggie API

```smalltalk
Compiler startProfiling.          "Start at 1000 Hz"
Compiler startProfiling: 500.     "Start at custom Hz"
result := Compiler stopProfiling. "Stop, returns folded-stacks String"
Compiler isProfiling.             "true/false"
```

### Go API

```go
sp := vm.StartSamplingProfiler(time.Millisecond) // 1000 Hz
// ... run code ...
sp = vm.StopSamplingProfiler()
sp.WriteFoldedStacks(os.Stdout)
```

The `SamplingProfiler` type lives in `vm/sampling_profiler.go`. Primitive calls are tagged via `activePrimitiveName` in the `Interpreter` struct (atomic pointer, zero cost when profiler is nil).

---

## Benchmarking

Run benchmarks and compare against a stored baseline using `scripts/bench-compare.sh`:

```bash
# Compare current performance against baseline
./scripts/bench-compare.sh

# Compare against a custom baseline file
./scripts/bench-compare.sh path/to/baseline.txt

# Generate a new baseline
go test -bench=BenchmarkHotPath -run='^$' -count=10 -benchmem ./vm/ > benchmarks/baseline.txt
```

Requires `benchstat` (`go install golang.org/x/perf/cmd/benchstat@latest`). The script runs `BenchmarkHotPath` benchmarks with `-count=10 -benchtime=100ms` and reports statistically significant regressions via `benchstat`.
