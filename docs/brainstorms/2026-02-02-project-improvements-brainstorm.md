# Maggie Project Improvement Brainstorm

**Date:** 2026-02-02
**Author:** Claude (brainstorm session)

---

## Phase 1: 30 Ideas (Unfiltered)

1. Add test coverage for the server/LSP package (currently 0%)
2. Implement a REPL history and tab-completion system
3. Add fuzzing tests for the parser and lexer
4. Implement proper super send in AOT compilation
5. Add a package manager CLI (`mag deps`) with actual dependency fetching
6. Create a standard library with collections (OrderedDictionary, Set, SortedCollection)
7. Add stack overflow protection with configurable limits
8. Implement tail-call optimization in the interpreter
9. Add property-based (QuickCheck-style) tests for arithmetic primitives
10. Build a profiler visualization tool (flame graphs from profiler data)
11. Implement a proper metaclass system
12. Add HTTP/gRPC primitive test suites
13. Create a language playground (web-based REPL)
14. Add garbage collection for global registries (channels, processes, blocks)
15. Implement block variable captures in the Maggie self-hosted compiler
16. Add CI/CD pipeline with automated test runs and coverage reporting
17. Write a comprehensive error message improvement pass
18. Add source maps for better debugging (bytecode → source line mapping)
19. Implement method inlining in the JIT compiler
20. Add a type inference system for optional static typing
21. Create integration tests that exercise full programs end-to-end
22. Implement pattern matching syntax (like Elixir/Rust match)
23. Add benchmarking infrastructure with regression detection
24. Improve the module system with proper visibility modifiers (public/private)
25. Add file I/O primitive tests
26. Implement persistent data structures (immutable collections)
27. Create a VS Code extension using the LSP server
28. Add structured concurrency (nursery pattern like Python's trio)
29. Implement proper class variables
30. Add a `mag fmt` source code formatter

---

## Phase 2: Critical Evaluation

### Rejected Ideas (with reasons)

**#2 - REPL history and tab-completion**: Nice-to-have polish, but the project has more fundamental gaps (test coverage, incomplete features). Low impact on language quality.

**#5 - Package manager CLI**: CLAUDE.md describes `mag deps` but this is a large feature with many edge cases (network failures, version conflicts, diamond dependencies). Too large for a single improvement, and premature until the core language is more stable.

**#6 - Standard library collections**: Adding more Maggie-level library code when the VM primitives underneath aren't fully tested is building on shaky ground. Test the foundation first.

**#9 - Property-based tests for arithmetic**: Interesting testing methodology but lower priority than getting *any* tests for the 15 untested primitive modules. Start with basic unit tests first.

**#10 - Profiler visualization**: Nice developer experience improvement, but the profiler itself works. This is polish.

**#11 - Metaclass system**: CLAUDE.md explicitly states this was a pragmatic design choice. Adding it would be a fundamental architecture change with high risk and unclear benefit.

**#13 - Web playground**: Exciting but enormous scope. Requires WASM compilation or server-side execution, security sandboxing, etc. Not the right time.

**#17 - Error message improvement pass**: Worthwhile eventually, but requires a systematic audit and is more of a polish task. Lower impact than structural improvements.

**#18 - Source maps**: The debugger already has breakpoint/step support. Source maps would help but aren't blocking any workflow.

**#19 - Method inlining in JIT**: The JIT is already incomplete (block compilation disabled, super send missing). Finish the existing JIT before adding optimizations.

**#20 - Type inference system**: Enormous scope, would fundamentally change the language character. Smalltalk's dynamism is a feature, not a bug.

**#22 - Pattern matching**: Language design addition that requires parser changes, new opcodes, and extensive testing. Large scope, unclear demand.

**#24 - Visibility modifiers**: Against Smalltalk philosophy. Private methods in Smalltalk are convention, not enforcement.

**#26 - Persistent data structures**: Interesting but niche. Not addressing current pain points.

**#27 - VS Code extension**: Depends on LSP server being tested and reliable first (#1).

**#28 - Structured concurrency**: The existing concurrency model (Go-style) is well-tested and working. Adding a second concurrency paradigm adds confusion.

**#29 - Class variables**: CLAUDE.md notes this as a known gap, but it's low-priority since most patterns can work around it.

### Accepted Ideas (passed scrutiny)

| # | Idea | Why it passed |
|---|------|---------------|
| 1 | Server/LSP test coverage | Critical: 2,800 lines with 0% test coverage, this is the external API |
| 3 | Fuzzing tests for parser/lexer | High-value: parsers are classic fuzz targets, finds real bugs |
| 4 | Proper super send in AOT | Completes an unfinished feature that blocks AOT maturity |
| 7 | Stack overflow protection | Safety: current fixed 1024 stack panics without bounds checking |
| 8 | Tail-call optimization | Smalltalk relies heavily on recursion; TCO prevents stack overflows |
| 12 | HTTP/gRPC primitive tests | 1,614 lines of network code with zero tests |
| 14 | GC for global registries | Memory leak: channels/processes/blocks never cleaned from global maps |
| 15 | Block captures in Maggie compiler | Blocks self-hosting compiler; key milestone for language maturity |
| 16 | CI/CD pipeline | Foundation: prevents regressions, makes all other improvements sustainable |
| 21 | End-to-end integration tests | Tests the full stack: parse → compile → execute for real programs |
| 23 | Benchmarking with regression detection | Prevents performance degradation as features are added |
| 25 | File I/O primitive tests | Data safety: untested file operations risk data loss |
| 30 | Source code formatter (`mag fmt`) | Developer experience: Go proved `gofmt` is a killer feature for any language |

---

## Phase 3: Detailed Plans for Accepted Ideas

---

### 1. Server/LSP Test Coverage (0% → 70%+)

**What:** Add comprehensive tests for the entire `server/` package, which currently has 2,800 lines of code and zero tests. This includes the LSP server, browse service, eval service, inspect service, modify service, and session management.

**Why it's a good improvement:** The server package is Maggie's external API — it's how IDEs, editors, and tools interact with the language. Untested external APIs lead to silent breakage that users discover in production. The server has complex error handling paths (Connect error wrapping, invalid argument validation, missing class lookups) that need verification.

**Concrete plan:**

Create test files using Go's `httptest` and the Connect framework's testing utilities:

```go
// server/browse_service_test.go
package server

import (
    "context"
    "testing"
    "connectrpc.com/connect"
)

func TestListClasses_ReturnsBootstrappedClasses(t *testing.T) {
    svc := newTestBrowseService(t)
    resp, err := svc.ListClasses(context.Background(), connect.NewRequest(&ListClassesRequest{}))
    if err != nil {
        t.Fatalf("ListClasses: %v", err)
    }
    // Core classes must be present
    names := classNames(resp.Msg.Classes)
    for _, expected := range []string{"Object", "Integer", "String", "Array"} {
        if !contains(names, expected) {
            t.Errorf("missing core class %q", expected)
        }
    }
}

func TestGetClass_NotFound(t *testing.T) {
    svc := newTestBrowseService(t)
    _, err := svc.GetClass(context.Background(), connect.NewRequest(&GetClassRequest{Name: "NonExistent"}))
    if err == nil {
        t.Fatal("expected error for missing class")
    }
    if connect.CodeOf(err) != connect.CodeNotFound {
        t.Errorf("expected CodeNotFound, got %v", connect.CodeOf(err))
    }
}
```

For the LSP server, test the core protocol handlers:

```go
// server/lsp_test.go
package server

func TestLSP_TextDocumentCompletion(t *testing.T) {
    lsp := newTestLSPServer(t)
    // Open a document
    lsp.didOpen("test.mag", "Object subclass: Foo\n  method: bar [self ")

    // Request completion at cursor position
    completions := lsp.completion("test.mag", Position{Line: 1, Character: 18})

    // Should suggest methods of Object (since receiver is self)
    if len(completions) == 0 {
        t.Error("expected completions for self sends")
    }
}
```

Test helper to create isolated test instances:

```go
func newTestBrowseService(t *testing.T) *BrowseService {
    t.Helper()
    vm := NewVM()
    vm.Bootstrap()
    worker := NewVMWorker(vm)
    return &BrowseService{worker: worker}
}
```

**Files to create:**
- `server/browse_service_test.go` (~200 lines)
- `server/eval_service_test.go` (~150 lines)
- `server/inspect_service_test.go` (~150 lines)
- `server/modify_service_test.go` (~200 lines)
- `server/session_service_test.go` (~100 lines)
- `server/lsp_test.go` (~300 lines)
- `server/test_helpers_test.go` (~100 lines)

**Possible downsides:**
- Tests may be slow due to VM bootstrap overhead (mitigate with `TestMain` shared setup)
- gRPC/Connect framework mocking may be complex
- LSP protocol compliance testing is non-trivial

**Confidence: 90%** — This is the most impactful improvement. Untested external APIs are a ticking time bomb.

---

### 3. Fuzzing Tests for Parser and Lexer

**What:** Add Go fuzzing tests (`func FuzzXxx`) for the lexer and parser. Parsers are the #1 target for fuzz testing because they process untrusted input with complex state machines.

**Why it's a good improvement:** The parser already has 1,410 test cases, which is excellent, but fuzz testing finds the edge cases humans miss — Unicode handling, deeply nested expressions, pathological token sequences, integer overflow in literals. Go's built-in fuzzing (`go test -fuzz`) makes this nearly free to add.

**Concrete plan:**

```go
// compiler/fuzz_test.go
package compiler

import "testing"

func FuzzLexer(f *testing.F) {
    // Seed with valid Maggie code
    f.Add("Object subclass: Foo instanceVars: x method: bar [^x]")
    f.Add("3 + 4 * 5")
    f.Add("[:a :b | a + b] value: 1 value: 2")
    f.Add("\"\"\"docstring\"\"\" method: foo [^42]")
    f.Add("'hello' , ' ' , 'world'")
    f.Add("#(1 2 3) at: 1")

    f.Fuzz(func(t *testing.T, input string) {
        lexer := NewLexer(input, "fuzz.mag")
        // Must not panic
        for {
            tok := lexer.NextToken()
            if tok.Type == TOKEN_EOF {
                break
            }
        }
    })
}

func FuzzParser(f *testing.F) {
    f.Add("Object subclass: Foo method: bar [^42]")
    f.Add("x := 3. y := x + 1. ^y")
    f.Add("[1 + 2] value")

    f.Fuzz(func(t *testing.T, input string) {
        parser := NewParser(input, "fuzz.mag")
        // Parse must not panic (errors are fine)
        _ = parser.Parse()
    })
}

func FuzzCompileAndRun(f *testing.F) {
    f.Add("^42")
    f.Add("^3 + 4")
    f.Add("^'hello'")

    f.Fuzz(func(t *testing.T, input string) {
        // Wrap in method context
        src := "Object subclass: FuzzClass method: fuzzMethod [" + input + "]"
        parser := NewParser(src, "fuzz.mag")
        ast := parser.Parse()
        if len(parser.Errors()) > 0 {
            return // Parse errors are fine
        }
        // Codegen must not panic
        gen := NewCodeGenerator()
        _ = gen.Generate(ast)
    })
}
```

**Files to create:**
- `compiler/fuzz_test.go` (~80 lines)

**Possible downsides:**
- Fuzz testing can run indefinitely (use `-fuzztime` flag)
- May find issues that are technically parser bugs but don't matter in practice
- Corpus management needs attention over time

**Confidence: 85%** — Fuzzing parsers is a well-established best practice. The only question is whether it finds real bugs or just cosmetic issues.

---

### 4. Proper Super Send in AOT Compilation

**What:** Implement the `OpSendSuper` opcode in the AOT (ahead-of-time) compiler, which is currently a TODO stub at `vm/aot.go:341`.

**Why it's a good improvement:** Super sends are fundamental to Smalltalk — every class that overrides a parent method and calls `super` relies on this. The AOT compiler can't compile any method that uses `super`, which limits what it can optimize.

**Concrete plan:**

The current stub:
```go
// vm/aot.go:341
case OpSendSuper:
    c.writeLine("  // TODO: implement super send properly")
```

The fix requires looking up the method in the superclass's vtable rather than the receiver's class:

```go
case OpSendSuper:
    selectorIdx := int(frame.code[pc+1])
    argCount := int(frame.code[pc+2])
    pc += 3
    selector := frame.method.Literals[selectorIdx]
    selectorName := c.vm.SymbolName(selector.SymbolID())

    // Super send starts lookup from the superclass of the defining class
    c.writeLine("  // Super send: %s (%d args)", selectorName, argCount)
    c.writeLine("  {")
    c.writeLine("    receiver := stack[sp-%d]", argCount)
    c.writeLine("    superClass := vm.ClassOf(receiver).SuperClass()")
    c.writeLine("    method := superClass.LookupMethod(vm.SymbolID(%q))", selectorName)
    c.writeLine("    if method == nil {")
    c.writeLine("      return Nil, fmt.Errorf(\"super: method not found: %%s\", %q)", selectorName)
    c.writeLine("    }")
    // Build args from stack
    c.writeLine("    args := make([]Value, %d)", argCount)
    for i := argCount - 1; i >= 0; i-- {
        c.writeLine("    args[%d] = stack[sp]; sp--", i)
    }
    c.writeLine("    result, err := vm.ExecuteMethod(method, receiver, args...)")
    c.writeLine("    if err != nil { return Nil, err }")
    c.writeLine("    sp++; stack[sp] = result")
    c.writeLine("  }")
```

**Note:** The exact implementation depends on how `SuperClass()` and `LookupMethod()` work in the AOT-generated context. This needs to be cross-referenced with `vm/interpreter.go`'s handling of `OpSendSuper` (around line 800-850).

**Files to modify:**
- `vm/aot.go` (implement super send)
- `vm/aot_test.go` (add test for super send compilation)

**Possible downsides:**
- AOT compilation of super sends is tricky because the "defining class" must be known at compile time, which requires tracking the method's owner class
- May need to pass the defining class as metadata to the AOT-compiled function

**Confidence: 70%** — The concept is straightforward but Smalltalk super semantics (start lookup from defining class's super, not receiver's class's super) have subtle edge cases.

---

### 7. Stack Overflow Protection

**What:** Add bounds checking to the interpreter's stack push operations and provide a configurable stack depth limit with clear error messages instead of panics.

**Why it's a good improvement:** The interpreter uses a fixed 1024-element stack and 256-frame limit (`vm/interpreter.go`). There's no bounds checking on push, meaning deeply recursive Maggie programs cause Go panics instead of Maggie exceptions. This is a crash bug that affects any user who writes recursive code that's too deep.

**Concrete plan:**

```go
// vm/interpreter.go - Add stack bounds checking

const (
    DefaultMaxStackDepth = 1024
    DefaultMaxFrameDepth = 256
)

// In the interpreter's push operation (or wherever sp is incremented):
func (interp *Interpreter) push(value Value) error {
    interp.sp++
    if interp.sp >= len(interp.stack) {
        return &StackOverflowError{
            Depth:    interp.sp,
            MaxDepth: len(interp.stack),
            Method:   interp.currentMethodName(),
        }
    }
    interp.stack[interp.sp] = value
    return nil
}

// Better: Raise a Maggie-level exception so user code can catch it
// In OpSend / method call setup:
if interp.frameCount >= interp.maxFrames {
    return interp.signalException("StackOverflow",
        fmt.Sprintf("stack depth exceeded %d frames (in %s)",
            interp.maxFrames, methodName))
}
```

Also make the limits configurable:

```go
type VMConfig struct {
    MaxStackDepth int // default 1024
    MaxFrameDepth int // default 256
}
```

**Files to modify:**
- `vm/interpreter.go` (add bounds checks)
- `vm/vm.go` (add configurable limits)
- `vm/interpreter_test.go` (test stack overflow produces clean error)

**Possible downsides:**
- Bounds checking on every push adds a branch to the hot path (mitigate: check only on frame push, not every value push)
- May break existing tests that rely on deep recursion
- Need to decide: Go panic vs Maggie exception vs configurable behavior

**Confidence: 88%** — Stack overflow protection is a clear correctness improvement. The only design question is the error mechanism.

---

### 8. Tail-Call Optimization (TCO)

**What:** Implement tail-call optimization in the bytecode interpreter so that methods ending with a self-recursive message send reuse the current frame instead of pushing a new one.

**Why it's a good improvement:** Smalltalk relies heavily on recursion (no `for` loops — iteration is done via message sends like `whileTrue:`). Without TCO, even simple loops like `[x > 0] whileTrue: [x := x - 1]` consume stack frames proportional to the number of iterations. Combined with #7's stack overflow protection, this prevents the stack limit from being a practical obstacle.

**Concrete plan:**

Detect tail calls at compile time in the bytecode generator:

```go
// compiler/codegen.go - Mark tail calls during bytecode generation

// When generating a send that is the last expression before a return:
func (g *CodeGenerator) generateSend(send *SendNode, isTailPosition bool) {
    // ... generate receiver and arguments as normal ...

    if isTailPosition && g.isSelfRecursive(send) {
        g.emit(OpTailSend, selectorIdx, argCount)
    } else {
        g.emit(OpSend, selectorIdx, argCount)
    }
}
```

Handle the new opcode in the interpreter:

```go
// vm/interpreter.go
case OpTailSend:
    selectorIdx := int(frame.code[pc+1])
    argCount := int(frame.code[pc+2])

    // Instead of pushing a new frame, reuse the current one:
    // 1. Pop arguments from stack
    // 2. Reset frame's pc to 0
    // 3. Set arguments as new locals
    // 4. Continue execution

    method := lookupMethod(receiver, selector)
    if method == frame.method {
        // Self-recursive tail call: reuse frame
        frame.pc = 0
        copy(frame.locals[:argCount], args)
        frame.sp = argCount
        continue // restart the frame loop
    }
    // Otherwise fall back to normal send
    // ...
```

**Files to modify:**
- `vm/bytecode.go` (add `OpTailSend` opcode)
- `compiler/codegen.go` (detect tail position, emit `OpTailSend`)
- `vm/interpreter.go` (handle `OpTailSend`)
- Add tests for tail-recursive factorial, fibonacci

**Possible downsides:**
- Complicates the bytecode and interpreter
- Debugging is harder with TCO (stack frames disappear)
- Only works for direct self-recursion initially; mutual recursion is much harder
- May break debugger expectations about stack frames

**Confidence: 65%** — Technically sound, but adds complexity to the interpreter's hot path. The benefit depends on how often users write tail-recursive code vs using blocks/closures for iteration.

---

### 12. HTTP/gRPC Primitive Tests

**What:** Add test suites for `vm/http_primitives.go` (561 lines) and `vm/grpc_primitives.go` (1,053 lines), which currently have zero dedicated tests.

**Why it's a good improvement:** These are the network-facing primitives — HTTP client/server and gRPC client/server functionality. Bugs here mean broken network requests, silent data corruption, or security vulnerabilities. 1,614 lines of network code without any tests is a significant risk.

**Concrete plan:**

```go
// vm/http_primitives_test.go
package vm

import (
    "net/http"
    "net/http/httptest"
    "testing"
)

func TestHTTPGet(t *testing.T) {
    // Create a test server
    ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        w.Header().Set("Content-Type", "application/json")
        w.Write([]byte(`{"status":"ok"}`))
    }))
    defer ts.Close()

    vm := newTestVM(t)
    // Execute Maggie code that does an HTTP GET
    result := vm.EvalString(fmt.Sprintf(`
        response := HTTP get: '%s'.
        response body
    `, ts.URL))

    if result.String() != `{"status":"ok"}` {
        t.Errorf("HTTP GET body = %q, want %q", result.String(), `{"status":"ok"}`)
    }
}

func TestHTTPPost(t *testing.T) {
    var receivedBody string
    ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        body, _ := io.ReadAll(r.Body)
        receivedBody = string(body)
        w.WriteHeader(201)
    }))
    defer ts.Close()

    vm := newTestVM(t)
    result := vm.EvalString(fmt.Sprintf(`
        HTTP post: '%s' body: 'hello world'
    `, ts.URL))

    if receivedBody != "hello world" {
        t.Errorf("server received %q, want %q", receivedBody, "hello world")
    }
}

func TestHTTPTimeout(t *testing.T) {
    ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        time.Sleep(5 * time.Second) // Hang
    }))
    defer ts.Close()

    vm := newTestVM(t)
    // Should timeout, not hang forever
    result := vm.EvalString(fmt.Sprintf(`
        HTTP get: '%s' timeout: 100
    `, ts.URL))
    // Expect an error result
}
```

**Files to create:**
- `vm/http_primitives_test.go` (~300 lines)
- `vm/grpc_primitives_test.go` (~400 lines)

**Possible downsides:**
- Network tests can be flaky (mitigate: use `httptest` for HTTP, in-process for gRPC)
- gRPC test setup is more complex (requires proto compilation)
- May reveal bugs that need fixing (that's actually the point)

**Confidence: 85%** — Network code needs tests. `httptest` makes HTTP testing straightforward. gRPC testing is harder but well-established.

---

### 14. Garbage Collection for Global Registries

**What:** Add automatic cleanup for the global registries (channelRegistry, processRegistry, blockRegistry, exceptionRegistry) that currently leak memory when Maggie objects are garbage collected.

**Why it's a good improvement:** Every channel, process, block, and exception created in Maggie allocates entries in global `sync.Mutex`-protected maps that are never cleaned up. Long-running programs (servers, REPLs, IDE sessions) will slowly leak memory. This is particularly bad for the LSP server use case where sessions may create thousands of objects.

**Concrete plan:**

Use Go's `runtime.SetFinalizer` to hook into GC:

```go
// vm/concurrency_registry.go

type RegistryEntry struct {
    value interface{}
    // weak reference tracking
}

// When registering a channel:
func (r *ChannelRegistry) Register(ch *MaggieChannel) ChannelID {
    r.mu.Lock()
    id := r.nextID
    r.nextID++
    r.channels[id] = ch
    r.mu.Unlock()

    // Register finalizer so GC cleans up
    runtime.SetFinalizer(ch, func(ch *MaggieChannel) {
        r.mu.Lock()
        delete(r.channels, id)
        r.mu.Unlock()
    })

    return id
}
```

Alternative approach — periodic sweep with weak references:

```go
// vm/registry_gc.go

func (r *ChannelRegistry) Sweep() int {
    r.mu.Lock()
    defer r.mu.Unlock()

    collected := 0
    for id, ch := range r.channels {
        if ch.IsClosed() && ch.IsEmpty() && ch.RefCount() == 0 {
            delete(r.channels, id)
            collected++
        }
    }
    return collected
}

// Run periodically or after N allocations
func (vm *VM) registryGC() {
    ticker := time.NewTicker(30 * time.Second)
    defer ticker.Stop()
    for range ticker.C {
        vm.channelRegistry.Sweep()
        vm.processRegistry.Sweep()
        vm.blockRegistry.Sweep()
    }
}
```

**Files to modify:**
- `vm/concurrency_registry.go` (add cleanup logic)
- `vm/vm.go` (start registry GC goroutine)
- `vm/concurrency_registry_test.go` (test cleanup)

**Possible downsides:**
- `runtime.SetFinalizer` has subtle semantics (no guarantee of timing, can't finalize objects with cycles)
- Periodic sweep needs careful tuning (too frequent = CPU waste, too infrequent = memory waste)
- Must ensure no dangling references after cleanup (other goroutines may still hold registry IDs)

**Confidence: 75%** — The problem is real, but the solution has tricky edge cases. The periodic sweep approach is simpler and safer than finalizers.

---

### 15. Block Variable Captures in Maggie Compiler

**What:** Implement proper variable capture (cell boxing) in the experimental self-hosted Maggie compiler, which currently can't compile blocks that reference outer scope variables.

**Why it's a good improvement:** This is THE blocking issue for Maggie's self-hosting story. Without variable captures, the Maggie compiler can't compile `whileTrue:` loops, closures that mutate state, or any non-trivial block. The test suite explicitly skips these cases with the comment "Maggie compiler doesn't yet implement proper variable captures."

**Concrete plan:**

This requires changes to the Maggie-side compiler (in `.mag` source files, not the Go code). The approach mirrors what the Go compiler already does in `compiler/codegen.go`:

1. **Variable analysis pass:** Before generating bytecode, scan blocks for variables that are read/written from enclosing scopes
2. **Cell boxing:** Variables that are captured by blocks get allocated in heap cells instead of stack slots
3. **Capture instructions:** Generate `OpLoadCapture`/`OpStoreCapture` instead of `OpLoadLocal`/`OpStoreLocal` for captured variables

The Go compiler's implementation in `codegen.go:1045` shows the pattern:
```
// variables now go through captures (not HomeBP) so blocks can outlive
```

**Files to modify:**
- Maggie compiler source files (in `lib/` or wherever the self-hosted compiler lives)
- `cmd/bootstrap/compiler_selfcompile_test.go` (un-skip capture tests)

**Possible downsides:**
- Complex feature with many edge cases (nested captures, capture of captures)
- The self-hosted compiler is already 10-50x slower; adding capture analysis may make it worse
- Risk of subtle bugs where captured variables behave differently than Go-compiled ones

**Confidence: 60%** — Important for the self-hosting goal, but complex to implement correctly. Would need extensive testing against the Go compiler's behavior as a reference.

---

### 16. CI/CD Pipeline

**What:** Add a GitHub Actions CI pipeline that runs all tests, reports coverage, and optionally runs fuzzing on the parser.

**Why it's a good improvement:** Every other improvement on this list is only sustainable if there's automated testing to prevent regressions. Currently, tests only run when someone remembers to run `go test ./...` locally. A CI pipeline catches regressions immediately, tracks coverage over time, and gives confidence that PRs don't break things.

**Concrete plan:**

```yaml
# .github/workflows/ci.yml
name: CI
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-go@v5
        with:
          go-version: '1.22'

      - name: Run tests
        run: go test -race -coverprofile=coverage.out ./...

      - name: Coverage report
        run: |
          go tool cover -func=coverage.out
          # Fail if total coverage drops below threshold
          COVERAGE=$(go tool cover -func=coverage.out | grep total | awk '{print $3}' | tr -d '%')
          echo "Total coverage: ${COVERAGE}%"
          if (( $(echo "$COVERAGE < 40" | bc -l) )); then
            echo "Coverage below 40% threshold"
            exit 1
          fi

      - name: Run vet
        run: go vet ./...

      - name: Run staticcheck
        uses: dominikh/staticcheck-action@v1

  fuzz:
    runs-on: ubuntu-latest
    if: github.event_name == 'push' && github.ref == 'refs/heads/main'
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-go@v5
        with:
          go-version: '1.22'
      - name: Fuzz parser (5 minutes)
        run: go test -fuzz=FuzzParser -fuzztime=5m ./compiler/
```

**Files to create:**
- `.github/workflows/ci.yml` (~60 lines)

**Possible downsides:**
- CI requires the bootstrap image for some tests (need to handle `maggie.image` not being present)
- gRPC tests may need proto compilation in CI
- Fuzzing in CI costs compute time

**Confidence: 95%** — This is table stakes for any serious project. Almost zero downside.

---

### 21. End-to-End Integration Tests

**What:** Create a suite of integration tests that exercise complete Maggie programs from source code through compilation to execution, testing the full stack.

**Why it's a good improvement:** Current tests are mostly unit-level (test the lexer, test the parser, test individual primitives). There are few tests that verify a complete program works end-to-end. This catches integration issues where individual components work correctly in isolation but fail when combined.

**Concrete plan:**

```go
// integration_test.go (at project root)
package maggie_test

import (
    "testing"
    "github.com/chazu/maggie/compiler"
    "github.com/chazu/maggie/vm"
)

func runMaggie(t *testing.T, source string) vm.Value {
    t.Helper()
    v := vm.NewVM()
    v.Bootstrap()
    result, err := v.EvalString(source)
    if err != nil {
        t.Fatalf("eval error: %v", err)
    }
    return result
}

func TestFactorial(t *testing.T) {
    result := runMaggie(t, `
        Object subclass: Math
          method: factorial: n [
            n <= 1 ifTrue: [^1].
            ^n * (self factorial: n - 1)
          ]

        Math new factorial: 10
    `)
    if result.SmallInt() != 3628800 {
        t.Errorf("10! = %d, want 3628800", result.SmallInt())
    }
}

func TestChannelPingPong(t *testing.T) {
    result := runMaggie(t, `
        ch := Channel new.
        [ch send: 42] fork.
        ch receive
    `)
    if result.SmallInt() != 42 {
        t.Errorf("channel receive = %d, want 42", result.SmallInt())
    }
}

func TestExceptionHandling(t *testing.T) {
    result := runMaggie(t, `
        [1 / 0] on: ZeroDivideError do: [:e | 'caught']
    `)
    if result.String() != "caught" {
        t.Errorf("exception result = %q, want %q", result.String(), "caught")
    }
}

func TestNamespaceImport(t *testing.T) {
    result := runMaggie(t, `
        namespace: 'TestNS'
        Object subclass: Greeter
          method: greet [^'hello from namespace']

        import: 'TestNS'
        Greeter new greet
    `)
    if result.String() != "hello from namespace" {
        t.Errorf("namespace import = %q, want %q", result.String(), "hello from namespace")
    }
}
```

**Files to create:**
- `integration_test.go` (~500 lines, covering 20-30 real programs)

**Possible downsides:**
- Integration tests are slower than unit tests
- May be fragile if internal APIs change
- Overlap with existing VM tests (some de-duplication needed)

**Confidence: 85%** — Integration tests find real bugs. The main question is scoping them to avoid overlap with existing VM-level tests.

---

### 23. Benchmarking Infrastructure with Regression Detection

**What:** Add Go benchmark tests for critical paths (message dispatch, block evaluation, channel operations) and a script that compares benchmark results against a baseline to detect regressions.

**Why it's a good improvement:** There's already a `benchmark_test.go` but it shows the Maggie compiler is 10,000x slower than the Go compiler. Without regular benchmarking, performance can degrade silently. Go's built-in benchmarking and `benchstat` tool make this straightforward.

**Concrete plan:**

```go
// vm/benchmark_test.go (expand existing)

func BenchmarkMessageSend(b *testing.B) {
    vm := newBenchVM(b)
    vm.EvalString(`Object subclass: Counter instanceVars: n
        method: init [n := 0]
        method: increment [n := n + 1]
        method: value [^n]
    `)
    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        vm.EvalString("Counter new increment increment increment value")
    }
}

func BenchmarkBlockEvaluation(b *testing.B) {
    vm := newBenchVM(b)
    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        vm.EvalString("[:x | x * x] value: 42")
    }
}

func BenchmarkChannelThroughput(b *testing.B) {
    vm := newBenchVM(b)
    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        vm.EvalString(`
            ch := Channel new: 100.
            100 timesRepeat: [ch send: 1].
            100 timesRepeat: [ch receive]
        `)
    }
}
```

Add a benchmark comparison script:

```bash
#!/bin/bash
# scripts/bench-compare.sh
go test -bench=. -count=10 ./vm/ > new.txt
benchstat baseline.txt new.txt
```

**Files to create/modify:**
- `vm/benchmark_test.go` (expand with targeted benchmarks)
- `scripts/bench-compare.sh` (~10 lines)
- `benchmarks/baseline.txt` (committed baseline results)

**Possible downsides:**
- Benchmarks are noisy on different hardware
- `benchstat` requires statistical significance (need -count=10)
- Maintaining baselines requires discipline

**Confidence: 80%** — Benchmarking infrastructure is valuable but only if maintained. Go's tooling makes the technical side easy; the challenge is making it a habit.

---

### 25. File I/O Primitive Tests

**What:** Add tests for `vm/file_primitives.go`, which handles file reading, writing, and directory operations.

**Why it's a good improvement:** File I/O primitives operate on the real filesystem. Bugs here can cause data loss, security issues (path traversal), or silent corruption. These are untested despite handling user data.

**Concrete plan:**

```go
// vm/file_primitives_test.go
package vm

import (
    "os"
    "path/filepath"
    "testing"
)

func TestFileRead(t *testing.T) {
    dir := t.TempDir()
    path := filepath.Join(dir, "test.txt")
    os.WriteFile(path, []byte("hello world"), 0644)

    vm := newTestVM(t)
    result := vm.EvalString(fmt.Sprintf("File read: '%s'", path))
    if result.String() != "hello world" {
        t.Errorf("File read = %q, want %q", result.String(), "hello world")
    }
}

func TestFileWrite(t *testing.T) {
    dir := t.TempDir()
    path := filepath.Join(dir, "output.txt")

    vm := newTestVM(t)
    vm.EvalString(fmt.Sprintf("File write: '%s' contents: 'test data'", path))

    data, err := os.ReadFile(path)
    if err != nil {
        t.Fatalf("reading written file: %v", err)
    }
    if string(data) != "test data" {
        t.Errorf("written data = %q, want %q", string(data), "test data")
    }
}

func TestFileReadNonexistent(t *testing.T) {
    vm := newTestVM(t)
    result := vm.EvalString("File read: '/nonexistent/path/file.txt'")
    // Should return error or nil, not panic
    // Verify graceful error handling
}
```

**Files to create:**
- `vm/file_primitives_test.go` (~200 lines)

**Possible downsides:**
- File tests need careful temp directory management (Go's `t.TempDir()` handles this)
- Platform-specific behavior (permissions differ on Windows vs Unix)
- May reveal bugs in the file primitives that need fixing

**Confidence: 90%** — File I/O tests are straightforward and high-value.

---

### 30. Source Code Formatter (`mag fmt`)

**What:** Implement a `mag fmt` subcommand that automatically formats Maggie source code to a canonical style, similar to `gofmt`, `rustfmt`, or `mix format`.

**Why it's a good improvement:** Go proved that an opinionated auto-formatter eliminates style debates, makes code reviews faster, and makes all Maggie code look consistent. Since Maggie has a parser that produces a complete AST, a formatter is a matter of walking the AST and printing it with consistent rules.

**Concrete plan:**

The formatter reads Maggie source, parses it to AST, then pretty-prints with canonical formatting rules:

```go
// cmd/mag/format.go

type Formatter struct {
    indent     int
    buf        bytes.Buffer
    lineWidth  int // target 80 chars
}

func (f *Formatter) FormatFile(source string) (string, error) {
    parser := compiler.NewParser(source, "format")
    ast := parser.Parse()
    if errs := parser.Errors(); len(errs) > 0 {
        return "", fmt.Errorf("parse errors: %v", errs)
    }
    f.formatNode(ast)
    return f.buf.String(), nil
}

func (f *Formatter) formatClassDef(node *ClassDefNode) {
    // ClassName subclass: SubClass
    //   instanceVars: var1 var2 var3
    //
    //   method: name [
    //     body
    //   ]
    f.write("%s subclass: %s\n", node.SuperClass, node.Name)
    if len(node.InstanceVars) > 0 {
        f.indent++
        f.write("instanceVars: %s\n", strings.Join(node.InstanceVars, " "))
    }
    f.write("\n")
    for _, method := range node.Methods {
        f.formatMethod(method)
        f.write("\n")
    }
}

// Formatting rules:
// 1. One blank line between methods
// 2. Two-space indentation inside method bodies
// 3. Space before and after binary operators
// 4. Keyword messages: each keyword on same line if fits, otherwise wrapped
// 5. Block brackets: [ on same line, ] on own line for multi-line blocks
// 6. Cascades: each ; on new line, indented
```

CLI integration:

```go
// In cmd/mag/main.go, add format subcommand
case "fmt":
    paths := flag.Args()[1:]
    for _, path := range paths {
        source, _ := os.ReadFile(path)
        formatted, err := formatter.FormatFile(string(source))
        if err != nil {
            fmt.Fprintf(os.Stderr, "%s: %v\n", path, err)
            continue
        }
        os.WriteFile(path, []byte(formatted), 0644)
    }
```

**Files to create:**
- `cmd/mag/format.go` (~300 lines)
- `cmd/mag/format_test.go` (~200 lines)

**Possible downsides:**
- Formatting is opinionated — the community (however small) may disagree on rules
- Comments are hard to preserve through AST round-tripping (need to attach comments to AST nodes)
- Docstrings need special handling
- May reformat existing `.mag` files in the lib/ directory, creating a large diff

**Confidence: 70%** — High value but comment preservation is the hard part. If the parser doesn't attach comments to AST nodes, this becomes much harder. Recommend implementing first without comments, then adding comment preservation.

---

## Summary: Priority-Ordered Recommendations

| Priority | Idea | Confidence | Impact |
|----------|------|------------|--------|
| 1 | CI/CD pipeline (#16) | 95% | Foundation for all other improvements |
| 2 | Server/LSP tests (#1) | 90% | Critical: external API completely untested |
| 3 | File I/O tests (#25) | 90% | Data safety |
| 4 | Stack overflow protection (#7) | 88% | Crash prevention |
| 5 | HTTP/gRPC tests (#12) | 85% | Network reliability |
| 6 | Fuzzing tests (#3) | 85% | Bug discovery |
| 7 | Integration tests (#21) | 85% | Full-stack verification |
| 8 | Benchmarking (#23) | 80% | Performance sustainability |
| 9 | Registry GC (#14) | 75% | Memory leak prevention |
| 10 | `mag fmt` (#30) | 70% | Developer experience |
| 11 | AOT super send (#4) | 70% | Feature completeness |
| 12 | TCO (#8) | 65% | Deep recursion support |
| 13 | Block captures in Maggie compiler (#15) | 60% | Self-hosting milestone |
