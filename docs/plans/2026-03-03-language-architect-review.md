# Language Architect Review — Maggie VM

**Date:** 2026-03-03
**Scope:** Full codebase review covering compiler, VM, concurrency, distribution, Go interop, image persistence, and CLI tooling.

---

## 1. Interpreter / VM Core

### 1a. Non-Local Return via panic/recover — Major Performance and Correctness Concern

**Files:** `vm/interpreter.go` lines 458-483, 1162-1183, 1265-1282

Every compiled method invocation in `send()` installs a `defer/recover` to catch `NonLocalReturn` panics. This is the single most expensive pattern in the interpreter. Go's `defer` has overhead on every call even when nothing panics, and `recover()` prevents the Go compiler from performing certain optimizations (inlining, escape analysis). Since `send()` is the hot path for every message dispatch to a compiled method, this adds measurable overhead to every Maggie method call.

The same pattern is duplicated three times: in `Execute()`, `send()`, and `sendSuper()`. Each has an identical defer/recover block. This is both a performance problem and a maintenance burden.

**Alternative:** Most modern Smalltalk VMs use an explicit return-address stack or a "return target" field on the call frame, checked in the main loop. When `OpReturnTop` fires in a block, the interpreter can unwind frames in a loop rather than using panic/recover. This would eliminate the defer overhead from the hot path entirely.

### 1b. VTable Lookup Is O(N) in Hierarchy Depth

**File:** `vm/vtable.go` lines 14-23

`VTable.Lookup()` walks the parent chain linearly. For deep hierarchies (common in Smalltalk), this is slow. The inline cache in `interpreter.go` mitigates this for monomorphic sites, but polymorphic and megamorphic sites still pay the full cost. Consider flattening the VTable at class creation time (copy parent methods into child) so all lookups are O(1). The trade-off is memory and mutation complexity, but it is what most production Smalltalk VMs do.

### 1c. VTable Method Array Indexed by Global Selector ID

**File:** `vm/vtable.go` line 36-44

The method array is indexed by selector ID, which is a globally incrementing counter. If the program uses 500 unique selectors, every VTable (even for a class with 3 methods) has up to 500 slots. For a system with hundreds of classes this wastes significant memory. Consider a hash-based or compact method dictionary for the VTable, falling back to the parent chain.

### 1d. Stack and Frame Growth Strategy

**File:** `vm/interpreter.go` lines 253-260, 349-354

The stack starts at 1024 and doubles when full. The frame stack starts at 256 and doubles. This is fine, but the growth path allocates a new slice and copies. For hot loops that push the stack near its boundary, this can cause repeated reallocations. Consider pre-allocating closer to the configured max, or using a segmented stack design.

### 1e. `popN()` Allocates Every Time

**File:** `vm/interpreter.go` lines 279-287

`popN()` creates a new `[]Value` slice on every call. This is called on every `OpSend` and `OpTailSend` (for arguments), making it a significant source of GC pressure. Consider returning a view into the stack (a sub-slice) instead, or using a pre-allocated argument buffer on the interpreter.

### 1f. `sendBinaryFallback` Missing NLR Protection

**File:** `vm/interpreter.go` lines 1678-1693

When `sendBinaryFallback` dispatches a compiled method via `pushFrame` + `runFrame`, it does not install a defer/recover for NonLocalReturn. If the target method contains a block that does `^value`, the NLR panic will escape unhandled and likely crash the VM. This is a correctness bug. The optimized send opcodes (`OpSendPlus`, `OpSendLT`, etc.) all go through this path when the fast primitive check fails.

### 1g. Integer Overflow Not Checked

**File:** `vm/interpreter.go` lines 1695-1697

`primitivePlus`, `primitiveMinus`, `primitiveTimes` do not check for overflow of 48-bit SmallInts. If `a.SmallInt() + b.SmallInt()` exceeds `MaxSmallInt`, `FromSmallInt` silently truncates the result. A correct implementation should promote to BigInt or Float on overflow. This is a known gap in most early Smalltalk VMs but worth flagging.

---

## 2. Object Registry — Scalability Bottleneck

### 2a. Registry Explosion

**File:** `vm/object_registry.go` — 1071 lines, 25+ separate registries

The ObjectRegistry is a God Object. It has grown to include separate maps and mutexes for: exceptions, results, contexts, dictionaries, strings, gRPC clients, gRPC streams, HTTP servers, HTTP requests, HTTP responses, cells, class vars, external processes, unix listeners, unix connections, JSON readers, JSON writers, GoObjects, CUE contexts, CUE values, class values — plus the embedded ConcurrencyRegistry (channels, processes, blocks, mutexes, waitgroups, semaphores, cancellation contexts).

Each map has its own `sync.RWMutex` and `atomic` ID counter. This is approximately 50+ synchronization primitives on a single struct. The struct is difficult to reason about, test in isolation, or extend.

**Recommendation:** Factor each registry into its own type behind a common interface. The ObjectRegistry can become a facade that delegates.

### 2b. String Registry Under Mutex for Every String Operation

The string registry wraps every string in a `StringObject` behind a mutex-protected map lookup. Since strings are the most common heap-allocated values in most programs, this is a serious contention point. Every string comparison in `primitiveEQ` does two mutex-guarded map lookups (line 1836). Consider interning strings or using a lock-free concurrent map.

### 2c. Registry GC Is Time-Based, Not Pressure-Based

**File:** `vm/registry_gc.go`

The RegistryGC sweeps every 30 seconds regardless of allocation pressure. This means: (a) short-lived programs waste time on unnecessary sweeps, and (b) programs that allocate rapidly can accumulate huge registries between sweeps. Consider triggering sweeps based on registry size thresholds.

---

## 3. NaN-Boxing and Value Representation

### 3a. Symbol-Space Overloading

**File:** `vm/markers.go`

The symbol tag space (3 bits of tag + 48 bits payload) is being used to encode 20+ distinct value types via marker bytes in the upper 8 bits of the symbol ID. This means the `IsSymbol()` check returns true for channels, processes, mutexes, exceptions, strings, dictionaries, class values, HTTP objects, etc. The `vtableForLegacy` function in `interpreter.go` (lines 1352-1461) is a 100-line chain of `if isXValue(v)` checks — a direct consequence of this design.

The `vtableForVM` path (lines 1304-1349) is better thanks to `SymbolDispatch`, but the legacy path remains and is used by standalone interpreters in tests.

This overloading also means that all of these types share the same 24-bit ID space per marker. With only 24 bits, you can have at most 16M strings, 16M channels, etc. For strings this is a real concern in long-running programs.

### 3b. No BigInteger Support

48-bit SmallInts are the only integer representation. There is no BigInteger or LargePositiveInteger class. Any computation that exceeds the 48-bit range silently produces wrong results. This is a fundamental gap for a Smalltalk-family language.

---

## 4. Compiler

### 4a. No Optimizer

There is no `optimizer.go` file — the compiler goes directly from AST to bytecode with no optimization passes. This means:

- No constant folding (`3 + 4` generates `OpPushInt8 3`, `OpPushInt8 4`, `OpSendPlus` instead of `OpPushInt8 7`)
- No dead code elimination
- No common subexpression elimination
- No loop-invariant code motion
- No inlining of simple methods (like accessors)

The TCO in `compileStmt` is the only optimization, and it only handles direct self-recursion in tail position.

### 4b. Block Compilation State Machine Complexity

**File:** `compiler/codegen.go` lines 714-907

The `compileBlock` function is nearly 200 lines and manages state via manual save/restore of 15+ fields on the Compiler struct. This is fragile and error-prone. If any new field is added to the Compiler, it must be added to both the save and restore sections, and forgetting it causes subtle bugs. A cleaner approach would be to create a new Compiler instance (or a CompilationContext struct) for each block scope.

### 4c. Cell Variable Analysis Is Conservative

**File:** `compiler/codegen.go` lines 1020-1149

The `findCellVariables` function walks the entire method AST to find variables that need cell boxing. This is correct but the analysis is O(method_size * nesting_depth) due to redundant walks of nested blocks. For deeply nested blocks with many variables, this could be expensive. Not critical but worth noting.

### 4d. Literal Dedup Uses `uint64` Key

**File:** `compiler/codegen.go` line 423

`addLiteral` deduplicates by casting `vm.Value` to `uint64`. This works for most value types but is fragile for string values, since two different string values with the same registry ID would collide (correct behavior), but the comment does not document why this is safe.

### 4e. No Source Map / Debug Info

The compiler preserves `SourceText` on method definitions but there is no bytecode-to-source-position mapping. This means: no line numbers in stack traces (only frame index and IP), no debugger step-through, and the LSP server cannot provide precise error locations for runtime errors.

---

## 5. Parser

### 5a. Error Recovery Is Minimal

**File:** `compiler/parser.go`

When the parser encounters an error, it records it and often returns `nil`. There is no synchronization or panic-mode recovery. This means a single syntax error often cascades into many spurious errors, and the parser cannot continue to find additional real errors in the same file. The `parseStatementsUntilBracket` method (lines 1233-1252) does break on unexpected tokens, but this is ad hoc rather than systematic.

### 5b. Source File Parsing Ambiguity

**File:** `compiler/parser.go` lines 975-983

The `parseClassDefBody` function detects the end of a class definition by checking if the next token pair looks like `Identifier "subclass:"` or `Identifier "trait"`. This heuristic can be fooled by instance variable names like `trait` or `subclass` at the wrong position, and it means the grammar is not context-free at this level. The fix in commit `3cb00fd` (preventing `class` from being parsed as an ivar name) hints at this being a recurring issue.

---

## 6. Concurrency

### 6a. Globals Map Is Not Thread-Safe

**Files:** `vm/vm.go` line 22, `vm/interpreter.go` lines 193, 208

`vm.Globals` is a plain `map[string]Value` shared across all interpreters (including forked ones running in separate goroutines). Forked interpreters write to `localWrites` (correct isolation), but the main interpreter writes directly to `Globals` without synchronization (line 679). If the main interpreter and a forked interpreter concurrently read and write globals, this is a data race. The `interpreters` field uses `sync.Map`, but `Globals` itself does not.

### 6b. Channel Send to Closed Channel Panics in Go

**File:** `vm/concurrency.go` lines 140-150

The `primSend:` primitive checks `ch.closed.Load()` before sending, but there is a TOCTOU race: the channel could be closed between the check and the actual `ch.ch <- val`. Sending to a closed Go channel panics. This should be wrapped in a recover or use a more careful synchronization pattern (e.g., holding the channel's mutex during send).

### 6c. Process Error Swallowing

**File:** `vm/concurrency.go` lines 380-389

When a forked process panics with anything other than `NonLocalReturn`, the error is silently swallowed (`proc.markDone(Nil, nil)` with nil error). This makes debugging concurrent code extremely difficult. The error should at minimum be logged, and ideally stored on the ProcessObject so `proc wait` can report it.

### 6d. No Deadlock Detection

There is no mechanism to detect when all goroutines are blocked on channels or mutexes. A simple program with two goroutines waiting on each other will hang silently forever. Most Go programs can rely on the Go runtime's deadlock detector, but forked Maggie processes running inside the VM prevent the Go detector from seeing the full picture.

---

## 7. Image Persistence

### 7a. No Backward Compatibility Strategy

**File:** `vm/image_writer.go` line 24

The image version is 4, with versions 1-3 preceding it. The reader checks for exact version match. There is no migration path — if you have a v3 image, you cannot load it in a v4 runtime. The MEMORY.md notes that "image version bumps only need 2 image files updated," but there is no automated migration tool. As features are added, old images become orphaned.

### 7b. Image Reads Entire File Into Memory

**File:** `vm/image_reader.go` line 84-91

`NewImageReader` calls `io.ReadAll(r)`, loading the entire image into memory. For large images (megabytes of compiled methods and class data), this is wasteful — the data could be memory-mapped or streamed. Not critical for current image sizes but will matter as the language grows.

---

## 8. CLI / main.go — God Object

**File:** `cmd/mag/main.go` — 1523 lines

This file handles: argument parsing, subcommand dispatch (deps, fmt, wrap, build, sync, doc, doctest, lsp), VM initialization, image loading, source compilation, REPL, project manifest loading, dependency resolution, namespace prefixing, and Yutani IDE integration. It is the largest non-generated file in the project and continues to grow. Functions like `compileAll` and `loadProject` contain critical pipeline logic that is untestable because it is deeply coupled to CLI state.

**Recommendation:** Extract a `pipeline` package for the compilation pipeline (skeleton registration, superclass resolution, method compilation). Extract a `repl` package. Keep `main.go` as a thin dispatcher.

---

## 9. Missing Language Features

For a Smalltalk-family language, these are notably absent:

1. **Become:** (identity swapping) — the `forward` field on Object suggests this was planned but is not wired up
2. **BigInteger / LargePositiveInteger** — arithmetic silently wraps at 48 bits
3. **Metaclass hierarchy** — the comment at `vm.go:191` says "we skip full metaclass support for simplicity." Class-side methods exist but there are no first-class metaclasses
4. **Method categories / protocols** — no organizational structure for methods within a class
5. **Exception resume/retry/pass** — only `on:do:` catching; the exception handler does not support `retry`, `resume:`, or `pass`
6. **Ordered/Sorted collections** — no SortedCollection, Set, Bag, etc. in the core library
7. **Proper block closures over mutable state** — the cell mechanism exists but the analysis only handles variables assigned in nested blocks, not variables shared across sibling blocks at the same nesting level
8. **Finalization** — no mechanism for cleanup when objects are garbage collected (weakRefs exist but no real GC integration with Go's finalizers)

---

## 10. Prioritized Improvement Recommendations

**Priority 1 — Correctness (bugs that produce wrong results or crashes):**

1. Fix `sendBinaryFallback` missing NLR protection (`interpreter.go:1678-1693`). This will crash the VM when optimized binary ops dispatch to Maggie methods containing blocks with `^`.
2. Fix the TOCTOU race in channel `primSend:` (`concurrency.go:140-150`). This will panic on send-to-closed-channel under concurrency.
3. Fix integer overflow in arithmetic primitives. Silent truncation produces wrong results.
4. Add synchronization to `Globals` map for the main interpreter, or document that concurrent writes from the main goroutine are not supported.

**Priority 2 — Performance (measurable impact on real programs):**

5. Eliminate defer/recover from the `send()` hot path. Use an explicit return-target mechanism on the call frame. This is the single biggest performance win available.
6. Eliminate `popN()` slice allocation by returning stack sub-slices or using a pre-allocated buffer.
7. Consider flattening VTables (copy parent methods into child) for O(1) dispatch without inline caching overhead.
8. Profile and reduce string registry contention (lock-free map or string interning).

**Priority 3 — Architecture (design debt that slows development):**

9. Break up `ObjectRegistry` into focused, independently testable registries.
10. Extract compilation pipeline from `cmd/mag/main.go` into a `pipeline` package.
11. Add bytecode-to-source-position mapping for meaningful stack traces and debugger support.
12. Add a peephole optimizer pass between codegen and final bytecode emission (constant folding, dead code elimination, instruction combining).

**Priority 4 — Language completeness:**

13. Implement BigInteger with automatic promotion from SmallInt on overflow.
14. Implement full metaclass hierarchy.
15. Add exception resume/retry/pass semantics.

---

This is a substantial codebase with impressive breadth — concurrency, distribution, Go interop, content addressing, and image persistence are all well beyond what most hobby language implementations achieve. The core architecture (NaN-boxed values, inline caching, TCO, namespace-aware compilation, process isolation) demonstrates serious design thought. The most impactful improvements are the correctness fixes in the interpreter's binary fallback path and channel primitives, followed by eliminating the defer/recover overhead from the message send hot path.

---

## 11. Implementation Status & Follow-ups (as of 2026-05-03)

### Closed

- **1a** NLR via panic/recover — replaced with explicit `unwinding`/`unwindTarget`/`unwindValue` mechanism. Panic survives only as a safety valve at `vm/interpreter.go:536`.
- **1b** O(N) VTable lookup — flattened, O(1) lookups with lazy rebuild.
- **1c** Selector-id-indexed method array — replaced with per-class `map[int]Method` and a power-of-two open-addressed hash flat table (Fibonacci hashing). Memory now proportional to method count, not global selector ID space.
- **1d** Stack/frame growth — initial sizes doubled (`DefaultInitialStack=2048`, `DefaultInitialFrames=512`), made configurable via `InterpreterConfig`.
- **1e** `popN` allocations — added `peekN`/`dropN`; `popN` reserved for >16-arg fallback and block captures.
- **1f** `sendBinaryFallback` NLR — addressed by 1a rework; verified correctly cooperates with the new unwinding mechanism via `handleNLR`.
- **1g** Integer overflow — `primitivePlus`/`Minus`/`Times` promote to BigInt via `TryFromSmallInt`; `primitiveTimes` uses divide-check trick to detect int64 wrap before promotion.
- **2a** Registry explosion — `TypedRegistry[K,V]` and `AutoIDRegistry[V]` extracted in `vm/typed_registry.go`. `ObjectRegistry` is now ~433 lines (down from 1071), a thin facade over per-kind registries. `ConcurrencyRegistry` and `IORegistry` extracted into their own files.
- **2c** Time-based GC — pressure-aware triggering in `vm/registry_gc.go`. Per-registry `growthThreshold` and `absoluteCeiling` hooks via `AutoIDRegistry.SetPressureHook`; non-blocking trigger channel coalesces bursts; 60s wall-clock floor preserved as safety net.
- **3a** `vtableForLegacy` deletion — `NewInterpreter()` now constructs a real minimal VM internally; `vtableForLegacy` deleted (-110 LOC). `SymbolDispatch.markers` map → `[256]*SymbolTypeEntry` array; symbol marker dispatch is one indexed load.
- **3b** No BigInteger — already done; `bigInts` registry, `BigIntObject`, automatic promotion via `vm.registry.NewBigIntValue`.
- **4b** `compileBlock` save/restore — addressed by commit `fe87f59` ("Refactor: extract compilationFrame from Compiler struct", Apr 14). `compileBlock` now uses atomic frame swap via `pushFrame`/`popFrame`.
- **4d** Literal dedup uint64 key — comment added explaining why bit-comparison is sound (NaN-box uniqueness + interning of registry-backed values).
- **4e** Source map / debug info — partially closed at the time of the review; remaining gap (exception stack capture) closed 2026-05-03. `ExceptionObject.CapturedFrames` snapshots stack at `signalExceptionObject`/`checkFrameOverflow`/raw-panic conversion. `Exception>>stackTrace` and `Exception>>printStackTrace` primitives added; `VM.ExecuteSafe` appends the trace to unhandled-exception messages.
- **6c** Process error swallowing — closed 2026-05-03. Centralized recover handler `VM.HandleForkedPanic` (in `vm/process_lifecycle.go`) replaces 7 duplicated `recover()` blocks across `vm/concurrency.go`, `vm/waitgroup.go`, `vm/sandbox_primitives.go`. Non-NLR panics are now (a) logged to stderr (with `debug.Stack()` if `MAGGIE_DEBUG=1`) and (b) routed through `FinishProcess(ExitError(...))` so they correctly notify links/monitors and surface via `primExitReason`. Previously `waitgroup.go` and `sandbox_primitives.go` called `markDone(Nil, nil)` which literally dropped the error.
- **2b** String registry contention — closed 2026-05-03. `GetStringContent` and `GetStringObject` now use the `RLock`+`UnsafeGet` single-lookup pattern (matching `CompareStrings` from earlier work).
- **6a** Globals map race — closed 2026-05-03. `globalsMu sync.RWMutex` already guarded `Interpreter.LookupGlobal`/`SetGlobal` and `VM.LookupGlobal`/`SetGlobal`. The remaining gap was production callers writing `vmInst.Globals[name] = val` directly in `pipeline/pipeline.go` and `pipeline/rehydrate.go` (executes during `mag wrap`/`mag build` and `--serve` startup). Updated to use `VM.SetGlobal`. Init-time writes in `vm.go`/`exception.go`/etc. left as direct assignment (single-threaded by construction; locking would be wasted contention).
- **REPL/compile error formatting** — closed 2026-05-03. `compiler.Compile`/`CompileExpr`/`ParseSourceFileFromString`/`CompileMethodDef*` previously rendered errors as `"parse errors: %v"` of a `[]string`, producing one-line `[err1 err2 err3]` output. Replaced with new `formatErrors(kind, errs)` helper in `compiler/codegen.go`: single error → `"<kind>: <msg>"`; multiple → `"<kind>:\n  <msg1>\n  <msg2>"`. Improves REPL UX and stack-trace readability everywhere.

### New work driven by review (not in original list)

- **`IsSymbol()` was lying** — returned true for every symbol-NaN-tagged value (channels, processes, mutexes, exceptions, strings, dictionaries, BigInts, GoObjects, RemoteRefs, etc.). Renamed to `IsSymbolEncoded()` (broad); added strict `IsSymbol()` that returns true only for actual interned Symbols (`marker==0 && id<stringIDOffset`). Audited 138 callers (53 broad / 53 narrow / 2 judgment); 3 latent printer/encoder bugs surfaced and silently fixed (REPL formatter, eval-service formatter, image encoder).
- **`AutoIDRegistry` ID exhaustion / silent wrap** — `nextID atomic.Uint32.Add(1)` would silently wrap from `MaxUint32` → 0 producing collisions. Now panics with the registry's name when free-list is empty *and* `nextID > maxID`. Free-list reuses IDs from `Delete` and `Sweep`. Per-registry `WithMaxID`/`WithName`/`WithMonotonic` options. `classValues` opted-into monotonic mode (cached `*Class.classValueID` would alias if reused).
- **`weakRefCounter` and `ConcurrencyRegistry` bespoke counters** — same silent-wrap bug class as AutoIDRegistry. Added panic-on-exhaustion via `allocConcurrencyID`/`allocConcurrencyID64` helpers for all 9 concurrency counters and `NextWeakRefID`. Free-list reuse intentionally not added — would conflict with `Sweep`'s nil-on-stale semantics.
- **`Value.ObjectPtr` checkptr violation** — `unsafe.Pointer(uintptr(v) & payloadMask)` synthesized a uintptr from arithmetic and converted in a separate statement, violating `unsafe.Pointer` Pattern 5. Tripped `-race`'s checkptr and `TestAOTDispatchIntegration`. Fixed with single-expression conversion + `//go:nocheckptr`. Same bug at `CellPtr`, fixed too. Zero perf impact.
- **`VTable.Lookup` / `rebuild` data race** — introduced (or surfaced) by 1c's open-addressed table. Lazy-rebuild without synchronization. Replaced `flat`/`flatMask`/`dirty` fields with `atomic.Pointer[vtSnapshot]` + `sync.Mutex` (DCL pattern). Hot path: one atomic load, no locks. 7 of 10 pre-existing race-test failures went green.

### Outstanding follow-ups (not yet addressed)

**Distribution / correctness:**
- `vm/serial.go:753` — Remote-deserialized `ExceptionObject` arrives with empty `CapturedFrames` (frames not on the wire). For distributed debugging, serialize a pre-resolved string trace from the originating side.
- ~~`vm/remote_channel.go:127` — `channelExportRegistry.Export` does an O(n) linear scan~~ — closed 2026-05-03. Added `byChan map[*ChannelObject]uint64` reverse index; export dedup is now O(1).

**Test/race issues (pre-existing, exposed during this work):**
- ~~`TestProfilerConcurrentAccess` — race on `Profiler.RecordMethodInvocation`~~ — closed 2026-05-03. `MethodProfile.IsHot` and `BlockProfile.IsHot` switched from `bool` to `atomic.Bool`; hot-transition uses `CompareAndSwap(false, true)` so the `hotMethodCount`/`hotBlockCount` increment fires exactly once per profile under contention. Reads through `IsMethodHot`/`IsBlockHot`/`Stats`/`HotMethods`/`HotBlocks` use `IsHot.Load()`. One test logger updated.
- ~~`TestRemoteProcess_CastWithSendFunc`~~ — closed 2026-05-03. Replaced `var sentEnvelope []byte` + sleep+read with a buffered channel handoff and `select`-with-timeout for proper synchronization.
- ~~`TestInterpreterIvarStore`~~ — closed 2026-05-03. No longer reproduces; passes 5/5 under `-race`. Likely silently fixed by intervening AutoIDRegistry/Class refactor work.
- ~~`TestRegistryGCPostSweepBaselineReset`~~ — closed 2026-05-03. Root cause: seed phase used `growthThreshold=100` while allocating 200 entries, which fired an auto-sweep on alloc #101. That trigger queued in `gc.trigger`; the manual `SweepNow()` was synchronous but did not drain the queue, so a later loop iteration popped the queued trigger and incremented `SweepCount` after `startCount` was snapshotted. Fix: keep threshold at `1_000_000` during seed, drop to 100 only after `SweepNow()`. 20/20 green.
- `TestRegistryGCCeilingTrigger` — flaky under `-race` with full vm suite (1 fail in 5); passes 100% in isolation. Scheduler-timing-sensitive 500ms wait. Same family as the post-sweep-baseline flake; the fix pattern (avoid seed-time auto-triggers) likely applies.

**Doctest failures (pre-existing on `main`, not caused by this work):**
- `DistributedSupervisor` doctests (2): `compile: parse errors: line 4: unexpected token: |` on the typed-temp setup line `| ds <DistributedSupervisor> |`. Confirmed 2026-05-03 to fail identically on a clean stash of `main`. Likely a doctest-harness issue handling `| name <Type> |` typed-temp syntax in setup blocks.

**Code organization / leaky encapsulation:**
- ~~`MonitorRef.ID` allocation reaches into `ConcurrencyRegistry.monitorRefID` directly~~ — closed 2026-05-03. `ConcurrencyRegistry.AllocMonitorRefID()` added; both call sites (`vm/monitor.go:83`, `vm/remote_lifecycle.go:41`) updated. Now uses `allocConcurrencyID64` like the other concurrency counters (panics on exhaustion instead of silent wrap).
- ~~`cmd/mag/repl.go:203` calls `Execute` not `ExecuteSafe`~~ — closed 2026-05-03. REPL now calls `ExecuteSafe` and prints the captured trace on unhandled exceptions.
- ~~`compiler/codegen.go` `compilationFrame.literals` initialized via append-to-nil~~ — reviewed 2026-05-03, kept as-is. Append-to-nil is idiomatic Go (slices don't need `make`, only maps do); not actually inconsistent with `literalMap: make(...)`.
- `compiler/codegen.go` `compilationFrame.outerTemps`/`outerArgs` initialized as empty maps in `compileBlock` but never populated within `compileBlock` itself. Reviewed 2026-05-03: confirmed read in 4 places (`codegen.go:583,588,683,970,973`) but never written; lookups always return false (dead branches). Removal needs git archaeology to confirm intent — deferred until someone touches block compilation.

**Performance:**
- ~~`vm/object_registry.go` `GetStringContent` still does the unoptimized two-lookup path~~ — closed 2026-05-03. `GetStringContent` and `GetStringObject` now use the `RLock`/`UnsafeGet` single-lookup pattern.
- ~~`vm/object_registry.go` `Cells` registry uses `sync.Mutex` for read methods~~ — closed 2026-05-03. Switched to `sync.RWMutex`; `HasCell`/`CellCount` now use `RLock`.

**Naming / API hygiene flagged but not necessarily acted on:**
- `ObjectRegistry.FullStats` returns the underlying map — callers could mutate it. Cosmetic.
- `AutoIDRegistry.nextID` could be plain `uint32` now (only mutated under `freeMu`).
- `Value.ObjectPtr` / `CellPtr` go vet still warns "possible misuse of unsafe.Pointer" — that's a static lint that fires on the pattern regardless of `//go:nocheckptr`. Not actionable without restructuring the encoding.

### Deferred (large-scope or low-priority)

- **1c-followup** Selector ID space pressure — 24-bit ceiling per kind. Free-list mitigates churn but doesn't widen the ceiling. Tag-scheme rework is its own multi-week project.
- **3a-medium** Reserve more tag space for kinds (e.g., split symbol tag into "real symbol" vs "kinded object" sub-tags). Not started.
- **3a-big** Widen per-kind payload to 32+ bits. Multi-week, separate decision.
- **4a** No optimizer — multi-week. Constant folding alone is non-trivial because Smalltalk operators are overridable; only primitive ops on literal SmallInts/Floats are safely foldable.
- **4c** Cell variable analysis O(method_size × nesting_depth) — not critical; defer until compile times bother someone.
