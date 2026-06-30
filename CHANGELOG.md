# Changelog

## 2026-06-30 — Adversarial review hardening sweep

A multi-round adversarial review (parallel subagent reviewers across
performance, robustness, compiler correctness, GC/memory, and stdlib, iterated
to convergence) found and fixed ~25 issues. All changes ship with regression
tests; the Go suite, `-race`, and `mag doctest` (1166 cases) stay green.

**Garbage collection — two default-on use-after-frees fixed.** The tracing GC
(on by default) did not mark several live roots, so a value reachable only
through them was swept and its id recycled, silently corrupting data:
- **Futures** now mark their cached `result`/`exceptionValue` (`markRoots` §12).
- **Channels** now mirror buffered and in-flight send values in a `pending`
  slice marked by `markRoots` (§13); every send records and every receive
  (including `reflect.Select` send/recv and the select fast path) pops, keeping
  the mirror a superset of the buffer. Blocking sends now `enterBlocked` so the
  collector can reach a safepoint (a parked send previously stalled every GC
  cycle until a receiver drained).

**Exception machinery.**
- `ensure:` now runs its finally block on **all** exits. The lib `Block>>ensure:`
  was a pure-Maggie `result := self value. finallyBlock value` that skipped the
  finally on any non-local return or exception; it now delegates to the correct
  `primEnsure:` primitive.
- `retry`/`resume:`/`pass` no longer leak the handler block's frame. A handler
  escaping via one of these left `fp` offset, so a successful `retry` returned
  into the leaked frame and **every statement after `on:do:` was silently
  skipped**. `evaluateHandlerBlock` now pops leaked frames in its recover.
- `e resume: <v>` returned `0` instead of `<v>` (its outcome reused
  `handlerDone` and was overwritten by the normal-completion path); fixed with a
  distinct `handlerResume` action.

**Number tower — made consistent across SmallInteger / Float / BigInteger.**
Every comparison and arithmetic operator now coerces or answers correctly for
all type pairs (verified by an exhaustive sweep):
- Integer & BigInteger `/` and `\\` by zero now raise a catchable `ZeroDivide`
  (was a silent `nil`).
- `=` via the `OpSendEQ` fast path dispatched identity only, so BigInteger
  value-equality and every user-defined `=` override were bypassed
  (`(100 factorial) = (100 factorial)` was `false`); it now dispatches for
  BigInteger and object receivers.
- BigInteger `< > <= >=` with a Float argument returned `nil`; they now promote
  via `big.Float`. Float `+ - * / < >` now accept a BigInteger argument
  (promote to Float). SmallInteger `< > <= >=` with a Float argument raised an
  error (`7 < 2.5`); they now coerce, matching SmallInteger arithmetic.
- Float `<=` and `>=` were unimplemented (returned `nil`, breaking `ifTrue:`
  and `between:and:`); added as primitives.
- BigInteger `abs`/`negated` called nonexistent `primAbs`/`primNegated` (a
  `doesNotUnderstand`); fixed to use the registered primitives.
- Integer literals beyond the SmallInteger range now compile to exact
  BigIntegers (were lossy Floats, or `0` past int64).
- `Dictionary`/`Set` BigInteger keys hashed by their NaN-boxed handle, so
  lookups always missed; they now hash by value.

**Safety / reliability.**
- `Process` self-link / self-unlink deadlocked the VM (same non-reentrant mutex
  locked twice); now a no-op.
- Unbounded user-controlled allocations (`Array`/`ArrayList`/`Channel new:`,
  `bitShift:`) fatally OOM-killed the process; they now raise a catchable error
  past a sane cap.
- `ExecuteSafe` left `fp`/`sp` elevated after an unhandled exception, so the
  REPL / doctest runner / LSP accumulated stale frames and eventually threw a
  spurious `StackOverflow`; it now restores them.
- Unhandled top-level exceptions printed the raw Go struct (`{9222… 0x…}`);
  they now render `ClassName: messageText` plus a stack trace and exit non-zero.
- `String>>do:` ran the block via a plain send, so a non-local return out of it
  crashed the VM uncatchably; it now uses `evaluateBlock` (matching the other
  collections).

**Compiler diagnostics.** `CompileExpr` now errors on trailing tokens
(`3 4`), an unterminated `"`-comment now produces an error instead of silently
swallowing the rest of the input, and duplicate method/block parameter or temp
names are now rejected.

**Standard library additions & fixes.** `join:`/`joinWith:` now coerce each
element via a new `displayString` (were dropping non-strings; `Object`/`String`/
`Character`/`Symbol` implement it). `Dictionary` gained `select:`/`collect:`/
`valuesDo:` and a fixed `reject:` (the inherited one called a missing
`select:`). `Enumerable` gained `do:separatedBy:`, `asArray`,
`asOrderedCollection`, `asSet`, `sum`, `max`, `min`. `Array` gained `reversed`
and `with:collect:`. `ArrayList` gained `remove:`/`remove:ifAbsent:`/
`removeFirst`/`sorted`/`sorted:`. `String` gained `collect:`/`select:`/
`reversed`.

**Performance (low-risk).** `size` fast path uses a pointer compare instead of
two class-name string compares; the per-instruction debugger probe is hoisted
to a per-frame local (free when no debugger is attached).

**Tooling.** The doctest runner now hoists temp declarations from setup lines
into its single prelude, so doctests whose setup declares its own temporaries
(`| ds <T> |`) compile instead of failing.

Deferred for dedicated, separately-reviewed work (documented with plans):
sweeping the `keepAlive` object/ArrayList registries (a bounded leak — but a
naive sweep risks a worse use-after-free via `becomeForward:` stubs and
Go-held objects); class-side dispatch falling back to `Object` instance
protocol (so `self error:` is not a no-op in a class method); a boolean
`sort:`/`sorted:` sort-block; an operand-stack depth cap; and true
resume-at-signal `resume:` semantics.

## 2026-06-25 — Parser surfaces previously-silent syntax errors

`mag build` / compile no longer silently drops malformed source. The parser had
three skip branches (top-level, class body, trait body) that advanced past
unexpected tokens *without recording an error*, so a mistyped `method:` keyword
or stray tokens vanished from the compiled image with no build error —
surfacing later as a confusing `doesNotUnderstand` at runtime. (Diagnosed in
`procyon-park`, where it repeatedly cost real debugging time.) Unexpected
tokens now produce `line N: unexpected <TYPE> "<lit>" in <context>` and fail
the build; resync to the next definition keeps one stray construct to a single
error rather than flooding.

This also fixed a latent gap: the parser had no `classVars:` case, so class
variable declarations were silently dropped at parse time. They are now parsed
into `ClassDef.ClassVariables`. (Not yet wired into `vm.Class` on the
fresh-compile path — only image rehydrate sets `ClassVars` — so class variables
still do not function from fresh source; deferred to avoid changing class
content hashes.)

Stdlib `--` section-separator lines (not valid Maggie — comments are `#`) in
`lib/Cli/Output.mag` and `lib/Process.mag`, previously silently dropped, were
corrected to `#` comments.

## 2026-06-18 — String & dictionary GC on by default

The string/dictionary collector (added below) is now **enabled by default** —
leaving the registries unbounded OOM-kills any long-running process, so the
safe behavior should not require opt-in. Disable with `MAGGIE_GC=0` (also
`off` / `false` / `no`) for the unsupported concurrent-`vm.Send`-shared-main-
interpreter pattern. Cost is ~2 % on a pure tight integer loop and unmeasurable
on dispatch-heavy code; disabled, the hot path is unchanged.

## 2026-06-18 — String & dictionary garbage collection

Maggie `Value`s are NaN-boxed `uint64`s, so the Go runtime GC cannot see the
heap objects they reference. Strings and dictionaries were registered in
`AutoIDRegistry`s that were never swept, so a program that creates many
distinct strings — e.g. a long-running server re-rendering HTML on every tick —
grew without bound until the OS OOM-killed it. (Diagnosed in `procyon-park`'s
`pp serve`: RSS climbed ~1 GB/min, 90 % of the live heap was strings from
`String>>,` concatenation.)

Added a precise, stop-the-world mark-sweep collector for the string and
dictionary registries:

- **Tracing.** Marks from the complete live-root set — every interpreter's
  operand stack / frames / exception handlers, globals, class variables,
  compiled-method & block literal pools, processes (result + mailbox), blocks,
  cells, contexts, results, exceptions — descends through objects, dictionaries,
  array-lists and cells, and sweeps the strings and dictionaries that are not
  reachable.
- **Stop-the-world via cooperative safepoints.** Interpreters poll a safepoint
  at loop back-edges and frame entry; blocking primitives (`Process sleep:`,
  mailbox/channel receive, HTTP serve) bracket their blocking call with an
  enter/exit-blocked pair that publishes the Values they hold. The collector
  arms a request, waits (bounded by a 250 ms timeout — it **aborts** rather than
  hang if a goroutine is stuck in an un-instrumented blocking primitive),
  traces+sweeps the frozen heap, then releases.
- **Opt-in, zero default cost.** Disabled unless `EnableStringGC()` is called or
  `MAGGIE_GC=1` is set. When disabled the safepoint is a single local-bool
  branch; the interpreter hot path is unchanged (benchmarked: tight integer
  loop identical to baseline). When enabled it is driven by `RegistryGC` growth
  pressure on the string registry.
- **Precondition.** One interpreter per mutator goroutine (Maggie's
  forked-process / serialized-dispatcher model). Driving Maggie execution with
  concurrent `vm.Send` from multiple goroutines that share the main interpreter
  is not supported under the collector.

Validated with per-root regression tests, barrier tests (stop-the-world,
timeout-abort, blocked-mutator, concurrent-churn-no-corruption), the full vm
suite GC-on and GC-off both under `-race`, and a live `pp serve` run where RSS
went from unbounded growth to a flat steady state with correct output.

## 2026-05-05 — petermattis/goid arm64 fast path

Upgraded `github.com/petermattis/goid` from the 2018 pin
(`b0b1615b78e5`) to `v0.0.0-20260330135022-df67b199bc81`. The old version
lacked an `arm64` build tag, so on Apple Silicon every
`currentInterpreter()` call fell into `getSlow()` →
`runtime.Stack(buf, false)` — roughly 10 µs per call plus a
runtime-internal lock that effectively serialised goroutine dispatch.

In long-running fork-heavy hosts (notably `procyon-park` `pp serve`)
this dominated idle CPU: the dispatcher, SSE drainer, notification hub,
and per-request HTTP handlers each register an interpreter, so the
`interpreterCount == 0` fast path is never hit and `goid.Get()` runs
on the hot path of every `Mutex critical:`, `[block] fork`,
`ExecuteBlockDetached`, exception handler, and channel op. A 4-minute
profile showed call stacks dominated by `vm.currentInterpreter →
goid.getSlow → runtime.Stack → runtime.traceback`.

The upgraded package selects an arm64 ASM trampoline (`getg().goid` via
one instruction). On the survey host pp serve idle CPU dropped from
~200% to ~0% and macOS top-mem from 32 G of committed pages to 1.5 G
once paired with downstream procyon-park fixes.

This effectively re-lands the work reverted in `0d9b60e` ("Revert
perf(vm): use petermattis/goid"), with the dependency upgrade that
makes it actually fast on arm64.

## 2026-05-04 — Block Registry: Slot Recycling + Generation Tags

Bounded the block registry's growth so long-running fork-heavy workloads
(notably `procyon-park`) no longer exhaust the block ID space and panic.

- **Fork goroutines release their slot on exit** — every `[block] fork`,
  `forkWith:`, `forkWithContext:`, `Process fork:`, `forkRestricted:`,
  `forkWithout:do:`, and `Sandbox run:` now calls `ReleaseBlock` in the
  goroutine's `defer`. Previously each fork permanently consumed a slot.
- **Generation-tagged slot recycling** — block `Value`s now encode a
  16-bit generation alongside the 32-bit slot id (bits 32-47 of the
  `tagBlock` payload). `ReleaseBlock` bumps the slot's generation and
  pushes it onto a free list; `RegisterBlock` reuses free slots before
  growing. Stale `Value`s pointing at the old `(slot, gen)` resolve to
  nil instead of silently aliasing onto whatever block now occupies
  that slot — same failure mode as if the block had been GC'd.
- **API change:** `RegisterBlock(*BlockValue)` now returns `Value` (not
  `int`); `GetBlock`, `HasBlock`, and `ReleaseBlock` take `Value` (not
  `int`). All in-tree callers updated.
- **Slot ceiling raised** — block slot id space is now 2^32 - 1 (was
  2^24 - 1). With recycling the practical bound is peak-live blocks,
  not cumulative allocations.
- **Known residual:** frame-bound blocks (every `[...]` literal that
  isn't forked) still leak — `popFrame` does not call
  `ReleaseBlocksForFrame`, and the registry GC explicitly skips the
  block sweep. Eliminating that requires either changing `popFrame` to
  release (and accepting that callbacks stored in long-lived
  containers may break) or full reachability tracing. Not addressed
  here. The generation tag added in this change makes the eventual
  fix safer (over-eager release fails to nil rather than aliasing).

## 2026-04-01 — ArrayList: Go-Backed Growable Collection

Added `ArrayList`, a first-class VM type backed by a Go `[]Value` slice,
with amortized O(1) `add:`, O(1) indexed access, and efficient `select:`,
`reject:`, `collect:` — eliminating the O(n²) copying penalty of building
Arrays incrementally with `copyWith:` or concatenation.

- **New primitive type** — NaN-boxed marker `56 << 24`, registered in
  `ConcurrencyRegistry`. Go struct in `vm/arraylist.go`, 30+ primitive
  methods in `vm/arraylist_primitives.go`, Maggie wrapper in
  `lib/ArrayList.mag` with 40 passing doctests.
- **Full collection protocol** — `new`, `new:`, `withAll:`, `add:`,
  `addAll:`, `at:`, `at:put:`, `size`, `capacity`, `removeLast`,
  `removeAt:`, `clear`, `first`, `last`, `isEmpty`, `notEmpty`,
  `includes:`, `indexOf:`, `asArray`, `do:`, `collect:`, `select:`,
  `reject:`, `inject:into:`, `detect:`, `detect:ifNone:`, `copy`,
  `sort:`, `printString`.
- **Eliminated O(n²) patterns across 11 files:**
  - `Array>>select:` / `Array>>reject:` — now O(n)
  - `Set>>collect:` / `Set>>printString` — now O(n)
  - `BytecodeGenerator` — all 6 accumulators (bytecode, literals,
    selectors, temps, captures, blockMethods) + 14 emit methods
  - `Lexer>>allTokens` — token accumulation
  - `Parser` — all 7 collection sites (cascades, keyword args, params,
    temps, arrays, statements, class bodies)
  - `Compiler` — method/class/trait accumulation
  - `Supervisor` / `DynamicSupervisor` — restartTimes, runningChildren
  - `Cluster` — failedSeeds, members, deadAddrs
  - `HashRing` — nodes, positions, nodesFor:count:
  - `SqliteRows>>toArray` — row accumulation
- **Documentation** — Guide chapter 5 (Collections) updated with 3 new
  sections. USER_GUIDE.md API reference section added. CLAUDE.md section
  added. HTML API docs regenerated.

Test count: 1102 → 1113 (+11 new guide doctests).

## 2026-03-31 — Fix Concurrent Map Writes Race Condition (#2)

Fixed `fatal error: concurrent map writes` crash when HTTP handlers run
Maggie code in parallel goroutines.

- **Bare keepAlive map writes** — 8 call sites across `array_primitives.go`,
  `object_primitives.go`, `set_primitives.go`, `message.go`, and
  `interpreter.go` were writing to `vm.keepAlive` without holding the
  existing `keepAliveMu` mutex. Replaced with `vm.KeepAlive()`.
- **VM dispatch queue** (`vm/dispatch.go`) — HTTP handlers now serialize
  Maggie execution through a dedicated VM goroutine via `vm.Dispatch()`.
  The calling HTTP goroutine blocks until the result is available. This
  prevents concurrent access to VTable, Globals, and other non-thread-safe
  VM internals. The dispatcher starts automatically when an HTTP server
  starts; single-threaded code falls back to inline execution.

## 2026-03-31 — Primitive Method Docstrings

Added `<primitive>` stubs with docstrings for ~136 Go-registered primitive
methods across 14 core library files, making them visible via `mag help`.
Bootstrap now generates `cmd/mag/prim_docstrings_gen.go` to apply `.mag`
docstrings onto Go primitives at startup (primitive methods are not
persisted in the image format).

Also fixed `mag help` to find primitive and class-side methods (was only
searching CompiledMethods), and to filter `prim*` internals from listings.

## 2026-03-30 — Fix 34 Silently Skipped Doctests

Tests in `<primitive>` method docstrings were never executed — the
Go-registered primitive overwrites the compiled method's docstring at
load time, so the doctest runner never sees them. Moved all affected
tests to class-level docstrings where they are discovered and run.

- **DateTime.mag** — 19 tests. Fixed 4 arithmetic tests that used `.`
  (statement separator) instead of parentheses for message chaining.
- **Float.mag** — 12 trig/log/exp tests.
- **Regex.mag** — 19 tests covering compile, match, find, replace, split.

Test count: 1019 → 1062 (+43 previously silent tests now running).

## 2026-03-30 — Dictionary Literal Syntax

Added `#{key -> value}` dictionary literal syntax, following GNU Smalltalk
precedent. Keys and values are arbitrary expressions; pairs are separated
by periods.

```smalltalk
d := #{#name -> 'Alice'. #age -> 30. #active -> true}.
d at: #name   "=> 'Alice'"
d size         "=> 3"
```

Full-stack implementation across 17 files: lexer (`#{` token), parser
(primary-only keys so `->` isn't consumed as binary message), AST
(`DictionaryLiteral`), codegen (`OpCreateDict` opcode 0x92), VM
interpreter, AOT compiler, semantic analysis, content hash system
(`TagDictLiteral` 0x1E), and `mag fmt` formatter support.

## 2026-03-30 — Type Annotations for Standard Library

Added type annotations, effect declarations, and typed examples across all 58
`lib/*.mag` files. Annotations are gradual (zero runtime cost, checked by
`mag typecheck`) and follow the Strongtalk model.

### New file
- `lib/protocols.mag` — structural protocol definitions for `Sizeable`,
  `Indexable`, and `Comparable`

### Annotations added to all library files
- **Return types** (`^<Integer>`, `^<String>`, `^<Boolean>`, `^<Self>`,
  `^<Array>`, `^<Result>`, `^<Process>`, `^<Future>`, etc.)
- **Parameter types** (`<Integer>`, `<String>`, `<Symbol>`, `<Array>`,
  `<Node>`, `<CueValue>`, etc.)
- **Typed instance variables** (`instanceVars: name <String> age <Integer>`)
- **Typed temporaries** in method bodies (`| result <Array> |`)
- **Effect annotations** (`! <IO>`, `! <Network>`, `! <Process>`,
  `! <IO, Network>`, `! <Process, Network>`)
- **Typed examples** — `example` blocks use typed temporaries to showcase
  idiomatic usage (e.g., `| db <SqliteDatabase> |`)

### Files annotated by category
- **Core types (15):** Object, Array, String, SmallInteger, Float, BigInteger,
  Boolean, True, False, Character, Symbol, UndefinedObject, Block, Printable,
  protocols
- **Collections & utilities (11):** Dictionary, Set, Json/JsonReader/JsonWriter,
  DateTime, Regex, Result, Success, Failure, Random, Message, Toml
- **Concurrency (10):** Process, Channel, Mutex, Semaphore, WaitGroup, Future,
  MailboxMessage, RemoteProcess, Node
- **I/O & networking (14):** File, HttpClient, HttpServer, HttpRequest,
  HttpResponse, SqliteDatabase, SqliteRows, SqliteStatement, ExternalProcess,
  SocketConnection, UnixSocketClient, UnixSocketServer, DuckDatabase,
  DuckAppender
- **Distribution & specialty (9):** Supervisor/ChildSpec, DynamicSupervisor,
  Cluster, HashRing, TupleSpace, CueValue, CueContext, ConstraintStore,
  DocServer

All 1015 doctests pass. All Go tests pass.

## 2026-03-30 — Distributed Computing Phase A: Remote Messaging

Complete implementation of distributed messaging between Maggie VMs.
Two VMs can exchange serialized values over HTTP/2 with Ed25519-signed
message envelopes, process mailboxes, and registered process names.

### Value Serialization (`vm/serial.go`)
- CBOR-based serialization for all Maggie value types (SmallInt, BigInteger,
  Float, Boolean, Nil, String, Symbol, Character, Array, Dictionary, Object,
  CueValue)
- Circular object reference handling via backreference tags
- Private CBOR tag range 27001-27012 for Maggie-specific types
- GC safety: deserialized objects registered with `vm.KeepAlive()`
- 21 round-trip tests including nested objects and cycles

### Node Identity (`vm/dist/identity.go`)
- Ed25519 keypair generation and persistence (`.maggie/node.key`)
- 32-byte public key as node ID, displayable as proquint
- Sign/verify for message authentication
- 10 tests including key persistence and corruption detection

### Message Envelope + DeliverMessage RPC
- `MessageEnvelope` with sender, target (ID or name), reply-to, selector,
  payload, class hints, nonce, Ed25519 signature
- Signature covers `payload || nonce || targetProcess` (prevents replay,
  redirection, and tampering)
- `DeliverMessage` gRPC handler: decode, verify, check reputation, deliver
- Bad signatures count toward peer ban threshold
- 8 envelope tests + 3 E2E integration tests

### Process Mailboxes (`vm/mailbox.go`)
- Bounded ring buffer (default 1024) with `sync.Cond` for blocking receive
- `TrySend`, `Receive`, `ReceiveTimeout`, `TryReceive`
- Snapshot + version for selective receive (`receiveMatching:`)
- Every `ProcessObject` gets a mailbox, closed on process death
- `MailboxMessage` class with sender/selector/payload accessors
- `Process current` now returns the actual current process
- Registered process names with lazy dead-process cleanup
- 16 mailbox unit tests + 8 integration tests

### Remote Messaging API
- **Node**: `Node connect: 'host:port'`, `ping`, `processNamed:`
  (NaN-boxed with marker 43)
- **RemoteProcess**: `cast:with:` (fire-and-forget), `asyncSend:with:`
  (returns Future) — regular 2-slot Object
- **Future**: write-once container backed by `chan Value` capacity 1,
  `await`, `await:`, `isResolved`, `error` (NaN-boxed with marker 44)
- NodeRefFactory injected from `cmd/mag` for gRPC client wiring
- `--serve` + `-m` now starts HTTP server before entry point
- 14 unit tests + 3 E2E integration tests (including distributed demo)

### Dual-Hash Distribution Updates
- `ContentStore` indexes by both semantic and typed hash
- `ClassToChunk` propagates `TypedHash` and `TypedMethodHashes`
- `VerifyChunkMethod` verifies typed hash when present
- `CompileResult` replaces bare `[32]byte` in compile function signatures
- 6 new tests for dual-hash indexing and verification

### Documentation
- CLAUDE.md: process mailboxes, registered names, remote messaging sections
- README.md: distributed messaging section with code examples
- Guide09Concurrency: mailbox and name registration sections with examples
- Guide15Distribution: dual-hash, node identity, message delivery, remote
  messaging API, updated security model
- New lib files: Process.mag (8 new methods), MailboxMessage.mag, Node.mag,
  RemoteProcess.mag, Future.mag
- Example app: `examples/distributed-counter/` (server + client, verified working)

### Test Summary
- 72 new tests across 8 test files
- 3 end-to-end integration tests with real HTTP transport
- Distributed counter demo validated: client sends 5 messages, server
  accumulates totals correctly

## 2026-03-26 — VM Performance: Remaining Allocation Hot Paths

Full audit of the 2026-03-03 VM improvement plan. Every P1 correctness
and P2 performance item is now resolved. Remaining: P3 (architecture
refactors needing design input) and P4 TODOs 14/15 (metaclass hierarchy,
exception resume/retry).

### P1 Correctness — All Done (pre-existing)

- **TODO 1 (sendBinaryFallback NLR):** Uses the `unwinding` flag, same as
  `send()` and `Execute()`. No defer/recover in the NLR path.

- **TODO 2 (channel TOCTOU):** `safeSend()` holds the mutex for the
  close-check + non-blocking send, falls back to `safeSendBlocking()` with
  `defer/recover` for the blocking case. Race-free.

- **TODO 3 (integer overflow):** `TryFromSmallInt` + BigInt promotion is wired
  into `primitivePlus`, `primitiveMinus`, `primitiveTimes`. Full BigInt
  implementation in `vm/bigint.go` with `bigint_primitives.go`.

- **TODO 4 (Globals sync):** `globalsMu sync.RWMutex` on the VM struct,
  used in all interpreter read/write paths and image writer.

### P2 Performance — Already Done (pre-existing)

- **TODO 5 (defer/recover elimination):** `send()`, `sendBinaryFallback()`,
  `Execute()` all use the `unwinding` flag mechanism.

- **TODO 6 (popN → peekN in send):** `send()` already uses `peekN`/`dropN`
  with a stack-local `argBuf[16]` for primitives.

- **TODO 7 (VTable flattening):** `flat`/`dirty` lazy rebuild — `Lookup()` is
  O(1) after first access, with automatic invalidation on
  `AddMethod`/`SetParent`/`RemoveMethod`.

### New Optimizations

- **OpCreateArray: `popN` → `peekN`/`dropN`** — Array literal creation
  (`#(1 2 3)`) no longer allocates a temporary `[]Value` slice.
  `NewObjectWithSlots` copies elements individually from the stack view.

- **sendBinaryFallback: `[]Value{arg}` → `argBuf[1]`** — When optimized binary
  opcodes (OpSendPlus, OpSendLT, etc.) fall through to a compiled Maggie method,
  the single-argument slice is now stack-allocated via `[1]Value` array.
  Eliminates 1 heap allocation (8 bytes) per fallback dispatch.

### Investigated and Rejected

- **String registry `sync.Map`:** Replacing `TypedRegistry` (RWMutex) with
  `sync.Map` for lock-free reads. Rejected because `sync.Map` boxes `uint32`
  keys as `interface{}`, adding 2 allocations per operation and ~60% regression
  on StringConcat. The RWMutex approach is faster for single-threaded hot paths
  where there's no lock contention.

## 2026-03-15 — CUE Integration & Tuplespace

A single session that took Maggie from "CUE as a library" to a full
concurrent constraint programming substrate with linear logic semantics.

### CUE Layer 1: Object Projection & Template Matching

- **`Object>>asCueValue`** — any Maggie object can now be projected into
  the CUE value lattice. Objects with named instance variables become CUE
  structs (field names match ivar names); scalars (numbers, strings,
  booleans) become CUE scalars. Handles inherited ivars, nested objects,
  and nil slots.

- **`CueValue>>matchesObject:`** — unify a CUE template against any
  Maggie object's projection. Returns true if unification succeeds (no
  bottom). This is the foundation for all tuplespace matching.

- **`CueValue>>subsumes:` / `subsumedBy:`** — entailment checking. "Is A
  more general than B?" Uses CUE's `Subsume()` with `cue.Final()`. This
  is the primitive the constraint store's `ask` operation is built on.

- **Fixed `cueExportValue`** — objects with named ivars now export as
  `map[string]interface{}` (named fields) instead of `[]interface{}`
  (anonymous slots). Arrays still export as slices.

- **Fixed CUE primitive naming** — all Go CUE primitives renamed from
  public selectors (`kind`, `validate`, etc.) to `prim*` names so the
  `.mag` wrapper methods (with docstrings and doctests) work correctly.
  This was a pre-existing bug where Go primitives shadowed the `.mag`
  methods, making docstrings and doctests invisible.

### Local TupleSpace (Linda Semantics)

- **`TupleSpace`** class with Linda-style coordination:
  - `out:` — non-blocking publish
  - `in:` — blocking destructive take (linear consumption)
  - `read:` — blocking non-destructive read
  - `tryIn:` / `tryRead:` — non-blocking variants
  - Templates are CueValues; matching uses CUE unification

- **Blocking** via goroutine parking — same mechanism as Channel. When
  `in:` or `read:` finds no match, the goroutine suspends until a
  matching `out:` wakes it.

### Linear Logic Extensions

- **Tuple modes** controlling consumption semantics:
  - **Linear** (default `out:`) — consumed exactly once
  - **Persistent** (`outPersistent:`) — never consumed; `in:` returns a
    copy but the tuple stays. Models shared facts / blackboard knowledge.
    This is `!A` (the exponential) from linear logic.
  - **Affine** (`outAffine:ttl:`) — consumed at most once, auto-removed
    after TTL in milliseconds. Expired tuples lazily swept during scans.

- **`out:withContext:`** — tuple auto-removed when a CancellationContext
  is cancelled.

- **`inAll:`** (tensor product) — atomically take ALL tuples matching an
  array of templates, or block until all are simultaneously satisfiable.
  Each template must match a different tuple. Prevents the coordination
  bug where an agent grabs a task but the required resource was taken
  between two separate `in:` calls.

- **`inAny:`** (additive disjunction) — take whichever tuple matches
  first from an array of templates. Like `Channel select:` but for
  tuple templates.

### Constraint Store (Concurrent Constraint Programming)

- **`ConstraintStore`** class — a monotonic shared store backed by a
  single CUE value that starts as top and narrows via unification:
  - `tell:` — unify a constraint into the store. Returns Failure if the
    constraint conflicts (store unchanged on failure).
  - `ask:` — block until the store entails (subsumes) the query.
  - `tryAsk:` — non-blocking entailment check.
  - `watch:do:` — register a callback for when a constraint becomes
    entailed.
  - `value` — snapshot the current store as a CueValue.

- Convenience methods `tellString:`, `askString:`, `tryAskString:` accept
  raw CUE source strings.

### Documentation

- **Guide17CueIntegration** — 8 sections: contexts/values, compiling,
  unification, schema validation, object projection, template matching,
  subsumption, fillPath. 28 doctests.

- **Guide18TupleSpace** — 7 sections: basics, tuple modes, template
  matching, atomic multi-take, choice, constraint store, combining both.
  13 doctests.

- **README** updated with CUE Integration section, TupleSpace &
  Constraint Store section, and guide chapter count (16).

- **ROADMAP.md** — full roadmap covering CUE layers 1-3, distribution
  gaps, node protocol, tuplespace phases 1-6, and agent orchestration.

- **Implementation plan** at
  `docs/plans/2026-03-15-tuplespace-implementation-plan.md` — detailed
  plan for all six phases with code sketches, test plans, and dependency
  graph.

### Test Coverage

- **51 Go unit tests** across tuplespace (25), constraint store (11),
  subsumption (5), and object projection / matching (10)
- **977 doctests** all passing (up from 906 at session start)

### Files Added

- `vm/tuplespace_primitives.go` — TupleSpace implementation
- `vm/tuplespace_test.go` — TupleSpace tests
- `vm/constraint_store.go` — ConstraintStore implementation
- `vm/constraint_store_test.go` — ConstraintStore tests
- `lib/TupleSpace.mag` — Maggie wrapper with docstrings
- `lib/ConstraintStore.mag` — Maggie wrapper with docstrings
- `lib/guide/Guide17CueIntegration.mag` — CUE guide chapter
- `lib/guide/Guide18TupleSpace.mag` — TupleSpace/CCP guide chapter
- `ROADMAP.md` — project roadmap
- `docs/constraint-programming-maggie-cue.md` — design conversation
- `docs/plans/2026-03-15-tuplespace-implementation-plan.md` — impl plan

### Files Modified

- `vm/cue_primitives.go` — asCueValue, matchesObject:, subsumes:,
  prim* rename
- `vm/cue_primitives_test.go` — new tests
- `vm/object_primitives.go` — primAsCueValue on Object
- `vm/markers.go` — tupleSpaceMarker (53), constraintStoreMarker (54)
- `vm/object_registry.go` — TupleSpace + ConstraintStore registries
- `vm/vm.go` — register new primitives
- `lib/Object.mag` — asCueValue method + doctests
- `lib/CueValue.mag` — matchesObject:, subsumes:, subsumedBy: + doctests
- `README.md` — CUE + TupleSpace sections, guide count
