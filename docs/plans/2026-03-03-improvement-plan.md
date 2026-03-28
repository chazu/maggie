# Maggie VM Improvement Plan

**Date:** 2026-03-03
**Source:** Language architect review (`docs/plans/2026-03-03-language-architect-review.md`)
**Status audit:** 2026-03-28 — P1 (all done), P2 (all done), P3 TODOs 9/10 (done), P4 TODOs 13/16 (done). Remaining: P3 TODOs 11/12, P4 TODOs 14/15.

---

## Priority 1 — Correctness

### ~~TODO 1: Fix `sendBinaryFallback` missing NLR protection~~ DONE
- **File:** `vm/interpreter.go:1678-1693`
- **Problem:** When optimized binary ops (`OpSendPlus`, `OpSendLT`, etc.) fall through to a compiled Maggie method via `sendBinaryFallback`, no `defer/recover` is installed for `NonLocalReturn`. If the target method contains a block with `^`, the NLR panic escapes and crashes the VM.
- **Fix:** Add the same `defer/recover` block used in `send()` to `sendBinaryFallback` when dispatching to a compiled method. The pattern is already duplicated in `Execute()`, `send()`, and `sendSuper()` — add it here as well.
- **Test:** Write a test where an optimized binary op (e.g., `+`) is overridden in a class with a method that contains a block performing `^value`. Verify the VM does not crash.

### ~~TODO 2: Fix TOCTOU race in channel `primSend:`~~ DONE
- **File:** `vm/concurrency.go:140-150`
- **Problem:** `primSend:` checks `ch.closed.Load()` before sending, but the channel can close between the check and `ch.ch <- val`, causing a Go panic (send on closed channel).
- **Fix:** Wrap the send in a `recover()`, or hold the channel's mutex during the close-check + send sequence. The recover approach is simpler: catch the panic from sending on a closed channel and return an error value to Maggie.
- **Test:** Write a concurrent test that rapidly closes a channel while another goroutine sends to it. Verify no panic.

### ~~TODO 3: Fix integer overflow in arithmetic primitives~~ DONE
- **File:** `vm/interpreter.go:1695-1697`
- **Problem:** `primitivePlus`, `primitiveMinus`, `primitiveTimes` do not check for overflow of 48-bit SmallInts. `FromSmallInt` silently truncates results that exceed `MaxSmallInt`.
- **Fix (short-term):** After arithmetic, check if the result exceeds `MaxSmallInt` or is below `MinSmallInt`. If so, promote to Float64 (since BigInteger doesn't exist yet). This is lossy but better than silent truncation.
- **Fix (long-term):** Implement BigInteger (see TODO 13) and promote to BigInt on overflow.
- **Test:** Test `MaxSmallInt + 1`, `MinSmallInt - 1`, and large multiplications.

### ~~TODO 4: Add synchronization to `Globals` map~~ DONE
- **Files:** `vm/vm.go:22`, `vm/interpreter.go:193,208`
- **Problem:** `vm.Globals` is a plain `map[string]Value` written by the main interpreter without synchronization while forked interpreters may read it concurrently. This is a data race.
- **Fix options:**
  - (a) Replace `Globals` with `sync.Map` — simple but changes the API surface.
  - (b) Use a `sync.RWMutex` — main interpreter takes write lock, forked interpreters take read lock.
  - (c) Document that globals writes only happen during compilation (before forks) and enforce this invariant.
- **Research needed:** Profile the frequency of globals reads in forked interpreters to determine if lock contention would be a problem. Option (c) may be sufficient if globals are only written during the loading phase.

---

## Priority 2 — Performance

### ~~TODO 5: Eliminate defer/recover from the `send()` hot path~~ DONE
- **Files:** `vm/interpreter.go:458-483, 1162-1183, 1265-1282`
- **Problem:** Every compiled method invocation installs `defer/recover` for NonLocalReturn handling. Go's `defer` has per-call overhead even when no panic occurs, and `recover()` inhibits Go compiler optimizations (inlining, escape analysis). This is the single most expensive pattern in the interpreter.
- **Fix:** Replace panic/recover NLR with an explicit return-target mechanism:
  1. Add a `returnTarget *CallFrame` field to each block's closure (pointing to the frame that should receive the return value).
  2. When `OpReturnTop` fires inside a block, set a flag on the interpreter (e.g., `unwinding = true`, `unwindTarget = frame`) and return up the call stack.
  3. In the main dispatch loop, after each `runFrame` returns, check `unwinding` and pop frames until `unwindTarget` is reached.
  4. Remove all three `defer/recover` blocks from `Execute()`, `send()`, and `sendSuper()`.
- **Research needed:** Study how Cog/Spur (Squeak/Pharo VM) handles NLR for implementation guidance. Benchmark before/after to quantify the improvement.
- **Test:** All existing NLR tests must still pass. Add benchmarks for message send throughput.

### ~~TODO 6: Eliminate `popN()` slice allocation~~ DONE
- **File:** `vm/interpreter.go:279-287`
- **Problem:** `popN()` creates a new `[]Value` slice on every call. Called on every `OpSend`/`OpTailSend`, this is a major source of GC pressure.
- **Fix options:**
  - (a) Return a sub-slice of the stack: `args := interp.stack[interp.sp-n : interp.sp]`. Must ensure callers don't hold references past the next stack mutation.
  - (b) Pre-allocate an argument buffer on the interpreter struct (e.g., `argBuf [16]Value`) and copy into it for sends with <= 16 args. Fall back to allocation for larger sends.
- **Research needed:** Audit all callers of `popN` to determine if any retain the slice beyond the immediate send. If not, option (a) is safe and zero-allocation.
- **Test:** Run `BenchmarkHotPath` before/after. Verify no test regressions.

### ~~TODO 7: Flatten VTables for O(1) dispatch~~ DONE
- **File:** `vm/vtable.go:14-23`
- **Problem:** `VTable.Lookup()` walks the parent chain linearly — O(n) in hierarchy depth. Inline caching mitigates for monomorphic sites but polymorphic sites pay full cost.
- **Fix:** At class creation time, copy all parent methods into the child VTable so every lookup is O(1). When a method is added/changed on a superclass, propagate to subclasses.
- **Research needed:** Determine the cost of propagation on method redefinition. Audit how many places modify VTables after class creation (e.g., `addMethod`, trait inclusion). Consider a "dirty bit" approach where flattened tables are rebuilt lazily.
- **Trade-off:** More memory per VTable, more complexity on method changes. Justified if benchmarks show meaningful improvement on polymorphic dispatch.

### ~~TODO 8: Reduce string registry contention~~ INVESTIGATED — no change needed
- **File:** `vm/object_registry.go` (string registry section)
- **Problem:** Every string operation goes through a mutex-protected map lookup. String comparison in `primitiveEQ` does two mutex-guarded lookups. Strings are the most common heap-allocated values.
- **Fix options:**
  - (a) Use `sync.Map` instead of `map` + `RWMutex` — better for read-heavy workloads.
  - (b) Implement string interning: deduplicate string storage so identity comparison suffices for equality.
  - (c) Use a sharded map (e.g., hash the string ID mod N and use N separate maps with separate locks).
- **Research needed:** Profile real Maggie programs to determine the read/write ratio on the string registry. This will determine which approach is best.

---

## Priority 3 — Architecture

### TODO 9: Break up ObjectRegistry
- **File:** `vm/object_registry.go` (1071 lines, 25+ registries, ~50 sync primitives)
- **Problem:** ObjectRegistry is a God Object. Difficult to reason about, test in isolation, or extend. Each new registry type adds more fields and mutexes.
- **Fix:**
  1. Define a `Registry[K, V]` generic type with `Get`, `Put`, `Delete`, `GC` methods and its own mutex.
  2. Factor each domain (strings, channels, processes, HTTP, gRPC, etc.) into its own `Registry` instance.
  3. ObjectRegistry becomes a facade struct holding named registry instances.
  4. Migrate one registry at a time (strings first, as it's the most performance-sensitive).
- **Research needed:** Determine if the generic registry approach works with NaN-boxed value IDs (uint32 keys). Check if any code relies on cross-registry operations that would be harder with separate types.

### ~~TODO 10: Extract compilation pipeline from `cmd/mag/main.go`~~ DONE
- **File:** `cmd/mag/main.go` (1523 lines)
- **Problem:** Critical compilation pipeline logic (`compileAll`, `loadProject`, skeleton registration, superclass resolution, method compilation) is interleaved with CLI argument parsing, REPL logic, and subcommand dispatch. This makes the pipeline untestable and `main.go` a God Object.
- **Fix:**
  1. Create `pipeline/` package with exported functions: `CompileAll(files, vm, opts)`, `LoadProject(manifest, vm)`, `RegisterSkeletons(files, vm)`, etc.
  2. Create `repl/` package for the REPL loop.
  3. `main.go` becomes a thin dispatcher that parses args and calls into these packages.
- **Research needed:** Map the dependency graph of functions in `main.go` to determine the cleanest extraction boundaries. Some functions reference CLI-specific state (flags, output writers) that needs to be parameterized.

### TODO 11: Add bytecode-to-source-position mapping
- **Problem:** No source position info is preserved in compiled bytecode. Stack traces show only frame index and instruction pointer — no file names or line numbers. The LSP server cannot provide precise runtime error locations.
- **Fix:**
  1. During compilation, emit a source position table alongside bytecodes: `[]SourcePos` where each entry maps a bytecode offset to a `(file, line, col)` triple.
  2. Store the table on `CompiledMethod` (new field: `SourceMap []SourcePos`).
  3. When formatting stack traces, look up the current IP in the source map.
  4. Update the image format (v5) to persist source maps.
- **Research needed:** Study how other bytecode VMs (CPython, Lua, JVM) represent source maps compactly. A run-length encoding approach (one entry per source line change, not per bytecode) would be efficient.

### TODO 12: Add a peephole optimizer pass
- **Problem:** The compiler goes directly from AST to final bytecode with no optimization. Constant expressions like `3 + 4` emit 3 instructions instead of 1. No dead code elimination, no common subexpression elimination.
- **Fix (incremental):**
  1. **Phase A — Constant folding:** After codegen, scan for patterns like `PushInt/PushInt/SendPlus` and replace with `PushInt(result)`. This is a simple peephole pass over the bytecode array.
  2. **Phase B — Dead code elimination:** Remove unreachable bytecodes after unconditional jumps.
  3. **Phase C — Instruction combining:** Combine `PushNil/Pop` sequences, `Push/Pop` pairs, etc.
- **Research needed:** Determine if optimizing at the bytecode level or AST level is more effective for Maggie's instruction set. Bytecode-level is simpler to implement; AST-level enables more powerful transformations.

---

## Priority 4 — Language Completeness

### ~~TODO 13: Implement BigInteger with automatic SmallInt promotion~~ DONE
- **Problem:** 48-bit SmallInts are the only integer representation. Arithmetic silently wraps/truncates on overflow. This is a fundamental gap for a Smalltalk-family language.
- **Fix:**
  1. Add a `BigInt` class backed by Go's `math/big.Int`.
  2. Register BigInt values in the object registry (new marker in `markers.go`).
  3. In arithmetic primitives, on overflow, create a BigInt and return it.
  4. BigInt arithmetic methods should check if the result fits in a SmallInt and demote if so.
  5. All comparison and hashing primitives must handle SmallInt-BigInt mixed operations.
- **Research needed:** Study the NaN-boxing marker space to find a free marker for BigInt. Determine if `math/big.Int` performance is acceptable or if a custom implementation is needed for small big-ints (e.g., 64-bit or 128-bit before going to arbitrary precision).

### TODO 14: Implement full metaclass hierarchy
- **File:** `vm/vm.go:191` — "we skip full metaclass support for simplicity"
- **Problem:** Class-side methods exist but there are no first-class metaclasses. This means no `Metaclass` class, no `class` message returning a proper metaclass object, and no ability to define class-side inheritance independently.
- **Research needed:** This is a major architectural change. Study how Squeak/Pharo implements the metaclass hierarchy (Class/Metaclass/ClassDescription/Behavior) and determine the minimal subset needed for Maggie. Key questions:
  - Should metaclasses be real objects with their own VTables?
  - How does this interact with the existing class-side method storage?
  - What breaks if metaclasses are added? (Image format, `allClasses`, reflection primitives, etc.)

### TODO 15: Add exception resume/retry/pass semantics
- **Problem:** Only `on:do:` catching is supported. No `retry` (restart the protected block), `resume:` (continue from the signal point with a value), or `pass` (re-signal to outer handler).
- **Research needed:** This requires understanding how the exception signaling mechanism currently works and where resume/retry control flow would re-enter. Key questions:
  - How is the exception handler stack maintained? (Is it a linked list of handler frames, or something else?)
  - Does `resume:` require saving a continuation at the signal point?
  - What Smalltalk standard (ANSI, Squeak, Pharo) should be followed for semantics?
- **Fix sketch:**
  1. `retry` — re-execute the `on:do:` protected block from the beginning. Requires saving the block's entry point in the handler frame.
  2. `resume:` — return a value from the `signal` call site. Requires saving a continuation (or at least the frame + IP) at the signal point.
  3. `pass` — pop the current handler and re-signal. Requires the handler stack to be traversable.

### ~~TODO 16: Wire up `become:` (identity swapping)~~ DONE
- **Status: Already implemented** — no work needed.
- **Implementation:**
  - `Become()` method at `vm/object.go:324-347` — two-way identity swap (swaps vtables, sizes, and all slot contents)
  - `BecomeForward()` method at `vm/object.go:349-357` — one-way forwarding (redirects all accesses to another object)
  - `become:` primitive registered at `vm/object_primitives.go:277-290` — validates both arguments are objects and calls `Become()`
  - `becomeForward:` primitive registered at `vm/object_primitives.go:294-307` — one-way forwarding variant
- **Features:**
  - Full two-way identity swapping via `become:`
  - One-way forwarding for proxies and lazy loading via `becomeForward:`
  - Supports NaN-boxed objects through the `forward` field resolution mechanism
  - No registry-based overhead — direct field swaps on heap-allocated Object structs

### TODO 17: Add sorted/hashed collections to core library
- **Problem:** No SortedCollection, Set, Bag, or OrderedDictionary in the core library. These are standard in Smalltalk and expected by users.
- **Fix:** Implement in Maggie (`.mag` files in `lib/`):
  1. `Set` — backed by Dictionary (keys only, values ignored)
  2. `Bag` — backed by Dictionary (keys to counts)
  3. `SortedCollection` — backed by Array with insertion sort or binary search
  4. `OrderedDictionary` — Dictionary that preserves insertion order
- **Research needed:** Determine if these should be implemented purely in Maggie or if performance-critical operations (hashing, comparison) need Go-side primitives. Check what collection protocols (`do:`, `select:`, `collect:`, `inject:into:`) already exist and ensure new collections conform.
