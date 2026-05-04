# Maggie VM Concurrency Audit — 2026-05-03

**Auditor**: HPC / Go performance engineering review
**Scope**: `/Users/chazu/dev/go/maggie/vm/` (entire package, 180 files, ~81k LOC)
**Trigger**: After replacing `getGoroutineID()` (runtime.Stack walk) with
`petermattis/goid`, the previously-serialized goroutines actually overlapped
inside the VM hot paths and exposed multiple data races (concurrent map
writes, IC inconsistency, exception-machinery panics).
**Method**: `go test -race` on the whole package, plus targeted stress tests
that spawn N goroutines through `newForkedInterpreter` against a shared
`CompiledMethod`. The race detector confirmed every issue listed below.

---

## Executive Summary

The VM is **fundamentally not safe for concurrent execution by multiple
forked Maggie processes**. Until today the previous `getGoroutineID()` cost
(`runtime.Stack` + heap allocation, ~10 µs and a runtime-internal lock) was
acting as a de-facto serializer that masked these bugs. With that cost
removed, two or more goroutines actually overlap in dispatch and trip the
race detector instantly.

There are **three classes of unsynchronized shared mutable state**:

1. **Inline caches on shared `CompiledMethod`s** — `InlineCacheTable.caches`
   (plain `map[int]*InlineCache`) and `InlineCache.{State, Entries, Count,
   Hits, Misses}` are read/written from every dispatching goroutine without
   any lock, atomic, or per-interpreter shard. **This is the dominant
   correctness bug** and the direct cause of the "concurrent map writes"
   crash and the "index out of range [6] with length 6" PIC corruption.
2. **`vm.Globals` is not consistently mutex-guarded.** The interpreter ops
   path takes `globalsMu`, but several Go-side callers
   (`serial.lookupClass`, primitives that read globals at runtime,
   tests/tools) read/write the bare map. Bootstrap also writes
   `vm.Globals` extensively without the mutex — fine if bootstrap is single
   threaded, but easy to break with lazy primitive registration.
3. **VM-wide unsynchronized fields**: `aotMethods` (`AOTDispatchTable`),
   `RehydratedClasses` (`map[string]bool`), `samplingProfiler` (read in
   hot path without atomic), `LocalListenAddr`, `RemoteChannelFactory`,
   `NodeRefFactory`, `fileInFunc`, `fileInBatchFunc`. All are written
   "once at boot" by convention but read from arbitrary goroutines.

Recommended fix order:

1. **PATCH 1 (inline cache)** — highest payoff, smallest blast radius.
   Without this, no concurrent Maggie execution is safe.
2. **PATCH 2 (Globals lockdown)** — funnel every direct-map access through
   a thin `Globals` accessor that takes `globalsMu`.
3. **PATCH 3 (post-init freeze invariant)** — formalize that
   `aotMethods`, `RehydratedClasses`, well-known class pointers, and
   profiler hooks are write-once by `vm.Setup*()` and read-only thereafter.
   Document and `go vet`-style assert.
4. **PATCH 4 (regression test)** — land a `TestForkedConcurrentDispatch`
   that does what my temp test did, so this can never silently regress.

After these four patches the goid speedup can be re-landed safely.

---

## Race Detector Findings

I ran `go test -race -timeout 180s -count=1 ./vm/...` (full package) and
also a focused stress test (`TestAuditInlineCacheRace`) that creates a
trivial user `CompiledMethod` and invokes it concurrently from 32
forked-interpreter goroutines. The full test run actually crashes early
with a `runtime: pointer ... to unallocated span` fault before the race
detector can flush most reports — that fault is itself a downstream
symptom of the inline-cache races corrupting `InlineCacheEntry.Method`
pointers (the bad pointer is dereferenced in `vtableForVM` /
`ObjectFromValue`). The stress test, run in isolation, produced clean
race reports.

### Race 1 — `CompiledMethod.GetInlineCaches()` (compiled_method.go:153)

```
WARNING: DATA RACE
Read at 0x... by goroutine 26:
  github.com/chazu/maggie/vm.(*CompiledMethod).GetInlineCaches()
      /Users/chazu/dev/go/maggie/vm/compiled_method.go:153 +0x338
  github.com/chazu/maggie/vm.(*Interpreter).send()
      /Users/chazu/dev/go/maggie/vm/interpreter.go:1089
```

`GetInlineCaches` is the textbook check-then-act:

```go
func (m *CompiledMethod) GetInlineCaches() *InlineCacheTable {
    if m.InlineCaches == nil {                // RACE
        m.InlineCaches = NewInlineCacheTable()
    }
    return m.InlineCaches
}
```

Two goroutines may both see `nil`, both allocate, and both publish — losing
one table (and any cached entries that other goroutines already filled
into the lost one). Beyond the lost-update, this is also a Go data race on
the `m.InlineCaches` pointer itself.

### Race 2 — `InlineCacheTable.GetOrCreate()` (inline_cache.go:155)

```
WARNING: DATA RACE
Read at 0x... by goroutine 29:
  runtime.mapdelete_fast32()
  github.com/chazu/maggie/vm.(*InlineCacheTable).GetOrCreate()
      /Users/chazu/dev/go/maggie/vm/inline_cache.go:155 +0x408
```

```go
func (t *InlineCacheTable) GetOrCreate(pc int) *InlineCache {
    if ic := t.caches[pc]; ic != nil { return ic }   // RACE
    ic := &InlineCache{State: CacheEmpty}
    t.caches[pc] = ic                                 // RACE → fatal: concurrent map writes
    return ic
}
```

Plain `map[int]*InlineCache` with no mutex. This is the original `fatal
error: concurrent map writes` you observed. The `mapdelete_fast32` in the
trace is a runtime-internal probe that fires during the same buckets
mutation that `mapassign` performs.

### Race 3 — `InlineCache.Lookup()` (inline_cache.go:48-52)

```
WARNING: DATA RACE
Read at 0x... by goroutine 21:
  github.com/chazu/maggie/vm.(*InlineCache).Lookup()
      /Users/chazu/dev/go/maggie/vm/inline_cache.go:48 +0x4e8
```

`Lookup` reads `ic.State`, `ic.Entries[*].Class`, `ic.Entries[*].Method`,
`ic.Count`, and increments `ic.Hits`/`ic.Misses` — all without
synchronization. This is what produced your "index out of range [6] with
length 6" panic: `Lookup` reads stale `State == CachePolymorphic` and
stale `Count == 6`, then `Update` on another goroutine transitions to
`CacheMegamorphic` and zeroes `Count`, but a *third* goroutine racing
concurrent `Update` calls may temporarily set `Count > MaxPICEntries` if
the entry-add and count-increment are not atomic together. (Even without
that, the simpler fault is reading torn `State`/`Count` pairs.)

### Race 4 — `vm.Globals` plain map

```
fatal error: concurrent map writes
goroutine 86 [running]:
internal/runtime/maps.fatal(...)
github.com/chazu/maggie/vm.TestAuditGlobalsRace.func1
```

`vm.Globals` is a `map[string]Value`. The interpreter (interpreter.go:211,
interpreter_ops.go:34/48/93) properly takes `vm.globalsMu` for ops
emitted by Maggie code. But:

- Bootstrap writes (`vm/vm.go:476-507` and every `register*Primitives()`
  function — at least 30 sites) write the map without the lock. **OK
  because bootstrap is single-threaded.**
- `vm/serial.go:622, 770` reads `vm.Globals` without the lock from
  deserialization (called by sync/rehydration goroutines).
- Tests, `image_writer.go`, and a few other Go-side callers do take the
  lock — confirming the convention exists, but it isn't enforced.

The race detector flagged this once I removed the lock from the test
itself; it's latent in production any time a sync/rehydration goroutine
overlaps with a forked Maggie process registering a global.

### Crash 5 — Bad pointer in vtable dispatch (downstream of Race 1-3)

```
fatal error: found bad pointer in Go heap
runtime.checkptrAlignment(...)
github.com/chazu/maggie/vm.ObjectFromValue(0x7ff900c00e5b5e68)
github.com/chazu/maggie/vm.(*Interpreter).vtableForVM(...interpreter.go:1317)
github.com/chazu/maggie/vm.(*Interpreter).send(...interpreter.go:1068)
```

The address `0xc00e5b5e68` does not point to any allocated span — it's a
NaN-boxed Value whose object-pointer payload was shredded. Two plausible
sources, both downstream of the races above:
1. An `InlineCacheEntry.Method` was read torn (half-old / half-new word)
   while one goroutine was assigning into it. Subsequent Invoke reads
   garbage receiver/args.
2. The interpreter's operand stack (`i.stack`) was somehow shared. The
   stress test under `vm.Execute` (which uses the *single*
   `vm.interpreter`, see Architectural Issue A below) made this worse,
   but even with per-goroutine interpreters the IC corruption is enough
   to misroute a primitive.

This is not its own bug — fix Races 1-3 and it should disappear.

---

## Per-Issue Table

| # | Symbol | File:line | Symptom (observed) | Proposed fix | Diff size | Risk |
|---|---|---|---|---|---|---|
| 1 | `CompiledMethod.InlineCaches` (lazy init) | compiled_method.go:152-157 | DATA RACE, lost-update | Eager-init in `NewCompiledMethod`/loader; or `sync.Once`; or `atomic.Pointer[InlineCacheTable]` + DCL | XS | Low |
| 2 | `InlineCacheTable.caches` (plain map) | inline_cache.go:140-160 | DATA RACE → `fatal: concurrent map writes` | Replace map with fixed-size array indexed by call-site PC; allocate at compile time when bytecode is finalized (sites known statically) | M | Low-Med |
| 3 | `InlineCache.{State,Entries,Count,Hits,Misses}` | inline_cache.go:46-118 | DATA RACE, "index out of range [6]/[6]", torn reads | Per-entry `sync.Mutex` is too heavy; instead make IC update *publish-once* via `atomic.Pointer[icEntry]` storing an immutable snapshot. Hit/miss counters → `atomic.Uint64` (or drop entirely). | M | Med |
| 4 | `vm.Globals` (plain `map[string]Value`) | vm.go:75; serial.go:622,770 | DATA RACE → fatal | Funnel ALL access through `vm.GetGlobal/SetGlobal` that take `globalsMu`; remove direct map indexing from non-bootstrap code | S | Low |
| 5 | `vm.aotMethods` (`AOTDispatchTable`) | vm.go:211; vm.go:1083,1179,1217 | latent — would race if AOT install ever happens after fork | Make assignment atomic via `atomic.Pointer[AOTDispatchTable]`, OR document write-once-pre-fork and add a `sync.Once`/runtime check | XS | Low |
| 6 | `vm.RehydratedClasses` (`map[string]bool`) | vm.go:233; vm.go:1266 | latent | Add dedicated `rehydratedMu sync.RWMutex`; sync/rehydration runs concurrently with user code | XS | Low |
| 7 | `vm.samplingProfiler` (read in hot path) | vm.go:241; interpreter.go:1136,1260 | latent — race when `StartSamplingProfiler` called mid-execution | `atomic.Pointer[SamplingProfiler]` | XS | Low |
| 8 | `Interpreter.activePrimitiveName` | interpreter.go:111,1262 | already `atomic.StorePointer` — OK | none | — | — |
| 9 | `vm.LocalListenAddr` (string) | vm.go:192 | latent — written when sync server starts, read by every send | `atomic.Pointer[string]` or set once before any goroutine starts | XS | Low |
| 10 | `vm.NodeRefFactory`, `RemoteChannelFactory`, `fileInFunc`, `fileInBatchFunc` | vm.go:187-222 | latent | Document write-once contract; add `sync.Once` guard or `atomic.Pointer` | S | Low |
| 11 | `VTable` (already audited) | vtable.go | OK — `atomic.Pointer[vtSnapshot]`, mu-guarded mutators | none | — | — |
| 12 | `SelectorTable`, `SymbolTable` | selector.go, symbol.go | OK — RWMutex with DCL | none | — | — |
| 13 | `ClassTable` | class.go:261 | OK — RWMutex | none | — | — |
| 14 | `ConcurrencyRegistry` (channels, processes, mutexes, etc.) | concurrency_registry.go | OK — per-kind RWMutex + atomic counters | none | — | — |
| 15 | `ObjectRegistry.cells`, `classVars`, weakRefCounter | object_registry.go | OK — mutex/atomic guarded | none | — | — |
| 16 | `Interpreter.exceptionHandlers` (linked list) | exception.go:111-153 | OK — per-interpreter, no sharing | none | — | — |
| 17 | `Interpreter.stack/sp/frames/fp` | interpreter.go:97-100 | OK *if* per-interp; **NOT OK** when `vm.Send`/`vm.Execute` reuse `vm.interpreter` from external goroutines (see Architectural Issue A) | See Patch 5 | M | Med |
| 18 | `Class.classValueID` (lazy assign in `RegisterClassValue`) | object_registry.go:382-389 | check-then-act on `c.classValueID != 0` | `atomic.CompareAndSwap`-style init or take a per-Class init lock | XS | Low |
| 19 | Stat counters in registries (e.g. `IC.Hits`/`Misses`, profiler counts) | inline_cache.go:42-43, others | benign "torn integer" — counts may drift | Defer; convert to `atomic.Uint64` when convenient | XS | Low |

---

## Architectural Recommendations

### Issue A — `vm.Send` / `vm.Execute` use the *single* `vm.interpreter`

`/Users/chazu/dev/go/maggie/vm/vm.go:988, 1031, 1088, 1184` all do
`vm.interpreter.Execute(...)` — that is, the *main* interpreter, not the
goroutine-local one. The race detector confirmed this immediately:

```
WARNING: DATA RACE
Read at ... by goroutine 49:
  github.com/chazu/maggie/vm.(*Interpreter).checkFrameOverflow()
  github.com/chazu/maggie/vm.(*Interpreter).pushFrame()
  github.com/chazu/maggie/vm.(*Interpreter).Execute()
  github.com/chazu/maggie/vm.(*VM).Execute()
```

…with two goroutines simultaneously mutating the same `frames` slice on
the main interpreter. Today this is hidden because:
- All Maggie-level forks go through `interp.ExecuteBlockDetached` on the
  per-goroutine interpreter (correct).
- External Go callers (HTTP handlers, gRPC handlers) are funneled through
  `dispatchQueue` — a serializing channel — so they don't actually
  overlap.

Recommendation: **make `vm.Send` / `vm.Execute` route through
`vm.currentInterpreter()`** rather than `vm.interpreter`. Today it works
only because the dispatch queue is the de-facto single-writer; the safer
invariant is that *any* call into the VM uses whatever interpreter belongs
to the calling goroutine, falling back to the main interpreter only on
the main goroutine.

### Issue B — Should there be a "post-init freeze" phase?

Yes. Recommend a `vm.Freeze()` that:
- Snapshots well-known class pointers, AOT table, RehydratedClasses,
  GoTypeRegistry contents into immutable copies.
- Sets a `frozen atomic.Bool`. All mutators (`AddClass`, `AddMethod` on
  bootstrap-only paths, `RegisterAOTMethods`) panic if called after
  freeze (in debug builds; no-op in release).
- Allows the dispatch hot path to skip locks on read-only structures.

This pattern mirrors how V8 marks builtin objects as immutable after
snapshot serialization. It separates "bootstrap, single-threaded,
mutate-everything" from "running, many-threaded, dispatch-only".

### Issue C — Per-interpreter inline caches?

Two viable fix shapes for the IC bug, with different tradeoffs:

| Approach | Memory | Contention | Cache locality | Complexity |
|---|---|---|---|---|
| Shared IC + locks | Low (1 table per method) | High under N goroutines | Good | Low |
| Per-interpreter IC | High (N tables per method) | Zero | OK (each goroutine has its own) | Med (need to pin IC to goroutine) |
| Shared IC + lock-free publish | Low | Low | Good | High (subtle memory ordering) |

**Recommendation: lock-free publish.** An `InlineCache` becomes an
`atomic.Pointer[icSnapshot]` where each `icSnapshot` is immutable
(`State`, `Entries [N]Entry`, `Count` baked in). `Update` builds a new
snapshot and CAS-publishes. `Lookup` does one atomic load. This matches
how `VTable.snap` already works — and it is the same Cog/V8 pattern that
inspired the IC design. Stat counters (Hits/Misses) move to
`atomic.Uint64` outside the snapshot, or are dropped from production
builds.

This also nicely handles the megamorphic transition: building a fresh
megamorphic snapshot and publishing it is one pointer write, no
torn-state window.

### Issue D — Should `vm.Globals` be CoW?

Probably not. Reads are far more frequent than writes, but writes are not
rare (every `Foo := bar` Maggie statement). A `sync.Map` would lose
the iteration order needed by image_writer; the simpler fix is keeping
`globalsMu` and *enforcing* its use everywhere. A small lint or `go vet`
plugin could reject `vm.Globals[` outside the bootstrap files.

For very read-heavy workloads, a future optimization could keep a
`atomic.Pointer[map[string]Value]` snapshot rebuilt on every write. Not
worth the complexity yet.

---

## Concrete Patch List (ordered by safety/effort)

### Patch 1 — Lock-free Inline Cache (eliminates Races 1, 2, 3, Crash 5)

**Files**: `vm/inline_cache.go`, `vm/compiled_method.go`
**Effort**: Medium (200-400 LOC). The current `InlineCache` API
(`Lookup`, `Update`, `Reset`) can stay; only the implementation changes.
**Risk**: Medium — needs careful memory-ordering reasoning, but mirrors
the existing `VTable.snap` pattern that already ships in production.

Sketch:
```go
type icEntry struct { class *Class; method Method }
type icSnapshot struct {
    state   CacheState
    count   int8
    entries [MaxPICEntries]icEntry
}
type InlineCache struct {
    snap atomic.Pointer[icSnapshot]
    hits, misses atomic.Uint64 // optional
}

type InlineCacheTable struct {
    // Pre-sized at method-load: callSites[pc/4] (one per OpSend opcode).
    callSites []atomic.Pointer[InlineCache]
}
```

`Lookup` is one atomic.Load + struct read (immutable). `Update` builds a
fresh `icSnapshot` and CAS-publishes; on CAS-fail the next Lookup will
see the winner's snapshot (lost updates are fine — IC is best-effort).
`InlineCacheTable.callSites` is allocated once when the
`CompiledMethod` is built; its slice header is published with the
method itself, so reads need no atomic (the slice header is immutable
post-construction). **This eliminates the `map[int]*InlineCache`
entirely**, which is both faster (array indexing) and race-free.

This patch alone should let the goid speedup re-land safely. Crashes 1-3
and the downstream bad-pointer fault all stem from inline cache state
corruption.

### Patch 2 — Globals lockdown

**Files**: `vm/vm.go`, `vm/serial.go`, all `vm/*_primitives.go` that read
`vm.Globals` post-bootstrap.
**Effort**: Small (50-100 LOC).
**Risk**: Low.

- Add `func (vm *VM) Global(name string) (Value, bool)` and `SetGlobal`
  that take `globalsMu`.
- `git grep "vm\.Globals\[" -- vm/*.go` → replace every site outside
  `vm.go:bootstrap()` and the `register*Primitives` setup with the
  accessor.
- Optionally: rename the field to `globals` (lowercase) so it's
  unreachable outside the package. External packages currently access
  it (image_writer does) — provide accessors for those too.

### Patch 3 — Post-init freeze + `atomic.Pointer` for late-bound fields

**Files**: `vm/vm.go`.
**Effort**: Small-Medium (100-150 LOC).
**Risk**: Low.

- `samplingProfiler`, `LocalListenAddr`, `aotMethods`,
  `RehydratedClasses`, `NodeRefFactory`, `RemoteChannelFactory`,
  `fileInFunc`, `fileInBatchFunc` → wrap each in `atomic.Pointer[T]`
  with explicit `Get`/`Set` methods.
- For `aotMethods` and `RehydratedClasses` (maps), publish via
  `atomic.Pointer[map[...]...]` with copy-on-write writers. Writes are
  rare; CoW gives lock-free reads.
- Add `vm.Freeze()` that records "no more bootstrap" — debug-only assert
  in `createClass`, `RegisterAOTMethods`, etc.

### Patch 4 — `Class.classValueID` lazy init race

**Files**: `vm/object_registry.go:376`.
**Effort**: XS.
**Risk**: Low.

Use `atomic.Uint32`:
```go
if id := c.classValueID.Load(); id != 0 { return classToValue(id) }
id := or.classValues.Register(c)
if !c.classValueID.CompareAndSwap(0, id) {
    // Another goroutine won; we leak our registration. Acceptable
    // because classValues is monotonic anyway.
    id = c.classValueID.Load()
}
return classToValue(id)
```

### Patch 5 — Route `vm.Send`/`vm.Execute` through current interpreter

**Files**: `vm/vm.go:988, 1031, 1088, 1184`.
**Effort**: Small.
**Risk**: Medium — touches the most-used external entry point.

Replace `vm.interpreter.Execute(cm, ...)` with
`vm.currentInterpreter().Execute(cm, ...)`. Today, callers from external
Go goroutines (HTTP handlers etc.) hit the dispatch queue first, so this
is a no-op. But it removes a sharp edge where someone calling `vm.Send`
from a goroutine that isn't on the dispatch queue silently corrupts the
main interpreter. Add a defensive panic in `Execute` if the receiver
interpreter doesn't match the current goroutine ID.

### Patch 6 — Regression test: `TestForkedConcurrentDispatch`

**Files**: new `vm/forked_dispatch_test.go`.
**Effort**: XS.
**Risk**: None.

Sketch (what I used to provoke the IC race; clean it up and land it):

```go
func TestForkedConcurrentDispatch(t *testing.T) {
    vm := NewVM()

    cls := vm.createClass("Bench", vm.ObjectClass)
    sel := vm.Selectors.Intern("id")
    cls.VTable.AddMethod(sel, &CompiledMethod{
        name: "id", Bytecode: []byte{byte(OpReturnSelf)},
    })

    callerSel := vm.Selectors.Intern("callIt:")
    caller := &CompiledMethod{
        name: "callIt:", Arity: 1, NumTemps: 1,
        Bytecode: []byte{
            byte(OpPushTemp), 0,
            byte(OpSend), byte(sel), byte(sel >> 8), 0,
            byte(OpReturnTop),
        },
    }
    caller.SetSelector(callerSel)
    cls.VTable.AddMethod(callerSel, caller)

    inst := cls.NewInstance().ToValue()

    const G, N = 32, 500
    var wg sync.WaitGroup
    wg.Add(G)
    for g := 0; g < G; g++ {
        go func() {
            defer wg.Done()
            interp := vm.newForkedInterpreter(nil)
            vm.registerInterpreter(interp)
            defer vm.unregisterInterpreter()
            for i := 0; i < N; i++ {
                _ = interp.Execute(caller, inst, []Value{inst})
            }
        }()
    }
    wg.Wait()
}
```

Run with `go test -race -count=10 -run TestForkedConcurrentDispatch`. It
must pass cleanly before re-landing the goid commit.

### Patch 7 (optional) — Re-land goid

**Files**: `vm/vm.go:1436-1448`, `go.mod`.
**Effort**: XS.
**Risk**: Low *after Patches 1-6*.

```go
import "github.com/petermattis/goid"
func getGoroutineID() int64 { return goid.Get() }
```

The CockroachDB project relies on `goid` heavily in production; the
trampoline is a few assembly instructions per supported arch. The 165%→12%
idle-CPU drop you measured is consistent with the prior implementation's
runtime.Stack overhead.

---

## What Can Be Deferred

- **IC stat counters (`Hits`, `Misses`)**. These race but are
  best-effort. Convert to `atomic.Uint64` when convenient; until then, the
  drift is cosmetic. They do *not* affect dispatch correctness.
- **`vm.dependents` map**. Already has its own RWMutex. Audited; OK.
- **`MethodCategories`/`AllCompiledMethods`/`MethodNamed` (class.go)**.
  These iterate `LocalMethods()` via VTable.mu. Reflection-only; not on
  hot path. Acceptable.
- **`pendingSpawns`, `remoteChannels`, `channelExports`,
  `processNames`, `weakRefs`, `nodeRefs`**. All have explicit mutexes.
  Audited; OK.
- **`registry_gc.go` GC sweep**. Runs as its own goroutine, takes locks
  on every registry it sweeps. OK.
- **Bootstrap writes to `vm.Globals`**. Single-threaded by definition.
  The only risk is if someone adds a `register*Primitives` that spawns
  a goroutine — flag in code review.

---

## Verification Plan

After landing Patches 1-6:

```bash
cd /Users/chazu/dev/go/maggie

# Stress test newly added regression
go test -race -count=10 -run TestForkedConcurrentDispatch ./vm/

# Whole-package race (some ld warnings about CGO LC_DYSYMTAB are pre-existing)
go test -race -timeout 240s -count=1 ./vm/...

# Re-introduce the goid hot-path patch (Patch 7), repeat both above

# Boot Procyon Park 5x in a row to confirm the 4-of-5 crash regression is gone
for i in 1 2 3 4 5; do (cd /Users/chazu/dev/maggie/procyon-park && pp ...); done
```

If `TestForkedConcurrentDispatch` passes 100/100 runs under `-race`, the
goid patch is safe to re-land.

---

## Summary

The Maggie VM correctly synchronizes the *named* shared structures
(SelectorTable, SymbolTable, ClassTable, VTable, ConcurrencyRegistry,
ObjectRegistry, dependents). It does **not** synchronize the *implicit*
shared structures introduced by inline caching — and inline caching is
on the dispatch hot path, executed by every goroutine on every send.
That single oversight, combined with `vm.Globals` having an inconsistent
locking convention and several "write once at boot" fields lacking
publication ordering, accounts for every crash class observed when goid
removes the runtime-Stack serialization.

Patches 1, 2, and 6 are the minimum viable set to re-land the goid
speedup safely. Patches 3, 4, 5 close the remaining latent race windows
and codify the freeze-after-bootstrap invariant the VM was already
informally relying on.
