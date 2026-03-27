# Changelog

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

- **Guide15CueIntegration** — 8 sections: contexts/values, compiling,
  unification, schema validation, object projection, template matching,
  subsumption, fillPath. 28 doctests.

- **Guide16TupleSpace** — 7 sections: basics, tuple modes, template
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
- `lib/guide/Guide15CueIntegration.mag` — CUE guide chapter
- `lib/guide/Guide16TupleSpace.mag` — TupleSpace/CCP guide chapter
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
