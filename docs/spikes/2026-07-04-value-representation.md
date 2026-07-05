# Spike: Value representation — NaN-boxed uint64 vs pointer-carrying

**Question (review #1):** Maggie's `Value` is a NaN-boxed `uint64`; heap objects
(strings, dicts, collections, blocks, …) live in lock+map side registries
(`vm/typed_registry.go`) and are invisible to Go's GC — which is why the VM
carries its own ~1,200-line stop-the-world collector (`heap_gc.go`,
`registry_gc.go`, `gc_safepoint.go`) plus per-instruction safepoint polling.
Would a pointer-carrying `Value` the Go runtime can trace be worth it?

This is a **throwaway model** (`docs/spikes/valuerep/`), not the VM. It measures
the two representations' hot-path costs in isolation. Reproduce:

```
go test -run=^$ -bench=. -benchmem -cpu=1,8 ./docs/spikes/valuerep/
```

## Results (Apple M-series, go1.26)

| Operation | NaN uint64 (current) | Pointer-carrying | Delta |
|---|---|---|---|
| Scalar add (inline) | 0.248 ns | 0.248 ns | **equal** |
| **Heap access (read a field)** | 5.68 ns | 0.25 ns | **~23× faster** |
| **Heap alloc** | 112.5 ns, 82 B | 9.5 ns, 32 B | **~12× faster, less memory** |
| Value copy (16 slots) | 17.2 ns | 17.5 ns | ~equal (2× size, negligible) |
| Heap access @ -cpu=1 | 5.70 ns | 0.77 ns | 7× |
| **Heap access @ -cpu=8** | **66.8 ns (12× WORSE than 1 core)** | 0.15 ns (scales) | **~440×** |
| sizeof(Value) | 8 B | 16 B | 2× |

## What the numbers say

1. **Scalars are free either way.** Inline arithmetic (the SmallInt hot path)
   is identical. No regression there.
2. **Heap access is the tax.** Every read of a string/dict/collection currently
   pays a RWMutex + map lookup (5.7 ns); a pointer deref is 0.25 ns. In a
   Smalltalk, heap reads are pervasive, so this is a broad, constant tax the
   pointer design erases.
3. **Allocation is 12× cheaper AND self-reclaiming.** The registry does lock +
   map insert (112 ns, 82 B) and then *needs the custom collector to ever free
   it*. A plain `new` (9.5 ns) is reclaimed by Go's GC for free — deleting the
   entire custom-collector subsystem, its safepoint polling, the ID-exhaustion
   machinery, and the open leak/soundness bug cluster the review flagged.
4. **The 16-byte concern doesn't materialize.** Copy cost is within noise of the
   8-byte value (17.5 vs 17.2 ns) — the operand-stack-copy worry that usually
   argues against fat values is not supported here.
5. **The current design gets WORSE under concurrency, which is the root of the
   single-threaded server (spike #2).** Registry heap access goes 5.7 ns → 66.8
   ns from 1→8 cores (RWMutex cache-line contention); the pointer design goes
   0.77 → 0.15 (scales). This is *the* mechanism forcing `VMWorker`'s single
   goroutine. A pointer-carrying, lock-free heap is the prerequisite for a
   concurrent server.

## Trade-offs / caveats (honest)

- **This is a model, not the VM.** The real `Value` also inlines floats,
  characters, booleans, and ~40 marker kinds; a pointer-carrying design still
  needs a tag discipline (e.g. a small kind byte, or low-bit tagging of `ptr`)
  to distinguish scalar-vs-pointer and float-vs-int. The benchmark's simple
  `{scalar; ptr}` is optimistic by a small constant, not by the order of
  magnitude the heap-access/alloc/concurrency numbers show.
- **Migration cost is large.** `Value` is threaded through the entire codebase;
  this is a multi-week change touching the interpreter, serialization, image
  format, and every primitive. It is not incremental — it's a fork-and-prove.
- **`unsafe.Pointer` discipline.** Correctness now depends on never fabricating a
  bad pointer; today's raw `uintptr` object encoding already has this hazard,
  so it's not new, but it becomes load-bearing.

## Recommendation

The measured case for pointer-carrying Values is strong and one-directional on
every axis that matters (heap access, allocation, concurrency, GC-subsystem
deletion), with the usual counter-argument (copy cost / value size) empirically
near-zero. The blocker is migration effort, not the design.

Suggested next step: a **branch prototype** that swaps `Value` in a vertical
slice (SmallInt + String + Array + the interpreter send path) and runs the real
`BenchmarkHotPath*` + a subset of doctests, to confirm the model holds against
the actual dispatch/serialization paths before committing to the full migration.
This spike de-risks the *decision*; the branch prototype would de-risk the
*execution*.

---

## Post-migration measurement (Stage 4 — the real VM, not the model)

The full migration landed on `migrate/pointer-value` (Stages 1–3): `Value` is now
`{hi uint64; ptr unsafe.Pointer}`, every heap kind is a Go-GC-traced pointer, and
the ~1,800-line custom collector (`heap_gc.go`, `registry_gc.go`'s GC role,
`gc_safepoint.go`, keepAlive, weakref liveness, block slot+gen recycling) is
deleted. `BenchmarkHotPath*` was re-run on the pre-migration merge-base
(`2e93e2a`) vs the migrated HEAD, same machine (Apple M3, go1.26.2), `-count=10`.
Raw benchstat: `benchmarks/pointer-migration-hotpath.txt`. `go test ./...` green;
1166 doctests pass; `-race` clean on `vm/`.

### Time — geomean **−18.2%** (net faster), but not uniformly

| HotPath benchmark | premig | migrated | Δ |
|---|---|---|---|
| **ClassInstantiation** | 451 ns | 63.2 ns | **−86.0%** |
| **StringConcat** | 142 ns | 58.7 ns | **−58.8%** |
| **ExceptionSignalCatch** | 76.8 ns | 43.2 ns | **−43.7%** |
| **BufferedChannelThroughput** | 88.1 ns | 66.4 ns | **−24.7%** |
| VTableCachedLookup | 1.77 ns | 1.47 ns | −16.9% |
| GlobalLookup | 1.74 µs | 1.58 µs | −9.3% |
| IntArithmeticLoop | 6.46 µs | 6.31 µs | ~ (n.s.) |
| BlockEvalClosure | 43.3 ns | 43.9 ns | +1.2% |
| BlockEvalSimple | 28.8 ns | 30.1 ns | +4.7% |
| UnaryDispatch | 12.0 ns | 13.1 ns | +9.5% |
| KeywordDispatch | 37.1 ns | 42.0 ns | +13.2% |
| MethodDispatchCached | 23.5 ns | 27.2 ns | +15.9% |
| BlockBodyUnarySend | 75.6 ns | 87.9 ns | +16.3% |
| ArrayAtPut | 28.7 ns | 34.2 ns | +19.1% |
| **BinaryDispatch** | 19.1 ns | 24.9 ns | **+30.4%** |

### What actually happened (vs the model's prediction)

1. **The allocation/heap-touch wins are real and large — the model held there.**
   `ClassInstantiation` (−86%), `StringConcat` (−59%), `ExceptionSignalCatch`
   (−44%), channel throughput (−25%) are all paths that previously paid the
   registry's lock+map insert and then leaned on the custom collector to free.
   A plain `new` reclaimed by Go's GC is exactly the 12×-alloc / self-reclaiming
   win the spike predicted, now visible end-to-end.

2. **The 16-byte copy tax the model waved off DOES show up — on tight dispatch.**
   The model measured a 16-slot Value copy as ~equal to 8-byte (17.5 vs 17.2 ns)
   and concluded value size "doesn't materialize." Against the real interpreter
   it does: the pure-dispatch micro-benchmarks with no heap access to amortize it
   regressed **+9% to +30%** (worst: `BinaryDispatch`), and per-call arg boxing
   **doubled** in bytes (8→16, 16→32 B/op) because every boxed operand/arg is now
   a 16-byte Value. `allocs/op` is flat (−1.9% geomean); the cost is bytes moved,
   not extra allocations. This is the one place the model was optimistic, and it's
   exactly where the spike said to look ("optimistic by a small constant").

3. **Net is favorable and the correctness/maintainability win is the real prize.**
   Geomean time is −18%; the regressions are concentrated in the tightest scalar
   loops while the wins are on the allocation-heavy paths that dominate real
   programs. Independent of the ledger, the migration deletes ~1.8k lines of
   custom-GC machinery (and the leak/soundness bug cluster the review flagged) and
   makes the heap lock-free — the prerequisite for Stage 5 server parallelism.

### Honest caveats / follow-ups

- The dispatch regression is a genuine cost, not noise (all p=0.000, ±1%). It is
  the 16-byte `Value` propagating through operand-stack copies and arg slices.
  Candidate follow-up optimizations (not done here): avoid materializing 1–2 arg
  slices on the hot send path, and audit hot loops that copy Values where a
  pointer/index would do. None of these block Stage 5.
- Memory: `B/op` geomean +26.6% on these micro-benchmarks reflects the doubled
  Value width in short-lived arg boxes; against whole-program RSS this trades
  against no longer retaining dead objects in ID registries until a sweep. Not
  measured at the process level yet — worth a macro RSS check if it matters.
- The concurrency axis (the model's most dramatic claim, ~440× under 8 cores) is
  **not** exercised by these single-threaded HotPath benchmarks; it is validated
  separately by Stage 5's server-parallelism spike.
