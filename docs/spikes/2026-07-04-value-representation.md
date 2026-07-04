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
