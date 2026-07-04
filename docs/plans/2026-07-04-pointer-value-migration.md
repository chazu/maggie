# Migration: pointer-carrying Value + delete the custom GC

Executes the review's #1 architectural decision, backed by
`docs/spikes/2026-07-04-value-representation.md` (heap access ~23× faster, alloc
~12× faster + Go-GC-reclaimed, scales under concurrency). Server request
parallelism (#2) is unblocked by this and follows in a later stage.

## Design

`Value` changes from a NaN-boxed `uint64` to:

```go
type Value struct {
    hi  uint64          // NaN-boxed immediate when ptr==nil; heap kind tag when ptr!=nil
    ptr unsafe.Pointer  // GC-visible heap pointer, or nil for immediates
}
```

- **Immediates** (float, SmallInt, nil/true/false, real Symbol, Character):
  `ptr == nil`, `hi` keeps the *existing* NaN-box encoding. Every immediate
  method swaps `uint64(v)` → `v.hi`; logic is otherwise unchanged.
- **Heap objects** (~35 kinds: String, Dictionary, ArrayList, Object, Cell,
  BigInt, Channel, Process, Mutex, …, and contrib http/grpc/cue/json/unix/sse):
  `ptr` points to the Go object (now traced by Go's GC), `hi` holds a small
  `kind` tag replacing the old marker byte. Access is a pointer deref + cast,
  not a registry map lookup.

Because a live `Value` now keeps its heap object reachable through `ptr`, the
Go runtime traces the whole object graph. The custom collector and the ID
registries become unnecessary and are deleted:
- `vm/heap_gc.go`, `vm/registry_gc.go`, `vm/gc_safepoint.go` (STW collector +
  safepoint polling)
- `vm/typed_registry.go` (AutoIDRegistry) and the ID-keyed maps in
  `object_registry.go` / `concurrency_registry.go`
- `keepAlive`, weak-ref liveness plumbing, block slot+generation recycling
  (direct pointers can't alias-after-recycle), the 2^24 ID-exhaustion machinery

Registries kept for *functional* reasons (not liveness) are evaluated
case-by-case: process-name lookup and distribution export tables may still need
an id↔object map; those become plain maps without the GC role.

## Staging (each stage ends compiling + tests green where noted)

1. **Core** — new `Value` struct; rewrite `value.go` immediates; add `kind`
   tags + heap accessors; `markers.go` redesign. (vm/ won't compile yet.)
2. **Heap types** — convert every Register/Get pair (object_registry,
   concurrency_registry, string/dict/arraylist/bigint/… primitives, contrib) to
   pointer form. Delete the AutoIDRegistry + ID maps. **vm/ compiles.**
3. **Delete GC** — remove heap_gc/registry_gc/gc_safepoint + safepoint calls in
   the interpreter; remove keepAlive/weakref liveness. **vm/ tests green.**
4. **Cascade** — fix compiler/server/cmd/contrib packages + their tests.
   **`go test ./...` green.** Re-run BenchmarkHotPath* vs the pre-migration
   baseline; update the spike doc with real numbers.
5. **Server parallelism** — with a thread-safe (pointer, no shared-registry)
   heap, run each request on its own interpreter; retire the `VMWorker`/
   `Dispatch` funnel; re-run the parallelism spike to confirm scaling.

## Risk / rollback

- Whole effort on branch `migrate/pointer-value`; `main` stays green.
- `unsafe.Pointer` discipline becomes load-bearing (already true for the old raw
  Object/Cell encoding, now uniform); `-race` + the full suite + doctests gate
  each stage.
- Serialization/image format keys heap objects by *content*, not id, so it is
  largely insulated; verified in stage 4.
