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

---

## RESUME HERE (next session pick-up)

**Status:** Stage 1 COMPLETE and pushed. Branch `migrate/pointer-value`
(commits `ec9308e` code + `162e71d` gitignore cleanup). `main` is untouched —
do NOT merge this branch until the migration is finished (it's mid-migration but
green at this checkpoint). Full `go test ./...` green, `-race` clean, 1166 lib
doctests pass, image rebuilds.

**What's already in place (value.go):**
- `type Value struct { hi uint64; ptr unsafe.Pointer }` with the full immediate
  method API preserved (reads `v.hi`).
- Heap-kind enum (`kindObject`, `kindCell`, `kindString`, `kindDictionary`,
  `kindArrayList`, `kindBigInt`, `kindException`, `kindResult`, `kindClassValue`,
  `kindGoObject`, `kindWeakRef`, `kindChannel`, `kindProcess`, `kindMutex`,
  `kindWaitGroup`, `kindSemaphore`, `kindCancellationContext`, `kindFuture`,
  `kindRemoteRef`, `kindRemoteChannel`, `kindPromise`, `kindExtension`) — most
  are ALREADY DEFINED, ready to use. `kindBlock`/`kindContext` exist but Block
  and Context are still immediate-id (deliberately deferred).
- Helpers: `makeHeap(kind, ptr)`, `v.isHeap()`, `v.heapKindOf()`, `RawBits()`.
- MIGRATED so far: Object, Cell (real pointers). Everything else is still
  immediate NaN-box or registry-ID.

**The proven recipe to migrate one registry-ID type (repeat per type):**
1. Change its `IsXxx(v)` to `v.ptr != nil && v.hi == kindXxx`.
2. Change its constructor (`NewXxxValue`/`RegisterXxx`) to
   `makeHeap(kindXxx, unsafe.Pointer(obj))` instead of registering in the
   AutoIDRegistry and returning `FromSymbolID(id|marker)`.
3. Change its accessor (`GetXxx(v)`) to `(*XxxObject)(v.ptr)` instead of a
   registry map lookup.
4. Delete that type's `AutoIDRegistry` field + its `mark`/`Sweep` in heap_gc.go
   / registry_gc.go. Remove its marker from markers.go.
5. `go build -gcflags=-e ./...`, fix the small cascade, `go test ./...`, doctests.
   Commit per type (or per small batch).

**Stage 2 order suggestion:** start with a LOW-volume, self-contained type to
re-confirm the recipe (e.g. `BigInt` or `Result`), then do `String` and
`Dictionary` and `ArrayList` (highest volume / most cascade), then the
concurrency kinds, then contrib (`kindExtension`).

**Then Stage 3:** once no type uses the registries, delete `heap_gc.go`,
`registry_gc.go`, `gc_safepoint.go`, `typed_registry.go`, `keepAlive`, the
safepoint polling in the interpreter, and the block slot+gen recycling.

**Then Stage 4/5:** re-run `BenchmarkHotPath*` vs baseline; then server
request-parallelism (per-request interpreters, retire VMWorker/Dispatch funnel).

**Gotchas learned in Stage 1:**
- `Value` is a comparable struct, so it works directly as a map key
  (compiler/codegen literalMap) and in `==` comparisons — no change needed there.
- `hi`/`ptr` are unexported; out-of-package debug/hex sites need `RawBits()`.
- Image serialization keys objects by content/pointer, largely insulated — but
  re-verify per type in Stage 2 (image_writer.go, serial.go).
- Object grew 80→112 bytes (16-byte Values × 4 inline slots); size guards in
  object_test.go / value_test.go were updated to match.
