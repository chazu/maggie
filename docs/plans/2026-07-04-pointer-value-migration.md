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

**Status:** Stage 1 COMPLETE. Stage 2 IN PROGRESS — 14 of the ~35 heap types
migrated to pointers across 8 commits (2a–2f). Branch `migrate/pointer-value`.
`main` untouched — do NOT merge until the migration finishes. At every commit:
`go test ./...` green, 1166 doctests pass, image rebuilds; vm `-race` clean at
the String, Exception, and Channel/Future checkpoints.

**MIGRATED to pointers (done, committed):** Object, Cell (stage 1); then Result,
BigInt, Exception, String, Dictionary, ArrayList, ClassValue, GoObject, Mutex,
WaitGroup, Semaphore, CancellationContext, Channel, Future.

**STILL id-based / symbol-encoded (deferred, deliberately):**
- **Process** (`processMarker`) — Values are constructed by-id in monitor/link/
  distribution paths (`processToValue(id)` in process_lifecycle.go, vm.go), so it
  needs a real id↔object map. Keep it symbol-encoded; the `processes` map also
  serves as a custom-GC root (mailbox scanning) until blocks migrate.
- **WeakRef** (`weakRefMarker`) — its clearing mechanism is the custom collector's
  `ProcessGC`; migrate together with the Go-native weak-ref rework in stage 3.
- **Block** (`tagBlock`) + **Context** (`tagContext`) — migrate in stage 3 (Block
  when the slot+gen recycling is removed).
- **Contrib/extension kinds** (`kindExtension`: http/grpc/cue/json/unix/sse/cli/
  exec, `externalProcessMarker`, etc.) + **RemoteRef/RemoteChannel** — STAGE 2g,
  next up. These use `ExtensionRegistry(marker)` AutoIDRegistries.

**Per-type mechanics that worked (keep doing this):**
- Reimplement the type's public `RegisterXxxValue`/`GetXxxFromValue` (or the
  `Register`/`Get` registry methods) as thin `makeHeap`/pointer-cast wrappers so
  CALL SITES DON'T CHANGE. Only the registry field + GC hooks + symbol dispatch go.
- Class-of resolution moved from `sd.Register(marker, …)` (symbol dispatch) to a
  `case kindXxx:` in `vm.classForHeap` (vm.go). Non-immediate heap dispatch also
  flows through `vtableForVM`'s `case v.isHeap()` (interpreter.go) and `Send`'s
  else-branch (vm.go). **Class values are special-cased in both to dispatch via
  `ClassVTable` directly — do NOT route them through classForHeap→MetaclassFor on
  the hot path (that races + eagerly builds metaclasses).**
- Serialization: `serialize()` routes heap Values to `serializeHeap` (serial.go),
  which switches on kind. Serializable kinds (String, BigInt, Dict, Exception,
  Channel) delegate; the rest return a clear non-serializable error. As a type
  migrates, move its case out of `serializeSymbolEncoded`'s marker switch.
- Custom GC (heap_gc.go): the `mark()` recursion cases for migrated container
  kinds (dict/arraylist/result/exception) are KEPT (they let the block sweep
  reach blocks stored inside), but the per-type ROOT ENUMERATION and per-type
  SWEEP are removed. Channels/Processes/Futures keep their id maps AS GC roots
  (buffered/mailbox block liveness) until blocks migrate — encoding-only change.
- GC tests (registry_gc_test.go, heap_gc_test.go) are being deleted/retargeted as
  types migrate; the pressure tests were repointed at the (deferred, still
  monitored) `Contexts` registry as stable filler.
- **Run `go test -race ./vm/` after any ClassValue/dispatch-touching change** —
  the metaclass race only surfaces under concurrent class-side sends.

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
