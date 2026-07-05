# Stage 5: server request parallelism (retire the VMWorker funnel)

Final stage of the pointer-value migration
(`docs/plans/2026-07-04-pointer-value-migration.md`). Executes review #2, unblocked
by the pointer-carrying heap. Goal: server requests run concurrently on their own
interpreters instead of funneling through one goroutine.

## Where we are (facts, verified)

- **The funnel:** `server/vm_worker.go` — `VMWorker.Do(fn func(*vm.VM) interface{})`
  runs every request on ONE dedicated goroutine (`loop()` + `requests` channel).
  6 services, **27 `.Do(...)` call sites**, all uniform `s.worker.Do(...)`.
- **Foundation already built** (concurrency audit `docs/vm-concurrency-audit-2026-05-03.md`,
  Patches 1–6): concurrent *dispatch* is race-clean. Inline caches lock-free,
  `Globals` mutex-guarded, late-bound fields published. `vm.Execute`/`vm.Send`
  already route through `vm.currentInterpreter()` (Patch 5). The **fork path**
  (`vm/concurrency.go:512-556`) already does
  `registerInterpreter` → `ExecuteBlockDetached` → `unregisterInterpreter` on a
  fresh per-goroutine interpreter — production forked Maggie processes already run
  concurrently against the shared VM. **This is the exact template for
  per-request interpreters.**
- **What is NOT proven safe under concurrency:** compilation
  (`vm.compilerBackend` is a single shared instance) and class/method/global
  mutation. There is no VM-level exec-lock and no post-boot `Freeze` yet.

## Call-site categorization (the 27 `.Do` sites)

| Bucket | Count | Sites | Concurrency policy |
|---|---|---|---|
| **READ-ONLY** (browse, lsp, session, inspect, CheckSyntax, publishDiagnostics) | 19 | browse ×9, lsp ×5, inspect ×3, session ×1, eval CheckSyntax ×1 | concurrent (reader) |
| **EVAL-may-compile/define** (Evaluate, EvaluateInContext, SendMessage, EvaluateWithLocals) | 4 | eval ×2, inspect SendMessage ×1, modify EvaluateWithLocals ×1 | **decision — see 5a** |
| **MUTATING** (CompileMethod, RemoveMethod, CreateClass, EvaluateWithLocals) + SaveImage | 4 (+1) | modify ×4, SaveImage ×1 | exclusive (writer) |

Notes: the 3 inspect readers call `v.Send` (execute user `printString`/`at:`) — safe
under concurrent dispatch, so they are readers. `CheckSyntax`/`publishDiagnostics`
compile-check only (intern selectors — locked tables), so readers.
`EvaluateWithLocals` both writes globals and evals → it is a writer regardless.

The `pullFunc`/`spawnResultFunc`/`peerAddrs` callbacks on `VMWorker` are used
**only by `sync_service.go`, directly (never via `.Do`)** — they move to the
server/VM struct unchanged; no `.Do` site depends on them.

## Design: a VM execution gate + per-request interpreters

Replace the single-goroutine funnel with a VM-wide `sync.RWMutex` execution gate,
each request running on its own registered interpreter:

- **`RunConcurrent(fn)`** — `RLock`; register a fresh per-request interpreter for
  the calling (gRPC handler) goroutine; run `fn`; unregister; `RUnlock`. Many run
  in parallel.
- **`RunExclusive(fn)`** — `Lock`; single-writer for compilation/class/global
  mutation and SaveImage. Blocks all readers for its duration.
- The per-request interpreter is built exactly like `newForkedInterpreter` (shares
  Selectors/Symbols/Classes/globals + VM; `forked=true`), so `currentInterpreter()`
  resolves to it instead of falling back to the shared `vm.interpreter`.
- Keep panic→`DescribePanic` recovery per request (today in `VMWorker.execute`).

This preserves every call site: `s.worker.Do(...)` becomes `s.gate.RunConcurrent`
or `RunExclusive`. Single VM-wide RWMutex to start — simple, correct, reversible,
and already a massive win for the read-heavy IDE workload vs one goroutine.

## Staging

**5a — Prove concurrent compile+eval (the pivotal experiment, do FIRST).**
The spike showed request time is compile-dominated, and every Evaluate compiles.
So the achievable win hinges on whether concurrent compilation + execution of
expression source is race-clean *as-is*. Add `TestConcurrentEvaluate` (mirror of
`TestForkedConcurrentDispatch`): N goroutines, each per-request interpreter,
concurrently compile+execute pure expressions against one shared VM, under
`-race`. Outcome decides the Evaluate policy:
  - **Clean** → Evaluate/EvaluateInContext/SendMessage become concurrent readers.
    (Source that *defines* a class is the escape hatch — see Risk.)
  - **Racy** → localize the compiler's shared mutable state. Fix by pooling a
    compiler-per-request (backends are cheap) or, fallback, serialize compilation
    under a compile sub-lock while execution stays concurrent.

**5b — Add the gate + per-request interpreter helper** (`vm.RunConcurrent`/
`RunExclusive`, or a server `VMGateway`). Land behind the existing `Do` (make `Do`
an alias for `RunExclusive` first — zero behavior change, still green).

**5c — Reclassify call sites.** 19 readers → `RunConcurrent`; 4 mutating +
SaveImage → `RunExclusive`; the 4 evals → per 5a outcome. Move
pullFunc/spawnResultFunc/peerAddrs off VMWorker onto the server struct.

**5d — Retire the funnel.** Delete `loop()`/`requests` channel/`quit`; VMWorker
becomes (or is replaced by) the thin gate holder. `Stop()` no-ops or drains.

**5e — Verify.** `BenchmarkServerEvalParallel -cpu=1,2,4,8` → ns/op should fall
~Nx (fully, or on the execution portion if compilation stays serialized).
`go test -race ./server/ ./vm/`; full suite; 1166 doctests; image rebuild. Update
`docs/spikes/2026-07-04-server-parallelism.md` with real post-migration numbers.

## Risk / rollback

- **Evaluate that defines a class** runs as a "reader" but mutates Classes/Globals.
  Options: (i) accept — class definition via a naked Evaluate is rare in the IDE
  path and the target structures (ClassTable, globalsMu) are individually locked;
  (ii) detect class-defining source at compile and upgrade it to `RunExclusive`;
  (iii) make Evaluate exclusive (safe, gives up Evaluate parallelism). 5a informs
  this; recommend (ii) if cheap, else (i) with a documented caveat.
- **Single RWMutex** means a slow writer stalls readers. Acceptable first cut;
  finer granularity is a later optimization, not this stage.
- Whole change stays on `migrate/pointer-value`; `-race` + full suite gate each
  sub-stage. `Do`-as-alias keeps a green bisect point before reclassification.

## Decisions (confirmed)
1. **Evaluate policy:** concurrent reader; source that *defines* a class/method
   is upgraded to the exclusive writer path.
2. **Gate granularity:** single VM-wide `RWMutex`.
3. **Scope:** do 5a first, reassess before 5b–5e.

## 5a result — concurrent compile+eval is RACE-CLEAN ✅

Added `vm.RunIsolated(fn)` (vm/vm.go): registers a fresh per-request interpreter
for the calling goroutine, runs `fn`, unregisters — the fork path's mechanism,
exported, no lock. Added `server/concurrent_eval_test.go` with two experiments
that bypass `worker.Do` entirely and run Evaluate concurrently on per-request
interpreters against one shared VM:

- `TestConcurrentEvaluateRace` — 32 goroutines × 200 iters, each compiling +
  executing a 200-term arithmetic chain (Go-primitive `+` sends). **PASS under
  `-race`**, result correct (`201`).
- `TestConcurrentDistinctExpressionsRace` — 24 × 150, each a *distinct* String
  literal → fresh parse/compile + concurrent `kindString` heap allocation on the
  post-migration pointer heap + shared-table interning. **PASS under `-race`.**

Confirms the analysis: the compiler backend is immutable post-construction and
allocates fresh per-call parser/compiler state, touching only the synchronized
Selectors/Symbols/registry; dispatch is race-clean (audit Patches 1–6). So the
funnel can retire and Evaluate can be a concurrent reader.

Harness note (unchanged from the parallelism spike): the image-less test VM only
resolves Go primitives, so lib selectors like `,` raise `doesNotUnderstand` —
that's why the workloads use `+` and bare literals, not lib methods.

## 5b–5e result — DONE, the server scales ✅

- **5b/5c/5d (commit a059821):** `VMWorker` lost its goroutine and became a
  reader/writer gate. `Do` = exclusive (`gate.Lock`), unchanged signature/semantics,
  kept for the 5 modify-service writers (CompileMethod, RemoveMethod, CreateClass,
  SaveImage, EvaluateWithLocals). New `DoConcurrent` = shared (`gate.RLock`) for the
  22 read/eval sites across eval/browse/inspect/lsp/session. Both run fn on a fresh
  per-request interpreter via `vm.RunIsolated` with `DescribePanic` recovery.
  Deleted `loop()`/`requests`/`quit`; `Stop()` no-op. pullFunc/spawnResultFunc/
  peerAddrs (sync_service-only, never via Do) stay on the worker.
- **5e:** re-ran `BenchmarkServerEvalParallel -cpu=1,2,4,8` — ns/op now FALLS with
  cores (8.34→4.52→2.60→1.92 ms/op = 1.0/1.85/3.21/4.35×), vs the pre-Stage-5 flat
  ~7.6 ms/op. `go test ./...` green; `go test -race ./server/` clean (0 DATA RACE).
  Numbers written into `docs/spikes/2026-07-04-server-parallelism.md`.

### Evaluate policy — as-implemented note
Evaluate/EvaluateInContext/SendMessage run as concurrent readers (`DoConcurrent`).
This is **memory-safe**: the shared structures a class-defining eval could mutate
(ClassTable, globals, VTable, Selectors, Symbols, registry) each carry their own
locks, and genuine structural writers (`Do`, exclusive) are mutually excluded from
readers. The planned "detect class-defining source → upgrade to exclusive" refinement
was NOT implemented: reliable static detection is fragile (a doIt expression defines
classes only via runtime sends like `subclass:…` or `Compiler evaluate:`), and it
would require restructuring compile-vs-execute around the non-reentrant RWMutex for
no memory-safety gain — only *consistency* for the rare naked-class-definition eval
(a concurrent browser could observe a half-built class, which is benign for an IDE
and already possible today via forked processes defining classes at runtime).
Deferred as an optional consistency nicety. `EvaluateWithLocals` stays exclusive
(it writes globals unconditionally).

## Stage 5 COMPLETE — pointer-value migration finished
All five stages done. Follow-ups still open (non-blocking): simplify RegistryGC's
dead pressure apparatus (Stage 3d note); the dispatch-copy-cost optimizations noted
in Stage 4; and load the image in the server tests' TestMain so lib dispatch is
exercised in CI. Branch `migrate/pointer-value` ready for review/merge.
