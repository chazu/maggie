# Maggie Codebase Review — 2026-07-04

A thorough multi-dimensional review of the Maggie VM covering: (1) memory & GC
issues, (2) YAGNI/over-engineering, (3) implementation inconsistencies, (4)
separation of concerns, and (5) testing gaps. Findings were produced by five
parallel deep-read passes and the highest-severity items were re-verified
against the source before ranking.

**Overall assessment.** The project is more mature than its experimental
framing suggests: CI runs `go test -race` with a coverage gate, staticcheck,
and scheduled fuzzing; the tracing GC, block-registry recycling, and
distribution E2E tests are genuinely well-built. The serious problems cluster
in three places: **the GC covers only 3 of ~12 Value-holding registries** (the
rest leak or are swept unsoundly), **the trust/restriction enforcement has real
holes** (a fork can escape restrictions; `--serve` runs permissive; a
documented permission is never checked), and **two large parallel-implementation
subsystems (AOT + self-hosted compiler, ~5,200 lines) carry maintenance cost
with zero default-path users**.

Most of the structural friction traces to a single root cause: the
`vm` ↛ `vm/dist` import cycle, worked around by duplicating the wire envelope
and signing logic in 2–3 places and by hosting ~260 lines of distribution glue
in the CLI binary.

---

## Criticality Ranking (all dimensions merged)

| # | Severity | Finding | Dim |
|---|----------|---------|-----|
| 1 | **CRITICAL** | Plain `Object`s (`keepAlive`) are never collected — sweeper exists but has zero callers | GC |
| 2 | **CRITICAL** | `ArrayList` instances are never swept anywhere — every collection leaks | GC |
| 3 | **CRITICAL** | STW soundness holes: HTTP/RPC goroutines allocate & mutate mid-collection | GC |
| 4 | **CRITICAL** | Extension registries (TupleSpace/CUE) hold Values but aren't GC roots → use-after-free / aliasing | GC |
| 5 | **CRITICAL** | Remote/mixed `SelectCase` polling path has zero test coverage (concurrent timing code) | Test |
| 6 | **HIGH** | Blocking primitives without `enterBlocked` (Mutex/WaitGroup/Semaphore/Future/`Process wait`/select) starve the GC entirely | GC |
| 7 | **HIGH** | `mainInterpreterActive` heuristic lets the collector run concurrently with a live main interpreter | GC |
| 8 | **HIGH** | Content-hash computation drifted between the two compile pipelines → distribution auto-ban risk | Consistency |
| 9 | **HIGH** | Plain `fork` does not inherit `forkRestricted:` restrictions (5 of 7 sites pass `nil`) | SoC / Security |
| 10 | **HIGH** | `mag --serve` mounts spawn/channel RPCs with a permissive trust store | SoC / Security |
| 11 | **HIGH** | `PermMessage` is parsed, serialized, documented — but never enforced at `DeliverMessage` | YAGNI / Security |
| 12 | **HIGH** | `GoToValue` panics the VM on any wrapped Go int64 > 2^47 | Consistency |
| 13 | **HIGH** | BigInt / Result / Context / Future / GoObject registries leak (registered, never swept) | GC |
| 14 | **HIGH** | Concurrency ID space is 2^24 (~9 min at 30k/s), not the "4G/5 days" the safety comment claims | GC |
| 15 | **HIGH** | AOT compiler (742 + 777 lines): zero production callers, doubles the opcode set forever | YAGNI |
| 16 | **HIGH** | Self-hosted compiler (~3,700 lines): one experimental flag, 6 weeks of drift, silent fallback masks breakage | YAGNI |
| 17 | **HIGH** | `vm/source_update.go` (rewrites user `.mag` files) is completely untested | Test |
| 18 | **HIGH** | lib doctests never run in CI; the known String-doctest crash can persist indefinitely | Test |
| 19 | **HIGH** | CI fuzz job only scans `./compiler/`; `FuzzImageReader` never runs; no CBOR/envelope fuzzing | Test |
| 20 | **MED-HIGH** | Five coexisting error-signaling conventions across primitives; `ArrayList at:` vs `Array at:` disagree | Consistency |
| 21 | **MED-HIGH** | Envelope signing triplicated in `vm` (2×) + `vm/dist` (1×); must stay byte-identical or all messages fail | Consistency / SoC |
| 22 | **MEDIUM** | `cmd/mag run()` — 377-line dispatcher, 3 subcommand mechanisms; `reorderArgs` already drifted (missing `typecheck`) | SoC |
| 23 | **MEDIUM** | `debug.FreeOSMemory()` on every sweep → forced full-Go-GC storms under allocation load | GC |
| 24 | **MEDIUM** | State-based sweeps (closed channels / done processes) free entries still held by live Values | GC |
| 25 | **MEDIUM** | `OpSendAt` fast path returns bytes for `String at:`; real path returns Characters (diverges under experimental compiler) | Consistency |
| 26 | **MEDIUM** | `cmd/bootstrap` reimplements `pipeline.CompileAll` (the hash-drift vector of #8) | SoC |
| 27 | **MEDIUM** | `compiler` imports all of `vm` for ~30 opcodes + builders; extract a `bytecode` package | SoC |
| 28 | **MEDIUM** | `SyncService` = sync + trust + messaging + spawn + channels in one 889-line type | SoC |
| 29 | **MEDIUM** | Three overlapping string-extraction helpers with different acceptance sets | Consistency |
| 30 | **MEDIUM** | Capability manifest / `ChunkModule` wire machinery is dead-in-practice (all callers pass `nil`) | YAGNI |
| 31 | **MEDIUM** | Distributed registries (`channelExports`, `remoteChannels`, `nodeRefs`, `pendingSpawns`) only grow / leak on node death | GC |
| 32 | **MEDIUM** | goleak / goroutine-leak detection absent; node-death-mid-spawn untested E2E; supervisor restart semantics untested | Test |
| 33 | **LOW** | Repo clutter: `*.prof`, `vm.test` (17MB), `mag` (88MB), duplicated `maggie.image`, 161MB stale worktrees; not gitignored | YAGNI |
| 34 | **LOW** | `prim` prefix / wrapper-over-primitive pattern applied ~25% of the time | Consistency |
| 35 | **LOW** | Indexing violations: `instVarAt:` is 0-based (ST-80 says 1-based), `SqliteRows columnAt:` 0-based | Consistency |
| 36 | **LOW** | Locking/registry style variance; ~82 unlocked `vm.globals` writes rely on implicit bootstrap-only invariant | Consistency / GC |
| 37 | **LOW** | Marker/CBOR-tag helpers unused (`isMarkedValue` 0 callers; 11 files hand-roll); contrib re-declares a CBOR tag | Consistency |
| 38 | **META** | `CLAUDE.md` mandates `pudl observe`, but the installed `pudl` has no such subcommand — instruction is unsatisfiable | Process |

---

## 1. Memory & GC Issues

The tracing collector (`vm/heap_gc.go`) is well-engineered but only sweeps
**strings, dictionaries, and blocks**. Every other Value-holding registry either
leaks unboundedly or is swept by object *state* rather than *reachability*, and
the stop-the-world barrier has four concrete soundness holes.

### CRITICAL

**1.1 — `keepAlive` (all plain Objects) is never collected.**
`vm/vm.go:1413 CollectGarbage()` is the only code that sweeps `vm.keepAlive`, and
it has **zero call sites** (verified). Every `Object new`, `Association`,
`Point`, and every deserialized RPC payload (`vm/serial.go:580`) pins into
`keepAlive` forever. `heap_gc.go:425` explicitly defers object sweeping to
"the existing CollectGarbage path" — which never runs. This is the same leak
class as the 3.97GB→89MB block fix, for the largest remaining category.
*Fix:* sweep `keepAlive` inside `collectHeapGarbageLocked` using the
already-computed `ls.objects` live set, under the existing STW barrier. Objects
are NaN-boxed raw pointers, so `keepAlive` is the *only* thing keeping Go's GC
from freeing a live object — the sweep must use the STW-complete root set.

**1.2 — ArrayLists are never swept.** No `SweepArrayLists` and no
`delete(cr.arrayLists, …)` exist anywhere (verified). `ArrayList` is the
workhorse collection; a server building a temporary list per request leaks the
`ArrayListObject` (with its `elements []Value` slice) unconditionally. Combined
with the 2^24 ID cap (1.14 below), `ArrayList new` fails permanently after
16.7M allocations. *Fix:* add `SweepArrayListsLive(ls.arrayLists)` to the STW
sweep.

**1.3 — STW soundness holes.** The barrier waits only for interpreters
registered *at check time*. Three unguarded paths allocate/mutate mid-cycle:
(a) new HTTP request goroutines register an interpreter and push a frame *before*
hitting the entry safepoint; (b) `DeliverMessage`/`ChannelSend` RPC handlers
deserialize payloads on plain goroutines with **no interpreter at all**,
registering new strings while the sweep is running; (c) `Mailbox.Receive`
dequeues before `exitBlocked`, so a receiver woken mid-trace removes the value
from the root mailbox while it lives only in a Go local (the channel path gets
this right — `popPending` runs *after* `exitBlocked`). Result: non-deterministic
frees of live strings, then aliasing after ID recycling. *Fix:* block
`registerInterpreter`/RPC entry on the GC barrier while `gcRequested` is set;
adopt "allocate-black" (IDs born during GC are treated live); in
`Process primReceive`, wait out GC before dequeuing.

**1.4 — Extension registries aren't GC roots.** `markRoots` enumerates 13 root
categories but not `or.extensions`. `TupleSpaceObject.tuples` stores raw
`vm.Value` entries; string IDs recycle through the free-list. `ts out: aString`
with no other reference → next GC frees and recycles the string id → later
`ts in:`/`rd:` returns an aliased Value = silent data corruption. Live risk
given the tuplespace-backed BBS. *Fix:* give extension registries an optional
`MarkValues(func(Value))` interface and call it from `markRoots`.

### HIGH

**1.5 — Blocking primitives starve the GC.** Only 7 sites call `enterBlocked`.
`Mutex lock`, `WaitGroup wait`, `Semaphore acquire`, `Future await`,
`Process wait`, and blocking `Channel select:` all leave `gcState == Running`,
so `allMutatorsStopped()` never returns true, the 250ms barrier times out, and
**the cycle aborts with no sweep**. One parked process disables GC entirely —
and Supervisor trees park on monitors constantly. *Fix:* bracket every blocking
primitive with `enterBlocked`/`exitBlocked`, publishing held Values; for
`reflect.Select`, publish all case Values.

**1.6 — `mainInterpreterActive` heuristic is unsound.** `gc_safepoint.go:124`
returns `mi != nil && mi.fp >= 0 && mi.sp > 0`, read unsynchronized (a data race
itself). The operand stack is legitimately empty (`sp == 0`) between statements
mid-method; the collector then sweeps while the main goroutine keeps running to
its next safepoint. *Fix:* an explicit atomic "inside Execute" flag, not an
inference from `sp`.

**1.7 — BigInt / Result / Context / Future / GoObject registries leak.** Each
has an `Unregister*` with zero callers and no sweep case, though
`registry_gc.go` installs *growth pressure hooks* for them (so they trigger
expensive STW traces that can't shrink them — see 1.9). `1000 factorial`,
idiomatic `Result ok:`/`error:`, `thisContext`, `asyncSend:`, and every gowrap
call leak until the 2^24 panic. Results/Contexts are additionally treated as GC
*roots*, pinning their payloads — a leak amplifier. *Fix:* one mechanism —
extend the STW liveSet to record live ids for every marker kind and sweep every
registry from it.

**1.8 — Weak references are strong; finalizers never run.** `WeakReference`
holds `*Object` directly; `Clear()` only happens in the dead `CollectGarbage`.
The feature is functionally broken and leaks. *Fix:* call
`weakRefs.ProcessGC(ls.objects)` from the STW sweep (needs 1.1's object live set).

**1.9 — 2^24 ID space, mis-documented.** `concurrencyIDMax = (1<<24)-1` =
16.7M, but the safety comment reasons about "4G IDs … 30k/sec for 5 days." At
30k/sec that's **9.3 minutes**. Channels/processes/futures/arrayLists never
recycle IDs. A channel-per-request server dies after 16.7M requests. *Fix:*
adopt the block registry's proven slot+generation scheme, or at minimum fix the
comment and add 50%-consumption telemetry.

### MEDIUM

- **1.10** State-based sweeps free live data: a *closed* channel with undrained
  buffered values, a *done* process whose result isn't yet awaited. Combine
  state with reachability using the STW liveSet.
- **1.11** `debug.FreeOSMemory()` (commit 46de074) runs on **every** sweep,
  including growth-pressure sweeps and no-op cycles — forced full Go GC storms
  under allocation load. Gate it to timer sweeps or `TotalSwept > N`, or
  rate-limit to ≥30s.
- **1.12** Distributed registries (`channelExports`, `remoteChannels`,
  `nodeRefs`) only grow; `pendingSpawns` + awaiting processes leak forever when
  the remote node dies mid-spawn (`remote_lifecycle.go` drains monitors/links/
  channels but not pending spawns). Resolve-error all futures for a dead node.
- **1.13** `debug.FreeOSMemory` growth hooks are installed for registries the
  sweep can't reclaim (falls out of fixing 1.7).

### LOW

- **1.14** `CollectGarbage` is unsafe if ever wired up (partial root set, no STW)
  — **delete it** before someone "fixes" 1.1 by calling it.
- **1.15** Symbol table append-only and reachable from `aString asSymbol` on
  attacker input. `heap_gc.go` header doc overstates coverage.
  `Mailbox.ReceiveTimeout` spawns a timer goroutine per wakeup iteration.

**Recommended GC remediation — one mechanism fixes most of it:** extend the
existing STW liveSet to record live ids for *all* marker kinds, sweep every
AutoID/concurrency registry from it, and add allocate-black semantics for
registrations that occur while `gcRequested` is armed. This closes 1.1, 1.2,
1.7, 1.8, and the state-vs-reachability half of 1.10 in one place.

---

## 2. YAGNI / Over-engineering

**Fairness baseline:** distributed execution, content-addressed sync, and
tuplespaces are *documented core goals* (ROADMAP.md), not YAGNI. TODO density is
only ~10 across all Go code. The findings target genuinely speculative or dead
parts.

**2.1 (HIGH) — AOT bytecode→Go compiler.** `vm/aot.go` (742) + `aot_test.go`
(777). `NewAOTCompiler` has zero callers outside one test; `RegisterAOTMethods`/
`LookupAOT` have zero non-test callers. README admits it's "not yet user-facing —
there is no CLI flag." It handles 63 opcodes vs the interpreter's 61 — every
bytecode change must be implemented twice, and it was already touched in the
2026-06-30 hardening sweep. `vm.Send`'s hot path pays an atomic load + nil check
per compiled-method send for a table nothing populates. *Fix:* gate behind
`//go:build aot` or delete to a branch; keep the design notes.

**2.2 (HIGH) — Self-hosted Maggie compiler.** `lib/compiler/*.mag` (3,186) +
`vm/compiler_dispatch.go` (553, no tests). Reachable only via
`-experimental-maggie-compiler`, exercised by no test/CI. Measurable drift:
`lib/compiler/` last tracked the 1-based switch (2026-05-20) while the Go
compiler advanced 6+ weeks past it. `MaggieCompilerBackend.Compile` silently
falls back to the Go compiler on failure, *masking* drift. *Fix:* either run the
doctest suite under the flag in CI so drift breaks loudly, or shelve it
(precedent: `lib/yutani/ide/shelved/`) — which then collapses the
`CompilerBackend` interface to one implementation.

**2.3 (MEDIUM) — Dead distribution machinery.** `PermMessage` never enforced
(see 3.x / security). Capability manifests: all six production call sites pass
`nil` caps (ROADMAP gap #3). `ChunkModule`/`ModuleToChunk` has zero non-test
callers but a full receive-side verification path. *Fix:* either populate caps
from `manifest.Sync.Capabilities` at push time (one plumbing change) or strip the
params (CBOR `omitempty` makes removal wire-compatible).

**2.4 (LOW) — Repo hygiene.** Untracked-but-not-gitignored clutter sitting for
2 months: `cpu*.prof` (6 files), `vm.test` (17.6MB), `mag` (88MB). Tracked
artifacts: `emacs/*.elc`, generated `site/` (124 files), `maggie.image`
committed **twice** (root + `cmd/mag/`, 57 commits of binary churn). Stale
`.claude/worktrees/` = 161MB that contaminates greps. *Fix:* gitignore
`*.prof`/`*.test`/`/mag`/`*.elc`; make `cmd/mag`'s image a build step; prune
worktrees.

**2.5 (LOW) — Contrib bloat.** All five contrib plugins (DuckDB+arrow, a second
SQLite, CUE, gRPC, ganso) unconditionally linked → 88MB binary with two embedded
SQL databases. The plugin seam already exists; add build tags for a
`-tags minimal` build.

**Cleared as not-YAGNI (verified live):** `server/` IDE RPCs (used by emacs),
`types/` effect system (used by `mag typecheck` + typed content hashes end-to-
end), `gowrap/`, `pipeline/`, `ServerOption` pattern, image v3/v4 (already
removed), Supervisor/Cluster/HashRing libs.

---

## 3. Implementation Inconsistencies

**3.1 (HIGH) — Content-hash drift between compile pipelines.**
`pipeline.CompileAll` hashes with instVar maps and namespace-resolved globals;
`cmd/bootstrap/main.go compileAllFiles` hashes with `resolveGlobal=nil` and
`instVars=nil` (verified at lines 200/311/350). Identical source → different
`ContentHash` depending on which binary compiled it. Because distribution
verifies chunks by hash and **the trust layer auto-bans a peer after 3 hash
mismatches**, this is a correctness/security risk, not just duplication. *Fix:*
route bootstrap through `pipeline.Pipeline` (it already imports it); add a golden
test asserting bootstrap-image hashes equal pipeline hashes.

**3.2 (HIGH) — `GoToValue` panics on large ints.** `FromSmallInt` panics out of
the 48-bit range (`value.go:239`), and `go_object.go:235/238` calls it unchecked.
Any `mag wrap`-generated binding returning an int64 > 2^47 (ns timestamps,
hashes, ids) **crashes the VM** instead of promoting to BigInt. *Fix:* use
`TryFromSmallInt` and fall back to `NewBigIntValue`.

**3.3 (HIGH) — Five error-signaling conventions.** Raise exception (~180 sites),
return Failure Result (~165), silently return `Nil` (272 `return Nil`;
`http_primitives.go` has 42 with zero signals), return a neutral value, or coerce
to zero. Choice is per-file, not per-error-kind. User-visible trap:
`Array at: 99` raises `SubscriptOutOfBounds` but `ArrayList at: 99` silently
returns nil — same selector, opposite contract. `Dictionary at:put:` on a wrong
receiver silently returns `value` while `at:` two lines up signals. *Fix:*
sanction two styles — exceptions for programmer errors, Result for environmental
failures — and migrate silent-Nil sites; fix `ArrayList at:` and
`Dictionary at:put:` now (one-liners).

**3.4 (MED-HIGH) — Envelope signing triplicated.** `node_primitives.go:108`,
`remote_lifecycle.go:228`, and `dist/message.go:36` each hand-build the
`payload || nonce || targetProcess` signature. The two vm-side builders differ
only in `TargetName` vs `TargetProcess`. Any change to the signed-field set
silently produces unverifiable envelopes. *Fix:* merge the two vm-side builders;
extract the signature payload into a leaf `wire` package importable by both `vm`
and `vm/dist` (also fixes 4.3).

**3.5 (MEDIUM) — `OpSendAt` semantics diverge.** The interpreter fast path
returns a byte SmallInteger for `String at:` and makes `at:put:` a silent no-op;
the real dispatch path (`String>>at:` → `primAt:`) returns a Character; `aot.go`
does a third thing (full dispatch — the correct one). The Go compiler never
emits `OpSendAt`, but the self-hosted compiler does, so experimental-compiler
code gets different string semantics. *Fix:* make `primitiveAt`/`primitiveAtPut`
fall back to full dispatch for non-Array receivers.

**3.6 (MEDIUM) — Three string-extraction helpers.** `vm.valueToString` (126
uses, String|Symbol), `registry.GetStringContent` (91 uses, String only),
`vm.getStringLike` (6 uses, String|Character|Symbol). Whether a primitive accepts
a Symbol depends on which helper the author grabbed; all three return `""` on
failure, indistinguishable from empty. *Fix:* one `(string, ok bool)` helper with
a documented acceptance policy.

**3.7 (LOW) —** `prim` prefix applied ~25% of the time (100 `prim*` vs 291 direct
public registrations); `instVarAt:` is 0-based (ST-80 is 1-based) and silently
returns Nil; `SqliteRows columnAt:` 0-based; marker/CBOR-tag helpers unused while
11 files hand-roll the mask math; contrib re-declares CBOR tag 27004 privately;
8 lib files use legacy `# comment` style with no class docstring.

---

## 4. Separation of Concerns

**4.1 (HIGH) — Fork restriction escape.** `newForkedInterpreter(nil)` is called
at 5 fork sites (`concurrency.go:590/627/668/720`, `http_primitives.go:450`)
while only the two `forkRestricted:`/`forkWithout:do:` sites pass the caller's
`hidden` map (verified). A restricted process running `[ Compiler evaluate: '…' ]
fork` gets a child with `hidden == nil` — full global access. MEMORY.md claims
"restrictions inherited by child forks," which the plain-fork sites contradict.
*Fix (also security):* move inheritance into the constructor — take the caller
interpreter and always merge its `hidden`; callers wanting a clean slate pass an
explicit flag. One change fixes all five sites. **Recommend a doctest to confirm
the escape before/after.**

**4.2 (HIGH) — `--serve` runs permissive.** The sync path builds `server.New`
with `WithTrustStore(loadTrustStore(m))`; `newLanguageServer` (the `--serve`/IDE
path) passes neither trust store nor spawn-result func, so `server.New` defaults
to `NewPermissiveTrustStore()` and still unconditionally mounts `SyncService`
including `DeliverMessage`/`SpawnProcess`/channel RPCs. `mag --serve` therefore
accepts remote spawn from any signed peer under allow-all permissions. *Fix:* a
single `distconn.Wire(...) []ServerOption` used by every `server.New` call, or
make `server.New` require an explicit trust store (no permissive default).

**4.3 (HIGH) — `vm` ↛ `vm/dist` cycle.** `MessageEnvelope` + signing are defined
twice (`node_primitives.go` vs `dist/message.go`) because the cycle blocks
importing the protocol types. But the cycle is narrow: only 4 files in `vm/dist`
import `vm` (for `ContentStore`/`CompiledMethod`/`ClassDigest`/`HashClass`);
`message.go`/`identity.go`/`trust.go` import nothing from `vm`. *Fix:* extract a
leaf `vm/dist/wire` package (envelope, signing, `NodeID`, canonical CBOR mode);
`vm/dist` re-exports via type aliases so no caller changes; `vm` then imports it
and deletes ~55 lines of hand-rolled signing. **This single extraction unblocks
3.1, 3.4, 2.3, 4.6, and shrinks 4.5.**

**4.4 (HIGH) — Interpreter struct mixes three concerns.** One struct holds shared
VM tables (aliased copies with a "would be part of VM" comment), per-process
security/COW state (`localWrites`/`hidden`/`forked`/`processID`), and bytecode
execution state. Restriction policy is inlined into `execPushGlobal`/
`execStoreGlobal` with hand-repeated locking. Process identity is located by
goroutine id with a silent fallback to the main (unrestricted) interpreter for
any unregistered goroutine. *Fix:* extract a `processContext` with
`lookupGlobal`/`storeGlobal` methods; provide one
`RunOnNewGoroutine(caller, hidden, fn)` entry point owning fork/register/
unregister/inheritance (also fixes 4.1).

**4.5 (MEDIUM) — `cmd/mag run()`.** 377 lines, three subcommand mechanisms
(immediate dispatch / stash-and-run-after-VM / mid-args scanning), each with its
own `paths[0]` strip hack. `reorderArgs`'s hardcoded subcommand set has **already
drifted** — `typecheck` is missing (verified), so `mag typecheck --flag` has its
flags eaten by the top-level parser. *Fix:* a `command` registry struct
(`{name, needsVM, run}`); `reorderArgs` derives its set from the registry,
killing the drift class.

**4.6 (MEDIUM) — Others.** `cmd/bootstrap` reimplements `pipeline.CompileAll`
(root of 3.1); `compiler` imports all of `vm` for ~30 opcodes + builders (extract
a `bytecode` package, `peephole.go` first — it uses only opcodes yet can't be
tested without the whole VM); `SyncService` is 889 lines doing sync + trust +
messaging + spawn + channels with `IsBanned` re-checked per handler (split into
embedded handler structs + a trust interceptor, no proto change);
distribution-wiring `build*Func` closures (~260 lines) live in the CLI only
because of the cycle.

**4.7 (LOW) —** Enumeration protocol implemented twice (Go primitives + `lib/
Enumerable.mag`); primitive docstrings applied by the CLI at boot rather than
baked into the image (embedders linking `vm` without `cmd/mag` get undocumented
primitives). The split rule is followed but written nowhere.

---

## 5. Testing Gaps

**Baseline (good):** CI runs `-race` + coverage (40% floor) + vet + staticcheck +
scheduled fuzz; vm/ has ~1,200 test functions incl. GC safepoint stress,
registry-sweep, and the block-registry-leak regression tests; 12 distribution
E2E tests incl. signature-tampering.

**5.1 (CRITICAL) — Remote/mixed `SelectCase` polling has zero coverage.**
`channel_select.go`'s `hasRemote` branch (exponential backoff 1ms→100ms) is
entered by no test; `SelectCase` with `Remote != nil` appears in no test file.
Untested concurrent timing code that can livelock, drop a ready case, or race a
close. *Fix:* `TestSelect_RemoteOnly_ReceiveReady`,
`_MixedLocalWinsWhileRemotePolling`, `_RemoteClosedDuringPoll`,
`_RemoteBackoff_ManyGoroutines` under `-race`.

**5.2 (HIGH) — `vm/source_update.go` untested.** 283 lines that rewrite user
`.mag` files in place — the one untested file that can **destroy user data** on a
bad range calculation. *Fix:* round-trip test (only target method changes),
multi-part keyword selectors, method-not-found leaves file byte-identical, nested
brackets in body, unicode.

**5.3 (HIGH) — lib doctests never run in CI.** CI runs only `go test`/vet/
staticcheck/fuzz; nothing executes `mag test lib/`. The entire Smalltalk stdlib
(72 doctest lines in String.mag alone) is verified only by hand — which is why
the known String-doctest crash can persist. *Fix:* add `make mag && ./mag test
lib/`; fix or `skip:`-annotate the String crash first.

**5.4 (HIGH) — Fuzz scope.** The CI fuzz step greps only `./compiler/*_test.go`,
so `vm/image_reader_fuzz_test.go` never runs in CI. No fuzz targets for the
network-facing decoders: `dist/wire.go`, `dist/message.go` (decoded *before*
signature verification — reachable by unauthenticated attackers), `serial.go`
(circular refs), `DeserializeSpawnBlock`. *Fix:* generalize the CI loop over
`./compiler/ ./vm/`; add `FuzzEnvelopeDecode`, `FuzzChunkUnmarshal`,
`FuzzSerialDecode`, `FuzzDeserializeSpawnBlock`.

**5.5 (HIGH) — `cmd/mag/main.go` loading pipeline untested.** No tests for
`loadProject`, `compileAll`, `checkNamespaceCollisions`, `prefixDepNamespaces`,
`remapImport` — the collision detector is the only guard against silent class
shadowing. *Fix:* temp-dir fixtures for two-pass forward references, reserved-name
rejection, consumer-override remap, PascalCase fallback.

**5.6 (MEDIUM) — Leak/perf/E2E gaps.** No goleak or `NumGoroutine` checks
anywhere (a leaked poller/ticker after `Shutdown()` is invisible); commit 46de074
shipped with no heap-return test; no node-death-mid-spawn or channel-owner-crash
E2E; Supervisor restart strategies (`oneForAll`/`restForOne`/intensity) have only
3 constructor doctests; benchmarks aren't run in CI (regression gate is
manual-only and covers only `BenchmarkHotPath*`); 33 lib files have zero
doctests including pure-functional `Json.mag`/`Number.mag`/`Cluster.mag`.

---

## Suggested Execution Order

Each step compiles and ships independently.

1. **`newForkedInterpreter` caller-merge fix** (4.1) — small, security-relevant;
   add a doctest confirming the escape.
2. **STW liveSet extended to all registries + allocate-black** (1.1, 1.2, 1.7,
   1.8, 1.10) — the single highest-leverage GC fix.
3. **`enterBlocked` on all blocking primitives** (1.5) + main-interpreter flag
   (1.6) + RPC/HTTP barrier entry (1.3) — closes the GC soundness holes.
4. **`vm/dist/wire` leaf package** (4.3) — unblocks 3.1, 3.4, 2.3, 4.6.
5. **Route bootstrap through `pipeline` + golden hash test** (3.1, 4.6) — kills
   the auto-ban risk.
6. **Unified `server.New` trust options** (4.2) + **enforce `PermMessage`** (2.3).
7. **`GoToValue` → `TryFromSmallInt`** (3.2); fix `ArrayList at:` /
   `Dictionary at:put:` (3.3).
8. **CI: `mag test lib/`, widen fuzz to vm/, add CBOR/envelope fuzz targets**
   (5.3, 5.4); test `source_update.go` (5.2) and remote-select (5.1).
9. **Gate/shelve AOT + self-hosted compiler** (2.1, 2.2); repo hygiene (2.4).
10. **`command` registry in `cmd/mag`** (4.5); `bytecode` package (4.6);
    `SyncService` split (4.6) — scheduled refactors.

---

## Process Note

`CLAUDE.md` mandates recording observations with `pudl observe`, but the `pudl`
binary on PATH is a different tool (a data-lake CLI) with no `observe`
subcommand (verified: `unknown command "observe" for "pudl"`). Every review pass
independently hit this. The instruction is currently unsatisfiable and should be
corrected or the tool updated.

---

## Remediation Status (updated during the fix sweep)

Work landed on branch `cleanup/delete-dead-code`. Commits: dead-code deletion,
GC sweep, security, consistency, GC-perf/tests, CI/fuzz.

### Fixed
- **Dead code**: AOT compiler and dead module-chunk path deleted; self-hosted
  compiler shelved to `lib/shelved/` (−2,229 LoC Go, −3,186 shelved).
- **GC leak (Tier 2 / C1, C2, H5)**: the tracing collector now sweeps keepAlive
  objects, array-lists, and weak references; `CollectGarbage` uses the complete
  root set; `RootMarker` + extension-root pass closes C4 for the persistent
  tuple store. `SaveImageBytes` serialized with the collector; a pre-existing
  `RegistryGC` data race fixed (`-race` clean).
- **Security**: fork-restriction escape (#9) closed with inheritance in the fork
  path + regression test; `mag --serve` no longer permissive — `server.New`
  defaults to a secure trust store and the peer-facing `SyncService` is mounted
  only via `WithSyncService` (closes the unauthenticated channel-RPC exposure);
  `PermMessage` now enforced in `DeliverMessage` + regression test.
- **Consistency**: `GoToValue` promotes large ints to BigInteger instead of
  panicking; `ArrayList>>at:`/`at:put:` and `Dictionary>>at:put:` signal like
  `Array`/`at:`; `reorderArgs` no longer drops `typecheck` flags.
- **GC perf (M2)** scavenge rate-limited; **ID-space doc (H8/H9)** corrected.
- **Tests/CI**: `source_update.go` tested; lib doctests now gate CI; fuzzing
  widened to `vm/`+`vm/dist/` with new decoder targets (envelope/chunk/value/
  spawn-block). `OpSendAt` divergence (3.5) resolved by shelving the emitter.

### Deferred (need a larger change or a design decision)
- **STW soundness for RPC/HTTP paths (C3, C4-RPC, H1, H2)** — the background
  collector can still race non-registered goroutines that allocate/hold Values
  (RPC handlers, HTTP forks) and blocking primitives that skip `enterBlocked`.
  Pre-existing for strings/dicts; needs the allocate-black mechanism + treating
  those goroutines as mutators. Highest-value deferred item.
- **Registry sweeping for BigInt/Result/Context/Future/GoObject (H3/H4/H6/H7)** —
  currently over-retained as roots (leak, not corruption). Futures need care
  (resolved-but-unawaited lives only in the future).
- **Content-hash drift bootstrap vs pipeline (3.1)** — route `cmd/bootstrap`
  through `pipeline.CompileAll` + a golden hash test; image-rebuild risk.
- **vm↔vm/dist cycle / triplicated envelope signing (4.3, 3.4)** — extract a
  leaf `wire` package.
- **`SyncService`/`cmd/mag` decomposition (4.5, 6)** and LOW consistency nits
  (string-helper unification 3.6, `prim` prefix 3.7, lib docstring backfill).
- **Concurrency ID recycling (H8/H9)** — slot+generation scheme for
  channels/processes/futures.
