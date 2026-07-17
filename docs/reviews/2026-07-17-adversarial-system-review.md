# Adversarial System Review — 2026-07-17

Second full adversarial pass, one day after the 2026-07-16 review was fully
remediated. Six parallel review agents covered: VM core, distribution stack
(dist/wire/server), compiler+types+manifest, lib/*.mag, cmd/tooling/docs, and a
cross-cutting dead-code/duplication/test sweep. All P0/P1 findings were verified
by reading code; several were **empirically reproduced** against the real
compiler/VM (noted inline). `go build ./...` clean; `go test -race ./vm/
./server/` passes; `go test ./...` passes — none of the P0s below are covered by
the existing test suite.

Finding IDs: VM-x (vm core), C-x (compiler/types/manifest), SD-x
(server/dist/wire), L-x (lib), T-x (cmd/tooling/docs), X-x (cross-cutting).

---

## P0 — reproduced breakage

### C-1 (P0, breakage) — Peephole constant folding is jump-target-blind: miscompiles ordinary code
`compiler/peephole.go:48-102` (`foldConstants`). The folder collapses
`PushInt8 a; PushInt8 b; SendArith` → `PushInt8 result` even when a jump label
points *into* the middle of that window. Only `eliminateDeadCode` computes jump
targets; the fold and push/pop phases never do.
**Reproduced:** `^(true ifTrue: [1] ifFalse: [2]) + 3` returns **1** instead of 4
— the inlined-conditional merge label pointed at the second push; the true-path
jump now lands past the `+ 3`. Corrupts any inlined `ifTrue:ifFalse:`/`and:`/`or:`
whose merge point is immediately followed by small-int arithmetic.

### VM-1 (P0, breakage) — `FinishProcess` double-finish aborts the whole VM
`vm/process_lifecycle.go:49-101` + `vm/concurrency.go:741-748`. No
already-terminated guard (no CAS, no early return): a second finish double-fires
`waitGroup.Done()` and `close(proc.done)`.
**Reproduced:** `p := [Process sleep: 100. 42] fork. p terminate.` → when the
block completes, its own `FinishProcess` panics `sync: negative WaitGroup
counter`, then `HandleForkedPanic`'s deferred `FinishProcess` re-panics inside
the defer → unrecoverable, `exit status 2`. Also reachable racily via link-kill
(`deliverExitSignal` → `FinishProcess`, guarded only by a TOCTOU `isDone()`) and
via node-death — Erlang-style link propagation and Supervisor trees can crash
the VM.

### L-1 (P0, breakage) — `BigInteger>>isZero` / `asSmallInt` are phantom API
`lib/BigInteger.mag:56,70`. Wrappers call `primIsZero`/`primAsSmallInt`, but Go
registers the primitives **unprefixed** (`vm/bigint_primitives.go:397,312`), so
the lib wrappers overwrite the primitives on image load and then DNU.
**Reproduced:** `20 factorial isZero` → `Message not understood: primIsZero`.
Neither method has a doctest — which is how both slipped the ratchet. Sibling
`sign` uses the correct `primSign` pattern. See L-3 for the gate blind spot that
let this ship.

---

## P1 — serious

### C-2 (P1, breakage) — Peephole push/pop elimination leaks operand-stack slots
`compiler/peephole.go:158-182` (`eliminatePushPop`). Same jump-blindness as C-1,
second manifestation: a `Push*/POP` pair is NOPed even when the `POP` is the
merge target of an inlined conditional; the then-path leaves its value orphaned
on the stack. **Reproduced:** value-discarded `ifTrue:ifFalse:` inside a
`whileTrue:` loop leaks one slot per iteration; the interpreter stack doubles on
demand and only resets at frame return → unbounded stack growth inside one
activation. Results stay correct, which is why tests pass.

### C-3 (P1, breakage) — Inside blocks, instance variables shadow same-named method temps
`compiler/codegen.go:588-592,689-694`. Block-body variable resolution checks
`instVars` **before** captures/outer temps — the opposite of method-body order.
**Reproduced:** with ivar `x`, `method: t [ | x | x := 5. ^[ x ] value ]`
compiles the block read as `PUSH_IVAR`; reads/writes silently hit the ivar. Also
breaks lockstep with the hash normalizer (`compiler/hash/normalize.go:239-257`
resolves scopes before instVars), so codegen and content-hash disagree about the
same reference.

### C-4 (P1, loose-end) — Live-coding compile path has no instance-variable plumbing
`vm/compiler_dispatch.go:49-61` + `compiler/codegen.go:1209-1227`.
`CompilerBackend.Compile(source, class)` never calls `SetInstanceVars`.
**Reproduced:** `compiler.Compile("getX\n ^x")` emits `PUSH_GLOBAL`. Every ivar
in a method installed via `Object>>compileAndInstall:`
(`vm/class_reflection_primitives.go:364`) or the ModifyService `CompileMethod`
RPC (`server/modify_service.go:54`) — the IDE "save method" path — compiles as
a global read (nil) / global store.

### C-5 (P1, loose-end) — The "dead" textual dot-splitter is alive on the Workspace RPC path
`server/modify_service.go:423-480` (`wrapWorkspaceSource` + `splitStatements`),
fed by `modify_service.go:316`. Splits on every `.` outside single-quoted
strings — including decimal points (`x := 3.14` → `x := 3` / `^14`) and periods
inside block bodies. This is exactly the corruption class `CompileDoIt` was
built to kill; the fixed path (`v.CompileExpression`) is one call away and never
got wired here.

### SD-1 (P1, breakage) — Two independent nonce streams share one replay window → legitimate messages rejected
`vm/dist/trust.go:248` (`CheckNonce`), fed by both `server/auth.go:160`
(request-auth nonce, seeded independently) and
`server/sync_service.go:529,778` (envelope nonce from `NodeRefData.nonce`,
`vm/node_primitives.go:67`). Both monotonic streams are checked against the
*same* per-peer window. The request stream advances on every RPC — including 5s
heartbeat Pings — while the envelope stream advances only on messages. After
~1024 request-only RPCs the window floor climbs above the envelope baseline and
real `asyncSend:`/spawn envelopes are rejected as "below replay window" (~85 min
of heartbeats with sparse messaging). Seeds are wall-clock-nanos apart, so the
streams can also collide outright ("nonce replayed").

### VM-2 (P1, breakage) — Cross-node identity conflation: peer NodeRefs carry *our* node ID
`vm/node_primitives.go:183-198,74-79`. `Node connect:` builds every peer
`NodeRefData` from the **local** identity keys, so `ref.NodeID()` is our own ID
for every peer. Fallout: (a) `ensureHealthMonitor` tracks all peers under one
key — second peer silently never heartbeated; (b) `handleNodeDown(ourID)` drains
state for *every* peer when any one dies; (c) `findNodeRefByID`
(`vm/remote_lifecycle.go:269-278`) compares true signature-proven peer IDs
against our-own-ID refs → never matches → cross-node DOWN silently never
delivered, deserialized remote channels get no heartbeat coverage
(`vm/serial.go:769-771`), and `DrainRemoteChannels` never matches
`RemoteChannelRef.OwnerNode` — the exact receive-hang SD-6 (2026-07-16) was
supposed to prevent.

### X-1 (P1, loose-end) — `mag` CLI signs with a fresh ephemeral key every run
`cmd/mag/clientauth.go:63`. `applyLocalIdentity` — the only caller of
`vm.SetNodeIdentityKeys` in the repo — is never invoked, so every `mag` process
falls back to `ed25519.GenerateKey` per process
(`vm/node_primitives.go:140-153`) instead of the persistent `.maggie/node.key`.
Its own comment states the intent it fails to deliver. Trust stores keyed by
node ID cannot whitelist a CLI node whose identity changes every run. (Related
to but distinct from VM-2.)

### VM-3 (P1, breakage) — `LinkProcesses` swap bug delivers exit signal to the dead process
`vm/monitor.go:38-64`. `aAlive`/`bAlive` are read from id-ordered
`first`/`second` but used as if they refer to `a`/`b`. When the dead process has
the smaller id: abnormal death → exit delivered *to the corpse* →
`FinishProcess` on a terminated process → the VM-1 double-finish panic; normal
death → notification silently lost, `links` entry never cleaned.

### VM-4 (P1, breakage) — `Mutex new unlock` hard-aborts the VM; `WaitGroup new done` panics
`vm/mutex.go:56-66`: Go's `fatal error: sync: unlock of unlocked mutex` is
unrecoverable — one line of user code kills the process (reproduced via mag
CLI). `vm/waitgroup.go:75-93`: `done` below zero panics the main goroutine.
Semaphore over-release *is* guarded (`vm/semaphore.go:77-84`) — hardening is
inconsistent across the three siblings.

### VM-5 (P1, breakage) — Awaiting a Future twice blocks forever
`vm/future_primitives.go:24-31` + `vm/future.go:30-36`. Resolution is one
buffered send; `await` is an unconditional `<-f.ch` that never consults the
`resolved` flag or cached result. Second await, await after a timed-out
`await:ifTimeout:`, or multiple waiters → all but one block forever.

### SD-2 (P1, unfinished) — `asyncSend:with:` request-response replies are always nil
`vm/node_primitives.go:380-399` creates a Future for `respPayload` and signs
`ReplyTo`, but the `DeliverMessage` handler (`server/sync_service.go:603`) never
populates `DeliverMessageResponse.ResponsePayload` and never reads `ReplyTo`
(zero server-side writers, grep-verified). Every request-response Future
resolves to Nil regardless of what the target computes.

### L-2 (P1, breakage) — Doctests CI job is red on main
`lib/Stream.mag:20` + `cmd/mag/doctest.go:256`. The comment-only line inside
Stream's class-docstring ```test block is captured as a "setup expression";
comments aren't statements, so the assembled doIt has an empty statement →
parse error → `./mag doctest lib/` (CI `.github/workflows/ci.yml:97`) exits 1.
1195/1196 assertions pass; the assertion itself is fine run by hand.

### L-3 (P1, breakage) — The phantom-primitive hard gate has two blind spots (one shipped L-1)
`cmd/bootstrap/conventions_test.go:81-113`. (a) Only `IsPrimitiveStub` methods
are checked — a lib method whose *body* calls a nonexistent `prim*` selector
(the exact Future.mag failure mode the gate was built for, and the exact L-1
failure mode) passes. (b) `cls == nil → continue` plus lib-only iteration makes
Go-registered classes with no lib file invisible to both gates (see L-7/Ganso).

### T-1 (P1, loose-end) — Checked-in `prim_docstrings_gen.go` is stale; working-tree copy is the correct one
The uncommitted diff to `cmd/mag/prim_docstrings_gen.go` is byte-identical to
what `go run ./cmd/bootstrap/` regenerates from HEAD (verified via git-archive
extract). Commit 1207441 rebuilt the images but forgot this third bootstrap
artifact, so a `mag` built from HEAD serves wrong help (`ArrayList>>first` "or
nil if empty" — it now raises), documents the deleted `Process>>isDone`, and
lacks docs for the five new primitives. Root cause: the documented rebuild
recipe (`CLAUDE.md`, `Makefile`) names only the two image files. **Fix: commit
the working-tree file as-is; update the recipe.**

---

## P2 — moderate

### SD-3 (P2, breakage) — Exported-channel RPCs have no per-object authorization
`server/sync_service.go:887-1055`. `ChannelSend/Receive/TrySend/TryReceive/
Close/Status` look up channels purely by sequential `ChannelId` (IDs from 1,
`vm/remote_channel.go:123`). Any peer with `PermMessage` can enumerate IDs and
drain, inject into, or close channels belonging to other peers/sessions.

### SD-4 (P2, breakage) — `DemonitorProcess` ignores the authenticated peer
`server/sync_service.go:722-728` removes by ref ID only; sibling
`MonitorProcess` (`:696`) uses the signature-proven identity. Any
`PermMessage` pe