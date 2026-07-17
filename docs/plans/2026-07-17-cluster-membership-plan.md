# Cluster membership: pluggable discovery, gossip, and failure detection

Fills the middle layer of the distribution stack (Phase E of
`docs/plans/2026-03-04-distributed-maggie-implementation-plan.md`). Today a node
knows only the peers it explicitly `connect:`ed, and `lib/Cluster.mag` detects
failure by ping-polling every member whenever any monitor fires. This plan adds a
membership layer that **discovers** peers, **disseminates** a shared liveness view
by gossip, and **feeds placement** — without every node polling every other node.

Design was settled interactively (2026-07-17). The three shaping decisions:

- **Where it lives:** a Go mechanism (core + gossip + detector) with Maggie-level
  *policy* hooks. Not mostly-Maggie (won't scale), not a closed Go subsystem
  (not customizable enough).
- **Discovery:** a pluggable provider interface; ship static seeds, add gossip,
  mDNS, and an external-registry adapter. Multiple providers run at once.
- **Failure detection:** a pluggable `FailureDetector` interface; ship a
  direct-heartbeat detector wrapping the existing `NodeHealthMonitor`, allow SWIM
  (or custom) to swap in later.

## Two load-bearing principles

**1. Gossip and failure detection are separate concerns.** The failure detector
*produces* liveness state (is peer X reachable?). Gossip *spreads* state (here is
my whole view; merge it with yours by incarnation number). That separation is
exactly why the detector is swappable: SWIM's clever part (indirect probes +
suspicion) lives entirely behind `FailureDetector`, while the gossip anti-entropy
loop never changes.

**2. Membership ≠ trust.** Discovery/gossip tell you who is *there*; the
`TrustStore` (`vm/dist/trust.go`) tells you what they may *do*. A discovered or
gossiped peer gets `DefaultPerms` (sync-only) until explicitly configured. Gossip
disseminates addresses, liveness, and metadata — **never trust grants**. This is
what keeps flexible discovery (especially mDNS on an open LAN) from becoming an
attack surface. `PermMessage`/`PermSpawn` still require configured trust or an
explicit trust-on-first-use policy the operator opts into.

## Architecture

```
Maggie policy layer (lib/Cluster.mag)
  onMemberUp:/Down:/Join:/Leave:  ·  placementStrategy:  ·  joinPolicy:
        ▲ events                              │ hooks
────────┼──────────────────────────────────────────────────────────────
  Membership core (Go, vm/dist/membership.go)
    view: map[PeerID]MemberRecord{addr, status, incarnation, metadata}
    gossip dissemination (anti-entropy, incarnation-numbered)
        ▲                    ▲
   Discovery[]          FailureDetector      ← the two pluggable seams
   static/gossip/       direct-heartbeat
   mDNS/registry        (→ SWIM later)
```

The core cannot import `vm` for VM types directly beyond what `vm/dist` already
does; it reuses the injected-callback pattern (`NodeRefFactory`/`SendFunc`) and
the `wire` leaf package. Membership events cross into the VM via a callback the
`vm` package registers (same shape as `handleNodeDown`).

## The two seams

```go
// Discovery streams candidate peer addresses to try. Multiple providers run
// concurrently; the core merges their streams. Providers run until ctx is
// cancelled (mDNS/registry keep discovering; static emits once and idles).
type Discovery interface {
    Discover(ctx context.Context) (<-chan PeerAddr, error)
    Name() string
}

// FailureDetector produces liveness transitions for the tracked members. The
// core applies the events to the view; gossip spreads the result. Ships as
// DirectHeartbeatDetector (wraps NodeHealthMonitor); SWIM slots in unchanged.
type FailureDetector interface {
    Start(ctx context.Context) (<-chan MemberEvent, error)
    Track(peer PeerID, ref *NodeRefData)
    Untrack(peer PeerID)
}
```

`PeerAddr` is `{Addr string, Source string}` (source = provider name, for
per-provider join policy). `MemberEvent` is `{Peer PeerID, Status Alive|Suspect|Dead}`.

## The member record and gossip payload

```go
type MemberStatus uint8 // Alive, Suspect, Dead, Left

type MemberRecord struct {
    Peer        PeerID            // Ed25519 node id (32 bytes)
    Addr        string
    Status      MemberStatus
    Incarnation uint64            // bumped by the OWNING node on any self-change
    Metadata    map[string]string // OPEN schema — see below
}
```

Gossip messages travel as a signed `__gossip__` infra-selector through the
existing `DeliverMessage` RPC (same pattern as `__down__`/`__reply__`), so they
inherit signing, replay windows, and ban checks for free. Payload is a CBOR
digest: `[]MemberRecord` (or a delta since a known version).

### Metadata: open schema, no wire-version coupling

Metadata is an **open `map[string]string`**, not a typed struct. Consequences,
locked in now:

- **New keys never bump `wire.Version`.** The wire version guards *framing*
  (field layout, signature construction). Map entries are data inside an
  unchanged frame; an old node decodes a map with unknown keys fine.
- **Forward-verbatim rule (the one constraint):** a node relaying gossip must
  re-emit each member's *entire* metadata map, including keys it has no code for.
  The core treats metadata as an opaque blob it merges and forwards — it never
  parses a key to spread it. This is what lets a future `zone`/`capabilities`
  label propagate through older nodes untouched. Anti-entropy convergence depends
  on it.
- **Merge unit is the record, per incarnation.** Metadata is owned by its node
  and replaced *whole* when the owner bumps its incarnation. We pin "metadata is
  a per-node map versioned atomically with the record," not the keys.
- **Guardrails:** cap metadata (≤4 KB, ≤32 keys per member) so a buggy/hostile
  node can't bloat every message; reserve conventional keys (`zone`,
  `region`, `capabilities`) documented-not-enforced; namespace app-private keys
  (`x-<app>-…`). Values are opaque strings (put CBOR-hex/JSON in a value if
  structured data is needed).

### Incarnation / merge rules (SWIM-style anti-entropy)

For each incoming `MemberRecord`, merge into the local view:

- Higher `Incarnation` always wins (replaces addr, status, metadata wholesale).
- Equal incarnation: `Dead`/`Left` beats `Suspect` beats `Alive` (failure is
  sticky within a generation).
- A node refutes a false `Suspect`/`Dead` about *itself* by bumping its own
  incarnation and re-gossiping `Alive` — standard SWIM self-defense.
- Only the owning node increments its own incarnation. Others never fabricate one.

## Join policy (configurable auto-connect)

Discovering a peer does not force a connection. Two levels:

- **Coarse default** from `maggie.toml` `[cluster] joinPolicy = "eager" | "lazy" |
  "manual"`. *Eager:* on discovery, run the `connect:` handshake → live ref +
  trust check + `FailureDetector.Track`. *Lazy:* peer enters the view as *known*
  (addr/status/metadata from gossip) but stays unconnected until first use, then
  connect-on-demand fires. *Manual:* discovery only populates the view.
- **Fine control** from a Maggie hook `cluster joinPolicy: [:peer | ...]`,
  defaulting to the config value, deciding per peer.
- **Per-provider defaults** fall out of `PeerAddr.Source`: eager for static
  seeds, lazy for mDNS (don't auto-connect every laptop on the LAN).

Synergy with gossiped metadata: because gossip already carried a peer's
`zone`/`capabilities`, a selective join hook (`[:peer | peer zone = myZone]`) can
decide **before** opening any connection. Open metadata + lazy-default + a join
predicate = cheap, policy-driven topology.

## Maggie policy surface (lib/Cluster.mag)

`Cluster.mag` stops ping-polling and instead subscribes to the core's event
stream:

- `onMemberUp:` / `onMemberDown:` / `onMemberJoin:` / `onMemberLeave:` — blocks
  fired on membership transitions (shape already exists in Cluster.mag).
- `joinPolicy: [:peer | <Boolean>]` — per-peer eager-connect predicate.
- `placementStrategy: [:key :members | <member>]` — defaults to `HashRing`
  (`lib/HashRing.mag`), overridable for zone-/capability-aware placement using
  gossiped metadata. This is what `forkAnywhere`/`spawnAnywhere` (Phase 6) consult.
- Member objects expose `id`, `addr`, `status`, `metadataAt:` for hooks.

## What it reuses (nothing greenfield in Phase 1)

- **`NodeHealthMonitor`** (`vm/remote_lifecycle.go`) → first `FailureDetector`.
- **`HashRing.mag`** → default `placementStrategy:`.
- **Signed envelopes / `DeliverMessage`** → `__gossip__` infra-selector, routed
  like `__down__`/`__reply__` in `server/sync_service.go`.
- **`TrustStore`** → gates whether a discovered peer may message/spawn.
- **`Cluster.mag`** → already has the event-hook shape.

Retires the request-response limitation noted in
`memory/dist-request-reply-and-cod.md`: once membership maintains live
bidirectional refs, a responder always has a route back to the requester.

## Build order

1. **Core + defaults.** `Discovery`/`FailureDetector` interfaces; `Membership`
   core with the gossip anti-entropy loop and incarnation merge; `StaticDiscovery`
   + `DirectHeartbeatDetector`; `__gossip__` selector + handler; membership→VM
   event callback. **Load-bearing phase.**
2. **Maggie hook surface.** `Cluster.mag` subscribes to events; `joinPolicy:` +
   `placementStrategy:` hooks; member objects with metadata access.
3. **mDNS discovery provider** (LAN zero-config; lazy join by default).
4. **External-registry provider** (adapter interface + one concrete, e.g. DNS-SD).
5. **SWIM `FailureDetector`** (swap-in behind the interface).
6. **`forkAnywhere`/placement** on the live view + `HashRing`.

Phases 3–6 each drop an implementation into an existing seam; only Phase 1
defines new structure.

## Open questions (defer to implementation)

- Gossip cadence and fanout (K peers / interval) — start with K=3, 1 s; tune.
- Delta vs full-digest gossip — start full-digest (simplest, converges), add
  deltas if message size becomes a problem.
- Whether `manual` join policy still accepts inbound gossip (probably yes: learn
  the view, connect nothing).
- Trust-on-first-use policy shape for discovered peers (opt-in; out of scope for
  Phase 1, which leaves discovered peers at `DefaultPerms`).

---

# Phase 2 design (settled 2026-07-17): the Maggie hook surface

Phase 1 built the Go core and left it observable only from Go. Phase 2 exposes it
to Maggie — event handlers, a live view, metadata, and connect control — so a
program can react to membership and (Phase 6) place work on it.

## The load-bearing rule: each crossing goes one direction only

- **Go → Maggie is always asynchronous, via a mailbox.** The core, on any event,
  builds a `MailboxMessage` and `TrySend`s it to a registered Maggie process —
  the exact off-goroutine path `server.DeliverMessage`/`handleReply` already use,
  so it needs no new threading model and no interpreter on the gossip goroutine.
- **Maggie → Go is always synchronous, via primitives.** `cluster members`,
  `member connect`, `cluster setMetadata:` are plain primitive calls that
  delegate to the core and return.
- **The diagonal is forbidden:** the core never synchronously evaluates a Maggie
  block to get a value back. Every hook below is shaped to avoid it. This is the
  invariant that keeps the concurrency tractable.

## Components

**`vm` package (new, no `vm/dist` dependency):**

- `ClusterCore` interface — the handle the Maggie primitives delegate to:
  ```go
  type ClusterMemberInfo struct {
      ID       [32]byte
      Addr     string
      Status   uint8            // mirrors dist.MemberStatus
      Metadata map[string]string
  }
  type ClusterCore interface {
      Members() []ClusterMemberInfo
      Connect(addr string)
      SetMetadata(md map[string]string)
      SetJoinPolicy(policy string) // "eager" | "lazy" | "manual"
  }
  ```
  `VM.SetClusterCore(ClusterCore)` stores it; `dist.Membership` implements it
  (dist→vm is legal, so its methods return `[]vm.ClusterMemberInfo`). No dist
  type leaks into `vm`.
- Bootstrapped **`ClusterMember`** class (like `MailboxMessage`): slots
  `{id, addr, status, metadata}`, accessors `id`/`addr`/`status`/`metadataAt:`,
  and `connect` (→ a `Node` on demand via `VM.ConnectNode`). Represents peers we
  are not yet connected to (gossip-learned), which raw `Node` can't.
- Bootstrapped **`Cluster`** class primitives over `ClusterCore`:
  `primMembers` (→ Array of `ClusterMember`), `primConnect:`, `primSetMetadata:`,
  `primSetJoinPolicy:`. `Cluster current` returns the singleton wrapping the core
  (nil-safe: empty view when no core is configured).
- `VM.DeliverClusterEvent(kind string, info ClusterMemberInfo)` — builds a
  `ClusterMember` value and a `MailboxMessage` (selector = kind) and `TrySend`s
  to the `__cluster_events__` process. Best-effort: no such process / full
  mailbox → dropped (the view stays authoritative via `primMembers`). Metadata
  crosses as a Maggie `Dictionary`.

**`vm/dist` (extend Membership):**

- Implement `vm.ClusterCore` (`Members`/`Connect`/`SetMetadata`/`SetJoinPolicy`).
- Emit a new **`Discovered`** event alongside Up/Down when a gossip-learned peer
  is seen under `JoinLazy` (drives the async join inversion below).
- `Subscribe`-wired adapter (in cmd/mag) maps each `MemberEvent` to
  `VM.DeliverClusterEvent(kindString, toInfo(ev))`.

**`cmd/mag`:** after constructing the core (Phase 1), call
`vmInst.SetClusterCore(mship)` and
`mship.Subscribe(func(ev){ vmInst.DeliverClusterEvent(kindOf(ev.Status/Discovered), toInfo(ev)) })`.

**`lib/Cluster.mag` (rewrite):** its runLoop already registers a process and
dispatches mailbox messages by selector — repoint it. It stops doing its own
`connectSeed`/ping-poll (the Go core owns discovery + failure detection) and
instead:
- registers `Process current` as `__cluster_events__` in `start`,
- dispatches `#memberUp:`/`#memberDown:`/`#memberDiscovered:` messages (payload =
  `ClusterMember`) to the user's `onMemberUp:`/`onMemberDown:`/`onMemberDiscovered:`
  blocks,
- delegates `members`/`nodeFor:`/`connect:` to `Cluster current` primitives,
- keeps `HashRing` for `nodeFor:`, fed from `members`.

## The three settled forks

1. **Join policy — async inversion + coarse enum.** `cluster joinPolicy: #eager |
   #lazy | #manual` maps to the Go enum via `primSetJoinPolicy:` (synchronous
   Maggie→Go, fine). For arbitrary predicates, under `#lazy` the core emits
   `#memberDiscovered:` (async); the Maggie handler decides and calls
   `member connect`. The core never evaluates a Maggie predicate — the invariant
   holds.
2. **Member representation — bootstrapped `ClusterMember` class** (above).
3. **Event loop start — on demand.** The Go core runs at boot; the Maggie
   `__cluster_events__` loop starts only when the program calls
   `Cluster current start`. Until then events drop and `cluster members` still
   reflects the live view. No Maggie code runs at boot unless asked.

## Maggie API (after Phase 2)

```smalltalk
| cluster |
cluster := Cluster current.
cluster start.                                  "spawn the event loop"
cluster onMemberUp:   [:m | ('up: ',   m addr) println].
cluster onMemberDown: [:m | ('down: ', m addr) println].
cluster joinPolicy: #lazy.
cluster onMemberDiscovered: [:m |               "the predicate lives here"
    (m metadataAt: 'zone') = 'us-east' ifTrue: [m connect]].
cluster members do: [:m | m addr println].      "authoritative live view"
cluster setMetadata: (Dictionary new at: 'zone' put: 'us-east'; yourself).
```

## Build sub-steps

1. `vm`: `ClusterCore`/`ClusterMemberInfo`, `SetClusterCore`, bootstrap
   `ClusterMember` + `Cluster` classes and primitives, `DeliverClusterEvent`.
2. `vm/dist`: implement `ClusterCore` on `Membership`; add the `Discovered` event;
   `SetJoinPolicy`.
3. `cmd/mag`: `SetClusterCore` + `Subscribe` adapter.
4. `lib/Cluster.mag`: rewrite runLoop onto `__cluster_events__` + core delegation;
   rebuild image.
5. Tests: `ClusterMember` accessors; `DeliverClusterEvent` → mailbox round-trip;
   a two-node cmd/mag-level test asserting node A's `Cluster current members`
   converges to include node B (the live convergence Phase 1 could not show);
   join-inversion (`#lazy` + `onMemberDiscovered:` → `connect`).

## Threading notes / invariants

- `DeliverClusterEvent` builds Values off the core goroutine — same as the server
  building mailbox payloads today; safe under the concurrency-audit patches.
- Event delivery is lossy by design; `primMembers` is the source of truth.
- `member connect` and `cluster connect:` route to the core's `connectPeer`,
  which is idempotent (already-connected peers are skipped).

---

# Phase 2 adversarial review (2026-07-17): findings & required changes

Four parallel reviewers (concurrency, consistency, security, API) attacked this
design against the real Phase 1 code. NOT READY TO BUILD as written. Two blockers
are in already-shipped Phase 1 code, not the Phase 2 design — fix those first.

## Phase 1 security defects (SHIPPED — REMEDIATED 2026-07-17)

Both fixed; see beads `maggie-gossip-ssrf-6dn`, `maggie-gossip-poison-1cj`.

- **SSRF / amplification — FIXED.** `DecodeGossip` now caps record count
  (`maxGossipRecords`); `connectableGossipAddr` blocks unspecified / link-local
  (incl. 169.254.169.254) / multicast targets on the auto-dial path; and the
  `cmd/mag` default is now `JoinLazy` — gossip-learned addresses are recorded but
  NOT auto-dialed (only operator-configured static seeds connect). Residual (noted
  on the bead): under an explicit `JoinEager` opt-in, loopback and private-range
  dials are still possible and leak node identity — an operator's informed choice.
- **View poisoning — FIXED.** `MemberRecord` now carries a per-record Ed25519
  signature; a node self-signs its own record (`SignWithNodeIdentity` /
  `SignMemberRecord`) and `ApplyGossip` drops any record not validly signed by its
  subject (NodeID == public key). Gossip carries only self-signed **Alive/Left**
  records; **Dead is decided solely by the local failure detector and never
  accepted from gossip**, so a forged Dead cannot evict a live peer and forged
  addr/metadata cannot be injected for another node. Residual (noted): incarnation
  resets to 0 on node restart (a recovered node's Alive can lose to a stale
  Dead@N) — a general SWIM incarnation-persistence issue, tracked separately from
  the poison fix.

## Phase 2 design blockers (must change the design)

1. **Compile collision.** `dist.Membership` already has `Members() []MemberRecord`;
   `ClusterCore.Members() []ClusterMemberInfo` on the same receiver won't compile.
   Rename the interface method (e.g. `MemberInfos()` / `ViewForMaggie()`).
2. **The "authoritative view" argument is false for placement.** Phase 1 never
   prunes Dead/Left records (`markDead` keeps them; `Members()`/`Snapshot()` don't
   filter). So `cluster members` and any ring "fed from members" surface dead
   tombstones → `nodeFor:` routes work to dead nodes. Required: the `ClusterCore`
   view adapter filters to `StatusAlive`, and the ring is rebuilt from that
   filtered authoritative view — NOT mutated from lossy events. (Also decide
   pruning of Dead records after a grace period.)
3. **`connectPeer` is NOT idempotent, and blocks.** It always calls `ConnectNode`
   (fresh ref + blocking 5 s `Ping`), no `conns` lookup; the doc's idempotency
   claim is wrong. Worse, `member connect` runs that blocking ping on the single
   `__cluster_events__` goroutine → head-of-line blocking of all event dispatch.
   Required: make `connectPeer` a real conns-guarded no-op for known peers, and
   make the Maggie-facing connect asynchronous (hand off to the core; never block
   the dispatch goroutine on a network RTT). This also fixes the duplicate-ref
   problem (Maggie's Node ≠ the core's routed/heartbeated ref).
4. **HashRing → `spawnOn:` type mismatch.** `HashRing` is object-agnostic but its
   outputs feed `spawnOn:`/`forkOn:`, which require a `kindRemoteRef` Value; a
   slotted `ClusterMember` fails `getNodeRef` → signals. Also `HashRing remove:`
   uses value-equality on fresh per-call `ClusterMember`s → stale virtual nodes.
   Required: ring holds stable `Node`s (member→Node cached and reused), or
   placement returns a member and the caller does `member connect spawnOn:`.
5. **Event ordering + no supervision.** Events fan out from multiple goroutines
   (detector, discovery, per-RPC gossip) after the lock releases, so the mailbox
   can see Down-after-Up / stale `Status`. And a single throwing handler kills
   `__cluster_events__` permanently and silently (name freed, all future events
   dropped). Required: handlers treat the event as a hint and re-read
   `cluster members` for truth (don't trust the payload `Status`); supervise/restart
   the event loop (or isolate handler exceptions); guard double-`start`.

## Phase 2 design gaps (specify before building)

- **`Cluster seeds:` fate (breaking).** Live callers in `DistributedSupervisor.mag`
  and Guides 13/19; `onMemberDown:` arg changes String→`ClusterMember`, `members`
  Node→`ClusterMember`. Decide: `Cluster current` replaces `seeds:` (migrate
  callers) vs coexist (two divergent cores — discouraged).
- **`status` representation.** Deliver `#alive/#suspect/#dead/#left`, not a raw
  SmallInt.
- **`ClusterMember` must be non-serializable** — a plain slotted object is sendable;
  mark it so a user can't `asyncSend:` a zombie member.
- **`Cluster current` singleton** must be one stable instance holding the handler
  ivars the loop reads; specify Go-held vs Maggie class-side and nil-core behavior
  per primitive.
- **`__cluster__` sentinel.** The rewrite drops it; peers on old `Cluster.mag`
  monitoring `__cluster__` lose cross-node failure detection. Keep registering it
  or document the version break.
- **Self inclusion.** `Snapshot` includes self, `Members` doesn't — pick one for
  the adapter; exclude self from the placement set.
- **`#lazy` transitive connect.** An auto-connecting `onMemberDiscovered:` handler
  pulls each connected peer's view → discovers+connects the whole cluster,
  defeating lazy. Bound it (don't cascade auto-connect).
- **Minors:** mandate pure-Go construction of the metadata `Dictionary`/`ClusterMember`
  (Maggie dispatch off-gate hits the audit's inline-cache races); cap self-metadata
  in `SetMetadata` (only inbound is capped today).

---

# Phase 2 resolutions (2026-07-17): how each blocker is settled

The five blockers and the gaps are resolved as follows. Blockers #1–#3 are
code-level and land in the Phase 1 core now (Alive-filtered view, idempotent/
async connect, ref reuse, the `ClusterCore` interface). #4–#5 are Maggie-layer
and are realized in the `Cluster.mag` rewrite.

**#1 Members() collision → rename.** The interface method is
`ClusterCore.AliveMemberInfos() []ClusterMemberInfo` (not `Members()`), so it
does not collide with `dist.Membership.Members() []MemberRecord`. A compile-time
`var _ vm.ClusterCore = (*Membership)(nil)` guards the implementation.

**#2 view filter → Alive-only, self-excluded adapter.** `AliveMemberInfos`
returns only `StatusAlive` records and never the self record. The Maggie ring is
**rebuilt from that authoritative view on every membership event** (not mutated
incrementally), so a dropped event self-heals on the next reconcile and dead
tombstones never reach placement. Dead records stay in the Go view (needed for
incarnation merges) but are invisible to Maggie.

**#3 connect idempotency + non-blocking + ref reuse.** `connectPeer(addr)` is now
idempotent — it scans `conns` for an existing ref to the same address and returns
it instead of re-dialing. Two new `ClusterCore` methods: `ConnectAsync(addr)`
(fire-and-forget, for event handlers — never blocks the dispatch goroutine) and
`ConnectedRef(peer)` (synchronous lookup, nil if not connected). `ClusterMember>>
node` wraps the core's *existing* ref (via `ConnectedRef`) as a `Node` value, so
the Maggie `Node` is the same ref the core routes/heartbeats on — no duplicate.

**#4 HashRing holds Nodes.** The ring is fed `member node` values (real
`kindRemoteRef` `Node`s reused from the core), so `nodeFor:` returns a `Node` that
`spawnOn:`/`forkOn:` accept directly. Only *connected* Alive members are added
(`member isConnected`), because you can only place work on a peer you can reach;
rebuilding from the authoritative view each event keeps membership and ring in
lockstep and sidesteps the fresh-object `remove:` equality problem (the ring is
rebuilt, never `remove:`-d).

**#5 event ordering + supervision.** The event payload is a *hint*: handlers (and
the ring rebuild) re-read `cluster members` for truth and never trust the event's
`Status`. The `Cluster.mag` loop wraps each handler call in `on: Error do:` so a
throwing handler cannot kill dispatch, and `start` is guarded against double-spawn
(checks a `running` flag / the `registerAs:` result).

**Gaps:**
- **`Cluster seeds:` fate → `Cluster current` is canonical; `seeds:` is retained
  as a thin shim** that returns `Cluster current` (ignoring its arg, since seeds
  come from `maggie.toml`) so existing callers keep working, with a deprecation
  note. `onMemberDown:`/`members` now yield `ClusterMember`; the guide callers are
  updated. No second core.
- **`status` → symbol.** `ClusterMember>>status` answers `#alive/#suspect/#dead/
  #left` (mapped from the `uint8` in the vm primitive).
- **`ClusterMember` non-serializable.** A `NonSerializable` flag on `Class`;
  `serializeObject` signals for such classes. Set on `ClusterMember`.
- **`Cluster current` singleton.** Held in a `Cluster` class-side variable (one
  stable instance carrying the handler ivars the loop reads); created on first
  `current`. Nil-core: `members` → empty, `start` → no-op with a warning,
  `connect:`/`setMetadata:` → no-op.
- **`__cluster__` sentinel kept.** The event loop registers as `__cluster__`
  (reusing the existing name), so mixed-version peers still find the sentinel; it
  now doubles as the event inbox.
- **Self excluded** from `AliveMemberInfos` (see #2).
- **`#lazy` cascade** is the handler's responsibility: the guide shows a
  *predicate* (`(m metadataAt: 'zone') = ...`) rather than a blanket connect, and
  the doc warns a blanket auto-connect handler transitively connects the cluster.
- **Minors.** Metadata `Dictionary` and `ClusterMember` are built with pure-Go
  constructors off the core goroutine (never Maggie dispatch); `SetMetadata` caps
  self-metadata with the same guardrails as inbound.
