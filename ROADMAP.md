# Maggie Roadmap

Last updated: 2026-03-15

---

## CUE Integration

### Layer 1: `Object>>asCueValue` — IN PROGRESS

Runtime projection of any Maggie object into a CUE value. No syntax changes, no compiler work — pure primitive method on Object.

- `asCueValue` builds a CUE struct from the receiver's ivar names and slot values
- Uses existing `AllInstVarNames()` / `GetSlot(i)` + `cueExportValue` machinery
- `CueValue>>matchesObject:` — unify a CUE template against an object's projection, return true unless bottom
- Enables tuplespace matching via CUE unification (structural, range-aware, composable) instead of Linda-style equality templates

**Depends on:** existing `CueContext` / `CueValue` wrappers (already NaN-boxed)

### Layer 2: `Class.CueSchema` — PLANNED

Optional CUE schema field on Class, compiled once at class definition time.

- New `cueSchema *cue.Value` field on `Class` struct
- `.mag` syntax: `schema: '{x: number, y: number}'` in class definition
- Slot assignment validates against cached schema via `FillPath` + `Err()`
- Opt-in per class (nil schema = no validation, zero cost)
- Required CUE fields = hard constraints; default-value fields = soft/preferred
- Maps to Kaleidoscope's constraint hierarchy insight: required vs. preferential

**Design considerations:**
- Compile schema once at class load, cache on Class — no per-write CUE parse cost
- Recompile only on class redefinition
- `SetSlotValidating` path behind a flag so unschema'd classes pay nothing
- Subclass schemas must unify with superclass schema (tightening only, never loosening)
- Parser/compiler changes needed for `schema:` clause in class definitions

### Layer 3: CUE Method Guards (Predicate Dispatch) — PLANNED

CUE constraints as guards on method applicability.

- Optional `CueGuard *cue.Value` on `CompiledMethod`
- At dispatch: project receiver via `asCueValue`, unify with guard, skip method if bottom
- Multiple methods with same selector disambiguated by guard specificity
- `.mag` syntax: `method: describe [cue: '{speed: >100}'] ^'fast'`

**Design considerations:**
- VTable surgery: method lookup must try guards in specificity order
- Guard compilation happens once at method compile time
- Performance: guard checks add cost per dispatch — consider caching projection per message send
- Interaction with traits: trait methods with guards should compose via unification
- Parser changes for `[cue: ...]` annotation syntax

---

## Distributed Execution

Detailed implementation plan: `docs/plans/2026-03-04-distributed-maggie-implementation-plan.md`

### Phase 6–7: Content-Addressed Code Distribution — COMPLETE

What's done:
- Content hashing (SHA-256, De Bruijn indexed, golden files)
- Chunk types (method, class, module) with CBOR wire format
- Disk cache (git-style content-addressed storage in `.maggie/cache/`)
- Sync service RPCs: Announce, Transfer, Serve, Ping, Resolve, List
- CLI: `mag sync push|pull|status|list|diff|show`
- Rehydration pipeline (topological sort, namespace-aware, hash-verified)
- Peer reputation tracking with auto-ban on hash mismatches
- Capability policy (allow/deny lists on server)

### Distribution Gaps (polish, not blocking) — TODO

Items that would harden the existing code distribution before moving to node protocol:

1. **Automatic cache loading on startup** — currently only loaded explicitly during `sync pull`
2. **Rehydration in main compile flow** — `RehydrateFromStore` only called after pull, not during normal project load
3. **Capability metadata on chunks** — `Chunk.Capabilities` field exists but never populated
4. **Peer management CLI** — no `mag peer` subcommand to inspect/manage reputation or ban list
5. **Reputation persistence** — peer trust data lost between sessions
6. **Import preservation for pulled code** — synced code only works with FQN, not unqualified imports

### Node Protocol (Two-VM Communication) — PLANNED

The bridge from "distribute code" to "distribute execution."

1. **Node identity** — `.maggie/node.key`, node IDs, authentication
2. **Value serialization** — `vm/dist/serial.go` with `SerializeValue`/`DeserializeValue` for cross-VM message passing
3. **Symbol table negotiation** — selector/symbol table sync between VMs (each VM has its own table)
4. **Inter-VM messaging** — `SendMessage` RPC for arbitrary message delivery between processes on different nodes
5. **Remote process spawning** — `forkOn: nodeRef` primitive, process runs on remote VM
6. **Code-on-demand** — automatic `sync pull` when deserializing an object whose class is unknown locally
7. **Distributed GC** — lease-based reference lifecycle for cross-VM references
8. **Health checking** — periodic ping + failure detection + process migration

### Distributed Programming Model — PLANNED

Higher-level abstractions built on top of node protocol.

1. **Location-transparent proxies** — remote references that forward messages via `SendMessage` RPC; leverages existing `become:` forwarding pointer system
2. **Supervisor trees** — supervision hierarchy spanning nodes, Erlang-style restart strategies
3. **Cluster membership** — gossip protocol or mDNS for node discovery
4. **Capability enforcement at runtime** — tie `forkRestricted:` mechanism to distributed capability policy

### Tuplespace & Concurrent Constraint Programming — IN PROGRESS

Detailed plan: `docs/plans/2026-03-15-tuplespace-implementation-plan.md`

1. **CUE Subsumption** — `CueValue>>subsumes:` / `subsumedBy:` for entailment checking
2. **Local TupleSpace** — Linda-style `in:`/`out:`/`read:` with CUE unification matching and goroutine-based blocking
3. **Linear Logic Extensions** — tuple modes (linear/affine/persistent), atomic multi-take (⊗), choice (⊕), lease/expiry
4. **Constraint Store (CCP)** — monotonic CUE-backed store with `tell:`/`ask:`/suspend, concurrent constraint programming semantics
5. **Agent Orchestration Patterns** — application-layer Maggie code: Agent class, task/result protocols, fan-out/fan-in/pipeline/auction/blackboard/consensus patterns
6. **Distributed Tuplespace** — depends on node protocol; partitioned tuples, CRDT-like constraint store replication

---

## Ordering

```
CUE Layer 1 (done)
    │
    ├──→ CUE Layer 2 (schema on class)
    │        │
    │        └──→ CUE Layer 3 (method guards)
    │
    ├──→ CUE Subsumption (now)
    │        │
    │        └──→ Local TupleSpace
    │                 │
    │                 ├──→ Linear Logic Extensions
    │                 │
    │                 └──→ Constraint Store (CCP)
    │                          │
    │                          └──→ Agent Orchestration (app layer)
    │
    ├──→ Distribution Gaps (polish)
    │        │
    │        └──→ Node Protocol
    │                 │
    │                 ├──→ Distributed Programming Model
    │                 │
    │                 └──→ Distributed TupleSpace
```

The local tuplespace track (subsumption → tuplespace → linear extensions / constraint store) is independent of the distribution track. Agent orchestration can begin as soon as the local tuplespace exists. Distribution comes later.
