# Distributed Runtime for Maggie

**Date:** 2026-02-02
**Status:** Brainstorm / Research Complete

## What We're Exploring

Making Maggie capable of running as a distributed runtime -- processes spanning multiple nodes, objects reachable across machines, fault tolerance through supervision -- while preserving Smalltalk's elegant message-passing model. Inspired by Erlang/BEAM, GemStone/S, E language, Croquet, and Newspeak.

## Why This Is Worth Exploring

Maggie already has the right foundation:

- **Goroutine-backed lightweight processes** (`ProcessObject`) with result futures and state tracking
- **Channel-based communication** with blocking/non-blocking send/receive and select
- **Concurrency primitives** (Mutex, WaitGroup, Semaphore, CancellationContext)
- **Image serialization** (`ImageEncoder`/`ImageWriter`/`ImageReader`) that captures full VM state
- **Portable bytecode** (97 opcodes, no platform-specific instructions)
- **gRPC client support** with protobuf message conversion
- **RPC server** exposing evaluation, browsing, modification, inspection services
- **Module system** with namespaces that provide natural distribution boundaries

Smalltalk's "everything is an object, all computation is message passing" maps directly to distributed actor models. This isn't bolting distribution onto a language that wasn't designed for it -- it's extending a model that was already message-based.

## Historical Context and Prior Art

### Distributed Smalltalk Systems

| System | Approach | Key Insight |
|--------|----------|-------------|
| **Emerald** (1983-87) | Fine-grained object mobility | Objects migrate between nodes with `move self to there` |
| **GemStone/S** (1986-present) | Distributed persistent Smalltalk | ACID transactions over shared object space, 1000s of VMs on 1000s of machines |
| **Croquet/TeaTime** (2003-present) | Replicated computation | Replicate deterministic computation instead of data |
| **Distributed Smalltalk** (Bennett 1987) | `doesNotUnderstand:` proxies | Transparent forwarding via Smalltalk's own reflective mechanism |

### Security and Reflection Models

| System | Approach | Key Insight |
|--------|----------|-------------|
| **E Language** (Mark Miller, 1997) | Object capabilities + vats | Eventual sends return promises; deadlock impossible by construction |
| **Newspeak** (Gilad Bracha) | Mirrors + capabilities + modules | Stratified reflection enables safe remote introspection |
| **Agoric** (2019-present) | Hardened JavaScript | Vat model + frozen primordials = ocap discipline in JS |
| **Cap'n Proto** (Kenton Varda) | Capability-based RPC | Promise pipelining reduces N round-trips to 1 |

### Key Design Patterns

- **Vat model**: Single-threaded event loop per unit of isolation. Sync calls within a vat, async between vats. Eliminates data races and deadlocks by construction.
- **Promise pipelining**: Send messages to not-yet-resolved results. All pipelined messages travel in one batch. (Invented in Argus, perfected in E and Cap'n Proto.)
- **Proxy objects**: Local stand-ins that forward messages over network. Smalltalk's `doesNotUnderstand:` makes this trivial.
- **Mirrors**: Separate reflection into its own objects. Local mirror vs. remote mirror -- same interface, different implementation. Browser/debugger code works unchanged for remote objects.
- **Lease-based distributed GC**: Remote references have time-limited leases. Expired leases reclaim objects. Robust against network partitions.

## Architecture Assessment: Gaps for Distribution

### Critical Missing Pieces

1. **Inter-VM communication protocol** -- Image format is file-oriented, not network-oriented. No wire format for sending individual objects or messages between VMs.

2. **Symbol/selector synchronization** -- Each VM has its own interned symbol and selector tables. Selector IDs are VM-local. Cross-VM message sends need symbol negotiation.

3. **Distributed object identity** -- Objects are identified by Go pointer (`uintptr`). Cross-machine transport loses identity. Need stable, cluster-wide object IDs.

4. **Remote process spawning** -- `fork` creates local goroutines only. No `forkOn: remoteNode` equivalent.

5. **Distributed garbage collection** -- `WeakRegistry` doesn't span VMs. No remote reference lifecycle management.

6. **Location transparency layer** -- No proxy objects, no remote references, no automatic message forwarding.

### Strengths to Build On

- `VMWorker` already serializes all VM access through a single goroutine -- this is essentially the vat model
- `ConcurrencyRegistry` tracks all processes, channels, and sync objects by ID -- extensible to remote references
- `RegistryGC` periodically sweeps registries -- can be extended for lease expiration
- `keepAlive` map prevents premature GC of externally-referenced objects -- natural hook for remote leases
- `doesNotUnderstand:` creates `Message` objects -- foundation for proxy forwarding
- `ObjectHandle` in the server layer already decouples protocol from internal representation

## Recommended Approach: Layered Architecture

### Design Philosophy

**Explicit core, transparent sugar.** The E language proved that completely hiding distribution is dangerous (it conceals latency, failure modes, partial failure). But Erlang proved the API can be *nearly* identical. The sweet spot:

1. Explicit primitives that make distribution boundaries visible (`forkOn:`, eventual sends, promises)
2. Channels and processes that work across nodes without code changes when you opt in
3. Ocap-compatible architecture from day one, with full ocap enforcement deferred

### Three Approaches Considered

#### Approach A: Erlang-Style (Process Distribution)

Distribute processes across nodes. Message passing is location-transparent. Processes are the unit of distribution.

```smalltalk
"Fork on a specific node"
proc := [expression] forkOn: remoteNode.

"Channels work across nodes"
ch := Channel new.
[ch send: 'hello'] forkOn: nodeA.
ch receive.  "works from any node"

"Supervisor trees"
sup := Supervisor new: [:child |
    child restart: #oneForOne
    child add: [myServer start] as: #server
].
```

**Pros:** Natural extension of existing concurrency model. Erlang has proven this works at massive scale. Maggie's process/channel model maps directly.
**Cons:** Requires solving distributed GC, symbol table sync, and process migration. Substantial VM-level work.

#### Approach B: GemStone-Style (Shared Object Space)

Multiple VMs share a persistent object repository. Objects are accessed transactionally.

```smalltalk
"Connect to shared object space"
space := ObjectSpace connect: 'cluster://maggie-cluster'.

"Transactional access"
space transaction: [
    user := User new.
    user name: 'Alice'.
    space root at: #users add: user.
].
```

**Pros:** Familiar database-like model. GemStone has proven this works for decades in production. Strong consistency guarantees.
**Cons:** Requires building a distributed object store (significant infrastructure). Transactional model adds complexity. Less natural for real-time interactive systems.

#### Approach C: Croquet-Style (Replicated Computation)

Replicate deterministic computation across nodes. All nodes run the same code on the same inputs and converge to the same state.

```smalltalk
"Define a replicated model"
MyModel subclass: ReplicatedModel
    method: handleEvent: event [
        "Deterministic -- same input, same result on all nodes"
        self state update: event data.
    ].

"Subscribe to replicated model"
model := ReplicatedModel joinSession: 'my-session'.
model publish: (Event type: #click data: position).
```

**Pros:** Elegant for collaborative/interactive applications. No distributed GC problem (state is replicated, not shared). Low bandwidth (send inputs, not state).
**Cons:** Requires deterministic execution (no random, no wall-clock time in model code). Only works for certain application types. Complex replay/catch-up mechanism.

### Recommended: Start with Approach A (Process Distribution)

**Approach A is the best starting point** because:

1. It builds directly on Maggie's existing process/channel/concurrency model
2. It's the most general -- B and C can be built on top of A, but not vice versa
3. Erlang's 40-year track record validates the approach
4. The Go ecosystem has strong tooling for this (Ergo, NATS, quic-go)

Approaches B and C become possible later as higher-level libraries built on the distributed process foundation.

## Recommended Technology Stack

| Layer | Technology | Why |
|-------|-----------|-----|
| **Transport** | quic-go | Stream multiplexing, 0-RTT reconnect, built-in TLS. Each process-to-process conversation gets its own QUIC stream. |
| **Serialization** | fxamacker/cbor v2 | CBOR tags map naturally to NaN-boxed value types. IETF standard, security-hardened, faster than msgpack in Go benchmarks. |
| **Actor/Process** | Ergo Framework | Goroutine + mailbox model matches Maggie's ProcessObject exactly. Network-transparent processes, supervision trees. Zero dependencies. |
| **Messaging** | NATS + JetStream | Subject-based addressing maps to Smalltalk selectors. JetStream KV store for distributed class tables. Go-native. |
| **Consensus** | Dragonboat | Per-namespace Raft groups. Each Maggie namespace gets independent replication. 9M writes/sec. |
| **Discovery** | HashiCorp memberlist | SWIM gossip for cluster membership. Vivaldi coordinates for latency-aware process placement. |
| **Distributed GC** | Lease-based + weighted ref counting | Leases integrate with existing RegistryGC sweep. WRC handles acyclic remote references cheaply. |
| **Browser access** | webtransport-go | Browser-based IDE connects to Maggie cluster with full stream multiplexing. |

## Security Model: Ocap-Ready Architecture

Full object capabilities require eliminating all ambient authority from Maggie (global variables, class-side state, unrestricted `Compiler evaluate:`, direct reflection). This is a language-level change affecting all existing programs.

**Strategy: Build ocap-compatible, enforce later.**

What the distributed layer provides for free:
- Remote references are already unforgeable (opaque IDs over encrypted channels)
- Process isolation prevents shared mutable state across goroutines
- The vat model (VMWorker = single-threaded access) enforces capability discipline at boundaries

What needs to be added later for full ocaps:
- Replace global variables with explicit dependency injection
- Implement mirrors (stratified reflection) instead of direct class access
- Restrict `Compiler evaluate:` to operate within a capability scope
- Add confinement checking (no authority escalation through reflection)

This is exactly how Agoric built Hardened JavaScript -- vat model first, confinement incrementally.

## Key Decisions

1. **Layered API**: Explicit distribution primitives (`forkOn:`, `~>` eventual send, promises) with transparent channel/process sugar on top
2. **Process distribution first**: Erlang-style, not GemStone-style or Croquet-style, as the foundation
3. **Ocap-ready but deferred**: Architecture supports capabilities; enforcement comes later
4. **CBOR over Protobuf**: Dynamic Smalltalk objects need schema-free serialization with extensible tags
5. **QUIC over TCP**: Stream multiplexing is essential for many concurrent cross-node process pairs
6. **Ergo + NATS**: Actor semantics + messaging infrastructure without building from scratch
7. **Per-namespace consensus groups**: Dragonboat multi-group Raft matches Maggie's module system

## Open Questions

1. **Symbol table sync protocol**: How do new nodes join and synchronize their selector/symbol tables? Eagerly (full table on connect) or lazily (negotiate per-message)?

2. **Object migration vs. proxy**: Should objects move to where they're used (Emerald-style) or stay put with proxies forwarding messages? Different strategies for different object sizes?

3. **Failure semantics**: What happens to a `ch receive` when the sending node dies? Erlang uses monitors and links. What's the Maggie equivalent?

4. **Code loading across cluster**: When a class is modified on one node, how does it propagate? Eager push (like Erlang's `code:load/1`)? Lazy pull? Version coexistence?

5. **Image persistence in a cluster**: What does `Compiler saveImage:` mean when state is distributed? Checkpoint coordination? Consistent snapshot?

6. **JIT/AOT across nodes**: JIT-compiled methods are architecture-specific. Do all nodes compile independently, or can compiled code be shared among same-architecture nodes?

7. **Determinism for Croquet-style replication**: If we later want replicated computation, what constraints does that place on the base runtime? (No wall-clock time in model code, deterministic hash iteration, etc.)

8. **Backpressure**: When a remote channel fills up, how does backpressure propagate across the network without deadlocking?

## Phased Implementation Sketch

**Phase 1: Wire Protocol and Remote References**
- Define CBOR-based wire format for Maggie values
- Symbol table negotiation protocol
- Remote reference type (new value tag in NaN-boxing)
- Proxy objects using `doesNotUnderstand:`

**Phase 2: Remote Process Spawning**
- `forkOn:` primitive targeting remote nodes
- Process registry spanning cluster (extends ConcurrencyRegistry)
- Cross-node channel communication
- Lease-based distributed GC

**Phase 3: Cluster Infrastructure**
- Node discovery via memberlist
- Supervisor trees spanning nodes
- Distributed class table (NATS JetStream KV)
- Live code loading across cluster

**Phase 4: Higher-Level Patterns**
- Promise pipelining for eventual sends
- Mirrors for remote reflection/debugging
- Per-namespace consensus groups
- Image checkpointing in distributed setting

**Phase 5: Ocap Enforcement (Language-Level)**
- Eliminate ambient authority from core language
- Mirror-based reflection replacing direct access
- Capability-scoped evaluation
- Confinement verification

---

## Deep Dive: Vat Model + Promise Pipelining

### How Maggie Maps to E's Vat Model

**VMWorker IS a vat.** The `server/vm_worker.go` serializes all VM access through a single goroutine with a 64-item buffered channel queue. This is conceptually identical to E's vat: a single-threaded event loop processing messages sequentially.

```
VMWorker (vat)
├── requests channel (message queue, capacity 64)
├── loop() goroutine (single-threaded executor)
└── execute(fn) → result (synchronous within vat)
```

**Interpreters are already parallelizable.** Each `fork` creates a new `Interpreter` with its own stack and frames, registered per-goroutine via `vm.interpreters` sync.Map. Multiple interpreters run truly in parallel (unlike E which is single-threaded per vat).

**Channels are the vat boundary.** Maggie's channels already decouple sender/receiver, support select-based multiplexing, and pass values by copy (not shared reference). This maps directly to E's eventual sends.

### What's Missing

1. **No distinction between immediate and eventual sends.** E has `x.foo()` (immediate, within vat) and `x <- foo()` (eventual, returns promise). Maggie only has immediate sends.

2. **No promise resolution.** `ProcessObject.result` is set once and read -- there's no resolver/promise pair. You can't send messages to a not-yet-resolved result.

3. **No far references.** Objects can't refer to entities in other vats. There's no `FarReference` type that serializes across boundaries.

4. **Processes share class/symbol tables.** E vats have completely separate object spaces. Maggie processes share `VM.Classes`, `VM.Symbols`, `VM.Selectors` -- mutation in one process affects all.

### Proposed Primitives

```smalltalk
"Eventual send -- returns a Promise"
promise := obj ~> foo: bar.

"Promise pipelining -- send to unresolved result"
promise2 := promise ~> baz.

"Fork on remote node"
proc := [expression] forkOn: remoteNode.

"Channel-backed promise resolution"
promise onResolve: [:value | value printString].
promise onReject: [:error | error signal].
```

### Implementation Path

The key insight: **Promises can be built on Channels.** An eventual send `obj ~> foo: bar` is equivalent to:

```smalltalk
ch := Channel new.
[ch send: (obj foo: bar)] fork.
^ch  "Return the channel as a promise"
```

Promise pipelining then becomes channel chaining:

```smalltalk
promise ~> baz
"Becomes:"
ch2 := Channel new.
[ch2 send: (promise receive baz)] fork.
^ch2
```

This avoids adding new concurrency primitives -- it reuses existing channels and processes.

---

## Deep Dive: Mirrors for Maggie

### Current Reflection Landscape

Maggie already has substantial reflection capabilities, but they violate Bracha's mirror principles:

**Smalltalk-side** (`vm/class_reflection_primitives.go`):
- `className`, `name`, `superclass`, `superclassName`
- `allInstVarNames`, `instVarNames`
- `allMethodNames`, `methodCategories`, `methodsInCategory:`
- `methodSourceFor:`, `classMethodSourceFor:`
- `allClasses`, `allClassesSorted`

**Server-side** (`server/browse_service.go`, `server/inspect_service.go`):
- `ListClasses`, `GetClass`, `GetHierarchy`, `ListMethods`, `GetMethod`
- `FindSenders`, `FindImplementors`, `SearchSelectors`
- `Inspect`, `InspectSlot`, `InspectIndex`, `SendMessage`

**ObjectHandle system** (`server/handles.go`):
- Opaque IDs for cross-RPC references
- Session-aware lifecycle with TTL sweeping
- Objects pinned in `vm.keepAlive` to prevent GC

### How This Violates Mirror Principles

1. **Encapsulation violated**: `class_reflection_primitives.go` exposes class internals directly on class objects. Any code with a class reference can introspect it.

2. **Stratification missing**: Reflection methods live on the objects themselves (e.g., `MyClass allMethodNames`), not on separate mirror objects.

3. **No ontological correspondence**: The server returns flat protobuf messages (`ClassInfo`, `MethodInfo`), not objects with identity that can be further queried.

### Proposed Mirror Architecture

```
BaseMirror (interface)
├── ClassMirror
│   ├── LocalClassMirror (wraps *vm.Class)
│   └── RemoteClassMirror (forwards over gRPC)
├── MethodMirror
│   ├── LocalMethodMirror (wraps *vm.CompiledMethod)
│   └── RemoteMethodMirror (forwards over gRPC)
├── InstanceMirror
│   ├── LocalInstanceMirror (wraps *vm.Object)
│   └── RemoteInstanceMirror (forwards via ObjectHandle)
└── SelectorMirror
```

**The key benefit**: A class browser built with mirrors works identically for local and remote objects. You swap `LocalClassMirror` for `RemoteClassMirror` -- the browser code never changes.

### Smalltalk-Side Mirrors

```smalltalk
"Get a mirror on a class"
mirror := Mirror on: Point.

"Query through the mirror (stratified -- not on Point itself)"
mirror name.                    "→ 'Point'"
mirror superclass.              "→ Mirror on: Object"
mirror methods.                 "→ Array of MethodMirror"
mirror methodNamed: 'x'.       "→ MethodMirror"

"Instance mirror"
pt := Point x: 3 y: 4.
im := Mirror on: pt.
im class.                       "→ Mirror on: Point"
im instVarNamed: 'x'.          "→ 3"
im instVarNamed: 'x' put: 5.   "→ (modifies through mirror)"

"Remote mirror (same API!)"
remoteMirror := RemoteMirror on: 'Point' at: remoteNode.
remoteMirror methods.           "→ Array of RemoteMethodMirror (fetched via gRPC)"
```

### Integration with doesNotUnderstand:

Mirrors enable safe proxy objects:

```smalltalk
RemoteProxy subclass: Object
    instanceVars: targetNode targetID

    method: doesNotUnderstand: msg [
        "Forward all messages to remote object via mirror"
        ^targetNode send: msg selector
                    to: targetID
                    with: msg arguments
    ]
```

This is the foundation for location-transparent distributed objects. The `doesNotUnderstand:` mechanism already creates `Message` objects with selector and arguments -- perfect for network serialization.

### Changes Needed

1. **New files**: `vm/mirror.go` (Go-side mirror interfaces and LocalMirror implementations)
2. **New primitives**: `Mirror on:` factory method in `class_reflection_primitives.go`
3. **New proto**: `mirrors.proto` defining MirrorService RPC
4. **Modify**: `server/inspect_service.go` to use mirrors internally
5. **Modify**: `server/handles.go` to cache mirrors alongside handles

---

## Deep Dive: Feasibility Assessment

### Honest Ratings

| Component | Local | Distributed | Key Issue |
|-----------|-------|-------------|-----------|
| VMWorker | GREEN | RED | Single-threaded semantics, not just buffering |
| Interpreter parallelism | GREEN | YELLOW | Works locally, shared class tables are the problem |
| Symbol/Selector IDs | GREEN | RED | Sequential assignment, not portable across VMs |
| Object identity | GREEN | RED | NaN-boxed Go pointers can't survive serialization |
| Global registries | GREEN | RED | 10+ global maps (channels, processes, mutexes...) |
| Image format | GREEN | YELLOW | All-or-nothing, pointer-based, but structurally sound |
| GC/weak refs | GREEN | RED | No cross-VM reference lifecycle management |
| Error handling | GREEN | YELLOW | Stack-based handlers, closures can't serialize |
| Class mutability | GREEN | RED | Live modification incompatible with replication |
| Blocking operations | GREEN | RED | Goroutine-blocking channel/mutex ops hang network handlers |

### The Hard Truths

**1. Symbol/Selector IDs are the deepest problem.** Bytecode contains selector IDs as integers (`OpSend` reads a uint16 selector ID). Two VMs compiling the same class independently get different IDs. You cannot send bytecode from one VM to another without rewriting every selector reference. This is baked into the instruction set.

**2. Object identity via Go pointers is fundamental.** NaN-boxing encodes object pointers directly into 64-bit values. Object identity = memory address. Serialization destroys identity. Two deserializations of the same object produce two distinct identities.

**3. Global mutable state is pervasive.** At least 10 global registries in the vm package: `channelRegistry`, `processRegistry`, `mutexRegistry`, `waitGroupRegistry`, `semaphoreRegistry`, `exceptionRegistry`, `blockRegistry`, `cellRegistry`, `classVarStorage`, `weakRefCounter`. Each uses package-level `var` + `sync.Mutex`. Running two VMs in one Go process would share these globals.

**4. Blocking operations break network handlers.** `Channel.receive` blocks the goroutine. In the server, gRPC handlers call through VMWorker which runs on a dedicated goroutine. If a Smalltalk expression does `ch receive` and the sender is on another node, the VMWorker goroutine blocks indefinitely, halting all other requests.

**5. Class mutation is incompatible with replication.** `Class.AddMethod` modifies the VTable in place. In a cluster, you'd need consensus or versioning for every method addition/modification. This is a fundamental tension between Smalltalk's live-coding nature and distributed consistency.

### What This Means: Two Viable Paths

**Path A: Shared-Nothing Microservices (Achievable)**

Multiple independent Maggie VMs communicating via message passing. No shared state. Each VM is autonomous.

```
[VM A] ←--gRPC/NATS--→ [VM B] ←--gRPC/NATS--→ [VM C]
```

- Each VM has its own symbols, selectors, classes
- Messages carry symbol names, not IDs
- Objects are copied (serialized/deserialized), not shared
- No distributed GC needed (each VM manages its own objects)
- Class modifications are local (no replication)
- **Effort: Medium. This builds on existing gRPC infrastructure.**

**Path B: Full Distributed Runtime (Research Project)**

Single logical VM spanning multiple nodes. Shared object space. Location-transparent message passing.

- Requires: global symbol registry, distributed object identity, network-aware GC, consensus for class changes, non-blocking interpreter
- **Effort: Very High. This is a multi-month research project.**

### Recommendation

**Start with Path A.** It's achievable within the current architecture:
1. Add CBOR serialization for Maggie values (wire protocol)
2. Add node discovery (memberlist/NATS)
3. Add remote process spawning (`forkOn:`) that creates a process on a remote VM
4. Add channel bridging (local channel backed by NATS subject)
5. Add proxy objects via `doesNotUnderstand:` for remote references

This gives you Erlang-style distribution without touching the VM's core assumptions. Path B can come later if Path A proves insufficient.

---

## Deep Dive: Wire Protocol Design

### NaN-Boxing to CBOR Mapping

| Maggie Value | NaN-Box Tag | CBOR Encoding | Size |
|-------------|-------------|---------------|------|
| nil | Special(0) | `0xf6` (CBOR null) | 1 byte |
| true | Special(1) | `0xf5` (CBOR true) | 1 byte |
| false | Special(2) | `0xf4` (CBOR false) | 1 byte |
| SmallInt 0-23 | 0x0002 | `0x00`-`0x17` | 1 byte |
| SmallInt 24-255 | 0x0002 | `0x18 XX` | 2 bytes |
| SmallInt 256-65535 | 0x0002 | `0x19 XX XX` | 3 bytes |
| Float64 | raw double | `0xfb` + 8 bytes | 9 bytes |
| Symbol | 0x0004 | Tag 70001 + text | 4-20 bytes |
| Character | special | Tag 70002 + uint32 | 5 bytes |
| Object | 0x0001 | Tag 70003 + [...] | variable |
| Object ref | 0x0001 | Tag 70004 + uint32 | 5 bytes |
| Channel | marker | Tag 70005 + metadata | 5-60 bytes |
| Process | marker | Tag 70006 + metadata | 5-60 bytes |

**Key insight:** CBOR's variable-length integers save 6-8 bytes per small value compared to the fixed 9-byte image format. For a typical message with 5 small integers and 2 symbols, that's 40-60 bytes saved per message.

### Symbol Synchronization Protocol

**Recommended: Name-based transmission** (simple, decoupled)

```
Node A sends message:
  { selector: "at:put:", receiver: <object>, args: [key, value] }

Node B receives:
  1. Looks up "at:put:" in local SelectorTable
  2. If not found, interns it (assigns local ID)
  3. Dispatches using local ID
```

No coordination needed. Each VM maintains its own ID space. Symbol names are the canonical identity.

**Optimization for persistent connections:** After the first exchange, cache a bidirectional ID mapping:

```
Connection A↔B:
  A's "at:put:" (ID 42) ↔ B's "at:put:" (ID 99)

Subsequent messages use local IDs with connection-scoped remapping.
```

### Object Serialization Strategy

**Two-pass encoding** for cycle handling:

1. **Pass 1**: Walk object graph breadth-first, assign CBOR object IDs
2. **Pass 2**: Emit each object with references using assigned IDs

```cbor
// Object graph message
Tag 70030 = {
    "objects": [
        // ID 0: Point instance
        Tag 70003 = { "class": "Point", "slots": [3, 4] },
        // ID 1: Array referencing Point
        Tag 70003 = { "class": "Array", "slots": [Tag 70004 = 0] }
    ],
    "root": 1  // Entry point is object ID 1
}
```

### CompiledMethod Transmission

```cbor
Tag 70010 = {
    "name": "foo:",
    "arity": 1,
    "numTemps": 2,
    "literals": [42, "hello", Tag 70001 = "bar"],
    "bytecode": h'0A0B0C...',  // Raw bytes
    "blocks": [Tag 70010 = {...}]  // Nested block methods
    // Source omitted by default for wire protocol
}
```

**Critical**: Bytecode contains selector IDs that are VM-local. When transmitting a method, selector references in bytecode must be rewritten using the symbol name mapping. This requires a bytecode rewriting pass on import.

### Size Estimates

| Payload | Current Image Format | CBOR Wire Format | Savings |
|---------|---------------------|-------------------|---------|
| nil | 9 bytes | 1 byte | 89% |
| SmallInt 42 | 9 bytes | 1 byte | 89% |
| Point(3,4) | 27 bytes | ~15 bytes | 44% |
| 100-element Array | ~900 bytes | ~200 bytes | 78% |
| Simple method | ~200 bytes | ~100 bytes | 50% |
| Class definition | ~3000 bytes | ~1500 bytes | 50% |

---

## References

### Historical Systems
- [Emerald Programming Language](https://www.emeraldprogramminglanguage.org/) -- Fine-grained object mobility
- [GemStone/S](https://gemtalksystems.com/products/gs64/) -- Distributed persistent Smalltalk
- [Croquet/TeaTime](https://en.wikipedia.org/wiki/Croquet_Project) -- Replicated computation
- [Argus](https://dl.acm.org/doi/10.1145/42392.42399) -- Distributed programming with nested transactions (Liskov, 1988)

### Security and Reflection
- [E Language](https://en.wikipedia.org/wiki/E_(programming_language)) -- Object capabilities, vats, promise pipelining
- [Mirrors: Design Principles](https://bracha.org/mirrors.pdf) -- Bracha & Ungar, OOPSLA 2004
- [Robust Composition](https://research.google/pubs/pub37154/) -- Mark Miller's PhD thesis on OCaps
- [Newspeak](https://bracha.org/Site/Newspeak.html) -- Mirrors + capabilities + modules
- [Cap'n Proto](https://capnproto.org/) -- Capability-based RPC with promise pipelining
- [Agoric / Hardened JavaScript](https://docs.agoric.com/guides/js-programming/hardened-js)

### Modern Infrastructure
- [Ergo Framework](https://github.com/ergo-services/ergo) -- Erlang/OTP patterns in Go
- [NATS](https://nats.io/) -- Go-native messaging with JetStream persistence
- [Dragonboat](https://github.com/lni/dragonboat) -- Multi-group Raft in Go
- [quic-go](https://github.com/quic-go/quic-go) -- QUIC transport for Go
- [fxamacker/cbor](https://github.com/fxamacker/cbor) -- Security-hardened CBOR for Go
- [HashiCorp memberlist](https://github.com/hashicorp/memberlist) -- SWIM gossip protocol

### Erlang/BEAM
- [Distributed Erlang](https://www.erlang.org/doc/system/distributed.html)
- Joe Armstrong's PhD thesis on process isolation and fault tolerance

### Distributed GC
- Weighted Reference Counting (Bevan, Watson & Watson, 1987)
- SSP Chains (Shapiro, Dickman & Plainfosse, 1992)
- [Survey of Distributed GC](https://link.springer.com/chapter/10.1007/3-540-60368-9_26) (Plainfosse & Shapiro)
