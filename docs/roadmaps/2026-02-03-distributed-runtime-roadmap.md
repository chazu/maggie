# Distributed Runtime Roadmap

**Date:** 2026-02-03
**Status:** Engineering roadmap, derived from brainstorm review
**Scope:** From current state to Erlang-grade distributed runtime with object capabilities

---

## Honest Assessment of Where We Are

Maggie today is a single-node, single-VM runtime with goroutine-backed concurrency. It has real strengths -- NaN-boxed values, a clean bytecode compiler, Go channels under the hood, inline caches, a working image format, a gRPC server. But between where it stands and "telecom-grade distributed runtime with object capabilities," there is roughly three years of focused engineering work.

Here is what exists, what is missing, and what is broken, stated plainly.

### What exists and works

- **Bytecode VM** with 55 opcodes (not 97 as the brainstorm claims), inline caches, TCO for self-recursive tail calls, and a profiler. The interpreter loop in `/Users/chazu/dev/go/maggie/vm/interpreter.go` (the `runFrame()` method at line 452) is the hot path.
- **NaN-boxed values** with 7 tags: object, int, special, symbol, block, cell, context. Defined in `/Users/chazu/dev/go/maggie/vm/value.go` lines 38-44.
- **Goroutine-backed processes** via `fork`/`forkWith:`/`forkWithContext:` in `/Users/chazu/dev/go/maggie/vm/concurrency.go` lines 435-557. Each fork gets a new `Interpreter` with its own stack and frames.
- **Go channels** exposed as Maggie channels. Blocking send/receive, non-blocking variants, select. The `ChannelObject` wraps a `chan Value` directly (line 14 of `concurrency.go`).
- **Image serialization** that captures the full VM state -- classes, methods, symbols, selectors, globals, objects. The `ImageWriter` in `/Users/chazu/dev/go/maggie/vm/image_writer.go` and `ImageEncoder` in `/Users/chazu/dev/go/maggie/vm/image_encoding.go` handle this.
- **gRPC/Connect server** with evaluation, browsing, modification, inspection, and session services. Uses a `VMWorker` to serialize access. See `/Users/chazu/dev/go/maggie/server/server.go`.
- **Concurrency primitives**: Mutex, WaitGroup, Semaphore, CancellationContext -- all wrapping Go sync primitives.
- **doesNotUnderstand:** dispatches to a `Message` object containing selector and arguments. See `sendDoesNotUnderstand` at `/Users/chazu/dev/go/maggie/vm/interpreter.go` line 1119.
- **become:** with forwarding pointers in `/Users/chazu/dev/go/maggie/vm/object.go` lines 324-357.
- **Module system** with namespaces, `import:`, and directory-as-namespace convention.

### What is broken or half-built

- **Dual registry architecture.** There are two parallel registries for channels, processes, mutexes, etc.: package-level globals (e.g., `channelRegistry` at `/Users/chazu/dev/go/maggie/vm/concurrency.go` line 22) and VM-local ones in `ConcurrencyRegistry` (`/Users/chazu/dev/go/maggie/vm/concurrency_registry.go`). Some code paths use one, some use the other. The globals prevent running two VMs in a single Go process.
- **Shared mutable state across goroutines.** `VM.Globals` is a plain `map[string]Value` with no synchronization (assigned at `/Users/chazu/dev/go/maggie/vm/vm.go` line 147 via `interp.Globals = vm.Globals`). Forked processes read and write this map concurrently. `classVarStorage` in `/Users/chazu/dev/go/maggie/vm/class.go` line 70 is similar -- global mutable map.
- **String and dictionary values are global-registry-encoded.** Strings use IDs in range `[0x80000000, 0xC0000000)` and dictionaries use IDs >= `0xC0000000`, both backed by package-level registries. See `/Users/chazu/dev/go/maggie/vm/string_primitives.go` line 35 and `/Users/chazu/dev/go/maggie/vm/dictionary_primitives.go` line 30. These cannot be isolated per-VM.
- **Block values capture interpreter-local state.** The `BlockValue` struct (line 1515 of `interpreter.go`) stores `HomeFrame int` -- an index into a specific interpreter's `frames` slice. This is meaningless on another interpreter, let alone another machine.
- **Exception handling uses Go panic/recover.** `SignaledException` is panicked in `/Users/chazu/dev/go/maggie/vm/exception.go` line 100, caught by `on:do:` handlers installed on the interpreter's handler stack. This does not cross goroutine boundaries and cannot cross machine boundaries.

### What is entirely missing

- Any wire protocol for sending individual values between VMs
- Any concept of a remote reference or proxy object
- Any process supervision or monitoring
- Any distributed garbage collection
- Any concept of eventual sends or promises
- Any mirror-based reflection (reflection is all direct class access)
- Any capability discipline (globals are ambient authority)

---

## The Technology Stack Question

The brainstorm recommends seven dependencies: quic-go, CBOR, Ergo, NATS, Dragonboat, memberlist, webtransport-go. This is overengineered for where we are. Here is what to actually use and when.

### Use immediately (Phase 0-1)

- **fxamacker/cbor v2** -- Yes. CBOR is the right wire format for Maggie values. It has extensible tags for NaN-boxed types, variable-length integers for compact small-value encoding, and it is an IETF standard. The brainstorm's recommendation is correct. msgpack would also work but CBOR's tag system maps more naturally to Maggie's type menagerie.
- **Existing gRPC/Connect** -- Maggie already has Connect services. Add a new `NodeService` proto for inter-node communication rather than introducing NATS. gRPC with HTTP/2 gives multiplexed streams over a single TCP connection. This is sufficient for the first two phases.

### Use when needed (Phase 2-3)

- **NATS** -- Useful for pub/sub messaging and node discovery once there are more than 2-3 nodes. Premature before that. Do not use JetStream KV for a distributed class table -- that is a solution in search of a problem at this stage.
- **HashiCorp memberlist** -- Useful once you need cluster membership. Not before Phase 3. For 2-3 nodes, static configuration is fine.

### Probably never use

- **Ergo Framework** -- Ergo reimplements Erlang's actor model in Go. But Maggie already has its own process model that maps directly to goroutines. Ergo would be a second, competing concurrency model. Instead, extend Maggie's existing `ProcessObject`/`ChannelObject` to be network-aware. The right reference is not Ergo's code but Erlang's distribution protocol specification.
- **Dragonboat** -- Per-namespace Raft consensus groups are a solution for a problem that does not exist yet. If you ever need consensus (and you might not -- eventual consistency via CRDTs is often better for live-coding environments), you should evaluate it then, not now.
- **quic-go** -- QUIC's advantages (stream multiplexing, 0-RTT) only matter when you have many concurrent cross-node conversations and need reconnection speed. gRPC over HTTP/2 already gives multiplexed streams. QUIC is a Phase 4+ concern.
- **webtransport-go** -- Browser IDE connectivity is orthogonal to the distributed runtime. It is a separate project.

### The key insight

Maggie's advantage is that it sits on top of Go's runtime. Go already provides goroutines (millions of them, cheaply), channels (lock-free, multiplexed), a mature HTTP/2 stack, and a garbage collector. The distributed runtime should lean on Go's strengths rather than layering additional frameworks that provide competing versions of what Go already offers.

---

## What Erlang/BEAM Gets Right That Maggie Must Replicate

### Must replicate

1. **Process isolation.** In BEAM, each process has its own heap. A crash in one process cannot corrupt another's memory. Maggie processes share `VM.Globals`, `classVarStorage`, and all 12+ global registries. This must be fixed before distribution makes any sense, because a crash in a remote process that corrupts shared state would be catastrophic.

2. **Asynchronous message passing.** In BEAM, `Pid ! Message` is asynchronous -- the sender never blocks. Maggie's `ch send:` blocks the goroutine when the channel buffer is full. For local concurrency this is fine (Go channels work this way by design). For distribution, blocking sends create deadlock risks across the network. You need an asynchronous send primitive.

3. **Monitors and links.** When an Erlang process dies, linked processes are notified and can choose to die too or handle the exit. Maggie has no equivalent. `ProcessObject.done` is a channel that closes on completion (line 163 of `concurrency.go`), but there is no link/monitor mechanism. Supervision trees are impossible without this.

4. **Hot code loading with version coexistence.** BEAM allows two versions of a module to coexist: running processes continue using the old version, new calls use the new version. Maggie mutates `VTable.methods` in place (see `AddMethod` in `/Users/chazu/dev/go/maggie/vm/vtable.go` line 36). This means live code changes affect all running processes immediately, with no atomicity guarantees and no rollback.

5. **Location-transparent process identifiers.** In BEAM, a PID is `{node, id, serial, creation}` -- it encodes the node of origin. Maggie's process IDs are sequential uint64s with no node information. See `processToValue` at line 132 of `concurrency.go`.

### Can skip or do differently

1. **Per-process heaps.** BEAM's per-process heap enables per-process GC (no global stop-the-world). Maggie uses Go's garbage collector, which is concurrent and generational. Per-process heaps would require reimplementing memory allocation for Maggie objects, which is not worth the cost. Instead, isolate *mutable state* (globals, class variables, registries) without isolating the heap.

2. **Preemptive scheduling.** BEAM's scheduler preempts processes based on reduction counts. Go's runtime preempts goroutines at function calls (and since Go 1.14, at arbitrary points via signals). Maggie gets preemptive scheduling for free from Go.

3. **Binary pattern matching.** BEAM's binary matching is designed for protocol parsing in telecom. Maggie does not need this -- it can use ByteArray and CBOR decoding in Go.

4. **Distribution protocol from scratch.** BEAM has its own TCP-based distribution protocol. Maggie can use gRPC/Connect, which is better tooled, better documented, and easier to debug.

---

## Where Object Capabilities Fit

The brainstorm's answer -- "build ocap-compatible, enforce later" -- is partly right and partly wrong. You cannot retrofit capabilities onto an architecture that was designed around ambient authority. But you also cannot build a useful distributed runtime if you insist on full ocap discipline from day one.

The correct strategy is: **make the distributed boundary capability-safe from the start, while leaving the local VM permissive.**

Concretely:
- Remote references should be unforgeable tokens (opaque IDs over authenticated channels). This falls out naturally from the wire protocol.
- The `NodeService` API should not expose `Compiler evaluate:` across the wire. Remote execution should be limited to sending messages to objects you hold references to.
- `doesNotUnderstand:` on proxy objects should only forward to objects the proxy was created for, not to arbitrary targets.
- The local VM can keep its globals, class variables, and unrestricted reflection for now.

This means Phase 1's wire protocol is the place to get capabilities right. If remote references are capability-safe from day one, the later ocap enforcement phases only need to constrain the *local* VM, which is a much smaller surface area.

---

## Phased Roadmap

### Phase 0: Foundation Cleanup

**What this phase does:** Eliminate the technical debt that blocks everything else. No new features, no distribution -- just fixing the foundations.

**Duration:** 4-6 weeks

**Concrete work:**

1. **Unify the dual registries.** Migrate all package-level global registries to `ConcurrencyRegistry` (or a new `ObjectRegistry` on the VM struct). Affected globals:
   - `channelRegistry` + `channelRegistryMu` + `nextChannelID` at `/Users/chazu/dev/go/maggie/vm/concurrency.go:22-24`
   - `processRegistry` + `processRegistryMu` + `nextProcessID` at lines 102-104
   - `blockRegistry` + `blockRegistryMu` + `nextBlockID` at `/Users/chazu/dev/go/maggie/vm/interpreter.go:1532-1534`
   - `blocksByHomeFrame` at line 1538
   - `cellRegistry` at `/Users/chazu/dev/go/maggie/vm/value.go:304`
   - `exceptionRegistry` + `exceptionRegistryMu` + `nextExceptionID` at `/Users/chazu/dev/go/maggie/vm/exception.go:36-38`
   - `resultRegistry` + `resultRegistryMu` + `nextResultID` at `/Users/chazu/dev/go/maggie/vm/result.go:30-32`
   - `classVarStorage` + `classVarMu` at `/Users/chazu/dev/go/maggie/vm/class.go:70-71`
   - `classValueRegistry` at `/Users/chazu/dev/go/maggie/vm/class_value.go`
   - String and dictionary registries at `/Users/chazu/dev/go/maggie/vm/string_primitives.go` and `dictionary_primitives.go`

   The approach: add fields to the `VM` struct for each registry. Update all primitive functions that currently call the global `registerChannel()` to call `v.registerChannel()` instead. Delete the globals. Verify with `go vet -vettool` that no package-level mutable state remains.

2. **Synchronize `VM.Globals`.** Replace `map[string]Value` with a concurrent map or add a `sync.RWMutex`. Every access must go through `LookupGlobal`/`SetGlobal` methods. Audit the interpreter loop -- `OpPushGlobal` and `OpStoreGlobal` access `i.Globals` directly (lines 569-607 of `interpreter.go`). These need to use the synchronized accessor.

3. **Formalize the marker allocation table.** Document all allocated NaN-boxing markers in one place. Current allocations:
   - `1 << 24` = channels
   - `2 << 24` = processes
   - `4 << 24` = results
   - `8 << 24` = exceptions
   - `16 << 24` = weak references
   - `36 << 24` = class values
   - `0x10 << 24` = mutexes (verify)
   - `0x14 << 24` = wait groups (verify)
   - `0x18 << 24` = semaphores (verify)
   - `0x20 << 24` = cancellation contexts (verify)
   - `0x28 << 24` = characters
   - `0x30 << 24` = gRPC clients
   - `0x34 << 24` = gRPC streams
   - String range: `0x80000000 - 0xBFFFFFFF`
   - Dictionary range: `0xC0000000+`

   Reserve markers for: remote references, promises, node IDs.

4. **Add a test that runs two VMs in one Go process.** This is the litmus test for registry isolation. If two `vm.NewVM()` instances can coexist without interfering with each other, the globals are eliminated.

**Demo moment:** Two independent `vm.NewVM()` instances run concurrently in one Go process, each evaluating expressions that create channels, processes, and exceptions, with no interference. This is boring but essential.

**Hard problems solved:** Package-level mutable state elimination. Concurrent Globals access.

**Explicitly deferred:** Everything distributed. This phase is pure local cleanup.

**Prerequisites:** None. This can start today.

---

### Phase 1: Wire Protocol and Value Serialization

**What this phase does:** Define how Maggie values travel over the wire. Build a CBOR codec that can round-trip every value type. No networking yet -- just serialization.

**Duration:** 4-6 weeks

**Concrete work:**

1. **New package: `vm/wire/`** containing CBOR encoding/decoding for all Maggie value types. Unlike the image format (which is all-or-nothing, designed for snapshotting an entire VM), the wire format encodes individual values, messages, and object graphs for transmission between independent VMs.

   CBOR tag allocations (in the IANA first-come-first-served range 256+):
   - Tag 70001: Symbol (text string payload)
   - Tag 70002: Character (uint32 code point)
   - Tag 70003: Object (class name + slots array)
   - Tag 70004: Object reference (backreference within a message, uint32 index)
   - Tag 70005: Remote reference (node ID + handle ID -- the capability token)
   - Tag 70006: Message send (selector string + arguments array)
   - Tag 70007: Method source (for code loading -- source text, not bytecode)
   - Tag 70010: Error/exception (class name + message text)
   - Tag 70020: Promise reference (promise ID)

   Critical design decisions:
   - **Symbols transmitted by name, not by ID.** Each VM resolves names to local IDs via `SelectorTable.Intern()`. No negotiation protocol needed.
   - **Objects serialized by value (deep copy) by default.** The sender walks the object graph breadth-first, assigns indices, and encodes each object with class name + slot values. The receiver reconstructs equivalent objects. Object identity is not preserved across the wire.
   - **Remote references are explicit.** A `RemoteRef` is `{nodeID: string, handleID: string}`. It encodes as Tag 70005. The receiver wraps it in a local proxy.
   - **Blocks are NOT serializable.** A block closure contains a `HomeFrame` and potentially cells (mutable heap-allocated boxes). These are machine-local by nature. The wire codec returns an error if asked to encode a block. This constraint propagates upward: you cannot send a block to a remote node. You can send a method (by source text) or a message, but not a block.

2. **String and dictionary serialization.** These must be handled specially because they are registry-encoded, not heap objects. The wire codec extracts the string content (via `GetStringContent()` from `/Users/chazu/dev/go/maggie/vm/string_primitives.go`) and encodes it as a CBOR text string. On decode, it calls `NewStringValue()` to allocate in the local registry.

3. **Round-trip test suite.** Every value type must survive encode/decode: nil, true, false, SmallInt (edge cases: 0, -1, MaxSmallInt, MinSmallInt), Float64 (edge cases: 0.0, -0.0, Inf, -Inf, NaN), Symbol, Character, String, Array, Dictionary, Object with cyclic references, nested objects.

4. **Message envelope format.** A wire message is:
   ```
   {
     "type": "send" | "reply" | "error" | "spawn" | "monitor" | "link",
     "id": uint64,          // message ID for correlating replies
     "target": RemoteRef,   // recipient
     "selector": string,    // method name
     "args": [Value...],    // encoded arguments
   }
   ```
   This is the fundamental unit of inter-node communication.

**Demo moment:** A Go test that serializes a complex Maggie object graph (Dictionary containing Arrays containing Points with cyclic back-references) to CBOR bytes, decodes it on a separately-created VM, and verifies structural equivalence.

**Hard problems solved:** Defining what can and cannot cross the wire (blocks cannot). CBOR tag allocation. Cycle handling in object graphs. String/dictionary encoding outside the image format.

**Explicitly deferred:** Networking. The codec is a pure library; it does not open sockets.

**Prerequisites:** Phase 0 (registry unification). The wire codec must allocate strings and objects in a VM's registry, not in global tables.

---

### Phase 2: Promises, Process Links, and the Process Model

**What this phase does:** Build the process infrastructure needed for distribution -- even though this phase is still single-node. Promises with rejection semantics, process links and monitors, and the `Process.current` identity.

**Duration:** 6-8 weeks

**Concrete work:**

1. **Promise class.** A `Promise` has three states: pending, resolved, rejected. It wraps a channel internally but adds failure propagation.

   Go-side implementation in `vm/promise.go`:
   ```
   type PromiseObject struct {
       state    atomic.Int32   // pending=0, resolved=1, rejected=2
       value    Value          // result on resolve
       reason   Value          // error on reject
       done     chan struct{}   // closed on resolution
       mu       sync.Mutex
       handlers []PromiseHandler // onResolve/onReject callbacks
   }
   ```

   Maggie-side API:
   ```smalltalk
   p := Promise new.
   p resolve: value.         "Resolve with value"
   p reject: error.          "Reject with error"
   p then: [:v | v + 1].     "Chain on resolution (returns new Promise)"
   p catch: [:e | fallback]. "Chain on rejection (returns new Promise)"
   p wait.                   "Block until resolved, return value or raise"
   p isResolved.
   p isRejected.
   p isPending.
   ```

   The critical feature: **if a process crashes (unhandled exception, Go panic), all promises it was computing are automatically rejected.** This is implemented by wrapping the `fork` goroutine's `defer/recover` to reject any outstanding promises associated with that process.

   Marker allocation: `0x40 << 24` for promise IDs.

2. **Process links and monitors.** Port Erlang's link/monitor semantics:

   - `proc link: otherProc` -- bidirectional death notification. If either dies, the other receives a `ProcessExit` exception (catchable via `on:do:`).
   - `proc monitor` -- returns a `MonitorRef`. When proc dies, the monitoring process receives a `ProcessDown` message on a notification channel.

   Go-side: add `links []*ProcessObject` and `monitors []MonitorRef` fields to `ProcessObject`. The `markDone` method (line 156 of `concurrency.go`) must iterate links and monitors, sending notifications.

3. **Process identity.** `Process current` currently returns Nil (line 636 of `concurrency.go`). Fix this: the per-goroutine interpreter tracking (`vm.interpreters` sync.Map) should also track the current `ProcessObject`. Implement `Process current` to return the actual process.

4. **Node-aware process IDs.** Change process ID encoding from plain uint64 to `{nodeID uint16, localID uint32}`. The `processToValue` function packs both into the 24 available bits of the symbol ID (after the marker byte). For Phase 2, nodeID is always 0 (local). The wire protocol can encode remote process IDs using the full `RemoteRef` format.

5. **Eventual send operator.** Add `~>` as syntactic sugar in the compiler (`/Users/chazu/dev/go/maggie/compiler/parser.go`). `obj ~> foo: bar` compiles to:

   ```
   Promise fromSend: obj selector: 'foo:' args: {bar}
   ```

   This creates a promise, spawns a lightweight process to perform the send, and returns the promise. The process is linked to the promise for failure propagation.

   Compiler changes: the lexer needs to recognize `~>` as a new binary operator (add to `token.go`). The parser treats it as a message send prefix that wraps in promise creation. The codegen emits a call to a well-known `Promise>>fromSend:selector:args:` class method.

**Demo moment:** A supervisor-like pattern expressed in Maggie:

```smalltalk
worker := [
    "... do work, might crash ..."
    1 / 0  "intentional crash"
] fork.

monitor := worker monitor.
monitor onDown: [:reason |
    'Worker died: ', reason printString.
    "restart it"
    worker := [self doWork] fork
].
```

Also: `promise := obj ~> expensiveComputation. promise then: [:result | result printString].` -- if the computation raises, the `then:` block never fires and `promise catch:` handles the error.

**Hard problems solved:** Promise rejection propagation. Process crash notification. Linking `~>` syntax through the full compiler pipeline (lexer/parser/codegen).

**Explicitly deferred:** Networking. All of this runs on a single node. But the process model is now ready for distribution because process IDs encode node information and promises handle failure.

**Prerequisites:** Phase 0 (registry unification -- promises need a VM-local promise registry). Phase 1 (wire protocol -- promise IDs and process IDs need marker allocations that do not conflict).

---

### Phase 3: Node-to-Node Communication

**What this phase does:** Two Maggie VMs on different machines can exchange messages. This is the first phase where bytes travel over the network.

**Duration:** 8-12 weeks

**Concrete work:**

1. **New proto: `node.proto`** defining the NodeService:

   ```protobuf
   service NodeService {
     // Send a message to an object on this node
     rpc Send(SendRequest) returns (SendReply);

     // Spawn a process on this node
     rpc Spawn(SpawnRequest) returns (SpawnReply);

     // Register a monitor for a process on this node
     rpc Monitor(MonitorRequest) returns (stream MonitorEvent);

     // Establish a channel bridge
     rpc ChannelBridge(stream ChannelMessage) returns (stream ChannelMessage);

     // Handshake: exchange node metadata
     rpc Handshake(HandshakeRequest) returns (HandshakeReply);
   }
   ```

   `SendRequest` contains the wire-format (CBOR) message envelope from Phase 1. `SendReply` contains the CBOR-encoded return value or error.

2. **Remote reference and proxy object system.** New files: `vm/remote_ref.go`, `vm/proxy.go`.

   A `RemoteRef` is a NaN-boxed value using the reserved marker (e.g., `0x44 << 24`). It encodes into a registry that maps to `{nodeID, handleID}`. When a message is sent to a `RemoteRef`, the `doesNotUnderstand:` mechanism (or a more efficient vtable-level intercept) forwards the message over gRPC to the target node.

   Implementation options for the forwarding intercept:
   - **Option A: doesNotUnderstand:.** Create a `RemoteProxy` class with no methods except `doesNotUnderstand:`. Every message falls through to DNU, which serializes and sends. This is the classic Smalltalk approach (Bennett 1987) and is already supported by the existing DNU infrastructure at `/Users/chazu/dev/go/maggie/vm/interpreter.go` line 1119.
   - **Option B: VTable intercept.** Install a special method at selector ID 0 (or a sentinel) that the interpreter checks before normal dispatch. This avoids the overhead of DNU (hash lookup for `doesNotUnderstand:`, Message object allocation) on every remote call.

   Recommendation: start with Option A for simplicity, measure, switch to Option B if proxy dispatch is a bottleneck.

3. **Channel bridging.** A "bridge" connects a local channel to a remote channel over a bidirectional gRPC stream. When a value is sent to the local side, the bridge serializes it (via Phase 1 codec) and sends it over the stream. Values arriving on the stream are deserialized and injected into the local channel.

   Critical constraint: **the bridge goroutine must not block the VMWorker.** The bridge runs in its own goroutine, reads from the local channel, and writes to the gRPC stream. This is separate from both the VMWorker and the interpreter goroutines.

   Backpressure: use the gRPC flow control window. When the remote side cannot consume fast enough, the gRPC stream blocks, which blocks the bridge goroutine, which blocks the local channel send. This is the same backpressure model as a local buffered channel.

4. **Remote process spawning.** `forkOn: node` sends a `SpawnRequest` containing the method source text (not bytecode, not a block closure). The remote node compiles and executes it. Returns a remote process reference.

   Why source text and not bytecode: bytecode contains VM-local selector IDs. Rewriting bytecode (as discussed in the review) is possible but error-prone. Source text is compiled by the target VM's own compiler, ensuring correct selector ID assignment. The cost is a compilation step on spawn, which is amortized over the process's lifetime.

   Why not block closures: as established in Phase 1, blocks capture `HomeFrame` and cells that are machine-local. The API for remote spawning must be:

   ```smalltalk
   proc := 'MyWorker start' forkOn: remoteNode.
   ```

   or:

   ```smalltalk
   proc := remoteNode spawn: MyWorker selector: #start args: {}.
   ```

   Not:

   ```smalltalk
   proc := [self doWork] forkOn: remoteNode.  "CANNOT WORK -- block not serializable"
   ```

   This is an honest limitation. Document it clearly. It is the same constraint Erlang has -- you cannot send a closure to a remote node; you send a module/function/args tuple (`spawn(Node, Module, Function, Args)`).

5. **Lease-based remote reference GC.** When node A holds a `RemoteRef` to an object on node B, node B pins the object via `KeepAlive`. The reference has a TTL (default: 5 minutes). Node A must send periodic lease renewals. If a renewal is not received (because A crashed or the network partitioned), node B unpins the object.

   This integrates with the existing `HandleStore.Sweep()` at `/Users/chazu/dev/go/maggie/server/handles.go` line 125, which already sweeps handles by TTL.

**Demo moment:** Two Maggie VMs on different machines. VM A spawns a process on VM B that computes `1000 factorial`. VM A receives the result via a channel bridge. If VM B crashes during computation, VM A's promise is rejected and the error is printed.

```smalltalk
node := MaggieNode connect: 'vm-b.example.com:9090'.
proc := node spawn: BigComputer selector: #factorial args: { 1000 }.
result := proc wait.
result printString.
```

**Hard problems solved:** Remote reference lifecycle (leases). Channel bridging with backpressure. The "blocks cannot be serialized" constraint (forcing source-text-based remote spawning). Process monitor notifications across the network.

**Explicitly deferred:** Cluster discovery (manual node addresses). Supervision trees (monitors exist but no supervisor pattern). Hot code loading. Multiple nodes (this phase is 2-node).

**Prerequisites:** Phase 0 (registry isolation -- two VMs coexist). Phase 1 (wire codec). Phase 2 (promises, monitors, node-aware process IDs).

---

### Phase 4: Supervision Trees and Fault Tolerance

**What this phase does:** The "let it crash" philosophy becomes real. Processes are organized into supervision trees that restart failed children automatically.

**Duration:** 6-8 weeks

**Concrete work:**

1. **Supervisor class.** A `Supervisor` manages a list of child specifications. Each child spec is `{id, startFn, restartStrategy}`. Restart strategies:
   - `#oneForOne` -- restart only the crashed child
   - `#oneForAll` -- restart all children when one crashes
   - `#restForOne` -- restart the crashed child and all children started after it

   The supervisor is itself a process. It monitors all children via the Phase 2 monitor mechanism. When a child dies, it applies the restart strategy.

   Implementation: `lib/Supervisor.mag` -- the supervisor logic is written in Maggie, not Go. The Go side only provides the monitor primitives (which already exist from Phase 2). This keeps the runtime small and the supervision logic inspectable/modifiable at the Smalltalk level.

   ```smalltalk
   Supervisor subclass: Object
       instanceVars: children strategy maxRestarts restartWindow

       classMethod: new: block strategy: strategy [
           | sup |
           sup := self basicNew.
           sup init: block strategy: strategy.
           ^sup
       ]

       method: init: block strategy: strategy [
           children := Array new.
           self strategy: strategy.
           block value: self.
           ^self start
       ]

       method: addChild: id start: startBlock [
           children add: (Association key: id value: startBlock)
       ]

       method: start [
           children do: [:spec |
               | proc |
               proc := spec value fork.
               proc monitor onDown: [:reason |
                   self handleChildDown: spec key reason: reason
               ]
           ]
       ]
   ```

2. **Application structure.** An `Application` groups supervisors and provides lifecycle management:

   ```smalltalk
   MyApp := Application new: [
       self addSupervisor: (Supervisor new: [:sup |
           sup addChild: #listener start: [TcpListener start: 8080].
           sup addChild: #worker   start: [WorkerPool start: 4].
       ] strategy: #oneForOne)
   ].

   MyApp start.
   ```

3. **Distributed supervisor.** A supervisor that spans nodes:

   ```smalltalk
   sup := Supervisor new: [:s |
       s addChild: #web start: [WebServer start] on: webNode.
       s addChild: #db  start: [DbProxy start]  on: dbNode.
   ] strategy: #oneForOne.
   ```

   The `on: node` variant uses the Phase 3 remote spawning. Monitor notifications flow over the gRPC monitor stream.

4. **Max restart intensity.** Like Erlang, a supervisor shuts down if it exceeds `maxRestarts` within `restartWindow`. Default: 5 restarts in 10 seconds.

**Demo moment:** A three-node cluster where a key process crashes every few seconds. The supervisor automatically restarts it. Kill an entire node -- the distributed supervisor detects the failure and restarts the process on a surviving node.

**Hard problems solved:** Restart strategy implementation. Max restart intensity with sliding window. Distributed supervisor that handles node failure vs. process failure.

**Explicitly deferred:** Hot code loading (next phase). Full ocap enforcement. Dynamic cluster membership.

**Prerequisites:** Phase 2 (monitors, links, promises). Phase 3 (remote spawning, node connectivity).

---

### Phase 5: Hot Code Loading

**What this phase does:** Change running code on a live cluster without stopping processes.

**Duration:** 8-12 weeks

**Concrete work:**

1. **Method versioning.** Currently, `VTable.AddMethod` at `/Users/chazu/dev/go/maggie/vm/vtable.go` line 36 overwrites the method slot in place. Change this to support two concurrent versions:

   New VTable structure:
   ```go
   type VTable struct {
       class    *Class
       parent   *VTable
       methods  []Method    // current version
       previous []Method    // previous version (nil if no upgrade in progress)
       version  uint64      // monotonically increasing
   }
   ```

   When a method is added/replaced, the old `methods` slice becomes `previous`, and a new slice (copy + modification) becomes `methods`. Running processes that are mid-execution in a method continue using their existing frame's method pointer (which points into the old version). New calls resolve from the current `methods`.

   After a configurable grace period (or when no frames reference old methods), `previous` is set to nil and old methods become GC-eligible.

2. **Cluster-wide code loading.** A new method/class definition is published to all nodes via a gRPC streaming endpoint:

   ```protobuf
   rpc CodeUpdate(stream CodeChange) returns (stream CodeChangeAck);

   message CodeChange {
     string class_name = 1;
     string selector = 2;
     string source = 3;
     bool is_class_side = 4;
     uint64 version = 5;
   }
   ```

   Each node compiles the source locally (ensuring correct local selector IDs) and installs it via the versioned `VTable.AddMethod`.

3. **Inline cache invalidation.** When a method is updated, all inline caches referencing the old method must be invalidated. The simplest approach: increment a global `cacheEpoch` counter on every method update. Each inline cache check compares its recorded epoch to the current epoch. On mismatch, the cache is cleared.

   This is a blunt instrument (invalidates all caches, not just the affected ones) but is simple and correct. Finer-grained invalidation (per-class or per-method) can be added later if profiling shows the global invalidation is too expensive.

4. **Code loading for remote nodes.** When node A loads new code, it pushes `CodeChange` messages to all connected nodes. Each node applies the change independently. This is eventually consistent -- there is a brief window where different nodes may have different versions. For a live-coding environment, this is acceptable.

**Demo moment:** A running web server with active connections. A developer changes a request handler method in the IDE. The change propagates to all cluster nodes within 1 second. Existing in-flight requests complete with the old handler. New requests use the new handler. No downtime, no dropped connections.

**Hard problems solved:** Method version coexistence. Inline cache invalidation. Cluster-wide code propagation. Determining when old versions are safe to collect.

**Explicitly deferred:** Full Erlang-style `code:purge` that forcibly kills processes running old code. Rolling upgrades with dependency ordering.

**Prerequisites:** Phase 3 (node connectivity). Phase 4 (supervision trees -- needed to restart processes if old code must be purged).

---

### Phase 6: Cluster Operations

**What this phase does:** Dynamic cluster membership, node discovery, process migration, and the operational tooling needed to run a Maggie cluster in production.

**Duration:** 8-12 weeks

**Concrete work:**

1. **Node discovery with memberlist.** This is where HashiCorp memberlist becomes useful. Nodes join a gossip mesh by contacting any existing member. When a new node joins, it receives the cluster's class table (source text, not bytecode) and bootstraps its VM.

2. **Process migration.** A process on node A can be migrated to node B by:
   a. Suspending the process (set state to `ProcessSuspended`)
   b. Serializing its state: source text of the method being executed, argument values, and any object graph reachable from the process's stack
   c. Spawning a new process on node B that re-executes the method from the beginning with the same arguments
   d. Killing the original process on node A

   Note: this is *re-execution*, not *state transfer*. Mid-execution migration (transferring the exact bytecode PC and stack) would require serializing interpreter frames, which includes HomeFrame indices and cell pointers -- machine-local state that cannot be transferred. Erlang does not support mid-execution migration either; `erlang:hibernate/3` restarts from a clean function call.

3. **Process placement.** A scheduler that decides where to spawn new processes based on load. Simple strategies first: round-robin, then least-loaded (based on goroutine count), then latency-aware (using memberlist's Vivaldi coordinates).

4. **Cluster-wide process registry.** A `Registry` that maps names to process references across the cluster:

   ```smalltalk
   Registry register: #myServer as: proc.
   server := Registry lookup: #myServer.
   ```

   Implementation: a replicated in-memory map. Updates are gossiped via memberlist's delegate mechanism. Conflict resolution: last-writer-wins with Lamport timestamps.

5. **Operational tooling.** `mag cluster` subcommand:
   ```bash
   mag cluster status          # Show all nodes, process counts, load
   mag cluster nodes           # List nodes with addresses
   mag cluster processes       # List processes across cluster
   mag cluster migrate PID NODE  # Migrate a process
   mag cluster eval EXPR        # Evaluate on a specific node
   ```

**Demo moment:** A three-node cluster running a chat server. Kill one node. The supervisor restarts the chat server's processes on surviving nodes. Add a new node. Processes are rebalanced. All of this happens without any client disconnections (clients reconnect to surviving nodes via their proxy references).

**Hard problems solved:** Node join/leave protocol. Process placement. State transfer semantics (re-execution vs. state serialization). Cluster-wide naming.

**Explicitly deferred:** Full ocap enforcement. Geographic distribution (multi-datacenter). Persistent storage.

**Prerequisites:** Phase 3 (node connectivity). Phase 4 (supervision). Phase 5 (code loading -- new nodes need to bootstrap their class tables).

---

### Phase 7: Object Capability Enforcement

**What this phase does:** Lock down the local VM so that all authority flows through explicit capability references. After this phase, Maggie is safe for running untrusted code in a distributed cluster.

**Duration:** 12-16 weeks (this is the largest single phase)

**Concrete work:**

1. **Remove ambient authority from `VM.Globals`.** Currently, any code can access `Compiler`, `Channel`, `Process`, and every class by name through the globals map. In a capability-safe system, these must be explicitly provided.

   The migration path:
   - Add a `PowerBox` object that provides access to system capabilities (file I/O, network, process spawning, class reflection).
   - New code receives capabilities through constructor arguments or method parameters, not through globals.
   - Existing code continues to work via a compatibility shim that provides a default PowerBox wired to the globals.
   - The default image boots with full authority; sandboxed images boot with restricted PowerBoxes.

2. **Mirror-based reflection.** Replace direct class access (`MyClass allMethodNames`, `MyClass superclass`) with mirrors:

   New files: `vm/mirror.go`, `lib/Mirror.mag`.

   ```smalltalk
   mirror := Mirror on: someObject.
   mirror classMirror name.           "class name"
   mirror classMirror methodNamed: 'foo:'.  "method mirror"
   mirror instVarNamed: 'x'.         "instance variable value"
   ```

   The existing `class_reflection_primitives.go` methods are deprecated but remain available through a `FullAccessMirror` (the capability-unrestricted variant).

3. **Restrict `Compiler evaluate:`.** In a capability-safe VM, `Compiler evaluate:` is a massive authority hole -- it can execute arbitrary code with all ambient authority. Options:
   - Remove it entirely from sandboxed images.
   - Scope it to a capability: `compiler evaluate: expr withPowerBox: restricted`.
   - Keep it in development/REPL mode, remove it in production mode.

   Recommendation: keep `evaluate:` available in development mode (the PowerBox includes a compiler capability). In production mode, the compiler capability is not provided.

4. **Confinement checking.** Verify that a sandboxed process cannot escalate its authority. This requires taint tracking through the object graph: if a process does not hold a reference to the `FileSystem` capability, it should not be able to obtain one through any chain of message sends.

   Full confinement checking is a research problem (Mark Miller's thesis discusses decidability limits). A practical approximation: the PowerBox controls what classes are available, and class-side methods that create capabilities (like `File open:`) check that the caller holds the appropriate capability.

5. **Membrane pattern for distributed capabilities.** When a capability is sent to a remote node, it is wrapped in a membrane that attenuates its authority. For example, a read-only file capability cannot be upgraded to read-write by the remote node.

**Demo moment:** A sandboxed process running untrusted code. The code tries `Compiler evaluate: 'File open: "/etc/passwd"'`. This fails because the sandbox's PowerBox does not include the compiler capability or the file system capability. The code can only access objects explicitly passed to it.

**Hard problems solved:** Eliminating ambient authority without breaking existing programs. Mirror-based reflection that preserves encapsulation. PowerBox design. Confinement verification (approximate).

**Explicitly deferred:** Formal verification of capability safety. Hardware-enforced isolation. Multi-tenant hosting.

**Prerequisites:** All previous phases. Ocap enforcement is the capstone.

---

## Timeline Reality

| Phase | Duration | Cumulative | Can start after |
|-------|----------|-----------|-----------------|
| Phase 0: Foundation Cleanup | 4-6 weeks | 4-6 weeks | Now |
| Phase 1: Wire Protocol | 4-6 weeks | 8-12 weeks | Phase 0 |
| Phase 2: Promises and Process Model | 6-8 weeks | 14-20 weeks | Phase 0 (partially overlaps Phase 1) |
| Phase 3: Node-to-Node Communication | 8-12 weeks | 22-32 weeks | Phase 1 + Phase 2 |
| Phase 4: Supervision Trees | 6-8 weeks | 28-40 weeks | Phase 3 |
| Phase 5: Hot Code Loading | 8-12 weeks | 36-52 weeks | Phase 3 |
| Phase 6: Cluster Operations | 8-12 weeks | 44-64 weeks | Phase 4 + Phase 5 |
| Phase 7: Object Capabilities | 12-16 weeks | 56-80 weeks | Phase 6 |

**Total: 14-20 months for a single focused developer. 2-3 years accounting for real-world interruptions, design iteration, and the inevitable discoveries that change the plan.**

Phases 4 and 5 can overlap (supervision and code loading are independent concerns). Phase 7 can start partially after Phase 3 (the remote reference capability model should be designed early).

### What determines the pace

1. **Phase 0 difficulty.** The registry unification is mechanical but touches every primitive registration file. If the dual-registry state is worse than it appears (subtle code paths that depend on global state), this phase will take longer.

2. **Phase 2 complexity.** Promises with proper rejection propagation and process links are subtle to get right. The interaction between Go's panic/recover (used for both exceptions and NLR) and process failure notification is tricky. Budget extra time here.

3. **Phase 3 is the critical path.** This is where the system becomes distributed for the first time. Every assumption about value serialization, process identity, and failure handling will be tested. Expect significant design iteration.

4. **Phase 7 is a research problem.** Full object capability enforcement in a dynamic Smalltalk is an open research area. Newspeak and E have done it, but Maggie's combination of Go-backed primitives, live coding, and image persistence adds constraints that neither Newspeak nor E had. Budget generously.

### The smallest useful increments

Each phase delivers something that justifies its existence independently:

- **Phase 0:** Two VMs in one process. Enables parallel test suites.
- **Phase 1:** CBOR codec for Maggie values. Enables saving objects to files, sending to REST APIs.
- **Phase 2:** Promises and monitors. Immediately useful for local concurrent programming.
- **Phase 3:** Two-node communication. Enables client-server Maggie applications.
- **Phase 4:** Supervision trees. Enables resilient local services even without distribution.
- **Phase 5:** Hot code loading. Enables live development of production services.
- **Phase 6:** Cluster operations. Enables multi-node deployments.
- **Phase 7:** Capability security. Enables running untrusted code.

You can stop after any phase and have something useful. The full vision is Phase 7, but Phase 3 alone is enough for many real applications.

---

## Summary of Differences From the Brainstorm

| Topic | Brainstorm says | This roadmap says |
|-------|-----------------|-------------------|
| Technology stack | 7 dependencies | Start with 2 (CBOR + existing gRPC). Add memberlist in Phase 6. |
| Ergo Framework | Core dependency | Do not use. Extend Maggie's own process model. |
| NATS | Core messaging | Defer until Phase 6 (if ever). gRPC streams are sufficient. |
| Dragonboat/Raft | Per-namespace consensus | Probably never. Use gossip + LWW for eventual consistency. |
| Block serialization | Not discussed | Cannot be done. Remote spawning uses source text. |
| VMWorker = vat | Stated as fact | Overstated. VMWorker serializes access but does not isolate state. |
| Channels copy values | Stated as fact | Wrong. Channels pass Go pointers. Wire bridges must deep-copy. |
| Opcode count | 97 | 55 |
| Phase ordering | Wire protocol -> remote spawning -> cluster -> promises | Phase 0 (cleanup) -> wire protocol -> promises -> remote spawning |
| Ocap timing | Deferred entirely | Remote reference capabilities from Phase 1. Local enforcement deferred. |
| Promise support | Phase 4 | Phase 2. Required before any remote work. |
| Mirrors | Phase 4 | Phase 7 (bundled with full ocap). Use existing reflection until then. |
