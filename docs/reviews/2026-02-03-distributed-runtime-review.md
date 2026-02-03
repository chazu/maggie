# Critical Review: Distributed Runtime Brainstorm

**Reviewer:** Code review agent (compiler/runtime + distributed systems perspective)
**Document under review:** `docs/brainstorms/2026-02-02-distributed-runtime-brainstorm.md`
**Date:** 2026-02-03

---

## Summary Assessment

The brainstorm is a well-researched survey of prior art and a credible first pass at identifying architectural gaps. The historical context section is strong, the technology recommendations are defensible, and the two-path conclusion (shared-nothing vs. full distributed runtime) is sound. However, the document has several significant omissions, underestimates certain difficulties, and contains some inaccurate characterizations of the current codebase. This review identifies specific issues organized by category.

---

## 1. Omissions: Important Topics Not Addressed

### 1.1 Block/Closure Serialization Is the Hardest Unsolved Problem

The document discusses object serialization and method transmission at length, but never directly confronts the problem of serializing closures (blocks). In Maggie, blocks are the fundamental unit of computation -- every `fork`, every `value:`, every control structure uses them. A `BlockValue` (defined in `/Users/chazu/dev/go/maggie/vm/interpreter.go`, line 1508) captures:

- A `*BlockMethod` (bytecode, literals, arity)
- A `[]Value` of captured variables (which may include cells -- mutable capture boxes)
- A `HomeFrame` index (an integer pointing into a specific interpreter's frame stack)
- A `HomeSelf` (a `Value`, potentially an object pointer)
- A `*CompiledMethod` for the enclosing method

`HomeFrame` is an index into a particular `Interpreter.frames` slice. It is meaningless on another machine, or even in another goroutine's interpreter on the same machine. Any distributed operation that involves sending a block across the wire -- which includes `forkOn:` and every callback-based API -- must solve this problem.

The document's Phase 2 proposes `forkOn:` but does not mention that the block passed to `forkOn:` contains a `HomeFrame` that cannot be resolved on the remote node. The "Promises can be built on Channels" insight on line 303 glosses over this by showing `[ch send: (obj foo: bar)] fork`, where the block's captures are simple values. But real-world blocks capture mutable cells (`OpMakeCell`/`OpCellGet`/`OpCellSet` in `/Users/chazu/dev/go/maggie/vm/bytecode.go`), and those cells are Go pointers into heap memory.

This is not merely an implementation detail -- it shapes the entire API design. If blocks cannot be serialized, then `forkOn:` must accept source text (like `Compiler evaluate:`) rather than block closures, which fundamentally changes the programming model.

### 1.2 The String and Dictionary Encoding Problem

The document's wire protocol section maps NaN-boxed value types to CBOR tags but omits strings and dictionaries entirely. In the current implementation, strings and dictionaries are *not* heap objects -- they are encoded in the symbol ID space using special ranges:

- Strings: IDs in range `[0x80000000, 0xC0000000)` (see `stringIDOffset` in `/Users/chazu/dev/go/maggie/vm/string_primitives.go`, line 35)
- Dictionaries: IDs >= `0xC0000000` (see `dictionaryIDOffset` in `/Users/chazu/dev/go/maggie/vm/dictionary_primitives.go`, line 30)

These are resolved through *global package-level registries* (the `stringRegistry` and `dictionaryRegistry` maps). The wire protocol table on line 496 has no entries for String or Dictionary values. Since strings are among the most commonly transmitted values, this is a significant omission that would block any wire protocol implementation.

### 1.3 No Discussion of Interpreter-Per-Goroutine Architecture

The document mentions that "Interpreters are already parallelizable" (line 270) but does not discuss the implications of the interpreter-per-goroutine model for distribution. Each `fork` creates a new `Interpreter` via `vm.newInterpreter()` and registers it per-goroutine in `vm.interpreters` (a `sync.Map` of goroutine ID to `*Interpreter`). These interpreters share the *same* `VM.Globals` map (see `/Users/chazu/dev/go/maggie/vm/interpreter.go`, line 75 and `/Users/chazu/dev/go/maggie/vm/vm.go`, line 147 where `interp.Globals = vm.Globals`).

This means:
- Multiple interpreters race on `VM.Globals` reads/writes (it is a plain `map[string]Value` with no locking)
- A forked process can observe mid-mutation state of globals set by another process
- The `Compiler evaluate: 'x := 42'` mechanism described in `CLAUDE.md` uses these shared globals

The brainstorm's assertion that "processes share class/symbol tables" (line 283) is correct but incomplete -- they also share the globals map, the class variable storage (`classVarStorage` in `/Users/chazu/dev/go/maggie/vm/class.go`, line 70), and several other mutable structures. This shared mutable state is a deeper problem than the document acknowledges, because it means even local concurrency is not truly isolated today.

### 1.4 No Treatment of the `become:` Mechanism

The `Object.Become` and `Object.BecomeForward` methods (`/Users/chazu/dev/go/maggie/vm/object.go`, lines 324-357) allow objects to swap identities or set up forwarding pointers. The brainstorm proposes proxy objects via `doesNotUnderstand:`, but `become:` is the classic Smalltalk mechanism for implementing proxies: create a lightweight proxy, then when the real object arrives, `become:` the proxy into the real object so all existing references seamlessly point to the real data.

This is directly relevant to distributed object loading (lazy proxies that become real objects when accessed) and to object migration (Emerald-style). The forwarding pointer mechanism in `Object.forward` is already half of a transparent proxy system. The document should either adopt this as part of the remote reference strategy or explicitly reject it with rationale.

### 1.5 No Discussion of the Trait System

Traits (`/Users/chazu/dev/go/maggie/vm/trait.go`) are a key part of Maggie's composition model. They hold `map[int]*CompiledMethod` keyed by selector IDs. Since selector IDs are VM-local (as the document correctly identifies), trait definitions cannot be transmitted between VMs without the same selector rewriting that methods need. But traits also have `Requires` (required method selector IDs) that must be renegotiated. The trait system adds another dimension to the class-distribution problem that the document does not mention.

### 1.6 No Discussion of Inline Caches

The inline cache system (`/Users/chazu/dev/go/maggie/vm/inline_cache.go`) caches `(*Class, Method)` pairs per call site. If classes or methods change on a remote node (live code loading, Phase 3), inline caches on other nodes become stale. The cache invalidation protocol for a distributed system is non-trivial and is not mentioned. Erlang solves this by having immutable modules with atomic code replacement and a "purge old version" protocol -- Maggie's mutable-in-place VTables are fundamentally different.

### 1.7 The AOT/JIT System Is Not What the Document Implies

The document's Open Question 6 asks about sharing JIT-compiled methods across nodes. But Maggie's "JIT" (`/Users/chazu/dev/go/maggie/vm/jit.go`) is actually an AOT-to-Go-source transpiler, not a true JIT. It generates Go source code strings (see `hotMethods map[string]string` on line 28 of `jit.go`) and writes them to files. These are not dynamically loaded -- they require Go compilation and linking. This makes the "share compiled code across nodes" question moot, since what is being "compiled" is Go source text that would need to be separately compiled on each architecture. The document should be precise about this.

---

## 2. Gaps in Analysis

### 2.1 VMWorker-as-Vat Is a Shallow Analogy

The document claims "VMWorker IS a vat" (line 261). This is partially true but overstated. A vat in E provides:

1. Single-threaded event loop -- VMWorker provides this.
2. Turn-based execution (each message fully completes before the next begins) -- VMWorker provides this.
3. Separate object heap -- VMWorker does NOT provide this. The VMWorker holds a reference to the shared `*vm.VM`, which in turn holds shared `Globals`, `Classes`, `Symbols`, `Selectors`, and all the global registries.
4. Eventual-send-only boundary -- VMWorker does NOT provide this. The `Do()` method blocks the caller synchronously.

The VMWorker is a serialization adapter for gRPC handlers, not a vat. Calling it a vat overstates the isolation it provides. The document should distinguish between "serialized access to shared state" (what VMWorker is) and "isolated computation unit with message-passing boundary" (what a vat is).

### 2.2 The "97 opcodes" Claim

The document states "Portable bytecode (97 opcodes, no platform-specific instructions)" on line 18. Counting the opcodes defined in `/Users/chazu/dev/go/maggie/vm/bytecode.go`, I count approximately 55 distinct opcodes (including the optimized sends). The "97" figure appears to be incorrect. This is a minor factual error, but it undermines confidence in other technical claims.

### 2.3 The Blocking-Operations Problem Is Understated

The document identifies that blocking `Channel.receive` in a VMWorker context blocks all other requests (line 451). But the analysis stops there. The problem is deeper:

- `Mutex.lock` blocks the goroutine (`/Users/chazu/dev/go/maggie/vm/mutex.go`)
- `WaitGroup.wait` blocks the goroutine
- `Semaphore.acquire` blocks the goroutine
- `Process.wait` blocks the goroutine (`ProcessObject.wait()` calls `p.waitGroup.Wait()`)
- `CancellationContext.wait` blocks the goroutine

Any of these operations, if executed in a process that happens to be the VMWorker's dedicated goroutine, will deadlock the entire server. But even outside the VMWorker, a remote `forkOn:` that sends a block containing `mutex lock` will block the remote interpreter's goroutine, potentially starving the remote node.

The document should analyze which blocking operations are safe for distributed use and which need async alternatives. Erlang avoids this entirely by making all inter-process communication asynchronous (mailbox-based, never blocking the scheduler loop).

### 2.4 Symbol Table Negotiation Is Simpler Than Presented

The document frames symbol/selector synchronization as a major problem (lines 59, 434, 445) and proposes a negotiation protocol. But the recommended Path A (shared-nothing) already solves this trivially: messages carry symbol/selector names as strings, and each VM resolves them locally. The `SelectorTable.Intern()` method (`/Users/chazu/dev/go/maggie/vm/selector.go`, line 29) is already thread-safe and creates-on-demand. There is no negotiation needed for Path A.

The complexity only arises if you try to transmit raw bytecode between VMs (which encodes selector IDs as uint16 operands in `OpSend`). But Path A explicitly avoids this by transmitting source or structured representations. The document conflates Path A and Path B concerns here.

### 2.5 CBOR Tag Space Is Not Specified Precisely Enough

The wire protocol proposes CBOR tags in the 70000 range (lines 505-510) but does not address IANA tag allocation. CBOR tags 0-23 are well-known, 24-32767 require specification review, and tags above 32767 are in the "first come first served" range. The 70000 range is in the FCFS space, which is fine, but the document should note that these tags should be formally registered to avoid collisions with other CBOR users. This is especially important if Maggie objects will ever be stored in CBOR-based databases or transmitted through CBOR-aware middleware.

---

## 3. Incorrect Assumptions

### 3.1 "Channels pass values by copy (not shared reference)" Is Wrong

Line 272 states: "Maggie's channels already decouple sender/receiver, support select-based multiplexing, and pass values by copy (not shared reference)."

This is incorrect. Maggie channels pass `Value` directly -- `ch.ch <- val` (see `/Users/chazu/dev/go/maggie/vm/concurrency.go`, line 216). A `Value` is a `uint64`, and when it contains an object pointer (`tagObject`), the pointer is the actual Go memory address of an `*Object` on the heap. The receiver gets the *same pointer*, not a copy. There is no deep-copy semantics on channel send.

This matters enormously for distribution. If you bridge a channel across the network, you must serialize the value (which the document acknowledges elsewhere), but the current channel semantics are shared-reference, not copy. The document should not claim otherwise, because it affects whether channels can be transparently bridged or require semantic changes.

### 3.2 "ObjectHandle already decouples protocol from internal representation"

The document claims the `ObjectHandle` system (line 76) provides a foundation for remote references. But examining `/Users/chazu/dev/go/maggie/server/handles.go`, the `HandleStore` stores `vm.Value` directly (line 15 of `handle` struct). The `Create` method pins objects via `vm.KeepAlive` and the `Lookup` method returns the raw `vm.Value`.

This is a GC-pinning mechanism for the gRPC server, not a decoupling layer. The handle IDs (`"h-1"`, `"h-2"`, etc.) are local to a single `HandleStore` instance and a single session. They cannot be sent to another VM because the other VM has no way to resolve `"h-42"` back to an object. The `handle` struct even stores the `sessionID` it belongs to (line 18).

For remote references, you would need globally unique handle IDs, a distributed lookup protocol, and lease management. The current `HandleStore` is a useful local primitive, but calling it a "foundation for remote references" overstates what it provides.

### 3.3 The 10+ Global Registries Count Is Incomplete

The document states "10+ global registries" (line 437). A grep for `var ... = make(map` in the vm package reveals at least 11 package-level global maps:

1. `channelRegistry` -- `/Users/chazu/dev/go/maggie/vm/concurrency.go:22`
2. `processRegistry` -- `/Users/chazu/dev/go/maggie/vm/concurrency.go:102`
3. `mutexRegistry` -- `/Users/chazu/dev/go/maggie/vm/mutex.go:20`
4. `waitGroupRegistry` -- `/Users/chazu/dev/go/maggie/vm/waitgroup.go:20`
5. `semaphoreRegistry` -- `/Users/chazu/dev/go/maggie/vm/semaphore.go:21`
6. `exceptionRegistry` -- `/Users/chazu/dev/go/maggie/vm/exception.go:36`
7. `blockRegistry` -- `/Users/chazu/dev/go/maggie/vm/interpreter.go:1532`
8. `blocksByHomeFrame` -- `/Users/chazu/dev/go/maggie/vm/interpreter.go:1538`
9. `cellRegistry` -- `/Users/chazu/dev/go/maggie/vm/value.go:304`
10. `classVarStorage` -- `/Users/chazu/dev/go/maggie/vm/class.go:70`
11. `resultRegistry` -- `/Users/chazu/dev/go/maggie/vm/result.go:30`
12. `classValueRegistry` -- `/Users/chazu/dev/go/maggie/vm/class_value.go:23`

Plus atomic counters: `nextChannelID`, `nextProcessID`, `nextBlockID`, `nextExceptionID`, `weakRefCounter`, `nextClassValueID`, etc.

The document mentions this problem but understates its severity. These are not just "legacy" globals that can be easily migrated to `ConcurrencyRegistry`. The `ConcurrencyRegistry` (in `/Users/chazu/dev/go/maggie/vm/concurrency_registry.go`) duplicates several of these registries as instance fields, meaning there are *two* parallel registries for channels, processes, etc. -- the global ones and the VM-local ones. Code paths differ on which one they use. Some primitives (registered in `registerChannelPrimitives`) call `v.registerChannel(ch)` which uses the VM-local registry, while the global `registerChannel(ch)` function writes to the package-level global. This dual-registry architecture would need to be unified before any distributed work can proceed.

---

## 4. Underestimated Difficulties

### 4.1 Bytecode Rewriting for Cross-VM Method Transfer

The document notes that "selector references in bytecode must be rewritten" (line 573) for cross-VM method transfer, but treats it as a straightforward mapping exercise. It is considerably harder than that:

- `OpSend`, `OpSendSuper`, and `OpTailSend` each encode a uint16 selector ID as two bytes in the bytecode stream (see `/Users/chazu/dev/go/maggie/vm/bytecode.go`, lines 57-59, with 3-byte operands: 2 for selector + 1 for argc)
- `OpPushGlobal` and `OpStoreGlobal` encode 16-bit indices into the method's literal table, where the literal is a symbol. The literal table itself contains `Value` entries that are NaN-boxed symbol IDs, which must also be rewritten.
- `OpPushClassVar` and `OpStoreClassVar` encode 16-bit symbol IDs for class variable names.
- `OpCreateBlock` references a block method index that points into the method's `Blocks` slice, and each block has its own literal table with symbol references.
- Inline caches (`InlineCacheTable`) are keyed by bytecode PC offset. If bytecode offsets change during rewriting (which they would not if you keep the same opcode sizes, but they could if the rewriting changes literal table indices), all caches become invalid.

A correct bytecode rewriter must walk the entire instruction stream, identify every opcode that references a selector or symbol, map through the name-based translation table, and rewrite the literal table. This is essentially a binary-to-binary compiler pass. It is doable but non-trivial, and the document should size it accordingly.

### 4.2 Class Identity Across VMs

The document discusses class mutation propagation (line 453) but not the more fundamental problem of class *identity*. Consider:

```smalltalk
[Point x: 3 y: 4] forkOn: nodeB.
```

Node B must have a `Point` class with the same structure (same instance variables in the same order, same superclass chain). But Maggie identifies classes by Go pointer (`*Class`). Two independently loaded `Point` classes are different objects even if structurally identical. The `VTable.class` pointer, the `Object.vtable` pointer, and all class-side method dispatch depend on pointer identity.

The document proposes a "Distributed class table (NATS JetStream KV)" in Phase 3, but does not address how class identity is established. Is it by name? By name + namespace? What about anonymous classes? What about metaclasses (every class in Maggie has a `ClassVTable` for class-side methods)? GemStone/S solves this with persistent OIDs; Erlang solves it by not having classes. Maggie would need a new concept of "class identity" that survives serialization.

### 4.3 The Promise-as-Channel Model Breaks Under Failure

The document proposes implementing promises via channels (line 303):

```smalltalk
ch := Channel new.
[ch send: (obj foo: bar)] fork.
^ch  "Return the channel as a promise"
```

This breaks when the forked process crashes. If `obj foo: bar` raises an unhandled exception, the goroutine panics, the `defer` in `fork` (see `/Users/chazu/dev/go/maggie/vm/concurrency.go`, line 447) catches the panic and calls `proc.markDone(Nil, nil)`, but *the channel never receives a value*. Any process waiting on `ch receive` blocks forever.

A real promise system needs rejection semantics: if the computation fails, the promise is rejected, and all downstream promises are also rejected. Erlang's monitors and links provide this; E's promises have explicit broken-promise propagation. Building this on top of bare channels requires significant additional infrastructure that the document does not account for.

### 4.4 Non-Local Returns Cannot Cross VM Boundaries

Maggie's block `^value` syntax causes a `NonLocalReturn` panic (see `/Users/chazu/dev/go/maggie/vm/interpreter.go`, line 1524-1529) that unwinds to the `HomeFrame`. The `fork` mechanism already handles this by using `ExecuteBlockDetached` which catches NLRs. But the deeper issue is that non-local returns are a semantic primitive of the language that fundamentally cannot work across VM boundaries.

If a block is sent to a remote VM via `forkOn:`, and that block contains `^value`, the NLR would need to unwind the *sender's* call stack on a different machine. This is analogous to a distributed exception that spans machines. The document does not address this at all. The only viable answer is that remote blocks always use detached semantics (NLRs become local returns), which means `forkOn:` has subtly different semantics from local `fork` -- a violation of the "channels and processes work across nodes without code changes" goal stated on line 86.

---

## 5. Overestimated Difficulties

### 5.1 Symbol/Selector Table Sync (for Path A) Is Easy

The document rates "Symbol/Selector IDs" as RED for distribution (line 434). For Path A (shared-nothing), this is overstated. Each VM independently interns selectors. Messages on the wire carry selector names as strings. The receiver calls `SelectorTable.Intern(name)` to get a local ID. This is O(1) amortized (hash map lookup with double-checked locking, already implemented thread-safely). There is no synchronization protocol needed.

The only cost is transmitting selector names as strings instead of integers. For typical messages with 1-2 selectors of 5-15 characters each, this adds 10-30 bytes per message. The CBOR encoding already handles variable-length strings efficiently. This is a non-issue for Path A.

### 5.2 The Global Registries Problem Is Solvable Incrementally

The document presents the 10+ global registries as a deep structural problem (line 449). But the `ConcurrencyRegistry` already demonstrates the solution pattern: wrap the global map in a struct, put it on the VM, and use method-based access. The migration from global to VM-local has already been partially completed for channels, processes, mutexes, waitgroups, semaphores, cancellation contexts, and blocks.

The remaining globals (`blockRegistry`, `blocksByHomeFrame`, `cellRegistry`, `classVarStorage`, `exceptionRegistry`, `resultRegistry`, `classValueRegistry`) follow the same pattern and could be migrated mechanically. This is tedious but straightforward refactoring work, not a research problem.

### 5.3 Image Format Limitations

The document rates the image format as YELLOW (line 437), noting it is "all-or-nothing, pointer-based, but structurally sound." In practice, for Path A, the image format is irrelevant -- each VM loads its own image independently. The image format only becomes relevant for Path B (distributed snapshots), which the document correctly defers. For Path A, this is a GREEN.

---

## 6. Missing Alternatives

### 6.1 Orleans/Virtual Actor Model

The document considers Erlang (actor), GemStone (shared space), and Croquet (replication) but omits the Virtual Actor model pioneered by Microsoft Orleans. In the virtual actor model, actors are always conceptually present -- the runtime transparently activates/deactivates them as needed. This maps well to Maggie's object model: objects are always "there" (you send them messages), the runtime handles placement and lifecycle. Orleans has been proven at massive scale (Halo game services) and has a simpler programming model than raw Erlang. The `ergo-services/ergo` framework the document recommends actually supports a similar abstraction.

### 6.2 Unison-Style Content-Addressed Code

The document discusses code loading across clusters (Open Question 4) but does not consider content-addressed code storage, as pioneered by Unison. If methods were identified by a hash of their source/bytecode rather than by (class, selector) pair, code distribution would become trivial: any node can fetch any method by hash from any other node, with automatic deduplication and cache-friendliness. This would also solve the class-versioning problem: two versions of a method have different hashes and can coexist.

### 6.3 CRDTs for Class Mutation

The document identifies class mutation as incompatible with replication (line 453) but only considers consensus (Raft) as a solution. An alternative is to model class definitions as CRDTs (Conflict-free Replicated Data Types). Method additions are commutative (adding method A and method B in either order gives the same result). Method replacements could use last-writer-wins semantics with Lamport timestamps. This would give eventual consistency for class definitions without the latency penalty of consensus.

### 6.4 WebAssembly as a Portable Bytecode Target

Instead of transmitting Maggie bytecode (which has VM-local selector IDs baked in), the document could consider compiling to WebAssembly for cross-node method transfer. WASM is designed for portable, sandboxed execution and has mature tooling. This would sidestep the bytecode-rewriting problem entirely, though at the cost of a more complex compilation pipeline.

---

## 7. Ordering/Dependency Issues

### 7.1 Phase 1 Depends on Unifying the Dual Registry Architecture

Phase 1 proposes "Remote reference type (new value tag in NaN-boxing)" and "Proxy objects using `doesNotUnderstand:`". But adding a new symbol marker tag for remote references requires modifying `SymbolDispatch` (`/Users/chazu/dev/go/maggie/vm/symbol_dispatch.go`) and ensuring the new marker does not collide with the existing ones:

- `1 << 24` = channels
- `2 << 24` = processes
- `4 << 24` = results
- `8 << 24` = exceptions
- `16 << 24` = weak references
- `36 << 24` = class values
- Plus string range (`0x80000000+`) and dictionary range (`0xC0000000+`)

The marker byte space is only 8 bits (values 0-255), but the encoding uses the full `0xFF << 24` mask. Several markers are already allocated. Before adding remote reference markers, the existing marker allocation scheme should be formalized and documented. This is prerequisite work that the phasing does not account for.

Additionally, the dual-registry problem (global registries vs. `ConcurrencyRegistry`) should be resolved before Phase 1, not during or after. Remote references will need to interact with the registry system, and having two parallel registries with different code paths is a recipe for subtle bugs.

### 7.2 Promise Pipelining (Phase 4) Should Come Before Remote Process Spawning (Phase 2)

The document places promise pipelining in Phase 4, after remote process spawning in Phase 2. But `forkOn:` inherently returns a future/promise (the remote process's result). Without a proper promise type with rejection semantics, failure handling for remote processes is undefined. Phase 2 needs at least a basic promise/future type to be usable, which means promise support should be moved to Phase 1 or early Phase 2.

### 7.3 Mirrors (Phase 4) Should Come Before Remote Reflection (Phase 3)

Phase 3 proposes "Distributed class table" and "Live code loading across cluster," both of which require remote introspection of classes and methods. Phase 4 proposes mirrors as the mechanism for remote reflection. This ordering is backwards: you need the abstraction layer (mirrors) before you build the distributed infrastructure (class tables, code loading) on top of it. Otherwise, Phase 3 will be built with direct class access, and Phase 4 will require rewriting Phase 3.

---

## 8. Conceptual Issues

### 8.1 The "Explicit Core, Transparent Sugar" Philosophy Is Internally Contradicted

The document states the design philosophy as "Explicit primitives that make distribution boundaries visible" (line 84). But then it says "Channels and processes that work across nodes without code changes" (line 85). These two principles are in tension. If channels transparently bridge the network, then distribution boundaries are hidden. If boundaries are explicit, then channels cannot work "without code changes."

Erlang resolves this tension by making *all* inter-process communication asynchronous and message-based, so there is no semantic difference between local and remote. Maggie's channels have blocking semantics (`ch receive` blocks the goroutine), which means local and remote channels have fundamentally different failure modes. The document should pick one side of this tension and be consistent.

### 8.2 The Ergo + NATS + Dragonboat Stack Is Over-Specified

The technology stack table on line 165 recommends 7 separate dependencies for different layers. This is an enormous surface area for a runtime that currently has zero distributed capability. Each dependency brings its own concurrency model, failure semantics, and operational complexity. Using Ergo (Erlang-style actors), NATS (pub/sub messaging), AND Dragonboat (Raft consensus) simultaneously creates a system with three overlapping communication models.

For Path A (shared-nothing microservices), a single technology -- either NATS or gRPC (which Maggie already has) -- is sufficient. Starting with the existing gRPC infrastructure, adding CBOR serialization for Maggie values, and building remote process spawning on top would be far more pragmatic than introducing 7 new dependencies.

### 8.3 The "Ocap-Ready but Deferred" Strategy Has a Consistency Problem

The document proposes building an ocap-compatible architecture now and enforcing it later (line 199). But the specific decision to use NATS subject-based addressing for message routing (line 170) is fundamentally incompatible with object capabilities. In an ocap system, you can only send a message to an object if you hold a reference (capability) to it. NATS subjects are ambient: any subscriber to a subject receives messages. This is the opposite of capability discipline.

If ocap enforcement is a real future goal, the transport layer must be capability-based from the start (like Cap'n Proto, which is mentioned in the references but not adopted). Retrofitting capability discipline onto a subject-based messaging system is architecturally much harder than building it into the transport from the beginning.

---

## 9. Constructive Recommendations

Given the issues identified above, here are concrete suggestions for improving the plan:

1. **Resolve the dual-registry architecture first.** Migrate all global registries to VM-local `ConcurrencyRegistry` (or a broader `ObjectRegistry`). This is the prerequisite for everything else and is achievable in a few focused sessions.

2. **Define the scope of "serializable computation."** Not all Maggie values can cross the wire (blocks with home frames, cells, forwarding pointers). Explicitly categorize values as serializable vs. non-serializable and design the API around these constraints.

3. **Start with gRPC, not NATS/Ergo/Dragonboat.** The existing server already speaks gRPC via Connect. Add CBOR-encoded Maggie value payloads to the existing service definitions. This gets wire-protocol work done without new dependencies.

4. **Implement promises before `forkOn:`.** A `Promise` class with resolve/reject semantics, built on `ResultObject` (which already exists in `/Users/chazu/dev/go/maggie/vm/result.go`), should come first. Then `forkOn:` returns a `Promise`.

5. **Accept that remote blocks have detached semantics.** Document clearly that `forkOn:` uses `ExecuteBlockDetached` (no non-local returns to the sender's frame). This is a reasonable limitation, not a deficiency.

6. **Add a formal marker allocation table.** The symbol-encoded type markers are scattered across files. Create a single authoritative table in a header comment or doc that lists all allocated markers, their ranges, and the types they encode.

---

## 10. Summary of Findings

| Category | Count | Severity |
|----------|-------|----------|
| Omissions | 7 | 3 critical, 4 significant |
| Gaps in analysis | 5 | 2 significant, 3 moderate |
| Incorrect assumptions | 3 | 1 critical, 2 moderate |
| Underestimated difficulties | 4 | 2 critical, 2 significant |
| Overestimated difficulties | 3 | all moderate |
| Missing alternatives | 4 | all informational |
| Ordering issues | 3 | 2 significant, 1 moderate |
| Conceptual issues | 3 | 1 critical, 2 significant |

The brainstorm is a strong starting point. The historical research is thorough, the technology selections are reasonable (if over-specified), and the two-path conclusion is correct. The main weakness is that the document does not engage deeply enough with the *specific* code paths and data structures in the current VM that would need to change. Several claims about what the VM "already provides" are either overstated or incorrect. The phasing assumes a cleaner starting point than actually exists.

The recommendation to start with Path A (shared-nothing) is sound. But even Path A requires resolving the dual-registry problem, defining serialization boundaries for blocks/closures/cells, and implementing basic promise semantics -- work that the current phasing either omits or misorients.
