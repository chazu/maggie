# Distributed Maggie: From Two-VM Demo to Full Distribution

**Date:** 2026-03-04
**Status:** Draft
**Scope:** Items 1-4 (demo prerequisites) + architectural vision for distributed programming

---

## Part 1: Four Items for the Two-VM Demo

### Item 1: Code Loading from Synced Source ("Rehydration")

This is the critical path. Today, `mag sync pull` receives method source text and content hashes via the `Serve` RPC and indexes them in the ContentStore as stub `CompiledMethod` objects (source + hash, no bytecode). The receiver has no way to compile those stubs into runnable code and plug them into the class hierarchy.

#### 1.1 The Problem in Detail

When chunks arrive via pull, we get:

- **Method chunks:** `Chunk{Type: ChunkMethod, Content: "method: hello [ ^'hello' ]", Hash: ...}`
- **Class chunks:** `Chunk{Type: ChunkClass, Content: "Greeter", Hash: ..., Dependencies: [methodHash1, methodHash2]}`

The class chunk's `Content` field currently carries only the class name (see `ClassToChunk` in `vm/dist/chunker.go:21` and every callsite passing `d.Name`). This is insufficient for reconstruction — we need superclass name, instance variables, namespace, class variables, and docstring.

#### 1.2 Enrich Class Chunk Content

**Change `ClassToChunk` to serialize full structural metadata** as the Content field. Use a simple deterministic text format (not CBOR — keep Content human-readable for debugging):

```
CLASS Greeter
NAMESPACE MyApp
SUPER Object
IVARS name greeting
CVARS defaultGreeting
DOC A simple greeter class.
```

This is a line-oriented format: tag at start of line, space, then value. Empty fields omit the line entirely. This keeps Content inspectable while carrying all structural data.

**Files to modify:**

- `vm/dist/chunker.go`: Change `ClassToChunk` signature to `ClassToChunk(d *vm.ClassDigest, caps []string)` (drop the `source` parameter — Content is now derived from the digest). Add `EncodeClassContent(d *ClassDigest) string` and `DecodeClassContent(content string) (*ClassDigest, error)` functions.
- `vm/dist/chunker_test.go`: Update test for new Content format.
- `cmd/mag/sync.go:132`: Update callsite — `dist.ClassToChunk(d, nil)` instead of `dist.ClassToChunk(d, d.Name, nil)`.
- `server/sync_service.go:253`: Same callsite update.
- `server/sync_integration_test.go:138`: Same callsite update.
- `server/sync_service.go:188` (`indexVerifiedClass`): Use `DecodeClassContent` to populate all `ClassDigest` fields, not just `Name`.

#### 1.3 The Rehydration Pipeline

Add a new function `RehydrateFromStore` to the pipeline package. This takes a ContentStore (populated from pull) and a VM, and compiles the received code into runnable classes.

**Data flow:**

```
ContentStore (methods + class digests)
  |
  v
Phase 1: Topological sort of class digests by superclass dependency
  |
  v
Phase 2: For each class (in dependency order):
  a. Create class skeleton (name, namespace, superclass, ivars)
  b. Register in VM ClassTable + Globals
  |
  v
Phase 3: For each class:
  For each method hash in the class digest:
    a. Look up method stub in ContentStore
    b. Parse source text via compiler.ParseSourceFileFromString (method body)
       or compile via CompileMethodDefWithContext
    c. Verify compiled hash matches stored hash
    d. Install method in class VTable
  |
  v
Phase 4: Index freshly compiled methods + class digests in ContentStore
  (replacing the stubs with real CompiledMethod objects)
```

**Key design decision: method source format.** Currently `MethodToChunk` stores `m.Source`, which is the raw method source text as written in the `.mag` file (e.g., `method: hello [ ^'hello' ]`). This is a complete method definition that can be parsed by the existing compiler.

However, methods are compiled in the context of a class (they need instance variable names for slot access, namespace for FQN resolution, imports for name resolution). The rehydration pipeline must reconstruct this context from the class digest before compiling each method.

**New file: `pipeline/rehydrate.go`**

```go
package pipeline

// RehydrateFromStore compiles all uncompiled content in the VM's
// ContentStore into runnable classes and methods.
//
// "Uncompiled" means ClassDigest entries whose classes don't exist in
// the VM's ClassTable, and method stubs with no bytecode.
func (p *Pipeline) RehydrateFromStore() (int, error) {
    store := p.VM.ContentStore()

    // 1. Collect class digests that need rehydration
    //    (class not yet in ClassTable, or class exists but methods missing)

    // 2. Topological sort by superclass dependency
    //    ClassDigest.SuperclassName -> find digest -> order
    //    Classes whose superclass is already in the VM sort first.

    // 3. Create class skeletons (pass 1a/1b equivalent)

    // 4. Compile methods (pass 2 equivalent)
    //    For each method stub: parse source, compile with class context,
    //    verify hash, install in VTable.

    // 5. Re-index in ContentStore (replace stubs with compiled methods)
}
```

**The method source parsing problem.** Method source text stored in `CompiledMethod.Source` is the raw MethodDef text (e.g., `method: hello [ ^'hello' ]`). To compile it, we need to:

1. Parse it as a MethodDef (not a full SourceFile). The compiler already has `compiler.ParseMethodDefSource(source string) (*MethodDef, error)` — if not, we add it. It's a thin wrapper around the parser.
2. Call `CompileMethodDefWithContext(methodDef, selectors, symbols, registry, instVars, namespace, imports, classTable)`.
3. The `namespace` and `imports` come from the ClassDigest. For rehydrated code, we use the class's namespace and an empty import list (since FQN resolution already happened at the original compile site — the source text should contain FQNs). Wait — no, the stored source is the *original* source text, which may contain bare class names. The FQN resolution happened during original compilation to produce opcodes, but the *source text* is pre-resolution.

**Resolution:** We need to either:
- **(A)** Store the post-FQN-resolution source, or
- **(B)** Store the namespace + imports alongside the method source so the receiver can re-resolve.

Option (B) is better because it preserves the original source for readability and `fileOut`. The method chunk needs to carry namespace/imports metadata. Add these as structured fields:

**Extend `Chunk` struct in `vm/dist/chunk.go`:**

```go
type Chunk struct {
    Hash         [32]byte   `cbor:"1,keyasint"`
    Type         ChunkType  `cbor:"2,keyasint"`
    Content      string     `cbor:"3,keyasint"`
    Dependencies [][32]byte `cbor:"4,keyasint,omitempty"`
    Capabilities []string   `cbor:"5,keyasint,omitempty"`
    Selector     string     `cbor:"6,keyasint,omitempty"` // method selector
    ClassName    string     `cbor:"7,keyasint,omitempty"` // owning class FQN
    IsClassSide  bool       `cbor:"8,keyasint,omitempty"` // class method vs instance
}
```

The `ClassName` field tells the receiver which class to install the method on. The receiver can derive namespace and imports from the class digest. The `Selector` field enables the receiver to install the method at the correct selector without re-parsing.

**Update `MethodToChunk` in `vm/dist/chunker.go`:**

```go
func MethodToChunk(m *vm.CompiledMethod, caps []string) *Chunk {
    h := m.GetContentHash()
    c := &Chunk{
        Hash:         h,
        Type:         ChunkMethod,
        Content:      m.Source,
        Capabilities: caps,
        Selector:     m.Name(),
    }
    if cls := m.Class(); cls != nil {
        c.ClassName = cls.FullName()
        c.IsClassSide = m.IsClassMethod
    }
    return c
}
```

**Update `indexVerifiedMethod` in `server/sync_service.go`:**

```go
func (s *SyncService) indexVerifiedMethod(chunk *dist.Chunk) {
    m := &vm.CompiledMethod{Source: chunk.Content}
    m.SetContentHash(chunk.Hash)
    // Preserve metadata for rehydration
    // (Selector and ClassName are available on the chunk but we store
    //  them as simple fields on the stub method)
    s.store.IndexMethod(m)
}
```

Actually, the ContentStore indexes by hash. When rehydrating, we iterate class digests and look up methods by hash. The class digest tells us which class each method belongs to. So we do not strictly need `ClassName` on the chunk — we can derive it from which ClassDigest references which method hash. But `Selector` is needed because the source text parse might not be reliable (primitive stubs, etc.). Let's keep both for robustness.

**Compile function for rehydration.** We need `compiler.ParseMethodDefSource(source string) (*compiler.MethodDef, error)`. Check if this exists:

The compiler has `ParseSourceFileFromString` which parses a full `.mag` file. For a single method definition, we need a parser entry point that handles just the method syntax. If this doesn't exist, we add a thin wrapper:

```go
// compiler/parser.go (or new file compiler/parse_method.go)
func ParseMethodDef(source string) (*MethodDef, error) {
    // Wrap in a dummy class to reuse the existing parser
    wrapped := "Dummy subclass: Object\n" + source
    sf, err := ParseSourceFileFromString(wrapped)
    if err != nil {
        return nil, err
    }
    if len(sf.Classes) == 0 || len(sf.Classes[0].Methods) == 0 {
        return nil, fmt.Errorf("no method found in source")
    }
    return sf.Classes[0].Methods[0], nil
}
```

This is a pragmatic approach. A cleaner one would be to expose the parser's method-parsing entry point directly, but wrapping is simpler and the dummy class is discarded.

**Files to create:**
- `pipeline/rehydrate.go`: `RehydrateFromStore` function
- `pipeline/rehydrate_test.go`: Unit tests

**Files to modify:**
- `vm/dist/chunk.go`: Add `Selector`, `ClassName`, `IsClassSide` fields
- `vm/dist/chunker.go`: Update `MethodToChunk`, `ClassToChunk`; add `EncodeClassContent`/`DecodeClassContent`
- `vm/dist/wire.go`: No changes needed (CBOR auto-handles new fields via tags)
- `compiler/codegen.go` or new `compiler/parse_method.go`: Add `ParseMethodDef` if needed
- `cmd/mag/sync.go`: After pull completes, call `pipeline.RehydrateFromStore`
- `server/sync_service.go`: Update `indexVerifiedMethod`, `indexVerifiedClass` to use richer chunk data

#### 1.4 Integration with `mag sync pull`

After the pull receives and indexes all chunks, add the rehydration step:

```go
// cmd/mag/sync.go - handleSyncPull, after indexing chunks:
pipe := &pipeline.Pipeline{VM: vmInst}
compiled, err := pipe.RehydrateFromStore()
if err != nil {
    fmt.Fprintf(os.Stderr, "Rehydration failed: %v\n", err)
    os.Exit(1)
}
fmt.Printf("Rehydrated: %d methods compiled\n", compiled)
```

#### 1.5 Testing Strategy

1. **Unit test in `pipeline/rehydrate_test.go`:** Create a VM, populate ContentStore with method stubs and class digests manually, call `RehydrateFromStore`, verify classes exist with working methods.
2. **Extend `TestEndToEnd_PushPull`:** After pull, call rehydrate and verify the class is callable.
3. **New integration test:** VM-A compiles real `.mag` source, pushes to VM-B, VM-B rehydrates and executes the code.

---

### Item 2: Transitive Pull on the Client Side

#### 2.1 Current State

`Serve` (server-side) already computes transitive closure and returns all missing chunks. `handleSyncPull` (client-side) processes the response correctly — it indexes methods and classes from the returned chunks. So transitive pull is *mostly* working.

The gap: the client sends `Have` (its local hashes) but does not walk dependencies after receiving chunks. If the server's transitive closure is incomplete (e.g., the server itself received partial content), the client has no way to detect or request the missing pieces.

#### 2.2 Changes Needed

**Add dependency verification after pull.** After indexing all received chunks, walk each class digest's `MethodHashes` and verify all referenced method hashes exist in the local store. Report any gaps.

**Add iterative pull.** If gaps exist, re-request from the peer with updated `Have` set. In practice, this should rarely be needed (Serve returns the full closure), but it makes the protocol robust.

**File to modify: `cmd/mag/sync.go`**

```go
func handleSyncPull(vmInst *vm.VM, peerAddr string, rootHashHex string, verbose bool) {
    // ... existing pull code ...

    // After indexing chunks, verify completeness
    missing := verifyCompleteness(store, rootHash)
    if len(missing) > 0 && verbose {
        fmt.Printf("Warning: %d hashes still missing after pull\n", len(missing))
    }

    // Rehydrate
    pipe := &pipeline.Pipeline{VM: vmInst}
    compiled, err := pipe.RehydrateFromStore()
    // ...
}

func verifyCompleteness(store *vm.ContentStore, root [32]byte) [][32]byte {
    var missing [][32]byte
    if d := store.LookupClass(root); d != nil {
        for _, mh := range d.MethodHashes {
            if !store.HasHash(mh) {
                missing = append(missing, mh)
            }
        }
    }
    return missing
}
```

#### 2.3 Pull by Class Name

Currently pull requires a 64-character hex hash. This is unusable in practice. Add class-name-based pull:

```
mag sync pull <peer-addr> --class MyApp::Greeter
```

This requires a new RPC or an extension to `Serve` that accepts a class name and resolves it to a root hash server-side. Simpler: add a `Resolve` RPC.

**Add to `proto/maggie/v1/sync.proto`:**

```protobuf
rpc Resolve(ResolveRequest) returns (ResolveResponse);

message ResolveRequest {
  string class_name = 1;  // FQN like "MyApp::Greeter"
}

message ResolveResponse {
  bytes root_hash = 1;    // 32-byte hash of the class digest
  bool found = 2;
}
```

**Add to `server/sync_service.go`:** Implement `Resolve` by searching ClassDigest entries by name.

**Add to `cmd/mag/sync.go`:** If the second argument doesn't look like a hex hash (not 64 chars), treat it as a class name and call Resolve first.

---

### Item 3: Content Store Persistence

#### 3.1 Options

**Option A: Disk-backed cache.** Store chunks on disk in a directory like `.maggie/cache/`. Each chunk is a file named by its hex hash. Simple, inspectable, works independently of image save/load.

**Option B: Integrate with image save/load.** Extend the image format (v5) to include ContentStore data. Pros: single-file distribution. Cons: image format change, ContentStore data is redundant with the compiled classes already in the image.

**Option C: Separate content store file.** A single binary file (e.g., `content.store`) using a simple format: `[count][hash1][cbor_len1][cbor1][hash2]...`. Loaded on startup if present.

**Recommendation: Option A (disk cache) for the demo, Option B later.**

Option A is simplest and provides immediate value. The cache directory is independent of the VM lifecycle. Multiple `mag` invocations share it. It's debuggable (you can `ls` the cache).

#### 3.2 Implementation

**New file: `vm/dist/cache.go`**

```go
package dist

// DiskCache persists content-addressed chunks to a directory.
type DiskCache struct {
    dir string // e.g., ".maggie/cache"
}

func NewDiskCache(dir string) (*DiskCache, error)
func (dc *DiskCache) Has(h [32]byte) bool
func (dc *DiskCache) Get(h [32]byte) (*Chunk, error)
func (dc *DiskCache) Put(chunk *Chunk) error
func (dc *DiskCache) AllHashes() ([][32]byte, error)

// LoadInto populates a ContentStore from the disk cache.
// Only loads chunks not already in the store.
func (dc *DiskCache) LoadInto(store *vm.ContentStore) (int, error)

// SaveFrom writes ContentStore entries to disk that aren't cached yet.
func (dc *DiskCache) SaveFrom(store *vm.ContentStore) (int, error)
```

File layout: `<dir>/<hex_hash_prefix_2>/<full_hex_hash>.chunk` (git-style 2-char prefix directories to avoid massive flat directories).

Each `.chunk` file is the CBOR-encoded `Chunk` struct — already have `MarshalChunk`/`UnmarshalChunk`.

#### 3.3 Integration Points

**On startup (`cmd/mag/main.go` or `pipeline/`):**

```go
cache, err := dist.NewDiskCache(".maggie/cache")
if err == nil {
    n, _ := cache.LoadInto(vmInst.ContentStore())
    if verbose && n > 0 {
        fmt.Printf("Loaded %d cached content entries\n", n)
    }
}
```

**After sync pull (`cmd/mag/sync.go`):**

```go
// After indexing chunks from pull response
if cache != nil {
    cache.SaveFrom(store)
}
```

**After compilation (`pipeline/pipeline.go` at end of `CompileAll`):**

The ContentStore is already populated. Optionally write to cache here too, but for the demo it's fine to only cache synced content.

#### 3.4 Cache Invalidation

Content-addressed storage has a beautiful property: hashes never go stale. A cached chunk is valid forever. The only maintenance operation is garbage collection (removing chunks no longer referenced by any class digest), which is a nice-to-have for later.

---

### Item 4: Visibility Commands

#### 4.1 `mag sync list`

Show all content in the local store, grouped by type.

```
$ mag sync list
Classes (3):
  MyApp::Greeter   a1b2c3d4...  (2 methods)
  MyApp::Main      e5f6a7b8...  (1 method)
  Core::Utils      c9d0e1f2...  (4 methods)

Methods (7):
  hello            1234abcd...  (MyApp::Greeter)
  world            5678ef01...  (MyApp::Greeter)
  start            9abc2345...  (MyApp::Main)
  ...

Total: 3 classes, 7 methods
```

**Implementation:** Iterate `ContentStore.classes` and `ContentStore.methods`. For methods, we need to know which class they belong to — reverse-index from class digests' `MethodHashes`. For classes, the `ClassDigest` has the name.

**Need new accessor on ContentStore:** `AllClassDigests() []*ClassDigest` and `AllMethods() map[[32]byte]*CompiledMethod`.

**File to modify:**
- `vm/content_store.go`: Add `AllClassDigests()`, `AllMethods()` methods.
- `cmd/mag/sync.go`: Add `case "list":` to `handleSyncCommand`, implement `handleSyncList`.

#### 4.2 `mag sync diff <peer>`

Show what a peer has that we don't (and vice versa).

```
$ mag sync diff localhost:9090
Remote has (we don't):
  MyApp::Handler   f1e2d3c4...  (3 methods)

We have (remote doesn't):
  MyApp::Greeter   a1b2c3d4...  (2 methods)

Common: 5 methods, 2 classes
```

**Implementation:** Call `Ping` to verify connectivity, then call `Announce` with our hashes — the response's `Want` list tells us what the peer is missing. For what the peer has that we don't, we need the reverse: call `Serve` with a known root hash from the peer. But we don't know the peer's root hash.

**Better approach:** Add a `List` RPC that returns all hashes the peer has (without transferring chunks).

**Add to `proto/maggie/v1/sync.proto`:**

```protobuf
rpc List(ListRequest) returns (ListResponse);

message ListRequest {}

message ListResponse {
  repeated bytes method_hashes = 1;
  repeated bytes class_hashes = 2;
  // Optional: class names for display
  repeated string class_names = 3;
}
```

**Implement in `server/sync_service.go`.** Simple: return all hashes from the local store.

**Implement `handleSyncDiff` in `cmd/mag/sync.go`:** Call `List` on peer, compare with local `AllHashes()`, display the symmetric difference.

#### 4.3 `mag sync show <hash-prefix>`

Allow short hash prefixes (like git) instead of full 64-char hashes:

```
$ mag sync show a1b2
Class: MyApp::Greeter (a1b2c3d4e5f6...)
  Namespace: MyApp
  Superclass: Object
  Instance vars: name, greeting
  Methods:
    hello  1234abcd...
    world  5678ef01...
```

**Implementation:** Add `ContentStore.LookupByPrefix(prefix string) (hash, type, error)`. Iterate all hashes, find matches. Error if ambiguous (multiple matches).

**File to modify:**
- `vm/content_store.go`: Add `LookupByPrefix`.
- `cmd/mag/sync.go`: Add `case "show":`, implement `handleSyncShow`.

---

## Part 2: Beyond the Demo — Distributed Programming

### Overview

The four demo items get us to "two VMs can share compiled code." The next question is: what does it mean to have a *distributed* Maggie system where processes on different nodes communicate, fail, recover, and share state?

Maggie's existing primitives (Process, Channel, Mutex, WaitGroup, CancellationContext) provide a solid single-node concurrency model. The content-addressed code layer provides a novel foundation: code is a verifiable, cacheable, content-addressed resource that can flow between nodes independently of the processes that use it. This separation of code distribution from process distribution is unusual and worth exploiting.

### Architecture: Three Layers

```
Layer 3: Distributed Programming Model (Maggie-level)
         RemoteProcess, RemoteChannel, NodeRef, Supervisor

Layer 2: Node Protocol (Go-level)
         Node discovery, health checking, message routing,
         code-on-demand, process migration

Layer 1: Content Distribution (exists today)
         ContentStore, Chunks, Announce/Transfer/Serve RPCs
```

Layer 1 exists. The demo items complete Layer 1. Layers 2 and 3 are the future work.

### Layer 2: Node Protocol

#### 2.1 Node Identity and Discovery

Each Maggie node has a stable identity (public key or UUID) and a transient network address. Nodes discover each other via:

- **Static peers:** Configured in `maggie.toml` `[sync].peers` (already exists)
- **Gossip:** Nodes share their peer lists periodically. A node joining one seed node eventually discovers the cluster.
- **mDNS:** For local development, auto-discover Maggie nodes on the LAN.

**Node identity file:** `.maggie/node.key` — generated on first run, contains an Ed25519 keypair. The public key is the node ID. This also enables signed announcements (code from node X is vouched for by X's reputation).

#### 2.2 Message Routing

Processes on different nodes need to exchange messages. The fundamental primitive is:

```
send(targetProcessID, message) -> ok | {error, reason}
receive(timeout) -> message | timeout
```

Process IDs become globally unique: `{nodeID, localProcessID}`. When sending to a remote process, the local node routes the message over the network.

**Wire format:** Extend the gRPC service with a `SendMessage` RPC:

```protobuf
rpc SendMessage(MessageRequest) returns (MessageResponse);

message MessageRequest {
  bytes target_process_id = 1;  // local process ID on the target node
  bytes payload = 2;            // CBOR-encoded Maggie Value
}
```

**Value serialization:** Maggie values need to be serializable for network transmission. Simple values (integers, floats, strings, symbols, booleans, nil) are straightforward. Objects are harder — see Section 2.5 below.

#### 2.3 Code-on-Demand

This is where Maggie's content-addressed approach creates a genuinely novel capability. When a node receives a message that references a class it doesn't have, it can:

1. Identify the missing class by its content hash.
2. Pull the class (and its transitive dependencies) from the sending node.
3. Rehydrate the code locally.
4. Continue processing the message.

This is transparent to the Maggie programmer. It's analogous to Java's class loading, but content-addressed: the receiving node can verify the code it downloads, cache it indefinitely, and serve it to other nodes.

**Implementation:** The `Serve` RPC + rehydration pipeline (Items 1-2) provide the foundation. The key addition is triggering pull automatically when a message references an unknown class.

The mechanism: when deserializing a message payload, if a value references a class hash not in the local store, pause deserialization, pull from the sender, rehydrate, resume. This requires the message format to include class hashes alongside object data.

#### 2.4 Remote Process Spawning

```smalltalk
"Spawn a process on a specific node"
proc := [self doWork] forkOn: nodeRef.

"Spawn on any available node (load balancing)"
proc := [self doWork] forkAnywhere.

"Spawn with code pre-push (ensure the remote has our code first)"
proc := [self doWork] forkOn: nodeRef withCode: MyApp::Worker.
```

Under the hood, `forkOn:` does:

1. Ensure the remote node has the code (content hash check, push if needed).
2. Send a `SpawnProcess` RPC with the block's compiled method hash and captured variables.
3. Return a `RemoteProcess` proxy that supports `wait`, `result`, `isDone`.

**Block serialization challenge.** A Maggie block captures local variables from its enclosing scope. To send a block to a remote node, we must serialize:
- The block's compiled method (identified by content hash — the remote can pull it).
- The captured variable values (must be serializable).

Non-serializable captures (open files, mutexes, channels) cannot cross node boundaries. The runtime should detect this and raise a clear error at fork time, not at message time.

#### 2.5 Value Serialization

For distributed messaging, Maggie values must be serializable. Define a CBOR-based serialization for the Maggie value space:

| Value type | Serialization |
|---|---|
| SmallInt | CBOR integer |
| Float | CBOR float64 |
| Boolean | CBOR bool |
| Nil | CBOR null |
| Symbol | CBOR text + tag |
| String | CBOR text |
| Array | CBOR array (recursive) |
| Object | CBOR map: {classHash, slots: [...]} |

Objects are serialized as their class's content hash plus their slot values. The receiver must have (or pull) the class to deserialize. This is the code-on-demand trigger.

**Non-serializable types:** Process, Channel, Mutex, Semaphore, WaitGroup, CancellationContext, GoObjectWrapper. Attempting to serialize these raises an error. Future work: Channel proxying (see Section 3.2).

**New file:** `vm/dist/serial.go` — `SerializeValue(v Value, vm *VM) ([]byte, error)` and `DeserializeValue(data []byte, vm *VM) (Value, error)`.

#### 2.6 Health Checking and Failure Detection

Nodes periodically ping each other (the `Ping` RPC already exists). Failure detection uses an adaptive timeout:

- Healthy peers: ping every 5 seconds, timeout 10 seconds.
- Suspect peers: ping every 1 second, timeout 3 seconds.
- Dead peers: stop pinging, mark as dead, notify supervisors.

**Supervisor trees (inspired by Erlang/OTP):**

```smalltalk
sup := Supervisor new.
sup addChild: [MyApp::Worker new run]
    name: 'worker-1'
    restart: #permanent    "always restart"
    maxRestarts: 5
    window: 60.            "max 5 restarts in 60 seconds"

sup addChild: [MyApp::Logger new run]
    name: 'logger'
    restart: #transient.   "restart only on abnormal exit"

sup start.
```

Supervisors are local-first: they manage processes on their own node. A `DistributedSupervisor` extends this to manage processes across nodes, restarting them on a different node if the original node dies.

### Layer 3: Distributed Programming Model

#### 3.1 Location Transparency

The core question: should a Maggie programmer be able to send a message to an object on another node transparently?

**Position: No full location transparency. Explicit distribution boundaries.**

Full location transparency (as in Akka's original design, or CORBA, or Java RMI) is widely considered a mistake. Network calls fail in ways local calls don't (partial failure, latency, ordering). Hiding the network boundary leads to fragile systems.

Instead, Maggie should make distribution explicit but ergonomic:

```smalltalk
"Remote reference is explicit"
remoteWorker := node at: 'worker-1'.

"Sending is explicit (returns a Future, not a result)"
future := remoteWorker asyncSend: #doWork: with: {data}.
result := future await.       "blocks until response"
result := future await: 5000. "blocks with timeout"

"Fire-and-forget"
remoteWorker cast: #logEvent: with: {event}.
```

The `asyncSend:with:` pattern makes it clear this is a remote call. The returned future makes it clear the result is not immediate. Timeouts are natural.

#### 3.2 Distributed Channels

Maggie's Channel primitive is the natural extension point for distributed messaging. A `RemoteChannel` proxies send/receive operations to a channel on another node:

```smalltalk
"Create a channel that's accessible from other nodes"
ch := Channel newPublished: 'events'.

"On another node, connect to it"
ch := node channel: 'events'.
ch send: myEvent.            "Transparently serializes and sends"
value := ch receive.          "Blocks until remote sends"
```

Under the hood, `RemoteChannel` wraps the Connect/gRPC `SendMessage` RPC. Each `send:` serializes the value and sends it. Each `receive` blocks on a local goroutine that's waiting for incoming messages on that channel name.

**Channel select across nodes:**

```smalltalk
result := Channel select: {
    localCh onReceive: [:v | handleLocal: v].
    remoteCh onReceive: [:v | handleRemote: v]
}.
```

This works because `RemoteChannel` implements the same interface as `Channel`. The select mechanism polls both local and remote channels.

**Ordering guarantee:** Messages on a single channel from a single sender arrive in order (FIFO). Messages across channels or from multiple senders have no ordering guarantee. This is the same as Go channels and is the right default.

#### 3.3 Consistency Model

**Default: Eventual consistency with causal ordering per sender-receiver pair.**

Each node's ContentStore is an eventually-consistent replica. Code pushed to one node eventually propagates to others (via gossip or explicit pull). There is no global consensus on "which version of a class is current."

**Per-sender-receiver causal ordering:** If node A sends messages M1 then M2 to node B, B sees M1 before M2. This is trivially ensured by TCP ordering on a single connection.

**No distributed transactions.** Maggie does not provide distributed locks or two-phase commit. Applications that need coordination use explicit patterns:

```smalltalk
"Saga pattern"
saga := Saga new.
saga step: [orderService createOrder: order]
     compensate: [orderService cancelOrder: order].
saga step: [paymentService charge: amount]
     compensate: [paymentService refund: amount].
saga execute.
```

#### 3.4 Content-Addressed Code Distribution Patterns

Maggie's content-addressed code layer enables several patterns that are difficult or impossible in traditional distributed systems:

**Pattern 1: Code as Capability.** A content hash is an unforgeable reference to a specific piece of code. Sending someone a hash gives them the ability to pull and run that exact code — no more, no less. Combined with process-level restrictions (Phase 4, already implemented), you can run untrusted code in a sandbox:

```smalltalk
"Receive a code hash from an untrusted peer"
hash := peer receiveCodeHash.

"Pull and rehydrate the code"
Compiler pullAndLoad: hash from: peer.

"Run it in a restricted process"
result := [UntrustedCode new run]
    forkRestricted: #('File' 'HTTP' 'Network').
result wait.
```

**Pattern 2: Deterministic Replay.** Because method content hashes are deterministic (same source = same hash), you can record a computation as a sequence of `(methodHash, arguments)` tuples and replay it on any node. This enables:
- Debugging distributed systems by replaying on a single node.
- Migrating a process from one node to another by replaying its message log.

**Pattern 3: Content-Addressed Deployment.** Instead of deploying "version 2.3.1 of MyApp," you deploy "class tree rooted at hash X." The hash uniquely identifies the exact code. Rolling back is just switching to the previous root hash. Canary deployments are running two root hashes simultaneously.

**Pattern 4: Lazy Code Loading.** A node doesn't need all the code upfront. It can start with just the entry point class and pull dependencies on demand as execution reaches them. This is natural with content-addressed code — the entry point's class digest lists its method hashes, each method's source may reference other classes (by FQN), and those classes have their own digests.

#### 3.5 Process Migration

Move a running process from one node to another:

```smalltalk
proc := [longRunningComputation] fork.
"... later ..."
proc migrateTo: otherNode.
```

This requires serializing the process's call stack, which is hard in general (open references to non-serializable resources). A practical subset:

- Processes that only use serializable values can be migrated.
- The migration mechanism is: serialize current frame's local variables, serialize the continuation (method hash + instruction pointer + remaining stack), send to target node, reconstruct and resume.

**Content-addressed code makes this tractable.** The target node can pull any code it's missing by hash. The migration payload is just: `{methodHash, instructionPointer, locals: [...], stack: [...], parentFrame: {...}}`.

This is a long-term goal. For the medium term, the simpler pattern is: checkpoint the computation's state as a serializable object, kill the process, restart on another node from the checkpoint.

#### 3.6 What Erlang/OTP, Akka, and Orleans Got Right

**From Erlang/OTP:**
- *Processes are cheap.* Maggie already has this (goroutine-backed processes).
- *Let it crash.* Supervisors restart failed processes. Maggie should adopt this philosophy.
- *Links and monitors.* Process A can link to Process B; if B dies, A is notified. Essential for distributed supervision.
- *Distribution is explicit.* Erlang's `{Name, Node} ! Message` syntax makes the network boundary visible.
- *Hot code loading.* Erlang can replace running code. Maggie's content-addressed model makes this natural — push a new class digest with updated method hashes, and new processes pick it up.

**From Akka:**
- *Location transparency was a mistake.* Akka moved away from transparent remoting toward explicit Cluster Sharding and streams.
- *Cluster membership.* Akka's cluster module handles node join/leave/unreachable states. Maggie needs something similar but simpler.
- *Persistence.* Akka Persistence (event sourcing) enables replay. Maggie's content-addressed methods make replay deterministic.

**From Orleans:**
- *Virtual actors.* Actors are always "available" — the runtime activates them on demand on any node. Maggie could adopt this: a "virtual process" is activated when it receives a message, on whatever node is convenient.
- *Automatic scaling.* Orleans distributes actors across nodes based on load. Maggie's `forkAnywhere` could do the same.
- *Single-threaded actors.* Orleans guarantees no concurrent execution of an actor. Maggie's Mutex already provides this locally; distributed mutual exclusion is harder.

**What's unique about Maggie:**
- *Content-addressed code.* No other mainstream actor system has this. It decouples code distribution from process distribution and enables code-on-demand, deterministic replay, and hash-based deployment.
- *Process-level restriction.* Already implemented. Running untrusted code from the network in a sandbox is a built-in capability, not an afterthought.
- *Smalltalk-style reflection.* `Object allClasses`, `Compiler evaluate:`, etc., interact with the distribution layer (they respect process restrictions, they see synced classes). This enables powerful metaprogramming in distributed contexts.

### Implementation Roadmap (Post-Demo)

#### Phase A: Value Serialization + Remote Messaging (4-6 weeks)
- `vm/dist/serial.go`: CBOR-based value serialization
- `SendMessage` / `ReceiveMessage` RPCs
- `RemoteProcess` class in Maggie
- `asyncSend:with:` and `cast:with:` primitives
- Process links and monitors

#### Phase B: Remote Process Spawning (3-4 weeks)
- Block serialization (method hash + captured vars)
- `forkOn:` primitive
- `SpawnProcess` RPC
- Code-on-demand trigger (auto-pull missing classes)

#### Phase C: Distributed Channels (3-4 weeks)
- `RemoteChannel` class
- `Channel newPublished:` / `node channel:`
- Cross-node channel select
- Backpressure handling

#### Phase D: Supervisor Trees (2-3 weeks)
- `Supervisor` class (local)
- Restart strategies (one-for-one, one-for-all)
- `DistributedSupervisor` (cross-node)
- Health checking integration

#### Phase E: Cluster Membership (3-4 weeks)
- Gossip protocol for peer discovery
- Node join/leave/unreachable states
- Consistent hashing for process placement
- `forkAnywhere` with load-based placement

#### Phase F: Process Migration (4-6 weeks)
- Call stack serialization
- Continuation checkpointing
- `migrateTo:` primitive
- Checkpoint-and-restart pattern

---

## Appendix: File Reference

### Existing Files (to modify for demo)

| File | Changes |
|---|---|
| `vm/dist/chunk.go` | Add Selector, ClassName, IsClassSide fields |
| `vm/dist/chunker.go` | Enrich MethodToChunk, rewrite ClassToChunk, add EncodeClassContent/DecodeClassContent |
| `vm/dist/chunker_test.go` | Update for new formats |
| `vm/content_store.go` | Add AllClassDigests(), AllMethods(), LookupByPrefix() |
| `cmd/mag/sync.go` | Add list/diff/show subcommands, rehydration after pull, class-name pull |
| `server/sync_service.go` | Add Resolve, List RPCs; update indexVerifiedMethod/indexVerifiedClass |
| `server/sync_integration_test.go` | Extend PushPull test with rehydration verification |
| `proto/maggie/v1/sync.proto` | Add Resolve, List RPCs |
| `pipeline/pipeline.go` | (reference only — rehydrate reuses patterns from CompileAll) |

### New Files (to create for demo)

| File | Purpose |
|---|---|
| `pipeline/rehydrate.go` | RehydrateFromStore: compile synced content into runnable classes |
| `pipeline/rehydrate_test.go` | Unit tests for rehydration |
| `vm/dist/cache.go` | DiskCache: persist content store to disk |
| `vm/dist/cache_test.go` | Unit tests for disk cache |
| `vm/dist/class_content.go` | EncodeClassContent/DecodeClassContent (or inline in chunker.go) |
| `compiler/parse_method.go` | ParseMethodDef: parse a single method definition from source text |

### Recommended Implementation Order

1. **Enrich class chunk Content** (Item 1.2) — prerequisite for everything else
2. **Add ParseMethodDef to compiler** (Item 1.3) — small, testable
3. **Implement RehydrateFromStore** (Item 1.3) — the big piece
4. **Wire rehydration into pull** (Item 1.4 + Item 2)
5. **Add visibility commands** (Item 4) — useful for debugging items 1-3
6. **Add disk cache** (Item 3) — can be done in parallel with items 1-4
7. **Add Resolve and List RPCs** (Items 2.3, 4.2) — proto regen required, batch it
