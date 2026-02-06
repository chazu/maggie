# Module System Design Brainstorm

**Date:** 2026-02-04
**Epic:** maggie-vc64 (Module System), maggie-gu5d (Project Manifest & Dependency System)
**Status:** In progress — design exploration

---

## What We're Building

A module system for Maggie that supports namespaces, imports, dependency management, and code distribution — designed so that it does not impede the long-term goal of a distributed object-capability Smalltalk dialect.

---

## Key Design Decisions

### 1. Trust Model: Erlang-style with Process-Level Restriction

**Decision:** Maggie is a trusting system within a node. The module/import system carries no security burden. Security boundaries live at the process level, not the module level.

**Rationale:**
- Pure ambient authority (like traditional Smalltalk) is too open for distributed systems.
- Pure object-capability (like Newspeak's parameterized modules with no globals) is ergonomically painful and a major departure from Smalltalk tradition.
- A gatekeeper layer over the existing import system would be "security theater" — enforcement-based, not structural.
- Erlang proves that a trusting model within a node, with process-level isolation, is practical for distributed systems at scale.

**How it works:**
- Within a node/image: full trust. Any code can import any namespace.
- Processes are the isolation boundary. Each process resolves class names through its own "environment" (a name → class binding table).
- By default, processes inherit the full class table from their parent.
- Restricted processes get a filtered environment where certain names simply don't exist — not a permission error, a name resolution failure.
- Users can implement `RestrictedProcess`-like patterns as library code, using the environment hook the VM provides.

**What this means for imports:** `import:` is purely about name resolution convenience. It has no security implications.

### 2. Content-Addressable Code for Distribution

**Decision:** Class/module definitions are content-addressed (identified by hash of their definition). Names are a local, mutable mapping layer on top of immutable, content-addressed definitions.

**Rationale:**
- Name-based restriction (Approach 1) has a distribution problem: names are locally meaningful but ambiguous across nodes.
- Content-addressing makes definitions globally unambiguous: "I need the class with hash `sha256:abc123`" is precise regardless of what any node calls it.
- This is the Unison model adapted for Smalltalk's mutable image tradition.

**Architecture:**
```
Human layer:     namespace: 'MyApp::Payments'
                 import: 'MyApp::Models'
                      |
Class table:     "MyApp::Models::User" → hash:a1b2c3   (mutable, per-image)
                      |
Content store:   hash:a1b2c3 → {bytecode, methods, slots, source}  (immutable)
```

**How it composes:**
- **Locally:** Names everywhere. Content-addressing is invisible.
- **Over the wire:** Ship hashes + definitions. The receiver controls naming.
- **Per-process restriction:** The process environment controls which name → hash mappings are visible.
- **Hot code swap:** Redefine a class = point name at a new hash. Old hash retained for rollback.
- **Versioning:** Redefinition produces a new hash. Old definitions are retained by their hash. Rollback = remap name to old hash.

**Analogy:** This is Git's data model applied to classes. Classes are blobs (content-addressed). The class table is a tree (name → hash). An image snapshot is a commit.

**Granularity (decided):** Layered, like Git's object model:

```
Method hash  = hash(selector + params + canonical_AST(body) + docstring)  ← "blob"
Class hash   = hash(name + super + ivars + sorted([method hashes]))       ← "tree"
Module hash  = hash(sorted([class hashes]))                               ← "commit"
```

Each level is built from the levels below. Changing one method changes its hash → changes its class hash → changes its module hash. Only the changed parts are new.

**Hashing target (decided):** Normalized AST, not source text or bytecode.
- Format-independent: whitespace, comment placement, line breaks don't matter.
- Compiler-independent: bytecode format can change without invalidating hashes.
- Semantically meaningful: same AST = same program structure.
- Computed at compile time (the compiler already has the AST; serializing + hashing adds microseconds).

**Method hashing (decided):** Methods are class-independent, like Git blobs.
- A method's hash = hash(selector + params + AST of body + docstring).
- Instance variable references are by NAME in the AST, not by slot index.
- `super` is a node type, not a reference to a specific superclass.
- Two classes with identical method source have the same method hash → deduplicated in the content store.
- Trait methods are automatically deduplicated across classes that use them.

**Normalization rules (decided):**
- **Method ordering:** Order-independent. Methods sorted by selector before computing class hash. Rearranging methods in source doesn't change the class hash.
- **Docstrings:** Included in hash. Documentation is content. Changing docs = new hash.
- **Literal values:** Normalized to canonical form. `16rFF` and `255` produce the same AST node → same hash.
- **Variable names:** Significant. `[:x | x + 1]` and `[:y | y + 1]` are different hashes. (De Bruijn indices to make names irrelevant is a possible future evolution, not for now.)

**Storage (initial):** In the image file. Content store embedded in the image alongside the name table. A separate on-disk store (like `.git/objects/`) can be added later for distribution/sharing.

**Hash algorithm:** SHA-256.

### 3. Module = Namespace (Not a First-Class Runtime Object)

**Decision:** A "module" is a namespace — a string prefix in the class table. It is not a first-class runtime object (unlike Newspeak's parameterized modules).

**Rationale:** Keeping modules as a naming convention (rather than runtime objects) preserves the Smalltalk "one image, everything live" feel. Process-level restriction + content-addressing provide the security and distribution properties that would otherwise require runtime module objects.

### 4. Existing Syntax Retained

**Decision:** The `namespace:` and `import:` syntax documented in CLAUDE.md is retained. The class resolution order (current namespace → imports in order → bare/root) is retained.

**Rationale:** Already implemented in the parser and VM. No reason to change — these decisions are sound and the syntax works.

### 5. No Visibility Modifiers

**Decision:** No public/private modifiers on classes or methods. All classes in a namespace are visible to all other namespaces.

**Rationale:** Matches Smalltalk philosophy. Explicitly rejected in the 2026-02-02 project-improvements brainstorm. Visibility enforcement is not the module system's job — process-level restriction handles isolation when needed.

### 6. Go Interop via Importable Wrapper Namespaces

**Decision (tentative):** Go packages appear as importable namespaces with a `Go::` prefix convention. Go functions become class-side methods on wrapper classes.

**Sketch:**
```smalltalk
import: 'Go::fmt'
import: 'Go::net/http'

Fmt println: 'Hello from Maggie'.
resp := HTTP get: 'https://example.com'.
```

Go wrappers get synthetic content hashes based on Go module path + version (e.g., `sha256(go:fmt@v1.22.0)`).

**Open:** The FFI boundary, what types can cross it, and how Go's error model maps to Maggie's exception model are all TBD.

### 7. Qualified and Unqualified Access: Both Work, Compiler Resolves to FQN

**Decision:** Both qualified (`Yutani::Widgets::Button`) and unqualified (`Button` after `import: 'Yutani::Widgets'`) access work. Unqualified names require an `import:` declaration. The compiler resolves all class references to fully-qualified names (FQN) at compile time — bytecode always contains the FQN.

**Rationale:**
- Qualified access already works mechanically: `main.go:820-826` registers every class under both short name and FQN in the Globals map, and `OpPushGlobal` does a flat string lookup.
- But `import:` currently only affects superclass resolution during class loading (`main.go:796` calls `LookupWithImports`). It has no effect on method body compilation — the compiler (`codegen.go:459`) emits the raw name as-is.
- This means unqualified access currently works by accident (short name registered in Globals) and breaks silently when two namespaces define the same class name (last-loaded wins).
- Fixing this by having the compiler resolve to FQN at compile time makes imports meaningful and bytecode unambiguous.

**How it works:**
- **Qualified names** pass through unchanged. `Yutani::Widgets::Button` in source → `Yutani::Widgets::Button` in bytecode → found in Globals.
- **Unqualified names** are resolved by the compiler using the file's `namespace:` and `import:` declarations. `Button` in a file with `import: 'Yutani::Widgets'` → compiler calls `LookupWithImports("Button", currentNS, imports)` → resolves to `Yutani::Widgets::Button` → that FQN is stored in bytecode.
- **No import = must use FQN.** If you don't `import: 'Yutani::Widgets'`, then `Button` alone won't resolve. Use `Yutani::Widgets::Button` explicitly.
- **Bare/root classes** (no namespace, like `Array`, `Object`) continue to resolve by their short name, since the FQN equals the short name.

**Why this fits the distributed process model:**
- **Bytecode is self-contained.** A serialized method carries FQNs — no import context needed to interpret it. Ship a method between nodes without its file's import list.
- **Per-process restriction is precise.** The Globals map contains FQNs. Removing `Yutani::Widgets::Button` from a process's map is surgical. No ambiguity about which `Button` is restricted.
- **Short-name registration in Globals can be dropped.** Once the compiler emits FQNs, registering classes under short names (`main.go:820`) is unnecessary and a source of silent collisions. Only the FQN entry is needed.

**Implementation gap (existing bug):** The compiler currently does not resolve class references in method bodies — it passes raw names through to `OpPushGlobal`. The fix: during method compilation, the compiler must know the current file's namespace and imports, and call `LookupWithImports` (or equivalent) to resolve unqualified names to FQNs before emitting literals. `LookupWithImports` already exists in `vm/class.go:333`.

### 8. Canonical AST Serialization: Unison-Inspired Frozen Hashing Types

**Decision:** Define a separate, frozen set of AST node types used only for hashing, distinct from the compiler's working AST. Follow Unison's architecture: a stable "hashing AST" with a version prefix, so the compiler internals can evolve without invalidating hashes.

**Rationale:**
- Unison proves this approach works at scale. Their `unison-hashing-v2` package has been stable for years while the compiler AST changed freely underneath.
- Decoupling prevents accidental hash invalidation when refactoring compiler internals.
- A version prefix means format changes produce entirely different hashes — no ambiguity about which scheme produced a given hash.

**Format (decided):**
- **Token stream:** Each AST node serializes to a sequence of typed tokens: tag bytes (uint8), raw bytes (length-prefixed), integers (int64 big-endian), unsigned integers (uint64 big-endian), floats (float64 big-endian IEEE 754), text (UTF-8, length-prefixed), and hashes (raw 32 bytes for SHA-256).
- **Tag byte per node type:** Every node type gets a unique uint8 discriminator. Terms, types, and declarations use different tag-space prefixes to prevent cross-domain collisions.
- **Merkle tree:** Child nodes are hashed first; their hash is embedded as raw bytes in the parent's token stream. Equality checking is O(1) at each level.
- **Version prefix:** The first byte of every hash input is a version tag (starting at `1`). If the serialization format changes, bump the version — all hashes change cleanly.
- **Length-prefixed strings:** All variable-length data (strings, byte arrays) preceded by uint64 big-endian length.
- **No framing between tokens:** The stream is not self-describing. Both writer and reader must agree on the exact token sequence for each node type. This is safe because the structure is deterministic — each node type always produces the same token pattern.

**What Maggie does differently from Unison:**
- **Variable names are significant** (no positional indices). `[:x | x + 1]` and `[:y | y + 1]` produce different hashes. Simpler to implement; de Bruijn indices are a future evolution.
- **SHA-256, not SHA3-512.** Sufficient collision resistance, more widely available, and matches the rest of Maggie's crypto choices.
- **Types are not incorporated into hashes.** A method's hash depends on its AST, not its inferred type. This keeps the hashing scheme independent of any future type system.

**Open:** The exact tag byte assignments for each AST node type need to be specified. This is mechanical work — enumerate every node type in the parser AST and assign a stable uint8.

### 9. Dependency → Namespace Mapping: Producer-Declared, Consumer-Overridable

**Decision:** A dependency's importable namespace comes from its own `maggie.toml` `[project] namespace` field. The consumer can override with a `namespace` key in the dependency declaration. Collisions are hard errors.

**Rationale:**
- Surveyed Rust/Cargo, Go modules, Elixir/Mix, Python/pip, Ruby/Bundler, and Node/npm.
- Python is the cautionary tale: distribution name and import name are independent, unrelated strings. Everyone hates it.
- Go and Node have the cleanest mappings (name = import path). Rust adds consumer renaming via the `package` key, which is essential for collision avoidance.
- Maggie needs all three properties: deterministic default, producer authority, consumer escape hatch.

**How it works:**

1. **Producer declares namespace** in their `maggie.toml`:
   ```toml
   [project]
   name = "yutani"
   namespace = "Yutani"
   ```
   All classes from this dependency appear under `Yutani::*`.

2. **Fallback:** If the dependency has no `maggie.toml` or no `namespace` field, the namespace is derived from the directory structure (already implemented) or PascalCase of the dependency name.

3. **Consumer can override** in their own `maggie.toml`:
   ```toml
   [dependencies]
   yutani = { git = "...", tag = "v0.5.0", namespace = "ThirdParty::Yutani" }
   ```
   This remaps all classes from `Yutani::*` to `ThirdParty::Yutani::*`.

4. **Hard error on collision:** If two dependencies inject the same fully-qualified class name, the loader fails with a clear error message naming both dependencies. The consumer resolves it by adding a `namespace` override to one or both.

5. **Hard error on missing namespace:** If no namespace can be determined for a dependency (no `maggie.toml`, no directory structure, no override), the loader fails. Classes are never silently dumped into the root namespace.

**The dependency key is not the import name.** The key (`yutani`) is a local alias for fetching and locking. The namespace (`Yutani`) is what code references via `import:`. These serve different purposes.

### 10. Process-Level Restriction: Lua-Style Per-Process Globals Map

**Decision:** Each process gets its own Globals map. Restricted processes get a filtered copy where certain names don't exist. The mechanism is a copy-on-write overlay on the parent's map.

**Rationale:**
- Surveyed BEAM, JVM (ClassLoaders), Smalltalk (Associations), Ruby (inline constant cache), CLR (AssemblyLoadContext), and Lua (`_ENV`).
- **Key discovery:** Maggie already does runtime name-based lookup. `OpPushGlobal` reads a name from the literal frame and does `i.Globals[name]` — a `map[string]Value` lookup on every execution. There is no "fast direct reference" to lose. The system is already late-bound.
- Lua's `_ENV` mechanism is the closest analog: each loaded chunk gets its own environment table. Sandboxing = loading code with a filtered table. Maggie's architecture (interpreter with Globals map, lightweight processes) maps directly.
- The JVM's ClassLoader approach is powerful but depends on JIT to eliminate indirection cost. Without JIT (Go runtime), keep indirection simple.
- .NET's AssemblyLoadContext is a cautionary tale: isolation is "leaky" when objects carry references that bypass boundaries. Maggie's message-passing process model naturally avoids this.

**How it works:**
- By default, a child process inherits a read-only reference to its parent's Globals map. Zero allocation cost — most processes never diverge.
- When a process is spawned with restrictions, the Globals map is shallow-copied and restricted names are deleted from the copy. This is the copy-on-write (COW) overlay.
- `OpPushGlobal` continues to do `i.Globals[name]` — no bytecode changes needed. The only change is that `i.Globals` points to a per-process map instead of a shared one.
- A restricted process that forks a child passes its restricted map to the child. Restriction is inherited.
- A name absent from the Globals map produces a name resolution failure (not a permission error). From the process's perspective, the class simply doesn't exist.

**What this means for the VM:**
- The `Interpreter` struct's `Globals` field becomes per-process (it may already be — need to verify the fork path).
- A `RestrictedProcess` pattern in library code: fork a process, filter its Globals map, done.
- No new opcodes. No bytecode changes. No compiler changes.

**Future optimization (not needed now):** The Smalltalk Association model (indexed literal frame with binding objects instead of map lookup) would be faster — replaces a hash map lookup with an array index + field read. Worth considering if profiling shows `OpPushGlobal` as a hot spot. But the Lua-style approach is correct and sufficient to start.

### 11. De Bruijn Indices in Hashing AST: Yes, From Day One

**Decision:** The hashing AST normalizes local variable references to de Bruijn-style `(scope_depth, slot_index)` pairs instead of names. This ensures alpha-equivalent code (`[:x | x + 1]` and `[:y | y + 1]`) produces identical content hashes. Built into the hashing layer from the start to avoid a hash-breaking migration later.

**What gets indexed:**
- Method args, method temps, block params, block temps, captured vars → `(scope_depth, slot_index)`
- Instance variables → slot index only (name dropped)
- Globals → FQN string (no de Bruijn; name is semantically meaningful)

**Rationale:**
- The hashing AST is already a separate "frozen" representation (Decision 8). De Bruijn indexing is one more normalization step in the `working AST → hashing AST` transformation. It touches nothing else.
- The compiler already resolves variables in exactly this order (`codegen.go:411-461`): method args → temps → ivars → captured → globals. The hashing transform replays that same resolution, emitting `(depth, slot)` instead of opcodes.
- Estimated ~100-150 lines of Go: a scope stack, a tree walk, and a resolution function mirroring existing compiler logic.
- **Risk is low:** a bug affects content-address dedup, not execution. The working AST, compiler, and VM never see de Bruijn indices. Easily tested with alpha-equivalent AST pairs.
- **Retrofitting later would break every existing content hash.** Building it in from day one avoids a migration.

### 12. Circular Imports: Non-Issue Under Two-Pass Loading

**Decision:** Circular imports are allowed and require no special handling. The loading pipeline must use two-pass loading: pass 1 registers all class skeletons (name, namespace, superclass), pass 2 compiles all method bodies.

**Rationale:**
- The circular import question (`A imports B, B imports A`) is actually a subset of the **forward reference** problem: any time file A references a class from file B and A loads first, compile-time FQN resolution (Decision 7) fails because B's classes aren't in the ClassTable yet.
- Circular imports make this unavoidable — no file ordering can satisfy both directions. But even without circularity, alphabetical `filepath.Walk` ordering can cause the same breakage.
- **Two-pass loading** resolves both problems at once:
  - **Pass 1:** Parse all source files, register class skeletons in the ClassTable (name + namespace + superclass). No method compilation. After this pass, every class name is known.
  - **Pass 2:** Compile all method bodies. The compiler can now resolve any class reference to FQN via `LookupWithImports` because the ClassTable is complete.
- This is the standard approach used by Java, C#, and Go itself. It's a small restructuring of `compileSourceFile` — split the class-registration loop (lines 782–835) from the method-compilation loop (lines 837–916).
- At **runtime**, circular references were never a problem: `OpPushGlobal` does a flat map lookup, and by execution time all classes are registered regardless of load order.

**What changes:**
- `compileSourceFile` (or its caller) needs to be split into two phases. The simplest approach: collect all parsed `SourceFile` ASTs in pass 1 while registering classes, then iterate again for method compilation.
- This is a prerequisite for the compiler FQN resolution fix (Decision 7, implementation step 1). The two changes should land together.

### 13. Go Interop: Hybrid Stub Generation + Dependency-Driven Build

**Decision:** Wrapper authors use `mag wrap` to generate stub files, implement the TODOs by hand, and ship the result as a Maggie library. Consumers declare the wrapper as a dependency; `mag deps` fetches both the Maggie source and the Go source; `mag build` compiles a custom binary with all Go wrappers linked in.

**Workflow for wrapper authors:**
```bash
mag wrap github.com/redis/go-redis/v9 --namespace "Redis" --out primitives/redis.go
# Edit primitives/redis.go, implement the TODOs
# Ship as a Maggie library with maggie.toml declaring the Go dependency
```

**Workflow for consumers:**
```bash
# maggie.toml
[dependencies]
redis-mag = { git = "github.com/someone/redis-mag" }

# Then:
mag deps    # Fetches .mag files + Go source for all [go-wrap] dependencies
mag build   # Compiles custom mag binary with Go wrappers linked in
./my-app    # Run with custom binary (or: mag run with plugin loading, future)
```

**What `mag wrap` generates:**
1. **Marker constants** — unique bit patterns for wrapped types (100+ range for user wrappers, 1-99 reserved for core)
2. **Registry additions** — storage for wrapped Go objects with thread-safe access
3. **Conversion helpers** — `typeToValue()` and `valueToType()` functions
4. **Method stubs** — empty implementations with TODO comments and correct signatures
5. **Registration function** — `RegisterXPrimitives(vm *VM)` that wires everything up

**What gets wrapped:**
| Go construct | Maggie equivalent |
|--------------|-------------------|
| Struct type + methods | Class with instance methods |
| Package-level function | Class method on a module class (`Redis get: key`) |
| Package constant | Class method returning the value |
| `init()` | Called once when `RegisterXPrimitives()` runs |

**Manifest format for wrapper libraries:**
```toml
# redis-mag/maggie.toml
[project]
name = "redis-mag"
namespace = "Redis"

[go-wrap]
package = "github.com/redis/go-redis/v9"
version = "v9.5.1"
primitives = "primitives/redis_primitives.go"
```

**Type mapping (wrapper author decides, generator provides defaults):**
- Go primitives (`int`, `string`, `bool`, `float64`) ↔ Maggie primitives (SmallInt, String, True/False, Float)
- Go structs ↔ Marker-based IDs stored in ObjectRegistry (opaque handles)
- Go `(T, error)` returns ↔ Maggie Result monad (Success/Failure)
- Go `context.Context` ↔ Auto-injected `context.Background()` by default; wrapper author can expose cancellation

**Error handling:**
- Go functions returning `(T, error)` generate stubs that return `Result` objects
- Go panics are caught in a `recover()` wrapper and converted to Failure Results
- Wrapper authors can customize error messages in their implementations

**Namespace control:**
- `--namespace` flag on `mag wrap` sets the default
- Wrapper author can edit the generated code to restructure as desired
- Consumer can override via `namespace =` in their dependency declaration (see Decision 9)

**Extension model (phased):**
1. **Phase 1 (MVP):** `mag build` compiles a custom binary. Every project with Go wrappers has its own `mag` executable.
2. **Phase 2 (future):** Go plugin support (`.so` loading) for platforms that support it. Share base `mag` binary, load wrappers as plugins.
3. **Phase 3 (future):** Investigate Yaegi or similar for interpreted Go (performance tradeoff, but true "fetch and run").

**Concurrency model:**
- Go goroutines spawned by wrapped code run independently of Maggie processes
- Maggie channels and Go channels are separate; bridging requires explicit wrapper code
- Wrapper authors can expose Go channels as Maggie Channel objects if needed (registry pattern)

### 14. Distribution Protocol: CBOR over HTTP/2, Unison-Style Sync

**Decision:** Content-addressed code is distributed using a Unison-inspired sync protocol with CBOR payloads over HTTP/2 (or gRPC for streaming). Receivers verify hashes, reject mismatches, and track peer reputation. Capability manifests enable early-reject for incompatible code.

**Transport:**
- HTTP/2 + CBOR payloads for simple request/response
- Optional gRPC streaming for large syncs
- Endpoint: `POST /sync` with CBOR request/response

**Wire format (CBOR):**
```
Chunk = {
  1: hash,           ; SHA-256 of canonical content
  2: type,           ; 1=method, 2=class, 3=module
  3: content,        ; CBOR-encoded hashing AST
  4: ?dependencies,  ; array of hashes this chunk references
  5: ?capabilities   ; array of capability names required
}

SyncRequest = {
  1: have,   ; hashes the requester already has
  2: want    ; hashes the requester needs
}

SyncResponse = {
  1: chunks  ; array of Chunk
}
```

**Sync protocol (Unison-inspired):**
1. Sender announces intent to ship code (hash + capability manifest)
2. Receiver checks capability manifest against local policy — early reject if incompatible
3. Receiver responds with `have` (hashes it already has) and `want` (hashes it needs)
4. Sender streams missing chunks
5. Receiver verifies each chunk's hash matches content, stores valid chunks
6. Receiver caches chunks for future syncs

**Hash verification (collision/disagreement handling):**
```go
func (node *Node) ReceiveChunk(hash Hash, data []byte) error {
    computed := sha256.Sum256(data)
    if computed != hash {
        node.RecordMismatch(sender)  // Track for reputation
        return fmt.Errorf("hash mismatch: expected %x, got %x", hash, computed)
    }
    node.Store(hash, data)
    return nil
}
```

- SHA-256 collisions are astronomically unlikely (~2^128 work to find one)
- Mismatch means corruption or malicious sender
- Reject and don't store; log the incident

**Peer reputation:**
```go
type PeerReputation struct {
    SuccessfulSyncs int
    FailedSyncs     int
    HashMismatches  int
    LastSeen        time.Time
}

func (node *Node) SelectPeer(peers []Peer) Peer {
    // Prefer peers with high success rate, deprioritize unreliable ones
    sort.Slice(peers, func(i, j int) bool {
        return peers[i].Reputation.Score() > peers[j].Reputation.Score()
    })
    return peers[0]
}
```

- Track successful vs failed syncs per peer
- Deprioritize peers with hash mismatches (possible corruption or malice)
- Optional: share reputation scores between trusted nodes

**Capability negotiation:**
```
CapabilityManifest = {
  1: hash,       ; root hash of the module/computation
  2: requires,   ; ["File", "HTTP", "Process", ...]
  3: provides    ; ["Redis::Client", "Redis::Connection", ...]
}
```

Receiver checks before accepting:
```go
func (node *Node) AcceptModule(manifest Manifest) bool {
    for _, cap := range manifest.Requires {
        if !node.Policy.Allows(cap) {
            return false  // Early reject
        }
    }
    return true
}
```

- Ties into process-level restriction (Decision 10): if code requires `File` but the process has `File` removed from Globals, it won't work anyway
- Capability negotiation is an early-reject optimization — fail fast before transferring bytes
- `provides` field enables discovery: "what modules on this node can give me Redis access?"

**Why CBOR:**
- Deterministic encoding (RFC 8949 §4.2) aligns with content-addressing
- Schema-less: methods, classes, modules have different shapes
- Compact binary format
- CBOR tags distinguish chunk types (stable identifiers like AST tag bytes)
- Go library: `github.com/fxamacker/cbor/v2` (fast, supports deterministic mode)

---

## What's Already Implemented

| Layer | Status |
|-------|--------|
| Parser (`namespace:`, `import:`) | Done |
| VM class registration with namespaces | Done |
| `LookupWithImports` resolution order | Done |
| Namespace derivation from directories | Done |
| Manifest parsing (`maggie.toml`) | Done |
| Dependency resolver (git + path deps) | Done |
| Lock file generation | Done |
| Image serialization with namespace table | Done |
| File I/O with namespace preservation | Done |

---

## Open Questions

### Content-Addressing
1. ~~**Granularity:**~~ Decided: layered (method → class → module), like Git.
2. ~~**Canonical AST serialization format:**~~ Decided: Unison-inspired frozen hashing types, tag-byte token stream, Merkle tree, version-prefixed. See Decision 7. **Remaining:** exact tag byte assignments per AST node type.
3. ~~**Hash algorithm:**~~ Decided: SHA-256.
4. ~~**Storage:**~~ Decided: in-image initially, on-disk store later.
5. ~~**De Bruijn indices:**~~ Decided: yes, from day one. The hashing AST normalizes local variable references to `(scope_depth, slot_index)` pairs. Avoids a hash-breaking migration later. See Decision 11.

### Process-Level Restriction
6. ~~**Environment interface:**~~ Decided: per-process Globals map (Lua `_ENV` style). See Decision 10. No new Go interface needed — the existing `map[string]Value` on the Interpreter is the hook.
7. ~~**Compile-time vs runtime resolution:**~~ Resolved: Maggie already does runtime resolution. `OpPushGlobal` does a map lookup on every execution. No change needed — the system is already late-bound. See Decision 10.
8. ~~**Inheritance:**~~ Decided: yes, child processes inherit the parent's (possibly restricted) Globals map. See Decision 10.

### Import Syntax & Dependencies
9. ~~**How do maggie.toml dependencies map to importable namespaces?**~~ Decided: producer declares namespace in their `maggie.toml`, consumer can override, hard error on collision. See Decision 10.
10. ~~**Qualified vs unqualified access:**~~ Decided: both work. Qualified names pass through; unqualified names require `import:` and are resolved to FQN at compile time. See Decision 7. **Implementation gap:** the compiler currently does not resolve class references in method bodies — it passes raw names to `OpPushGlobal`. Needs fix.
11. ~~**Circular imports:**~~ Decided: allowed, no special handling needed. Two-pass loading (register all class names first, compile method bodies second) makes load order irrelevant. See Decision 12.

### Go Interop
12. ~~**FFI boundary:**~~ Decided: Go primitives map to Maggie primitives; Go structs become opaque handles via marker-based IDs in ObjectRegistry. See Decision 13.
13. ~~**Error model:**~~ Decided: Go `(T, error)` returns become Maggie Result monad; Go panics are caught and converted to Failure. See Decision 13.
14. ~~**Concurrency model:**~~ Decided: Go goroutines and Maggie processes are separate; bridging is explicit wrapper code. Channels can be exposed via registry pattern. See Decision 13.
15. ~~**Generation:**~~ Decided: hybrid stub generation. `mag wrap` generates boilerplate; wrapper authors implement TODOs; consumers use `mag build` for custom binary. See Decision 13.

### Distribution
16. ~~**Transport:**~~ Decided: HTTP/2 + CBOR payloads (or gRPC for streaming). Unison-inspired sync protocol. See Decision 14.
17. ~~**Version conflicts:**~~ Decided: verify hash on receive, reject on mismatch, track peer reputation. See Decision 14.
18. ~~**Capability negotiation:**~~ Decided: CBOR manifest with `requires`/`provides` arrays, checked before transfer. Early-reject for incompatible code. See Decision 14.

---

## Approaches Considered & Rejected

### Pure Object-Capability (Newspeak-style Parameterized Modules)
Modules have no access to globals; all dependencies injected as constructor parameters. Maximum security but "a slippery slope to a much less ergonomic-feeling language." Rejected in favor of process-level restriction.

### Capability-Aware Imports (Gatekeeper Layer)
Keep `import:` syntax but add a gatekeeper that can restrict imports in sandboxed contexts. Rejected as "security theater" — enforcement-based restriction over a still-global class table is not structurally sound.

### Capability Objects (Authority from Objects, Not Names)
System classes (File, HTTP) require injected capability objects to function. More ocap-pure than name restriction, but requires restructuring all system classes around capability injection. Rejected as too ergonomically costly for the benefit.

---

## Next Steps

### Decided (ready for implementation planning)
- Content-addressing granularity, hashing target, normalization rules, storage location
- Canonical AST serialization approach (Unison-inspired frozen hashing types, tag-byte token stream, Merkle tree, version-prefixed) — **remaining:** enumerate AST node types and assign tag bytes
- Qualified + unqualified access (both work; compiler resolves to FQN at compile time; `import:` required for unqualified)
- Dependency → namespace mapping (producer-declared, consumer-overridable, hard error on collision)
- Process-level restriction mechanism (Lua-style per-process Globals map with COW overlay)
- Circular imports allowed; two-pass loading makes load order irrelevant
- De Bruijn indices in hashing AST from day one (avoids hash-breaking migration)
- Go interop via hybrid stub generation (`mag wrap`), dependency-driven build (`mag build`)
- Distribution protocol: CBOR over HTTP/2, Unison-style sync, capability negotiation, peer reputation

### Still needs design exploration
1. **Tag byte assignments** — enumerate every AST node type and assign stable uint8 discriminators. Mechanical but must be done carefully.

### Implementation bugs to fix
1. **Compiler does not resolve class refs to FQN in method bodies.** `codegen.go:459` emits raw names. Needs to call `LookupWithImports` (or equivalent) during compilation, using the file's namespace and imports. Without this fix, `import:` has no effect on method bodies — unqualified names work only by accident via short-name registration in Globals.
2. **Short-name Globals registration should be removed.** `main.go:820` registers every class under its short name, causing silent last-loaded-wins collisions. Once the compiler emits FQNs, only the FQN entry is needed.
3. **Loading pipeline is single-pass.** `compileSourceFile` registers classes and compiles methods in one pass per file. Must be split into two passes (register all class skeletons first, compile all method bodies second) so that compile-time FQN resolution works regardless of file order. See Decision 12. This is a prerequisite for bug #1.

### Implementation order (tentative)
The content-addressing infrastructure is foundational — it should be built early because the image format, class table, and compiler all need to know about hashes. The compiler FQN resolution fix is a prerequisite for correct namespace behavior and should land early. The dependency namespace mapping is largely wiring on top of existing infrastructure. Process-level restriction is a small change (per-process Globals map) that can land whenever convenient. Distribution layers on top of content-addressing.

1. **Two-pass loading + compiler FQN resolution** (split loading into skeleton registration and method compilation; resolve class refs to FQN at compile time; drop short-name Globals)
2. Content-addressing (hashing types, compiler integration, content store in image)
3. Dependency namespace mapping (loader wiring, collision detection, consumer override)
4. Process-level restriction (per-process Globals map, COW overlay)
5. Go interop (`mag wrap` stub generator, `mag build` custom binary compilation)
6. Distribution protocol (CBOR sync, capability negotiation, peer reputation — needs content-addressing first)

Run `/workflows:plan` when ready to begin implementation.
