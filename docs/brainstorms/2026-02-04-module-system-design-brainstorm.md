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

### Content-Addressing (mostly decided — see Decision 2 above)
1. ~~**Granularity:**~~ Decided: layered (method → class → module), like Git.
2. **Canonical AST serialization format:** Need to define the exact binary encoding for hashing. Compact binary with tag bytes per node type, length-prefixed strings, canonical number format. Must be specified precisely so different implementations produce identical hashes.
3. ~~**Hash algorithm:**~~ Decided: SHA-256.
4. ~~**Storage:**~~ Decided: in-image initially, on-disk store later.
5. **De Bruijn indices (future):** Should variable names eventually be eliminated from hashes so `[:x | x + 1]` and `[:y | y + 1]` hash identically? Deferred — worth revisiting for distribution efficiency.

### Process-Level Restriction
5. **Environment interface:** What does the VM hook look like for per-process name resolution? Is it a Go interface? A Maggie-level protocol?
6. **Compile-time vs runtime resolution:** Currently the compiler resolves class names at compile time and embeds direct references. Process-level restriction requires either late resolution or an indirection layer. Which?
7. **Inheritance:** When a restricted process forks a child, does the child inherit the restriction? (Probably yes.)

### Import Syntax & Dependencies
8. **How do maggie.toml dependencies map to importable namespaces?** Is it automatic (dependency name → namespace) or explicit (each dep declares its namespace)?
9. **Qualified vs unqualified access:** Should there be `HTTP::Client` (qualified) alongside `import:` (unqualified)? Or always import first?
10. **Circular imports:** What happens if A imports B and B imports A? (Probably fine — classes exist in the class table; import order shouldn't matter for resolution.)

### Go Interop
11. **FFI boundary:** What types can cross? Maggie Values ↔ Go values? Only primitives? Serialized objects?
12. **Error model:** Go returns `(value, error)`. Maggie uses exceptions. How do these bridge?
13. **Concurrency model:** Go goroutines vs Maggie processes. Can they share channels?
14. **Generation:** Are Go wrappers hand-written, auto-generated from Go type info, or a mix?

### Distribution
15. **Transport:** When shipping code between nodes, what protocol? The existing gRPC/Connect infrastructure?
16. **Version conflicts:** If Node A sends hash:abc123 but Node B already has that hash with different content (collision or disagreement), what happens?
17. **Capability negotiation:** How does a module declare what capabilities it needs, so the receiving node can decide whether to accept it?

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

### Still needs design exploration
1. **Canonical AST serialization format** — the exact binary encoding that gets hashed. Must be specified precisely.
2. **Import syntax and maggie.toml dependency mapping** — how deps become importable namespaces, qualified vs unqualified access
3. **Go interop FFI design** — the boundary, what types cross, error model bridging
4. **Process environment VM hook** — runtime interface for per-process name resolution, compile-time vs runtime class resolution
5. **Distribution protocol** — how content-addressed definitions are shipped between nodes

### Implementation order (tentative)
The content-addressing infrastructure is foundational — it should be built early because the image format, class table, and compiler all need to know about hashes. Process-level restriction and distribution can layer on top.

Run `/workflows:plan` when design is sufficiently settled to begin implementation.
