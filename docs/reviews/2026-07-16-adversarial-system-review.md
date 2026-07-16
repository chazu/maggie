# Adversarial System Review — 2026-07-16

Four parallel deep reviews (VM core, compiler/types/pipeline, server/dist, language design)
plus inline scans. Findings marked **(v)** were empirically reproduced with throwaway tests
during the review; the rest were confirmed by reading the cited code. Post pointer-value
migration (branch `migrate/pointer-value`, merged at `8132b5e`).

**Summary verdict:** the migration's core (Value representation, CAS-published inline
caches, `globalsMu`, `RunIsolated`) is clean and well-documented. The debt concentrates in
three rings around it:

1. A half-deleted **registry layer** that survived as load-bearing-looking but mostly
   functionless shells (leaks, 2^24 hard caps, dead code).
2. **Knowledge duplicated without enforcement** — five AST walkers, three envelope
   builders, two hashing contexts, two statement-splitters, three type vocabularies.
   Every confirmed miscompile/mishash in this review is drift between two copies.
3. A distribution **trust boundary enforced at ~3 of 14 peer-facing RPCs**, with crypto
   that authenticates payload bytes but not routing or freshness.

---

## Part 1 — VM core (`vm/`)

### VM-1. HIGH — Shared heap collections have zero synchronization; process-fatal under the concurrent server
`vm/dictionary_primitives.go:9-12` (`DictionaryObject{Data, Keys map[uint64]Value}`) and
`vm/arraylist.go:10-12` (`ArrayListObject{elements []Value}`) have no locks; primitives
mutate them bare (`dict.Data[h] = value` at dictionary_primitives.go:98-100). Stage 5 made
server requests run concurrently under `DoConcurrent` against shared globals; two requests
writing one global Dictionary is a Go `fatal error: concurrent map writes` — uncatchable,
kills the whole server. ArrayList append races corrupt the slice header. Cell got an
RWMutex and StringObject is immutable, but the two workhorse collections were skipped.
**Fix:** per-object RWMutex (like Cell) or document + enforce confinement for
shared-global collections. Fails `-race` trivially today.

### VM-2. HIGH — Futures registry is a pure leak: swept by nobody, capped at 2^24 forever
`vm/concurrency_registry.go:330-341` — `RegisterFuture` inserts every future into
`cr.futures` "for GC root enumeration / sweeping", but there is no root enumeration
anymore, `SweepFutures` (line 359) has zero callers, and `RegistryGC.sweep`
(`vm/registry_gc.go:199-205`) only sweeps channels+processes. Every FutureObject ever
created is pinned until process exit, and `allocConcurrencyID` makes future creation
return `ErrIDSpaceExhausted` permanently after 16.7M futures.
**Fix:** delete the map and counter — future Values are pointer-carrying (`GetFuture`
reads `v.ptr`, lines 344-349); the map serves nothing.

### VM-3. HIGH — `typed_registry.go` is 378 lines of dead code
`vm/typed_registry.go` — `TypedRegistry`/`AutoIDRegistry` (free-lists, pressure hooks, ID
ceilings) has zero production users; only remaining references are its own test and a
false comment at object_registry.go:20. Project memory already claims it was deleted by
the migration; it wasn't. **Fix:** delete file + test.

### VM-4. MED-HIGH — Channels id map: vestigial 24-bit counter that will hard-fail, plus a GC-defeating pin
`vm/concurrency_registry.go:105-152` — `RegisterChannel` still burns `allocConcurrencyID`
(permanent error at 2^24 allocations; the file's own comment estimates ~9 minutes at
30k/sec) even though channel Values are now pointers and the id is only a map key. The
map's sole consumers are `ChannelCount()` stats and `SweepChannels` — the sweeper exists
only to clean a map that exists only to be swept. Every *unclosed* unreachable channel is
pinned forever, defeating the migration's point. Same circularity infects `RegistryGC`
(registry_gc.go), now a background goroutine + `debug.FreeOSMemory` wrapper justified by
maps that could mostly be deleted.
**Fix:** drop the channels map and counter; keep only the process map (see VM-5); move
the scavenger to its own small type.

### VM-5. MED-HIGH — Process values are half-migrated: symbol-encoded ids, 24-bit cap, no recycling
`vm/markers.go:70-72` ("Process remains symbol-encoded"), `concurrency_registry.go:159-204`.
`processToValue(proc.id)` + `GetProcess` map lookup means (a) every process op takes
`processesMu`, (b) `CreateProcess` fails permanently after 16.7M spawns — days of uptime
for an Erlang-style spawn-per-request design the language explicitly markets (Supervisor,
Cluster). **Fix:** finish the migration — make Process a pointer-carrying kind like
Channel/Mutex/Future; keep only the name registry.

### VM-6. MED-HIGH — Wire-format struct duplicated by hand between `vm` and `vm/dist`
`vm/node_primitives.go:77-133` — `envelopeData` re-declares `dist.MessageEnvelope`'s CBOR
`keyasint` field numbers AND re-implements the signature payload layout
(`payload||nonce||targetProcess`) that `dist.MessageEnvelope.signaturePayload`
(`vm/dist/message.go:36`) must verify byte-for-byte. Self-described "temporary solution
until the package structure is refactored." One field added on either side = silent
signature-verification failure across nodes. (See also SD-10 — it's actually
*triplicated*.) **Fix:** extract wire structs into a leaf package (`vm/wire`) importable
by both.

### VM-7. MED — Data race on `vm.localIdentity` lazy init
`vm/node_primitives.go:164-175` writes `vm.localIdentity` with no lock; `vm/serial.go:344,685`
read it on serializer/deserializer paths running on concurrent request goroutines. Two
concurrent `Node connect:` calls can mint two *different* ephemeral node identities
(breaking reply routing) besides failing `-race`.
**Fix:** `sync.OnceValue` or `atomic.Pointer` like the other late-bound fields.

### VM-8. MED — `dispatch.go` is a contradictory second concurrency regime built on false premises
`vm/dispatch.go:6-7` still says "The Maggie VM is single-threaded by design — its internal
data structures (VTable, keepAlive, Globals, etc.) are not safe for concurrent access" —
every clause now false. Its only consumer is the HTTP-server primitive
(`http_primitives.go:297,439,476`), whose handlers reach the **shared main interpreter**
via the `currentInterpreter()` fallback — safe only because the goroutine that called
`server start` happens to be parked in `ListenAndServe`. If the server is started from a
`fork`ed block, two goroutines share one interpreter stack.
**Fix:** route HTTP handlers through `RunIsolated` like server requests; delete the
dispatcher.

### VM-9. MED — `currentInterpreter()` silently hands the shared main interpreter to any unregistered goroutine
`vm/vm.go:1416-1433` — the fallback (`return vm.interpreter`) means any Go callback/timer
goroutine invoking `vm.Send` mutates the main interpreter's stack/frames with no ownership
check — corruption instead of a diagnosable error.
**Fix:** register the main goroutine at NewVM and make the fallback panic (or debug-mode
assertion at minimum).

### VM-10. MED — Dead collector scaffolding on the hottest path in the VM
`vm/interpreter.go:1169-1190` — the `savedSP := i.sp; i.sp += argc + 1 … i.sp = savedSP`
dance exists solely to "re-expose roots" to a tracing collector that was deleted. Go's GC
traces the stack slice's whole backing array regardless of `sp`. Every primitive
invocation carries this vestigial sp mutation and a 20-line justification of dead
machinery. **Fix:** remove (verify nested block-eval frame placement with the test suite
first).

### VM-11. MED — Actively dangerous stale doc comments mandating deleted APIs
`vm/class.go:140-152` — `NewInstance`'s "GC note" tells callers they **MUST** call
`vm.KeepAlive` (deleted symbol) to avoid crashes that are now impossible. Also stale:
`concurrency.go:277-280` (collector safepoint), `http_primitives.go:139-147` (whole
`extractHTTPResult` contract justified by string-collector sweeps),
`http_primitives.go:477-478`, `extension.go:12`, `object_registry.go:20`.
**Fix:** one sweep-and-delete pass.

### VM-12. MED — `ObjectRegistry`/`ConcurrencyRegistry` are vestigial indirection with lying names and dead error returns
`vm/object_registry.go` — four section headers claim "delegates to AutoIDRegistry"
(false); nearly every method ignores its receiver and one-lines to `makeHeap`/a cast.
`RegisterMutex/WaitGroup/Semaphore/CancellationContext/ArrayList`
(`concurrency_registry.go:236,253,270,288,379`) keep `(Value, error)` signatures where err
is provably always nil, so ~40 call sites carry dead error handling; `NewCell`
(`value.go:347`) takes an unused registry param.
**Fix:** flatten to package-level constructors, drop the error returns, keep
`ObjectRegistry` only for cells/classVars/process-names that genuinely need shared state.

### VM-13. MED — Dictionary is hash-only with no collision handling
`vm/dictionary_primitives.go:31-101` — keys are stored purely as their 64-bit FNV-1a hash
(`dict.Data[h] = value`); key equality is never checked on lookup, so two distinct keys
with colliding hashes silently alias/overwrite each other. FNV-1a-64 collisions are
adversarially constructible. Not migration debt, but it's the core map type of the
language. **Fix:** bucket entries `map[uint64][]entry` with an equality probe.

### VM-14. LOW — `SourceMayMutateSchema` gate is a manually-synced string list
`vm/schema_mutation.go:19-33` — "Keep in sync with the selectors registered in
compiler_primitives.go and class_reflection_primitives.go." A new mutating primitive
nobody adds here runs schema mutation under the server's shared read gate (consistency
gap only — memory safety holds). See also SD-9 for the `X := 42` syntax gap.
**Fix:** register mutating selectors at primitive-registration time so the list can't
drift.

### VM-15. LOW — Grow-only reverse indexes pin distribution objects forever
`vm/node_primitives.go:150-156` (`vm.nodeRefs` — no delete anywhere) and
`vm/remote_channel.go:68-73` (`remoteChannelRegistry.track` — no removal path) accumulate
one entry per connection/proxy for the process lifetime. Related: `signalRemoteError`
(`future_primitives.go:141-148`) deliberately discards errors returning Nil, and spawn
failures `Printf` to stdout (`remote_spawn.go:274-277`) despite `SignalPrimitiveError`
existing and being used elsewhere.

---

## Part 2 — Compiler, hash, types, pipeline

### C-1. HIGH (v) — Sync verification hashes with the wrong parser entry point; every pipeline-compiled chunk fails verification
`cmd/mag/main.go:743-754` (`buildCompileFunc`) verifies sync chunks via
`compiler.NewParser(source).ParseMethod()` with nil instVars and nil resolveGlobal. But
chunk content is `CompiledMethod.Source` (`vm/dist/chunker.go:15`), which for
pipeline-compiled methods is the `SourceText` captured from the `method:` keyword onward
(`compiler/parser.go:1461`). Reproduced: `ParseMethod` on
`"method: bump [ count := count + 1. ^count ]"` parses a method whose selector is
`method:` with parameter `bump` — pipeline hash `431d9ec7…` vs verifier hash `cea7f3d4…`.
Three independent divergences: wrong entry point (should be `ParseMethodDef`), nil
instVars (ivars hash as `hGlobalRef` instead of `hInstanceVarRef`), nil FQN resolver.
Consequence: `VerifyChunkMethod` (`vm/dist/wire.go:123`, called at
`server/sync_service.go:144`) rejects legitimate code, and with auto-ban after 3 hash
mismatches, **honest peers get banned**.
**Fix:** verifier must reuse `pipeline.hashAndSetMethod`'s exact path (ParseMethodDef +
class-context instVar map + resolver); add a compile → chunk → verify round-trip test.

### C-2. HIGH (v) — Two-pass loader computes `NumSlots` in file order; deep hierarchies in unlucky filename order allocate instances too small
`pipeline/pipeline.go:269-290` (pass 1b) resolves superclasses and recomputes
`ce.class.NumSlots = len(ce.class.AllInstVarNames())` per entry, in file order. If
`C subclass: B` is fixed up before `B subclass: A` (A has ivars), C's chain still ends at
the ObjectClass placeholder, so C misses A's ivars — nothing recomputes it. Reproduced: 3
files (`1_C.mag`, `2_B.mag`, `3_A.mag`) yield `CCC.NumSlots=0` while
`AllInstVarNames=[xx yy]`; pass-2 methods compile ivar indexes 0-1 against instances
allocated with 0 slots (`vm/class.go:153-155`). Silent memory-layout corruption triggered
purely by filename ordering.
**Fix:** topo-sort by superclass before fixup — the correct algorithm already exists in
`pipeline/rehydrate.go:221-311` (`topoSortClasses`) but `CompileAll` doesn't use it. Or
recompute all `NumSlots` in a final pass.

### C-3. HIGH (v) — `Compiler evaluate:` / server eval wraps expressions via a blind textual dot-splitter that corrupts string literals
`vm/compiler_dispatch.go:64-104` finds "the last statement" by scanning backwards for `.`
with only a digit-adjacency check — no awareness of strings, comments, or blocks (its own
comment at line 70 claims a whitespace rule the code never implements). Reproduced:
evaluating `3 + 4. 'x.y'` compiles a method whose string literal is `x. ^y` — the return
caret injected *inside* the user's string. Dots inside blocks similarly misplace the `^`.
Live path for `Compiler evaluate:` (`vm/compiler_primitives.go:39,69,187`),
`server/eval_service.go:99`, and the LSP (`server/lsp.go:624`). Root cause: no "compile
statement sequence" entry point exists (`CompileExpr` parses one expression), so vm/ grew
a duplicate micro-parser.
**Fix:** add `compiler.CompileDoIt(source)` using `ParseStatements`; delete the splitter.

### C-4. HIGH (v) — Inside blocks, instance variables silently shadow same-named method temps — and the content hash disagrees with the emitted code
`compiler/codegen.go:567-624` (`compileVariable`) checks `instVars` (589) *before*
`capturedVars`/`outerTemps` (595-618); same order in `compileAssignment` (690, 697-716).
At method level temps win over ivars; inside a block the ivar wins. Reproduced: for
`foo | v | v := 42. ^[ v ] value` with ivar `v`, the block bytecode is `21 00`
(OpPushIvar 0) while the method body uses the temp. Worse,
`compiler/hash/normalize.go:225-243` resolves scopes before ivars (and its comment cites a
stale "codegen.go:438-489"), so two semantically different compilations can share one
content hash.
**Fix:** resolve captured/outer scope before ivars in blocks (matching normalize.go), or
make shadowing a compile error.

### C-5. MED-HIGH (v) — BigInteger literals all hash as `0`; distinct methods collide in the content-addressed store
`compiler/hash/normalize.go:106-107` builds `hIntLiteral{Value: e.Value}` and ignores
`IntLiteral.BigValue` (set precisely when `Value` is left 0, `parser.go:725-740`).
Reproduced: three methods returning different 30-digit literals (and `^0`) produce the
same SHA-256 content hash. In a system that dedups, syncs, and *trusts* by content hash,
different code sharing a hash is a correctness/security hole.
**Fix:** add `TagBigIntLiteral` (tags are append-only per `tags.go`) serializing big-int
bytes; bump `HashVersion` if old hashes must invalidate.

### C-6. MED-HIGH (v) — Five hand-rolled AST walkers with confirmed drift: `findCellVariables` misses `DynamicArray`
`compiler/codegen.go:1168-1219` (`findCellVariables.walkExpr`) has no `*DynamicArray` case
while its twin `findCapturedVariables` does (codegen.go:1113-1117). Reproduced:
`foo | x | x := 1. { [ x := 2 ] }. ^x` yields `cellVars={}` vs `{x:true}` for the same
block passed as a message argument — the block captures a copy and `OpStoreCaptured`
writes never reach the method's `x`. Structural problem: five independent switch-based
walkers (codegen.go:1035, codegen.go:1168, semantic.go:152, hash/normalize.go:104,
types/inferrer.go:119) each re-encode the AST shape; every new node type must be added
five times or something silently mis-analyzes.
**Fix:** fix the missing case now; then add a generic `Walk(node, fn)` to `ast.go`, rebase
all five on it, add an exhaustiveness test over AST node types.

### C-7. MED (v) — Cascades on `super` compile as dynamic self-sends — recursion trap
`compiler/codegen.go:833-862` (`compileCascade`) never checks whether `cascade.Receiver`
is `*Super`; all messages use `emitSend`. Reproduced: `super initialize; reset` emits no
`OpSendSuper`. If the subclass overrides `initialize`, this self-dispatches into infinite
recursion. **Fix:** thread an `isSuper` flag through `compileCascade` (normalize.go and
types/inferrer.go treat cascade receivers uniformly, so no hash-format change needed).

### C-8. MED — The semantic analyzer is fully computed and fully discarded
`compiler/codegen.go:1313, 1368, 1387`: `warnings := Analyze(...); _ = warnings` in all
three compile entry points — the undefined-variable warning (`semantic.go:231`) that would
catch "typo silently compiles to a nil global" is dead. Compounding: `warnAt` appends
warnings into the same `errors` slice (`semantic.go:107-111`, no severity separation);
`checkAssignmentTarget` (semantic.go:235-246) is unreachable; `defaultKnownGlobals`
(semantic.go:42-74) is a hand-maintained duplicate of the class table already missing
ArrayList/OrderedCollection/etc.; `Parser.Warnings()` (orphan-docstring detection,
parser.go:89) has zero callers repo-wide.
**Fix:** separate warnings from errors, plumb through `Compile*` returns, surface in
pipeline/mag output, seed known globals from the live ClassTable.

### C-9. MED — `mag typecheck` ignores parse errors and emits file-less diagnostics; type comparison is name-equality with a one-off alias patch
`cmd/mag/typecheck.go:61-67` never checks `p.Errors()` — a file that fails to parse is
silently "checked" against a partial AST. `types.Diagnostic` (`types/checker.go:11-18`)
has no filename, so multi-file runs print positions with no file. The inferrer compares
return types by string name only (`types/inferrer.go:94-102`) — no subtype walk despite
`Class.IsSubclassOf` in `vm/class.go:50`; the `Integer`→`SmallInteger` alias hack
(`return_types.go:36-42`) patches exactly this gap and doesn't generalize. Cross-file
annotation harvest is order-dependent (`checker.go:42-64`).
**Fix:** fail/report on parse errors, add `File` to Diagnostic, use the VM hierarchy for
assignability, harvest annotations in a first pass over all files.

### C-10. MED — types/ triplicates class knowledge that lives in vm/ and lib/
Three hand-maintained vocabularies drift independently of the runtime: `builtinTypes`
(`types/checker.go:124-130` — includes `Integer`/`Nil`/`Block` names that don't match
runtime class names `SmallInteger`/`UndefinedObject`), `populateBuiltins`
(`types/return_types.go:64-134` — ~40 selectors frozen in Go while `lib/*.mag` evolves),
and `GlobalEffects`/`SelectorEffects` (`types/effect.go:61-89`). None are generated or
cross-checked against the image.
**Fix:** derive return-type/effect seed tables from annotations in `lib/*.mag` at
bootstrap (the harvest machinery exists); validate `builtinTypes` against the loaded
ClassTable in a test.

### C-11. MED — Hashing AST has no unknown-node guard — the exact "add a node, silently break hashing" trap
`compiler/hash/normalize.go:214-215` maps any unrecognized expression to `hNilLiteral{}`,
and `serialize.go:82-241`'s switch has no default: a future AST node would hash as `nil`
with no error, making distinct methods hash-equal — precisely the failure mode the
frozen-tag discipline in `tags.go` tries to prevent. Also: `serialize.go:227` hashes
`DocString`, so a doc-only edit changes content identity (defensible, undocumented);
`hBlock.ParamTypes`/`serializeBlockTypes` (`serialize.go:276-287`) is dead scaffolding —
nothing populates block param types and the parser has no syntax for them.
**Fix:** panic/error on unknown node in both normalize and serialize; exhaustiveness test
enumerating `compiler.Expr`/`Stmt` implementations.

### C-12. MED — Zero control-flow inlining; the jump infrastructure is dead weight
No jump opcode is ever emitted by the compiler (grep-verified) — no
`ifTrue:`/`whileTrue:`/`and:` inlining exists, so every conditional and loop allocates a
`BlockMethod` closure and goes through full message dispatch. Meanwhile three layers of
speculative machinery are maintained for never-emitted jumps: interpreter cases
(`vm/interpreter.go:966-1003`), builder labels (`vm/bytecode.go:329-371`), and the
peephole's jump-retargeting/dead-code passes (`compiler/peephole.go:111-150, 219-228`).
For a performance-sensitive VM this is the single biggest codegen win on the table.
**Fix:** inline the standard control-flow selectors (the opcodes are ready) or delete the
jump machinery until then.

### C-13. LOW-MED (v) — Unconditional negative-literal lexing makes `x-1` a two-token puzzle
`compiler/lexer.go:177` treats `-` as a numeric sign whenever a digit follows,
context-free. Confirmed: `3-4` tokenizes as `[INTEGER(3), INTEGER(-4)]`, `x-1` as
`[IDENTIFIER(x), INTEGER(-1)]`. Surfaces as bewildering errors far from the real problem;
classic Smalltalks restrict sign-minus to post-operator/open-delimiter positions.
**Fix:** track the previous significant token in the lexer, or have the parser fuse
`expr INTEGER(-n)` into a "did you mean `x - 1`?" diagnostic.

### C-14. LOW — Type-annotation close-`>` matching swallows compound operators
`compiler/parser.go:303-310` closes `<Type` on any binary selector *starting* with `>` and
consumes the whole token — a `>=`/`>>` following an annotation is eaten whole, silently
deleting the `=`/`>` operator from the program. The identical hack is duplicated in
`parseEffectAnnotations` (parser.go:358-363).
**Fix:** split the token (re-lex the remainder) or reject compound closers with a clear
error.

### C-15. LOW — Grab-bag of smaller confirmed debt
(a) Parser errors carry line but never column or file (`parser.go:73`); codegen's
`errorAt` does emit columns — inconsistent. (b) `parse_method.go:41,47` formats errors
with `%v` on `[]string` — the ugliness `formatErrors` (codegen.go:1297) was written to
fix. (c) `pipeline.go:519-613` vs `616-710`: `LoadProject`/`LoadTarget` are ~95-line
copy-paste twins. (d) `pipeline.go:226-262`: an un-namespaced user class whose name
collides with any image class is silently treated as "extending a core class" and pass 1b
(284-286) can re-parent the core class's vtables per the user's declaration.
(e) `rehydrate.go:316` `verifyMethodHash` is unused. (f) `token.go:23` documents `$\n`
escapes; `lexer.go:468-479` has no escape handling — `$\` is the backslash character.
(g) `CompileExpression` (codegen.go:268-299) never runs `findCellVariables`, so blocks
with mutable captures compile differently through that entry point than through
`CompileMethod` (latent — the live eval path wraps into a `doIt` method).

---

## Part 3 — Server + distribution (`server/`, `vm/dist/`, distributed vm/)

### SD-1. HIGH — Envelope signature doesn't cover routing fields, and nonces are never checked
`vm/dist/message.go:36-46` — signed bytes are only `payload || nonce || targetProcess`.
`Selector`, `TargetName`, `ReplyTo`, `ClassHints` are unsigned, and `buildSignedEnvelope`
(`vm/node_primitives.go:109-133`) never sets `TargetProcess`, so the actual routing target
(`TargetName`) is entirely unsigned. Anyone on-path can redirect a valid signed message to
a different process or rewrite `Selector` to `__spawn_result__`/`__down__` (routed at
`server/sync_service.go:393-397`) to spoof spawn results and DOWN notifications. No
receiver anywhere tracks nonces (grep-confirmed: only construction exists), so any
captured envelope replays indefinitely. The doc comment at message.go:13-16 claims replay
and redirection prevention — both claims are false.
**Fix:** sign the full envelope minus signature (canonical CBOR); per-peer last-seen-nonce
in TrustStore.

### SD-2. HIGH — Auto-ban can be weaponized: frame a victim peer into a ban with 3 garbage messages
`server/sync_service.go:382` and `:615` call `RecordHashMismatch(peerID)` on signature
failure, where `peerID` is the envelope's *self-declared* `SenderNode` (`:373`, `:599`).
Attacker sets `SenderNode` = victim's public key with a garbage signature; three messages
ban the victim (`vm/dist/trust.go:214-226`, threshold 3). On the sync path,
`peerNodeIDFromRequest` (`sync_service.go:870-887`) trusts unauthenticated
`X-Maggie-Node-ID`/`X-Forwarded-For` headers, so `Transfer`'s mismatch recording (`:147`,
`:159`) can frame anyone; rotating IDs evades bans while `getOrCreate`
(`trust.go:284-294`) grows one `PeerRecord` per spoofed ID — unbounded memory.
**Fix:** only count mismatches against identities proven by a valid signature; never
against header-asserted IDs.

### SD-3. HIGH — Permission checks are wildly inconsistent across the RPC surface
Per-endpoint audit of `server/sync_service.go`: `Announce` checks `PermSync` (`:68`);
`Transfer` (`:116`) and `Serve` (`:218`) check only `IsBanned` — a peer with zero
permissions can push chunks and pull the entire transitive code closure. `Ping` (`:291`),
`Resolve` (`:302`), `List` (`:325`), `MonitorProcess` (`:540`), `DemonitorProcess`
(`:571`), and all six Channel RPCs (`:715-865`) have **no trust check at all, not even
IsBanned** — anyone who can reach a peer-mounted endpoint can enumerate content,
install/cancel monitors by guessing ref IDs, and drain/close/inject into any exported
channel (`ChannelReceive` at `:746` returns serialized values — data exfiltration).
`server.go:83` even documents this.
**Fix:** one auth interceptor deriving peer identity once, with a per-RPC required-perm
table; deny by default.

### SD-4. HIGH — Backref index misalignment between serializer and deserializer corrupts shared-object graphs
Serializer registers every object — Arrays included — in the backref map before dispatch
(`vm/serial.go:242-256`). The deserializer assigns backref indices only in
`deserializeObject` (`:594-596`); Arrays decode through `fromInterface`'s
`[]interface{}` case (`:473-487`) with no registration. Any payload containing an Array
before a shared object shifts all subsequent backref indices: backrefs resolve to the
*wrong* object silently, or fail "backreference not found". A self-referential Array
cannot round-trip at all.
**Fix:** assign deserializer ref indices for arrays in the same traversal order — or
encode the index explicitly in the object tag.

### SD-5. HIGH — Unbounded recursion in both serializer and deserializer = fatal crash
Serializer: cycle detection (`seen`) exists only for Objects; `serializeDictionary`
(`vm/serial.go:303-325`) recurses into values with no cycle check — a Dictionary
containing itself infinite-loops into a fatal Go stack overflow. Deserializer: each object
slot is an independently-encoded CBOR document (`Slots [][]byte`, `:56`), so fxamacker's
per-document nesting limit never triggers; `deserialize → deserializeObject → deserialize`
(`:599-607`) recursion depth is attacker-controlled. A peer with only `PermMessage` can
crash the node with one deep `DeliverMessage` payload (`sync_service.go:437`).
**Fix:** add dicts to `seen`; explicit depth counter in `valueDeserializer`.

### SD-6. HIGH — forkOn: futures leak forever on remote node death; node-death channel cleanup is dead code
`doForkOn` registers the future in `pendingSpawns` (`vm/remote_spawn.go:293`), removed
only when `__spawn_result__` arrives. `handleNodeDown`
(`vm/remote_lifecycle.go:161-181`) drains monitors and links but never touches
`pendingSpawns`, and `DrainRemoteChannels` (`vm/remote_channel.go:185`) has zero callers
(grep-verified). The health monitor only starts when a *monitor* is created
(`remote_lifecycle.go:91`) — a plain `forkOn:` gets no heartbeat coverage. Remote node
dies after accepting the spawn → `Future wait` blocks the Maggie process forever. Also:
`ensureHealthMonitor`'s nil-check (`:185-189`) is unsynchronized — racy double-create.
**Fix:** index pending spawns and remote channels by NodeID, resolve/close them in
`handleNodeDown`, `ensureHealthMonitor` on spawn/channel creation.

### SD-7. HIGH — Channel RPCs block forever, ignoring context: free goroutine-exhaustion DoS
`SyncService.ChannelSend`/`ChannelReceive` (`server/sync_service.go:735`, `:758`) call
`SafeSend`/`Receive` (`vm/concurrency.go:96`, `:102-105`) which block indefinitely on a
full/empty channel; `ctx` is never consulted, so client disconnects don't free the handler
goroutine. Combined with SD-3 (no auth), an anonymous caller can pin unbounded goroutines.
**Fix:** select against `ctx.Done()` (needs ctx-aware send/receive on ChannelObject) or
bounded-wait with a "retry" status.

### SD-8. MED — The Do/DoConcurrent gate is a partial fiction: the entire sync surface bypasses it
Every `SyncService` handler reaches through `s.worker.vm` directly (`sync_service.go:416`,
`:437`, `:652`, `:719`, …) — never `Do`/`DoConcurrent` — so deserialization, mailbox
delivery, and above all `ExecuteSpawnBlock` (arbitrary remote code, `:652`) run wholly
outside the gate. Locally forked Maggie processes also run outside it by construction. So
`Do`'s exclusivity (the `saveImage:` consistency story, `vm_worker.go:19-27`) holds only
against sibling RPC threads. It also violates `VMWorker.VM()`'s stated "read-only
metadata" contract (`vm_worker.go:116-119`) and skips `run()`'s panic-to-error recovery —
a panic in `DeliverMessage` deserialization propagates raw into connect. The split IS
principled for the IDE services (all 5 `modify_service.go` handlers use `Do`, all readers
`DoConcurrent` — call-site audit), but the boundary of what the gate governs is
undeclared. **Fix:** route sync handlers through `DoConcurrent` (they're non-structural)
or document the gate as "IDE-request coordination only" and stop implying image-save
exclusivity.

### SD-9. MED — `DoForSource` misses the most ordinary global write: `X := 42`
`SourceMayMutateSchema` (`vm/schema_mutation.go:22-33`) matches only reflective selector
heads (`setGlobal:`, `evaluate:`, …). A plain assignment to a global in an eval compiles
to a global write yet classifies as SHARED — while `vm_worker.go:20-22,61` explicitly
claims `Do` covers "write globals". The file's own comment (`:52-58`) discloses the
indirect-mutation gap but not the `:=` syntax gap; `server/eval_global_persist_test.go`
proves eval global writes are a supported path. Memory-safe (guarded map) but the
documented contract is wrong.
**Fix:** have the doIt compiler report "compiled a global-store opcode" and gate on that
instead of text matching.

### SD-10. MED — Envelope building is triplicated across two packages
Quantified: `envelopeData` + `replyAddr` + `buildSignedEnvelope`
(`vm/node_primitives.go:86-133`, ~48 lines) and `buildSignedEnvelopeForProcess`
(`vm/remote_lifecycle.go:228-248`, ~21 lines) duplicate `dist.MessageEnvelope` +
`signaturePayload` + `Sign` (`vm/dist/message.go:17-68`). Three copies of the CBOR
`keyasint` 1–9 field layout and three copies of the signature-byte construction must stay
bit-identical or nodes stop interoperating — and the fix for SD-1 must land in all three
or you get a silent protocol fork. Self-labeled temporary (`node_primitives.go:82`).
**Fix:** extract leaf package `vm/wire` (envelope struct + sign/verify only, no vm or dist
imports); the cycle only exists because the struct lives in `dist`.

### SD-11. MED — Distribution registries only ever grow; exported channels are pinned against GC forever
`channelExportRegistry` has `Export`/`Lookup` but no unexport
(`vm/remote_channel.go:107-131`; no `delete` on `exports`/`byChan`), and **serializing a
channel exports it as a side effect** (`serial.go:334-354`) — any channel that ever
crossed the wire is strongly referenced for the life of the VM, directly undermining the
migration's "Go GC reclaims heap objects" story. Same pattern: `vm.nodeRefs`
(`node_primitives.go:151`) and `remoteChannels.track` (`remote_channel.go:68`).
**Fix:** lease-based exports (expire on owner-side close/idle); delete node refs on
disconnect/node-down.

### SD-12. MED — No wire versioning; version skew fails silently instead of loudly
Serialized values and envelopes carry no version field (contrast: the image format's
versioned envelope). Unknown CBOR tags fall through to `fromInterface(tag.Content)`
(`serial.go:560-566`) — a newer node's new type arrives at an older node as a naked
map/array, semantics stripped, no error. Class identity claims "semantic hash is the
identity key" (`serial.go:15`) but `lookupClass` tries name first (`:613-625`), and shape
mismatch is silently truncated (`:599-601` `break` on excess slots) — skewed class
definitions across nodes corrupt object state without a diagnostic.
**Fix:** version byte in the envelope, hash-first class lookup with explicit shape check,
hard error on unknown tags.

### SD-13. MED — `TrustStore.CheckCapabilities` is security theater
`vm/dist/trust.go:231-247`: the inner loop's only statement is `break`; the function
unconditionally returns nil. Yet `Announce` invokes it as a rejection gate
(`sync_service.go:81-86`), and readers will assume manifests are enforced.
**Fix:** delete the function and the call, or implement it — the current state is worse
than either.

### SD-14. LOW-MED — Error propagation is stringly, lossy, and swallowed at the primitive layer
`ErrorKind` is a free-form string duplicated between a comment (`message.go:82`) and ~10
literals in `sync_service.go`; no enum in the proto. Nearly every distributed failure
collapses to `Nil` on the Maggie side: `doRemoteSpawn` prints to stdout and returns Nil
(`remote_spawn.go:275-277`, the acknowledged TODO), `remoteSend` returns Nil on
serialize/envelope errors (`node_primitives.go:368-374`), and `RemoteChannel>>receive`
returns Nil identically for closed / network error / deserialize error /
legitimately-sent-nil (`remote_channel.go:227-236`). Also two independent decodings of
process-ID-from-Value: `procVal.SymbolID() & ^uint32(0xFF<<24)` (`sync_service.go:424`,
`:680`) vs `markedIDFromValue` (`remote_lifecycle.go:114`).
**Fix:** error-kind enum in proto; raise Maggie exceptions from the primitives
(`SignalPrimitiveError` already exists and is used at `remote_spawn.go:291`); one exported
PID decoder.

### SD-15. LOW — Reputation/ban bookkeeping quirks
`DeliverMessage` calls `RecordSuccess` (`sync_service.go:390`) *before* the `PermMessage`
check (`:405`) — unauthorized peers accrue reputation; `ResolveBlockMethod` falls back to
"first block" on arity/capture mismatch (`remote_spawn.go:192-193`) — silently executing
the wrong block of a multi-block method; `setupResultDelivery` goroutines wait forever on
non-terminating processes (`sync_service.go:686-707`); bans are memory-only, cleared on
restart. Collectively the "3-strike ban" is more a suggestion than a mechanism.

---

## Part 4 — Language design & stdlib (`lib/`, guides, semantics)

### L-1. HIGH (v) — String violates the `hash`/`=` contract
`lib/Object.mag:349` (`hash` primitive, identity-based) + `lib/String.mag:508`
(`hashCode`, content FNV-1a). Verified: `('hello ', 'world') = 'hello world'` → `true`,
but `a hash = b hash` → `false`. String never overrides `hash` with content hashing; the
content hash hides under the Java-named `hashCode`. The built-in Dictionary hashes at the
Go level, which masks this — but any Maggie-level hashed structure keyed on `hash` (the
documented contract: "Keys can be any object that responds to `hash` and `=`",
`lib/Dictionary.mag:3`) silently breaks for the most common key type. `lib/HashRing.mag:136`
already reaches for `hashCode` because `hash` is useless — a smell inside the stdlib.
**Tighten:** make `String>>hash` content-based, delete `hashCode`, keep `identityHash`.

### L-2. HIGH (v) — `forkAt:` / `priority` / `priority:` are phantom API
`lib/Block.mag:156` `forkAt:` sends `Process fork: self at: priority` — no `fork:at:` is
ever registered (only `fork:` at `vm/concurrency.go:623`). Verified: `[1] forkAt: 1`
silently returns nil — no error, no process. `vm/concurrency.go:739-744`: `primPriority`
is a stub returning 0, `primPriority:` a stub returning self, yet `lib/Process.mag:121-136`
documents both as real and `docs/api/classes/Block.html` publishes `forkAt:`. Goroutines
have no priorities; this is Smalltalk-80 cargo cult.
**Tighten:** delete `forkAt:`, `priority`, `priority:` entirely.

### L-3. HIGH — CLAUDE.md documents `forkWithResult`, which does not exist
`grep -r forkWithResult` hits only `CLAUDE.md` and `concurrency.md:1231` — the latter a
design proposal with hypothetical Go code. Never implemented: not in `lib/Block.mag`, not
in `vm/concurrency.go`. The shipped semantics is `fork` treating NLR as local
(`lib/Block.mag:136-138`). Agents and users are being taught a phantom distinction.
**Tighten:** fix CLAUDE.md; implement the variant or excise it.

### L-4. HIGH (v) — Stream is a lie twice over: nil-sentinel truncation and fake laziness
`lib/Stream.mag:5` "nil signals end of stream" means `(Stream over: #(1 nil 2)) toArray`
→ `#(1 )` (verified) — arrays can hold nil, so `asStream` silently truncates valid data.
The class comment sells "Lazy filtering and mapping" while `select:`/`collect:`/`reject:`
(`Stream.mag:129-168`) eagerly drain the entire stream into an Array — the chained example
at line 16 only "works" because `select:` returns an Array and Array has `collect:`. Zero
laziness after the first combinator.
**Tighten:** distinct end-marker object (or `atEnd`-driven protocol) instead of nil;
either make combinators return Streams or delete the "lazy" claim.

### L-5. HIGH — Four incompatible error-reporting conventions, and the guide misstates which one wins
`Guide08ErrorHandling.mag:9`: "Most Maggie code uses the Result pattern." Reality: only
File, Regex, and Cue* use Result (grep). HttpClient returns nil on error throughout
(`lib/HttpClient.mag:5` — the error reason is *discarded*); Sqlite signals exceptions
(`SqliteDatabase.mag:240`) and returns nil for no-rows; `Dictionary>>at:` returns nil for
absent keys (`Dictionary.mag:179`); `Future>>await` returns nil on error with a
side-channel `error` accessor (`Future.mag:19-68`) and never touches the stdlib's own
Result type; `ArrayList>>remove:` raises. A user cannot predict the failure mode of any
API without reading its source.
**Tighten:** pick a doctrine (Result for expected failures, exceptions for programmer
errors — what Guide08 already claims), migrate HttpClient and Future first.

### L-6. HIGH — `File readFileContents:` returns an untagged `String | Failure` union
`vm/file_primitives.go:23-38`: success returns the raw string, failure returns a Failure
object — while `writeFileContents:contents:` uniformly returns Success/Failure.
Consequence: `(File readFileContents: p) isSuccess` DNUs *only on the happy path*, and the
doctest at `File.mag:118` compares the bare string while the header (`File.mag:4-5`)
claims "Methods that can fail return a Result." The class docs contradict the primitive
within the same file.
**Tighten:** return `Success with: contents`, or add `readFileContents:ifAbsent:`.

### L-7. MED — The lib sources lie about the class hierarchy
`lib/Array.mag:19`, `ArrayList.mag:27`, `Dictionary.mag:37`, `Set.mag:27`, `String.mag:21`
all declare `subclass: Object`, but `vm/vm.go:446-454` hardwires them under `Collection` —
which is where their inline comments ("# isEmpty, notEmpty inherited from Collection",
`Array.mag:82`) and all Enumerable behavior actually come from. Anyone learning Maggie by
reading `lib/` builds a wrong model, and a user who imitates the pattern
(`MyColl subclass: Object` + define `do:`/`size`) gets **no** Enumerable methods.
**Tighten:** make the declarations say `subclass: Collection`; have the loader verify
rather than override.

### L-8. MED — Trait/protocol surface is doubled: `uses:` vs `include:`, and `Comparable` names two different things
`compiler/parser.go:1261` accepts both `include:` and `uses:` as synonyms; the stdlib
mixes them (`Collection.mag:6` and `Magnitude.mag:14` use `uses:`, `Array.mag:20` uses
`include:`) while `Guide07Classes.mag:437` teaches only `include:`. Separately,
`Comparable` is simultaneously a trait (`lib/Comparable.mag:12`) and a protocol
(`lib/protocols.mag:26`) — one name, two constructs, different machinery.
**Tighten:** pick one trait keyword, deprecate the other; rename the protocol (e.g.
`Ordered protocol`).

### L-9. MED — The Comparable trait understates its own requirements
`lib/Comparable.mag:2-3`: "Classes using this trait must define `<`." But `max:` sends
`self > other` (line 23), `between:and:` sends `>=` and `<=` (line 49), and the trait
derives none of them. A class that does exactly what the doc says gets a DNU from `max:`.
**Tighten:** derive `>`, `<=`, `>=` from `<` in the trait (Smalltalk-80 Magnitude
approach), or document the real requirement.

### L-10. MED — nil-conflation pervades the concurrency surface with no disambiguating variants
`Channel>>receive` returns nil for closed (`Channel.mag:112-113`); `tryReceive` returns
nil for empty (`:94`) — nil channel *values* are indistinguishable from both.
`Future>>await:` nil = timeout OR error OR nil result. `Process>>result` nil =
not-finished OR nil result (`Process.mag:98`). Go solved this with the ok-flag; Smalltalk
solves it with `ifNone:`/exception variants; Maggie has neither on any of these.
**Tighten:** add `receive:ifClosed:`, `tryReceive:ifEmpty:`; make `Future>>await` signal
on error (it already carries the message).

### L-11. MED — Same feature, two names: process-restriction fork
`Block>>forkRestricted:` (`Block.mag:243`) and `Process class>>forkWithout:do:`
(`Process.mag:382`) are the same capability with different shapes, neither referencing the
other. Guide09 (line 574+) teaches only `forkRestricted:`. Unresolved doctrine: restricted
globals resolve to nil *silently* — combined with L-10, sandboxed code fails as mysterious
nil-DNUs far from the cause.
**Tighten:** keep one selector; consider a catchable `RestrictedGlobal` signal instead of
silent nil (or at least document the silence in the docstring).

### L-12. MED — Process liveness has three overlapping predicates and a misplaced `yield`
`isAlive` (`Process.mag:78`), `isDone` (primitive, `:385`), `isTerminated` (`:93`, defined
as `isAlive not`) — three spellings of one axis, one a primitive duplicate. Instance-side
`method: yield` (`:38`) is documented as "Yield the **current** process's time slice" —
`someProcess yield` yields the *caller*, not the receiver; a receiver/effect mismatch
(class-side `Process yield` at `:379` is the honest one).
**Tighten:** keep `isAlive` + one negation; delete the instance-side `yield`.

### L-13. MED — `onSend:do:` exists in the VM but nowhere in the language surface
`vm/channel_select.go:258` registers `Channel>>onSend:do:`, but `lib/Channel.mag` declares
only `onReceive:` (`:202`), and neither Guide09 nor USER_GUIDE mentions it. Half of Go's
select semantics is shipped but invisible; users will conclude select is receive-only.
**Tighten:** declare it in Channel.mag with a doctest, or remove it.

### L-14. LOW — Naming-discipline leaks accumulating
`hashCode` (Java, see L-1); `Stream>>toArray` (`Stream.mag:298`) vs the `asArray` used
everywhere else; `trimBoth` + `trimSeparators` alias pair (`String.mag:390-397`);
`join:` + `joinWith:` alias pair (`Array.mag:325-341`);
`String>>isDigit/isLetter/isWhitespace` duplicating Character predicates on single-char
*strings* (`String.mag:341-380` — `'ab' isDigit` → false, verified: a quiet multi-char
footgun); `Channel>>nextPut:/next` "stream-compatible aliases" (`Channel.mag:146,161`) for
a Stream class that itself has no `nextPut:`. Collectively they teach users that Maggie
has no naming doctrine.
**Tighten:** one blessed name per concept, aliases deprecated in one release.

### L-15. LOW — Collection protocol matrix holes
Set has no `remove:ifAbsent:` — its `remove:` delegates to `Dictionary removeKey:` which
raises (`vm/set_primitives.go:42-53`), while ArrayList and Dictionary both offer the
`ifAbsent:` form. `ArrayList first/last` return nil on empty (`ArrayList.mag:148-163`)
while `Array first` raises via `at: 1` — the two sequence classes disagree on the empty
case. Dictionary has no `keysAndValuesSelect:`/`associationsDo:`, no key-aware `collect:`.
`Object perform:` stops at one argument (`Object.mag:364-367`) — no `perform:with:with:`
or `perform:withArguments:`, which any metaprogramming (and agent-dispatch routing) needs
immediately. `Object isString` (`Object.mag:361`) is a lone type-test with no
`isNumber`/`isSymbol`/`isCollection` siblings — commit to the family or delete the
one-off.

---

## Inline scan notes

- Only 6 TODO/FIXME markers in ~86k lines of Go — debt is structural, not annotated.
  Notable: `vm/cancellation.go:42` ships `context.TODO()`; `vm/remote_spawn.go:275` and
  `vm/future_primitives.go:144` both defer "raise a real Maggie exception"; `server/lsp.go:934`
  wants `Format()` extracted from cmd/mag to a shared package.
- 50 `panic(` calls in non-test vm/compiler/server code.
- 37 separate `*_primitives.go` files in vm/ — registration sprawl.
- Biggest files: `vm/interpreter.go` (2008), `compiler/parser.go` (1690), `vm/vm.go`
  (1476), `compiler/codegen.go` (1399).

---

## Highest-leverage structural fixes

1. **`vm/wire` leaf package + one auth interceptor + fully-signed versioned envelope** —
   collapses SD-1, SD-2, SD-3, SD-10, SD-12, VM-6 into one design change.

   **DECISION (2026-07-16, locked):** clean wire break, no compat shim (no deployed
   users). New leaf package `vm/wire` (stdlib + cbor + ed25519 only): versioned Envelope,
   Sign/Verify over canonical-CBOR of the whole envelope minus signature; per-peer replay
   window in TrustStore, checked in one connect auth interceptor that derives peer
   identity once and enforces a static per-RPC perm table, deny by default.
   Mismatch/ban recording only against signature-proven identities; anonymous requests
   never create PeerRecords or touch ban bookkeeping. Perm table:
   - `Ping` — anonymous, liveness only (content count moves to authenticated response)
   - `Resolve`, `List`, `Transfer`, `Serve`, `Announce` — authenticated + `PermSync`
   - `DeliverMessage`, `MonitorProcess`, `DemonitorProcess`, all Channel RPCs —
     authenticated + `PermMessage` (no separate channel perm: channels and mailboxes are
     the same data plane over the same envelope machinery; a fourth perm adds config
     surface with no matching distinction in the Go layer)
   - remote spawn (`ExecuteSpawnBlock` path) — authenticated + `PermSpawn`
   Public anonymous discovery, if ever wanted, is an explicit opt-in server flag, not the
   default. SD-4/SD-5 (serializer backrefs/recursion) explicitly out of scope.
2. **Delete the registry ring** — VM-2, VM-3, VM-4, VM-10, VM-11, VM-12 (+ finish VM-5).
   ~1,000+ lines removed and two permanent-failure modes.

   **DECISION (2026-07-16, locked):** one PR on main covering both the pure deletions
   (futures map+counter, typed_registry.go, channels map + allocConcurrencyID, dead
   sp-juggling in interpreter.go:1169-1190, stale KeepAlive/collector docs, ObjectRegistry
   flattening + dead error returns) AND the Process pointer migration (VM-5): local
   processes become pointer-carrying Values (no registry, no lock, no 2^24 cap); wire-
   visible processes get an export table entered only when a PID crosses the boundary
   (same pattern as remote channels), which becomes the single home of the PID codec
   (also retires SD-14's duplicated PID decoding). Name registry stays. RegistryGC and
   its debug.FreeOSMemory timer are DELETED entirely — they were justified by the old
   pinning behavior; reintroduce only with a benchmark showing RSS regression.
   ChannelCount()-style stats become atomic counters or go away.
3. **Generic `ast.Walk` + exhaustiveness test** — rebases the five walkers (C-6), the
   hasher (C-11), and the type inferrer on one traversal.

   **DECISION (2026-07-16, locked):**
   - Fix `findCellVariables`'s missing `*DynamicArray` case immediately with a regression
     test, independent of the refactor.
   - Add `ast.Walk(v Visitor, node Node)` mirroring go/ast (Visit(nil) exit events, so
     scope-tracking walkers can push/pop at block boundaries). `default:` panics on
     unhandled node types.
   - Rebase the two codegen analyzers, the semantic analyzer, and the type inferrer onto
     Walk. The hash normalizer is NOT rebased (it's a transformer; traversal-order changes
     would silently change every content hash) — it only gets panic-on-unknown guards
     replacing the silent `hNilLiteral{}`/missing-default fallbacks.
   - Exhaustiveness test constructs an instance of every Expr/Stmt type and walks it.
   - ONE HashVersion bump bundles the format corrections: add `TagBigIntLiteral` (C-5).
     DocString REMAINS in the hash (deliberate: identity = full source; excluding docs
     creates a which-doc-wins ambiguity in a content-addressed store that keeps one
     artifact per key). Documented as a design decision, no format change needed for it.
4. **Written conventions doctrine + doctest CI** ("every lib selector has a passing
   doctest; every VM-registered selector appears in lib/") — L-2, L-3, L-5, L-6, L-13,
   L-14, L-15 all fail it on day one.

   **DECISION (2026-07-16, locked):**
   - `docs/CONVENTIONS.md`, referenced from CLAUDE.md. Four rulings: (1) failure doctrine
     as Guide08 claims — Result for expected failures, exceptions for programmer errors,
     nil never signals; HttpClient and Future migrate first. (2) nil is only ever a value
     — absence/closure/timeout get `ifAbsent:`/`ifClosed:`/`ifNone:` variants.
     (3) one blessed selector per concept, conversions are `as*`; aliases deprecated in
     one release. (4) selector honesty — every lib selector backed by real implementation,
     every VM-registered selector declared in lib/.
   - CI check A (hard gate, day one): bootstrap-time surface-parity diff between
     VM-registered selectors and lib/-declared selectors, both directions. Kills phantom
     API (forkAt:) and invisible API (onSend:do:) as build failures.
   - CI check B (ratchet): baseline count of doctest-less selectors recorded; PRs may not
     increase it; baseline only decreases. New code held to full standard immediately.
   - `forkRestricted:` silent-nil globals: REJECTED as an exception — restricted global
     access signals a catchable `RestrictedGlobal` instead. Silent nil is too confusing
     (sandboxed code currently fails as nil-DNUs far from the cause).
5. **`compiler.CompileDoIt`** — kills the eval dot-splitter (C-3) and the latent
   `CompileExpression` divergence (C-15g).

   **DECISION (2026-07-16, locked):**
   - `compiler.CompileDoIt(source, resolveGlobal) (method, warnings, error)`: lex once,
     ParseStatements (incl. leading temps), build the doIt method as AST (wrap last
     statement in Return at AST level), run the FULL codegen path (findCellVariables,
     semantic analysis, peephole). No text manipulation.
   - Replace the textual splitter at the single choke point (vm/compiler_dispatch.go);
     Compiler evaluate:, eval_service, LSP inherit the fix. Delete the splitter;
     CompileExpression becomes CompileDoIt of one expression.
   - Warnings plumbing included NOW (first consumer of C-8's discarded diagnostics):
     separate warnings from errors in semantic.go, return them, publish via LSP
     diagnostics. Full C-8 (pipeline/CLI surfacing) stays separate work.

**Fix-this-week candidates:** C-1 (sync verifier bans honest peers) and VM-1 (concurrent
map writes kill the server).
