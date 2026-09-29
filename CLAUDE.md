## PUDL Observation Collection

Record notable observations with `pudl facts observe` before your session ends.

```
pudl facts observe "<one-sentence description>" --kind <kind> --scope <repo:path> [--source <agent-name>]
```

`--scope` takes a `repo:path` value (e.g. `maggie:vm/heap_gc.go`). Kinds: fact, obstacle, pattern, antipattern, suggestion, bug, opportunity. You MUST post at least one observation before your work is considered complete.

---

## Language Reference

Maggie is a Smalltalk-family language VM written in Go. For full API docs:

- **User Guide:** `docs/USER_GUIDE.md`
- **Guided tutorials:** `lib/guide/Guide*.mag` (collections, concurrency, modules, distribution, clustering, etc.)
- **Lib source:** `lib/*.mag` (ArrayList, Supervisor, Cluster, HashRing, etc.)

### Key Non-Obvious Facts

- **1-based indexing** — Maggie uses 1-based indexing (Smalltalk-80 convention). `indexOf:` returns 0 for not-found. `copyFrom:to:` uses closed intervals `[from, to]`.
- **NaN-boxed values** — all values are 64-bit NaN-boxed; markers defined in `vm/markers.go`
- **`fork` NLR semantics** — `fork` treats non-local returns (^) as local within the forked process to prevent NLR crashes across goroutines (there is no `forkWithResult`; use `Process>>wait`/`result` or `forkOn:` futures)
- **Process-level restriction** — `forkRestricted:` hides globals; touching a hidden global signals a catchable `RestrictedGlobal` error. Restrictions inherited by child forks. `Compiler evaluate:` and `Object allClasses` respect restrictions.
- **Tail-call optimization** — compiler auto-detects `^self selector: args` in tail position → `OpTailSend`
- **Control-flow inlining** — `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`/`and:`/`or:`/`whileTrue:`/`whileFalse:` compile to jump bytecode when blocks are literal (param-less, temp-less); `keywordInlineParts` in `compiler/inline_control_flow.go` is the single predicate shared by codegen AND `findCellVariables` — keep them in lockstep. Non-boolean conditions raise a catchable `mustBeBoolean` Error.
- **Failure doctrine** (`docs/CONVENTIONS.md`) — expected failures return `Result`, programmer errors signal, nil never signals. File/HttpClient return Results; `Future>>await` signals on error; `Channel>>receiveIfClosed:`/`tryReceiveIfEmpty:` disambiguate nil.
- **Serialization depth cap** — `maxSerialDepth` (256) bounds serializer AND deserializer recursion; Arrays/Dictionaries participate in backref identity across the wire.
- **Stack overflow** at 8192 frames → catchable `StackOverflow` exception
- **BigInteger** auto-promotion when SmallInteger overflows 48-bit range
- **Type annotations** are optional, Strongtalk-model — checked by `mag typecheck`, never affect runtime
- **Image format** is CBOR-based (tagged envelope with string/symbol/class/method/object/trait tables; the trait table lets post-load code `include:` lib traits)
- **`CompiledMethod.Source`** is the source text field (not `SourceText`)
- **`vm` cannot import `vm/dist`** (cycle) — envelope building duplicated in vm package
- **Image rebuild after lib changes:** `go run ./cmd/bootstrap/ && cp maggie.image cmd/mag/`

### Module System Summary

- `namespace:` / `import:` declarations before class definitions
- `::` separator (e.g., `Yutani::Widgets::Button`)
- Directory-as-namespace: `src/myapp/models/User.mag` → `MyApp::Models`
- FQN resolution at compile time, no runtime cost
- Project manifest: `maggie.toml` (see `docs/USER_GUIDE.md` or `lib/guide/Guide11Projects.mag`)
- Two-pass loading: skeleton registration → superclass resolution → method compilation
- Dep namespace resolution: consumer override > producer manifest > PascalCase fallback

### Concurrency Quick Reference

All primitives are fully implemented. See `lib/guide/Guide09Concurrency.mag` and `lib/guide/Guide15Distribution.mag` for details.

| Primitive | Key files |
|-----------|-----------|
| Channel, Channel select | `vm/channel_primitives.go`, `lib/Channel.mag` |
| Process, Mailboxes, Links/Monitors | `vm/process_primitives.go`, `vm/mailbox.go` |
| Mutex, WaitGroup, Semaphore | `vm/concurrency_primitives.go` |
| CancellationContext | `vm/cancellation_primitives.go` |
| Node, RemoteProcess, Future | `vm/node_primitives.go`, `vm/future.go` |
| Remote Spawn (forkOn:/spawnOn:) | `vm/remote_spawn.go`, `lib/Block.mag` |
| Distributed Channels | `vm/remote_channel.go` |
| Supervisor Trees | `lib/Supervisor.mag` |
| Cluster, HashRing | `lib/Cluster.mag`, `lib/HashRing.mag` |

### Trust Model

Peer trust via `TrustStore` (`vm/dist/trust.go`). Ed25519 identity from `.maggie/node.key`. Configured in `maggie.toml` under `[trust]`. Permissions: `PermSync`, `PermMessage`, `PermSpawn`. Auto-ban after 3 hash mismatches.

---

## Debugging Yutani TUI Applications

Use Yutani's DebugService. Find session ID in output: `YutaniSession: session created with ID: ...`

```bash
yutani debug screen -s <session-id> --bounds --legend   # ASCII screen dump
yutani debug widget -s <session-id> -w <widget-id>      # Widget state
yutani debug bounds -s <session-id>                      # All widget positions
```

Full docs: `~/dev/go/yutani/DEBUG_GUIDE.md`

---

## Profiling

```bash
mag --profile -m Main.start                     # 1000 Hz → profile.folded
mag --profile --profile-rate 500 -m Main.start   # Custom rate
mag --pprof -m Main.start                        # Go pprof → cpu.pprof
```

Maggie API: `Compiler startProfiling` / `stopProfiling` / `isProfiling`. Profiler in `vm/sampling_profiler.go`.

---

## Benchmarking

```bash
./scripts/bench-compare.sh                       # Compare against baseline
go test -bench=BenchmarkHotPath -run='^$' -count=10 -benchmem ./vm/ > benchmarks/baseline.txt  # New baseline
```

Requires `benchstat`.

<!-- gitnexus:start -->
# GitNexus — Code Intelligence

This project is indexed by GitNexus as **maggie** (11627 symbols, 48176 relationships, 588 execution flows).

> Index stale? Run `node .gitnexus/run.cjs analyze --index-only` from the project root — it auto-selects an available runner. No `.gitnexus/run.cjs` yet? Bootstrap with `npx`, `bunx`, or `pnpm dlx` — e.g. `bunx gitnexus@latest analyze` (npm 11 npx crash; #1939).

## Always Do

- **MUST run impact before editing.** Use `impact({target: "symbolName", direction: "upstream"})` or `node .gitnexus/run.cjs impact "symbolName" --direction upstream --repo .`; report callers, processes, and risk. Never substitute grep for graph analysis.
- **MUST analyze graph changes before committing.** Use `detect_changes({scope: "all"})` (MCP) or `node .gitnexus/run.cjs detect-changes --scope all --repo .` (CLI fallback). `partial: true` or `truncated: true` is not a clean check — a zero means unseen, not unaffected; re-run it. For regression review: `detect_changes({scope: "compare", base_ref: "main"})` or `node .gitnexus/run.cjs detect-changes --scope compare --base-ref "main" --repo .`.
- MUST warn on HIGH/CRITICAL `risk` pre-edit; never use `riskSharedAxes` to waive a HIGH/CRITICAL `risk` warning. Compare File/symbol: MCP File omits axes; Graph-RAG expands File.
- **MUST treat `risk: UNKNOWN` as unresolved, not as low.** An empty caller set is not evidence the symbol is unused — it can also mean the callers are not resolvable by the index (plain-object property access, dynamic dispatch, cross-language calls). `impact` pairs `UNKNOWN` with a `riskNote` saying so. Confirm with a text search before treating the symbol as safe to change or delete; do not proceed on the strength of a zero.
- **MUST use `query({search_query: "concept"})` for concepts/flows, `context({name: "symbolName"})` for a named symbol, or `impact` for blast radius, on read-only callers, dependencies, imports, or execution flow.** Graph first; text search only for empty/`UNKNOWN`/literals.
- For security review, `explain({target: "fileOrSymbol"})` lists taint findings (source→sink flows; needs `analyze --pdg`).

## Never Do

- NEVER edit a function, class, or method before MCP/CLI impact analysis.
- NEVER ignore HIGH or CRITICAL risk warnings from impact analysis, and never read `UNKNOWN` as an all-clear — it means the walk could not answer, which is the one verdict that requires confirming by other means.
- NEVER rename symbols with find-and-replace — use `rename` which understands the call graph.
- NEVER commit before MCP/CLI graph change analysis.

## Resources

| Resource | Use for |
| --- | --- |
| `gitnexus://repo/maggie/context` | Codebase overview, check index freshness |
| `gitnexus://repo/maggie/clusters` | All functional areas |
| `gitnexus://repo/maggie/processes` | All execution flows |
| `gitnexus://repo/maggie/process/{name}` | Step-by-step execution trace |

## CLI

| Task | Read this skill file |
| --- | --- |
| Understand architecture / "How does X work?" | `.claude/skills/gitnexus-exploring/SKILL.md` |
| Blast radius / "What breaks if I change X?" | `.claude/skills/gitnexus-impact-analysis/SKILL.md` |
| Trace bugs / "Why is X failing?" | `.claude/skills/gitnexus-debugging/SKILL.md` |
| Rename / extract / split / refactor | `.claude/skills/gitnexus-refactoring/SKILL.md` |
| Tools, resources, schema reference | `.claude/skills/gitnexus-guide/SKILL.md` |
| Index, status, clean, wiki CLI commands | `.claude/skills/gitnexus-cli/SKILL.md` |

<!-- gitnexus:end -->
