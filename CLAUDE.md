## PUDL Observation Collection

Record notable observations with `pudl observe` before your session ends.

```
pudl observe "<one-sentence description>" --kind <kind> [--repo <path>] [--source <agent-name>]
```

Kinds: fact, obstacle, pattern, antipattern, suggestion, bug, opportunity. You MUST post at least one observation before your work is considered complete.

---

## Language Reference

Maggie is a Smalltalk-family language VM written in Go. For full API docs:

- **User Guide:** `docs/USER_GUIDE.md`
- **Guided tutorials:** `lib/guide/Guide*.mag` (collections, concurrency, modules, distribution, clustering, etc.)
- **Lib source:** `lib/*.mag` (ArrayList, Supervisor, Cluster, HashRing, etc.)

### Key Non-Obvious Facts

- **0-based indexing** — Maggie arrays are 0-based, not 1-based like Smalltalk-80
- **NaN-boxed values** — all values are 64-bit NaN-boxed; markers defined in `vm/markers.go`
- **`fork` vs `forkWithResult`** — `fork` treats non-local returns (^) as local within the forked process to prevent NLR crashes across goroutines
- **Process-level restriction** — `forkRestricted:` hides globals (resolve to nil, no error). Restrictions inherited by child forks. `Compiler evaluate:` and `Object allClasses` respect restrictions.
- **Tail-call optimization** — compiler auto-detects `^self selector: args` in tail position → `OpTailSend`
- **Stack overflow** at 4096 frames → catchable `StackOverflow` exception
- **BigInteger** auto-promotion when SmallInteger overflows 48-bit range
- **Type annotations** are optional, Strongtalk-model — checked by `mag typecheck`, never affect runtime
- **Image format** is v4 (includes content hashes); v3 backward compat on load
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

All primitives are fully implemented. See `lib/guide/Guide09Concurrency.mag` and `lib/guide/Guide13Distribution.mag` for details.

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

This project is indexed by GitNexus as **maggie** (9136 symbols, 42227 relationships, 300 execution flows). Use the GitNexus MCP tools to understand code, assess impact, and navigate safely.

> If any GitNexus tool warns the index is stale, run `npx gitnexus analyze` in terminal first.

## Always Do

- **MUST run impact analysis before editing any symbol.** Before modifying a function, class, or method, run `gitnexus_impact({target: "symbolName", direction: "upstream"})` and report the blast radius (direct callers, affected processes, risk level) to the user.
- **MUST run `gitnexus_detect_changes()` before committing** to verify your changes only affect expected symbols and execution flows.
- **MUST warn the user** if impact analysis returns HIGH or CRITICAL risk before proceeding with edits.
- When exploring unfamiliar code, use `gitnexus_query({query: "concept"})` to find execution flows instead of grepping. It returns process-grouped results ranked by relevance.
- When you need full context on a specific symbol — callers, callees, which execution flows it participates in — use `gitnexus_context({name: "symbolName"})`.

## When Debugging

1. `gitnexus_query({query: "<error or symptom>"})` — find execution flows related to the issue
2. `gitnexus_context({name: "<suspect function>"})` — see all callers, callees, and process participation
3. `READ gitnexus://repo/maggie/process/{processName}` — trace the full execution flow step by step
4. For regressions: `gitnexus_detect_changes({scope: "compare", base_ref: "main"})` — see what your branch changed

## When Refactoring

- **Renaming**: MUST use `gitnexus_rename({symbol_name: "old", new_name: "new", dry_run: true})` first. Review the preview — graph edits are safe, text_search edits need manual review. Then run with `dry_run: false`.
- **Extracting/Splitting**: MUST run `gitnexus_context({name: "target"})` to see all incoming/outgoing refs, then `gitnexus_impact({target: "target", direction: "upstream"})` to find all external callers before moving code.
- After any refactor: run `gitnexus_detect_changes({scope: "all"})` to verify only expected files changed.

## Never Do

- NEVER edit a function, class, or method without first running `gitnexus_impact` on it.
- NEVER ignore HIGH or CRITICAL risk warnings from impact analysis.
- NEVER rename symbols with find-and-replace — use `gitnexus_rename` which understands the call graph.
- NEVER commit changes without running `gitnexus_detect_changes()` to check affected scope.

## Tools Quick Reference

| Tool | When to use | Command |
|------|-------------|---------|
| `query` | Find code by concept | `gitnexus_query({query: "auth validation"})` |
| `context` | 360-degree view of one symbol | `gitnexus_context({name: "validateUser"})` |
| `impact` | Blast radius before editing | `gitnexus_impact({target: "X", direction: "upstream"})` |
| `detect_changes` | Pre-commit scope check | `gitnexus_detect_changes({scope: "staged"})` |
| `rename` | Safe multi-file rename | `gitnexus_rename({symbol_name: "old", new_name: "new", dry_run: true})` |
| `cypher` | Custom graph queries | `gitnexus_cypher({query: "MATCH ..."})` |

## Impact Risk Levels

| Depth | Meaning | Action |
|-------|---------|--------|
| d=1 | WILL BREAK — direct callers/importers | MUST update these |
| d=2 | LIKELY AFFECTED — indirect deps | Should test |
| d=3 | MAY NEED TESTING — transitive | Test if critical path |

## Resources

| Resource | Use for |
|----------|---------|
| `gitnexus://repo/maggie/context` | Codebase overview, check index freshness |
| `gitnexus://repo/maggie/clusters` | All functional areas |
| `gitnexus://repo/maggie/processes` | All execution flows |
| `gitnexus://repo/maggie/process/{name}` | Step-by-step execution trace |

## Self-Check Before Finishing

Before completing any code modification task, verify:
1. `gitnexus_impact` was run for all modified symbols
2. No HIGH/CRITICAL risk warnings were ignored
3. `gitnexus_detect_changes()` confirms changes match expected scope
4. All d=1 (WILL BREAK) dependents were updated

## Keeping the Index Fresh

After committing code changes, the GitNexus index becomes stale. Re-run analyze to update it:

```bash
npx gitnexus analyze
```

If the index previously included embeddings, preserve them by adding `--embeddings`:

```bash
npx gitnexus analyze --embeddings
```

To check whether embeddings exist, inspect `.gitnexus/meta.json` — the `stats.embeddings` field shows the count (0 means no embeddings). **Running analyze without `--embeddings` will delete any previously generated embeddings.**

> Claude Code users: A PostToolUse hook handles this automatically after `git commit` and `git merge`.

## CLI

| Task | Read this skill file |
|------|---------------------|
| Understand architecture / "How does X work?" | `.claude/skills/gitnexus/gitnexus-exploring/SKILL.md` |
| Blast radius / "What breaks if I change X?" | `.claude/skills/gitnexus/gitnexus-impact-analysis/SKILL.md` |
| Trace bugs / "Why is X failing?" | `.claude/skills/gitnexus/gitnexus-debugging/SKILL.md` |
| Rename / extract / split / refactor | `.claude/skills/gitnexus/gitnexus-refactoring/SKILL.md` |
| Tools, resources, schema reference | `.claude/skills/gitnexus/gitnexus-guide/SKILL.md` |
| Index, status, clean, wiki CLI commands | `.claude/skills/gitnexus/gitnexus-cli/SKILL.md` |
| Work in the Vm area (2930 symbols) | `.claude/skills/generated/vm/SKILL.md` |
| Work in the Compiler area (301 symbols) | `.claude/skills/generated/compiler/SKILL.md` |
| Work in the Server area (270 symbols) | `.claude/skills/generated/server/SKILL.md` |
| Work in the Mag area (199 symbols) | `.claude/skills/generated/mag/SKILL.md` |
| Work in the Types area (99 symbols) | `.claude/skills/generated/types/SKILL.md` |
| Work in the Maggiev1connect area (68 symbols) | `.claude/skills/generated/maggiev1connect/SKILL.md` |
| Work in the Manifest area (61 symbols) | `.claude/skills/generated/manifest/SKILL.md` |
| Work in the Hash area (52 symbols) | `.claude/skills/generated/hash/SKILL.md` |
| Work in the Gowrap area (50 symbols) | `.claude/skills/generated/gowrap/SKILL.md` |
| Work in the Bootstrap area (41 symbols) | `.claude/skills/generated/bootstrap/SKILL.md` |
| Work in the Pipeline area (41 symbols) | `.claude/skills/generated/pipeline/SKILL.md` |
| Work in the V1 area (14 symbols) | `.claude/skills/generated/v1/SKILL.md` |

<!-- gitnexus:end -->
