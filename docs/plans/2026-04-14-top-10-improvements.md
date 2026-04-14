# Top 10 Maggie Improvements

Identified 2026-04-14 from a comprehensive codebase survey.

## 1. Break up `interpreter.go::runFrame()` (793 lines, 64 opcode cases)

The main bytecode dispatch loop is the single most complex function in the codebase. Extract opcode handlers into grouped methods (arithmetic ops, stack ops, send ops, jump ops, etc.). This would make the interpreter easier to reason about, test individual opcode families, and reduce the cognitive load for anyone touching the hot path.

## 2. Add tests for `cmd/mag/` — 13 of 17 source files untested

The CLI layer (`docgen.go` at 1,442 lines, `sync.go` at 722, `doctest.go` at 630, `main.go` at 958) has almost no test coverage. These are user-facing tools. A bug here means a broken user experience with no regression safety net.

## 3. Extract `Compiler` struct responsibilities (18 fields, 32 private methods)

The `Compiler` in `codegen.go` is a god object managing compilation, variable tracking, block compilation, cell tracking, and capture analysis simultaneously. `compileBlock()` alone saves/restores 13 state variables. Extracting `BlockCompiler` and `VariableTracker` into separate types would reduce coupling and make the compilation pipeline testable in isolation.

## 4. Make hardcoded limits configurable

Stack depth (65536), frame depth (4096), mailbox capacity (1024), GC interval (30s), HTTP timeout (5s), inline cache entries (6) — all hardcoded constants. These should be configurable via `maggie.toml` or environment variables for production deployments with different resource profiles.

## 5. Fix the CI Go version mismatch

CI uses Go 1.24 but `go.mod` requires Go 1.25.7. This means CI may not be catching real build issues, or it's silently broken. A one-line fix with high impact on build reliability.

## 6. Standardize error handling — wrap bare `return err` with context

Multiple locations in `pipeline/`, `manifest/`, `vm/file_in.go`, `vm/image_writer.go`, and `gowrap/build.go` return bare errors without wrapping. When something fails, the error message lacks the call chain context needed to diagnose it quickly. Systematic `fmt.Errorf("doing X: %w", err)` wrapping would significantly improve debuggability.

## 7. Deduplicate NLR unwinding logic in `send()` / `sendSuper()`

`interpreter.go` lines 1407-1421 and 1519-1531 are nearly identical NLR (non-local return) handling code. Extract into a shared helper. This is the kind of duplication where a bug fix in one copy gets missed in the other.

## 8. Clean up stale repo artifacts

Untracked files (`s01_definingAClass` through `s08_puttingItTogether`, `+`, `size`), shelved Yutani IDE code in `lib/yutani/ide/shelved/` (7 files), stale worktree in `.claude/worktrees/`. These add noise for anyone exploring the repo.

## 9. Fix guide numbering collisions

`Guide10Modules.mag` / `Guide10Supervisors.mag` and `Guide11Clustering.mag` / `Guide11Projects.mag` share numbers. While docs render correctly, this creates confusion in source and tooling. Renumber to a consistent sequence.

## 10. Unexport internal hash package types

28 `H*` types (`HArrayLiteral`, `HAssignment`, etc.) are exported from `compiler/hash/` but never used outside the package. Only `HashMethod` and `HashTypedMethod` are the real public API. Unexporting the internals would shrink the public surface area and make the package's contract clear.
