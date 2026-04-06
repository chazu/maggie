# PP.mag Compilation Debug Investigation

**Date:** 2026-04-06
**Status:** Root cause identified
**Severity:** Low (not a compiler bug)

## Summary

The `cmdGc:` method at line 872 of `src/cli/PP.mag` **parses and compiles successfully**. It is not callable at runtime because the `runWith:` dispatch table is missing a `cmd = 'gc'` entry, not because of any compilation pipeline issue.

## Investigation Results

### 1. Parser Analysis

**Finding: Parser works correctly on PP.mag.**

Tested by programmatically parsing PP.mag with `compiler.NewParser` and `ParseSourceFile()`. The parser finds:
- 1 class (`PP`, superclass `Object`)
- 31 instance methods (all parsed correctly)
- 1 class method (`new`)
- Zero parse errors, zero warnings

The `cmdGc:` method at line 871 is parsed as the 25th instance method. All methods after it (`silentInp:scope:identity:`, `post:body:`, `safeScan:scope:`, `safeGetNotifications`, `flagValue:in:`, `printUsage`) also parse correctly.

**Parser architecture notes:** `parseClassDefBody()` (parser.go:1126) loops until EOF or another top-level definition (`subclass:`, `trait`, `protocol`). There is no file size limit, no method count limit, and no early termination on large files.

### 2. Compiler Analysis

**Finding: All 31 methods compile successfully.**

Tested by calling `compiler.CompileMethodDefWithIvars()` on every method definition in the parsed AST. All 31 methods compile with zero errors, including `cmdGc:` at line 871.

**Compiler architecture notes:** `CompileMethodDef` creates a fresh `Compiler` instance per method. There is no per-class method count limit. Each method gets its own literal pool, temp map, and bytecode buffer. The compilation pipeline (`pipeline/pipeline.go:CompileAll`) processes methods independently and halts on first error with `return compiled, fmt.Errorf(...)` — but no error occurs.

### 3. Bootstrap/Image Loading

**Finding: No method count limit per class.**

The image reader/writer use `uint32` for method counts (up to 4 billion). The VTable (`vm/vtable.go`) uses a dynamically-sized `[]Method` slice with no artificial limit. Previous research (documented in `docs/research/maggie-operand-widening.md`) also confirms: "There is no class-level compilation limit separate from method-level limits."

### 4. Per-Method vs Per-File Issue

**Finding: Neither. The issue is not with compilation at all.**

All methods parse and compile individually. The issue is a missing dispatch entry.

### 5. Actual Root Cause: Missing Dispatch Entry

The `runWith:` method (line 21) dispatches commands via a series of `ifTrue:` checks:

```smalltalk
cmd = 'observe'   ifTrue: [^self cmdObserve: args].
cmd = 'decide'    ifTrue: [^self cmdDecide: args].
...
cmd = 'dashboard' ifTrue: [^self cmdDashboard: args].
cmd = 'workitem'  ifTrue: [^WorkItemCLI new runWith: args].
cmd = 'repo'      ifTrue: [^Repo new runWith: args].
cmd = 'worktree'  ifTrue: [...].
cmd = 'help'      ifTrue: [^self printUsage].
```

**There is no `cmd = 'gc' ifTrue: [^self cmdGc: args]` line.** The `gc` command falls through to "Unknown command" (line 56).

The `printUsage` method at line 1058 *does* mention gc in the help text:
```
'  gc       [--dry-run]    Garbage-collect finished workflows & branches' println.
```

So the method was defined and help text was added, but the dispatch entry was never wired up.

## Recommended Fix

Add the following line to `runWith:` in the dispatch table (e.g., after line 45, the `dashboard` entry):

```smalltalk
cmd = 'gc'        ifTrue: [^self cmdGc: args].
```

## Additional Observations

- PP.mag is 1066 lines with 31 instance methods — a large file but well within all parser/compiler limits.
- The `cmdGc:` method itself (lines 871-966) is well-formed and substantial (~95 lines).
- Previous research in `docs/research/maggie-operand-widening.md` speculated the issue might be parser errors or superclass resolution. Both hypotheses are disproven — the issue is simply a missing dispatch line.
