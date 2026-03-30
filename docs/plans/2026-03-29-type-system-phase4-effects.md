# Type System Phase 4: Effect Annotations

**Date:** 2026-03-29
**Depends on:** Phases 1-3 (complete)
**Status:** COMPLETE
**Estimated scope:** ~640 LOC, 1 day

---

## Summary

Effect annotations declare what side effects a method performs. The checker verifies that declared effects match inferred effects from the method body. Unannotated methods get no effect checking (gradual effects).

## Syntax

```smalltalk
method: computeTotal: items <Indexable> ^<Integer> ! <Pure> [
  items inject: 0 into: [:sum :item | sum + item price]
]

method: saveReport: data <String> ^<Boolean> ! <IO> [
  File write: data to: '/tmp/report.txt'. ^true
]

method: fetchAndStore: url <String> ^<String> ! <IO, Network> [
  | data | data := HTTP get: url. File write: data to: '/tmp/cache.txt'. ^data
]
```

## Effect categories (fixed, bitmask)

- `Pure` — no side effects (assertion)
- `IO` — file system access (File, ExternalProcess, SqliteDatabase, DuckDatabase)
- `Network` — network access (HTTP, HttpClient, HttpServer, UnixSocket*, GrpcClient)
- `Process` — concurrency (Process, Channel, Mutex, WaitGroup, Semaphore, CancellationContext)
- `State` — mutable state (Compiler setGlobal:to:, evaluate:, global assignment)

## Key decisions

- Effects are a bitmask, combined with OR
- `! <Pure>` is an assertion — checker warns if body has effects
- Unannotated methods: effects inferred but never checked (gradual)
- Block bodies walked for effect inference
- Included in typed hash
- `!` must be separated from `<` by a space (lexer constraint)
