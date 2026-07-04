---
name: bootstrap
description: "Skill for the Bootstrap area of maggie. 41 symbols across 7 files."
---

# Bootstrap

41 symbols | 7 files | Cohesion: 50%

## When to Use

- Working with code in `cmd/`
- Understanding how NewGoCompilerBackend, NewMaggieCompilerBackend, TestSelfCompile work
- Modifying bootstrap-related functionality

## Key Files

| File | Symbols |
|------|---------|
| `cmd/bootstrap/compiler_benchmark_test.go` | benchmarkVMWithImage, BenchmarkGoCompilerSimple, BenchmarkMaggieCompilerSimple, BenchmarkGoCompilerArithmetic, BenchmarkMaggieCompilerArithmetic (+15) |
| `cmd/bootstrap/set_select_test.go` | setupVMWithSet, eval, TestSetAddAndSize, TestSetMultiStatementEval, TestSetSelectViaEval (+1) |
| `cmd/bootstrap/compiler_selfcompile_test.go` | TestSelfCompileCompilerMethods, TestSelfCompileRealTokenMethods, TestSelfCompileExecutionEquivalence, TestSelfCompileAllCompilerMethods, TestTripleBootstrap |
| `vm/compiler_dispatch.go` | NewGoCompilerBackend, NewMaggieCompilerBackend, UseGoCompiler, UseMaggieCompiler |
| `cmd/bootstrap/selfcompile_test.go` | TestSelfCompile, TestMaggieCompilerExists, TestMaggieCompilerInstantiation, TestIfTrue |
| `vm/bytecode.go` | Skip |
| `cmd/bootstrap/main.go` | main |

## Entry Points

Start here when exploring this area:

- **`NewGoCompilerBackend`** (Function) — `vm/compiler_dispatch.go:43`
- **`NewMaggieCompilerBackend`** (Function) — `vm/compiler_dispatch.go:123`
- **`TestSelfCompile`** (Function) — `cmd/bootstrap/selfcompile_test.go:20`
- **`TestMaggieCompilerExists`** (Function) — `cmd/bootstrap/selfcompile_test.go:149`
- **`TestMaggieCompilerInstantiation`** (Function) — `cmd/bootstrap/selfcompile_test.go:177`

## Key Symbols

| Symbol | Type | File | Line |
|--------|------|------|------|
| `NewGoCompilerBackend` | Function | `vm/compiler_dispatch.go` | 43 |
| `NewMaggieCompilerBackend` | Function | `vm/compiler_dispatch.go` | 123 |
| `TestSelfCompile` | Function | `cmd/bootstrap/selfcompile_test.go` | 20 |
| `TestMaggieCompilerExists` | Function | `cmd/bootstrap/selfcompile_test.go` | 149 |
| `TestMaggieCompilerInstantiation` | Function | `cmd/bootstrap/selfcompile_test.go` | 177 |
| `TestIfTrue` | Function | `cmd/bootstrap/selfcompile_test.go` | 272 |
| `TestSelfCompileCompilerMethods` | Function | `cmd/bootstrap/compiler_selfcompile_test.go` | 15 |
| `TestSelfCompileRealTokenMethods` | Function | `cmd/bootstrap/compiler_selfcompile_test.go` | 208 |
| `TestSelfCompileExecutionEquivalence` | Function | `cmd/bootstrap/compiler_selfcompile_test.go` | 309 |
| `TestSelfCompileAllCompilerMethods` | Function | `cmd/bootstrap/compiler_selfcompile_test.go` | 432 |
| `TestTripleBootstrap` | Function | `cmd/bootstrap/compiler_selfcompile_test.go` | 557 |
| `BenchmarkGoCompilerSimple` | Function | `cmd/bootstrap/compiler_benchmark_test.go` | 33 |
| `BenchmarkMaggieCompilerSimple` | Function | `cmd/bootstrap/compiler_benchmark_test.go` | 49 |
| `BenchmarkGoCompilerArithmetic` | Function | `cmd/bootstrap/compiler_benchmark_test.go` | 66 |
| `BenchmarkMaggieCompilerArithmetic` | Function | `cmd/bootstrap/compiler_benchmark_test.go` | 82 |
| `BenchmarkGoCompilerLocalVariables` | Function | `cmd/bootstrap/compiler_benchmark_test.go` | 99 |
| `BenchmarkMaggieCompilerLocalVariables` | Function | `cmd/bootstrap/compiler_benchmark_test.go` | 120 |
| `BenchmarkGoCompilerBlock` | Function | `cmd/bootstrap/compiler_benchmark_test.go` | 142 |
| `BenchmarkMaggieCompilerBlock` | Function | `cmd/bootstrap/compiler_benchmark_test.go` | 159 |
| `BenchmarkGoCompilerMethodWithArgs` | Function | `cmd/bootstrap/compiler_benchmark_test.go` | 177 |

## Execution Flows

| Flow | Type | Steps |
|------|------|-------|
| `Main → Parser` | cross_community | 5 |
| `Main → SourceFile` | cross_community | 5 |
| `Main → CurTokenIs` | cross_community | 5 |
| `Main → SelectorTable` | cross_community | 4 |
| `Main → SymbolTable` | cross_community | 4 |
| `Main → ClassTable` | cross_community | 4 |
| `Main → GoCompilerBackend` | intra_community | 4 |
| `Main → Errors` | cross_community | 4 |
| `Main → Trait` | cross_community | 4 |
| `Main → Session` | cross_community | 4 |

## Connected Areas

| Area | Connections |
|------|-------------|
| Vm | 49 calls |
| Mag | 12 calls |
| Server | 2 calls |
| Compiler | 1 calls |

## How to Explore

1. `gitnexus_context({name: "NewGoCompilerBackend"})` — see callers and callees
2. `gitnexus_query({query: "bootstrap"})` — find related execution flows
3. Read key files listed above for implementation details
