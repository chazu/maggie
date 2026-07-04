---
name: types
description: "Skill for the Types area of maggie. 99 symbols across 15 files."
---

# Types

99 symbols | 15 files | Cohesion: 76%

## When to Use

- Working with code in `types/`
- Understanding how IsDynamic, Compatible, NewProtocolRegistry work
- Modifying types-related functionality

## Key Files

| File | Symbols |
|------|---------|
| `types/inferrer_test.go` | newTestInferrer, parseMethod, TestInferLiteralTypes, TestInferAssignmentPropagation, TestInferAssignmentChain (+14) |
| `types/checker_test.go` | testVM, TestCheckKnownTypes, TestCheckUnknownType, TestCheckDynamicAndSelf, TestCheckProtocolType (+13) |
| `types/checker.go` | NewChecker, CheckSourceFile, CheckProtocolSatisfaction, String, checkEffects (+4) |
| `types/inferrer.go` | InferMethod, NewInferrer, SetEffectTable, inferExpr, inferSend (+2) |
| `types/return_types_test.go` | TestReturnTypeTableBuiltins, TestReturnTypeTableIntegerAlias, TestReturnTypeTableUnknown, TestReturnTypeTableRegister, TestReturnTypeTableHarvestFromMethod (+2) |
| `types/protocol.go` | NewProtocolRegistry, Register, Lookup, RegisterFromAST, Satisfies (+1) |
| `types/type_env_test.go` | TestTypeEnvSetAndLookup, TestTypeEnvUnbound, TestTypeEnvParentScope, TestTypeEnvShadowing, TestTypeEnvSnapshot (+1) |
| `types/effect_test.go` | TestEffectIsEmpty, TestEffectIsPure, TestParseEffects, TestIsValidEffect, TestEffectUnion (+1) |
| `types/effect.go` | IsEmpty, IsPure, ParseEffects, IsValidEffect, Union (+1) |
| `types/type_env.go` | NewTypeEnv, Set, Lookup, Snapshot, collectBindings |

## Entry Points

Start here when exploring this area:

- **`IsDynamic`** (Function) — `types/types.go:58`
- **`Compatible`** (Function) — `types/types.go:68`
- **`NewProtocolRegistry`** (Function) — `types/protocol.go:13`
- **`TestInferLiteralTypes`** (Function) — `types/inferrer_test.go:28`
- **`TestInferAssignmentPropagation`** (Function) — `types/inferrer_test.go:110`

## Key Symbols

| Symbol | Type | File | Line |
|--------|------|------|------|
| `IsDynamic` | Function | `types/types.go` | 58 |
| `Compatible` | Function | `types/types.go` | 68 |
| `NewProtocolRegistry` | Function | `types/protocol.go` | 13 |
| `TestInferLiteralTypes` | Function | `types/inferrer_test.go` | 28 |
| `TestInferAssignmentPropagation` | Function | `types/inferrer_test.go` | 110 |
| `TestInferAssignmentChain` | Function | `types/inferrer_test.go` | 133 |
| `TestInferReassignmentLastWriteWins` | Function | `types/inferrer_test.go` | 157 |
| `TestInferMessageSendReturnType` | Function | `types/inferrer_test.go` | 181 |
| `TestInferChainedSends` | Function | `types/inferrer_test.go` | 204 |
| `TestInferBinaryMessageReturnType` | Function | `types/inferrer_test.go` | 228 |
| `TestInferComparisonReturnType` | Function | `types/inferrer_test.go` | 249 |
| `TestInferDoesNotUnderstandWarning` | Function | `types/inferrer_test.go` | 270 |
| `TestInferDynamicSuppressesWarnings` | Function | `types/inferrer_test.go` | 297 |
| `TestInferUnboundVariableIsDynamic` | Function | `types/inferrer_test.go` | 316 |
| `TestInferParameterWithAnnotation` | Function | `types/inferrer_test.go` | 337 |
| `TestInferReturnTypeMismatch` | Function | `types/inferrer_test.go` | 358 |
| `TestInferNoReturnStatementIsDynamic` | Function | `types/inferrer_test.go` | 385 |
| `TestInferYourselfReturnsSelf` | Function | `types/inferrer_test.go` | 403 |
| `TestInferKeywordMessage` | Function | `types/inferrer_test.go` | 425 |
| `TestInferStringConcat` | Function | `types/inferrer_test.go` | 446 |

## Execution Flows

| Flow | Type | Steps |
|------|------|-------|
| `MakeEvalHandler → ReadUint32` | cross_community | 5 |
| `MakeEvalHandler → Remaining` | cross_community | 5 |

## Connected Areas

| Area | Connections |
|------|-------------|
| Compiler | 32 calls |
| Vm | 5 calls |
| Mag | 1 calls |

## How to Explore

1. `gitnexus_context({name: "IsDynamic"})` — see callers and callees
2. `gitnexus_query({query: "types"})` — find related execution flows
3. Read key files listed above for implementation details
