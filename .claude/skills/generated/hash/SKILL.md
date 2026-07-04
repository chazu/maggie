---
name: hash
description: "Skill for the Hash area of maggie. 52 symbols across 7 files."
---

# Hash

52 symbols | 7 files | Cohesion: 85%

## When to Use

- Working with code in `compiler/`
- Understanding how TestNormalize_SimpleMethod_ParamResolution, TestNormalize_MethodWithTemps, TestNormalize_AlphaEquivalence work
- Modifying hash-related functionality

## Key Files

| File | Symbols |
|------|---------|
| `compiler/hash/normalize_test.go` | methodDef, exprStmt, variable, intLit, TestNormalize_SimpleMethod_ParamResolution (+9) |
| `compiler/hash/serialize.go` | SerializeTyped, writeByte, writeUint16, writeUint32, writeInt64 (+9) |
| `compiler/hash/typed_test.go` | parseMethod, TestTypedHashUntypedMethod, TestTypedHashSameCodeDifferentTypes, TestTypedHashSameCodeSameTypes, TestTypedHashWithReturnTypeOnly (+3) |
| `compiler/hash/serialize_test.go` | TestSerialize_Deterministic, TestSerialize_VersionPrefix, TestSerialize_IntLiteral, TestSerialize_FloatLiteral, TestSerialize_StringLiteral (+3) |
| `compiler/hash/normalize.go` | NormalizeMethod, normalizeStmt, normalizeExpr, resolveVariable, normalizeBlock |
| `compiler/hash/typed.go` | HashTypedMethod, NormalizeTypedMethod |
| `compiler/hash/golden_test.go` | TestGoldenFiles |

## Entry Points

Start here when exploring this area:

- **`TestNormalize_SimpleMethod_ParamResolution`** (Function) — `compiler/hash/normalize_test.go:30`
- **`TestNormalize_MethodWithTemps`** (Function) — `compiler/hash/normalize_test.go:61`
- **`TestNormalize_AlphaEquivalence`** (Function) — `compiler/hash/normalize_test.go:98`
- **`TestNormalize_NestedBlocks_DeBruijn`** (Function) — `compiler/hash/normalize_test.go:135`
- **`TestNormalize_InstanceVariables`** (Function) — `compiler/hash/normalize_test.go:198`

## Key Symbols

| Symbol | Type | File | Line |
|--------|------|------|------|
| `TestNormalize_SimpleMethod_ParamResolution` | Function | `compiler/hash/normalize_test.go` | 30 |
| `TestNormalize_MethodWithTemps` | Function | `compiler/hash/normalize_test.go` | 61 |
| `TestNormalize_AlphaEquivalence` | Function | `compiler/hash/normalize_test.go` | 98 |
| `TestNormalize_NestedBlocks_DeBruijn` | Function | `compiler/hash/normalize_test.go` | 135 |
| `TestNormalize_InstanceVariables` | Function | `compiler/hash/normalize_test.go` | 198 |
| `TestNormalize_GlobalRef` | Function | `compiler/hash/normalize_test.go` | 217 |
| `TestNormalize_SelfSuperNilTrueFalseThisContext` | Function | `compiler/hash/normalize_test.go` | 241 |
| `TestNormalize_PrimitiveMethod` | Function | `compiler/hash/normalize_test.go` | 272 |
| `TestNormalize_DocStringAffectsHash` | Function | `compiler/hash/normalize_test.go` | 293 |
| `TestNormalize_DeeplyNestedBlocks` | Function | `compiler/hash/normalize_test.go` | 308 |
| `NormalizeMethod` | Function | `compiler/hash/normalize.go` | 31 |
| `SerializeTyped` | Function | `compiler/hash/serialize.go` | 32 |
| `TestTypedHashUntypedMethod` | Function | `compiler/hash/typed_test.go` | 29 |
| `TestTypedHashSameCodeDifferentTypes` | Function | `compiler/hash/typed_test.go` | 51 |
| `TestTypedHashSameCodeSameTypes` | Function | `compiler/hash/typed_test.go` | 74 |
| `TestTypedHashWithReturnTypeOnly` | Function | `compiler/hash/typed_test.go` | 88 |
| `TestTypedHashGoldenFiles` | Function | `compiler/hash/typed_test.go` | 100 |
| `TestNormalizeTypedMethodPopulatesTypes` | Function | `compiler/hash/typed_test.go` | 168 |
| `TestTypedHashWithResolveGlobal` | Function | `compiler/hash/typed_test.go` | 188 |
| `HashTypedMethod` | Function | `compiler/hash/typed.go` | 13 |

## Connected Areas

| Area | Connections |
|------|-------------|
| Server | 4 calls |
| Compiler | 3 calls |

## How to Explore

1. `gitnexus_context({name: "TestNormalize_SimpleMethod_ParamResolution"})` — see callers and callees
2. `gitnexus_query({query: "hash"})` — find related execution flows
3. Read key files listed above for implementation details
