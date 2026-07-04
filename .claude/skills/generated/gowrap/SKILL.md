---
name: gowrap
description: "Skill for the Gowrap area of maggie. 50 symbols across 14 files."
---

# Gowrap

50 symbols | 14 files | Cohesion: 70%

## When to Use

- Working with code in `gowrap/`
- Understanding how BuildEmbedded, BuildFullSystem, RunScript work
- Modifying gowrap-related functionality

## Key Files

| File | Symbols |
|------|---------|
| `gowrap/gen_go.go` | sanitizePackageName, hasUnconvertibleParam, generateFunctionBinding, generateMethodBinding, buildCallArgs (+4) |
| `gowrap/build.go` | detectModulePath, BuildEmbedded, BuildFullSystem, generateProjectConfig, parseGoModDirectives (+2) |
| `gowrap/introspect.go` | IntrospectPackage, extractConstant, extractFunction, extractType, functionModelFromSig (+2) |
| `gowrap/introspect_test.go` | TestIntrospectPackage_Strings, TestIntrospectPackage_WithFilter, TestIntrospectPackage_EncodingJson, TestIntrospectPackage_BadPath, TestIntrospectPackage_Constants |
| `gowrap/gen_go_test.go` | TestGenerateGoGlue_Strings, TestGenerateGoGlue_ErrorHandling, TestGenerateGoGlue_EmptyModel |
| `gowrap/naming_test.go` | TestGoNameToMaggieSelector, TestGoPackageToMaggieNamespace, TestToPascal |
| `gowrap/naming.go` | GoNameToMaggieSelector, GoPackageToMaggieNamespace, toPascal |
| `gowrap/gen_mag_test.go` | TestGenerateMaggieStubs_Strings, TestGenerateMaggieStubs_EmptyModel, TestGenerateMaggieStubs_WithMethods |
| `gowrap/gen_mag.go` | GenerateMaggieStubs, formatSelector, paramVarName |
| `cmd/mag/build.go` | buildTarget, detectMaggieDir |

## Entry Points

Start here when exploring this area:

- **`BuildEmbedded`** (Function) — `gowrap/build.go:46`
- **`BuildFullSystem`** (Function) — `gowrap/build.go:134`
- **`RunScript`** (Function) — `manifest/scripts.go:10`
- **`TestIntrospectPackage_Strings`** (Function) — `gowrap/introspect_test.go:6`
- **`TestIntrospectPackage_WithFilter`** (Function) — `gowrap/introspect_test.go:59`

## Key Symbols

| Symbol | Type | File | Line |
|--------|------|------|------|
| `BuildEmbedded` | Function | `gowrap/build.go` | 46 |
| `BuildFullSystem` | Function | `gowrap/build.go` | 134 |
| `RunScript` | Function | `manifest/scripts.go` | 10 |
| `TestIntrospectPackage_Strings` | Function | `gowrap/introspect_test.go` | 6 |
| `TestIntrospectPackage_WithFilter` | Function | `gowrap/introspect_test.go` | 59 |
| `TestIntrospectPackage_EncodingJson` | Function | `gowrap/introspect_test.go` | 77 |
| `TestIntrospectPackage_BadPath` | Function | `gowrap/introspect_test.go` | 125 |
| `TestIntrospectPackage_Constants` | Function | `gowrap/introspect_test.go` | 132 |
| `IntrospectPackage` | Function | `gowrap/introspect.go` | 12 |
| `TestGenerateGoGlue_Strings` | Function | `gowrap/gen_go_test.go` | 9 |
| `TestGoNameToMaggieSelector` | Function | `gowrap/naming_test.go` | 26 |
| `GoNameToMaggieSelector` | Function | `gowrap/naming.go` | 23 |
| `TestGenerateMaggieStubs_Strings` | Function | `gowrap/gen_mag_test.go` | 8 |
| `TestGenerateMaggieStubs_EmptyModel` | Function | `gowrap/gen_mag_test.go` | 48 |
| `TestGenerateMaggieStubs_WithMethods` | Function | `gowrap/gen_mag_test.go` | 64 |
| `GenerateMaggieStubs` | Function | `gowrap/gen_mag.go` | 9 |
| `TestGenerateGoGlue_ErrorHandling` | Function | `gowrap/gen_go_test.go` | 50 |
| `TestGenerateGoGlue_EmptyModel` | Function | `gowrap/gen_go_test.go` | 69 |
| `GenerateGoGlue` | Function | `gowrap/gen_go.go` | 10 |
| `TestGoPackageToMaggieNamespace` | Function | `gowrap/naming_test.go` | 4 |

## Connected Areas

| Area | Connections |
|------|-------------|
| Mag | 1 calls |

## How to Explore

1. `gitnexus_context({name: "BuildEmbedded"})` — see callers and callees
2. `gitnexus_query({query: "gowrap"})` — find related execution flows
3. Read key files listed above for implementation details
