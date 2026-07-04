---
name: pipeline
description: "Skill for the Pipeline area of maggie. 41 symbols across 7 files."
---

# Pipeline

41 symbols | 7 files | Cohesion: 65%

## When to Use

- Working with code in `pipeline/`
- Understanding how TestTwoPass_ForwardReference, TestTwoPass_CircularImports, TestTwoPass_UnresolvedSuperclassError work
- Modifying pipeline-related functionality

## Key Files

| File | Symbols |
|------|---------|
| `pipeline/pipeline_test.go` | newTestVM, newPipeline, TestTwoPass_ForwardReference, TestTwoPass_CircularImports, TestTwoPass_UnresolvedSuperclassError (+17) |
| `pipeline/pipeline.go` | CompilePath, logf, CollectFiles, collectFilesImpl, filterExcluded (+2) |
| `pipeline/namespace_test.go` | TestCheckNamespaceCollisions, TestPrefixDepNamespaces, TestPrefixDepNamespacesWithRemap, TestPrefixDepNamespacesNoRemap |
| `manifest/manifest_test.go` | TestSourceDirPaths, TestAllDependencies, TestAllDependenciesConflict |
| `pipeline/namespace.go` | CheckNamespaceCollisions, PrefixDepNamespaces |
| `manifest/manifest.go` | SourceDirPaths, AllDependencies |
| `manifest/resolver.go` | NewResolver |

## Entry Points

Start here when exploring this area:

- **`TestTwoPass_ForwardReference`** (Function) — `pipeline/pipeline_test.go:50`
- **`TestTwoPass_CircularImports`** (Function) — `pipeline/pipeline_test.go:96`
- **`TestTwoPass_UnresolvedSuperclassError`** (Function) — `pipeline/pipeline_test.go:138`
- **`TestTwoPass_TraitForwardReference`** (Function) — `pipeline/pipeline_test.go:161`
- **`TestTwoPass_NamespaceIsolation`** (Function) — `pipeline/pipeline_test.go:198`

## Key Symbols

| Symbol | Type | File | Line |
|--------|------|------|------|
| `TestTwoPass_ForwardReference` | Function | `pipeline/pipeline_test.go` | 50 |
| `TestTwoPass_CircularImports` | Function | `pipeline/pipeline_test.go` | 96 |
| `TestTwoPass_UnresolvedSuperclassError` | Function | `pipeline/pipeline_test.go` | 138 |
| `TestTwoPass_TraitForwardReference` | Function | `pipeline/pipeline_test.go` | 161 |
| `TestTwoPass_NamespaceIsolation` | Function | `pipeline/pipeline_test.go` | 198 |
| `TestTwoPass_CrossNamespaceSuperclass` | Function | `pipeline/pipeline_test.go` | 235 |
| `TestTwoPass_SingleFileCompilePath` | Function | `pipeline/pipeline_test.go` | 308 |
| `TestFQN_CrossNamespaceClassReference` | Function | `pipeline/pipeline_test.go` | 335 |
| `TestFQN_SameNamespaceClassReference` | Function | `pipeline/pipeline_test.go` | 374 |
| `TestFQN_BareNameRootNamespace` | Function | `pipeline/pipeline_test.go` | 408 |
| `TestFQN_AssignmentResolvesNamespace` | Function | `pipeline/pipeline_test.go` | 444 |
| `TestTwoPass_TraitNamespace` | Function | `pipeline/pipeline_test.go` | 485 |
| `TestNoShortNameGlobals_NamespacedClassOnlyFQN` | Function | `pipeline/pipeline_test.go` | 515 |
| `TestNoShortNameGlobals_RootClassBareNameInGlobals` | Function | `pipeline/pipeline_test.go` | 559 |
| `TestNoShortNameGlobals_CrossNamespaceMethodRef` | Function | `pipeline/pipeline_test.go` | 578 |
| `TestFQN_ExplicitFQNInMethodBody` | Function | `pipeline/pipeline_test.go` | 624 |
| `TestFQN_ExplicitMultiLevelFQN` | Function | `pipeline/pipeline_test.go` | 662 |
| `TestCompileAll_PopulatesContentStore` | Function | `pipeline/pipeline_test.go` | 706 |
| `TestCompileAll_ContentStore_MultipleClasses` | Function | `pipeline/pipeline_test.go` | 759 |
| `TestCollectFiles_ReturnsWithoutCompiling` | Function | `pipeline/pipeline_test.go` | 279 |

## Connected Areas

| Area | Connections |
|------|-------------|
| Vm | 46 calls |
| Mag | 5 calls |
| Manifest | 2 calls |
| Bootstrap | 1 calls |
| Server | 1 calls |

## How to Explore

1. `gitnexus_context({name: "TestTwoPass_ForwardReference"})` — see callers and callees
2. `gitnexus_query({query: "pipeline"})` — find related execution flows
3. Read key files listed above for implementation details
