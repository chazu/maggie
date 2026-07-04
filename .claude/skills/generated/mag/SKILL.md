---
name: mag
description: "Skill for the Mag area of maggie. 199 symbols across 38 files."
---

# Mag

199 symbols | 38 files | Cohesion: 70%

## When to Use

- Working with code in `cmd/`
- Understanding how TestVMLookupClass, TestImageSaveToFile, TestImageSaveToInvalidPath work
- Modifying mag-related functionality

## Key Files

| File | Symbols |
|------|---------|
| `cmd/mag/format.go` | Format, formatFile, handleFmtCommand, collectMagFiles, write (+36) |
| `cmd/mag/format_test.go` | TestFormat_Idempotent, TestFormat_IdempotentComplex, TestFormat_BinaryMessageSpacing, TestFormat_MultiStatementBlock, TestFormat_BlockWithTemps (+24) |
| `cmd/mag/image_cli_test.go` | newTestVM, newPipeline, lookupClassMethod, lookupInstanceMethod, TestSaveImage_CreatesNonEmptyFile (+16) |
| `cmd/mag/docgen.go` | isGuideClass, handleDocCommand, groupByNamespace, writeCSS, writePlaygroundJS (+13) |
| `cmd/mag/help_test.go` | captureStdout, TestHelp_NoArgs_ListsClasses, TestHelp_ClassName_ShowsClassHelp, TestHelp_ClassName_WithDocString, TestHelp_ClassMethod_ShowsMethodDoc (+7) |
| `cmd/mag/doctest.go` | handleDoctestCommand, printDoctestResults, tallyDoctestResults, collectAndRunDoctests, runDoctestMethods (+3) |
| `cmd/mag/usage.go` | progName, wantsHelp, subcmdUsage, usageFlags, usageExamples (+1) |
| `cmd/mag/main.go` | runMain, reorderArgs, main, run, loadRC (+1) |
| `server/server.go` | WithCompileFunc, WithTrustStore, WithDiskCache, WithSpawnResultFunc, ListenAndServe (+1) |
| `cmd/mag/docserve.go` | docArgsContain, docArgPort, docArgOutput, evalWithTimeout, wrapAsDoIt (+1) |

## Entry Points

Start here when exploring this area:

- **`TestVMLookupClass`** (Function) — `vm/vm_test.go:124`
- **`TestImageSaveToFile`** (Function) — `vm/image_writer_test.go:543`
- **`TestImageSaveToInvalidPath`** (Function) — `vm/image_writer_test.go:576`
- **`TestSaveImage_CreatesNonEmptyFile`** (Function) — `cmd/mag/image_cli_test.go:93`
- **`TestSaveImage_AfterCompilingSource`** (Function) — `cmd/mag/image_cli_test.go:111`

## Key Symbols

| Symbol | Type | File | Line |
|--------|------|------|------|
| `TestVMLookupClass` | Function | `vm/vm_test.go` | 124 |
| `TestImageSaveToFile` | Function | `vm/image_writer_test.go` | 543 |
| `TestImageSaveToInvalidPath` | Function | `vm/image_writer_test.go` | 576 |
| `TestSaveImage_CreatesNonEmptyFile` | Function | `cmd/mag/image_cli_test.go` | 93 |
| `TestSaveImage_AfterCompilingSource` | Function | `cmd/mag/image_cli_test.go` | 111 |
| `TestSaveImage_ErrorOnUnwritablePath` | Function | `cmd/mag/image_cli_test.go` | 146 |
| `TestLoadImage_ClassesSurviveRoundTrip` | Function | `cmd/mag/image_cli_test.go` | 160 |
| `TestLoadImage_MethodsSurviveRoundTrip` | Function | `cmd/mag/image_cli_test.go` | 217 |
| `TestLoadImage_ErrorOnNonexistentFile` | Function | `cmd/mag/image_cli_test.go` | 272 |
| `TestImageRoundTrip_ExecuteMethod` | Function | `cmd/mag/image_cli_test.go` | 284 |
| `TestImageRoundTrip_ExecuteInstanceMethod` | Function | `cmd/mag/image_cli_test.go` | 338 |
| `TestImageRoundTrip_GlobalsSurvive` | Function | `cmd/mag/image_cli_test.go` | 389 |
| `TestImageRoundTrip_MultipleClasses` | Function | `cmd/mag/image_cli_test.go` | 427 |
| `TestLoadImage_FreshVMCanLoadSavedImage` | Function | `cmd/mag/image_cli_test.go` | 498 |
| `TestLoadImage_CanCompileAdditionalSource` | Function | `cmd/mag/image_cli_test.go` | 555 |
| `TestSaveImage_OverwritesExistingFile` | Function | `cmd/mag/image_cli_test.go` | 606 |
| `TestLoadImage_CorruptFileReturnsError` | Function | `cmd/mag/image_cli_test.go` | 643 |
| `TestLoadImage_EmptyFileReturnsError` | Function | `cmd/mag/image_cli_test.go` | 659 |
| `TestImageRoundTrip_ClassMethodIsClassMethodFlag` | Function | `cmd/mag/image_cli_test.go` | 679 |
| `TestSaveImage_ReadOnlyDirectoryFails` | Function | `cmd/mag/image_cli_test.go` | 801 |

## Execution Flows

| Flow | Type | Steps |
|------|------|-------|
| `Run → ProgName` | cross_community | 4 |
| `Main → Session` | cross_community | 4 |

## Connected Areas

| Area | Connections |
|------|-------------|
| Vm | 104 calls |
| Pipeline | 35 calls |
| Bootstrap | 16 calls |
| Manifest | 9 calls |
| Gowrap | 4 calls |
| Server | 4 calls |
| Maggiev1connect | 3 calls |
| Compiler | 3 calls |

## How to Explore

1. `gitnexus_context({name: "TestVMLookupClass"})` — see callers and callees
2. `gitnexus_query({query: "mag"})` — find related execution flows
3. Read key files listed above for implementation details
