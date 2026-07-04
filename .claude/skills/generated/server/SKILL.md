---
name: server
description: "Skill for the Server area of maggie. 270 symbols across 37 files."
---

# Server

270 symbols | 37 files | Cohesion: 76%

## When to Use

- Working with code in `server/`
- Understanding how TestSyncPing_Empty, TestSyncAnnounce_WantsAll, TestSyncAnnounce_InvalidRootHash work
- Modifying server-related functionality

## Key Files

| File | Symbols |
|------|---------|
| `server/lsp_test.go` | TestLSP_Complete, TestLSP_DocumentSymbol, TestExtractWord_SimpleWord, TestExtractWord_AtEnd, TestExtractWord_AtSpace (+25) |
| `server/browse_service_test.go` | TestListClasses_ReturnsClasses, TestListClasses_FilterByCategory, TestListClasses_ResultsAreSorted, TestGetClass_ValidClass, TestGetClass_NotFound (+24) |
| `server/inspect_service_test.go` | TestInspect_SmallInteger, TestInspect_Nil, TestInspect_Boolean, TestInspect_EmptyHandle, TestInspect_InvalidHandle (+18) |
| `server/modify_service_test.go` | TestCompileMethod_AddInstanceMethod, TestCompileMethod_AddClassSideMethod, TestCompileMethod_EmptyClassName, TestCompileMethod_EmptySource, TestCompileMethod_ClassNotFound (+17) |
| `server/lsp.go` | textDocumentCompletion, complete, textDocumentDidOpen, textDocumentDidChange, publishDiagnostics (+16) |
| `server/eval_service_test.go` | TestEvaluate_EmptySource, TestEvaluateInContext_MissingHandle, TestEvaluateInContext_InvalidHandle, TestEvaluate_SimpleInteger, TestEvaluate_Arithmetic (+10) |
| `server/browse_service.go` | ListClasses, GetClass, GetHierarchy, ListMethods, GetMethod (+8) |
| `server/sync_service_test.go` | TestSyncPing_Empty, TestSyncAnnounce_WantsAll, TestSyncAnnounce_InvalidRootHash, TestSyncAnnounce_RejectedByTrust, TestSyncTransfer_HashMismatch (+7) |
| `server/session_service_test.go` | TestCreateSession_Default, TestCreateSession_WithName, TestCreateSession_MultipleSessionsGetUniqueIDs, TestDestroySession_Valid, TestDestroySession_EmptyId (+7) |
| `server/modify_service.go` | NewModifyService, CompileMethod, RemoveMethod, CreateClass, SaveImage (+6) |

## Entry Points

Start here when exploring this area:

- **`TestSyncPing_Empty`** (Function) — `server/sync_service_test.go:28`
- **`TestSyncAnnounce_WantsAll`** (Function) — `server/sync_service_test.go:61`
- **`TestSyncAnnounce_InvalidRootHash`** (Function) — `server/sync_service_test.go:102`
- **`TestSyncAnnounce_RejectedByTrust`** (Function) — `server/sync_service_test.go:113`
- **`TestSyncTransfer_HashMismatch`** (Function) — `server/sync_service_test.go:173`

## Key Symbols

| Symbol | Type | File | Line |
|--------|------|------|------|
| `TestSyncPing_Empty` | Function | `server/sync_service_test.go` | 28 |
| `TestSyncAnnounce_WantsAll` | Function | `server/sync_service_test.go` | 61 |
| `TestSyncAnnounce_InvalidRootHash` | Function | `server/sync_service_test.go` | 102 |
| `TestSyncAnnounce_RejectedByTrust` | Function | `server/sync_service_test.go` | 113 |
| `TestSyncTransfer_HashMismatch` | Function | `server/sync_service_test.go` | 173 |
| `TestSyncTransfer_InvalidCBOR` | Function | `server/sync_service_test.go` | 201 |
| `TestSyncTransfer_NilCompileFunc` | Function | `server/sync_service_test.go` | 219 |
| `TestSyncTransfer_ClassChunk_MissingDep` | Function | `server/sync_service_test.go` | 291 |
| `TestSyncAnnounce_BannedPeer` | Function | `server/sync_service_test.go` | 323 |
| `TestSyncTransfer_BannedPeer` | Function | `server/sync_service_test.go` | 345 |
| `TestSyncServe_UnknownRoot` | Function | `server/sync_service_test.go` | 457 |
| `NewSyncService` | Function | `server/sync_service.go` | 32 |
| `TestCreateSession_Default` | Function | `server/session_service_test.go` | 14 |
| `TestCreateSession_WithName` | Function | `server/session_service_test.go` | 29 |
| `TestCreateSession_MultipleSessionsGetUniqueIDs` | Function | `server/session_service_test.go` | 55 |
| `TestDestroySession_Valid` | Function | `server/session_service_test.go` | 80 |
| `TestDestroySession_EmptyId` | Function | `server/session_service_test.go` | 105 |
| `TestDestroySession_NotFound` | Function | `server/session_service_test.go` | 122 |
| `TestComplete_ClassNames` | Function | `server/session_service_test.go` | 143 |
| `TestComplete_EmptyPrefix` | Function | `server/session_service_test.go` | 170 |

## Execution Flows

| Flow | Type | Steps |
|------|------|-------|
| `CompileAllFiles → Lexer` | cross_community | 5 |
| `CompileAllFiles → ReadChar` | cross_community | 5 |
| `CompileAllFiles → CurTokenIs` | cross_community | 5 |
| `CompileAllFiles → Errorf` | cross_community | 5 |
| `CompileAllFiles → NamespaceDecl` | cross_community | 5 |
| `Main → Parser` | cross_community | 5 |
| `Main → SourceFile` | cross_community | 5 |
| `Main → CurTokenIs` | cross_community | 5 |
| `Main → Errors` | cross_community | 4 |

## Connected Areas

| Area | Connections |
|------|-------------|
| Vm | 87 calls |
| Mag | 6 calls |
| Maggiev1connect | 5 calls |
| Compiler | 3 calls |
| Bootstrap | 2 calls |
| Hash | 2 calls |
| Gowrap | 1 calls |
| Types | 1 calls |

## How to Explore

1. `gitnexus_context({name: "TestSyncPing_Empty"})` — see callers and callees
2. `gitnexus_query({query: "server"})` — find related execution flows
3. Read key files listed above for implementation details
