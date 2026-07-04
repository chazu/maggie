---
name: maggiev1connect
description: "Skill for the Maggiev1connect area of maggie. 68 symbols across 14 files."
---

# Maggiev1connect

68 symbols | 14 files | Cohesion: 70%

## When to Use

- Working with code in `gen/`
- Understanding how TestCueValueValidateSuccess, TestCueValueValidateFailure, TestCueValueUnify work
- Modifying maggiev1connect-related functionality

## Key Files

| File | Symbols |
|------|---------|
| `gen/maggie/v1/maggiev1connect/sync.connect.go` | Announce, Transfer, Serve, Ping, Resolve (+11) |
| `vm/cue_primitives_test.go` | TestCueValueValidateSuccess, TestCueValueValidateFailure, TestCueValueUnify, TestCueValueUnifyConflict, TestCueValueFields (+7) |
| `gen/maggie/v1/maggiev1connect/browsing.connect.go` | ListClasses, GetClass, GetHierarchy, ListMethods, GetMethod (+5) |
| `gen/maggie/v1/maggiev1connect/modification.connect.go` | CompileMethod, RemoveMethod, CreateClass, SaveImage, EvaluateWithLocals (+1) |
| `gen/maggie/v1/maggiev1connect/inspection.connect.go` | Inspect, InspectSlot, InspectIndex, SendMessage, ReleaseHandle (+1) |
| `cmd/mag/docgen.go` | writeIndexPage, depthPrefix, writeClassPage, writeGuidePage |
| `gen/maggie/v1/maggiev1connect/session.connect.go` | CreateSession, DestroySession, Complete, NewSessionServiceHandler |
| `gen/maggie/v1/maggiev1connect/evaluation.connect.go` | Evaluate, EvaluateInContext, CheckSyntax, NewEvaluationServiceHandler |
| `server/sessions.go` | Create |
| `server/session_service_test.go` | TestSessionStore_CreateAndGet |

## Entry Points

Start here when exploring this area:

- **`TestCueValueValidateSuccess`** (Function) — `vm/cue_primitives_test.go:114`
- **`TestCueValueValidateFailure`** (Function) — `vm/cue_primitives_test.go:122`
- **`TestCueValueUnify`** (Function) — `vm/cue_primitives_test.go:134`
- **`TestCueValueUnifyConflict`** (Function) — `vm/cue_primitives_test.go:144`
- **`TestCueValueFields`** (Function) — `vm/cue_primitives_test.go:188`

## Key Symbols

| Symbol | Type | File | Line |
|--------|------|------|------|
| `TestCueValueValidateSuccess` | Function | `vm/cue_primitives_test.go` | 114 |
| `TestCueValueValidateFailure` | Function | `vm/cue_primitives_test.go` | 122 |
| `TestCueValueUnify` | Function | `vm/cue_primitives_test.go` | 134 |
| `TestCueValueUnifyConflict` | Function | `vm/cue_primitives_test.go` | 144 |
| `TestCueValueFields` | Function | `vm/cue_primitives_test.go` | 188 |
| `TestCueSchemaValidation` | Function | `vm/cue_primitives_test.go` | 386 |
| `TestCueSchemaValidationFail` | Function | `vm/cue_primitives_test.go` | 397 |
| `TestCueValidationErrorPath` | Function | `vm/cue_primitives_test.go` | 475 |
| `TestSubsumesIntType` | Function | `vm/cue_primitives_test.go` | 831 |
| `TestSubsumesRange` | Function | `vm/cue_primitives_test.go` | 846 |
| `TestSubsumesStruct` | Function | `vm/cue_primitives_test.go` | 860 |
| `TestSubsumesReflexive` | Function | `vm/cue_primitives_test.go` | 874 |
| `TestSessionStore_CreateAndGet` | Function | `server/session_service_test.go` | 227 |
| `NewSessionServiceImpl` | Function | `server/session_service.go` | 22 |
| `New` | Function | `server/server.go` | 65 |
| `NewInspectService` | Function | `server/inspect_service.go` | 22 |
| `NewBrowseService` | Function | `server/browse_service.go` | 22 |
| `NewSessionServiceHandler` | Function | `gen/maggie/v1/maggiev1connect/session.connect.go` | 124 |
| `NewModificationServiceHandler` | Function | `gen/maggie/v1/maggiev1connect/modification.connect.go` | 163 |
| `NewInspectionServiceHandler` | Function | `gen/maggie/v1/maggiev1connect/inspection.connect.go` | 163 |

## Execution Flows

| Flow | Type | Steps |
|------|------|-------|
| `Main → Session` | cross_community | 4 |

## Connected Areas

| Area | Connections |
|------|-------------|
| Server | 8 calls |
| Vm | 1 calls |

## How to Explore

1. `gitnexus_context({name: "TestCueValueValidateSuccess"})` — see callers and callees
2. `gitnexus_query({query: "maggiev1connect"})` — find related execution flows
3. Read key files listed above for implementation details
