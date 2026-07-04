---
name: v1
description: "Skill for the V1 area of maggie. 14 symbols across 7 files."
---

# V1

14 symbols | 7 files | Cohesion: 100%

## When to Use

- Working with code in `gen/`
- Understanding how init, file_maggie_v1_session_proto_init, init work
- Modifying v1-related functionality

## Key Files

| File | Symbols |
|------|---------|
| `gen/maggie/v1/session.pb.go` | init, file_maggie_v1_session_proto_init |
| `gen/maggie/v1/modification.pb.go` | init, file_maggie_v1_modification_proto_init |
| `gen/maggie/v1/inspection.pb.go` | init, file_maggie_v1_inspection_proto_init |
| `gen/maggie/v1/evaluation.pb.go` | init, file_maggie_v1_evaluation_proto_init |
| `gen/maggie/v1/common.pb.go` | init, file_maggie_v1_common_proto_init |
| `gen/maggie/v1/browsing.pb.go` | init, file_maggie_v1_browsing_proto_init |
| `gen/maggie/v1/sync.pb.go` | init, file_maggie_v1_sync_proto_init |

## Key Symbols

| Symbol | Type | File | Line |
|--------|------|------|------|
| `init` | Function | `gen/maggie/v1/session.pb.go` | 432 |
| `file_maggie_v1_session_proto_init` | Function | `gen/maggie/v1/session.pb.go` | 433 |
| `init` | Function | `gen/maggie/v1/modification.pb.go` | 807 |
| `file_maggie_v1_modification_proto_init` | Function | `gen/maggie/v1/modification.pb.go` | 808 |
| `init` | Function | `gen/maggie/v1/inspection.pb.go` | 634 |
| `file_maggie_v1_inspection_proto_init` | Function | `gen/maggie/v1/inspection.pb.go` | 635 |
| `init` | Function | `gen/maggie/v1/evaluation.pb.go` | 377 |
| `file_maggie_v1_evaluation_proto_init` | Function | `gen/maggie/v1/evaluation.pb.go` | 378 |
| `init` | Function | `gen/maggie/v1/common.pb.go` | 507 |
| `file_maggie_v1_common_proto_init` | Function | `gen/maggie/v1/common.pb.go` | 508 |
| `init` | Function | `gen/maggie/v1/browsing.pb.go` | 1281 |
| `file_maggie_v1_browsing_proto_init` | Function | `gen/maggie/v1/browsing.pb.go` | 1282 |
| `init` | Function | `gen/maggie/v1/sync.pb.go` | 1910 |
| `file_maggie_v1_sync_proto_init` | Function | `gen/maggie/v1/sync.pb.go` | 1911 |

## How to Explore

1. `gitnexus_context({name: "init"})` — see callers and callees
2. `gitnexus_query({query: "v1"})` — find related execution flows
3. Read key files listed above for implementation details
