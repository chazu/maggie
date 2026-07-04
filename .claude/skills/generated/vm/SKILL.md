---
name: vm
description: "Skill for the Vm area of maggie. 2930 symbols across 197 files."
---

# Vm

2930 symbols | 197 files | Cohesion: 61%

## When to Use

- Working with code in `vm/`
- Understanding how TestWeakReferencePrimitiveOn, TestWeakReferencePrimitiveGet, TestWeakReferencePrimitiveIsAlive work
- Modifying vm-related functionality

## Key Files

| File | Symbols |
|------|---------|
| `vm/concurrency_test.go` | TestChannelNew, TestChannelNewBuffered, TestChannelSendReceive, TestChannelTrySend, TestChannelTryReceive (+79) |
| `vm/image_reader_test.go` | newTestImageBuilder, writeUint32, writeUint64, writeString, writeBytes (+72) |
| `vm/vm.go` | NewVM, classValue, Concurrency, Registry, ConcurrencyStats (+69) |
| `vm/file_primitives_test.go` | TestFileListDirectoryEmpty, TestFileCreateDirectoryListAndDelete, fileClass, assertSuccess, assertFailure (+69) |
| `vm/interpreter_test.go` | TestPrimitiveAtArray, TestPrimitiveAtPutArray, TestPrimitiveSize, TestPrimitiveAtString, TestArithmeticOverflowPlus (+54) |
| `vm/benchmark_test.go` | BenchmarkSmallIntAdd, BenchmarkSmallIntMultiply, BenchmarkSmallIntCompare, BenchmarkFloatAdd, BenchmarkUnaryMessageSend (+53) |
| `vm/object_registry.go` | RegisterDictionary, GetDictionary, NewDictionaryValue, GetDictionaryObject, RegisterString (+52) |
| `vm/interpreter.go` | vtableForVM, primitivePlus, primitiveMinus, primitiveTimes, primitiveDiv (+52) |
| `vm/class.go` | ClassVarIndex, AllClassVarNames, NewInstance, NewClass, NewClassInNamespace (+48) |
| `vm/grpc_primitives_test.go` | TestIsGrpcClientValueFalse, TestGrpcClientMultipleRegistrations, TestUnregisterGrpcClientInvalidValue, TestGrpcSuccess, TestGrpcGlobalsRegistered (+47) |

## Entry Points

Start here when exploring this area:

- **`TestWeakReferencePrimitiveOn`** (Function) — `vm/weak_reference_test.go:264`
- **`TestWeakReferencePrimitiveGet`** (Function) — `vm/weak_reference_test.go:280`
- **`TestWeakReferencePrimitiveIsAlive`** (Function) — `vm/weak_reference_test.go:296`
- **`TestNewVM`** (Function) — `vm/vm_test.go:10`
- **`TestVMBootstrapClasses`** (Function) — `vm/vm_test.go:26`

## Key Symbols

| Symbol | Type | File | Line |
|--------|------|------|------|
| `TestWeakReferencePrimitiveOn` | Function | `vm/weak_reference_test.go` | 264 |
| `TestWeakReferencePrimitiveGet` | Function | `vm/weak_reference_test.go` | 280 |
| `TestWeakReferencePrimitiveIsAlive` | Function | `vm/weak_reference_test.go` | 296 |
| `TestNewVM` | Function | `vm/vm_test.go` | 10 |
| `TestVMBootstrapClasses` | Function | `vm/vm_test.go` | 26 |
| `TestVMBootstrapInheritance` | Function | `vm/vm_test.go` | 58 |
| `TestVMBootstrapGlobals` | Function | `vm/vm_test.go` | 80 |
| `TestVMClassFor` | Function | `vm/vm_test.go` | 102 |
| `TestVMSendBasic` | Function | `vm/vm_test.go` | 142 |
| `TestVMSendArithmetic` | Function | `vm/vm_test.go` | 164 |
| `TestVMSendComparison` | Function | `vm/vm_test.go` | 198 |
| `TestVMSendBoolean` | Function | `vm/vm_test.go` | 226 |
| `TestVMSendIdentity` | Function | `vm/vm_test.go` | 254 |
| `TestVMSendFloat` | Function | `vm/vm_test.go` | 277 |
| `TestVMSendYourself` | Function | `vm/vm_test.go` | 299 |
| `TestVMGlobals` | Function | `vm/vm_test.go` | 336 |
| `TestVMSendSymbol` | Function | `vm/vm_test.go` | 391 |
| `TestVMSendFloorDivision` | Function | `vm/vm_test.go` | 439 |
| `TestVMArrayPrimitives` | Function | `vm/vm_test.go` | 473 |
| `TestVMArrayWithFactoryMethods` | Function | `vm/vm_test.go` | 542 |

## Execution Flows

| Flow | Type | Steps |
|------|------|-------|
| `Bootstrap → VTable` | cross_community | 7 |
| `Bootstrap → Class` | cross_community | 6 |
| `Bootstrap → FromSymbolID` | cross_community | 6 |
| `ExecuteSpawnBlock → FromSymbolID` | cross_community | 6 |
| `ExecuteSpawnBlock → FromBool` | cross_community | 6 |
| `ExecuteSpawnBlock → FromSmallInt` | cross_community | 6 |
| `MonitorRemoteProcess → RLock` | cross_community | 6 |
| `MonitorRemoteProcess → RUnlock` | cross_community | 6 |
| `RegisterDuckDBPrimitives → VTable` | cross_community | 6 |
| `RegisterRandomPrimitives → VTable` | cross_community | 6 |

## Connected Areas

| Area | Connections |
|------|-------------|
| Compiler | 94 calls |
| Bootstrap | 75 calls |
| Server | 50 calls |
| Mag | 39 calls |
| Maggiev1connect | 34 calls |
| Types | 9 calls |
| Manifest | 3 calls |
| Hash | 3 calls |

## How to Explore

1. `gitnexus_context({name: "TestWeakReferencePrimitiveOn"})` — see callers and callees
2. `gitnexus_query({query: "vm"})` — find related execution flows
3. Read key files listed above for implementation details
