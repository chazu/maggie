# Type System Phase 2: Local Type Inference

**Date:** 2026-03-29
**Depends on:** Phase 1 (protocols and structural matching) — COMPLETE
**Status:** COMPLETE

---

## Goal

Add forward type inference so the checker can catch "sent message to wrong type" errors without the programmer writing annotations. Literals have known types, assignments propagate types, and message sends look up return types from a built-in table + harvested annotations.

---

## Key Design Decisions

1. **Return type bootstrap:** Hybrid — ~40 built-in entries for arithmetic/comparison/collection core, plus harvesting `^<Type>` annotations from `.mag` files
2. **Inference depth:** Straight-line assignment chains + message send return lookup. No block body inference, no generic type parameters
3. **Re-assignment:** Last-write-wins. `x := 42; x := 'hello'` makes x String. No union types
4. **Verbose mode:** `mag typecheck --verbose` shows inferred types per variable
5. **Minimum viable:** Literal typing + assignment propagation + return type lookup + "does not understand" warnings

---

## Architecture

```
               ┌─────────────┐
               │  Checker     │  (existing — orchestrates)
               └──────┬──────┘
                      │ calls
               ┌──────▼──────┐
               │  Inferrer    │  NEW — forward type synthesis
               └──────┬──────┘
                      │ queries
          ┌───────────┼───────────┐
          │           │           │
   ┌──────▼──┐ ┌─────▼─────┐ ┌──▼──────────┐
   │ TypeEnv │ │ ReturnType │ │ Protocol    │
   │  (NEW)  │ │ Table (NEW)│ │ Registry    │
   └─────────┘ └───────────┘ │ (existing)  │
                              └─────────────┘
```

---

## Built-in Return Type Table (~40 entries)

| Class | Selectors | Return Type |
|-------|-----------|-------------|
| SmallInteger | `+ - * // \\ abs negated` | Integer |
| SmallInteger | `= < > <= >= ~=` | Boolean |
| SmallInteger | `asFloat` | Float |
| SmallInteger | `asString printString` | String |
| Float | `+ - * / abs negated` | Float |
| Float | `= < > <= >=` | Boolean |
| Float | `ceiling floor truncated rounded` | Integer |
| Float | `asString printString` | String |
| String | `size` | Integer |
| String | `, copyFrom:to:` | String |
| String | `= includes:` | Boolean |
| String | `asInteger` | Integer |
| String | `asFloat` | Float |
| Array | `size` | Integer |
| Array | `= isEmpty notEmpty includes:` | Boolean |
| Array | `, copyFrom:to:` | Array |
| Array | `asString printString` | String |
| Boolean | `not and: or:` | Boolean |
| Boolean | `asString printString` | String |
| Object | `= ~= isNil notNil` | Boolean |
| Object | `asString printString` | String |
| Object | `yourself` | Self |

---

## Inference Algorithm

### Per-method walk

1. Bind parameters from annotations (or Dynamic if untyped)
2. Bind temps from annotations (or leave unbound — first assignment sets type)
3. Bind `self` to the class being checked
4. Walk statements in order, threading the TypeEnv
5. Return type = last explicit return's type, or declared return type if annotated

### Expression type synthesis

| AST Node | Inferred Type |
|----------|--------------|
| IntLiteral | SmallInteger |
| FloatLiteral | Float |
| StringLiteral | String |
| SymbolLiteral | Symbol |
| CharLiteral | Character |
| NilLiteral | UndefinedObject |
| TrueLiteral / FalseLiteral | Boolean |
| ArrayLiteral / DynamicArray | Array |
| Self / Super | self's type |
| Variable(name) | env.Lookup(name), or Dynamic if unbound |
| Assignment(name, expr) | type of expr; sets env[name] |
| UnaryMessage(recv, sel) | inferSend(recvType, sel) |
| BinaryMessage(recv, sel, arg) | inferSend(recvType, sel) |
| KeywordMessage(recv, sel, args) | inferSend(recvType, sel) |
| Cascade(recv, msgs) | recvType (cascade returns receiver) |
| Block | Block (opaque — no body inference) |

### Message send return type lookup

1. If receiver is Dynamic → return Dynamic (no warning)
2. Look up receiver class + selector in ReturnTypeTable → return type if found
3. Check if receiver class responds to selector via VM class table → if not, emit "does not understand" warning
4. If class has method but no return type info → return Dynamic

---

## Warning generation

The checker emits warnings only when inference **positively determines** an incompatibility:

- `SmallInteger does not understand #isEmpty` (receiver type known, method missing)
- `inferred return type String is not assignable to declared Integer` (annotation mismatch)

No warnings when inference produces Dynamic (untyped code is left alone).

---

## Files to Create

| File | Purpose | Est. lines |
|------|---------|-----------|
| `types/inferrer.go` | Inferrer struct, InferMethod, inferExpr, inferSend | 250-350 |
| `types/inferrer_test.go` | Literal typing, assignment, send lookup, warnings | 200-300 |
| `types/type_env.go` | TypeEnv with scoping and merge | 60-80 |
| `types/type_env_test.go` | Scope and merge tests | 50-80 |
| `types/return_types.go` | ReturnTypeTable with built-in entries and harvest | 120-150 |
| `types/return_types_test.go` | Lookup and harvest tests | 60-80 |

## Files to Modify

| File | Change |
|------|--------|
| `types/checker.go` | Instantiate Inferrer in check methods, harvest return types |
| `types/checker_test.go` | Integration tests with inference |

## Total estimated: ~800-1100 new lines

---

## Implementation Order

1. **TypeEnv** — scoped type environment with merge
2. **ReturnTypeTable** — built-in entries + lookup
3. **Inferrer core** — literal typing, variables, assignment, message send return lookup, "does not understand" warnings
4. **Checker integration** — wire Inferrer into Checker, add harvest
5. **Annotation cross-checking** — inferred return vs declared return

---

## What Phase 2 Deliberately Omits

- Block body inference (blocks are `<Block>`, internal types not tracked)
- Union types (re-assignment is last-write-wins)
- Generic/parameterized types (no `Array<Integer>`)
- Cross-method inference (each method independent)
- Nil tracking / nil-safety
- Flow-sensitive narrowing (`isKindOf:` / `isNil` branch narrowing)
