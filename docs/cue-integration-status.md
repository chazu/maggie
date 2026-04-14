# CUE Integration in Maggie: Status, Plans, and Type System Intersection

**Date:** 2026-03-29

---

## Current Integration (Complete)

Maggie has deep CUE integration across three subsystems, all production-ready with full test coverage and documentation.

### 1. CUE as a Library (`CueContext` / `CueValue`)

The foundation layer provides CUE compilation, validation, and value manipulation as first-class Maggie objects.

**CueContext** (6 methods): `compileString:`, `loadFile:`, `loadDir:`, `validate:against:`, `inject:into:`

**CueValue** (14 methods): `validate`, `unify:`, `lookup:`, `fillPath:with:`, `fields`, `toMaggie`, `toJSON`, `kind`, `isConcrete`, `exists`, `error`, `matchesObject:`, `subsumes:`, `subsumedBy:`, `printString`

```smalltalk
ctx := CueContext new.
schema := ctx compileString: '{ name: string, age: int & >0 }'.
data := ctx compileString: '{ name: "Alice", age: 30 }'.
(schema unify: data) value validate   "→ Success"
```

### 2. Object Projection (`Object>>asCueValue`)

Any Maggie object can be projected into the CUE value lattice. Objects with named instance variables become CUE structs (field names match ivar names). Scalars project as CUE scalars.

```smalltalk
user := User new name: 'Alice' age: 30.
cueVal := user asCueValue.
cueVal kind                    "→ 'struct'"

"Template matching: does the object satisfy the schema?"
template := ctx compileString: '{ name: string, age: int & >0 }'.
template matchesObject: user   "→ true"
```

This is the bridge between Smalltalk's mutable object world and CUE's immutable value lattice. The projection is a one-way snapshot — changes to the object are not reflected in the CUE value.

### 3. Subsumption (Entailment Checking)

CUE's `Subsume()` operation is exposed as `subsumes:` / `subsumedBy:`:

```smalltalk
intType := ctx compileString: 'int'.
fortytwo := ctx compileString: '42'.
intType subsumes: fortytwo      "→ true (int is more general than 42)"
fortytwo subsumedBy: intType    "→ true"
```

This is the primitive that powers the constraint store's `ask` operation.

### 4. TupleSpace with CUE Template Matching

A Linda-style coordination space where templates are CUE values. Matching uses CUE unification — a tuple matches a template if their unification doesn't produce bottom.

```smalltalk
ts := TupleSpace new.
ts out: (ctx compileString: '{ type: "order", amount: 42 }').
template := ctx compileString: '{ type: "order", amount: int & >0 }'.
result := ts in: template.     "→ the matching tuple (consumed)"
```

**Tuple modes** (linear logic semantics):
- **Linear** (`out:`): consumed exactly once
- **Persistent** (`outPersistent:`): never consumed; `in:` returns a copy. Models the exponential `!A` from linear logic.
- **Affine** (`outAffine:ttl:`): consumed at most once, auto-removed after TTL.

**Compound operations**: `inAll:` (atomic multi-take / tensor product), `inAny:` (choice over multiple templates).

### 5. Constraint Store (Concurrent Constraint Programming)

A monotonic store that starts as CUE top (unconstrained) and narrows via unification.

```smalltalk
store := ConstraintStore new.
store tell: (ctx compileString: '{ x: int }').        "narrow: x is int"
store tell: (ctx compileString: '{ x: >0 }').         "narrow: x is positive int"
store tryAsk: (ctx compileString: '{ x: int & >0 }')  "→ true (entailed)"
```

`ask:` blocks until the query is entailed by the store. `watch:do:` registers a callback that fires when entailment is achieved. This implements the "ask" primitive from concurrent constraint programming (Saraswat 1993).

---

## Future Plans (Documented but Unimplemented)

### Layer 2: Class-Level CUE Schemas

Each class gains an optional CUE schema that validates slot contents on instantiation and assignment.

```smalltalk
User subclass: Object
  instanceVars: name age
  schema: '{ name: string, age: int & >0 & <150 }'
```

The schema would be checked:
- On `new` / `basicNew` (all slots must satisfy)
- On ivar assignment (the modified slot must satisfy)
- Via `Object>>isValid` (explicit revalidation)

**Status:** Proposed in `CLAUDE.md` and `docs/constraint-programming-maggie-cue.md`. Not implemented.

### Layer 3: Method Guards (Predicate Dispatch)

CUE constraints as method applicability guards — a form of predicate dispatch where the "type test" is CUE subsumption.

```smalltalk
"Method dispatched only when amount > 1000"
method: process: order <{ amount: >1000 }> [
    self flagForReview: order
]

"Default case"
method: process: order [
    self approve: order
]
```

**Status:** Mentioned in `CLAUDE.md`. No design document yet.

### The Monotonicity Problem

CUE is monotonic (values can only be constrained further, never relaxed). Smalltalk is freely mutable. The tension:

- **CUE snapshots are stale.** `asCueValue` captures a moment in time. If you mutate the object and re-project, you get a different CUE value. The CUE value lattice has no notion of "this value changed."
- **Constraint stores can't retract.** Once you `tell:` a constraint, it's permanent. This is by design (monotonic = compositional), but it means the store can't model mutable state directly.

The proposed resolution (from `docs/constraint-programming-maggie-cue.md`) draws on Kaleidoscope's "stay constraints" — a constraint that holds until explicitly released. This would require extending the CUE integration beyond pure unification into a custom constraint solver. Deferred to a future phase.

---

## Intersection with the Type System

The type system (Phases 1-4) and CUE integration address related but distinct concerns. Here's where they overlap and how they could converge:

### Where They Overlap

**Structural typing ≈ CUE struct constraints.** A Maggie protocol like:

```smalltalk
Sizeable protocol
  size ^<Integer>.
  isEmpty ^<Boolean>.
```

is conceptually similar to a CUE struct constraint:

```cue
Sizeable: {
    size: int
    isEmpty: bool
}
```

Both describe structural expectations — "this thing must respond to these messages with these types." The difference is that Maggie protocols check message *availability* (does the class have the method?) while CUE checks value *shape* (does the data have the right fields with the right constraints?).

**Effect annotations ≈ capability checking.** The effect system's `! <Pure>` annotation and the process restriction system's `forkRestricted:` both answer the question "what can this code do?" Effects check it statically; restrictions enforce it at runtime. CUE schemas could provide a third angle: validating that data flowing into effectful operations satisfies structural constraints.

### Where They Don't Overlap

**Types are compile-time, CUE is runtime.** The type checker runs on ASTs before execution. CUE operations (`compileString:`, `unify:`, `matchesObject:`) happen during execution. They live in different phases of the program lifecycle.

**Types are about messages, CUE is about data.** The type system asks "does this class respond to `size`?" CUE asks "does this struct have a field `name` that's a string?" One is behavioral (protocol conformance), the other is structural (data shape).

**The typed hash includes type annotations but not CUE schemas.** The two-layer hash model hashes type annotations (parameter types, return types, effects) but not CUE constraint expressions. CUE constraints embedded in method guards (Layer 3) would need a separate hashing strategy.

### Potential Convergence Points

**1. CUE-backed protocol checking.** Instead of checking protocol conformance by walking the VM class table, the checker could project a class into CUE and check subsumption against a CUE-encoded protocol. This would unify the structural matching of protocols and CUE schemas under one mechanism.

**2. Effect inference from CUE schemas.** If a class has a CUE schema that references `File` or `Network` types, the type checker could infer effects from the schema rather than from the method body.

**3. Refinement types as CUE constraints.** The brainstorm doc proposes refinement types like `<Integer where: [:n | n > 0]>`. CUE already has constraint expressions (`int & >0`). Instead of building a custom refinement checker, refinements could compile to CUE constraints and use CUE's built-in validation. This would give refinement types the full power of CUE's constraint language (disjunctions, bounds, regular expressions, structural recursion) without implementing a custom solver.

**4. Typed content hashing for CUE schemas.** When Layer 2 (class schemas) lands, the schema text could be included in the typed hash alongside type annotations. Two classes with the same methods but different CUE schemas would have the same semantic hash but different typed hashes.

**5. Distributed type verification via CUE.** When a remote node receives code with type annotations, it could verify type correctness by compiling the annotations into CUE constraints and checking subsumption — leveraging CUE's existing verification infrastructure rather than building a separate type-level verifier.

### Recommended Sequencing

1. **Now:** Types and CUE are independent. Use both as-is.
2. **When Layer 2 (class schemas) is built:** Explore CUE-backed protocol checking. The type checker's `Satisfies()` could optionally use CUE subsumption for richer structural matching.
3. **When refinement types (Phase 5) are built:** Seriously consider compiling refinements to CUE constraints instead of building a custom predicate checker. This avoids the decidability boundary problem (CUE already solved it).
4. **When method guards (Layer 3) are built:** Method guard dispatch and type-based dispatch could share infrastructure — both are "does this argument satisfy this constraint?"

---

## Test Coverage

| Component | Test File | Tests | Lines |
|-----------|-----------|-------|-------|
| CUE primitives | `vm/cue_primitives_test.go` | ~40 | 910 |
| TupleSpace | `vm/tuplespace_test.go` | ~30 | 873 |
| ConstraintStore | `vm/constraint_store_test.go` | ~15 | 314 |
| CueContext doctests | `lib/CueContext.mag` | 1 | — |
| CueValue doctests | `lib/CueValue.mag` | 13 | — |
| TupleSpace doctests | `lib/TupleSpace.mag` | 18 | — |
| ConstraintStore doctests | `lib/ConstraintStore.mag` | 8 | — |
| Guide17 doctests | `lib/guide/Guide17CueIntegration.mag` | 26 | — |
| Guide18 doctests | `lib/guide/Guide18TupleSpace.mag` | 13 | — |

---

## File Reference

| File | Purpose |
|------|---------|
| `vm/cue_primitives.go` | CueContext + CueValue Go primitives (686 lines) |
| `vm/tuplespace_primitives.go` | TupleSpace Go primitives (726 lines) |
| `vm/constraint_store.go` | ConstraintStore Go primitives (314 lines) |
| `lib/CueContext.mag` | CueContext Maggie API |
| `lib/CueValue.mag` | CueValue Maggie API |
| `lib/TupleSpace.mag` | TupleSpace Maggie API |
| `lib/ConstraintStore.mag` | ConstraintStore Maggie API |
| `lib/guide/Guide17CueIntegration.mag` | CUE tutorial |
| `lib/guide/Guide18TupleSpace.mag` | TupleSpace + ConstraintStore tutorial |
| `docs/constraint-programming-maggie-cue.md` | Architectural discussion + future roadmap |
