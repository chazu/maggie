# Constraint Programming, Smalltalk, and CUE Integration

*A conversation about constraint-based programming history, Kaleidoscope, and integrating CUE as a first-class citizen in Maggie (a Smalltalk dialect in Go)*

---

## Constraint-based programming in Smalltalk and related languages

**Q:** I'm wondering if work has been done on constraint based programming in Smalltalk or other similar languages like Self.

**A:**

Constraint-based programming has a rich history in Smalltalk and related prototype/object systems — it's actually one of the areas where Smalltalk shone early on.

### ThingLab (1977 — Alan Borning)

The canonical starting point. Borning built ThingLab on top of Smalltalk-76 at Xerox PARC, making it one of the earliest constraint-based simulation environments. It used a local propagation algorithm to satisfy constraints across object networks. ThingLab II (later ported to Smalltalk-80) refined this. The big idea was that constraints were *first-class objects* in the system, and the UI could be built declaratively by expressing relationships rather than imperative update logic.

### Sketchpad's lineage

ThingLab was consciously building on Sutherland's Sketchpad (1963), which had constraint satisfaction baked into its geometry. Borning saw Smalltalk's object model as the right substrate to resurrect and generalize that idea.

### Kaleidoscope

Borning and colleagues later developed the Kaleidoscope constraint language family ('90s), which explored integrating constraints directly into an OO language as a first-class construct rather than a library. It had multiple incarnations (Kaleidoscope '90, '93, '99) with increasing sophistication around *constraint hierarchies* — the idea that not all constraints are equal; some are required, others are preferences with weights. This was a big conceptual contribution.

### Self

Self didn't have a dedicated constraint system baked in, but it was used as a substrate for constraint UI work. The **Alternate Reality Kit (ARK)** and related systems at Sun's Self group experimented with physics-like constraint propagation in the UI. Self's prototype chain and slot model were actually quite amenable to constraint propagation because you could intercept slot access naturally.

### Squeak — Etoys & Morphic

The Squeak Morphic system included a simple constraint mechanism for layout. More interestingly, **SqueakDBX** and various projects around **Constraints for Morphic** explored making UI layout fully constraint-driven à la Cassowary.

### Cassowary

This is the algorithm that came out of the constraint UI research lineage (Greg Badros, Borning, et al., late '90s). It's an incremental simplex-based solver optimized for UI constraints. It was originally implemented with Smalltalk experiments in mind and is now the engine behind Apple's Auto Layout and Flutter's layout system — so this research thread has had enormous downstream impact.

### Oz/Mozart

Not Smalltalk, but worth mentioning in the same breath — Oz was a concurrent constraint language with a deeply prototype-flavored object system and first-class constraint variables. The Mozart implementation is the landmark artifact. If you're interested in how constraints interact with concurrency and message passing (relevant to tuplespace work!), the Oz computation model is fascinating — constraint stores and dataflow variables are essentially a form of structured shared memory with monotonic update semantics.

---

For the tuplespace angle specifically — the **Kaleidoscope constraint hierarchy** model has interesting resonance with how you might think about tuplespace tuple schemas and their lifecycle classes. The idea of "required vs. preferred" constraints maps naturally onto things like "this tuple must exist" vs. "this tuple should be consumed within N seconds." Borning's papers are very readable if you haven't dug into them.

---

## Considering CUE as a first-class citizen in a Smalltalk-in-Go

**Q:** I've got a Smalltalk I've implemented in Golang and I'm considering adding CUElang as a first class citizen somehow. Thoughts?

**A:**

### The Core Resonance

CUE's value lattice and Smalltalk's object model have a deep structural similarity that isn't immediately obvious:

- CUE's `⊤` (top) ≈ Smalltalk's `Object` / `nil` slot (unconstrained)
- CUE unification (`&`) ≈ prototype delegation / slot inheritance narrowing
- CUE disjunction (`|`) ≈ a slot that accepts multiple shapes
- CUE `⊥` (bottom/error) ≈ `doesNotUnderstand:` / constraint violation
- CUE open structs (`...`) ≈ open prototypes; closed structs ≈ sealed objects

CUE is essentially a constraint system over a lattice of partial values, which maps cleanly onto the ThingLab/Kaleidoscope lineage. The big difference is that CUE is **monotonic** — you only ever add information, never retract it. Smalltalk is heavily mutable. That tension is the interesting design problem.

### Integration Approaches

**1. CUE as schema validation layer (shallow)**

The boring but immediately useful version: CUE `#Definition`s describe the shape of Smalltalk objects. Slot assignment validates against the schema. Low integration cost since `cuelang.org/go/cue` is a solid Go library.

**2. CUE values as first-class objects (medium)**

Expose CUE's value lattice as actual Smalltalk objects — `CUEValue`, `CUEStruct`, etc. — that respond to messages like `unify:`, `disjoin:`, `complete`, `isBottom`. You get CUE's constraint propagation as a programmable substrate inside Smalltalk. This is where it starts to feel like Kaleidoscope — constraints as objects that live alongside regular objects.

**3. CUE as the prototype slot descriptor (deep)**

The genuinely novel integration: Smalltalk prototypes *are* CUE structs. Each slot has a CUE value governing what it can hold. Delegation chains map to CUE struct embedding. When you send a message, you're doing a CUE lookup in a struct hierarchy.

```
parent := {
    x: number
    greet: Method
}

child := parent & {
    x: >0 & <100   // narrowing the constraint
    name: string
}
```

Slot inheritance is value unification narrowing down the lattice. A child object can only *tighten* constraints, never loosen them — which is actually a cleaner model than most prototype chains.

**4. CUE constraints as method guards (predicate dispatch)**

Use CUE constraints as guards on method applicability:

```smalltalk
Shape>>area [cue: {sides: >2}]
    "only dispatches if receiver has sides > 2"
```

This is predicate dispatch with a richer, composable constraint language instead of arbitrary boolean predicates.

### The Monotonicity Problem

The hardest design issue: CUE is monotonic (add-only), Smalltalk assumes mutation. Options:

- **Versioned frames**: each mutation creates a new CUE value. Expensive but pure.
- **Escape hatch**: CUE governs *descriptive* slots; mutable state lives in a separate non-CUE slot layer.
- **Lean into it**: CUE-backed slots are *specifications* that constrain a separate mutable *state* slot. The CUE value is the contract; actual runtime state must satisfy it.

The last option has real appeal for tuplespace work — a tuple template is basically a CUE partial struct, and matching is unification.

### Tuplespace Angle

CUE unification is a *much* richer tuple matching primitive than Linda's equality-based templates. If the tuplespace uses CUE-backed tuple schemas, then `In` takes a CUE partial struct as template, and matching is unification — any tuple whose value unifies with the template without hitting bottom is a match. You get wildcard matches, range matches, structural matches all from the same mechanism.

---

## Reading order for Borning's papers

**Q:** What order should I read Borning's papers to understand Kaleidoscope most quickly?

**A:**

1. **"ThingLab" (1979, CACM)** — the short paper, not the full thesis. Just enough to have the ThingLab mental model as foundation. 20 minutes.

2. **"Constraint Hierarchies" (1992, Borning, Freeman-Benson, Wilson, LISP and Symbolic Computation)** — read this before any Kaleidoscope paper. It's the core conceptual contribution (required vs. preferential constraints, comparators). Everything in Kaleidoscope assumes it.

3. **"Kaleidoscope: Mixing Objects, Constraints, and Imperative Programming" (1990, Freeman-Benson, Maloney, Borning, OOPSLA)** — the first Kaleidoscope paper. Relatively short, readable, shows the basic integration ideas.

4. **"Kaleidoscope '93" (Lopez, Freeman-Benson, Borning)** — skimable if you read '90 carefully, but has the refined semantics around constraint satisfaction in the presence of mutation.

Skip Kaleidoscope '99 until you have a specific reason to go there — it's the most complete but also the most baroque. The constraint hierarchies paper is the real payload.

---

## Kaleidoscope's solution to the monotonicity problem

**Q:** So Kaleidoscope's solution to the monotonicity problem was versioned frames, basically — that and hierarchies of constraints are the big lessons of Kaleidoscope?

**A:**

Mostly yes, but the monotonicity answer is more subtle than versioned frames.

Kaleidoscope's actual answer is **stay constraints**. Rather than versioning the world, the weakest level of the hierarchy is a "stay" constraint: *prefer this value remains as it is*. Assignment becomes "introduce a stronger constraint that this equals X, which outranks the existing stay." The stay loses, the new value wins, no frame versioning needed.

So mutation is reframed as **constraint override via hierarchy** rather than time-travel or snapshotting. The monotonicity problem dissolves because you're not accumulating constraints that all must hold simultaneously — the hierarchy is a total order on *which constraints win when they conflict*, and stay constraints are always at the bottom.

That's actually the piece Cassowary inherits most directly — required constraints, then strong/medium/weak preferentials, with stay constraints as the implicit weakest preference underlying all mutable state. When you use Auto Layout and a view "springs back" unless you pin it, that spring is a stay.

The two real lessons:

1. **Constraint hierarchies with comparators** — you know this from Cassowary
2. **Stay constraints as the mutation model** — assignment-as-constraint-override, not versioning

The second one is the less obvious insight and the one that makes the whole thing practically usable. It's also the piece most worth thinking about when integrating CUE with a mutable Smalltalk — CUE has no equivalent of stay, which is part of why the monotonicity mismatch is real.

---

## Code review and concrete integration advice for Maggie

**Q:** My Smalltalk implementation lives at github.com/chazu/maggie — can you take a look at it? Then let's discuss thoughts on integrating CUElang as a first-class citizen.

**A:** *(After reviewing the repository)*

**What's already there:** `CueContext` and `CueValue` are solid, ergonomic wrapper objects — compile, unify, lookup, fillPath, validate, toMaggie, inject. They live as NaN-boxed symbols alongside everything else. The conversion in both directions (`cueToMaggie`, `cueExportValue`) is well thought out.

**What's missing:** CUE as part of the *object model*. Right now it's a library you call explicitly — like a JSON library. "First class" means CUE participating in class definition, slot assignment, and method dispatch.

### Layer 1: `Object>>asCueValue` (low cost, high leverage)

Add a primitive that builds a CUE struct from any object's current ivar layout — ivar names become field names, slot values get exported via `cueExportValue`. The `AllInstVarNames()` / `GetSlot(i)` machinery is already there, so this is a dozen lines.

```smalltalk
| cueView |
cueView := somePoint asCueValue.
"=> a CueValue: {x: 3, y: 7}"
```

This is the "read projection" — a cheap snapshot of mutable state into the CUE lattice. Immediately enables tuplespace matching: `template matchesObject: tuple` unifies the template against the tuple's CUE projection and checks for `⊥`. Linda semantics replaced by CUE unification — structural, range-aware, composable.

### Layer 2: `Class.CueSchema` (medium cost, the Kaleidoscope layer)

Add an optional `cueSchema *cue.Value` field to `Class`. In `.mag` syntax:

```smalltalk
Point subclass: Object
  instanceVars: x y
  schema: '{x: number, y: number}'
```

The schema is compiled once at class definition time and cached. Add a `SetSlotValidating` path (behind a flag, opt-in per class) that before writing checks:

```go
if class.CueSchema != nil {
    fieldName := class.AllInstVarNames()[index]
    partial := class.CueSchema.FillPath(cue.ParsePath(fieldName), newGoValue)
    if partial.Err() != nil { /* constraint violation */ }
}
```

Required fields (concrete type) are enforced hard; optional/constrained fields (`x: >0`) are enforced softly. The hierarchy of required vs. preferred maps to CUE's required fields vs. default-value fields — you get Kaleidoscope's insight almost for free from CUE's own semantics.

### Layer 3: Method guards (predicate dispatch)

The `Method` interface already abstracts over primitive and compiled methods. Add an optional `CueGuard *cue.Value` to `CompiledMethod`. At dispatch, before invoking, check:

```go
if method.CueGuard != nil {
    projection := vm.objectAsCueValue(receiver) // Layer 1
    unified := method.CueGuard.Unify(projection)
    if unified.Err() != nil { /* guard failed, try next */ }
}
```

In `.mag`:

```smalltalk
method: describe [cue: '{speed: >100}']
    ^'fast'

method: describe [cue: '{speed: <=100}']
    ^'slow'
```

### Practical sequencing

1. `asCueValue` on Object — ~20 lines, no model changes, immediately useful for moor tuplespace matching
2. `matchesObject:` on CueValue — uses #1, enables template matching
3. `Class.CueSchema` + validate on instantiation — enforce invariants at construction without touching hot paths
4. `SetSlotValidating` behind a flag — opt-in per class
5. Method guards last — biggest vtable surgery, but everything from 1-4 informs the design

Worth noting: `become:` is already implemented with the forwarding pointer system. That gives an interesting option for "CUE-constrained proxy" objects down the road — a proxy that holds a CUE schema and validates before forwarding mutations to the real object. No changes to Object required.
