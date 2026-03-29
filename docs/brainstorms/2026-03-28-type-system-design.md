# Type System Architecture for Maggie

**Date:** 2026-03-28
**Source:** Language architect consultation
**Status:** Brainstorm — no implementation decisions made yet

---

## Executive Summary

The right design for Maggie is **structural gradual typing with content-hash-separated type layers, protocol-based inference from message sends, and effect annotations that compose with process restrictions**. This is not Strongtalk warmed over — the content-addressed code model creates a genuinely novel design space where types can be both erased (for backward compatibility) and first-class (for distribution verification) simultaneously.

---

## 1. Types and Content Hashing: The Two-Layer Model

This is the most consequential decision and the one where Maggie's architecture creates an opportunity no prior system has had.

**Recommendation: Two hashes per method — one semantic, one typed.**

The semantic hash (what you already have) ignores type annotations entirely. The typed hash includes them. Both are stored in the ContentStore. The semantic hash is the method's identity for execution. The typed hash is the method's identity for verification.

```
CompiledMethod
  contentHash    [32]byte   // existing: normalized AST, De Bruijn, no types
  typedHash      [32]byte   // new: normalized AST + type annotations
  typeSignature  *TypeSig   // new: parsed type info, nil if untyped
```

**Why two hashes instead of one?**

If types are part of the single hash, then adding type annotations to existing code changes its identity. Every node in the distributed network that has the untyped version now has a "different" method. You lose the Unison property that identical semantics produce identical hashes. Code sharing fractures along typed/untyped lines.

If types are excluded from the single hash, then you cannot use hashes to verify type-level properties across nodes. Two methods with the same hash could have contradictory type claims. The distributed verification story collapses.

Two hashes solve both problems. The semantic hash preserves code identity across typing states. The typed hash enables distributed type verification. A node can say "I have method abc123 (semantic) and I've verified it as typed variant def456." Another node that trusts the first node's type checker can accept def456 without re-checking.

This is genuinely novel. Unison conflates types into identity (which forces everyone into the type system). Strongtalk erases types entirely (which prevents distributed type reasoning). The two-layer model gives you both.

**Interaction with De Bruijn encoding:** Type variable references in polymorphic types should also use De Bruijn indices within the typed hash. If you have `<T> method: at: index <Integer> -> <T>`, the `T` references use indices, so alpha-equivalent type signatures produce the same typed hash.

---

## 2. Structural Typing as the Default, Nominal as Opt-In

**Recommendation: Protocol-based structural typing is the default. Nominal class types are available but secondary.**

Smalltalk's entire message-passing philosophy is structural. When you write `anObject size`, you don't care whether it's an Array, a String, or a Dictionary — you care that it responds to `size`. The type system should reflect this.

A protocol type is a set of message signatures:

```smalltalk
"Protocol definition — a named set of message signatures"
Sizeable protocol
  size -> <Integer>.
  isEmpty -> <Boolean>.

Indexable protocol
  includes: Sizeable.
  at: <Integer> -> <Object>.
  at: <Integer> put: <Object> -> <Object>.
```

Protocol types are structural: any class whose methods match the signatures satisfies the protocol without declaring that it does. This is duck typing made precise.

Nominal types (specific class names) work as shorthand: `<Array>` means "must be exactly an Array or subclass." But the idiomatic style should prefer protocols for parameter types and class names for return types, because parameters should be permissive and returns should be precise.

**What to steal from each system:**

- **From Strongtalk:** The `<Type>` angle-bracket syntax (it works in Smalltalk and doesn't conflict with anything). Protocol inclusion (`includes:`). The separation of type checking from execution.
- **From Go:** Implicit interface satisfaction. No `implements` declaration needed. If your class has the right methods, it satisfies the protocol.
- **From Typed Racket:** The concept of occurrence typing — after `anObject isKindOf: Integer` in a conditional branch, the type checker narrows the type within that branch without a cast.
- **From Grace:** Structural type matching with pattern-based syntax that doesn't feel like Java.

**What NOT to steal:**

- **From Java/C#:** Explicit `implements` declarations. They add ceremony without value in a structural system.
- **From TypeScript:** Union types as the primary composition mechanism. In Smalltalk, protocol inclusion is more natural than `Type1 | Type2`.
- **From Sorbet:** The `T.let` / `T.cast` ceremony. It works for Ruby's syntactic constraints but would be ugly in Smalltalk.

---

## 3. Type Inference from Message Sends

**Recommendation: Bidirectional local inference with message-send-driven constraint generation. No global Hindley-Milner.**

Full Hindley-Milner inference is wrong for Smalltalk for two reasons. First, Smalltalk's pervasive message passing creates constraint sets that are essentially protocol types — HM would infer principal types that are unreadably complex structural types. Second, HM requires a complete program — it doesn't work well with the incremental, image-based development model where methods are compiled independently.

Instead, use bidirectional local inference within a method body:

**Forward inference (synthesis):** From literals and known-typed expressions, propagate types forward through message sends.

```smalltalk
method: example
  | x |
  x := 42.              "x inferred as <Integer>"
  x := x + 1.           "still <Integer>, because Integer>>+ returns Integer"
  ^x asString            "return type inferred as <String>"
```

**Backward inference (checking):** From an annotated parameter or return type, propagate constraints backward.

```smalltalk
method: process: items <Indexable>
  items do: [:each |
    "each inferred as <Object> (Indexable>>do: yields elements typed by the protocol)"
    each printString     "known to work — Object responds to printString"
  ]
```

**Message-send constraint generation:** When the type checker sees `anObject foo: bar`, it generates the constraint "anObject must respond to `foo:` with an argument compatible with bar's type." This naturally produces protocol-shaped constraints without the programmer writing protocol definitions.

The key insight is that **most Smalltalk code is already implicitly typed by its message sends.** The type checker's job is to make these implicit types explicit and verify consistency, not to force the programmer to write annotations.

**What inference buys you:** For a typical Maggie method, the programmer writes zero type annotations and the checker infers return types, local variable types, and the implicit protocol requirements on parameters. Annotations are only needed at API boundaries (public method signatures) and for disambiguation.

**What to explicitly NOT do:**

- **Don't infer across method boundaries globally.** Each method is a unit of inference. Cross-method reasoning uses declared signatures (or inferred signatures that have been cached). This keeps compilation incremental and comprehensible.
- **Don't try to infer through `perform:` or `Compiler evaluate:`.** These are escape hatches into dynamic land. The type checker should treat them as returning `<Dynamic>` (the any-type).
- **Don't require complete type coverage.** Partially typed code is fine. Untyped methods have signature `<Dynamic> -> <Dynamic>`. Typed methods calling untyped methods get `<Dynamic>` at the boundary, not an error.

---

## 4. Types and the Distributed Code Model

**Recommendation: Type signatures are part of the wire protocol for value transfer, but verification is opt-in per node.**

When Maggie serializes a value for transfer between nodes (via the existing CBOR-based distribution protocol), the typed hash can accompany the payload. The receiving node can then:

1. **Skip verification** — trust the sender (current behavior, zero cost)
2. **Check the typed hash** — verify the payload matches the expected type signature without running the type checker (cheap: just hash comparison)
3. **Full verification** — run the type checker on the received code (expensive, for untrusted sources)

This composes with the existing reputation system in `vm/dist/reputation.go`. A node with high reputation gets level 1 trust (hash check only). An unknown node gets level 3 (full verification). A banned node gets rejected.

**The compelling use case:** Remote process spawning. When you fork a restricted process on a remote node, the type signature tells the remote node what capabilities the process needs without executing any code. Combined with effect types, you could verify at the type level that a forked process is pure before accepting it for execution.

---

## 5. Refinement Types: Yes, But Bounded

**Recommendation: Support refinement types as compile-time-checked contracts with explicit opt-in to runtime checking.**

```smalltalk
"Refinement type definition"
PositiveInteger type: Integer where: [:n | n > 0].
NonEmptyString type: String where: [:s | s isEmpty not].
Percentage type: Integer where: [:n | n >= 0 and: [n <= 100]].
```

The refinement predicate is a block that takes a value and returns a Boolean. At compile time, the type checker can verify some refinements statically (e.g., if a literal `42` is passed where `<PositiveInteger>` is expected, no runtime check needed). Where static verification is impossible, the type checker inserts a runtime assertion — but **only if the method opts in**.

**The zero-runtime-cost question:** Pure refinement checking violates the Strongtalk invariant of zero runtime cost, because some refinements require runtime checks. The resolution: refinement predicates are checked by the type checker at compile time where possible. Where not possible, the compiler emits a guard **only in checked mode**. In release mode, the guards are elided.

```bash
mag build --checked      # refinement guards included
mag build                # refinement guards elided (zero cost)
mag test                 # always checked
```

**What NOT to do with refinement types:**

- Don't allow arbitrary computation in predicates. Restrict to a decidable subset: comparisons, logical connectives, `isKindOf:`, `respondsTo:`, `isEmpty`, `size`.
- Don't make refinement types nominal. Structural equivalence applies.

---

## 6. Effect Types: Aligned with Process Restrictions

**Recommendation: Lightweight effect annotations that mirror the process restriction system.**

```smalltalk
method: computeTotal: items <Indexable> -> <Integer> ! <Pure>
  items inject: 0 into: [:sum :item | sum + item price]

method: saveReport: data <String> -> <Boolean> ! <IO>
  File write: data to: '/tmp/report.txt'.
  ^true

method: fetchAndStore: url <String> -> <String> ! <IO, Network>
  | data |
  data := HTTP get: url.
  File write: data to: '/tmp/cache.txt'.
  ^data
```

Effects:
- `<Pure>` — no effects
- `<IO>` — file system access
- `<Network>` — network access
- `<State>` — mutation of shared mutable state
- `<Process>` — spawning processes, channel operations

**The connection to process restrictions:** `forkRestricted:` hides globals at runtime. Effect types verify the same constraints at compile time.

**Effect inference:** Effects propagate upward automatically. If method A calls method B which has `! <IO>`, then A also has `! <IO>` unless it's explicitly annotated as `<Pure>` (which would be a type error).

---

## 7. Concrete Syntax

### Class definitions with types

```smalltalk
Account subclass: Object
  instanceVars: owner <String> balance <Integer> history <Array <Transaction>>

  classMethod: new: owner <String> -> <Account>
    ^self basicNew init: owner

  method: deposit: amount <PositiveInteger> -> <Self> ! <State>
    balance := balance + amount.
    history add: (Transaction deposit: amount).

  method: summary -> <String> ! <Pure>
    ^'Account(', owner, ': $', balance printString, ')'
```

### Block types

```smalltalk
method: filter: predicate <[:Account -> <Boolean>]> -> <Array <Account>>
  ^accounts select: predicate
```

### Gradual boundaries

```smalltalk
"Fully untyped — works exactly as today"
method: doStuff: thing
  ^thing foo + thing bar

"Partially typed"
method: process: thing -> <String>
  ^thing printString

"Fully typed"
method: calculate: x <Number> y <Number> -> <Number> ! <Pure>
  ^(x * x) + (y * y)
```

### Occurrence typing

```smalltalk
method: describe: value <Object> -> <String>
  (value isKindOf: Integer) ifTrue: [
    "value is <Integer> here"
    ^'Number: ', value printString
  ].
  (value respondsTo: #size) ifTrue: [
    "value is <Sizeable> here"
    ^'Collection of ', value size printString, ' items'
  ].
  ^value printString
```

---

## 8. What Is Novel vs. Reimplemented

**Genuinely novel to Maggie:**

1. **The two-layer hash model.** No existing system separates semantic identity from typed identity.
2. **Effect types that mirror a runtime enforcement mechanism.** Mapping effect types onto existing process restrictions.
3. **Distributed type verification via typed hashes.** Type checking amortized across a network.

**Reimplemented from existing systems:**

- Protocol-based structural typing: Strongtalk (1993), Go (2009), Grace (2012)
- Gradual typing with Dynamic escape hatch: Typed Racket (2006), TypeScript (2012)
- Occurrence typing: Typed Racket (2010), TypeScript (2014), Kotlin
- Refinement predicates: Liquid Haskell (2012), Racket contracts (2002)

**Honest assessment:** About 60% well-understood technology applied to Smalltalk syntax. The two-layer hash model and effect-restriction alignment are the genuinely new contributions.

---

## 9. Anti-Patterns to Avoid

1. **Don't make the type checker block compilation.** Type errors are warnings, never failures.
2. **Don't add runtime type checks by default.** Zero-cost erasure (Strongtalk invariant).
3. **Don't try to type `doesNotUnderstand:`.** Treat as `<Dynamic>`.
4. **Don't create annotations more complex than the code.** If the type signature is longer than the method body, the system has failed.
5. **Don't require annotations to use the standard library.**
6. **Don't invent new syntax where Smalltalk syntax works.**
7. **Don't try to make the type system sound.** `perform:`, `evaluate:`, `doesNotUnderstand:` make soundness impossible. Useful beats correct.

---

## 10. Implementation Architecture

```
Source → Lexer → Parser → AST → Compiler (bytecode, semantic hash)
                           ↓
                      Type Checker (typed hash, diagnostics)
```

The type checker lives in a `types/` package that depends on `compiler/` (for AST types) but not on `vm/`. Runs as an LSP feature without starting the VM.

```
types/
  checker.go       — bidirectional type checking
  infer.go         — message-send inference, occurrence typing
  protocol.go      — protocol definition, structural matching
  effects.go       — effect annotation, propagation
  refinement.go    — refinement predicates, static checking
  hash.go          — typed hash computation
  env.go           — type environment (scopes, bindings)
```

---

## 11. Phasing Recommendation

1. **Protocols and structural matching.** Most value with least syntax.
2. **Local type inference from message sends.** Where it starts feeling like magic.
3. **Typed hash and distribution integration.** Requires reliable checker.
4. **Effect annotations.** Requires protocols and inference to be stable.
5. **Refinement types.** Most complex. Save for last.

Each phase is independently useful.

---

## Key Decisions Summary

| Decision | Choice | Rationale |
|---|---|---|
| Hash interaction | Two hashes (semantic + typed) | Preserve code identity; enable distributed verification |
| Structural vs nominal | Structural default (protocols) | Matches Smalltalk's message-passing philosophy |
| Inference | Bidirectional local, message-send-driven | Incremental compilation; comprehensible errors |
| Annotation syntax | `<Type>` after names, `! <Effect>` at end | Strongtalk convention; minimal visual disruption |
| Soundness | Deliberately unsound | `perform:`, `evaluate:`, `doesNotUnderstand:` make soundness impossible |
| Refinement checking | Compile-time where possible, guarded opt-in | Zero-cost default; checked mode for development |
| Effect types | Mirror process restrictions | Existing runtime mechanism; static verification bonus |
| Type errors | Warnings, never compilation blockers | Preserve Smalltalk live-coding model |
| Runtime cost | Zero (type erasure) | Strongtalk invariant; non-negotiable |
