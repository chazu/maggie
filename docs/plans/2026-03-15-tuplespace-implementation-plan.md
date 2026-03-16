# Tuplespace & Concurrent Constraint Programming Implementation Plan

**Date:** 2026-03-15
**Status:** Active
**Depends on:** CUE Layer 1 (complete)

---

## Phase 1: CUE Subsumption Primitive

**Goal:** Add entailment checking to CueValue — "is A more general than B?"

**Why:** Unification checks compatibility (can these merge without error?). Subsumption checks entailment (does A already imply B?). The constraint store needs subsumption for `ask` — "does the store's accumulated state entail my query?"

### Implementation

Add two Go primitives to `vm/cue_primitives.go`:

```go
// CueValue>>primSubsumes: — does self subsume other?
// self is more general, other is more specific
// e.g., `int` subsumes `42`, `>0` subsumes `5`
cueValueClass.AddMethod1(vm.Selectors, "primSubsumes:", func(...) Value {
    // self.val.Subsume(other.val, cue.Final()) == nil means subsumes
})

// CueValue>>primSubsumedBy: — is self subsumed by other?
cueValueClass.AddMethod1(vm.Selectors, "primSubsumedBy:", func(...) Value {
    // other.val.Subsume(self.val, cue.Final()) == nil
})
```

Add `.mag` wrappers in `lib/CueValue.mag`:

```smalltalk
method: subsumes: other [ ^self primSubsumes: other ]
method: subsumedBy: other [ ^self primSubsumedBy: other ]
```

### Tests

Go unit tests in `vm/cue_primitives_test.go`:
- `int` subsumes `42` → true
- `42` subsumes `int` → false
- `>0 & <100` subsumes `50` → true
- `>0 & <100` subsumes `200` → false
- `{x: int}` subsumes `{x: 42}` → true
- `{x: int}` subsumes `{x: "hello"}` → false
- Reflexivity: A subsumes A → true

Doctests on `CueValue>>subsumes:` and in Guide15.

### Files

- `vm/cue_primitives.go` — add two primitives
- `vm/cue_primitives_test.go` — unit tests
- `lib/CueValue.mag` — wrappers + docstrings
- `lib/guide/Guide15CueIntegration.mag` — add subsumption section

---

## Phase 2: Local TupleSpace

**Goal:** Linda-style tuplespace with CUE unification matching, blocking operations via goroutine suspension.

### Data Structure

```go
type TupleEntry struct {
    value   Value         // the Maggie object stored as a tuple
    cueProj *CueValueObject // cached CUE projection (lazily computed)
    mode    TupleMode     // linear, affine, persistent
}

type TupleSpaceObject struct {
    mu       sync.Mutex
    tuples   []TupleEntry       // stored tuples
    waiters  []*tupleWaiter     // blocked in/read operations
}

type tupleWaiter struct {
    template *CueValueObject   // CUE template to match against
    ch       chan Value         // wake channel
    consume  bool              // true = in (destructive), false = read
}
```

When `out:` is called, scan waiters first — if a waiter's template matches the new tuple, deliver directly (like an unbuffered channel). Otherwise append to tuples slice.

When `in:` or `read:` is called, scan tuples for a match. If found, return (and remove for `in:`). If not found, park the goroutine by creating a waiter and blocking on its channel.

### Primitives

Register in `vm/tuplespace_primitives.go`:

| Selector | Blocking | Consumes | Description |
|---|---|---|---|
| `out:` | No | — | Publish a tuple |
| `in:` | Yes | Yes | Take first matching tuple (blocks until available) |
| `read:` | Yes | No | Copy first matching tuple (blocks, non-destructive) |
| `tryIn:` | No | Yes | Take if available, nil otherwise |
| `tryRead:` | No | No | Peek if available, nil otherwise |
| `size` | No | — | Number of stored tuples |
| `isEmpty` | No | — | True if no tuples stored |

Templates are CueValue objects. Matching uses the existing `matchesObject:` mechanism — project the stored tuple via `objectAsCueValue`, unify with the template, succeed if no bottom.

### Maggie Wrapper

`lib/TupleSpace.mag`:

```smalltalk
TupleSpace subclass: Object

  method: out: aTuple [ ^self primOut: aTuple ]
  method: in: aTemplate [ ^self primIn: aTemplate ]
  method: read: aTemplate [ ^self primRead: aTemplate ]
  method: tryIn: aTemplate [ ^self primTryIn: aTemplate ]
  method: tryRead: aTemplate [ ^self primTryRead: aTemplate ]
  method: size [ ^self primSize ]
  method: isEmpty [ ^self primIsEmpty ]
```

### Blocking Mechanism

Reuse the same goroutine parking pattern as Channel:
- `in:` / `read:` create a `chan Value`, register a waiter, then `<-ch` blocks the goroutine
- `out:` checks waiters, sends on the first matching waiter's channel
- This works with `Process.fork` because forked blocks run in their own goroutine

### Tests

Go unit tests in `vm/tuplespace_test.go`:
- out then in: value retrieved and removed
- out then read: value retrieved but NOT removed
- tryIn on empty: returns nil
- tryIn on matching: returns value and removes
- tryRead on matching: returns value, keeps it
- Multiple tuples, template selects correct one
- Blocking in: goroutine blocks, out from another goroutine wakes it
- Blocking read: same, non-destructive
- Multiple waiters: first matching waiter gets the tuple
- Template with range constraints: `>0 & <100` matches 50, not 200
- Struct template matches object with ivars

Doctests and Guide15 update.

### Files

- `vm/tuplespace_primitives.go` — new file, ~250 lines
- `vm/tuplespace_test.go` — new file, ~300 lines
- `lib/TupleSpace.mag` — new file
- `lib/guide/Guide15CueIntegration.mag` — add tuplespace section (or create Guide16)
- `vm/vm.go` — register in bootstrap
- `vm/markers.go` — allocate NaN-boxing marker for TupleSpace
- `vm/object_registry.go` — add TupleSpace registry methods

---

## Phase 3: Linear Logic Extensions

**Goal:** Enrich tuplespace with resource semantics from linear logic.

### 3a: Tuple Modes

Add a `TupleMode` to control consumption semantics:

```go
type TupleMode int
const (
    TupleModeLinear     TupleMode = iota // consumed exactly once (default)
    TupleModeAffine                       // consumed at most once (can expire)
    TupleModePersistent                   // never consumed (!A exponential)
)
```

New primitives:
- `outPersistent:` — tuple stays in space after read AND after in (copies returned)
- `outAffine:ttl:` — tuple auto-removed after N milliseconds if not consumed
- `in:` on a persistent tuple returns a copy, original stays
- Persistent tuples model shared facts / blackboard knowledge
- Affine tuples model ephemeral resources / offers with expiry

### 3b: Atomic Multi-Take (Tensor Product ⊗)

```smalltalk
"Take task AND capability atomically, or block"
ts inAll: { taskTemplate. capabilityTemplate }
```

Implementation: hold the tuplespace lock, find ALL matching tuples, remove all atomically, or register a compound waiter that only fires when all templates are satisfiable simultaneously.

This prevents the classic coordination bug: agent takes a task but the required API key was grabbed by another agent between the two `in:` calls.

### 3c: Choice (Additive Disjunction ⊕)

```smalltalk
"Take whichever matches first"
ts inAny: { summaryTemplate. translationTemplate }
```

Like `Channel select:` but for tuple templates. Returns the first matching tuple. If none match, blocks until any one does.

### 3d: Lease / Expiry

- `outAffine:ttl:` stores a tuple with a deadline
- Background goroutine (or lazy check on access) removes expired tuples
- Expired tuples are dead-lettered (optionally: callback or dead-letter tuplespace)
- CancellationContext integration: `out:context:` — tuple removed when context cancelled

### Files

- `vm/tuplespace_primitives.go` — extend with modes, multi-take, choice
- `vm/tuplespace_test.go` — extend
- `lib/TupleSpace.mag` — add new methods
- Guide update

---

## Phase 4: Constraint Store (CCP)

**Goal:** Monotonic shared constraint store backed by CUE, with ask/tell/suspend semantics from concurrent constraint programming.

### Core Semantics

The constraint store holds a single CUE value that starts as top (⊤, unconstrained) and monotonically narrows via unification. Processes `tell` constraints to add information and `ask` constraints to check entailment, blocking until the store implies their query.

```go
type ConstraintStoreObject struct {
    mu       sync.Mutex
    ctx      *cue.Context
    store    cue.Value          // current accumulated constraints
    watchers []*constraintWatcher
}

type constraintWatcher struct {
    query cue.Value    // the constraint being asked
    ch    chan struct{} // wake channel
}
```

### Primitives

| Selector | Blocking | Description |
|---|---|---|
| `tell:` | No | Unify constraint into store. Returns Failure if bottom. |
| `ask:` | Yes | Block until store subsumes the query. |
| `tryAsk:` | No | Non-blocking entailment check (true/false). |
| `watch:do:` | No | Register callback for when constraint becomes entailed. |
| `value` | No | Return current store as CueValue (snapshot). |
| `isConsistent` | No | True if store is not bottom. |

### Tell Semantics

```go
func (cs *ConstraintStoreObject) tell(constraint cue.Value) error {
    cs.mu.Lock()
    defer cs.mu.Unlock()

    unified := cs.store.Unify(constraint)
    if unified.Err() != nil {
        return unified.Err() // constraint violation — store unchanged
    }
    cs.store = unified

    // Wake any watchers whose queries are now entailed
    cs.checkWatchers()
    return nil
}
```

### Ask Semantics

```go
func (cs *ConstraintStoreObject) ask(query cue.Value) {
    cs.mu.Lock()
    if cs.store.Subsume(query) == nil {
        cs.mu.Unlock()
        return // already entailed
    }
    // Not yet entailed — park goroutine
    w := &constraintWatcher{query: query, ch: make(chan struct{})}
    cs.watchers = append(cs.watchers, w)
    cs.mu.Unlock()
    <-w.ch // block until woken
}
```

After each `tell`, iterate watchers and wake any whose query is now subsumed by the store.

### Maggie Wrapper

`lib/ConstraintStore.mag`:

```smalltalk
ConstraintStore subclass: Object

  method: tell: aConstraintString [
      | ctx constraint |
      ctx := CueContext new.
      constraint := (ctx compileString: aConstraintString) value.
      ^self primTell: constraint
  ]

  method: ask: aConstraintString [
      | ctx constraint |
      ctx := CueContext new.
      constraint := (ctx compileString: aConstraintString) value.
      ^self primAsk: constraint
  ]
```

### Interaction with TupleSpace

The constraint store and tuplespace are complementary:
- **TupleSpace:** transient resources, consumed on read. Coordination via presence/absence.
- **ConstraintStore:** monotonic knowledge, never consumed. Coordination via entailment.

An agent orchestration system uses BOTH:
- Tasks and results flow through the tuplespace (linear resources)
- Shared knowledge accumulates in the constraint store (monotonic facts)
- An agent `ask`s the constraint store to check prerequisites before `in`-ing a task from the tuplespace

### Tests

- Tell then ask: tell `x: 42`, ask `x: int` → immediately entailed
- Ask then tell: ask `x: int` blocks, tell `x: 42` wakes it
- Multiple tells accumulate: tell `x: 42`, tell `y: "hello"`, ask `{x: int, y: string}` → entailed
- Conflicting tell: tell `x: 42`, tell `x: "hello"` → Failure (bottom)
- Store remains consistent after failed tell
- Multiple watchers: different queries, each wakes independently
- tryAsk: non-blocking check

### Files

- `vm/constraint_store.go` — new file, ~250 lines
- `vm/constraint_store_test.go` — new file, ~300 lines
- `lib/ConstraintStore.mag` — new file
- `vm/vm.go` — register in bootstrap
- `vm/markers.go` — allocate marker
- `vm/object_registry.go` — registry methods
- Guide update

---

## Phase 5: Agent Orchestration Patterns (APPLICATION LAYER)

**Not implemented in Go.** This is pure Maggie code built on phases 1-4.

Outlined here so the VM primitives are designed with these patterns in mind.

### Agent Class

```smalltalk
Agent subclass: Object
  instanceVars: process tupleSpace constraintStore capabilities name

  classMethod: spawn:on:with: name ts caps [
      | agent |
      agent := self new.
      agent init: name on: ts with: caps.
      ^agent
  ]

  method: init: aName on: aTS with: caps [
      name := aName.
      tupleSpace := aTS.
      capabilities := caps.
  ]

  method: run: aBlock [
      process := [aBlock value: self] fork.
  ]

  method: takeTask: template [
      "Atomically take a task matching template"
      ^tupleSpace in: template
  ]

  method: publishResult: result [
      tupleSpace out: result
  ]
```

### Standard Tuple Schemas (CUE)

```cue
#Task: {
    type:    "task"
    kind:    string
    id:      string
    payload: _
    status:  "pending" | "claimed"
}

#Result: {
    type:    "result"
    taskId:  string
    output:  _
    status:  "done" | "error"
}

#Capability: {
    type:  "capability"
    agent: string
    can:   [...string]
}
```

### Coordination Patterns

All built on tuplespace + constraint store:

- **Fan-out:** One agent `out:`s N task tuples, N workers `in:` one each
- **Fan-in:** Aggregator `read:`s N result tuples (non-destructive), combines
- **Pipeline:** Agent A's result tuple matches Agent B's task template
- **Auction:** Agents `out:` bid tuples, coordinator `inAll:` bids + task atomically
- **Blackboard:** `outPersistent:` for shared knowledge, `read:` to query
- **Barrier:** Constraint store — each agent `tell:`s "done", coordinator `ask:`s "all done"
- **Consensus:** Each agent `tell:`s a vote, `ask:` blocks until votes converge

### Observation / Tracing

- TupleSpace lifecycle hooks: `onOut:do:`, `onIn:do:`, `onExpire:do:`
- Constraint store hooks: `onTell:do:`, `onEntail:do:`
- These are registered callbacks, not blocking operations

---

## Phase 6: Distribution (FUTURE)

**Not in scope for initial implementation.** Documented for architectural awareness.

### Distributed TupleSpace

- Tuples partitioned across nodes (hash of CUE projection, or replicated)
- `in:` / `out:` route to the correct node via the node protocol
- Network partition semantics: tuples on unreachable nodes are unavailable
- Consistency model: eventual for persistent tuples, strong for linear

### Distributed Constraint Store

- Store replicated across nodes with CRDT-like merge (CUE unification is commutative, associative, idempotent — it IS a join-semilattice merge)
- `tell:` broadcasts to all replicas
- `ask:` checks local replica (may be stale — eventually consistent)
- Strong `ask:` routes to a coordinator node (consistent but slower)

### Agent Migration

- Serialize agent state (process + capability + in-flight tuples)
- Resume on another node
- Depends on value serialization from node protocol

---

## Implementation Order

```
Phase 1: CUE Subsumption     (~1 hour)
    │
    ▼
Phase 2: Local TupleSpace     (~1 day)
    │
    ├──→ Phase 3: Linear Extensions  (~2 days)
    │
    └──→ Phase 4: Constraint Store   (~1 day)
              │
              ▼
         Phase 5: Agent Patterns (application code, ongoing)
              │
              ▼
         Phase 6: Distribution (future, depends on node protocol)
```

Phases 3 and 4 are independent of each other and can proceed in parallel.
Phase 5 can begin as soon as Phase 2 is done (basic tuplespace is sufficient
for simple agent patterns; linear extensions and constraint store add
sophistication).
