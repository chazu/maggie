# String & dictionary garbage collection

Maggie `Value`s are NaN-boxed `uint64`s. The Go runtime GC therefore cannot see
the heap objects a `Value` references — strings and dictionaries live in
`AutoIDRegistry`s (`ObjectRegistry.Strings`, `ObjectRegistry.Dictionaries`)
keyed by an integer id, and the `Value` carries only that id. Until this
collector landed nothing ever removed registry entries, so a program that
creates many *distinct* strings grew without bound. A long-running server that
re-renders HTML on every tick (procyon-park's `pp serve`) leaked ~1 GB/min and
was eventually OOM-killed.

The string/dictionary collector reclaims those entries. It is **opt-in** and
costs nothing when disabled.

## Enabling

```go
vm := vm.NewVM()
vm.EnableStringGC()        // programmatic
```

or set the environment variable before starting any Maggie binary:

```sh
MAGGIE_GC=1 ./your-app          # "0" / "off" / unset = disabled
```

When enabled, collection is driven automatically by `RegistryGC`: growth
pressure on the string registry triggers a stop-the-world cycle. You can also
force one synchronously from a non-mutator goroutine with
`vm.CollectStringGarbage()`.

## How it works

It is a precise mark-sweep collector.

**Mark** traces from the complete live-root set and follows every reference:

- each interpreter's operand stack, call frames (receiver / home-self /
  captures) and installed exception handlers — the main interpreter and every
  forked one;
- globals and class variables;
- compiled-method and block literal pools (string literals are roots via the
  loaded classes);
- processes (result value + buffered mailbox messages);
- registered blocks (captures + home self + literal pool);
- cells, reified contexts, results, exceptions.

The trace descends through objects (instance slots), dictionaries (keys +
values), array-lists (elements) and cells (contents).

**Sweep** frees the strings and dictionaries the mark did not reach. Objects and
array-lists are not swept here (the existing `CollectGarbage` owns the
`keepAlive` object set); an unreachable, un-swept container is itself
unreachable, so the strings it still references by id are never read after they
are freed — the same invariant that makes string sweeping safe.

## Stop-the-world

The collector reads mutable VM state without per-field locks, so it must run
with no other goroutine mutating that state. Maggie runs each process on its own
goroutine with no global interpreter lock, so collection is coordinated with
cooperative **safepoints**:

- Every interpreter polls a safepoint at loop back-edges (`OpJump` with a
  negative offset) and at frame entry. At those points all live `Value`s are in
  the operand stack / frames — none are stranded in Go locals — so the frozen
  state is a complete root set. Straight-line code is never polled, so the
  default-disabled path adds no per-instruction cost.
- Blocking primitives (`Process sleep:`, mailbox / channel receive, HTTP
  `start`) bracket their blocking call with `enterBlocked` / `exitBlocked`. A
  blocked interpreter counts as stopped and publishes the `Value`s it holds in
  Go locals (receiver/args) so they are still marked.
- `CollectStringGarbage` arms the request, waits for every interpreter to be
  parked or blocked, traces + sweeps, then releases everyone.

### Liveness over aggressiveness

The barrier wait is bounded by a 250 ms timeout. If a goroutine never reaches a
safepoint — e.g. it is stuck in a blocking primitive that has not been
instrumented with `enterBlocked` — the collector **aborts the cycle** rather
than hang the VM. A missed instrumentation only costs a skipped collection; it
can never deadlock the VM or free a still-live string.

## Precondition

The collector assumes **one interpreter per mutator goroutine** — Maggie's
normal execution model (forked processes via `[block] fork`, plus a serialized
dispatcher for host-driven calls). Driving Maggie execution by calling
`vm.Send` concurrently from multiple goroutines that share the main interpreter
is **not supported** while the collector is enabled.

## Adding instrumentation for new blocking primitives

If you add a primitive that blocks the goroutine (network read, lock, wait),
bracket the blocking call so the collector does not have to wait out the
timeout:

```go
v.enterBlocked(recv /*, args… */)
result := somethingThatBlocks()
v.exitBlocked()
```

Pass any `Value`s held in Go locals across the block as roots. Omitting this is
safe (the cycle just aborts more often); getting the roots wrong is not — pass
every live `Value` the call holds.
