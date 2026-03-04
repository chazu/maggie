# VM Internals Research: Performance and Correctness Improvements

This document covers five areas of the Maggie VM where targeted changes can
improve throughput, reduce allocation pressure, or eliminate data races.
Each section describes the current implementation, analyses the problem, and
proposes a concrete fix with risk assessment.

---

## TODO 4: Globals Map Synchronization

### Current Implementation

`VM.Globals` is a plain `map[string]Value` (`vm/vm.go:22`).  It is created in
`NewVM()` at `vm/vm.go:126` and shared by reference with every interpreter:

```go
// vm/vm.go:157
interp.Globals = vm.Globals

// vm/vm.go:173  (forked interpreters get the same pointer)
interp.Globals = vm.Globals
```

**Writes to Globals happen in two contexts:**

1. **Bootstrap/compilation time** -- `vm.go:291-322` populates `Globals` with
   class values.  The compiler's `OpStoreGlobal` handler (`interpreter.go:671-681`)
   writes directly when `interp.forked == false`.  The `SetGlobal` method on
   `Interpreter` (`interpreter.go:198-210`) also writes to the shared map for
   non-forked interpreters.

2. **Runtime** -- User code can write globals via `Compiler evaluate:` (which
   calls `OpStoreGlobal` on the main interpreter) and via `Compiler setGlobal:to:`
   (`compiler_primitives.go`).  Both reach the un-synchronized shared
   `map[string]Value`.

**Reads from Globals** happen on every `OpPushGlobal` instruction
(`interpreter.go:622-654`), which runs in the main interpreter and in every
forked goroutine.  Forked interpreters read `i.Globals[globalName]` at line 639
and 644.

**The data race:** The main interpreter can write to `Globals` (e.g. via REPL
evaluation) while a forked interpreter reads from the same map on a different
goroutine.  Go maps are not safe for concurrent read-write access.  This is a
**real, existing data race** that `go test -race` would flag if triggered.

Forked interpreters mitigate this partially -- their writes go to `localWrites`
(`interpreter.go:671-676`) -- but their reads still hit the shared `Globals` map
without synchronization.

### Analysis

| Access Pattern | Frequency | Goroutine |
|---|---|---|
| Read (`OpPushGlobal`) | Every global variable reference (extremely hot) | Main + forked |
| Write (`OpStoreGlobal`, non-forked) | REPL assignments, class definitions | Main only (usually) |
| Write (`Compiler setGlobal:to:`) | Explicit API call | Main (via `currentInterpreter()`) |

Key observations:

- Reads vastly outnumber writes during normal execution.
- Writes from forked interpreters already go to `localWrites`, so concurrent
  writers are rare.
- The main risk is main-interpreter writes concurrent with forked-interpreter
  reads.
- Globals is typically populated at load time and then treated as mostly-read.

### Recommended Fix

**Use `sync.RWMutex`** wrapping the `Globals` map.  `sync.Map` is tempting but
its API is awkward for the iteration in `CollectGarbage` (`vm.go:860`) and its
performance advantage only appears when keys are stable -- which is already
true here, making RWMutex's simpler API the better choice.

**Step-by-step:**

1. Add a `globalsMu sync.RWMutex` field to `VM`.
2. Wrap `VM.LookupGlobal()` and `VM.SetGlobal()` with `RLock`/`Lock`.
3. Change `Interpreter.LookupGlobal()` to call `i.vm.globalsMu.RLock()` before
   reading `i.Globals[name]` (only when `i.vm != nil`).  For standalone test
   interpreters without a VM, the single-goroutine invariant holds.
4. Change `Interpreter.SetGlobal()` (the `i.Globals[name] = val` path at line
   208) to call `i.vm.globalsMu.Lock()`.
5. In the `OpPushGlobal` handler, use `i.vm.globalsMu.RLock()` / `RUnlock()`
   around the `i.Globals[globalName]` reads at lines 639 and 644.
6. In the `OpStoreGlobal` handler, use `i.vm.globalsMu.Lock()` around line 679.
7. Wrap `CollectGarbage`'s globals iteration (`vm.go:860`) with `RLock`.

**Performance concern:** RLock/RUnlock on an uncontended `sync.RWMutex` costs
roughly 20-40ns.  `OpPushGlobal` is not in the innermost hot loop (most
variables are temps or ivars), so this overhead is acceptable.  If profiling
later shows it as a bottleneck, we can move to a copy-on-write snapshot
approach: forked interpreters get a snapshot reference at fork time, and writes
replace the snapshot atomically.

### Risk/Complexity

**Low.**  Mechanical wrapping of map accesses.  The main risk is missing a
call site that accesses `Globals` directly (bypassing the helpers).  A `grep`
for `\.Globals\[` across the codebase will catch these.

---

## TODO 5: Eliminate defer/recover from send() Hot Path

### Current Implementation

Non-local returns (NLR) in Maggie use Go's `panic`/`recover` mechanism.  The
`NonLocalReturn` struct is defined at `interpreter.go:1604`:

```go
type NonLocalReturn struct {
    Value     Value
    HomeFrame int // target frame to return to
}
```

When a block executes `^value` (`OpReturnTop`, `interpreter.go:993-1006`), it
calls `panic(NonLocalReturn{...})`.  Three places install `defer`/`recover` to
catch this:

1. **`Execute()`** -- `interpreter.go:458-479`.  This is the top-level entry
   point.  Called once per VM.Execute invocation.  Not hot.

2. **`send()`** -- `interpreter.go:1162-1183`.  Called on **every compiled
   method invocation** via `OpSend`.  This is extremely hot.  The `defer func()`
   allocates a closure on every call, and `recover()` has runtime overhead even
   when no panic occurs.

3. **`sendSuper()`** -- `interpreter.go:1265-1282`.  Same pattern.  Less
   frequent than `send()` but still on the dispatch path.

The cost is significant: Go's `defer` has been optimized in recent versions
(open-coded defer since Go 1.14), but the closure allocation and the
`recover()` check still show up in profiles.  `BenchmarkHotPath` likely
attributes measurable time here.

### Analysis

The fundamental problem is that `panic`/`recover` is a blunt instrument for
what is really a structured control flow operation.  NLR has well-defined
semantics: when `^value` executes inside a block, control should return to the
method frame that created the block (the "home frame").

**How Cog/Spur (Squeak/Pharo VM) handles NLR:**  In Cog, each block closure
stores a pointer to its "home context" (the MethodContext that created it).
When `^` is executed, the VM walks up the context chain, popping frames until
it reaches the home context, then returns the value from that context.  There
is no exception mechanism involved.  The VM simply performs an explicit
multi-frame unwind in a loop.

We can replicate this approach.

### Recommended Fix

**Replace panic/recover with an explicit unwinding flag on the interpreter.**

**New fields on `Interpreter`:**

```go
type Interpreter struct {
    // ... existing fields ...

    // NLR unwinding state
    unwinding    bool
    unwindValue  Value
    unwindTarget int // home frame index to return to
}
```

**Changes to `OpReturnTop` (block case, `interpreter.go:993-1006`):**

Instead of `panic(NonLocalReturn{...})`, set the unwinding fields and return:

```go
case OpReturnTop:
    result := i.pop()
    if isBlock {
        if frame.HomeFrame == -1 {
            // Detached block: local return
            i.popFrame()
            return result
        }
        // Non-local return: set unwinding state and exit runFrame
        i.popFrame()
        i.unwinding = true
        i.unwindValue = result
        i.unwindTarget = frame.HomeFrame
        return Nil // sentinel; caller checks i.unwinding
    }
    i.popFrame()
    return result
```

**Changes to `send()` (`interpreter.go:1110-1191`):**

Remove the `defer`/`recover` block entirely.  After `runFrame()` returns,
check the unwinding flag:

```go
func (i *Interpreter) send(selector int, argc int) Value {
    args := i.popN(argc)
    rcvr := i.pop()
    // ... vtable lookup, cache check ...

    if cm, ok := method.(*CompiledMethod); ok {
        i.pushFrame(cm, rcvr, args)
        homeFrame := i.fp

        result := i.runFrame()

        // Check for NLR unwinding
        if i.unwinding {
            if i.unwindTarget == homeFrame {
                // This is our target frame -- unwind complete
                for i.fp > homeFrame {
                    i.popFrame()
                }
                if i.fp == homeFrame {
                    i.popFrame()
                }
                i.unwinding = false
                return i.unwindValue
            }
            // Not our frame -- propagate (caller will also check)
            return Nil // sentinel
        }

        return result
    }

    return method.Invoke(i.vm, rcvr, args)
}
```

**Changes to `sendSuper()`:** Same pattern as `send()`.

**Changes to `Execute()`:** Same pattern.  The `defer`/`recover` in `Execute`
can be replaced similarly.  `Execute` is the outermost catch point, so if
`i.unwinding` is still true when `runFrame()` returns, it means the home
frame has already been popped (block escaped its home method).  In that case,
we can clear the flag and return the unwind value (or signal an error).

**Changes to `runFrame()`:** After `OpSend` calls `i.send()` and pushes the
result, check `i.unwinding`:

```go
case OpSend:
    sel := int(binary.LittleEndian.Uint16(bc[frame.IP:]))
    frame.IP += 2
    argc := int(bc[frame.IP])
    frame.IP++
    result := i.send(sel, argc)
    if i.unwinding {
        i.popFrame()
        return Nil // propagate up
    }
    i.push(result)
```

This check adds a single boolean test per send (branch-predicted false), which
is negligible compared to the cost of `defer`/`recover`.

**What about `SignaledException`?**  The exception handling mechanism also uses
`panic`/`recover` (for `SignaledException` and `StackOverflow`).  These are
**not** on the hot path -- they are exceptional conditions.  We should keep
`defer`/`recover` in `Execute()` specifically for catching `SignaledException`,
but remove it from `send()` and `sendSuper()`.  Signalled exceptions can
propagate up to `Execute()` naturally via Go panic.

**What about `sendBinaryFallback()`?**  This function at `interpreter.go:1678`
calls `pushFrame` + `runFrame` without any NLR protection.  With the new
unwinding mechanism, it needs the same `i.unwinding` check after `runFrame()`.

### Risk/Complexity

**Medium.**  The logic is straightforward but touches the interpreter's most
critical code path.  Every `OpSend` site in `runFrame()` needs the unwinding
check.  The main risks are:

- Missing an NLR catch site (e.g. `sendBinaryFallback`, block evaluation in
  primitives).
- Interactions with `ExecuteBlockDetached` (HomeFrame == -1) need to be
  verified.
- Exception handling (`SignaledException`) must remain unaffected.

Thorough testing with the existing NLR test suite (`vm/interpreter_test.go`,
block tests) and the stack overflow tests is essential.  The TCO tests
(`vm/tco_test.go`) should also be re-run since `OpTailSend` uses `i.send()`.

---

## TODO 6: Eliminate popN() Slice Allocation

### Current Implementation

`popN()` is defined at `interpreter.go:279`:

```go
func (i *Interpreter) popN(n int) []Value {
    if i.sp < n {
        panic("stack underflow")
    }
    result := make([]Value, n)
    i.sp -= n
    copy(result, i.stack[i.sp:i.sp+n])
    return result
}
```

Every call allocates a new `[]Value` slice on the heap.

**All call sites:**

| Call Site | File:Line | Retains Slice? |
|---|---|---|
| `OpTailSend` | `interpreter.go:790` | Yes -- `tailArgs` is used after vtable lookup and potentially re-pushed |
| `OpCreateBlock` captures | `interpreter.go:1055` | Yes -- stored in `BlockValue.Captures` |
| `OpCreateArray` elements | `interpreter.go:1083` | Yes -- passed to `NewArrayWithElements()` which stores it |
| `send()` args | `interpreter.go:1111` | Passed to `pushFrame()` which iterates and pushes to stack; also passed to `method.Invoke()` |
| `sendSuper()` args | `interpreter.go:1232` | Same as `send()` |

### Analysis

The key question: does any caller store the returned slice beyond the immediate
method call?

- **`OpCreateBlock`**: The captures slice is stored in `BlockValue.Captures`
  and lives as long as the block.  **Must allocate.**
- **`OpCreateArray`**: The elements slice is stored in the array object.
  **Must allocate.**
- **`send()` / `sendSuper()`**: The `args` slice is consumed immediately --
  either pushed onto the stack by `pushFrame()` or passed to `Invoke()`.  After
  `send()` returns, the slice is dead.  **Does not need allocation.**
- **`OpTailSend`**: `tailArgs` is used across a vtable lookup and then either
  used to reset the frame or re-pushed.  It is dead after the `OpTailSend`
  handler completes.  **Does not need allocation** but needs the values to
  survive the vtable lookup.

### Recommended Fix

**Two-pronged approach:**

**A) Sub-slice for send/sendSuper (zero-alloc):**

For `send()` and `sendSuper()`, instead of allocating, return a sub-slice of
the interpreter's stack.  The values are already contiguous on the stack:

```go
func (i *Interpreter) peekN(n int) []Value {
    if i.sp < n {
        panic("stack underflow")
    }
    return i.stack[i.sp-n : i.sp]
}

func (i *Interpreter) dropN(n int) {
    i.sp -= n
}
```

Then in `send()`:

```go
func (i *Interpreter) send(selector int, argc int) Value {
    args := i.peekN(argc)
    i.sp -= argc
    rcvr := i.pop()
    // ... use args ...
}
```

**Warning:** This is safe only if `pushFrame()` copies the args into temp slots
before any subsequent pushes could overwrite them.  Looking at `pushFrame()`
(`interpreter.go:345-378`), it sets `bp = i.sp`, then pushes args one by one.
Since `bp` is set after we've already popped, the sub-slice region is below
`bp` and could be overwritten.  Actually, `pushFrame` does `for _, arg := range args { i.push(arg) }` which copies from the sub-slice into new stack
positions.  As long as the source and destination regions don't overlap, this
is fine.

Let's verify: after `send()` pops argc+1 values, `i.sp` points below the args.
Then `pushFrame` sets `bp = i.sp` and pushes args starting at `bp`.  The args
sub-slice is at `[old_sp - argc, old_sp)`.  The new pushes go to
`[i.sp, i.sp + argc)` which equals `[old_sp - argc - 1, old_sp - 1)`.  These
overlap by argc-1 positions!  **This would corrupt the args.**

So the sub-slice approach needs care.  We need to pop the receiver first, then
sub-slice the args which are now at `[i.sp - argc, i.sp)`, then in pushFrame
copy them to the new position.  But the receiver is below the args on the stack.
The stack layout is: `[... rcvr arg0 arg1 ...]`.

Alternative: **Pre-allocated buffer on the Interpreter:**

```go
type Interpreter struct {
    // ...
    argBuf [16]Value // covers up to 16-arg sends without allocation
}

func (i *Interpreter) popNFast(n int) []Value {
    if i.sp < n {
        panic("stack underflow")
    }
    i.sp -= n
    if n <= len(i.argBuf) {
        copy(i.argBuf[:n], i.stack[i.sp:i.sp+n])
        return i.argBuf[:n]
    }
    // Fallback for rare >16-arg methods
    result := make([]Value, n)
    copy(result, i.stack[i.sp:i.sp+n])
    return result
}
```

This eliminates the heap allocation for all practical cases (Smalltalk methods
rarely exceed 4-5 arguments).  The buffer is reused across calls since the
returned slice is consumed before the next `popNFast` call.

**B) Keep allocating for OpCreateBlock and OpCreateArray:**

These callers store the result long-term, so allocation is unavoidable.  Keep
using the current `popN()` for them.  Rename the current `popN` to
`popNAlloc()` and use `popNFast()` for send/sendSuper/OpTailSend.

### Risk/Complexity

**Low** for the pre-allocated buffer approach.  The buffer is stack-local to
the interpreter (one per goroutine), so no concurrency issues.  The only risk
is a caller that stores the returned slice -- but we've verified that
`send()`, `sendSuper()`, and `OpTailSend` do not.

---

## TODO 7: Flatten VTables

### Current Implementation

VTables use a parent-chain linked list for inheritance (`vm/object.go:44-48`,
`vm/vtable.go:14-23`):

```go
type VTable struct {
    class   *Class
    parent  *VTable   // Parent vtable for inheritance lookup
    methods []Method  // Methods indexed by selector ID
}

func (vt *VTable) Lookup(selector int) Method {
    for v := vt; v != nil; v = v.parent {
        if selector >= 0 && selector < len(v.methods) {
            if m := v.methods[selector]; m != nil {
                return m
            }
        }
    }
    return nil
}
```

This means every method lookup walks the parent chain until a non-nil entry is
found.  For a class at depth N in the hierarchy, the worst case is N+1 map
lookups (one per class in the chain).

**Class hierarchy statistics:**

- Bootstrap creates ~36 core classes (counted from `vm.go`).
- The lib/ directory defines ~144 more classes via `subclass:` declarations.
- Total: ~180 classes in a typical loaded image.
- Maximum depth: most classes are depth 1 (direct subclass of Object).  The
  deepest chains are likely in the Yutani widget hierarchy (e.g.
  `YutaniButton > YutaniWidget > Object` = depth 2) and the exception
  hierarchy (`ZeroDivide > Error > Exception > Object` = depth 3).  The
  compiler hierarchy (`AssignmentNode > ... > Object`) is depth 1.
- Typical depth: 1-3.  Maximum observed: ~4.

**Method counts:**

- Selector table pre-allocates 256 slots.  A loaded image likely has 300-500
  unique selectors.
- VTable methods arrays are sparse -- sized to the maximum selector ID used by
  that class, with nil entries for unused slots.
- Object class defines the most primitives (~40+ methods).

**When VTables are modified after creation:**

- `IncludeTrait()` (`vm/trait.go:126-145`) calls `c.VTable.AddMethod()` for
  each trait method.  This happens during compilation.
- `AddMethod*` / `AddPrimitiveMethod` / `AddClassMethod*` in `vm/class.go`
  are called during bootstrap and class compilation.
- Image loading (`image_reader.go`) reads methods and adds them to VTables.
- **At runtime:** Methods are not added to VTables during normal execution.
  VTables are effectively immutable after loading completes.

### Analysis

With a typical depth of 1-3, the parent chain walk is short.  The inline cache
in `send()` already mitigates the cost for monomorphic call sites (the common
case).  However, for cache misses (megamorphic sites, first calls, after GC
invalidation), the chain walk is redundant work.

**Memory impact of flattening:**

If we copy all parent methods into each child VTable, every class needs a
methods array sized to the maximum selector ID.  With ~500 selectors and ~180
classes:

- Current (sparse): each class stores only methods up to its own max selector
  ID, plus the chain of parent pointers.  Very memory-efficient.
- Flattened: each class stores ~500 method slots = 500 * 8 bytes = 4KB per
  class.  180 classes * 4KB = 720KB.  This is negligible.

### Recommended Fix

**Eager flatten with a dirty bit for lazy rebuild.**

**Step 1: Add a `flat []Method` field to `VTable`:**

```go
type VTable struct {
    class   *Class
    parent  *VTable
    methods []Method   // local methods only (for AddMethod)
    flat    []Method   // flattened: all inherited + local methods
    dirty   bool       // true if flat needs rebuild
}
```

**Step 2: Build flat table on first lookup after modification:**

```go
func (vt *VTable) Lookup(selector int) Method {
    if vt.flat == nil || vt.dirty {
        vt.rebuild()
    }
    if selector >= 0 && selector < len(vt.flat) {
        return vt.flat[selector]
    }
    return nil
}

func (vt *VTable) rebuild() {
    // Determine max selector ID across the chain
    maxID := 0
    for v := vt; v != nil; v = v.parent {
        if len(v.methods) > maxID {
            maxID = len(v.methods)
        }
    }

    vt.flat = make([]Method, maxID)

    // Walk from root to leaf so child methods override parent methods
    var chain []*VTable
    for v := vt; v != nil; v = v.parent {
        chain = append(chain, v)
    }
    for i := len(chain) - 1; i >= 0; i-- {
        for sel, m := range chain[i].methods {
            if m != nil {
                vt.flat[sel] = m
            }
        }
    }

    vt.dirty = false
}
```

**Step 3: Mark dirty on AddMethod:**

```go
func (vt *VTable) AddMethod(selector int, method Method) {
    // ... existing grow logic ...
    vt.methods[selector] = method
    vt.markDirty()
}

func (vt *VTable) markDirty() {
    vt.dirty = true
    vt.flat = nil // force rebuild
    // Note: child VTables are NOT automatically dirtied.
    // This is acceptable because AddMethod is only called during
    // compilation/loading, and all VTables will be rebuilt on first
    // lookup after loading completes.
}
```

**Trait inclusion:** `IncludeTrait` calls `AddMethod` which marks dirty.
After all traits are included, the next `Lookup` will rebuild.

**Why lazy rebuild instead of eager:**  During bootstrap, hundreds of
`AddMethod` calls happen in sequence.  Rebuilding after each one would be
wasteful.  The lazy dirty-bit approach means the flat table is built exactly
once, on the first `Lookup` after all methods have been registered.

**Inline cache interaction:** The inline cache already stores the resolved
`Method` directly, bypassing `Lookup()`.  Flattening the VTable speeds up
cache misses, which is exactly the case that needs help.

### Risk/Complexity

**Low-Medium.**  The main risk is forgetting to mark dirty when a VTable is
modified through an unusual path.  A search for all calls to `AddMethod` and
`SetParent` will identify these.  The lazy rebuild ensures correctness even if
dirtying is delayed -- the worst case is one extra rebuild.

The memory overhead (720KB) is negligible for a VM that already loads ~180
classes worth of method bytecode and string data.

---

## TODO 8: String Registry Contention

### Current Implementation

Strings are stored in the `ObjectRegistry` string registry
(`vm/object_registry.go:42-43`):

```go
// String registry
strings   map[uint32]*StringObject
stringsMu sync.RWMutex
stringID  atomic.Uint32
```

Key operations:

**String creation** (`NewStringValue`, `object_registry.go:445-449`):
```go
func (or *ObjectRegistry) NewStringValue(s string) Value {
    obj := &StringObject{Content: s}
    id := or.RegisterString(obj)    // acquires stringsMu.Lock()
    return FromSymbolID(id)
}
```
Every string creation acquires a write lock.

**String read** (`GetStringContent`, `object_registry.go:453-467`):
```go
func (or *ObjectRegistry) GetStringContent(v Value) string {
    // ...
    obj := or.GetString(id)    // acquires stringsMu.RLock()
    if obj != nil {
        return obj.Content
    }
    return ""
}
```
Every string content access acquires a read lock.

**Frequency of string operations:**

Grep counts across the VM package show:
- `GetStringContent`: ~835 call sites across 57 files
- `NewStringValue`: included in the same count

The most critical hot path is `primitiveEQ` (`interpreter.go:1835-1839`):

```go
if IsStringValue(a) && IsStringValue(b) {
    if i.vm.registry.GetStringContent(a) == i.vm.registry.GetStringContent(b) {
        return True
    }
}
```

This acquires `stringsMu.RLock()` **twice** per string comparison -- once for
each operand.  String comparisons in loops (e.g. dictionary lookups, string
matching) will contend on this lock.

String creation is also frequent: every string literal evaluation, string
concatenation, `printString`, format operations, etc. all call
`NewStringValue` which acquires the write lock.

### Analysis

The string registry has the highest contention potential of any registry
because:

1. **Read frequency is extreme** -- every string operation (comparison, size,
   at:, substring, etc.) must look up the string content via `GetStringContent`.
2. **Write frequency is moderate** -- string concatenation and other operations
   create new strings constantly.
3. **The write lock in `RegisterString` blocks all readers**, creating a
   bottleneck when forked processes create strings concurrently.

The `sync.RWMutex` is a reasonable default but becomes a bottleneck under
concurrent string-heavy workloads because writer starvation prevention in
Go's RWMutex means a pending writer blocks new readers.

**String interning** is not applicable here -- Maggie strings are mutable
identity-bearing objects (two string literals with the same content get
different registry IDs).

### Recommended Fix

**Sharded map with N=16 shards, keyed on the lower bits of the string ID.**

```go
const stringShardBits = 4
const stringShardCount = 1 << stringShardBits // 16

type StringShard struct {
    mu      sync.RWMutex
    strings map[uint32]*StringObject
}

type ShardedStringRegistry struct {
    shards [stringShardCount]StringShard
    nextID atomic.Uint32
}

func (r *ShardedStringRegistry) shard(id uint32) *StringShard {
    return &r.shards[id&(stringShardCount-1)]
}

func (r *ShardedStringRegistry) Register(s *StringObject) uint32 {
    id := r.nextID.Add(1) - 1
    sh := r.shard(id)
    sh.mu.Lock()
    sh.strings[id] = s
    sh.mu.Unlock()
    return id
}

func (r *ShardedStringRegistry) Get(id uint32) *StringObject {
    sh := r.shard(id)
    sh.mu.RLock()
    s := sh.strings[id]
    sh.mu.RUnlock()
    return s
}
```

**Integration:** Replace the `strings`/`stringsMu`/`stringID` fields in
`ObjectRegistry` with a `*ShardedStringRegistry`.  The `RegisterString`,
`GetString`, `UnregisterString`, `StringCount`, `NewStringValue`, and
`GetStringContent` methods delegate to the sharded registry.

**Why sharding over `sync.Map`:**  `sync.Map` has higher per-operation overhead
due to its two-map design and is optimized for append-mostly workloads with
rare deletes.  String registries do not delete (no string GC currently), so
`sync.Map` could work, but the sharded approach gives more predictable
performance and simpler profiling.

**Additional optimization -- cache string content in the Value:**

A more aggressive optimization (for a future phase) would be to store short
strings (up to ~6 bytes) directly in the NaN-boxed Value, avoiding the
registry lookup entirely.  This is how some JavaScript engines handle short
strings.  However, this requires changes to the Value representation and is
a much larger undertaking.

**Immediate win -- reduce `primitiveEQ` lock acquisitions:**

Even without sharding, we can reduce the lock acquisitions in `primitiveEQ`
from 2 to 1 by holding the read lock across both lookups:

```go
func (i *Interpreter) primitiveEQ(a, b Value) Value {
    if a == b {
        return True
    }
    if a.IsFloat() && b.IsFloat() {
        if a.Float64() == b.Float64() { return True }
    }
    if IsStringValue(a) && IsStringValue(b) {
        reg := i.vm.registry
        reg.stringsMu.RLock()
        sa := reg.strings[a.SymbolID()]
        sb := reg.strings[b.SymbolID()]
        reg.stringsMu.RUnlock()
        if sa != nil && sb != nil && sa.Content == sb.Content {
            return True
        }
    }
    return False
}
```

This halves the lock overhead for string comparisons and can be done
immediately, independent of the sharding change.

### Risk/Complexity

**Low** for the `primitiveEQ` optimization (single function change).

**Medium** for the sharded registry (new type, delegation methods, need to
update `StringCount` to sum across shards).  The main risk is correctness of
the shard selection -- using lower bits of a monotonically increasing ID
gives excellent distribution.

---

## Summary

| TODO | Fix | Expected Impact | Risk | Priority |
|---|---|---|---|---|
| 4: Globals sync | `sync.RWMutex` wrapper | Eliminates data race | Low | High (correctness) |
| 5: defer/recover | Explicit unwinding flag | ~20-40ns per send() saved | Medium | High (hot path) |
| 6: popN alloc | Pre-allocated arg buffer | 1 heap alloc per send() eliminated | Low | Medium |
| 7: Flatten VTables | Lazy flat table + dirty bit | Faster cache-miss lookups | Low-Medium | Low (cache handles common case) |
| 8: String contention | Sharded registry + primitiveEQ fix | Reduced lock contention | Low-Medium | Medium |

Recommended implementation order: 4 (correctness fix), then 6 (easiest win),
then 5 (biggest perf impact but most invasive), then 8, then 7.
