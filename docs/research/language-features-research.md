# Language Features Research

Research document covering five proposed language feature additions for the Maggie VM.
Each section includes current implementation analysis, design recommendations, and risk assessment.

---

## TODO 13: BigInteger with Automatic SmallInt Promotion

### Current Implementation

**NaN-boxing scheme** (`vm/value.go:22-71`): Values are 64-bit IEEE 754 doubles. Non-float types use the quiet NaN space with 3 tag bits and a 48-bit payload. SmallInt uses `tagInt` (0x0002...) with a 48-bit signed integer payload, giving a range of -140,737,488,355,328 to 140,737,488,355,327.

**Arithmetic primitives** (`vm/interpreter.go:1695-1769`): `primitivePlus`, `primitiveMinus`, `primitiveTimes`, `primitiveDiv`, and `primitiveMod` handle SmallInt-SmallInt, Float-Float, and mixed operands. SmallInt arithmetic calls `FromSmallInt()` directly, which **panics** if the result overflows 48 bits (`vm/value.go:196-198`). There is no overflow detection today -- adding two large SmallInts silently wraps or crashes.

**Marker allocation** (`vm/markers.go`): Markers use bits 24-31 of a symbol ID. Currently allocated: 1, 2, 4, 7-9, 16, 32-51 (with gaps at 3, 5, 6, 10-15, 17-31). Free slots include 3, 5, 6, 10-15, 17-31, and 52+.

**ObjectRegistry** (`vm/object_registry.go`): Registry-based values follow a consistent pattern: a Go struct, a map from ID to struct pointer, an atomic ID counter, and Register/Get/Unregister methods. BigInt would follow this same pattern.

### Analysis

The current code has a critical bug: `primitivePlus` at line 1697 does `FromSmallInt(a.SmallInt() + b.SmallInt())` with no overflow check. If `a + b` exceeds `MaxSmallInt`, this panics. Even without BigInteger support, this needs to be fixed (e.g., by promoting to Float).

BigInteger requires:
1. A new marker for NaN-boxed BigInt values
2. A registry entry wrapping `math/big.Int`
3. Overflow detection in all arithmetic primitives
4. BigInt-specific arithmetic methods
5. Demotion back to SmallInt when results fit in 48 bits
6. Hashing support for Dictionary keys

The `TryFromSmallInt` function (`vm/value.go:203-208`) already exists and returns `(Nil, false)` on overflow -- this is the right hook for overflow detection.

### Recommended Implementation

**Step 1: Allocate BigInt marker and define the struct**

```go
// vm/markers.go
bigIntMarker uint32 = 10 << 24  // pick from free range

// vm/bigint.go
type BigIntObject struct {
    Value *big.Int
}
```

Use marker 10 (in the free range 10-15).

**Step 2: Add BigInt registry to ObjectRegistry**

Follow the existing pattern (see any registry in `vm/object_registry.go`):

```go
// In ObjectRegistry struct:
bigInts   map[uint32]*BigIntObject
bigIntsMu sync.RWMutex
bigIntID  atomic.Uint32

// Plus Register/Get/Unregister methods
```

**Step 3: Add BigInt Value constructors**

```go
// vm/bigint.go
func (v Value) IsBigInt() bool {
    if !v.IsSymbol() { return false }
    return (v.SymbolID() & markerMask) == bigIntMarker
}

func (v Value) BigIntID() uint32 {
    return v.SymbolID() & ^bigIntMarker
}

func FromBigIntID(id uint32) Value {
    return FromSymbolID(id | bigIntMarker)
}
```

**Step 4: Fix arithmetic overflow with BigInt promotion**

Replace the SmallInt fast path in all arithmetic primitives:

```go
func (i *Interpreter) primitivePlus(a, b Value) Value {
    if a.IsSmallInt() && b.IsSmallInt() {
        result := a.SmallInt() + b.SmallInt()
        if v, ok := TryFromSmallInt(result); ok {
            return v
        }
        // Overflow: promote to BigInt
        big := new(big.Int).Add(
            big.NewInt(a.SmallInt()),
            big.NewInt(b.SmallInt()),
        )
        return i.vm.newBigIntValue(big)
    }
    // ... existing float and mixed cases ...
    // Add BigInt cases:
    if isBigInt(a) || isBigInt(b) {
        return i.bigIntPlus(a, b)
    }
    return i.sendBinaryFallback(a, b, i.selectorPlus)
}
```

Note: SmallInt multiplication is trickier because `int64 * int64` can overflow Go's int64. Use `math/bits.Mul64` or convert to `big.Int` for the multiply and check if the result fits.

**Step 5: BigInt arithmetic helper**

```go
func (i *Interpreter) bigIntPlus(a, b Value) Value {
    ba := i.toBigInt(a)
    bb := i.toBigInt(b)
    result := new(big.Int).Add(ba, bb)
    return i.vm.bigIntOrSmallInt(result)
}

// toBigInt converts SmallInt or BigInt value to *big.Int
func (i *Interpreter) toBigInt(v Value) *big.Int {
    if v.IsSmallInt() {
        return big.NewInt(v.SmallInt())
    }
    obj := i.vm.registry.GetBigInt(v.BigIntID())
    return new(big.Int).Set(obj.Value) // copy to avoid mutation
}

// bigIntOrSmallInt demotes to SmallInt if the result fits
func (vm *VM) bigIntOrSmallInt(n *big.Int) Value {
    if n.IsInt64() {
        i := n.Int64()
        if v, ok := TryFromSmallInt(i); ok {
            return v
        }
    }
    return vm.newBigIntValue(n)
}
```

**Step 6: Hashing for Dictionary keys**

Add a case to `hashValue()` in `vm/dictionary_primitives.go`:

```go
func hashValue(or *ObjectRegistry, v Value) uint64 {
    // ... existing string case ...
    if isBigIntValue(v) {
        obj := or.GetBigInt(v.BigIntID())
        if obj != nil {
            // Use big.Int's Bytes() for deterministic hashing
            bytes := obj.Value.Bytes()
            var h uint64 = 14695981039346656037
            // Include sign
            if obj.Value.Sign() < 0 {
                h ^= 1
                h *= 1099511628211
            }
            for _, b := range bytes {
                h ^= uint64(b)
                h *= 1099511628211
            }
            return h
        }
    }
    return uint64(v)
}
```

**Step 7: Register BigInteger class and primitives**

Create `BigInteger` class as a subclass of `Object`. Register primitives for `+`, `-`, `*`, `/`, `\\`, `=`, `<`, `>`, `printString`, `hash`, `asSmallInteger`, `asFloat`.

**Step 8: Image format**

BigInt values are registry-based (stored as symbol IDs with a marker), so they follow the same serialization path as other registry values. Image persistence would need to serialize the `big.Int` bytes and reconstruct on load. This requires an image format version bump.

### Risk/Complexity

**Medium-High.** The core arithmetic change touches the hottest code path in the VM (every `+`, `-`, `*` operation). Key risks:

- **Performance regression**: The overflow check adds a branch to every SmallInt arithmetic op. Using `TryFromSmallInt` (which is already a bounds check) keeps it minimal. Benchmark before/after.
- **Multiplication overflow**: Go's `int64 * int64` can silently overflow before we even call `TryFromSmallInt`. Must either use `math/bits.Mul64` to detect overflow or always go through `big.Int` for multiply. The former is faster.
- **Image format**: Serializing BigInt values requires a version bump and migration logic.
- **Comparison semantics**: `SmallInt = BigInt` must work correctly (e.g., `SmallInt(5) = BigInt(5)` should be true). The `primitiveEQ` path needs updating.

---

## TODO 14: Metaclass Hierarchy

### Current Implementation

**Class struct** (`vm/object.go:50-64`): Each Class has both a `VTable` (instance methods) and a `ClassVTable` (class-side methods). The ClassVTable is created alongside the VTable in `NewClass()` (`vm/class.go:369-387`).

**Bootstrap** (`vm/vm.go:185-193`): `ObjectClass` and `ClassClass` are created, but the comment explicitly says "For now, we skip full metaclass support for simplicity." The `MetaclassClass` field exists on the VM struct (`vm/vm.go:27`) but is never assigned.

**Class-side dispatch** (`vm/vm.go:656-659`): When the interpreter detects a class-side send, it dispatches through `class.ClassVTable.Lookup(selectorID)`. ClassVTable has a parent pointer set to the superclass's ClassVTable in `NewClass()` (`vm/class.go:375`), so class-side inheritance already works.

**`class` message** (`vm/object_primitives.go:66-68`): The `class` primitive calls `vm.primitiveClass(recv)` which returns a class value (using the classValueRegistry). It does NOT return a Metaclass instance.

**Current behavior**: Sending `class` to an instance returns a first-class class value (an integer-encoded ID via `classValueMarker`). Sending `class` to a class value returns the ClassClass. There is no per-class metaclass -- all classes share `ClassClass`.

### Analysis

In Pharo/Squeak Smalltalk, the metaclass hierarchy is:

```
Point             -> Point class (metaclass)      -> Metaclass
Object            -> Object class (metaclass)      -> Metaclass
Point class       inherits from Object class
```

Key properties:
1. Every class C has exactly one metaclass "C class"
2. `C class` is an instance of `Metaclass`
3. Metaclass inheritance mirrors class inheritance: `(C subclass) class` is a subclass of `C class`
4. `Metaclass` is an instance of `Metaclass class`, which is an instance of `Metaclass` (the circularity)

Maggie already has the most important practical benefit: class-side methods and class-side inheritance via `ClassVTable`. What it lacks is:

- Metaclass as a first-class object (so `Point class` returns a real metaclass, not just `Class`)
- Per-class metaclass identity (each class has a unique metaclass)
- The ability to define methods on a specific metaclass at runtime

### Recommended Implementation

Given that Maggie already has working class-side dispatch and inheritance, a **minimal metaclass subset** is recommended rather than the full Pharo hierarchy.

**Step 1: Create Metaclass objects lazily**

Add a `Metaclass` field to the Class struct:

```go
// vm/object.go - add to Class struct
type Class struct {
    // ... existing fields ...
    Metaclass *Class // The metaclass for this class (lazily created)
}
```

Each metaclass is itself a Class whose VTable IS the ClassVTable of the original class. This means no dispatch changes are needed.

**Step 2: Lazy metaclass creation**

```go
func (c *Class) GetMetaclass() *Class {
    if c.Metaclass != nil {
        return c.Metaclass
    }
    meta := &Class{
        Name:       c.Name + " class",
        Namespace:  c.Namespace,
        VTable:     c.ClassVTable, // REUSE the existing ClassVTable
        NumSlots:   0,
    }
    // Metaclass's ClassVTable points to Metaclass class methods
    // For now, just use a fresh VTable
    meta.ClassVTable = NewVTable(meta, nil)

    // Wire up metaclass inheritance: (C subclass).Metaclass.VTable.parent
    // is already correct because ClassVTable parents were set up in NewClass()

    c.Metaclass = meta
    return meta
}
```

**Step 3: Update the `class` primitive**

```go
// When `class` is sent to a class value, return its metaclass
// When `class` is sent to an instance, return its class
c.AddMethod0(vm.Selectors, "class", func(vmPtr interface{}, recv Value) Value {
    v := vmPtr.(*VM)
    if IsClassValue(recv) {
        cls := v.GetClassFromValue(recv)
        if cls != nil {
            meta := cls.GetMetaclass()
            return v.registry.RegisterClassValue(meta)
        }
    }
    return v.primitiveClass(recv)
})
```

**Step 4: Bootstrap MetaclassClass**

```go
// In vm.bootstrap():
vm.MetaclassClass = vm.createBootstrapClass("Metaclass", vm.ClassClass)
vm.Globals["Metaclass"] = vm.classValue(vm.MetaclassClass)
```

**Step 5: Update `name` on metaclass**

Metaclass name should return "ClassName class":

```go
// Already handled by setting meta.Name = c.Name + " class" above
```

**Impact on image format**: Metaclasses are lazily created and not persisted separately -- they are reconstructed from the ClassVTable relationship on image load. No image format change needed.

**Impact on allClasses**: Metaclasses should NOT appear in `allClasses` / `allClassesSorted` -- they are implementation details. Filter them out by checking if the name ends with " class".

**Impact on compiler**: No changes needed. Class-side method compilation already targets `ClassVTable`. The compiler does not need to know about metaclasses.

### Risk/Complexity

**Low-Medium.** The key insight is that Maggie already has the underlying mechanism (ClassVTable with parent chain). Metaclasses are just a first-class wrapper around what already exists. Risks:

- **Circularity**: The Pharo metaclass tower has `Metaclass instance of Metaclass class instance of Metaclass`. This circularity needs careful bootstrap ordering but only affects the `class` message on Metaclass itself.
- **Reflection**: Code that inspects classes (IDE, debugger, fileOut) needs to handle metaclass names and not double-count them.
- **Image persistence**: If metaclasses share the ClassVTable, no extra serialization is needed. But if someone adds methods to a metaclass at runtime, those mutations need to be persisted. The current image format already saves ClassVTable methods per class, so this should work.

---

## TODO 15: Exception resume/retry/pass

### Current Implementation

**Exception handler stack** (`vm/exception.go:13-21`): Handlers are a linked list (via `Prev` pointer) stored on the `Interpreter` struct at `exceptionHandlers` (`vm/interpreter.go:98`). Each handler records:
- `ExceptionClass` -- what it catches
- `HandlerBlock` -- the `[:ex | ...]` block
- `FrameIndex` -- the call frame where `on:do:` was installed
- `HomeFrame`, `HomeSelf`, `Captures` -- for executing the handler block

**Signal mechanism** (`vm/exception.go:60-64, 378-419`): Exception signaling uses Go's `panic`/`recover`. `signalException()` creates an `ExceptionObject`, looks for a handler via `FindHandler()`, and either: (a) calls the handler block directly if found, or (b) `panic(SignaledException{...})` if no handler exists.

**on:do: implementation** (`vm/exception.go:422-487`): `evaluateBlockWithHandler()` installs a handler, evaluates the protected block inside a `func()` with `defer`/`recover`, and catches `SignaledException` panics. If caught and the handler matches, it unwinds frames and executes the handler block.

**Current resume/retry/pass** (`vm/exception.go:240-278`):
- `resume` (line 240): Sets `Handled = true` and returns `Nil`. This does NOT actually resume from the signal point -- it only marks the exception as handled.
- `resume:` (line 252): Same, but returns the provided value. Also does not actually resume.
- `pass` (line 263): Calls `signalExceptionObject()` again, which searches for the NEXT handler. But since the current handler was already popped in the initial signal handling, this finds the same handler or none. **This is broken.**
- `retry` (line 274): Stub that returns `Nil`. Not implemented.

**ExceptionObject** (`vm/exception.go:24-31`): Has `Resumable` bool (defaults to `true`).

### Analysis

The core problem is that Go `panic`/`recover` provides one-shot non-local transfer. Once the handler block is executing, the signal site's continuation (frame + IP) is gone -- the Go stack was unwound by `recover()`. Implementing true `resume:` requires saving the continuation at the signal point.

**resume:**: In ANSI Smalltalk, `resume:` returns a value from the `signal` call site. The signaling method continues execution as if `signal` returned that value. This requires:
1. Saving the interpreter state (frame pointer, instruction pointer, stack) at the signal point
2. When `resume:` is called, restoring that state and pushing the resume value

**retry**: Re-executes the protected block of `on:do:`. This requires:
1. Saving the block value and handler state at the `on:do:` call
2. When `retry` is called, re-entering the protected block evaluation

**pass**: Pops the current handler and re-signals. The current handler must not catch the re-signaled exception. This requires:
1. A way to mark the current handler as "exhausted" so `FindHandler()` skips it
2. Re-signaling the exception

### Recommended Implementation

**Approach A (Recommended): Interpreter-level continuations (no Go panic)**

Replace the `panic`/`recover` mechanism with interpreter-level exception handling. Instead of panicking, `signal` walks the handler stack directly, saves the signal continuation, and transfers control.

**Step 1: Save signal continuation in ExceptionObject**

```go
type ExceptionObject struct {
    // ... existing fields ...

    // Signal continuation (for resume:)
    SignalFrame   int     // Frame index at signal point
    SignalIP      int     // Instruction pointer at signal point
    SignalSP      int     // Stack pointer at signal point
    HasContinuation bool // Whether continuation was saved
}
```

**Step 2: Implement signal as interpreter-level operation**

Instead of `panic(SignaledException{...})`, make signal a special opcode or primitive that:

1. Saves current frame/IP/SP in the ExceptionObject
2. Finds the handler via `FindHandler()`
3. Unwinds frames to the handler's FrameIndex
4. Sets up a call to the handler block with the exception as argument
5. Marks the handler with a "currently handling" flag

```go
func (vm *VM) signalExceptionInterpreter(exVal Value, ex *ExceptionObject) {
    interp := vm.interpreter

    // Save continuation for potential resume:
    ex.SignalFrame = interp.fp
    ex.SignalIP = interp.frames[interp.fp].ip
    ex.SignalSP = interp.sp
    ex.HasContinuation = true

    // Find handler
    handler := interp.FindHandler(ex.ExceptionClass)
    if handler == nil {
        // No handler: fall back to panic for top-level catch
        panic(SignaledException{Exception: exVal, Object: ex})
    }

    // Mark handler as "in use" (for pass)
    handler.Active = true

    // Unwind to handler frame
    for interp.fp > handler.FrameIndex {
        interp.popFrame()
    }

    // Execute handler block (push exception as argument)
    bv := interp.getBlockValue(handler.HandlerBlock)
    interp.ExecuteBlock(bv.Block, bv.Captures, []Value{exVal},
        bv.HomeFrame, bv.HomeSelf, bv.HomeMethod)
}
```

**Step 3: resume: implementation**

```go
ex.AddMethod1(vm.Selectors, "resume:", func(vmPtr interface{}, recv Value, val Value) Value {
    v := vmPtr.(*VM)
    exObj := v.registry.GetException(recv.ExceptionID())
    if exObj == nil || !exObj.Resumable || !exObj.HasContinuation {
        return Nil
    }

    // Restore to signal point
    interp := v.interpreter
    interp.fp = exObj.SignalFrame
    interp.frames[interp.fp].ip = exObj.SignalIP
    interp.sp = exObj.SignalSP

    // Push the resume value as the "return value" of signal
    interp.push(val)

    exObj.Handled = true
    exObj.HasContinuation = false

    // Continue execution from the signal point
    // This needs to use a special return mechanism -- see below
    panic(ResumeException{Value: val, Frame: exObj.SignalFrame, IP: exObj.SignalIP})
})
```

The challenge is that `resume:` needs to transfer control back to a point that may be several Go function calls deep. Two sub-approaches:

**Sub-approach A1: Use Go panic for resume transfer too.** Define `ResumeException` and `RetryException` panic types. The `evaluateBlockWithHandler` defer catches these and acts accordingly. This keeps the existing `panic`/`recover` architecture but extends it.

**Sub-approach A2: Trampoline.** Make `ExecuteBlock` return a signal value (resume/retry/pass) and check it at the `on:do:` boundary. This avoids panic/recover entirely but requires changing ExecuteBlock's return convention.

**Recommendation: Sub-approach A1** -- it is the least invasive change.

**Step 4: retry implementation**

```go
// Define a RetryException panic type
type RetryException struct{}

ex.AddMethod0(vm.Selectors, "retry", func(_ interface{}, recv Value) Value {
    panic(RetryException{})
})
```

In `evaluateBlockWithHandler`, add a retry loop:

```go
func (vm *VM) evaluateBlockWithHandler(blockVal Value, exceptionClass *Class, handlerBlock Value) Value {
    bv := vm.interpreter.getBlockValue(blockVal)
    hbv := vm.interpreter.getBlockValue(handlerBlock)

    for { // retry loop
        handler := &ExceptionHandler{...}
        vm.interpreter.PushExceptionHandler(handler)

        var result Value
        var shouldRetry bool

        func() {
            defer func() {
                if r := recover(); r != nil {
                    switch r.(type) {
                    case RetryException:
                        shouldRetry = true
                    case SignaledException:
                        // ... existing handler logic ...
                    default:
                        panic(r)
                    }
                }
            }()
            result = vm.interpreter.ExecuteBlock(bv.Block, ...)
        }()

        vm.interpreter.PopExceptionHandler()

        if !shouldRetry {
            return result
        }
        // Loop back to re-evaluate the protected block
    }
}
```

**Step 5: pass implementation**

```go
ex.AddMethod0(vm.Selectors, "pass", func(vmPtr interface{}, recv Value) Value {
    v := vmPtr.(*VM)
    exObj := v.registry.GetException(recv.ExceptionID())
    if exObj == nil {
        return Nil
    }
    // The current handler is already popped (it was popped before invoking handler block).
    // Re-signal the exception to find the next handler in the chain.
    panic(SignaledException{Exception: recv, Object: exObj})
})
```

The current `pass` implementation (`vm/exception.go:263-271`) calls `signalExceptionObject` which does `interpreter.PopExceptionHandler()` again -- this is wrong because the handler was already popped. The fix is to simply re-panic, since the handler was already removed from the handler stack when the handler block started executing.

**Step 6: Proper resume: via ResumeException panic**

```go
type ResumeException struct {
    Value Value
}

ex.AddMethod1(vm.Selectors, "resume:", func(_ interface{}, recv Value, val Value) Value {
    // Transfer control back to the on:do: boundary with a resume value
    panic(ResumeException{Value: val})
})
```

In `evaluateBlockWithHandler`, catch `ResumeException`:

```go
case ResumeException:
    result = re.Value
    // The handler block's execution is abandoned;
    // the on:do: expression returns the resume value
```

This is a simplified resume that returns from the `on:do:` expression, not from the `signal` call site. True Pharo-style `resume:` (continuing from the signal point) is significantly harder with the current architecture and may not be worth the complexity.

### Risk/Complexity

**Medium-High.** The exception handling mechanism is delicate:

- **pass is almost free to fix** -- just change the method body to `panic(SignaledException{...})`.
- **retry is moderate** -- requires wrapping `evaluateBlockWithHandler` in a loop and adding a new panic type.
- **resume: (simplified)** -- returning a value from the `on:do:` expression is straightforward with a new panic type.
- **resume: (full Pharo semantics)** -- continuing from the signal site requires saving/restoring interpreter state across Go stack unwinds. This is architecturally difficult with the current panic/recover approach. Recommend deferring full resume: until/unless the interpreter moves to a trampoline model.
- The `ensure:` and `ifCurtailed:` interactions need testing -- a retry that re-enters the protected block must not trigger the ensure block prematurely.

---

## TODO 16: become: (Identity Swapping)

### Current Implementation

**become: is already fully implemented.** The Object struct has a `forward` field (`vm/object.go:20`) and three operations:

1. **Two-way become:** `Object.Become(other)` (`vm/object.go:324-347`) swaps vtable, size, and all slot contents between two objects. All existing references see the swapped contents because they still point to the same Object in memory.

2. **One-way becomeForward:** `Object.BecomeForward(other)` (`vm/object.go:352-357`) sets `obj.forward = other`, so all future accesses through `ObjectFromValue()` follow the forwarding chain via `Resolve()` (`vm/object.go:216-222`).

3. **Forwarding resolution:** `ObjectFromValue()` (`vm/object.go:247-254`) automatically follows forwarding pointers. `ObjectFromValueRaw()` (`vm/object.go:258-263`) skips forwarding for GC/become operations.

**Primitives** (`vm/object_primitives.go:275-307`): Both `become:` and `becomeForward:` are registered as instance methods on Object, plus an `isForwarded` query.

**Limitations with NaN-boxing**: `become:` only works for Object-tagged values (heap pointers). SmallInts, Floats, Symbols, Blocks, and other non-object values cannot be swapped because their identity IS their bit pattern. This is consistent with Pharo, where `become:` only works on object-reference types.

### Analysis

The implementation is solid and follows standard Smalltalk semantics. The two-way `Become()` swaps contents in place, which is the right approach for NaN-boxing since all references are raw pointers to the same `Object` struct.

**Performance considerations:**
- `Resolve()` is called on every `ObjectFromValue()`, which is a hot path. The `for` loop in `Resolve()` adds one nil check per access for non-forwarded objects (branch-predicted true -- essentially free).
- Forwarding chains could theoretically grow long. In practice, `BecomeForward` calls `other.Resolve()` to collapse chains, so the maximum chain length is 1 for new forwards.

**What could be improved:**
- **Become for registry-based values** (Dictionaries, Strings, BigInts): These are symbol-encoded, not object-encoded. To support `become:` on them, you would need an indirection layer in the registry (swap the entries two IDs point to). This is feasible but adds overhead to every registry lookup.
- **Bulk become** (`elementsExchange:with:` in Pharo): Not yet implemented but would be a simple loop over pairs.

### Recommended Fix

**No implementation work needed** -- `become:` is already functional. Potential enhancements to consider:

1. **Add `become:` error reporting**: Currently, `become:` on non-objects silently returns `recv`. Consider signaling a `PrimitiveFailed` exception instead.

2. **Add registry-level become**: If ever needed for String/Dictionary identity swapping:
```go
func (or *ObjectRegistry) SwapStrings(idA, idB uint32) {
    or.stringsMu.Lock()
    defer or.stringsMu.Unlock()
    or.strings[idA], or.strings[idB] = or.strings[idB], or.strings[idA]
}
```

3. **Forwarding pointer compaction**: Add a periodic compaction pass that follows forwarding chains and updates in-place, collapsing chains to direct pointers. This would be part of a future GC.

### Risk/Complexity

**Already done.** No risk for the current implementation. Registry-level become would be Low complexity if ever needed.

---

## TODO 17: Sorted/Hashed Collections

### Current Implementation

**Array** (`lib/Array.mag`): Fixed-size, 0-indexed collection. Has: `do:`, `collect:`, `select:`, `reject:`, `detect:`, `detect:ifNone:`, `inject:into:`, `includes:`, `indexOf:`, `,` (concatenation), `copyFrom:to:`, `copyWith:`, `first`, `last`, `isEmpty`, `notEmpty`, `printString`. No sort methods.

**Dictionary** (`lib/Dictionary.mag`, `vm/dictionary_primitives.go`): Backed by Go maps (`map[uint64]Value` for data and keys). Has: `at:`, `at:put:`, `at:ifAbsent:`, `at:ifAbsentPut:`, `includesKey:`, `removeKey:`, `removeKey:ifAbsent:`, `size`, `isEmpty`, `keys`, `values`, `do:`, `keysAndValuesDo:`, `copy`, `printString`. Uses FNV-1a for string keys, raw bits for other values (`vm/dictionary_primitives.go:33-49`).

**Missing collections:**
- **Set** -- not implemented
- **Bag** -- not implemented
- **SortedCollection / OrderedCollection** -- not implemented
- **OrderedDictionary** -- not implemented (Go maps are unordered)
- **Array sort** -- no sort primitive or method

**Existing protocols summary:**

| Protocol | Array | Dictionary |
|----------|-------|------------|
| do: | yes | yes |
| collect: | yes | no |
| select: | yes | no |
| reject: | yes | no |
| detect: | yes | no |
| detect:ifNone: | yes | no |
| inject:into: | yes | no |
| includes: | yes | no |
| includesKey: | no | yes |
| size | yes | yes |
| isEmpty | yes | yes |
| notEmpty | yes | yes |
| keys | no | yes |
| values | no | yes |
| copy | no | yes |

### Analysis

The collection hierarchy should prioritize practical utility over Smalltalk faithfulness. The most requested features in similar VMs are:

1. **Array sorting** -- needed constantly, requires a Go primitive for performance
2. **Set** -- can be backed by Dictionary (keys only), pure Maggie is feasible
3. **OrderedDictionary** -- insertion-ordered iteration, needs a Go-backed data structure
4. **Bag** -- can be backed by Dictionary with counts, pure Maggie
5. **SortedCollection** -- can be backed by sorted Array, mostly pure Maggie

### Recommended Implementation

**Phase 1: Array sort primitive (Go-side)**

Add to `vm/array_primitives.go`:

```go
// sort - in-place sort using a comparison block
c.AddMethod1(vm.Selectors, "sort:", func(vmPtr interface{}, recv Value, block Value) Value {
    v := vmPtr.(*VM)
    if !recv.IsObject() {
        return recv
    }
    obj := ObjectFromValue(recv)
    if obj == nil {
        return recv
    }
    n := obj.NumSlots()

    // Use Go's sort.Slice with the Maggie comparison block
    sort.SliceStable(/* ... */)
    // The challenge: sort.Slice needs a []T, but Object stores slots inline.
    // Solution: extract to a temporary []Value, sort, write back.

    slots := make([]Value, n)
    for i := 0; i < n; i++ {
        slots[i] = obj.GetSlot(i)
    }

    sort.SliceStable(slots, func(i, j int) bool {
        result := v.Send(block.AsBlock(), "value:value:", []Value{slots[i], slots[j]})
        return result.IsTruthy()
    })

    for i := 0; i < n; i++ {
        obj.SetSlot(i, slots[i])
    }

    return recv
})

// sort - default ascending sort (for numbers and strings)
c.AddMethod0(vm.Selectors, "sort", func(vmPtr interface{}, recv Value) Value {
    v := vmPtr.(*VM)
    // Sort using < as the comparison
    // Implementation: extract slots, sort by comparing with Send("<", ...)
    // ...
})

// sorted - return a sorted copy (non-destructive)
c.AddMethod0(vm.Selectors, "sorted", func(vmPtr interface{}, recv Value) Value {
    // Copy the array, then sort the copy
})
```

Note: Calling `vm.Send()` inside `sort.SliceStable` is expensive but correct. For numeric-only arrays, a fast path can compare SmallInt values directly without a message send.

**Phase 2: Set (pure Maggie, in lib/Set.mag)**

```smalltalk
Set subclass: Object
    instanceVars: dict

    classMethod: new [
        ^self basicNew init
    ]

    method: init [
        dict := Dictionary new.
        ^self
    ]

    method: add: anObject [
        dict at: anObject put: true.
        ^anObject
    ]

    method: remove: anObject [
        dict removeKey: anObject.
        ^anObject
    ]

    method: remove: anObject ifAbsent: aBlock [
        dict removeKey: anObject ifAbsent: aBlock.
        ^anObject
    ]

    method: includes: anObject [
        ^dict includesKey: anObject
    ]

    method: size [
        ^dict size
    ]

    method: isEmpty [
        ^dict isEmpty
    ]

    method: do: aBlock [
        dict keysAndValuesDo: [:k :v | aBlock value: k]
    ]

    method: collect: aBlock [
        | result |
        result := Set new.
        self do: [:each | result add: (aBlock value: each)].
        ^result
    ]

    method: select: aBlock [
        | result |
        result := Set new.
        self do: [:each | (aBlock value: each) ifTrue: [result add: each]].
        ^result
    ]

    method: reject: aBlock [
        ^self select: [:each | (aBlock value: each) not]
    ]

    method: union: otherSet [
        | result |
        result := self copy.
        otherSet do: [:each | result add: each].
        ^result
    ]

    method: intersection: otherSet [
        ^self select: [:each | otherSet includes: each]
    ]

    method: difference: otherSet [
        ^self reject: [:each | otherSet includes: each]
    ]

    method: asArray [
        | result i |
        result := Array new: self size.
        i := 0.
        self do: [:each | result at: i put: each. i := i + 1].
        ^result
    ]

    method: printString [
        | s |
        s := 'Set('.
        self do: [:each | s := s, each printString, ' '].
        ^s, ')'
    ]
```

**Phase 3: Bag (pure Maggie, in lib/Bag.mag)**

```smalltalk
Bag subclass: Object
    instanceVars: counts

    classMethod: new [
        ^self basicNew init
    ]

    method: init [
        counts := Dictionary new.
        ^self
    ]

    method: add: anObject [
        | current |
        current := counts at: anObject ifAbsent: [0].
        counts at: anObject put: current + 1.
        ^anObject
    ]

    method: remove: anObject [
        | current |
        current := counts at: anObject ifAbsent: [^self error: 'Element not found'].
        current <= 1
            ifTrue: [counts removeKey: anObject]
            ifFalse: [counts at: anObject put: current - 1].
        ^anObject
    ]

    method: occurrencesOf: anObject [
        ^counts at: anObject ifAbsent: [0]
    ]

    method: includes: anObject [
        ^counts includesKey: anObject
    ]

    method: size [
        | total |
        total := 0.
        counts do: [:count | total := total + count].
        ^total
    ]

    method: do: aBlock [
        counts keysAndValuesDo: [:element :count |
            count timesRepeat: [aBlock value: element]
        ]
    ]

    method: printString [
        ^'Bag(', counts printString, ')'
    ]
```

**Phase 4: OrderedDictionary (Go primitive)**

OrderedDictionary needs a Go-backed data structure because insertion-ordered iteration requires a linked list or ordered slice alongside the hash map. Pure Maggie implementation would be O(n) for lookups.

```go
type OrderedDictionaryObject struct {
    Data   map[uint64]Value   // hash -> value
    Keys   map[uint64]Value   // hash -> key
    Order  []uint64           // insertion-order hash list
}
```

Register with a new marker (e.g., `orderedDictMarker uint32 = 11 << 24`) or as a subclass of Dictionary with an additional `order` slice managed by Go primitives.

**Phase 5: SortedCollection (pure Maggie with sort primitive)**

```smalltalk
SortedCollection subclass: Object
    instanceVars: elements sortBlock

    classMethod: new [
        ^self sortBlock: [:a :b | a < b]
    ]

    classMethod: sortBlock: aBlock [
        ^self basicNew initWithSortBlock: aBlock
    ]

    method: initWithSortBlock: aBlock [
        elements := Array new: 0.
        sortBlock := aBlock.
        ^self
    ]

    method: add: anObject [
        elements := elements copyWith: anObject.
        elements sort: sortBlock.
        ^anObject
    ]

    method: size [ ^elements size ]
    method: at: index [ ^elements at: index ]
    method: first [ ^elements first ]
    method: last [ ^elements last ]
    method: do: aBlock [ elements do: aBlock ]
    method: includes: anObject [ ^elements includes: anObject ]

    method: printString [
        ^'SortedCollection(', elements printString, ')'
    ]
```

Note: This is O(n log n) per insertion due to full re-sort. A binary search insertion would be O(n) due to array copying but more efficient in practice. The sort primitive makes this feasible.

### Risk/Complexity

**Low-Medium overall.**

- **Array sort primitive**: Medium. The main challenge is invoking Maggie blocks inside Go's `sort.SliceStable`. This requires the VM's `Send` mechanism, which re-enters the interpreter. Must verify re-entrancy is safe (it should be, since `on:do:` and other primitives already do this).
- **Set/Bag**: Low. Pure Maggie, backed by existing Dictionary primitives. No Go changes needed.
- **OrderedDictionary**: Medium. Requires a new Go-backed data structure, registry entry, and marker allocation. Follow the Dictionary pattern.
- **SortedCollection**: Low. Pure Maggie, depends on Array sort primitive.

---

## Summary Table

| TODO | Feature | Complexity | Priority | Dependencies |
|------|---------|-----------|----------|-------------|
| 13 | BigInteger | Medium-High | High (fixes crash) | None |
| 14 | Metaclass Hierarchy | Low-Medium | Medium | None |
| 15 | Exception resume/retry/pass | Medium-High | Medium | None |
| 16 | become: | Already done | N/A | N/A |
| 17 | Sorted/Hashed Collections | Low-Medium | High (practical utility) | None (Phase 1 enables Phase 5) |

**Recommended execution order:**

1. **TODO 13 (BigInteger)** -- priority because the current SmallInt overflow is a crash bug. Even if BigInteger support is deferred, the overflow detection + Float promotion should be done immediately.
2. **TODO 17 Phase 1-3 (Array sort, Set, Bag)** -- high practical value, relatively low risk.
3. **TODO 15 (Exception pass fix)** -- the `pass` fix is a one-line change; `retry` is moderate effort.
4. **TODO 14 (Metaclass)** -- low risk, moderate value for Smalltalk conformance.
5. **TODO 17 Phase 4-5 (OrderedDictionary, SortedCollection)** -- nice to have, can follow later.
6. **TODO 15 (Exception resume: full)** -- defer until needed, high complexity for limited practical value.
