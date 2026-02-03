# Maggie Language Design Document

## Overview

Maggie is a late-bound Smalltalk dialect implemented in Go. It combines Smalltalk's live, image-based development experience with Go's goroutine-based concurrency model.

### Design Goals

1. **Late binding** - Message sends resolved at runtime via vtables
2. **Everything is an object** - Uniform object model (Piumarta-inspired)
3. **Live development** - Hot reload, inspect/modify running code, image snapshots
4. **Fast concurrency** - Goroutines and channels as first-class primitives
5. **Deployable** - AOT compilation to standalone binaries
6. **Pragmatic** - Simple core with room for optimization

### Lineage

Maggie is forked from Procyon (the Trashtalk compiler). While Trashtalk targets Bash and maintains backward compatibility, Maggie is a clean break: a proper Smalltalk with Go as both implementation language and interop target.

---

## Syntax

Maggie uses **standard Smalltalk syntax**. No extensions or modifications.

```smalltalk
"Class definition"
Object subclass: #Counter
    instanceVariableNames: 'value'
    classVariableNames: ''
    package: 'MyApp'.

"Methods"
Counter >> increment [
    value := value + 1.
    ^value
]

Counter >> increment: amount [
    value := value + amount.
    ^value
]

"Message sends"
counter := Counter new.
counter increment.
counter increment: 5.

"Blocks"
[:x | x * 2] value: 5.
[self doSomething] fork.

"Control flow"
(x > 0) ifTrue: [self positive] ifFalse: [self negative].
1 to: 10 do: [:i | Transcript show: i].
```

**Rationale**: Standard Smalltalk syntax is well-understood, has extensive documentation, and a proven track record. We can always add extensions later if real pain points emerge. Starting simple.

---

## Module System

Maggie uses **class-level namespaces** with `::` as the separator. Classes live in namespaces; methods belong to their class.

### Declaring Namespaces

Namespace and import declarations appear at the top of a `.mag` file, before any class or method definitions:

```smalltalk
namespace: 'MyApp::Core'
import: 'MyApp::Utils'

Counter subclass: Object
  instanceVars: value

  method: increment [
    value := value + 1.
    ^value
  ]
```

### Imports and Class Resolution

```smalltalk
namespace: 'MyApp::Views'
import: 'MyApp::Models'
import: 'MyApp::Core'

-- User is resolved by checking:
-- 1. MyApp::Views::User (current namespace)
-- 2. MyApp::Models::User (first import)
-- 3. MyApp::Core::User (second import)
-- 4. User (bare/root namespace)
profile := User find: userId.
```

### Directory-as-Namespace Convention

When loading source from directories, the directory structure maps to namespaces automatically:

```
src/myapp/models/User.mag  → MyApp::Models
src/myapp/Main.mag         → MyApp
lib/Array.mag              → (no namespace, root)
```

Path segments are PascalCased: `my-app/` → `MyApp`, `user_models/` → `UserModels`. An explicit `namespace:` in the file overrides the derived namespace.

### Rules

- Every class lives in exactly one namespace
- Method names are not namespaced (they belong to their class)
- Within a namespace, classes can reference each other unqualified
- Cross-namespace references require qualification or import
- The empty namespace is the default (for bootstrapping and REPL)
- Classes are registered with both `Namespace::Name` and bare `Name` (bare name only if no conflict)

### Runtime fileIn/fileOut

Source files can be loaded and exported at runtime:

```smalltalk
Compiler fileIn: 'path/to/File.mag'.              -- load one file
Compiler fileInAll: 'path/to/src'.                 -- load directory recursively
Compiler fileOut: 'Counter' to: '/tmp/Counter.mag'. -- export class to source
Compiler fileOutNamespace: 'MyApp' to: '/tmp/'.     -- export namespace
Compiler saveImage: 'my-app.image'.                 -- save VM state
```

### Project Manifest (maggie.toml)

Projects use a TOML manifest for configuration and dependency management:

```toml
[project]
name = "my-app"
namespace = "MyApp"
version = "0.1.0"

[source]
dirs = ["src"]
entry = "Main.start"

[dependencies]
yutani = { git = "https://github.com/chazu/yutani-mag", tag = "v0.5.0" }
helper = { path = "../helper" }

[image]
output = "my-app.image"
include-source = true
```

Dependencies are resolved via topological sort, fetched to `.maggie/deps/`, and locked in `.maggie/lock.toml`. Transitive dependencies are handled automatically.

**Rationale**: Simple, matches how most modern languages work. Avoids the complexity of method-level namespacing while preventing class name collisions. The `::` separator follows Ruby/C++ convention and is familiar to developers.

---

## Object Model

### Inspiration

The object model draws from Ian Piumarta and Alessandro Warth's "Open, Extensible Object Models" paper. The core insight: everything is an object with a vtable pointer, and method lookup is itself a message send.

### Value Representation: NaN-Boxing

All values are represented as 64-bit IEEE 754 doubles. Non-float values are encoded in the NaN (Not-a-Number) space.

**Why NaN-boxing?** Float operations are native speed - no boxing, no indirection. This matters for numeric-heavy code.

```go
type Value uint64

// IEEE 754 double NaN: exponent bits all 1, non-zero mantissa
// Quiet NaN prefix: 0x7FF8_0000_0000_0000
// This leaves 51 bits for payload (more than enough for 48-bit pointers)

const (
    nanBits     = 0x7FF8000000000000  // Quiet NaN prefix
    tagMask     = 0x0007000000000000  // 3 tag bits within NaN space
    payloadMask = 0x0000FFFFFFFFFFFF  // 48 bits for payload

    tagObject  = 0x0001000000000000  // Heap object pointer
    tagInt     = 0x0002000000000000  // Small integer (48-bit signed)
    tagSpecial = 0x0003000000000000  // nil, true, false
    tagSymbol  = 0x0004000000000000  // Interned symbol ID
)
```

**Encoding/decoding:**

```go
import "math"

// Float (native, no encoding needed)
func isFloat(v Value) bool {
    // If it's not a NaN, it's a float
    return (v & 0x7FF0000000000000) != 0x7FF0000000000000
}

func toFloat(v Value) float64 {
    return math.Float64frombits(uint64(v))
}

func fromFloat(f float64) Value {
    return Value(math.Float64bits(f))
}

// Object pointer
func isObject(v Value) bool {
    return (v & (nanBits | tagMask)) == (nanBits | tagObject)
}

func toObject(v Value) *Object {
    ptr := uintptr(v & payloadMask)
    return (*Object)(unsafe.Pointer(ptr))
}

func fromObject(obj *Object) Value {
    ptr := uint64(uintptr(unsafe.Pointer(obj)))
    return Value(nanBits | tagObject | ptr)
}

// Small integer (48-bit signed)
func isSmallInt(v Value) bool {
    return (v & (nanBits | tagMask)) == (nanBits | tagInt)
}

func toSmallInt(v Value) int64 {
    payload := int64(v & payloadMask)
    // Sign extend from 48 bits
    if payload&0x800000000000 != 0 {
        payload |= 0xFFFF000000000000
    }
    return payload
}

func fromSmallInt(n int64) Value {
    return Value(nanBits | tagInt | (uint64(n) & payloadMask))
}

// Special values
const (
    valueNil   = Value(nanBits | tagSpecial | 0)
    valueTrue  = Value(nanBits | tagSpecial | 1)
    valueFalse = Value(nanBits | tagSpecial | 2)
)

// Symbols
func isSymbol(v Value) bool {
    return (v & (nanBits | tagMask)) == (nanBits | tagSymbol)
}

func toSymbolID(v Value) int {
    return int(v & payloadMask)
}

func fromSymbolID(id int) Value {
    return Value(nanBits | tagSymbol | uint64(id))
}
```

**What this gives us:**

| Type | Representation | Performance |
|------|----------------|-------------|
| Float | Native IEEE 754 double | Native speed (unboxed!) |
| SmallInt | 48-bit signed in NaN payload | Fast (decode is cheap) |
| Object | 48-bit pointer in NaN payload | One indirection |
| Symbol | ID in NaN payload | Integer comparison |
| Special | nil/true/false encoded | Direct comparison |

### Object Layout

Objects use a hybrid layout optimized for common cases:

```go
type Object struct {
    vtable   *VTable
    slot0    Value
    slot1    Value
    slot2    Value
    slot3    Value
    overflow []Value  // for objects with >4 instance variables
}
```

**Rationale**: Most objects have ≤4 instance variables (Point, Association, Range, small widgets). Inline slots avoid slice overhead for the common case. Larger objects spill to overflow.

Field access is by index:
```go
func (obj *Object) getSlot(index int) Value {
    switch index {
    case 0: return obj.slot0
    case 1: return obj.slot1
    case 2: return obj.slot2
    case 3: return obj.slot3
    default: return obj.overflow[index-4]
    }
}

func (obj *Object) setSlot(index int, value Value) {
    switch index {
    case 0: obj.slot0 = value
    case 1: obj.slot1 = value
    case 2: obj.slot2 = value
    case 3: obj.slot3 = value
    default: obj.overflow[index-4] = value
    }
}
```

### VTables and Method Dispatch

VTables map selector IDs to methods:

```go
type VTable struct {
    class     *Class
    methods   []Method        // indexed by selector ID
    parent    *VTable         // for inheritance lookup
}

type Class struct {
    name       string
    namespace  string
    superclass *Class
    vtable     *VTable
    instVars   []string       // instance variable names
    classVars  map[string]Value
}
```

**Selector IDs**: Selectors are assigned numeric IDs at compile time via a global table:
```go
var selectorTable = &SelectorTable{
    byName: map[string]int{},
    byID:   []string{},
}

func (st *SelectorTable) intern(name string) int {
    if id, ok := st.byName[name]; ok {
        return id
    }
    id := len(st.byID)
    st.byName[name] = id
    st.byID = append(st.byID, name)
    return id
}
```

Method dispatch:
```go
func (vt *VTable) lookup(selector int) Method {
    if selector < len(vt.methods) && vt.methods[selector] != nil {
        return vt.methods[selector]
    }
    if vt.parent != nil {
        return vt.parent.lookup(selector)
    }
    return nil  // does not understand
}
```

### Method Representation

Methods are specialized by arity to avoid slice allocation on common calls:

```go
type Method interface {
    invoke(vm *VM, receiver Value, args []Value) Value
}

type Method0 func(vm *VM, receiver Value) Value
type Method1 func(vm *VM, receiver Value, arg1 Value) Value
type Method2 func(vm *VM, receiver Value, arg1, arg2 Value) Value
type Method3 func(vm *VM, receiver Value, arg1, arg2, arg3 Value) Value
type MethodN func(vm *VM, receiver Value, args []Value) Value

func (m Method0) invoke(vm *VM, receiver Value, args []Value) Value {
    return m(vm, receiver)
}

func (m Method1) invoke(vm *VM, receiver Value, args []Value) Value {
    return m(vm, receiver, args[0])
}
// ... etc
```

### Inline Caching

Call sites cache the last vtable lookup to accelerate repeated sends:

```go
type CallSite struct {
    selector     int
    cachedVT     *VTable
    cachedMethod Method
}

func (cs *CallSite) send(vm *VM, receiver Value, args []Value) Value {
    vt := vm.getVTable(receiver)

    if vt == cs.cachedVT {
        // Fast path: cache hit
        return cs.cachedMethod.invoke(vm, receiver, args)
    }

    // Slow path: lookup and cache
    method := vt.lookup(cs.selector)
    if method == nil {
        return vm.doesNotUnderstand(receiver, cs.selector, args)
    }
    cs.cachedVT = vt
    cs.cachedMethod = method
    return method.invoke(vm, receiver, args)
}
```

This turns repeated monomorphic sends into: one pointer comparison + direct call.

**Future optimization**: Polymorphic inline caches (PICs) for call sites that see multiple types.

---

## Numeric Tower

Maggie supports the following numeric types:

| Type | Representation | Range/Precision |
|------|----------------|-----------------|
| **SmallInteger** | NaN-boxed (48-bit signed) | ±140 trillion |
| **LargeInteger** | Boxed, arbitrary precision | Unlimited |
| **Float** | Native IEEE 754 double (unboxed!) | Standard 64-bit float |
| **Fraction** | Boxed, pair of integers | Exact rationals |
| **Decimal** | Boxed (wraps Go decimal library) | Arbitrary precision decimals |

**No Complex numbers** - not needed for target use cases.

### Automatic Promotion

```smalltalk
"SmallInteger overflow promotes to LargeInteger"
x := 140000000000000.  "SmallInteger"
y := x * 1000.         "Overflows → LargeInteger"

"Integer division can produce Fraction"
f := 1 / 3.            "Fraction (1/3)"

"Mixed arithmetic"
f + 0.5.               "Float (0.833...)"
```

### Decimal for Money

```smalltalk
price := Decimal fromString: '19.99'.
tax := price * (Decimal fromString: '0.08').
total := price + tax.  "Exact decimal arithmetic"
```

**Implementation**: Decimal wraps Go's `shopspring/decimal` or similar library.

---

## Error Handling

Maggie uses a **dual approach**:

1. **Result types** for expected failures (file not found, parse error, network timeout)
2. **Exceptions** for unexpected failures (bugs, invariant violations, out of memory)

### Result Types

Result is an object that represents either success or failure:

```smalltalk
"Basic usage"
result := File open: '/path/to/file'.

result isSuccess ifTrue: [
    result value readContents
] ifFalse: [
    self handleError: result error
].

"Pattern-matching style"
(File open: '/path/to/file')
    onSuccess: [:file | file readContents]
    onFailure: [:err | self log: err message].

"Chaining (monadic bind)"
(File open: path)
    then: [:f | f readContents]
    then: [:contents | Json parse: contents]
    then: [:data | self process: data]
    onFailure: [:err | self handleError: err].

"Early return on failure"
result := File open: path.
result ifFailed: [:err | ^self handleError: err].
file := result value.
"continue with file..."
```

### Result Class Implementation

```smalltalk
Object subclass: #Result
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Core'.

Result subclass: #Success
    instanceVariableNames: 'value'
    package: 'Core'.

Success >> value [
    ^value
]

Success >> isSuccess [
    ^true
]

Success >> onSuccess: successBlock onFailure: failureBlock [
    ^successBlock value: value
]

Success >> then: aBlock [
    ^aBlock value: value
]

Success >> ifFailed: aBlock [
    "Do nothing, we're successful"
    ^self
]

Result subclass: #Failure
    instanceVariableNames: 'error'
    package: 'Core'.

Failure >> error [
    ^error
]

Failure >> isSuccess [
    ^false
]

Failure >> onSuccess: successBlock onFailure: failureBlock [
    ^failureBlock value: error
]

Failure >> then: aBlock [
    "Propagate failure without executing block"
    ^self
]

Failure >> ifFailed: aBlock [
    ^aBlock value: error
]
```

### Exceptions

For unexpected failures, use traditional Smalltalk exceptions:

```smalltalk
"Signaling"
self error: 'Something went wrong'.
AssertionError signal: 'Invariant violated'.

"Handling"
[self riskyOperation]
    on: Error
    do: [:ex | self handleException: ex].

"Ensuring cleanup"
[self riskyOperation]
    ensure: [self cleanup].
```

### Exception Hierarchy

```
Exception
  Error
    MessageNotUnderstood
    ZeroDivide
    SubscriptOutOfBounds
    StackOverflow
```

`StackOverflow` is raised when call frame depth exceeds `DefaultMaxFrameDepth` (4096). It is catchable via `on:do:` like any other exception.

### When to Use Which

| Situation | Use |
|-----------|-----|
| File not found | Result (expected) |
| Network timeout | Result (expected) |
| Parse error | Result (expected) |
| Invalid user input | Result (expected) |
| Nil pointer | Exception (bug) |
| Assertion failure | Exception (bug) |
| Out of memory | Exception (unexpected) |
| Index out of bounds | Exception (bug) |

**Guideline**: If the caller should reasonably handle the failure, use Result. If it indicates a bug or unrecoverable situation, use Exception.

---

## Blocks and Closures

Blocks are objects with a dedicated vtable. They capture their lexical environment and support non-local returns.

### Block Structure

```go
type Block struct {
    vtable *VTable          // always blockVTable
    method *BlockMethod     // compiled bytecode
    env    []Value          // captured variables
    home   *ActivationRecord // for non-local return
}

type BlockMethod struct {
    arity      int
    numTemps   int
    bytecode   []byte
    literals   []Value
    outer      *CompiledMethod  // enclosing method (for debugging)
}
```

### Block Invocation

Blocks respond to `value`, `value:`, `value:value:`, etc. The VM fast-paths these:

```go
var (
    selectorValue  = selectorTable.intern("value")
    selectorValue1 = selectorTable.intern("value:")
    selectorValue2 = selectorTable.intern("value:value:")
)

func (vm *VM) send(receiver Value, selector int, args []Value) Value {
    if isObject(receiver) {
        obj := toObject(receiver)

        // Fast path for block invocation
        if obj.vtable == blockVTable {
            switch selector {
            case selectorValue, selectorValue1, selectorValue2:
                return vm.invokeBlock(obj, args)
            }
        }

        // Normal dispatch
        return vm.dispatch(obj.vtable, receiver, selector, args)
    }
    // Handle NaN-boxed values...
}
```

Direct block invocation (bypassing vtable):
```go
func (vm *VM) invokeBlock(blockObj *Object, args []Value) Value {
    block := toBlock(blockObj)
    return vm.executeBlock(block, args)
}
```

### Non-Local Returns

Blocks can return from their enclosing method using `^`:

```smalltalk
findFirst: predicate
    self do: [:each |
        (predicate value: each) ifTrue: [^each]  "returns from findFirst:"
    ].
    ^nil
```

**Implementation**: panic/recover with goroutine boundary checking.

```go
type ActivationRecord struct {
    method      *CompiledMethod
    goroutineID uint64
    valid       bool  // false after method returns
}

type nonLocalReturn struct {
    target *ActivationRecord
    value  Value
}

func (vm *VM) blockReturn(block *Block, value Value) {
    home := block.home

    if !home.valid {
        panic("cannot return: enclosing method has already returned")
    }

    if home.goroutineID != vm.currentGoroutineID() {
        panic("cannot return: target is on different goroutine")
    }

    panic(nonLocalReturn{target: home, value: value})
}

func (vm *VM) executeMethod(method *CompiledMethod, receiver Value, args []Value) (result Value) {
    activation := &ActivationRecord{
        method:      method,
        goroutineID: vm.currentGoroutineID(),
        valid:       true,
    }

    defer func() {
        activation.valid = false
        if r := recover(); r != nil {
            if nlr, ok := r.(nonLocalReturn); ok && nlr.target == activation {
                result = nlr.value
                return
            }
            panic(r)  // not ours, propagate
        }
    }()

    return vm.interpret(activation, receiver, args)
}
```

**Cross-goroutine restriction**: Non-local returns across goroutine boundaries are not allowed. This is a conscious design decision - non-local return is a control flow mechanism, not a cross-thread signaling mechanism.

---

## Concurrency Model

Maggie uses Go's goroutines and channels directly, exposed as first-class Smalltalk objects.

### Goroutines

Spawning a goroutine:
```smalltalk
worker := [ self processItems ] fork.
```

Implementation:
```go
func blockFork(vm *VM, receiver Value, args []Value) Value {
    block := toBlock(toObject(receiver))

    process := vm.createProcess()

    go func() {
        defer vm.processExited(process)
        vm.invokeBlock(block, nil)
    }()

    return process
}
```

The `Process` object wraps goroutine metadata:
```go
type Process struct {
    vtable *VTable
    id     uint64
    done   chan struct{}
    result Value
    err    error
}
```

### Channels

Channels wrap Go channels directly:
```go
type Channel struct {
    vtable *VTable
    ch     chan Value
    closed bool
}
```

Usage:
```smalltalk
ch := Channel new.
ch := Channel new: 10.  "buffered"

[ ch send: 42 ] fork.
value := ch receive.

ch close.
```

Primitive implementations:
```go
func channelNew(vm *VM, receiver Value, args []Value) Value {
    ch := &Channel{
        vtable: channelVTable,
        ch:     make(chan Value),
    }
    return fromObject(ch)
}

func channelNewBuffered(vm *VM, receiver Value, args []Value) Value {
    size := toSmallInt(args[0])
    ch := &Channel{
        vtable: channelVTable,
        ch:     make(chan Value, size),
    }
    return fromObject(ch)
}

func channelSend(vm *VM, receiver Value, args []Value) Value {
    ch := toChannel(toObject(receiver))
    ch.ch <- args[0]
    return receiver
}

func channelReceive(vm *VM, receiver Value, args []Value) Value {
    ch := toChannel(toObject(receiver))
    return <-ch.ch
}
```

### Select

**Static select** (compiled to Go select):
```smalltalk
Process select: {
    ch1 onReceive: [:msg | self handleMessage: msg].
    ch2 onSend: value then: [self afterSend].
    Process after: 5 seconds do: [self timeout].
}
```

The compiler recognizes this pattern and generates native Go select.

**Dynamic select** (using reflect.Select):
```go
func processSelect(vm *VM, receiver Value, args []Value) Value {
    cases := toArray(args[0])

    reflectCases := make([]reflect.SelectCase, len(cases))
    handlers := make([]Value, len(cases))

    for i, c := range cases {
        selectCase := toSelectCase(c)
        reflectCases[i] = selectCase.toReflectCase()
        handlers[i] = selectCase.handler
    }

    chosen, recv, _ := reflect.Select(reflectCases)

    handler := handlers[chosen]
    if recv.IsValid() {
        return vm.send(handler, selectorValue1, []Value{fromReflect(recv)})
    }
    return vm.send(handler, selectorValue, nil)
}
```

Dynamic select is ~10-20x slower than native but allows runtime-determined cases.

---

## Bytecode Format

### Design Principles

- **Stack-based**: Matches Smalltalk tradition, simple to compile and interpret
- **Compact**: 8-bit opcodes, variable operands
- **Optimized common cases**: Dedicated opcodes for frequent operations

### Instruction Set

```
=== Stack Operations ===
00  NOP
01  POP                              # discard top
02  DUP                              # duplicate top

=== Push Constants ===
10  PUSH_NIL
11  PUSH_TRUE
12  PUSH_FALSE
13  PUSH_SELF
14  PUSH_INT8       <int8>           # inline small integer
15  PUSH_INT32      <int32>          # inline integer
16  PUSH_LITERAL    <index:16>       # from literal frame
17  PUSH_FLOAT      <float64>        # inline float (8 bytes)

=== Variables ===
20  PUSH_TEMP       <index:8>        # local/argument
21  PUSH_IVAR       <index:8>        # instance variable
22  PUSH_GLOBAL     <index:16>       # global/class reference
23  STORE_TEMP      <index:8>
24  STORE_IVAR      <index:8>
25  STORE_GLOBAL    <index:16>
26  PUSH_CAPTURED   <index:8>        # from block environment
27  STORE_CAPTURED  <index:8>

=== Message Sends ===
30  SEND            <selector:16> <argc:8>
31  SEND_SUPER      <selector:16> <argc:8>
32  TAIL_SEND       <selector:16> <argc:8>   # reuse frame for self-recursion

=== Optimized Sends ===
40  SEND_PLUS                        # +
41  SEND_MINUS                       # -
42  SEND_TIMES                       # *
43  SEND_DIV                         # /
44  SEND_MOD                         # \\
45  SEND_LT                          # <
46  SEND_GT                          # >
47  SEND_LE                          # <=
48  SEND_GE                          # >=
49  SEND_EQ                          # =
4A  SEND_NE                          # ~=
4B  SEND_AT                          # at:
4C  SEND_AT_PUT                      # at:put:
4D  SEND_SIZE                        # size
4E  SEND_VALUE                       # value
4F  SEND_VALUE1                      # value:
50  SEND_VALUE2                      # value:value:
51  SEND_NEW                         # new
52  SEND_CLASS                       # class

=== Control Flow ===
60  JUMP            <offset:16>      # unconditional
61  JUMP_TRUE       <offset:16>      # pop, jump if true
62  JUMP_FALSE      <offset:16>      # pop, jump if false
63  JUMP_NIL        <offset:16>      # pop, jump if nil
64  JUMP_NOT_NIL    <offset:16>      # pop, jump if not nil

=== Returns ===
70  RETURN_TOP                       # return top of stack
71  RETURN_SELF                      # return self (implicit)
72  RETURN_NIL                       # return nil
73  BLOCK_RETURN                     # non-local return from block

=== Blocks ===
80  CREATE_BLOCK    <method:16> <nCaptures:8>
81  CAPTURE_TEMP    <index:8>        # capture local into building block
82  CAPTURE_IVAR    <index:8>        # capture ivar into building block

=== Object Creation ===
90  CREATE_ARRAY    <size:8>         # pop N items, push array
91  CREATE_OBJECT   <class:16> <size:8>  # create instance with N slots
```

### Compiled Method Structure

```go
type CompiledMethod struct {
    Selector   int              // selector ID
    Class      *Class           // defining class
    Arity      int              // number of arguments
    NumTemps   int              // arguments + locals
    Literals   []Value          // constants used by method
    Bytecode   []byte           // the bytecode
    Blocks     []*BlockMethod   // nested block methods

    // Debugging support
    Source     string           // original source text
    SourceMap  []SourceLoc      // bytecode offset → source position
}

type SourceLoc struct {
    Offset int  // bytecode offset
    Line   int
    Column int
}

type BlockMethod struct {
    Arity      int
    NumTemps   int
    NumCaptures int
    Literals   []Value
    Bytecode   []byte
    Outer      *CompiledMethod  // for debugging/source mapping
}
```

### Bytecode Example

Smalltalk:
```smalltalk
increment
    | newValue |
    newValue := value + 1.
    value := newValue.
    ^newValue
```

Bytecode:
```
PUSH_IVAR      0          # push 'value'
PUSH_INT8      1          # push 1
SEND_PLUS                 # value + 1
STORE_TEMP     0          # newValue := ...
PUSH_TEMP      0          # push newValue
STORE_IVAR     0          # value := newValue
PUSH_TEMP      0          # push newValue
RETURN_TOP                # ^newValue
```

---

## VM Architecture

### Core VM Structure

```go
type VM struct {
    // Global state
    namespaces    map[string]*Namespace
    globals       map[string]Value
    selectorTable *SelectorTable
    symbolTable   *SymbolTable

    // Well-known vtables (for fast-path checks)
    objectVTable  *VTable
    classVTable   *VTable
    blockVTable   *VTable
    integerVTable *VTable
    floatVTable   *VTable
    stringVTable  *VTable
    arrayVTable   *VTable
    channelVTable *VTable

    // Memory management
    heap          *Heap

    // Debugging
    debugServer   *DebugServer
    breakpoints   map[*CompiledMethod][]int  // method → bytecode offsets
}

type Namespace struct {
    name    string
    classes map[string]*Class
}
```

### Interpreter Loop

```go
func (vm *VM) interpret(activation *ActivationRecord, receiver Value, args []Value) Value {
    method := activation.method
    bc := method.Bytecode
    literals := method.Literals

    temps := make([]Value, method.NumTemps)
    copy(temps, args)  // arguments are first temps

    stack := &Stack{data: make([]Value, 0, 32)}
    ip := 0

    for {
        op := bc[ip]
        ip++

        switch op {
        case PUSH_NIL:
            stack.push(valueNil)

        case PUSH_TRUE:
            stack.push(valueTrue)

        case PUSH_FALSE:
            stack.push(valueFalse)

        case PUSH_SELF:
            stack.push(receiver)

        case PUSH_INT8:
            val := int8(bc[ip])
            ip++
            stack.push(fromSmallInt(int64(val)))

        case PUSH_FLOAT:
            bits := binary.LittleEndian.Uint64(bc[ip:])
            ip += 8
            stack.push(Value(bits))  // Already in NaN-boxed format

        case PUSH_LITERAL:
            idx := binary.LittleEndian.Uint16(bc[ip:])
            ip += 2
            stack.push(literals[idx])

        case PUSH_TEMP:
            idx := bc[ip]
            ip++
            stack.push(temps[idx])

        case STORE_TEMP:
            idx := bc[ip]
            ip++
            temps[idx] = stack.top()

        case PUSH_IVAR:
            idx := bc[ip]
            ip++
            obj := toObject(receiver)
            stack.push(obj.getSlot(int(idx)))

        case STORE_IVAR:
            idx := bc[ip]
            ip++
            obj := toObject(receiver)
            obj.setSlot(int(idx), stack.top())

        case SEND:
            sel := int(binary.LittleEndian.Uint16(bc[ip:]))
            ip += 2
            argc := int(bc[ip])
            ip++
            args := stack.popN(argc)
            rcvr := stack.pop()
            result := vm.send(rcvr, sel, args)
            stack.push(result)

        case SEND_PLUS:
            b := stack.pop()
            a := stack.pop()
            if isSmallInt(a) && isSmallInt(b) {
                // Fast path: integer addition
                sum := toSmallInt(a) + toSmallInt(b)
                // TODO: overflow check → LargeInteger
                stack.push(fromSmallInt(sum))
            } else if isFloat(a) && isFloat(b) {
                // Fast path: float addition (native speed!)
                stack.push(fromFloat(toFloat(a) + toFloat(b)))
            } else {
                stack.push(vm.send(a, selectorPlus, []Value{b}))
            }

        case JUMP:
            offset := int16(binary.LittleEndian.Uint16(bc[ip:]))
            ip += int(offset)

        case JUMP_FALSE:
            offset := int16(binary.LittleEndian.Uint16(bc[ip:]))
            ip += 2
            cond := stack.pop()
            if cond == valueFalse || cond == valueNil {
                ip += int(offset) - 2  // -2 because we already advanced
            }

        case RETURN_TOP:
            return stack.pop()

        case RETURN_SELF:
            return receiver

        case BLOCK_RETURN:
            // Non-local return handled by blockReturn()
            value := stack.pop()
            vm.blockReturn(activation.currentBlock, value)

        case CREATE_BLOCK:
            methodIdx := binary.LittleEndian.Uint16(bc[ip:])
            ip += 2
            nCaptures := int(bc[ip])
            ip++
            captures := stack.popN(nCaptures)
            block := vm.createBlock(method.Blocks[methodIdx], captures, activation)
            stack.push(block)

        // ... remaining opcodes
        }
    }
}
```

### Send Dispatch

```go
func (vm *VM) send(receiver Value, selector int, args []Value) Value {
    // Check breakpoints if debugging
    if vm.debugServer != nil && vm.debugServer.active {
        vm.debugServer.checkBreakpoint(receiver, selector)
    }

    var vt *VTable

    // Get vtable based on value type (NaN-boxing aware)
    switch {
    case isFloat(receiver):
        vt = vm.floatVTable
    case isSmallInt(receiver):
        vt = vm.integerVTable
    case isSymbol(receiver):
        vt = vm.symbolVTable
    case receiver == valueTrue || receiver == valueFalse:
        vt = vm.booleanVTable
    case receiver == valueNil:
        vt = vm.undefinedObjectVTable
    case isObject(receiver):
        vt = toObject(receiver).vtable
    default:
        panic("unknown value type")
    }

    // Lookup method
    method := vt.lookup(selector)
    if method == nil {
        return vm.doesNotUnderstand(receiver, selector, args)
    }

    return method.invoke(vm, receiver, args)
}

func (vm *VM) doesNotUnderstand(receiver Value, selector int, args []Value) Value {
    // Create Message object and send #doesNotUnderstand:
    message := vm.createMessage(selector, args)
    return vm.send(receiver, selectorDoesNotUnderstand, []Value{message})
}
```

### Registry Garbage Collection

The VM includes a `RegistryGC` that periodically sweeps concurrency registries (channels, processes, cancellation contexts, exceptions) to reclaim entries for completed or closed objects. This prevents memory leaks in long-running programs.

- **Default interval**: 30 seconds (`DefaultGCInterval`)
- **Swept registries**: channels (closed), processes (terminated), cancellation contexts (cancelled), handled exceptions
- **Lifecycle**: Starts automatically with the VM; stopped by `VM.Shutdown()`
- **Manual sweep**: `registryGC.SweepNow()` for immediate collection

### Tail-Call Optimization

The compiler detects self-recursive tail calls — methods ending with `^self selector: args` where the selector matches the current method — and emits `OpTailSend` (0x32) instead of `OpSend`.

At runtime, `OpTailSend` checks if the send target is `self` and the selector matches the current method. If so, it reuses the current call frame (updating arguments in place and resetting the instruction pointer) rather than pushing a new frame. This allows unbounded self-recursion without stack overflow.

For non-self sends or different selectors, `OpTailSend` falls back to a normal send.

---

## Image Format

The image is a binary snapshot of the entire object memory, enabling save/restore of live system state.

### Image Structure

```
┌─────────────────────────────────────────┐
│ Header                                  │
│   Magic number (MAGI)                   │
│   Version                               │
│   Flags                                 │
│   Object count                          │
│   String table offset                   │
│   Class table offset                    │
│   Entry point (main method)             │
├─────────────────────────────────────────┤
│ String Table                            │
│   Count                                 │
│   [length:32 | utf8 bytes]...           │
├─────────────────────────────────────────┤
│ Symbol Table                            │
│   Count                                 │
│   [string-index:32]...                  │
├─────────────────────────────────────────┤
│ Selector Table                          │
│   Count                                 │
│   [symbol-index:32]...                  │
├─────────────────────────────────────────┤
│ Namespace Table                         │
│   Count                                 │
│   [name: string-index | class-count     │
│    | class-indices...]...               │
├─────────────────────────────────────────┤
│ Class Definitions                       │
│   Count                                 │
│   [                                     │
│     name: string-index                  │
│     namespace: namespace-index          │
│     superclass: class-index             │
│     instVarNames: [string-index]...     │
│     classVarNames: [string-index]...    │
│     methods: [method-def]...            │
│   ]...                                  │
├─────────────────────────────────────────┤
│ Compiled Methods                        │
│   Count                                 │
│   [                                     │
│     selector: selector-index            │
│     class: class-index                  │
│     arity, numTemps                     │
│     literals: [value]...                │
│     bytecode: [byte]...                 │
│     blocks: [block-method]...           │
│     sourceMap: [source-loc]...          │
│   ]...                                  │
├─────────────────────────────────────────┤
│ Objects                                 │
│   Count                                 │
│   [                                     │
│     class: class-index                  │
│     slots: [value]...                   │
│   ]...                                  │
├─────────────────────────────────────────┤
│ Globals                                 │
│   Count                                 │
│   [name: string-index | value]...       │
└─────────────────────────────────────────┘
```

### Value Encoding in Image

```go
// Value encoding for image serialization
// High 4 bits indicate type, remaining bits are payload

const (
    imageTagFloat    = 0x0  // 64-bit float (stored directly)
    imageTagSmallInt = 0x1  // 48-bit signed integer
    imageTagObject   = 0x2  // object index
    imageTagNil      = 0x3
    imageTagTrue     = 0x4
    imageTagFalse    = 0x5
    imageTagSymbol   = 0x6  // symbol table index
    imageTagString   = 0x7  // string table index (for literal strings)
    imageTagClass    = 0x8  // class table index
    imageTagMethod   = 0x9  // method table index
)
```

### Image Load/Save

```go
func (vm *VM) saveImage(path string) error {
    w := newImageWriter(path)

    // Traverse all reachable objects
    objects := vm.collectAllObjects()

    // Write header
    w.writeHeader(len(objects))

    // Write tables
    w.writeStringTable(vm.strings)
    w.writeSymbolTable(vm.symbols)
    w.writeSelectorTable(vm.selectorTable)
    w.writeNamespaceTable(vm.namespaces)

    // Write classes
    w.writeClasses(vm.allClasses())

    // Write methods
    w.writeMethods(vm.allMethods())

    // Write objects
    for _, obj := range objects {
        w.writeObject(obj)
    }

    // Write globals
    w.writeGlobals(vm.globals)

    return w.finish()
}

func (vm *VM) loadImage(path string) error {
    r := newImageReader(path)

    // Read and validate header
    header := r.readHeader()
    if header.magic != "MAGI" {
        return errors.New("invalid image format")
    }

    // Read tables
    vm.strings = r.readStringTable()
    vm.symbols = r.readSymbolTable()
    vm.selectorTable = r.readSelectorTable()
    vm.namespaces = r.readNamespaceTable()

    // Read classes (creates vtables)
    r.readClasses(vm)

    // Read methods (populates vtables)
    r.readMethods(vm)

    // Read objects
    objects := r.readObjects(vm)

    // Read globals
    vm.globals = r.readGlobals(objects)

    return nil
}
```

---

## AOT Compilation

For deployment, bytecode can be compiled to Go source, then built as a standalone binary.

### AOT Compiler Structure

```go
type AOTCompiler struct {
    vm       *VM
    output   *bytes.Buffer
    methods  map[*CompiledMethod]string  // method → Go function name
}

func (c *AOTCompiler) compile(image *Image) string {
    c.emitPrelude()
    c.emitValueTypes()
    c.emitVTables()
    c.emitPrimitives()

    for _, method := range image.methods {
        c.compileMethod(method)
    }

    c.emitInitialization()
    c.emitMain()

    return c.output.String()
}
```

### Method Compilation

Bytecode is translated to Go:

```go
func (c *AOTCompiler) compileMethod(m *CompiledMethod) {
    funcName := c.methodName(m)

    c.emit("func %s(vm *VM, receiver Value, args []Value) Value {\n", funcName)

    // Declare temps
    for i := 0; i < m.NumTemps; i++ {
        c.emit("    var temp%d Value\n", i)
    }

    // Copy args to temps
    for i := 0; i < m.Arity; i++ {
        c.emit("    temp%d = args[%d]\n", i, i)
    }

    // Compile bytecode to Go
    c.compileBytecode(m)

    c.emit("}\n\n")
}
```

### Generated Code Example

Smalltalk:
```smalltalk
increment
    value := value + 1.
    ^value
```

Generated Go:
```go
func Counter_increment(vm *VM, receiver Value, args []Value) Value {
    obj := toObject(receiver)
    t0 := obj.getSlot(0)  // value
    var t1 Value
    if isSmallInt(t0) {
        t1 = fromSmallInt(toSmallInt(t0) + 1)
    } else if isFloat(t0) {
        t1 = fromFloat(toFloat(t0) + 1.0)
    } else {
        t1 = vm.send(t0, selectorPlus, []Value{fromSmallInt(1)})
    }
    obj.setSlot(0, t1)
    return t1
}
```

### AOT Coverage

The AOT compiler handles the full bytecode instruction set, including:

- `OpSendSuper` — generates `vm.SendSuper()` calls for super sends
- `OpCreateBlock` — generates block closure creation with captured variables
- `OpTailSend` — generates tail-call optimized dispatch

---

## Debugging Architecture

Maggie supports remote debugging via a debug server that IDEs (Yutani, Emacs) connect to.

### Debug Server

```go
type DebugServer struct {
    vm          *VM
    listener    net.Listener
    clients     []*DebugClient
    active      bool

    breakpoints map[breakpointKey]bool
    pauseChan   chan pauseRequest
    resumeChan  chan struct{}

    mu          sync.Mutex
}

type breakpointKey struct {
    method *CompiledMethod
    offset int
}

type pauseRequest struct {
    goroutineID uint64
    activation  *ActivationRecord
    reason      string
}
```

### Debug Protocol

Wire protocol over TCP/WebSocket (JSON-based for simplicity):

```go
// Commands (client → server)
type SetBreakpointCmd struct {
    Type     string `json:"type"`  // "setBreakpoint"
    Class    string `json:"class"`
    Method   string `json:"method"`
    Line     int    `json:"line"`
}

type ContinueCmd struct {
    Type string `json:"type"`  // "continue"
}

type StepOverCmd struct {
    Type string `json:"type"`  // "stepOver"
}

type EvalCmd struct {
    Type       string `json:"type"`  // "eval"
    Expression string `json:"expression"`
    FrameID    int    `json:"frameId"`
}

type InspectCmd struct {
    Type     string `json:"type"`  // "inspect"
    ObjectID int    `json:"objectId"`
}

// Events (server → client)
type StoppedEvent struct {
    Type        string `json:"type"`  // "stopped"
    Reason      string `json:"reason"`  // "breakpoint", "step", "pause"
    GoroutineID uint64 `json:"goroutineId"`
    Location    SourceLoc `json:"location"`
}

type OutputEvent struct {
    Type    string `json:"type"`  // "output"
    Content string `json:"content"`
}
```

### Breakpoint Implementation

```go
func (vm *VM) checkBreakpoint(activation *ActivationRecord, ip int) {
    key := breakpointKey{activation.method, ip}

    if vm.debugServer.breakpoints[key] {
        vm.debugServer.pause(pauseRequest{
            goroutineID: vm.currentGoroutineID(),
            activation:  activation,
            reason:      "breakpoint",
        })
    }
}

func (ds *DebugServer) pause(req pauseRequest) {
    ds.pauseChan <- req

    // Notify clients
    ds.broadcast(StoppedEvent{
        Type:        "stopped",
        Reason:      req.reason,
        GoroutineID: req.goroutineID,
        Location:    req.activation.currentLocation(),
    })

    // Wait for resume
    <-ds.resumeChan
}
```

### REPL at Breakpoint

```go
func (ds *DebugServer) handleEval(cmd EvalCmd) (Value, error) {
    // Get the paused activation
    activation := ds.pausedActivations[cmd.FrameID]
    if activation == nil {
        return nil, errors.New("invalid frame")
    }

    // Parse expression
    ast, err := ds.vm.parse(cmd.Expression)
    if err != nil {
        return nil, err
    }

    // Compile to temporary method
    method := ds.vm.compileExpression(ast)

    // Execute in context of paused activation
    return ds.vm.evalInContext(method, activation)
}

func (vm *VM) evalInContext(method *CompiledMethod, activation *ActivationRecord) Value {
    // Create a child activation that can see parent's temps
    childActivation := &ActivationRecord{
        method: method,
        parent: activation,  // for variable lookup
    }

    return vm.interpretWithContext(childActivation, activation.receiver)
}
```

### Client Integration Points

**Yutani IDE**: Native Maggie debugging UI
- Breakpoint gutter in code editor
- Variable inspector panel
- Call stack view
- REPL panel at bottom

**Emacs**: Via `maggie-mode` package
- `maggie-set-breakpoint` (click in fringe or `C-c C-b`)
- `maggie-debug` to start debug session
- Integration with `realgud` or custom `maggie-debug-mode`

---

## Go Interop

Maggie provides mechanisms to call Go code and wrap Go types.

### Calling Go from Maggie

Primitives are Go functions registered in the VM:

```go
func (vm *VM) registerPrimitive(class, selector string, fn Method) {
    cls := vm.classes[class]
    sel := vm.selectorTable.intern(selector)
    cls.vtable.methods[sel] = fn
}

// Example: File class
vm.registerPrimitive("File", "readContents", fileReadContents)
vm.registerPrimitive("File", "writeContents:", fileWriteContents)

func fileReadContents(vm *VM, receiver Value, args []Value) Value {
    file := toFile(toObject(receiver))
    contents, err := os.ReadFile(file.path)
    if err != nil {
        // Return a Failure result
        return vm.newFailure(err.Error())
    }
    // Return a Success result
    return vm.newSuccess(vm.newString(string(contents)))
}
```

### Wrapping Go Types

Go values can be wrapped as Maggie objects:

```go
type GoWrapper struct {
    vtable   *VTable
    goValue  interface{}
    goType   reflect.Type
}

func (vm *VM) wrapGoValue(v interface{}) Value {
    wrapper := &GoWrapper{
        vtable:  vm.goWrapperVTable,
        goValue: v,
        goType:  reflect.TypeOf(v),
    }
    return fromObject(wrapper)
}
```

Method calls on wrapped objects use reflection:

```go
func goWrapperDoesNotUnderstand(vm *VM, receiver Value, args []Value) Value {
    wrapper := toGoWrapper(toObject(receiver))
    selector := vm.selectorTable.name(args[0])
    msgArgs := toArray(args[1])

    // Convert selector to Go method name
    methodName := selectorToGoName(selector)  // "doSomething:" → "DoSomething"

    // Look up method via reflection
    rv := reflect.ValueOf(wrapper.goValue)
    method := rv.MethodByName(methodName)
    if !method.IsValid() {
        return vm.superDoesNotUnderstand(receiver, args)
    }

    // Convert arguments and call
    goArgs := convertToGoArgs(method.Type(), msgArgs)
    results := method.Call(goArgs)

    return convertFromGoResults(results)
}
```

---

## Dual-Compiler Architecture

Maggie employs a dual-compiler architecture: a Go compiler for bootstrapping and AOT compilation, and a self-hosting Maggie compiler for live development.

### Compiler Implementations

| Location | Language | Purpose |
|----------|----------|---------|
| `compiler/` | Go | Bootstrap, AOT compilation, fallback |
| `lib/compiler/` | Maggie | Live development, hot reload, REPL |

### CompilerBackend Interface

The `CompilerBackend` interface (defined in `vm/compiler_dispatch.go`) allows runtime switching between compilers:

```go
type CompilerBackend interface {
    // Compile compiles a method source string for the given class.
    Compile(source string, class *Class) (*CompiledMethod, error)

    // CompileExpression compiles a single expression.
    CompileExpression(source string) (*CompiledMethod, error)

    // Name returns the name of this compiler backend.
    Name() string
}
```

Two implementations are provided:
- **GoCompilerBackend**: Wraps the Go compiler in `compiler/`
- **MaggieCompilerBackend**: Invokes the Maggie `Compiler` class, with Go fallback

### Use Cases

| Use Case | Recommended Compiler | Rationale |
|----------|---------------------|-----------|
| Bootstrap | Go | No Maggie compiler available yet |
| AOT compilation | Go | Produces standalone binaries |
| Image loading | Go | Parse class definitions at startup |
| REPL | Maggie | Live, interactive development |
| Hot reload | Maggie | Modify running code |
| Method recompilation | Maggie | IDE-driven development |

### Switching Compilers

```go
// At startup (bootstrap)
vm.UseGoCompiler(compiler.Compile)

// After loading Maggie compiler classes
vm.UseMaggieCompiler()

// Check current compiler
fmt.Println(vm.CompilerName())  // "Go" or "Maggie"
```

### Rationale

Keeping the Go compiler permanently (rather than removing it after self-hosting) provides:

1. **Fast bootstrap**: Go compiler is always available, no chicken-and-egg problem
2. **AOT compilation**: Generate optimized standalone binaries for deployment
3. **Fallback safety**: If Maggie compiler has bugs, Go compiler still works
4. **Testing**: Compare outputs between compilers for correctness validation

---

## Bootstrap Path

### Phase 1: Minimal VM

1. Implement Value representation (NaN-boxing)
2. Implement Object layout and VTables
3. Implement bytecode interpreter (core opcodes)
4. Implement primitive classes: Object, Integer, Float, String, Array, Block
5. Bootstrap from handwritten bytecode or simple DSL

### Phase 2: Compiler in Go

1. Implement parser (Smalltalk syntax → AST)
2. Implement compiler (AST → bytecode)
3. Compile basic classes from source
4. Add Channel, Process primitives for concurrency
5. Implement Result/Success/Failure classes

### Phase 3: Image System

1. Implement image save/load
2. Add development tools (Inspector, Debugger)
3. Create initial development image

### Phase 4: Self-Hosting Compiler

1. Write Maggie compiler in Maggie
2. Bootstrap: compile compiler with Go compiler, then compile compiler with itself
3. Remove Go compiler (optional - keep for AOT)

### Phase 5: IDE and Tooling

1. Implement debug server
2. Build Yutani IDE in Maggie
3. Create Emacs integration
4. AOT compiler for deployment

---

## Summary

Maggie is a Smalltalk dialect that combines:

- **Standard Smalltalk syntax**: Familiar, proven, well-documented
- **Piumarta-inspired object model**: Everything is an object, vtable dispatch
- **NaN-boxing**: Floats are unboxed for native performance
- **Class-level namespaces**: `::` separated module system with imports and directory convention
- **Project manifest**: `maggie.toml` with dependency resolution and lock files
- **fileIn/fileOut**: Runtime source loading and class-to-source export
- **Result types**: For expected failures (file not found, parse errors)
- **Exceptions**: For unexpected failures (bugs, invariant violations)
- **Rich numeric tower**: SmallInteger, LargeInteger, Float, Fraction, Decimal
- **Go runtime**: Goroutines and channels as primitives
- **Bytecode VM**: For live development experience
- **AOT compilation**: For deployable binaries
- **Remote debugging**: For IDE integration (Yutani, Emacs)

The design prioritizes pragmatism over purity: simple core, room for optimization, tight Go integration. It's a Smalltalk for building real systems, not a museum piece.

---

*Document created: 2026-01-18*
*Updated: 2026-02-02 - Tier 1-3: stack overflow protection, TCO, registry GC, AOT super/block, benchmarks, formatter*
*Based on design discussions for Maggie (forked from Procyon)*
