# Maggie Smalltalk Concurrency Analysis

> **Note**: This document was originally a design analysis. Most "proposed" features are now **fully implemented**:
> - ✅ Channels (with select statement)
> - ✅ Processes (fork, forkWith:, forkWithContext:)
> - ✅ Mutex (lock, unlock, tryLock, critical:)
> - ✅ WaitGroup (add:, done, wait, wrap:)
> - ✅ Semaphore (acquire, release, tryAcquire, critical:)
> - ✅ CancellationContext (withCancel, withTimeout:, cancel, isCancelled)
> - ✅ Channel select (select:, select:ifNone:, onReceive:, onSend:do:)
> - ✅ Non-local return handling in forked blocks
> - ✅ Memory management (sweep functions for GC integration)
>
> See `examples/concurrency.mag` for working examples and `docs/USER_GUIDE.md` for API documentation.

This document provides a comprehensive analysis of Maggie's concurrency implementation, mapping Go concurrency patterns to Maggie Smalltalk syntax, and recommending design improvements.

---

## Part 1: Go Concurrency Patterns in Maggie Syntax

### 1.1 Basic Goroutine Spawning

#### Go Code
```go
go func() {
    fmt.Println("Hello from goroutine")
}()
```

#### Maggie Syntax
```smalltalk
[ Transcript show: 'Hello from goroutine' ] fork.
```

Or using the Process class method:
```smalltalk
Process fork: [ Transcript show: 'Hello from goroutine' ].
```

#### Implementation Details

**VM Layer (`vm/concurrency.go`):**
- `Block>>fork` is registered as a primitive method in `registerProcessPrimitives()`
- Creates a `ProcessObject` with state tracking via `createProcess()`
- Spawns a native Go goroutine using `go func() { ... }()`
- Creates a new `Interpreter` for the goroutine via `vm.newInterpreter()`
- Registers the interpreter with `vm.registerInterpreter(interp)` for goroutine-local lookup

**Bytecode/Primitives:**
- No special bytecode - uses standard `OpSend` to invoke `fork`
- The primitive extracts the `BlockValue` from the block registry
- Calls `interp.ExecuteBlock()` in the new goroutine

**Go Runtime Mapping:**
```go
// From concurrency.go
go func() {
    defer func() {
        if r := recover(); r != nil {
            proc.markDone(Nil, nil)
        }
        v.unregisterInterpreter()
    }()
    interp := v.newInterpreter()
    v.registerInterpreter(interp)
    result := interp.ExecuteBlock(bv.Block, bv.Captures, nil, bv.HomeFrame, bv.HomeSelf, bv.HomeMethod)
    proc.markDone(result, nil)
}()
```

---

### 1.2 Channels (Buffered and Unbuffered)

#### Go Code
```go
// Unbuffered
ch := make(chan int)

// Buffered
ch := make(chan int, 5)

// Send
ch <- 42

// Receive
val := <-ch
```

#### Maggie Syntax
```smalltalk
"Unbuffered"
ch := Channel new.

"Buffered"
ch := Channel new: 5.

"Send"
ch send: 42.

"Receive"
val := ch receive.
```

#### Implementation Details

**Data Structure (`vm/concurrency.go`):**
```go
type ChannelObject struct {
    vtable *VTable
    ch     chan Value
    closed atomic.Bool
    mu     sync.Mutex  // protects close operation
}
```

**Value Encoding:**
- Channels are encoded using the symbol tag with marker `1 << 24`
- `channelToValue(id)` creates `FromSymbolID(uint32(id) | channelMarker)`
- `getChannel(v)` extracts the channel from the global `channelRegistry`

**Registry:**
```go
var channelRegistry = make(map[int]*ChannelObject)
var channelRegistryMu sync.Mutex
var nextChannelID = 1
```

**Primitive Methods:**
| Maggie Method | Go Implementation |
|--------------|------------------|
| `Channel new` | `make(chan Value)` |
| `Channel new: n` | `make(chan Value, n)` |
| `send: value` | `ch.ch <- val` |
| `receive` | `val := <-ch.ch` |
| `trySend: value` | `select { case ch.ch <- val: return True; default: return False }` |
| `tryReceive` | `select { case val := <-ch.ch: return val; default: return Nil }` |
| `close` | `close(ch.ch)` with atomic flag |
| `isClosed` | `ch.closed.Load()` |
| `size` | `len(ch.ch)` |
| `capacity` | `cap(ch.ch)` |

---

### 1.3 Select Statement (Waiting on Multiple Channels)

#### Go Code
```go
select {
case v := <-ch1:
    fmt.Println("Received from ch1:", v)
case v := <-ch2:
    fmt.Println("Received from ch2:", v)
case ch3 <- 42:
    fmt.Println("Sent to ch3")
default:
    fmt.Println("No activity")
}
```

#### Maggie Syntax (Proposed)
```smalltalk
"Currently NOT implemented in Maggie"
"Proposed syntax using a select: method"

Channel select: {
    ch1 -> [ :v | Transcript show: 'Received from ch1: ', v printString ].
    ch2 -> [ :v | Transcript show: 'Received from ch2: ', v printString ].
} ifNone: [ Transcript show: 'No activity' ].
```

Alternative polling pattern (currently possible):
```smalltalk
| v1 v2 |
v1 := ch1 tryReceive.
v1 notNil ifTrue: [ ^v1 ].
v2 := ch2 tryReceive.
v2 notNil ifTrue: [ ^v2 ].
```

#### Implementation Details

**Current State:** Maggie does NOT have a native `select` equivalent. The `tryReceive` and `trySend:` primitives use Go's `select` with `default` internally for non-blocking operations, but there's no multi-channel select.

**Proposed Implementation:**
```go
// New primitive: Channel class>>select:ifNone:
// Takes an array of (channel, handler) pairs and an optional default block
func (vm *VM) primitiveSelect(channels []Value, handlers []Value, defaultBlock Value) Value {
    cases := make([]reflect.SelectCase, len(channels))
    for i, chVal := range channels {
        ch := getChannel(chVal)
        cases[i] = reflect.SelectCase{
            Dir:  reflect.SelectRecv,
            Chan: reflect.ValueOf(ch.ch),
        }
    }
    if defaultBlock != Nil {
        cases = append(cases, reflect.SelectCase{Dir: reflect.SelectDefault})
    }

    chosen, recv, ok := reflect.Select(cases)
    // Execute appropriate handler...
}
```

---

### 1.4 Fan-Out / Fan-In

#### Go Code
```go
// Fan-out: distribute work to multiple workers
func fanOut(input <-chan int, n int) []<-chan int {
    outputs := make([]<-chan int, n)
    for i := 0; i < n; i++ {
        outputs[i] = worker(input)
    }
    return outputs
}

// Fan-in: merge multiple channels into one
func fanIn(inputs ...<-chan int) <-chan int {
    output := make(chan int)
    var wg sync.WaitGroup
    for _, ch := range inputs {
        wg.Add(1)
        go func(c <-chan int) {
            defer wg.Done()
            for v := range c {
                output <- v
            }
        }(ch)
    }
    go func() {
        wg.Wait()
        close(output)
    }()
    return output
}
```

#### Maggie Syntax
```smalltalk
"Fan-out: distribute work to multiple workers"
fanOut := [ :input :n |
    | outputs |
    outputs := Array new: n.
    1 to: n do: [ :i |
        outputs at: i put: ([
            | val |
            [ (val := input receive) notNil ] whileTrue: [
                "Process val..."
            ]
        ] fork)
    ].
    outputs
].

"Fan-in: merge multiple channels"
fanIn := [ :inputs |
    | output remaining |
    output := Channel new.
    remaining := inputs size.
    inputs do: [ :input |
        [
            | val |
            [ (val := input receive) notNil ] whileTrue: [
                output send: val
            ].
            remaining := remaining - 1.
            remaining = 0 ifTrue: [ output close ]
        ] fork
    ].
    output
].
```

#### Implementation Details

The fan-out/fan-in pattern works through composition of existing primitives:
- `Channel new` creates output channels
- `fork` spawns worker goroutines
- `receive` blocks waiting for input
- `send:` forwards to the merged output
- `close` signals completion

**Limitation:** Without a WaitGroup equivalent, Maggie uses manual counting for synchronization. See Section 1.9 for WaitGroup proposal.

---

### 1.5 Worker Pools

#### Go Code
```go
func workerPool(jobs <-chan int, results chan<- int, numWorkers int) {
    var wg sync.WaitGroup
    for i := 0; i < numWorkers; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            for job := range jobs {
                results <- process(job)
            }
        }()
    }
    wg.Wait()
    close(results)
}
```

#### Maggie Syntax
```smalltalk
workerPool := [ :jobs :results :numWorkers |
    | workers |
    workers := Array new: numWorkers.
    1 to: numWorkers do: [ :i |
        workers at: i put: ([
            | job |
            [ (job := jobs receive) notNil ] whileTrue: [
                results send: (self process: job)
            ]
        ] fork)
    ].
    "Wait for all workers to complete"
    workers do: [ :w | w wait ].
    results close
].
```

#### Implementation Details

**ProcessObject State Machine:**
```go
type ProcessState int

const (
    ProcessRunning ProcessState = iota
    ProcessSuspended
    ProcessTerminated
)

type ProcessObject struct {
    vtable    *VTable
    id        uint64
    state     atomic.Int32
    done      chan struct{}
    result    Value
    err       error
    mu        sync.Mutex
    waitGroup sync.WaitGroup
}
```

**Wait Mechanism:**
```go
func (p *ProcessObject) wait() Value {
    p.waitGroup.Wait()  // Blocks until process completes
    p.mu.Lock()
    defer p.mu.Unlock()
    return p.result
}
```

---

### 1.6 Pipeline Pattern

#### Go Code
```go
func pipeline() {
    // Stage 1: Generate numbers
    gen := func(nums ...int) <-chan int {
        out := make(chan int)
        go func() {
            for _, n := range nums {
                out <- n
            }
            close(out)
        }()
        return out
    }

    // Stage 2: Square numbers
    sq := func(in <-chan int) <-chan int {
        out := make(chan int)
        go func() {
            for n := range in {
                out <- n * n
            }
            close(out)
        }()
        return out
    }

    // Run pipeline
    for n := range sq(sq(gen(2, 3))) {
        fmt.Println(n)
    }
}
```

#### Maggie Syntax
```smalltalk
"Stage 1: Generate numbers"
gen := [ :nums |
    | out |
    out := Channel new.
    [
        nums do: [ :n | out send: n ].
        out close
    ] fork.
    out
].

"Stage 2: Square numbers"
sq := [ :input |
    | out |
    out := Channel new.
    [
        | n |
        [ (n := input receive) notNil ] whileTrue: [
            out send: n * n
        ].
        out close
    ] fork.
    out
].

"Run pipeline"
| numbers result |
numbers := gen value: #(2 3).
result := sq value: (sq value: numbers).
[ (n := result receive) notNil ] whileTrue: [
    Transcript show: n printString; cr
].
```

---

### 1.7 Context/Cancellation

#### Go Code
```go
func worker(ctx context.Context) error {
    for {
        select {
        case <-ctx.Done():
            return ctx.Err()
        default:
            // Do work...
        }
    }
}

ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
defer cancel()
go worker(ctx)
```

#### Maggie Syntax (Proposed)
```smalltalk
"Context not currently implemented"
"Proposed API:"

| ctx cancel |
ctx := Context withTimeout: 5000.  "milliseconds"
cancel := ctx cancelBlock.

[
    [ ctx isCancelled ] whileFalse: [
        "Do work..."
    ].
    ctx error ifNotNil: [ :err | ^Failure with: err ]
] forkWithContext: ctx.

"Later: cancel manually"
cancel value.
```

**Workaround with Current Implementation:**
```smalltalk
| cancelled done |
cancelled := false.
done := Channel new.

[
    [ cancelled not ] whileTrue: [
        "Do work..."
    ].
    done send: #done
] fork.

"Timeout after 5 seconds"
Process sleep: 5000.
cancelled := true.
```

#### Implementation Details

**Current State:** No context support. Cancellation must be done through shared variables or channels.

**Proposed Implementation:**
```go
type ContextObject struct {
    vtable   *VTable
    deadline time.Time
    cancel   chan struct{}
    err      error
    mu       sync.Mutex
}

func (c *ContextObject) IsCancelled() bool {
    select {
    case <-c.cancel:
        return true
    default:
        return time.Now().After(c.deadline)
    }
}
```

---

### 1.8 Mutexes and Synchronization Primitives

#### Go Code
```go
var mu sync.Mutex
var counter int

func increment() {
    mu.Lock()
    defer mu.Unlock()
    counter++
}
```

#### Maggie Syntax (Proposed)
```smalltalk
"Mutex not currently implemented"
"Proposed API:"

| mu counter |
mu := Mutex new.
counter := 0.

increment := [
    mu critical: [
        counter := counter + 1
    ]
].
```

**Workaround using Channels as Locks:**
```smalltalk
| lock counter |
lock := Channel new: 1.
lock send: true.  "Initialize the lock"
counter := 0.

increment := [
    lock receive.  "Acquire lock"
    [ counter := counter + 1 ] ensure: [ lock send: true ]  "Release lock"
].
```

#### Implementation Details

**Current State:** No Mutex class. The channel-as-semaphore pattern works but is verbose.

**Proposed Implementation:**
```go
type MutexObject struct {
    vtable *VTable
    mu     sync.Mutex
}

func (vm *VM) registerMutexPrimitives() {
    m := vm.MutexClass

    m.AddClassMethod0(vm.Selectors, "new", func(_ interface{}, recv Value) Value {
        mutex := &MutexObject{}
        return registerMutex(mutex)
    })

    m.AddMethod0(vm.Selectors, "lock", func(_ interface{}, recv Value) Value {
        mu := getMutex(recv)
        mu.mu.Lock()
        return recv
    })

    m.AddMethod0(vm.Selectors, "unlock", func(_ interface{}, recv Value) Value {
        mu := getMutex(recv)
        mu.mu.Unlock()
        return recv
    })

    m.AddMethod1(vm.Selectors, "critical:", func(vmPtr interface{}, recv Value, block Value) Value {
        v := vmPtr.(*VM)
        mu := getMutex(recv)
        mu.mu.Lock()
        defer mu.mu.Unlock()
        return v.evaluateBlock(block, nil)
    })
}
```

---

### 1.9 WaitGroups

#### Go Code
```go
var wg sync.WaitGroup

for i := 0; i < 5; i++ {
    wg.Add(1)
    go func(n int) {
        defer wg.Done()
        fmt.Println("Worker", n)
    }(i)
}

wg.Wait()
```

#### Maggie Syntax (Proposed)
```smalltalk
"WaitGroup not currently implemented"
"Proposed API:"

| wg |
wg := WaitGroup new.

1 to: 5 do: [ :i |
    wg add: 1.
    [
        [ Transcript show: 'Worker ', i printString ] ensure: [ wg done ]
    ] fork
].

wg wait.
```

**Workaround with Current Implementation:**
```smalltalk
| workers |
workers := OrderedCollection new.

1 to: 5 do: [ :i |
    workers add: ([
        Transcript show: 'Worker ', i printString
    ] fork)
].

workers do: [ :w | w wait ].
```

#### Implementation Details

**Proposed Implementation:**
```go
type WaitGroupObject struct {
    vtable *VTable
    wg     sync.WaitGroup
}

func (vm *VM) registerWaitGroupPrimitives() {
    w := vm.WaitGroupClass

    w.AddClassMethod0(vm.Selectors, "new", func(_ interface{}, recv Value) Value {
        wg := &WaitGroupObject{}
        return registerWaitGroup(wg)
    })

    w.AddMethod1(vm.Selectors, "add:", func(_ interface{}, recv Value, delta Value) Value {
        wg := getWaitGroup(recv)
        wg.wg.Add(int(delta.SmallInt()))
        return recv
    })

    w.AddMethod0(vm.Selectors, "done", func(_ interface{}, recv Value) Value {
        wg := getWaitGroup(recv)
        wg.wg.Done()
        return recv
    })

    w.AddMethod0(vm.Selectors, "wait", func(_ interface{}, recv Value) Value {
        wg := getWaitGroup(recv)
        wg.wg.Wait()
        return recv
    })
}
```

---

### 1.10 Once (Singleton Initialization)

#### Go Code
```go
var once sync.Once
var instance *Singleton

func GetInstance() *Singleton {
    once.Do(func() {
        instance = &Singleton{}
    })
    return instance
}
```

#### Maggie Syntax (Proposed)
```smalltalk
"Once not currently implemented"
"Proposed API:"

Object subclass: Singleton [
    | ClassVar: instance once |

    class method: getInstance [
        once ifNil: [ once := Once new ].
        once do: [ instance := self basicNew initialize ].
        ^instance
    ]
]
```

**Workaround using Class Variables:**
```smalltalk
Object subclass: Singleton [
    | ClassVar: instance |

    class method: getInstance [
        instance ifNil: [ instance := self basicNew initialize ].
        ^instance
    ]
]
```

Note: The workaround is NOT thread-safe. A proper Once implementation is needed.

#### Implementation Details

**Proposed Implementation:**
```go
type OnceObject struct {
    vtable *VTable
    once   sync.Once
    value  Value
}

func (vm *VM) registerOncePrimitives() {
    o := vm.OnceClass

    o.AddClassMethod0(vm.Selectors, "new", func(_ interface{}, recv Value) Value {
        once := &OnceObject{value: Nil}
        return registerOnce(once)
    })

    o.AddMethod1(vm.Selectors, "do:", func(vmPtr interface{}, recv Value, block Value) Value {
        v := vmPtr.(*VM)
        once := getOnce(recv)
        once.once.Do(func() {
            once.value = v.evaluateBlock(block, nil)
        })
        return once.value
    })
}
```

---

### 1.11 Condition Variables

#### Go Code
```go
var mu sync.Mutex
var cond = sync.NewCond(&mu)
var ready bool

// Waiting goroutine
mu.Lock()
for !ready {
    cond.Wait()
}
mu.Unlock()

// Signaling goroutine
mu.Lock()
ready = true
cond.Signal()
mu.Unlock()
```

#### Maggie Syntax (Proposed)
```smalltalk
"Condition variables not currently implemented"
"Proposed API:"

| mu cond ready |
mu := Mutex new.
cond := Condition on: mu.
ready := false.

"Waiting process"
[
    mu critical: [
        [ ready not ] whileTrue: [ cond wait ].
    ]
] fork.

"Signaling process"
mu critical: [
    ready := true.
    cond signal
].
```

---

### 1.12 Atomic Operations

#### Go Code
```go
var counter int64

atomic.AddInt64(&counter, 1)
val := atomic.LoadInt64(&counter)
atomic.StoreInt64(&counter, 0)
atomic.CompareAndSwapInt64(&counter, 10, 20)
```

#### Maggie Syntax (Proposed)
```smalltalk
"Atomic operations not currently implemented"
"Proposed API using an Atomic wrapper class:"

| counter |
counter := Atomic on: 0.

counter add: 1.        "atomic increment"
counter get.           "atomic load"
counter set: 0.        "atomic store"
counter compareAndSwap: 10 with: 20.  "CAS"
```

#### Implementation Details

**Proposed Implementation:**
```go
type AtomicObject struct {
    vtable *VTable
    value  atomic.Value  // or atomic.Int64 for integers
}

func (vm *VM) registerAtomicPrimitives() {
    a := vm.AtomicClass

    a.AddClassMethod1(vm.Selectors, "on:", func(_ interface{}, recv Value, initial Value) Value {
        atom := &AtomicObject{}
        atom.value.Store(initial)
        return registerAtomic(atom)
    })

    a.AddMethod0(vm.Selectors, "get", func(_ interface{}, recv Value) Value {
        atom := getAtomic(recv)
        return atom.value.Load().(Value)
    })

    a.AddMethod1(vm.Selectors, "set:", func(_ interface{}, recv Value, val Value) Value {
        atom := getAtomic(recv)
        atom.value.Store(val)
        return recv
    })
}
```

---

### 1.13 Rate Limiting

#### Go Code
```go
limiter := time.NewTicker(time.Millisecond * 200)
defer limiter.Stop()

for req := range requests {
    <-limiter.C  // Wait for rate limiter
    process(req)
}
```

#### Maggie Syntax (Proposed)
```smalltalk
"Rate limiting using delays"
| limiter |
limiter := RateLimiter every: 200.  "milliseconds"

requests do: [ :req |
    limiter wait.
    self process: req
].
```

**Workaround with Current Implementation:**
```smalltalk
requests do: [ :req |
    Process sleep: 200.
    self process: req
].
```

Or using a ticker channel pattern:
```smalltalk
| ticker requests |
ticker := Channel new.

"Ticker process"
[
    [ true ] whileTrue: [
        Process sleep: 200.
        ticker send: true
    ]
] fork.

"Worker"
[ (req := requests receive) notNil ] whileTrue: [
    ticker receive.  "Wait for tick"
    self process: req
].
```

---

### 1.14 Semaphores

#### Go Code
```go
sem := make(chan struct{}, maxConcurrent)

func worker() {
    sem <- struct{}{}  // Acquire
    defer func() { <-sem }()  // Release
    // Do work...
}
```

#### Maggie Syntax
```smalltalk
"Using a buffered channel as semaphore"
| sem maxConcurrent |
maxConcurrent := 5.
sem := Channel new: maxConcurrent.

worker := [
    sem send: true.  "Acquire"
    [ "Do work..." ] ensure: [ sem receive ]  "Release"
].
```

Or with proposed Semaphore class:
```smalltalk
| sem |
sem := Semaphore new: 5.

worker := [
    sem wait.      "Acquire"
    [ "Do work..." ] ensure: [ sem signal ]  "Release"
].
```

#### Implementation Details

**Proposed Semaphore Implementation:**
```go
type SemaphoreObject struct {
    vtable *VTable
    sem    chan struct{}
}

func (vm *VM) registerSemaphorePrimitives() {
    s := vm.SemaphoreClass

    s.AddClassMethod1(vm.Selectors, "new:", func(_ interface{}, recv Value, permits Value) Value {
        n := int(permits.SmallInt())
        sem := &SemaphoreObject{
            sem: make(chan struct{}, n),
        }
        // Fill with permits
        for i := 0; i < n; i++ {
            sem.sem <- struct{}{}
        }
        return registerSemaphore(sem)
    })

    s.AddMethod0(vm.Selectors, "wait", func(_ interface{}, recv Value) Value {
        sem := getSemaphore(recv)
        <-sem.sem
        return recv
    })

    s.AddMethod0(vm.Selectors, "signal", func(_ interface{}, recv Value) Value {
        sem := getSemaphore(recv)
        sem.sem <- struct{}{}
        return recv
    })
}
```

---

### 1.15 Error Handling in Concurrent Code

#### Go Code
```go
type Result struct {
    Value interface{}
    Err   error
}

func worker(jobs <-chan int, results chan<- Result) {
    for job := range jobs {
        val, err := process(job)
        results <- Result{val, err}
    }
}
```

#### Maggie Syntax
```smalltalk
"Using Success/Failure Result pattern"
| jobs results |
jobs := Channel new.
results := Channel new.

"Worker process"
[
    | job |
    [ (job := jobs receive) notNil ] whileTrue: [
        | result |
        result := [ self process: job ]
            on: Error
            do: [ :ex | Failure with: ex messageText ].
        result isFailure ifFalse: [
            result := Success with: result
        ].
        results send: result
    ]
] fork.

"Sender"
jobs send: 42.

"Receiver with error handling"
| result |
result := results receive.
result
    ifSuccess: [ :val | Transcript show: 'Got: ', val printString ]
    ifFailure: [ :err | Transcript show: 'Error: ', err printString ].
```

#### Implementation Details

**Result Pattern (`vm/result.go`):**
```go
type ResultType int

const (
    ResultSuccess ResultType = iota
    ResultFailure
)

type ResultObject struct {
    vtable     *VTable
    resultType ResultType
    value      Value
}
```

**Exception Integration (`vm/exception.go`):**
- `SignaledException` is used with Go's panic/recover
- Handlers are tracked per-interpreter in `exceptionHandlers` stack
- `on:do:` installs handlers that catch matching exception classes

---

## Part 2: Design Change Summary

### 2.1 Registry Fixes

**Current Problems:**

1. **Global Registries with Mutex Contention:**
   ```go
   var channelRegistry = make(map[int]*ChannelObject)
   var channelRegistryMu sync.Mutex

   var processRegistry = make(map[uint64]*ProcessObject)
   var processRegistryMu sync.RWMutex

   var blockRegistry = make(map[int]*BlockValue)
   // No mutex! Race condition.

   var resultRegistry = make(map[int]*ResultObject)
   var resultRegistryMu sync.Mutex
   ```

2. **Block Registry Has No Mutex:**
   ```go
   // RACE CONDITION: blockRegistry accessed without synchronization
   func (i *Interpreter) createBlockValue(block *BlockMethod, captures []Value) Value {
       id := nextBlockID
       nextBlockID++  // NOT ATOMIC
       blockRegistry[id] = &BlockValue{...}  // NO MUTEX
   ```

3. **Memory Leaks - Objects Never Removed:**
   - Channels registered but never unregistered when closed
   - Processes registered but never unregistered when terminated
   - Blocks have `releaseBlocksForFrame` but it's disabled

**Recommended Changes:**

```go
// 1. Add proper synchronization to blockRegistry
var blockRegistry = make(map[int]*BlockValue)
var blockRegistryMu sync.RWMutex
var nextBlockID int32 = 1  // Use atomic

func registerBlock(bv *BlockValue) Value {
    id := int(atomic.AddInt32(&nextBlockID, 1) - 1)
    blockRegistryMu.Lock()
    blockRegistry[id] = bv
    blockRegistryMu.Unlock()
    return FromBlockID(uint32(id))
}

func getBlock(v Value) *BlockValue {
    if !v.IsBlock() {
        return nil
    }
    blockRegistryMu.RLock()
    defer blockRegistryMu.RUnlock()
    return blockRegistry[int(v.BlockID())]
}

// 2. Add cleanup methods
func unregisterChannel(id int) {
    channelRegistryMu.Lock()
    delete(channelRegistry, id)
    channelRegistryMu.Unlock()
}

func unregisterProcess(id uint64) {
    processRegistryMu.Lock()
    delete(processRegistry, id)
    processRegistryMu.Unlock()
}

// 3. Add registry to VM instead of globals
type VM struct {
    // ... existing fields ...

    // Per-VM registries instead of globals
    channels   *ChannelRegistry
    processes  *ProcessRegistry
    blocks     *BlockRegistry
}
```

---

### 2.2 Interpreter Lookup Refactoring

**Current Problems:**

1. **Expensive Goroutine ID Lookup:**
   ```go
   func getGoroutineID() int64 {
       var buf [64]byte
       n := runtime.Stack(buf[:], false)  // EXPENSIVE!
       // Parse string to get ID
   }
   ```

2. **Fallback to Main Interpreter:**
   ```go
   func (vm *VM) currentInterpreter() *Interpreter {
       gid := getGoroutineID()
       if interp, ok := vm.interpreters.Load(gid); ok {
           return interp.(*Interpreter)
       }
       return vm.interpreter  // Dangerous fallback
   }
   ```

**Recommended Changes:**

```go
// 1. Use goroutine-local storage pattern with explicit passing
type ExecutionContext struct {
    Interpreter *Interpreter
    Process     *ProcessObject
    VM          *VM
}

// 2. Pass context explicitly through all execution paths
func (i *Interpreter) Execute(ctx *ExecutionContext, method *CompiledMethod, receiver Value, args []Value) Value {
    // Store ctx in frame for access during execution
}

// 3. Alternative: Use runtime.Goexit callback registration
// or thread-local storage via cgo if performance critical

// 4. For primitives, always receive vm interface{} and extract interpreter:
func (vm *VM) evaluateBlock(blockVal Value, args []Value) Value {
    interp := vm.currentInterpreter()
    if interp == nil {
        // Create one-shot interpreter for this call
        interp = vm.newInterpreter()
    }
    // ...
}
```

---

### 2.3 Non-Local Returns: `fork` vs `forkWithResult`

**Current Problem:**

Non-local returns (`^value`) from blocks cause panics that can escape goroutine boundaries incorrectly:

```go
type NonLocalReturn struct {
    Value     Value
    HomeFrame int  // Frame index - NOT VALID across goroutines!
}
```

When a block is forked, its `HomeFrame` reference becomes invalid in the new goroutine's interpreter.

**Recommended Design:**

```go
// 1. Two fork variants with different NLR semantics

// fork - NLRs terminate the process (like Go's goroutine behavior)
func (vm *VM) registerProcessPrimitives() {
    vm.BlockClass.AddMethod0(vm.Selectors, "fork", func(vmPtr interface{}, recv Value) Value {
        v := vmPtr.(*VM)
        bv := v.currentInterpreter().getBlockValue(recv)
        if bv == nil {
            return Nil
        }

        proc := createProcess()
        procValue := registerProcess(proc)

        go func() {
            defer func() {
                if r := recover(); r != nil {
                    if _, ok := r.(NonLocalReturn); ok {
                        // NLR escaping fork terminates process with nil
                        proc.markDone(Nil, fmt.Errorf("non-local return escaped forked process"))
                    } else {
                        proc.markDone(Nil, fmt.Errorf("%v", r))
                    }
                }
                v.unregisterInterpreter()
            }()

            interp := v.newInterpreter()
            v.registerInterpreter(interp)

            // Mark block as "detached" - NLRs will be local
            result := interp.ExecuteBlockDetached(bv.Block, bv.Captures, nil)
            proc.markDone(result, nil)
        }()

        return procValue
    })
}

// 2. ExecuteBlockDetached - treats NLRs as local returns
func (i *Interpreter) ExecuteBlockDetached(block *BlockMethod, captures []Value, args []Value) Value {
    i.pushBlockFrame(block, captures, args, -1, 0, Nil, nil)  // HomeFrame = -1 (detached)
    return i.runFrame()
}

// 3. Modify runFrame to handle detached blocks
case OpReturnTop:
    result := i.pop()
    if isBlock {
        if frame.HomeFrame == -1 {
            // Detached block - treat as local return
            i.popFrame()
            return result
        }
        // Attached block - non-local return
        i.popFrame()
        panic(NonLocalReturn{Value: result, HomeFrame: frame.HomeFrame})
    }
    // ...

// 4. forkWithResult - allows block to return value to parent
// Uses a channel to communicate result back
vm.BlockClass.AddMethod0(vm.Selectors, "forkWithResult", func(vmPtr interface{}, recv Value) Value {
    v := vmPtr.(*VM)
    // Returns a Future/Promise object that can be awaited
    // ...
})
```

---

### 2.4 Cell Synchronization

**Current Problem:**

Cells (mutable boxes for captured variables) have no synchronization:

```go
type Cell struct {
    Value Value  // NOT THREAD-SAFE
}

func (v Value) CellSet(newValue Value) {
    v.CellPtr().Value = newValue  // RACE CONDITION
}
```

When a block captures a variable by reference and is forked, multiple goroutines can access the same Cell.

**Recommended Design:**

```go
// Option 1: Atomic Value (simple, limited to Value-sized types)
type Cell struct {
    value atomic.Value
}

func (c *Cell) Get() Value {
    v := c.value.Load()
    if v == nil {
        return Nil
    }
    return v.(Value)
}

func (c *Cell) Set(newValue Value) {
    c.value.Store(newValue)
}

// Option 2: Mutex-protected (safer, allows compound operations)
type Cell struct {
    mu    sync.RWMutex
    value Value
}

func (c *Cell) Get() Value {
    c.mu.RLock()
    defer c.mu.RUnlock()
    return c.value
}

func (c *Cell) Set(newValue Value) {
    c.mu.Lock()
    defer c.mu.Unlock()
    c.value = newValue
}

func (c *Cell) Update(fn func(Value) Value) Value {
    c.mu.Lock()
    defer c.mu.Unlock()
    c.value = fn(c.value)
    return c.value
}

// Option 3: Copy-on-fork semantics
// When a block is forked, its captured cells are deep-copied
func (bv *BlockValue) DeepCopy() *BlockValue {
    newCaptures := make([]Value, len(bv.Captures))
    for i, cap := range bv.Captures {
        if cap.IsCell() {
            // Create new cell with copied value
            newCaptures[i] = NewCell(cap.CellGet())
        } else {
            newCaptures[i] = cap
        }
    }
    return &BlockValue{
        Block:      bv.Block,
        Captures:   newCaptures,
        HomeFrame:  -1,  // Detached
        HomeSelf:   bv.HomeSelf,
        HomeMethod: bv.HomeMethod,
    }
}
```

---

### 2.5 Memory Management / GC Integration

**Current Problems:**

1. **Registries Prevent GC:**
   ```go
   var channelRegistry = make(map[int]*ChannelObject)  // Keeps channels alive forever
   var blockRegistry = make(map[int]*BlockValue)      // Keeps blocks alive forever
   ```

2. **No Finalizers for Cleanup:**
   - Closed channels stay in registry
   - Terminated processes stay in registry

3. **Cell Registry Has Same Issue:**
   ```go
   var cellRegistry = make(map[*Cell]struct{})  // Never cleaned
   ```

**Recommended Design:**

```go
// 1. Integrate with existing GC in vm.go
func (vm *VM) CollectGarbage() int {
    // ... existing marking phase ...

    // Mark channels reachable from stack/globals
    vm.markChannels(marked)

    // Mark processes reachable from stack/globals
    vm.markProcesses(marked)

    // Sweep unreachable channels
    vm.sweepChannels()

    // Sweep terminated processes
    vm.sweepProcesses()

    // ... rest of sweep phase ...
}

// 2. Add weak reference support for concurrency objects
func (vm *VM) sweepChannels() {
    channelRegistryMu.Lock()
    defer channelRegistryMu.Unlock()

    for id, ch := range channelRegistry {
        // Remove if closed and no pending values
        if ch.closed.Load() && len(ch.ch) == 0 {
            delete(channelRegistry, id)
        }
    }
}

func (vm *VM) sweepProcesses() {
    processRegistryMu.Lock()
    defer processRegistryMu.Unlock()

    for id, proc := range processRegistry {
        // Remove terminated processes with no waiters
        if proc.isDone() {
            delete(processRegistry, id)
        }
    }
}

// 3. Add explicit cleanup primitives
// Channel>>finalize (called when channel becomes unreachable)
// Process>>finalize (called when process becomes unreachable)

// 4. Use WeakRegistry for blocks
func (vm *VM) NewBlockWeakRef(bv *BlockValue) Value {
    // Store block value behind weak reference
    // GC can collect when no strong refs remain
}
```

---

### 2.6 New Primitives Needed

#### Summary Table

| Class | Method | Go Backing | Priority |
|-------|--------|-----------|----------|
| **Mutex** | `new` | `sync.Mutex{}` | High |
| | `lock` | `mu.Lock()` | High |
| | `unlock` | `mu.Unlock()` | High |
| | `critical:` | Lock/defer unlock/eval | High |
| | `tryLock` | `mu.TryLock()` | Medium |
| **WaitGroup** | `new` | `sync.WaitGroup{}` | High |
| | `add:` | `wg.Add(n)` | High |
| | `done` | `wg.Done()` | High |
| | `wait` | `wg.Wait()` | High |
| **Semaphore** | `new:` | `chan struct{}` | Medium |
| | `wait` | `<-sem` | Medium |
| | `signal` | `sem <- {}` | Medium |
| | `tryWait` | select with default | Medium |
| **Once** | `new` | `sync.Once{}` | Medium |
| | `do:` | `once.Do(fn)` | Medium |
| **Atomic** | `on:` | `atomic.Value` | Medium |
| | `get` | `v.Load()` | Medium |
| | `set:` | `v.Store()` | Medium |
| | `compareAndSwap:with:` | `v.CompareAndSwap()` | Medium |
| **Condition** | `on:` | `sync.NewCond(mu)` | Low |
| | `wait` | `cond.Wait()` | Low |
| | `signal` | `cond.Signal()` | Low |
| | `broadcast` | `cond.Broadcast()` | Low |
| **Context** | `withTimeout:` | `context.WithTimeout()` | Low |
| | `withCancel` | `context.WithCancel()` | Low |
| | `isCancelled` | `<-ctx.Done()` | Low |
| | `cancel` | `cancel()` | Low |
| **Channel** | `select:` | `reflect.Select()` | Medium |
| | `select:ifNone:` | Select with default | Medium |

#### Implementation Skeleton

```go
// vm/mutex.go
package vm

import "sync"

type MutexObject struct {
    vtable *VTable
    mu     sync.Mutex
}

var mutexRegistry = make(map[int]*MutexObject)
var mutexRegistryMu sync.Mutex
var nextMutexID int32 = 1

const mutexMarker uint32 = 32 << 24

func (vm *VM) registerMutexPrimitives() {
    vm.MutexClass = vm.createClass("Mutex", vm.ObjectClass)
    vm.Globals["Mutex"] = vm.classValue(vm.MutexClass)

    m := vm.MutexClass

    m.AddClassMethod0(vm.Selectors, "new", func(_ interface{}, recv Value) Value {
        mutex := &MutexObject{}
        return registerMutex(mutex)
    })

    m.AddMethod0(vm.Selectors, "lock", func(_ interface{}, recv Value) Value {
        mu := getMutex(recv)
        if mu == nil { return Nil }
        mu.mu.Lock()
        return recv
    })

    m.AddMethod0(vm.Selectors, "unlock", func(_ interface{}, recv Value) Value {
        mu := getMutex(recv)
        if mu == nil { return Nil }
        mu.mu.Unlock()
        return recv
    })

    m.AddMethod1(vm.Selectors, "critical:", func(vmPtr interface{}, recv Value, block Value) Value {
        v := vmPtr.(*VM)
        mu := getMutex(recv)
        if mu == nil { return Nil }
        mu.mu.Lock()
        defer mu.mu.Unlock()
        return v.evaluateBlock(block, nil)
    })
}
```

---

## Part 3: Migration Path

### Phase 1: Critical Bug Fixes (Week 1)

**Order:**
1. Fix blockRegistry race condition
2. Add proper synchronization to all registries
3. Fix Cell thread-safety

**What Breaks:**
- Nothing should break - these are bug fixes

**Steps:**

```bash
# 1. Create branch
git checkout -b fix/concurrency-race-conditions

# 2. Add mutex to blockRegistry
# Edit vm/interpreter.go

# 3. Add tests for concurrent block creation
# Edit vm/concurrency_test.go

# 4. Run race detector
go test -race ./vm/...

# 5. Fix Cell synchronization
# Edit vm/value.go
```

**Code Changes:**

```go
// vm/interpreter.go - Add synchronization
var blockRegistry = make(map[int]*BlockValue)
var blockRegistryMu sync.RWMutex  // ADD THIS
var nextBlockID int32 = 1  // CHANGE TO atomic type

func (i *Interpreter) createBlockValue(block *BlockMethod, captures []Value) Value {
    id := int(atomic.AddInt32(&nextBlockID, 1))  // ATOMIC

    blockRegistryMu.Lock()  // ADD
    blockRegistry[id] = &BlockValue{...}
    blocksByHomeFrame[homeFrame] = append(blocksByHomeFrame[homeFrame], id)
    blockRegistryMu.Unlock()  // ADD

    return FromBlockID(uint32(id))
}

func (i *Interpreter) getBlockValue(v Value) *BlockValue {
    if v.IsBlock() {
        id := int(v.BlockID())
        blockRegistryMu.RLock()  // ADD
        bv := blockRegistry[id]
        blockRegistryMu.RUnlock()  // ADD
        return bv
    }
    return nil
}
```

---

### Phase 2: NLR Semantics for Fork (Week 2)

**Order:**
1. Add `ExecuteBlockDetached` method
2. Modify `fork` to use detached execution
3. Add tests for NLR behavior

**What Breaks:**
- Code relying on NLRs escaping forked blocks will now get `nil` result instead of crash
- This is actually more correct behavior

**Backwards Compatibility:**
- `fork` behavior changes but becomes safer
- Add `forkAttached` for legacy behavior if needed (not recommended)

**Steps:**

```go
// vm/interpreter.go
func (i *Interpreter) ExecuteBlockDetached(block *BlockMethod, captures []Value, args []Value) Value {
    // HomeFrame = -1 signals detached mode
    i.pushBlockFrame(block, captures, args, -1, 0, Nil, nil)
    return i.runFrame()
}

// vm/concurrency.go - Update fork
vm.BlockClass.AddMethod0(vm.Selectors, "fork", func(vmPtr interface{}, recv Value) Value {
    // ... setup ...
    go func() {
        defer func() {
            if r := recover(); r != nil {
                if nlr, ok := r.(NonLocalReturn); ok {
                    // Log warning: NLR escaped forked process
                    proc.markDone(nlr.Value, nil)  // Return the NLR value
                } else {
                    proc.markDone(Nil, fmt.Errorf("%v", r))
                }
            }
            v.unregisterInterpreter()
        }()

        interp := v.newInterpreter()
        v.registerInterpreter(interp)
        result := interp.ExecuteBlockDetached(bv.Block, bv.Captures, nil)
        proc.markDone(result, nil)
    }()
    return procValue
})
```

---

### Phase 3: Add Mutex Class (Week 3)

**Order:**
1. Create `vm/mutex.go`
2. Register MutexClass in bootstrap
3. Add primitives
4. Add Mutex.mag library file

**What Breaks:**
- Nothing - additive change

**Steps:**

```bash
# 1. Create mutex implementation
touch vm/mutex.go

# 2. Update bootstrap
# Edit vm/vm.go - add vm.MutexClass

# 3. Create Smalltalk wrapper
touch lib/Mutex.mag
```

**lib/Mutex.mag:**
```smalltalk
Mutex subclass: Object

  method: withLock: aBlock [
      "Execute aBlock while holding the lock"
      self lock.
      ^aBlock ensure: [ self unlock ]
  ]

  method: critical: aBlock [
      "Evaluate aBlock while holding the lock"
      ^self primCritical: aBlock
  ]
```

---

### Phase 4: Add WaitGroup and Semaphore (Week 4)

**Order:**
1. Create `vm/waitgroup.go`
2. Create `vm/semaphore.go`
3. Register classes in bootstrap
4. Add library files

**What Breaks:**
- Nothing - additive change

---

### Phase 5: Memory Management Integration (Week 5-6)

**Order:**
1. Move registries to VM struct
2. Add cleanup hooks to GC
3. Implement finalizers
4. Add tests

**What Breaks:**
- Global registry access patterns change
- Tests using registries directly need updates

**Backwards Compatibility:**
- Keep global accessors as wrappers initially:

```go
// Compatibility layer
func getChannel(v Value) *ChannelObject {
    // This will be called from primitives that have vmPtr
    // But for now, use global registry
    return globalChannelRegistry.Get(v)
}

// New VM-local method
func (vm *VM) getChannel(v Value) *ChannelObject {
    return vm.channels.Get(v)
}
```

---

### Phase 6: Select Statement (Week 7)

**Order:**
1. Design API
2. Implement using `reflect.Select`
3. Add tests
4. Document

**What Breaks:**
- Nothing - additive change

---

### Phase 7: Context/Cancellation (Week 8)

**Order:**
1. Design Context class
2. Implement using `context.Context`
3. Integrate with fork
4. Add tests

---

### Summary: Change Impact Matrix

| Change | Risk | Breaks Existing | Migration Difficulty |
|--------|------|-----------------|---------------------|
| Block registry mutex | Low | No | Easy |
| Cell synchronization | Low | No | Easy |
| NLR fork semantics | Medium | Behavior change | Medium |
| Mutex class | Low | No (additive) | Easy |
| WaitGroup class | Low | No (additive) | Easy |
| Move registries to VM | Medium | Test code | Medium |
| GC integration | High | Timing sensitive | Hard |
| Select statement | Medium | No (additive) | Medium |
| Context support | Medium | No (additive) | Medium |

### Testing Strategy

```bash
# Run all tests with race detector
go test -race -v ./vm/...

# Run concurrency-specific tests
go test -race -v ./vm/ -run TestChannel
go test -race -v ./vm/ -run TestProcess
go test -race -v ./vm/ -run TestConcurrent

# Stress test with many goroutines
go test -race -v ./vm/ -run TestConcurrent -count=100

# Check for goroutine leaks
go test -v ./vm/ -run TestProcess -leaktest
```

### Documentation Updates

After each phase:
1. Update CLAUDE.md with new primitives
2. Update examples/concurrency.mag
3. Add examples for new patterns
4. Update existing tests if necessary and add new tests
5. Update API documentation

---

## Appendix A: Current Registry Marker Allocation

| Marker | Hex | Class |
|--------|-----|-------|
| 1 << 24 | 0x01000000 | Channel |
| 2 << 24 | 0x02000000 | Process |
| 4 << 24 | 0x04000000 | Result |
| 8 << 24 | 0x08000000 | Exception |
| 16 << 24 | 0x10000000 | WeakReference |
| 32 << 24 | 0x20000000 | (Reserved for Mutex) |
| 64 << 24 | 0x40000000 | (Reserved for future) |

---

## Appendix B: Performance Considerations

### Current Bottlenecks

1. **getGoroutineID()** - Parses stack trace on every primitive call
2. **Global registry mutexes** - Contention under high concurrency
3. **Block creation overhead** - Allocates new captures array each time

### Optimization Opportunities

1. **Thread-local interpreter caching:**
   ```go
   // Use sync.Pool for interpreter reuse
   var interpPool = sync.Pool{
       New: func() interface{} {
           return NewInterpreter()
       },
   }
   ```

2. **Lock-free registries:**
   ```go
   // Use sync.Map for better concurrent access
   var channelRegistry sync.Map  // map[int]*ChannelObject
   ```

3. **Escape analysis for blocks:**
   ```go
   // If block doesn't escape, inline captures
   // Compiler optimization
   ```

---

## Appendix C: Related Files

| File | Purpose |
|------|---------|
| `vm/concurrency.go` | Channel and Process primitives |
| `vm/interpreter.go` | Bytecode execution, block handling |
| `vm/value.go` | NaN-boxing, Cell type |
| `vm/block_primitives.go` | Block evaluation helpers |
| `vm/exception.go` | Exception handling (uses similar panic/recover) |
| `vm/result.go` | Result pattern (Success/Failure) |
| `vm/vm.go` | VM bootstrap, goroutine-local interpreter tracking |
| `lib/Channel.mag` | Smalltalk Channel wrapper |
| `lib/Process.mag` | Smalltalk Process wrapper |
| `lib/Block.mag` | Smalltalk Block class |
| `examples/concurrency.mag` | Usage examples |
