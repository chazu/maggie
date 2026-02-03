# Top 10 Feature Ideas for Maggie

**Date:** 2026-02-02
**Status:** Brainstorm

---

## What We're Building

The 10 most impactful, pragmatic, and innovative features that would make Maggie dramatically more compelling — selected from ~100 candidates based on:
- **Impact/effort ratio** — maximum value without drowning in complexity
- **Architectural fit** — leverages what Maggie already does well (Go concurrency, NaN-boxing, image system, late binding)
- **Differentiation** — things that make Maggie stand out, not just table stakes

---

## 1. Time-Travel Debugging (Execution Recording & Replay)

### The Idea

Record method invocations, argument values, and return values as a program executes. Allow rewinding to any point in the execution trace, inspecting state, and stepping forward/backward. Think [`rr`](https://rr-project.org/) or [Replay.io](https://replay.io), but built into the language runtime itself rather than bolted onto the OS.

### Prior Art & Context

- **[rr (Record and Replay)](https://github.com/rr-debugger/rr)** — Mozilla's C/C++ debugging tool records all non-deterministic inputs (system calls, CPU effects) and replays deterministically. Achieves ~1.2x slowdown by intercepting syscalls via ptrace rather than recording every instruction. The key insight: you only need to record *non-determinism*, not every operation.
- **[Replay.io](https://replay.io)** — Brings time-travel debugging to JavaScript. Records browser sessions and lets you place "print statements" retroactively anywhere in the recording. Used by major companies to debug production issues.
- **[Undo (LiveRecorder)](https://undo.io/resources/gdb-watchpoint/time-travel-debugging-rr-debugger/)** — Commercial time-travel debugger that uses dynamic JIT binary translation rather than hardware performance counters. Recently integrated with AI coding assistants via MCP server for "agentic debugging."
- **[Pharo's Epicea](https://github.com/tinchodias/epicea)** — Records all code changes (not execution) with fine-grained recovery. Uses STON-based persistence. Demonstrates the value of recording *something* for later inspection — Maggie could record execution events similarly.
- **[Elm's Time-Travel Debugger](https://elm-lang.org/news/the-perfect-bug-report)** — For pure functional programs, records all messages flowing through the system. Since state is derived from messages, you can replay to any point. Maggie's channel messages are analogous.
- **[Smalltalk's thisContext](https://wiki.squeak.org/squeak/2138)** — Classic Smalltalk reifies the execution stack as `thisContext`, making the call stack inspectable at runtime. Maggie could extend this from "inspect current state" to "inspect any past state."

### Benefits to the Language

**Concurrent bug diagnosis.** Maggie's biggest strength — Go-style concurrency — is also its biggest debugging challenge. Race conditions, deadlocks, and message ordering bugs are non-reproducible by nature. A recording captures the exact interleaving that caused the bug, then replays it deterministically. This alone makes Maggie viable for serious concurrent systems work.

**Post-mortem debugging.** When a production system fails, the recording is the bug report. No more "steps to reproduce" — hand someone the recording file and they can step through the exact failure.

**Learning tool.** New developers can record a working session and step through it to understand how the system behaves. Combined with Maggie's live image, this creates an unmatched learning experience.

**Composability with other features.** Time-travel debugging composes with nearly every other feature in this list: process labels (#5) make recordings navigable, moldable inspectors (#4) make recorded state viewable, first-class environments (#8) let you capture and restore scope snapshots from recordings.

### Sketch

```smalltalk
"Record execution"
recording := Recorder start.
self doComplexWork.
recording stop.

"Replay and inspect"
replay := recording replay.
replay stepForward.
replay stepBack.
replay inspectAt: 42.           "Frame #42"
replay findWhere: [:frame | frame selector = #processPayment:].

"Channel replay — see exact message ordering"
replay channelEvents: myChannel. "All sends/receives in order"

"Save recording to file for sharing"
recording saveTo: '/tmp/bug-report.magrecord'.
loaded := Recorder loadFrom: '/tmp/bug-report.magrecord'.
```

### Implementation Path

- Ring buffer of `{frameID, selector, receiver-class, args-snapshot, return-value}` entries
- Toggle recording on/off at runtime (zero overhead when off)
- Since Values are 64-bit NaN-boxed, snapshotting args is just copying uint64s (objects need shallow copy of slots)
- Store as append-only log, index by frame number
- Replay = read log + reconstruct stack states
- For concurrency: record channel send/receive events with logical timestamps (Lamport clocks) to capture ordering

### Complexity: Medium

The interpreter already has clean send/return dispatch points (`OpSend`, `OpReturn`). The hard part is efficient object snapshotting, but shallow snapshots (class + slot values as NaN-boxed uint64s) suffice for inspection. Channel event recording piggybacks on existing channel primitives.

---

## 2. `become:` — Object Identity Swapping

### The Idea

Implement Smalltalk's legendary `become:` primitive, which atomically swaps the identity of two objects so that all existing references to object A now point to object B, and vice versa.

### Prior Art & Context

- **[Smalltalk-80 become:](http://www.mirandabanda.org/bluebook/bluebook_chapter26.html)** — Part of Smalltalk since the Blue Book (1983). Goldberg & Robson describe it as fundamental to the system's ability to evolve at runtime. Used internally by the system for growing objects (e.g., when an Array needs more space, allocate a bigger one and `become:` the old into the new).
- **[Squeak/Pharo becomeForward:](https://wiki.squeak.org/squeak/2950)** — One-way variant where all references to A are redirected to B, but not vice versa. More common in practice since you usually want to replace old with new, not swap. Pharo uses this extensively for class migration.
- **[Spur Memory Manager](https://clementbera.wordpress.com/2014/01/16/spurs-new-object-format/)** — Pharo's modern VM uses an object table with forwarding pointers, making `become:` an O(1) table swap. The trade-off is an extra indirection on every object access. Spur chose this because `become:` is used frequently enough in Pharo's internals to justify it.
- **[GemStone/S become:](https://gemtalksystems.com/products/gs64/)** — The distributed Smalltalk object database uses `become:` for transparent object faulting — database proxies `become:` the real object when first accessed.
- **[JavaScript Proxy](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Proxy)** — The closest equivalent in mainstream languages. Proxies can intercept operations but can't swap identity — `become:` is strictly more powerful.

### Benefits to the Language

**Live class migration.** Change a class's structure (add/remove instance variables) and migrate all existing instances without restarting the system. In a Smalltalk image that's been running for hours with thousands of live objects, this is essential. Without `become:`, you'd need to manually find and update every reference — impractical in a late-bound system.

**Transparent proxies.** Replace any object with a proxy that intercepts all messages. Use cases: lazy loading (proxy `become:` the real object on first access), remote objects (proxy forwards messages over the network), access control (proxy checks permissions before forwarding). This is the foundation for distributed Maggie.

**Object versioning.** Atomically swap an old version of an object for a new one. Combined with method versioning (#10), this gives you full live-upgrade capability at both the class and instance level.

**Growing collections.** When an Array or Dictionary needs to expand, allocate a larger one, copy contents, and `become:` the old into the new. All existing references automatically point to the larger version. No need for a separate "resize" protocol that every caller must use.

### Sketch

```smalltalk
"Swap identity of two objects"
oldObj become: newObj.

"One-way forwarding (more common)"
oldObj becomeForward: newObj.

"Live class reshape — add an instance variable"
User class addInstanceVariable: #email.
User allInstances do: [:each |
    new := User new.
    new name: each name.
    new email: 'unknown'.
    each becomeForward: new
].

"Transparent lazy-loading proxy"
proxy := LazyProxy for: [Database fetchUser: userId].
"When first message sent to proxy:"
proxy doesNotUnderstand: aMessage
  [ realObject := loader value.
    self becomeForward: realObject.
    ^realObject perform: aMessage selector withArguments: aMessage arguments ]
```

### Implementation Path

Since Maggie uses NaN-boxing with `tagObject` pointing directly to heap addresses, there are two options:
- **(a) Object table (indirection)** — Every object reference goes through a table. Swap = swap two table entries. Classic approach. Adds one pointer dereference to every object access.
- **(b) Heap scan** — Walk all reachable objects and rewrite pointers. O(live objects) per `become:` call, but zero overhead otherwise. Feasible because Maggie's Object struct has a fixed layout (vtable + 4 inline slots + overflow slice).

Option (b) is recommended: `become:` is called rarely, so paying O(n) on the rare call beats paying +1 indirection on every object access in the hot path. The heap scan needs a "visit all reachable objects" traversal — useful infrastructure that also enables `allInstances`, heap profiling, and GC statistics.

### Complexity: Medium-High

Requires implementing a reliable heap object enumeration. Doesn't change the hot path. The `becomeForward:` one-way variant is simpler (just rewrite references to A → B, no swap needed).

---

## 3. Algebraic Effects (Resumable Exceptions on Steroids)

### The Idea

Introduce effect handlers — like exceptions, but the handler can *resume* the code that raised the effect with a value. This single mechanism subsumes exceptions, async/await, generators/iterators, dependency injection, and logging.

### Prior Art & Context

- **[Eff Language](https://www.eff-lang.org/)** — Research language by Matija Pretnar and Andrej Bauer designed specifically around algebraic effects. Demonstrated that effects and handlers can replace exceptions, state, I/O, concurrency, and nondeterminism as a single unified mechanism.
- **[Koka Language (Microsoft Research)](https://koka-lang.github.io/koka/doc/index.html)** — Production-quality language with first-class algebraic effects and a novel effect type system. Compiles to C with excellent performance. Shows that effects are practical, not just theoretical.
- **[OCaml 5.0 Effect Handlers](https://v2.ocaml.org/manual/effects.html)** — OCaml added algebraic effects as the foundation of its new multicore runtime (2022). Effects replaced the old threading model. Key insight: concurrency *is* an effect.
- **[Unison Language](https://www.unison-lang.org/docs/abilities/)** — Calls effects "abilities." Every function declares what abilities it requires. Handlers provide implementations. Makes code inherently testable since you can swap handlers.
- **[Frank Language](https://arxiv.org/abs/1611.09259)** — Research language where functions are implicitly effect handlers. "Do be do be do" paper by Conor McBride and Sam Lindley.
- **[Java Virtual Threads (Project Loom)](https://openjdk.org/jeps/444)** — While not algebraic effects per se, virtual threads solve the same "don't block the carrier thread" problem. Effects are a more general solution that also handles state, exceptions, and DI.
- **Smalltalk's `on:do:` + `resume:`** — Classic Smalltalk already has resumable exceptions! The `ExceptionHandler` can call `resume:` to return a value to the signaling code. Maggie's effects would generalize this existing mechanism.

### Benefits to the Language

**Testability without mocks.** Effects separate *what* code does from *how* it does it. A method that performs `Log perform: msg` doesn't know or care whether the log goes to a file, a network, or `/dev/null`. The handler decides. In tests, you inject a silent handler. No mock frameworks, no dependency injection containers — just handler composition.

**Composable side effects.** Effects compose naturally. A handler for `Log` can wrap a handler for `Database` which wraps a handler for `Http`. Each layer handles its concern independently. This is the monad transformer stack from Haskell, but usable by humans.

**Generators for free.** A `Yield` effect gives you Python-style generators: `Yield perform: value` suspends execution and delivers a value to the consumer. The handler decides whether to continue or stop. No special coroutine syntax needed.

**Concurrency as an effect.** Fork and channel operations can be modeled as effects. This means you can test concurrent code with a deterministic single-threaded handler that controls scheduling. Combined with time-travel debugging (#1), this is extremely powerful for concurrent system verification.

### Sketch

```smalltalk
"Define effects"
Effect subclass: #Log.
Effect subclass: #Ask.
Effect subclass: #Yield.

"Perform an effect (like 'raise' but resumable)"
method: processOrder: order
  [
    Log perform: 'Processing order: ', order id.
    approved := Ask perform: 'Approve order #', order id, '?'.
    approved ifTrue: [order ship]
  ]

"Handle effects — caller decides behavior"
self processOrder: myOrder
  handleEffect: Log with: [:msg | logger write: msg. msg resume]
  handleEffect: Ask with: [:question | true resume].

"In tests — inject mock behavior"
self processOrder: testOrder
  handleEffect: Log with: [:msg | msg resume]  "Swallow logs"
  handleEffect: Ask with: [:q | false resume].  "Always reject"

"Generator via Yield effect"
method: fibonacci
  [ a := 0. b := 1.
    [true] whileTrue: [
      Yield perform: a.
      temp := b. b := a + b. a := temp
    ]
  ]

self fibonacci
  handleEffect: Yield with: [:val |
    val value < 100
      ifTrue: [results add: val value. val resume]
      ifFalse: [nil]  "Stop iteration"
  ].
```

### Implementation Path

- Effects are resumable exceptions — the handler receives a continuation (the frozen state of the raiser)
- Extend the existing exception mechanism: add a `resume:` capability to effect handler frames
- `perform:` = unwind stack to nearest matching handler (just like `signal`), but save the unwound frames
- `resume:` = restore the saved frames and push the resume value onto the operand stack
- Use existing block/closure mechanism for handler bodies
- The VM already tracks exception handler frames — adding a "resumable" flag is straightforward

### Complexity: Medium

A generalization of the existing exception mechanism. The core insight: Smalltalk's `resume:` on exceptions already does 90% of this. The new part is making it a first-class design pattern rather than an obscure exception trick.

---

## 4. Moldable Inspectors (Custom Per-Class Views)

### The Idea

Let every class define how it wants to be displayed when inspected. Instead of the generic "here are your slots" view, a Dictionary shows a key-value table, an Array shows an indexed list, a CompiledMethod shows formatted bytecode with source maps, and a Process shows its call stack with local variables.

### Prior Art & Context

- **[Glamorous Toolkit (GT)](https://gtoolkit.com/)** — The flagship implementation of moldable development, built on Pharo. Ships with thousands of contextual "views" for different object types. A `GitCommit` shows its diff, a `JSON` object shows a tree view, an `HTTP response` shows headers and body separately. The core philosophy: [the environment should adapt to your data](https://moldabledevelopment.com/).
- **[Tudor Girba's "Moldable Development" paper](https://tudorgirba.com/blog/moldable-development)** — Argues that developers spend ~50% of their time reading/understanding code, and that custom tools tailored to specific problems reduce this dramatically. Every object carries the knowledge of how it should be presented.
- **[Pharo's Inspector Framework](https://pharo.org/)** — Pharo's inspector uses a tab-based approach where each tab is a "presentation" defined by the object's class. The `gtInspectorPresentationsIn:` protocol lets any class add custom tabs.
- **[Jupyter Notebooks `_repr_html_`](https://ipython.readthedocs.io/en/stable/config/integrating.html)** — Python objects can define `_repr_html_()`, `_repr_json_()`, etc. for rich display in notebooks. Pandas DataFrames show as HTML tables, Matplotlib figures render inline. This is moldable inspection for Python.
- **[Swift Playgrounds](https://developer.apple.com/swift-playgrounds/)** — Objects conforming to `CustomPlaygroundDisplayConvertible` get rich inline visualization. Shows the value of per-type rendering even outside a full IDE.

### Benefits to the Language

**The inspector becomes the IDE.** Instead of building separate tools for different data types, you build one inspector that delegates to objects. A web response shows its status code, headers, and body. A channel shows its buffer contents and connected processes. A running benchmark shows a live chart. The development environment becomes infinitely extensible by users writing Maggie code.

**Debugging goes from "read printString" to "see what's happening."** The difference between `#(1 2 3 4 5)` appearing as text vs. appearing as a visual list with indices is the difference between debugging being painful and being pleasant. For complex objects (nested dictionaries, process trees, bytecode), the gap is even wider.

**Composability with Yutani.** Maggie already has a rich TUI widget system. Inspector views are just Yutani widgets. This means every widget you build for your application can also be an inspector view, and vice versa. The MaggieDesktop IDE and user applications share the same visual vocabulary.

**Self-documenting objects.** When you inspect a class and see a "Methods" tab, an "Hierarchy" tab, and an "Examples" tab (pulling from doctest assertions), the class *shows you how to use it*. Documentation becomes embedded in the object's self-presentation.

### Sketch

```smalltalk
"Protocol: any class can define inspector views"
Dictionary method: inspectorViews
  [
    ^{
      'Table' -> [:dict |
        YutaniTable columns: #(Key Value)
                   rows: (dict associations collect: [:a | {a key. a value}])
      ].
      'Size' -> [:dict | YutaniLabel text: 'Entries: ', dict size printString]
    }
  ]

CompiledMethod method: inspectorViews
  [
    ^{
      'Source' -> [:m | YutaniTextArea text: m sourceText].
      'Bytecode' -> [:m | YutaniList items: m disassemble].
      'Senders' -> [:m | YutaniList items: (Compiler sendersOf: m selector)]
    }
  ]

Process method: inspectorViews
  [
    ^{
      'Stack' -> [:proc | YutaniList items: proc callStack].
      'State' -> [:proc | YutaniLabel text: proc stateDescription].
      'Channels' -> [:proc |
        YutaniList items: (proc connectedChannels collect: [:ch | ch label])
      ]
    }
  ]
```

### Implementation Path

- Define `inspectorViews` protocol on Object (default: show instance variable names and values)
- The existing Inspector component checks for this method and renders each entry as a tab
- Views return Yutani widgets (or nil to hide that view)
- Ship with rich default views for: Array, Dictionary, String, CompiledMethod, Class, Process, Channel, Block, Error, Result

### Complexity: Low-Medium

The protocol is ~10 lines. The real work is writing good default views for core classes — but that's fun Maggie programming, not VM hacking.

---

## 5. Process Labels + Live Process Dashboard

### The Idea

Add named labels to processes, track parent-child relationships and blocking state, and provide a live dashboard showing all running processes, their states, channel connections, and message flow.

### Prior Art & Context

- **[Erlang/OTP Observer](https://www.erlang.org/doc/man/observer.html)** — The gold standard for concurrent system visibility. Shows all processes in a table (pid, name, message queue length, reductions, memory), plus a supervision tree view, an ETS table browser, and a system load chart. Every serious Erlang developer keeps Observer open.
- **[BEAM Process Labels (OTP 27, 2024)](https://nathanmlong.com/2024/03/beam-process-labels/)** — Added human-readable labels to processes. Before labels, processes were identified only by PIDs (`<0.123.0>`), making debugging a guessing game. Labels let you write `Process.set_label(:payment_processor)` and see meaningful names in Observer.
- **[Go pprof + goroutine dumps](https://pkg.go.dev/net/http/pprof)** — Go's built-in profiler shows goroutine stacks, but lacks labels, parent-child tracking, and channel topology. Maggie can do much better since it controls the concurrency primitives.
- **[Tokio Console (Rust)](https://github.com/tokio-rs/console)** — Real-time diagnostic tool for async Rust programs. Shows tasks, resources they're waiting on, and timing. Demonstrates the value of language-runtime-aware observability vs. generic OS tools.
- **[Java Flight Recorder / JFR](https://openjdk.org/jeps/328)** — JVM's built-in event recording system with near-zero overhead. Continuous profiling with structured events. Shows that production observability can be always-on without performance penalty.
- **[DeployEx (Elixir, 2024)](https://github.com/thiagoesteves/deployex)** — Observer for distributed Elixir applications with safe tracing. Shows that process observability tools need to work across nodes, not just within a single VM.

### Benefits to the Language

**Concurrency goes from "hope it works" to "see it working."** The #1 pain point in concurrent programming is invisible state. You fork 10 processes, they communicate over channels, and when something goes wrong you have no idea which process is stuck or what message it's waiting for. Labels + a dashboard make the system's concurrent structure visible.

**Deadlock detection.** If the dashboard shows a cycle in the "blocked on" graph (Process A is waiting on Channel X, which is waiting for Process B, which is waiting on Channel Y, which is waiting for Process A), you've found your deadlock. Visually.

**Resource leak detection.** Processes that have been alive for unexpectedly long, or channels that have accumulated messages without consumers, are immediately visible. Combined with structured concurrency (#9), leaked processes become a dashboard warning rather than a silent memory leak.

**Production monitoring.** The dashboard isn't just a development tool. Expose it via the existing gRPC/Connect server and you have production process monitoring. Watch message queue depths, track process lifetimes, alert on stuck processes.

### Sketch

```smalltalk
"Label processes"
worker := [self processQueue] forkWithLabel: 'Queue Worker #1'.
listener := [self listenForConnections] forkWithLabel: 'HTTP Listener'.

"Live dashboard"
ProcessDashboard open.

"Programmatic access"
Process all.                        "All living processes"
Process labeled: 'Queue Worker*'.   "Glob match on label"
Process blocked.                    "Processes waiting on channels/mutexes"
Process tree.                       "Parent-child tree"

"Channel topology"
Channel connections.                "Graph: process → channel → process"

"Blocking info"
proc blockingInfo.                  "→ 'Waiting on Channel #recv (label: orders)'"
```

### Implementation Path

- Add `label` string field to the Process Go struct (optional, empty by default)
- Add `forkWithLabel:` primitive that sets the label at creation time
- Track parent process ID (who forked whom) in the process registry
- Track blocked-on resource (channel ID, mutex ID) by updating the registry on each blocking operation
- Dashboard = Yutani widget that reads the process registry on a timer (100ms refresh)
- Render as: sortable table (columns: Label, State, Blocked On, CPU, Memory, Age) and as a graph (process → channel → process edges)
- Expose via existing gRPC InspectService for remote access

### Complexity: Low-Medium

The process registry already exists for GC purposes. Adding label, parent, and blocked-on fields is straightforward. The dashboard is UI work on existing Yutani widgets.

---

## 6. Pattern Matching with Destructuring

### The Idea

Add first-class pattern matching that works on any object — match by class, by slot values, by structure, with variable binding and guards. Not a type system — a dispatch mechanism that makes conditional logic dramatically clearer.

### Prior Art & Context

- **[Bob Nystrom's "Pattern Matching in a Dynamic OOP Language" (Magpie)](https://journal.stuffwithstuff.com/2011/01/16/pattern-matching-in-a-dynamic-oop-language/)** — Demonstrates that patterns in a dynamic language are more powerful than in static ones because they can match on runtime structure. Patterns in Magpie work in match expressions, variable declarations, catch clauses, and function signatures — one concept, many uses.
- **[Geller, Hirschfeld, Bracha: "Pattern Matching for an Object-oriented and Dynamically Typed Programming Language"](https://www.readkong.com/page/pattern-matching-for-an-object-oriented-and-dynamically-6128298)** — Research paper on adding pattern matching to Newspeak (Smalltalk-derivative). Key insight: patterns should be *literals* that translate to messages, preserving Smalltalk's everything-is-a-message philosophy. Patterns enable "multiple dispatch" based on nested structure.
- **[rwilcox/PatternMatcher (Smalltalk library)](https://github.com/rwilcox/PatternMatcher)** — Library-level pattern matching for Smalltalk using `PMRule` objects with `whenMatches:` / `execute:` blocks. Demonstrates that patterns can be added as a library, but with awkward syntax. Language-level support would be much cleaner.
- **[Elixir Pattern Matching](https://hexdocs.pm/elixir/pattern-matching.html)** — Elixir uses pattern matching pervasively: in function heads, `case` expressions, `=` assignments, and `with` expressions. The `=` operator is a match operator, not assignment. Shows that patterns can be the *primary* dispatch mechanism, not an add-on.
- **[Rust match](https://doc.rust-lang.org/book/ch06-02-match.html)** — Exhaustive pattern matching with the compiler checking that all cases are covered. While Maggie can't enforce exhaustiveness at compile time (dynamic language), it can warn at runtime when no pattern matches.
- **[Scala 3 Pattern Matching](https://docs.scala-lang.org/scala3/reference/changed-features/pattern-matching.html)** — Scala's extractors let any class define how it participates in pattern matching via `unapply`. This is the OOP-friendly approach: objects control their own destructuring.

### Benefits to the Language

**Conditional logic becomes readable.** Compare:

```smalltalk
"Without patterns (current)"
response isKindOf: Success
  ifTrue: [response value processResult]
  ifFalse: [
    response isKindOf: Failure
      ifTrue: [self handleError: response error]
      ifFalse: [self handleUnknown: response]
  ]

"With patterns"
response match: {
    [Success :value] -> [value processResult].
    [Failure :error] -> [self handleError: error].
    [_]              -> [self handleUnknown: response]
}
```

The pattern version is shorter, flatter (no nesting), and the structure of each case is immediately visible.

**Destructuring eliminates boilerplate.** Extracting fields from objects currently requires explicit accessor calls. Patterns bind variables in one shot:

```smalltalk
point match: { [Point :x :y] -> [x + y] }
"vs."
x := point x. y := point y. x + y.
```

**Guards express intent.** `[:n | n > 0] -> [...]` directly states the condition. No separate `if` needed.

**Natural fit for Result types.** Maggie already has Success/Failure result types. Pattern matching makes them as ergonomic as Rust's `match` on `Result<T, E>`:

```smalltalk
(self parseInput: raw) match: {
    [Success :parsed] -> [self process: parsed].
    [Failure 'timeout'] -> [self retry].
    [Failure :other] -> [self log: other. nil]
}
```

### Sketch

```smalltalk
"Match on class and destructure slots"
response match: {
    [Success :value]  -> ['Got: ', value].
    [Failure :msg]    -> ['Error: ', msg].
    [nil]             -> ['Nothing']
}

"Match on literal values"
command match: {
    ['quit']          -> [self shutdown].
    ['help']          -> [self showHelp].
    ['run' :name]     -> [self runTask: name].
    [_]               -> ['Unknown command']
}

"Match on array structure"
data match: {
    [#(1 :x :y)]     -> ['Point: ', x, ',', y].
    [#(:head | :tail)]-> ['Head: ', head, ' rest: ', tail size].
    [_]               -> ['Other']
}

"Guards"
n match: {
    [:x | x > 0]     -> ['positive'].
    [:x | x < 0]     -> ['negative'].
    [0]               -> ['zero']
}
```

### Implementation Path

Two approaches:

**(a) Syntactic extension** — Add `match:` as new syntax the compiler recognizes. Patterns compile to optimized conditional jump sequences (class check → slot extraction → guard evaluation). Compiler can skip redundant class checks across arms.

**(b) Library + message protocol** — Implement `match:` as a regular keyword message. Each pattern is a block that tests and binds. Slightly less optimized but requires zero compiler changes.

Approach (a) is recommended for performance — patterns are on the hot path of program logic. The compiler can generate specialized bytecode that avoids redundant type checks and uses the inline cache for class tests.

### Complexity: Medium

Mostly compiler work. No new opcodes needed — patterns compile to existing conditional jumps + slot reads. The runtime cost is just conditional branches.

---

## 7. Reactive Slot Observers (Watchpoints)

### The Idea

Set watchpoints on object instance variable slots that fire a callback whenever the slot value changes. Any slot on any object can be observed. Zero overhead for unwatched objects.

### Prior Art & Context

- **[KVO (Key-Value Observing) in Cocoa/Swift](https://developer.apple.com/documentation/swift/using-key-value-observing-in-swift)** — Apple's framework for observing property changes on objects. Any property marked `@objc dynamic` can be observed with `observe(\.property)`. Used pervasively in macOS/iOS for UI bindings. Shows that slot observation is practical and widely useful.
- **[Vue.js Reactivity System](https://vuejs.org/guide/extras/reactivity-in-depth.html)** — Vue intercepts property getters/setters via JavaScript Proxy to automatically track dependencies and re-render when data changes. Demonstrates that reactive slot observation can be the foundation of an entire UI framework.
- **[MobX](https://mobx.js.org/README.html)** — Transparent reactive state management for JavaScript. `observable` properties automatically trigger `autorun` and `reaction` when modified. No manual subscription/unsubscription — the system tracks dependencies automatically.
- **[Smalltalk ValueHolder / Announcements](http://pharo.org/)** — Pharo uses `ValueHolder` objects that wrap a value and announce changes. The limitation: you must use the wrapper instead of a raw slot. Maggie's approach would observe raw slots directly, requiring no wrapper.
- **[GDB Hardware Watchpoints](https://sourceware.org/gdb/onlinedocs/gdb/Set-Watchpoints.html)** — GDB can set hardware watchpoints on memory addresses. When the address is written, execution stops. Extremely valuable for debugging. Maggie's software watchpoints would be similar but programmable.
- **[Dart Streams / ChangeNotifier](https://api.flutter.dev/flutter/foundation/ChangeNotifier-class.html)** — Flutter's `ChangeNotifier` requires manually calling `notifyListeners()` after changes. Maggie's approach would be automatic — the VM detects writes.

### Benefits to the Language

**Reactive UI bindings without a framework.** Connect a model slot to a UI widget in one line. When the slot changes, the widget updates. No manual event bus, no `notifyListeners()`, no re-render cycle — the connection is direct:

```smalltalk
model watchSlot: #items do: [:old :new | listWidget items: new].
```

This turns Yutani from a widget toolkit into a reactive UI system.

**Debugging without breakpoints.** "Stop whenever `account.balance` changes" is often more useful than "stop at line 42." Watchpoints let you debug data-flow bugs where you don't know *where* the corruption happens — you just know *what* value shouldn't change.

**Constraint systems.** Watchpoints are the foundation for constraint programming: "whenever X changes, recompute Y." Build spreadsheet-like dependency graphs between object slots.

**Audit trails.** Log every change to a sensitive field: `account watchSlot: #balance do: [:old :new | AuditLog record: ...]`. Zero-effort audit logging.

### Sketch

```smalltalk
"Watch a slot"
account watchSlot: #balance do: [:old :new |
    Transcript show: 'Balance changed: ', old, ' -> ', new
].

"Reactive UI binding"
model watchSlot: #items do: [:old :new | listWidget items: new].

"Conditional watchpoint"
suspiciousObject watchSlot: #state
    when: [:old :new | new = #corrupted]
    do: [:old :new | Debugger halt: 'State corrupted!'].

"Remove watchpoint"
account unwatchSlot: #balance.

"Remove all watchpoints on an object"
account unwatchAll.
```

### Implementation Path

- Intercept slot writes in the interpreter (`OpStoreIvar`)
- Check a watched-object table (hash set keyed by object pointer). If the object isn't in the table, no overhead (hash lookup miss is ~2ns)
- If watched, look up the watcher list for that slot index, invoke each callback with old and new values
- Watcher table uses weak references to objects so watched objects can still be GC'd (when the object is collected, its watchers are removed)
- Guard against infinite recursion: if a callback writes to the watched slot, skip re-notification (or cap recursion depth to 1)

### Complexity: Low-Medium

The interpreter already has `OpStoreIvar`. Adding a hash set check before the write is surgical. The callback machinery uses existing block evaluation. The only subtle part is the weak reference handling and recursion guard.

---

## 8. First-Class Environments (Reified Scope)

### The Idea

Make lexical scopes first-class objects that can be captured, passed around, serialized, and restored. An Environment contains local variable bindings and can be used to evaluate expressions "as if" you were inside that method at that point.

### Prior Art & Context

- **[Smalltalk's thisContext](https://wiki.squeak.org/squeak/2138)** — Smalltalk reifies the current execution context as `thisContext`, which is an object representing the call stack frame. You can inspect it, walk the sender chain, and modify the stack. Maggie's first-class environments would extend this from "see the current frame" to "capture, serialize, and restore any frame's bindings."
- **[Scheme's First-Class Continuations (call/cc)](https://en.wikipedia.org/wiki/Continuation)** — Scheme captures the entire future of a computation as a function. Environments are one component of a continuation. The [classic Hieb/Dybvig/Bruggeman implementation](https://www2.ccs.neu.edu/racket/pubs/stackhack4.html) shows how to represent continuations efficiently using stack segments.
- **[Racket's Serializable Continuations](https://blog.racket-lang.org/2010/10/the-two-state-solution-native-and-serializable-continuations-accord.html)** — Racket's "Two-State Solution" separates continuations into native (fast, not serializable) and serializable (slower, but can be persisted). The key insight: you don't need to serialize the *whole* continuation — just the part that crosses a serialization boundary. Maggie's approach (serialize bindings, not code) is simpler.
- **[Jupyter Notebooks / IPython](https://jupyter.org/)** — Each cell shares a persistent namespace. Variables defined in cell 1 are visible in cell 3. This is a form of reified environment — the notebook kernel maintains a dictionary of bindings that persists across evaluations. Maggie already has `Compiler evaluate:` with persistent globals; first-class environments would extend this to local scopes.
- **[Unison's Content-Addressed Code](https://www.unison-lang.org/docs/the-big-idea/)** — Unison stores code as hashes of ASTs. Because code is content-addressed, a function's "environment" is fully determined by its hash. This makes code portable across machines without dependency resolution. Maggie's serializable environments serve a similar purpose: ship a scope to another machine.
- **[Python's pickle + exec](https://docs.python.org/3/library/pickle.html)** — Python can serialize objects (pickle) and evaluate code in a given namespace (exec with `globals`/`locals` dicts). Clunky but proves the concept. Maggie's approach would be seamless.

### Benefits to the Language

**Crash forensics.** When an exception occurs, capture the environment at the crash site. Later, in the REPL, evaluate expressions *in that environment* to understand what went wrong. No need to reproduce the bug — you have the exact state.

```smalltalk
[riskyCode] on: Error do: [:ex |
    env := ex signalerContext captureEnvironment.
    "Later, in the REPL:"
    env evaluate: 'x + y'.    "See the actual values of x and y at the crash"
].
```

**Notebook-style REPL.** Each evaluation scope is a persistent, serializable environment. Close the REPL, reopen it, restore the environment, and continue where you left off. This makes Maggie's REPL as persistent as its image.

**Distributed computation.** Serialize an environment, send it to another Maggie VM over the network, and evaluate an expression there. The remote VM sees the same variable bindings. This is the foundation for distributed Maggie — not by building a complex RPC system, but by shipping scopes.

**Educational tool.** Teachers can prepare environments with specific variable bindings and share them with students. "Here's an environment where `balance` is 0 and `transactions` is this array. Write an expression to compute the total."

### Sketch

```smalltalk
"Capture current environment"
env := thisContext captureEnvironment.

"Evaluate in captured environment"
env evaluate: 'x + y'.    "Uses x, y from captured scope"

"Serialize for later or remote use"
frozen := env freeze.      "Returns byte array"
restored := Environment thaw: frozen.
restored evaluate: 'x + 1'.

"Crash forensics"
[riskyCode] on: Error do: [:ex |
    env := ex signalerContext captureEnvironment.
    Debugger openOn: env.  "Inspect all locals at crash point"
].

"Compose environments"
combined := env1 mergedWith: env2.  "env2 bindings shadow env1"
```

### Implementation Path

- An Environment is a dictionary of `{variableName -> Value}` plus a reference to the enclosing environment (lexical chain)
- `captureEnvironment` walks the current frame's temp slots + cell captures and builds the dictionary. Names come from the CompiledMethod's debug info (variable names are already stored for the debugger)
- `freeze`/`thaw` serializes the binding dictionary using the existing image serializer (NaN-boxed values are 64-bit portable words; heap objects serialize recursively)
- `evaluate:` in an environment = compile expression, but resolve free variables against the environment dictionary instead of normal scope lookup
- This requires a small compiler change: accept an "external bindings" map during compilation

### Complexity: Medium

The compiler already resolves variables against scopes. Adding an "external scope" resolution mode is the main work. Serialization leverages the existing image format.

---

## 9. Structured Concurrency (Process Groups with Lifecycle Guarantees)

### The Idea

Add `ProcessGroup` — a scope that owns child processes and guarantees cleanup. When the group exits (normally or via error), all child processes are cancelled. No leaked goroutines, ever.

### Prior Art & Context

- **[Trio (Python)](https://trio.readthedocs.io/en/stable/)** — The library that popularized structured concurrency. Nathaniel J. Smith's [seminal blog post "Notes on structured concurrency"](https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful/) argues that `go`/`fork` without a scope is the concurrent equivalent of `goto` — it creates invisible control flow that's impossible to reason about.
- **[JEP 453/462: Structured Concurrency (Java)](https://openjdk.org/jeps/462)** — Java's `StructuredTaskScope` creates a scope for virtual threads where: the scope blocks until all children finish; if one child fails, others are cancelled; child lifetimes are bounded by the parent. Promoted from incubator to preview in Java 21.
- **[Kotlin Coroutine Scopes](https://kotlinlang.org/docs/coroutines-basics.html#structured-concurrency)** — Kotlin's `coroutineScope {}` ensures all launched coroutines complete before the scope exits. Parent cancellation propagates to children. This is the structured concurrency model that millions of Android developers use daily.
- **[Go's errgroup](https://pkg.go.dev/golang.org/x/sync/errgroup)** — Go's closest thing to structured concurrency: `errgroup.Group` manages a set of goroutines and propagates the first error. But it's opt-in and doesn't prevent leaks — goroutines launched outside the group are still unmanaged.
- **[Swift's TaskGroup](https://developer.apple.com/documentation/swift/taskgroup)** — Swift's structured concurrency (introduced with Swift 5.5) enforces child task scoping at the language level. A `TaskGroup` automatically cancels remaining children when the group scope exits.

### Benefits to the Language

**Goroutine leak prevention by construction.** The #1 real-world problem with Go-style concurrency is goroutine leaks. A process blocks on a channel that nobody will ever write to, and it sits there forever, consuming memory. Structured concurrency makes this impossible: when the group exits, all children are cancelled.

**Error propagation.** When one process in a group fails, the group cancels all other children and propagates the error. No more "one process crashed silently and the others are waiting forever on a channel that will never deliver."

**Composable concurrency.** Groups nest naturally. A "fetch all user data" group contains sub-groups for "fetch profile," "fetch preferences," "fetch history." Each sub-group manages its own lifecycle. If the outer group is cancelled, all sub-groups and their children are cancelled recursively.

**Racing pattern.** `ProcessGroup race: [...]` forks multiple strategies and returns the first result, cancelling the rest. Useful for hedged requests (try cache, database, and remote API simultaneously), timeouts (race work against a sleep), and speculative execution.

**Builds on existing primitives.** This doesn't require new VM support — it's a ~200-line Maggie library composing CancellationContext + WaitGroup + Process.

### Sketch

```smalltalk
"All processes scoped to this group"
ProcessGroup do: [:group |
    group fork: [self fetchUserData].
    group fork: [self fetchOrderHistory].
    group fork: [self fetchRecommendations].
    "Block exits when ALL children complete"
    "If any child fails, all others cancelled"
].

"With timeout"
ProcessGroup withTimeout: 5000 do: [:group |
    group fork: [self slowOperation].
    "Auto-cancelled after 5 seconds if not done"
].

"First-wins racing"
result := ProcessGroup race: {
    [self fetchFromCacheServer].
    [self fetchFromDatabase].
    [self fetchFromRemoteAPI]
}.
"Returns first result, cancels the rest"

"Nested groups"
ProcessGroup do: [:outer |
    outer fork: [
        ProcessGroup do: [:inner |
            inner fork: [work1].
            inner fork: [work2].
        ]
    ].
    outer fork: [work3].
].
```

### Implementation Path

- `ProcessGroup` is a Maggie class wrapping: a `CancellationContext`, a `WaitGroup`, a list of child processes, and an error accumulator
- `group fork:` = fork block + add to waitgroup + bind process to group's cancellation context
- `do:` block waits on waitgroup at exit (all children must finish)
- On error in any child: cancel the group's context → all children receive cancellation via their context
- `race:` = fork all blocks, first to complete writes to a result channel + cancels the group
- Nested groups: child group's CancellationContext is a child of parent's (already supported by `CancellationContext withCancel`)

### Complexity: Low

All the primitives exist. This is a pure Maggie library. The only VM consideration is ensuring `fork` properly checks its CancellationContext's `isCancelled` flag at reasonable intervals (which it already does if using `forkWithContext:`).

---

## 10. Hot-Swappable Method Replacement with Versioned Dispatch

### The Idea

Replace any method on any class while the system is running, with version history and safe rollback. Running processes continue with the old version until they return; new calls use the new version.

### Prior Art & Context

- **[Erlang Hot Code Swapping](https://www.erlang.org/doc/system/code_loading.html)** — Erlang keeps two versions of each module in memory simultaneously: the "current" and the "old." Running processes continue with the old version. When a process makes an external function call, it switches to the current version. A third load purges the old version and kills any processes still running it. This "two-version" scheme has been production-proven for 30+ years in telecom systems.
- **[Smalltalk Method Compilation](http://www.mirandabanda.org/bluebook/bluebook_chapter26.html)** — Classic Smalltalk lets you edit any method at any time. The new method immediately replaces the old one in the method dictionary. No versioning — if you break something, you need to retype the old version. This is both Smalltalk's greatest strength (instant feedback) and a footgun (no undo).
- **[Pharo's Epicea Change Tracking](https://github.com/tinchodias/epicea)** — Records every code change as an event in a log file. You can browse changes, filter, and recover previous versions. Added file-out support and search filtering in 2024. Demonstrates the value of method version history.
- **[Clojure Vars and Binding](https://clojure.org/reference/vars)** — Clojure's `Var` is an indirection cell that can be rebound. All calls go through the Var, so replacing its value atomically replaces the function everywhere. Thread-local bindings allow per-thread overrides. Similar to how Maggie could use VTable entries as indirection cells.
- **[Ruby Method Redefinition + Module#prepend](https://ruby-doc.org/3.3.0/Module.html#method-i-prepend)** — Ruby allows redefining methods and wrapping them via `prepend`. The `method_added` and `method_removed` hooks notify when methods change. No version history, but demonstrates method replacement hooks.

### Benefits to the Language

**Zero-downtime development.** Change a method while 100 processes are running. They don't crash, they don't need to restart. Running invocations finish on the old code; new invocations use the new code. This is what makes Smalltalk's "live development" actually safe for concurrent systems.

**Rollback on failure.** Deployed a bad method? Rollback to the previous version in one call. No git revert, no redeploy. Combined with validation blocks (`replaceMethod:with:validate:`), you can gate method replacements on test suites passing.

**Method diffing and history.** Every method carries its version history. You can see how `processPayment:` evolved over a development session, diff any two versions, and understand the trajectory of changes. This is version control at the method level, not the file level — more natural for Smalltalk's object-oriented development model.

**Safe experimentation.** Try a different implementation, see if it works, roll back if it doesn't. No need to save the old version manually. The system remembers.

### Sketch

```smalltalk
"Replace a method (immediate effect for new calls)"
User compile: 'method: fullName [^firstName, '' '', lastName]'.

"Version history"
(User >> #fullName) versions.       "Array of all past versions"
(User >> #fullName) version: 3.     "Get specific version"
(User >> #fullName) rollbackTo: 2.  "Revert to version 2"

"Safe replacement with validation"
User replaceMethod: #processPayment:
    with: 'method: processPayment: order [...]'
    validate: [:old :new | self runPaymentTests].
    "Only applies if validation block returns true"

"Diff between versions"
(User >> #fullName) diffFrom: 1 to: 3.

"Change notifications"
User onMethodChange: [:class :selector :oldMethod :newMethod |
    Logger info: selector, ' updated on ', class name
].
```

### Implementation Path

- Each CompiledMethod gets a `version` field (integer, monotonically increasing per selector per class)
- The VTable always points to the latest version
- Call frames already snapshot the method pointer at send time (`frame.method`), so running code is unaffected by replacement
- Replacement = compile new method → increment version → store old method in version history → swap VTable entry (atomic pointer assignment in Go)
- Inline cache invalidation: bump a global "method epoch" counter. Caches check epoch on access — if stale, re-lookup. This is a single integer comparison in the hot path.
- Version history: each class has a `methodHistory` dictionary mapping `selector → [CompiledMethod]` ring buffer. Default depth: 10 versions per method.

### Complexity: Low-Medium

VTable swap is already atomic (pointer assignment). Inline cache invalidation via epoch counter is ~5 lines of Go. Version history storage is a simple ring buffer. The compile/replace workflow already exists in the ModifyService.

---

## Summary: How These Features Compose

These 10 features form a coherent system where each amplifies the others:

| Feature | Enables | Amplified By |
|---|---|---|
| Time-Travel Debugging (#1) | Replay any execution | Process Labels (#5) for navigating recordings |
| `become:` (#2) | Live object migration | Hot-Swap Methods (#10) for class evolution |
| Algebraic Effects (#3) | Testable side effects | Pattern Matching (#6) for effect dispatch |
| Moldable Inspectors (#4) | Rich object views | Watchpoints (#7) for live-updating views |
| Process Labels (#5) | Visible concurrency | Structured Concurrency (#9) for process lifecycle |
| Pattern Matching (#6) | Clean conditional logic | Effects (#3) for handler dispatch |
| Watchpoints (#7) | Reactive bindings | Moldable Inspectors (#4) for reactive UIs |
| First-Class Environments (#8) | Scope capture/restore | Time-Travel (#1) for crash forensics |
| Structured Concurrency (#9) | Safe process groups | Process Labels (#5) for group debugging |
| Hot-Swap Methods (#10) | Live code evolution | `become:` (#2) for instance migration |

## Recommended Implementation Order

Based on dependencies, effort, and immediate value:

1. **Process Labels + Dashboard** (#5) — Low effort, immediate value
2. **Structured Concurrency** (#9) — Pure Maggie library, no VM changes
3. **Pattern Matching** (#6) — Compiler work, huge ergonomic payoff
4. **Hot-Swappable Methods** (#10) — Enables the live development workflow
5. **Moldable Inspectors** (#4) — Rich development experience
6. **Watchpoints** (#7) — Reactive programming foundation
7. **Algebraic Effects** (#3) — Powerful abstraction, medium effort
8. **`become:`** (#2) — Live migration, higher effort
9. **First-Class Environments** (#8) — Advanced debugging and distribution
10. **Time-Travel Debugging** (#1) — Most ambitious, builds on everything above

## Open Questions

- Should pattern matching be a syntactic extension or a library (message-based)?
- Should effects use a new keyword or extend the existing exception mechanism?
- What's the version history depth for hot-swapped methods? (10? 100? Configurable?)
- Should the process dashboard be a Yutani widget or a web UI via gRPC?
- For `become:`, should we support both two-way swap and one-way forwarding (`becomeForward:`)?
- Should watchpoints fire synchronously (before the write completes) or asynchronously (queued)?
