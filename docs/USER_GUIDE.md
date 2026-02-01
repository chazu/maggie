# Maggie User Guide

Maggie is a late-bound Smalltalk dialect implemented in Go. It features a simple, expressive syntax, first-class blocks, and Go-style concurrency with channels and lightweight processes.

## Installation

### Building from Source

```bash
git clone https://github.com/chazu/maggie.git
cd maggie
make
```

This builds the `mag` binary with an embedded default image containing the core class library.

### Verifying Installation

```bash
./mag -i
```

You should see:
```
Maggie REPL (type 'exit' to quit)

>>
```

## Running Maggie

The `mag` command is the main entry point:

```bash
mag [options] [paths...]
```

### Options

| Option | Description |
|--------|-------------|
| `-i` | Start interactive REPL |
| `-m <entry>` | Run specified method as main (e.g., `main` or `App.start`) |
| `-v` | Verbose output |
| `-no-rc` | Skip loading ~/.maggierc |
| `--image <path>` | Load a custom image instead of the embedded default |
| `--save-image <path>` | Save VM state to an image after loading all files |

### Subcommands

| Command | Description |
|---------|-------------|
| `mag deps` | Resolve and fetch project dependencies |
| `mag deps update` | Re-resolve dependencies, ignoring lock file |
| `mag deps list` | Show the resolved dependency tree |

### Examples

```bash
# Start the REPL
mag -i

# Load files from a directory and start REPL
mag ./src -i

# Load files recursively and run main
mag ./... -m main

# Run an application entry point
mag ./app -m App.start

# Verbose loading
mag -v ./lib

# Build and save an image
mag ./src/... --save-image my-app.image

# Run from a custom image
mag --image my-app.image -m Main.start

# Run a project (auto-detects maggie.toml)
mag

# Manage dependencies
mag deps
mag deps update
mag deps list
```

## The REPL

The REPL (Read-Eval-Print Loop) lets you interactively evaluate Maggie expressions.

### Basic Usage

```
>> 3 + 4.
7
>> 5 factorial.
120
>> 'hello' , ' world'.
'hello world'
```

### Multi-line Input

For expressions spanning multiple lines, type without a trailing period. Press Enter on a blank line to execute:

```
>> 1 to: 5 do: [:i |
..     i printString
.. ]
```

Or end with a period to execute immediately:

```
>> (1 to: 10) inject: 0 into: [:sum :n | sum + n].
55
```

### Defining Methods

You can define methods in the REPL:

```
>> double
..     ^self * 2

>> 21 double.
42
```

### Exiting

Type `exit` or `quit` to leave the REPL.

## Maggie Syntax

### Literals

```smalltalk
-- Numbers
42          -- SmallInteger
3.14        -- Float
-17         -- Negative integer

-- Strings
'hello'     -- String (single quotes)
'it''s'     -- Escaped quote

-- Symbols
#foo        -- Symbol
#'hello world'  -- Symbol with spaces

-- Booleans
true
false

-- Nil
nil

-- Arrays
#(1 2 3)    -- Literal array
```

### Messages

Maggie uses Smalltalk-style message passing:

```smalltalk
-- Unary messages (no arguments)
5 factorial
'hello' size
true not

-- Binary messages (one argument, operator)
3 + 4
10 - 3
'hello' , ' world'

-- Keyword messages (named arguments)
array at: 1
array at: 1 put: 'value'
3 between: 1 and: 10
```

### Message Precedence

1. Parentheses
2. Unary messages (left to right)
3. Binary messages (left to right)
4. Keyword messages (left to right)

```smalltalk
2 + 3 * 4           -- = 20 (not 14!)
2 + (3 * 4)         -- = 14
1 + 2 between: 0 and: 5  -- (1 + 2) between: 0 and: 5
```

### Variables

```smalltalk
-- Temporaries (local variables)
| x y |
x := 10.
y := x + 5.

-- Assignment
x := 42
```

### Blocks

Blocks are anonymous functions (closures):

```smalltalk
-- No arguments
[42]

-- With arguments
[:x | x * 2]
[:x :y | x + y]

-- Evaluating blocks
[42] value                  -- 42
[:x | x * 2] value: 21      -- 42
[:x :y | x + y] value: 3 value: 4  -- 7
```

### Control Flow

```smalltalk
-- Conditionals
x > 0 ifTrue: ['positive']
x > 0 ifFalse: ['not positive']
x > 0 ifTrue: ['positive'] ifFalse: ['not positive']

-- Loops
[x < 10] whileTrue: [x := x + 1]
[x > 0] whileFalse: [x := x + 1]

-- Iteration
5 timesRepeat: [self doSomething]
1 to: 10 do: [:i | i printString]
1 to: 10 by: 2 do: [:i | i printString]  -- odd numbers
```

### Methods

Methods are defined in `.mag` files. Each method starts at column 0:

```smalltalk
-- Unary method
double
    ^self * 2

-- Binary method
+ other
    ^self add: other

-- Keyword method
between: min and: max
    ^self >= min & (self <= max)
```

### Returning Values

Use `^` to return a value:

```smalltalk
factorial
    self = 0 ifTrue: [^1].
    ^self * (self - 1) factorial
```

### Comments

```smalltalk
-- This is a comment
x := 42  -- inline comment
```

## Source Files (.mag)

Maggie source files use the `.mag` extension. Files can define classes using Trashtalk-style declarations, or add methods to existing classes.

### Class Definitions

Classes are defined with `subclass:`, instance variables with `instanceVars:`, and methods with `method:` / `classMethod:`:

```smalltalk
-- Counter.mag
Counter subclass: Object
  instanceVars: value

  method: initialize [
    value := 0
  ]

  method: increment [
    value := value + 1.
    ^value
  ]

  method: value [ ^value ]

  classMethod: startingAt: n [
    | c |
    c := self new.
    c setValue: n.
    ^c
  ]
```

### Adding Methods to Existing Classes

Files can also add methods to existing classes. The filename determines the class name (`MyMath.mag` adds methods to class `MyMath`). Methods are separated by blank lines:

```smalltalk
-- MyMath.mag
-- Custom math utilities

square
    ^self * self

cube
    ^self * self * self

sqrt
    ^self primSqrt
```

### Namespaces and Imports

Files can declare a namespace and import other namespaces. These must appear before any class or method definitions:

```smalltalk
namespace: 'MyApp::Models'
import: 'MyApp::Core'

User subclass: Object
  instanceVars: name email

  method: name [ ^name ]
  method: email [ ^email ]
```

- `namespace:` — optional, one per file, sets the namespace for all classes defined in the file
- `import:` — zero or more, allows referencing classes from other namespaces without full qualification
- The `::` separator is used for nested namespaces (e.g., `'MyApp::Models::Base'`)
- Files without `namespace:` or `import:` work exactly as before (backward compatible)

**Class resolution order**: When a class name is referenced, Maggie looks in this order:
1. Current namespace (e.g., `MyApp::Models::ClassName`)
2. Each imported namespace (e.g., `MyApp::Core::ClassName`)
3. Bare name (e.g., `ClassName` — the default/root namespace)

### Directory-as-Namespace Convention

When loading directories, the directory structure automatically maps to namespaces:

```
src/myapp/models/User.mag  → namespace MyApp::Models
src/myapp/Main.mag         → namespace MyApp
src/Helper.mag             → no namespace (root)
```

Path segments are converted to PascalCase (e.g., `my-app` becomes `MyApp`). An explicit `namespace:` declaration in the file overrides the directory-derived namespace.

### Loading Files

```bash
# Load a single file
mag ./MyMath.mag -i

# Load a directory
mag ./src -i

# Load recursively
mag ./... -i
```

## Core Classes

### Object

Base class for all objects.

```smalltalk
yourself        -- returns self
== other        -- identity comparison
= other         -- equality comparison
~= other        -- inequality
isNil           -- false for all objects except nil
notNil          -- true for all objects except nil
class           -- returns the object's class
printString     -- string representation
error: message  -- signal an error
```

### SmallInteger

Immediate integer values.

```smalltalk
-- Arithmetic
+ - * / //      -- basic operations (// is integer division)
\\ other        -- modulo
negated         -- negate
abs             -- absolute value

-- Comparison
< > <= >= = ~=

-- Testing
even odd
positive negative isZero

-- Iteration
timesRepeat: block
to: stop do: block
to: stop by: step do: block

-- Math
factorial
gcd: other
lcm: other
max: other
min: other
between: min and: max
```

### Float

Floating-point numbers.

```smalltalk
-- Arithmetic
+ - * /
negated abs sign

-- Comparison
< > <= >= = ~=
max: min:

-- Rounding
floor ceiling rounded truncated

-- Testing
positive negative isZero
```

### Boolean (True/False)

```smalltalk
not             -- logical not
& other         -- logical and
| other         -- logical or
and: block      -- short-circuit and
or: block       -- short-circuit or

ifTrue: block
ifFalse: block
ifTrue: trueBlock ifFalse: falseBlock
```

### String

Character strings.

```smalltalk
size            -- length
at: index       -- character at index (1-based)
, other         -- concatenation
isEmpty notEmpty

-- Comparison
< > <= >= =

-- Searching
includes: char
indexOf: char

-- Conversion
asSymbol
asUppercase asLowercase

-- Copying
copyFrom: start to: end
```

### Symbol

Interned, immutable strings. Identity comparison.

```smalltalk
asSymbol        -- returns self
asString        -- convert to string
= other         -- identity comparison (fast)
```

### Array

Ordered, indexable collections.

```smalltalk
size
at: index
at: index put: value
first last
isEmpty notEmpty

-- Iteration
do: block
collect: block
select: block
reject: block
detect: block
detect: block ifNone: noneBlock

-- Aggregation
inject: initial into: block

-- Testing
includes: anObject
indexOf: anObject

-- Combining
, other                     -- concatenation
copyFrom: start to: end
```

### Block

Closures / anonymous functions.

```smalltalk
value
value: arg
value: arg1 value: arg2
value: arg1 value: arg2 value: arg3

-- Control flow
whileTrue: block
whileFalse: block
whileTrue
whileFalse
repeat

-- Exception handling
on: exception do: handler
ensure: finallyBlock

-- Concurrency
fork                    -- spawn as new process, returns Process
forkWith: arg           -- fork with single argument
forkWithContext: ctx    -- fork with CancellationContext
```

**fork semantics**: When a block is forked, non-local returns (`^`) are treated as local returns within the forked process. This prevents crashes from returns attempting to cross goroutine boundaries.

### Channel

Go-style communication channels for safe inter-process communication.

```smalltalk
-- Creation
Channel new             -- unbuffered channel
Channel new: size       -- buffered channel with capacity

-- Blocking operations
send: value             -- blocking send
receive                 -- blocking receive

-- Non-blocking operations
trySend: value          -- non-blocking send (returns true/false)
tryReceive              -- non-blocking receive (returns value or nil)

-- State
close                   -- close the channel
isClosed                -- check if closed
isEmpty                 -- check if no pending values
size                    -- number of buffered items
capacity                -- buffer capacity

-- Select (multiplexed channel operations)
Channel select: casesArray              -- wait on multiple channels
Channel select: casesArray ifNone: blk  -- with default case
ch onReceive: [:v | ...]                -- create receive case for select
ch onSend: val do: [...]                -- create send case for select

-- Stream interface
nextPut: value          -- alias for send:
next                    -- alias for receive
```

**Channel select** allows waiting on multiple channels simultaneously:

```smalltalk
result := Channel select: {
    ch1 onReceive: [:v | 'Got from ch1: ', v].
    ch2 onReceive: [:v | 'Got from ch2: ', v]
}.

-- Non-blocking with default
result := Channel select: {
    ch onReceive: [:v | v]
} ifNone: ['No channel ready'].
```

### Process

Lightweight concurrent processes (goroutines).

```smalltalk
-- Creation (via Block)
[expression] fork                   -- spawn block as new process
[expr] forkWith: arg                -- fork with argument
[:ctx | ...] forkWithContext: ctx   -- fork with cancellation context
Process fork: block                 -- class method version

-- Waiting
wait                    -- block until complete, returns result
isDone                  -- check if process finished
result                  -- get result (nil if not done)

-- Control
Process current         -- get current process
Process yield           -- yield to other goroutines
Process sleep: ms       -- sleep for milliseconds
```

**fork vs forkWithContext**: Use `forkWithContext:` when you need cancellation or timeout support. The context is passed to the block:

```smalltalk
ctx := CancellationContext withTimeout: 5000.
proc := [:context |
    [context isCancelled not] whileTrue: [
        -- do work
        Process sleep: 100
    ]
] forkWithContext: ctx.
```

### Mutex

Mutual exclusion lock for protecting shared state.

```smalltalk
-- Creation
Mutex new               -- create a mutex

-- Locking
lock                    -- acquire lock (blocks if held)
unlock                  -- release lock
tryLock                 -- non-blocking attempt (returns true/false)
isLocked                -- check if currently locked

-- Safe pattern
critical: block         -- execute block while holding lock (auto-unlock)
```

**Best practice**: Always use `critical:` instead of manual lock/unlock:

```smalltalk
mutex := Mutex new.
mutex critical: [
    counter := counter + 1
].
```

### WaitGroup

Synchronization barrier for waiting on multiple processes.

```smalltalk
-- Creation
WaitGroup new           -- create a wait group

-- Counter operations
add: count              -- add to counter
done                    -- decrement counter by 1
wait                    -- block until counter reaches zero
count                   -- get current counter value

-- Convenience
wrap: block             -- add 1, fork block, auto-done on completion
```

**Example**: Wait for multiple workers:

```smalltalk
wg := WaitGroup new.
results := Channel new: 5.

1 to: 5 do: [:i |
    wg wrap: [
        Process sleep: (i * 10).
        results send: i * i
    ]
].

wg wait.  -- blocks until all workers done
results close.
```

### Semaphore

Counting semaphore for limiting concurrent access.

```smalltalk
-- Creation
Semaphore new           -- binary semaphore (1 permit)
Semaphore new: n        -- semaphore with n permits

-- Permit operations
acquire                 -- acquire permit (blocks if none available)
release                 -- release permit
tryAcquire              -- non-blocking attempt (returns true/false)

-- State
available               -- number of available permits
capacity                -- total capacity

-- Safe pattern
critical: block         -- execute while holding permit (auto-release)
withPermit: block       -- alias for critical:
```

**Example**: Limit concurrent database connections:

```smalltalk
dbPool := Semaphore new: 3.  -- max 3 connections

dbPool critical: [
    -- only 3 processes can be here at once
    self queryDatabase
].
```

### CancellationContext

Go-style context for cancellation and timeouts.

```smalltalk
-- Creation
CancellationContext background      -- never-cancelled base context
CancellationContext todo            -- placeholder context
CancellationContext withCancel      -- cancellable context
CancellationContext withTimeout: ms -- timeout after milliseconds

-- Child contexts (inherit parent cancellation)
withCancel              -- create cancellable child
withTimeout: ms         -- create child with timeout

-- Cancellation
cancel                  -- cancel this context
isCancelled             -- check if cancelled
isDone                  -- alias for isCancelled

-- Deadline info
hasDeadline             -- check if has timeout
deadline                -- deadline in milliseconds
remainingTime           -- milliseconds until deadline

-- Blocking
wait                    -- block until context is done
doneChannel             -- channel that closes when cancelled

-- Execution
do: block               -- execute block, auto-cancel when done
ifCancelled: block      -- execute if cancelled
parent                  -- get parent context
```

**Timeout pattern**:

```smalltalk
ctx := CancellationContext withTimeout: 5000.

[ctx isCancelled not] whileTrue: [
    -- do work
    Process sleep: 100
].
-- automatically stops after 5 seconds
```

**Graceful shutdown pattern**:

```smalltalk
ctx := CancellationContext withCancel.
workers := OrderedCollection new.

-- Start workers that respect cancellation
1 to: 5 do: [:i |
    workers add: ([:context |
        [context isCancelled not] whileTrue: [
            -- process work
            Process sleep: 100
        ]
    ] forkWithContext: ctx)
].

-- Later: trigger shutdown
ctx cancel.
workers do: [:w | w wait].
```

### Result (Success/Failure)

Functional error handling.

```smalltalk
-- Testing
isSuccess
isFailure

-- Accessing
value           -- get success value
error           -- get failure reason

-- Chaining
then: block     -- execute if success
map: block      -- transform success value
flatMap: block  -- chain operations

-- Conditional
ifSuccess: block
ifFailure: block
onSuccess: successBlock onFailure: failureBlock
```

### UndefinedObject (nil)

The singleton `nil` value.

```smalltalk
isNil           -- true
notNil          -- false
ifNil: block
ifNotNil: block
ifNil: nilBlock ifNotNil: notNilBlock
```

## Configuration

### ~/.maggierc

Maggie automatically loads `~/.maggierc` on startup (after the default image). Use it to define custom methods or configure your environment:

```smalltalk
-- ~/.maggierc
-- Personal Maggie configuration

greet
    ^'Hello from maggierc!'

-- Custom utilities
pp
    ^self printString
```

Skip loading with `-no-rc`:

```bash
mag -no-rc -i
```

## Example: A Simple Application

Create `App.mag`:

```smalltalk
-- App.mag
-- Simple application example

start
    'Welcome to Maggie!' printString.
    self run

run
    | count |
    count := 1.
    [count <= 5] whileTrue: [
        count printString.
        count := count + 1
    ].
    'Done!' printString.
    ^0
```

Run it:

```bash
mag ./App.mag -m App.start
```

## Example: Concurrency

### Basic Producer/Consumer

```smalltalk
-- Producer/Consumer with channels
producer: channel count: n
    1 to: n do: [:i |
        channel send: i
    ].
    channel close

consumer: channel
    | val |
    [channel isClosed not] whileTrue: [
        val := channel receive.
        val notNil ifTrue: [
            Transcript show: 'Received: ', val printString; cr
        ]
    ]

main
    | ch |
    ch := Channel new: 10.
    [self producer: ch count: 5] fork.
    [self consumer: ch] fork.
    ^0
```

### Worker Pool with WaitGroup

```smalltalk
runWorkerPool
    | wg jobs results |
    wg := WaitGroup new.
    jobs := Channel new: 10.
    results := Channel new: 10.

    -- Start 3 workers
    1 to: 3 do: [:id |
        wg wrap: [
            | job |
            [(job := jobs tryReceive) notNil] whileTrue: [
                results send: job * job
            ]
        ]
    ].

    -- Send jobs
    1 to: 10 do: [:i | jobs send: i].
    jobs close.

    -- Wait for workers to finish
    wg wait.
    results close
```

### Timeout with CancellationContext

```smalltalk
fetchWithTimeout: url timeout: ms
    | ctx result |
    ctx := CancellationContext withTimeout: ms.
    result := nil.

    [:context |
        [context isCancelled not] whileTrue: [
            result := self fetch: url.
            context cancel  -- done, exit loop
        ]
    ] forkWithContext: ctx.

    ctx wait.
    result ifNil: ['Timeout!']
```

### Select with Timeout

```smalltalk
receiveWithTimeout: channel timeout: ms
    | timeoutCh |
    timeoutCh := Channel new: 1.
    [Process sleep: ms. timeoutCh send: #timeout] fork.

    ^Channel select: {
        channel onReceive: [:v | v].
        timeoutCh onReceive: [:v | nil]
    }
```

See `examples/concurrency.mag` for more comprehensive examples.

## Compiler Primitives

The `Compiler` class provides methods for dynamic code loading, saving, and evaluation.

### fileIn / fileOut

Load source files at runtime or export classes back to source:

```smalltalk
-- Load a single source file
Compiler fileIn: 'path/to/MyClass.mag'.

-- Load all .mag files from a directory (recursive)
Compiler fileInAll: 'path/to/src'.

-- Export a class to a .mag file
Compiler fileOut: 'Counter' to: '/tmp/Counter.mag'.

-- Export all classes in a namespace to a directory (one file per class)
Compiler fileOutNamespace: 'MyApp::Models' to: '/tmp/models/'.
```

### Image Persistence

Save the entire VM state (all loaded classes, methods, and globals) to a binary image:

```smalltalk
-- Save current VM state
Compiler saveImage: 'my-app.image'.
```

From the command line:

```bash
# Build an image from source files
mag ./src/... --save-image my-app.image

# Run from a saved image
mag --image my-app.image -m Main.start
```

### Dynamic Evaluation

```smalltalk
-- Evaluate expressions at runtime
Compiler evaluate: '3 + 4'.           -- 7

-- Evaluate with a specific receiver
arr := Array new: 3.
Compiler evaluate: 'self size' in: arr.  -- 3

-- Globals persist across evaluations
Compiler evaluate: 'x := 42'.
Compiler evaluate: 'x + 1'.           -- 43
```

## Project Manifest (maggie.toml)

For larger projects, create a `maggie.toml` manifest in the project root:

```toml
[project]
name = "my-app"
namespace = "MyApp"
version = "0.1.0"

[source]
dirs = ["src", "lib"]
entry = "Main.start"

[dependencies]
yutani = { git = "https://github.com/chazu/yutani-mag", tag = "v0.5.0" }
helper = { path = "../helper" }

[image]
output = "my-app.image"
include-source = true
```

### Sections

| Section | Description |
|---------|-------------|
| `[project]` | Project metadata: `name`, `namespace`, `version` |
| `[source]` | Source directories (`dirs`, default `["src"]`) and `entry` point |
| `[dependencies]` | External packages — `git` (with optional `tag`) or local `path` |
| `[image]` | Image output path and whether to include source for `fileOut:` |

### Running a Project

When `mag` is invoked in a directory containing `maggie.toml` (with no explicit paths), it automatically:
1. Resolves and fetches dependencies
2. Loads dependency source directories (in topological order)
3. Loads project source directories
4. Runs the entry point

```bash
cd my-app/
mag              # auto-detects maggie.toml
mag -v           # verbose loading
```

### Dependency Management

```bash
mag deps              # Resolve and fetch all dependencies
mag deps update       # Re-resolve, ignoring the lock file
mag deps list         # Show the resolved dependency tree
```

Dependencies are cloned/fetched into `.maggie/deps/` and their resolved versions are recorded in `.maggie/lock.toml`. Transitive dependencies are resolved automatically.

## Tips

1. **Message chaining**: Messages return self by default, enabling fluent chains
2. **Blocks are closures**: They capture variables from enclosing scope
3. **Everything is an object**: Including numbers, booleans, and nil
4. **No operator precedence**: Binary messages evaluate left-to-right; use parentheses
5. **1-based indexing**: Arrays and strings start at index 1, not 0
