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

Maggie source files use the `.mag` extension. The filename determines the class name:

- `Foo.mag` → methods for class `Foo`
- `MyApp.mag` → methods for class `MyApp`

### File Structure

Methods are separated by blank lines. No delimiters needed:

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
fork            -- spawn as new process
forkAt: priority
```

### Channel

Go-style communication channels.

```smalltalk
send: value     -- blocking send
receive         -- blocking receive
trySend: value  -- non-blocking send (returns boolean)
tryReceive      -- non-blocking receive (returns Result)
close
isClosed

-- Stream interface
nextPut: value  -- alias for send:
next            -- alias for receive
```

### Process

Lightweight concurrent processes.

```smalltalk
yield           -- give up time slice
terminate
wait            -- wait for completion
isAlive
isTerminated
result          -- get return value
priority
priority: level
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

```smalltalk
-- Producer/Consumer with channels
producer: channel count: n
    1 to: n do: [:i |
        channel send: i.
        'Sent: ' , i printString
    ].
    channel close

consumer: channel
    | value |
    [(value := channel tryReceive) isSuccess] whileTrue: [
        'Received: ' , value value printString
    ]

main
    | ch |
    ch := Channel new: 10.
    [self producer: ch count: 5] fork.
    [self consumer: ch] fork.
    ^0
```

## Tips

1. **Message chaining**: Messages return self by default, enabling fluent chains
2. **Blocks are closures**: They capture variables from enclosing scope
3. **Everything is an object**: Including numbers, booleans, and nil
4. **No operator precedence**: Binary messages evaluate left-to-right; use parentheses
5. **1-based indexing**: Arrays and strings start at index 1, not 0
