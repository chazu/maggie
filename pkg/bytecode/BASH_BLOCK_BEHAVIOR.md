# Bash Block Behavior Reference

This document describes the current behavior of Trashtalk blocks when executed via
the Bash runtime. This serves as the compatibility baseline that bytecode execution
must match.

## Block Syntax

Trashtalk uses square brackets for block literals:

```smalltalk
[body]                    " Zero-arg block "
[:x | body]               " Single-arg block "
[:x :y | body]            " Multi-arg block "
[:x | |temp| body]        " Block with local variable "
```

## Block Creation

When the Bash compiler encounters a block literal, it generates:

1. A unique block ID: `block_<method>_<counter>`
2. A Bash function containing the block body
3. Registration with the block registry

Example compilation:

```smalltalk
" Source "
method: doSomething [
  | blk |
  blk := [:x | x + 1].
  ^ blk valueWith: 5
]
```

```bash
# Compiled output
__Counter__doSomething() {
  local blk

  # Block creation - generates function and returns ID
  _block_counter_1() {
    local x="$1"
    echo $(( x + 1 ))
  }
  blk="block_doSomething_1"

  # Block invocation
  _invoke_block "$blk" "valueWith:" "5"
}
```

## Block Invocation Selectors

| Selector | Args | Description |
|----------|------|-------------|
| `value` | 0 | Invoke zero-arg block |
| `valueWith:` | 1 | Invoke with one argument |
| `valueWith:and:` | 2 | Invoke with two arguments |
| `valueWith:and:and:` | 3 | Invoke with three arguments |
| `valueWithArgs:` | N | Invoke with array of arguments |

## Variable Capture Behavior

### Local Variables

Blocks capture local variables from their enclosing scope. Changes made inside
the block are visible outside (reference semantics):

```smalltalk
method: example [
  | counter |
  counter := 0.
  3 timesRepeat: [counter := counter + 1].
  ^ counter  " Returns 3 "
]
```

### Method Parameters

Method parameters are captured the same way as locals:

```smalltalk
method: addTo: x [
  | blk |
  blk := [:n | x := x + n].
  blk valueWith: 5.
  ^ x  " Returns original + 5 "
]
```

### Instance Variables

Instance variables accessed in blocks are read from and written to SQLite:

```smalltalk
method: increment [
  | blk |
  blk := [value := value + 1].
  blk value.
  " value ivar is now persisted with new value "
]
```

## Control Flow Blocks

### ifTrue: / ifFalse:

```smalltalk
(x > 0) ifTrue: [^ 'positive'].
(x < 0) ifFalse: [^ 'non-negative'].
```

These are optimized by the compiler into Bash conditionals:

```bash
if [[ "$x" -gt 0 ]]; then
  echo "positive"
  return
fi
```

### ifTrue:ifFalse:

```smalltalk
^ (x > 0) ifTrue: ['positive'] ifFalse: ['non-positive']
```

### whileTrue: / whileFalse:

```smalltalk
[i < 10] whileTrue: [
  @ Console print: i.
  i := i + 1
]
```

## Iteration Blocks

### do: (each)

```smalltalk
items do: [:item | @ Console print: item]
```

The block receives each element.

### collect: (map)

```smalltalk
doubled := numbers collect: [:n | n * 2]
```

Returns new array with block results.

### select: (filter)

```smalltalk
positives := numbers select: [:n | n > 0]
```

Returns array of elements where block returns true.

### detect: (find)

```smalltalk
first := items detect: [:x | x > 10]
firstOrNil := items detect: [:x | x > 100] ifNone: [nil]
```

### inject:into: (reduce)

```smalltalk
sum := numbers inject: 0 into: [:acc :n | acc + n]
```

## Non-Local Return

Blocks can return from their enclosing method using `^`:

```smalltalk
method: findFirst: target in: collection [
  collection do: [:item |
    (item = target) ifTrue: [^ item]  " Returns from findFirst:in:, not just the block "
  ].
  ^ nil
]
```

In Bash, this is implemented by setting a flag and checking after block invocation.

## Block Nesting

Blocks can contain other blocks:

```smalltalk
method: matrix [
  rows collect: [:row |
    row collect: [:cell |
      cell * 2
    ]
  ]
]
```

Inner blocks capture variables from all enclosing scopes.

## Edge Cases

### Empty Blocks

```smalltalk
emptyBlock := [].
emptyBlock value  " Returns empty string "
```

### Blocks as Return Values

```smalltalk
method: makeAdder: n [
  ^ [:x | x + n]
]

" Usage "
add5 := @ obj makeAdder: 5.
@ add5 valueWith: 10  " Returns 15 "
```

### Recursive Blocks

```smalltalk
method: factorial: n [
  | fact |
  fact := [:x |
    (x <= 1) ifTrue: [1] ifFalse: [x * (fact valueWith: (x - 1))]
  ].
  ^ fact valueWith: n
]
```

The block can reference itself through the captured `fact` variable.

## Performance Characteristics

| Operation | Relative Cost |
|-----------|---------------|
| Block creation | Low (function definition) |
| Block invocation | High (subshell or function call) |
| Variable capture | Low (Bash dynamic scoping) |
| ivar access in block | Very High (SQLite query per access) |

## Known Limitations

1. **No closures escaping scope**: Blocks cannot outlive their creating method
   in the Bash runtime (function goes out of scope).

2. **Subshell isolation**: Some block invocations create subshells, causing
   variable changes to be lost.

3. **Performance**: Each block invocation has function call overhead; nested
   blocks multiply this cost.

4. **No tail-call optimization**: Deep recursion with blocks can exhaust stack.

## Bytecode Compatibility Requirements

The bytecode VM must match this behavior:

1. **Reference capture**: Changes to captured variables must be visible outside
2. **ivar persistence**: ivar changes must write through to SQLite
3. **Control flow**: ifTrue:/whileTrue:/etc. must behave identically
4. **Iteration**: do:/collect:/select: must produce same results
5. **Non-local return**: ^ in block must exit enclosing method
6. **Recursive blocks**: Self-referencing blocks must work

Areas where bytecode can improve:

1. **Performance**: Native execution instead of function calls
2. **Closures**: Blocks can escape their creating scope
3. **Reduced subshell overhead**: Direct variable access
