# Command-Line Apps (Cli)

The `Cli` namespace lets you build Unix-style command-line tools directly
from Maggie. It provides a thin, Smalltalk-idiomatic facade over
[spf13/cobra](https://github.com/spf13/cobra) for argument parsing,
subcommands, and help text; a pure-Maggie environment-variable binding
layer; and a shared output formatter that supports both plain text and
JSON.

- **Lib source:** [`lib/Cli/Command.mag`](../lib/Cli/Command.mag),
  [`lib/Cli/Flag.mag`](../lib/Cli/Flag.mag),
  [`lib/Cli/EnvBinding.mag`](../lib/Cli/EnvBinding.mag),
  [`lib/Cli/Output.mag`](../lib/Cli/Output.mag)
- **Primitives:** `vm/cli_primitives.go` (cobra bridge)
- **Runnable guide:** [`lib/guide/Guide20Cli.mag`](../lib/guide/Guide20Cli.mag)

## 1. Overview

### What `Cli` does

- Wraps `*cobra.Command` in a NaN-boxed Maggie value so applications can
  describe their CLI shape in Smalltalk syntax.
- Dispatches to a Maggie block when a command is invoked, marshaling
  positional arguments as an `Array<String>`.
- Binds `--flag` values (string / bool / integer) to cobra's flag set
  and makes them readable inside the run block.
- Registers subcommands in the usual cobra tree, so `myapp build`,
  `myapp test --verbose`, and `myapp help build` all work as expected.
- Projects environment variables onto a target object via
  `Cli::EnvBinding`, with type-coerced values (`#string`, `#integer`,
  `#bool`, `#duration`).
- Formats results as aligned key/value text, tables, or pretty-printed
  JSON through `Cli::Output`.

### What `Cli` does NOT do

- It does **not** wrap the Go
  [`envconfig`](https://github.com/kelseyhightower/envconfig) library.
  `envconfig` works by reading Go struct tags via reflection, which has
  no Maggie equivalent. `Cli::EnvBinding` replaces it with a
  pure-Maggie value object that describes one env var per instance.
- It does **not** mirror every cobra feature verbatim. Cobra-level
  concepts that a Maggie caller does not need (persistent flags,
  custom completion generators, shell-specific completion scripts)
  are intentionally absent; see [§10 Limitations](#10-limitations).
- It does **not** auto-generate help text. Cobra's built-in `--help` and
  `completion bash|zsh|fish|powershell` subcommands are inherited as-is.

## 2. Quickstart

A minimal CLI with one subcommand and one `--name` flag:

```smalltalk
"app.mag — 'mag run app.mag -- greet --name Alice' prints 'Hello, Alice!'"
| root greet |

root := Cli::Command named: 'hello' doc: 'A minimal Maggie CLI'.

greet := Cli::Command named: 'greet' doc: 'Say hello'.
greet flag: 'name' type: #string default: 'world' doc: 'Who to greet'.
greet run: [:args |
    ('Hello, ', (greet flagValue: 'name'), '!') println.
    0
].

root registerSubcommand: greet.
root execute
```

Run it:

```
$ mag run app.mag -- greet
Hello, world!

$ mag run app.mag -- greet --name Alice
Hello, Alice!

$ mag run app.mag -- greet --help
Say hello

Usage:
  hello greet [flags]

Flags:
  -h, --help          help for greet
      --name string   Who to greet (default "world")
```

## 3. Defining a command

Use `Cli::Command named:doc:run:` for the common three-part shape:

```smalltalk
cmd := Cli::Command
    named: 'status'
    doc:   'Show the current status'
    run:   [:args |
        'All systems nominal.' println.
        0
    ].
```

The run block receives the positional args as an `Array<String>`. Its
return value determines the process exit code:

| Return value                           | Exit code |
| -------------------------------------- | --------- |
| `SmallInteger` (e.g., `0`, `2`, `42`)  | that integer |
| Any other value (`nil`, strings, …)    | `0`       |
| Unhandled exception                    | `1` (cobra prints `Error: <msg>` to stderr) |

Two helpers are available when you want to build up the command
progressively:

| Selector                               | Purpose                           |
| -------------------------------------- | --------------------------------- |
| `Cli::Command named: 'foo'`            | name only, no docs, no run block  |
| `Cli::Command named: 'foo' doc: 'bar'` | name + short doc                  |

Both return a `Cli::Command` handle you can extend with `run:`,
`flag:type:default:doc:`, `registerSubcommand:`, etc.

## 4. Adding flags

Use `flag:type:default:doc:` to declare a flag. The type symbol picks
the accessor cobra will use on the command line:

| `type`     | Cobra accessor      | Example                              |
| ---------- | ------------------- | ------------------------------------ |
| `#string`  | `StringVar`         | `--name world`                       |
| `#bool`    | `BoolVar`           | `--verbose` / `--verbose=false`      |
| `#integer` | `IntVar`            | `--count 10`                         |

> `#int` is accepted as a synonym for `#integer`; the underlying primitive
> is called `addIntFlag:`.

```smalltalk
cmd flag: 'name'    type: #string  default: 'world' doc: 'Who to greet'.
cmd flag: 'verbose' type: #bool    default: false   doc: 'Chatty output'.
cmd flag: 'count'   type: #integer default: 1       doc: 'How many'.
```

Read the parsed value from inside the run block with `flagValue:`:

```smalltalk
cmd run: [:args |
    (cmd flagValue: 'verbose') ifTrue: [ 'verbose mode' println ].
    1 to: (cmd flagValue: 'count') do: [:i |
        ('Hello, ', (cmd flagValue: 'name'), '!') println
    ].
    0
].
```

`flagValue:` dispatches on the registered type; it returns `nil` for a
flag that was never declared, so checking the return value is the
safest way to detect a typo in the flag name.

### Flag value objects

If you are generating flag definitions programmatically, build a
`Cli::Flag` instance and pass it with the single-argument `flag:`
selector:

```smalltalk
spec := Cli::Flag
    name: 'endpoint' type: #string default: 'https://api.example.com'
    doc: 'API endpoint' required: true.
cmd flag: spec.
```

The `required` bit is advisory — it is surfaced to your own validation
code but is not enforced by the primitive.

## 5. Subcommand registration

Nest commands by calling `registerSubcommand:` on the parent:

```smalltalk
root  := Cli::Command named: 'mytool' doc: 'Multi-command tool'.

build := Cli::Command named: 'build' doc: 'Build the project'.
build run: [:args | 'building…' println. 0].

test  := Cli::Command named: 'test' doc: 'Run tests'.
test  flag: 'verbose' type: #bool default: false doc: 'Chatty output'.
test  run: [:args |
    (test flagValue: 'verbose') ifTrue: [ 'verbose mode' println ].
    'all tests pass' println.
    0
].

root registerSubcommand: build.
root registerSubcommand: test.
root execute
```

Subcommands can themselves have subcommands — the underlying cobra tree
is arbitrarily deep. `mytool build help`, `mytool help test`, and
`mytool test --help` are all generated for free.

`registerSubcommand:` is a thin alias for the lower-level primitive
`addSubcommand:`.

## 6. Env-var binding

`Cli::EnvBinding` describes how one environment variable should be
projected onto a target object via a one-argument setter selector.

```smalltalk
| cfg binding |
cfg := AppConfig new.
binding := Cli::EnvBinding
    envName: 'PP_AGENT_COMMAND'
    setter:  #agentCommand:
    type:    #string.
binding applyTo: cfg.
"→ if $PP_AGENT_COMMAND is set, invokes cfg agentCommand: <the value>"
```

### Coercion rules

| `type`      | Behaviour                                                                  |
| ----------- | -------------------------------------------------------------------------- |
| `#string`   | value is passed through verbatim (after `trim`)                            |
| `#integer`  | `asInteger`; raises an `Error` when the string does not parse              |
| `#bool`     | `true` / `1` / `yes` (case-insensitive) → `true`; anything else → `false`  |
| `#duration` | delegates to `Duration parse:` when that class is loaded; otherwise returns the trimmed string |

### Defaults and required bindings

```smalltalk
"Apply a default when the variable is unset:"
Cli::EnvBinding
    envName: 'PP_COUNT' setter: #count: type: #integer default: 10.

"Fail hard when a required variable is unset (answers a Failure):"
Cli::EnvBinding
    envName: 'PP_TOKEN' setter: #token: type: #string default: nil required: true.
```

### Attaching bindings to a command

`Cli::Command` exposes `envBinding:` and `applyEnvBindings:`:

```smalltalk
cmd := Cli::Command named: 'server' doc: 'Run the server'.
cmd envBinding: (Cli::EnvBinding envName: 'PP_PORT'  setter: #port:  type: #integer default: 8080).
cmd envBinding: (Cli::EnvBinding envName: 'PP_DEBUG' setter: #debug: type: #bool    default: false).

cmd run: [:args |
    | cfg |
    cfg := AppConfig new.
    cmd applyEnvBindings: cfg.  "reads env, coerces, calls setters"
    cfg startServer.
    0
].
```

`applyEnvBindings:` answers the first `Failure` it encounters and stops
evaluation, so you can branch on its return value.

> **Scope note:** the Go-side wrapper does not yet invoke
> `applyEnvBindings:` from inside `execute` automatically — callers
> invoke it from their run block (or a subclass override). A future
> refactor of `vm/cli_primitives.go` is expected to lift this into the
> wrapper so env bindings fire before the run block. Until then, the
> caller-invoked pattern above is the supported shape.

## 7. Output formatting

`Cli::Output` centralises result rendering so commands agree on
formatting. It provides both a stateful class-side API (pick a format
once, render many results) and a stateless instance-side API (one-shot
helpers that ignore the global mode).

### Text vs JSON

```smalltalk
"Class-side: pick format, then render"
Cli::Output format: 'json'.
Cli::Output result: (Dictionary new at: 'ok' put: true; yourself).
"→ pretty-printed JSON"

Cli::Output format: 'text'.
Cli::Output result: (Dictionary new at: 'ok' put: true; yourself).
"→ aligned 'ok    true'"
```

`Cli::Output result:` is smart:

- In `'text'` mode, Dictionaries render as aligned `key  value` lines
  and any other value renders via `printString`.
- In `'json'` mode, every value renders as pretty-printed JSON (via
  `Json encodePretty:`).

`Cli::Output resultOrError:` layers on a `Result`-style branch: a
`Failure` prints the wrapped error (plain text or `{"error": "..."}`
in JSON mode) and the method answers `false`; a success delegates to
`result:` and answers `true`.

### Tables

```smalltalk
| rows |
rows := Array
    with: (Dictionary new at: 'name' put: 'alice'; at: 'status' put: 'online'; yourself)
    with: (Dictionary new at: 'name' put: 'bob'  ; at: 'status' put: 'away';   yourself).

Cli::Output
    table:   rows
    headers: #('NAME' 'STATUS')
    keys:    #('name' 'status').
"→
NAME   STATUS
alice  online
bob    away
"
```

In JSON mode `table:` emits the rows as a JSON array unchanged; in text
mode column widths are computed to fit the widest header or cell.

The instance-side `Cli::Output new printTable:columns:` takes a list of
`#(header key)` pairs instead of two parallel arrays:

```smalltalk
Cli::Output new
    printTable: rows
    columns:    #(#('NAME' 'name') #('STATUS' 'status')).
```

## 8. Testing your CLI

Two testing patterns are well supported.

### Doctests

Every selector on `Cli::Command`, `Cli::Flag`, `Cli::EnvBinding`, and
`Cli::Output` ships with `mag doctest`-style assertions in its
docstring. Follow the same pattern in your own commands:

```smalltalk
"""
Construct and configure a command.

```test
cmd := Cli::Command named: 'demo'.
cmd flag: 'count' type: #integer default: 7 doc: 'How many'.
cmd flagValue: 'count' >>> 7
```
"""
```

Run them with:

```
$ mag doctest --class MyApp::Cli
```

### Capturing output with `setOutput:` and `setArgs:`

For integration-level tests that exercise the full cobra dispatch,
redirect the command's output to a Go-wrapped buffer and override its
argument list:

```smalltalk
"Pseudocode — the real test uses a GoObject wrapping *bytes.Buffer"
| cmd buffer |
cmd    := Cli::Command named: 'greet' doc: 'Say hello'.
cmd    flag: 'name' type: #string default: 'world' doc: 'Who to greet'.
cmd    run: [:args |
    ('Hello, ', (cmd flagValue: 'name'), '!') println. 0
].

buffer := BufferedStream new.               "go-wrapped io.Writer"
cmd    setOutput: buffer.                   "redirect cobra Out + Err"
cmd    setArgs:   #('--name' 'alice').      "skip os.Args"
cmd    execute.                             "=> 0"
buffer contents                             "=> 'Hello, alice!\n'"
```

`setOutput:` accepts a `GoObject` whose wrapped Go value implements
`io.Writer` (typically a `*bytes.Buffer`). Passing `nil` restores
cobra's default writers. `setArgs:` accepts a Maggie
`Array<String>` and takes precedence over `os.Args`.

Reference: see `vm/cli_primitives_test.go` in the VM for end-to-end
examples driving `Cli::Command` from Go.

## 9. Generated help and completion

Because `Cli::Command` is a thin wrapper over cobra, the standard cobra
UX is inherited unchanged:

- `<cmd> --help` and `<cmd> <subcommand> --help` render cobra's
  auto-generated help screen, including all declared flags and their
  defaults.
- `<cmd> help <subcommand>` is the alternate syntax — cobra handles
  both forms.
- `<cmd> completion bash` (and `zsh`, `fish`, `powershell`) prints a
  shell completion script for the whole command tree. Pipe the output
  to a file, source it from your shell rc, and tab completion works
  out of the box.

No additional Maggie code is needed for any of these — they come for
free the moment you call `cmd execute`.

The `--help` text uses `shortDoc:` (one-line summary) and `longDoc:`
(multi-paragraph description) when set. `named:doc:` wires `shortDoc:`
automatically; call `longDoc:` directly when you want more detail.

## 10. Limitations

The `Cli` namespace ships a deliberate subset of cobra. The following
items are not currently exposed. Each is tracked as a potential
follow-up story; open an issue if you hit one.

- **Persistent flags.** Cobra distinguishes local flags (declared on
  one command) from persistent flags (declared on a parent, inherited
  by every child). `Cli::Command` currently only exposes local flags
  via `addStringFlag:default:doc:` and siblings. If you need a
  `--verbose` that applies to every subcommand, duplicate the flag
  on each child for now.
- **Shell completion script generation primitive.** Cobra's
  `--completion` subcommand works end-to-end, but there is no
  Maggie-side selector to trigger completion generation
  programmatically (e.g., to write the script to a specific file). Use
  shell redirection for now: `mytool completion bash > ~/.mytool-completion`.
- **Cobra-level hooks** (`PreRun`, `PostRun`, `PersistentPreRun`,
  `PersistentPostRun`) are not plumbed through. Wrap the relevant
  logic inside the run block instead.
- **Automatic env-binding application.** `applyEnvBindings:` is
  caller-invoked from inside the run block. A future refactor of the
  Go-side wrapper is expected to call it from `execute` before the
  block fires.
- **Required-flag enforcement.** `Cli::Flag>>required` is advisory;
  higher-level validation code must consult it directly.
- **Flag value objects beyond string / bool / integer.** Float,
  duration, slice, and map flag types are not yet wired through the
  primitive. Roll your own coercion in the run block from a
  `#string` flag if needed.

## See also

- Runnable guide: [`lib/guide/Guide20Cli.mag`](../lib/guide/Guide20Cli.mag)
- Framework decision: `pp read decision story:maggie-cli-framework`
- Cobra upstream: <https://github.com/spf13/cobra>
