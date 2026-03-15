---
title: "Research: mag help <topic> CLI command"
task: maggie-nfv
agent: Zephyr
date: 2026-03-14
status: complete
---

# Research: `mag help <topic>` CLI Command

## Question 1: How does the current CLI parse subcommands?

**File:** `cmd/mag/main.go`

The CLI uses Go's `flag` package for flag parsing, then handles subcommands via a `switch` on `args[0]` (lines 95-131):

```go
if len(args) > 0 {
    switch args[0] {
    case "lsp":     // sets flag
    case "deps":    handleDepsCommand(args[1:], *verbose); return
    case "fmt":     handleFmtCommand(args[1:]); return
    case "wrap":    handleWrapCommand(args[1:], *verbose); return
    case "build":   handleBuildCommand(args[1:], *verbose); return
    case "sync":    syncArgs = args[1:]   // deferred — needs VM
    case "doc":     docMode = "doc"; docArgs = args[1:]   // deferred
    case "doctest": docMode = "doctest"; docArgs = args[1:]  // deferred
    }
}
```

**Two patterns exist:**
1. **Early-return subcommands** (`deps`, `fmt`, `wrap`, `build`): These don't need the VM. They call their handler and `return` immediately.
2. **Deferred subcommands** (`sync`, `doc`, `doctest`): These store args in variables and dispatch *after* VM initialization (lines 297-317), because they need the loaded image.

A `help` subcommand would need the VM (to query classes/docstrings from the loaded image), so it should follow the **deferred pattern** — store args, dispatch after VM init.

**Adding a new subcommand:**
1. Add a case in the switch: `case "help": helpArgs = args[1:]`
2. Strip it from source paths (like sync/doc do)
3. Add handler call after VM init: `handleHelpCommand(vmInst, helpArgs)`
4. Update `flag.Usage` to include `mag help <topic>` in the usage string

## Question 2: How does REPL `:help` work internally?

**File:** `cmd/mag/main.go`, lines 522-592

The REPL's `:help` command is handled in `handleREPLCommand()`:

- `:help` with no args prints the command list
- `:help ClassName` calls `handleHelpLookup(vmInst, "ClassName")`
- `:help Class>>method` calls `handleHelpLookup(vmInst, "Class>>method")`

`handleHelpLookup()` (line 557) does:
1. Checks for `>>` separator to distinguish class vs method lookup
2. For methods: `cls.MethodNamed(methodName)` → prints `cm.DocString()` or "(no documentation)"
3. For classes: `cls := vmInst.Classes.Lookup(query)` → calls `vm.FormatClassHelp(cls, vmInst.Selectors)`

**Key insight:** The REPL help logic is entirely in `cmd/mag/main.go` (not in the VM). It calls into the VM's public API:
- `vmInst.Classes.Lookup(name)` — find a class by name
- `cls.MethodNamed(name)` — find a method on a class
- `vm.FormatClassHelp(cls, selectors)` — format class help as string
- `cm.DocString()` — get method docstring

This is the **exact same API** a CLI `help` subcommand would use. The REPL's `handleHelpLookup` can be directly reused (or factored out) for the CLI command.

## Question 3: How are docstrings stored and indexed?

### Storage

**Compiler level** (`compiler/ast.go`, `compiler/parser.go`):
- The parser recognizes `""" ... """` triple-quoted strings at the start of method bodies
- Parsed into `ast.MethodDef.DocString` field
- Compiled into `CompiledMethod.docString` (private field, accessed via `DocString()` method)

**Class level** (`vm/class.go`):
- `Class.DocString string` field — stores class-level documentation
- Set during compilation when a class definition includes a docstring

**Method level** (`vm/compiled_method.go`):
- `CompiledMethod.docString string` — stores method documentation
- Accessed via `DocString()` accessor method
- `MethodDocString(m Method)` — works for any Method interface (compiled or primitive)

### Querying without the REPL

Yes, docstrings are fully queryable without the REPL:

```go
// Query a class
cls := vmInst.Classes.Lookup("Array")
cls.DocString  // class docstring

// Query a method
m := cls.MethodNamed("size")
m.DocString()  // method docstring

// List all classes
allClasses := vmInst.Classes.All()  // returns []*Class

// Format for display
output := vm.FormatClassHelp(cls, vmInst.Selectors)
```

The image contains all docstrings — they survive save/load. No REPL session needed.

### VM-side primitives (`vm/docstring_primitives.go`)

The VM also registers Maggie-level primitives:
- `anObject help` — prints class help to stdout
- `ClassName help` — prints class help (class-side)
- `ClassName help: #methodName` — prints specific method help
- `anObject docString` — returns docstring as string
- `ClassName methodDocFor: #methodName` — returns method docstring as string

## Question 4: What format should output be?

### Current formats in the codebase

1. **REPL `:help`** — plain text via `FormatClassHelp()`:
   ```
   Array (subclass of Object)

   Ordered collection of elements.

   Instance methods:
     size
     at:
     at:put:

   Class methods:
     new:
   ```

2. **`mag doc`** — HTML via `cmd/mag/docgen.go`:
   Full HTML documentation site with templates, cross-references, etc.

3. **VM primitives** — plain text to stdout

### Recommendation: Plain text

For a CLI `help` command, **plain text** is the right default:
- Consistent with REPL `:help` output
- Works in terminals, pipes, scripts
- Can use the existing `FormatClassHelp()` and `formatMethodHelpAny()` formatters
- Other CLI tools (Go's `go doc`, Rust's `cargo doc --document-private-items`, Python's `pydoc`) use plain text for terminal output

The existing `FormatClassHelp()` format is already well-structured and suitable. For method help, `formatMethodHelpAny()` works well too.

A `--json` flag could be added later for machine consumption, but plain text should be the default and is sufficient for a first implementation.

## Question 5: How other Smalltalk tools expose help

### Pharo

- **System Browser**: Primary interactive tool. Classes organized by packages/protocols.
- **Playground/Workspace**: `ClassName browse` opens browser at that class.
- **`printIt`/`doIt`**: Evaluate `ClassName comment` to get class comment, `ClassName >> #methodName) comment` for method comment.
- **Spotter**: Cmd+Shift+P search tool, finds classes/methods/packages by name.
- **No standalone CLI help** — Pharo is image-based, help is always interactive.

### Squeak

- Similar to Pharo — Browser-centric. `ClassName comment` in workspace.
- No CLI help tool (image-based workflow).

### GNU Smalltalk

- `gst --help` for CLI options
- **`gst-doc`**: Standalone documentation generator. Extracts class/method docs from .st files.
- Can query in batch mode: `echo 'Array comment' | gst`

### Cuis Smalltalk

- Browser-based like Pharo/Squeak.
- `Smalltalk browseClass: ClassName` to open browser.

### Key takeaway

Most Smalltalks rely on interactive image-based tools for help. Maggie is unique in having a CLI-first workflow, so a `mag help` command would fill a gap that traditional Smalltalks don't need (they have the browser). The closest precedent is GNU Smalltalk's `gst-doc` and Go's `go doc` command.

## Implementation Recommendations

### Simplest approach

Add a `help` case to the subcommand switch in `main.go`:

```go
case "help":
    helpArgs = args[1:]
```

Then after VM init:

```go
if helpArgs != nil {
    handleHelpCLI(vmInst, helpArgs)
    return
}
```

The handler reuses `handleHelpLookup()` directly — the REPL's help logic already does exactly what's needed:

```go
func handleHelpCLI(vmInst *vm.VM, args []string) {
    if len(args) == 0 {
        // List all classes (like `go doc` with no args)
        // Or show usage
    }
    for _, query := range args {
        handleHelpLookup(vmInst, query)
    }
}
```

### Estimated effort

~30 lines of code. The infrastructure (`FormatClassHelp`, `handleHelpLookup`, `ClassTable.Lookup`, `ClassTable.All`) already exists. This is essentially wiring an existing REPL feature to a CLI entry point.

### Open questions for implementation

1. Should `mag help` with no args list all classes? (Like `go doc .`) Or show usage help?
2. Should it support namespace-qualified names? (`mag help Yutani::Button`)
   - `ClassTable.Lookup` already handles FQN keys, so this works for free.
3. Should it load project sources (from `maggie.toml`) or only work with the embedded image?
   - Recommend: try `maggie.toml` first, fall back to embedded image (same as `mag doc`).
4. Should `mag help` also search method selectors? (e.g., `mag help at:put:` finds all classes implementing it)
   - Nice-to-have for v2, not needed initially.
