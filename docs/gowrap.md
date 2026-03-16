# Go Interop (`gowrap`) Reference

This document covers `mag wrap` and `mag build` — the pipeline for wrapping Go packages as Maggie classes.

## Pipeline Overview

```
mag wrap <pkg>  →  introspect Go package  →  generate Go glue  →  generate .mag stubs
mag build       →  wrap (if needed)  →  compile .mag sources  →  embed image  →  go build
```

Output is written to `.maggie/wrap/<pkgname>/` by default:
- `wrap.go` — Go glue code (registers primitives with the VM)
- `stubs.mag` — Maggie stub classes with `<primitive>` methods

## Type Conversion

### Supported Go types

These Go types are automatically converted between Go and Maggie values:

| Go Type | Maggie Type | Notes |
|---------|-------------|-------|
| `string` | `String` | |
| `int`, `int8`..`int64` | `SmallInt` | All integer widths |
| `uint`, `uint8`..`uint64` | `SmallInt` | All unsigned widths |
| `float32`, `float64` | `Float` | |
| `bool` | `true` / `false` | |
| `[]byte` | `String` | Converted to/from string |
| `*T` (same-package struct) | `GoObject` | Wrapped as GoObject pointer |
| `T` (same-package struct value) | `GoObject` | Dereferenced from `*T` GoObject |
| `*string`, `*int`, etc. | basic type | Pointer to basic type — auto-boxed |
| Named type with basic underlying (same-package) | corresponding basic | e.g., `type Format string` → `String` |
| `interface{}` / `any` | any Maggie value | Empty interface only |

### Silently skipped Go types

Functions or methods with these parameter/return types are **skipped without error** — they simply won't appear in the generated wrappers:

| Go Type | Why Skipped |
|---------|-------------|
| Cross-package struct types | Can't import arbitrary packages in generated code |
| Cross-package named types (e.g., `time.Duration`) | Same reason — no import available |
| Non-empty interfaces (e.g., `io.Reader`, `context.Context`) | No automatic conversion from Maggie values |
| Channels (`chan T`) | No Maggie equivalent mapping |
| Maps (`map[K]V`) | No automatic conversion |
| Slices (except `[]byte`) | Only `[]byte` is handled (as string) |
| Function types (`func(...)`) | No automatic conversion |

A skipped method gets a comment in the generated Go glue:
```go
// Skipped: methodName — unconvertible parameter type
```

### Additional limits

- Functions with **more than 4 parameters** are skipped entirely.
- Only **pointer-receiver methods** on structs are wrapped (value-receiver methods are included too, but only exported methods).
- **Constants** are extracted but only as metadata — not yet exposed as Maggie globals.

## Error Handling

Go functions returning `error` (either `(T, error)` or just `error`) are wrapped with automatic error conversion:

```go
// Generated Go glue checks the error and panics:
result, err := pkg.SomeFunction(args...)
if err != nil {
    panic(fmt.Sprintf("Maggie error: GoError: %v", err))
}
```

In Maggie, catch these with exception handling:

```smalltalk
[Go::Json marshal: obj]
    on: Error
    do: [:e | 'JSON error: ', e message].
```

The error message is prefixed with `GoError:` so you can distinguish Go errors from Maggie errors.

## Selector Naming

Go function names are converted to Maggie selectors following these rules:

| Go Signature | Maggie Selector | Rule |
|-------------|----------------|------|
| `Contains(s string)` | `contains:` | camelCase + `:` for 1 param |
| `HasPrefix(s string)` | `hasPrefix:` | PascalCase → camelCase |
| `Count()` | `count` | No params → no colon |
| `Replace(s, old, new string, n int)` | `replace:_:_:_:` | Multi-param: positional `_:` |

Multi-parameter selectors use `_:` placeholders because Go parameter names aren't semantic keywords. A 3-param function `Foo(a, b, c)` becomes `foo:_:_:`.

### Calling multi-param wrapped functions

```smalltalk
"Go: strings.Replace(s, old, new, n)"
Go::Strings replace: myString _: 'old' _: 'new' _: -1.
```

## Namespace Convention

All wrapped Go packages are placed under the `Go::` namespace prefix:

| Go Import Path | Maggie Namespace |
|---------------|-----------------|
| `strings` | `Go::Strings` |
| `encoding/json` | `Go::Json` |
| `net/http` | `Go::Http` |

Only the last segment of the import path is used (PascalCased). There is no deep nesting — `encoding/json` becomes `Go::Json`, not `Go::Encoding::Json`.

## Manifest Configuration

Configure wrapping in `maggie.toml`:

```toml
[go-wrap]
output = ".maggie/wrap"

[[go-wrap.packages]]
import = "strings"
include = ["Contains", "HasPrefix", "Builder"]  # optional filter

[[go-wrap.packages]]
import = "encoding/json"
# no filter = wrap all exported names
```

## CLI Usage

### `mag wrap`

```bash
mag wrap                        # wrap all packages from maggie.toml
mag wrap strings                # ad-hoc: wrap a single package
mag wrap --output ./wrappers    # custom output directory
```

### `mag build`

```bash
mag build              # entry-point-only binary (./mag-custom)
mag build -o myapp     # custom output binary name
mag build --full       # full mag CLI with project baked in
mag build --full -o myapp
```

Requires `maggie.toml`. Compiles `.mag` sources into an embedded image binary that registers wrapped Go types at startup.

Without `--full`, the binary only loads the image and runs the entry point.

With `--full`, the binary is a complete `mag` CLI — REPL, `fmt`, `doctest`, `help`, LSP, and all other subcommands work. Your project's classes are pre-loaded in the embedded image. When invoked with no arguments, it runs the project's entry point; use `-i` for REPL access.

## Integration: Loading Stubs

The generated `.mag` stubs must be loaded into the VM for the Maggie compiler to know the class hierarchy. This happens automatically during `mag build` (stubs are compiled into the embedded image). For manual workflows, load stubs with:

```smalltalk
Compiler fileIn: '.maggie/wrap/strings/stubs.mag'.
```
