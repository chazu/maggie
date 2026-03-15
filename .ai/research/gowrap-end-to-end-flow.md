---
title: "gowrap End-to-End Flow and Gaps"
task: maggie-reo
date: 2026-03-15
agent: Scuttle
status: complete
tags: [gowrap, go-interop, research]
---

# gowrap End-to-End Flow and Gaps

## 1. Pipeline Overview

The gowrap pipeline has four stages, triggered by two CLI commands:

```
mag wrap <pkg>  →  introspect  →  gen_go  →  gen_mag  →  write files
mag build       →  (wrap if needed)  →  compile image  →  generate main.go  →  go build
```

### Stage 1: Introspect (`gowrap/introspect.go`)

`IntrospectPackage(importPath, includeFilter)` uses `golang.org/x/tools/go/packages` to load a Go package and extract its exported API into a `PackageModel`:

- **Functions**: package-level exported functions (name, params, results, error flag)
- **Types**: exported named types — only structs get methods/fields extracted; non-struct named types are captured but have no methods
- **Constants**: exported constants (name, type, literal value)
- **Methods**: pointer-receiver methods on struct types (non-inherited only)

The `includeFilter` (map[string]bool) restricts which exported names are included. When nil, all exported names are captured.

### Stage 2: Generate Go Glue (`gowrap/gen_go.go`)

`GenerateGoGlue(model)` produces a Go source file (`wrap.go`) containing:

- Package declaration: `package wrap_<pkgname>`
- Imports: `reflect`, `fmt` (if error returns), the wrapped package as `pkg`, and `github.com/chazu/maggie/vm`
- `RegisterPrimitives(v *vm.VM)` function that:
  - Registers struct types via `v.RegisterGoType(className, reflect.TypeOf((*pkg.Type)(nil)))`
  - Creates a namespace class for package-level functions (e.g., `Go::Strings`)
  - Registers functions as `AddClassMethod` on the namespace class
  - Registers methods as `AddPrimitiveMethod` on type classes

**Type conversion** (`goTypeConversion`): handles basic types (string, int, float, bool), `[]byte` ↔ string, pointer types (same-package struct pointers, basic pointers), named types with basic underlying types (same-package only), and `interface{}`. Returns `""` for unconvertible types — those methods are skipped with a comment.

**Error handling**: Functions returning `(T, error)` or `error` get Go-side error checks that `panic(fmt.Sprintf("Maggie error: GoError: %v", err))`.

**Limits**: Functions with >4 parameters are skipped. Methods from other packages (cross-package param types) are skipped.

### Stage 3: Generate Maggie Stubs (`gowrap/gen_mag.go`)

`GenerateMaggieStubs(model)` produces a `.mag` source file:

- Namespace declaration (e.g., `namespace: 'Go'`)
- Package class with `classMethod:` entries (e.g., `Strings subclass: Object`)
- Type classes with `method:` entries (e.g., `Builder subclass: Object`)
- All methods marked `<primitive>` (implementation is in the Go glue)

### Stage 4: File Output

`wrapPackage()` in `cmd/mag/wrap.go` orchestrates stages 1-3 and writes:
- `<outputDir>/<pkgname>/wrap.go` — Go glue code
- `<outputDir>/<pkgname>/stubs.mag` — Maggie stub classes

Default output dir: `.maggie/wrap/`

### Stage 5: Build (`gowrap/build.go`)

Two build paths:

**Legacy (`Build`)**: Creates a temp Go module, generates `main.go` that imports VM + wrapper packages, runs `go build`. The generated binary loads and runs `.mag` source files from CLI args. This path is used when `maggie.toml` has `[go-wrap]` packages but no `[source]` dirs.

**Embedded (`BuildEmbedded`)**: Full pipeline — compiles project `.mag` sources into a VM image, embeds it via `//go:embed`, generates `main.go` that loads the image + registers wrapped Go types + runs the entry point. This path is used when `maggie.toml` has `[source]` dirs.

## 2. CLI Integration (`cmd/mag/wrap.go`, `cmd/mag/build.go`)

### `mag wrap`

```bash
mag wrap                        # wraps all packages from [go-wrap.packages] in maggie.toml
mag wrap encoding/json          # ad-hoc single package wrap
mag wrap --output ./wrappers    # custom output directory
```

Supports both manifest-driven and ad-hoc wrapping. The manifest (`maggie.toml`) can specify `include` filters per package.

### `mag build`

```bash
mag build              # produces ./mag-custom
mag build -o myapp     # custom output binary name
```

Requires `maggie.toml`. If project has `[source]` dirs, compiles into embedded image binary. If only `[go-wrap]` packages, does legacy gowrap-only build.

### Manifest Configuration

```toml
[go-wrap]
output = ".maggie/wrap"

[[go-wrap.packages]]
import = "strings"
include = ["Contains", "HasPrefix", "Builder"]

[[go-wrap.packages]]
import = "encoding/json"
```

## 3. Test Coverage

| File | Tests | Coverage |
|------|-------|----------|
| `introspect.go` | 5 tests: strings, filtered, encoding/json, bad path, constants (math) | Good: exercises all extract* functions, error types, constants |
| `gen_go.go` | 3 tests: strings golden, error handling (json), empty model | Good: golden file comparison, error codegen |
| `gen_mag.go` | 3 tests: strings golden, empty model, methods (json Decoder) | Good: golden file comparison |
| `naming.go` | 4 test functions with table-driven subtests | Good: covers all naming converters |
| `build.go` | 2 tests: `generateMain`, `generateGoMod` (string content checks) | Moderate: only tests string generation, not actual `go build` |

**Not covered by unit tests:**
- `Build()` and `BuildEmbedded()` — these shell out to `go build`, so they're integration-level
- `goTypeConversion` — tested indirectly via golden files, but no focused unit tests for each branch
- Multi-param keyword selectors (e.g., 3+ params) — tested via skip behavior only
- `wrapPackage()` and `handleWrapCommand()` in `cmd/mag/wrap.go` — CLI integration, not unit tested

## 4. Good Example Package: `encoding/base64`

`encoding/base64` is a reasonable example but has limitations for gowrap:

**Pros**: Small API surface, well-known stdlib package.

**Cons**: Its key function `NewEncoding(string) *Encoding` returns a pointer to a struct, which works. But `NewDecoder` takes `*Encoding` and `io.Reader` — the `io.Reader` interface is a non-empty interface that `goTypeConversion` can't handle (returns `""`), so `NewDecoder` would be skipped.

**Better example**: `strings` is already used in tests and is the best example — it has:
- Package-level functions with basic types (`Contains`, `HasPrefix`)
- A struct type with methods (`Builder`)
- Error-returning methods (`Builder.Write`)
- All types are convertible (strings, ints, bytes, runes)

For a new example, `strconv` would also work well: `Atoi(string) (int, error)`, `Itoa(int) string` — all basic types, error handling.

## 5. Does `mag wrap` Produce Runnable Output?

**Yes, but with manual steps for `mag wrap` alone:**

1. `mag wrap strings` produces `.maggie/wrap/strings/wrap.go` and `stubs.mag`
2. The `wrap.go` file is a Go package — it can't run standalone
3. You need `mag build` to compile it into a binary
4. `mag build` requires `maggie.toml` with either `[source]` or `[go-wrap]` sections

**`mag build` produces a runnable binary**, but:
- The legacy build path (`Build()`) generates a `main.go` that expects CLI args but doesn't actually implement source loading — it prints usage and exits. The generated main has `_ = v` which means the VM is created but never used beyond wrapper registration.
- The embedded build path (`BuildEmbedded()`) works end-to-end: compiles sources → embeds image → runs entry point.

**Gap**: The legacy `generateMain()` in `build.go:338-377` creates a binary that doesn't actually do anything useful — it creates a VM, registers primitives, then expects CLI args but doesn't load or run them. There's no source loading, no REPL, nothing. This is a dead end that should either be removed or completed.

## 6. Documentation Gaps

### For agents/contributors:

1. **No example `maggie.toml` with go-wrap** — The CLAUDE.md documents the gowrap API but there's no complete end-to-end example of a project that wraps a Go package and uses it from Maggie code.

2. **Type conversion limitations undocumented** — Which Go types can be wrapped and which are silently skipped? The code handles: basic types, `[]byte`, same-package struct pointers, same-package named types with basic underlying types, and `interface{}`. Everything else (cross-package types, channels, maps, slices of non-byte, function types, non-empty interfaces like `io.Reader`) is silently skipped.

3. **Error handling model** — Go errors are converted to panics (`GoError`). This is documented in code comments but not in CLAUDE.md. Important for anyone writing Maggie code that calls wrapped Go functions.

4. **Multi-param selector naming** — Go functions with 2+ params use `name:_:_:` style positional colons. This is a pragmatic choice documented only in `naming.go:42` comment. Could be confusing: `strings.Replace(s, old, new, n)` becomes `replace:_:_:_:`.

5. **Legacy build path is broken** — `generateMain()` produces a binary that doesn't actually work (see gap #5 above).

6. **No doc on how `.mag` stubs integrate** — When does the VM load `stubs.mag`? Answer: it doesn't automatically. The stubs are metadata for the Maggie compiler to know the class hierarchy — they'd need to be `fileIn`'d or included in the image compilation. This integration story is missing.

7. **`BuildEmbedded` uses `filepath.Base(wrapDir)`** — This silently breaks if `wrapDir` is nested (e.g., `.maggie/wrap`) because it extracts `wrap` not the full relative path. The import path becomes `<projectModule>/wrap/<pkg>` instead of `<projectModule>/.maggie/wrap/<pkg>`.

8. **No `go.sum` handling** — The generated temp Go modules don't handle `go.sum`, relying on `go mod tidy` only in the embedded path. The legacy path doesn't run `go mod tidy` at all.

## 7. Architectural Observations

- **Clean separation**: `gowrap/` is a pure library package. CLI integration is in `cmd/mag/`. Model types are separate from generation logic. Good.
- **Golden file tests**: Prevent format drift. Update with `UPDATE_GOLDEN=1 go test ./gowrap/`.
- **Naming convention is lossy**: Go's PascalCase → Maggie's camelCase is one-way. Multi-param methods lose param name semantics (`_:` placeholders). This is documented in code.
- **No round-trip**: You can't go from Maggie stubs back to Go types. This is fine — the Go glue is the source of truth.
- **Namespace convention**: All wrapped Go packages live under `Go::` prefix (e.g., `Go::Strings`, `Go::Json`). Flat — no hierarchy reflecting Go import paths (`encoding/json` → `Go::Json`, not `Go::Encoding::Json`).
