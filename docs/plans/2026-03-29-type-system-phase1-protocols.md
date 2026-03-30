# Type System Phase 1: Protocols and Structural Matching

**Date:** 2026-03-29
**Depends on:** Brainstorm at `docs/brainstorms/2026-03-28-type-system-design.md`
**Status:** Draft — review before implementation

---

## Goal

Add optional type annotations and protocol definitions to Maggie. The type checker is a separate tool (`mag typecheck`) that produces warnings, never blocks compilation. Zero runtime cost — annotations are parsed and stored on the AST but do not affect bytecode generation.

This is Phase 1 only: protocols, structural matching, and explicit annotations. No inference, no effects, no refinements.

---

## Syntax Design

### Type annotations

Type annotations use `<TypeName>` after the thing they annotate. This is Strongtalk's convention.

```smalltalk
"Method parameter types"
method: at: index <Integer> put: value <Object> [ ... ]

"Return type (after selector, before body)"
method: size -> <Integer> [ ... ]

"Binary method"
method: + other <Number> -> <Number> [ ... ]

"Temporary variable types"
| count <Integer> name <String> |

"Instance variable types"
instanceVars: name <String> age <Integer>
```

### Protocol definitions

Protocols use `Name protocol` syntax (same pattern as `Name trait`):

```smalltalk
Sizeable protocol
  size -> <Integer>.
  isEmpty -> <Boolean>.

Indexable protocol
  includes: Sizeable.         "protocol inclusion"
  at: <Integer> -> <Object>.
  at: <Integer> put: <Object> -> <Self>.
```

A protocol is a named set of message signatures. Protocol entries are method signatures without bodies, terminated by periods. The `includes:` directive imports all signatures from another protocol.

### What `<Self>` means

`<Self>` refers to the type of the receiver. In a protocol, it means "the conforming type." In a class, it means "this class or a subclass."

### The `<Dynamic>` escape hatch

`<Dynamic>` is compatible with everything. Untyped parameters are implicitly `<Dynamic>`. Untyped return values are implicitly `<Dynamic>`. This is how typed and untyped code coexist.

---

## Disambiguation: `<` as type annotation vs binary selector

This is the key parser challenge. `<` is currently a binary selector (less-than). In type annotation position, it starts a type expression.

**Rule: `<` is a type annotation only in these syntactic positions:**

1. **After a parameter name in a method signature:** `at: index <Integer>` — the parser just consumed an identifier that's a parameter name. If the next token is `<`, it's a type annotation.

2. **After `->` in a method signature:** `method: size -> <Integer>` — the `->` unambiguously introduces a return type.

3. **After a temp name in `| ... |`:** `| count <Integer> |` — inside a temp declaration, after an identifier.

4. **After an ivar name in `instanceVars:`:** `instanceVars: name <String> age <Integer>` — after each identifier in the ivar list.

**In all other positions, `<` is the binary selector.** Inside method bodies, `3 < 4` is always a message send. There is no ambiguity because type annotations only appear in declaration positions, never in expression positions.

**Implementation:** The parser does NOT need lexer changes. The lexer still emits `<` as `TokenBinarySelector`. The parser checks for `<` in the specific declaration contexts above and interprets it as starting a type annotation. This is context-sensitive parsing, which the parser already does (e.g., `|` means both "temp delimiter" and "binary or" depending on context).

---

## AST Changes

### New AST nodes

```go
// TypeExpr represents a type annotation.
type TypeExpr struct {
    SpanVal Span
    Name    string      // "Integer", "Self", "Dynamic", or protocol name
    Params  []*TypeExpr // type parameters, e.g., Array <Integer> → Params: [Integer]
}

func (n *TypeExpr) Span() Span { return n.SpanVal }
func (n *TypeExpr) node()      {}

// ProtocolEntry is a single message signature in a protocol definition.
type ProtocolEntry struct {
    SpanVal    Span
    Selector   string      // "size", "at:put:", etc.
    ParamTypes []*TypeExpr // one per parameter
    ReturnType *TypeExpr   // nil means <Dynamic>
}

// ProtocolDef represents a protocol definition.
type ProtocolDef struct {
    SpanVal   Span
    Name      string
    Includes  []string         // names of included protocols
    Entries   []*ProtocolEntry // message signatures
    DocString string
}

func (n *ProtocolDef) Span() Span { return n.SpanVal }
func (n *ProtocolDef) node()      {}
```

### Changes to existing AST nodes

```go
// MethodDef — add type annotations
type MethodDef struct {
    // ... existing fields ...
    ParamTypes []*TypeExpr // parallel to Parameters, nil entries = untyped
    ReturnType *TypeExpr   // nil = untyped
}

// ClassDef — add typed instance variables
type ClassDef struct {
    // ... existing fields ...
    InstanceVarTypes []*TypeExpr // parallel to InstanceVariables, nil = untyped
}

// TraitDef — could have typed method signatures, but defer to Phase 2

// SourceFile — add protocol definitions
type SourceFile struct {
    // ... existing fields ...
    Protocols []*ProtocolDef
}
```

Temporary variable types are stored on MethodDef:

```go
type MethodDef struct {
    // ... existing fields ...
    TempTypes []*TypeExpr // parallel to Temps, nil entries = untyped
}
```

---

## Parser Changes

### 1. Parse type annotations after parameters

In `parseMethodSignature()`, after consuming each parameter identifier, check for `<`:

```go
// In keyword method parsing, after consuming parameter name:
params = append(params, p.curToken.Literal)
p.nextToken()
// Check for type annotation
if p.curTokenIs(TokenBinarySelector) && p.curToken.Literal == "<" {
    paramTypes = append(paramTypes, p.parseTypeExpr())
} else {
    paramTypes = append(paramTypes, nil) // untyped
}
```

### 2. Parse return type annotation

After the full selector is parsed, check for `->`:

```go
// After parseMethodSignature returns:
var returnType *TypeExpr
if p.curTokenIs(TokenBinarySelector) && p.curToken.Literal == "-" {
    // Peek: is the next char '>'?  Actually, -> would lex as binary selector "->"
    // or as "-" followed by ">". Need to check lexer behavior.
    // DECISION: treat "->" as a two-character binary selector.
    // The lexer already reads multi-char binary selectors.
}
```

**Open question:** Does `->` lex as a single `TokenBinarySelector` with literal `"->"`, or as two separate tokens `"-"` and `">"` ? Since both `-` and `>` are binary selector characters, and the lexer reads consecutive binary chars as one token, `->` will lex as `TokenBinarySelector("->")`  if there is no space between them. This works — the parser checks for `"->"` as a binary selector literal.

BUT: `->` is also a valid binary message send (`anObject -> anotherObject`). In practice, `->` is not a standard Smalltalk selector and no Maggie code uses it. But it's technically a breaking change if anyone does.

**Alternative:** Use `^<Type>` for return types instead of `-> <Type>`. The caret already means "return" in method bodies. In a method signature, `^<Integer>` before the body brackets could mean "returns Integer":

```smalltalk
method: size ^<Integer> [ ^items size ]
```

This is exactly what Strongtalk did. It reuses existing syntax (`^`) in a natural way and avoids the `->` ambiguity entirely.

**Recommendation: Use `^<Type>` for return types (Strongtalk style).**

### 3. Parse protocol definitions

In `ParseSourceFile`, add a case for `Name protocol` alongside `Name subclass:` and `Name trait`:

```go
case p.curTokenIs(TokenIdentifier) && p.curToken.Literal == "protocol":
    protocolDef := p.parseProtocolDefBody(name, startPos)
    // ...
```

`parseProtocolDefBody` parses entries until end of indented block:

```
Sizeable protocol
  size ^<Integer>.
  isEmpty ^<Boolean>.
```

Each entry is: selector (unary, binary, or keyword) + optional param types + optional return type + period.

### 4. Parse type expressions

`parseTypeExpr()` consumes `< TypeName >` or `< TypeName <Param> >`:

```go
func (p *Parser) parseTypeExpr() *TypeExpr {
    // consume <
    p.nextToken()
    // expect identifier (type name)
    name := p.curToken.Literal
    p.nextToken()
    // check for type parameters
    var params []*TypeExpr
    if p.curTokenIs(TokenBinarySelector) && p.curToken.Literal == "<" {
        // nested type parameter
        params = append(params, p.parseTypeExpr())
    }
    // expect >
    // But > is also a binary selector. The parser needs to consume it.
    if p.curTokenIs(TokenBinarySelector) && p.curToken.Literal == ">" {
        p.nextToken()
    }
    return &TypeExpr{Name: name, Params: params}
}
```

**Complication with `>`:** The `>` character is a binary selector. When parsing `<Integer>`, the lexer produces `<` `Integer` `>` — but will the lexer join `>` with a following binary char? E.g., `<Integer>=` would lex `<`, `Integer`, `>=`. This is a problem.

**Solution:** Type annotations are only in declaration positions (not expression positions), so `>=` after a type annotation is not valid syntax anyway. The parser consuming the `>` inside `parseTypeExpr` handles this. If the lexer emits `>=`, the parser needs to split it. This is messy.

**Better solution:** After seeing `<` in a type position, switch the lexer into "type mode" where `>` terminates the type expression regardless of what follows. This can be done without a mode flag — the parser just looks for a `>` as the first character of whatever binary selector token follows the type name.

**Simplest solution:** Since type annotations are always after identifiers in declaration positions, the parser knows when it's in type-annotation context. When it sees a `TokenBinarySelector` starting with `>`, it consumes just the `>` and pushes back any remaining characters. The lexer would need a `pushBack` or the parser needs to split the token.

**Recommendation for Phase 1:** Require a space before `>` in type annotations if there's an ambiguity concern. In practice, `<Integer>` has no ambiguity because after the `>` comes either another parameter name, `^`, `[`, `|`, or `.` — none of which start with a binary character. So the lexer will emit `>` as its own token. **This is a non-issue in practice.** Only `<Integer>>` (with `>>`) would be a problem, and that's not valid syntax.

### 5. Parse typed temps

In `parseTemporaries()`, after each identifier, check for `<`:

```go
for p.curTokenIs(TokenIdentifier) {
    temps = append(temps, p.curToken.Literal)
    p.nextToken()
    if p.curTokenIs(TokenBinarySelector) && p.curToken.Literal == "<" {
        tempTypes = append(tempTypes, p.parseTypeExpr())
    } else {
        tempTypes = append(tempTypes, nil)
    }
}
```

### 6. Parse typed instance variables

In the instance variable parsing within class definitions, same pattern.

---

## Type Checker (`types/` package)

### Architecture

```
types/
  checker.go    — main entry point, walks AST
  protocol.go   — protocol registry, structural matching
  types.go      — type representation (MaggieType interface)
  env.go        — type environment (variable → type bindings)
```

### Type representation

```go
type MaggieType interface {
    String() string
    Equals(other MaggieType) bool
}

type NamedType struct {
    Name   string       // "Integer", "String", "MyClass"
    Params []MaggieType // type parameters
}

type ProtocolType struct {
    Name    string                    // protocol name
    Methods map[string]*MethodSig    // selector → signature
}

type MethodSig struct {
    ParamTypes []MaggieType
    ReturnType MaggieType
}

type DynamicType struct{} // compatible with everything

type SelfType struct{} // the receiver's type
```

### Structural matching

The core algorithm: does class X satisfy protocol P?

```go
func (c *Checker) Satisfies(class *vm.Class, protocol *ProtocolType) bool {
    for selector, sig := range protocol.Methods {
        method := class.LookupMethod(c.selectors, selector)
        if method == nil {
            return false // class doesn't respond to this message
        }
        // Check parameter count matches
        if method.Arity() != len(sig.ParamTypes) {
            return false
        }
        // If the method has type annotations, check compatibility
        // If untyped, assume compatible (gradual typing)
    }
    return true
}
```

### What the checker verifies in Phase 1

1. **Protocol satisfaction:** When a parameter is typed `<Sizeable>` and the argument is a known class, verify the class has all methods in the Sizeable protocol.
2. **Class compatibility:** When a parameter is typed `<Integer>` and the argument is typed `<Float>`, report a warning (unless Float is a subclass of Integer or vice versa).
3. **Return type consistency:** If a method declares `^<Integer>` but the body's last expression is typed `<String>`, report a warning.
4. **Undefined protocols/types:** If a type annotation references `<Foo>` and no class or protocol named Foo exists, report a warning.

### What the checker does NOT verify in Phase 1

- No inference (every untyped thing is `<Dynamic>`, no warnings)
- No block types
- No effect types
- No refinement types
- No cross-method analysis (each method checked independently)

---

## Integration Points

### `mag typecheck` command

New subcommand in `cmd/mag/`:

```bash
mag typecheck                  # check all .mag files in project
mag typecheck src/MyClass.mag  # check specific file
mag typecheck --verbose        # show all checks, not just warnings
```

Reports warnings to stderr, exits 0 regardless (types never block).

### LSP update

The LSP server should recognize type annotations in the parsed AST for syntax awareness (e.g., not treating `<Integer>` as a broken less-than expression). Full type-on-hover and protocol mismatch diagnostics deferred to Phase 1b when the checker is stable.

### Formatter update

`mag fmt` must preserve type annotations in method signatures, temp declarations, ivar declarations, and protocol definitions. The formatter already handles the source text — it needs to recognize and format the new annotation positions.

### Guide and documentation

- Update Guide07 (classes) with typed method signature examples
- Add a new guide chapter or section on protocols and type annotations
- Update CLAUDE.md with protocol syntax and type annotation reference

### Typed hash (deferred to Phase 2)

The two-layer hash model (semantic + typed) is deferred. Phase 1 focuses on getting the syntax, AST, and checker right.

---

## Codegen: No Changes

The compiler (`compiler/codegen.go`) completely ignores type annotations. It reads the same AST nodes but skips `ParamTypes`, `ReturnType`, `TempTypes`, and `InstanceVarTypes`. Bytecode generation is unchanged. This is the zero-runtime-cost guarantee.

---

## Image Format: No Changes

Type annotations are on the AST, not on `CompiledMethod`. They're preserved in `.mag` source files and in `CompiledMethod.Source` (the raw source text). No image format changes needed.

---

## Files to Create

| File | Purpose | Est. lines |
|------|---------|-----------|
| `types/checker.go` | Main checker, AST walker | 200-300 |
| `types/protocol.go` | Protocol registry, structural matching | 150-200 |
| `types/types.go` | Type representation | 80-100 |
| `types/env.go` | Type environment | 60-80 |
| `types/checker_test.go` | Checker tests | 200-300 |
| `types/protocol_test.go` | Protocol matching tests | 100-150 |
| `cmd/mag/typecheck.go` | CLI subcommand | 50-80 |

## Files to Modify

| File | Changes | Est. lines changed |
|------|---------|-------------------|
| `compiler/ast.go` | Add TypeExpr, ProtocolDef, ProtocolEntry; extend MethodDef, ClassDef, SourceFile | +60 |
| `compiler/parser.go` | Parse type annotations, protocol defs, return types | +150-200 |
| `compiler/parser_test.go` | Tests for parsing type annotations and protocols | +100-150 |
| `compiler/token.go` | No changes (< and > already lex as binary selectors) | 0 |
| `compiler/lexer.go` | No changes | 0 |
| `cmd/mag/format.go` | Handle type annotations and protocol defs in formatter | +50-80 |
| `server/lsp.go` | Syntax-aware handling of type annotations | +30-50 |
| `lib/guide/Guide07Classes.mag` | Add typed method examples, protocol section | +30-50 |
| `CLAUDE.md` | Protocol and type annotation reference | +30-40 |

## Total estimated: ~1500-2000 new lines

---

## Resolved Decisions

1. **Return type syntax:** `^<Type>` (Strongtalk style). Avoids `->` binary selector ambiguity. Caret already means "return" in Maggie.

2. **Protocol entry syntax:** Yes, use `^<Type>` for return types in protocol entries:
   ```smalltalk
   Sizeable protocol
     size ^<Integer>.
     isEmpty ^<Boolean>.
   ```

3. **Type parameters (generics):** Deferred to Phase 2. Phase 1 uses only simple named types.

4. **Protocol file placement:** In `.mag` files alongside classes, same as traits. Protocols participate in the namespace/import system — `import: 'MyApp'` makes `MyApp::Sizeable` available as `Sizeable`. A protocol and a class cannot share a name within the same namespace.

5. **Formatter (`mag fmt`):** Must handle type annotations from day one.

6. **LSP:** Update to recognize type annotations for syntax awareness. Full type-on-hover deferred to Phase 1b when checker is stable.

7. **Guide/docs:** Update relevant guide chapters and CLAUDE.md to document the type annotation syntax and protocol definitions.

8. **Doctests referencing protocols:** Deferred to Phase 2 (requires runtime `satisfiesProtocol:` primitive).

---

## Implementation Order

1. **AST changes** — add TypeExpr, ProtocolDef, extend MethodDef/ClassDef/SourceFile
2. **Parser: type annotations** — parse `<Type>` after params, `^<Type>` return types, typed temps/ivars
3. **Parser: protocol definitions** — parse `Name protocol` with entries
4. **Parser tests** — verify parsing roundtrips for all annotation positions
5. **Formatter update** — `mag fmt` preserves type annotations and protocol definitions
6. **Type representation** — `types/types.go` with MaggieType interface
7. **Protocol registry** — `types/protocol.go` with structural matching
8. **Checker** — `types/checker.go` walking AST, checking annotations
9. **Checker tests** — protocol satisfaction, class compatibility, undefined types
10. **CLI** — `cmd/mag/typecheck.go` subcommand
11. **LSP update** — syntax-aware handling of type annotations
12. **Guide/docs** — update Guide07 (classes), CLAUDE.md with protocol and type annotation syntax
13. **Verify** — all existing tests still pass (codegen unchanged), doctests pass, Go tests pass
