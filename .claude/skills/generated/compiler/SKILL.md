---
name: compiler
description: "Skill for the Compiler area of maggie. 301 symbols across 22 files."
---

# Compiler

301 symbols | 22 files | Cohesion: 84%

## When to Use

- Working with code in `compiler/`
- Understanding how TestParseUnaryMethodWithReturnType, TestParseBinaryMethodWithTypes, TestParseKeywordMethodWithTypes work
- Modifying compiler-related functionality

## Key Files

| File | Symbols |
|------|---------|
| `compiler/parser_test.go` | TestParserLiterals, TestParserVariables, TestParserUnaryMessage, TestParserUnaryChain, TestParserBinaryMessage (+52) |
| `compiler/parser.go` | NewParser, Errors, Warnings, ParseExpression, ParseMethod (+49) |
| `compiler/codegen.go` | Compile, CompileExpr, CompileMethodDef, errorf, emitSourcePos (+25) |
| `compiler/codegen_test.go` | newTestRegistry, TestCompileInteger, TestCompileNegativeInteger, TestCompileFloat, TestCompileNil (+25) |
| `compiler/lexer_test.go` | TestLexerBasicTokens, TestLexerIntegers, TestLexerFloats, TestLexerStrings, TestLexerSymbols (+23) |
| `compiler/type_annotation_test.go` | TestParseUnaryMethodWithReturnType, TestParseBinaryMethodWithTypes, TestParseKeywordMethodWithTypes, TestParseKeywordMethodPartialTypes, TestParseMethodNoTypes (+15) |
| `compiler/lexer.go` | NewLexer, position, NextToken, Tokenize, readChar (+15) |
| `compiler/semantic.go` | NewSemanticAnalyzer, defaultKnownGlobals, AddKnownGlobal, SetInstanceVars, Errors (+11) |
| `compiler/peephole_test.go` | TestConstantFoldIntAdd, TestConstantFoldIntMul, TestConstantFoldOverflowToInt32, TestConstantFoldDivByZeroSkipped, TestConstantFoldChained (+7) |
| `compiler/semantic_test.go` | TestSemanticAnalyzer_UndefinedVariable, TestSemanticAnalyzer_DefinedVariable, TestSemanticAnalyzer_InstanceVariable, TestSemanticAnalyzer_KnownGlobals, TestSemanticAnalyzer_UnreachableCode (+3) |

## Entry Points

Start here when exploring this area:

- **`TestParseUnaryMethodWithReturnType`** (Function) — `compiler/type_annotation_test.go:10`
- **`TestParseBinaryMethodWithTypes`** (Function) — `compiler/type_annotation_test.go:28`
- **`TestParseKeywordMethodWithTypes`** (Function) — `compiler/type_annotation_test.go:49`
- **`TestParseKeywordMethodPartialTypes`** (Function) — `compiler/type_annotation_test.go:73`
- **`TestParseMethodNoTypes`** (Function) — `compiler/type_annotation_test.go:92`

## Key Symbols

| Symbol | Type | File | Line |
|--------|------|------|------|
| `TestParseUnaryMethodWithReturnType` | Function | `compiler/type_annotation_test.go` | 10 |
| `TestParseBinaryMethodWithTypes` | Function | `compiler/type_annotation_test.go` | 28 |
| `TestParseKeywordMethodWithTypes` | Function | `compiler/type_annotation_test.go` | 49 |
| `TestParseKeywordMethodPartialTypes` | Function | `compiler/type_annotation_test.go` | 73 |
| `TestParseMethodNoTypes` | Function | `compiler/type_annotation_test.go` | 92 |
| `TestParseTypedTemporaries` | Function | `compiler/type_annotation_test.go` | 114 |
| `TestParseSelfReturnType` | Function | `compiler/type_annotation_test.go` | 135 |
| `TestParseDynamicType` | Function | `compiler/type_annotation_test.go` | 147 |
| `TestParseProtocolDefinition` | Function | `compiler/type_annotation_test.go` | 166 |
| `TestParseProtocolWithKeywordEntry` | Function | `compiler/type_annotation_test.go` | 204 |
| `TestParseProtocolWithIncludes` | Function | `compiler/type_annotation_test.go` | 239 |
| `TestParseProtocolWithDocstring` | Function | `compiler/type_annotation_test.go` | 258 |
| `TestParseSourceFileWithProtocolAndClass` | Function | `compiler/type_annotation_test.go` | 277 |
| `TestParseEffectSinglePure` | Function | `compiler/type_annotation_test.go` | 320 |
| `TestParseEffectMultiple` | Function | `compiler/type_annotation_test.go` | 338 |
| `TestParseEffectNoAnnotation` | Function | `compiler/type_annotation_test.go` | 359 |
| `TestParseEffectInBrackets` | Function | `compiler/type_annotation_test.go` | 371 |
| `TestParseEffectOnProtocolEntry` | Function | `compiler/type_annotation_test.go` | 391 |
| `TestParseEffectWithReturnTypeAndTypes` | Function | `compiler/type_annotation_test.go` | 410 |
| `TestPrimitiveStubStillWorks` | Function | `compiler/type_annotation_test.go` | 431 |

## Execution Flows

| Flow | Type | Steps |
|------|------|-------|
| `CompileAllFiles → Lexer` | cross_community | 5 |
| `CompileAllFiles → ReadChar` | cross_community | 5 |
| `CompileAllFiles → CurTokenIs` | cross_community | 5 |
| `CompileAllFiles → Errorf` | cross_community | 5 |
| `CompileAllFiles → NamespaceDecl` | cross_community | 5 |
| `CompileAllFiles → SetInstanceVars` | cross_community | 5 |
| `Main → Parser` | cross_community | 5 |
| `Main → SourceFile` | cross_community | 5 |
| `Main → CurTokenIs` | cross_community | 5 |
| `BuildCompileFunc → Token` | cross_community | 5 |

## Connected Areas

| Area | Connections |
|------|-------------|
| Vm | 103 calls |
| Server | 3 calls |
| Types | 1 calls |
| Hash | 1 calls |

## How to Explore

1. `gitnexus_context({name: "TestParseUnaryMethodWithReturnType"})` — see callers and callees
2. `gitnexus_query({query: "compiler"})` — find related execution flows
3. Read key files listed above for implementation details
