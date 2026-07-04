package main

import (
	"os"
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// =============================================================================
// Compiler Benchmark Infrastructure
// =============================================================================

// benchmarkVM creates a VM loaded with maggie.image for benchmarking
func benchmarkVMWithImage(b *testing.B) *vm.VM {
	imagePath := "../../maggie.image"
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		b.Skip("maggie.image not found - run bootstrap first")
	}

	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		b.Fatalf("Failed to load maggie.image: %v", err)
	}
	return vmInst
}

// =============================================================================
// Compilation Time Benchmarks
// =============================================================================

// BenchmarkGoCompilerSimple benchmarks Go compiler on simple expressions
func BenchmarkGoCompilerSimple(b *testing.B) {
	vmInst := benchmarkVMWithImage(b)
	vmInst.UseGoCompiler(compiler.Compile)

	source := "doIt\n    ^42"

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := vmInst.Compile(source, nil)
		if err != nil {
			b.Fatalf("Compile failed: %v", err)
		}
	}
}

// BenchmarkGoCompilerArithmetic benchmarks Go compiler on arithmetic expressions
func BenchmarkGoCompilerArithmetic(b *testing.B) {
	vmInst := benchmarkVMWithImage(b)
	vmInst.UseGoCompiler(compiler.Compile)

	source := "doIt\n    ^3 + 4 * 5 - 2"

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := vmInst.Compile(source, nil)
		if err != nil {
			b.Fatalf("Compile failed: %v", err)
		}
	}
}

// BenchmarkGoCompilerLocalVariables benchmarks Go compiler with local variables
func BenchmarkGoCompilerLocalVariables(b *testing.B) {
	vmInst := benchmarkVMWithImage(b)
	vmInst.UseGoCompiler(compiler.Compile)

	source := `doIt
    | x y z |
    x := 10.
    y := 20.
    z := x + y.
    ^z`

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := vmInst.Compile(source, nil)
		if err != nil {
			b.Fatalf("Compile failed: %v", err)
		}
	}
}

// BenchmarkGoCompilerBlock benchmarks Go compiler with blocks
func BenchmarkGoCompilerBlock(b *testing.B) {
	vmInst := benchmarkVMWithImage(b)
	vmInst.UseGoCompiler(compiler.Compile)

	source := `doIt
    ^(3 < 5) ifTrue: [1] ifFalse: [0]`

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := vmInst.Compile(source, nil)
		if err != nil {
			b.Fatalf("Compile failed: %v", err)
		}
	}
}

// BenchmarkGoCompilerMethodWithArgs benchmarks Go compiler with method arguments
func BenchmarkGoCompilerMethodWithArgs(b *testing.B) {
	vmInst := benchmarkVMWithImage(b)
	vmInst.UseGoCompiler(compiler.Compile)

	source := `add: x to: y
    ^x + y`

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := vmInst.Compile(source, nil)
		if err != nil {
			b.Fatalf("Compile failed: %v", err)
		}
	}
}

// BenchmarkGoCompilerComplex benchmarks Go compiler on more complex code
func BenchmarkGoCompilerComplex(b *testing.B) {
	vmInst := benchmarkVMWithImage(b)
	vmInst.UseGoCompiler(compiler.Compile)

	source := `factorial: n
    | result i |
    result := 1.
    i := 1.
    [i <= n] whileTrue: [
        result := result * i.
        i := i + 1
    ].
    ^result`

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := vmInst.Compile(source, nil)
		if err != nil {
			b.Fatalf("Compile failed: %v", err)
		}
	}
}

// =============================================================================
// Lexer Benchmarks
// =============================================================================

// BenchmarkMaggieLexer benchmarks the Maggie lexer tokenization
func BenchmarkMaggieLexer(b *testing.B) {
	vmInst := benchmarkVMWithImage(b)
	vmInst.UseGoCompiler(compiler.Compile)

	lexerClass, ok := vmInst.Global("Lexer")
	if !ok {
		b.Fatal("Lexer class not found")
	}

	source := vmInst.Registry().NewStringValue("factorial: n | result i | result := 1. i := 1. [i <= n] whileTrue: [result := result * i. i := i + 1]. ^result")

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		lexer := vmInst.Send(lexerClass, "on:", []vm.Value{source})
		for {
			token := vmInst.Send(lexer, "nextToken", nil)
			if token == vm.Nil {
				break
			}
			tokenType := vmInst.Send(token, "type", nil)
			if tokenType.IsSymbol() && vmInst.Symbols.Name(tokenType.SymbolID()) == "eof" {
				break
			}
		}
	}
}

// BenchmarkGoLexer benchmarks the Go lexer tokenization
func BenchmarkGoLexer(b *testing.B) {
	source := "factorial: n | result i | result := 1. i := 1. [i <= n] whileTrue: [result := result * i. i := i + 1]. ^result"

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		lexer := compiler.NewLexer(source)
		for {
			tok := lexer.NextToken()
			if tok.Type == compiler.TokenEOF {
				break
			}
		}
	}
}

// =============================================================================
// Parser Benchmarks
// =============================================================================

// BenchmarkMaggieParser benchmarks the Maggie parser
func BenchmarkMaggieParser(b *testing.B) {
	vmInst := benchmarkVMWithImage(b)
	vmInst.UseGoCompiler(compiler.Compile)

	parserClass, ok := vmInst.Global("Parser")
	if !ok {
		b.Fatal("Parser class not found")
	}

	source := vmInst.Registry().NewStringValue(`factorial: n
    | result i |
    result := 1.
    i := 1.
    [i <= n] whileTrue: [
        result := result * i.
        i := i + 1
    ].
    ^result`)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		parser := vmInst.Send(parserClass, "on:", []vm.Value{source})
		vmInst.Send(parser, "parseMethod", nil)
	}
}

// BenchmarkGoParser benchmarks the Go parser
func BenchmarkGoParser(b *testing.B) {
	source := `factorial: n
    | result i |
    result := 1.
    i := 1.
    [i <= n] whileTrue: [
        result := result * i.
        i := i + 1
    ].
    ^result`

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		parser := compiler.NewParser(source)
		_ = parser.ParseMethod()
	}
}

// =============================================================================
// Combined Parsing + Code Generation Benchmarks
// =============================================================================

// BenchmarkGoCompilerFullPipeline measures complete compilation pipeline
func BenchmarkGoCompilerFullPipeline(b *testing.B) {
	vmInst := benchmarkVMWithImage(b)
	vmInst.UseGoCompiler(compiler.Compile)

	sources := []string{
		"doIt\n    ^42",
		"add: x to: y\n    ^x + y",
		"factorial: n\n    | result i |\n    result := 1.\n    i := 1.\n    [i <= n] whileTrue: [\n        result := result * i.\n        i := i + 1\n    ].\n    ^result",
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		for _, source := range sources {
			_, err := vmInst.Compile(source, nil)
			if err != nil {
				b.Fatalf("Compile failed: %v", err)
			}
		}
	}
}

// =============================================================================
// Memory Allocation Benchmarks
// =============================================================================

// BenchmarkGoCompilerAllocs measures allocations in Go compiler
func BenchmarkGoCompilerAllocs(b *testing.B) {
	vmInst := benchmarkVMWithImage(b)
	vmInst.UseGoCompiler(compiler.Compile)

	source := `factorial: n
    | result i |
    result := 1.
    i := 1.
    [i <= n] whileTrue: [
        result := result * i.
        i := i + 1
    ].
    ^result`

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = vmInst.Compile(source, nil)
	}
}

// =============================================================================
// Inline Cache Statistics
// =============================================================================
