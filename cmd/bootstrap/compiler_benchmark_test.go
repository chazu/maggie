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

// BenchmarkMaggieCompilerSimple benchmarks Maggie compiler on simple expressions
func BenchmarkMaggieCompilerSimple(b *testing.B) {
	vmInst := benchmarkVMWithImage(b)
	vmInst.UseGoCompiler(compiler.Compile) // Needed for Maggie compiler internals
	vmInst.UseMaggieCompiler()

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

// BenchmarkMaggieCompilerArithmetic benchmarks Maggie compiler on arithmetic expressions
func BenchmarkMaggieCompilerArithmetic(b *testing.B) {
	vmInst := benchmarkVMWithImage(b)
	vmInst.UseGoCompiler(compiler.Compile)
	vmInst.UseMaggieCompiler()

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

// BenchmarkMaggieCompilerLocalVariables benchmarks Maggie compiler with local variables
func BenchmarkMaggieCompilerLocalVariables(b *testing.B) {
	vmInst := benchmarkVMWithImage(b)
	vmInst.UseGoCompiler(compiler.Compile)
	vmInst.UseMaggieCompiler()

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

// BenchmarkMaggieCompilerBlock benchmarks Maggie compiler with blocks
func BenchmarkMaggieCompilerBlock(b *testing.B) {
	vmInst := benchmarkVMWithImage(b)
	vmInst.UseGoCompiler(compiler.Compile)
	vmInst.UseMaggieCompiler()

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

// BenchmarkMaggieCompilerMethodWithArgs benchmarks Maggie compiler with method arguments
func BenchmarkMaggieCompilerMethodWithArgs(b *testing.B) {
	vmInst := benchmarkVMWithImage(b)
	vmInst.UseGoCompiler(compiler.Compile)
	vmInst.UseMaggieCompiler()

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

// BenchmarkMaggieCompilerComplex benchmarks Maggie compiler on more complex code
func BenchmarkMaggieCompilerComplex(b *testing.B) {
	vmInst := benchmarkVMWithImage(b)
	vmInst.UseGoCompiler(compiler.Compile)
	vmInst.UseMaggieCompiler()

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

	lexerClass, ok := vmInst.Globals["Lexer"]
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

	parserClass, ok := vmInst.Globals["Parser"]
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

// BenchmarkMaggieCompilerFullPipeline measures complete Maggie compilation pipeline
func BenchmarkMaggieCompilerFullPipeline(b *testing.B) {
	vmInst := benchmarkVMWithImage(b)
	vmInst.UseGoCompiler(compiler.Compile)
	vmInst.UseMaggieCompiler()

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

// BenchmarkMaggieCompilerAllocs measures allocations in Maggie compiler
func BenchmarkMaggieCompilerAllocs(b *testing.B) {
	vmInst := benchmarkVMWithImage(b)
	vmInst.UseGoCompiler(compiler.Compile)
	vmInst.UseMaggieCompiler()

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

// TestProfilerStatsDuringCompilation measures method/block profiling during Maggie compilation
func TestProfilerStatsDuringCompilation(t *testing.T) {
	imagePath := "../../maggie.image"
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		t.Skip("maggie.image not found - run bootstrap first")
	}

	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		t.Fatalf("Failed to load maggie.image: %v", err)
	}
	vmInst.UseGoCompiler(compiler.Compile)
	vmInst.UseMaggieCompiler()

	// Compile multiple methods to exercise the interpreter
	sources := []string{
		"doIt\n    ^42",
		"add: x to: y\n    ^x + y",
		"factorial: n\n    | result i |\n    result := 1.\n    i := 1.\n    [i <= n] whileTrue: [\n        result := result * i.\n        i := i + 1\n    ].\n    ^result",
		"min: a max: b\n    ^(a < b) ifTrue: [a] ifFalse: [b]",
		"sum: list\n    | total |\n    total := 0.\n    list do: [:x | total := total + x].\n    ^total",
	}

	// Run compilations multiple times
	iterations := 10
	for iter := 0; iter < iterations; iter++ {
		for _, source := range sources {
			_, err := vmInst.Compile(source, nil)
			if err != nil {
				t.Logf("Note: Compilation error (expected for some tests): %v", err)
			}
		}
	}

	// Get profiler stats from the VM
	profiler := vmInst.GetProfiler()
	stats := profiler.Stats()

	t.Logf("=== Profiler Statistics ===")
	t.Logf("Total methods profiled: %d", stats.TotalMethods)
	t.Logf("Total blocks profiled:  %d", stats.TotalBlocks)
	t.Logf("Hot methods:            %d", stats.HotMethods)
	t.Logf("Hot blocks:             %d", stats.HotBlocks)
	t.Logf("Method invocations:     %d", stats.MethodInvocations)
	t.Logf("Block invocations:      %d", stats.BlockInvocations)
	t.Logf("Total invocations:      %d", stats.TotalInvocations)

	// Show top 5 methods
	topMethods := profiler.TopMethods(5)
	t.Logf("=== Top 5 Methods ===")
	for i, m := range topMethods {
		profile := profiler.GetMethodProfile(m)
		t.Logf("  %d. %s (invocations: %d, hot: %v)", i+1, m.String(), profile.InvocationCount, profile.IsHot)
	}

	// Show top 5 blocks
	topBlocks := profiler.TopBlocks(5)
	t.Logf("=== Top 5 Blocks ===")
	for i, b := range topBlocks {
		profile := profiler.GetBlockProfile(b)
		ownerName := "<unknown>"
		if profile.OwningMethod != nil {
			ownerName = profile.OwningMethod.String()
		}
		t.Logf("  %d. block in %s (invocations: %d, hot: %v)", i+1, ownerName, profile.InvocationCount, profile.IsHot)
	}

	// Sanity checks
	if stats.TotalMethods == 0 {
		t.Error("No methods profiled - profiler might not be instrumented correctly")
	}
	if stats.TotalInvocations == 0 {
		t.Error("No invocations recorded")
	}
}

// TestInlineCacheHitRateDuringCompilation measures IC hit rates during Maggie compilation
func TestInlineCacheHitRateDuringCompilation(t *testing.T) {
	imagePath := "../../maggie.image"
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		t.Skip("maggie.image not found - run bootstrap first")
	}

	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		t.Fatalf("Failed to load maggie.image: %v", err)
	}
	vmInst.UseGoCompiler(compiler.Compile)
	vmInst.UseMaggieCompiler()

	// Compile multiple methods to exercise the interpreter and build up IC data
	sources := []string{
		"doIt\n    ^42",
		"add: x to: y\n    ^x + y",
		"factorial: n\n    | result i |\n    result := 1.\n    i := 1.\n    [i <= n] whileTrue: [\n        result := result * i.\n        i := i + 1\n    ].\n    ^result",
		"min: a max: b\n    ^(a < b) ifTrue: [a] ifFalse: [b]",
		"sum: list\n    | total |\n    total := 0.\n    list do: [:x | total := total + x].\n    ^total",
	}

	// Run compilations multiple times to warm up caches
	iterations := 10
	for iter := 0; iter < iterations; iter++ {
		for _, source := range sources {
			_, err := vmInst.Compile(source, nil)
			if err != nil {
				t.Logf("Note: Compilation error (expected for some tests): %v", err)
			}
		}
	}

	// Collect stats
	stats := vm.CollectICStats(vmInst.Classes)

	t.Logf("=== Inline Cache Statistics ===")
	t.Logf("Total call sites:     %d", stats.TotalCallSites)
	t.Logf("  Monomorphic:        %d (%.1f%%)", stats.Monomorphic, stats.MonomorphicRate)
	t.Logf("  Polymorphic:        %d", stats.Polymorphic)
	t.Logf("  Megamorphic:        %d", stats.Megamorphic)
	t.Logf("  Empty:              %d", stats.Empty)
	t.Logf("Total hits:           %d", stats.TotalHits)
	t.Logf("Total misses:         %d", stats.TotalMisses)
	t.Logf("Hit rate:             %.2f%%", stats.HitRate)

	// Verify we have reasonable stats (sanity check)
	if stats.TotalCallSites == 0 {
		t.Error("No call sites found - IC might not be instrumented correctly")
	}

	// Check hit rate is reasonable (based on Cog VM expectation of ~90% monomorphic)
	if stats.HitRate > 0 && stats.HitRate < 50 {
		t.Logf("Warning: Hit rate (%.2f%%) is lower than expected", stats.HitRate)
	}

	// Log expected Cog VM distribution comparison
	t.Logf("=== Cog VM Expected Distribution ===")
	t.Logf("  Monomorphic: ~90%% of call sites")
	t.Logf("  Polymorphic: ~9%% of call sites")
	t.Logf("  Megamorphic: ~1%% of call sites")
}
