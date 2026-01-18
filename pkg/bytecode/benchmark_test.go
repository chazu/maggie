// Package bytecode benchmarks
//
// These benchmarks measure the performance of:
// - Bytecode compilation
// - VM execution
// - Serialization/deserialization
// - Various operations (arithmetic, loops, etc.)
//
// Run: go test -bench=. ./pkg/bytecode/...
// Run with memory stats: go test -bench=. -benchmem ./pkg/bytecode/...
package bytecode

import (
	"testing"

	"github.com/chazu/procyon/pkg/parser"
)

// ============================================================
// Compilation Benchmarks
// ============================================================

// BenchmarkCompileSimple measures compilation of a simple block
func BenchmarkCompileSimple(b *testing.B) {
	block := &parser.BlockExpr{
		Params: []string{"x"},
		Statements: []parser.Statement{
			&parser.Return{
				Value: &parser.BinaryExpr{
					Op:    "+",
					Left:  &parser.Identifier{Name: "x"},
					Right: &parser.NumberLit{Value: "1"},
				},
			},
		},
	}

	ctx := &CompilerContext{}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = CompileBlock(block, ctx)
	}
}

// BenchmarkCompileComplex measures compilation of a complex block with loops
func BenchmarkCompileComplex(b *testing.B) {
	block := &parser.BlockExpr{
		Params: []string{"n"},
		Statements: []parser.Statement{
			&parser.Assignment{Target: "sum", Value: &parser.NumberLit{Value: "0"}},
			&parser.Assignment{Target: "i", Value: &parser.NumberLit{Value: "0"}},
			&parser.WhileExpr{
				Condition: &parser.ComparisonExpr{
					Op:    "<",
					Left:  &parser.Identifier{Name: "i"},
					Right: &parser.Identifier{Name: "n"},
				},
				Body: []parser.Statement{
					&parser.Assignment{
						Target: "sum",
						Value: &parser.BinaryExpr{
							Op:    "+",
							Left:  &parser.Identifier{Name: "sum"},
							Right: &parser.Identifier{Name: "i"},
						},
					},
					&parser.Assignment{
						Target: "i",
						Value: &parser.BinaryExpr{
							Op:    "+",
							Left:  &parser.Identifier{Name: "i"},
							Right: &parser.NumberLit{Value: "1"},
						},
					},
				},
			},
			&parser.Return{Value: &parser.Identifier{Name: "sum"}},
		},
	}

	ctx := &CompilerContext{}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = CompileBlock(block, ctx)
	}
}

// BenchmarkCompileConditional measures compilation of conditionals
func BenchmarkCompileConditional(b *testing.B) {
	block := &parser.BlockExpr{
		Params: []string{"x"},
		Statements: []parser.Statement{
			&parser.IfExpr{
				Condition: &parser.ComparisonExpr{
					Op:    ">",
					Left:  &parser.Identifier{Name: "x"},
					Right: &parser.NumberLit{Value: "0"},
				},
				TrueBlock: []parser.Statement{
					&parser.Return{Value: &parser.StringLit{Value: "positive"}},
				},
				FalseBlock: []parser.Statement{
					&parser.Return{Value: &parser.StringLit{Value: "non-positive"}},
				},
			},
		},
	}

	ctx := &CompilerContext{}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = CompileBlock(block, ctx)
	}
}

// ============================================================
// Execution Benchmarks
// ============================================================

// BenchmarkExecuteAddition measures execution of simple addition
func BenchmarkExecuteAddition(b *testing.B) {
	block := &parser.BlockExpr{
		Params: []string{"x", "y"},
		Statements: []parser.Statement{
			&parser.Return{
				Value: &parser.BinaryExpr{
					Op:    "+",
					Left:  &parser.Identifier{Name: "x"},
					Right: &parser.Identifier{Name: "y"},
				},
			},
		},
	}

	chunk, _ := CompileBlock(block, &CompilerContext{})
	args := []string{"10", "20"}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm := NewVM()
		_, _ = vm.Execute(chunk, "", args)
	}
}

// BenchmarkExecuteArithmetic measures execution of complex arithmetic
func BenchmarkExecuteArithmetic(b *testing.B) {
	// (a + b) * (c - d)
	block := &parser.BlockExpr{
		Params: []string{"a", "b", "c", "d"},
		Statements: []parser.Statement{
			&parser.Return{
				Value: &parser.BinaryExpr{
					Op: "*",
					Left: &parser.BinaryExpr{
						Op:    "+",
						Left:  &parser.Identifier{Name: "a"},
						Right: &parser.Identifier{Name: "b"},
					},
					Right: &parser.BinaryExpr{
						Op:    "-",
						Left:  &parser.Identifier{Name: "c"},
						Right: &parser.Identifier{Name: "d"},
					},
				},
			},
		},
	}

	chunk, _ := CompileBlock(block, &CompilerContext{})
	args := []string{"10", "20", "100", "50"}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm := NewVM()
		_, _ = vm.Execute(chunk, "", args)
	}
}

// BenchmarkExecuteLoop10 measures execution of a loop with 10 iterations
func BenchmarkExecuteLoop10(b *testing.B) {
	block := createSumBlock()
	chunk, _ := CompileBlock(block, &CompilerContext{})
	args := []string{"10"}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm := NewVM()
		_, _ = vm.Execute(chunk, "", args)
	}
}

// BenchmarkExecuteLoop100 measures execution of a loop with 100 iterations
func BenchmarkExecuteLoop100(b *testing.B) {
	block := createSumBlock()
	chunk, _ := CompileBlock(block, &CompilerContext{})
	args := []string{"100"}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm := NewVM()
		_, _ = vm.Execute(chunk, "", args)
	}
}

// BenchmarkExecuteLoop1000 measures execution of a loop with 1000 iterations
func BenchmarkExecuteLoop1000(b *testing.B) {
	block := createSumBlock()
	chunk, _ := CompileBlock(block, &CompilerContext{})
	args := []string{"1000"}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm := NewVM()
		_, _ = vm.Execute(chunk, "", args)
	}
}

// BenchmarkExecuteConditional measures execution of conditionals
func BenchmarkExecuteConditional(b *testing.B) {
	block := &parser.BlockExpr{
		Params: []string{"x"},
		Statements: []parser.Statement{
			&parser.IfExpr{
				Condition: &parser.ComparisonExpr{
					Op:    ">",
					Left:  &parser.Identifier{Name: "x"},
					Right: &parser.NumberLit{Value: "0"},
				},
				TrueBlock: []parser.Statement{
					&parser.Return{Value: &parser.NumberLit{Value: "1"}},
				},
				FalseBlock: []parser.Statement{
					&parser.Return{Value: &parser.NumberLit{Value: "0"}},
				},
			},
		},
	}

	chunk, _ := CompileBlock(block, &CompilerContext{})

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm := NewVM()
		// Alternate between true and false branches
		if i%2 == 0 {
			_, _ = vm.Execute(chunk, "", []string{"5"})
		} else {
			_, _ = vm.Execute(chunk, "", []string{"-5"})
		}
	}
}

// BenchmarkExecuteLocalVars measures execution with multiple local variables
func BenchmarkExecuteLocalVars(b *testing.B) {
	block := &parser.BlockExpr{
		Params: []string{"x"},
		Statements: []parser.Statement{
			&parser.Assignment{Target: "a", Value: &parser.BinaryExpr{Op: "+", Left: &parser.Identifier{Name: "x"}, Right: &parser.NumberLit{Value: "1"}}},
			&parser.Assignment{Target: "b", Value: &parser.BinaryExpr{Op: "*", Left: &parser.Identifier{Name: "a"}, Right: &parser.NumberLit{Value: "2"}}},
			&parser.Assignment{Target: "c", Value: &parser.BinaryExpr{Op: "+", Left: &parser.Identifier{Name: "b"}, Right: &parser.Identifier{Name: "a"}}},
			&parser.Return{Value: &parser.Identifier{Name: "c"}},
		},
	}

	chunk, _ := CompileBlock(block, &CompilerContext{})
	args := []string{"10"}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm := NewVM()
		_, _ = vm.Execute(chunk, "", args)
	}
}

// ============================================================
// Serialization Benchmarks
// ============================================================

// BenchmarkSerializeSmall measures serialization of a small chunk
func BenchmarkSerializeSmall(b *testing.B) {
	block := &parser.BlockExpr{
		Params: []string{"x"},
		Statements: []parser.Statement{
			&parser.Return{
				Value: &parser.BinaryExpr{
					Op:    "+",
					Left:  &parser.Identifier{Name: "x"},
					Right: &parser.NumberLit{Value: "1"},
				},
			},
		},
	}

	chunk, _ := CompileBlock(block, &CompilerContext{})

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = chunk.Serialize()
	}
}

// BenchmarkSerializeLarge measures serialization of a larger chunk
func BenchmarkSerializeLarge(b *testing.B) {
	block := createSumBlock()
	chunk, _ := CompileBlock(block, &CompilerContext{})

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = chunk.Serialize()
	}
}

// BenchmarkDeserializeSmall measures deserialization of a small chunk
func BenchmarkDeserializeSmall(b *testing.B) {
	block := &parser.BlockExpr{
		Params: []string{"x"},
		Statements: []parser.Statement{
			&parser.Return{
				Value: &parser.BinaryExpr{
					Op:    "+",
					Left:  &parser.Identifier{Name: "x"},
					Right: &parser.NumberLit{Value: "1"},
				},
			},
		},
	}

	chunk, _ := CompileBlock(block, &CompilerContext{})
	data, _ := chunk.Serialize()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = Deserialize(data)
	}
}

// BenchmarkDeserializeLarge measures deserialization of a larger chunk
func BenchmarkDeserializeLarge(b *testing.B) {
	block := createSumBlock()
	chunk, _ := CompileBlock(block, &CompilerContext{})
	data, _ := chunk.Serialize()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = Deserialize(data)
	}
}

// BenchmarkSerializeRoundTrip measures full serialize/deserialize cycle
func BenchmarkSerializeRoundTrip(b *testing.B) {
	block := &parser.BlockExpr{
		Params: []string{"x", "y"},
		Statements: []parser.Statement{
			&parser.Return{
				Value: &parser.BinaryExpr{
					Op:    "+",
					Left:  &parser.Identifier{Name: "x"},
					Right: &parser.Identifier{Name: "y"},
				},
			},
		},
	}

	chunk, _ := CompileBlock(block, &CompilerContext{})

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		data, _ := chunk.Serialize()
		_, _ = Deserialize(data)
	}
}

// ============================================================
// Helper Functions
// ============================================================

// createSumBlock creates a block that computes sum(1..n)
func createSumBlock() *parser.BlockExpr {
	return &parser.BlockExpr{
		Params: []string{"input"},
		Statements: []parser.Statement{
			&parser.Assignment{Target: "n", Value: &parser.Identifier{Name: "input"}},
			&parser.Assignment{Target: "sum", Value: &parser.NumberLit{Value: "0"}},
			&parser.WhileExpr{
				Condition: &parser.ComparisonExpr{
					Op:    ">",
					Left:  &parser.Identifier{Name: "n"},
					Right: &parser.NumberLit{Value: "0"},
				},
				Body: []parser.Statement{
					&parser.Assignment{
						Target: "sum",
						Value:  &parser.BinaryExpr{Op: "+", Left: &parser.Identifier{Name: "sum"}, Right: &parser.Identifier{Name: "n"}},
					},
					&parser.Assignment{
						Target: "n",
						Value:  &parser.BinaryExpr{Op: "-", Left: &parser.Identifier{Name: "n"}, Right: &parser.NumberLit{Value: "1"}},
					},
				},
			},
			&parser.Return{Value: &parser.Identifier{Name: "sum"}},
		},
	}
}

// ============================================================
// Comparison Benchmarks (for reference)
// ============================================================

// BenchmarkStringConcat measures string concatenation performance
func BenchmarkStringConcat(b *testing.B) {
	block := &parser.BlockExpr{
		Params: []string{"a", "b"},
		Statements: []parser.Statement{
			&parser.Return{
				Value: &parser.BinaryExpr{
					Op:    ",",
					Left:  &parser.Identifier{Name: "a"},
					Right: &parser.Identifier{Name: "b"},
				},
			},
		},
	}

	chunk, _ := CompileBlock(block, &CompilerContext{})
	args := []string{"hello", "world"}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm := NewVM()
		_, _ = vm.Execute(chunk, "", args)
	}
}

// BenchmarkVMReuse measures performance when reusing the same VM
func BenchmarkVMReuse(b *testing.B) {
	block := &parser.BlockExpr{
		Params: []string{"x"},
		Statements: []parser.Statement{
			&parser.Return{
				Value: &parser.BinaryExpr{
					Op:    "+",
					Left:  &parser.Identifier{Name: "x"},
					Right: &parser.NumberLit{Value: "1"},
				},
			},
		},
	}

	chunk, _ := CompileBlock(block, &CompilerContext{})
	vm := NewVM()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = vm.Execute(chunk, "", []string{"10"})
	}
}
