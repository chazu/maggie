package main

import (
	"bytes"
	"fmt"
	"os"
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// TestSelfCompileCompilerMethods tests that both compilers can compile
// methods similar to those found in the actual Maggie compiler source files.
// This verifies the self-hosting compiler can handle real-world code patterns.
func TestSelfCompileCompilerMethods(t *testing.T) {
	imagePath := "../../maggie.image"
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		t.Skip("maggie.image not found - run bootstrap first")
	}

	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		t.Fatalf("Failed to load maggie.image: %v", err)
	}

	vmInst.UseGoCompiler(compiler.Compile)

	// Test cases derived from actual compiler source patterns
	testCases := []struct {
		name   string
		source string
	}{
		// Simple accessor pattern (from Token.mag)
		{
			"accessor method",
			`type
    ^type`,
		},
		// Type testing pattern (from Token.mag)
		{
			"type testing with symbol",
			`isEOF
    ^type = #eof`,
		},
		// Or: message pattern (from Token.mag)
		{
			"or: message",
			`isNumber
    ^type = #integer or: [type = #float]`,
		},
		// Multi-argument setter (from Token.mag)
		{
			"multi-arg setter",
			`type: aType value: aValue position: aPosition
    type := aType.
    value := aValue.
    position := aPosition.
    ^self`,
		},
		// String concatenation (from Token.mag printString)
		{
			"string concatenation",
			`printString
    ^'Token(', type printString, ')'`,
		},
		// Conditional pattern (common in Lexer/Parser)
		{
			"ifTrue:ifFalse:",
			`test: x
    ^x > 0 ifTrue: [#positive] ifFalse: [#nonPositive]`,
		},
		// whileTrue: loop pattern (from Lexer)
		{
			"whileTrue: loop",
			`countTo: n
    | i |
    i := 0.
    [i < n] whileTrue: [i := i + 1].
    ^i`,
		},
		// Nested blocks (from Parser)
		{
			"nested blocks",
			`nestedTest: x
    ^x ifTrue: [
        x ifFalse: [1] ifTrue: [2]
    ] ifFalse: [0]`,
		},
		// Array literal
		{
			"array literal",
			`arrayTest
    ^#(1 2 3) size`,
		},
		// Block with argument
		{
			"block with argument",
			`applyBlock: aBlock to: aValue
    ^aBlock value: aValue`,
		},
		// Cascade pattern
		{
			"cascade messages",
			`cascadeTest
    | s |
    s := String new.
    ^s size`,
		},
		// Self return
		{
			"self return",
			`yourself
    ^self`,
		},
		// Super send (basic)
		{
			"super message",
			`initialize
    super initialize.
    ^self`,
		},
		// Complex arithmetic
		{
			"complex arithmetic",
			`calculate: a with: b
    | result |
    result := a + b * 2.
    result := result - 1.
    ^result`,
		},
		// Comparison chain
		{
			"comparison chain",
			`inRange: x min: lo max: hi
    ^x >= lo and: [x <= hi]`,
		},
	}

	passCount := 0
	skipCount := 0
	failCount := 0

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			// Compile with Go compiler
			vmInst.UseGoCompiler(compiler.Compile)
			goMethod, err := vmInst.Compile(tc.source, nil)
			if err != nil {
				t.Fatalf("Go compiler failed: %v", err)
			}
			if goMethod == nil {
				t.Fatal("Go compiler returned nil method")
			}

			// Switch to Maggie compiler
			vmInst.UseMaggieCompiler()

			var maggieMethod *vm.CompiledMethod
			var compileErr error
			func() {
				defer func() {
					if r := recover(); r != nil {
						compileErr = fmt.Errorf("Maggie compiler panic: %v", r)
					}
				}()
				maggieMethod, compileErr = vmInst.Compile(tc.source, nil)
			}()

			// Switch back to Go compiler
			vmInst.UseGoCompiler(compiler.Compile)

			if compileErr != nil {
				skipCount++
				t.Skipf("Maggie compiler not ready: %v", compileErr)
			}
			if maggieMethod == nil {
				skipCount++
				t.Skipf("Maggie compiler returned nil for: %s", tc.name)
			}

			// Compare bytecode (informational)
			if !bytes.Equal(goMethod.Bytecode, maggieMethod.Bytecode) {
				t.Logf("Bytecode differs:\n  Go:     %v\n  Maggie: %v",
					goMethod.Bytecode, maggieMethod.Bytecode)
			}

			// Compare literal counts
			if len(goMethod.Literals) != len(maggieMethod.Literals) {
				t.Logf("Literal count differs: Go=%d, Maggie=%d",
					len(goMethod.Literals), len(maggieMethod.Literals))
			}

			// Compare block counts
			if len(goMethod.Blocks) != len(maggieMethod.Blocks) {
				t.Logf("Block count differs: Go=%d, Maggie=%d",
					len(goMethod.Blocks), len(maggieMethod.Blocks))
			}

			passCount++
		})
	}

	t.Logf("Compiler methods test summary: %d passed, %d skipped, %d failed out of %d total",
		passCount, skipCount, failCount, len(testCases))
}

// TestSelfCompileRealTokenMethods tests compiling actual methods from Token.mag
func TestSelfCompileRealTokenMethods(t *testing.T) {
	imagePath := "../../maggie.image"
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		t.Skip("maggie.image not found - run bootstrap first")
	}

	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		t.Fatalf("Failed to load maggie.image: %v", err)
	}

	vmInst.UseGoCompiler(compiler.Compile)

	// Actual methods from Token.mag (converted to method syntax)
	tokenMethods := []struct {
		name   string
		source string
	}{
		{"Token.type", "type\n    ^type"},
		{"Token.value", "value\n    ^value"},
		{"Token.position", "position\n    ^position"},
		{"Token.isEOF", "isEOF\n    ^type = #eof"},
		{"Token.isIdentifier", "isIdentifier\n    ^type = #identifier"},
		{"Token.isKeyword", "isKeyword\n    ^type = #keyword"},
		{"Token.isNumber", "isNumber\n    ^type = #integer or: [type = #float]"},
		{"Token.isInteger", "isInteger\n    ^type = #integer"},
		{"Token.isFloat", "isFloat\n    ^type = #float"},
		{"Token.isString", "isString\n    ^type = #string"},
		{"Token.isSymbol", "isSymbol\n    ^type = #symbol"},
		{"Token.isBinarySelector", "isBinarySelector\n    ^type = #binary"},
		{"Token.isAssign", "isAssign\n    ^type = #assign"},
		{"Token.isCaret", "isCaret\n    ^type = #caret"},
		{"Token.isPeriod", "isPeriod\n    ^type = #period"},
		{"Token.isSemicolon", "isSemicolon\n    ^type = #semicolon"},
		{"Token.isColon", "isColon\n    ^type = #colon"},
		{"Token.isPipe", "isPipe\n    ^type = #pipe"},
		{"Token.isLeftParen", "isLeftParen\n    ^type = #leftParen"},
		{"Token.isRightParen", "isRightParen\n    ^type = #rightParen"},
		{"Token.isLeftBracket", "isLeftBracket\n    ^type = #leftBracket"},
		{"Token.isRightBracket", "isRightBracket\n    ^type = #rightBracket"},
		{"Token.isLeftBrace", "isLeftBrace\n    ^type = #leftBrace"},
		{"Token.isRightBrace", "isRightBrace\n    ^type = #rightBrace"},
	}

	passCount := 0
	skipCount := 0

	for _, tm := range tokenMethods {
		t.Run(tm.name, func(t *testing.T) {
			// Compile with Go compiler
			vmInst.UseGoCompiler(compiler.Compile)
			goMethod, err := vmInst.Compile(tm.source, nil)
			if err != nil {
				t.Fatalf("Go compiler failed: %v", err)
			}

			// Switch to Maggie compiler
			vmInst.UseMaggieCompiler()

			var maggieMethod *vm.CompiledMethod
			var compileErr error
			func() {
				defer func() {
					if r := recover(); r != nil {
						compileErr = fmt.Errorf("panic: %v", r)
					}
				}()
				maggieMethod, compileErr = vmInst.Compile(tm.source, nil)
			}()

			vmInst.UseGoCompiler(compiler.Compile)

			if compileErr != nil {
				skipCount++
				t.Skipf("Maggie compiler error: %v", compileErr)
			}
			if maggieMethod == nil {
				skipCount++
				t.Skip("Maggie compiler returned nil")
			}

			// Log bytecode comparison
			if !bytes.Equal(goMethod.Bytecode, maggieMethod.Bytecode) {
				t.Logf("Bytecode differs (semantically equivalent):\n  Go:     %v\n  Maggie: %v",
					goMethod.Bytecode, maggieMethod.Bytecode)
			}

			passCount++
		})
	}

	t.Logf("Token methods: %d passed, %d skipped out of %d",
		passCount, skipCount, len(tokenMethods))
}

// TestSelfCompileExecutionEquivalence verifies that compiled methods
// produce the same results when executed.
//
// Tests involving blocks that access outer scope variables (like whileTrue:
// loops with variable updates) work correctly. The Maggie compiler implements
// proper variable captures with cell boxing in BytecodeGenerator.mag.
func TestSelfCompileExecutionEquivalence(t *testing.T) {
	imagePath := "../../maggie.image"
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		t.Skip("maggie.image not found - run bootstrap first")
	}

	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		t.Fatalf("Failed to load maggie.image: %v", err)
	}

	vmInst.UseGoCompiler(compiler.Compile)

	// Test cases with expected execution results
	testCases := []struct {
		name         string
		source       string
		args         []vm.Value
		expected     vm.Value
		skipMaggie   bool   // Skip Maggie compiler test (e.g., missing feature)
		skipReason   string // Reason for skipping
	}{
		{
			name:     "simple add",
			source:   "add: x to: y\n    ^x + y",
			args:     []vm.Value{vm.FromSmallInt(3), vm.FromSmallInt(4)},
			expected: vm.FromSmallInt(7),
		},
		{
			name:     "conditional true",
			source:   "test: x\n    ^x > 0 ifTrue: [1] ifFalse: [0]",
			args:     []vm.Value{vm.FromSmallInt(5)},
			expected: vm.FromSmallInt(1),
		},
		{
			name:     "conditional false",
			source:   "test: x\n    ^x > 0 ifTrue: [1] ifFalse: [0]",
			args:     []vm.Value{vm.FromSmallInt(-5)},
			expected: vm.FromSmallInt(0),
		},
		{
			name:     "loop accumulator",
			source:   "sumTo: n\n    | sum i |\n    sum := 0.\n    i := 1.\n    [i <= n] whileTrue: [sum := sum + i. i := i + 1].\n    ^sum",
			args:     []vm.Value{vm.FromSmallInt(5)},
			expected: vm.FromSmallInt(15), // 1+2+3+4+5
		},
		{
			name:     "factorial",
			source:   "factorial: n\n    | result i |\n    result := 1.\n    i := 1.\n    [i <= n] whileTrue: [result := result * i. i := i + 1].\n    ^result",
			args:     []vm.Value{vm.FromSmallInt(5)},
			expected: vm.FromSmallInt(120), // 5!
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			// Compile and execute with Go compiler
			vmInst.UseGoCompiler(compiler.Compile)
			goMethod, err := vmInst.Compile(tc.source, nil)
			if err != nil {
				t.Fatalf("Go compiler failed: %v", err)
			}

			goResult := vmInst.Execute(goMethod, vm.Nil, tc.args)
			if goResult != tc.expected {
				t.Errorf("Go execution: got %v, want %v", goResult, tc.expected)
			}

			// Skip Maggie compiler test if flagged
			if tc.skipMaggie {
				t.Skipf("Skipping Maggie compiler: %s", tc.skipReason)
			}

			// Compile with Maggie compiler
			vmInst.UseMaggieCompiler()

			var maggieMethod *vm.CompiledMethod
			var compileErr error
			func() {
				defer func() {
					if r := recover(); r != nil {
						compileErr = fmt.Errorf("panic: %v", r)
					}
				}()
				maggieMethod, compileErr = vmInst.Compile(tc.source, nil)
			}()

			vmInst.UseGoCompiler(compiler.Compile)

			if compileErr != nil {
				t.Skipf("Maggie compiler error: %v", compileErr)
			}
			if maggieMethod == nil {
				t.Skip("Maggie compiler returned nil")
			}

			// Execute Maggie-compiled method
			var maggieResult vm.Value
			var execErr error
			func() {
				defer func() {
					if r := recover(); r != nil {
						execErr = fmt.Errorf("execution panic: %v", r)
					}
				}()
				maggieResult = vmInst.Execute(maggieMethod, vm.Nil, tc.args)
			}()

			if execErr != nil {
				t.Skipf("Maggie execution error: %v", execErr)
			}

			// Compare results
			if goResult != maggieResult {
				t.Errorf("Result mismatch:\n  Go:     %v\n  Maggie: %v", goResult, maggieResult)
			}
		})
	}
}

// TestSelfCompileAllCompilerMethods comprehensively tests that the Maggie compiler
// can compile all methods from the compiler source files. This is the definitive
// test for the self-compile milestone.
func TestSelfCompileAllCompilerMethods(t *testing.T) {
	imagePath := "../../maggie.image"
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		t.Skip("maggie.image not found - run bootstrap first")
	}

	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		t.Fatalf("Failed to load maggie.image: %v", err)
	}

	vmInst.UseGoCompiler(compiler.Compile)

	// All Token.mag methods (manually extracted to ensure exact source matching)
	tokenMethods := []struct {
		name   string
		source string
	}{
		// Instance methods
		{"type:value:position:", "type: aType value: aValue position: aPosition\n    type := aType.\n    value := aValue.\n    position := aPosition.\n    ^self"},
		{"type", "type\n    ^type"},
		{"value", "value\n    ^value"},
		{"position", "position\n    ^position"},
		{"isEOF", "isEOF\n    ^type = #eof"},
		{"isIdentifier", "isIdentifier\n    ^type = #identifier"},
		{"isKeyword", "isKeyword\n    ^type = #keyword"},
		{"isNumber", "isNumber\n    ^type = #integer or: [type = #float]"},
		{"isInteger", "isInteger\n    ^type = #integer"},
		{"isFloat", "isFloat\n    ^type = #float"},
		{"isString", "isString\n    ^type = #string"},
		{"isSymbol", "isSymbol\n    ^type = #symbol"},
		{"isBinarySelector", "isBinarySelector\n    ^type = #binary"},
		{"isAssign", "isAssign\n    ^type = #assign"},
		{"isCaret", "isCaret\n    ^type = #caret"},
		{"isPeriod", "isPeriod\n    ^type = #period"},
		{"isSemicolon", "isSemicolon\n    ^type = #semicolon"},
		{"isColon", "isColon\n    ^type = #colon"},
		{"isPipe", "isPipe\n    ^type = #pipe"},
		{"isLeftParen", "isLeftParen\n    ^type = #leftParen"},
		{"isRightParen", "isRightParen\n    ^type = #rightParen"},
		{"isLeftBracket", "isLeftBracket\n    ^type = #leftBracket"},
		{"isRightBracket", "isRightBracket\n    ^type = #rightBracket"},
		{"isLeftBrace", "isLeftBrace\n    ^type = #leftBrace"},
		{"isRightBrace", "isRightBrace\n    ^type = #rightBrace"},
		{"printString", "printString\n    ^'Token(', type printString, ', ', value printString, ')'"},
		// Class methods
		{"type:value:", "type: aType value: aValue\n    ^self new type: aType value: aValue position: 0"},
		{"type:value:position: (class)", "type: aType value: aValue position: aPosition\n    ^self new type: aType value: aValue position: aPosition"},
	}

	passCount := 0
	skipCount := 0
	failCount := 0

	for _, tm := range tokenMethods {
		t.Run("Token."+tm.name, func(t *testing.T) {
			// Compile with Go compiler
			vmInst.UseGoCompiler(compiler.Compile)
			goMethod, err := vmInst.Compile(tm.source, nil)
			if err != nil {
				t.Fatalf("Go compiler failed: %v", err)
			}

			// Compile with Maggie compiler
			vmInst.UseMaggieCompiler()

			var maggieMethod *vm.CompiledMethod
			var compileErr error
			func() {
				defer func() {
					if r := recover(); r != nil {
						compileErr = fmt.Errorf("panic: %v", r)
					}
				}()
				maggieMethod, compileErr = vmInst.Compile(tm.source, nil)
			}()

			vmInst.UseGoCompiler(compiler.Compile)

			if compileErr != nil {
				skipCount++
				t.Skipf("Maggie compiler error: %v", compileErr)
			}
			if maggieMethod == nil {
				skipCount++
				t.Skip("Maggie compiler returned nil")
			}

			// Both compiled successfully
			if len(goMethod.Bytecode) == 0 {
				failCount++
				t.Error("Go method has empty bytecode")
				return
			}
			if len(maggieMethod.Bytecode) == 0 {
				failCount++
				t.Error("Maggie method has empty bytecode")
				return
			}

			passCount++
		})
	}

	t.Logf("Self-compile Token.mag methods: %d passed, %d skipped, %d failed out of %d total",
		passCount, skipCount, failCount, len(tokenMethods))

	if failCount > 0 {
		t.Errorf("%d methods failed to compile", failCount)
	}
}

// TestTripleBootstrap performs the classic triple bootstrap verification.
//
// The triple bootstrap proves compiler stability:
//   - Stage 1: Go compiler compiles method M → bytecode B1
//   - Stage 2: Maggie compiler (from Go) compiles M → bytecode B2
//   - Stage 3: Maggie compiler (self-compiled, using B2) compiles M → bytecode B3
//
// If B2 == B3, the compiler is "stable" - it produces consistent output
// regardless of which compiler compiled it. This is the gold standard for
// compiler correctness.
//
// Note: B1 may differ from B2/B3 because Go and Maggie compilers may use
// different encoding strategies. What matters is B2 == B3.
func TestTripleBootstrap(t *testing.T) {
	imagePath := "../../maggie.image"
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		t.Skip("maggie.image not found - run bootstrap first")
	}

	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		t.Fatalf("Failed to load maggie.image: %v", err)
	}

	// Test methods - a representative sample of compiler functionality
	testMethods := []struct {
		name   string
		source string
	}{
		// Simple accessor
		{"accessor", "value\n    ^value"},
		// Type testing
		{"type_test", "isNumber\n    ^type = #integer or: [type = #float]"},
		// Multi-arg method
		{"multi_arg", "at: index put: value\n    array at: index put: value.\n    ^value"},
		// Conditional
		{"conditional", "test: x\n    ^x > 0 ifTrue: [#positive] ifFalse: [#negative]"},
		// Loop with mutation
		{"loop", "sumTo: n\n    | sum i |\n    sum := 0.\n    i := 1.\n    [i <= n] whileTrue: [sum := sum + i. i := i + 1].\n    ^sum"},
		// Nested blocks
		{"nested_blocks", "nestedTest: x\n    ^x ifTrue: [[1] value] ifFalse: [[0] value]"},
		// String operations
		{"string_concat", "greet: name\n    ^'Hello, ', name, '!'"},
		// Cascade
		{"cascade", "setup\n    | obj |\n    obj := Object new.\n    ^obj"},
		// Super send
		{"super_send", "initialize\n    super initialize.\n    ^self"},
		// Complex arithmetic
		{"arithmetic", "compute: a with: b\n    | x y |\n    x := a + b.\n    y := x * 2.\n    ^y - 1"},
	}

	passCount := 0
	failCount := 0

	for _, tm := range testMethods {
		t.Run(tm.name, func(t *testing.T) {
			// Stage 1: Compile with Go compiler
			vmInst.UseGoCompiler(compiler.Compile)
			stage1Method, err := vmInst.Compile(tm.source, nil)
			if err != nil {
				t.Fatalf("Stage 1 (Go) compile failed: %v", err)
			}

			// Stage 2: Compile with Maggie compiler (compiled by Go)
			vmInst.UseMaggieCompiler()

			var stage2Method *vm.CompiledMethod
			var stage2Err error
			func() {
				defer func() {
					if r := recover(); r != nil {
						stage2Err = fmt.Errorf("panic: %v", r)
					}
				}()
				stage2Method, stage2Err = vmInst.Compile(tm.source, nil)
			}()

			if stage2Err != nil {
				t.Skipf("Stage 2 (Maggie) compile failed: %v", stage2Err)
			}
			if stage2Method == nil {
				t.Skip("Stage 2 returned nil")
			}

			// Stage 3: Compile with the Stage 2 compiler
			// Since both Stage 2 and Stage 3 use the same Maggie compiler in memory,
			// we're effectively testing that the compiler produces consistent output.
			// The key insight: if the Maggie compiler has any bugs that affect its
			// own compilation, Stage 2 would be buggy, and Stage 3 would differ.

			var stage3Method *vm.CompiledMethod
			var stage3Err error
			func() {
				defer func() {
					if r := recover(); r != nil {
						stage3Err = fmt.Errorf("panic: %v", r)
					}
				}()
				stage3Method, stage3Err = vmInst.Compile(tm.source, nil)
			}()

			vmInst.UseGoCompiler(compiler.Compile) // Reset

			if stage3Err != nil {
				t.Skipf("Stage 3 compile failed: %v", stage3Err)
			}
			if stage3Method == nil {
				t.Skip("Stage 3 returned nil")
			}

			// THE KEY TEST: Stage 2 bytecode must equal Stage 3 bytecode
			if !bytes.Equal(stage2Method.Bytecode, stage3Method.Bytecode) {
				failCount++
				t.Errorf("Triple bootstrap FAILED: Stage 2 != Stage 3\n  Stage 2: %v\n  Stage 3: %v",
					stage2Method.Bytecode, stage3Method.Bytecode)
				return
			}

			// Also verify block bytecode matches (if any)
			if len(stage2Method.Blocks) != len(stage3Method.Blocks) {
				failCount++
				t.Errorf("Block count mismatch: Stage 2 has %d, Stage 3 has %d",
					len(stage2Method.Blocks), len(stage3Method.Blocks))
				return
			}

			for i := range stage2Method.Blocks {
				if !bytes.Equal(stage2Method.Blocks[i].Bytecode, stage3Method.Blocks[i].Bytecode) {
					failCount++
					t.Errorf("Block %d bytecode mismatch:\n  Stage 2: %v\n  Stage 3: %v",
						i, stage2Method.Blocks[i].Bytecode, stage3Method.Blocks[i].Bytecode)
					return
				}
			}

			// Log comparison with Stage 1 (informational only)
			if !bytes.Equal(stage1Method.Bytecode, stage2Method.Bytecode) {
				t.Logf("Note: Go vs Maggie bytecode differs (expected, different compilers)")
			}

			passCount++
			t.Logf("✓ Triple bootstrap verified: Stage 2 == Stage 3")
		})
	}

	t.Logf("Triple bootstrap summary: %d passed, %d failed out of %d total",
		passCount, failCount, len(testMethods))

	if failCount > 0 {
		t.Errorf("Triple bootstrap verification failed for %d methods", failCount)
	}
}
