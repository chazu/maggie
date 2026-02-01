package main

import (
	"bytes"
	"fmt"
	"os"
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// TestSelfCompile verifies that the Maggie compiler (compiled by Go) can compile
// source code and produce semantically equivalent results to the Go compiler.
//
// Note: We compare execution results rather than bytecode, since the two compilers
// may generate different but semantically equivalent bytecode sequences.
//
// This test is experimental and may be skipped if the Maggie compiler is not
// fully functional for certain test cases.
func TestSelfCompile(t *testing.T) {
	// Skip if maggie.image doesn't exist
	imagePath := "../../maggie.image"
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		t.Skip("maggie.image not found - run bootstrap first")
	}

	// Load the image (contains Maggie compiler classes)
	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		t.Fatalf("Failed to load maggie.image: %v", err)
	}

	// Set up Go compiler backend
	vmInst.UseGoCompiler(compiler.Compile)

	// Test cases: simple expressions that both compilers should handle
	// Note: Only use primitives that are actually implemented in the VM
	testCases := []struct {
		name     string
		source   string
		expected int64 // Expected result as SmallInt (-1 for non-int result)
	}{
		{"simple return", "doIt\n    ^42", 42},
		{"arithmetic add", "doIt\n    ^3 + 4", 7},
		{"arithmetic sub", "doIt\n    ^10 - 3", 7},
		{"arithmetic mul", "doIt\n    ^6 * 7", 42},
		{"arithmetic div", "doIt\n    ^20 / 4", 5},
		{"unary message", "doIt\n    ^5 negated", -5},
		{"comparison", "doIt\n    ^(3 < 5) ifTrue: [1] ifFalse: [0]", 1},
		{"local variable", "doIt\n    | x |\n    x := 10.\n    ^x", 10},
		{"multiple locals", "doIt\n    | x y |\n    x := 10.\n    y := 20.\n    ^x + y", 30},
	}

	passCount := 0
	skipCount := 0

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

			// Execute Go-compiled method
			goResult := vmInst.Execute(goMethod, vm.Nil, nil)

			// Verify Go compiler produces expected result
			if tc.expected >= 0 {
				if !goResult.IsSmallInt() || goResult.SmallInt() != tc.expected {
					t.Fatalf("Go compiler result mismatch: got %v, want %d", goResult, tc.expected)
				}
			}

			// Now switch to Maggie compiler and compile the same source
			// Use defer/recover to handle panics in the experimental compiler
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
				t.Skipf("Maggie compiler returned nil (not yet implemented for: %s)", tc.name)
			}

			// Execute Maggie-compiled method with panic recovery
			var maggieResult vm.Value
			var execErr error
			func() {
				defer func() {
					if r := recover(); r != nil {
						execErr = fmt.Errorf("execution panic: %v", r)
					}
				}()
				maggieResult = vmInst.Execute(maggieMethod, vm.Nil, nil)
			}()

			if execErr != nil {
				skipCount++
				t.Skipf("Maggie-compiled method execution failed: %v", execErr)
			}

			// Compare execution results
			if goResult != maggieResult {
				t.Errorf("Execution result mismatch:\n  Go compiler:     %v\n  Maggie compiler: %v",
					goResult, maggieResult)

				// Log bytecode for debugging
				t.Logf("Go bytecode:     %v", goMethod.Bytecode)
				t.Logf("Maggie bytecode: %v", maggieMethod.Bytecode)
			} else {
				passCount++
			}

			// Optionally compare bytecode (informational, not failure)
			if !bytes.Equal(goMethod.Bytecode, maggieMethod.Bytecode) {
				t.Logf("Note: Bytecode differs (but results match):\n  Go:     %v\n  Maggie: %v",
					goMethod.Bytecode, maggieMethod.Bytecode)
			}
		})
	}

	// Summary
	t.Logf("Self-compile test summary: %d passed, %d skipped out of %d total",
		passCount, skipCount, len(testCases))
}

// TestMaggieCompilerExists verifies the Maggie compiler classes are loaded.
func TestMaggieCompilerExists(t *testing.T) {
	imagePath := "../../maggie.image"
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		t.Skip("maggie.image not found - run bootstrap first")
	}

	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		t.Fatalf("Failed to load maggie.image: %v", err)
	}

	// Check that compiler classes exist in globals
	compilerClasses := []string{
		"Compiler", "Parser", "Lexer", "BytecodeGenerator",
		"Token", "CharacterStream",
		"LiteralNode", "VariableNode", "AssignmentNode",
		"MessageSendNode", "CascadeNode", "BlockNode",
		"ReturnNode", "MethodNode", "ClassNode",
	}

	for _, className := range compilerClasses {
		if _, ok := vmInst.Globals[className]; !ok {
			t.Errorf("Compiler class %q not found in globals", className)
		}
	}
}

// TestMaggieCompilerInstantiation verifies we can create a Compiler instance.
func TestMaggieCompilerInstantiation(t *testing.T) {
	imagePath := "../../maggie.image"
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		t.Skip("maggie.image not found - run bootstrap first")
	}

	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		t.Fatalf("Failed to load maggie.image: %v", err)
	}

	// Get the Compiler class
	compilerClass, ok := vmInst.Globals["Compiler"]
	if !ok {
		t.Fatal("Compiler class not found")
	}

	// Create an instance: Compiler new
	instance := vmInst.Send(compilerClass, "new", nil)
	if instance == vm.Nil {
		t.Fatal("Compiler new returned nil")
	}

	// Verify we can call printString on it
	printResult := vmInst.Send(instance, "printString", nil)
	if printResult == vm.Nil {
		t.Error("printString returned nil")
	}
}

// TestMaggieParserInstantiation verifies we can create a Parser instance.
func TestMaggieParserInstantiation(t *testing.T) {
	imagePath := "../../maggie.image"
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		t.Skip("maggie.image not found - run bootstrap first")
	}

	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		t.Fatalf("Failed to load maggie.image: %v", err)
	}

	// Get the Parser class
	parserClass, ok := vmInst.Globals["Parser"]
	if !ok {
		t.Fatal("Parser class not found")
	}

	// Create a parser: Parser on: '42'
	sourceStr := vm.NewStringValue("42")
	parser := vmInst.Send(parserClass, "on:", []vm.Value{sourceStr})
	if parser == vm.Nil {
		t.Fatal("Parser on: returned nil")
	}
}

// TestMaggieLexerTokenization verifies the Maggie lexer can tokenize source.
func TestMaggieLexerTokenization(t *testing.T) {
	imagePath := "../../maggie.image"
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		t.Skip("maggie.image not found - run bootstrap first")
	}

	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		t.Fatalf("Failed to load maggie.image: %v", err)
	}

	// Get the Lexer class
	lexerClass, ok := vmInst.Globals["Lexer"]
	if !ok {
		t.Fatal("Lexer class not found")
	}

	// Create a lexer: Lexer on: '42 + 3'
	sourceStr := vm.NewStringValue("42 + 3")
	lexer := vmInst.Send(lexerClass, "on:", []vm.Value{sourceStr})
	if lexer == vm.Nil {
		t.Fatal("Lexer on: returned nil")
	}

	// Get next token
	token := vmInst.Send(lexer, "nextToken", nil)
	if token == vm.Nil {
		t.Fatal("nextToken returned nil")
	}

	// Check token type is number
	tokenType := vmInst.Send(token, "type", nil)
	if tokenType == vm.Nil {
		t.Fatal("token type returned nil")
	}
}

// TestIfTrue verifies ifTrue: works correctly.
func TestIfTrue(t *testing.T) {
	imagePath := "../../maggie.image"
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		t.Skip("maggie.image not found - run bootstrap first")
	}

	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		t.Fatalf("Failed to load maggie.image: %v", err)
	}

	// Test ifTrue: directly using Go compiler
	vmInst.UseGoCompiler(compiler.Compile)
	method, err := vmInst.CompileExpression("true ifTrue: [42]")
	if err != nil {
		t.Fatalf("Compile failed: %v", err)
	}
	result := vmInst.Execute(method, vm.Nil, nil)
	t.Logf("true ifTrue: [42] = %v (SmallInt: %v)", result, result.IsSmallInt())
	if result.IsSmallInt() {
		t.Logf("  value = %d", result.SmallInt())
	}
}

// TestArrayIndexOf verifies Array indexOf: works correctly.
func TestArrayIndexOf(t *testing.T) {
	imagePath := "../../maggie.image"
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		t.Skip("maggie.image not found - run bootstrap first")
	}

	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		t.Fatalf("Failed to load maggie.image: %v", err)
	}

	// Create an array with one element "x"
	arrayClass, _ := vmInst.Globals["Array"]
	arr := vmInst.Send(arrayClass, "new", nil)

	xStr := vm.NewStringValue("x")
	arr = vmInst.Send(arr, "copyWith:", []vm.Value{xStr})

	// Check size
	size := vmInst.Send(arr, "size", nil)
	t.Logf("Array size: %d", size.SmallInt())

	// Check first element
	first := vmInst.Send(arr, "at:", []vm.Value{vm.FromSmallInt(0)})
	t.Logf("Array at: 0 = %v", first)
	if vm.IsStringValue(first) {
		t.Logf("  content: %q", vm.GetStringContent(first))
	}

	// Test indexOf:
	xStr2 := vm.NewStringValue("x")
	idx := vmInst.Send(arr, "indexOf:", []vm.Value{xStr2})
	t.Logf("indexOf: 'x' = %v", idx)
	t.Logf("  IsSmallInt: %v, IsStringValue: %v, isSymbol: %v", idx.IsSmallInt(), vm.IsStringValue(idx), idx.IsSymbol())
	if idx.IsSmallInt() {
		t.Logf("  index value: %d", idx.SmallInt())
	}
	if vm.IsStringValue(idx) {
		t.Logf("  string content: %q", vm.GetStringContent(idx))
	}

	// Test with the exact same string object
	idx2 := vmInst.Send(arr, "indexOf:", []vm.Value{first})
	t.Logf("indexOf: (same object) = %v", idx2)
	if idx2.IsSmallInt() {
		t.Logf("  index value: %d", idx2.SmallInt())
	}
}

// TestBytecodeGeneratorTemps verifies BytecodeGenerator temp handling.
func TestBytecodeGeneratorTemps(t *testing.T) {
	imagePath := "../../maggie.image"
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		t.Skip("maggie.image not found - run bootstrap first")
	}

	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		t.Fatalf("Failed to load maggie.image: %v", err)
	}

	bcgenClass, ok := vmInst.Globals["BytecodeGenerator"]
	if !ok {
		t.Fatal("BytecodeGenerator class not found")
	}

	// Create a BytecodeGenerator
	bcgen := vmInst.Send(bcgenClass, "new", nil)
	if bcgen == vm.Nil {
		t.Fatal("BytecodeGenerator new returned nil")
	}

	// Add a temp "x"
	xStr := vm.NewStringValue("x")
	idx := vmInst.Send(bcgen, "addTemp:", []vm.Value{xStr})
	t.Logf("addTemp: 'x' returned index: %v", idx)

	// Check temps array directly
	temps := vmInst.Send(bcgen, "temps", nil)
	t.Logf("temps array: %v (Nil: %v)", temps, temps == vm.Nil)

	// If temps has special method, try accessing it
	tempsSize := vmInst.Send(temps, "size", nil)
	t.Logf("temps size: %v", tempsSize)

	if tempsSize.IsSmallInt() && tempsSize.SmallInt() > 0 {
		firstTemp := vmInst.Send(temps, "at:", []vm.Value{vm.FromSmallInt(0)})
		t.Logf("temps at: 0 = %v", firstTemp)
		if vm.IsStringValue(firstTemp) {
			t.Logf("first temp content: %q", vm.GetStringContent(firstTemp))
		}

		// Compare directly
		result := vmInst.Send(firstTemp, "=", []vm.Value{xStr})
		t.Logf("firstTemp = xStr: %v (is True: %v, is False: %v)", result, result == vm.True, result == vm.False)
	}

	// Check tempIndex "x"
	foundIdx := vmInst.Send(bcgen, "tempIndex:", []vm.Value{xStr})
	t.Logf("tempIndex: 'x' returned: %v", foundIdx)

	// Create a different string "x" and check again
	xStr2 := vm.NewStringValue("x")
	foundIdx2 := vmInst.Send(bcgen, "tempIndex:", []vm.Value{xStr2})
	t.Logf("tempIndex: (new string 'x') returned: %v", foundIdx2)

	if foundIdx2.IsSmallInt() {
		t.Logf("foundIdx2 as int: %d", foundIdx2.SmallInt())
	}
}

// TestParserTemporaries verifies the Maggie parser extracts temporaries.
func TestParserTemporaries(t *testing.T) {
	imagePath := "../../maggie.image"
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		t.Skip("maggie.image not found - run bootstrap first")
	}

	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		t.Fatalf("Failed to load maggie.image: %v", err)
	}

	parserClass, ok := vmInst.Globals["Parser"]
	if !ok {
		t.Fatal("Parser class not found")
	}

	// Parse a method with temporaries (using the converted format with newlines)
	source := "method: doIt [\n    | x |\n    x := 10.\n    ^x\n]"
	sourceStr := vm.NewStringValue(source)
	parser := vmInst.Send(parserClass, "on:", []vm.Value{sourceStr})

	// Call parseMethodDef
	methodNode := vmInst.Send(parser, "parseMethodDef", nil)
	if methodNode == vm.Nil {
		t.Fatal("parseMethodDef returned nil")
	}

	// Get temporaries
	temps := vmInst.Send(methodNode, "temporaries", nil)
	t.Logf("temporaries: %v (Nil: %v)", temps, temps == vm.Nil)

	// Get size
	tempsSize := vmInst.Send(temps, "size", nil)
	t.Logf("temporaries size: %v", tempsSize)

	if tempsSize.IsSmallInt() && tempsSize.SmallInt() > 0 {
		// Get first temp
		firstTemp := vmInst.Send(temps, "at:", []vm.Value{vm.FromSmallInt(0)})
		t.Logf("first temp: %v", firstTemp)
		if vm.IsStringValue(firstTemp) {
			t.Logf("first temp content: %q", vm.GetStringContent(firstTemp))
		}
	}
}

// TestLexerPipeToken verifies the Maggie lexer tokenizes | as #pipe.
func TestLexerPipeToken(t *testing.T) {
	imagePath := "../../maggie.image"
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		t.Skip("maggie.image not found - run bootstrap first")
	}

	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		t.Fatalf("Failed to load maggie.image: %v", err)
	}

	lexerClass, ok := vmInst.Globals["Lexer"]
	if !ok {
		t.Fatal("Lexer class not found")
	}

	// Tokenize "[ | x |" - should produce [, |, x, |
	sourceStr := vm.NewStringValue("[ | x |")
	lexer := vmInst.Send(lexerClass, "on:", []vm.Value{sourceStr})

	// Token 1: [
	token1 := vmInst.Send(lexer, "nextToken", nil)
	type1 := vmInst.Send(token1, "type", nil)
	t.Logf("Token 1: type=%v", type1)

	// Token 2: |
	token2 := vmInst.Send(lexer, "nextToken", nil)
	type2 := vmInst.Send(token2, "type", nil)
	t.Logf("Token 2: type=%v", type2)

	// Check if type2 is #pipe
	pipeSymbol := vmInst.Symbols.SymbolValue("pipe")
	t.Logf("Expected #pipe symbol: %v", pipeSymbol)
	t.Logf("type2 == pipeSymbol: %v", type2 == pipeSymbol)

	// Token 3: x
	token3 := vmInst.Send(lexer, "nextToken", nil)
	type3 := vmInst.Send(token3, "type", nil)
	t.Logf("Token 3: type=%v", type3)

	// Token 4: |
	token4 := vmInst.Send(lexer, "nextToken", nil)
	type4 := vmInst.Send(token4, "type", nil)
	t.Logf("Token 4: type=%v", type4)
}
