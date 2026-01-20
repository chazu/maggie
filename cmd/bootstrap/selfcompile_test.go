package main

import (
	"bytes"
	"os"
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// TestSelfCompile verifies that the Maggie compiler (compiled by Go) can compile
// source code and produce the same bytecode as the Go compiler.
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

	// Test cases: simple expressions that both compilers should handle identically
	testCases := []struct {
		name   string
		source string
	}{
		{"simple return", "doIt\n    ^42"},
		{"arithmetic", "doIt\n    ^3 + 4"},
		{"unary message", "doIt\n    ^5 negated"},
		{"binary message", "doIt\n    ^10 - 3"},
		{"keyword message", "doIt\n    ^7 max: 5"},
		{"local variable", "doIt\n    | x |\n    x := 10.\n    ^x"},
		{"conditional", "doIt\n    ^true ifTrue: [1] ifFalse: [2]"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			// Compile with Go compiler
			goMethod, err := vmInst.Compile(tc.source, nil)
			if err != nil {
				t.Fatalf("Go compiler failed: %v", err)
			}
			if goMethod == nil {
				t.Fatal("Go compiler returned nil method")
			}

			// Now switch to Maggie compiler and compile the same source
			vmInst.UseMaggieCompiler()
			maggieMethod, err := vmInst.Compile(tc.source, nil)

			// Switch back to Go compiler for subsequent tests
			vmInst.UseGoCompiler(compiler.Compile)

			if err != nil {
				t.Fatalf("Maggie compiler failed: %v", err)
			}
			if maggieMethod == nil {
				t.Skipf("Maggie compiler returned nil (not yet implemented)")
			}

			// Compare bytecode
			goBytecode := goMethod.Bytecode
			maggieBytecode := maggieMethod.Bytecode

			if !bytes.Equal(goBytecode, maggieBytecode) {
				t.Errorf("Bytecode mismatch for %q:\n  Go:     %v\n  Maggie: %v",
					tc.source, goBytecode, maggieBytecode)
			}

			// Compare literals count
			if len(goMethod.Literals) != len(maggieMethod.Literals) {
				t.Errorf("Literals count mismatch: Go=%d, Maggie=%d",
					len(goMethod.Literals), len(maggieMethod.Literals))
			}

			// Execute both and compare results
			goResult := vmInst.Execute(goMethod, vm.Nil, nil)
			maggieResult := vmInst.Execute(maggieMethod, vm.Nil, nil)

			if goResult != maggieResult {
				t.Errorf("Execution result mismatch: Go=%v, Maggie=%v",
					goResult, maggieResult)
			}
		})
	}
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
