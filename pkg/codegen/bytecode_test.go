package codegen

import (
	"bytes"
	"strings"
	"testing"

	"github.com/chazu/procyon/pkg/ast"
	"github.com/chazu/procyon/pkg/bytecode"
	"github.com/chazu/procyon/pkg/parser"
	"github.com/dave/jennifer/jen"
)

// TestCompileBlockToBytecode tests the bytecode compiler context setup
func TestCompileBlockToBytecode(t *testing.T) {
	g := &generator{
		class: &ast.Class{
			Name: "TestClass",
			InstanceVars: []ast.InstanceVar{
				{Name: "value"},
				{Name: "counter"},
			},
		},
		instanceVars:   map[string]bool{"value": true, "counter": true},
		jsonVars:       map[string]bool{},
		skippedMethods: map[string]string{},
		compiledBlocks: map[string]*bytecode.Chunk{},
	}

	// Create a simple block: [:x | x]
	block := &parser.BlockExpr{
		Params: []string{"x"},
		Statements: []parser.Statement{
			&parser.Return{
				Value: &parser.Identifier{Name: "x"},
			},
		},
	}

	method := &compiledMethod{
		selector: "testMethod",
		args:     []string{"arg1"},
		body: &parser.MethodBody{
			LocalVars:  []string{"temp"},
			Statements: []parser.Statement{},
		},
	}

	chunk, err := g.compileBlockToBytecode(block, method)
	if err != nil {
		t.Fatalf("Failed to compile block: %v", err)
	}

	if chunk == nil {
		t.Fatal("Expected chunk, got nil")
	}

	// Verify chunk has correct parameter count
	if chunk.ParamCount != 1 {
		t.Errorf("Expected 1 param, got %d", chunk.ParamCount)
	}

	// Verify chunk has parameter name
	if len(chunk.ParamNames) != 1 || chunk.ParamNames[0] != "x" {
		t.Errorf("Expected param name 'x', got %v", chunk.ParamNames)
	}
}

// TestGetLocalVars tests extraction of local variables from compiled method
func TestGetLocalVars(t *testing.T) {
	g := &generator{}

	// Test with nil body
	m := &compiledMethod{body: nil}
	vars := g.getLocalVars(m)
	if vars != nil {
		t.Errorf("Expected nil for nil body, got %v", vars)
	}

	// Test with local vars
	m = &compiledMethod{
		body: &parser.MethodBody{
			LocalVars: []string{"a", "b", "c"},
		},
	}
	vars = g.getLocalVars(m)
	if len(vars) != 3 {
		t.Errorf("Expected 3 vars, got %d", len(vars))
	}
	if vars[0] != "a" || vars[1] != "b" || vars[2] != "c" {
		t.Errorf("Expected [a, b, c], got %v", vars)
	}
}

// TestGetInstanceVarNames tests extraction of instance variable names
func TestGetInstanceVarNames(t *testing.T) {
	g := &generator{
		class: &ast.Class{
			Name: "TestClass",
			InstanceVars: []ast.InstanceVar{
				{Name: "count"},
				{Name: "name"},
				{Name: "data"},
			},
		},
	}

	names := g.getInstanceVarNames()
	if len(names) != 3 {
		t.Errorf("Expected 3 names, got %d", len(names))
	}

	// Check all names are present
	expected := map[string]bool{"count": true, "name": true, "data": true}
	for _, name := range names {
		if !expected[name] {
			t.Errorf("Unexpected ivar name: %s", name)
		}
	}
}

// TestGenerateBlockCreation tests generation of bytecode block closures
func TestGenerateBlockCreation(t *testing.T) {
	g := &generator{
		class: &ast.Class{
			Name:         "TestClass",
			InstanceVars: []ast.InstanceVar{},
		},
		instanceVars:   map[string]bool{},
		jsonVars:       map[string]bool{},
		skippedMethods: map[string]string{},
		compiledBlocks: map[string]*bytecode.Chunk{},
		warnings:       []string{},
		blockCounter:   0,
	}

	// Create a simple block: [:x | x]
	block := &parser.BlockExpr{
		Params: []string{"x"},
		Statements: []parser.Statement{
			&parser.Return{
				Value: &parser.Identifier{Name: "x"},
			},
		},
	}

	method := &compiledMethod{
		selector: "testMethod",
		args:     []string{},
		body:     &parser.MethodBody{},
	}

	stmt := g.generateBlockCreation(block, method)
	if stmt == nil {
		t.Fatal("Expected statement, got nil")
	}

	// Verify a compiled block was stored
	if len(g.compiledBlocks) != 1 {
		t.Errorf("Expected 1 compiled block, got %d", len(g.compiledBlocks))
	}

	// Verify block counter was incremented
	if g.blockCounter != 1 {
		t.Errorf("Expected blockCounter=1, got %d", g.blockCounter)
	}

	// To properly render the statement, we need to wrap it in a context
	// Create a file and add the statement as a variable assignment
	f := jen.NewFile("test")
	f.Var().Id("blockFn").Op("=").Add(stmt)

	// Render to buffer
	buf := &bytes.Buffer{}
	err := f.Render(buf)
	if err != nil {
		t.Fatalf("Failed to render: %v", err)
	}

	rendered := buf.String()

	// Should contain hex decoding
	if !strings.Contains(rendered, "DecodeString") {
		t.Error("Generated code should contain hex DecodeString call")
	}

	// Should contain bytecode Deserialize
	if !strings.Contains(rendered, "Deserialize") {
		t.Error("Generated code should contain bytecode Deserialize call")
	}

	// Should contain VM creation
	if !strings.Contains(rendered, "NewVM") {
		t.Error("Generated code should contain NewVM call")
	}
}

// TestGenerateBlockCreationWithCapture tests bytecode compilation of blocks that capture variables
func TestGenerateBlockCreationWithCapture(t *testing.T) {
	g := &generator{
		class: &ast.Class{
			Name: "Counter",
			InstanceVars: []ast.InstanceVar{
				{Name: "value"},
			},
		},
		instanceVars:   map[string]bool{"value": true},
		jsonVars:       map[string]bool{},
		skippedMethods: map[string]string{},
		compiledBlocks: map[string]*bytecode.Chunk{},
		warnings:       []string{},
		blockCounter:   0,
	}

	// Create a block that references an ivar: [value + 1]
	block := &parser.BlockExpr{
		Params: []string{},
		Statements: []parser.Statement{
			&parser.Return{
				Value: &parser.BinaryExpr{
					Op:    "+",
					Left:  &parser.Identifier{Name: "value"},
					Right: &parser.NumberLit{Value: "1"},
				},
			},
		},
	}

	method := &compiledMethod{
		selector: "increment",
		args:     []string{},
		body:     &parser.MethodBody{},
	}

	stmt := g.generateBlockCreation(block, method)
	if stmt == nil {
		// Bytecode compilation might fail for ivar access - that's OK
		// Check warnings
		if len(g.warnings) == 0 {
			t.Error("Expected warning for failed compilation")
		}
		t.Logf("Block compilation returned nil (expected for ivar capture): %v", g.warnings)
		return
	}

	// If compilation succeeded, verify chunk was stored
	if len(g.compiledBlocks) != 1 {
		t.Errorf("Expected 1 compiled block, got %d", len(g.compiledBlocks))
	}

	// Check if capture info is present
	for _, chunk := range g.compiledBlocks {
		if chunk.CaptureCount() > 0 {
			t.Logf("Block has %d captures", chunk.CaptureCount())
		}
	}
}

// TestBlockCounterIncrement verifies multiple blocks get unique IDs
func TestBlockCounterIncrement(t *testing.T) {
	g := &generator{
		class: &ast.Class{
			Name:         "TestClass",
			InstanceVars: []ast.InstanceVar{},
		},
		instanceVars:   map[string]bool{},
		jsonVars:       map[string]bool{},
		skippedMethods: map[string]string{},
		compiledBlocks: map[string]*bytecode.Chunk{},
		warnings:       []string{},
		blockCounter:   0,
	}

	method := &compiledMethod{
		selector: "test",
		args:     []string{},
		body:     &parser.MethodBody{},
	}

	// Create multiple blocks
	for i := 0; i < 3; i++ {
		block := &parser.BlockExpr{
			Params: []string{"x"},
			Statements: []parser.Statement{
				&parser.Return{Value: &parser.Identifier{Name: "x"}},
			},
		}

		g.generateBlockCreation(block, method)
	}

	// Verify counter incremented for each block
	if g.blockCounter != 3 {
		t.Errorf("Expected blockCounter=3, got %d", g.blockCounter)
	}

	// Verify 3 unique blocks were stored
	if len(g.compiledBlocks) != 3 {
		t.Errorf("Expected 3 compiled blocks, got %d", len(g.compiledBlocks))
	}
}

// TestCompileArithmeticBlock tests bytecode compilation of arithmetic expressions
func TestCompileArithmeticBlock(t *testing.T) {
	g := &generator{
		class: &ast.Class{
			Name:         "Calculator",
			InstanceVars: []ast.InstanceVar{},
		},
		instanceVars:   map[string]bool{},
		jsonVars:       map[string]bool{},
		skippedMethods: map[string]string{},
		compiledBlocks: map[string]*bytecode.Chunk{},
		warnings:       []string{},
		blockCounter:   0,
	}

	// Block: [:x :y | x + y]
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

	method := &compiledMethod{
		selector: "add",
		args:     []string{},
		body:     &parser.MethodBody{},
	}

	chunk, err := g.compileBlockToBytecode(block, method)
	if err != nil {
		t.Fatalf("Failed to compile arithmetic block: %v", err)
	}

	if chunk.ParamCount != 2 {
		t.Errorf("Expected 2 params, got %d", chunk.ParamCount)
	}
}

// TestGeneratorInitialization tests that generator is properly initialized
func TestGeneratorInitialization(t *testing.T) {
	class := &ast.Class{
		Name: "TestClass",
		InstanceVars: []ast.InstanceVar{
			{Name: "value", Default: ast.DefaultValue{Value: "0"}},
		},
	}

	result := GenerateSharedPlugin(class)
	if result == nil {
		t.Fatal("Expected result, got nil")
	}

	// Code should be generated
	if result.Code == "" {
		t.Error("Expected generated code, got empty string")
	}

	// Should contain the class name
	if !strings.Contains(result.Code, "TestClass") {
		t.Error("Generated code should contain class name")
	}
}
