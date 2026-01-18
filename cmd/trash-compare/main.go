// Package main provides a CLI tool for comparing jq-compiler and Procyon compiler outputs.
//
// This tool exposes the Procyon lexer, parser, and bash_backend with JSON-compatible
// output for comparison with the jq-compiler.
//
// Usage:
//
//	trash-compare tokenize <file.trash>    # Output JSON tokens (same format as jq-compiler)
//	trash-compare parse <file.trash>       # Output JSON AST
//	trash-compare bash <file.trash>        # Output compiled Bash (via bash_backend)
package main

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/chazu/procyon/pkg/ast"
	"github.com/chazu/procyon/pkg/codegen"
	"github.com/chazu/procyon/pkg/ir"
	"github.com/chazu/procyon/pkg/lexer"
	"github.com/chazu/procyon/pkg/parser"
)

func main() {
	if len(os.Args) < 2 {
		printUsage()
		os.Exit(1)
	}

	command := os.Args[1]

	switch command {
	case "tokenize":
		if len(os.Args) < 3 {
			fmt.Fprintln(os.Stderr, "Error: missing file argument")
			printUsage()
			os.Exit(1)
		}
		if err := cmdTokenize(os.Args[2]); err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}

	case "parse":
		if len(os.Args) < 3 {
			fmt.Fprintln(os.Stderr, "Error: missing file argument")
			printUsage()
			os.Exit(1)
		}
		if err := cmdParse(os.Args[2]); err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}

	case "bash":
		if len(os.Args) < 3 {
			fmt.Fprintln(os.Stderr, "Error: missing file argument")
			printUsage()
			os.Exit(1)
		}
		if err := cmdBash(os.Args[2]); err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}

	case "-h", "--help", "help":
		printUsage()

	default:
		fmt.Fprintf(os.Stderr, "Error: unknown command '%s'\n", command)
		printUsage()
		os.Exit(1)
	}
}

func printUsage() {
	fmt.Println(`trash-compare - Compare jq-compiler and Procyon compiler outputs

Usage:
  trash-compare tokenize <file.trash>    Output JSON tokens (same format as jq-compiler)
  trash-compare parse <file.trash>       Output JSON AST
  trash-compare bash <file.trash>        Output compiled Bash (via bash_backend)
  trash-compare help                     Show this help message

Examples:
  trash-compare tokenize Counter.trash
  trash-compare parse Counter.trash | jq .
  trash-compare bash Counter.trash > Counter.bash`)
}

// cmdTokenize reads a file and outputs JSON tokens.
func cmdTokenize(filename string) error {
	content, err := os.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("reading file: %w", err)
	}

	lex := lexer.New(string(content))
	jsonOutput, err := lex.TokenizeJSON()
	if err != nil {
		return fmt.Errorf("tokenizing: %w", err)
	}

	fmt.Println(jsonOutput)
	return nil
}

// cmdParse reads a file, tokenizes, parses, and outputs JSON AST.
func cmdParse(filename string) error {
	content, err := os.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("reading file: %w", err)
	}

	// Tokenize
	lex := lexer.New(string(content))
	tokens, err := lex.Tokenize()
	if err != nil {
		return fmt.Errorf("tokenizing: %w", err)
	}

	// Convert lexer tokens to parser tokens
	parserTokens := convertTokens(tokens)

	// Parse
	classAST, parseErrors := parser.ParseClass(parserTokens)
	if len(parseErrors) > 0 {
		// Output parse errors as JSON
		result := map[string]interface{}{
			"error":  true,
			"errors": parseErrors,
		}
		if classAST != nil {
			result["partial"] = classAST
		}
		jsonOutput, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(jsonOutput))
		return nil
	}

	// Marshal AST to JSON
	jsonOutput, err := json.MarshalIndent(classAST, "", "  ")
	if err != nil {
		return fmt.Errorf("marshaling AST: %w", err)
	}

	fmt.Println(string(jsonOutput))
	return nil
}

// cmdBash reads a file, tokenizes, parses, builds IR, and outputs compiled Bash.
func cmdBash(filename string) error {
	content, err := os.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("reading file: %w", err)
	}

	// Tokenize
	lex := lexer.New(string(content))
	tokens, err := lex.Tokenize()
	if err != nil {
		return fmt.Errorf("tokenizing: %w", err)
	}

	// Convert lexer tokens to parser tokens
	parserTokens := convertTokens(tokens)

	// Parse to ClassAST
	classAST, parseErrors := parser.ParseClass(parserTokens)
	if len(parseErrors) > 0 {
		for _, pe := range parseErrors {
			fmt.Fprintf(os.Stderr, "Parse error: %s\n", pe.Error())
		}
		return fmt.Errorf("parsing failed with %d errors", len(parseErrors))
	}

	// Convert ClassAST to ast.Class for IR builder
	astClass := convertClassASTToAstClass(classAST)

	// Build IR
	builder := ir.NewBuilder(astClass)
	program, warnings, errors := builder.Build()

	// Print warnings
	for _, w := range warnings {
		fmt.Fprintf(os.Stderr, "Warning: %s\n", w)
	}

	// Check for errors
	if len(errors) > 0 {
		for _, e := range errors {
			fmt.Fprintf(os.Stderr, "Error: %s\n", e)
		}
		return fmt.Errorf("IR building failed with %d errors", len(errors))
	}

	// Generate Bash code
	backend := codegen.NewBashBackend()
	output, err := backend.Generate(program)
	if err != nil {
		return fmt.Errorf("generating bash: %w", err)
	}

	fmt.Print(output)
	return nil
}

// convertTokens converts lexer.Token slice to parser.Token slice.
func convertTokens(tokens []lexer.Token) []parser.Token {
	result := make([]parser.Token, len(tokens))
	for i, t := range tokens {
		result[i] = parser.Token{
			Type:  parser.TokenType(t.Type),
			Value: t.Value,
			Line:  t.Line,
			Col:   t.Column,
		}
	}
	return result
}

// convertClassASTToAstClass converts parser.ClassAST to ast.Class for the IR builder.
func convertClassASTToAstClass(classAST *parser.ClassAST) *ast.Class {
	if classAST == nil {
		return nil
	}

	// Import the ast package
	astClass := &ast.Class{
		Type:               classAST.Type,
		Name:               classAST.Name,
		Parent:             classAST.Parent,
		Package:            classAST.Package,
		Imports:            classAST.Imports,
		IsTrait:            classAST.IsTrait,
		Traits:             classAST.Traits,
		Requires:           classAST.Requires,
		MethodRequirements: classAST.MethodRequirements,
		Location: ast.Location{
			Line: classAST.Location.Line,
			Col:  classAST.Location.Col,
		},
	}

	// Convert instance variables
	for _, v := range classAST.InstanceVars {
		ivar := ast.InstanceVar{
			Name: v.Name,
			Location: ast.Location{
				Line: v.Location.Line,
				Col:  v.Location.Col,
			},
		}
		if v.Default != nil {
			ivar.Default = ast.DefaultValue{
				Type:  v.Default.Type,
				Value: v.Default.Value,
			}
		}
		astClass.InstanceVars = append(astClass.InstanceVars, ivar)
	}

	// Convert class instance variables
	for _, v := range classAST.ClassInstanceVars {
		cvar := ast.InstanceVar{
			Name: v.Name,
			Location: ast.Location{
				Line: v.Location.Line,
				Col:  v.Location.Col,
			},
		}
		if v.Default != nil {
			cvar.Default = ast.DefaultValue{
				Type:  v.Default.Type,
				Value: v.Default.Value,
			}
		}
		astClass.ClassInstanceVars = append(astClass.ClassInstanceVars, cvar)
	}

	// Convert methods
	for _, m := range classAST.Methods {
		method := ast.Method{
			Type:     m.Type,
			Kind:     m.Kind,
			Raw:      m.Raw,
			Selector: m.Selector,
			Keywords: m.Keywords,
			Args:     m.Args,
			Pragmas:  m.Pragmas,
			Location: ast.Location{
				Line: m.Location.Line,
				Col:  m.Location.Col,
			},
		}

		// Convert body tokens
		method.Body = ast.Block{
			Type: m.Body.Type,
		}
		for _, t := range m.Body.Tokens {
			method.Body.Tokens = append(method.Body.Tokens, ast.Token{
				Type:  string(t.Type),
				Value: t.Value,
				Line:  t.Line,
				Col:   t.Col,
			})
		}

		astClass.Methods = append(astClass.Methods, method)
	}

	// Convert aliases
	for _, a := range classAST.Aliases {
		astClass.Aliases = append(astClass.Aliases, ast.Alias{
			From: a.AliasName,
			To:   a.OriginalMethod,
		})
	}

	// Convert advice
	for _, adv := range classAST.Advice {
		advice := ast.Advice{
			Type:     adv.AdviceType,
			Selector: adv.Selector,
		}
		advice.Body = ast.Block{
			Type: adv.Block.Type,
		}
		for _, t := range adv.Block.Tokens {
			advice.Body.Tokens = append(advice.Body.Tokens, ast.Token{
				Type:  string(t.Type),
				Value: t.Value,
				Line:  t.Line,
				Col:   t.Col,
			})
		}
		astClass.Advice = append(astClass.Advice, advice)
	}

	return astClass
}
