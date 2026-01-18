package parser

import (
	"testing"

	"github.com/chazu/procyon/pkg/ast"
)

// newParser creates a new parser for testing
func newParser(tokens []ast.Token) *Parser {
	return &Parser{tokens: tokens, pos: 0}
}

func TestParseJSONPrimitiveChaining(t *testing.T) {
	tests := []struct {
		name     string
		tokens   []ast.Token
		wantOps  []string // Expected operations in order (innermost first)
		wantArgs int      // Total number of arguments
	}{
		{
			name: "single arrayPush",
			tokens: []ast.Token{
				{Type: ast.TokenIdentifier, Value: "items"},
				{Type: ast.TokenKeyword, Value: "arrayPush:"},
				{Type: ast.TokenIdentifier, Value: "x"},
			},
			wantOps:  []string{"arrayPush"},
			wantArgs: 1,
		},
		{
			name: "two chained arrayPush",
			tokens: []ast.Token{
				{Type: ast.TokenIdentifier, Value: "items"},
				{Type: ast.TokenKeyword, Value: "arrayPush:"},
				{Type: ast.TokenIdentifier, Value: "x"},
				{Type: ast.TokenKeyword, Value: "arrayPush:"},
				{Type: ast.TokenIdentifier, Value: "y"},
			},
			wantOps:  []string{"arrayPush", "arrayPush"},
			wantArgs: 2,
		},
		{
			name: "three chained arrayPush",
			tokens: []ast.Token{
				{Type: ast.TokenIdentifier, Value: "items"},
				{Type: ast.TokenKeyword, Value: "arrayPush:"},
				{Type: ast.TokenIdentifier, Value: "x"},
				{Type: ast.TokenKeyword, Value: "arrayPush:"},
				{Type: ast.TokenIdentifier, Value: "y"},
				{Type: ast.TokenKeyword, Value: "arrayPush:"},
				{Type: ast.TokenIdentifier, Value: "z"},
			},
			wantOps:  []string{"arrayPush", "arrayPush", "arrayPush"},
			wantArgs: 3,
		},
		{
			name: "arrayPush followed by unary",
			tokens: []ast.Token{
				{Type: ast.TokenIdentifier, Value: "items"},
				{Type: ast.TokenKeyword, Value: "arrayPush:"},
				{Type: ast.TokenNumber, Value: "1"},
				{Type: ast.TokenIdentifier, Value: "arrayLength"},
			},
			wantOps:  []string{"arrayPush", "arrayLength"},
			wantArgs: 1,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := newParser(tt.tokens)
			result, err := p.parseExpr()
			if err != nil {
				t.Fatalf("parseExpr() error = %v", err)
			}

			// Collect operations by walking the tree
			var ops []string
			var totalArgs int
			collectOps(result, &ops, &totalArgs)

			if len(ops) != len(tt.wantOps) {
				t.Errorf("got %d operations, want %d", len(ops), len(tt.wantOps))
			}

			for i, op := range ops {
				if i < len(tt.wantOps) && op != tt.wantOps[i] {
					t.Errorf("ops[%d] = %q, want %q", i, op, tt.wantOps[i])
				}
			}

			if totalArgs != tt.wantArgs {
				t.Errorf("totalArgs = %d, want %d", totalArgs, tt.wantArgs)
			}
		})
	}
}

// collectOps walks the expression tree and collects JSON primitive operations
func collectOps(expr Expr, ops *[]string, totalArgs *int) {
	switch e := expr.(type) {
	case *JSONPrimitiveExpr:
		// Recurse into receiver first (for chained ops)
		collectOps(e.Receiver, ops, totalArgs)
		*ops = append(*ops, e.Operation)
		*totalArgs += len(e.Args)
	}
}

func TestExprResultsInArray(t *testing.T) {
	// This tests that type tracking works through chains
	// We test this indirectly through the parser output structure

	tokens := []ast.Token{
		{Type: ast.TokenIdentifier, Value: "items"},
		{Type: ast.TokenKeyword, Value: "arrayPush:"},
		{Type: ast.TokenIdentifier, Value: "x"},
		{Type: ast.TokenKeyword, Value: "arrayPush:"},
		{Type: ast.TokenIdentifier, Value: "y"},
	}

	p := newParser(tokens)
	result, err := p.parseExpr()
	if err != nil {
		t.Fatalf("parseExpr() error = %v", err)
	}

	// The outer expression should be a JSONPrimitiveExpr
	outer, ok := result.(*JSONPrimitiveExpr)
	if !ok {
		t.Fatalf("expected JSONPrimitiveExpr, got %T", result)
	}

	if outer.Operation != "arrayPush" {
		t.Errorf("outer.Operation = %q, want %q", outer.Operation, "arrayPush")
	}

	// The receiver of outer should also be a JSONPrimitiveExpr
	inner, ok := outer.Receiver.(*JSONPrimitiveExpr)
	if !ok {
		t.Fatalf("expected inner JSONPrimitiveExpr, got %T", outer.Receiver)
	}

	if inner.Operation != "arrayPush" {
		t.Errorf("inner.Operation = %q, want %q", inner.Operation, "arrayPush")
	}

	// The receiver of inner should be an Identifier
	base, ok := inner.Receiver.(*Identifier)
	if !ok {
		t.Fatalf("expected Identifier, got %T", inner.Receiver)
	}

	if base.Name != "items" {
		t.Errorf("base.Name = %q, want %q", base.Name, "items")
	}
}

func TestParseBlockExpr(t *testing.T) {
	tests := []struct {
		name       string
		tokens     []ast.Token
		wantParams []string
		wantStmts  int
	}{
		{
			name: "block with one param",
			tokens: []ast.Token{
				{Type: ast.TokenLBracket, Value: "["},
				{Type: ast.TokenBlockParam, Value: "each"},
				{Type: ast.TokenPipe, Value: "|"},
				{Type: ast.TokenIdentifier, Value: "x"},
				{Type: ast.TokenPlus, Value: "+"},
				{Type: ast.TokenNumber, Value: "1"},
				{Type: ast.TokenRBracket, Value: "]"},
			},
			wantParams: []string{"each"},
			wantStmts:  1,
		},
		{
			name: "block with two params",
			tokens: []ast.Token{
				{Type: ast.TokenLBracket, Value: "["},
				{Type: ast.TokenBlockParam, Value: "key"},
				{Type: ast.TokenBlockParam, Value: "value"},
				{Type: ast.TokenPipe, Value: "|"},
				{Type: ast.TokenIdentifier, Value: "key"},
				{Type: ast.TokenRBracket, Value: "]"},
			},
			wantParams: []string{"key", "value"},
			wantStmts:  1,
		},
		{
			name: "block with no params",
			tokens: []ast.Token{
				{Type: ast.TokenLBracket, Value: "["},
				{Type: ast.TokenIdentifier, Value: "x"},
				{Type: ast.TokenPlus, Value: "+"},
				{Type: ast.TokenNumber, Value: "1"},
				{Type: ast.TokenRBracket, Value: "]"},
			},
			wantParams: []string{},
			wantStmts:  1,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := newParser(tt.tokens)
			result, err := p.parseBlockExpr()
			if err != nil {
				t.Fatalf("parseBlockExpr() error = %v", err)
			}

			if len(result.Params) != len(tt.wantParams) {
				t.Errorf("got %d params, want %d", len(result.Params), len(tt.wantParams))
			}

			for i, param := range result.Params {
				if i < len(tt.wantParams) && param != tt.wantParams[i] {
					t.Errorf("params[%d] = %q, want %q", i, param, tt.wantParams[i])
				}
			}

			if len(result.Statements) != tt.wantStmts {
				t.Errorf("got %d statements, want %d", len(result.Statements), tt.wantStmts)
			}
		})
	}
}

func TestParseQualifiedName(t *testing.T) {
	tests := []struct {
		name         string
		tokens       []ast.Token
		wantPackage  string
		wantClass    string
		wantSelector string
	}{
		{
			name: "qualified class message send",
			tokens: []ast.Token{
				{Type: ast.TokenAt, Value: "@"},
				{Type: ast.TokenIdentifier, Value: "Yutani"},
				{Type: ast.TokenNamespaceSep, Value: "::"},
				{Type: ast.TokenIdentifier, Value: "Widget"},
				{Type: ast.TokenIdentifier, Value: "new"},
			},
			wantPackage:  "Yutani",
			wantClass:    "Widget",
			wantSelector: "new",
		},
		{
			name: "qualified class with keyword message",
			tokens: []ast.Token{
				{Type: ast.TokenAt, Value: "@"},
				{Type: ast.TokenIdentifier, Value: "Core"},
				{Type: ast.TokenNamespaceSep, Value: "::"},
				{Type: ast.TokenIdentifier, Value: "Array"},
				{Type: ast.TokenKeyword, Value: "with:"},
				{Type: ast.TokenNumber, Value: "42"},
			},
			wantPackage:  "Core",
			wantClass:    "Array",
			wantSelector: "with_",
		},
		{
			name: "unqualified class message send",
			tokens: []ast.Token{
				{Type: ast.TokenAt, Value: "@"},
				{Type: ast.TokenIdentifier, Value: "Counter"},
				{Type: ast.TokenIdentifier, Value: "new"},
			},
			wantPackage:  "",
			wantClass:    "Counter",
			wantSelector: "new",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := newParser(tt.tokens)
			result, err := p.parsePrimary()
			if err != nil {
				t.Fatalf("parsePrimary() error = %v", err)
			}

			msg, ok := result.(*MessageSend)
			if !ok {
				t.Fatalf("expected MessageSend, got %T", result)
			}

			if msg.Selector != tt.wantSelector {
				t.Errorf("Selector = %q, want %q", msg.Selector, tt.wantSelector)
			}

			if tt.wantPackage != "" {
				// Should be a QualifiedName
				qn, ok := msg.Receiver.(*QualifiedName)
				if !ok {
					t.Fatalf("expected QualifiedName receiver, got %T", msg.Receiver)
				}
				if qn.Package != tt.wantPackage {
					t.Errorf("Package = %q, want %q", qn.Package, tt.wantPackage)
				}
				if qn.Name != tt.wantClass {
					t.Errorf("Name = %q, want %q", qn.Name, tt.wantClass)
				}
				// Test FullName method
				expectedFullName := tt.wantPackage + "::" + tt.wantClass
				if qn.FullName() != expectedFullName {
					t.Errorf("FullName() = %q, want %q", qn.FullName(), expectedFullName)
				}
			} else {
				// Should be an Identifier
				ident, ok := msg.Receiver.(*Identifier)
				if !ok {
					t.Fatalf("expected Identifier receiver, got %T", msg.Receiver)
				}
				if ident.Name != tt.wantClass {
					t.Errorf("Name = %q, want %q", ident.Name, tt.wantClass)
				}
			}
		})
	}
}

func TestParseIterationExpr(t *testing.T) {
	tests := []struct {
		name       string
		tokens     []ast.Token
		wantKind   string
		wantVar    string
		wantStmts  int
	}{
		{
			name: "do: iteration",
			tokens: []ast.Token{
				{Type: ast.TokenIdentifier, Value: "items"},
				{Type: ast.TokenKeyword, Value: "do:"},
				{Type: ast.TokenLBracket, Value: "["},
				{Type: ast.TokenBlockParam, Value: "each"},
				{Type: ast.TokenPipe, Value: "|"},
				{Type: ast.TokenIdentifier, Value: "sum"},
				{Type: ast.TokenAssign, Value: ":="},
				{Type: ast.TokenIdentifier, Value: "sum"},
				{Type: ast.TokenPlus, Value: "+"},
				{Type: ast.TokenIdentifier, Value: "each"},
				{Type: ast.TokenRBracket, Value: "]"},
			},
			wantKind:  "do",
			wantVar:   "each",
			wantStmts: 1,
		},
		{
			name: "collect: iteration",
			tokens: []ast.Token{
				{Type: ast.TokenIdentifier, Value: "items"},
				{Type: ast.TokenKeyword, Value: "collect:"},
				{Type: ast.TokenLBracket, Value: "["},
				{Type: ast.TokenBlockParam, Value: "x"},
				{Type: ast.TokenPipe, Value: "|"},
				{Type: ast.TokenIdentifier, Value: "x"},
				{Type: ast.TokenStar, Value: "*"},
				{Type: ast.TokenNumber, Value: "2"},
				{Type: ast.TokenRBracket, Value: "]"},
			},
			wantKind:  "collect",
			wantVar:   "x",
			wantStmts: 1,
		},
		{
			name: "select: iteration",
			tokens: []ast.Token{
				{Type: ast.TokenIdentifier, Value: "items"},
				{Type: ast.TokenKeyword, Value: "select:"},
				{Type: ast.TokenLBracket, Value: "["},
				{Type: ast.TokenBlockParam, Value: "x"},
				{Type: ast.TokenPipe, Value: "|"},
				{Type: ast.TokenIdentifier, Value: "x"},
				{Type: ast.TokenGT, Value: ">"},
				{Type: ast.TokenNumber, Value: "0"},
				{Type: ast.TokenRBracket, Value: "]"},
			},
			wantKind:  "select",
			wantVar:   "x",
			wantStmts: 1,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := newParser(tt.tokens)
			result, err := p.parseStatement()
			if err != nil {
				t.Fatalf("parseStatement() error = %v", err)
			}

			iter, ok := result.(*IterationExpr)
			if !ok {
				t.Fatalf("expected IterationExpr, got %T", result)
			}

			if iter.Kind != tt.wantKind {
				t.Errorf("Kind = %q, want %q", iter.Kind, tt.wantKind)
			}

			if iter.IterVar != tt.wantVar {
				t.Errorf("IterVar = %q, want %q", iter.IterVar, tt.wantVar)
			}

			if len(iter.Body) != tt.wantStmts {
				t.Errorf("got %d body statements, want %d", len(iter.Body), tt.wantStmts)
			}
		})
	}
}
