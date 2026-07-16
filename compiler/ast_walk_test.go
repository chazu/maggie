package compiler

import (
	"go/ast"
	"go/parser"
	"go/token"
	"testing"
)

// astNodeTypeNames parses ast.go and returns the receiver type names of all
// expr() and stmt() marker methods — i.e., every AST node type that exists,
// derived from the source itself so this test cannot go stale.
func astNodeTypeNames(t *testing.T) (exprs, stmts []string) {
	t.Helper()
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "ast.go", nil, 0)
	if err != nil {
		t.Fatalf("parse ast.go: %v", err)
	}
	for _, decl := range f.Decls {
		fd, ok := decl.(*ast.FuncDecl)
		if !ok || fd.Recv == nil || len(fd.Recv.List) != 1 {
			continue
		}
		star, ok := fd.Recv.List[0].Type.(*ast.StarExpr)
		if !ok {
			continue
		}
		ident, ok := star.X.(*ast.Ident)
		if !ok {
			continue
		}
		switch fd.Name.Name {
		case "expr":
			exprs = append(exprs, ident.Name)
		case "stmt":
			stmts = append(stmts, ident.Name)
		}
	}
	return exprs, stmts
}

// exhaustiveExprSamples returns one instance of every Expr node type, keyed
// by type name. When a new node type is added to ast.go, this test fails
// until the new type is added here AND handled by Walk (and the hash
// serializer — see hash package's exhaustiveness test).
func exhaustiveExprSamples() map[string]Expr {
	leafVar := &Variable{Name: "x"}
	return map[string]Expr{
		"IntLiteral":        &IntLiteral{Value: 1},
		"FloatLiteral":      &FloatLiteral{Value: 1.5},
		"StringLiteral":     &StringLiteral{Value: "s"},
		"SymbolLiteral":     &SymbolLiteral{Value: "sym"},
		"CharLiteral":       &CharLiteral{Value: 'c'},
		"ArrayLiteral":      &ArrayLiteral{Elements: []Expr{leafVar}},
		"DictionaryLiteral": &DictionaryLiteral{Keys: []Expr{leafVar}, Values: []Expr{leafVar}},
		"DynamicArray":      &DynamicArray{Elements: []Expr{leafVar}},
		"Variable":          leafVar,
		"Assignment":        &Assignment{Variable: "x", Value: leafVar},
		"UnaryMessage":      &UnaryMessage{Receiver: leafVar, Selector: "foo"},
		"BinaryMessage":     &BinaryMessage{Receiver: leafVar, Selector: "+", Argument: leafVar},
		"KeywordMessage":    &KeywordMessage{Receiver: leafVar, Selector: "at:put:", Arguments: []Expr{leafVar, leafVar}},
		"Cascade": &Cascade{Receiver: leafVar, Messages: []CascadedMessage{
			{Type: KeywordMsg, Selector: "at:", Arguments: []Expr{leafVar}},
		}},
		"Block":        &Block{Statements: []Stmt{&ExprStmt{Expr: leafVar}}},
		"Self":         &Self{},
		"Super":        &Super{},
		"ThisContext":  &ThisContext{},
		"NilLiteral":   &NilLiteral{},
		"TrueLiteral":  &TrueLiteral{},
		"FalseLiteral": &FalseLiteral{},
	}
}

func exhaustiveStmtSamples() map[string]Stmt {
	leafVar := &Variable{Name: "x"}
	return map[string]Stmt{
		"ExprStmt": &ExprStmt{Expr: leafVar},
		"Return":   &Return{Value: leafVar},
	}
}

// TestWalk_CoversEveryNodeType walks one instance of every AST node type.
// Walk panics on unhandled types, so a node type added to ast.go without a
// Walk case fails here loudly instead of being silently mis-analyzed by
// every Walk-based analyzer.
func TestWalk_CoversEveryNodeType(t *testing.T) {
	exprNames, stmtNames := astNodeTypeNames(t)
	exprSamples := exhaustiveExprSamples()
	stmtSamples := exhaustiveStmtSamples()

	for _, name := range exprNames {
		sample, ok := exprSamples[name]
		if !ok {
			t.Errorf("no exhaustiveness sample for expression node %s — add one here and teach Walk/hash about the new node", name)
			continue
		}
		visited := 0
		Inspect(sample, func(n Node) bool { visited++; return true })
		if visited == 0 {
			t.Errorf("Walk visited no nodes for %s", name)
		}
	}

	for _, name := range stmtNames {
		sample, ok := stmtSamples[name]
		if !ok {
			t.Errorf("no exhaustiveness sample for statement node %s — add one here and teach Walk/hash about the new node", name)
			continue
		}
		visited := 0
		Inspect(sample, func(n Node) bool { visited++; return true })
		if visited == 0 {
			t.Errorf("Walk visited no nodes for %s", name)
		}
	}
}

// TestFindCellVariables_DynamicArray is the regression test for the walker
// drift bug: a block INSIDE a {…} dynamic array literal that assigns to an
// outer variable must force that variable into a cell. The hand-rolled
// predecessor of findCellVariables had no *DynamicArray case, so the block
// captured a stale copy and the write was silently lost.
func TestFindCellVariables_DynamicArray(t *testing.T) {
	source := `method: foo [ | x b |
  x := 1.
  b := { [ x := 2 ] }.
  (b at: 1) value.
  ^x
]`
	method, err := ParseMethodDef(source)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}

	c := NewCompiler(nil, nil, nil)
	cellVars := c.findCellVariables(method)
	if !cellVars["x"] {
		t.Fatalf("x must be a cell variable (assigned from a block inside a dynamic array); got %v", cellVars)
	}
}
