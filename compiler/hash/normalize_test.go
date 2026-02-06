package hash

import (
	"testing"

	"github.com/chazu/maggie/compiler"
)

// helper: make a simple method with statements
func methodDef(selector string, params, temps []string, stmts ...compiler.Stmt) *compiler.MethodDef {
	return &compiler.MethodDef{
		Selector:   selector,
		Parameters: params,
		Temps:      temps,
		Statements: stmts,
	}
}

func exprStmt(e compiler.Expr) *compiler.ExprStmt {
	return &compiler.ExprStmt{Expr: e}
}

func variable(name string) *compiler.Variable {
	return &compiler.Variable{Name: name}
}

func intLit(v int64) *compiler.IntLiteral {
	return &compiler.IntLiteral{Value: v}
}

func TestNormalize_SimpleMethod_ParamResolution(t *testing.T) {
	// method: foo: x [ ^x ]
	md := methodDef("foo:", []string{"x"}, nil,
		&compiler.Return{Value: variable("x")},
	)

	hm := NormalizeMethod(md, nil, nil)

	if hm.Selector != "foo:" {
		t.Errorf("selector: got %q, want %q", hm.Selector, "foo:")
	}
	if hm.Arity != 1 {
		t.Errorf("arity: got %d, want 1", hm.Arity)
	}
	if len(hm.Statements) != 1 {
		t.Fatalf("statements: got %d, want 1", len(hm.Statements))
	}

	ret, ok := hm.Statements[0].(*HReturn)
	if !ok {
		t.Fatalf("statement[0]: got %T, want *HReturn", hm.Statements[0])
	}
	ref, ok := ret.Value.(*HLocalVarRef)
	if !ok {
		t.Fatalf("return value: got %T, want *HLocalVarRef", ret.Value)
	}
	if ref.ScopeDepth != 0 || ref.SlotIndex != 0 {
		t.Errorf("var ref: got depth=%d slot=%d, want depth=0 slot=0", ref.ScopeDepth, ref.SlotIndex)
	}
}

func TestNormalize_MethodWithTemps(t *testing.T) {
	// method: foo: x | y | y := x
	md := methodDef("foo:", []string{"x"}, []string{"y"},
		exprStmt(&compiler.Assignment{Variable: "y", Value: variable("x")}),
	)

	hm := NormalizeMethod(md, nil, nil)

	if hm.NumTemps != 2 { // 1 param + 1 temp
		t.Errorf("numTemps: got %d, want 2", hm.NumTemps)
	}

	stmt := hm.Statements[0].(*HExprStmt)
	assign, ok := stmt.Expr.(*HAssignment)
	if !ok {
		t.Fatalf("got %T, want *HAssignment", stmt.Expr)
	}

	// Target (y) should be slot 1 (after param x at slot 0)
	target, ok := assign.Target.(*HLocalVarRef)
	if !ok {
		t.Fatalf("target: got %T, want *HLocalVarRef", assign.Target)
	}
	if target.SlotIndex != 1 {
		t.Errorf("target slot: got %d, want 1", target.SlotIndex)
	}

	// Value (x) should be slot 0
	val, ok := assign.Value.(*HLocalVarRef)
	if !ok {
		t.Fatalf("value: got %T, want *HLocalVarRef", assign.Value)
	}
	if val.SlotIndex != 0 {
		t.Errorf("value slot: got %d, want 0", val.SlotIndex)
	}
}

func TestNormalize_AlphaEquivalence(t *testing.T) {
	// [:x | x + 1] and [:y | y + 1] should produce identical hashing ASTs
	blockX := &compiler.Block{
		Parameters: []string{"x"},
		Statements: []compiler.Stmt{
			exprStmt(&compiler.BinaryMessage{
				Receiver: variable("x"),
				Selector: "+",
				Argument: intLit(1),
			}),
		},
	}
	blockY := &compiler.Block{
		Parameters: []string{"y"},
		Statements: []compiler.Stmt{
			exprStmt(&compiler.BinaryMessage{
				Receiver: variable("y"),
				Selector: "+",
				Argument: intLit(1),
			}),
		},
	}

	mdX := methodDef("test", nil, nil, exprStmt(blockX))
	mdY := methodDef("test", nil, nil, exprStmt(blockY))

	hmX := NormalizeMethod(mdX, nil, nil)
	hmY := NormalizeMethod(mdY, nil, nil)

	dataX := Serialize(hmX)
	dataY := Serialize(hmY)

	if string(dataX) != string(dataY) {
		t.Error("alpha-equivalent blocks should produce identical serializations")
	}
}

func TestNormalize_NestedBlocks_DeBruijn(t *testing.T) {
	// method: foo: a [ [:b | [:c | a + b + c]] ]
	innerBlock := &compiler.Block{
		Parameters: []string{"c"},
		Statements: []compiler.Stmt{
			exprStmt(&compiler.BinaryMessage{
				Receiver: &compiler.BinaryMessage{
					Receiver: variable("a"),
					Selector: "+",
					Argument: variable("b"),
				},
				Selector: "+",
				Argument: variable("c"),
			}),
		},
	}
	outerBlock := &compiler.Block{
		Parameters: []string{"b"},
		Statements: []compiler.Stmt{
			exprStmt(innerBlock),
		},
	}
	md := methodDef("foo:", []string{"a"}, nil,
		exprStmt(outerBlock),
	)

	hm := NormalizeMethod(md, nil, nil)

	// Drill down to the inner block
	es := hm.Statements[0].(*HExprStmt)
	ob := es.Expr.(*HBlock)
	ibes := ob.Statements[0].(*HExprStmt)
	ib := ibes.Expr.(*HBlock)

	// In the inner block body: a + b + c
	// c is depth=0, slot=0 (inner block param)
	// b is depth=1, slot=0 (outer block param)
	// a is depth=2, slot=0 (method param)
	bodyES := ib.Statements[0].(*HExprStmt)
	outerPlus := bodyES.Expr.(*HBinaryMessage)

	// outerPlus.Argument = c (depth=0, slot=0)
	cRef := outerPlus.Argument.(*HLocalVarRef)
	if cRef.ScopeDepth != 0 || cRef.SlotIndex != 0 {
		t.Errorf("c ref: got depth=%d slot=%d, want depth=0 slot=0", cRef.ScopeDepth, cRef.SlotIndex)
	}

	// outerPlus.Receiver = (a + b), a BinaryMessage
	innerPlus := outerPlus.Receiver.(*HBinaryMessage)

	// innerPlus.Receiver = a (depth=2, slot=0)
	aRef := innerPlus.Receiver.(*HLocalVarRef)
	if aRef.ScopeDepth != 2 || aRef.SlotIndex != 0 {
		t.Errorf("a ref: got depth=%d slot=%d, want depth=2 slot=0", aRef.ScopeDepth, aRef.SlotIndex)
	}

	// innerPlus.Argument = b (depth=1, slot=0)
	bRef := innerPlus.Argument.(*HLocalVarRef)
	if bRef.ScopeDepth != 1 || bRef.SlotIndex != 0 {
		t.Errorf("b ref: got depth=%d slot=%d, want depth=1 slot=0", bRef.ScopeDepth, bRef.SlotIndex)
	}
}

func TestNormalize_InstanceVariables(t *testing.T) {
	// method: name [ ^name ] with instVars = {"name": 0, "age": 1}
	md := methodDef("name", nil, nil,
		&compiler.Return{Value: variable("name")},
	)
	instVars := map[string]int{"name": 0, "age": 1}

	hm := NormalizeMethod(md, instVars, nil)

	ret := hm.Statements[0].(*HReturn)
	ref, ok := ret.Value.(*HInstanceVarRef)
	if !ok {
		t.Fatalf("got %T, want *HInstanceVarRef", ret.Value)
	}
	if ref.Index != 0 {
		t.Errorf("ivar index: got %d, want 0", ref.Index)
	}
}

func TestNormalize_GlobalRef(t *testing.T) {
	// method: test [ ^Button ] with resolveGlobal("Button") = "Widgets::Button"
	md := methodDef("test", nil, nil,
		&compiler.Return{Value: variable("Button")},
	)
	resolve := func(name string) string {
		if name == "Button" {
			return "Widgets::Button"
		}
		return name
	}

	hm := NormalizeMethod(md, nil, resolve)

	ret := hm.Statements[0].(*HReturn)
	ref, ok := ret.Value.(*HGlobalRef)
	if !ok {
		t.Fatalf("got %T, want *HGlobalRef", ret.Value)
	}
	if ref.FQN != "Widgets::Button" {
		t.Errorf("FQN: got %q, want %q", ref.FQN, "Widgets::Button")
	}
}

func TestNormalize_SelfSuperNilTrueFalseThisContext(t *testing.T) {
	md := methodDef("test", nil, nil,
		exprStmt(&compiler.Self{}),
		exprStmt(&compiler.Super{}),
		exprStmt(&compiler.NilLiteral{}),
		exprStmt(&compiler.TrueLiteral{}),
		exprStmt(&compiler.FalseLiteral{}),
		exprStmt(&compiler.ThisContext{}),
	)

	hm := NormalizeMethod(md, nil, nil)

	types := []string{"*HSelfRef", "*HSuperRef", "*HNilLiteral", "*HBoolLiteral(true)", "*HBoolLiteral(false)", "*HThisContext"}

	checks := []func(HNode) bool{
		func(n HNode) bool { _, ok := n.(*HSelfRef); return ok },
		func(n HNode) bool { _, ok := n.(*HSuperRef); return ok },
		func(n HNode) bool { _, ok := n.(*HNilLiteral); return ok },
		func(n HNode) bool { b, ok := n.(*HBoolLiteral); return ok && b.Value },
		func(n HNode) bool { b, ok := n.(*HBoolLiteral); return ok && !b.Value },
		func(n HNode) bool { _, ok := n.(*HThisContext); return ok },
	}

	for i, check := range checks {
		es := hm.Statements[i].(*HExprStmt)
		if !check(es.Expr) {
			t.Errorf("statement[%d]: expected %s, got %T", i, types[i], es.Expr)
		}
	}
}

func TestNormalize_PrimitiveMethod(t *testing.T) {
	md := &compiler.MethodDef{
		Selector:   "printString",
		Parameters: nil,
		Primitive:  42,
		DocString:  "Prints a string",
	}

	hm := NormalizeMethod(md, nil, nil)

	if hm.Primitive != 42 {
		t.Errorf("primitive: got %d, want 42", hm.Primitive)
	}
	if hm.DocString != "Prints a string" {
		t.Errorf("docstring: got %q", hm.DocString)
	}
	if len(hm.Statements) != 0 {
		t.Errorf("primitive methods should have no statements, got %d", len(hm.Statements))
	}
}

func TestNormalize_DocStringAffectsHash(t *testing.T) {
	md1 := methodDef("test", nil, nil, exprStmt(intLit(1)))
	md1.DocString = "First doc"

	md2 := methodDef("test", nil, nil, exprStmt(intLit(1)))
	md2.DocString = "Second doc"

	h1 := HashMethod(md1, nil, nil)
	h2 := HashMethod(md2, nil, nil)

	if h1 == h2 {
		t.Error("different docstrings should produce different hashes")
	}
}

func TestNormalize_DeeplyNestedBlocks(t *testing.T) {
	// 3 levels: method -> block1 -> block2 -> block3
	// method: foo: a [ [:b | [:c | [:d | a]]] ]
	block3 := &compiler.Block{
		Parameters: []string{"d"},
		Statements: []compiler.Stmt{exprStmt(variable("a"))},
	}
	block2 := &compiler.Block{
		Parameters: []string{"c"},
		Statements: []compiler.Stmt{exprStmt(block3)},
	}
	block1 := &compiler.Block{
		Parameters: []string{"b"},
		Statements: []compiler.Stmt{exprStmt(block2)},
	}
	md := methodDef("foo:", []string{"a"}, nil, exprStmt(block1))

	hm := NormalizeMethod(md, nil, nil)

	// Drill to innermost block
	b1 := hm.Statements[0].(*HExprStmt).Expr.(*HBlock)
	b2 := b1.Statements[0].(*HExprStmt).Expr.(*HBlock)
	b3 := b2.Statements[0].(*HExprStmt).Expr.(*HBlock)

	// In block3, 'a' is at depth=3 (method scope), slot=0
	aRef := b3.Statements[0].(*HExprStmt).Expr.(*HLocalVarRef)
	if aRef.ScopeDepth != 3 || aRef.SlotIndex != 0 {
		t.Errorf("a ref in block3: got depth=%d slot=%d, want depth=3 slot=0", aRef.ScopeDepth, aRef.SlotIndex)
	}
}
