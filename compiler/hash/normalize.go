package hash

import (
	"github.com/chazu/maggie/compiler"
)

// ---------------------------------------------------------------------------
// AST Normalization: compiler AST → frozen hashing AST
//
// Walks the compiler's working AST and produces the frozen hashing AST with
// de Bruijn indices for local variables, positional indices for instance
// variables, and fully-qualified names for globals.
// ---------------------------------------------------------------------------

// scope tracks variables at one nesting level.
type scope struct {
	vars map[string]uint16 // variable name → slot index
}

// normalizer holds state for the normalization walk.
type normalizer struct {
	scopes        []scope            // scope stack: [0]=method, [1]=first block, etc.
	instVars      map[string]int     // instance variable name → index
	resolveGlobal func(string) string // bare name → FQN
}

// NormalizeMethod transforms a compiler MethodDef into a frozen HMethodDef.
//
// instVars maps instance variable names to their index in the class's
// all-instance-variables list. resolveGlobal maps a bare class/global name
// to its fully-qualified name (e.g., "Button" → "Widgets::Button").
func NormalizeMethod(method *compiler.MethodDef, instVars map[string]int, resolveGlobal func(string) string) *HMethodDef {
	if resolveGlobal == nil {
		resolveGlobal = func(name string) string { return name }
	}
	if instVars == nil {
		instVars = make(map[string]int)
	}

	n := &normalizer{
		instVars:      instVars,
		resolveGlobal: resolveGlobal,
	}

	// Push method-level scope with parameters and temps.
	methodVars := make(map[string]uint16)
	slot := uint16(0)
	for _, p := range method.Parameters {
		methodVars[p] = slot
		slot++
	}
	for _, t := range method.Temps {
		methodVars[t] = slot
		slot++
	}
	n.scopes = []scope{{vars: methodVars}}

	// Handle primitive methods
	if method.Primitive > 0 {
		return &HMethodDef{
			Selector:  method.Selector,
			Arity:     len(method.Parameters),
			NumTemps:  len(method.Parameters) + len(method.Temps),
			Primitive: method.Primitive,
			DocString: method.DocString,
		}
	}

	stmts := make([]HNode, len(method.Statements))
	for i, s := range method.Statements {
		stmts[i] = n.normalizeStmt(s)
	}

	return &HMethodDef{
		Selector:   method.Selector,
		Arity:      len(method.Parameters),
		NumTemps:   len(method.Parameters) + len(method.Temps),
		Primitive:  0,
		DocString:  method.DocString,
		Statements: stmts,
	}
}

// ---------------------------------------------------------------------------
// Statement normalization
// ---------------------------------------------------------------------------

func (n *normalizer) normalizeStmt(stmt compiler.Stmt) HNode {
	switch s := stmt.(type) {
	case *compiler.ExprStmt:
		return &HExprStmt{Expr: n.normalizeExpr(s.Expr)}
	case *compiler.Return:
		return &HReturn{Value: n.normalizeExpr(s.Value)}
	default:
		// Unknown statement type — should not happen
		return &HNilLiteral{}
	}
}

// ---------------------------------------------------------------------------
// Expression normalization
// ---------------------------------------------------------------------------

func (n *normalizer) normalizeExpr(expr compiler.Expr) HNode {
	switch e := expr.(type) {
	case *compiler.IntLiteral:
		return &HIntLiteral{Value: e.Value}
	case *compiler.FloatLiteral:
		return &HFloatLiteral{Value: e.Value}
	case *compiler.StringLiteral:
		return &HStringLiteral{Value: e.Value}
	case *compiler.SymbolLiteral:
		return &HSymbolLiteral{Value: e.Value}
	case *compiler.CharLiteral:
		return &HCharLiteral{Value: e.Value}
	case *compiler.NilLiteral:
		return &HNilLiteral{}
	case *compiler.TrueLiteral:
		return &HBoolLiteral{Value: true}
	case *compiler.FalseLiteral:
		return &HBoolLiteral{Value: false}
	case *compiler.Self:
		return &HSelfRef{}
	case *compiler.Super:
		return &HSuperRef{}
	case *compiler.ThisContext:
		return &HThisContext{}

	case *compiler.ArrayLiteral:
		elems := make([]HNode, len(e.Elements))
		for i, el := range e.Elements {
			elems[i] = n.normalizeExpr(el)
		}
		return &HArrayLiteral{Elements: elems}

	case *compiler.DynamicArray:
		elems := make([]HNode, len(e.Elements))
		for i, el := range e.Elements {
			elems[i] = n.normalizeExpr(el)
		}
		return &HDynamicArray{Elements: elems}

	case *compiler.Variable:
		return n.resolveVariable(e.Name)

	case *compiler.Assignment:
		target := n.resolveVariable(e.Variable)
		value := n.normalizeExpr(e.Value)
		return &HAssignment{Target: target, Value: value}

	case *compiler.UnaryMessage:
		return &HUnaryMessage{
			Receiver: n.normalizeExpr(e.Receiver),
			Selector: e.Selector,
		}

	case *compiler.BinaryMessage:
		return &HBinaryMessage{
			Receiver: n.normalizeExpr(e.Receiver),
			Selector: e.Selector,
			Argument: n.normalizeExpr(e.Argument),
		}

	case *compiler.KeywordMessage:
		args := make([]HNode, len(e.Arguments))
		for i, a := range e.Arguments {
			args[i] = n.normalizeExpr(a)
		}
		return &HKeywordMessage{
			Receiver:  n.normalizeExpr(e.Receiver),
			Selector:  e.Selector,
			Arguments: args,
		}

	case *compiler.Cascade:
		msgs := make([]HCascadedMessage, len(e.Messages))
		for i, m := range e.Messages {
			args := make([]HNode, len(m.Arguments))
			for j, a := range m.Arguments {
				args[j] = n.normalizeExpr(a)
			}
			var msgType byte
			switch m.Type {
			case compiler.UnaryMsg:
				msgType = TagCascadeUnary
			case compiler.BinaryMsg:
				msgType = TagCascadeBinary
			case compiler.KeywordMsg:
				msgType = TagCascadeKeyword
			}
			msgs[i] = HCascadedMessage{
				Type:      msgType,
				Selector:  m.Selector,
				Arguments: args,
			}
		}
		return &HCascade{
			Receiver: n.normalizeExpr(e.Receiver),
			Messages: msgs,
		}

	case *compiler.Block:
		return n.normalizeBlock(e)

	default:
		return &HNilLiteral{}
	}
}

// ---------------------------------------------------------------------------
// Variable resolution → de Bruijn indices
// ---------------------------------------------------------------------------

// resolveVariable resolves a variable name to HLocalVarRef, HInstanceVarRef,
// or HGlobalRef. This mirrors the resolution order in codegen.go:438-489.
func (n *normalizer) resolveVariable(name string) HNode {
	// 1. Search scopes from innermost to outermost
	for depth := len(n.scopes) - 1; depth >= 0; depth-- {
		if slot, ok := n.scopes[depth].vars[name]; ok {
			return &HLocalVarRef{
				ScopeDepth: uint16(len(n.scopes) - 1 - depth),
				SlotIndex:  slot,
			}
		}
	}

	// 2. Instance variables
	if idx, ok := n.instVars[name]; ok {
		return &HInstanceVarRef{Index: uint16(idx)}
	}

	// 3. Global — resolve to FQN
	return &HGlobalRef{FQN: n.resolveGlobal(name)}
}

// ---------------------------------------------------------------------------
// Block normalization
// ---------------------------------------------------------------------------

func (n *normalizer) normalizeBlock(block *compiler.Block) *HBlock {
	// Push a new scope for this block
	blockVars := make(map[string]uint16)
	slot := uint16(0)
	for _, p := range block.Parameters {
		blockVars[p] = slot
		slot++
	}
	for _, t := range block.Temps {
		blockVars[t] = slot
		slot++
	}
	n.scopes = append(n.scopes, scope{vars: blockVars})

	stmts := make([]HNode, len(block.Statements))
	for i, s := range block.Statements {
		stmts[i] = n.normalizeStmt(s)
	}

	// Pop scope
	n.scopes = n.scopes[:len(n.scopes)-1]

	return &HBlock{
		Arity:      len(block.Parameters),
		NumTemps:   len(block.Parameters) + len(block.Temps),
		Statements: stmts,
	}
}
