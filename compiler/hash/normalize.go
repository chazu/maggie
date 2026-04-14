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

// normalizeMethod transforms a compiler MethodDef into a frozen hMethodDef.
//
// instVars maps instance variable names to their index in the class's
// all-instance-variables list. resolveGlobal maps a bare class/global name
// to its fully-qualified name (e.g., "Button" → "Widgets::Button").
func normalizeMethod(method *compiler.MethodDef, instVars map[string]int, resolveGlobal func(string) string) *hMethodDef {
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
		return &hMethodDef{
			Selector:  method.Selector,
			Arity:     len(method.Parameters),
			NumTemps:  len(method.Parameters) + len(method.Temps),
			Primitive: method.Primitive,
			DocString: method.DocString,
		}
	}

	stmts := make([]hNode, len(method.Statements))
	for i, s := range method.Statements {
		stmts[i] = n.normalizeStmt(s)
	}

	return &hMethodDef{
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

func (n *normalizer) normalizeStmt(stmt compiler.Stmt) hNode {
	switch s := stmt.(type) {
	case *compiler.ExprStmt:
		return &hExprStmt{Expr: n.normalizeExpr(s.Expr)}
	case *compiler.Return:
		return &hReturn{Value: n.normalizeExpr(s.Value)}
	default:
		// Unknown statement type — should not happen
		return &hNilLiteral{}
	}
}

// ---------------------------------------------------------------------------
// Expression normalization
// ---------------------------------------------------------------------------

func (n *normalizer) normalizeExpr(expr compiler.Expr) hNode {
	switch e := expr.(type) {
	case *compiler.IntLiteral:
		return &hIntLiteral{Value: e.Value}
	case *compiler.FloatLiteral:
		return &hFloatLiteral{Value: e.Value}
	case *compiler.StringLiteral:
		return &hStringLiteral{Value: e.Value}
	case *compiler.SymbolLiteral:
		return &hSymbolLiteral{Value: e.Value}
	case *compiler.CharLiteral:
		return &hCharLiteral{Value: e.Value}
	case *compiler.NilLiteral:
		return &hNilLiteral{}
	case *compiler.TrueLiteral:
		return &hBoolLiteral{Value: true}
	case *compiler.FalseLiteral:
		return &hBoolLiteral{Value: false}
	case *compiler.Self:
		return &hSelfRef{}
	case *compiler.Super:
		return &hSuperRef{}
	case *compiler.ThisContext:
		return &hThisContext{}

	case *compiler.ArrayLiteral:
		elems := make([]hNode, len(e.Elements))
		for i, el := range e.Elements {
			elems[i] = n.normalizeExpr(el)
		}
		return &hArrayLiteral{Elements: elems}

	case *compiler.DictionaryLiteral:
		keys := make([]hNode, len(e.Keys))
		vals := make([]hNode, len(e.Values))
		for i := range e.Keys {
			keys[i] = n.normalizeExpr(e.Keys[i])
			vals[i] = n.normalizeExpr(e.Values[i])
		}
		return &hDictLiteral{Keys: keys, Values: vals}

	case *compiler.DynamicArray:
		elems := make([]hNode, len(e.Elements))
		for i, el := range e.Elements {
			elems[i] = n.normalizeExpr(el)
		}
		return &hDynamicArray{Elements: elems}

	case *compiler.Variable:
		return n.resolveVariable(e.Name)

	case *compiler.Assignment:
		target := n.resolveVariable(e.Variable)
		value := n.normalizeExpr(e.Value)
		return &hAssignment{Target: target, Value: value}

	case *compiler.UnaryMessage:
		return &hUnaryMessage{
			Receiver: n.normalizeExpr(e.Receiver),
			Selector: e.Selector,
		}

	case *compiler.BinaryMessage:
		return &hBinaryMessage{
			Receiver: n.normalizeExpr(e.Receiver),
			Selector: e.Selector,
			Argument: n.normalizeExpr(e.Argument),
		}

	case *compiler.KeywordMessage:
		args := make([]hNode, len(e.Arguments))
		for i, a := range e.Arguments {
			args[i] = n.normalizeExpr(a)
		}
		return &hKeywordMessage{
			Receiver:  n.normalizeExpr(e.Receiver),
			Selector:  e.Selector,
			Arguments: args,
		}

	case *compiler.Cascade:
		msgs := make([]hCascadedMessage, len(e.Messages))
		for i, m := range e.Messages {
			args := make([]hNode, len(m.Arguments))
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
			msgs[i] = hCascadedMessage{
				Type:      msgType,
				Selector:  m.Selector,
				Arguments: args,
			}
		}
		return &hCascade{
			Receiver: n.normalizeExpr(e.Receiver),
			Messages: msgs,
		}

	case *compiler.Block:
		return n.normalizeBlock(e)

	default:
		return &hNilLiteral{}
	}
}

// ---------------------------------------------------------------------------
// Variable resolution → de Bruijn indices
// ---------------------------------------------------------------------------

// resolveVariable resolves a variable name to hLocalVarRef, hInstanceVarRef,
// or hGlobalRef. This mirrors the resolution order in codegen.go:438-489.
func (n *normalizer) resolveVariable(name string) hNode {
	// 1. Search scopes from innermost to outermost
	for depth := len(n.scopes) - 1; depth >= 0; depth-- {
		if slot, ok := n.scopes[depth].vars[name]; ok {
			return &hLocalVarRef{
				ScopeDepth: uint16(len(n.scopes) - 1 - depth),
				SlotIndex:  slot,
			}
		}
	}

	// 2. Instance variables
	if idx, ok := n.instVars[name]; ok {
		return &hInstanceVarRef{Index: uint16(idx)}
	}

	// 3. Global — resolve to FQN
	return &hGlobalRef{FQN: n.resolveGlobal(name)}
}

// ---------------------------------------------------------------------------
// Block normalization
// ---------------------------------------------------------------------------

func (n *normalizer) normalizeBlock(block *compiler.Block) *hBlock {
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

	stmts := make([]hNode, len(block.Statements))
	for i, s := range block.Statements {
		stmts[i] = n.normalizeStmt(s)
	}

	// Pop scope
	n.scopes = n.scopes[:len(n.scopes)-1]

	return &hBlock{
		Arity:      len(block.Parameters),
		NumTemps:   len(block.Parameters) + len(block.Temps),
		Statements: stmts,
	}
}
