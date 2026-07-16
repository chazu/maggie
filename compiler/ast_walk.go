package compiler

import "fmt"

// ---------------------------------------------------------------------------
// Generic AST traversal
// ---------------------------------------------------------------------------
//
// Walk is the ONE place that encodes the AST's shape for analyzers.
// Historically every analyzer hand-rolled its own switch-based walker, and
// each new node type had to be added to all of them; a forgotten case meant
// silent mis-analysis (findCellVariables missed *DynamicArray, so mutations
// from blocks inside {…} literals were lost). Walk panics on unknown node
// types instead — adding a node type without teaching Walk about it is a
// loud dev-time failure, enforced by TestWalk_CoversEveryNodeType.
//
// Transformers that must produce a value per node (the hash normalizer, the
// type inferrer) keep their own switches; they are guarded by the same
// exhaustiveness test.

// Visitor's Visit method is invoked for each node encountered by Walk. If
// the returned visitor w is non-nil, Walk visits each child of the node
// with w, followed by a call of w.Visit(nil) — the exit event scope-tracking
// visitors use to pop.
type Visitor interface {
	Visit(node Node) (w Visitor)
}

// Walk traverses an AST in depth-first order: it starts by calling
// v.Visit(node); node must not be nil. If the visitor returned by
// v.Visit(node) is not nil, Walk is invoked recursively with that visitor
// for each non-nil child of node, followed by a call of w.Visit(nil).
//
// Mirrors go/ast.Walk.
func Walk(v Visitor, node Node) {
	if v = v.Visit(node); v == nil {
		return
	}

	switch n := node.(type) {
	// Leaves
	case *IntLiteral, *FloatLiteral, *StringLiteral, *SymbolLiteral,
		*CharLiteral, *NilLiteral, *TrueLiteral, *FalseLiteral,
		*Variable, *Self, *Super, *ThisContext:
		// no children

	case *ArrayLiteral:
		for _, elem := range n.Elements {
			walkIfNotNil(v, elem)
		}
	case *DynamicArray:
		for _, elem := range n.Elements {
			walkIfNotNil(v, elem)
		}
	case *DictionaryLiteral:
		for _, k := range n.Keys {
			walkIfNotNil(v, k)
		}
		for _, val := range n.Values {
			walkIfNotNil(v, val)
		}

	case *Assignment:
		walkIfNotNil(v, n.Value)
	case *UnaryMessage:
		walkIfNotNil(v, n.Receiver)
	case *BinaryMessage:
		walkIfNotNil(v, n.Receiver)
		walkIfNotNil(v, n.Argument)
	case *KeywordMessage:
		walkIfNotNil(v, n.Receiver)
		for _, arg := range n.Arguments {
			walkIfNotNil(v, arg)
		}
	case *Cascade:
		walkIfNotNil(v, n.Receiver)
		// CascadedMessage is not a Node; its arguments are walked directly.
		for _, msg := range n.Messages {
			for _, arg := range msg.Arguments {
				walkIfNotNil(v, arg)
			}
		}

	case *Block:
		for _, stmt := range n.Statements {
			walkIfNotNil(v, stmt)
		}

	case *ExprStmt:
		walkIfNotNil(v, n.Expr)
	case *Return:
		walkIfNotNil(v, n.Value)

	default:
		panic(fmt.Sprintf("compiler.Walk: unhandled node type %T — teach Walk (and the hash serializer) about it", node))
	}

	v.Visit(nil)
}

func walkIfNotNil(v Visitor, node Node) {
	if node == nil {
		return
	}
	Walk(v, node)
}

// WalkMethod walks every statement of a method body with v. (MethodDef is
// not itself a Node, so it has its own entry point.)
func WalkMethod(v Visitor, method *MethodDef) {
	for _, stmt := range method.Statements {
		walkIfNotNil(v, stmt)
	}
}

// inspector adapts a function to the Visitor interface (like go/ast.Inspect,
// without exit events).
type inspector func(Node) bool

func (f inspector) Visit(node Node) Visitor {
	if node != nil && f(node) {
		return f
	}
	return nil
}

// Inspect traverses the AST calling f(node) for each non-nil node. If f
// returns false, the node's children are skipped.
func Inspect(node Node, f func(Node) bool) {
	Walk(inspector(f), node)
}
