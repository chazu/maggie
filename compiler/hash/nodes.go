package hash

// ---------------------------------------------------------------------------
// Frozen hashing AST types.
//
// These are stripped-down parallels of compiler/ast.go with no Span/position
// data and de Bruijn indices instead of variable names. Two methods with the
// same semantics (same body, ignoring variable names) produce identical
// hashing ASTs.
// ---------------------------------------------------------------------------

// hNode is the interface implemented by all hashing AST nodes.
type hNode interface {
	hnode() // marker method
}

// ---------------------------------------------------------------------------
// Literal nodes
// ---------------------------------------------------------------------------

type hIntLiteral struct{ Value int64 }
type hFloatLiteral struct{ Value float64 }
type hStringLiteral struct{ Value string }
type hSymbolLiteral struct{ Value string }
type hCharLiteral struct{ Value rune }
type hBoolLiteral struct{ Value bool }
type hNilLiteral struct{}
type hSelfRef struct{}
type hSuperRef struct{}
type hThisContext struct{}

type hArrayLiteral struct{ Elements []hNode }
type hDictLiteral struct{ Keys, Values []hNode }
type hDynamicArray struct{ Elements []hNode }

func (*hIntLiteral) hnode()    {}
func (*hFloatLiteral) hnode()  {}
func (*hStringLiteral) hnode() {}
func (*hSymbolLiteral) hnode() {}
func (*hCharLiteral) hnode()   {}
func (*hBoolLiteral) hnode()   {}
func (*hNilLiteral) hnode()    {}
func (*hSelfRef) hnode()       {}
func (*hSuperRef) hnode()      {}
func (*hThisContext) hnode()   {}
func (*hArrayLiteral) hnode()  {}
func (*hDictLiteral) hnode()   {}
func (*hDynamicArray) hnode()  {}

// ---------------------------------------------------------------------------
// Variable reference nodes (de Bruijn indexed)
// ---------------------------------------------------------------------------

// hLocalVarRef references a local variable by de Bruijn indices.
// ScopeDepth 0 = current scope, 1 = one enclosing scope up, etc.
// SlotIndex is the position within that scope's parameter/temp list.
type hLocalVarRef struct {
	ScopeDepth uint16
	SlotIndex  uint16
}

// hInstanceVarRef references an instance variable by its index in the
// class's all-instance-variables list.
type hInstanceVarRef struct {
	Index uint16
}

// hGlobalRef references a global variable by its fully-qualified name.
type hGlobalRef struct {
	FQN string
}

func (*hLocalVarRef) hnode()    {}
func (*hInstanceVarRef) hnode() {}
func (*hGlobalRef) hnode()      {}

// ---------------------------------------------------------------------------
// Message send nodes
// ---------------------------------------------------------------------------

type hUnaryMessage struct {
	Receiver hNode
	Selector string
}

type hBinaryMessage struct {
	Receiver hNode
	Selector string
	Argument hNode
}

type hKeywordMessage struct {
	Receiver  hNode
	Selector  string
	Arguments []hNode
}

type hCascade struct {
	Receiver hNode
	Messages []hCascadedMessage
}

// hCascadedMessage is a message in a cascade.
type hCascadedMessage struct {
	Type      byte // TagCascadeUnary, TagCascadeBinary, or TagCascadeKeyword
	Selector  string
	Arguments []hNode
}

func (*hUnaryMessage) hnode()   {}
func (*hBinaryMessage) hnode()  {}
func (*hKeywordMessage) hnode() {}
func (*hCascade) hnode()        {}

// ---------------------------------------------------------------------------
// Statement / structure nodes
// ---------------------------------------------------------------------------

// hAssignment assigns a value to a variable target.
type hAssignment struct {
	Target hNode // hLocalVarRef, hInstanceVarRef, or hGlobalRef
	Value  hNode
}

type hReturn struct {
	Value hNode
}

type hExprStmt struct {
	Expr hNode
}

// hBlock represents a block closure, stripped of parameter names.
type hBlock struct {
	Arity      int
	NumTemps   int
	Statements []hNode

	// Type annotations (only populated for typed hashing)
	ParamTypes []hTypeAnnotation // nil slice = untyped mode
}

// hPrimitive represents a primitive call.
type hPrimitive struct {
	Number int
}

func (*hAssignment) hnode() {}
func (*hReturn) hnode()     {}
func (*hExprStmt) hnode()   {}
func (*hBlock) hnode()      {}
func (*hPrimitive) hnode()  {}

// ---------------------------------------------------------------------------
// Top-level definition nodes
// ---------------------------------------------------------------------------

// hTypeAnnotation represents a type annotation in the hashing AST.
// Empty Name means untyped/Dynamic.
type hTypeAnnotation struct {
	Name string // FQN-resolved type name, "" = untyped
}

// hMethodDef is the top-level hashing node for a method.
type hMethodDef struct {
	Selector   string
	Arity      int
	NumTemps   int
	Primitive  int
	DocString  string
	Statements []hNode

	// Type annotations (only populated for typed hashing)
	ParamTypes []hTypeAnnotation // parallel to params, nil slice = untyped mode
	TempTypes  []hTypeAnnotation // parallel to temps
	ReturnType hTypeAnnotation   // Name="" means untyped

	// Effect annotations (sorted for determinism, only for typed hashing)
	Effects []string // e.g., ["IO", "Network"]
}

func (*hMethodDef) hnode() {}
