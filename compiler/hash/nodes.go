package hash

// ---------------------------------------------------------------------------
// Frozen hashing AST types.
//
// These are stripped-down parallels of compiler/ast.go with no Span/position
// data and de Bruijn indices instead of variable names. Two methods with the
// same semantics (same body, ignoring variable names) produce identical
// hashing ASTs.
// ---------------------------------------------------------------------------

// HNode is the interface implemented by all hashing AST nodes.
type HNode interface {
	hnode() // marker method
}

// ---------------------------------------------------------------------------
// Literal nodes
// ---------------------------------------------------------------------------

type HIntLiteral struct{ Value int64 }
type HFloatLiteral struct{ Value float64 }
type HStringLiteral struct{ Value string }
type HSymbolLiteral struct{ Value string }
type HCharLiteral struct{ Value rune }
type HBoolLiteral struct{ Value bool }
type HNilLiteral struct{}
type HSelfRef struct{}
type HSuperRef struct{}
type HThisContext struct{}

type HArrayLiteral struct{ Elements []HNode }
type HDictLiteral struct{ Keys, Values []HNode }
type HDynamicArray struct{ Elements []HNode }

func (*HIntLiteral) hnode()    {}
func (*HFloatLiteral) hnode()  {}
func (*HStringLiteral) hnode() {}
func (*HSymbolLiteral) hnode() {}
func (*HCharLiteral) hnode()   {}
func (*HBoolLiteral) hnode()   {}
func (*HNilLiteral) hnode()    {}
func (*HSelfRef) hnode()       {}
func (*HSuperRef) hnode()      {}
func (*HThisContext) hnode()   {}
func (*HArrayLiteral) hnode()  {}
func (*HDictLiteral) hnode()   {}
func (*HDynamicArray) hnode()  {}

// ---------------------------------------------------------------------------
// Variable reference nodes (de Bruijn indexed)
// ---------------------------------------------------------------------------

// HLocalVarRef references a local variable by de Bruijn indices.
// ScopeDepth 0 = current scope, 1 = one enclosing scope up, etc.
// SlotIndex is the position within that scope's parameter/temp list.
type HLocalVarRef struct {
	ScopeDepth uint16
	SlotIndex  uint16
}

// HInstanceVarRef references an instance variable by its index in the
// class's all-instance-variables list.
type HInstanceVarRef struct {
	Index uint16
}

// HGlobalRef references a global variable by its fully-qualified name.
type HGlobalRef struct {
	FQN string
}

func (*HLocalVarRef) hnode()    {}
func (*HInstanceVarRef) hnode() {}
func (*HGlobalRef) hnode()      {}

// ---------------------------------------------------------------------------
// Message send nodes
// ---------------------------------------------------------------------------

type HUnaryMessage struct {
	Receiver HNode
	Selector string
}

type HBinaryMessage struct {
	Receiver HNode
	Selector string
	Argument HNode
}

type HKeywordMessage struct {
	Receiver  HNode
	Selector  string
	Arguments []HNode
}

type HCascade struct {
	Receiver HNode
	Messages []HCascadedMessage
}

// HCascadedMessage is a message in a cascade.
type HCascadedMessage struct {
	Type      byte // TagCascadeUnary, TagCascadeBinary, or TagCascadeKeyword
	Selector  string
	Arguments []HNode
}

func (*HUnaryMessage) hnode()   {}
func (*HBinaryMessage) hnode()  {}
func (*HKeywordMessage) hnode() {}
func (*HCascade) hnode()        {}

// ---------------------------------------------------------------------------
// Statement / structure nodes
// ---------------------------------------------------------------------------

// HAssignment assigns a value to a variable target.
type HAssignment struct {
	Target HNode // HLocalVarRef, HInstanceVarRef, or HGlobalRef
	Value  HNode
}

type HReturn struct {
	Value HNode
}

type HExprStmt struct {
	Expr HNode
}

// HBlock represents a block closure, stripped of parameter names.
type HBlock struct {
	Arity      int
	NumTemps   int
	Statements []HNode

	// Type annotations (only populated for typed hashing)
	ParamTypes []HTypeAnnotation // nil slice = untyped mode
}

// HPrimitive represents a primitive call.
type HPrimitive struct {
	Number int
}

func (*HAssignment) hnode() {}
func (*HReturn) hnode()     {}
func (*HExprStmt) hnode()   {}
func (*HBlock) hnode()      {}
func (*HPrimitive) hnode()  {}

// ---------------------------------------------------------------------------
// Top-level definition nodes
// ---------------------------------------------------------------------------

// HTypeAnnotation represents a type annotation in the hashing AST.
// Empty Name means untyped/Dynamic.
type HTypeAnnotation struct {
	Name string // FQN-resolved type name, "" = untyped
}

// HMethodDef is the top-level hashing node for a method.
type HMethodDef struct {
	Selector   string
	Arity      int
	NumTemps   int
	Primitive  int
	DocString  string
	Statements []HNode

	// Type annotations (only populated for typed hashing)
	ParamTypes []HTypeAnnotation // parallel to params, nil slice = untyped mode
	TempTypes  []HTypeAnnotation // parallel to temps
	ReturnType HTypeAnnotation   // Name="" means untyped

	// Effect annotations (sorted for determinism, only for typed hashing)
	Effects []string // e.g., ["IO", "Network"]
}

func (*HMethodDef) hnode() {}
