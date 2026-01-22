package compiler

// ---------------------------------------------------------------------------
// AST: Abstract Syntax Tree for Maggie/Smalltalk
// ---------------------------------------------------------------------------

// Position represents a source location.
type Position struct {
	Offset int // byte offset
	Line   int // 1-based line number
	Column int // 1-based column number
}

// Span represents a range in source code.
type Span struct {
	Start Position
	End   Position
}

// Node is the interface implemented by all AST nodes.
type Node interface {
	Span() Span
	node() // marker method
}

// ---------------------------------------------------------------------------
// Expression nodes
// ---------------------------------------------------------------------------

// Expr is the interface for expression nodes.
type Expr interface {
	Node
	expr() // marker method
}

// IntLiteral represents an integer literal.
type IntLiteral struct {
	SpanVal Span
	Value   int64
}

func (n *IntLiteral) Span() Span { return n.SpanVal }
func (n *IntLiteral) node()      {}
func (n *IntLiteral) expr()      {}

// FloatLiteral represents a floating-point literal.
type FloatLiteral struct {
	SpanVal Span
	Value   float64
}

func (n *FloatLiteral) Span() Span { return n.SpanVal }
func (n *FloatLiteral) node()      {}
func (n *FloatLiteral) expr()      {}

// StringLiteral represents a string literal.
type StringLiteral struct {
	SpanVal Span
	Value   string
}

func (n *StringLiteral) Span() Span { return n.SpanVal }
func (n *StringLiteral) node()      {}
func (n *StringLiteral) expr()      {}

// SymbolLiteral represents a symbol literal (#foo).
type SymbolLiteral struct {
	SpanVal Span
	Value   string
}

func (n *SymbolLiteral) Span() Span { return n.SpanVal }
func (n *SymbolLiteral) node()      {}
func (n *SymbolLiteral) expr()      {}

// CharLiteral represents a character literal ($a).
type CharLiteral struct {
	SpanVal Span
	Value   rune
}

func (n *CharLiteral) Span() Span { return n.SpanVal }
func (n *CharLiteral) node()      {}
func (n *CharLiteral) expr()      {}

// ArrayLiteral represents a literal array #(1 2 3).
type ArrayLiteral struct {
	SpanVal  Span
	Elements []Expr
}

func (n *ArrayLiteral) Span() Span { return n.SpanVal }
func (n *ArrayLiteral) node()      {}
func (n *ArrayLiteral) expr()      {}

// DynamicArray represents a dynamic array {1. 2. 3}.
type DynamicArray struct {
	SpanVal  Span
	Elements []Expr
}

func (n *DynamicArray) Span() Span { return n.SpanVal }
func (n *DynamicArray) node()      {}
func (n *DynamicArray) expr()      {}

// Variable represents a variable reference.
type Variable struct {
	SpanVal Span
	Name    string
}

func (n *Variable) Span() Span { return n.SpanVal }
func (n *Variable) node()      {}
func (n *Variable) expr()      {}

// Assignment represents a variable assignment (x := expr).
type Assignment struct {
	SpanVal  Span
	Variable string
	Value    Expr
}

func (n *Assignment) Span() Span { return n.SpanVal }
func (n *Assignment) node()      {}
func (n *Assignment) expr()      {}

// UnaryMessage represents a unary message send (recv selector).
type UnaryMessage struct {
	SpanVal  Span
	Receiver Expr
	Selector string
}

func (n *UnaryMessage) Span() Span { return n.SpanVal }
func (n *UnaryMessage) node()      {}
func (n *UnaryMessage) expr()      {}

// BinaryMessage represents a binary message send (recv + arg).
type BinaryMessage struct {
	SpanVal  Span
	Receiver Expr
	Selector string
	Argument Expr
}

func (n *BinaryMessage) Span() Span { return n.SpanVal }
func (n *BinaryMessage) node()      {}
func (n *BinaryMessage) expr()      {}

// KeywordMessage represents a keyword message send (recv key1: arg1 key2: arg2).
type KeywordMessage struct {
	SpanVal   Span
	Receiver  Expr
	Selector  string   // full selector: "key1:key2:"
	Keywords  []string // individual keywords: ["key1:", "key2:"]
	Arguments []Expr
}

func (n *KeywordMessage) Span() Span { return n.SpanVal }
func (n *KeywordMessage) node()      {}
func (n *KeywordMessage) expr()      {}

// Cascade represents a cascade (recv msg1; msg2; msg3).
type Cascade struct {
	SpanVal  Span
	Receiver Expr
	Messages []CascadedMessage
}

func (n *Cascade) Span() Span { return n.SpanVal }
func (n *Cascade) node()      {}
func (n *Cascade) expr()      {}

// CascadedMessage represents a message in a cascade.
type CascadedMessage struct {
	Type      MessageType // Unary, Binary, or Keyword
	Selector  string
	Keywords  []string // for keyword messages
	Arguments []Expr   // for binary and keyword messages
}

// MessageType indicates the type of message.
type MessageType int

const (
	UnaryMsg MessageType = iota
	BinaryMsg
	KeywordMsg
)

// Block represents a block closure [:a :b | stmts].
type Block struct {
	SpanVal    Span
	Parameters []string
	Temps      []string
	Statements []Stmt
}

func (n *Block) Span() Span { return n.SpanVal }
func (n *Block) node()      {}
func (n *Block) expr()      {}

// Self represents the 'self' pseudo-variable.
type Self struct {
	SpanVal Span
}

func (n *Self) Span() Span { return n.SpanVal }
func (n *Self) node()      {}
func (n *Self) expr()      {}

// Super represents the 'super' pseudo-variable.
type Super struct {
	SpanVal Span
}

func (n *Super) Span() Span { return n.SpanVal }
func (n *Super) node()      {}
func (n *Super) expr()      {}

// ThisContext represents the 'thisContext' pseudo-variable.
type ThisContext struct {
	SpanVal Span
}

func (n *ThisContext) Span() Span { return n.SpanVal }
func (n *ThisContext) node()      {}
func (n *ThisContext) expr()      {}

// NilLiteral represents the 'nil' literal.
type NilLiteral struct {
	SpanVal Span
}

func (n *NilLiteral) Span() Span { return n.SpanVal }
func (n *NilLiteral) node()      {}
func (n *NilLiteral) expr()      {}

// TrueLiteral represents the 'true' literal.
type TrueLiteral struct {
	SpanVal Span
}

func (n *TrueLiteral) Span() Span { return n.SpanVal }
func (n *TrueLiteral) node()      {}
func (n *TrueLiteral) expr()      {}

// FalseLiteral represents the 'false' literal.
type FalseLiteral struct {
	SpanVal Span
}

func (n *FalseLiteral) Span() Span { return n.SpanVal }
func (n *FalseLiteral) node()      {}
func (n *FalseLiteral) expr()      {}

// ---------------------------------------------------------------------------
// Statement nodes
// ---------------------------------------------------------------------------

// Stmt is the interface for statement nodes.
type Stmt interface {
	Node
	stmt() // marker method
}

// ExprStmt is an expression used as a statement.
type ExprStmt struct {
	SpanVal Span
	Expr    Expr
}

func (n *ExprStmt) Span() Span { return n.SpanVal }
func (n *ExprStmt) node()      {}
func (n *ExprStmt) stmt()      {}

// Return represents a return statement (^expr).
type Return struct {
	SpanVal Span
	Value   Expr
}

func (n *Return) Span() Span { return n.SpanVal }
func (n *Return) node()      {}
func (n *Return) stmt()      {}

// ---------------------------------------------------------------------------
// Method and class definition nodes
// ---------------------------------------------------------------------------

// MethodDef represents a method definition.
type MethodDef struct {
	SpanVal    Span
	Selector   string   // full selector
	Parameters []string // argument names
	Temps      []string // temporary variables
	Statements []Stmt
	Primitive  int // primitive number, 0 if not primitive
}

func (n *MethodDef) Span() Span { return n.SpanVal }
func (n *MethodDef) node()      {}

// ClassDef represents a class definition.
type ClassDef struct {
	SpanVal           Span
	Name              string
	Superclass        string
	InstanceVariables []string
	ClassVariables    []string
	PoolDictionaries  []string
	Category          string
	Traits            []string // Names of included traits
	Methods           []*MethodDef
	ClassMethods      []*MethodDef
}

func (n *ClassDef) Span() Span { return n.SpanVal }
func (n *ClassDef) node()      {}

// TraitDef represents a trait definition.
type TraitDef struct {
	SpanVal  Span
	Name     string
	Methods  []*MethodDef
	Requires []string // Required method selectors
}

func (n *TraitDef) Span() Span { return n.SpanVal }
func (n *TraitDef) node()      {}

// NamespaceDecl represents a namespace declaration.
type NamespaceDecl struct {
	SpanVal Span
	Name    string
}

func (n *NamespaceDecl) Span() Span { return n.SpanVal }
func (n *NamespaceDecl) node()      {}

// ImportDecl represents an import declaration.
type ImportDecl struct {
	SpanVal Span
	Path    string // e.g., "MyApp.Counter" or "MyApp"
}

func (n *ImportDecl) Span() Span { return n.SpanVal }
func (n *ImportDecl) node()      {}

// ---------------------------------------------------------------------------
// Top-level structure
// ---------------------------------------------------------------------------

// SourceFile represents a complete source file.
type SourceFile struct {
	SpanVal    Span
	Namespace  *NamespaceDecl
	Imports    []*ImportDecl
	Classes    []*ClassDef
	Traits     []*TraitDef
	Methods    []*MethodDef // extension methods
	Statements []Stmt       // top-level statements (for scripts/REPL)
}

func (n *SourceFile) Span() Span { return n.SpanVal }
func (n *SourceFile) node()      {}

// ---------------------------------------------------------------------------
// Helper functions
// ---------------------------------------------------------------------------

// MakeSpan creates a span from start and end positions.
func MakeSpan(start, end Position) Span {
	return Span{Start: start, End: end}
}

// ZeroSpan returns an empty span.
func ZeroSpan() Span {
	return Span{}
}
