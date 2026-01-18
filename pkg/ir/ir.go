// Package ir defines the Intermediate Representation for Trashtalk compilation.
// The IR sits between parsed AST and backend-specific code generation, providing:
// - Name resolution (what kind of thing is each identifier?)
// - Backend affinity marking (can this run natively? must it use Bash?)
// - Optimization opportunities (constant folding, dead code elimination)
package ir

// Program represents a compiled class
type Program struct {
	Package       string
	Name          string
	QualifiedName string
	Parent        string
	ParentPackage string
	Traits        []string
	InstanceVars  []VarDecl
	ClassVars     []VarDecl
	Methods       []Method
	SourceCode    string   // Original source code for embedding
	ClassPragmas  []string // Class-level pragmas (e.g., "primitiveClass")
}

// IsPrimitiveClass returns true if the program has the primitiveClass pragma.
func (p *Program) IsPrimitiveClass() bool {
	for _, pragma := range p.ClassPragmas {
		if pragma == "primitiveClass" {
			return true
		}
	}
	return false
}

// VarDecl represents a variable declaration with resolved type
type VarDecl struct {
	Name       string
	Type       Type
	Default    Value
	IsIVar     bool // Instance variable
	IsClassVar bool // Class variable
	IsLocal    bool // Local variable
	IsParam    bool // Method parameter
}

// Value represents a default value for a variable
type Value struct {
	Type    string      // "number", "string", "bool", "json", "nil"
	Raw     string      // Original string representation
	Parsed  interface{} // Parsed value (int64, string, bool, etc.)
}

// Type represents resolved type information
type Type int

const (
	TypeUnknown Type = iota
	TypeInt
	TypeString
	TypeBool
	TypeJSON     // JSON object or array
	TypeBlock    // Closure/block
	TypeInstance // Trashtalk object instance
	TypeClass    // Class reference
	TypeAny      // Dynamic/untyped
)

func (t Type) String() string {
	switch t {
	case TypeInt:
		return "int"
	case TypeString:
		return "string"
	case TypeBool:
		return "bool"
	case TypeJSON:
		return "json"
	case TypeBlock:
		return "block"
	case TypeInstance:
		return "instance"
	case TypeClass:
		return "class"
	case TypeAny:
		return "any"
	default:
		return "unknown"
	}
}

// Method represents a compiled method
type Method struct {
	Selector       string
	Kind           MethodKind // Instance or Class
	Args           []VarDecl
	Locals         []VarDecl
	Body           []Statement
	Backend        Backend // Preferred backend
	CanCompile     bool    // Can be compiled to Go?
	FallbackReason string  // Why it needs Bash fallback
	IsRaw          bool    // Raw method (no transformation)
	IsPrimitive    bool    // Primitive method (has native Procyon implementation)
	RawBody        string  // For raw methods: the unprocessed Bash code
}

// MethodKind distinguishes instance and class methods
type MethodKind int

const (
	InstanceMethod MethodKind = iota
	ClassMethod
)

func (k MethodKind) String() string {
	if k == ClassMethod {
		return "class"
	}
	return "instance"
}

// Backend indicates which backend a construct requires
type Backend int

const (
	BackendAny  Backend = iota // Can run on either
	BackendGo                  // Prefers/requires Go
	BackendBash                // Requires Bash
)

func (b Backend) String() string {
	switch b {
	case BackendGo:
		return "go"
	case BackendBash:
		return "bash"
	default:
		return "any"
	}
}

// Statement represents an IR statement
type Statement interface {
	irStmt()
}

// Expression represents an IR expression
type Expression interface {
	irExpr()
	ResultType() Type
}

// === Statements ===

// AssignStmt represents variable assignment
type AssignStmt struct {
	Target string
	Value  Expression
	Kind   AssignKind // Local, IVar, or ClassVar
}

func (AssignStmt) irStmt() {}

// AssignKind distinguishes assignment targets
type AssignKind int

const (
	AssignLocal AssignKind = iota
	AssignIVar
	AssignClassVar
)

func (k AssignKind) String() string {
	switch k {
	case AssignIVar:
		return "ivar"
	case AssignClassVar:
		return "classvar"
	default:
		return "local"
	}
}

// ReturnStmt represents a method return
type ReturnStmt struct {
	Value Expression // nil for bare return
}

func (ReturnStmt) irStmt() {}

// ExprStmt wraps an expression as a statement
type ExprStmt struct {
	Expr Expression
}

func (ExprStmt) irStmt() {}

// IfStmt represents conditional execution
type IfStmt struct {
	Condition Expression
	ThenBlock []Statement
	ElseBlock []Statement // nil if no else
}

func (IfStmt) irStmt() {}

// WhileStmt represents a while loop
type WhileStmt struct {
	Condition Expression
	Body      []Statement
}

func (WhileStmt) irStmt() {}

// ForEachStmt represents iteration over a collection
type ForEachStmt struct {
	IterVar    string
	Collection Expression
	Body       []Statement
}

func (ForEachStmt) irStmt() {}

// BashStmt represents raw Bash code that cannot be compiled
type BashStmt struct {
	Code   string
	Reason string // Why this needs Bash
}

func (BashStmt) irStmt() {}

// === Expressions ===

// LiteralExpr represents a literal value
type LiteralExpr struct {
	Value interface{}
	Type_ Type
}

func (LiteralExpr) irExpr()            {}
func (e LiteralExpr) ResultType() Type { return e.Type_ }

// VarRefExpr represents a variable reference
type VarRefExpr struct {
	Name  string
	Kind  VarKind
	Type_ Type
}

func (VarRefExpr) irExpr()            {}
func (e VarRefExpr) ResultType() Type { return e.Type_ }

// VarKind distinguishes variable kinds
type VarKind int

const (
	VarLocal VarKind = iota
	VarParam
	VarIVar
	VarClassVar
	VarGlobal // Bash global
)

func (k VarKind) String() string {
	switch k {
	case VarParam:
		return "param"
	case VarIVar:
		return "ivar"
	case VarClassVar:
		return "classvar"
	case VarGlobal:
		return "global"
	default:
		return "local"
	}
}

// BinaryExpr represents a binary operation
type BinaryExpr struct {
	Left  Expression
	Op    string
	Right Expression
	Type_ Type
}

func (BinaryExpr) irExpr()            {}
func (e BinaryExpr) ResultType() Type { return e.Type_ }

// UnaryExpr represents a unary operation
type UnaryExpr struct {
	Op      string
	Operand Expression
	Type_   Type
}

func (UnaryExpr) irExpr()            {}
func (e UnaryExpr) ResultType() Type { return e.Type_ }

// MessageSendExpr represents a Smalltalk-style message send
type MessageSendExpr struct {
	Receiver    Expression
	Selector    string
	Args        []Expression
	IsSelfSend  bool
	IsClassSend bool   // @ ClassName method
	TargetClass string // For class sends
	Type_       Type
	Backend     Backend // Required backend for this call
}

func (MessageSendExpr) irExpr()            {}
func (e MessageSendExpr) ResultType() Type { return e.Type_ }

// BlockExpr represents a closure/block
type BlockExpr struct {
	Params []string
	Body   []Statement
	Type_  Type
}

func (BlockExpr) irExpr()            {}
func (e BlockExpr) ResultType() Type { return TypeBlock }

// SubshellExpr represents a raw bash subshell
type SubshellExpr struct {
	Code string
}

func (SubshellExpr) irExpr()            {}
func (e SubshellExpr) ResultType() Type { return TypeString }

// JSONPrimitiveExpr represents JSON operations
type JSONPrimitiveExpr struct {
	Receiver  Expression
	Operation string // "arrayPush", "objectAt", etc.
	Args      []Expression
	Type_     Type
}

func (JSONPrimitiveExpr) irExpr()            {}
func (e JSONPrimitiveExpr) ResultType() Type { return e.Type_ }

// ClassPrimitiveExpr represents primitive class method calls like:
// @ String isEmpty: str
// @ File exists: path
type ClassPrimitiveExpr struct {
	ClassName string       // "String" or "File"
	Operation string       // "stringIsEmpty", "fileExists", etc.
	Args      []Expression // Arguments
	Type_     Type
}

func (ClassPrimitiveExpr) irExpr()            {}
func (e ClassPrimitiveExpr) ResultType() Type { return e.Type_ }

// SelfExpr represents the self reference
type SelfExpr struct{}

func (SelfExpr) irExpr()            {}
func (e SelfExpr) ResultType() Type { return TypeInstance }

// ClassRefExpr represents a class reference
type ClassRefExpr struct {
	Package string // Empty for non-namespaced
	Name    string
}

func (ClassRefExpr) irExpr()            {}
func (e ClassRefExpr) ResultType() Type { return TypeClass }

func (c ClassRefExpr) FullName() string {
	if c.Package != "" {
		return c.Package + "::" + c.Name
	}
	return c.Name
}
