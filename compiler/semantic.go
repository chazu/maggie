package compiler

import (
	"fmt"
)

// ---------------------------------------------------------------------------
// Semantic Analyzer: Pre-codegen semantic checks
// ---------------------------------------------------------------------------

// SemanticAnalyzer performs semantic analysis on the AST before code generation.
// It checks for undefined variables, unreachable code, and other semantic issues.
type SemanticAnalyzer struct {
	errors []string

	// Known globals that are always defined
	knownGlobals map[string]bool

	// Scope tracking
	args     map[string]bool // current method/block arguments
	temps    map[string]bool // current method temps
	instVars map[string]bool // instance variables

	// For blocks
	outerScopes []scopeFrame // stack of outer scopes for blocks
}

// scopeFrame represents a scope (method or block) for variable lookup
type scopeFrame struct {
	args  map[string]bool
	temps map[string]bool
}

// NewSemanticAnalyzer creates a new semantic analyzer.
func NewSemanticAnalyzer() *SemanticAnalyzer {
	return &SemanticAnalyzer{
		knownGlobals: defaultKnownGlobals(),
	}
}

// defaultKnownGlobals returns the set of always-defined global names.
func defaultKnownGlobals() map[string]bool {
	return map[string]bool{
		// Core classes
		"Object":          true,
		"Class":           true,
		"Boolean":         true,
		"True":            true,
		"False":           true,
		"UndefinedObject": true,
		"SmallInteger":    true,
		"Float":           true,
		"String":          true,
		"Symbol":          true,
		"Array":           true,
		"Block":           true,
		"Channel":         true,
		"Process":         true,
		"Result":          true,
		"Success":         true,
		"Failure":         true,
		"Dictionary":      true,
		"Association":     true,
		"ByteArray":       true,
		"Transcript":      true,
		"Compiler":        true,
		"GrpcClient":      true,
		"GrpcStream":      true,
		// Well-known globals
		"nil":   true,
		"true":  true,
		"false": true,
	}
}

// AddKnownGlobal adds a global to the known globals set.
func (s *SemanticAnalyzer) AddKnownGlobal(name string) {
	s.knownGlobals[name] = true
}

// SetInstanceVars sets the instance variable names for analysis.
func (s *SemanticAnalyzer) SetInstanceVars(names []string) {
	s.instVars = make(map[string]bool)
	for _, name := range names {
		s.instVars[name] = true
	}
}

// Errors returns accumulated analysis errors.
func (s *SemanticAnalyzer) Errors() []string {
	return s.errors
}

// errorf records an error without position.
func (s *SemanticAnalyzer) errorf(format string, args ...interface{}) {
	s.errors = append(s.errors, fmt.Sprintf(format, args...))
}

// errorAt records an error with position information.
func (s *SemanticAnalyzer) errorAt(node Node, format string, args ...interface{}) {
	pos := node.Span().Start
	msg := fmt.Sprintf("line %d, column %d: %s", pos.Line, pos.Column, fmt.Sprintf(format, args...))
	s.errors = append(s.errors, msg)
}

// warnAt records a warning with position information.
func (s *SemanticAnalyzer) warnAt(node Node, format string, args ...interface{}) {
	pos := node.Span().Start
	msg := fmt.Sprintf("warning: line %d, column %d: %s", pos.Line, pos.Column, fmt.Sprintf(format, args...))
	s.errors = append(s.errors, msg)
}

// AnalyzeMethod performs semantic analysis on a method definition.
func (s *SemanticAnalyzer) AnalyzeMethod(method *MethodDef) {
	// Set up method scope
	s.args = make(map[string]bool)
	s.temps = make(map[string]bool)
	s.outerScopes = nil

	for _, param := range method.Parameters {
		s.args[param] = true
	}
	for _, temp := range method.Temps {
		s.temps[temp] = true
	}

	// Analyze statements
	s.analyzeStatements(method.Statements)

	// Check for unreachable code after return
	s.checkUnreachableCode(method.Statements)
}

// analyzeStatements analyzes a list of statements.
func (s *SemanticAnalyzer) analyzeStatements(stmts []Stmt) {
	for _, stmt := range stmts {
		s.analyzeStmt(stmt)
	}
}

// analyzeStmt analyzes a single statement.
func (s *SemanticAnalyzer) analyzeStmt(stmt Stmt) {
	switch st := stmt.(type) {
	case *ExprStmt:
		s.analyzeExpr(st.Expr)
	case *Return:
		s.analyzeExpr(st.Value)
	}
}

// analyzeExpr analyzes an expression.
func (s *SemanticAnalyzer) analyzeExpr(expr Expr) {
	switch e := expr.(type) {
	case *Variable:
		s.checkVariableDefined(e)
	case *Assignment:
		s.analyzeExpr(e.Value)
		s.checkAssignmentTarget(e)
	case *UnaryMessage:
		s.analyzeExpr(e.Receiver)
	case *BinaryMessage:
		s.analyzeExpr(e.Receiver)
		s.analyzeExpr(e.Argument)
	case *KeywordMessage:
		s.analyzeExpr(e.Receiver)
		for _, arg := range e.Arguments {
			s.analyzeExpr(arg)
		}
	case *Cascade:
		s.analyzeExpr(e.Receiver)
		for _, msg := range e.Messages {
			// CascadedMessage is not an Expr, analyze its arguments
			for _, arg := range msg.Arguments {
				s.analyzeExpr(arg)
			}
		}
	case *Block:
		s.analyzeBlock(e)
	case *ArrayLiteral:
		for _, elem := range e.Elements {
			s.analyzeExpr(elem)
		}
	case *DynamicArray:
		for _, elem := range e.Elements {
			s.analyzeExpr(elem)
		}
	// Literals and special values don't need checking
	case *IntLiteral, *FloatLiteral, *StringLiteral, *SymbolLiteral, *CharLiteral:
		// OK
	case *Self, *Super, *NilLiteral, *TrueLiteral, *FalseLiteral, *ThisContext:
		// OK
	}
}

// checkVariableDefined checks if a variable is defined.
func (s *SemanticAnalyzer) checkVariableDefined(v *Variable) {
	name := v.Name

	// Check local scope
	if s.args[name] || s.temps[name] {
		return
	}

	// Check instance variables
	if s.instVars != nil && s.instVars[name] {
		return
	}

	// Check outer scopes (for blocks)
	for i := len(s.outerScopes) - 1; i >= 0; i-- {
		scope := s.outerScopes[i]
		if scope.args[name] || scope.temps[name] {
			return
		}
	}

	// Check known globals
	if s.knownGlobals[name] {
		return
	}

	// Unknown variable - likely a typo or undefined global
	// This is a warning, not an error, since globals can be defined at runtime
	s.warnAt(v, "variable '%s' may be undefined (assuming global)", name)
}

// checkAssignmentTarget checks if an assignment target is valid.
func (s *SemanticAnalyzer) checkAssignmentTarget(a *Assignment) {
	name := a.Variable

	// Can't assign to self, super, true, false, nil
	reserved := map[string]bool{
		"self": true, "super": true, "true": true, "false": true, "nil": true,
		"thisContext": true,
	}
	if reserved[name] {
		s.errorAt(a, "cannot assign to reserved name '%s'", name)
	}
}

// analyzeBlock analyzes a block expression.
func (s *SemanticAnalyzer) analyzeBlock(block *Block) {
	// Push current scope onto stack
	s.outerScopes = append(s.outerScopes, scopeFrame{
		args:  s.args,
		temps: s.temps,
	})

	// Create new scope for block
	oldArgs := s.args
	oldTemps := s.temps
	s.args = make(map[string]bool)
	s.temps = make(map[string]bool)

	// Block parameters become args
	for _, param := range block.Parameters {
		s.args[param] = true
	}

	// Block temps
	for _, temp := range block.Temps {
		s.temps[temp] = true
	}

	// Analyze block body
	s.analyzeStatements(block.Statements)
	s.checkUnreachableCode(block.Statements)

	// Restore outer scope
	s.args = oldArgs
	s.temps = oldTemps
	s.outerScopes = s.outerScopes[:len(s.outerScopes)-1]
}

// checkUnreachableCode checks for code after a return statement.
func (s *SemanticAnalyzer) checkUnreachableCode(stmts []Stmt) {
	for i, stmt := range stmts {
		if _, isReturn := stmt.(*Return); isReturn {
			if i < len(stmts)-1 {
				// There's code after the return
				nextStmt := stmts[i+1]
				s.warnAt(nextStmt, "unreachable code after return")
				return // Only warn once
			}
		}
	}
}

// ---------------------------------------------------------------------------
// Integration with Compile function
// ---------------------------------------------------------------------------

// Analyze runs semantic analysis on a method and returns any errors/warnings.
func Analyze(method *MethodDef, instVars []string) []string {
	analyzer := NewSemanticAnalyzer()
	if len(instVars) > 0 {
		analyzer.SetInstanceVars(instVars)
	}
	analyzer.AnalyzeMethod(method)
	return analyzer.Errors()
}
