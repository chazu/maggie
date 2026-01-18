// Package ir provides the IR builder that converts AST to IR with name resolution.
package ir

import (
	"strconv"
	"strings"

	"github.com/chazu/procyon/pkg/ast"
	"github.com/chazu/procyon/pkg/parser"
)

// Scope tracks variable bindings for name resolution.
type Scope struct {
	parent   *Scope
	bindings map[string]VarDecl
}

// NewScope creates a new scope with the given parent.
func NewScope(parent *Scope) *Scope {
	return &Scope{
		parent:   parent,
		bindings: make(map[string]VarDecl),
	}
}

// Define adds a variable binding to the scope.
func (s *Scope) Define(name string, decl VarDecl) {
	s.bindings[name] = decl
}

// Resolve looks up a variable in this scope and parent scopes.
func (s *Scope) Resolve(name string) (VarDecl, bool) {
	if decl, ok := s.bindings[name]; ok {
		return decl, true
	}
	if s.parent != nil {
		return s.parent.Resolve(name)
	}
	return VarDecl{}, false
}

// Builder converts AST classes to IR programs.
type Builder struct {
	class    *ast.Class
	scope    *Scope
	errors   []string
	warnings []string
}

// NewBuilder creates a new builder for the given AST class.
func NewBuilder(class *ast.Class) *Builder {
	b := &Builder{
		class:    class,
		errors:   []string{},
		warnings: []string{},
	}

	// Initialize root scope with instance variables
	b.scope = NewScope(nil)
	for _, ivar := range class.InstanceVars {
		decl := VarDecl{
			Name:   ivar.Name,
			Type:   inferTypeFromDefault(ivar.Default),
			IsIVar: true,
		}
		if ivar.Default.Value != "" {
			decl.Default = parseDefaultValue(ivar.Default)
		}
		b.scope.Define(ivar.Name, decl)
	}

	// Add class instance variables to scope
	for _, cvar := range class.ClassInstanceVars {
		decl := VarDecl{
			Name:       cvar.Name,
			Type:       inferTypeFromDefault(cvar.Default),
			IsClassVar: true,
		}
		if cvar.Default.Value != "" {
			decl.Default = parseDefaultValue(cvar.Default)
		}
		b.scope.Define(cvar.Name, decl)
	}

	return b
}

// Build converts the AST class to an IR program.
// Returns the program, warnings, and errors.
func (b *Builder) Build() (*Program, []string, []string) {
	// Create program from class metadata
	program := &Program{
		Package:       b.class.Package,
		Name:          b.class.Name,
		QualifiedName: b.class.QualifiedName(),
		Parent:        b.class.Parent,
		Traits:        b.class.Traits,
		ClassPragmas:  b.class.ClassPragmas,
	}

	// Handle parent package (if parent is qualified like Pkg::Parent)
	if strings.Contains(b.class.Parent, "::") {
		parts := strings.Split(b.class.Parent, "::")
		program.ParentPackage = parts[0]
		program.Parent = parts[1]
	}

	// Convert instance variables to VarDecl slice
	for _, ivar := range b.class.InstanceVars {
		decl := VarDecl{
			Name:   ivar.Name,
			Type:   inferTypeFromDefault(ivar.Default),
			IsIVar: true,
		}
		if ivar.Default.Value != "" {
			decl.Default = parseDefaultValue(ivar.Default)
		}
		program.InstanceVars = append(program.InstanceVars, decl)
	}

	// Convert class instance variables to VarDecl slice
	for _, cvar := range b.class.ClassInstanceVars {
		decl := VarDecl{
			Name:       cvar.Name,
			Type:       inferTypeFromDefault(cvar.Default),
			IsClassVar: true,
		}
		if cvar.Default.Value != "" {
			decl.Default = parseDefaultValue(cvar.Default)
		}
		program.ClassVars = append(program.ClassVars, decl)
	}

	// Convert each method
	for i := range b.class.Methods {
		method := b.buildMethod(&b.class.Methods[i])
		program.Methods = append(program.Methods, method)
	}

	return program, b.warnings, b.errors
}

// buildMethod converts an AST method to an IR method.
func (b *Builder) buildMethod(m *ast.Method) Method {
	// Create method scope with parent = class scope
	methodScope := NewScope(b.scope)

	// Determine method kind
	kind := InstanceMethod
	if m.Kind == "class" {
		kind = ClassMethod
	}

	// Check if class has primitiveClass pragma - all methods become raw
	isPrimitiveClass := b.class.IsPrimitiveClass()

	// Create method
	method := Method{
		Selector:    m.Selector,
		Kind:        kind,
		IsRaw:       m.Raw || isPrimitiveClass,
		IsPrimitive: m.Primitive || isPrimitiveClass,
		CanCompile:  !m.Raw && !isPrimitiveClass, // Raw/primitive methods can't be compiled to Go
	}

	// If raw method (or primitiveClass method), mark it for Bash backend and capture raw body
	if m.Raw || isPrimitiveClass {
		method.Backend = BackendBash
		method.FallbackReason = "raw method requires Bash"
		// Raw methods still need their arguments captured for parameter binding
		for _, arg := range m.Args {
			decl := VarDecl{
				Name:    arg,
				Type:    TypeAny,
				IsParam: true,
			}
			method.Args = append(method.Args, decl)
		}
		// Convert tokens to raw Bash code
		method.RawBody = tokensToRawBash(m.Body.Tokens)
		return method
	}

	// Add parameters to scope
	for _, arg := range m.Args {
		decl := VarDecl{
			Name:    arg,
			Type:    TypeAny, // Parameters have unknown type initially
			IsParam: true,
		}
		methodScope.Define(arg, decl)
		method.Args = append(method.Args, decl)
	}

	// Parse the method body
	parseResult := parser.ParseMethod(m.Body.Tokens)
	if parseResult.Unsupported {
		method.CanCompile = false
		method.Backend = BackendBash
		method.FallbackReason = parseResult.Reason
		b.warnings = append(b.warnings, "method "+m.Selector+": "+parseResult.Reason)
		return method
	}

	if parseResult.Body != nil {
		// Add locals to scope
		for _, local := range parseResult.Body.LocalVars {
			decl := VarDecl{
				Name:    local,
				Type:    TypeAny, // Locals have unknown type initially
				IsLocal: true,
			}
			methodScope.Define(local, decl)
			method.Locals = append(method.Locals, decl)
		}

		// Build statements from method body
		stmts, backend, reason := b.buildStatements(parseResult.Body.Statements, methodScope)
		method.Body = stmts
		if backend == BackendBash {
			method.Backend = BackendBash
			method.CanCompile = false
			method.FallbackReason = reason
		} else {
			method.Backend = BackendAny
		}
	}

	return method
}

// buildStatements converts parser statements to IR statements.
func (b *Builder) buildStatements(stmts []parser.Statement, scope *Scope) ([]Statement, Backend, string) {
	var result []Statement
	backend := BackendAny
	var reason string

	for _, stmt := range stmts {
		irStmt, stmtBackend, stmtReason := b.buildStatement(stmt, scope)
		if irStmt != nil {
			result = append(result, irStmt)
		}
		if stmtBackend == BackendBash {
			backend = BackendBash
			if reason == "" {
				reason = stmtReason
			}
		}
	}

	return result, backend, reason
}

// buildStatement converts a parser statement to an IR statement.
func (b *Builder) buildStatement(stmt parser.Statement, scope *Scope) (Statement, Backend, string) {
	switch s := stmt.(type) {
	case *parser.Assignment:
		return b.buildAssignment(s, scope)
	case *parser.Return:
		return b.buildReturn(s, scope)
	case *parser.ExprStmt:
		return b.buildExprStmt(s, scope)
	case *parser.IfExpr:
		return b.buildIfStmt(s, scope)
	case *parser.WhileExpr:
		return b.buildWhileStmt(s, scope)
	case *parser.IterationExpr:
		return b.buildForEachStmt(s, scope)
	case *parser.DynamicIterationExpr:
		// Dynamic iteration requires Bash fallback
		return &BashStmt{
			Code:   "# dynamic iteration",
			Reason: "dynamic block invocation requires Bash",
		}, BackendBash, "dynamic block invocation requires Bash"
	case *parser.MessageSend:
		expr, exprBackend, reason := b.buildExpr(s, scope)
		return &ExprStmt{Expr: expr}, exprBackend, reason
	case *parser.LocalVarDecl:
		// LocalVarDecl is handled at method level, not as a statement
		return nil, BackendAny, ""
	default:
		return nil, BackendAny, ""
	}
}

// buildAssignment converts a parser assignment to an IR assignment.
func (b *Builder) buildAssignment(a *parser.Assignment, scope *Scope) (Statement, Backend, string) {
	value, backend, reason := b.buildExpr(a.Value, scope)

	// Determine assignment kind by resolving target
	kind := AssignLocal
	if decl, found := scope.Resolve(a.Target); found {
		if decl.IsIVar {
			kind = AssignIVar
		} else if decl.IsClassVar {
			kind = AssignClassVar
		}
	}

	return &AssignStmt{
		Target: a.Target,
		Value:  value,
		Kind:   kind,
	}, backend, reason
}

// buildReturn converts a parser return to an IR return.
func (b *Builder) buildReturn(r *parser.Return, scope *Scope) (Statement, Backend, string) {
	if r.Value == nil {
		return &ReturnStmt{Value: nil}, BackendAny, ""
	}
	value, backend, reason := b.buildExpr(r.Value, scope)
	return &ReturnStmt{Value: value}, backend, reason
}

// buildExprStmt wraps an expression as a statement.
func (b *Builder) buildExprStmt(e *parser.ExprStmt, scope *Scope) (Statement, Backend, string) {
	expr, backend, reason := b.buildExpr(e.Expr, scope)
	return &ExprStmt{Expr: expr}, backend, reason
}

// buildIfStmt converts a parser if expression to an IR if statement.
func (b *Builder) buildIfStmt(i *parser.IfExpr, scope *Scope) (Statement, Backend, string) {
	condition, condBackend, condReason := b.buildExpr(i.Condition, scope)

	thenBlock, thenBackend, thenReason := b.buildStatements(i.TrueBlock, scope)
	var elseBlock []Statement
	elseBackend := BackendAny
	elseReason := ""
	if len(i.FalseBlock) > 0 {
		elseBlock, elseBackend, elseReason = b.buildStatements(i.FalseBlock, scope)
	}

	// Determine overall backend requirement
	backend := condBackend
	reason := condReason
	if thenBackend == BackendBash {
		backend = BackendBash
		if reason == "" {
			reason = thenReason
		}
	}
	if elseBackend == BackendBash {
		backend = BackendBash
		if reason == "" {
			reason = elseReason
		}
	}

	return &IfStmt{
		Condition: condition,
		ThenBlock: thenBlock,
		ElseBlock: elseBlock,
	}, backend, reason
}

// buildWhileStmt converts a parser while expression to an IR while statement.
func (b *Builder) buildWhileStmt(w *parser.WhileExpr, scope *Scope) (Statement, Backend, string) {
	// If condition is a block (e.g., [i < len] whileTrue: [...]), extract the inner expression
	// Otherwise the block wrapper causes incorrect code generation
	var condition Expression
	var condBackend Backend
	var condReason string

	if block, ok := w.Condition.(*parser.BlockExpr); ok {
		// Extract the expression from the block's statements
		condition, condBackend, condReason = b.extractBlockExpr(block.Statements, scope)
	} else {
		condition, condBackend, condReason = b.buildExpr(w.Condition, scope)
	}

	body, bodyBackend, bodyReason := b.buildStatements(w.Body, scope)

	backend := condBackend
	reason := condReason
	if bodyBackend == BackendBash {
		backend = BackendBash
		if reason == "" {
			reason = bodyReason
		}
	}

	return &WhileStmt{
		Condition: condition,
		Body:      body,
	}, backend, reason
}

// buildForEachStmt converts a parser iteration expression to an IR foreach statement.
func (b *Builder) buildForEachStmt(i *parser.IterationExpr, scope *Scope) (Statement, Backend, string) {
	collection, collBackend, collReason := b.buildExpr(i.Collection, scope)

	// Create new scope for loop body with iteration variable
	loopScope := NewScope(scope)
	loopScope.Define(i.IterVar, VarDecl{
		Name:    i.IterVar,
		Type:    TypeAny,
		IsLocal: true,
	})

	body, bodyBackend, bodyReason := b.buildStatements(i.Body, loopScope)

	backend := collBackend
	reason := collReason
	if bodyBackend == BackendBash {
		backend = BackendBash
		if reason == "" {
			reason = bodyReason
		}
	}

	return &ForEachStmt{
		IterVar:    i.IterVar,
		Collection: collection,
		Body:       body,
	}, backend, reason
}

// buildExpr converts a parser expression to an IR expression.
func (b *Builder) buildExpr(expr parser.Expr, scope *Scope) (Expression, Backend, string) {
	switch e := expr.(type) {
	case *parser.NumberLit:
		val, _ := strconv.ParseInt(e.Value, 10, 64)
		return &LiteralExpr{
			Value: val,
			Type_: TypeInt,
		}, BackendAny, ""

	case *parser.StringLit:
		return &LiteralExpr{
			Value: e.Value,
			Type_: TypeString,
		}, BackendAny, ""

	case *parser.Identifier:
		return b.buildIdentifier(e, scope)

	case *parser.QualifiedName:
		return &ClassRefExpr{
			Package: e.Package,
			Name:    e.Name,
		}, BackendAny, ""

	case *parser.BinaryExpr:
		return b.buildBinaryExpr(e, scope)

	case *parser.ComparisonExpr:
		return b.buildComparisonExpr(e, scope)

	case *parser.MessageSend:
		return b.buildMessageSend(e, scope)

	case *parser.BlockExpr:
		return b.buildBlockExpr(e, scope)

	case *parser.JSONPrimitiveExpr:
		return b.buildJSONPrimitive(e, scope)

	case *parser.ClassPrimitiveExpr:
		return b.buildClassPrimitive(e, scope)

	case *parser.IterationExprAsValue:
		// For return statements with iteration
		return b.buildIterationAsValue(e.Iteration, scope)

	case *parser.DynamicIterationExprAsValue:
		// Dynamic iteration requires Bash
		return &SubshellExpr{Code: "# dynamic iteration"}, BackendBash, "dynamic block invocation requires Bash"

	case *parser.UnsupportedExpr:
		return &SubshellExpr{Code: "# unsupported: " + e.Reason}, BackendBash, e.Reason

	case *parser.OrExpr:
		return b.buildOrExpr(e, scope)

	case *parser.AndExpr:
		return b.buildAndExpr(e, scope)

	default:
		return &LiteralExpr{Value: nil, Type_: TypeAny}, BackendAny, ""
	}
}

// buildIdentifier resolves an identifier to a variable reference.
func (b *Builder) buildIdentifier(id *parser.Identifier, scope *Scope) (Expression, Backend, string) {
	// Handle bash variable references like $client
	if id.IsVariable {
		// Strip the $ prefix since generateVarRef will add it back
		name := id.Name
		if len(name) > 0 && name[0] == '$' {
			name = name[1:]
		}
		return &VarRefExpr{
			Name:  name,
			Kind:  VarGlobal, // Bash variable reference
			Type_: TypeAny,
		}, BackendBash, ""
	}

	// Handle special identifiers
	if id.Name == "self" {
		return &SelfExpr{}, BackendAny, ""
	}
	if id.Name == "true" {
		return &LiteralExpr{Value: true, Type_: TypeBool}, BackendAny, ""
	}
	if id.Name == "false" {
		return &LiteralExpr{Value: false, Type_: TypeBool}, BackendAny, ""
	}
	if id.Name == "nil" {
		return &LiteralExpr{Value: nil, Type_: TypeAny}, BackendAny, ""
	}

	// Check if it's a class reference (starts with uppercase)
	if len(id.Name) > 0 && id.Name[0] >= 'A' && id.Name[0] <= 'Z' {
		return &ClassRefExpr{Name: id.Name}, BackendAny, ""
	}

	// Resolve variable
	if decl, found := scope.Resolve(id.Name); found {
		kind := VarLocal
		if decl.IsParam {
			kind = VarParam
		} else if decl.IsIVar {
			kind = VarIVar
		} else if decl.IsClassVar {
			kind = VarClassVar
		}
		return &VarRefExpr{
			Name:  id.Name,
			Kind:  kind,
			Type_: decl.Type,
		}, BackendAny, ""
	}

	// Unknown variable - check if class has parent (might be inherited ivar)
	// In Trashtalk, variable access is essentially a slot lookup on self,
	// so unknown vars in a subclass are likely inherited instance variables.
	if b.class.Parent != "" && b.class.Parent != "Object" {
		return &VarRefExpr{
			Name:  id.Name,
			Kind:  VarIVar,
			Type_: TypeAny,
		}, BackendAny, ""
	}

	// Root class or no parent - treat as local variable (might be a typo/error)
	return &VarRefExpr{
		Name:  id.Name,
		Kind:  VarLocal,
		Type_: TypeAny,
	}, BackendAny, ""
}

// buildBinaryExpr converts a parser binary expression to IR.
func (b *Builder) buildBinaryExpr(e *parser.BinaryExpr, scope *Scope) (Expression, Backend, string) {
	left, leftBackend, leftReason := b.buildExpr(e.Left, scope)
	right, rightBackend, rightReason := b.buildExpr(e.Right, scope)

	backend := leftBackend
	reason := leftReason
	if rightBackend == BackendBash {
		backend = BackendBash
		if reason == "" {
			reason = rightReason
		}
	}

	// Determine result type based on operation
	resultType := TypeInt
	if e.Op == "+" {
		// String concatenation if either side is string
		if left.ResultType() == TypeString || right.ResultType() == TypeString {
			resultType = TypeString
		}
	}

	return &BinaryExpr{
		Left:  left,
		Op:    e.Op,
		Right: right,
		Type_: resultType,
	}, backend, reason
}

// buildComparisonExpr converts a parser comparison expression to IR.
func (b *Builder) buildComparisonExpr(e *parser.ComparisonExpr, scope *Scope) (Expression, Backend, string) {
	left, leftBackend, leftReason := b.buildExpr(e.Left, scope)
	right, rightBackend, rightReason := b.buildExpr(e.Right, scope)

	backend := leftBackend
	reason := leftReason
	if rightBackend == BackendBash {
		backend = BackendBash
		if reason == "" {
			reason = rightReason
		}
	}

	return &BinaryExpr{
		Left:  left,
		Op:    e.Op,
		Right: right,
		Type_: TypeBool,
	}, backend, reason
}

// buildOrExpr builds a short-circuit OR expression: expr or: [block]
// The block is only evaluated if expr is false.
func (b *Builder) buildOrExpr(e *parser.OrExpr, scope *Scope) (Expression, Backend, string) {
	left, leftBackend, leftReason := b.buildExpr(e.Left, scope)

	// Extract the expression from the block
	// The block should contain a single expression that evaluates to a boolean
	right, rightBackend, rightReason := b.extractBlockExpr(e.Right, scope)

	backend := leftBackend
	reason := leftReason
	if rightBackend == BackendBash {
		backend = BackendBash
		if reason == "" {
			reason = rightReason
		}
	}

	return &BinaryExpr{
		Left:  left,
		Op:    "||",
		Right: right,
		Type_: TypeBool,
	}, backend, reason
}

// buildAndExpr builds a short-circuit AND expression: expr and: [block]
// The block is only evaluated if expr is true.
func (b *Builder) buildAndExpr(e *parser.AndExpr, scope *Scope) (Expression, Backend, string) {
	left, leftBackend, leftReason := b.buildExpr(e.Left, scope)

	// Extract the expression from the block
	right, rightBackend, rightReason := b.extractBlockExpr(e.Right, scope)

	backend := leftBackend
	reason := leftReason
	if rightBackend == BackendBash {
		backend = BackendBash
		if reason == "" {
			reason = rightReason
		}
	}

	return &BinaryExpr{
		Left:  left,
		Op:    "&&",
		Right: right,
		Type_: TypeBool,
	}, backend, reason
}

// extractBlockExpr extracts an expression from a block's statements.
// For boolean blocks, this is typically the last (or only) expression.
func (b *Builder) extractBlockExpr(stmts []parser.Statement, scope *Scope) (Expression, Backend, string) {
	if len(stmts) == 0 {
		return &LiteralExpr{Value: false, Type_: TypeBool}, BackendAny, ""
	}

	// Get the last statement and extract its expression
	lastStmt := stmts[len(stmts)-1]

	switch s := lastStmt.(type) {
	case *parser.ExprStmt:
		return b.buildExpr(s.Expr, scope)
	case *parser.Return:
		return b.buildExpr(s.Value, scope)
	default:
		// For other statement types, we can't easily extract an expression
		// Return a placeholder - this shouldn't happen for well-formed or:/and: blocks
		return &LiteralExpr{Value: false, Type_: TypeBool}, BackendBash, "cannot extract expression from block"
	}
}

// buildMessageSend converts a parser message send to IR.
func (b *Builder) buildMessageSend(m *parser.MessageSend, scope *Scope) (Expression, Backend, string) {
	var receiver Expression
	var backend Backend = BackendAny
	var reason string

	if m.IsSelf {
		receiver = &SelfExpr{}
	} else {
		receiver, backend, reason = b.buildExpr(m.Receiver, scope)
	}

	var args []Expression
	for _, arg := range m.Args {
		argExpr, argBackend, argReason := b.buildExpr(arg, scope)
		args = append(args, argExpr)
		if argBackend == BackendBash {
			backend = BackendBash
			if reason == "" {
				reason = argReason
			}
		}
	}

	// Determine if this is a class send (receiver is a class reference)
	isClassSend := false
	targetClass := ""
	if classRef, ok := receiver.(*ClassRefExpr); ok {
		isClassSend = true
		targetClass = classRef.FullName()
	}

	return &MessageSendExpr{
		Receiver:    receiver,
		Selector:    m.Selector,
		Args:        args,
		IsSelfSend:  m.IsSelf,
		IsClassSend: isClassSend,
		TargetClass: targetClass,
		Type_:       TypeAny, // Return type unknown at this point
		Backend:     backend,
	}, backend, reason
}

// buildBlockExpr converts a parser block expression to IR.
func (b *Builder) buildBlockExpr(blk *parser.BlockExpr, scope *Scope) (Expression, Backend, string) {
	// Create block scope
	blockScope := NewScope(scope)
	for _, param := range blk.Params {
		blockScope.Define(param, VarDecl{
			Name:    param,
			Type:    TypeAny,
			IsParam: true,
		})
	}

	body, backend, reason := b.buildStatements(blk.Statements, blockScope)

	return &BlockExpr{
		Params: blk.Params,
		Body:   body,
		Type_:  TypeBlock,
	}, backend, reason
}

// buildJSONPrimitive converts a parser JSON primitive to IR.
func (b *Builder) buildJSONPrimitive(j *parser.JSONPrimitiveExpr, scope *Scope) (Expression, Backend, string) {
	receiver, backend, reason := b.buildExpr(j.Receiver, scope)

	var args []Expression
	for _, arg := range j.Args {
		argExpr, argBackend, argReason := b.buildExpr(arg, scope)
		args = append(args, argExpr)
		if argBackend == BackendBash {
			backend = BackendBash
			if reason == "" {
				reason = argReason
			}
		}
	}

	// Determine result type based on operation
	resultType := TypeAny
	switch j.Operation {
	case "arrayLength", "objectLength":
		resultType = TypeInt
	case "arrayIsEmpty", "objectIsEmpty", "objectHasKey":
		resultType = TypeBool
	case "arrayFirst", "arrayLast", "arrayAt", "objectAt":
		resultType = TypeAny
	case "arrayPush", "arrayRemoveAt", "objectRemoveKey", "arrayAtPut", "objectAtPut":
		resultType = TypeJSON
	case "objectKeys", "objectValues":
		resultType = TypeJSON
	}

	return &JSONPrimitiveExpr{
		Receiver:  receiver,
		Operation: j.Operation,
		Args:      args,
		Type_:     resultType,
	}, backend, reason
}

// buildClassPrimitive converts a parser ClassPrimitiveExpr to IR.
// These are class method calls like @ String isEmpty: str, @ File exists: path
func (b *Builder) buildClassPrimitive(c *parser.ClassPrimitiveExpr, scope *Scope) (Expression, Backend, string) {
	var args []Expression
	var backend Backend = BackendAny
	var reason string

	for _, arg := range c.Args {
		argExpr, argBackend, argReason := b.buildExpr(arg, scope)
		args = append(args, argExpr)
		if argBackend == BackendBash {
			backend = BackendBash
			if reason == "" {
				reason = argReason
			}
		}
	}

	// Determine result type based on operation
	resultType := TypeString
	switch c.Operation {
	// String predicates return bool
	case "stringIsEmpty", "stringNotEmpty", "stringContains",
		"stringStartsWith", "stringEndsWith", "stringEquals":
		resultType = TypeBool
	// String length returns int
	case "stringLength":
		resultType = TypeInt
	// File predicates return bool
	case "fileExists", "fileIsFile", "fileIsDirectory", "fileIsSymlink",
		"fileIsFifo", "fileIsSocket", "fileIsBlockDevice", "fileIsCharDevice",
		"fileIsReadable", "fileIsWritable", "fileIsExecutable",
		"fileIsEmpty", "fileNotEmpty",
		"fileIsNewer", "fileIsOlder", "fileIsSame":
		resultType = TypeBool
	}

	return &ClassPrimitiveExpr{
		ClassName: c.ClassName,
		Operation: c.Operation,
		Args:      args,
		Type_:     resultType,
	}, backend, reason
}

// buildIterationAsValue handles iteration expressions used as values (e.g., in return).
func (b *Builder) buildIterationAsValue(i *parser.IterationExpr, scope *Scope) (Expression, Backend, string) {
	// For collect: and select:, we need to return the result
	// For now, treat as a message send that returns a collection
	collection, backend, reason := b.buildExpr(i.Collection, scope)

	// Create block scope for iteration variable
	blockScope := NewScope(scope)
	blockScope.Define(i.IterVar, VarDecl{
		Name:    i.IterVar,
		Type:    TypeAny,
		IsLocal: true,
	})

	body, bodyBackend, bodyReason := b.buildStatements(i.Body, blockScope)
	if bodyBackend == BackendBash {
		backend = BackendBash
		if reason == "" {
			reason = bodyReason
		}
	}

	// Wrap as a block expression for the iteration
	block := &BlockExpr{
		Params: []string{i.IterVar},
		Body:   body,
		Type_:  TypeBlock,
	}

	// Create message send for the iteration method (collect:, select:, do:)
	selector := i.Kind + "_"
	return &MessageSendExpr{
		Receiver: collection,
		Selector: selector,
		Args:     []Expression{block},
		Type_:    TypeJSON,
		Backend:  backend,
	}, backend, reason
}

// resolve looks up a variable in the current scope chain.
func (b *Builder) resolve(name string) (VarDecl, bool) {
	return b.scope.Resolve(name)
}

// inferTypeFromDefault infers the IR Type from an AST default value.
func inferTypeFromDefault(def ast.DefaultValue) Type {
	switch def.Type {
	case "number":
		return TypeInt
	case "string":
		return TypeString
	case "bool":
		return TypeBool
	case "json", "array", "object":
		return TypeJSON
	default:
		return TypeAny
	}
}

// tokensToRawBash converts AST tokens back to raw Bash code for raw methods.
// This preserves the original Bash code without any transformation.
func tokensToRawBash(tokens []ast.Token) string {
	var result strings.Builder
	prevLine := 0

	for i, tok := range tokens {
		// Handle line breaks
		if tok.Line > prevLine && prevLine > 0 {
			result.WriteString("\n")
		}
		prevLine = tok.Line

		switch tok.Type {
		case "NEWLINE":
			result.WriteString("\n")

		case "EQUALS":
			// No space before or after equals in assignments
			result.WriteString("=")

		case "MINUS":
			// Check if this is a test flag like -n, -z, -e, -eq, -ne, etc
			result.WriteString(tok.Value)
			// Don't add space if next is identifier that looks like a test flag
			if i+1 < len(tokens) {
				next := tokens[i+1]
				// Common test flags: -n, -z, -e, -f, -d, -eq, -ne, -lt, -gt, -le, -ge
				if next.Type == "IDENTIFIER" && isBashTestFlag(next.Value) {
					// This is a test flag, don't add space
				} else if tok.Line == next.Line && next.Col == tok.Col+1 {
					// Adjacent, no space (for regex patterns like -?[0-9])
				} else if next.Type != "NEWLINE" {
					result.WriteString(" ")
				}
			}

		case "DSTRING", "SSTRING", "SUBSHELL", "VARIABLE":
			// These tokens include their delimiters - just add them
			result.WriteString(tok.Value)
			// Add trailing space unless next is newline, adjacent, or end
			if i+1 < len(tokens) {
				next := tokens[i+1]
				// Check adjacency for glob patterns like "darwin"*
				expectedNextCol := tok.Col + len(tok.Value)
				if tok.Line == next.Line && next.Col == expectedNextCol {
					// Adjacent, no space (for "darwin"*)
				} else if next.Type != "NEWLINE" {
					result.WriteString(" ")
				}
			}

		case "IDENTIFIER":
			result.WriteString(tok.Value)
			// Don't add space if next token is EQUALS (for assignment like id=...)
			if i+1 < len(tokens) {
				next := tokens[i+1]
				if next.Type != "EQUALS" && next.Type != "NEWLINE" {
					result.WriteString(" ")
				}
			}

		case "LT":
			result.WriteString(tok.Value)
			// Check if this LT is adjacent to next token (for << or <( process substitution)
			if i+1 < len(tokens) {
				next := tokens[i+1]
				// If next token is on same line and adjacent (col difference = 1), no space
				// Otherwise add a space
				if tok.Line == next.Line && next.Col == tok.Col+1 {
					// Adjacent, no space (for <<, <()
				} else if next.Type != "NEWLINE" {
					result.WriteString(" ")
				}
			}

		case "LPAREN":
			result.WriteString(tok.Value)
			// No space after opening paren

		case "RPAREN":
			result.WriteString(tok.Value)
			if i+1 < len(tokens) && tokens[i+1].Type != "NEWLINE" {
				result.WriteString(" ")
			}

		case "SEMI":
			result.WriteString(tok.Value)
			// Don't add space if next token is also SEMI (for case statement ;;)
			if i+1 < len(tokens) && tokens[i+1].Type != "NEWLINE" && tokens[i+1].Type != "SEMI" {
				result.WriteString(" ")
			}

		case "NUMBER":
			result.WriteString(tok.Value)
			// Check adjacency for patterns like [0-9], 2>/dev/null, 2>&1
			if i+1 < len(tokens) {
				next := tokens[i+1]
				expectedNextCol := tok.Col + len(tok.Value)
				if tok.Line == next.Line && next.Col == expectedNextCol {
					// Adjacent, no space (for [0-9], 2>/dev/null)
				} else if next.Type != "NEWLINE" {
					result.WriteString(" ")
				}
			}

		case "REDIRECT":
			result.WriteString(tok.Value)
			// Don't add space after redirect operators like >& (for 2>&1)

		case "GT":
			result.WriteString(tok.Value)
			// Don't add space after > for redirects (2>/dev/null)
			// Check if next token is adjacent (for 2>&1, >/path)
			if i+1 < len(tokens) {
				next := tokens[i+1]
				if tok.Line == next.Line && next.Col == tok.Col+1 {
					// Adjacent, no space
				} else if next.Type != "NEWLINE" {
					result.WriteString(" ")
				}
			}

		default:
			result.WriteString(tok.Value)
			if i+1 < len(tokens) {
				next := tokens[i+1]
				// Check adjacency for patterns like [0-9], ^...$, etc.
				expectedNextCol := tok.Col + len(tok.Value)
				if tok.Line == next.Line && next.Col == expectedNextCol {
					// Adjacent tokens, no space
				} else if next.Type != "NEWLINE" && next.Type != "RPAREN" {
					result.WriteString(" ")
				}
			}
		}
	}

	return strings.TrimSpace(result.String())
}

// isBashTestFlag returns true if the identifier is a bash test flag
func isBashTestFlag(s string) bool {
	// Single-letter test flags: -n, -z, -e, -f, -d, -r, -w, -x, -s, -L, etc.
	if len(s) == 1 {
		return true
	}
	// Multi-letter test flags: -eq, -ne, -lt, -gt, -le, -ge, -nt, -ot, -ef
	switch s {
	case "eq", "ne", "lt", "gt", "le", "ge", "nt", "ot", "ef":
		return true
	}
	return false
}

// parseDefaultValue converts an AST default value to an IR Value.
func parseDefaultValue(def ast.DefaultValue) Value {
	v := Value{
		Type: def.Type,
		Raw:  def.Value,
	}

	switch def.Type {
	case "number":
		if val, err := strconv.ParseInt(def.Value, 10, 64); err == nil {
			v.Parsed = val
		} else if val, err := strconv.ParseFloat(def.Value, 64); err == nil {
			v.Parsed = val
		}
	case "string":
		v.Parsed = def.Value
	case "bool":
		v.Parsed = def.Value == "true"
	default:
		v.Parsed = def.Value
	}

	return v
}
