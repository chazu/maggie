// Package parser converts token streams into expression trees.
package parser

import (
	"fmt"
	"strings"

	"github.com/chazu/procyon/pkg/ast"
	"github.com/chazu/procyon/pkg/lexer"
)

// Expr represents an expression in the parsed method body.
type Expr interface {
	exprNode()
}

// Statement represents a statement in the parsed method body.
type Statement interface {
	stmtNode()
}

// LocalVarDecl declares local variables: | x y z |
type LocalVarDecl struct {
	Names []string
}

func (LocalVarDecl) stmtNode() {}

// Assignment represents: x := expr
type Assignment struct {
	Target string
	Value  Expr
}

func (Assignment) stmtNode() {}

// Return represents: ^ expr
type Return struct {
	Value Expr
}

func (Return) stmtNode() {}

// ExprStmt wraps an expression as a statement
type ExprStmt struct {
	Expr Expr
}

func (ExprStmt) stmtNode() {}

// BinaryExpr represents: left op right
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (BinaryExpr) exprNode() {}

// Identifier represents a variable reference
type Identifier struct {
	Name       string
	IsVariable bool // true if this is a bash variable like $client
}

func (Identifier) exprNode() {}

// QualifiedName represents a namespaced class reference: Package::Class
type QualifiedName struct {
	Package string // The package/namespace name (e.g., "Yutani")
	Name    string // The class name (e.g., "Widget")
}

func (QualifiedName) exprNode() {}

// FullName returns the fully qualified name as "Package::Name"
func (q QualifiedName) FullName() string {
	return q.Package + "::" + q.Name
}

// NumberLit represents a numeric literal
type NumberLit struct {
	Value string
}

func (NumberLit) exprNode() {}

// StringLit represents a string literal
type StringLit struct {
	Value string
}

func (StringLit) exprNode() {}

// UnsupportedExpr represents an expression we can't compile
type UnsupportedExpr struct {
	Reason string
	Token  ast.Token
}

func (UnsupportedExpr) exprNode() {}

// ComparisonExpr represents: left op right (where op is >, <, >=, <=, ==, !=)
type ComparisonExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (ComparisonExpr) exprNode() {}

// IfExpr represents: (condition) ifTrue: [trueBlock] ifFalse: [falseBlock]
type IfExpr struct {
	Condition  Expr
	TrueBlock  []Statement
	FalseBlock []Statement // nil for ifTrue: only
}

func (IfExpr) exprNode() {}
func (IfExpr) stmtNode() {}

// WhileExpr represents: [condition] whileTrue: [body]
type WhileExpr struct {
	Condition Expr
	Body      []Statement
}

func (WhileExpr) exprNode() {}
func (WhileExpr) stmtNode() {}

// IfNilExpr represents: value ifNil: [nilBlock] ifNotNil: [:v | notNilBlock]
type IfNilExpr struct {
	Subject     Expr        // The value being tested for nil
	NilBlock    []Statement // Block to execute if nil (for ifNil:)
	NotNilBlock []Statement // Block to execute if not nil (for ifNotNil:)
	BindingVar  string      // Variable name for ifNotNil: binding (e.g., :v)
}

func (IfNilExpr) exprNode() {}
func (IfNilExpr) stmtNode() {}

// BlockExpr represents a block literal: [:param1 :param2 | body]
type BlockExpr struct {
	Params     []string
	Statements []Statement
}

func (BlockExpr) exprNode() {}

// IterationExpr represents: collection do: [:item | body]
// This is a special pattern that generates a Go for loop
type IterationExpr struct {
	Collection Expr        // The collection to iterate over (e.g., "items")
	IterVar    string      // The iteration variable name (e.g., "item")
	Body       []Statement // The loop body
	Kind       string      // "do", "collect", "select", etc.
}

func (IterationExpr) exprNode() {}
func (IterationExpr) stmtNode() {}

// DynamicIterationExpr represents: collection do: blockVar
// When the block is a variable/parameter, not a literal
// This requires shell-out to invoke the block at runtime
type DynamicIterationExpr struct {
	Collection Expr   // The collection to iterate over
	BlockVar   Expr   // The block variable (identifier or expression)
	Kind       string // "do", "collect", "select", etc.
}

func (DynamicIterationExpr) exprNode() {}
func (DynamicIterationExpr) stmtNode() {}

// IterationExprAsValue wraps IterationExpr for use in return statements
// For example: ^ items collect: [:x | x * 2]
type IterationExprAsValue struct {
	Iteration *IterationExpr
}

func (IterationExprAsValue) exprNode() {}

// DynamicIterationExprAsValue wraps DynamicIterationExpr for use in return statements
// For example: ^ items collect: aBlock
type DynamicIterationExprAsValue struct {
	Iteration *DynamicIterationExpr
}

func (DynamicIterationExprAsValue) exprNode() {}

// JSONPrimitiveExpr represents JSON primitive operations like:
// items arrayPush: value
// items arrayAt: index
// items arrayLength
// data objectAt: key
type JSONPrimitiveExpr struct {
	Receiver  Expr   // The receiver (e.g., "items", "data")
	Operation string // "arrayPush", "arrayAt", "objectAt", etc.
	Args      []Expr // Arguments for the operation
}

func (JSONPrimitiveExpr) exprNode() {}

// ClassPrimitiveExpr represents primitive class method calls like:
// @ String isEmpty: str
// @ File exists: path
// These are optimized to native code instead of message sends
type ClassPrimitiveExpr struct {
	ClassName string // "String" or "File"
	Operation string // "stringIsEmpty", "fileExists", etc.
	Args      []Expr // Arguments for the operation
}

func (ClassPrimitiveExpr) exprNode() {}

// MessageSend represents: @ receiver selector or @ receiver key1: arg1 key2: arg2
type MessageSend struct {
	Receiver Expr     // self, identifier, or other expression
	Selector string   // "increment", "setValue_", "at_put_"
	Args     []Expr   // arguments for keyword messages
	IsSelf   bool     // true if receiver is "self"
}

func (MessageSend) exprNode() {}
func (MessageSend) stmtNode() {}

// OrExpr represents: expr or: [block]
// The block is evaluated lazily only if expr is false
type OrExpr struct {
	Left  Expr        // The left operand (evaluated first)
	Right []Statement // The block to evaluate if Left is false
}

func (OrExpr) exprNode() {}

// AndExpr represents: expr and: [block]
// The block is evaluated lazily only if expr is true
type AndExpr struct {
	Left  Expr        // The left operand (evaluated first)
	Right []Statement // The block to evaluate if Left is true
}

func (AndExpr) exprNode() {}

// MethodBody represents a parsed method body
type MethodBody struct {
	LocalVars  []string
	Statements []Statement
}

// ParseResult contains the parsed method body and any errors
type ParseResult struct {
	Body        *MethodBody
	Unsupported bool
	Reason      string
}

// Parser converts token streams to expression trees
type Parser struct {
	tokens []ast.Token
	pos    int
}

// ParseMethod parses a method body from tokens
func ParseMethod(tokens []ast.Token) *ParseResult {
	p := &Parser{tokens: tokens, pos: 0}
	return p.parseBody()
}

func (p *Parser) parseBody() *ParseResult {
	body := &MethodBody{
		LocalVars:  []string{},
		Statements: []Statement{},
	}

	// Skip leading newlines
	p.skipNewlines()

	// Check for local variable declarations: | x y z |
	if p.peek().Type == ast.TokenPipe {
		vars, err := p.parseLocalVars()
		if err != nil {
			return &ParseResult{Unsupported: true, Reason: err.Error()}
		}
		body.LocalVars = vars
	}

	// Parse statements
	for !p.atEnd() {
		p.skipNewlines()
		if p.atEnd() {
			break
		}

		stmt, err := p.parseStatement()
		if err != nil {
			return &ParseResult{Unsupported: true, Reason: err.Error()}
		}
		if stmt != nil {
			body.Statements = append(body.Statements, stmt)
		}
	}

	return &ParseResult{Body: body}
}

func (p *Parser) parseLocalVars() ([]string, error) {
	// Consume opening |
	p.advance() // skip |

	vars := []string{}
	for !p.atEnd() && p.peek().Type != ast.TokenPipe {
		if p.peek().Type == ast.TokenIdentifier {
			vars = append(vars, p.peek().Value)
			p.advance()
		} else if p.peek().Type == ast.TokenNewline || p.peek().Type == ast.TokenDot {
			p.advance()
		} else {
			return nil, fmt.Errorf("expected identifier in local var declaration, got %s", p.peek().Type)
		}
	}

	if p.atEnd() {
		return nil, fmt.Errorf("unclosed local variable declaration")
	}
	p.advance() // skip closing |

	return vars, nil
}

func (p *Parser) parseStatement() (Statement, error) {
	p.skipNewlines()
	if p.atEnd() {
		return nil, nil
	}

	tok := p.peek()

	// Return statement: ^ expr (which may include iteration like ^ items collect: block)
	if tok.Type == ast.TokenCaret {
		p.advance()
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}

		// Check for iteration keywords (^ items collect: block) or (^ items select: block)
		if p.peek().Type == ast.TokenKeyword {
			keyword := p.peek().Value
			switch keyword {
			case "do:":
				stmt, err := p.parseDoIteration(expr)
				if err != nil {
					return nil, err
				}
				// For do:, still wrap in return (though do: doesn't produce a value)
				if iter, ok := stmt.(*IterationExpr); ok {
					return &Return{Value: &IterationExprAsValue{Iteration: iter}}, nil
				}
				if dyn, ok := stmt.(*DynamicIterationExpr); ok {
					return &Return{Value: &DynamicIterationExprAsValue{Iteration: dyn}}, nil
				}
			case "collect:":
				stmt, err := p.parseCollectIteration(expr)
				if err != nil {
					return nil, err
				}
				if iter, ok := stmt.(*IterationExpr); ok {
					return &Return{Value: &IterationExprAsValue{Iteration: iter}}, nil
				}
				if dyn, ok := stmt.(*DynamicIterationExpr); ok {
					return &Return{Value: &DynamicIterationExprAsValue{Iteration: dyn}}, nil
				}
			case "select:":
				stmt, err := p.parseSelectIteration(expr)
				if err != nil {
					return nil, err
				}
				if iter, ok := stmt.(*IterationExpr); ok {
					return &Return{Value: &IterationExprAsValue{Iteration: iter}}, nil
				}
				if dyn, ok := stmt.(*DynamicIterationExpr); ok {
					return &Return{Value: &DynamicIterationExprAsValue{Iteration: dyn}}, nil
				}
			}
		}

		return &Return{Value: expr}, nil
	}

	// Check for assignment: identifier := expr
	if tok.Type == ast.TokenIdentifier {
		// Look ahead for :=
		if p.peekAhead(1).Type == ast.TokenAssign {
			name := tok.Value
			p.advance() // skip identifier
			p.advance() // skip :=
			expr, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			return &Assignment{Target: name, Value: expr}, nil
		}
	}

	// Parse expression and check for control flow
	expr, err := p.parseExpr()
	if err != nil {
		return nil, err
	}

	// Handle chained boolean operators: expr or: [block] or: [block] and: [block]
	// In Smalltalk, or: and and: are lazy boolean operators that take blocks
	// Skip newlines before checking for boolean operators
	p.skipNewlines()
	for p.peek().Type == ast.TokenKeyword {
		keyword := p.peek().Value
		if keyword == "or:" {
			p.advance() // consume "or:"
			block, err := p.parseBlock()
			if err != nil {
				return nil, err
			}
			expr = &OrExpr{Left: expr, Right: block}
			p.skipNewlines() // Skip newlines after block for chained operators
		} else if keyword == "and:" {
			p.advance() // consume "and:"
			block, err := p.parseBlock()
			if err != nil {
				return nil, err
			}
			expr = &AndExpr{Left: expr, Right: block}
			p.skipNewlines() // Skip newlines after block for chained operators
		} else {
			break // Not a boolean operator, check for control flow
		}
	}

	// Check for control flow keywords after the expression
	// (newlines already skipped by the loop above or if loop didn't run)
	if p.peek().Type == ast.TokenKeyword {
		keyword := p.peek().Value
		switch keyword {
		case "ifTrue:":
			return p.parseIfTrue(expr)
		case "ifFalse:":
			return p.parseIfFalse(expr)
		case "whileTrue:":
			return p.parseWhileTrue(expr)
		case "ifNil:":
			return p.parseIfNil(expr)
		case "ifNotNil:":
			return p.parseIfNotNil(expr)
		case "do:":
			return p.parseDoIteration(expr)
		case "collect:":
			return p.parseCollectIteration(expr)
		case "select:":
			return p.parseSelectIteration(expr)
		}
	}

	return &ExprStmt{Expr: expr}, nil
}

// parseIfTrue parses: (condition) ifTrue: [block] [ifFalse: [block]]
func (p *Parser) parseIfTrue(condition Expr) (Statement, error) {
	p.advance() // consume "ifTrue:"

	trueBlock, err := p.parseBlock()
	if err != nil {
		return nil, err
	}

	// Check for optional ifFalse:
	var falseBlock []Statement
	p.skipNewlines()
	if p.peek().Type == ast.TokenKeyword && p.peek().Value == "ifFalse:" {
		p.advance() // consume "ifFalse:"
		falseBlock, err = p.parseBlock()
		if err != nil {
			return nil, err
		}
	}

	return &IfExpr{
		Condition:  condition,
		TrueBlock:  trueBlock,
		FalseBlock: falseBlock,
	}, nil
}

// parseIfFalse parses: (condition) ifFalse: [block]
func (p *Parser) parseIfFalse(condition Expr) (Statement, error) {
	p.advance() // consume "ifFalse:"

	falseBlock, err := p.parseBlock()
	if err != nil {
		return nil, err
	}

	// ifFalse: alone means: if NOT condition, do block
	// We represent this as IfExpr with only FalseBlock set
	return &IfExpr{
		Condition:  condition,
		TrueBlock:  nil,
		FalseBlock: falseBlock,
	}, nil
}

// parseWhileTrue parses: [condition] whileTrue: [body]
func (p *Parser) parseWhileTrue(condition Expr) (Statement, error) {
	p.advance() // consume "whileTrue:"

	body, err := p.parseBlock()
	if err != nil {
		return nil, err
	}

	return &WhileExpr{
		Condition: condition,
		Body:      body,
	}, nil
}

// parseIfNil parses: value ifNil: [block] [ifNotNil: [:v | block]]
func (p *Parser) parseIfNil(subject Expr) (Statement, error) {
	p.advance() // consume "ifNil:"

	nilBlock, err := p.parseBlock()
	if err != nil {
		return nil, err
	}

	// Check for optional ifNotNil:
	var notNilBlock []Statement
	var bindingVar string
	p.skipNewlines()
	if p.peek().Type == ast.TokenKeyword && p.peek().Value == "ifNotNil:" {
		p.advance() // consume "ifNotNil:"

		// Check if we have a block with binding [:v | ...]
		if p.peek().Type == ast.TokenLBracket {
			block, err := p.parseBlockExpr()
			if err != nil {
				return nil, err
			}
			if len(block.Params) > 0 {
				bindingVar = block.Params[0]
			}
			notNilBlock = block.Statements
		} else {
			// Simple block
			notNilBlock, err = p.parseBlock()
			if err != nil {
				return nil, err
			}
		}
	}

	return &IfNilExpr{
		Subject:     subject,
		NilBlock:    nilBlock,
		NotNilBlock: notNilBlock,
		BindingVar:  bindingVar,
	}, nil
}

// parseIfNotNil parses: value ifNotNil: [:v | block]
func (p *Parser) parseIfNotNil(subject Expr) (Statement, error) {
	p.advance() // consume "ifNotNil:"

	var notNilBlock []Statement
	var bindingVar string

	// Check if we have a block with binding [:v | ...]
	if p.peek().Type == ast.TokenLBracket {
		block, err := p.parseBlockExpr()
		if err != nil {
			return nil, err
		}
		if len(block.Params) > 0 {
			bindingVar = block.Params[0]
		}
		notNilBlock = block.Statements
	} else {
		// Simple block
		var err error
		notNilBlock, err = p.parseBlock()
		if err != nil {
			return nil, err
		}
	}

	// ifNotNil: alone means: if NOT nil, do block
	// We represent this as IfNilExpr with only NotNilBlock set
	return &IfNilExpr{
		Subject:     subject,
		NilBlock:    nil,
		NotNilBlock: notNilBlock,
		BindingVar:  bindingVar,
	}, nil
}

// parseIteration parses: collection <kind>: [:item | body] or collection <kind>: blockVar
// kind should be "do", "collect", or "select"
func (p *Parser) parseIteration(collection Expr, kind string) (Statement, error) {
	p.advance() // consume the keyword (e.g., "do:", "collect:", "select:")

	// Check if we have a block literal or a variable
	if p.peek().Type == ast.TokenLBracket {
		// Block literal - inline the iteration
		block, err := p.parseBlockExpr()
		if err != nil {
			return nil, err
		}

		if len(block.Params) != 1 {
			return nil, fmt.Errorf("%s: block must have exactly one parameter, got %d", kind, len(block.Params))
		}

		return &IterationExpr{
			Collection: collection,
			IterVar:    block.Params[0],
			Body:       block.Statements,
			Kind:       kind,
		}, nil
	}

	// Block variable - dynamic iteration (Phase 2)
	blockVar, err := p.parseMessageArg()
	if err != nil {
		return nil, err
	}

	return &DynamicIterationExpr{
		Collection: collection,
		BlockVar:   blockVar,
		Kind:       kind,
	}, nil
}

// parseDoIteration parses: collection do: [:item | body] or collection do: blockVar
func (p *Parser) parseDoIteration(collection Expr) (Statement, error) {
	return p.parseIteration(collection, "do")
}

// parseCollectIteration parses: collection collect: [:item | expr] or collection collect: blockVar
func (p *Parser) parseCollectIteration(collection Expr) (Statement, error) {
	return p.parseIteration(collection, "collect")
}

// parseSelectIteration parses: collection select: [:item | condition] or collection select: blockVar
func (p *Parser) parseSelectIteration(collection Expr) (Statement, error) {
	return p.parseIteration(collection, "select")
}

// parseBlock parses: [statements] (for control flow)
func (p *Parser) parseBlock() ([]Statement, error) {
	if p.peek().Type != ast.TokenLBracket {
		return nil, fmt.Errorf("expected [ to start block, got %s", p.peek().Type)
	}
	p.advance() // consume [

	var statements []Statement

	// Parse statements until we hit ]
	for !p.atEnd() && p.peek().Type != ast.TokenRBracket {
		p.skipNewlines()
		if p.peek().Type == ast.TokenRBracket {
			break
		}

		stmt, err := p.parseStatement()
		if err != nil {
			return nil, err
		}
		if stmt != nil {
			statements = append(statements, stmt)
		}
	}

	if p.peek().Type != ast.TokenRBracket {
		return nil, fmt.Errorf("expected ] to end block, got %s", p.peek().Type)
	}
	p.advance() // consume ]

	return statements, nil
}

// parseBlockExpr parses a block literal: [:param1 :param2 | body] or [body]
// Returns a BlockExpr with parameters and statements
func (p *Parser) parseBlockExpr() (*BlockExpr, error) {
	if p.peek().Type != ast.TokenLBracket {
		return nil, fmt.Errorf("expected [ to start block, got %s", p.peek().Type)
	}
	p.advance() // consume [

	var params []string
	var statements []Statement

	// Check for block parameters: [:param1 :param2 | ...]
	for p.peek().Type == ast.TokenBlockParam {
		params = append(params, p.peek().Value)
		p.advance()
	}

	// If we had parameters, consume the |
	if len(params) > 0 {
		if p.peek().Type != ast.TokenPipe {
			return nil, fmt.Errorf("expected | after block parameters, got %s", p.peek().Type)
		}
		p.advance() // consume |
	}

	// Parse statements until we hit ]
	for !p.atEnd() && p.peek().Type != ast.TokenRBracket {
		p.skipNewlines()
		if p.peek().Type == ast.TokenRBracket {
			break
		}

		stmt, err := p.parseStatement()
		if err != nil {
			return nil, err
		}
		if stmt != nil {
			statements = append(statements, stmt)
		}
	}

	if p.peek().Type != ast.TokenRBracket {
		return nil, fmt.Errorf("expected ] to end block, got %s", p.peek().Type)
	}
	p.advance() // consume ]

	return &BlockExpr{
		Params:     params,
		Statements: statements,
	}, nil
}

func (p *Parser) parseExpr() (Expr, error) {
	return p.parseComparison()
}

func (p *Parser) parseComparison() (Expr, error) {
	left, err := p.parseConcat()
	if err != nil {
		return nil, err
	}

	// Check for comparison operators
	for !p.atEnd() {
		tok := p.peek()
		var op string
		switch tok.Type {
		case ast.TokenGT:
			op = ">"
		case ast.TokenLT:
			op = "<"
		case ast.TokenGE:
			op = ">="
		case ast.TokenLE:
			op = "<="
		case ast.TokenEQ:
			op = "=="
		case ast.TokenNE:
			op = "!="
		case ast.TokenEquals:
			op = "==" // Single = is equality in Smalltalk
		case ast.TokenStrNe:
			op = "!=" // ~= is string not equal in Smalltalk
		default:
			return left, nil
		}

		p.advance()
		right, err := p.parseConcat()
		if err != nil {
			return nil, err
		}
		left = &ComparisonExpr{Left: left, Op: op, Right: right}
	}

	return left, nil
}

// parseConcat handles string concatenation with comma operator
func (p *Parser) parseConcat() (Expr, error) {
	left, err := p.parseAddSub()
	if err != nil {
		return nil, err
	}

	for !p.atEnd() {
		tok := p.peek()
		if tok.Type == ast.TokenComma {
			p.advance()
			right, err := p.parseAddSub()
			if err != nil {
				return nil, err
			}
			left = &BinaryExpr{Left: left, Op: ",", Right: right}
		} else {
			break
		}
	}

	return left, nil
}

func (p *Parser) parseAddSub() (Expr, error) {
	left, err := p.parseMulDiv()
	if err != nil {
		return nil, err
	}

	for !p.atEnd() {
		tok := p.peek()
		if tok.Type == ast.TokenPlus || tok.Type == ast.TokenMinus {
			op := tok.Value
			p.advance()
			right, err := p.parseMulDiv()
			if err != nil {
				return nil, err
			}
			left = &BinaryExpr{Left: left, Op: op, Right: right}
		} else {
			break
		}
	}

	return left, nil
}

func (p *Parser) parseMulDiv() (Expr, error) {
	left, err := p.parsePrimary()
	if err != nil {
		return nil, err
	}

	// Check for JSON primitives after primary expression
	left, err = p.parseJSONPrimitive(left)
	if err != nil {
		return nil, err
	}

	for !p.atEnd() {
		tok := p.peek()
		if tok.Type == ast.TokenStar || tok.Type == ast.TokenSlash {
			op := tok.Value
			p.advance()
			right, err := p.parsePrimary()
			if err != nil {
				return nil, err
			}
			// Check for JSON primitives on right operand too
			right, err = p.parseJSONPrimitive(right)
			if err != nil {
				return nil, err
			}
			left = &BinaryExpr{Left: left, Op: op, Right: right}
		} else {
			break
		}
	}

	return left, nil
}

// isJSONPrimitiveUnary checks if the identifier is a unary JSON primitive
func isJSONPrimitiveUnary(name string) bool {
	switch name {
	case "arrayLength", "arrayFirst", "arrayLast", "arrayIsEmpty",
		"objectKeys", "objectValues", "objectLength", "objectIsEmpty",
		"stringToJsonArray":
		return true
	}
	return false
}

// isUnaryMessage checks if the identifier is a known unary message
// These are common Smalltalk-style messages that take no arguments
func isUnaryMessage(name string) bool {
	// Check for known unary messages
	switch name {
	case "notEmpty", "isEmpty", "isNil", "notNil", "class", "size",
		"asString", "asNumber", "asArray", "first", "last", "hash":
		return true
	}
	return false
}

// isJSONPrimitiveKeyword checks if the keyword is a JSON primitive keyword
func isJSONPrimitiveKeyword(keyword string) (string, int, bool) {
	// Returns: operation name, number of args, is valid
	switch keyword {
	case "arrayPush:":
		return "arrayPush", 1, true
	case "arrayAt:":
		return "arrayAt", 1, true
	case "arrayRemoveAt:":
		return "arrayRemoveAt", 1, true
	case "objectAt:":
		return "objectAt", 1, true
	case "objectHasKey:":
		return "objectHasKey", 1, true
	case "objectRemoveKey:":
		return "objectRemoveKey", 1, true
	}
	return "", 0, false
}

// isJSONPrimitiveKeyword2 checks for two-arg JSON primitive keywords
func isJSONPrimitiveKeyword2(keyword string) (string, bool) {
	switch keyword {
	case "arrayAt:":
		return "arrayAtPut", true // Will be "arrayAt:put:" when we see "put:"
	case "objectAt:":
		return "objectAtPut", true // Will be "objectAt:put:" when we see "put:"
	}
	return "", false
}

// isStringPrimitive checks if a selector on String class is a known primitive.
// Returns (operation name, true) if it's a primitive.
func isStringPrimitive(selector string) (string, bool) {
	switch selector {
	case "isEmpty_":
		return "stringIsEmpty", true
	case "notEmpty_":
		return "stringNotEmpty", true
	case "contains_substring_":
		return "stringContains", true
	case "startsWith_prefix_":
		return "stringStartsWith", true
	case "endsWith_suffix_":
		return "stringEndsWith", true
	case "equals_to_":
		return "stringEquals", true
	case "trimPrefix_from_":
		return "stringTrimPrefix", true
	case "trimSuffix_from_":
		return "stringTrimSuffix", true
	case "replace_with_in_":
		return "stringReplace", true
	case "replaceAll_with_in_":
		return "stringReplaceAll", true
	case "substring_from_length_":
		return "stringSubstring", true
	case "length_":
		return "stringLength", true
	case "uppercase_":
		return "stringUppercase", true
	case "lowercase_":
		return "stringLowercase", true
	case "trim_":
		return "stringTrim", true
	case "concat_with_":
		return "stringConcat", true
	}
	return "", false
}

// isFilePrimitive checks if a selector on File class is a known primitive.
// Returns (operation name, true) if it's a primitive.
func isFilePrimitive(selector string) (string, bool) {
	switch selector {
	case "exists_":
		return "fileExists", true
	case "isFile_":
		return "fileIsFile", true
	case "isDirectory_":
		return "fileIsDirectory", true
	case "isSymlink_":
		return "fileIsSymlink", true
	case "isFifo_":
		return "fileIsFifo", true
	case "isSocket_":
		return "fileIsSocket", true
	case "isBlockDevice_":
		return "fileIsBlockDevice", true
	case "isCharDevice_":
		return "fileIsCharDevice", true
	case "isReadable_":
		return "fileIsReadable", true
	case "isWritable_":
		return "fileIsWritable", true
	case "isExecutable_":
		return "fileIsExecutable", true
	case "isEmpty_":
		return "fileIsEmpty", true
	case "notEmpty_":
		return "fileNotEmpty", true
	case "isNewer_than_":
		return "fileIsNewer", true
	case "isOlder_than_":
		return "fileIsOlder", true
	case "isSame_as_":
		return "fileIsSame", true
	}
	return "", false
}

// isClassPrimitive checks if a message send to a class is a known primitive.
// Returns (operation name, true) if it's a primitive.
func isClassPrimitive(className, selector string) (string, bool) {
	switch className {
	case "String":
		return isStringPrimitive(selector)
	case "File":
		return isFilePrimitive(selector)
	}
	return "", false
}

// parseJSONPrimitive checks if the next token is a JSON primitive and parses it.
// Also handles general unary messages like: result notEmpty
// Supports chained primitives like: items arrayPush: x arrayPush: y
func (p *Parser) parseJSONPrimitive(receiver Expr) (Expr, error) {
	result := receiver

	for {
		// Check for unary JSON primitives (identifier without colon)
		if p.peek().Type == ast.TokenIdentifier {
			name := p.peek().Value
			if isJSONPrimitiveUnary(name) {
				p.advance() // consume the primitive name
				result = &JSONPrimitiveExpr{
					Receiver:  result,
					Operation: name,
					Args:      nil,
				}
				continue // Check for more primitives
			}
			// Check for general unary messages (lowercase identifier, not a keyword)
			// Examples: notEmpty, isNil, class, etc.
			if isUnaryMessage(name) {
				p.advance() // consume the message name
				result = &MessageSend{
					Receiver: result,
					Selector: name,
					Args:     nil,
					IsSelf:   false,
				}
				continue // Check for more messages
			}
		}

		// Check for keyword JSON primitives (like "arrayPush:")
		if p.peek().Type == ast.TokenKeyword {
			keyword := p.peek().Value
			if op, argCount, ok := isJSONPrimitiveKeyword(keyword); ok {
				p.advance() // consume the keyword

				// Parse the first argument
				arg1, err := p.parseMessageArg()
				if err != nil {
					return nil, err
				}

				// Check for second keyword (like "put:" for "arrayAt:put:")
				if p.peek().Type == ast.TokenKeyword && p.peek().Value == "put:" {
					p.advance() // consume "put:"
					arg2, err := p.parseMessageArg()
					if err != nil {
						return nil, err
					}
					// Adjust operation name for two-arg variants
					if op == "arrayAt" {
						op = "arrayAtPut"
					} else if op == "objectAt" {
						op = "objectAtPut"
					}
					result = &JSONPrimitiveExpr{
						Receiver:  result,
						Operation: op,
						Args:      []Expr{arg1, arg2},
					}
					continue // Check for more primitives
				}

				args := []Expr{arg1}
				// Handle single-arg case
				if argCount == 1 {
					result = &JSONPrimitiveExpr{
						Receiver:  result,
						Operation: op,
						Args:      args,
					}
					continue // Check for more primitives
				}
			}
		}

		// No more JSON primitives, done
		break
	}

	return result, nil
}

func (p *Parser) parsePrimary() (Expr, error) {
	tok := p.peek()

	switch tok.Type {
	case ast.TokenNumber:
		p.advance()
		return &NumberLit{Value: tok.Value}, nil

	case ast.TokenIdentifier:
		p.advance()
		return &Identifier{Name: tok.Value}, nil

	case ast.TokenDString, ast.TokenSString:
		// Strip quotes from string literals - tokenizer includes them
		p.advance()
		val := tok.Value
		if len(val) >= 2 && ((val[0] == '\'' && val[len(val)-1] == '\'') || (val[0] == '"' && val[len(val)-1] == '"')) {
			val = val[1 : len(val)-1]
		}
		return &StringLit{Value: val}, nil

	case ast.TokenTripleString:
		// Strip quotes from triple-quoted strings
		p.advance()
		val := tok.Value
		if len(val) >= 6 && val[:3] == "'''" && val[len(val)-3:] == "'''" {
			val = val[3 : len(val)-3]
		}
		return &StringLit{Value: val}, nil

	case ast.TokenString:
		// STRING tokens from other sources may still have quotes
		p.advance()
		val := tok.Value
		if len(val) >= 2 && ((val[0] == '\'' && val[len(val)-1] == '\'') || (val[0] == '"' && val[len(val)-1] == '"')) {
			val = val[1 : len(val)-1]
		}
		return &StringLit{Value: val}, nil

	case ast.TokenLParen:
		// Parenthesized expression: (expr), comparison: (a = b), or message send: (a matches: b)
		p.advance() // consume (
		left, err := p.parseExpr()
		if err != nil {
			return nil, err
		}

		// Check for comparison operators inside parentheses
		if p.isComparisonOp(p.peek().Type) {
			op := p.peek()
			p.advance() // consume operator
			right, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			if p.peek().Type != ast.TokenRParen {
				return nil, fmt.Errorf("expected ) after comparison expression, got %s", p.peek().Type)
			}
			p.advance() // consume )
			return &ComparisonExpr{
				Left:  left,
				Op:    p.comparisonOpString(op),
				Right: right,
			}, nil
		}

		// Check for keyword message send inside parentheses: (receiver keyword: arg)
		// This handles patterns like (actual matches: pattern)
		if p.peek().Type == ast.TokenKeyword {
			keyword := p.peek().Value
			// Only handle non-control-flow keywords as message sends
			if !p.isControlFlowKeyword(keyword) {
				return p.parseParenthesizedMessageSend(left)
			}
		}

		if p.peek().Type != ast.TokenRParen {
			return nil, fmt.Errorf("expected ) after parenthesized expression, got %s", p.peek().Type)
		}
		p.advance() // consume )
		return left, nil

	case ast.TokenVariable:
		// $variable - bash variable reference (e.g., $client holds an instance ID)
		p.advance()
		return &Identifier{Name: tok.Value, IsVariable: true}, nil

	case ast.TokenSubshell:
		// Check if subshell contains a message send: $(@ receiver message)
		// In native code, message sends return values directly, so we can
		// strip the $() wrapper and just parse the message send.
		subshellContent := tok.Value
		if len(subshellContent) > 4 && subshellContent[:2] == "$(" && subshellContent[len(subshellContent)-1] == ')' {
			inner := strings.TrimSpace(subshellContent[2 : len(subshellContent)-1]) // strip $( and )
			// Check if it starts with @ (message send)
			if len(inner) > 0 && inner[0] == '@' {
				// It's a message send - re-lex and parse the inner content
				p.advance() // consume the subshell token
				lex := lexer.New(inner)
				tokens, err := lex.Tokenize()
				if err != nil {
					return nil, fmt.Errorf("failed to lex subshell content: %w", err)
				}
				// Convert lexer tokens to ast tokens
				astTokens := make([]ast.Token, len(tokens))
				for i, t := range tokens {
					astTokens[i] = ast.Token{Type: string(t.Type), Value: t.Value, Line: t.Line, Col: t.Column}
				}
				innerParser := &Parser{tokens: astTokens, pos: 0}
				return innerParser.parseExpr()
			}
		}
		return nil, fmt.Errorf("subshell expressions not supported (only $(@ ...) message sends allowed)")

	case ast.TokenAt:
		p.advance() // consume @
		return p.parseMessageSend()

	case ast.TokenLBracket:
		// Block literal as expression: [condition] or [:param | body]
		// Used for [condition] whileTrue: [body]
		return p.parseBlockExpr()

	case ast.TokenNewline, ast.TokenDot:
		// End of expression
		return nil, fmt.Errorf("unexpected end of expression")

	default:
		return nil, fmt.Errorf("unexpected token: %s (%s)", tok.Type, tok.Value)
	}
}

// parseMessageSend parses: receiver selector or receiver key1: arg1 key2: arg2
// Called after @ has been consumed
// Supports qualified names: @ Pkg::Class selector
// Supports variable receivers: @ $client selector
func (p *Parser) parseMessageSend() (Expr, error) {
	// Parse receiver (identifier or variable like $client)
	if p.peek().Type != ast.TokenIdentifier && p.peek().Type != ast.TokenVariable {
		return nil, fmt.Errorf("expected receiver identifier after @, got %s", p.peek().Type)
	}

	receiverName := p.peek().Value
	isVariable := p.peek().Type == ast.TokenVariable
	p.advance() // consume receiver

	isSelf := receiverName == "self"
	var receiver Expr = &Identifier{Name: receiverName, IsVariable: isVariable}

	// Check for qualified name: Pkg::Class
	if p.peek().Type == ast.TokenNamespaceSep {
		p.advance() // consume ::

		// Next must be the class name
		if p.peek().Type != ast.TokenIdentifier {
			return nil, fmt.Errorf("expected class name after ::, got %s", p.peek().Type)
		}
		className := p.peek().Value
		p.advance() // consume class name

		// Create qualified name as receiver
		receiver = &QualifiedName{
			Package: receiverName,
			Name:    className,
		}
		// Qualified names like Pkg::Class are never "self"
		isSelf = false
	}

	// Check what follows - unary or keyword message?
	if p.atEnd() || p.peek().Type == ast.TokenNewline || p.peek().Type == ast.TokenDot || p.peek().Type == ast.TokenRBracket {
		return nil, fmt.Errorf("expected selector after receiver")
	}

	// If next token is a plain identifier, it's a unary message: @ self increment
	if p.peek().Type == ast.TokenIdentifier {
		selector := p.peek().Value
		p.advance() // consume selector
		return &MessageSend{
			Receiver: receiver,
			Selector: selector,
			Args:     nil,
			IsSelf:   isSelf,
		}, nil
	}

	// If next token is a keyword, it's a keyword message: @ self setValue: 42
	if p.peek().Type == ast.TokenKeyword {
		return p.parseKeywordMessage(receiver, isSelf)
	}

	return nil, fmt.Errorf("expected selector or keyword after receiver, got %s", p.peek().Type)
}

// parseKeywordMessage parses: key1: arg1 key2: arg2 ...
// Returns a MessageSend with combined selector (e.g., "at_put_") and args
// Or returns a ClassPrimitiveExpr if receiver is String/File with a known primitive selector
func (p *Parser) parseKeywordMessage(receiver Expr, isSelf bool) (Expr, error) {
	var selectorParts []string
	var args []Expr

	for !p.atEnd() && p.peek().Type == ast.TokenKeyword {
		keyword := p.peek().Value
		p.advance() // consume keyword

		// Convert "setValue:" to "setValue_"
		// Remove trailing colon and add underscore
		if len(keyword) > 0 && keyword[len(keyword)-1] == ':' {
			keyword = keyword[:len(keyword)-1] + "_"
		}
		selectorParts = append(selectorParts, keyword)

		// Parse the argument expression
		arg, err := p.parseMessageArg()
		if err != nil {
			return nil, err
		}
		args = append(args, arg)
	}

	// Combine selector parts: ["at_", "put_"] -> "at_put_"
	selector := ""
	for _, part := range selectorParts {
		selector += part
	}

	// Check if this is a class primitive (e.g., @ String isEmpty: str)
	// The receiver must be an Identifier with a class name (String or File)
	if ident, ok := receiver.(*Identifier); ok && !isSelf {
		if op, isPrimitive := isClassPrimitive(ident.Name, selector); isPrimitive {
			return &ClassPrimitiveExpr{
				ClassName: ident.Name,
				Operation: op,
				Args:      args,
			}, nil
		}
	}

	return &MessageSend{
		Receiver: receiver,
		Selector: selector,
		Args:     args,
		IsSelf:   isSelf,
	}, nil
}

// parseMessageArg parses a single argument to a keyword message
// This is simpler than full expression parsing - just primary expressions
func (p *Parser) parseMessageArg() (Expr, error) {
	tok := p.peek()

	switch tok.Type {
	case ast.TokenNumber:
		p.advance()
		return &NumberLit{Value: tok.Value}, nil

	case ast.TokenIdentifier:
		p.advance()
		return &Identifier{Name: tok.Value}, nil

	case ast.TokenDString, ast.TokenSString:
		// Strip quotes from string literals - tokenizer includes them
		p.advance()
		val := tok.Value
		if len(val) >= 2 && ((val[0] == '\'' && val[len(val)-1] == '\'') || (val[0] == '"' && val[len(val)-1] == '"')) {
			val = val[1 : len(val)-1]
		}
		return &StringLit{Value: val}, nil

	case ast.TokenTripleString:
		// Strip quotes from triple-quoted strings
		p.advance()
		val := tok.Value
		if len(val) >= 6 && val[:3] == "'''" && val[len(val)-3:] == "'''" {
			val = val[3 : len(val)-3]
		}
		return &StringLit{Value: val}, nil

	case ast.TokenString:
		// STRING tokens from other sources may still have quotes
		p.advance()
		val := tok.Value
		if len(val) >= 2 && ((val[0] == '\'' && val[len(val)-1] == '\'') || (val[0] == '"' && val[len(val)-1] == '"')) {
			val = val[1 : len(val)-1]
		}
		return &StringLit{Value: val}, nil

	case ast.TokenLParen:
		// Parenthesized expression
		p.advance() // consume (
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		if p.peek().Type != ast.TokenRParen {
			return nil, fmt.Errorf("expected ) in message argument, got %s", p.peek().Type)
		}
		p.advance() // consume )
		return expr, nil

	case ast.TokenLBracket:
		// Block expression: [:param | body] or [body]
		return p.parseBlockExpr()

	default:
		return nil, fmt.Errorf("unexpected token in message argument: %s (%s)", tok.Type, tok.Value)
	}
}

func (p *Parser) peek() ast.Token {
	if p.pos >= len(p.tokens) {
		return ast.Token{Type: "EOF"}
	}
	return p.tokens[p.pos]
}

func (p *Parser) peekAhead(n int) ast.Token {
	pos := p.pos + n
	if pos >= len(p.tokens) {
		return ast.Token{Type: "EOF"}
	}
	return p.tokens[pos]
}

func (p *Parser) advance() ast.Token {
	if p.pos >= len(p.tokens) {
		return ast.Token{Type: "EOF"}
	}
	tok := p.tokens[p.pos]
	p.pos++
	return tok
}

func (p *Parser) atEnd() bool {
	return p.pos >= len(p.tokens)
}

func (p *Parser) skipNewlines() {
	for !p.atEnd() && (p.peek().Type == ast.TokenNewline || p.peek().Type == ast.TokenDot || p.peek().Type == ast.TokenComment) {
		p.advance()
	}
}

// isComparisonOp returns true if the token type is a comparison operator
func (p *Parser) isComparisonOp(tokenType string) bool {
	switch tokenType {
	case ast.TokenEquals, ast.TokenEQ, ast.TokenNE, ast.TokenStrNe,
		ast.TokenGT, ast.TokenLT, ast.TokenGE, ast.TokenLE:
		return true
	}
	return false
}

// comparisonOpString converts a comparison token to its string representation
func (p *Parser) comparisonOpString(tok ast.Token) string {
	switch tok.Type {
	case ast.TokenEquals:
		return "=="
	case ast.TokenEQ:
		return "=="
	case ast.TokenNE:
		return "!="
	case ast.TokenStrNe:
		return "!="
	case ast.TokenGT:
		return ">"
	case ast.TokenLT:
		return "<"
	case ast.TokenGE:
		return ">="
	case ast.TokenLE:
		return "<="
	default:
		return tok.Value
	}
}

// isControlFlowKeyword returns true if the keyword is a control flow keyword
func (p *Parser) isControlFlowKeyword(keyword string) bool {
	switch keyword {
	case "ifTrue:", "ifFalse:", "whileTrue:", "whileFalse:",
		"ifNil:", "ifNotNil:", "do:", "collect:", "select:",
		"or:", "and:":
		return true
	}
	return false
}

// parseParenthesizedMessageSend parses: (receiver keyword: arg) or (receiver key1: arg1 key2: arg2)
// Called when we've already parsed the receiver inside parens and see a non-control-flow keyword
func (p *Parser) parseParenthesizedMessageSend(receiver Expr) (Expr, error) {
	// Parse keyword message: keyword: arg [keyword: arg ...]
	var selector string
	var args []Expr

	for p.peek().Type == ast.TokenKeyword {
		keyword := p.peek().Value
		// Stop if we hit a control flow keyword
		if p.isControlFlowKeyword(keyword) {
			break
		}

		selector += strings.TrimSuffix(keyword, ":") + "_"
		p.advance() // consume keyword

		// Parse argument
		arg, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		args = append(args, arg)
	}

	if p.peek().Type != ast.TokenRParen {
		return nil, fmt.Errorf("expected ) after parenthesized message send, got %s", p.peek().Type)
	}
	p.advance() // consume )

	// Determine receiver - could be self, identifier, or other expression
	isSelf := false
	if id, ok := receiver.(*Identifier); ok && id.Name == "self" {
		isSelf = true
	}

	return &MessageSend{
		Receiver: receiver,
		Selector: selector,
		Args:     args,
		IsSelf:   isSelf,
	}, nil
}
