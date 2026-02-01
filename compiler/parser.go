package compiler

import (
	"fmt"
	"strconv"
	"strings"
)

// ---------------------------------------------------------------------------
// Parser: Recursive descent parser for Smalltalk syntax
// ---------------------------------------------------------------------------

// Parser parses Smalltalk source code into an AST.
type Parser struct {
	lexer     *Lexer
	curToken  Token
	peekToken Token
	errors    []string
	input     string // original source text (for source preservation)
}

// NewParser creates a new parser for the given input.
func NewParser(input string) *Parser {
	p := &Parser{
		lexer: NewLexer(input),
		input: input,
	}
	// Read two tokens to fill curToken and peekToken
	p.nextToken()
	p.nextToken()
	return p
}

// nextToken advances to the next token.
func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.lexer.NextToken()
}

// curTokenIs checks if the current token is of the given type.
func (p *Parser) curTokenIs(t TokenType) bool {
	return p.curToken.Type == t
}

// peekTokenIs checks if the peek token is of the given type.
func (p *Parser) peekTokenIs(t TokenType) bool {
	return p.peekToken.Type == t
}

// expect advances if the current token matches, otherwise records an error.
func (p *Parser) expect(t TokenType) bool {
	if p.curTokenIs(t) {
		p.nextToken()
		return true
	}
	p.errorf("expected %s, got %s", t, p.curToken.Type)
	return false
}

// errorf records a parse error.
func (p *Parser) errorf(format string, args ...interface{}) {
	msg := fmt.Sprintf("line %d: %s", p.curToken.Pos.Line, fmt.Sprintf(format, args...))
	p.errors = append(p.errors, msg)
}

// Errors returns accumulated parse errors.
func (p *Parser) Errors() []string {
	return p.errors
}

// ---------------------------------------------------------------------------
// Top-level parsing
// ---------------------------------------------------------------------------

// ParseExpression parses a single expression.
func (p *Parser) ParseExpression() Expr {
	return p.parseKeywordSend()
}

// ParseStatement parses a single statement.
func (p *Parser) ParseStatement() Stmt {
	// Check for return
	if p.curTokenIs(TokenCaret) {
		return p.parseReturn()
	}

	// Parse expression
	expr := p.parseKeywordSend()
	if expr == nil {
		return nil
	}

	// Check for assignment
	if assign, ok := expr.(*Assignment); ok {
		return &ExprStmt{SpanVal: assign.SpanVal, Expr: assign}
	}

	return &ExprStmt{SpanVal: expr.Span(), Expr: expr}
}

// ParseStatements parses multiple statements separated by periods.
func (p *Parser) ParseStatements() []Stmt {
	var stmts []Stmt

	for !p.curTokenIs(TokenEOF) && !p.curTokenIs(TokenRBracket) && !p.curTokenIs(TokenRBrace) {
		stmt := p.ParseStatement()
		if stmt != nil {
			stmts = append(stmts, stmt)
		}

		// Consume period if present
		if p.curTokenIs(TokenPeriod) {
			p.nextToken()
		} else {
			break
		}
	}

	return stmts
}

// ParseMethod parses a method definition.
func (p *Parser) ParseMethod() *MethodDef {
	startPos := p.curToken.Pos

	// Parse method signature
	selector, params := p.parseMethodSignature()
	if selector == "" {
		return nil
	}

	// Parse temporaries (| temp1 temp2 |)
	var temps []string
	if p.curTokenIs(TokenBar) {
		temps = p.parseTemporaries()
	}

	// Parse statements
	stmts := p.ParseStatements()

	return &MethodDef{
		SpanVal:    MakeSpan(startPos, p.curToken.Pos),
		Selector:   selector,
		Parameters: params,
		Temps:      temps,
		Statements: stmts,
	}
}

// parseMethodSignature parses a method signature.
func (p *Parser) parseMethodSignature() (string, []string) {
	switch {
	case p.curTokenIs(TokenIdentifier):
		// Unary method
		selector := p.curToken.Literal
		p.nextToken()
		return selector, nil

	case p.curTokenIs(TokenBinarySelector):
		// Binary method
		selector := p.curToken.Literal
		p.nextToken()
		if !p.curTokenIs(TokenIdentifier) {
			p.errorf("expected parameter name after binary selector")
			return "", nil
		}
		param := p.curToken.Literal
		p.nextToken()
		return selector, []string{param}

	case p.curTokenIs(TokenKeyword):
		// Keyword method
		var selector strings.Builder
		var params []string
		for p.curTokenIs(TokenKeyword) {
			selector.WriteString(p.curToken.Literal)
			p.nextToken()
			if !p.curTokenIs(TokenIdentifier) {
				p.errorf("expected parameter name after keyword")
				return "", nil
			}
			params = append(params, p.curToken.Literal)
			p.nextToken()
		}
		return selector.String(), params

	default:
		p.errorf("expected method signature")
		return "", nil
	}
}

// parseTemporaries parses | temp1 temp2 |
func (p *Parser) parseTemporaries() []string {
	p.nextToken() // consume |
	var temps []string
	for p.curTokenIs(TokenIdentifier) {
		temps = append(temps, p.curToken.Literal)
		p.nextToken()
	}
	if !p.expect(TokenBar) {
		return nil
	}
	return temps
}

// parseReturn parses ^expr
func (p *Parser) parseReturn() *Return {
	startPos := p.curToken.Pos
	p.nextToken() // consume ^

	value := p.parseKeywordSend()
	if value == nil {
		return nil
	}

	return &Return{
		SpanVal: MakeSpan(startPos, value.Span().End),
		Value:   value,
	}
}

// ---------------------------------------------------------------------------
// Expression parsing (message precedence)
// ---------------------------------------------------------------------------

// parseKeywordSend parses keyword message sends (lowest precedence).
func (p *Parser) parseKeywordSend() Expr {
	receiver := p.parseBinarySendNoCascade()
	if receiver == nil {
		return nil
	}

	var result Expr

	// Check for keyword message
	if p.curTokenIs(TokenKeyword) {
		result = p.parseKeywordMessage(receiver)
	} else {
		result = receiver
	}

	// Check for cascade AFTER the full keyword/binary message
	if p.curTokenIs(TokenSemicolon) {
		return p.parseCascade(result)
	}

	return result
}

// parseKeywordMessage parses a keyword message with given receiver.
func (p *Parser) parseKeywordMessage(receiver Expr) Expr {
	startPos := receiver.Span().Start

	var selector strings.Builder
	var keywords []string
	var args []Expr

	for p.curTokenIs(TokenKeyword) {
		keyword := p.curToken.Literal
		keywords = append(keywords, keyword)
		selector.WriteString(keyword)
		p.nextToken()

		// Parse argument (binary level, not keyword to avoid ambiguity)
		// Use NoCascade version to prevent semicolons from being consumed as cascades
		arg := p.parseBinarySendNoCascade()
		if arg == nil {
			return nil
		}
		args = append(args, arg)
	}

	return &KeywordMessage{
		SpanVal:   MakeSpan(startPos, p.curToken.Pos),
		Receiver:  receiver,
		Selector:  selector.String(),
		Keywords:  keywords,
		Arguments: args,
	}
}

// parseBinarySend parses binary message sends (middle precedence).
// This version includes cascade handling for standalone expressions.
func (p *Parser) parseBinarySend() Expr {
	left := p.parseBinarySendNoCascade()
	if left == nil {
		return nil
	}

	// Check for cascades
	if p.curTokenIs(TokenSemicolon) {
		return p.parseCascade(left)
	}

	return left
}

// parseBinarySendNoCascade parses binary message sends without cascade handling.
// Used when parsing keyword message arguments where cascades shouldn't be triggered.
func (p *Parser) parseBinarySendNoCascade() Expr {
	left := p.parseUnarySend()
	if left == nil {
		return nil
	}

	// Parse binary messages (left associative)
	// Note: TokenBar (|) is also a binary selector in expression context
	for p.curTokenIs(TokenBinarySelector) || p.curTokenIs(TokenBar) {
		selector := p.curToken.Literal
		p.nextToken()

		right := p.parseUnarySend()
		if right == nil {
			return nil
		}

		left = &BinaryMessage{
			SpanVal:  MakeSpan(left.Span().Start, right.Span().End),
			Receiver: left,
			Selector: selector,
			Argument: right,
		}
	}

	return left
}

// parseCascade parses cascaded messages.
func (p *Parser) parseCascade(first Expr) Expr {
	// first is already the first message send result
	// We need to extract the receiver from first

	var receiver Expr
	var messages []CascadedMessage

	// Convert first to a cascaded message
	switch msg := first.(type) {
	case *UnaryMessage:
		receiver = msg.Receiver
		messages = append(messages, CascadedMessage{
			Type:     UnaryMsg,
			Selector: msg.Selector,
		})
	case *BinaryMessage:
		receiver = msg.Receiver
		messages = append(messages, CascadedMessage{
			Type:      BinaryMsg,
			Selector:  msg.Selector,
			Arguments: []Expr{msg.Argument},
		})
	case *KeywordMessage:
		receiver = msg.Receiver
		messages = append(messages, CascadedMessage{
			Type:      KeywordMsg,
			Selector:  msg.Selector,
			Keywords:  msg.Keywords,
			Arguments: msg.Arguments,
		})
	default:
		// Not a message, can't cascade
		p.errorf("cascade requires a message send")
		return first
	}

	// Parse remaining cascaded messages
	for p.curTokenIs(TokenSemicolon) {
		p.nextToken() // consume ;

		msg := p.parseCascadedMessage()
		if msg != nil {
			messages = append(messages, *msg)
		}
	}

	return &Cascade{
		SpanVal:  MakeSpan(first.Span().Start, p.curToken.Pos),
		Receiver: receiver,
		Messages: messages,
	}
}

// parseCascadedMessage parses a single cascaded message (without receiver).
func (p *Parser) parseCascadedMessage() *CascadedMessage {
	switch {
	case p.curTokenIs(TokenIdentifier):
		// Unary message
		selector := p.curToken.Literal
		p.nextToken()
		return &CascadedMessage{
			Type:     UnaryMsg,
			Selector: selector,
		}

	case p.curTokenIs(TokenBinarySelector):
		// Binary message
		selector := p.curToken.Literal
		p.nextToken()
		arg := p.parseUnarySend()
		if arg == nil {
			return nil
		}
		return &CascadedMessage{
			Type:      BinaryMsg,
			Selector:  selector,
			Arguments: []Expr{arg},
		}

	case p.curTokenIs(TokenKeyword):
		// Keyword message
		var selector strings.Builder
		var keywords []string
		var args []Expr
		for p.curTokenIs(TokenKeyword) {
			keyword := p.curToken.Literal
			keywords = append(keywords, keyword)
			selector.WriteString(keyword)
			p.nextToken()
			// Use NoCascade version to prevent semicolons from being consumed as cascades
			arg := p.parseBinarySendNoCascade()
			if arg == nil {
				return nil
			}
			args = append(args, arg)
		}
		return &CascadedMessage{
			Type:      KeywordMsg,
			Selector:  selector.String(),
			Keywords:  keywords,
			Arguments: args,
		}

	default:
		p.errorf("expected message in cascade")
		return nil
	}
}

// parseUnarySend parses unary message sends (highest precedence).
func (p *Parser) parseUnarySend() Expr {
	primary := p.parsePrimary()
	if primary == nil {
		return nil
	}

	// Parse chain of unary messages
	for p.curTokenIs(TokenIdentifier) && !p.peekTokenIs(TokenAssign) && !p.peekTokenIs(TokenColon) {
		selector := p.curToken.Literal
		p.nextToken()

		primary = &UnaryMessage{
			SpanVal:  MakeSpan(primary.Span().Start, p.curToken.Pos),
			Receiver: primary,
			Selector: selector,
		}
	}

	return primary
}

// parsePrimary parses primary expressions.
func (p *Parser) parsePrimary() Expr {
	switch p.curToken.Type {
	case TokenInteger:
		return p.parseInteger()
	case TokenFloat:
		return p.parseFloat()
	case TokenString:
		return p.parseString()
	case TokenSymbol:
		return p.parseSymbol()
	case TokenCharacter:
		return p.parseCharacter()
	case TokenHash:
		return p.parseHashLiteral()
	case TokenHashLParen:
		return p.parseLiteralArray()
	case TokenLParen:
		return p.parseParenExpr()
	case TokenLBracket:
		return p.parseBlock()
	case TokenLBrace:
		return p.parseDynamicArray()
	case TokenIdentifier:
		return p.parseIdentifier()
	case TokenSelf:
		return p.parseSelf()
	case TokenSuper:
		return p.parseSuper()
	case TokenThisContext:
		return p.parseThisContext()
	case TokenNil:
		return p.parseNil()
	case TokenTrue:
		return p.parseTrue()
	case TokenFalse:
		return p.parseFalse()
	default:
		p.errorf("unexpected token: %s", p.curToken.Type)
		return nil
	}
}

// ---------------------------------------------------------------------------
// Literal parsing
// ---------------------------------------------------------------------------

func (p *Parser) parseInteger() *IntLiteral {
	pos := p.curToken.Pos
	literal := p.curToken.Literal

	// Handle radix notation (16rFF)
	var value int64
	var err error
	if idx := strings.Index(literal, "r"); idx > 0 {
		radixStr := literal[:idx]
		digits := literal[idx+1:]
		radix, _ := strconv.ParseInt(radixStr, 10, 64)
		value, err = strconv.ParseInt(digits, int(radix), 64)
	} else {
		value, err = strconv.ParseInt(literal, 10, 64)
	}

	if err != nil {
		p.errorf("invalid integer: %s", literal)
		value = 0
	}

	p.nextToken()
	return &IntLiteral{
		SpanVal: MakeSpan(pos, p.curToken.Pos),
		Value:   value,
	}
}

func (p *Parser) parseFloat() *FloatLiteral {
	pos := p.curToken.Pos
	value, err := strconv.ParseFloat(p.curToken.Literal, 64)
	if err != nil {
		p.errorf("invalid float: %s", p.curToken.Literal)
		value = 0
	}
	p.nextToken()
	return &FloatLiteral{
		SpanVal: MakeSpan(pos, p.curToken.Pos),
		Value:   value,
	}
}

func (p *Parser) parseString() *StringLiteral {
	pos := p.curToken.Pos
	value := p.curToken.Literal
	p.nextToken()
	return &StringLiteral{
		SpanVal: MakeSpan(pos, p.curToken.Pos),
		Value:   value,
	}
}

func (p *Parser) parseSymbol() *SymbolLiteral {
	pos := p.curToken.Pos
	value := p.curToken.Literal
	p.nextToken()
	return &SymbolLiteral{
		SpanVal: MakeSpan(pos, p.curToken.Pos),
		Value:   value,
	}
}

func (p *Parser) parseCharacter() *CharLiteral {
	pos := p.curToken.Pos
	value := []rune(p.curToken.Literal)[0]
	p.nextToken()
	return &CharLiteral{
		SpanVal: MakeSpan(pos, p.curToken.Pos),
		Value:   value,
	}
}

func (p *Parser) parseHashLiteral() Expr {
	// We have a lone # - could be followed by ( for array or other things
	// This case shouldn't normally occur as lexer handles #( specially
	pos := p.curToken.Pos
	p.nextToken()
	if p.curTokenIs(TokenLParen) {
		return p.parseLiteralArray()
	}
	p.errorf("unexpected # token")
	return &SymbolLiteral{SpanVal: MakeSpan(pos, pos), Value: ""}
}

func (p *Parser) parseLiteralArray() *ArrayLiteral {
	pos := p.curToken.Pos
	p.nextToken() // consume #( or (

	var elements []Expr
	for !p.curTokenIs(TokenRParen) && !p.curTokenIs(TokenEOF) {
		elem := p.parseLiteralArrayElement()
		if elem != nil {
			elements = append(elements, elem)
		}
	}

	p.expect(TokenRParen)

	return &ArrayLiteral{
		SpanVal:  MakeSpan(pos, p.curToken.Pos),
		Elements: elements,
	}
}

func (p *Parser) parseLiteralArrayElement() Expr {
	switch p.curToken.Type {
	case TokenInteger:
		return p.parseInteger()
	case TokenFloat:
		return p.parseFloat()
	case TokenString:
		return p.parseString()
	case TokenSymbol:
		return p.parseSymbol()
	case TokenCharacter:
		return p.parseCharacter()
	case TokenIdentifier:
		// In literal arrays, bare identifiers are symbols
		pos := p.curToken.Pos
		value := p.curToken.Literal
		p.nextToken()
		return &SymbolLiteral{SpanVal: MakeSpan(pos, p.curToken.Pos), Value: value}
	case TokenHashLParen, TokenLParen:
		return p.parseLiteralArray()
	case TokenNil:
		return p.parseNil()
	case TokenTrue:
		return p.parseTrue()
	case TokenFalse:
		return p.parseFalse()
	default:
		p.errorf("unexpected token in literal array: %s", p.curToken.Type)
		p.nextToken()
		return nil
	}
}

func (p *Parser) parseParenExpr() Expr {
	p.nextToken() // consume (
	expr := p.parseKeywordSend()
	p.expect(TokenRParen)
	return expr
}

func (p *Parser) parseDynamicArray() *DynamicArray {
	pos := p.curToken.Pos
	p.nextToken() // consume {

	var elements []Expr
	for !p.curTokenIs(TokenRBrace) && !p.curTokenIs(TokenEOF) {
		elem := p.parseKeywordSend()
		if elem != nil {
			elements = append(elements, elem)
		}
		if p.curTokenIs(TokenPeriod) {
			p.nextToken()
		} else if !p.curTokenIs(TokenRBrace) {
			break
		}
	}

	p.expect(TokenRBrace)

	return &DynamicArray{
		SpanVal:  MakeSpan(pos, p.curToken.Pos),
		Elements: elements,
	}
}

func (p *Parser) parseBlock() *Block {
	pos := p.curToken.Pos
	p.nextToken() // consume [

	// Parse block parameters :x :y |
	var params []string
	for p.curTokenIs(TokenColon) {
		p.nextToken() // consume :
		if !p.curTokenIs(TokenIdentifier) {
			p.errorf("expected parameter name after :")
			break
		}
		params = append(params, p.curToken.Literal)
		p.nextToken()
	}

	// Consume | after parameters
	if len(params) > 0 {
		if !p.expect(TokenBar) {
			return nil
		}
	}

	// Parse temporaries | temp1 temp2 |
	var temps []string
	if p.curTokenIs(TokenBar) {
		temps = p.parseTemporaries()
	}

	// Parse statements
	stmts := p.ParseStatements()

	p.expect(TokenRBracket)

	return &Block{
		SpanVal:    MakeSpan(pos, p.curToken.Pos),
		Parameters: params,
		Temps:      temps,
		Statements: stmts,
	}
}

func (p *Parser) parseIdentifier() Expr {
	pos := p.curToken.Pos
	name := p.curToken.Literal
	p.nextToken()

	// Check for assignment
	if p.curTokenIs(TokenAssign) {
		p.nextToken() // consume :=
		value := p.parseKeywordSend()
		if value == nil {
			return nil
		}
		return &Assignment{
			SpanVal:  MakeSpan(pos, value.Span().End),
			Variable: name,
			Value:    value,
		}
	}

	return &Variable{
		SpanVal: MakeSpan(pos, p.curToken.Pos),
		Name:    name,
	}
}

func (p *Parser) parseSelf() *Self {
	pos := p.curToken.Pos
	p.nextToken()
	return &Self{SpanVal: MakeSpan(pos, p.curToken.Pos)}
}

func (p *Parser) parseSuper() *Super {
	pos := p.curToken.Pos
	p.nextToken()
	return &Super{SpanVal: MakeSpan(pos, p.curToken.Pos)}
}

func (p *Parser) parseThisContext() *ThisContext {
	pos := p.curToken.Pos
	p.nextToken()
	return &ThisContext{SpanVal: MakeSpan(pos, p.curToken.Pos)}
}

func (p *Parser) parseNil() *NilLiteral {
	pos := p.curToken.Pos
	p.nextToken()
	return &NilLiteral{SpanVal: MakeSpan(pos, p.curToken.Pos)}
}

func (p *Parser) parseTrue() *TrueLiteral {
	pos := p.curToken.Pos
	p.nextToken()
	return &TrueLiteral{SpanVal: MakeSpan(pos, p.curToken.Pos)}
}

func (p *Parser) parseFalse() *FalseLiteral {
	pos := p.curToken.Pos
	p.nextToken()
	return &FalseLiteral{SpanVal: MakeSpan(pos, p.curToken.Pos)}
}

// ---------------------------------------------------------------------------
// Source file parsing (Trashtalk-style class definitions)
// ---------------------------------------------------------------------------

// ParseSourceFile parses a complete source file containing class and trait definitions.
// This is the entry point for Trashtalk-style .mag files.
//
// File format:
//
//	namespace: 'Yutani::Widgets'     (optional, one per file, before classes)
//	import: 'Yutani'                 (zero or more, before classes)
//	import: 'Yutani::Events'
//
//	MyClass subclass: Object
//	  ...
func (p *Parser) ParseSourceFile() *SourceFile {
	startPos := p.curToken.Pos
	sf := &SourceFile{}

	// Parse namespace and import declarations (must come before class/trait defs)
	for !p.curTokenIs(TokenEOF) {
		if p.curTokenIs(TokenKeyword) {
			switch p.curToken.Literal {
			case "namespace:":
				ns := p.parseNamespaceDecl()
				if ns != nil {
					sf.Namespace = ns
				}
				continue
			case "import:":
				imp := p.parseImportDecl()
				if imp != nil {
					sf.Imports = append(sf.Imports, imp)
				}
				continue
			}
		}
		// Not a namespace/import — stop preamble parsing
		break
	}

	for !p.curTokenIs(TokenEOF) {
		// Look for class or trait definitions
		if p.curTokenIs(TokenIdentifier) {
			name := p.curToken.Literal
			p.nextToken()

			switch {
			case p.curTokenIs(TokenKeyword) && p.curToken.Literal == "subclass:":
				// Class definition: Name subclass: Superclass
				classDef := p.parseClassDefBody(name, startPos)
				if classDef != nil {
					sf.Classes = append(sf.Classes, classDef)
				}

			case p.curTokenIs(TokenIdentifier) && p.curToken.Literal == "trait":
				// Trait definition: Name trait
				traitDef := p.parseTraitDefBody(name, startPos)
				if traitDef != nil {
					sf.Traits = append(sf.Traits, traitDef)
				}

			default:
				p.errorf("expected 'subclass:' or 'trait' after class/trait name %s", name)
				p.nextToken()
			}
		} else {
			// Skip unexpected tokens
			p.nextToken()
		}

		startPos = p.curToken.Pos
	}

	sf.SpanVal = MakeSpan(startPos, p.curToken.Pos)
	return sf
}

// parseNamespaceDecl parses a namespace declaration.
// Format: namespace: 'Yutani::Widgets' or namespace: Compiler
func (p *Parser) parseNamespaceDecl() *NamespaceDecl {
	startPos := p.curToken.Pos
	p.nextToken() // consume "namespace:"

	var name string
	if p.curTokenIs(TokenString) {
		name = p.curToken.Literal
		p.nextToken()
	} else if p.curTokenIs(TokenIdentifier) {
		// Single-segment namespace can be a bare identifier
		name = p.curToken.Literal
		p.nextToken()
	} else {
		p.errorf("expected string or identifier after 'namespace:'")
		return nil
	}

	return &NamespaceDecl{
		SpanVal: MakeSpan(startPos, p.curToken.Pos),
		Name:    name,
	}
}

// parseImportDecl parses an import declaration.
// Format: import: 'Yutani::Events' or import: Yutani
func (p *Parser) parseImportDecl() *ImportDecl {
	startPos := p.curToken.Pos
	p.nextToken() // consume "import:"

	var path string
	if p.curTokenIs(TokenString) {
		path = p.curToken.Literal
		p.nextToken()
	} else if p.curTokenIs(TokenIdentifier) {
		// Single-segment import can be a bare identifier
		path = p.curToken.Literal
		p.nextToken()
	} else {
		p.errorf("expected string or identifier after 'import:'")
		return nil
	}

	return &ImportDecl{
		SpanVal: MakeSpan(startPos, p.curToken.Pos),
		Path:    path,
	}
}

// parseClassDefBody parses the body of a class definition after the class name.
// Expects: subclass: SuperclassName followed by indented body
func (p *Parser) parseClassDefBody(className string, startPos Position) *ClassDef {
	p.nextToken() // consume "subclass:"

	// Accept identifier or nil as superclass
	var superclass string
	if p.curTokenIs(TokenIdentifier) {
		superclass = p.curToken.Literal
		p.nextToken()
	} else if p.curTokenIs(TokenNil) {
		superclass = "nil"
		p.nextToken()
	} else {
		p.errorf("expected superclass name after 'subclass:'")
		return nil
	}

	classDef := &ClassDef{
		SpanVal:    MakeSpan(startPos, p.curToken.Pos),
		Name:       className,
		Superclass: superclass,
	}

	// Parse class body elements: instanceVars, include, method, classMethod
	for !p.curTokenIs(TokenEOF) {
		// Check if we've reached another top-level definition
		if p.curTokenIs(TokenIdentifier) && (p.peekTokenIs(TokenKeyword) || p.peekTokenIs(TokenIdentifier)) {
			// Could be another class or trait definition
			// Peek ahead to see if it's "subclass:" or "trait"
			if p.peekToken.Literal == "subclass:" || p.peekToken.Literal == "trait" {
				break
			}
		}

		if p.curTokenIs(TokenKeyword) {
			keyword := p.curToken.Literal
			switch keyword {
			case "instanceVars:", "instanceVariables:":
				vars := p.parseInstanceVars()
				classDef.InstanceVariables = append(classDef.InstanceVariables, vars...)

			case "include:":
				p.nextToken() // consume "include:"
				if p.curTokenIs(TokenIdentifier) {
					classDef.Traits = append(classDef.Traits, p.curToken.Literal)
					p.nextToken()
				} else {
					p.errorf("expected trait name after 'include:'")
				}

			case "method:":
				method := p.parseMethodInBrackets(false)
				if method != nil {
					classDef.Methods = append(classDef.Methods, method)
				}

			case "classMethod:":
				method := p.parseMethodInBrackets(true)
				if method != nil {
					classDef.ClassMethods = append(classDef.ClassMethods, method)
				}

			default:
				// Unknown keyword, skip
				p.nextToken()
			}
		} else {
			// Skip non-keyword tokens (whitespace handled by lexer)
			p.nextToken()
		}
	}

	classDef.SpanVal = MakeSpan(startPos, p.curToken.Pos)
	return classDef
}

// parseTraitDefBody parses the body of a trait definition after the trait name.
// Expects: trait followed by indented body with methods
func (p *Parser) parseTraitDefBody(traitName string, startPos Position) *TraitDef {
	p.nextToken() // consume "trait"

	traitDef := &TraitDef{
		SpanVal: MakeSpan(startPos, p.curToken.Pos),
		Name:    traitName,
	}

	// Parse trait body: method definitions
	for !p.curTokenIs(TokenEOF) {
		// Check if we've reached another top-level definition
		if p.curTokenIs(TokenIdentifier) && (p.peekTokenIs(TokenKeyword) || p.peekTokenIs(TokenIdentifier)) {
			if p.peekToken.Literal == "subclass:" || p.peekToken.Literal == "trait" {
				break
			}
		}

		if p.curTokenIs(TokenKeyword) {
			keyword := p.curToken.Literal
			switch keyword {
			case "method:":
				method := p.parseMethodInBrackets(false)
				if method != nil {
					traitDef.Methods = append(traitDef.Methods, method)
				}

			case "requires:":
				// Parse required method selectors
				p.nextToken() // consume "requires:"
				for p.curTokenIs(TokenSymbol) || p.curTokenIs(TokenIdentifier) {
					traitDef.Requires = append(traitDef.Requires, p.curToken.Literal)
					p.nextToken()
				}

			default:
				p.nextToken()
			}
		} else {
			p.nextToken()
		}
	}

	traitDef.SpanVal = MakeSpan(startPos, p.curToken.Pos)
	return traitDef
}

// parseInstanceVars parses instance variable declarations.
// Format: instanceVars: name1 name2 name3
// OR:     instanceVariables: 'name1 name2 name3'
func (p *Parser) parseInstanceVars() []string {
	p.nextToken() // consume "instanceVars:" or "instanceVariables:"

	var vars []string

	// Handle string literal format: instanceVariables: 'var1 var2 var3'
	if p.curTokenIs(TokenString) {
		str := p.curToken.Literal
		p.nextToken()
		// Split the string by whitespace to get variable names
		for _, v := range strings.Fields(str) {
			if v != "" {
				vars = append(vars, v)
			}
		}
		return vars
	}

	// Handle identifier format: instanceVars: var1 var2 var3
	for p.curTokenIs(TokenIdentifier) {
		vars = append(vars, p.curToken.Literal)
		p.nextToken()
	}

	return vars
}

// parseMethodInBrackets parses a method definition with selector and body in brackets.
// Format: method: selector [body] or classMethod: selector [body]
func (p *Parser) parseMethodInBrackets(isClassMethod bool) *MethodDef {
	startPos := p.curToken.Pos
	sourceStart := p.curToken.Pos.Offset // capture start offset for source text
	p.nextToken()                         // consume "method:" or "classMethod:"

	// Parse method signature
	selector, params := p.parseMethodSignature()
	if selector == "" {
		return nil
	}

	// Expect opening bracket
	if !p.curTokenIs(TokenLBracket) {
		p.errorf("expected '[' after method signature")
		return nil
	}
	p.nextToken() // consume [

	// Parse temporaries (| temp1 temp2 |)
	var temps []string
	if p.curTokenIs(TokenBar) {
		temps = p.parseTemporaries()
	}

	// Parse statements until ]
	stmts := p.parseStatementsUntilBracket()

	// Expect closing bracket
	if !p.expect(TokenRBracket) {
		return nil
	}

	// Capture original source text for fileOut support
	sourceEnd := p.curToken.Pos.Offset
	var sourceText string
	if sourceEnd > sourceStart && sourceEnd <= len(p.input) {
		sourceText = p.input[sourceStart:sourceEnd]
	}

	return &MethodDef{
		SpanVal:    MakeSpan(startPos, p.curToken.Pos),
		Selector:   selector,
		Parameters: params,
		Temps:      temps,
		Statements: stmts,
		SourceText: sourceText,
	}
}

// parseStatementsUntilBracket parses statements until a closing bracket is found.
func (p *Parser) parseStatementsUntilBracket() []Stmt {
	var stmts []Stmt

	for !p.curTokenIs(TokenEOF) && !p.curTokenIs(TokenRBracket) {
		stmt := p.ParseStatement()
		if stmt != nil {
			stmts = append(stmts, stmt)
		}

		// Consume period if present
		if p.curTokenIs(TokenPeriod) {
			p.nextToken()
		} else if !p.curTokenIs(TokenRBracket) && !p.curTokenIs(TokenEOF) {
			// No period and not at end - could be last statement
			break
		}
	}

	return stmts
}
