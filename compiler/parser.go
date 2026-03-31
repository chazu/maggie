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
	warnings  []string
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

// warnf records a parse warning.
func (p *Parser) warnf(format string, args ...interface{}) {
	msg := fmt.Sprintf("line %d: %s", p.curToken.Pos.Line, fmt.Sprintf(format, args...))
	p.warnings = append(p.warnings, msg)
}

// Warnings returns accumulated parse warnings.
func (p *Parser) Warnings() []string {
	return p.warnings
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
	selector, params, paramTypes := p.parseMethodSignature()
	if selector == "" {
		return nil
	}

	// Parse optional return type annotation: ^<Type>
	var returnType *TypeExpr
	if p.curTokenIs(TokenCaret) && p.peekTokenIs(TokenBinarySelector) && p.peekToken.Literal == "<" {
		p.nextToken() // consume ^
		returnType = p.parseTypeExpr()
	}

	// Parse optional effect annotations: ! <Effect> or ! <Effect, Effect>
	effects := p.parseEffectAnnotations()

	// Parse temporaries (| temp1 temp2 |)
	var temps []string
	var tempTypes []*TypeExpr
	if p.curTokenIs(TokenBar) {
		temps, tempTypes = p.parseTemporaries()
	}

	// Parse statements
	stmts := p.ParseStatements()

	return &MethodDef{
		SpanVal:    MakeSpan(startPos, p.curToken.Pos),
		Selector:   selector,
		Parameters: params,
		Temps:      temps,
		Statements: stmts,
		ParamTypes: paramTypes,
		TempTypes:  tempTypes,
		ReturnType: returnType,
		Effects:    effects,
	}
}

// parseMethodSignature parses a method signature.
// Returns selector, parameter names, and optional type annotations (parallel to params).
func (p *Parser) parseMethodSignature() (string, []string, []*TypeExpr) {
	switch {
	case p.curTokenIs(TokenIdentifier):
		// Unary method
		selector := p.curToken.Literal
		p.nextToken()
		return selector, nil, nil

	case p.curTokenIs(TokenBinarySelector):
		// Binary method
		selector := p.curToken.Literal
		p.nextToken()
		if !p.curTokenIs(TokenIdentifier) {
			p.errorf("expected parameter name after binary selector")
			return "", nil, nil
		}
		param := p.curToken.Literal
		p.nextToken()
		var paramTypes []*TypeExpr
		if p.curTokenIs(TokenBinarySelector) && p.curToken.Literal == "<" {
			paramTypes = append(paramTypes, p.parseTypeExpr())
		} else {
			paramTypes = append(paramTypes, nil)
		}
		return selector, []string{param}, paramTypes

	case p.curTokenIs(TokenKeyword):
		// Keyword method
		var selector strings.Builder
		var params []string
		var paramTypes []*TypeExpr
		for p.curTokenIs(TokenKeyword) {
			selector.WriteString(p.curToken.Literal)
			p.nextToken()
			if !p.curTokenIs(TokenIdentifier) {
				p.errorf("expected parameter name after keyword")
				return "", nil, nil
			}
			params = append(params, p.curToken.Literal)
			p.nextToken()
			if p.curTokenIs(TokenBinarySelector) && p.curToken.Literal == "<" {
				paramTypes = append(paramTypes, p.parseTypeExpr())
			} else {
				paramTypes = append(paramTypes, nil)
			}
		}
		return selector.String(), params, paramTypes

	default:
		p.errorf("expected method signature")
		return "", nil, nil
	}
}

// parseTypeExpr parses a type annotation <TypeName>.
// Expects the current token to be a TokenBinarySelector with literal "<".
func (p *Parser) parseTypeExpr() *TypeExpr {
	startPos := p.curToken.Pos
	p.nextToken() // consume <

	if !p.curTokenIs(TokenIdentifier) {
		p.errorf("expected type name after '<'")
		return nil
	}
	name := p.curToken.Literal
	p.nextToken() // consume type name

	// Expect > — it's a TokenBinarySelector with literal ">"
	if p.curTokenIs(TokenBinarySelector) && len(p.curToken.Literal) > 0 && p.curToken.Literal[0] == '>' {
		if p.curToken.Literal == ">" {
			p.nextToken() // consume >
		} else {
			// Handle ">=" etc by consuming the whole token
			p.nextToken()
		}
	} else {
		p.errorf("expected '>' to close type annotation")
		return nil
	}

	return &TypeExpr{SpanVal: MakeSpan(startPos, p.curToken.Pos), Name: name}
}

// parseEffectAnnotations parses optional effect annotations: ! <Effect> or ! <IO, Network>.
// The ! is a TokenBinarySelector. Inside the angle brackets, effect names are
// identifiers separated by commas (which lex as TokenBinarySelector with literal ",").
// Returns nil if no effect annotation is present.
func (p *Parser) parseEffectAnnotations() []*TypeExpr {
	// Check for standalone "!" (not "!=" or "!<" etc.)
	if !p.curTokenIs(TokenBinarySelector) || p.curToken.Literal != "!" {
		return nil
	}
	// Peek must be "<" to distinguish from other uses of "!"
	if !p.peekTokenIs(TokenBinarySelector) || p.peekToken.Literal != "<" {
		return nil
	}

	p.nextToken() // consume !
	p.nextToken() // consume <

	var effects []*TypeExpr
	for {
		if !p.curTokenIs(TokenIdentifier) {
			p.errorf("expected effect name after '<' or ','")
			return nil
		}
		startPos := p.curToken.Pos
		effects = append(effects, &TypeExpr{
			SpanVal: MakeSpan(startPos, p.curToken.Pos),
			Name:    p.curToken.Literal,
		})
		p.nextToken() // consume effect name

		// Check for comma separator
		if p.curTokenIs(TokenBinarySelector) && p.curToken.Literal == "," {
			p.nextToken() // consume ,
			continue
		}
		break
	}

	// Expect closing >
	if p.curTokenIs(TokenBinarySelector) && len(p.curToken.Literal) > 0 && p.curToken.Literal[0] == '>' {
		if p.curToken.Literal == ">" {
			p.nextToken() // consume >
		} else {
			p.nextToken() // consume >= etc.
		}
	} else {
		p.errorf("expected '>' to close effect annotation")
		return nil
	}

	return effects
}

// parseTemporaries parses | temp1 temp2 |
func (p *Parser) parseTemporaries() ([]string, []*TypeExpr) {
	p.nextToken() // consume |
	var temps []string
	var tempTypes []*TypeExpr
	for p.curTokenIs(TokenIdentifier) {
		temps = append(temps, p.curToken.Literal)
		p.nextToken()
		if p.curTokenIs(TokenBinarySelector) && p.curToken.Literal == "<" {
			tempTypes = append(tempTypes, p.parseTypeExpr())
		} else {
			tempTypes = append(tempTypes, nil)
		}
	}
	if !p.expect(TokenBar) {
		return nil, nil
	}
	return temps, tempTypes
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
	case TokenHashLBrace:
		return p.parseDictionaryLiteral()
	case TokenLParen:
		return p.parseParenExpr()
	case TokenLBracket:
		b := p.parseBlock()
		if b == nil {
			return nil
		}
		return b
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

func (p *Parser) parseDictionaryLiteral() *DictionaryLiteral {
	pos := p.curToken.Pos
	p.nextToken() // consume #{

	var keys []Expr
	var values []Expr

	for !p.curTokenIs(TokenRBrace) && !p.curTokenIs(TokenEOF) {
		// Parse key as a primary expression (not full expression,
		// so -> is not consumed as a binary message send)
		key := p.parsePrimary()
		if key == nil {
			p.errorf("expected key expression in dictionary literal")
			break
		}

		// Expect ->
		if !p.curTokenIs(TokenBinarySelector) || p.curToken.Literal != "->" {
			p.errorf("expected '->' in dictionary literal")
			break
		}
		p.nextToken() // consume ->

		// Parse value as a full expression
		value := p.ParseExpression()
		if value == nil {
			p.errorf("expected value expression in dictionary literal")
			break
		}

		keys = append(keys, key)
		values = append(values, value)

		// Consume optional period separator
		if p.curTokenIs(TokenPeriod) {
			p.nextToken()
		}
	}

	p.expect(TokenRBrace)

	return &DictionaryLiteral{
		SpanVal: MakeSpan(pos, p.curToken.Pos),
		Keys:    keys,
		Values:  values,
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
		temps, _ = p.parseTemporaries()
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

	// Parse namespace and import declarations (must come before class/trait defs).
	// Skip docstrings that may appear before or between them, preserving the
	// last one as a pending docstring for the first class/trait definition.
	var pendingDocString string
	for !p.curTokenIs(TokenEOF) {
		if p.curTokenIs(TokenDocstring) {
			pendingDocString = p.curToken.Literal
			p.nextToken()
			continue
		}
		if p.curTokenIs(TokenKeyword) {
			switch p.curToken.Literal {
			case "namespace:":
				ns := p.parseNamespaceDecl()
				if ns != nil {
					sf.Namespace = ns
				}
				pendingDocString = "" // docstring before namespace: is file-level, not class-level
				continue
			case "import:":
				imp := p.parseImportDecl()
				if imp != nil {
					sf.Imports = append(sf.Imports, imp)
				}
				pendingDocString = "" // docstring before import: is file-level, not class-level
				continue
			}
		}
		// Not a namespace/import/docstring — stop preamble parsing
		break
	}
	for !p.curTokenIs(TokenEOF) {
		// Capture docstrings for the next class/trait definition
		if p.curTokenIs(TokenDocstring) {
			pendingDocString = p.curToken.Literal
			p.nextToken()
			continue
		}

		// Look for class or trait definitions
		if p.curTokenIs(TokenIdentifier) {
			name := p.curToken.Literal
			p.nextToken()

			switch {
			case p.curTokenIs(TokenKeyword) && p.curToken.Literal == "subclass:":
				// Class definition: Name subclass: Superclass
				classDef := p.parseClassDefBody(name, startPos)
				if classDef != nil {
					classDef.DocString = pendingDocString
					sf.Classes = append(sf.Classes, classDef)
				}
				pendingDocString = ""

			case p.curTokenIs(TokenIdentifier) && p.curToken.Literal == "trait":
				// Trait definition: Name trait
				traitDef := p.parseTraitDefBody(name, startPos)
				if traitDef != nil {
					traitDef.DocString = pendingDocString
					sf.Traits = append(sf.Traits, traitDef)
				}
				pendingDocString = ""

			case p.curTokenIs(TokenIdentifier) && p.curToken.Literal == "protocol":
				// Protocol definition: Name protocol
				protocolDef := p.parseProtocolDefBody(name, startPos)
				if protocolDef != nil {
					protocolDef.DocString = pendingDocString
					sf.Protocols = append(sf.Protocols, protocolDef)
				}
				pendingDocString = ""

			default:
				p.errorf("expected 'subclass:', 'trait', or 'protocol' after class/trait name %s", name)
				p.nextToken()
				pendingDocString = ""
			}
		} else {
			// Skip unexpected tokens
			p.nextToken()
		}

		startPos = p.curToken.Pos
	}

	if pendingDocString != "" {
		p.warnf("orphan docstring: not followed by a class or trait definition")
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
	var pendingDocString string
	for !p.curTokenIs(TokenEOF) {
		// Check if we've reached another top-level definition
		if p.curTokenIs(TokenIdentifier) && (p.peekTokenIs(TokenKeyword) || p.peekTokenIs(TokenIdentifier)) {
			// Could be another class or trait definition
			// Peek ahead to see if it's "subclass:" or "trait"
			if p.peekToken.Literal == "subclass:" || p.peekToken.Literal == "trait" || p.peekToken.Literal == "protocol" {
				break
			}
		}

		// Capture docstrings for the next method definition
		if p.curTokenIs(TokenDocstring) {
			pendingDocString = p.curToken.Literal
			p.nextToken()
			continue
		}

		if p.curTokenIs(TokenKeyword) {
			keyword := p.curToken.Literal
			switch keyword {
			case "instanceVars:", "instanceVariables:":
				vars, varTypes := p.parseInstanceVars()
				classDef.InstanceVariables = append(classDef.InstanceVariables, vars...)
				classDef.InstanceVarTypes = append(classDef.InstanceVarTypes, varTypes...)
				pendingDocString = ""

			case "include:":
				p.nextToken() // consume "include:"
				if p.curTokenIs(TokenIdentifier) {
					classDef.Traits = append(classDef.Traits, p.curToken.Literal)
					p.nextToken()
				} else {
					p.errorf("expected trait name after 'include:'")
				}
				pendingDocString = ""

			case "method:":
				method := p.parseMethodInBrackets(false)
				if method != nil {
					method.DocString = pendingDocString
					classDef.Methods = append(classDef.Methods, method)
				}
				pendingDocString = ""

			case "classMethod:":
				method := p.parseMethodInBrackets(true)
				if method != nil {
					method.DocString = pendingDocString
					classDef.ClassMethods = append(classDef.ClassMethods, method)
				}
				pendingDocString = ""

			default:
				// Unknown keyword, skip
				p.nextToken()
				pendingDocString = ""
			}
		} else if p.curTokenIs(TokenIdentifier) && p.curToken.Literal == "class" && p.peekTokenIs(TokenKeyword) && p.peekToken.Literal == "method:" {
			// Handle two-word "class method:" syntax as a class method
			p.nextToken() // consume "class"
			// Now current token is "method:" — parse as class method
			method := p.parseMethodInBrackets(true)
			if method != nil {
				method.DocString = pendingDocString
				classDef.ClassMethods = append(classDef.ClassMethods, method)
			}
			pendingDocString = ""
		} else {
			// Skip non-keyword tokens (whitespace handled by lexer)
			p.nextToken()
		}
	}

	if pendingDocString != "" {
		p.warnf("orphan docstring: not followed by a method definition")
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
	var pendingDocString string
	for !p.curTokenIs(TokenEOF) {
		// Check if we've reached another top-level definition
		if p.curTokenIs(TokenIdentifier) && (p.peekTokenIs(TokenKeyword) || p.peekTokenIs(TokenIdentifier)) {
			if p.peekToken.Literal == "subclass:" || p.peekToken.Literal == "trait" || p.peekToken.Literal == "protocol" {
				break
			}
		}

		// Capture docstrings for the next method definition
		if p.curTokenIs(TokenDocstring) {
			pendingDocString = p.curToken.Literal
			p.nextToken()
			continue
		}

		if p.curTokenIs(TokenKeyword) {
			keyword := p.curToken.Literal
			switch keyword {
			case "method:":
				method := p.parseMethodInBrackets(false)
				if method != nil {
					method.DocString = pendingDocString
					traitDef.Methods = append(traitDef.Methods, method)
				}
				pendingDocString = ""

			case "requires:":
				// Parse required method selectors
				p.nextToken() // consume "requires:"
				for p.curTokenIs(TokenSymbol) || p.curTokenIs(TokenIdentifier) {
					traitDef.Requires = append(traitDef.Requires, p.curToken.Literal)
					p.nextToken()
				}
				pendingDocString = ""

			default:
				p.nextToken()
				pendingDocString = ""
			}
		} else {
			p.nextToken()
		}
	}

	if pendingDocString != "" {
		p.warnf("orphan docstring: not followed by a method definition")
	}

	traitDef.SpanVal = MakeSpan(startPos, p.curToken.Pos)
	return traitDef
}

// parseInstanceVars parses instance variable declarations.
// Format: instanceVars: name1 name2 name3
// OR:     instanceVars: name1 <Type1> name2 name3 <Type3>
// OR:     instanceVariables: 'name1 name2 name3'
func (p *Parser) parseInstanceVars() ([]string, []*TypeExpr) {
	p.nextToken() // consume "instanceVars:" or "instanceVariables:"

	var vars []string
	var varTypes []*TypeExpr

	// Handle string literal format: instanceVariables: 'var1 var2 var3'
	if p.curTokenIs(TokenString) {
		str := p.curToken.Literal
		p.nextToken()
		// Split the string by whitespace to get variable names
		for _, v := range strings.Fields(str) {
			if v != "" {
				vars = append(vars, v)
				varTypes = append(varTypes, nil)
			}
		}
		return vars, varTypes
	}

	// Handle identifier format: instanceVars: var1 var2 var3
	// Stop when we hit "class" followed by "method:" (class method syntax)
	for p.curTokenIs(TokenIdentifier) {
		if p.curToken.Literal == "class" && p.peekTokenIs(TokenKeyword) && p.peekToken.Literal == "method:" {
			break
		}
		vars = append(vars, p.curToken.Literal)
		p.nextToken()
		if p.curTokenIs(TokenBinarySelector) && p.curToken.Literal == "<" {
			varTypes = append(varTypes, p.parseTypeExpr())
		} else {
			varTypes = append(varTypes, nil)
		}
	}

	return vars, varTypes
}

// parseMethodInBrackets parses a method definition with selector and body in brackets.
// Format: method: selector [body] or classMethod: selector [body]
func (p *Parser) parseMethodInBrackets(isClassMethod bool) *MethodDef {
	startPos := p.curToken.Pos
	sourceStart := p.curToken.Pos.Offset // capture start offset for source text
	p.nextToken()                         // consume "method:" or "classMethod:"

	// Parse method signature
	selector, params, paramTypes := p.parseMethodSignature()
	if selector == "" {
		return nil
	}

	// Parse optional return type annotation: ^<Type> (before the opening bracket)
	var returnType *TypeExpr
	if p.curTokenIs(TokenCaret) && p.peekTokenIs(TokenBinarySelector) && p.peekToken.Literal == "<" {
		p.nextToken() // consume ^
		returnType = p.parseTypeExpr()
	}

	// Parse optional effect annotations: ! <Effect> or ! <Effect, Effect>
	effects := p.parseEffectAnnotations()

	// Expect opening bracket
	if !p.curTokenIs(TokenLBracket) {
		p.errorf("expected '[' after method signature")
		return nil
	}
	p.nextToken() // consume [

	// Check for <primitive> annotation (docstring-only stub)
	if p.curTokenIs(TokenBinarySelector) && p.curToken.Literal == "<" &&
		p.peekTokenIs(TokenIdentifier) && p.peekToken.Literal == "primitive" {
		p.nextToken() // consume <
		p.nextToken() // consume primitive
		if !p.curTokenIs(TokenBinarySelector) || p.curToken.Literal != ">" {
			p.errorf("expected '>' after 'primitive'")
			return nil
		}
		p.nextToken() // consume >
		if !p.expect(TokenRBracket) {
			return nil
		}
		// Capture source text
		sourceEnd := p.curToken.Pos.Offset
		var sourceText string
		if sourceEnd > sourceStart && sourceEnd <= len(p.input) {
			sourceText = p.input[sourceStart:sourceEnd]
		}
		return &MethodDef{
			SpanVal:         MakeSpan(startPos, p.curToken.Pos),
			Selector:        selector,
			Parameters:      params,
			IsPrimitiveStub: true,
			SourceText:      sourceText,
			ParamTypes:      paramTypes,
			ReturnType:      returnType,
			Effects:         effects,
		}
	}

	// Parse temporaries (| temp1 temp2 |)
	var temps []string
	var tempTypes []*TypeExpr
	if p.curTokenIs(TokenBar) {
		temps, tempTypes = p.parseTemporaries()
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
		ParamTypes: paramTypes,
		TempTypes:  tempTypes,
		ReturnType: returnType,
		Effects:    effects,
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

// ---------------------------------------------------------------------------
// Protocol parsing
// ---------------------------------------------------------------------------

// parseProtocolDefBody parses the body of a protocol definition after the protocol name.
// Expects: protocol followed by entries (message signatures and includes)
func (p *Parser) parseProtocolDefBody(protocolName string, startPos Position) *ProtocolDef {
	p.nextToken() // consume "protocol"

	protDef := &ProtocolDef{
		SpanVal: MakeSpan(startPos, p.curToken.Pos),
		Name:    protocolName,
	}

	// Parse protocol body: message signatures and includes
	for !p.curTokenIs(TokenEOF) {
		// Check if we've reached another top-level definition
		if p.curTokenIs(TokenIdentifier) && (p.peekTokenIs(TokenKeyword) || p.peekTokenIs(TokenIdentifier)) {
			if p.peekToken.Literal == "subclass:" || p.peekToken.Literal == "trait" || p.peekToken.Literal == "protocol" {
				break
			}
		}

		// Skip docstrings (attach to protocol)
		if p.curTokenIs(TokenDocstring) {
			if protDef.DocString == "" {
				protDef.DocString = p.curToken.Literal
			}
			p.nextToken()
			continue
		}

		// includes: OtherProtocol
		if p.curTokenIs(TokenKeyword) && p.curToken.Literal == "includes:" {
			p.nextToken() // consume "includes:"
			if p.curTokenIs(TokenIdentifier) {
				protDef.Includes = append(protDef.Includes, p.curToken.Literal)
				p.nextToken()
			}
			if p.curTokenIs(TokenPeriod) {
				p.nextToken()
			}
			continue
		}

		// Parse protocol entry: selector with optional param types and return type
		entry := p.parseProtocolEntry()
		if entry != nil {
			protDef.Entries = append(protDef.Entries, entry)
		} else {
			// Skip unrecognized tokens to avoid infinite loop
			p.nextToken()
		}
	}

	protDef.SpanVal = MakeSpan(startPos, p.curToken.Pos)
	return protDef
}

// parseProtocolEntry parses a single protocol entry like "at: <Integer> put: <Object> ^<Object>."
func (p *Parser) parseProtocolEntry() *ProtocolEntry {
	startPos := p.curToken.Pos
	entry := &ProtocolEntry{SpanVal: MakeSpan(startPos, p.curToken.Pos)}

	switch {
	case p.curTokenIs(TokenIdentifier):
		// Unary entry: "size ^<Integer>."
		entry.Selector = p.curToken.Literal
		p.nextToken()

	case p.curTokenIs(TokenBinarySelector) && p.curToken.Literal != "<":
		// Binary entry: "+ <Number> ^<Number>."
		entry.Selector = p.curToken.Literal
		p.nextToken()
		if p.curTokenIs(TokenBinarySelector) && p.curToken.Literal == "<" {
			entry.ParamTypes = append(entry.ParamTypes, p.parseTypeExpr())
		}

	case p.curTokenIs(TokenKeyword):
		// Keyword entry: "at: <Integer> put: <Object> ^<Object>."
		var selector strings.Builder
		for p.curTokenIs(TokenKeyword) {
			selector.WriteString(p.curToken.Literal)
			p.nextToken()
			if p.curTokenIs(TokenBinarySelector) && p.curToken.Literal == "<" {
				entry.ParamTypes = append(entry.ParamTypes, p.parseTypeExpr())
			} else {
				entry.ParamTypes = append(entry.ParamTypes, nil)
			}
		}
		entry.Selector = selector.String()

	default:
		return nil
	}

	// Parse optional return type: ^<Type>
	if p.curTokenIs(TokenCaret) {
		p.nextToken() // consume ^
		if p.curTokenIs(TokenBinarySelector) && p.curToken.Literal == "<" {
			entry.ReturnType = p.parseTypeExpr()
		}
	}

	// Parse optional effect annotations: ! <Effect>
	entry.Effects = p.parseEffectAnnotations()

	// Consume trailing period
	if p.curTokenIs(TokenPeriod) {
		p.nextToken()
	}

	entry.SpanVal = MakeSpan(startPos, p.curToken.Pos)
	return entry
}
