// Package parser implements the Trashtalk class parser.
//
// This parser consumes tokens from the lexer and produces an AST representing
// Trashtalk classes, traits, methods, and other declarations.
//
// The grammar supports:
//   - Package and import declarations
//   - Class definitions (subclass:) and traits
//   - Instance and class instance variables with optional defaults
//   - Method definitions (method:, classMethod:, rawMethod:, rawClassMethod:)
//   - Trait inclusion (include:)
//   - File dependencies and protocol requirements (requires:)
//   - Method aliases (alias: for:)
//   - Method advice (before:/after: do:)
package parser

import (
	"fmt"
	"strings"
)

// =============================================================================
// Token Types (matches tokenizer.bash output)
// =============================================================================

// TokenType represents the type of a lexer token.
type TokenType string

const (
	TokenIdentifier    TokenType = "IDENTIFIER"
	TokenKeyword       TokenType = "KEYWORD"
	TokenString        TokenType = "STRING"
	TokenDString       TokenType = "DSTRING"
	TokenTripleString  TokenType = "TRIPLESTRING"
	TokenNumber        TokenType = "NUMBER"
	TokenLBracket      TokenType = "LBRACKET"
	TokenRBracket      TokenType = "RBRACKET"
	TokenDLBracket     TokenType = "DLBRACKET"
	TokenDRBracket     TokenType = "DRBRACKET"
	TokenPipe          TokenType = "PIPE"
	TokenCaret         TokenType = "CARET"
	TokenAt            TokenType = "AT"
	TokenAssign        TokenType = "ASSIGN"
	TokenDot           TokenType = "DOT"
	TokenNewline       TokenType = "NEWLINE"
	TokenComment       TokenType = "COMMENT"
	TokenNamespaceSep  TokenType = "NAMESPACE_SEP"
	TokenSymbol        TokenType = "SYMBOL"
	TokenVariable      TokenType = "VARIABLE"
	TokenSubshell      TokenType = "SUBSHELL"
	TokenArithmetic    TokenType = "ARITHMETIC"
	TokenArithCmd      TokenType = "ARITH_CMD"
	TokenLParen        TokenType = "LPAREN"
	TokenRParen        TokenType = "RPAREN"
	TokenLBrace        TokenType = "LBRACE"
	TokenRBrace        TokenType = "RBRACE"
	TokenHashLParen    TokenType = "HASH_LPAREN"
	TokenHashLBrace    TokenType = "HASH_LBRACE"
	TokenBlockParam    TokenType = "BLOCK_PARAM"
	TokenSemi          TokenType = "SEMI"
	TokenAnd           TokenType = "AND"
	TokenOr            TokenType = "OR"
	TokenAmp           TokenType = "AMP"
	TokenRedirect      TokenType = "REDIRECT"
	TokenHeredoc       TokenType = "HEREDOC"
	TokenHerestring    TokenType = "HERESTRING"
	TokenGT            TokenType = "GT"
	TokenLT            TokenType = "LT"
	TokenGE            TokenType = "GE"
	TokenLE            TokenType = "LE"
	TokenEQ            TokenType = "EQ"
	TokenNE            TokenType = "NE"
	TokenEquals        TokenType = "EQUALS"
	TokenMatch         TokenType = "MATCH"
	TokenBang          TokenType = "BANG"
	TokenMinus         TokenType = "MINUS"
	TokenPlus          TokenType = "PLUS"
	TokenStar          TokenType = "STAR"
	TokenSlash         TokenType = "SLASH"
	TokenPercent       TokenType = "PERCENT"
	TokenPath          TokenType = "PATH"
	TokenQuestion      TokenType = "QUESTION"
	TokenComma         TokenType = "COMMA"
	TokenTilde         TokenType = "TILDE"
	TokenStrNE         TokenType = "STR_NE"
	TokenBackslash     TokenType = "BACKSLASH"
	TokenLiteral       TokenType = "LITERAL"
	TokenError         TokenType = "ERROR"
)

// =============================================================================
// Token
// =============================================================================

// Token represents a lexer token with position information.
type Token struct {
	Type  TokenType `json:"type"`
	Value string    `json:"value"`
	Line  int       `json:"line"`
	Col   int       `json:"col"`
}

// Location represents a source location for AST nodes.
type Location struct {
	Line int `json:"line"`
	Col  int `json:"col"`
}

// =============================================================================
// AST Node Types
// =============================================================================

// ClassAST represents a complete parsed class or trait.
type ClassAST struct {
	Type               string         `json:"type"`               // "class"
	Name               string         `json:"name"`               // Class name
	Package            string         `json:"package"`            // Package name (empty if none)
	Imports            []string       `json:"imports"`            // Imported packages
	Parent             string         `json:"parent"`             // Parent class name (empty for traits)
	ParentPackage      string         `json:"parentPackage"`      // Parent's package (if qualified)
	IsTrait            bool           `json:"isTrait"`            // True if this is a trait definition
	InstanceVars       []VarSpec      `json:"instanceVars"`       // Instance variables
	ClassInstanceVars  []VarSpec      `json:"classInstanceVars"`  // Class instance variables
	Traits             []string       `json:"traits"`             // Included traits
	Requires           []string       `json:"requires"`           // File dependencies
	MethodRequirements []string       `json:"methodRequirements"` // Protocol method requirements
	Methods            []MethodAST    `json:"methods"`            // Method definitions
	Aliases            []AliasAST     `json:"aliases"`            // Method aliases
	Advice             []AdviceAST    `json:"advice"`             // Before/after advice
	Warnings           []ParseWarning `json:"warnings"`           // Non-fatal parse warnings
	Location           Location       `json:"location"`           // Source location
}

// VarSpec represents an instance variable declaration with optional default.
type VarSpec struct {
	Name     string        `json:"name"`     // Variable name
	Default  *DefaultValue `json:"default"`  // Default value (nil if none)
	Location Location      `json:"location"` // Source location
}

// DefaultValue represents a default value for a variable.
type DefaultValue struct {
	Type  string `json:"type"`  // "number", "string", or "triplestring"
	Value string `json:"value"` // The actual value
}

// MethodAST represents a method definition.
type MethodAST struct {
	Type     string   `json:"type"`     // "method"
	Kind     string   `json:"kind"`     // "instance" or "class"
	Raw      bool     `json:"raw"`      // True for rawMethod/rawClassMethod
	Selector string   `json:"selector"` // Method selector (e.g., "getValue", "at_put_")
	Keywords []string `json:"keywords"` // Keywords for keyword methods
	Args     []string `json:"args"`     // Argument names
	Body     BlockAST `json:"body"`     // Method body
	Pragmas  []string `json:"pragmas"`  // Method pragmas (e.g., "direct")
	Category string   `json:"category"` // Method category (empty if none)
	Location Location `json:"location"` // Source location
}

// BlockAST represents a block of code (method body or advice block).
type BlockAST struct {
	Type   string  `json:"type"`   // "block"
	Tokens []Token `json:"tokens"` // Tokens within the block
}

// AliasAST represents a method alias declaration.
type AliasAST struct {
	Type           string   `json:"type"`           // "alias"
	AliasName      string   `json:"aliasName"`      // New method name
	OriginalMethod string   `json:"originalMethod"` // Existing method name
	Location       Location `json:"location"`       // Source location
}

// AdviceAST represents before/after method advice.
type AdviceAST struct {
	Type       string   `json:"type"`       // "advice"
	AdviceType string   `json:"adviceType"` // "before" or "after"
	Selector   string   `json:"selector"`   // Method selector to advise
	Block      BlockAST `json:"block"`      // Advice body
	Location   Location `json:"location"`   // Source location
}

// ParseWarning represents a non-fatal parse warning.
type ParseWarning struct {
	Type    string `json:"type"`    // Warning type (e.g., "possible_typo")
	Message string `json:"message"` // Warning message
	Line    int    `json:"line"`    // Source line
	Col     int    `json:"col"`     // Source column
}

// ParseError represents a parse error with context.
type ParseError struct {
	Type    string `json:"type"`    // Error type
	Message string `json:"message"` // Error message
	Token   *Token `json:"token"`   // Token that caused the error
	Context string `json:"context"` // Parsing context
}

func (e *ParseError) Error() string {
	if e.Token != nil {
		return fmt.Sprintf("%s at line %d, col %d: %s (context: %s)",
			e.Type, e.Token.Line, e.Token.Col, e.Message, e.Context)
	}
	return fmt.Sprintf("%s: %s (context: %s)", e.Type, e.Message, e.Context)
}

// =============================================================================
// ClassParser State
// =============================================================================

// ClassParser holds the state for parsing a class/trait token stream.
type ClassParser struct {
	tokens   []Token
	pos      int
	errors   []ParseError
	warnings []ParseWarning
}

// NewClassParser creates a new class parser for the given token stream.
func NewClassParser(tokens []Token) *ClassParser {
	return &ClassParser{
		tokens:   tokens,
		pos:      0,
		errors:   nil,
		warnings: nil,
	}
}

// =============================================================================
// ClassParser Utilities
// =============================================================================

// current returns the current token or nil if at end.
func (p *ClassParser) current() *Token {
	if p.pos < len(p.tokens) {
		return &p.tokens[p.pos]
	}
	return nil
}

// atEnd returns true if we've consumed all tokens.
func (p *ClassParser) atEnd() bool {
	return p.pos >= len(p.tokens)
}

// advance moves to the next token.
func (p *ClassParser) advance() {
	if p.pos < len(p.tokens) {
		p.pos++
	}
}

// skipNewlines skips NEWLINE and COMMENT tokens.
func (p *ClassParser) skipNewlines() {
	for !p.atEnd() {
		tok := p.current()
		if tok.Type != TokenNewline && tok.Type != TokenComment {
			break
		}
		p.advance()
	}
}

// isSyncPoint returns true if current token is a class-level keyword.
func (p *ClassParser) isSyncPoint() bool {
	tok := p.current()
	if tok == nil {
		return false
	}
	switch tok.Value {
	case "method:", "rawMethod:", "classMethod:", "rawClassMethod:",
		"instanceVars:", "classInstanceVars:", "include:", "requires:",
		"category:", "alias:", "before:", "after:":
		return true
	}
	return false
}

// synchronize skips tokens until we find a class-level keyword.
func (p *ClassParser) synchronize() {
	for !p.atEnd() && !p.isSyncPoint() {
		p.advance()
	}
}

// addError records a parse error.
func (p *ClassParser) addError(errType, message, context string) {
	p.errors = append(p.errors, ParseError{
		Type:    errType,
		Message: message,
		Token:   p.current(),
		Context: context,
	})
}

// addWarning records a parse warning.
func (p *ClassParser) addWarning(warnType, message string, line, col int) {
	p.warnings = append(p.warnings, ParseWarning{
		Type:    warnType,
		Message: message,
		Line:    line,
		Col:     col,
	})
}

// =============================================================================
// Class Reference Parsing
// =============================================================================

// ClassRef represents a possibly-qualified class reference.
type ClassRef struct {
	Package string // Package name (empty if unqualified)
	Name    string // Class name
}

// Format returns the string representation of the class reference.
func (r ClassRef) Format() string {
	if r.Package != "" {
		return r.Package + "::" + r.Name
	}
	return r.Name
}

// parseClassRef parses a class reference: IDENTIFIER or IDENTIFIER::IDENTIFIER
func (p *ClassParser) parseClassRef() (*ClassRef, bool) {
	tok := p.current()
	if tok == nil || tok.Type != TokenIdentifier {
		return nil, false
	}

	first := tok.Value
	p.advance()
	p.skipNewlines()

	// Check for namespace separator
	tok = p.current()
	if tok != nil && tok.Type == TokenNamespaceSep {
		p.advance()
		p.skipNewlines()

		tok = p.current()
		if tok == nil || tok.Type != TokenIdentifier {
			return nil, false
		}
		name := tok.Value
		p.advance()
		return &ClassRef{Package: first, Name: name}, true
	}

	return &ClassRef{Name: first}, true
}

// =============================================================================
// Package Declaration Parsing
// =============================================================================

// PackageDecl holds package and import declarations.
type PackageDecl struct {
	Package  string
	Imports  []string
	Location Location
}

// parsePackageDecl parses: package: Name import: Other import: Another
func (p *ClassParser) parsePackageDecl() *PackageDecl {
	p.skipNewlines()

	tok := p.current()
	if tok == nil || tok.Value != "package:" {
		return nil
	}

	loc := Location{Line: tok.Line, Col: tok.Col}
	p.advance()
	p.skipNewlines()

	tok = p.current()
	if tok == nil || tok.Type != TokenIdentifier {
		return nil
	}

	packageName := tok.Value
	p.advance()
	p.skipNewlines()

	// Collect imports
	var imports []string
	for {
		tok = p.current()
		if tok == nil || tok.Value != "import:" {
			break
		}
		p.advance()
		p.skipNewlines()

		tok = p.current()
		if tok != nil && tok.Type == TokenIdentifier {
			imports = append(imports, tok.Value)
			p.advance()
			p.skipNewlines()
		}
	}

	return &PackageDecl{
		Package:  packageName,
		Imports:  imports,
		Location: loc,
	}
}

// =============================================================================
// Class Header Parsing
// =============================================================================

// ClassHeader holds the parsed class header information.
type ClassHeader struct {
	Name          string
	Parent        string
	ParentPackage string
	IsTrait       bool
	Location      Location
}

// parseClassHeader parses: ClassName subclass: Parent | ClassName trait
func (p *ClassParser) parseClassHeader() (*ClassHeader, bool) {
	p.skipNewlines()

	tok := p.current()
	if tok == nil || tok.Type != TokenIdentifier {
		return nil, false
	}

	loc := Location{Line: tok.Line, Col: tok.Col}
	name := tok.Value
	p.advance()
	p.skipNewlines()

	tok = p.current()
	if tok == nil {
		return nil, false
	}

	if tok.Value == "subclass:" {
		p.advance()
		p.skipNewlines()

		parentRef, ok := p.parseClassRef()
		if !ok {
			return nil, false
		}

		return &ClassHeader{
			Name:          name,
			Parent:        parentRef.Format(),
			ParentPackage: parentRef.Package,
			IsTrait:       false,
			Location:      loc,
		}, true
	}

	if tok.Value == "trait" {
		p.advance()
		return &ClassHeader{
			Name:    name,
			IsTrait: true,
			Location: loc,
		}, true
	}

	return nil, false
}

// =============================================================================
// Instance Variables Parsing
// =============================================================================

// parseInstanceVars parses: instanceVars: var1 var2:0 var3:'default'
func (p *ClassParser) parseInstanceVars() ([]VarSpec, bool) {
	tok := p.current()
	if tok == nil || tok.Value != "instanceVars:" {
		return nil, false
	}

	p.advance()
	p.skipNewlines()

	return p.parseVarSpecs()
}

// parseClassInstanceVars parses: classInstanceVars: var1 var2:0
func (p *ClassParser) parseClassInstanceVars() ([]VarSpec, bool) {
	tok := p.current()
	if tok == nil || tok.Value != "classInstanceVars:" {
		return nil, false
	}

	p.advance()
	p.skipNewlines()

	return p.parseVarSpecs()
}

// parseVarSpecs collects variable specifications until we hit a sync point.
func (p *ClassParser) parseVarSpecs() ([]VarSpec, bool) {
	var vars []VarSpec

	for !p.atEnd() {
		tok := p.current()
		if tok == nil {
			break
		}

		// Stop at newline or sync point
		if tok.Type == TokenNewline || p.isSyncPoint() {
			break
		}

		loc := Location{Line: tok.Line, Col: tok.Col}

		if tok.Type == TokenKeyword {
			// Parse keyword with potential default
			kw := tok.Value
			parts := strings.SplitN(kw, ":", 2)
			name := strings.TrimSuffix(parts[0], "")

			// Check for embedded numeric default (e.g., "value:42")
			var def *DefaultValue
			if len(parts) > 1 && parts[1] != "" {
				def = &DefaultValue{Type: "number", Value: parts[1]}
			}

			p.advance()
			p.skipNewlines()

			// If no embedded default, check for explicit default value
			if def == nil {
				tok = p.current()
				if tok != nil {
					switch tok.Type {
					case TokenNumber:
						def = &DefaultValue{Type: "number", Value: tok.Value}
						p.advance()
						p.skipNewlines()
					case TokenString:
						val := strings.TrimPrefix(tok.Value, "'")
						val = strings.TrimSuffix(val, "'")
						def = &DefaultValue{Type: "string", Value: val}
						p.advance()
						p.skipNewlines()
					case TokenTripleString:
						def = &DefaultValue{Type: "triplestring", Value: tok.Value}
						p.advance()
						p.skipNewlines()
					case TokenIdentifier:
						// Bare identifier after keyword - might be typo
						p.addWarning("possible_typo",
							fmt.Sprintf("'%s: %s' - if this is meant to be a default, remove the space", name, tok.Value),
							loc.Line, loc.Col)
						// Treat as separate var, don't advance
					}
				}
			}

			vars = append(vars, VarSpec{Name: name, Default: def, Location: loc})

		} else if tok.Type == TokenIdentifier {
			// Simple variable name without default
			vars = append(vars, VarSpec{Name: tok.Value, Default: nil, Location: loc})
			p.advance()
			p.skipNewlines()
		} else {
			// Unknown token, stop
			break
		}
	}

	return vars, len(vars) > 0
}

// =============================================================================
// Include Parsing
// =============================================================================

// parseInclude parses: include: TraitName or include: Pkg::TraitName
func (p *ClassParser) parseInclude() (string, bool) {
	tok := p.current()
	if tok == nil || tok.Value != "include:" {
		return "", false
	}

	p.advance()
	p.skipNewlines()

	ref, ok := p.parseClassRef()
	if !ok {
		return "", false
	}

	return ref.Format(), true
}

// =============================================================================
// Requires Parsing
// =============================================================================

// RequiresResult holds the result of parsing a requires: declaration.
type RequiresResult struct {
	IsFile   bool   // True if file dependency, false if method requirement
	Value    string // Path or selector
	Location Location
}

// parseRequires parses: requires: 'path' OR requires: methodSelector
func (p *ClassParser) parseRequires() (*RequiresResult, bool) {
	tok := p.current()
	if tok == nil || tok.Value != "requires:" {
		return nil, false
	}

	loc := Location{Line: tok.Line, Col: tok.Col}
	p.advance()
	p.skipNewlines()

	tok = p.current()
	if tok == nil {
		return nil, false
	}

	if tok.Type == TokenString {
		// File dependency
		val := strings.TrimPrefix(tok.Value, "'")
		val = strings.TrimSuffix(val, "'")
		p.advance()
		return &RequiresResult{IsFile: true, Value: val, Location: loc}, true
	}

	if tok.Type == TokenKeyword {
		// Protocol method requirement - collect consecutive keywords
		var selector strings.Builder
		for {
			tok = p.current()
			if tok == nil || tok.Type != TokenKeyword || p.isSyncPoint() {
				break
			}

			if selector.Len() == 0 {
				selector.WriteString(tok.Value)
			} else {
				selector.WriteString(strings.TrimSuffix(tok.Value, ":"))
				selector.WriteString(":")
			}
			p.advance()
			p.skipNewlines()
		}

		return &RequiresResult{IsFile: false, Value: selector.String(), Location: loc}, true
	}

	return nil, false
}

// =============================================================================
// Alias Parsing
// =============================================================================

// parseAlias parses: alias: newName for: existingMethod
func (p *ClassParser) parseAlias() (*AliasAST, bool) {
	tok := p.current()
	if tok == nil || tok.Value != "alias:" {
		return nil, false
	}

	loc := Location{Line: tok.Line, Col: tok.Col}
	p.advance()
	p.skipNewlines()

	tok = p.current()
	if tok == nil || tok.Type != TokenIdentifier {
		return nil, false
	}
	aliasName := tok.Value
	p.advance()
	p.skipNewlines()

	tok = p.current()
	if tok == nil || tok.Value != "for:" {
		return nil, false
	}
	p.advance()
	p.skipNewlines()

	tok = p.current()
	if tok == nil || tok.Type != TokenIdentifier {
		return nil, false
	}
	originalMethod := tok.Value
	p.advance()

	return &AliasAST{
		Type:           "alias",
		AliasName:      aliasName,
		OriginalMethod: originalMethod,
		Location:       loc,
	}, true
}

// =============================================================================
// Advice Parsing
// =============================================================================

// parseAdvice parses: before: selector do: [block] OR after: selector do: [block]
func (p *ClassParser) parseAdvice() (*AdviceAST, bool) {
	tok := p.current()
	if tok == nil || (tok.Value != "before:" && tok.Value != "after:") {
		return nil, false
	}

	loc := Location{Line: tok.Line, Col: tok.Col}
	adviceType := "before"
	if tok.Value == "after:" {
		adviceType = "after"
	}
	p.advance()
	p.skipNewlines()

	tok = p.current()
	if tok == nil || tok.Type != TokenIdentifier {
		return nil, false
	}
	selector := tok.Value
	p.advance()
	p.skipNewlines()

	tok = p.current()
	if tok == nil || tok.Value != "do:" {
		return nil, false
	}
	p.advance()
	p.skipNewlines()

	block, ok := p.collectBlock()
	if !ok {
		return nil, false
	}

	return &AdviceAST{
		Type:       "advice",
		AdviceType: adviceType,
		Selector:   selector,
		Block:      block,
		Location:   loc,
	}, true
}

// =============================================================================
// Block Collection
// =============================================================================

// collectBlock collects tokens between [ and ], respecting nesting.
func (p *ClassParser) collectBlock() (BlockAST, bool) {
	tok := p.current()
	if tok == nil || tok.Type != TokenLBracket {
		return BlockAST{}, false
	}

	p.advance() // consume [

	var tokens []Token
	depth := 1

	for !p.atEnd() && depth > 0 {
		tok = p.current()
		if tok == nil {
			break
		}

		if tok.Type == TokenLBracket {
			depth++
			tokens = append(tokens, *tok)
			p.advance()
		} else if tok.Type == TokenRBracket {
			depth--
			if depth > 0 {
				tokens = append(tokens, *tok)
			}
			p.advance()
		} else {
			tokens = append(tokens, *tok)
			p.advance()
		}
	}

	return BlockAST{Type: "block", Tokens: tokens}, true
}

// =============================================================================
// Pragma Extraction
// =============================================================================

// extractPragmas extracts pragma directives from the start of method body tokens.
// Pragma format: pragma: <name>
func extractPragmas(tokens []Token) (pragmas []string, remaining []Token) {
	i := 0

	// Skip leading newlines
	for i < len(tokens) && tokens[i].Type == TokenNewline {
		i++
	}

	// Look for pragma: identifier patterns
	for i+1 < len(tokens) {
		if tokens[i].Type == TokenKeyword && tokens[i].Value == "pragma:" {
			i++ // skip pragma:
			// Skip newlines
			for i < len(tokens) && tokens[i].Type == TokenNewline {
				i++
			}
			if i < len(tokens) && tokens[i].Type == TokenIdentifier {
				pragmas = append(pragmas, tokens[i].Value)
				i++
				// Skip newlines after pragma value
				for i < len(tokens) && tokens[i].Type == TokenNewline {
					i++
				}
			}
		} else {
			break
		}
	}

	return pragmas, tokens[i:]
}

// =============================================================================
// Method Signature Parsing
// =============================================================================

// MethodSig holds a parsed method signature.
type MethodSig struct {
	Selector string
	Keywords []string
	Args     []string
}

// parseMethodSig parses a method signature (unary or keyword).
func (p *ClassParser) parseMethodSig() (*MethodSig, bool) {
	p.skipNewlines()

	tok := p.current()
	if tok == nil {
		return nil, false
	}

	if tok.Type == TokenKeyword {
		// Keyword method: key1: arg1 key2: arg2 ...
		var keywords []string
		var args []string
		var selectorParts []string

		for {
			tok = p.current()
			if tok == nil || tok.Type != TokenKeyword {
				break
			}

			kw := strings.TrimSuffix(tok.Value, ":")
			p.advance()
			p.skipNewlines()

			tok = p.current()
			if tok == nil || tok.Type != TokenIdentifier {
				break
			}

			keywords = append(keywords, kw)
			args = append(args, tok.Value)
			selectorParts = append(selectorParts, kw)
			p.advance()
			p.skipNewlines()
		}

		if len(keywords) == 0 {
			return nil, false
		}

		// Keyword selectors get trailing underscore: skip: -> skip_, at:put: -> at_put_
		selector := strings.Join(selectorParts, "_") + "_"

		return &MethodSig{
			Selector: selector,
			Keywords: keywords,
			Args:     args,
		}, true
	}

	if tok.Type == TokenIdentifier {
		// Unary method (no trailing underscore)
		selector := tok.Value
		p.advance()
		return &MethodSig{Selector: selector}, true
	}

	return nil, false
}

// =============================================================================
// Method Parsing
// =============================================================================

// parseMethod parses a method declaration.
func (p *ClassParser) parseMethod() (*MethodAST, bool) {
	p.skipNewlines()

	tok := p.current()
	if tok == nil {
		return nil, false
	}

	loc := Location{Line: tok.Line, Col: tok.Col}

	// Determine method kind and raw status
	var kind string
	var raw bool

	switch tok.Value {
	case "method:":
		kind = "instance"
		raw = false
	case "rawMethod:":
		kind = "instance"
		raw = true
	case "classMethod:":
		kind = "class"
		raw = false
	case "rawClassMethod:":
		kind = "class"
		raw = true
	default:
		return nil, false
	}

	p.advance()
	p.skipNewlines()

	sig, ok := p.parseMethodSig()
	if !ok {
		return nil, false
	}

	p.skipNewlines()

	body, ok := p.collectBlock()
	if !ok {
		return nil, false
	}

	// Extract pragmas from body tokens
	pragmas, remaining := extractPragmas(body.Tokens)
	body.Tokens = remaining

	return &MethodAST{
		Type:     "method",
		Kind:     kind,
		Raw:      raw,
		Selector: sig.Selector,
		Keywords: sig.Keywords,
		Args:     sig.Args,
		Body:     body,
		Pragmas:  pragmas,
		Location: loc,
	}, true
}

// =============================================================================
// Class Body Parsing
// =============================================================================

// parseClassBody parses all declarations within a class body.
func (p *ClassParser) parseClassBody() (
	instanceVars []VarSpec,
	classInstanceVars []VarSpec,
	traits []string,
	requires []string,
	methodRequirements []string,
	methods []MethodAST,
	aliases []AliasAST,
	advice []AdviceAST,
) {
	var currentCategory string

	for !p.atEnd() {
		p.skipNewlines()

		if p.atEnd() {
			break
		}

		tok := p.current()
		if tok == nil {
			break
		}

		switch tok.Value {
		case "category:":
			// Parse category directive
			p.advance()
			p.skipNewlines()
			tok = p.current()
			if tok != nil && (tok.Type == TokenString || tok.Type == TokenDString) {
				val := strings.TrimPrefix(tok.Value, "'")
				val = strings.TrimSuffix(val, "'")
				val = strings.TrimPrefix(val, "\"")
				val = strings.TrimSuffix(val, "\"")
				currentCategory = val
				p.advance()
			} else {
				p.addError("parse_error", "Expected string after category:", "category")
				p.synchronize()
			}

		case "instanceVars:":
			if vars, ok := p.parseInstanceVars(); ok {
				instanceVars = vars
			} else {
				p.addError("parse_error", "Failed to parse instanceVars declaration", "instanceVars")
				p.advance()
				p.synchronize()
			}

		case "classInstanceVars:":
			if vars, ok := p.parseClassInstanceVars(); ok {
				classInstanceVars = vars
			} else {
				p.addError("parse_error", "Failed to parse classInstanceVars declaration", "classInstanceVars")
				p.advance()
				p.synchronize()
			}

		case "include:":
			if trait, ok := p.parseInclude(); ok {
				traits = append(traits, trait)
			} else {
				p.addError("parse_error", "Failed to parse include declaration", "include")
				p.advance()
				p.synchronize()
			}

		case "requires:":
			if req, ok := p.parseRequires(); ok {
				if req.IsFile {
					requires = append(requires, req.Value)
				} else {
					methodRequirements = append(methodRequirements, req.Value)
				}
			} else {
				p.addError("parse_error", "Failed to parse requires declaration", "requires")
				p.advance()
				p.synchronize()
			}

		case "alias:":
			if alias, ok := p.parseAlias(); ok {
				aliases = append(aliases, *alias)
			} else {
				p.addError("parse_error", "Failed to parse alias declaration", "alias")
				p.advance()
				p.synchronize()
			}

		case "before:", "after:":
			if adv, ok := p.parseAdvice(); ok {
				advice = append(advice, *adv)
			} else {
				p.addError("parse_error", "Failed to parse advice declaration", "advice")
				p.advance()
				p.synchronize()
			}

		case "method:", "rawMethod:", "classMethod:", "rawClassMethod:":
			if method, ok := p.parseMethod(); ok {
				if currentCategory != "" {
					method.Category = currentCategory
				}
				methods = append(methods, *method)
			} else {
				p.addError("parse_error", "Failed to parse method declaration", "method")
				p.advance()
				p.synchronize()
			}

		default:
			// Unknown token
			if tok.Type == TokenNewline || tok.Type == TokenComment {
				p.advance()
			} else {
				p.addError("unknown_token", "Unexpected token in class body", "class_body")
				p.synchronize()
			}
		}
	}

	return
}

// =============================================================================
// Main Parse Function
// =============================================================================

// Parse parses a token stream into a ClassAST.
// Returns the parsed AST and any parse errors.
func (p *ClassParser) Parse() (*ClassAST, []ParseError) {
	// First check for package declaration
	pkgDecl := p.parsePackageDecl()

	// Parse class header
	header, ok := p.parseClassHeader()
	if !ok {
		p.addError("parse_error", "Failed to parse class header", "class_header")
		return nil, p.errors
	}

	// Parse class body
	instanceVars, classInstanceVars, traits, requires, methodRequirements, methods, aliases, advice := p.parseClassBody()

	// Build the AST
	ast := &ClassAST{
		Type:               "class",
		Name:               header.Name,
		Parent:             header.Parent,
		ParentPackage:      header.ParentPackage,
		IsTrait:            header.IsTrait,
		InstanceVars:       instanceVars,
		ClassInstanceVars:  classInstanceVars,
		Traits:             traits,
		Requires:           requires,
		MethodRequirements: methodRequirements,
		Methods:            methods,
		Aliases:            aliases,
		Advice:             advice,
		Warnings:           p.warnings,
		Location:           header.Location,
	}

	// Add package info if present
	if pkgDecl != nil {
		ast.Package = pkgDecl.Package
		ast.Imports = pkgDecl.Imports
	}

	return ast, p.errors
}

// ParseClass is a convenience function to parse tokens into a ClassAST.
func ParseClass(tokens []Token) (*ClassAST, []ParseError) {
	p := NewClassParser(tokens)
	return p.Parse()
}
