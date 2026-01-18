// Package lexer provides tokenization for the Trashtalk language.
package lexer

// TokenType represents the type of a token.
type TokenType string

// Token types matching the Bash tokenizer output.
const (
	// Basic tokens
	IDENTIFIER   TokenType = "IDENTIFIER"   // Variable/class names (e.g., Counter, myVar, _private)
	KEYWORD      TokenType = "KEYWORD"      // Identifiers ending with colon (e.g., method:, subclass:)
	STRING       TokenType = "STRING"       // Single-quoted strings (e.g., 'hello')
	TRIPLESTRING TokenType = "TRIPLESTRING" // Triple-quoted strings (e.g., '''multi-line''')
	DSTRING      TokenType = "DSTRING"      // Double-quoted strings (e.g., "hello $var")
	NUMBER       TokenType = "NUMBER"       // Numeric literals (e.g., 42, -1, 3.14)
	SYMBOL       TokenType = "SYMBOL"       // Symbols starting with # (e.g., #symbolName)
	COMMENT      TokenType = "COMMENT"      // Comments starting with #
	PATH         TokenType = "PATH"         // File paths (e.g., /dev/null)

	// Brackets and delimiters
	LBRACKET   TokenType = "LBRACKET"   // [
	RBRACKET   TokenType = "RBRACKET"   // ]
	DLBRACKET  TokenType = "DLBRACKET"  // [[
	DRBRACKET  TokenType = "DRBRACKET"  // ]]
	LPAREN     TokenType = "LPAREN"     // (
	RPAREN     TokenType = "RPAREN"     // )
	LBRACE     TokenType = "LBRACE"     // {
	RBRACE     TokenType = "RBRACE"     // }
	HASHlparen TokenType = "HASH_LPAREN" // #( for array literals
	HASHLBRACE TokenType = "HASH_LBRACE" // #{ for dictionary literals

	// Operators
	PIPE         TokenType = "PIPE"          // |
	OR           TokenType = "OR"            // ||
	AND          TokenType = "AND"           // &&
	CARET        TokenType = "CARET"         // ^ (return)
	AT           TokenType = "AT"            // @ (message send)
	ASSIGN       TokenType = "ASSIGN"        // :=
	EQUALS       TokenType = "EQUALS"        // =
	EQ           TokenType = "EQ"            // ==
	NE           TokenType = "NE"            // !=
	MATCH        TokenType = "MATCH"         // =~
	GT           TokenType = "GT"            // >
	GE           TokenType = "GE"            // >=
	LT           TokenType = "LT"            // <
	LE           TokenType = "LE"            // <=
	STR_NE       TokenType = "STR_NE"        // ~=
	PLUS         TokenType = "PLUS"          // +
	MINUS        TokenType = "MINUS"         // -
	STAR         TokenType = "STAR"          // *
	SLASH        TokenType = "SLASH"         // /
	PERCENT      TokenType = "PERCENT"       // %
	BANG         TokenType = "BANG"          // !
	NAMESPACE_SEP TokenType = "NAMESPACE_SEP" // ::

	// Punctuation
	DOT        TokenType = "DOT"        // .
	COMMA      TokenType = "COMMA"      // ,
	SEMI       TokenType = "SEMI"       // ;
	AMP        TokenType = "AMP"        // &
	TILDE      TokenType = "TILDE"      // ~
	QUESTION   TokenType = "QUESTION"   // ?
	BACKSLASH  TokenType = "BACKSLASH"  // \

	// Whitespace and structure
	NEWLINE TokenType = "NEWLINE" // Line break

	// Shell-specific tokens
	VARIABLE   TokenType = "VARIABLE"   // $var, ${...}
	SUBSHELL   TokenType = "SUBSHELL"   // $(...)
	ARITHMETIC TokenType = "ARITHMETIC" // $((...))
	ARITH_CMD  TokenType = "ARITH_CMD"  // ((...))
	REDIRECT   TokenType = "REDIRECT"   // >, >>, <, <<, &>, etc.
	HEREDOC    TokenType = "HEREDOC"    // <<
	HERESTRING TokenType = "HERESTRING" // <<<
	BLOCKPARAM TokenType = "BLOCK_PARAM" // :x, :each (block parameters)

	// Special tokens
	LITERAL TokenType = "LITERAL" // Unknown/literal characters
	ERROR   TokenType = "ERROR"   // Error token
	EOF     TokenType = "EOF"     // End of file
)

// Token represents a single token from the lexer.
type Token struct {
	Type   TokenType `json:"type"`
	Value  string    `json:"value"`
	Line   int       `json:"line"`
	Column int       `json:"col"`
}

// NewToken creates a new token with the given properties.
func NewToken(typ TokenType, value string, line, col int) Token {
	return Token{
		Type:   typ,
		Value:  value,
		Line:   line,
		Column: col,
	}
}

// IsKeyword returns true if the token is a keyword (identifier ending with colon).
func (t Token) IsKeyword() bool {
	return t.Type == KEYWORD
}

// IsIdentifier returns true if the token is an identifier.
func (t Token) IsIdentifier() bool {
	return t.Type == IDENTIFIER
}

// IsOperator returns true if the token is an operator.
func (t Token) IsOperator() bool {
	switch t.Type {
	case PIPE, OR, AND, CARET, AT, ASSIGN, EQUALS, EQ, NE, MATCH,
		GT, GE, LT, LE, STR_NE, PLUS, MINUS, STAR, SLASH, PERCENT,
		BANG, NAMESPACE_SEP:
		return true
	}
	return false
}

// IsLiteral returns true if the token represents a literal value.
func (t Token) IsLiteral() bool {
	switch t.Type {
	case STRING, TRIPLESTRING, DSTRING, NUMBER, SYMBOL:
		return true
	}
	return false
}
