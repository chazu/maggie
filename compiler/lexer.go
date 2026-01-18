package compiler

import (
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

// ---------------------------------------------------------------------------
// Lexer: Tokenizer for Smalltalk syntax
// ---------------------------------------------------------------------------

// Lexer tokenizes Smalltalk source code.
type Lexer struct {
	input   string
	pos     int  // current position in input
	readPos int  // reading position (after current char)
	ch      rune // current character
	line    int  // current line (1-based)
	col     int  // current column (1-based)
	lineStart int // offset of current line start
}

// NewLexer creates a new lexer for the given input.
func NewLexer(input string) *Lexer {
	l := &Lexer{
		input: input,
		line:  1,
		col:   1,
	}
	l.readChar()
	return l
}

// readChar reads the next character.
func (l *Lexer) readChar() {
	if l.readPos >= len(l.input) {
		l.ch = 0 // EOF
		l.pos = l.readPos // Update pos even at EOF
	} else {
		r, size := utf8.DecodeRuneInString(l.input[l.readPos:])
		l.ch = r
		l.pos = l.readPos
		l.readPos += size

		// Track line/column
		if r == '\n' {
			l.line++
			l.col = 1
			l.lineStart = l.readPos
		} else {
			l.col++
		}
	}
}

// peekChar returns the next character without consuming it.
func (l *Lexer) peekChar() rune {
	if l.readPos >= len(l.input) {
		return 0
	}
	r, _ := utf8.DecodeRuneInString(l.input[l.readPos:])
	return r
}

// position returns the current position.
func (l *Lexer) position() Position {
	return Position{
		Offset: l.pos,
		Line:   l.line,
		Column: l.col,
	}
}

// NextToken returns the next token.
func (l *Lexer) NextToken() Token {
	l.skipWhitespaceAndComments()

	pos := l.position()

	switch {
	case l.ch == 0:
		return Token{Type: TokenEOF, Literal: "", Pos: pos}

	case l.ch == '(':
		l.readChar()
		return Token{Type: TokenLParen, Literal: "(", Pos: pos}

	case l.ch == ')':
		l.readChar()
		return Token{Type: TokenRParen, Literal: ")", Pos: pos}

	case l.ch == '[':
		l.readChar()
		return Token{Type: TokenLBracket, Literal: "[", Pos: pos}

	case l.ch == ']':
		l.readChar()
		return Token{Type: TokenRBracket, Literal: "]", Pos: pos}

	case l.ch == '{':
		l.readChar()
		return Token{Type: TokenLBrace, Literal: "{", Pos: pos}

	case l.ch == '}':
		l.readChar()
		return Token{Type: TokenRBrace, Literal: "}", Pos: pos}

	case l.ch == '^':
		l.readChar()
		return Token{Type: TokenCaret, Literal: "^", Pos: pos}

	case l.ch == '.':
		l.readChar()
		return Token{Type: TokenPeriod, Literal: ".", Pos: pos}

	case l.ch == ';':
		l.readChar()
		return Token{Type: TokenSemicolon, Literal: ";", Pos: pos}

	case l.ch == ':':
		l.readChar()
		if l.ch == '=' {
			l.readChar()
			return Token{Type: TokenAssign, Literal: ":=", Pos: pos}
		}
		return Token{Type: TokenColon, Literal: ":", Pos: pos}

	case l.ch == '|':
		l.readChar()
		return Token{Type: TokenBar, Literal: "|", Pos: pos}

	case l.ch == '#':
		return l.readHashToken(pos)

	case l.ch == '\'':
		return l.readString(pos)

	case l.ch == '$':
		return l.readCharacter(pos)

	case isDigit(l.ch):
		return l.readNumber(pos)

	case l.ch == '-' && isDigit(l.peekChar()):
		return l.readNumber(pos)

	case isLetter(l.ch) || l.ch == '_':
		return l.readIdentifierOrKeyword(pos)

	case IsBinaryChar(l.ch):
		return l.readBinarySelector(pos)

	default:
		ch := l.ch
		l.readChar()
		return Token{Type: TokenError, Literal: fmt.Sprintf("unexpected character: %c", ch), Pos: pos}
	}
}

// skipWhitespaceAndComments skips whitespace and comments.
func (l *Lexer) skipWhitespaceAndComments() {
	for {
		// Skip whitespace
		for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
			l.readChar()
		}

		// Skip comments
		if l.ch == '"' {
			l.readChar()
			for l.ch != '"' && l.ch != 0 {
				l.readChar()
			}
			if l.ch == '"' {
				l.readChar()
			}
			continue
		}

		break
	}
}

// readHashToken reads a token starting with #.
func (l *Lexer) readHashToken(pos Position) Token {
	l.readChar() // consume #

	switch {
	case l.ch == '(':
		l.readChar()
		return Token{Type: TokenHashLParen, Literal: "#(", Pos: pos}

	case l.ch == '\'':
		// Quoted symbol: #'hello world'
		return l.readQuotedSymbol(pos)

	case isLetter(l.ch) || l.ch == '_':
		// Symbol: #foo or #foo:bar:
		return l.readSymbol(pos)

	case IsBinaryChar(l.ch):
		// Binary symbol: #+, #--, etc.
		start := l.pos
		for IsBinaryChar(l.ch) {
			l.readChar()
		}
		return Token{Type: TokenSymbol, Literal: l.input[start:l.pos], Pos: pos}

	default:
		return Token{Type: TokenHash, Literal: "#", Pos: pos}
	}
}

// readSymbol reads a symbol starting with a letter.
func (l *Lexer) readSymbol(pos Position) Token {
	var sb strings.Builder

	// Read identifier parts and colons
	for {
		// Read identifier part
		for isLetter(l.ch) || isDigit(l.ch) || l.ch == '_' {
			sb.WriteRune(l.ch)
			l.readChar()
		}

		// Check for colon (keyword symbol)
		if l.ch == ':' {
			sb.WriteRune(':')
			l.readChar()
			// Continue if more identifier follows
			if isLetter(l.ch) || l.ch == '_' {
				continue
			}
		}
		break
	}

	return Token{Type: TokenSymbol, Literal: sb.String(), Pos: pos}
}

// readQuotedSymbol reads a quoted symbol #'...'.
func (l *Lexer) readQuotedSymbol(pos Position) Token {
	l.readChar() // consume opening '

	var sb strings.Builder
	for l.ch != 0 {
		if l.ch == '\'' {
			if l.peekChar() == '\'' {
				// Escaped quote
				sb.WriteRune('\'')
				l.readChar() // consume first '
				l.readChar() // consume second '
				continue
			}
			// End of symbol
			break
		}
		sb.WriteRune(l.ch)
		l.readChar()
	}

	if l.ch == '\'' {
		l.readChar() // consume closing '
	}

	return Token{Type: TokenSymbol, Literal: sb.String(), Pos: pos}
}

// readString reads a string literal.
func (l *Lexer) readString(pos Position) Token {
	l.readChar() // consume opening '

	var sb strings.Builder
	for l.ch != 0 {
		if l.ch == '\'' {
			if l.peekChar() == '\'' {
				// Escaped quote
				sb.WriteRune('\'')
				l.readChar() // consume first '
				l.readChar() // consume second '
				continue
			}
			// End of string
			break
		}
		sb.WriteRune(l.ch)
		l.readChar()
	}

	if l.ch == '\'' {
		l.readChar() // consume closing '
	} else {
		return Token{Type: TokenError, Literal: "unterminated string", Pos: pos}
	}

	return Token{Type: TokenString, Literal: sb.String(), Pos: pos}
}

// readCharacter reads a character literal.
func (l *Lexer) readCharacter(pos Position) Token {
	l.readChar() // consume $

	if l.ch == 0 {
		return Token{Type: TokenError, Literal: "unexpected EOF in character literal", Pos: pos}
	}

	ch := l.ch
	l.readChar()

	return Token{Type: TokenCharacter, Literal: string(ch), Pos: pos}
}

// readNumber reads an integer or float literal.
func (l *Lexer) readNumber(pos Position) Token {
	start := l.pos
	isFloat := false

	// Handle negative sign
	if l.ch == '-' {
		l.readChar()
	}

	// Check for radix notation (16rFF)
	digitStart := l.pos
	for isDigit(l.ch) {
		l.readChar()
	}

	if l.ch == 'r' {
		// Radix notation
		l.readChar()
		for isHexDigit(l.ch) {
			l.readChar()
		}
		return Token{Type: TokenInteger, Literal: l.input[start:l.pos], Pos: pos}
	}

	// Check for float
	if l.ch == '.' && isDigit(l.peekChar()) {
		isFloat = true
		l.readChar() // consume .
		for isDigit(l.ch) {
			l.readChar()
		}
	}

	// Check for exponent
	if l.ch == 'e' || l.ch == 'E' {
		isFloat = true
		l.readChar()
		if l.ch == '+' || l.ch == '-' {
			l.readChar()
		}
		for isDigit(l.ch) {
			l.readChar()
		}
	}

	// Handle case where we only read digits but nothing else
	_ = digitStart

	if isFloat {
		return Token{Type: TokenFloat, Literal: l.input[start:l.pos], Pos: pos}
	}
	return Token{Type: TokenInteger, Literal: l.input[start:l.pos], Pos: pos}
}

// readIdentifierOrKeyword reads an identifier or keyword.
func (l *Lexer) readIdentifierOrKeyword(pos Position) Token {
	start := l.pos

	for isLetter(l.ch) || isDigit(l.ch) || l.ch == '_' {
		l.readChar()
	}

	literal := l.input[start:l.pos]

	// Check for keyword (ends with :)
	if l.ch == ':' && l.peekChar() != '=' {
		l.readChar() // consume :
		return Token{Type: TokenKeyword, Literal: literal + ":", Pos: pos}
	}

	// Check for reserved word
	if tokType, ok := reservedWords[literal]; ok {
		return Token{Type: tokType, Literal: literal, Pos: pos}
	}

	return Token{Type: TokenIdentifier, Literal: literal, Pos: pos}
}

// readBinarySelector reads a binary selector.
func (l *Lexer) readBinarySelector(pos Position) Token {
	start := l.pos

	for IsBinaryChar(l.ch) {
		l.readChar()
	}

	return Token{Type: TokenBinarySelector, Literal: l.input[start:l.pos], Pos: pos}
}

// Helper functions

func isLetter(r rune) bool {
	return unicode.IsLetter(r)
}

func isDigit(r rune) bool {
	return r >= '0' && r <= '9'
}

func isHexDigit(r rune) bool {
	return isDigit(r) || (r >= 'a' && r <= 'f') || (r >= 'A' && r <= 'F')
}

// Tokenize returns all tokens from the input.
func Tokenize(input string) []Token {
	l := NewLexer(input)
	var tokens []Token
	for {
		tok := l.NextToken()
		tokens = append(tokens, tok)
		if tok.Type == TokenEOF || tok.Type == TokenError {
			break
		}
	}
	return tokens
}
