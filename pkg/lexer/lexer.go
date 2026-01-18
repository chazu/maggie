// Package lexer provides tokenization for the Trashtalk language.
//
// This is a Go port of the original Bash tokenizer from
// lib/jq-compiler/tokenizer.bash. It performs character-by-character
// processing to produce tokens for the parser.
//
// Token Types:
//
//	IDENTIFIER  - Variable/class names (e.g., Counter, myVar, _private)
//	KEYWORD     - Identifiers ending with colon (e.g., method:, subclass:)
//	STRING      - Single-quoted strings (e.g., 'hello')
//	NUMBER      - Numeric literals (e.g., 42, -1, 3.14)
//	LBRACKET    - Left bracket [
//	RBRACKET    - Right bracket ]
//	PIPE        - Pipe character |
//	CARET       - Caret ^ (return)
//	AT          - At sign @ (message send)
//	ASSIGN      - Assignment operator :=
//	DOT         - Period . (statement terminator)
//	NEWLINE     - Line break (preserved for error reporting)
//
// Output Format (JSON array):
//
//	[{"type": "IDENTIFIER", "value": "Counter", "line": 1, "col": 0}, ...]
package lexer

import (
	"encoding/json"
	"fmt"
	"io"
	"strings"
)

// Lexer tokenizes Trashtalk source code.
type Lexer struct {
	input   string // The source code being tokenized
	pos     int    // Current position in input
	line    int    // Current line number (1-indexed)
	col     int    // Current column number (0-indexed)
	tokens  []Token
}

// New creates a new Lexer for the given input.
func New(input string) *Lexer {
	return &Lexer{
		input:  input,
		pos:    0,
		line:   1,
		col:    0,
		tokens: make([]Token, 0),
	}
}

// NewFromReader creates a new Lexer from an io.Reader.
func NewFromReader(r io.Reader) (*Lexer, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, fmt.Errorf("failed to read input: %w", err)
	}
	return New(string(data)), nil
}

// Tokenize processes the entire input and returns all tokens.
func (l *Lexer) Tokenize() ([]Token, error) {
	for !l.isAtEnd() {
		if err := l.scanToken(); err != nil {
			return nil, err
		}
	}
	return l.tokens, nil
}

// TokenizeJSON processes the input and returns tokens as a JSON array.
// This matches the output format of the original Bash tokenizer.
func (l *Lexer) TokenizeJSON() (string, error) {
	tokens, err := l.Tokenize()
	if err != nil {
		return "", err
	}
	data, err := json.Marshal(tokens)
	if err != nil {
		return "", fmt.Errorf("failed to marshal tokens: %w", err)
	}
	return string(data), nil
}

// Helper methods for character access and movement

func (l *Lexer) isAtEnd() bool {
	return l.pos >= len(l.input)
}

func (l *Lexer) peek() byte {
	if l.isAtEnd() {
		return 0
	}
	return l.input[l.pos]
}

func (l *Lexer) peekNext() byte {
	if l.pos+1 >= len(l.input) {
		return 0
	}
	return l.input[l.pos+1]
}

func (l *Lexer) peekAhead(n int) byte {
	if l.pos+n >= len(l.input) {
		return 0
	}
	return l.input[l.pos+n]
}

func (l *Lexer) advance() byte {
	ch := l.input[l.pos]
	l.pos++
	l.col++
	return ch
}

func (l *Lexer) addToken(typ TokenType, value string) {
	// Calculate the start column (we've already advanced past the token)
	startCol := l.col - len(value)
	if startCol < 0 {
		startCol = 0
	}
	l.tokens = append(l.tokens, NewToken(typ, value, l.line, startCol))
}

func (l *Lexer) addTokenAt(typ TokenType, value string, line, col int) {
	l.tokens = append(l.tokens, NewToken(typ, value, line, col))
}

func isAlpha(c byte) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

func isDigit(c byte) bool {
	return c >= '0' && c <= '9'
}

func isAlphaNumeric(c byte) bool {
	return isAlpha(c) || isDigit(c)
}

// scanToken scans a single token from the current position.
func (l *Lexer) scanToken() error {
	char := l.peek()
	next := l.peekNext()

	switch char {
	// Whitespace (space, tab) - skip but track column
	case ' ', '\t':
		l.advance()
		return nil

	// Newline - emit token and update position
	case '\n':
		l.addTokenAt(NEWLINE, "\\n", l.line, l.col)
		l.advance()
		l.line++
		l.col = 0
		return nil

	// Hash - could be comment, symbol, array literal, or dict literal
	case '#':
		return l.scanHash()

	// Brackets
	case '[':
		if next == '[' {
			startCol := l.col
			l.advance()
			l.advance()
			l.addTokenAt(DLBRACKET, "[[", l.line, startCol)
		} else {
			startCol := l.col
			l.advance()
			l.addTokenAt(LBRACKET, "[", l.line, startCol)
		}
		return nil

	case ']':
		if next == ']' {
			startCol := l.col
			l.advance()
			l.advance()
			l.addTokenAt(DRBRACKET, "]]", l.line, startCol)
		} else {
			startCol := l.col
			l.advance()
			l.addTokenAt(RBRACKET, "]", l.line, startCol)
		}
		return nil

	// Pipe
	case '|':
		if next == '|' {
			startCol := l.col
			l.advance()
			l.advance()
			l.addTokenAt(OR, "||", l.line, startCol)
		} else {
			startCol := l.col
			l.advance()
			l.addTokenAt(PIPE, "|", l.line, startCol)
		}
		return nil

	// Caret (return)
	case '^':
		startCol := l.col
		l.advance()
		l.addTokenAt(CARET, "^", l.line, startCol)
		return nil

	// At sign (message send)
	case '@':
		startCol := l.col
		l.advance()
		l.addTokenAt(AT, "@", l.line, startCol)
		return nil

	// Dot (statement terminator)
	case '.':
		startCol := l.col
		l.advance()
		l.addTokenAt(DOT, ".", l.line, startCol)
		return nil

	// Semicolon (statement separator in bash)
	case ';':
		startCol := l.col
		l.advance()
		l.addTokenAt(SEMI, ";", l.line, startCol)
		return nil

	// Ampersand
	case '&':
		return l.scanAmpersand()

	// Greater than
	case '>':
		return l.scanGreaterThan()

	// Less than
	case '<':
		return l.scanLessThan()

	// Equal sign
	case '=':
		return l.scanEquals()

	// Exclamation
	case '!':
		if next == '=' {
			startCol := l.col
			l.advance()
			l.advance()
			l.addTokenAt(NE, "!=", l.line, startCol)
		} else {
			startCol := l.col
			l.advance()
			l.addTokenAt(BANG, "!", l.line, startCol)
		}
		return nil

	// Colon
	case ':':
		return l.scanColon()

	// Single or triple quoted string
	case '\'':
		return l.scanSingleQuotedString()

	// Double-quoted string
	case '"':
		return l.scanDoubleQuotedString()

	// Numbers
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		return l.scanNumber()

	// Minus (negative number or operator)
	case '-':
		if isDigit(next) {
			return l.scanNegativeNumber()
		}
		startCol := l.col
		l.advance()
		l.addTokenAt(MINUS, "-", l.line, startCol)
		return nil

	// Dollar sign (variables, subshells, arithmetic)
	case '$':
		return l.scanDollar()

	// Parentheses
	case '(':
		return l.scanLeftParen()

	case ')':
		startCol := l.col
		l.advance()
		l.addTokenAt(RPAREN, ")", l.line, startCol)
		return nil

	// Curly braces
	case '{':
		startCol := l.col
		l.advance()
		l.addTokenAt(LBRACE, "{", l.line, startCol)
		return nil

	case '}':
		startCol := l.col
		l.advance()
		l.addTokenAt(RBRACE, "}", l.line, startCol)
		return nil

	// Forward slash (path or division)
	case '/':
		return l.scanSlash()

	// Question mark
	case '?':
		startCol := l.col
		l.advance()
		l.addTokenAt(QUESTION, "?", l.line, startCol)
		return nil

	// Plus sign
	case '+':
		startCol := l.col
		l.advance()
		l.addTokenAt(PLUS, "+", l.line, startCol)
		return nil

	// Asterisk
	case '*':
		startCol := l.col
		l.advance()
		l.addTokenAt(STAR, "*", l.line, startCol)
		return nil

	// Comma
	case ',':
		startCol := l.col
		l.advance()
		l.addTokenAt(COMMA, ",", l.line, startCol)
		return nil

	// Tilde
	case '~':
		if next == '=' {
			startCol := l.col
			l.advance()
			l.advance()
			l.addTokenAt(STR_NE, "~=", l.line, startCol)
		} else {
			startCol := l.col
			l.advance()
			l.addTokenAt(TILDE, "~", l.line, startCol)
		}
		return nil

	// Percent (modulo)
	case '%':
		startCol := l.col
		l.advance()
		l.addTokenAt(PERCENT, "%", l.line, startCol)
		return nil

	// Backslash
	case '\\':
		startCol := l.col
		l.advance()
		l.addTokenAt(BACKSLASH, "\\\\", l.line, startCol)
		return nil

	default:
		// Identifier or keyword
		if isAlpha(char) {
			return l.scanIdentifierOrKeyword()
		}
		// Unknown character - emit as literal
		startCol := l.col
		l.advance()
		l.addTokenAt(LITERAL, string(char), l.line, startCol)
		return nil
	}
}

// scanHash handles #, #(, #{, #symbol, and comments.
func (l *Lexer) scanHash() error {
	startCol := l.col
	next := l.peekNext()

	if next == '(' {
		// Array literal: #(...)
		l.advance()
		l.advance()
		l.addTokenAt(HASHlparen, "#(", l.line, startCol)
		return nil
	}

	if next == '{' {
		// Dictionary literal: #{...}
		l.advance()
		l.advance()
		l.addTokenAt(HASHLBRACE, "#{", l.line, startCol)
		return nil
	}

	if isAlpha(next) {
		// Symbol: #symbolName
		l.advance() // skip #
		symStartCol := l.col
		var symbol strings.Builder
		for !l.isAtEnd() && isAlphaNumeric(l.peek()) {
			symbol.WriteByte(l.advance())
		}
		l.addTokenAt(SYMBOL, symbol.String(), l.line, symStartCol-1)
		return nil
	}

	// Comment - capture until end of line
	var comment strings.Builder
	for !l.isAtEnd() && l.peek() != '\n' {
		comment.WriteByte(l.advance())
	}
	l.addTokenAt(COMMENT, comment.String(), l.line, startCol)
	return nil
}

// scanAmpersand handles &, &&, &>, and &>>.
func (l *Lexer) scanAmpersand() error {
	startCol := l.col
	next := l.peekNext()

	if next == '&' {
		l.advance()
		l.advance()
		l.addTokenAt(AND, "&&", l.line, startCol)
		return nil
	}

	if next == '>' {
		l.advance()
		l.advance()
		if l.peek() == '>' {
			l.advance()
			l.addTokenAt(REDIRECT, "&>>", l.line, startCol)
		} else {
			l.addTokenAt(REDIRECT, "&>", l.line, startCol)
		}
		return nil
	}

	l.advance()
	l.addTokenAt(AMP, "&", l.line, startCol)
	return nil
}

// scanGreaterThan handles >, >>, >&, and >=.
func (l *Lexer) scanGreaterThan() error {
	startCol := l.col
	next := l.peekNext()

	if next == '>' {
		l.advance()
		l.advance()
		l.addTokenAt(REDIRECT, ">>", l.line, startCol)
		return nil
	}

	if next == '&' {
		l.advance()
		l.advance()
		l.addTokenAt(REDIRECT, ">&", l.line, startCol)
		return nil
	}

	if next == '=' {
		l.advance()
		l.advance()
		l.addTokenAt(GE, ">=", l.line, startCol)
		return nil
	}

	l.advance()
	l.addTokenAt(GT, ">", l.line, startCol)
	return nil
}

// scanLessThan handles <, <<, <<<, and <=.
func (l *Lexer) scanLessThan() error {
	startCol := l.col
	next := l.peekNext()

	if next == '<' {
		next2 := l.peekAhead(2)
		if next2 == '<' {
			// Here-string <<<
			l.advance()
			l.advance()
			l.advance()
			l.addTokenAt(HERESTRING, "<<<", l.line, startCol)
		} else {
			// Heredoc <<
			l.advance()
			l.advance()
			l.addTokenAt(HEREDOC, "<<", l.line, startCol)
		}
		return nil
	}

	if next == '=' {
		l.advance()
		l.advance()
		l.addTokenAt(LE, "<=", l.line, startCol)
		return nil
	}

	l.advance()
	l.addTokenAt(LT, "<", l.line, startCol)
	return nil
}

// scanEquals handles =, ==, and =~.
func (l *Lexer) scanEquals() error {
	startCol := l.col
	next := l.peekNext()

	if next == '~' {
		l.advance()
		l.advance()
		l.addTokenAt(MATCH, "=~", l.line, startCol)
		return nil
	}

	if next == '=' {
		l.advance()
		l.advance()
		l.addTokenAt(EQ, "==", l.line, startCol)
		return nil
	}

	l.advance()
	l.addTokenAt(EQUALS, "=", l.line, startCol)
	return nil
}

// scanColon handles :=, ::, :x (block param), and bare colon.
func (l *Lexer) scanColon() error {
	startCol := l.col
	next := l.peekNext()

	if next == '=' {
		l.advance()
		l.advance()
		l.addTokenAt(ASSIGN, ":=", l.line, startCol)
		return nil
	}

	if next == ':' {
		// Namespace separator ::
		l.advance()
		l.advance()
		l.addTokenAt(NAMESPACE_SEP, "::", l.line, startCol)
		return nil
	}

	if isAlpha(next) {
		// Block parameter like :x or :each
		l.advance() // skip the colon
		paramCol := l.col
		var paramName strings.Builder
		for !l.isAtEnd() && isAlphaNumeric(l.peek()) {
			paramName.WriteByte(l.advance())
		}
		l.addTokenAt(BLOCKPARAM, paramName.String(), l.line, paramCol-1)
		return nil
	}

	// Bare colon - error token
	l.advance()
	l.addTokenAt(ERROR, ":", l.line, startCol)
	return nil
}

// scanSingleQuotedString handles 'string' and '''triple-quoted''' strings.
func (l *Lexer) scanSingleQuotedString() error {
	startCol := l.col
	startLine := l.line

	// Check for triple-quoted string '''...'''
	if l.pos+2 < len(l.input) && l.input[l.pos:l.pos+3] == "'''" {
		l.advance() // skip first '
		l.advance() // skip second '
		l.advance() // skip third '

		var str strings.Builder
		for !l.isAtEnd() {
			if l.pos+2 < len(l.input) && l.input[l.pos:l.pos+3] == "'''" {
				// Found closing delimiter
				l.advance()
				l.advance()
				l.advance()
				break
			}
			c := l.peek()
			if c == '\n' {
				str.WriteByte(c)
				l.advance()
				l.line++
				l.col = 0
			} else {
				str.WriteByte(l.advance())
			}
		}
		l.addTokenAt(TRIPLESTRING, str.String(), startLine, startCol)
		return nil
	}

	// Regular single-quoted string
	var str strings.Builder
	str.WriteByte(l.advance()) // consume opening quote

	for !l.isAtEnd() && l.peek() != '\'' {
		c := l.peek()
		if c == '\n' {
			str.WriteByte(c)
			l.advance()
			l.line++
			l.col = 0
		} else {
			str.WriteByte(l.advance())
		}
	}

	// Consume closing quote if present
	if !l.isAtEnd() {
		str.WriteByte(l.advance())
	}

	l.addTokenAt(STRING, str.String(), startLine, startCol)
	return nil
}

// scanDoubleQuotedString handles "string" with nested subshells.
func (l *Lexer) scanDoubleQuotedString() error {
	startCol := l.col
	startLine := l.line

	var dstr strings.Builder
	dstr.WriteByte(l.advance()) // consume opening quote

	subshellDepth := 0
	for !l.isAtEnd() {
		c := l.peek()
		next := l.peekNext()

		// Check for subshell start: $(
		if c == '$' && next == '(' {
			dstr.WriteString("$(")
			l.advance()
			l.advance()
			subshellDepth++
			continue
		}

		// Check for subshell end: )
		if c == ')' && subshellDepth > 0 {
			dstr.WriteByte(l.advance())
			subshellDepth--
			continue
		}

		// If not in a subshell and we hit a quote, end the string
		if c == '"' && subshellDepth == 0 {
			break
		}

		if c == '\n' {
			dstr.WriteByte(c)
			l.advance()
			l.line++
			l.col = 0
		} else {
			dstr.WriteByte(l.advance())
		}
	}

	// Consume closing quote if present
	if !l.isAtEnd() {
		dstr.WriteByte(l.advance())
	}

	l.addTokenAt(DSTRING, dstr.String(), startLine, startCol)
	return nil
}

// scanNumber handles integer and floating-point numbers.
func (l *Lexer) scanNumber() error {
	startCol := l.col

	var num strings.Builder
	for !l.isAtEnd() && isDigit(l.peek()) {
		num.WriteByte(l.advance())
	}

	// Check for decimal point followed by digit (true floating point)
	if !l.isAtEnd() && l.peek() == '.' && isDigit(l.peekNext()) {
		num.WriteByte(l.advance()) // consume the dot
		for !l.isAtEnd() && isDigit(l.peek()) {
			num.WriteByte(l.advance())
		}
	}

	l.addTokenAt(NUMBER, num.String(), l.line, startCol)
	return nil
}

// scanNegativeNumber handles negative numbers (minus followed by digit).
func (l *Lexer) scanNegativeNumber() error {
	startCol := l.col

	var num strings.Builder
	num.WriteByte(l.advance()) // consume the minus

	for !l.isAtEnd() && isDigit(l.peek()) {
		num.WriteByte(l.advance())
	}

	// Check for decimal point followed by digit
	if !l.isAtEnd() && l.peek() == '.' && isDigit(l.peekNext()) {
		num.WriteByte(l.advance()) // consume the dot
		for !l.isAtEnd() && isDigit(l.peek()) {
			num.WriteByte(l.advance())
		}
	}

	l.addTokenAt(NUMBER, num.String(), l.line, startCol)
	return nil
}

// scanDollar handles $var, ${...}, $(...), $((...)), and special variables.
func (l *Lexer) scanDollar() error {
	startCol := l.col

	// Check for arithmetic $((...))
	if l.peekNext() == '(' && l.peekAhead(2) == '(' {
		var arith strings.Builder
		arith.WriteString("$((")
		l.advance() // $
		l.advance() // (
		l.advance() // (

		parenDepth := 2
		for !l.isAtEnd() && parenDepth > 0 {
			c := l.peek()
			arith.WriteByte(l.advance())
			if c == '(' {
				parenDepth++
			} else if c == ')' {
				parenDepth--
			}
		}
		l.addTokenAt(ARITHMETIC, arith.String(), l.line, startCol)
		return nil
	}

	// Check for subshell $(...)
	if l.peekNext() == '(' {
		var sub strings.Builder
		sub.WriteString("$(")
		l.advance() // $
		l.advance() // (

		parenDepth := 1
		for !l.isAtEnd() && parenDepth > 0 {
			c := l.peek()
			sub.WriteByte(l.advance())
			if c == '(' {
				parenDepth++
			} else if c == ')' {
				parenDepth--
			}
		}
		l.addTokenAt(SUBSHELL, sub.String(), l.line, startCol)
		return nil
	}

	// Check for parameter expansion ${...}
	if l.peekNext() == '{' {
		var v strings.Builder
		v.WriteString("${")
		l.advance() // $
		l.advance() // {

		braceDepth := 1
		for !l.isAtEnd() && braceDepth > 0 {
			c := l.peek()
			v.WriteByte(l.advance())
			if c == '{' {
				braceDepth++
			} else if c == '}' {
				braceDepth--
			}
		}
		l.addTokenAt(VARIABLE, v.String(), l.line, startCol)
		return nil
	}

	// Check for special variables: $!, $?, $$, $@, $*, $#, $-
	next := l.peekNext()
	if next == '!' || next == '?' || next == '$' || next == '@' || next == '*' || next == '#' || next == '-' {
		l.advance() // $
		l.advance() // special char
		l.addTokenAt(VARIABLE, "$"+string(next), l.line, startCol)
		return nil
	}

	// Simple variable like $var or $1
	var v strings.Builder
	v.WriteByte(l.advance()) // $
	for !l.isAtEnd() && isAlphaNumeric(l.peek()) {
		v.WriteByte(l.advance())
	}
	l.addTokenAt(VARIABLE, v.String(), l.line, startCol)
	return nil
}

// scanLeftParen handles ( and (( arithmetic command.
func (l *Lexer) scanLeftParen() error {
	startCol := l.col

	// Check for (( arithmetic )) - bash arithmetic command (no $ prefix)
	if l.peekNext() == '(' {
		var arith strings.Builder
		arith.WriteString("((")
		l.advance() // (
		l.advance() // (

		parenDepth := 2
		for !l.isAtEnd() && parenDepth > 0 {
			c := l.peek()
			arith.WriteByte(l.advance())
			if c == '(' {
				parenDepth++
			} else if c == ')' {
				parenDepth--
			}
		}
		l.addTokenAt(ARITH_CMD, arith.String(), l.line, startCol)
		return nil
	}

	l.advance()
	l.addTokenAt(LPAREN, "(", l.line, startCol)
	return nil
}

// scanSlash handles / for paths or division.
func (l *Lexer) scanSlash() error {
	startCol := l.col
	next := l.peekNext()

	// Check if this is an absolute path (e.g., /dev/null, /tmp/file)
	if isAlphaNumeric(next) {
		var path strings.Builder
		for !l.isAtEnd() {
			c := l.peek()
			// Path can contain: alphanumeric, underscore, slash, dot, dash
			if isAlphaNumeric(c) || c == '/' || c == '.' || c == '-' {
				path.WriteByte(l.advance())
			} else {
				break
			}
		}
		l.addTokenAt(PATH, path.String(), l.line, startCol)
		return nil
	}

	l.advance()
	l.addTokenAt(SLASH, "/", l.line, startCol)
	return nil
}

// scanIdentifierOrKeyword handles identifiers and keywords.
func (l *Lexer) scanIdentifierOrKeyword() error {
	startCol := l.col

	var word strings.Builder
	for !l.isAtEnd() && isAlphaNumeric(l.peek()) {
		word.WriteByte(l.advance())
	}

	// Check if followed by colon (making it a keyword)
	// But NOT := (assignment) or :: (namespace separator)
	if !l.isAtEnd() && l.peek() == ':' && l.peekNext() != '=' && l.peekNext() != ':' {
		word.WriteByte(l.advance()) // consume the colon

		// Check if immediately followed by a number (no whitespace) for varspec default
		// e.g., value:42 should be a single token
		if !l.isAtEnd() && isDigit(l.peek()) {
			for !l.isAtEnd() && isDigit(l.peek()) {
				word.WriteByte(l.advance())
			}
		}
		l.addTokenAt(KEYWORD, word.String(), l.line, startCol)
		return nil
	}

	l.addTokenAt(IDENTIFIER, word.String(), l.line, startCol)
	return nil
}

// String returns a string representation of the lexer state (for debugging).
func (l *Lexer) String() string {
	return fmt.Sprintf("Lexer{pos=%d, line=%d, col=%d, tokens=%d}",
		l.pos, l.line, l.col, len(l.tokens))
}
