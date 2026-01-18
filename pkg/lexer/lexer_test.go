// Package lexer provides tokenization for the Trashtalk language.
package lexer

import (
	"encoding/json"
	"io"
	"strings"
	"testing"
)

// TestTokenize_BasicTokens tests tokenization of basic single-character tokens.
func TestTokenize_BasicTokens(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "empty input",
			input: "",
			expected: []Token{},
		},
		{
			name:  "single bracket left",
			input: "[",
			expected: []Token{
				{Type: LBRACKET, Value: "[", Line: 1, Column: 0},
			},
		},
		{
			name:  "single bracket right",
			input: "]",
			expected: []Token{
				{Type: RBRACKET, Value: "]", Line: 1, Column: 0},
			},
		},
		{
			name:  "double brackets left",
			input: "[[",
			expected: []Token{
				{Type: DLBRACKET, Value: "[[", Line: 1, Column: 0},
			},
		},
		{
			name:  "double brackets right",
			input: "]]",
			expected: []Token{
				{Type: DRBRACKET, Value: "]]", Line: 1, Column: 0},
			},
		},
		{
			name:  "pipe",
			input: "|",
			expected: []Token{
				{Type: PIPE, Value: "|", Line: 1, Column: 0},
			},
		},
		{
			name:  "or operator",
			input: "||",
			expected: []Token{
				{Type: OR, Value: "||", Line: 1, Column: 0},
			},
		},
		{
			name:  "caret",
			input: "^",
			expected: []Token{
				{Type: CARET, Value: "^", Line: 1, Column: 0},
			},
		},
		{
			name:  "at sign",
			input: "@",
			expected: []Token{
				{Type: AT, Value: "@", Line: 1, Column: 0},
			},
		},
		{
			name:  "dot",
			input: ".",
			expected: []Token{
				{Type: DOT, Value: ".", Line: 1, Column: 0},
			},
		},
		{
			name:  "semicolon",
			input: ";",
			expected: []Token{
				{Type: SEMI, Value: ";", Line: 1, Column: 0},
			},
		},
		{
			name:  "ampersand",
			input: "&",
			expected: []Token{
				{Type: AMP, Value: "&", Line: 1, Column: 0},
			},
		},
		{
			name:  "and operator",
			input: "&&",
			expected: []Token{
				{Type: AND, Value: "&&", Line: 1, Column: 0},
			},
		},
		{
			name:  "left paren",
			input: "(",
			expected: []Token{
				{Type: LPAREN, Value: "(", Line: 1, Column: 0},
			},
		},
		{
			name:  "right paren",
			input: ")",
			expected: []Token{
				{Type: RPAREN, Value: ")", Line: 1, Column: 0},
			},
		},
		{
			name:  "left brace",
			input: "{",
			expected: []Token{
				{Type: LBRACE, Value: "{", Line: 1, Column: 0},
			},
		},
		{
			name:  "right brace",
			input: "}",
			expected: []Token{
				{Type: RBRACE, Value: "}", Line: 1, Column: 0},
			},
		},
		{
			name:  "question mark",
			input: "?",
			expected: []Token{
				{Type: QUESTION, Value: "?", Line: 1, Column: 0},
			},
		},
		{
			name:  "plus",
			input: "+",
			expected: []Token{
				{Type: PLUS, Value: "+", Line: 1, Column: 0},
			},
		},
		{
			name:  "star",
			input: "*",
			expected: []Token{
				{Type: STAR, Value: "*", Line: 1, Column: 0},
			},
		},
		{
			name:  "comma",
			input: ",",
			expected: []Token{
				{Type: COMMA, Value: ",", Line: 1, Column: 0},
			},
		},
		{
			name:  "tilde",
			input: "~",
			expected: []Token{
				{Type: TILDE, Value: "~", Line: 1, Column: 0},
			},
		},
		{
			name:  "percent",
			input: "%",
			expected: []Token{
				{Type: PERCENT, Value: "%", Line: 1, Column: 0},
			},
		},
		{
			name:  "backslash",
			input: "\\",
			expected: []Token{
				{Type: BACKSLASH, Value: "\\\\", Line: 1, Column: 0},
			},
		},
		{
			name:  "slash alone",
			input: "/ ",
			expected: []Token{
				{Type: SLASH, Value: "/", Line: 1, Column: 0},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := New(tt.input)
			tokens, err := lexer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenize() error = %v", err)
			}
			// Filter out whitespace tokens for comparison
			filtered := filterNonWhitespace(tokens)
			compareTokens(t, tt.expected, filtered)
		})
	}
}

// TestTokenize_Operators tests tokenization of comparison and assignment operators.
func TestTokenize_Operators(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "assignment :=",
			input: ":=",
			expected: []Token{
				{Type: ASSIGN, Value: ":=", Line: 1, Column: 0},
			},
		},
		{
			name:  "equals =",
			input: "=",
			expected: []Token{
				{Type: EQUALS, Value: "=", Line: 1, Column: 0},
			},
		},
		{
			name:  "equality ==",
			input: "==",
			expected: []Token{
				{Type: EQ, Value: "==", Line: 1, Column: 0},
			},
		},
		{
			name:  "not equal !=",
			input: "!=",
			expected: []Token{
				{Type: NE, Value: "!=", Line: 1, Column: 0},
			},
		},
		{
			name:  "regex match =~",
			input: "=~",
			expected: []Token{
				{Type: MATCH, Value: "=~", Line: 1, Column: 0},
			},
		},
		{
			name:  "greater than >",
			input: ">",
			expected: []Token{
				{Type: GT, Value: ">", Line: 1, Column: 0},
			},
		},
		{
			name:  "greater or equal >=",
			input: ">=",
			expected: []Token{
				{Type: GE, Value: ">=", Line: 1, Column: 0},
			},
		},
		{
			name:  "less than <",
			input: "<",
			expected: []Token{
				{Type: LT, Value: "<", Line: 1, Column: 0},
			},
		},
		{
			name:  "less or equal <=",
			input: "<=",
			expected: []Token{
				{Type: LE, Value: "<=", Line: 1, Column: 0},
			},
		},
		{
			name:  "string not equal ~=",
			input: "~=",
			expected: []Token{
				{Type: STR_NE, Value: "~=", Line: 1, Column: 0},
			},
		},
		{
			name:  "bang !",
			input: "!",
			expected: []Token{
				{Type: BANG, Value: "!", Line: 1, Column: 0},
			},
		},
		{
			name:  "namespace separator ::",
			input: "::",
			expected: []Token{
				{Type: NAMESPACE_SEP, Value: "::", Line: 1, Column: 0},
			},
		},
		{
			name:  "minus alone",
			input: "- ",
			expected: []Token{
				{Type: MINUS, Value: "-", Line: 1, Column: 0},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := New(tt.input)
			tokens, err := lexer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenize() error = %v", err)
			}
			filtered := filterNonWhitespace(tokens)
			compareTokens(t, tt.expected, filtered)
		})
	}
}

// TestTokenize_Redirects tests tokenization of bash redirect operators.
func TestTokenize_Redirects(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "append >>",
			input: ">>",
			expected: []Token{
				{Type: REDIRECT, Value: ">>", Line: 1, Column: 0},
			},
		},
		{
			name:  "redirect stdout to fd >&",
			input: ">&",
			expected: []Token{
				{Type: REDIRECT, Value: ">&", Line: 1, Column: 0},
			},
		},
		{
			name:  "redirect both &>",
			input: "&>",
			expected: []Token{
				{Type: REDIRECT, Value: "&>", Line: 1, Column: 0},
			},
		},
		{
			name:  "append both &>>",
			input: "&>>",
			expected: []Token{
				{Type: REDIRECT, Value: "&>>", Line: 1, Column: 0},
			},
		},
		{
			name:  "heredoc <<",
			input: "<<",
			expected: []Token{
				{Type: HEREDOC, Value: "<<", Line: 1, Column: 0},
			},
		},
		{
			name:  "herestring <<<",
			input: "<<<",
			expected: []Token{
				{Type: HERESTRING, Value: "<<<", Line: 1, Column: 0},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := New(tt.input)
			tokens, err := lexer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenize() error = %v", err)
			}
			filtered := filterNonWhitespace(tokens)
			compareTokens(t, tt.expected, filtered)
		})
	}
}

// TestTokenize_Identifiers tests tokenization of identifiers.
func TestTokenize_Identifiers(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "simple identifier",
			input: "Counter",
			expected: []Token{
				{Type: IDENTIFIER, Value: "Counter", Line: 1, Column: 0},
			},
		},
		{
			name:  "lowercase identifier",
			input: "myVar",
			expected: []Token{
				{Type: IDENTIFIER, Value: "myVar", Line: 1, Column: 0},
			},
		},
		{
			name:  "underscore prefix",
			input: "_private",
			expected: []Token{
				{Type: IDENTIFIER, Value: "_private", Line: 1, Column: 0},
			},
		},
		{
			name:  "with numbers",
			input: "var123",
			expected: []Token{
				{Type: IDENTIFIER, Value: "var123", Line: 1, Column: 0},
			},
		},
		{
			name:  "underscore in middle",
			input: "my_var",
			expected: []Token{
				{Type: IDENTIFIER, Value: "my_var", Line: 1, Column: 0},
			},
		},
		{
			name:  "single char",
			input: "x",
			expected: []Token{
				{Type: IDENTIFIER, Value: "x", Line: 1, Column: 0},
			},
		},
		{
			name:  "multiple identifiers",
			input: "foo bar baz",
			expected: []Token{
				{Type: IDENTIFIER, Value: "foo", Line: 1, Column: 0},
				{Type: IDENTIFIER, Value: "bar", Line: 1, Column: 4},
				{Type: IDENTIFIER, Value: "baz", Line: 1, Column: 8},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := New(tt.input)
			tokens, err := lexer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenize() error = %v", err)
			}
			filtered := filterNonWhitespace(tokens)
			compareTokens(t, tt.expected, filtered)
		})
	}
}

// TestTokenize_Keywords tests tokenization of keywords (identifiers ending with colon).
func TestTokenize_Keywords(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "simple keyword",
			input: "method:",
			expected: []Token{
				{Type: KEYWORD, Value: "method:", Line: 1, Column: 0},
			},
		},
		{
			name:  "subclass keyword",
			input: "subclass:",
			expected: []Token{
				{Type: KEYWORD, Value: "subclass:", Line: 1, Column: 0},
			},
		},
		{
			name:  "instanceVars keyword",
			input: "instanceVars:",
			expected: []Token{
				{Type: KEYWORD, Value: "instanceVars:", Line: 1, Column: 0},
			},
		},
		{
			name:  "keyword with default value",
			input: "value:42",
			expected: []Token{
				{Type: KEYWORD, Value: "value:42", Line: 1, Column: 0},
			},
		},
		{
			name:  "keyword with zero default",
			input: "count:0",
			expected: []Token{
				{Type: KEYWORD, Value: "count:0", Line: 1, Column: 0},
			},
		},
		{
			name:  "multiple keywords",
			input: "at: x put: y",
			expected: []Token{
				{Type: KEYWORD, Value: "at:", Line: 1, Column: 0},
				{Type: IDENTIFIER, Value: "x", Line: 1, Column: 4},
				{Type: KEYWORD, Value: "put:", Line: 1, Column: 6},
				{Type: IDENTIFIER, Value: "y", Line: 1, Column: 11},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := New(tt.input)
			tokens, err := lexer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenize() error = %v", err)
			}
			filtered := filterNonWhitespace(tokens)
			compareTokens(t, tt.expected, filtered)
		})
	}
}

// TestTokenize_Numbers tests tokenization of numeric literals.
func TestTokenize_Numbers(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "single digit",
			input: "5",
			expected: []Token{
				{Type: NUMBER, Value: "5", Line: 1, Column: 0},
			},
		},
		{
			name:  "multi digit",
			input: "42",
			expected: []Token{
				{Type: NUMBER, Value: "42", Line: 1, Column: 0},
			},
		},
		{
			name:  "large number",
			input: "1234567890",
			expected: []Token{
				{Type: NUMBER, Value: "1234567890", Line: 1, Column: 0},
			},
		},
		{
			name:  "zero",
			input: "0",
			expected: []Token{
				{Type: NUMBER, Value: "0", Line: 1, Column: 0},
			},
		},
		{
			name:  "negative number",
			input: "-42",
			expected: []Token{
				{Type: NUMBER, Value: "-42", Line: 1, Column: 0},
			},
		},
		{
			name:  "negative zero",
			input: "-0",
			expected: []Token{
				{Type: NUMBER, Value: "-0", Line: 1, Column: 0},
			},
		},
		{
			name:  "floating point",
			input: "3.14",
			expected: []Token{
				{Type: NUMBER, Value: "3.14", Line: 1, Column: 0},
			},
		},
		{
			name:  "negative floating point",
			input: "-3.14",
			expected: []Token{
				{Type: NUMBER, Value: "-3.14", Line: 1, Column: 0},
			},
		},
		{
			name:  "number followed by dot space",
			input: "42. ",
			expected: []Token{
				{Type: NUMBER, Value: "42", Line: 1, Column: 0},
				{Type: DOT, Value: ".", Line: 1, Column: 2},
			},
		},
		{
			name:  "multiple numbers",
			input: "1 2 3",
			expected: []Token{
				{Type: NUMBER, Value: "1", Line: 1, Column: 0},
				{Type: NUMBER, Value: "2", Line: 1, Column: 2},
				{Type: NUMBER, Value: "3", Line: 1, Column: 4},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := New(tt.input)
			tokens, err := lexer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenize() error = %v", err)
			}
			filtered := filterNonWhitespace(tokens)
			compareTokens(t, tt.expected, filtered)
		})
	}
}

// TestTokenize_SingleQuotedStrings tests tokenization of single-quoted strings.
func TestTokenize_SingleQuotedStrings(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "empty string",
			input: "''",
			expected: []Token{
				{Type: STRING, Value: "''", Line: 1, Column: 0},
			},
		},
		{
			name:  "simple string",
			input: "'hello'",
			expected: []Token{
				{Type: STRING, Value: "'hello'", Line: 1, Column: 0},
			},
		},
		{
			name:  "string with spaces",
			input: "'hello world'",
			expected: []Token{
				{Type: STRING, Value: "'hello world'", Line: 1, Column: 0},
			},
		},
		{
			name:  "string with special chars",
			input: "'hello@world!'",
			expected: []Token{
				{Type: STRING, Value: "'hello@world!'", Line: 1, Column: 0},
			},
		},
		{
			name:  "string with dollar sign (not interpolated)",
			input: "'hello $name'",
			expected: []Token{
				{Type: STRING, Value: "'hello $name'", Line: 1, Column: 0},
			},
		},
		{
			name:  "multiple strings",
			input: "'foo' 'bar'",
			expected: []Token{
				{Type: STRING, Value: "'foo'", Line: 1, Column: 0},
				{Type: STRING, Value: "'bar'", Line: 1, Column: 6},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := New(tt.input)
			tokens, err := lexer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenize() error = %v", err)
			}
			filtered := filterNonWhitespace(tokens)
			compareTokens(t, tt.expected, filtered)
		})
	}
}

// TestTokenize_TripleQuotedStrings tests tokenization of triple-quoted strings.
func TestTokenize_TripleQuotedStrings(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "empty triple string",
			input: "''''''",
			expected: []Token{
				{Type: TRIPLESTRING, Value: "", Line: 1, Column: 0},
			},
		},
		{
			name:  "simple triple string",
			input: "'''hello'''",
			expected: []Token{
				{Type: TRIPLESTRING, Value: "hello", Line: 1, Column: 0},
			},
		},
		{
			name:  "triple string with single quotes inside",
			input: "'''it's fine'''",
			expected: []Token{
				{Type: TRIPLESTRING, Value: "it's fine", Line: 1, Column: 0},
			},
		},
		{
			name:  "triple string with two quotes inside",
			input: "'''say ''hi'' '''",
			expected: []Token{
				{Type: TRIPLESTRING, Value: "say ''hi'' ", Line: 1, Column: 0},
			},
		},
		{
			name:  "multiline triple string",
			input: "'''line1\nline2\nline3'''",
			expected: []Token{
				{Type: TRIPLESTRING, Value: "line1\nline2\nline3", Line: 1, Column: 0},
			},
		},
		{
			name:  "triple string with heredoc-like content",
			input: "'''#!/bin/bash\necho hello\n'''",
			expected: []Token{
				{Type: TRIPLESTRING, Value: "#!/bin/bash\necho hello\n", Line: 1, Column: 0},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := New(tt.input)
			tokens, err := lexer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenize() error = %v", err)
			}
			filtered := filterNonWhitespace(tokens)
			compareTokens(t, tt.expected, filtered)
		})
	}
}

// TestTokenize_DoubleQuotedStrings tests tokenization of double-quoted strings.
func TestTokenize_DoubleQuotedStrings(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "empty dstring",
			input: `""`,
			expected: []Token{
				{Type: DSTRING, Value: `""`, Line: 1, Column: 0},
			},
		},
		{
			name:  "simple dstring",
			input: `"hello"`,
			expected: []Token{
				{Type: DSTRING, Value: `"hello"`, Line: 1, Column: 0},
			},
		},
		{
			name:  "dstring with variable",
			input: `"hello $name"`,
			expected: []Token{
				{Type: DSTRING, Value: `"hello $name"`, Line: 1, Column: 0},
			},
		},
		{
			name:  "dstring with subshell",
			input: `"result: $(echo hi)"`,
			expected: []Token{
				{Type: DSTRING, Value: `"result: $(echo hi)"`, Line: 1, Column: 0},
			},
		},
		{
			name:  "dstring with nested subshells",
			input: `"outer $(inner $(deepest))"`,
			expected: []Token{
				{Type: DSTRING, Value: `"outer $(inner $(deepest))"`, Line: 1, Column: 0},
			},
		},
		{
			name:  "dstring with arithmetic",
			input: `"value: $((1+2))"`,
			expected: []Token{
				{Type: DSTRING, Value: `"value: $((1+2))"`, Line: 1, Column: 0},
			},
		},
		{
			name:  "dstring with parameter expansion",
			input: `"path: ${HOME}/file"`,
			expected: []Token{
				{Type: DSTRING, Value: `"path: ${HOME}/file"`, Line: 1, Column: 0},
			},
		},
		{
			name:  "multiline dstring",
			input: "\"line1\nline2\"",
			expected: []Token{
				{Type: DSTRING, Value: "\"line1\nline2\"", Line: 1, Column: 0},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := New(tt.input)
			tokens, err := lexer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenize() error = %v", err)
			}
			filtered := filterNonWhitespace(tokens)
			compareTokens(t, tt.expected, filtered)
		})
	}
}

// TestTokenize_BlockParameters tests tokenization of block parameters like :x, :each.
func TestTokenize_BlockParameters(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "single letter block param",
			input: ":x",
			expected: []Token{
				{Type: BLOCKPARAM, Value: "x", Line: 1, Column: 0},
			},
		},
		{
			name:  "word block param",
			input: ":each",
			expected: []Token{
				{Type: BLOCKPARAM, Value: "each", Line: 1, Column: 0},
			},
		},
		{
			name:  "block param with numbers",
			input: ":item1",
			expected: []Token{
				{Type: BLOCKPARAM, Value: "item1", Line: 1, Column: 0},
			},
		},
		{
			name:  "multiple block params",
			input: ":key :value",
			expected: []Token{
				{Type: BLOCKPARAM, Value: "key", Line: 1, Column: 0},
				{Type: BLOCKPARAM, Value: "value", Line: 1, Column: 5},
			},
		},
		{
			name:  "block param in context",
			input: "[:x | x + 1]",
			expected: []Token{
				{Type: LBRACKET, Value: "[", Line: 1, Column: 0},
				{Type: BLOCKPARAM, Value: "x", Line: 1, Column: 1},
				{Type: PIPE, Value: "|", Line: 1, Column: 4},
				{Type: IDENTIFIER, Value: "x", Line: 1, Column: 6},
				{Type: PLUS, Value: "+", Line: 1, Column: 8},
				{Type: NUMBER, Value: "1", Line: 1, Column: 10},
				{Type: RBRACKET, Value: "]", Line: 1, Column: 11},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := New(tt.input)
			tokens, err := lexer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenize() error = %v", err)
			}
			filtered := filterNonWhitespace(tokens)
			compareTokens(t, tt.expected, filtered)
		})
	}
}

// TestTokenize_ShellConstructs tests tokenization of shell-specific constructs.
func TestTokenize_ShellConstructs(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "simple variable",
			input: "$var",
			expected: []Token{
				{Type: VARIABLE, Value: "$var", Line: 1, Column: 0},
			},
		},
		{
			name:  "positional parameter",
			input: "$1",
			expected: []Token{
				{Type: VARIABLE, Value: "$1", Line: 1, Column: 0},
			},
		},
		{
			name:  "special variable $?",
			input: "$?",
			expected: []Token{
				{Type: VARIABLE, Value: "$?", Line: 1, Column: 0},
			},
		},
		{
			name:  "special variable $!",
			input: "$!",
			expected: []Token{
				{Type: VARIABLE, Value: "$!", Line: 1, Column: 0},
			},
		},
		{
			name:  "special variable $$",
			input: "$$",
			expected: []Token{
				{Type: VARIABLE, Value: "$$", Line: 1, Column: 0},
			},
		},
		{
			name:  "special variable $@",
			input: "$@",
			expected: []Token{
				{Type: VARIABLE, Value: "$@", Line: 1, Column: 0},
			},
		},
		{
			name:  "special variable $*",
			input: "$*",
			expected: []Token{
				{Type: VARIABLE, Value: "$*", Line: 1, Column: 0},
			},
		},
		{
			name:  "special variable $#",
			input: "$#",
			expected: []Token{
				{Type: VARIABLE, Value: "$#", Line: 1, Column: 0},
			},
		},
		{
			name:  "special variable $-",
			input: "$-",
			expected: []Token{
				{Type: VARIABLE, Value: "$-", Line: 1, Column: 0},
			},
		},
		{
			name:  "parameter expansion simple",
			input: "${var}",
			expected: []Token{
				{Type: VARIABLE, Value: "${var}", Line: 1, Column: 0},
			},
		},
		{
			name:  "parameter expansion with default",
			input: "${var:-default}",
			expected: []Token{
				{Type: VARIABLE, Value: "${var:-default}", Line: 1, Column: 0},
			},
		},
		{
			name:  "parameter expansion with substitution",
			input: "${var/old/new}",
			expected: []Token{
				{Type: VARIABLE, Value: "${var/old/new}", Line: 1, Column: 0},
			},
		},
		{
			name:  "subshell",
			input: "$(echo hello)",
			expected: []Token{
				{Type: SUBSHELL, Value: "$(echo hello)", Line: 1, Column: 0},
			},
		},
		{
			name:  "nested subshell",
			input: "$(outer $(inner))",
			expected: []Token{
				{Type: SUBSHELL, Value: "$(outer $(inner))", Line: 1, Column: 0},
			},
		},
		{
			name:  "arithmetic expansion",
			input: "$((1+2))",
			expected: []Token{
				{Type: ARITHMETIC, Value: "$((1+2))", Line: 1, Column: 0},
			},
		},
		{
			name:  "complex arithmetic",
			input: "$((a * b + c / d))",
			expected: []Token{
				{Type: ARITHMETIC, Value: "$((a * b + c / d))", Line: 1, Column: 0},
			},
		},
		{
			name:  "arithmetic command (no $)",
			input: "((i++))",
			expected: []Token{
				{Type: ARITH_CMD, Value: "((i++))", Line: 1, Column: 0},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := New(tt.input)
			tokens, err := lexer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenize() error = %v", err)
			}
			filtered := filterNonWhitespace(tokens)
			compareTokens(t, tt.expected, filtered)
		})
	}
}

// TestTokenize_HashConstructs tests tokenization of # constructs.
func TestTokenize_HashConstructs(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "comment",
			input: "# this is a comment",
			expected: []Token{
				{Type: COMMENT, Value: "# this is a comment", Line: 1, Column: 0},
			},
		},
		{
			name:  "symbol",
			input: "#mySymbol",
			expected: []Token{
				{Type: SYMBOL, Value: "mySymbol", Line: 1, Column: 0},
			},
		},
		{
			name:  "array literal start",
			input: "#(",
			expected: []Token{
				{Type: HASHlparen, Value: "#(", Line: 1, Column: 0},
			},
		},
		{
			name:  "dict literal start",
			input: "#{",
			expected: []Token{
				{Type: HASHLBRACE, Value: "#{", Line: 1, Column: 0},
			},
		},
		{
			name:  "array literal with content",
			input: "#(1 2 3)",
			expected: []Token{
				{Type: HASHlparen, Value: "#(", Line: 1, Column: 0},
				{Type: NUMBER, Value: "1", Line: 1, Column: 2},
				{Type: NUMBER, Value: "2", Line: 1, Column: 4},
				{Type: NUMBER, Value: "3", Line: 1, Column: 6},
				{Type: RPAREN, Value: ")", Line: 1, Column: 7},
			},
		},
		{
			name:  "dict literal with content",
			input: "#{ a: 1 }",
			expected: []Token{
				{Type: HASHLBRACE, Value: "#{", Line: 1, Column: 0},
				{Type: KEYWORD, Value: "a:", Line: 1, Column: 3},
				{Type: NUMBER, Value: "1", Line: 1, Column: 6},
				{Type: RBRACE, Value: "}", Line: 1, Column: 8},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := New(tt.input)
			tokens, err := lexer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenize() error = %v", err)
			}
			filtered := filterNonWhitespace(tokens)
			compareTokens(t, tt.expected, filtered)
		})
	}
}

// TestTokenize_Paths tests tokenization of file paths.
func TestTokenize_Paths(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "dev null",
			input: "/dev/null",
			expected: []Token{
				{Type: PATH, Value: "/dev/null", Line: 1, Column: 0},
			},
		},
		{
			name:  "tmp file",
			input: "/tmp/file.txt",
			expected: []Token{
				{Type: PATH, Value: "/tmp/file.txt", Line: 1, Column: 0},
			},
		},
		{
			name:  "path with dashes",
			input: "/usr/local/my-app",
			expected: []Token{
				{Type: PATH, Value: "/usr/local/my-app", Line: 1, Column: 0},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := New(tt.input)
			tokens, err := lexer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenize() error = %v", err)
			}
			filtered := filterNonWhitespace(tokens)
			compareTokens(t, tt.expected, filtered)
		})
	}
}

// TestTokenize_Newlines tests newline handling and line tracking.
func TestTokenize_Newlines(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "single newline",
			input: "\n",
			expected: []Token{
				{Type: NEWLINE, Value: "\\n", Line: 1, Column: 0},
			},
		},
		{
			name:  "multiple newlines",
			input: "\n\n\n",
			expected: []Token{
				{Type: NEWLINE, Value: "\\n", Line: 1, Column: 0},
				{Type: NEWLINE, Value: "\\n", Line: 2, Column: 0},
				{Type: NEWLINE, Value: "\\n", Line: 3, Column: 0},
			},
		},
		{
			name:  "identifier then newline then identifier",
			input: "foo\nbar",
			expected: []Token{
				{Type: IDENTIFIER, Value: "foo", Line: 1, Column: 0},
				{Type: NEWLINE, Value: "\\n", Line: 1, Column: 3},
				{Type: IDENTIFIER, Value: "bar", Line: 2, Column: 0},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := New(tt.input)
			tokens, err := lexer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenize() error = %v", err)
			}
			compareTokens(t, tt.expected, tokens)
		})
	}
}

// TestTokenize_LineColumnTracking tests accurate line and column tracking.
func TestTokenize_LineColumnTracking(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "simple line tracking",
			input: "a b c",
			expected: []Token{
				{Type: IDENTIFIER, Value: "a", Line: 1, Column: 0},
				{Type: IDENTIFIER, Value: "b", Line: 1, Column: 2},
				{Type: IDENTIFIER, Value: "c", Line: 1, Column: 4},
			},
		},
		{
			name:  "multiline tracking",
			input: "a\nb\nc",
			expected: []Token{
				{Type: IDENTIFIER, Value: "a", Line: 1, Column: 0},
				{Type: NEWLINE, Value: "\\n", Line: 1, Column: 1},
				{Type: IDENTIFIER, Value: "b", Line: 2, Column: 0},
				{Type: NEWLINE, Value: "\\n", Line: 2, Column: 1},
				{Type: IDENTIFIER, Value: "c", Line: 3, Column: 0},
			},
		},
		{
			name:  "with indentation",
			input: "  foo",
			expected: []Token{
				{Type: IDENTIFIER, Value: "foo", Line: 1, Column: 2},
			},
		},
		{
			name:  "tabs",
			input: "\tfoo",
			expected: []Token{
				{Type: IDENTIFIER, Value: "foo", Line: 1, Column: 1},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := New(tt.input)
			tokens, err := lexer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenize() error = %v", err)
			}
			compareTokens(t, tt.expected, tokens)
		})
	}
}

// TestTokenize_NamespaceSeparator tests the :: namespace separator.
func TestTokenize_NamespaceSeparator(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "namespace separator alone",
			input: "::",
			expected: []Token{
				{Type: NAMESPACE_SEP, Value: "::", Line: 1, Column: 0},
			},
		},
		{
			name:  "qualified class name",
			input: "MyApp::Counter",
			expected: []Token{
				{Type: IDENTIFIER, Value: "MyApp", Line: 1, Column: 0},
				{Type: NAMESPACE_SEP, Value: "::", Line: 1, Column: 5},
				{Type: IDENTIFIER, Value: "Counter", Line: 1, Column: 7},
			},
		},
		{
			name:  "message to namespaced class",
			input: "@ MyApp::Counter new",
			expected: []Token{
				{Type: AT, Value: "@", Line: 1, Column: 0},
				{Type: IDENTIFIER, Value: "MyApp", Line: 1, Column: 2},
				{Type: NAMESPACE_SEP, Value: "::", Line: 1, Column: 7},
				{Type: IDENTIFIER, Value: "Counter", Line: 1, Column: 9},
				{Type: IDENTIFIER, Value: "new", Line: 1, Column: 17},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := New(tt.input)
			tokens, err := lexer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenize() error = %v", err)
			}
			filtered := filterNonWhitespace(tokens)
			compareTokens(t, tt.expected, filtered)
		})
	}
}

// TestTokenize_FullPrograms tests tokenization of complete Trashtalk programs.
func TestTokenize_FullPrograms(t *testing.T) {
	tests := []struct {
		name          string
		input         string
		expectedTypes []TokenType
	}{
		{
			name: "class definition",
			input: `Counter subclass: Object
  instanceVars: value:0`,
			expectedTypes: []TokenType{
				IDENTIFIER, KEYWORD, IDENTIFIER,
				KEYWORD, KEYWORD,
			},
		},
		{
			name: "method definition",
			input: `method: increment [
    value := value + 1
  ]`,
			expectedTypes: []TokenType{
				KEYWORD, IDENTIFIER, LBRACKET,
				IDENTIFIER, ASSIGN, IDENTIFIER, PLUS, NUMBER,
				RBRACKET,
			},
		},
		{
			name:  "message send with block",
			input: "array do: [:each | @ self process: each]",
			expectedTypes: []TokenType{
				IDENTIFIER, KEYWORD, LBRACKET, BLOCKPARAM, PIPE,
				AT, IDENTIFIER, KEYWORD, IDENTIFIER, RBRACKET,
			},
		},
		{
			name:  "return statement",
			input: "^ value",
			expectedTypes: []TokenType{
				CARET, IDENTIFIER,
			},
		},
		{
			name:  "local variables",
			input: "| temp result |",
			expectedTypes: []TokenType{
				PIPE, IDENTIFIER, IDENTIFIER, PIPE,
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := New(tt.input)
			tokens, err := lexer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenize() error = %v", err)
			}
			filtered := filterNonWhitespace(tokens)
			if len(filtered) != len(tt.expectedTypes) {
				t.Errorf("token count mismatch: got %d, expected %d", len(filtered), len(tt.expectedTypes))
				for i, tok := range filtered {
					t.Logf("  [%d] %s: %q", i, tok.Type, tok.Value)
				}
				return
			}
			for i, expectedType := range tt.expectedTypes {
				if filtered[i].Type != expectedType {
					t.Errorf("token[%d] type = %s, expected %s (value=%q)",
						i, filtered[i].Type, expectedType, filtered[i].Value)
				}
			}
		})
	}
}

// TestTokenize_EdgeCases tests edge cases and error handling.
func TestTokenize_EdgeCases(t *testing.T) {
	tests := []struct {
		name          string
		input         string
		expectedTypes []TokenType
		expectedVals  []string
	}{
		{
			name:          "bare colon is error",
			input:         ": ",
			expectedTypes: []TokenType{ERROR},
			expectedVals:  []string{":"},
		},
		{
			name:          "unterminated single string",
			input:         "'hello",
			expectedTypes: []TokenType{STRING},
			expectedVals:  []string{"'hello"},
		},
		{
			name:          "unterminated double string",
			input:         `"hello`,
			expectedTypes: []TokenType{DSTRING},
			expectedVals:  []string{`"hello`},
		},
		{
			name:          "unknown character",
			input:         "`",
			expectedTypes: []TokenType{LITERAL},
			expectedVals:  []string{"`"},
		},
		{
			name:          "mixed whitespace",
			input:         "a   \t  b",
			expectedTypes: []TokenType{IDENTIFIER, IDENTIFIER},
			expectedVals:  []string{"a", "b"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := New(tt.input)
			tokens, err := lexer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenize() error = %v", err)
			}
			filtered := filterNonWhitespace(tokens)
			if len(filtered) != len(tt.expectedTypes) {
				t.Errorf("token count mismatch: got %d, expected %d", len(filtered), len(tt.expectedTypes))
				for i, tok := range filtered {
					t.Logf("  [%d] %s: %q", i, tok.Type, tok.Value)
				}
				return
			}
			for i := range tt.expectedTypes {
				if filtered[i].Type != tt.expectedTypes[i] {
					t.Errorf("token[%d] type = %s, expected %s", i, filtered[i].Type, tt.expectedTypes[i])
				}
				if filtered[i].Value != tt.expectedVals[i] {
					t.Errorf("token[%d] value = %q, expected %q", i, filtered[i].Value, tt.expectedVals[i])
				}
			}
		})
	}
}

// TestTokenizeJSON tests the JSON output format.
func TestTokenizeJSON(t *testing.T) {
	tests := []struct {
		name  string
		input string
	}{
		{
			name:  "simple tokens",
			input: "foo bar",
		},
		{
			name:  "with operators",
			input: "x := 1 + 2",
		},
		{
			name:  "with strings",
			input: `'hello' "world"`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := New(tt.input)
			jsonStr, err := lexer.TokenizeJSON()
			if err != nil {
				t.Fatalf("TokenizeJSON() error = %v", err)
			}

			// Verify it's valid JSON
			var tokens []map[string]interface{}
			if err := json.Unmarshal([]byte(jsonStr), &tokens); err != nil {
				t.Fatalf("TokenizeJSON() produced invalid JSON: %v", err)
			}

			// Verify structure of each token
			for i, tok := range tokens {
				if _, ok := tok["type"]; !ok {
					t.Errorf("token[%d] missing 'type' field", i)
				}
				if _, ok := tok["value"]; !ok {
					t.Errorf("token[%d] missing 'value' field", i)
				}
				if _, ok := tok["line"]; !ok {
					t.Errorf("token[%d] missing 'line' field", i)
				}
				if _, ok := tok["col"]; !ok {
					t.Errorf("token[%d] missing 'col' field", i)
				}
			}
		})
	}
}

// TestNewFromReader tests creating a lexer from an io.Reader.
func TestNewFromReader(t *testing.T) {
	tests := []struct {
		name          string
		input         string
		expectedTypes []TokenType
	}{
		{
			name:          "simple input",
			input:         "hello world",
			expectedTypes: []TokenType{IDENTIFIER, IDENTIFIER},
		},
		{
			name:          "empty input",
			input:         "",
			expectedTypes: []TokenType{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			reader := strings.NewReader(tt.input)
			lexer, err := NewFromReader(reader)
			if err != nil {
				t.Fatalf("NewFromReader() error = %v", err)
			}
			tokens, err := lexer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenize() error = %v", err)
			}
			filtered := filterNonWhitespace(tokens)
			if len(filtered) != len(tt.expectedTypes) {
				t.Errorf("token count mismatch: got %d, expected %d", len(filtered), len(tt.expectedTypes))
				return
			}
			for i, expectedType := range tt.expectedTypes {
				if filtered[i].Type != expectedType {
					t.Errorf("token[%d] type = %s, expected %s", i, filtered[i].Type, expectedType)
				}
			}
		})
	}
}

// TestNewFromReader_Error tests error handling when reader fails.
func TestNewFromReader_Error(t *testing.T) {
	reader := &errorReader{}
	_, err := NewFromReader(reader)
	if err == nil {
		t.Error("NewFromReader() expected error, got nil")
	}
}

// TestLexer_String tests the String() method for debugging.
func TestLexer_String(t *testing.T) {
	lexer := New("test input")
	str := lexer.String()
	if str == "" {
		t.Error("String() returned empty string")
	}
	// Should contain pos, line, col info
	if !strings.Contains(str, "Lexer{") {
		t.Errorf("String() = %q, expected to contain 'Lexer{'", str)
	}
}

// TestToken_IsKeyword tests the IsKeyword helper method.
func TestToken_IsKeyword(t *testing.T) {
	tests := []struct {
		token    Token
		expected bool
	}{
		{Token{Type: KEYWORD, Value: "method:"}, true},
		{Token{Type: IDENTIFIER, Value: "method"}, false},
		{Token{Type: STRING, Value: "'method:'"}, false},
	}

	for _, tt := range tests {
		t.Run(string(tt.token.Type), func(t *testing.T) {
			if got := tt.token.IsKeyword(); got != tt.expected {
				t.Errorf("IsKeyword() = %v, expected %v", got, tt.expected)
			}
		})
	}
}

// TestToken_IsIdentifier tests the IsIdentifier helper method.
func TestToken_IsIdentifier(t *testing.T) {
	tests := []struct {
		token    Token
		expected bool
	}{
		{Token{Type: IDENTIFIER, Value: "foo"}, true},
		{Token{Type: KEYWORD, Value: "foo:"}, false},
		{Token{Type: STRING, Value: "'foo'"}, false},
	}

	for _, tt := range tests {
		t.Run(string(tt.token.Type), func(t *testing.T) {
			if got := tt.token.IsIdentifier(); got != tt.expected {
				t.Errorf("IsIdentifier() = %v, expected %v", got, tt.expected)
			}
		})
	}
}

// TestToken_IsOperator tests the IsOperator helper method.
func TestToken_IsOperator(t *testing.T) {
	operatorTypes := []TokenType{
		PIPE, OR, AND, CARET, AT, ASSIGN, EQUALS, EQ, NE, MATCH,
		GT, GE, LT, LE, STR_NE, PLUS, MINUS, STAR, SLASH, PERCENT,
		BANG, NAMESPACE_SEP,
	}
	nonOperatorTypes := []TokenType{
		IDENTIFIER, KEYWORD, STRING, NUMBER, LBRACKET, RBRACKET,
	}

	for _, typ := range operatorTypes {
		t.Run(string(typ)+"_is_operator", func(t *testing.T) {
			tok := Token{Type: typ, Value: "x"}
			if !tok.IsOperator() {
				t.Errorf("IsOperator() = false for %s, expected true", typ)
			}
		})
	}

	for _, typ := range nonOperatorTypes {
		t.Run(string(typ)+"_is_not_operator", func(t *testing.T) {
			tok := Token{Type: typ, Value: "x"}
			if tok.IsOperator() {
				t.Errorf("IsOperator() = true for %s, expected false", typ)
			}
		})
	}
}

// TestToken_IsLiteral tests the IsLiteral helper method.
func TestToken_IsLiteral(t *testing.T) {
	literalTypes := []TokenType{
		STRING, TRIPLESTRING, DSTRING, NUMBER, SYMBOL,
	}
	nonLiteralTypes := []TokenType{
		IDENTIFIER, KEYWORD, PIPE, LBRACKET, RBRACKET,
	}

	for _, typ := range literalTypes {
		t.Run(string(typ)+"_is_literal", func(t *testing.T) {
			tok := Token{Type: typ, Value: "x"}
			if !tok.IsLiteral() {
				t.Errorf("IsLiteral() = false for %s, expected true", typ)
			}
		})
	}

	for _, typ := range nonLiteralTypes {
		t.Run(string(typ)+"_is_not_literal", func(t *testing.T) {
			tok := Token{Type: typ, Value: "x"}
			if tok.IsLiteral() {
				t.Errorf("IsLiteral() = true for %s, expected false", typ)
			}
		})
	}
}

// Helper functions

// filterNonWhitespace removes NEWLINE tokens for easier comparison in most tests.
func filterNonWhitespace(tokens []Token) []Token {
	result := make([]Token, 0, len(tokens))
	for _, tok := range tokens {
		if tok.Type != NEWLINE {
			result = append(result, tok)
		}
	}
	return result
}

// compareTokens compares two token slices and reports differences.
func compareTokens(t *testing.T, expected, actual []Token) {
	t.Helper()
	if len(expected) != len(actual) {
		t.Errorf("token count mismatch: got %d, expected %d", len(actual), len(expected))
		for i, tok := range actual {
			t.Logf("  actual[%d] = %s: %q (line=%d, col=%d)", i, tok.Type, tok.Value, tok.Line, tok.Column)
		}
		for i, tok := range expected {
			t.Logf("  expected[%d] = %s: %q (line=%d, col=%d)", i, tok.Type, tok.Value, tok.Line, tok.Column)
		}
		return
	}
	for i := range expected {
		if actual[i].Type != expected[i].Type {
			t.Errorf("token[%d] type = %s, expected %s", i, actual[i].Type, expected[i].Type)
		}
		if actual[i].Value != expected[i].Value {
			t.Errorf("token[%d] value = %q, expected %q", i, actual[i].Value, expected[i].Value)
		}
		if actual[i].Line != expected[i].Line {
			t.Errorf("token[%d] line = %d, expected %d", i, actual[i].Line, expected[i].Line)
		}
		if actual[i].Column != expected[i].Column {
			t.Errorf("token[%d] column = %d, expected %d", i, actual[i].Column, expected[i].Column)
		}
	}
}

// errorReader is a reader that always returns an error.
type errorReader struct{}

func (r *errorReader) Read(p []byte) (n int, err error) {
	return 0, io.ErrUnexpectedEOF
}
