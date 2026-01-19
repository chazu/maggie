package compiler

import (
	"testing"
)

func TestLexerBasicTokens(t *testing.T) {
	input := `( ) [ ] { } ^ . ; := : |`
	expected := []struct {
		typ TokenType
		lit string
	}{
		{TokenLParen, "("},
		{TokenRParen, ")"},
		{TokenLBracket, "["},
		{TokenRBracket, "]"},
		{TokenLBrace, "{"},
		{TokenRBrace, "}"},
		{TokenCaret, "^"},
		{TokenPeriod, "."},
		{TokenSemicolon, ";"},
		{TokenAssign, ":="},
		{TokenColon, ":"},
		{TokenBar, "|"},
		{TokenEOF, ""},
	}

	l := NewLexer(input)
	for i, exp := range expected {
		tok := l.NextToken()
		if tok.Type != exp.typ {
			t.Errorf("token[%d] type = %v, want %v", i, tok.Type, exp.typ)
		}
		if tok.Literal != exp.lit {
			t.Errorf("token[%d] literal = %q, want %q", i, tok.Literal, exp.lit)
		}
	}
}

func TestLexerIntegers(t *testing.T) {
	tests := []struct {
		input string
		want  string
	}{
		{"42", "42"},
		{"0", "0"},
		{"-123", "-123"},
		{"16rFF", "16rFF"},
		{"2r1010", "2r1010"},
		{"8r777", "8r777"},
	}

	for _, tc := range tests {
		l := NewLexer(tc.input)
		tok := l.NextToken()
		if tok.Type != TokenInteger {
			t.Errorf("Lexer(%q): type = %v, want INTEGER", tc.input, tok.Type)
		}
		if tok.Literal != tc.want {
			t.Errorf("Lexer(%q): literal = %q, want %q", tc.input, tok.Literal, tc.want)
		}
	}
}

func TestLexerFloats(t *testing.T) {
	tests := []struct {
		input string
		want  string
	}{
		{"3.14", "3.14"},
		{"0.5", "0.5"},
		{"-2.5", "-2.5"},
		{"1e10", "1e10"},
		{"1.5e-3", "1.5e-3"},
		{"2.0E+5", "2.0E+5"},
	}

	for _, tc := range tests {
		l := NewLexer(tc.input)
		tok := l.NextToken()
		if tok.Type != TokenFloat {
			t.Errorf("Lexer(%q): type = %v, want FLOAT", tc.input, tok.Type)
		}
		if tok.Literal != tc.want {
			t.Errorf("Lexer(%q): literal = %q, want %q", tc.input, tok.Literal, tc.want)
		}
	}
}

func TestLexerStrings(t *testing.T) {
	tests := []struct {
		input string
		want  string
	}{
		{"'hello'", "hello"},
		{"'hello world'", "hello world"},
		{"''", ""},
		{"'it''s'", "it's"}, // escaped quote
		{"'line1\nline2'", "line1\nline2"},
	}

	for _, tc := range tests {
		l := NewLexer(tc.input)
		tok := l.NextToken()
		if tok.Type != TokenString {
			t.Errorf("Lexer(%q): type = %v, want STRING", tc.input, tok.Type)
		}
		if tok.Literal != tc.want {
			t.Errorf("Lexer(%q): literal = %q, want %q", tc.input, tok.Literal, tc.want)
		}
	}
}

func TestLexerSymbols(t *testing.T) {
	tests := []struct {
		input string
		want  string
	}{
		{"#foo", "foo"},
		{"#FooBar", "FooBar"},
		{"#at:", "at:"},
		{"#at:put:", "at:put:"},
		{"#+", "+"},
		{"#--", "--"},
		{"#<=", "<="},
		{"#'hello world'", "hello world"},
	}

	for _, tc := range tests {
		l := NewLexer(tc.input)
		tok := l.NextToken()
		if tok.Type != TokenSymbol {
			t.Errorf("Lexer(%q): type = %v, want SYMBOL", tc.input, tok.Type)
		}
		if tok.Literal != tc.want {
			t.Errorf("Lexer(%q): literal = %q, want %q", tc.input, tok.Literal, tc.want)
		}
	}
}

func TestLexerCharacters(t *testing.T) {
	tests := []struct {
		input string
		want  string
	}{
		{"$a", "a"},
		{"$Z", "Z"},
		{"$0", "0"},
		{"$ ", " "},
	}

	for _, tc := range tests {
		l := NewLexer(tc.input)
		tok := l.NextToken()
		if tok.Type != TokenCharacter {
			t.Errorf("Lexer(%q): type = %v, want CHARACTER", tc.input, tok.Type)
		}
		if tok.Literal != tc.want {
			t.Errorf("Lexer(%q): literal = %q, want %q", tc.input, tok.Literal, tc.want)
		}
	}
}

func TestLexerIdentifiers(t *testing.T) {
	tests := []struct {
		input string
		typ   TokenType
		lit   string
	}{
		{"foo", TokenIdentifier, "foo"},
		{"FooBar", TokenIdentifier, "FooBar"},
		{"foo123", TokenIdentifier, "foo123"},
		{"_private", TokenIdentifier, "_private"},
		{"self", TokenSelf, "self"},
		{"super", TokenSuper, "super"},
		{"nil", TokenNil, "nil"},
		{"true", TokenTrue, "true"},
		{"false", TokenFalse, "false"},
		{"thisContext", TokenThisContext, "thisContext"},
	}

	for _, tc := range tests {
		l := NewLexer(tc.input)
		tok := l.NextToken()
		if tok.Type != tc.typ {
			t.Errorf("Lexer(%q): type = %v, want %v", tc.input, tok.Type, tc.typ)
		}
		if tok.Literal != tc.lit {
			t.Errorf("Lexer(%q): literal = %q, want %q", tc.input, tok.Literal, tc.lit)
		}
	}
}

func TestLexerKeywords(t *testing.T) {
	tests := []struct {
		input string
		want  string
	}{
		{"at:", "at:"},
		{"put:", "put:"},
		{"ifTrue:", "ifTrue:"},
		{"ifTrue:ifFalse:", "ifTrue:"}, // Only reads first keyword
	}

	for _, tc := range tests {
		l := NewLexer(tc.input)
		tok := l.NextToken()
		if tok.Type != TokenKeyword {
			t.Errorf("Lexer(%q): type = %v, want KEYWORD", tc.input, tok.Type)
		}
		if tok.Literal != tc.want {
			t.Errorf("Lexer(%q): literal = %q, want %q", tc.input, tok.Literal, tc.want)
		}
	}
}

func TestLexerBinarySelectors(t *testing.T) {
	tests := []struct {
		input string
		want  string
	}{
		{"+", "+"},
		{"-", "-"},
		{"*", "*"},
		{"/", "/"},
		{"<", "<"},
		{">", ">"},
		{"<=", "<="},
		{">=", ">="},
		{"=", "="},
		{"~=", "~="},
		{"==", "=="},
		{"~~", "~~"},
		{"@", "@"},
		{"->", "->"},
		{"\\\\", "\\\\"},
	}

	for _, tc := range tests {
		l := NewLexer(tc.input)
		tok := l.NextToken()
		if tok.Type != TokenBinarySelector {
			t.Errorf("Lexer(%q): type = %v, want BINARY", tc.input, tok.Type)
		}
		if tok.Literal != tc.want {
			t.Errorf("Lexer(%q): literal = %q, want %q", tc.input, tok.Literal, tc.want)
		}
	}
}

func TestLexerComments(t *testing.T) {
	input := `foo "this is a comment" bar`
	l := NewLexer(input)

	tok := l.NextToken()
	if tok.Type != TokenIdentifier || tok.Literal != "foo" {
		t.Errorf("expected foo identifier, got %v", tok)
	}

	tok = l.NextToken()
	if tok.Type != TokenIdentifier || tok.Literal != "bar" {
		t.Errorf("expected bar identifier, got %v", tok)
	}
}

func TestLexerHashComments(t *testing.T) {
	tests := []struct {
		name   string
		input  string
		tokens []struct {
			typ TokenType
			lit string
		}
	}{
		{
			name:  "hash comment at start of line",
			input: "# this is a comment\nfoo",
			tokens: []struct {
				typ TokenType
				lit string
			}{
				{TokenIdentifier, "foo"},
				{TokenEOF, ""},
			},
		},
		{
			name:  "hash comment after code",
			input: "foo # this is a comment\nbar",
			tokens: []struct {
				typ TokenType
				lit string
			}{
				{TokenIdentifier, "foo"},
				{TokenIdentifier, "bar"},
				{TokenEOF, ""},
			},
		},
		{
			name:  "hash symbol not treated as comment",
			input: "#symbol",
			tokens: []struct {
				typ TokenType
				lit string
			}{
				{TokenSymbol, "symbol"},
				{TokenEOF, ""},
			},
		},
		{
			name:  "hash array not treated as comment",
			input: "#(1 2)",
			tokens: []struct {
				typ TokenType
				lit string
			}{
				{TokenHashLParen, "#("},
				{TokenInteger, "1"},
				{TokenInteger, "2"},
				{TokenRParen, ")"},
				{TokenEOF, ""},
			},
		},
		{
			name:  "multiple hash comments",
			input: "# comment 1\nfoo\n# comment 2\nbar",
			tokens: []struct {
				typ TokenType
				lit string
			}{
				{TokenIdentifier, "foo"},
				{TokenIdentifier, "bar"},
				{TokenEOF, ""},
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			l := NewLexer(tc.input)
			for i, exp := range tc.tokens {
				tok := l.NextToken()
				if tok.Type != exp.typ {
					t.Errorf("token[%d] type = %v, want %v", i, tok.Type, exp.typ)
				}
				if tok.Literal != exp.lit {
					t.Errorf("token[%d] literal = %q, want %q", i, tok.Literal, exp.lit)
				}
			}
		})
	}
}

func TestLexerHashParen(t *testing.T) {
	input := `#(1 2 3)`
	l := NewLexer(input)

	tok := l.NextToken()
	if tok.Type != TokenHashLParen {
		t.Errorf("expected #(, got %v", tok)
	}

	tok = l.NextToken()
	if tok.Type != TokenInteger || tok.Literal != "1" {
		t.Errorf("expected 1, got %v", tok)
	}
}

func TestLexerMethodSignature(t *testing.T) {
	input := `at: index put: value`
	expected := []struct {
		typ TokenType
		lit string
	}{
		{TokenKeyword, "at:"},
		{TokenIdentifier, "index"},
		{TokenKeyword, "put:"},
		{TokenIdentifier, "value"},
		{TokenEOF, ""},
	}

	l := NewLexer(input)
	for i, exp := range expected {
		tok := l.NextToken()
		if tok.Type != exp.typ {
			t.Errorf("token[%d] type = %v, want %v", i, tok.Type, exp.typ)
		}
		if tok.Literal != exp.lit {
			t.Errorf("token[%d] literal = %q, want %q", i, tok.Literal, exp.lit)
		}
	}
}

func TestLexerCompleteMethod(t *testing.T) {
	input := `increment: amount
    | result |
    result := value + amount.
    ^result`

	expected := []struct {
		typ TokenType
		lit string
	}{
		{TokenKeyword, "increment:"},
		{TokenIdentifier, "amount"},
		{TokenBar, "|"},
		{TokenIdentifier, "result"},
		{TokenBar, "|"},
		{TokenIdentifier, "result"},
		{TokenAssign, ":="},
		{TokenIdentifier, "value"},
		{TokenBinarySelector, "+"},
		{TokenIdentifier, "amount"},
		{TokenPeriod, "."},
		{TokenCaret, "^"},
		{TokenIdentifier, "result"},
		{TokenEOF, ""},
	}

	l := NewLexer(input)
	for i, exp := range expected {
		tok := l.NextToken()
		if tok.Type != exp.typ {
			t.Errorf("token[%d] type = %v, want %v", i, tok.Type, exp.typ)
		}
		if tok.Literal != exp.lit {
			t.Errorf("token[%d] literal = %q, want %q", i, tok.Literal, exp.lit)
		}
	}
}

func TestLexerBlock(t *testing.T) {
	input := `[:x :y | x + y]`
	expected := []struct {
		typ TokenType
		lit string
	}{
		{TokenLBracket, "["},
		{TokenColon, ":"},
		{TokenIdentifier, "x"},
		{TokenColon, ":"},
		{TokenIdentifier, "y"},
		{TokenBar, "|"},
		{TokenIdentifier, "x"},
		{TokenBinarySelector, "+"},
		{TokenIdentifier, "y"},
		{TokenRBracket, "]"},
		{TokenEOF, ""},
	}

	l := NewLexer(input)
	for i, exp := range expected {
		tok := l.NextToken()
		if tok.Type != exp.typ {
			t.Errorf("token[%d] type = %v, want %v", i, tok.Type, exp.typ)
		}
		if tok.Literal != exp.lit {
			t.Errorf("token[%d] literal = %q, want %q", i, tok.Literal, exp.lit)
		}
	}
}

func TestLexerCascade(t *testing.T) {
	input := `obj msg1; msg2; msg3`
	expected := []struct {
		typ TokenType
		lit string
	}{
		{TokenIdentifier, "obj"},
		{TokenIdentifier, "msg1"},
		{TokenSemicolon, ";"},
		{TokenIdentifier, "msg2"},
		{TokenSemicolon, ";"},
		{TokenIdentifier, "msg3"},
		{TokenEOF, ""},
	}

	l := NewLexer(input)
	for i, exp := range expected {
		tok := l.NextToken()
		if tok.Type != exp.typ {
			t.Errorf("token[%d] type = %v, want %v", i, tok.Type, exp.typ)
		}
		if tok.Literal != exp.lit {
			t.Errorf("token[%d] literal = %q, want %q", i, tok.Literal, exp.lit)
		}
	}
}

func TestLexerLineTracking(t *testing.T) {
	input := "foo\nbar\nbaz"
	l := NewLexer(input)

	tok := l.NextToken()
	if tok.Pos.Line != 1 {
		t.Errorf("foo should be on line 1, got %d", tok.Pos.Line)
	}

	tok = l.NextToken()
	if tok.Pos.Line != 2 {
		t.Errorf("bar should be on line 2, got %d", tok.Pos.Line)
	}

	tok = l.NextToken()
	if tok.Pos.Line != 3 {
		t.Errorf("baz should be on line 3, got %d", tok.Pos.Line)
	}
}

func TestTokenize(t *testing.T) {
	input := "x := 42"
	tokens := Tokenize(input)

	if len(tokens) != 4 { // x, :=, 42, EOF
		t.Errorf("expected 4 tokens, got %d", len(tokens))
	}

	if tokens[0].Type != TokenIdentifier {
		t.Errorf("token[0] should be identifier")
	}
	if tokens[1].Type != TokenAssign {
		t.Errorf("token[1] should be assign")
	}
	if tokens[2].Type != TokenInteger {
		t.Errorf("token[2] should be integer")
	}
	if tokens[3].Type != TokenEOF {
		t.Errorf("token[3] should be EOF")
	}
}
