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

func TestLexerDocstrings(t *testing.T) {
	tests := []struct {
		name    string
		input   string
		wantTyp TokenType
		wantLit string
	}{
		{
			name:    "basic docstring",
			input:   `"""Hello world"""`,
			wantTyp: TokenDocstring,
			wantLit: "Hello world",
		},
		{
			name:    "empty docstring",
			input:   `""""""`,
			wantTyp: TokenDocstring,
			wantLit: "",
		},
		{
			name: "multiline docstring",
			input: `"""
This is a docstring.
It has multiple lines.
"""`,
			wantTyp: TokenDocstring,
			wantLit: "This is a docstring.\nIt has multiple lines.",
		},
		{
			name: "docstring with embedded single quote",
			input: `"""Contains a " quote"""`,
			wantTyp: TokenDocstring,
			wantLit: `Contains a " quote`,
		},
		{
			name: "docstring with embedded double quote",
			input: `"""Contains "" two quotes"""`,
			wantTyp: TokenDocstring,
			wantLit: `Contains "" two quotes`,
		},
		{
			name: "docstring with fenced blocks",
			input: "\"\"\"Docs here.\n\n```test\n3 + 4 >>> 7\n```\n\"\"\"",
			wantTyp: TokenDocstring,
			wantLit: "Docs here.\n\n```test\n3 + 4 >>> 7\n```",
		},
		{
			name:    "unterminated docstring",
			input:   `"""no closing`,
			wantTyp: TokenError,
			wantLit: "unterminated docstring",
		},
		{
			name: "docstring strips common indent",
			input: "  \"\"\"\n  Hello\n  World\n  \"\"\"",
			wantTyp: TokenDocstring,
			wantLit: "Hello\nWorld",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			l := NewLexer(tc.input)
			tok := l.NextToken()
			if tok.Type != tc.wantTyp {
				t.Errorf("type = %v, want %v", tok.Type, tc.wantTyp)
			}
			if tok.Literal != tc.wantLit {
				t.Errorf("literal = %q, want %q", tok.Literal, tc.wantLit)
			}
		})
	}
}

func TestLexerDocstringDoesNotBreakComments(t *testing.T) {
	// Regular comments should still work
	input := `"this is a comment" foo`
	l := NewLexer(input)
	tok := l.NextToken()
	if tok.Type != TokenIdentifier || tok.Literal != "foo" {
		t.Errorf("expected identifier 'foo' after comment, got %s(%q)", tok.Type, tok.Literal)
	}

	// Empty comment
	input2 := `"" foo`
	l2 := NewLexer(input2)
	tok2 := l2.NextToken()
	if tok2.Type != TokenIdentifier || tok2.Literal != "foo" {
		t.Errorf("expected identifier 'foo' after empty comment, got %s(%q)", tok2.Type, tok2.Literal)
	}
}

func TestLexerDocstringFollowedByIdentifier(t *testing.T) {
	input := `"""A docstring""" MyClass`
	l := NewLexer(input)

	tok1 := l.NextToken()
	if tok1.Type != TokenDocstring || tok1.Literal != "A docstring" {
		t.Errorf("token[0] = %s(%q), want DOCSTRING(\"A docstring\")", tok1.Type, tok1.Literal)
	}

	tok2 := l.NextToken()
	if tok2.Type != TokenIdentifier || tok2.Literal != "MyClass" {
		t.Errorf("token[1] = %s(%q), want IDENTIFIER(\"MyClass\")", tok2.Type, tok2.Literal)
	}
}

// ---------------------------------------------------------------------------
// FQN (fully-qualified name) lexer tests
// ---------------------------------------------------------------------------

func TestLexerFQN_SimpleNamespace(t *testing.T) {
	input := "Widgets::Button"
	l := NewLexer(input)
	tok := l.NextToken()
	if tok.Type != TokenIdentifier {
		t.Errorf("type = %v, want IDENTIFIER", tok.Type)
	}
	if tok.Literal != "Widgets::Button" {
		t.Errorf("literal = %q, want %q", tok.Literal, "Widgets::Button")
	}
	tok2 := l.NextToken()
	if tok2.Type != TokenEOF {
		t.Errorf("expected EOF after FQN, got %v(%q)", tok2.Type, tok2.Literal)
	}
}

func TestLexerFQN_MultiLevel(t *testing.T) {
	input := "A::B::C"
	l := NewLexer(input)
	tok := l.NextToken()
	if tok.Type != TokenIdentifier {
		t.Errorf("type = %v, want IDENTIFIER", tok.Type)
	}
	if tok.Literal != "A::B::C" {
		t.Errorf("literal = %q, want %q", tok.Literal, "A::B::C")
	}
}

func TestLexerFQN_FollowedByUnaryMessage(t *testing.T) {
	input := "Widgets::Button new"
	l := NewLexer(input)

	tok1 := l.NextToken()
	if tok1.Type != TokenIdentifier || tok1.Literal != "Widgets::Button" {
		t.Errorf("token[0] = %s(%q), want IDENTIFIER(\"Widgets::Button\")", tok1.Type, tok1.Literal)
	}

	tok2 := l.NextToken()
	if tok2.Type != TokenIdentifier || tok2.Literal != "new" {
		t.Errorf("token[1] = %s(%q), want IDENTIFIER(\"new\")", tok2.Type, tok2.Literal)
	}
}

func TestLexerFQN_FollowedByKeywordMessage(t *testing.T) {
	input := "Widgets::Button label: 'OK'"
	l := NewLexer(input)

	tok1 := l.NextToken()
	if tok1.Type != TokenIdentifier || tok1.Literal != "Widgets::Button" {
		t.Errorf("token[0] = %s(%q), want IDENTIFIER(\"Widgets::Button\")", tok1.Type, tok1.Literal)
	}

	tok2 := l.NextToken()
	if tok2.Type != TokenKeyword || tok2.Literal != "label:" {
		t.Errorf("token[1] = %s(%q), want KEYWORD(\"label:\")", tok2.Type, tok2.Literal)
	}

	tok3 := l.NextToken()
	if tok3.Type != TokenString || tok3.Literal != "OK" {
		t.Errorf("token[2] = %s(%q), want STRING(\"OK\")", tok3.Type, tok3.Literal)
	}
}

func TestLexerFQN_NotConfusedWithKeyword(t *testing.T) {
	// A regular keyword like "foo:" should NOT be affected
	input := "foo: bar"
	l := NewLexer(input)

	tok1 := l.NextToken()
	if tok1.Type != TokenKeyword || tok1.Literal != "foo:" {
		t.Errorf("token[0] = %s(%q), want KEYWORD(\"foo:\")", tok1.Type, tok1.Literal)
	}

	tok2 := l.NextToken()
	if tok2.Type != TokenIdentifier || tok2.Literal != "bar" {
		t.Errorf("token[1] = %s(%q), want IDENTIFIER(\"bar\")", tok2.Type, tok2.Literal)
	}
}

func TestLexerFQN_AssignContext(t *testing.T) {
	// x := Widgets::Button new
	input := "x := Widgets::Button new"
	expected := []struct {
		typ TokenType
		lit string
	}{
		{TokenIdentifier, "x"},
		{TokenAssign, ":="},
		{TokenIdentifier, "Widgets::Button"},
		{TokenIdentifier, "new"},
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

func TestLexerFQN_InMethodBody(t *testing.T) {
	// ^Widgets::Button new
	input := "^Widgets::Button new"
	expected := []struct {
		typ TokenType
		lit string
	}{
		{TokenCaret, "^"},
		{TokenIdentifier, "Widgets::Button"},
		{TokenIdentifier, "new"},
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
