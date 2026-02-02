package compiler

import "fmt"

// ---------------------------------------------------------------------------
// Token types for Smalltalk lexer
// ---------------------------------------------------------------------------

// TokenType represents the type of a token.
type TokenType int

const (
	// Special tokens
	TokenEOF TokenType = iota
	TokenError

	// Literals
	TokenInteger    // 42, 16rFF, 2r1010
	TokenFloat      // 3.14, 1.5e10
	TokenString     // 'hello'
	TokenDocstring  // """docstring"""
	TokenSymbol     // #foo, #'hello world', #+
	TokenCharacter  // $a, $\n
	TokenIdentifier // foo, Bar

	// Keywords and selectors
	TokenKeyword       // foo:, at:put:
	TokenBinarySelector // +, -, *, /, <, >, =, @, etc.

	// Delimiters
	TokenLParen       // (
	TokenRParen       // )
	TokenLBracket     // [
	TokenRBracket     // ]
	TokenLBrace       // {
	TokenRBrace       // }
	TokenHash         // #
	TokenHashLParen   // #(
	TokenCaret        // ^
	TokenPeriod       // .
	TokenSemicolon    // ;
	TokenAssign       // :=
	TokenColon        // :
	TokenBar          // |

	// Reserved identifiers
	TokenSelf
	TokenSuper
	TokenNil
	TokenTrue
	TokenFalse
	TokenThisContext
)

var tokenNames = map[TokenType]string{
	TokenEOF:           "EOF",
	TokenError:         "ERROR",
	TokenInteger:       "INTEGER",
	TokenFloat:         "FLOAT",
	TokenString:        "STRING",
	TokenDocstring:     "DOCSTRING",
	TokenSymbol:        "SYMBOL",
	TokenCharacter:     "CHARACTER",
	TokenIdentifier:    "IDENTIFIER",
	TokenKeyword:       "KEYWORD",
	TokenBinarySelector: "BINARY",
	TokenLParen:        "(",
	TokenRParen:        ")",
	TokenLBracket:      "[",
	TokenRBracket:      "]",
	TokenLBrace:        "{",
	TokenRBrace:        "}",
	TokenHash:          "#",
	TokenHashLParen:    "#(",
	TokenCaret:         "^",
	TokenPeriod:        ".",
	TokenSemicolon:     ";",
	TokenAssign:        ":=",
	TokenColon:         ":",
	TokenBar:           "|",
	TokenSelf:          "self",
	TokenSuper:         "super",
	TokenNil:           "nil",
	TokenTrue:          "true",
	TokenFalse:         "false",
	TokenThisContext:   "thisContext",
}

func (t TokenType) String() string {
	if name, ok := tokenNames[t]; ok {
		return name
	}
	return fmt.Sprintf("Token(%d)", t)
}

// Token represents a lexical token.
type Token struct {
	Type    TokenType
	Literal string   // the raw text
	Pos     Position // start position
}

func (t Token) String() string {
	if t.Type == TokenEOF {
		return "EOF"
	}
	if t.Type == TokenError {
		return fmt.Sprintf("ERROR(%s)", t.Literal)
	}
	if len(t.Literal) > 20 {
		return fmt.Sprintf("%s(%q...)", t.Type, t.Literal[:20])
	}
	return fmt.Sprintf("%s(%q)", t.Type, t.Literal)
}

// Reserved words mapped to their token types.
var reservedWords = map[string]TokenType{
	"self":        TokenSelf,
	"super":       TokenSuper,
	"nil":         TokenNil,
	"true":        TokenTrue,
	"false":       TokenFalse,
	"thisContext": TokenThisContext,
}

// IsBinaryChar returns true if r is a valid binary selector character.
func IsBinaryChar(r rune) bool {
	switch r {
	case '+', '-', '*', '/', '\\', '~', '<', '>', '=', '@', '%', '|', '&', '?', '!', ',':
		return true
	}
	return false
}
