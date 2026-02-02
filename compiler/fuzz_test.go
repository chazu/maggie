package compiler

import (
	"testing"

	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// FuzzLexer: ensure the lexer never panics on arbitrary input.
// ---------------------------------------------------------------------------

func FuzzLexer(f *testing.F) {
	// Seed corpus: valid Maggie code snippets covering diverse token types
	seeds := []string{
		// Basic tokens
		`( ) [ ] { } ^ . ; := : |`,
		// Integers
		`42`, `0`, `-123`, `16rFF`, `2r1010`, `8r777`,
		// Floats
		`3.14`, `0.5`, `-2.5`, `1e10`, `1.5e-3`, `2.0E+5`,
		// Strings
		`'hello'`, `'hello world'`, `''`, `'it''s'`,
		// Symbols
		`#foo`, `#FooBar`, `#at:`, `#at:put:`, `#+`, `#--`, `#<=`, `#'hello world'`,
		// Characters
		`$a`, `$Z`, `$0`, `$ `,
		// Identifiers and reserved words
		`foo`, `FooBar`, `foo123`, `_private`, `self`, `super`, `nil`, `true`, `false`, `thisContext`,
		// Keywords
		`at:`, `put:`, `ifTrue:`,
		// Binary selectors
		`+`, `-`, `*`, `/`, `<`, `>`, `<=`, `>=`, `=`, `~=`, `==`, `@`,
		// Comments
		`"this is a comment"`, `foo "this is a comment" bar`,
		// Hash comments
		"# this is a comment\nfoo",
		// Complete expressions
		`x := 42`,
		`obj foo`,
		`3 + 4`,
		`arr at: 1 put: 'hello'`,
		`[:x :y | x + y]`,
		`obj msg1; msg2; msg3`,
		`#(1 2 3)`,
		`{1. 2. 3}`,
		// Docstrings
		`"""Hello world"""`,
		`""""""`,
		// A method
		"increment: amount\n    | result |\n    result := value + amount.\n    ^result",
		// Edge cases
		`$`, `#`, `'unterminated`, `"""unterminated`,
		// Unicode
		`'こんにちは'`, `café`, `naïve`,
		// Empty
		``,
		// Whitespace only
		`   `, "\t\n\r",
		// Binary soup
		`+-*/\\~<>=@%|&?!,`,
	}

	for _, s := range seeds {
		f.Add(s)
	}

	f.Fuzz(func(t *testing.T, data string) {
		defer func() {
			if r := recover(); r != nil {
				t.Fatalf("lexer panicked on input %q: %v", data, r)
			}
		}()

		l := NewLexer(data)
		for i := 0; i < len(data)+100; i++ {
			tok := l.NextToken()
			if tok.Type == TokenEOF || tok.Type == TokenError {
				break
			}
		}
	})
}

// ---------------------------------------------------------------------------
// FuzzParser: ensure the parser never panics on arbitrary input.
// Parse errors are acceptable; panics are not.
// ---------------------------------------------------------------------------

func FuzzParser(f *testing.F) {
	seeds := []string{
		// Literals
		`42`, `-5`, `3.14`, `'hello'`, `#foo`, `$a`,
		// Variables and reserved words
		`foo`, `self`, `super`, `nil`, `true`, `false`, `thisContext`,
		// Unary messages
		`obj foo`, `obj foo bar baz`,
		// Binary messages
		`3 + 4`, `a + b * c`,
		// Keyword messages
		`arr at: 1`, `arr at: 1 put: 'hello'`,
		// Assignment
		`x := 42`, `x := y + z`,
		// Blocks
		`[42]`, `[:x | x + 1]`, `[:x :y | x + y]`,
		`[| temp | temp := 42. temp]`,
		// Cascades
		`obj msg1; msg2; msg3`,
		`obj add: 1; add: 2; yourself`,
		// Parenthesized expressions
		`(3 + 4) * 5`,
		// Arrays
		`#(1 2 3)`, `#(#foo 'bar' 42)`,
		`{1. 2. 3}`, `{1 + 2. 3 * 4}`,
		// Return
		`^42`, `^self`,
		// Nested blocks
		`[[:x | x + 1] value: 42]`,
		// Source file with class definition
		"Object subclass: MyClass\n  instanceVars: x y\n  method: foo [^x]",
		// Source file with trait
		"Printable trait\n  method: printString [^'<object>']",
		// Namespace and import
		"namespace: 'MyApp'\nimport: 'Collections'\nObject subclass: Foo\n  method: bar [nil]",
		// Docstrings
		`"""A docstring""" Object subclass: Foo method: bar [nil]`,
		// Edge cases that might trip up the parser
		``, `(`, `)`, `[`, `]`, `{`, `}`, `^`, `.`, `;`,
		`:=`, `|`, `#`, `::`,
		`Object subclass:`,
		`method: [`,
		`[:`,
		`[|]`,
	}

	for _, s := range seeds {
		f.Add(s)
	}

	f.Fuzz(func(t *testing.T, data string) {
		defer func() {
			if r := recover(); r != nil {
				t.Fatalf("parser panicked on input %q: %v", data, r)
			}
		}()

		// Test ParseExpression
		func() {
			defer func() {
				if r := recover(); r != nil {
					t.Fatalf("ParseExpression panicked on input %q: %v", data, r)
				}
			}()
			p := NewParser(data)
			_ = p.ParseExpression()
			_ = p.Errors()
		}()

		// Test ParseStatements
		func() {
			defer func() {
				if r := recover(); r != nil {
					t.Fatalf("ParseStatements panicked on input %q: %v", data, r)
				}
			}()
			p := NewParser(data)
			_ = p.ParseStatements()
			_ = p.Errors()
		}()

		// Test ParseSourceFile
		func() {
			defer func() {
				if r := recover(); r != nil {
					t.Fatalf("ParseSourceFile panicked on input %q: %v", data, r)
				}
			}()
			p := NewParser(data)
			_ = p.ParseSourceFile()
			_ = p.Errors()
		}()

		// Test ParseMethod
		func() {
			defer func() {
				if r := recover(); r != nil {
					t.Fatalf("ParseMethod panicked on input %q: %v", data, r)
				}
			}()
			p := NewParser(data)
			_ = p.ParseMethod()
			_ = p.Errors()
		}()
	})
}

// ---------------------------------------------------------------------------
// FuzzCompileAndRun: feed arbitrary method bodies through the full pipeline
// (parse -> codegen). Parse errors are fine, panics are not.
// ---------------------------------------------------------------------------

func FuzzCompileAndRun(f *testing.F) {
	seeds := []string{
		// Simple expressions
		`42`,
		`'hello'`,
		`nil`,
		`true`,
		`false`,
		`self`,
		`3 + 4`,
		`x := 42. x`,
		`^42`,
		// Blocks
		`[:x | x + 1] value: 5`,
		`[42] value`,
		// Message sends
		`self foo`,
		`self at: 1`,
		`self at: 1 put: 2`,
		// Arrays
		`#(1 2 3)`,
		`{1. 2. 3}`,
		// Cascades
		`self add: 1; add: 2; yourself`,
		// Nested
		`(3 + 4) * (5 - 2)`,
		// Assignment and return
		`| temp | temp := 42. ^temp`,
		// Complex
		`| acc | acc := 0. [:x | acc := acc + x] value: 10. ^acc`,
		// Edge cases
		``, `^self`,
	}

	for _, s := range seeds {
		f.Add(s)
	}

	f.Fuzz(func(t *testing.T, data string) {
		// Wrap the fuzzed input in a valid method context
		methodSource := "m [" + data + "]"
		classSource := "Object subclass: FuzzClass\n  method: " + methodSource

		// Phase 1: Parse the class source file
		func() {
			defer func() {
				if r := recover(); r != nil {
					t.Fatalf("ParseSourceFile panicked on input %q: %v", data, r)
				}
			}()

			sf, err := ParseSourceFileFromString(classSource)
			if err != nil {
				return // parse errors are fine
			}
			if sf == nil || len(sf.Classes) == 0 {
				return
			}

			classDef := sf.Classes[0]
			if len(classDef.Methods) == 0 {
				return
			}

			method := classDef.Methods[0]

			// Phase 2: Compile the method to bytecode
			func() {
				defer func() {
					if r := recover(); r != nil {
						t.Fatalf("CompileMethodDef panicked on input %q: %v", data, r)
					}
				}()

				selectors := vm.NewSelectorTable()
				symbols := vm.NewSymbolTable()
				_, _ = CompileMethodDef(method, selectors, symbols)
			}()
		}()

		// Also test standalone expression compilation
		func() {
			defer func() {
				if r := recover(); r != nil {
					t.Fatalf("CompileExpr panicked on input %q: %v", data, r)
				}
			}()

			selectors := vm.NewSelectorTable()
			symbols := vm.NewSymbolTable()
			_, _ = CompileExpr(data, selectors, symbols)
		}()
	})
}
