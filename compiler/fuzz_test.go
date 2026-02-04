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
				registry := vm.NewObjectRegistry()
				_, _ = CompileMethodDef(method, selectors, symbols, registry)
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
			registry := vm.NewObjectRegistry()
			_, _ = CompileExpr(data, selectors, symbols, registry)
		}()
	})
}

// ---------------------------------------------------------------------------
// FuzzSemantic: ensure the semantic analyzer never panics on arbitrary input.
// Semantic errors/warnings are acceptable; panics are bugs.
// ---------------------------------------------------------------------------

func FuzzSemantic(f *testing.F) {
	seeds := []string{
		// ---- Basic method definitions ----
		`doIt [^42]`,
		`doIt: x [^x + 1]`,
		`at: i put: v [^self]`,
		`+ other [^self]`,
		`doIt [| x y z | x := 1. y := 2. z := x + y. ^z]`,

		// ---- Undefined and reserved variables ----
		`doIt [^undefinedVar]`,
		`doIt [^Object new]`,
		`doIt [^self]`,
		`doIt [^super foo]`,
		`doIt [^nil]`,
		`doIt [^true]`,
		`doIt [^false]`,
		`doIt [^thisContext]`,

		// ---- Unreachable code ----
		`doIt [^1. ^2]`,
		`doIt [^1. self foo. self bar]`,

		// ---- Blocks and closures ----
		`doIt [[:x | x + 1] value: 5]`,
		`doIt [[:x :y | x + y] value: 1 value: 2]`,
		`doIt [| x | x := 5. [:y | x + y] value: 3]`,
		`doIt [[:a | [:b | a + b]] value: 1]`,
		`doIt [[[[42]]]]`,
		`doIt [[] value]`,

		// ---- Cascades ----
		`doIt [self add: 1; add: 2; yourself]`,
		`doIt [| c | c := OrderedCollection new. c add: 1; add: 2; yourself]`,

		// ---- Arrays ----
		`doIt [#(1 2 3)]`,
		`doIt [{1. 2. 3}]`,
		`doIt [{1 + 2. 3 * 4. undefinedVar}]`,

		// ---- Instance variables ----
		`getValue [^value]`,
		`setValue: v [value := v]`,

		// ---- Deeply nested expressions ----
		`doIt [((((((1 + 2) + 3) + 4) + 5) + 6) + 7)]`,
		`doIt [self a b c d e f g h i j]`,
		`doIt [self at: (self at: (self at: 1))]`,

		// ---- Empty/minimal methods ----
		`doIt []`,
		`doIt [nil]`,
		`x [self]`,

		// ---- Assignment edge cases ----
		`doIt [| x | x := x]`,
		`doIt [| x | x := x := 1]`,

		// ---- Class definitions with methods (source file format) ----
		"Object subclass: MyClass\n  instanceVars: x y\n  method: getX [^x]\n  method: setX: v [x := v]",

		// ---- Circular inheritance (A subclass: B, B subclass: A) ----
		"Object subclass: A\n  method: foo [^1]\nObject subclass: B\n  method: bar [^2]\nA subclass: B\n  method: baz [^3]",
		"A subclass: B\n  method: foo [^1]\nB subclass: A\n  method: bar [^2]",

		// ---- Class with itself as superclass ----
		"A subclass: A\n  method: foo [^self]",

		// ---- Deeply nested class hierarchy ----
		"Object subclass: A\n  method: a [^1]\nA subclass: B\n  method: b [^2]\nB subclass: C\n  method: c [^3]\nC subclass: D\n  method: d [^4]\nD subclass: E\n  method: e [^5]",

		// ---- Trait definitions ----
		"Printable trait\n  method: printString [^'<object>']",
		"Enumerable trait\n  method: do: aBlock [self subclassResponsibility]\n  method: collect: aBlock [| result | result := Array new. self do: [:each | result add: (aBlock value: each)]. ^result]",
		"Printable trait\n  requires: printString\n  method: print [Transcript show: self printString]",

		// ---- Class using traits ----
		"Object subclass: MyClass\n  uses: Printable\n  method: printString [^'MyClass']",
		"Object subclass: MyClass\n  uses: Printable Enumerable\n  instanceVars: items\n  method: do: aBlock [items do: aBlock]",

		// ---- Namespace and import declarations ----
		"namespace: 'MyApp'\nimport: 'Collections'\nObject subclass: Foo\n  method: bar [^nil]",
		"namespace: 'MyApp::Models'\nimport: 'MyApp'\nimport: 'MyApp::Events'\nObject subclass: User\n  instanceVars: name email\n  method: name [^name]\n  method: email [^email]",

		// ---- Multiple namespace declarations (edge case) ----
		"namespace: 'A'\nnamespace: 'B'\nObject subclass: C\n  method: m [nil]",

		// ---- Multiple imports ----
		"import: 'A'\nimport: 'B'\nimport: 'C'\nimport: 'D'\nimport: 'E'\nObject subclass: X\n  method: m [nil]",

		// ---- Class with many methods (varying arity) ----
		"Object subclass: Multi\n  method: unary [^1]\n  method: + other [^2]\n  method: at: i [^3]\n  method: at: i put: v [^4]\n  method: from: a to: b by: c [^5]",

		// ---- Method with docstrings ----
		"Object subclass: Doc\n  \"\"\"A documented class\"\"\"\n  method: \"\"\"Returns forty-two\"\"\" answer [^42]",

		// ---- Class methods ----
		"Object subclass: Factory\n  classMethod: new [^super new initialize]\n  method: initialize [nil]",

		// ---- Large class with instance vars, class vars, pools ----
		"Object subclass: BigClass\n  instanceVars: a b c d e f g\n  classVars: X Y Z\n  method: a [^a]\n  method: b [^b]\n  method: c [^c]",

		// ---- Edge: class with no methods ----
		"Object subclass: Empty",
		"Object subclass: EmptyWithVars\n  instanceVars: x y z",

		// ---- Edge: trait with no methods ----
		"EmptyTrait trait",

		// ---- Complex block nesting with variables ----
		`doIt [| a | a := 1. [:b | | c | c := a + b. [:d | c + d] value: 3] value: 2]`,

		// ---- Binary message chains ----
		`doIt [1 + 2 - 3 * 4 / 5]`,

		// ---- Keyword messages with complex arguments ----
		`doIt [self at: (1 + 2) put: ([:x | x * x] value: 3)]`,

		// ---- Multiple statements with returns ----
		`doIt [| x | x := 1. ^x. x := 2. ^x. x := 3. ^x]`,

		// ---- Source file with everything ----
		"namespace: 'TestNS'\nimport: 'Base'\n\nPrintable trait\n  method: printString [^'<object>']\n\nObject subclass: MyClass\n  uses: Printable\n  instanceVars: value\n  method: value [^value]\n  method: value: v [value := v]\n  classMethod: withValue: v [^self new value: v]",

		// ---- Garbage that parses as something ----
		`x [self yourself yourself yourself yourself]`,
		`x [| | nil]`,
		`x [| a a a | a]`,
	}

	for _, s := range seeds {
		f.Add(s)
	}

	f.Fuzz(func(t *testing.T, data string) {
		// Strategy 1: Parse as a method and analyze it
		func() {
			defer func() {
				if r := recover(); r != nil {
					t.Fatalf("semantic analysis panicked on method input %q: %v", data, r)
				}
			}()

			p := NewParser(data)
			method := p.ParseMethod()
			if len(p.Errors()) > 0 || method == nil {
				return // parse errors are fine
			}

			// Analyze without instance variables
			_ = Analyze(method, nil)

			// Analyze with some instance variables
			_ = Analyze(method, []string{"x", "y", "value", "items"})
		}()

		// Strategy 2: Parse as a source file and analyze all methods found
		func() {
			defer func() {
				if r := recover(); r != nil {
					t.Fatalf("semantic analysis panicked on source file input %q: %v", data, r)
				}
			}()

			sf, err := ParseSourceFileFromString(data)
			if err != nil || sf == nil {
				return // parse errors are fine
			}

			// Analyze methods in each class
			for _, class := range sf.Classes {
				ivars := class.InstanceVariables
				for _, method := range class.Methods {
					_ = Analyze(method, ivars)
				}
				for _, method := range class.ClassMethods {
					_ = Analyze(method, nil)
				}
			}

			// Analyze methods in each trait
			for _, trait := range sf.Traits {
				for _, method := range trait.Methods {
					_ = Analyze(method, nil)
				}
			}

			// Analyze extension methods
			for _, method := range sf.Methods {
				_ = Analyze(method, nil)
			}
		}()

		// Strategy 3: Use the lower-level API directly for more coverage
		func() {
			defer func() {
				if r := recover(); r != nil {
					t.Fatalf("SemanticAnalyzer panicked on method input %q: %v", data, r)
				}
			}()

			p := NewParser(data)
			method := p.ParseMethod()
			if len(p.Errors()) > 0 || method == nil {
				return
			}

			analyzer := NewSemanticAnalyzer()
			analyzer.AddKnownGlobal("CustomGlobal")
			analyzer.SetInstanceVars([]string{"a", "b", "c"})
			analyzer.AnalyzeMethod(method)
			_ = analyzer.Errors()
		}()
	})
}
