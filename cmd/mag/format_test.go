package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// ---------------------------------------------------------------------------
// Idempotency tests
// ---------------------------------------------------------------------------

func TestFormat_Idempotent(t *testing.T) {
	input := `"""
A sample class.
"""
Foo subclass: Object
  instanceVars: x y

  method: x [
      ^x
  ]

  method: x: aValue [
      x := aValue
  ]
`
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("first format failed: %v", err)
	}

	formatted2, err := Format(formatted)
	if err != nil {
		t.Fatalf("second format failed: %v", err)
	}

	if formatted != formatted2 {
		t.Errorf("not idempotent.\nFirst:\n%s\nSecond:\n%s", formatted, formatted2)
	}
}

func TestFormat_IdempotentComplex(t *testing.T) {
	input := `Array subclass: Object
  include: Printable

  method: collect: block [
      | result |
      result := Array new: self size.
      0 to: self size - 1 do: [:i | result at: i put: (block value: (self at: i))].
      ^result
  ]

  method: printString [
      | s |
      s := '#('.
      self do: [:each | s := s , each printString , ' '].
      s := s , ')'.
      ^s
  ]
`
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("first format failed: %v", err)
	}

	formatted2, err := Format(formatted)
	if err != nil {
		t.Fatalf("second format failed: %v", err)
	}

	if formatted != formatted2 {
		t.Errorf("not idempotent.\nFirst:\n%s\nSecond:\n%s", formatted, formatted2)
	}
}

// ---------------------------------------------------------------------------
// Expression formatting tests
// ---------------------------------------------------------------------------

func TestFormat_IntLiteral(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^42 ]",
		"^42")
}

func TestFormat_FloatLiteral(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^3.14 ]",
		"^3.14")
}

func TestFormat_StringLiteral(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^'hello' ]",
		"^'hello'")
}

func TestFormat_StringWithQuotes(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^'it''s' ]",
		"^'it''s'")
}

func TestFormat_SymbolLiteral(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^#foo ]",
		"^#foo")
}

func TestFormat_SymbolWithColons(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^#at:put: ]",
		"^#at:put:")
}

func TestFormat_QuotedSymbol(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^#':' ]",
		"^#':'")
}

func TestFormat_BinarySymbol(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^#+ ]",
		"^#+")
}

func TestFormat_CharLiteral(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^$a ]",
		"^$a")
}

func TestFormat_ArrayLiteral(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^#(1 2 3) ]",
		"^#(1 2 3)")
}

func TestFormat_DynamicArray(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^{1. 2. 3} ]",
		"^{1. 2. 3}")
}

func TestFormat_NilLiteral(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^nil ]",
		"^nil")
}

func TestFormat_TrueLiteral(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^true ]",
		"^true")
}

func TestFormat_FalseLiteral(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^false ]",
		"^false")
}

func TestFormat_SelfPseudoVar(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^self ]",
		"^self")
}

func TestFormat_SuperPseudoVar(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^super ]",
		"^super")
}

func TestFormat_ThisContextPseudoVar(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^thisContext ]",
		"^thisContext")
}

// ---------------------------------------------------------------------------
// Assignment formatting
// ---------------------------------------------------------------------------

func TestFormat_Assignment(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ x := 42 ]",
		"x := 42")
}

// ---------------------------------------------------------------------------
// Message formatting
// ---------------------------------------------------------------------------

func TestFormat_UnaryMessage(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^self size ]",
		"^self size")
}

func TestFormat_UnaryChain(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^self size printString ]",
		"^self size printString")
}

func TestFormat_BinaryMessage(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^a + b ]",
		"^a + b")
}

func TestFormat_BinaryMessageSpacing(t *testing.T) {
	// The formatter should produce canonical spacing around binary operators
	input := "Foo subclass: Object\n  method: m [ ^a+b ]"
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}
	if !strings.Contains(formatted, "^a + b") {
		t.Errorf("expected spaces around +, got: %s", formatted)
	}
}

func TestFormat_KeywordMessage(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^self at: 0 ]",
		"^self at: 0")
}

func TestFormat_KeywordMultipleArgs(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^self at: 0 put: 42 ]",
		"^self at: 0 put: 42")
}

func TestFormat_KeywordNeedsParensOnReceiver(t *testing.T) {
	// self foo: a bar: b => receiver self gets keyword message
	// but (self foo: a) bar: b => different parse
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^(a + b) printString ]",
		"^(a + b) printString")
}

// ---------------------------------------------------------------------------
// Block formatting
// ---------------------------------------------------------------------------

func TestFormat_EmptyBlock(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^[] ]",
		"^[]")
}

func TestFormat_SimpleBlock(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^[42] ]",
		"^[42]")
}

func TestFormat_BlockWithParams(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^[:x | x + 1] ]",
		"^[:x | x + 1]")
}

func TestFormat_BlockWithMultipleParams(t *testing.T) {
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^[:x :y | x + y] ]",
		"^[:x :y | x + y]")
}

func TestFormat_MultiStatementBlock(t *testing.T) {
	input := "Foo subclass: Object\n  method: m [ ^[x := 1. y := 2. x + y] ]"
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}
	// Multi-statement blocks should be multi-line
	if !strings.Contains(formatted, "x := 1.\n") {
		t.Errorf("expected multi-line block, got: %s", formatted)
	}
}

func TestFormat_BlockWithTemps(t *testing.T) {
	// Blocks with temps and multiple statements go multi-line
	input := "Foo subclass: Object\n  method: m [ ^[| t | t := 1. t] ]"
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}
	if !strings.Contains(formatted, "| t |") {
		t.Errorf("expected temps declaration, got: %s", formatted)
	}
	if !strings.Contains(formatted, "t := 1.") {
		t.Errorf("expected first statement, got: %s", formatted)
	}
}

func TestFormat_BlockWithTempsSingleStmt(t *testing.T) {
	// Block with temps but single statement stays single-line
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^[| t | t + 1] ]",
		"^[| t | t + 1]")
}

// ---------------------------------------------------------------------------
// Cascade formatting
// ---------------------------------------------------------------------------

func TestFormat_Cascade(t *testing.T) {
	input := "Foo subclass: Object\n  method: m [ self foo; bar; baz ]"
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}
	if !strings.Contains(formatted, "self foo;") {
		t.Errorf("expected cascade with semicolons, got: %s", formatted)
	}
}

func TestFormat_CascadeWithKeywords(t *testing.T) {
	input := "Foo subclass: Object\n  method: m [ self at: 1 put: 2; at: 3 put: 4 ]"
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}
	if !strings.Contains(formatted, "self at: 1 put: 2;") {
		t.Errorf("expected keyword cascade, got: %s", formatted)
	}
}

func TestFormat_CascadeNeedsParensInKeywordArg(t *testing.T) {
	// A cascade used as an argument to a keyword message must be parenthesized
	input := "Foo subclass: Object\n  method: m [ ^self foo: (a bar; baz) ]"
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}
	if !strings.Contains(formatted, "(a bar;") {
		t.Errorf("expected parenthesized cascade in keyword arg, got: %s", formatted)
	}
}

// ---------------------------------------------------------------------------
// Class definition formatting
// ---------------------------------------------------------------------------

func TestFormat_ClassDefinition(t *testing.T) {
	input := `Foo subclass: Object
  instanceVars: x y
  method: x [ ^x ]
`
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}

	if !strings.Contains(formatted, "Foo subclass: Object") {
		t.Error("missing class header")
	}
	if !strings.Contains(formatted, "  instanceVars: x y") {
		t.Error("missing instance variables")
	}
	if !strings.Contains(formatted, "  method: x [") {
		t.Error("missing method definition")
	}
}

func TestFormat_ClassWithDocstring(t *testing.T) {
	input := `"""
A test class.
"""
Foo subclass: Object
  method: m [ ^42 ]
`
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}

	if !strings.Contains(formatted, `"""`) {
		t.Error("missing docstring markers")
	}
	if !strings.Contains(formatted, "A test class.") {
		t.Error("missing docstring content")
	}
}

func TestFormat_ClassWithTraits(t *testing.T) {
	input := `Foo subclass: Object
  include: Printable
  method: m [ ^42 ]
`
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}

	if !strings.Contains(formatted, "  include: Printable") {
		t.Error("missing trait include")
	}
}

func TestFormat_ClassMethods(t *testing.T) {
	input := `Foo subclass: Object
  method: m [ ^42 ]
  classMethod: new [ ^super new ]
`
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}

	if !strings.Contains(formatted, "  classMethod: new [") {
		t.Error("missing class method")
	}
}

func TestFormat_MethodWithTemps(t *testing.T) {
	input := `Foo subclass: Object
  method: m [
    | x y |
    x := 1.
    y := 2.
    ^x + y
  ]
`
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}

	if !strings.Contains(formatted, "      | x y |") {
		t.Errorf("expected properly indented temps, got: %s", formatted)
	}
}

func TestFormat_PrimitiveStub(t *testing.T) {
	input := `Foo subclass: Object
  method: size [ <primitive> ]
`
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}

	if !strings.Contains(formatted, "  method: size [ <primitive> ]") {
		t.Errorf("expected primitive stub, got: %s", formatted)
	}
}

// ---------------------------------------------------------------------------
// Trait definition formatting
// ---------------------------------------------------------------------------

func TestFormat_TraitDefinition(t *testing.T) {
	input := `Printable trait
  method: description [
    ^'<', self class, '>'
  ]
`
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}

	if !strings.Contains(formatted, "Printable trait") {
		t.Error("missing trait header")
	}
	if !strings.Contains(formatted, "  method: description [") {
		t.Error("missing trait method")
	}
}

// ---------------------------------------------------------------------------
// Namespace and import formatting
// ---------------------------------------------------------------------------

func TestFormat_NamespaceDecl(t *testing.T) {
	input := `namespace: 'MyApp::Models'
Foo subclass: Object
  method: m [ ^42 ]
`
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}

	if !strings.HasPrefix(formatted, "namespace: 'MyApp::Models'\n") {
		t.Errorf("expected namespace at top, got: %s", formatted)
	}
}

func TestFormat_ImportDecl(t *testing.T) {
	input := `namespace: 'MyApp'
import: 'Yutani'
import: 'Yutani::Events'
Foo subclass: Object
  method: m [ ^42 ]
`
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}

	if !strings.Contains(formatted, "import: 'Yutani'\n") {
		t.Errorf("missing first import, got: %s", formatted)
	}
	if !strings.Contains(formatted, "import: 'Yutani::Events'\n") {
		t.Errorf("missing second import, got: %s", formatted)
	}
	// Blank line between imports and class definition
	if !strings.Contains(formatted, "import: 'Yutani::Events'\n\nFoo subclass:") {
		t.Errorf("expected blank line after imports, got: %s", formatted)
	}
}

// ---------------------------------------------------------------------------
// Docstring formatting
// ---------------------------------------------------------------------------

func TestFormat_MethodDocstring(t *testing.T) {
	input := `Foo subclass: Object
  """
  Return the size.
  """
  method: size [ ^42 ]
`
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}

	if !strings.Contains(formatted, "  \"\"\"\n  Return the size.\n  \"\"\"") {
		t.Errorf("expected properly indented docstring, got: %s", formatted)
	}
}

func TestFormat_DocstringWithTestBlock(t *testing.T) {
	input := "Foo subclass: Object\n  \"\"\"\n  Test.\n\n  ```test\n  1 + 1 >>> 2\n  ```\n  \"\"\"\n  method: m [ ^42 ]\n"
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}

	if !strings.Contains(formatted, "```test") {
		t.Errorf("expected test block in docstring, got: %s", formatted)
	}
}

// ---------------------------------------------------------------------------
// Method signature formatting
// ---------------------------------------------------------------------------

func TestFormat_UnaryMethodSignature(t *testing.T) {
	input := `Foo subclass: Object
  method: size [ ^42 ]
`
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}
	if !strings.Contains(formatted, "  method: size [") {
		t.Errorf("expected unary method signature, got: %s", formatted)
	}
}

func TestFormat_BinaryMethodSignature(t *testing.T) {
	input := `Foo subclass: Object
  method: + other [ ^self primAdd: other ]
`
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}
	if !strings.Contains(formatted, "  method: + other [") {
		t.Errorf("expected binary method signature, got: %s", formatted)
	}
}

func TestFormat_KeywordMethodSignature(t *testing.T) {
	input := `Foo subclass: Object
  method: at: index put: value [ ^self primAt: index put: value ]
`
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}
	if !strings.Contains(formatted, "  method: at: index put: value [") {
		t.Errorf("expected keyword method signature, got: %s", formatted)
	}
}

// ---------------------------------------------------------------------------
// Trailing whitespace and final newline
// ---------------------------------------------------------------------------

func TestFormat_TrailingNewline(t *testing.T) {
	input := "Foo subclass: Object\n  method: m [ ^42 ]"
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}
	if !strings.HasSuffix(formatted, "\n") {
		t.Error("expected trailing newline")
	}
	// Should not have multiple trailing newlines
	if strings.HasSuffix(formatted, "\n\n") {
		t.Error("should have exactly one trailing newline, not multiple")
	}
}

func TestFormat_NoTrailingWhitespace(t *testing.T) {
	input := "Foo subclass: Object\n  method: m [ ^42 ]  \n"
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}
	for i, line := range strings.Split(formatted, "\n") {
		if line != strings.TrimRight(line, " \t") {
			t.Errorf("line %d has trailing whitespace: %q", i+1, line)
		}
	}
}

// ---------------------------------------------------------------------------
// Blank line between methods
// ---------------------------------------------------------------------------

func TestFormat_BlankLineBetweenMethods(t *testing.T) {
	input := `Foo subclass: Object
  method: a [ ^1 ]
  method: b [ ^2 ]
`
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}

	// Should have a blank line between methods
	if !strings.Contains(formatted, "  ]\n\n  method: b") {
		t.Errorf("expected blank line between methods, got: %s", formatted)
	}
}

// ---------------------------------------------------------------------------
// File I/O tests
// ---------------------------------------------------------------------------

func TestFormatFile_InPlace(t *testing.T) {
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "test.mag")

	input := "Foo subclass: Object\n  method: m [ ^42 ]\n"
	err := os.WriteFile(testFile, []byte(input), 0644)
	if err != nil {
		t.Fatalf("write failed: %v", err)
	}

	changed, err := formatFile(testFile, false)
	if err != nil {
		t.Fatalf("formatFile failed: %v", err)
	}

	// Read back
	content, err := os.ReadFile(testFile)
	if err != nil {
		t.Fatalf("read back failed: %v", err)
	}

	formatted := string(content)
	if !strings.Contains(formatted, "Foo subclass: Object") {
		t.Error("formatted content missing class definition")
	}

	// Second pass should report no changes
	changed2, err := formatFile(testFile, false)
	if err != nil {
		t.Fatalf("second formatFile failed: %v", err)
	}
	if changed2 {
		t.Error("expected no changes on second format")
	}
	_ = changed // first pass may or may not change depending on existing format
}

func TestFormatFile_CheckMode(t *testing.T) {
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "test.mag")

	// Write something that needs formatting (bad indentation)
	input := "Foo subclass: Object\n  method: m [\n^42\n]\n"
	err := os.WriteFile(testFile, []byte(input), 0644)
	if err != nil {
		t.Fatalf("write failed: %v", err)
	}

	changed, err := formatFile(testFile, true)
	if err != nil {
		t.Fatalf("formatFile check failed: %v", err)
	}
	if !changed {
		t.Error("expected check mode to detect changes needed")
	}

	// Verify file was NOT modified
	content, err := os.ReadFile(testFile)
	if err != nil {
		t.Fatalf("read back failed: %v", err)
	}
	if string(content) != input {
		t.Error("check mode should not modify the file")
	}
}

// ---------------------------------------------------------------------------
// Precedence and parenthesization tests
// ---------------------------------------------------------------------------

func TestFormat_BinaryLeftAssocNoExtraParens(t *testing.T) {
	// Binary messages are left-associative, so (a + b) + c = a + b + c
	// The formatter should not add unnecessary parens
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^(a + b) + c ]",
		"^a + b + c")
}

func TestFormat_KeywordArgAcceptsBinary(t *testing.T) {
	// Keyword message arguments naturally consume binary expressions
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^self at: (a + b) ]",
		"^self at: a + b")
}

func TestFormat_KeywordAsReceiverNeedsParens(t *testing.T) {
	// A keyword send as receiver of a unary send needs parens
	assertFormatsExpr(t,
		"Foo subclass: Object\n  method: m [ ^(self at: 0) printString ]",
		"^(self at: 0) printString")
}

// ---------------------------------------------------------------------------
// Integration: test real .mag files from lib/
// ---------------------------------------------------------------------------

func TestFormat_RealMagFiles_Idempotent(t *testing.T) {
	// Find all .mag files in ../../lib
	libDir := filepath.Join("..", "..", "lib")
	if _, err := os.Stat(libDir); os.IsNotExist(err) {
		t.Skip("lib/ directory not found at ../../lib, skipping real file tests")
	}

	var magFiles []string
	err := filepath.Walk(libDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() && strings.HasSuffix(path, ".mag") {
			magFiles = append(magFiles, path)
		}
		return nil
	})
	if err != nil {
		t.Fatalf("walking lib/: %v", err)
	}

	if len(magFiles) == 0 {
		t.Fatal("no .mag files found in lib/")
	}

	for _, path := range magFiles {
		t.Run(filepath.Base(path), func(t *testing.T) {
			content, err := os.ReadFile(path)
			if err != nil {
				t.Fatalf("reading %s: %v", path, err)
			}

			// First format
			formatted, err := Format(string(content))
			if err != nil {
				t.Fatalf("format failed for %s: %v", path, err)
			}

			// Second format (idempotency)
			formatted2, err := Format(formatted)
			if err != nil {
				t.Fatalf("second format failed for %s: %v", path, err)
			}

			if formatted != formatted2 {
				t.Errorf("not idempotent for %s", path)
				// Show first differing line
				lines1 := strings.Split(formatted, "\n")
				lines2 := strings.Split(formatted2, "\n")
				for i := 0; i < len(lines1) && i < len(lines2); i++ {
					if lines1[i] != lines2[i] {
						t.Errorf("  line %d differs:\n    pass1: %q\n    pass2: %q", i+1, lines1[i], lines2[i])
						break
					}
				}
			}
		})
	}
}

// ---------------------------------------------------------------------------
// Helper
// ---------------------------------------------------------------------------

// assertFormatsExpr checks that formatting the input produces a method body
// containing the expected expression string.
func assertFormatsExpr(t *testing.T, input, expectedExpr string) {
	t.Helper()
	formatted, err := Format(input)
	if err != nil {
		t.Fatalf("format failed: %v", err)
	}
	// Find the expression inside the method body (between [ and ])
	// The method body should contain the expected expression
	if !strings.Contains(formatted, expectedExpr) {
		t.Errorf("expected %q in formatted output:\n%s", expectedExpr, formatted)
	}
}
