package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/chazu/maggie/compiler"
)

// ---------------------------------------------------------------------------
// mag fmt — Canonical source code formatter for Maggie
// ---------------------------------------------------------------------------

const maxLineWidth = 80

// Format parses a Maggie source string and returns canonically formatted output.
// This is the library-level entry point; it does not touch the filesystem.
func Format(source string) (string, error) {
	sf, err := compiler.ParseSourceFileFromString(source)
	if err != nil {
		return "", err
	}

	f := &formatter{
		indent: 0,
		buf:    &strings.Builder{},
	}

	f.formatSourceFile(sf)

	result := f.buf.String()
	// Ensure file ends with exactly one newline
	result = strings.TrimRight(result, "\n") + "\n"
	return result, nil
}

// formatter walks the AST and emits canonically formatted source.
type formatter struct {
	indent int
	buf    *strings.Builder
}

// write appends text to the output buffer.
func (f *formatter) write(s string) {
	f.buf.WriteString(s)
}

// writeln appends text followed by a newline.
func (f *formatter) writeln(s string) {
	f.buf.WriteString(s)
	f.buf.WriteByte('\n')
}

// newline writes a blank line.
func (f *formatter) newline() {
	f.buf.WriteByte('\n')
}

// writeIndent writes the current indentation prefix (two spaces per level).
func (f *formatter) writeIndent() {
	for i := 0; i < f.indent; i++ {
		f.buf.WriteString("  ")
	}
}

// ---------------------------------------------------------------------------
// Source file formatting
// ---------------------------------------------------------------------------

func (f *formatter) formatSourceFile(sf *compiler.SourceFile) {
	needsBlank := false

	// Namespace declaration
	if sf.Namespace != nil {
		f.writeln(fmt.Sprintf("namespace: '%s'", sf.Namespace.Name))
		needsBlank = true
	}

	// Import declarations
	for _, imp := range sf.Imports {
		f.writeln(fmt.Sprintf("import: '%s'", imp.Path))
		needsBlank = true
	}

	// Blank line after namespace/import block
	if needsBlank {
		f.newline()
	}

	// Classes
	for i, cls := range sf.Classes {
		if i > 0 {
			f.newline()
		}
		f.formatClassDef(cls)
	}

	// Traits
	for i, trait := range sf.Traits {
		if i > 0 || len(sf.Classes) > 0 {
			f.newline()
		}
		f.formatTraitDef(trait)
	}

	// Top-level statements (scripts/REPL)
	for _, stmt := range sf.Statements {
		f.formatStmt(stmt)
		f.writeln(".")
	}
}

// ---------------------------------------------------------------------------
// Class definition formatting
// ---------------------------------------------------------------------------

func (f *formatter) formatClassDef(cls *compiler.ClassDef) {
	// Class docstring
	if cls.DocString != "" {
		f.formatDocString(cls.DocString, 0)
	}

	// Class header
	f.writeln(fmt.Sprintf("%s subclass: %s", cls.Name, cls.Superclass))

	// Traits
	for _, trait := range cls.Traits {
		f.writeln(fmt.Sprintf("  include: %s", trait))
	}

	// Instance variables
	if len(cls.InstanceVariables) > 0 {
		ivars := strings.Join(cls.InstanceVariables, " ")
		f.writeln(fmt.Sprintf("  instanceVars: %s", ivars))
	}

	// Instance methods
	for _, method := range cls.Methods {
		f.newline()
		f.formatMethodDef(method, "method:", 1)
	}

	// Class methods
	for _, method := range cls.ClassMethods {
		f.newline()
		f.formatMethodDef(method, "classMethod:", 1)
	}
}

// ---------------------------------------------------------------------------
// Trait definition formatting
// ---------------------------------------------------------------------------

func (f *formatter) formatTraitDef(trait *compiler.TraitDef) {
	// Trait docstring
	if trait.DocString != "" {
		f.formatDocString(trait.DocString, 0)
	}

	// Trait header
	f.writeln(fmt.Sprintf("%s trait", trait.Name))

	// Required methods
	if len(trait.Requires) > 0 {
		f.write("  requires: ")
		for i, req := range trait.Requires {
			if i > 0 {
				f.write(" ")
			}
			f.write(req)
		}
		f.newline()
	}

	// Methods
	for _, method := range trait.Methods {
		f.newline()
		f.formatMethodDef(method, "method:", 1)
	}
}

// ---------------------------------------------------------------------------
// Method definition formatting
// ---------------------------------------------------------------------------

func (f *formatter) formatMethodDef(method *compiler.MethodDef, keyword string, baseIndent int) {
	// Method docstring
	if method.DocString != "" {
		f.formatDocString(method.DocString, baseIndent)
	}

	// Method header: keyword selector [
	f.writeNIndent(baseIndent)
	f.write(keyword + " ")
	f.write(formatMethodSignature(method))

	// Handle primitive stub
	if method.IsPrimitiveStub {
		f.writeln(" [ <primitive> ]")
		return
	}

	f.writeln(" [")

	// Set the formatter indent to match the method body content level
	// so that nested blocks pick up the right indentation.
	savedIndent := f.indent
	f.indent = baseIndent + 2

	// Temporaries
	if len(method.Temps) > 0 {
		f.writeIndent()
		f.write("| ")
		f.write(strings.Join(method.Temps, " "))
		f.writeln(" |")
	}

	// Statements
	for i, stmt := range method.Statements {
		f.writeIndent()
		f.formatStmt(stmt)
		// Period after each statement except the last
		if i < len(method.Statements)-1 {
			f.write(".")
		}
		f.newline()
	}

	f.indent = savedIndent
	f.writeNIndent(baseIndent)
	f.writeln("]")
}

// formatMethodSignature produces the canonical selector with parameters.
func formatMethodSignature(method *compiler.MethodDef) string {
	sel := method.Selector
	params := method.Parameters

	// Unary selector (no parameters)
	if len(params) == 0 {
		return sel
	}

	// Binary selector (one parameter)
	if len(params) == 1 && !strings.Contains(sel, ":") {
		return sel + " " + params[0]
	}

	// Keyword selector: split by ":" to pair keywords with parameters
	keywords := splitKeywordSelector(sel)
	var parts []string
	for i, kw := range keywords {
		if i < len(params) {
			parts = append(parts, kw+": "+params[i])
		} else {
			parts = append(parts, kw+":")
		}
	}
	return strings.Join(parts, " ")
}

// splitKeywordSelector splits "at:put:" into ["at", "put"].
func splitKeywordSelector(sel string) []string {
	parts := strings.Split(sel, ":")
	// Remove trailing empty string from "at:put:" -> ["at", "put", ""]
	var result []string
	for _, p := range parts {
		if p != "" {
			result = append(result, p)
		}
	}
	return result
}

// writeNIndent writes n levels of indentation.
func (f *formatter) writeNIndent(n int) {
	for i := 0; i < n; i++ {
		f.buf.WriteString("  ")
	}
}

// ---------------------------------------------------------------------------
// Docstring formatting
// ---------------------------------------------------------------------------

func (f *formatter) formatDocString(doc string, indentLevel int) {
	f.writeNIndent(indentLevel)
	f.writeln(`"""`)

	lines := strings.Split(doc, "\n")
	for _, line := range lines {
		if strings.TrimSpace(line) == "" {
			f.newline()
		} else {
			f.writeNIndent(indentLevel)
			f.writeln(line)
		}
	}

	f.writeNIndent(indentLevel)
	f.writeln(`"""`)
}

// ---------------------------------------------------------------------------
// Statement formatting
// ---------------------------------------------------------------------------

func (f *formatter) formatStmt(stmt compiler.Stmt) {
	switch s := stmt.(type) {
	case *compiler.ExprStmt:
		f.formatExpr(s.Expr)
	case *compiler.Return:
		f.write("^")
		f.formatExpr(s.Value)
	}
}

// ---------------------------------------------------------------------------
// Expression formatting
// ---------------------------------------------------------------------------

func (f *formatter) formatExpr(expr compiler.Expr) {
	switch e := expr.(type) {
	case *compiler.IntLiteral:
		f.write(fmt.Sprintf("%d", e.Value))

	case *compiler.FloatLiteral:
		f.write(formatFloat(e.Value))

	case *compiler.StringLiteral:
		f.write("'" + escapeString(e.Value) + "'")

	case *compiler.SymbolLiteral:
		f.formatSymbol(e)

	case *compiler.CharLiteral:
		f.write("$" + string(e.Value))

	case *compiler.ArrayLiteral:
		f.write("#(")
		for i, elem := range e.Elements {
			if i > 0 {
				f.write(" ")
			}
			f.formatLiteralArrayElement(elem)
		}
		f.write(")")

	case *compiler.DynamicArray:
		f.write("{")
		for i, elem := range e.Elements {
			if i > 0 {
				f.write(". ")
			}
			f.formatExpr(elem)
		}
		f.write("}")

	case *compiler.Variable:
		f.write(e.Name)

	case *compiler.Assignment:
		f.write(e.Variable)
		f.write(" := ")
		f.formatExpr(e.Value)

	case *compiler.Self:
		f.write("self")

	case *compiler.Super:
		f.write("super")

	case *compiler.ThisContext:
		f.write("thisContext")

	case *compiler.NilLiteral:
		f.write("nil")

	case *compiler.TrueLiteral:
		f.write("true")

	case *compiler.FalseLiteral:
		f.write("false")

	case *compiler.UnaryMessage:
		f.formatUnaryMessage(e)

	case *compiler.BinaryMessage:
		f.formatBinaryMessage(e)

	case *compiler.KeywordMessage:
		f.formatKeywordMessage(e)

	case *compiler.Cascade:
		f.formatCascade(e)

	case *compiler.Block:
		f.formatBlock(e)
	}
}

// ---------------------------------------------------------------------------
// Message formatting
// ---------------------------------------------------------------------------

func (f *formatter) formatUnaryMessage(msg *compiler.UnaryMessage) {
	f.formatExprPrecedence(msg.Receiver, precedenceUnary)
	f.write(" ")
	f.write(msg.Selector)
}

func (f *formatter) formatBinaryMessage(msg *compiler.BinaryMessage) {
	f.formatExprPrecedence(msg.Receiver, precedenceBinary)
	f.write(" " + msg.Selector + " ")
	f.formatExprPrecedence(msg.Argument, precedenceUnary)
}

func (f *formatter) formatKeywordMessage(msg *compiler.KeywordMessage) {
	f.formatExprPrecedence(msg.Receiver, precedenceBinary)

	keywords := msg.Keywords
	args := msg.Arguments

	// Measure the full message on one line
	receiverStr := exprToStringPrec(msg.Receiver, precedenceBinary)
	totalLen := f.indent*2 + len(receiverStr)
	for i, kw := range keywords {
		argStr := exprToString(args[i])
		totalLen += 1 + len(kw) + 1 + len(argStr)
	}

	if totalLen <= maxLineWidth || len(keywords) == 1 {
		// Single-line: receiver kw1: arg1 kw2: arg2
		for i, kw := range keywords {
			f.write(" ")
			f.write(kw)
			f.write(" ")
			f.formatExprPrecedence(args[i], precedenceBinary)
		}
	} else {
		// Multi-line: wrap each keyword on its own line, aligned
		// First keyword on same line as receiver
		f.write(" ")
		f.write(keywords[0])
		f.write(" ")
		f.formatExprPrecedence(args[0], precedenceBinary)

		// Remaining keywords indented to align
		for i := 1; i < len(keywords); i++ {
			f.newline()
			f.writeIndent()
			f.write("  ") // extra indent for continuation
			f.write(keywords[i])
			f.write(" ")
			f.formatExprPrecedence(args[i], precedenceBinary)
		}
	}
}

func (f *formatter) formatCascade(cascade *compiler.Cascade) {
	// Format receiver
	// The first message acts on the receiver
	f.formatExpr(cascade.Receiver)

	for i, msg := range cascade.Messages {
		if i > 0 {
			f.write(";")
			f.newline()
			f.writeIndent()
			f.write("  ") // extra indent for cascade continuation
		}
		f.formatCascadedMessage(msg)
	}
}

func (f *formatter) formatCascadedMessage(msg compiler.CascadedMessage) {
	switch msg.Type {
	case compiler.UnaryMsg:
		f.write(" ")
		f.write(msg.Selector)
	case compiler.BinaryMsg:
		f.write(" " + msg.Selector + " ")
		f.formatExprPrecedence(msg.Arguments[0], precedenceUnary)
	case compiler.KeywordMsg:
		for i, kw := range msg.Keywords {
			f.write(" ")
			f.write(kw)
			f.write(" ")
			f.formatExprPrecedence(msg.Arguments[i], precedenceBinary)
		}
	}
}

// ---------------------------------------------------------------------------
// Block formatting
// ---------------------------------------------------------------------------

func (f *formatter) formatBlock(block *compiler.Block) {
	// Try single-line first
	singleLine := blockToSingleLine(block)
	isMultiLine := len(block.Statements) > 1 ||
		(len(block.Statements) == 1 && isComplexExpr(block.Statements[0])) ||
		(f.indent*2+len(singleLine) > maxLineWidth)

	if !isMultiLine {
		f.write(singleLine)
	} else {
		// Multi-line block
		f.write("[")
		if len(block.Parameters) > 0 {
			for _, p := range block.Parameters {
				f.write(":" + p + " ")
			}
			f.write("|")
		}
		f.newline()

		f.indent++

		if len(block.Temps) > 0 {
			f.writeIndent()
			f.write("| ")
			f.write(strings.Join(block.Temps, " "))
			f.writeln(" |")
		}

		for i, stmt := range block.Statements {
			f.writeIndent()
			f.formatStmt(stmt)
			if i < len(block.Statements)-1 {
				f.write(".")
			}
			f.newline()
		}

		f.indent--
		f.writeIndent()
		f.write("]")
	}
}

// blockToSingleLine formats a block as a single line string (for length checking).
func blockToSingleLine(block *compiler.Block) string {
	var sb strings.Builder
	sb.WriteString("[")
	if len(block.Parameters) > 0 {
		for _, p := range block.Parameters {
			sb.WriteString(":" + p + " ")
		}
		sb.WriteString("| ")
	}
	if len(block.Temps) > 0 {
		sb.WriteString("| ")
		sb.WriteString(strings.Join(block.Temps, " "))
		sb.WriteString(" | ")
	}
	for i, stmt := range block.Statements {
		if i > 0 {
			sb.WriteString(". ")
		}
		sb.WriteString(stmtToString(stmt))
	}
	sb.WriteString("]")
	return sb.String()
}

// isComplexExpr returns true if a statement is "complex enough" to warrant
// multi-line block formatting. Cascades and very long expressions trigger this.
func isComplexExpr(stmt compiler.Stmt) bool {
	exprStmt, ok := stmt.(*compiler.ExprStmt)
	if !ok {
		// Return statements can also be complex
		if ret, isRet := stmt.(*compiler.Return); isRet {
			return len(exprToString(ret.Value)) > 70
		}
		return false
	}

	switch exprStmt.Expr.(type) {
	case *compiler.Cascade:
		return true
	default:
		return false
	}
}

// ---------------------------------------------------------------------------
// Precedence helpers (for parenthesization)
// ---------------------------------------------------------------------------

const (
	precedenceCascade = 0 // cascade has lowest precedence
	precedenceKeyword = 1
	precedenceBinary  = 2
	precedenceUnary   = 3
	precedencePrimary = 4
)

func exprPrecedence(expr compiler.Expr) int {
	switch expr.(type) {
	case *compiler.Cascade:
		return precedenceCascade
	case *compiler.KeywordMessage:
		return precedenceKeyword
	case *compiler.Assignment:
		return precedenceKeyword // assignment is like keyword level
	case *compiler.BinaryMessage:
		return precedenceBinary
	case *compiler.UnaryMessage:
		return precedenceUnary
	default:
		return precedencePrimary
	}
}

// formatExprPrecedence formats an expression, adding parentheses if the
// expression has lower precedence than the required context.
func (f *formatter) formatExprPrecedence(expr compiler.Expr, minPrecedence int) {
	if exprPrecedence(expr) < minPrecedence {
		f.write("(")
		f.formatExpr(expr)
		f.write(")")
	} else {
		f.formatExpr(expr)
	}
}

// ---------------------------------------------------------------------------
// Symbol formatting
// ---------------------------------------------------------------------------

func (f *formatter) formatSymbol(sym *compiler.SymbolLiteral) {
	val := sym.Value
	// Determine if the symbol needs quoting
	if needsQuoting(val) {
		f.write("#'" + escapeString(val) + "'")
	} else {
		f.write("#" + val)
	}
}

// needsQuoting returns true if a symbol value requires the #'...' syntax.
func needsQuoting(s string) bool {
	if s == "" {
		return true
	}

	// Check if it's a valid bare identifier/keyword symbol (e.g. #foo, #at:put:)
	// Must start with letter/underscore, contain letters/digits/underscores,
	// and optionally colons (for keyword symbols).
	if isLetterRune(rune(s[0])) || s[0] == '_' {
		for _, r := range s {
			if !(isLetterRune(r) || isDigitRune(r) || r == '_' || r == ':') {
				return true
			}
		}
		return false
	}

	// Check if it's a valid binary symbol (e.g. #+, #>=, #~=)
	allBinary := true
	for _, r := range s {
		if !isBinaryRune(r) {
			allBinary = false
			break
		}
	}
	if allBinary {
		return false
	}

	// Everything else needs quoting
	return true
}

// formatLiteralArrayElement formats an element inside #(...).
// In literal arrays, bare identifiers become symbols.
func (f *formatter) formatLiteralArrayElement(expr compiler.Expr) {
	switch e := expr.(type) {
	case *compiler.SymbolLiteral:
		// In literal arrays, symbols can appear as bare identifiers
		f.write(e.Value)
	case *compiler.ArrayLiteral:
		// Nested literal array
		f.write("#(")
		for i, elem := range e.Elements {
			if i > 0 {
				f.write(" ")
			}
			f.formatLiteralArrayElement(elem)
		}
		f.write(")")
	default:
		f.formatExpr(expr)
	}
}

// ---------------------------------------------------------------------------
// Helper functions
// ---------------------------------------------------------------------------

// escapeString escapes single quotes in a Smalltalk string.
func escapeString(s string) string {
	return strings.ReplaceAll(s, "'", "''")
}

// formatFloat produces a canonical float string.
func formatFloat(v float64) string {
	s := fmt.Sprintf("%g", v)
	// Ensure there's a decimal point
	if !strings.Contains(s, ".") && !strings.Contains(s, "e") && !strings.Contains(s, "E") {
		s += ".0"
	}
	return s
}

func isLetterRune(r rune) bool {
	return (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z')
}

func isDigitRune(r rune) bool {
	return r >= '0' && r <= '9'
}

func isBinaryRune(r rune) bool {
	switch r {
	case '+', '-', '*', '/', '\\', '~', '<', '>', '=', '@', '%', '|', '&', '?', '!', ',':
		return true
	}
	return false
}

// exprToString formats an expression into a string (for length measurement).
func exprToString(expr compiler.Expr) string {
	f := &formatter{indent: 0, buf: &strings.Builder{}}
	f.formatExpr(expr)
	return f.buf.String()
}

// exprToStringPrec formats an expression with precedence wrapping to a string.
func exprToStringPrec(expr compiler.Expr, minPrec int) string {
	f := &formatter{indent: 0, buf: &strings.Builder{}}
	f.formatExprPrecedence(expr, minPrec)
	return f.buf.String()
}

// stmtToString formats a statement to a string (for length measurement).
func stmtToString(stmt compiler.Stmt) string {
	f := &formatter{indent: 0, buf: &strings.Builder{}}
	f.formatStmt(stmt)
	return f.buf.String()
}

// ---------------------------------------------------------------------------
// CLI command: mag fmt
// ---------------------------------------------------------------------------

func handleFmtCommand(args []string) {
	checkMode := false
	var files []string

	for _, arg := range args {
		if arg == "--check" {
			checkMode = true
		} else if arg == "--help" || arg == "-h" {
			fmt.Fprintf(os.Stderr, "Usage: mag fmt [--check] <files or directories...>\n\n")
			fmt.Fprintf(os.Stderr, "Format Maggie source files to canonical style.\n\n")
			fmt.Fprintf(os.Stderr, "Options:\n")
			fmt.Fprintf(os.Stderr, "  --check   Check formatting without modifying files.\n")
			fmt.Fprintf(os.Stderr, "            Exits with code 1 if any files need formatting.\n\n")
			fmt.Fprintf(os.Stderr, "If no files are given, formats all .mag files in the current directory.\n")
			os.Exit(0)
		} else {
			files = append(files, arg)
		}
	}

	// Default: current directory
	if len(files) == 0 {
		files = []string{"."}
	}

	// Resolve files/directories to a list of .mag files
	magFiles, err := collectMagFiles(files)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	if len(magFiles) == 0 {
		fmt.Fprintf(os.Stderr, "No .mag files found\n")
		os.Exit(0)
	}

	anyChanged := false
	for _, path := range magFiles {
		changed, err := formatFile(path, checkMode)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error formatting %s: %v\n", path, err)
			os.Exit(1)
		}
		if changed {
			anyChanged = true
		}
	}

	if checkMode && anyChanged {
		os.Exit(1)
	}
}

// formatFile formats a single .mag file.
// In check mode, returns true if the file would be changed.
// Otherwise, rewrites the file in place and returns true if it changed.
func formatFile(path string, checkMode bool) (bool, error) {
	content, err := os.ReadFile(path)
	if err != nil {
		return false, err
	}

	original := string(content)
	formatted, err := Format(original)
	if err != nil {
		return false, fmt.Errorf("parse error: %w", err)
	}

	if original == formatted {
		return false, nil
	}

	if checkMode {
		fmt.Printf("would format: %s\n", path)
		return true, nil
	}

	// Write back
	err = os.WriteFile(path, []byte(formatted), 0644)
	if err != nil {
		return false, err
	}

	fmt.Printf("formatted: %s\n", path)
	return true, nil
}

// collectMagFiles resolves paths to a flat list of .mag file paths.
func collectMagFiles(paths []string) ([]string, error) {
	var result []string

	for _, p := range paths {
		abs, err := filepath.Abs(p)
		if err != nil {
			return nil, fmt.Errorf("invalid path %q: %w", p, err)
		}

		info, err := os.Stat(abs)
		if err != nil {
			return nil, fmt.Errorf("cannot access %q: %w", abs, err)
		}

		if info.IsDir() {
			err := filepath.Walk(abs, func(path string, fi os.FileInfo, err error) error {
				if err != nil {
					return err
				}
				if !fi.IsDir() && strings.HasSuffix(path, ".mag") {
					result = append(result, path)
				}
				return nil
			})
			if err != nil {
				return nil, err
			}
		} else {
			if strings.HasSuffix(abs, ".mag") {
				result = append(result, abs)
			} else {
				return nil, fmt.Errorf("%q is not a .mag file", abs)
			}
		}
	}

	return result, nil
}
