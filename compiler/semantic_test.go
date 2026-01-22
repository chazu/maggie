package compiler

import (
	"strings"
	"testing"
)

func TestSemanticAnalyzer_UndefinedVariable(t *testing.T) {
	source := `doIt
    ^undefinedVar`

	parser := NewParser(source)
	method := parser.ParseMethod()
	if len(parser.Errors()) > 0 {
		t.Fatalf("parse errors: %v", parser.Errors())
	}

	warnings := Analyze(method, nil)

	// Should have a warning about undefined variable
	found := false
	for _, w := range warnings {
		if strings.Contains(w, "undefinedVar") && strings.Contains(w, "may be undefined") {
			found = true
			break
		}
	}
	if !found {
		t.Errorf("expected warning about undefined variable, got: %v", warnings)
	}
}

func TestSemanticAnalyzer_DefinedVariable(t *testing.T) {
	source := `doIt: x
    | y |
    y := x + 1.
    ^y`

	parser := NewParser(source)
	method := parser.ParseMethod()
	if len(parser.Errors()) > 0 {
		t.Fatalf("parse errors: %v", parser.Errors())
	}

	warnings := Analyze(method, nil)

	// Should have no warnings about x or y
	for _, w := range warnings {
		if strings.Contains(w, "'x'") || strings.Contains(w, "'y'") {
			t.Errorf("unexpected warning: %s", w)
		}
	}
}

func TestSemanticAnalyzer_InstanceVariable(t *testing.T) {
	source := `getValue
    ^value`

	parser := NewParser(source)
	method := parser.ParseMethod()
	if len(parser.Errors()) > 0 {
		t.Fatalf("parse errors: %v", parser.Errors())
	}

	// Without instance variables, should warn
	warnings := Analyze(method, nil)
	foundWarning := false
	for _, w := range warnings {
		if strings.Contains(w, "value") && strings.Contains(w, "may be undefined") {
			foundWarning = true
			break
		}
	}
	if !foundWarning {
		t.Errorf("expected warning about undefined 'value', got: %v", warnings)
	}

	// With instance variable, should not warn
	warnings = Analyze(method, []string{"value"})
	for _, w := range warnings {
		if strings.Contains(w, "'value'") {
			t.Errorf("unexpected warning with ivar defined: %s", w)
		}
	}
}

func TestSemanticAnalyzer_KnownGlobals(t *testing.T) {
	source := `doIt
    ^Object new`

	parser := NewParser(source)
	method := parser.ParseMethod()
	if len(parser.Errors()) > 0 {
		t.Fatalf("parse errors: %v", parser.Errors())
	}

	warnings := Analyze(method, nil)

	// Should not warn about Object (known global)
	for _, w := range warnings {
		if strings.Contains(w, "'Object'") {
			t.Errorf("unexpected warning about known global: %s", w)
		}
	}
}

func TestSemanticAnalyzer_UnreachableCode(t *testing.T) {
	source := `doIt
    ^1.
    ^2`

	parser := NewParser(source)
	method := parser.ParseMethod()
	if len(parser.Errors()) > 0 {
		t.Fatalf("parse errors: %v", parser.Errors())
	}

	warnings := Analyze(method, nil)

	// Should warn about unreachable code
	found := false
	for _, w := range warnings {
		if strings.Contains(w, "unreachable") {
			found = true
			break
		}
	}
	if !found {
		t.Errorf("expected warning about unreachable code, got: %v", warnings)
	}
}

func TestSemanticAnalyzer_AssignToReserved(t *testing.T) {
	// Note: The parser handles reserved words like 'self' at the syntax level,
	// so "self := 1" won't create an Assignment node. This test verifies
	// that if someone managed to create an Assignment to a reserved word,
	// the semantic analyzer would catch it.

	// Create an Assignment node directly to test the semantic check
	method := &MethodDef{
		Selector: "doIt",
		Statements: []Stmt{
			&ExprStmt{
				Expr: &Assignment{
					Variable: "self",
					Value:    &IntLiteral{Value: 1},
				},
			},
		},
	}

	warnings := Analyze(method, nil)

	// Should error about assigning to self
	found := false
	for _, w := range warnings {
		if strings.Contains(w, "cannot assign") && strings.Contains(w, "self") {
			found = true
			break
		}
	}
	if !found {
		t.Errorf("expected error about assigning to self, got: %v", warnings)
	}
}

func TestSemanticAnalyzer_BlockVariable(t *testing.T) {
	source := `doIt
    | x |
    x := 5.
    [:y | x + y] value: 3`

	parser := NewParser(source)
	method := parser.ParseMethod()
	if len(parser.Errors()) > 0 {
		t.Fatalf("parse errors: %v", parser.Errors())
	}

	warnings := Analyze(method, nil)

	// Should not warn about x or y (both defined)
	for _, w := range warnings {
		if strings.Contains(w, "'x'") || strings.Contains(w, "'y'") {
			t.Errorf("unexpected warning: %s", w)
		}
	}
}

func TestSemanticAnalyzer_PositionInErrors(t *testing.T) {
	source := `doIt
    ^undefinedVar`

	parser := NewParser(source)
	method := parser.ParseMethod()
	if len(parser.Errors()) > 0 {
		t.Fatalf("parse errors: %v", parser.Errors())
	}

	warnings := Analyze(method, nil)

	// Should include line and column information
	found := false
	for _, w := range warnings {
		if strings.Contains(w, "line") && strings.Contains(w, "column") {
			found = true
			break
		}
	}
	if !found {
		t.Errorf("expected position information in warning, got: %v", warnings)
	}
}
