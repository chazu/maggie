package compiler

import (
	"testing"
)

func TestParserLiterals(t *testing.T) {
	tests := []struct {
		input string
		check func(Expr) bool
		desc  string
	}{
		{"42", func(e Expr) bool { return e.(*IntLiteral).Value == 42 }, "integer"},
		{"-5", func(e Expr) bool { return e.(*IntLiteral).Value == -5 }, "negative integer"},
		{"3.14", func(e Expr) bool { return e.(*FloatLiteral).Value == 3.14 }, "float"},
		{"'hello'", func(e Expr) bool { return e.(*StringLiteral).Value == "hello" }, "string"},
		{"#foo", func(e Expr) bool { return e.(*SymbolLiteral).Value == "foo" }, "symbol"},
		{"$a", func(e Expr) bool { return e.(*CharLiteral).Value == 'a' }, "character"},
	}

	for _, tc := range tests {
		p := NewParser(tc.input)
		expr := p.ParseExpression()
		if len(p.Errors()) > 0 {
			t.Errorf("%s: parse errors: %v", tc.desc, p.Errors())
			continue
		}
		if expr == nil {
			t.Errorf("%s: nil expression", tc.desc)
			continue
		}
		if !tc.check(expr) {
			t.Errorf("%s: check failed for %q", tc.desc, tc.input)
		}
	}
}

func TestParserVariables(t *testing.T) {
	tests := []struct {
		input string
		name  string
	}{
		{"foo", "foo"},
		{"self", ""},   // Self is its own type
		{"super", ""},  // Super is its own type
		{"nil", "nil"}, // nil/true/false are variables
		{"true", "true"},
		{"false", "false"},
	}

	for _, tc := range tests {
		p := NewParser(tc.input)
		expr := p.ParseExpression()
		if len(p.Errors()) > 0 {
			t.Errorf("parse %q: errors: %v", tc.input, p.Errors())
			continue
		}

		switch tc.input {
		case "self":
			if _, ok := expr.(*Self); !ok {
				t.Errorf("parse %q: expected Self, got %T", tc.input, expr)
			}
		case "super":
			if _, ok := expr.(*Super); !ok {
				t.Errorf("parse %q: expected Super, got %T", tc.input, expr)
			}
		default:
			v, ok := expr.(*Variable)
			if !ok {
				t.Errorf("parse %q: expected Variable, got %T", tc.input, expr)
				continue
			}
			if v.Name != tc.name {
				t.Errorf("parse %q: name = %q, want %q", tc.input, v.Name, tc.name)
			}
		}
	}
}

func TestParserUnaryMessage(t *testing.T) {
	input := "obj foo"
	p := NewParser(input)
	expr := p.ParseExpression()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	msg, ok := expr.(*UnaryMessage)
	if !ok {
		t.Fatalf("expected UnaryMessage, got %T", expr)
	}
	if msg.Selector != "foo" {
		t.Errorf("selector = %q, want %q", msg.Selector, "foo")
	}

	recv, ok := msg.Receiver.(*Variable)
	if !ok {
		t.Fatalf("receiver: expected Variable, got %T", msg.Receiver)
	}
	if recv.Name != "obj" {
		t.Errorf("receiver name = %q, want %q", recv.Name, "obj")
	}
}

func TestParserUnaryChain(t *testing.T) {
	input := "obj foo bar baz"
	p := NewParser(input)
	expr := p.ParseExpression()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	// Should be ((obj foo) bar) baz
	msg, ok := expr.(*UnaryMessage)
	if !ok {
		t.Fatalf("expected UnaryMessage, got %T", expr)
	}
	if msg.Selector != "baz" {
		t.Errorf("outer selector = %q, want %q", msg.Selector, "baz")
	}

	msg2, ok := msg.Receiver.(*UnaryMessage)
	if !ok {
		t.Fatalf("expected UnaryMessage, got %T", msg.Receiver)
	}
	if msg2.Selector != "bar" {
		t.Errorf("middle selector = %q, want %q", msg2.Selector, "bar")
	}
}

func TestParserBinaryMessage(t *testing.T) {
	input := "1 + 2"
	p := NewParser(input)
	expr := p.ParseExpression()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	msg, ok := expr.(*BinaryMessage)
	if !ok {
		t.Fatalf("expected BinaryMessage, got %T", expr)
	}
	if msg.Selector != "+" {
		t.Errorf("selector = %q, want %q", msg.Selector, "+")
	}

	recv, ok := msg.Receiver.(*IntLiteral)
	if !ok || recv.Value != 1 {
		t.Errorf("receiver = %v, want 1", msg.Receiver)
	}

	arg, ok := msg.Argument.(*IntLiteral)
	if !ok || arg.Value != 2 {
		t.Errorf("argument = %v, want 2", msg.Argument)
	}
}

func TestParserBinaryChain(t *testing.T) {
	input := "1 + 2 * 3"
	p := NewParser(input)
	expr := p.ParseExpression()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	// Smalltalk: left associative, so ((1 + 2) * 3)
	msg, ok := expr.(*BinaryMessage)
	if !ok {
		t.Fatalf("expected BinaryMessage, got %T", expr)
	}
	if msg.Selector != "*" {
		t.Errorf("outer selector = %q, want *", msg.Selector)
	}

	inner, ok := msg.Receiver.(*BinaryMessage)
	if !ok {
		t.Fatalf("expected BinaryMessage for inner, got %T", msg.Receiver)
	}
	if inner.Selector != "+" {
		t.Errorf("inner selector = %q, want +", inner.Selector)
	}
}

func TestParserKeywordMessage(t *testing.T) {
	input := "arr at: 1"
	p := NewParser(input)
	expr := p.ParseExpression()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	msg, ok := expr.(*KeywordMessage)
	if !ok {
		t.Fatalf("expected KeywordMessage, got %T", expr)
	}
	if msg.Selector != "at:" {
		t.Errorf("selector = %q, want at:", msg.Selector)
	}
	if len(msg.Arguments) != 1 {
		t.Errorf("arguments count = %d, want 1", len(msg.Arguments))
	}
}

func TestParserMultiKeywordMessage(t *testing.T) {
	input := "arr at: 1 put: 42"
	p := NewParser(input)
	expr := p.ParseExpression()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	msg, ok := expr.(*KeywordMessage)
	if !ok {
		t.Fatalf("expected KeywordMessage, got %T", expr)
	}
	if msg.Selector != "at:put:" {
		t.Errorf("selector = %q, want at:put:", msg.Selector)
	}
	if len(msg.Arguments) != 2 {
		t.Errorf("arguments count = %d, want 2", len(msg.Arguments))
	}
	if len(msg.Keywords) != 2 {
		t.Errorf("keywords count = %d, want 2", len(msg.Keywords))
	}
}

func TestParserMessagePrecedence(t *testing.T) {
	// Unary binds tighter than binary binds tighter than keyword
	input := "1 + 2 negated"
	p := NewParser(input)
	expr := p.ParseExpression()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	// Should be 1 + (2 negated)
	msg, ok := expr.(*BinaryMessage)
	if !ok {
		t.Fatalf("expected BinaryMessage, got %T", expr)
	}
	if msg.Selector != "+" {
		t.Errorf("selector = %q, want +", msg.Selector)
	}

	unary, ok := msg.Argument.(*UnaryMessage)
	if !ok {
		t.Fatalf("expected UnaryMessage argument, got %T", msg.Argument)
	}
	if unary.Selector != "negated" {
		t.Errorf("unary selector = %q, want negated", unary.Selector)
	}
}

func TestParserParenExpr(t *testing.T) {
	input := "(1 + 2) * 3"
	p := NewParser(input)
	expr := p.ParseExpression()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	msg, ok := expr.(*BinaryMessage)
	if !ok {
		t.Fatalf("expected BinaryMessage, got %T", expr)
	}
	if msg.Selector != "*" {
		t.Errorf("selector = %q, want *", msg.Selector)
	}

	inner, ok := msg.Receiver.(*BinaryMessage)
	if !ok {
		t.Fatalf("receiver: expected BinaryMessage, got %T", msg.Receiver)
	}
	if inner.Selector != "+" {
		t.Errorf("inner selector = %q, want +", inner.Selector)
	}
}

func TestParserAssignment(t *testing.T) {
	input := "x := 42"
	p := NewParser(input)
	expr := p.ParseExpression()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	assign, ok := expr.(*Assignment)
	if !ok {
		t.Fatalf("expected Assignment, got %T", expr)
	}
	if assign.Variable != "x" {
		t.Errorf("variable = %q, want x", assign.Variable)
	}

	val, ok := assign.Value.(*IntLiteral)
	if !ok || val.Value != 42 {
		t.Errorf("value = %v, want 42", assign.Value)
	}
}

func TestParserBlock(t *testing.T) {
	input := "[:x | x + 1]"
	p := NewParser(input)
	expr := p.ParseExpression()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	block, ok := expr.(*Block)
	if !ok {
		t.Fatalf("expected Block, got %T", expr)
	}
	if len(block.Parameters) != 1 || block.Parameters[0] != "x" {
		t.Errorf("parameters = %v, want [x]", block.Parameters)
	}
	if len(block.Statements) != 1 {
		t.Errorf("statements count = %d, want 1", len(block.Statements))
	}
}

func TestParserBlockMultipleParams(t *testing.T) {
	input := "[:a :b | a + b]"
	p := NewParser(input)
	expr := p.ParseExpression()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	block, ok := expr.(*Block)
	if !ok {
		t.Fatalf("expected Block, got %T", expr)
	}
	if len(block.Parameters) != 2 {
		t.Errorf("parameters count = %d, want 2", len(block.Parameters))
	}
}

func TestParserBlockNoParams(t *testing.T) {
	input := "[42]"
	p := NewParser(input)
	expr := p.ParseExpression()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	block, ok := expr.(*Block)
	if !ok {
		t.Fatalf("expected Block, got %T", expr)
	}
	if len(block.Parameters) != 0 {
		t.Errorf("parameters count = %d, want 0", len(block.Parameters))
	}
}

func TestParserLiteralArray(t *testing.T) {
	input := "#(1 2 3)"
	p := NewParser(input)
	expr := p.ParseExpression()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	arr, ok := expr.(*ArrayLiteral)
	if !ok {
		t.Fatalf("expected ArrayLiteral, got %T", expr)
	}
	if len(arr.Elements) != 3 {
		t.Errorf("elements count = %d, want 3", len(arr.Elements))
	}
}

func TestParserLiteralArraySymbols(t *testing.T) {
	input := "#(foo bar baz)"
	p := NewParser(input)
	expr := p.ParseExpression()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	arr, ok := expr.(*ArrayLiteral)
	if !ok {
		t.Fatalf("expected ArrayLiteral, got %T", expr)
	}
	if len(arr.Elements) != 3 {
		t.Errorf("elements count = %d, want 3", len(arr.Elements))
	}

	// Elements should be symbols
	for i, elem := range arr.Elements {
		sym, ok := elem.(*SymbolLiteral)
		if !ok {
			t.Errorf("element[%d]: expected SymbolLiteral, got %T", i, elem)
		} else if sym.Value == "" {
			t.Errorf("element[%d]: empty symbol", i)
		}
	}
}

func TestParserDynamicArray(t *testing.T) {
	input := "{1. 2. 3}"
	p := NewParser(input)
	expr := p.ParseExpression()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	arr, ok := expr.(*DynamicArray)
	if !ok {
		t.Fatalf("expected DynamicArray, got %T", expr)
	}
	if len(arr.Elements) != 3 {
		t.Errorf("elements count = %d, want 3", len(arr.Elements))
	}
}

func TestParserCascade(t *testing.T) {
	input := "obj msg1; msg2; msg3"
	p := NewParser(input)
	expr := p.ParseExpression()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	cascade, ok := expr.(*Cascade)
	if !ok {
		t.Fatalf("expected Cascade, got %T", expr)
	}
	if len(cascade.Messages) != 3 {
		t.Errorf("messages count = %d, want 3", len(cascade.Messages))
	}
}

func TestParserReturn(t *testing.T) {
	input := "^42"
	p := NewParser(input)
	stmt := p.ParseStatement()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	ret, ok := stmt.(*Return)
	if !ok {
		t.Fatalf("expected Return, got %T", stmt)
	}

	val, ok := ret.Value.(*IntLiteral)
	if !ok || val.Value != 42 {
		t.Errorf("return value = %v, want 42", ret.Value)
	}
}

func TestParserMethod(t *testing.T) {
	input := "increment: amount | result | result := value + amount. ^result"
	p := NewParser(input)
	method := p.ParseMethod()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}
	if method == nil {
		t.Fatal("nil method")
	}

	if method.Selector != "increment:" {
		t.Errorf("selector = %q, want increment:", method.Selector)
	}
	if len(method.Parameters) != 1 || method.Parameters[0] != "amount" {
		t.Errorf("parameters = %v, want [amount]", method.Parameters)
	}
	if len(method.Temps) != 1 || method.Temps[0] != "result" {
		t.Errorf("temps = %v, want [result]", method.Temps)
	}
	if len(method.Statements) != 2 {
		t.Errorf("statements count = %d, want 2", len(method.Statements))
	}
}

func TestParserUnaryMethod(t *testing.T) {
	input := "negated ^0 - self"
	p := NewParser(input)
	method := p.ParseMethod()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	if method.Selector != "negated" {
		t.Errorf("selector = %q, want negated", method.Selector)
	}
	if len(method.Parameters) != 0 {
		t.Errorf("parameters count = %d, want 0", len(method.Parameters))
	}
}

func TestParserBinaryMethod(t *testing.T) {
	input := "+ other ^self add: other"
	p := NewParser(input)
	method := p.ParseMethod()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	if method.Selector != "+" {
		t.Errorf("selector = %q, want +", method.Selector)
	}
	if len(method.Parameters) != 1 || method.Parameters[0] != "other" {
		t.Errorf("parameters = %v, want [other]", method.Parameters)
	}
}

func TestParserStatements(t *testing.T) {
	input := "x := 1. y := 2. ^x + y"
	p := NewParser(input)
	stmts := p.ParseStatements()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	if len(stmts) != 3 {
		t.Errorf("statements count = %d, want 3", len(stmts))
	}
}

func TestParserIfTrueIfFalse(t *testing.T) {
	input := "(x > 0) ifTrue: [1] ifFalse: [-1]"
	p := NewParser(input)
	expr := p.ParseExpression()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	msg, ok := expr.(*KeywordMessage)
	if !ok {
		t.Fatalf("expected KeywordMessage, got %T", expr)
	}
	if msg.Selector != "ifTrue:ifFalse:" {
		t.Errorf("selector = %q, want ifTrue:ifFalse:", msg.Selector)
	}
}
