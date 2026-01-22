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
		{"self", ""},    // Self is its own type
		{"super", ""},   // Super is its own type
		{"nil", ""},     // NilLiteral is its own type
		{"true", ""},    // TrueLiteral is its own type
		{"false", ""},   // FalseLiteral is its own type
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
		case "nil":
			if _, ok := expr.(*NilLiteral); !ok {
				t.Errorf("parse %q: expected NilLiteral, got %T", tc.input, expr)
			}
		case "true":
			if _, ok := expr.(*TrueLiteral); !ok {
				t.Errorf("parse %q: expected TrueLiteral, got %T", tc.input, expr)
			}
		case "false":
			if _, ok := expr.(*FalseLiteral); !ok {
				t.Errorf("parse %q: expected FalseLiteral, got %T", tc.input, expr)
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

// ---------------------------------------------------------------------------
// Source file parsing tests (Trashtalk-style class definitions)
// ---------------------------------------------------------------------------

func TestParserSourceFileSimpleClass(t *testing.T) {
	input := `SmallInteger subclass: Object

  method: negated [
    ^0 - self
  ]
`
	p := NewParser(input)
	sf := p.ParseSourceFile()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	if len(sf.Classes) != 1 {
		t.Fatalf("expected 1 class, got %d", len(sf.Classes))
	}

	cls := sf.Classes[0]
	if cls.Name != "SmallInteger" {
		t.Errorf("class name = %q, want SmallInteger", cls.Name)
	}
	if cls.Superclass != "Object" {
		t.Errorf("superclass = %q, want Object", cls.Superclass)
	}
	if len(cls.Methods) != 1 {
		t.Fatalf("expected 1 method, got %d", len(cls.Methods))
	}
	if cls.Methods[0].Selector != "negated" {
		t.Errorf("method selector = %q, want negated", cls.Methods[0].Selector)
	}
}

func TestParserSourceFileClassWithInstanceVars(t *testing.T) {
	input := `Counter subclass: Object
  instanceVars: value step

  method: initialize [
    value := 0.
    step := 1
  ]

  method: increment [
    value := value + step
  ]
`
	p := NewParser(input)
	sf := p.ParseSourceFile()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	if len(sf.Classes) != 1 {
		t.Fatalf("expected 1 class, got %d", len(sf.Classes))
	}

	cls := sf.Classes[0]
	if cls.Name != "Counter" {
		t.Errorf("class name = %q, want Counter", cls.Name)
	}
	if len(cls.InstanceVariables) != 2 {
		t.Fatalf("expected 2 instance vars, got %d", len(cls.InstanceVariables))
	}
	if cls.InstanceVariables[0] != "value" || cls.InstanceVariables[1] != "step" {
		t.Errorf("instance vars = %v, want [value step]", cls.InstanceVariables)
	}
	if len(cls.Methods) != 2 {
		t.Errorf("expected 2 methods, got %d", len(cls.Methods))
	}
}

func TestParserSourceFileClassWithTraitInclude(t *testing.T) {
	input := `MyClass subclass: Object
  include: Debuggable
  include: Persistable

  method: foo [
    ^42
  ]
`
	p := NewParser(input)
	sf := p.ParseSourceFile()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	if len(sf.Classes) != 1 {
		t.Fatalf("expected 1 class, got %d", len(sf.Classes))
	}

	cls := sf.Classes[0]
	if len(cls.Traits) != 2 {
		t.Fatalf("expected 2 traits, got %d", len(cls.Traits))
	}
	if cls.Traits[0] != "Debuggable" || cls.Traits[1] != "Persistable" {
		t.Errorf("traits = %v, want [Debuggable Persistable]", cls.Traits)
	}
}

func TestParserSourceFileKeywordMethod(t *testing.T) {
	input := `Array subclass: Object

  method: at: index put: value [
    ^self primAt: index put: value
  ]
`
	p := NewParser(input)
	sf := p.ParseSourceFile()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	if len(sf.Classes) != 1 {
		t.Fatalf("expected 1 class, got %d", len(sf.Classes))
	}

	cls := sf.Classes[0]
	if len(cls.Methods) != 1 {
		t.Fatalf("expected 1 method, got %d", len(cls.Methods))
	}

	method := cls.Methods[0]
	if method.Selector != "at:put:" {
		t.Errorf("selector = %q, want at:put:", method.Selector)
	}
	if len(method.Parameters) != 2 {
		t.Fatalf("expected 2 params, got %d", len(method.Parameters))
	}
	if method.Parameters[0] != "index" || method.Parameters[1] != "value" {
		t.Errorf("params = %v, want [index value]", method.Parameters)
	}
}

func TestParserSourceFileBinaryMethod(t *testing.T) {
	input := `SmallInteger subclass: Object

  method: + other [
    ^self primAdd: other
  ]
`
	p := NewParser(input)
	sf := p.ParseSourceFile()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	if len(sf.Classes) != 1 {
		t.Fatalf("expected 1 class, got %d", len(sf.Classes))
	}

	cls := sf.Classes[0]
	if len(cls.Methods) != 1 {
		t.Fatalf("expected 1 method, got %d", len(cls.Methods))
	}

	method := cls.Methods[0]
	if method.Selector != "+" {
		t.Errorf("selector = %q, want +", method.Selector)
	}
	if len(method.Parameters) != 1 || method.Parameters[0] != "other" {
		t.Errorf("params = %v, want [other]", method.Parameters)
	}
}

func TestParserSourceFileClassMethod(t *testing.T) {
	input := `Point subclass: Object
  instanceVars: x y

  classMethod: x: xVal y: yVal [
    | p |
    p := self new.
    p setX: xVal y: yVal.
    ^p
  ]

  method: setX: xVal y: yVal [
    x := xVal.
    y := yVal
  ]
`
	p := NewParser(input)
	sf := p.ParseSourceFile()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	if len(sf.Classes) != 1 {
		t.Fatalf("expected 1 class, got %d", len(sf.Classes))
	}

	cls := sf.Classes[0]
	if len(cls.ClassMethods) != 1 {
		t.Fatalf("expected 1 class method, got %d", len(cls.ClassMethods))
	}
	if len(cls.Methods) != 1 {
		t.Fatalf("expected 1 instance method, got %d", len(cls.Methods))
	}

	classMethod := cls.ClassMethods[0]
	if classMethod.Selector != "x:y:" {
		t.Errorf("class method selector = %q, want x:y:", classMethod.Selector)
	}
}

func TestParserSourceFileTrait(t *testing.T) {
	input := `Debuggable trait

  method: inspect [
    ^'Instance of: ', self class name
  ]

  method: log: message [
    Transcript show: message
  ]
`
	p := NewParser(input)
	sf := p.ParseSourceFile()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	if len(sf.Traits) != 1 {
		t.Fatalf("expected 1 trait, got %d", len(sf.Traits))
	}

	trait := sf.Traits[0]
	if trait.Name != "Debuggable" {
		t.Errorf("trait name = %q, want Debuggable", trait.Name)
	}
	if len(trait.Methods) != 2 {
		t.Fatalf("expected 2 methods, got %d", len(trait.Methods))
	}
	if trait.Methods[0].Selector != "inspect" {
		t.Errorf("first method = %q, want inspect", trait.Methods[0].Selector)
	}
	if trait.Methods[1].Selector != "log:" {
		t.Errorf("second method = %q, want log:", trait.Methods[1].Selector)
	}
}

func TestParserSourceFileMultipleClasses(t *testing.T) {
	input := `True subclass: Boolean

  method: ifTrue: trueBlock [
    ^trueBlock value
  ]

False subclass: Boolean

  method: ifTrue: trueBlock [
    ^nil
  ]
`
	p := NewParser(input)
	sf := p.ParseSourceFile()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	if len(sf.Classes) != 2 {
		t.Fatalf("expected 2 classes, got %d", len(sf.Classes))
	}

	if sf.Classes[0].Name != "True" {
		t.Errorf("first class = %q, want True", sf.Classes[0].Name)
	}
	if sf.Classes[1].Name != "False" {
		t.Errorf("second class = %q, want False", sf.Classes[1].Name)
	}
}

func TestParserSourceFileWithHashComments(t *testing.T) {
	input := `# SmallInteger.mag - integer operations

SmallInteger subclass: Object

  # Basic arithmetic
  method: negated [
    ^0 - self
  ]

  method: abs [
    # Return absolute value
    self < 0 ifTrue: [^self negated].
    ^self
  ]
`
	p := NewParser(input)
	sf := p.ParseSourceFile()
	if len(p.Errors()) > 0 {
		t.Fatalf("parse errors: %v", p.Errors())
	}

	if len(sf.Classes) != 1 {
		t.Fatalf("expected 1 class, got %d", len(sf.Classes))
	}

	cls := sf.Classes[0]
	if cls.Name != "SmallInteger" {
		t.Errorf("class name = %q, want SmallInteger", cls.Name)
	}
	if len(cls.Methods) != 2 {
		t.Errorf("expected 2 methods, got %d", len(cls.Methods))
	}
}
