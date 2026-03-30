package types

import (
	"strings"
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

func newTestInferrer() *Inferrer {
	rt := NewReturnTypeTable()
	protocols := NewProtocolRegistry()
	vmInst := vm.NewVM()
	return NewInferrer(rt, protocols, vmInst, false)
}

// Helper to parse a method body and return the MethodDef.
// Source should be a complete class definition.
func parseMethod(source string) (*compiler.MethodDef, string) {
	p := compiler.NewParser(source)
	sf := p.ParseSourceFile()
	if sf == nil || len(sf.Classes) == 0 || len(sf.Classes[0].Methods) == 0 {
		return nil, ""
	}
	return sf.Classes[0].Methods[0], sf.Classes[0].Name
}

func TestInferLiteralTypes(t *testing.T) {
	inf := newTestInferrer()

	tests := []struct {
		name   string
		source string
		want   string
	}{
		{
			"integer literal",
			`Foo subclass: Object method: test [ ^42 ]`,
			"SmallInteger",
		},
		{
			"float literal",
			`Foo subclass: Object method: test [ ^3.14 ]`,
			"Float",
		},
		{
			"string literal",
			`Foo subclass: Object method: test [ ^'hello' ]`,
			"String",
		},
		{
			"symbol literal",
			`Foo subclass: Object method: test [ ^#foo ]`,
			"Symbol",
		},
		{
			"true literal",
			`Foo subclass: Object method: test [ ^true ]`,
			"Boolean",
		},
		{
			"false literal",
			`Foo subclass: Object method: test [ ^false ]`,
			"Boolean",
		},
		{
			"nil literal",
			`Foo subclass: Object method: test [ ^nil ]`,
			"UndefinedObject",
		},
		{
			"array literal",
			`Foo subclass: Object method: test [ ^#(1 2 3) ]`,
			"Array",
		},
		{
			"dynamic array",
			`Foo subclass: Object method: test [ ^{1. 2. 3} ]`,
			"Array",
		},
		{
			"block",
			`Foo subclass: Object method: test [ ^[42] ]`,
			"Block",
		},
		{
			"self",
			`Foo subclass: Object method: test [ ^self ]`,
			"Foo",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			md, className := parseMethod(tc.source)
			if md == nil {
				t.Fatal("failed to parse method")
			}
			retType, diags := inf.InferMethod(className, md)
			if len(diags) > 0 {
				t.Errorf("unexpected diagnostics: %v", diags)
			}
			if retType.String() != tc.want {
				t.Errorf("inferred type = %s, want %s", retType, tc.want)
			}
		})
	}
}

func TestInferAssignmentPropagation(t *testing.T) {
	inf := newTestInferrer()

	source := `Foo subclass: Object
  method: test [
    | x |
    x := 42.
    ^x
  ]`
	md, className := parseMethod(source)
	if md == nil {
		t.Fatal("failed to parse method")
	}

	retType, diags := inf.InferMethod(className, md)
	if len(diags) > 0 {
		t.Errorf("unexpected diagnostics: %v", diags)
	}
	if retType.String() != "SmallInteger" {
		t.Errorf("expected SmallInteger, got %s", retType)
	}
}

func TestInferAssignmentChain(t *testing.T) {
	inf := newTestInferrer()

	source := `Foo subclass: Object
  method: test [
    | x y |
    x := 42.
    y := x.
    ^y
  ]`
	md, className := parseMethod(source)
	if md == nil {
		t.Fatal("failed to parse method")
	}

	retType, diags := inf.InferMethod(className, md)
	if len(diags) > 0 {
		t.Errorf("unexpected diagnostics: %v", diags)
	}
	if retType.String() != "SmallInteger" {
		t.Errorf("expected SmallInteger, got %s", retType)
	}
}

func TestInferReassignmentLastWriteWins(t *testing.T) {
	inf := newTestInferrer()

	source := `Foo subclass: Object
  method: test [
    | x |
    x := 42.
    x := 'hello'.
    ^x
  ]`
	md, className := parseMethod(source)
	if md == nil {
		t.Fatal("failed to parse method")
	}

	retType, diags := inf.InferMethod(className, md)
	if len(diags) > 0 {
		t.Errorf("unexpected diagnostics: %v", diags)
	}
	if retType.String() != "String" {
		t.Errorf("expected String after reassignment, got %s", retType)
	}
}

func TestInferMessageSendReturnType(t *testing.T) {
	inf := newTestInferrer()

	source := `Foo subclass: Object
  method: test [
    | x |
    x := 'hello' size.
    ^x
  ]`
	md, className := parseMethod(source)
	if md == nil {
		t.Fatal("failed to parse method")
	}

	retType, diags := inf.InferMethod(className, md)
	if len(diags) > 0 {
		t.Errorf("unexpected diagnostics: %v", diags)
	}
	if retType.String() != "SmallInteger" {
		t.Errorf("expected SmallInteger from 'hello' size, got %s", retType)
	}
}

func TestInferChainedSends(t *testing.T) {
	inf := newTestInferrer()

	// 'hello' size asString -> SmallInteger asString -> no entry for SmallInteger asString
	// But SmallInteger inherits from Object, so it should look up asString there
	// Actually, the built-in table has SmallInteger asString -> String
	source := `Foo subclass: Object
  method: test [
    ^'hello' size printString
  ]`
	md, className := parseMethod(source)
	if md == nil {
		t.Fatal("failed to parse method")
	}

	retType, diags := inf.InferMethod(className, md)
	if len(diags) > 0 {
		t.Errorf("unexpected diagnostics: %v", diags)
	}
	if retType.String() != "String" {
		t.Errorf("expected String from chained size printString, got %s", retType)
	}
}

func TestInferBinaryMessageReturnType(t *testing.T) {
	inf := newTestInferrer()

	source := `Foo subclass: Object
  method: test [
    ^3 + 4
  ]`
	md, className := parseMethod(source)
	if md == nil {
		t.Fatal("failed to parse method")
	}

	retType, diags := inf.InferMethod(className, md)
	if len(diags) > 0 {
		t.Errorf("unexpected diagnostics: %v", diags)
	}
	if retType.String() != "SmallInteger" {
		t.Errorf("expected SmallInteger from 3 + 4, got %s", retType)
	}
}

func TestInferComparisonReturnType(t *testing.T) {
	inf := newTestInferrer()

	source := `Foo subclass: Object
  method: test [
    ^3 < 4
  ]`
	md, className := parseMethod(source)
	if md == nil {
		t.Fatal("failed to parse method")
	}

	retType, diags := inf.InferMethod(className, md)
	if len(diags) > 0 {
		t.Errorf("unexpected diagnostics: %v", diags)
	}
	if retType.String() != "Boolean" {
		t.Errorf("expected Boolean from 3 < 4, got %s", retType)
	}
}

func TestInferDoesNotUnderstandWarning(t *testing.T) {
	inf := newTestInferrer()

	source := `Foo subclass: Object
  method: test [
    ^42 isEmpty
  ]`
	md, className := parseMethod(source)
	if md == nil {
		t.Fatal("failed to parse method")
	}

	_, diags := inf.InferMethod(className, md)
	if len(diags) == 0 {
		t.Fatal("expected 'does not understand' diagnostic")
	}
	found := false
	for _, d := range diags {
		if strings.Contains(d.Message, "does not understand") && strings.Contains(d.Message, "isEmpty") {
			found = true
		}
	}
	if !found {
		t.Errorf("expected 'does not understand #isEmpty' diagnostic, got: %v", diags)
	}
}

func TestInferDynamicSuppressesWarnings(t *testing.T) {
	inf := newTestInferrer()

	// Untyped parameter -> Dynamic -> no warning
	source := `Foo subclass: Object
  method: test: x [
    ^x isEmpty
  ]`
	md, className := parseMethod(source)
	if md == nil {
		t.Fatal("failed to parse method")
	}

	_, diags := inf.InferMethod(className, md)
	if len(diags) > 0 {
		t.Errorf("Dynamic receiver should suppress warnings, got: %v", diags)
	}
}

func TestInferUnboundVariableIsDynamic(t *testing.T) {
	inf := newTestInferrer()

	source := `Foo subclass: Object
  method: test [
    ^unknown
  ]`
	md, className := parseMethod(source)
	if md == nil {
		t.Fatal("failed to parse method")
	}

	retType, diags := inf.InferMethod(className, md)
	if len(diags) > 0 {
		t.Errorf("unbound variable should be Dynamic with no warnings, got: %v", diags)
	}
	if !IsDynamic(retType) {
		t.Errorf("expected Dynamic for unbound variable, got %s", retType)
	}
}

func TestInferParameterWithAnnotation(t *testing.T) {
	inf := newTestInferrer()

	source := `Foo subclass: Object
  method: test: x <SmallInteger> [
    ^x + 1
  ]`
	md, className := parseMethod(source)
	if md == nil {
		t.Fatal("failed to parse method")
	}

	retType, diags := inf.InferMethod(className, md)
	if len(diags) > 0 {
		t.Errorf("unexpected diagnostics: %v", diags)
	}
	if retType.String() != "SmallInteger" {
		t.Errorf("expected SmallInteger from annotated param + 1, got %s", retType)
	}
}

func TestInferReturnTypeMismatch(t *testing.T) {
	inf := newTestInferrer()

	source := `Foo subclass: Object
  method: test ^<SmallInteger> [
    ^'hello'
  ]`
	md, className := parseMethod(source)
	if md == nil {
		t.Fatal("failed to parse method")
	}

	_, diags := inf.InferMethod(className, md)
	if len(diags) == 0 {
		t.Fatal("expected return type mismatch diagnostic")
	}
	found := false
	for _, d := range diags {
		if strings.Contains(d.Message, "not assignable") {
			found = true
		}
	}
	if !found {
		t.Errorf("expected 'not assignable' diagnostic, got: %v", diags)
	}
}

func TestInferNoReturnStatementIsDynamic(t *testing.T) {
	inf := newTestInferrer()

	source := `Foo subclass: Object
  method: test [
    42
  ]`
	md, className := parseMethod(source)
	if md == nil {
		t.Fatal("failed to parse method")
	}

	retType, _ := inf.InferMethod(className, md)
	if !IsDynamic(retType) {
		t.Errorf("expected Dynamic when no return statement, got %s", retType)
	}
}

func TestInferYourselfReturnsSelf(t *testing.T) {
	inf := newTestInferrer()

	source := `Foo subclass: Object
  method: test [
    ^self yourself
  ]`
	md, className := parseMethod(source)
	if md == nil {
		t.Fatal("failed to parse method")
	}

	retType, diags := inf.InferMethod(className, md)
	if len(diags) > 0 {
		t.Errorf("unexpected diagnostics: %v", diags)
	}
	// yourself returns Self, which resolves to the receiver type (Foo)
	if retType.String() != "Foo" {
		t.Errorf("expected Foo from self yourself, got %s", retType)
	}
}

func TestInferKeywordMessage(t *testing.T) {
	inf := newTestInferrer()

	source := `Foo subclass: Object
  method: test [
    ^'hello' includes: 'h'
  ]`
	md, className := parseMethod(source)
	if md == nil {
		t.Fatal("failed to parse method")
	}

	retType, diags := inf.InferMethod(className, md)
	if len(diags) > 0 {
		t.Errorf("unexpected diagnostics: %v", diags)
	}
	if retType.String() != "Boolean" {
		t.Errorf("expected Boolean from includes:, got %s", retType)
	}
}

func TestInferStringConcat(t *testing.T) {
	inf := newTestInferrer()

	source := `Foo subclass: Object
  method: test [
    ^'hello' , ' world'
  ]`
	md, className := parseMethod(source)
	if md == nil {
		t.Fatal("failed to parse method")
	}

	retType, diags := inf.InferMethod(className, md)
	if len(diags) > 0 {
		t.Errorf("unexpected diagnostics: %v", diags)
	}
	if retType.String() != "String" {
		t.Errorf("expected String from string concat, got %s", retType)
	}
}
