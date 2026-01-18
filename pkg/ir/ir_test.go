package ir

import (
	"testing"

	"github.com/chazu/procyon/pkg/ast"
)

func TestScopeResolve(t *testing.T) {
	// Create parent scope with an ivar
	parent := NewScope(nil)
	parent.Define("value", VarDecl{Name: "value", Type: TypeInt, IsIVar: true})

	// Create child scope with a local
	child := NewScope(parent)
	child.Define("temp", VarDecl{Name: "temp", Type: TypeString, IsLocal: true})

	tests := []struct {
		name     string
		scope    *Scope
		varName  string
		wantOK   bool
		wantKind bool // true = should be ivar, false = should be local
	}{
		{"ivar from parent", child, "value", true, true},
		{"local from child", child, "temp", true, false},
		{"missing var", child, "missing", false, false},
		{"ivar direct", parent, "value", true, true},
		{"local not in parent", parent, "temp", false, false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			decl, ok := tt.scope.Resolve(tt.varName)
			if ok != tt.wantOK {
				t.Errorf("Resolve(%q) ok = %v, want %v", tt.varName, ok, tt.wantOK)
				return
			}
			if ok && decl.IsIVar != tt.wantKind {
				t.Errorf("Resolve(%q) IsIVar = %v, want %v", tt.varName, decl.IsIVar, tt.wantKind)
			}
		})
	}
}

func TestScopeShadowing(t *testing.T) {
	// Parent has value as ivar
	parent := NewScope(nil)
	parent.Define("value", VarDecl{Name: "value", Type: TypeInt, IsIVar: true})

	// Child shadows with local
	child := NewScope(parent)
	child.Define("value", VarDecl{Name: "value", Type: TypeString, IsLocal: true})

	// Child should see local, not ivar
	decl, ok := child.Resolve("value")
	if !ok {
		t.Fatal("expected to find 'value'")
	}
	if decl.IsIVar {
		t.Error("child scope should shadow ivar with local")
	}
	if decl.Type != TypeString {
		t.Errorf("expected TypeString, got %v", decl.Type)
	}

	// Parent should still see ivar
	decl, ok = parent.Resolve("value")
	if !ok {
		t.Fatal("expected to find 'value' in parent")
	}
	if !decl.IsIVar {
		t.Error("parent scope should have ivar")
	}
}

func TestNewBuilder(t *testing.T) {
	class := &ast.Class{
		Name:    "Counter",
		Parent:  "Object",
		Package: "MyApp",
		InstanceVars: []ast.InstanceVar{
			{Name: "value", Default: ast.DefaultValue{Type: "number", Value: "0"}},
			{Name: "step", Default: ast.DefaultValue{Type: "number", Value: "1"}},
		},
	}

	b := NewBuilder(class)

	// Check that ivars are in scope
	decl, ok := b.scope.Resolve("value")
	if !ok {
		t.Fatal("expected 'value' in scope")
	}
	if !decl.IsIVar {
		t.Error("expected 'value' to be ivar")
	}
	if decl.Type != TypeInt {
		t.Errorf("expected TypeInt for 'value', got %v", decl.Type)
	}

	decl, ok = b.scope.Resolve("step")
	if !ok {
		t.Fatal("expected 'step' in scope")
	}
	if !decl.IsIVar {
		t.Error("expected 'step' to be ivar")
	}
}

func TestBuildSimpleClass(t *testing.T) {
	class := &ast.Class{
		Name:    "Counter",
		Parent:  "Object",
		Package: "Test",
		InstanceVars: []ast.InstanceVar{
			{Name: "value", Default: ast.DefaultValue{Type: "number", Value: "0"}},
		},
		Methods: []ast.Method{
			{
				Selector: "increment",
				Kind:     "instance",
				Args:     []string{},
				Body:     ast.Block{},
			},
		},
	}

	b := NewBuilder(class)
	prog, warnings, errors := b.Build()

	if len(errors) > 0 {
		t.Fatalf("unexpected errors: %v", errors)
	}

	if prog.Name != "Counter" {
		t.Errorf("expected Name='Counter', got %q", prog.Name)
	}
	if prog.Package != "Test" {
		t.Errorf("expected Package='Test', got %q", prog.Package)
	}
	if prog.QualifiedName != "Test::Counter" {
		t.Errorf("expected QualifiedName='Test::Counter', got %q", prog.QualifiedName)
	}
	if prog.Parent != "Object" {
		t.Errorf("expected Parent='Object', got %q", prog.Parent)
	}

	if len(prog.InstanceVars) != 1 {
		t.Fatalf("expected 1 instance var, got %d", len(prog.InstanceVars))
	}
	if prog.InstanceVars[0].Name != "value" {
		t.Errorf("expected ivar name='value', got %q", prog.InstanceVars[0].Name)
	}

	if len(prog.Methods) != 1 {
		t.Fatalf("expected 1 method, got %d", len(prog.Methods))
	}
	if prog.Methods[0].Selector != "increment" {
		t.Errorf("expected method selector='increment', got %q", prog.Methods[0].Selector)
	}

	_ = warnings // may have warnings about unparseable body
}

func TestBuildRawMethod(t *testing.T) {
	class := &ast.Class{
		Name:   "Tool",
		Parent: "Object",
		Methods: []ast.Method{
			{
				Selector: "run",
				Kind:     "instance",
				Raw:      true,
			},
		},
	}

	b := NewBuilder(class)
	prog, _, errors := b.Build()

	if len(errors) > 0 {
		t.Fatalf("unexpected errors: %v", errors)
	}

	method := prog.Methods[0]
	if method.CanCompile {
		t.Error("raw method should not be compilable")
	}
	if method.Backend != BackendBash {
		t.Errorf("raw method should require Bash backend, got %v", method.Backend)
	}
	if !method.IsRaw {
		t.Error("method should be marked as raw")
	}
	if method.FallbackReason == "" {
		t.Error("expected fallback reason for raw method")
	}
}

func TestBuildClassMethod(t *testing.T) {
	class := &ast.Class{
		Name:   "Factory",
		Parent: "Object",
		Methods: []ast.Method{
			{
				Selector: "create",
				Kind:     "class",
				Args:     []string{"name"},
			},
		},
	}

	b := NewBuilder(class)
	prog, _, errors := b.Build()

	if len(errors) > 0 {
		t.Fatalf("unexpected errors: %v", errors)
	}

	method := prog.Methods[0]
	if method.Kind != ClassMethod {
		t.Errorf("expected ClassMethod, got %v", method.Kind)
	}
	if len(method.Args) != 1 {
		t.Fatalf("expected 1 arg, got %d", len(method.Args))
	}
	if method.Args[0].Name != "name" {
		t.Errorf("expected arg name='name', got %q", method.Args[0].Name)
	}
	if !method.Args[0].IsParam {
		t.Error("arg should be marked as param")
	}
}

func TestBuildQualifiedParent(t *testing.T) {
	class := &ast.Class{
		Name:   "MyWidget",
		Parent: "Yutani::Widget",
	}

	b := NewBuilder(class)
	prog, _, errors := b.Build()

	if len(errors) > 0 {
		t.Fatalf("unexpected errors: %v", errors)
	}

	// Parent is split: Parent gets class name, ParentPackage gets package
	if prog.Parent != "Widget" {
		t.Errorf("expected Parent='Widget', got %q", prog.Parent)
	}
	if prog.ParentPackage != "Yutani" {
		t.Errorf("expected ParentPackage='Yutani', got %q", prog.ParentPackage)
	}
}

func TestInferTypeFromDefault(t *testing.T) {
	tests := []struct {
		input    ast.DefaultValue
		expected Type
	}{
		{ast.DefaultValue{Type: "number", Value: "42"}, TypeInt},
		{ast.DefaultValue{Type: "string", Value: "'hello'"}, TypeString},
		{ast.DefaultValue{Type: "bool", Value: "true"}, TypeBool},
		{ast.DefaultValue{Type: "array", Value: "[]"}, TypeJSON},
		{ast.DefaultValue{Type: "object", Value: "{}"}, TypeJSON},
		{ast.DefaultValue{Type: "", Value: ""}, TypeAny},
	}

	for _, tt := range tests {
		t.Run(tt.input.Type, func(t *testing.T) {
			result := inferTypeFromDefault(tt.input)
			if result != tt.expected {
				t.Errorf("inferTypeFromDefault(%v) = %v, want %v", tt.input, result, tt.expected)
			}
		})
	}
}

func TestTypeStringer(t *testing.T) {
	tests := []struct {
		t    Type
		want string
	}{
		{TypeInt, "int"},
		{TypeString, "string"},
		{TypeBool, "bool"},
		{TypeJSON, "json"},
		{TypeBlock, "block"},
		{TypeInstance, "instance"},
		{TypeClass, "class"},
		{TypeAny, "any"},
		{TypeUnknown, "unknown"},
	}

	for _, tt := range tests {
		if got := tt.t.String(); got != tt.want {
			t.Errorf("Type(%d).String() = %q, want %q", tt.t, got, tt.want)
		}
	}
}

func TestBackendStringer(t *testing.T) {
	tests := []struct {
		b    Backend
		want string
	}{
		{BackendAny, "any"},
		{BackendGo, "go"},
		{BackendBash, "bash"},
	}

	for _, tt := range tests {
		if got := tt.b.String(); got != tt.want {
			t.Errorf("Backend(%d).String() = %q, want %q", tt.b, got, tt.want)
		}
	}
}

func TestMethodKindStringer(t *testing.T) {
	if InstanceMethod.String() != "instance" {
		t.Errorf("InstanceMethod.String() = %q, want 'instance'", InstanceMethod.String())
	}
	if ClassMethod.String() != "class" {
		t.Errorf("ClassMethod.String() = %q, want 'class'", ClassMethod.String())
	}
}

func TestVarKindStringer(t *testing.T) {
	tests := []struct {
		k    VarKind
		want string
	}{
		{VarLocal, "local"},
		{VarParam, "param"},
		{VarIVar, "ivar"},
		{VarClassVar, "classvar"},
		{VarGlobal, "global"},
	}

	for _, tt := range tests {
		if got := tt.k.String(); got != tt.want {
			t.Errorf("VarKind(%d).String() = %q, want %q", tt.k, got, tt.want)
		}
	}
}

func TestAssignKindStringer(t *testing.T) {
	tests := []struct {
		k    AssignKind
		want string
	}{
		{AssignLocal, "local"},
		{AssignIVar, "ivar"},
		{AssignClassVar, "classvar"},
	}

	for _, tt := range tests {
		if got := tt.k.String(); got != tt.want {
			t.Errorf("AssignKind(%d).String() = %q, want %q", tt.k, got, tt.want)
		}
	}
}

func TestClassRefExprFullName(t *testing.T) {
	tests := []struct {
		ref  ClassRefExpr
		want string
	}{
		{ClassRefExpr{Package: "Yutani", Name: "Widget"}, "Yutani::Widget"},
		{ClassRefExpr{Package: "", Name: "Counter"}, "Counter"},
	}

	for _, tt := range tests {
		if got := tt.ref.FullName(); got != tt.want {
			t.Errorf("ClassRefExpr{%q, %q}.FullName() = %q, want %q",
				tt.ref.Package, tt.ref.Name, got, tt.want)
		}
	}
}

func TestExpressionResultTypes(t *testing.T) {
	// Test that all expression types implement ResultType correctly
	tests := []struct {
		name string
		expr Expression
		want Type
	}{
		{"LiteralExpr int", LiteralExpr{Value: 42, Type_: TypeInt}, TypeInt},
		{"LiteralExpr string", LiteralExpr{Value: "hello", Type_: TypeString}, TypeString},
		{"VarRefExpr", VarRefExpr{Name: "x", Type_: TypeAny}, TypeAny},
		{"BinaryExpr", BinaryExpr{Op: "+", Type_: TypeInt}, TypeInt},
		{"UnaryExpr", UnaryExpr{Op: "-", Type_: TypeInt}, TypeInt},
		{"MessageSendExpr", MessageSendExpr{Selector: "foo", Type_: TypeAny}, TypeAny},
		{"BlockExpr", BlockExpr{Params: []string{"x"}}, TypeBlock},
		{"SubshellExpr", SubshellExpr{Code: "$(ls)"}, TypeString},
		{"JSONPrimitiveExpr", JSONPrimitiveExpr{Operation: "push", Type_: TypeJSON}, TypeJSON},
		{"SelfExpr", SelfExpr{}, TypeInstance},
		{"ClassRefExpr", ClassRefExpr{Name: "Counter"}, TypeClass},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.expr.ResultType(); got != tt.want {
				t.Errorf("%s.ResultType() = %v, want %v", tt.name, got, tt.want)
			}
		})
	}
}

// Test that Statement interface is implemented
func TestStatementInterface(t *testing.T) {
	statements := []Statement{
		AssignStmt{Target: "x", Kind: AssignLocal},
		ReturnStmt{},
		ExprStmt{},
		IfStmt{},
		WhileStmt{},
		ForEachStmt{IterVar: "i"},
		BashStmt{Code: "echo hi"},
	}

	for i, stmt := range statements {
		// Just verify they implement the interface (compile-time check mostly)
		stmt.irStmt()
		_ = i
	}
}

// Test that Expression interface is implemented
func TestExpressionInterface(t *testing.T) {
	expressions := []Expression{
		LiteralExpr{},
		VarRefExpr{},
		BinaryExpr{},
		UnaryExpr{},
		MessageSendExpr{},
		BlockExpr{},
		SubshellExpr{},
		JSONPrimitiveExpr{},
		SelfExpr{},
		ClassRefExpr{},
	}

	for _, expr := range expressions {
		expr.irExpr()
		_ = expr.ResultType()
	}
}
