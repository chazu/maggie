package server

import (
	"testing"

	"connectrpc.com/connect"

	"github.com/chazu/maggie/compiler"
	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// CompileMethod — happy paths
// ---------------------------------------------------------------------------

func TestCompileMethod_AddInstanceMethod(t *testing.T) {
	env := newIsolatedEnv()
	defer env.Stop()

	svc := NewModifyService(env.Worker, env.Handles, env.Sessions)

	resp, err := svc.CompileMethod(bg(), connectReq(&maggiev1.CompileMethodRequest{
		ClassName: "Object",
		Source:    "testServerMethod42 ^42",
	}))
	if err != nil {
		t.Fatalf("CompileMethod returned error: %v", err)
	}
	if !resp.Msg.Success {
		t.Fatalf("CompileMethod was not successful: %s", resp.Msg.ErrorMessage)
	}
	if resp.Msg.Selector != "testServerMethod42" {
		t.Errorf("Selector = %q, want %q", resp.Msg.Selector, "testServerMethod42")
	}

	// Verify the method is callable
	evalSvc := NewEvalService(env.Worker, env.Handles, env.Sessions)
	evalResp, err := evalSvc.Evaluate(bg(), connectReq(&maggiev1.EvaluateRequest{
		Source: "nil testServerMethod42",
	}))
	if err != nil {
		t.Fatalf("Evaluate returned error: %v", err)
	}
	if !evalResp.Msg.Success {
		t.Fatalf("Evaluate was not successful: %s", evalResp.Msg.ErrorMessage)
	}
	if evalResp.Msg.Result != "42" {
		t.Errorf("result = %q, want %q", evalResp.Msg.Result, "42")
	}
}

func TestCompileMethod_AddClassSideMethod(t *testing.T) {
	env := newIsolatedEnv()
	defer env.Stop()

	svc := NewModifyService(env.Worker, env.Handles, env.Sessions)

	resp, err := svc.CompileMethod(bg(), connectReq(&maggiev1.CompileMethodRequest{
		ClassName: "Object",
		Source:    "testClassMethod99 ^99",
		ClassSide: true,
	}))
	if err != nil {
		t.Fatalf("CompileMethod returned error: %v", err)
	}
	if !resp.Msg.Success {
		t.Fatalf("CompileMethod was not successful: %s", resp.Msg.ErrorMessage)
	}
	if resp.Msg.Selector != "testClassMethod99" {
		t.Errorf("Selector = %q, want %q", resp.Msg.Selector, "testClassMethod99")
	}
}

// ---------------------------------------------------------------------------
// CompileMethod — error paths
// ---------------------------------------------------------------------------

func TestCompileMethod_EmptyClassName(t *testing.T) {
	svc := newTestModifyService()

	_, err := svc.CompileMethod(bg(), connectReq(&maggiev1.CompileMethodRequest{
		ClassName: "",
		Source:    "foo ^1",
	}))
	if err == nil {
		t.Fatal("CompileMethod with empty class_name should return error")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeInvalidArgument {
			t.Errorf("expected CodeInvalidArgument, got %v", connectErr.Code())
		}
	}
}

func TestCompileMethod_EmptySource(t *testing.T) {
	svc := newTestModifyService()

	_, err := svc.CompileMethod(bg(), connectReq(&maggiev1.CompileMethodRequest{
		ClassName: "Object",
		Source:    "",
	}))
	if err == nil {
		t.Fatal("CompileMethod with empty source should return error")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeInvalidArgument {
			t.Errorf("expected CodeInvalidArgument, got %v", connectErr.Code())
		}
	}
}

func TestCompileMethod_ClassNotFound(t *testing.T) {
	svc := newTestModifyService()

	resp, err := svc.CompileMethod(bg(), connectReq(&maggiev1.CompileMethodRequest{
		ClassName: "NoSuchClass12345",
		Source:    "foo ^1",
	}))
	if err != nil {
		t.Fatalf("CompileMethod returned transport error: %v", err)
	}
	if resp.Msg.Success {
		t.Error("CompileMethod should not succeed for unknown class")
	}
	if resp.Msg.ErrorMessage == "" {
		t.Error("ErrorMessage should describe the error")
	}
}

func TestCompileMethod_BadSyntax(t *testing.T) {
	svc := newTestModifyService()

	resp, err := svc.CompileMethod(bg(), connectReq(&maggiev1.CompileMethodRequest{
		ClassName: "Object",
		Source:    "[[[",
	}))
	if err != nil {
		t.Fatalf("CompileMethod returned transport error: %v", err)
	}
	if resp.Msg.Success {
		t.Error("CompileMethod should not succeed with invalid syntax")
	}
	if len(resp.Msg.Diagnostics) == 0 {
		t.Error("expected at least one diagnostic for invalid syntax")
	}
}

// ---------------------------------------------------------------------------
// RemoveMethod
// ---------------------------------------------------------------------------

func TestRemoveMethod_AddThenRemove(t *testing.T) {
	env := newIsolatedEnv()
	defer env.Stop()

	svc := NewModifyService(env.Worker, env.Handles, env.Sessions)

	// First add a method
	compileResp, err := svc.CompileMethod(bg(), connectReq(&maggiev1.CompileMethodRequest{
		ClassName: "Object",
		Source:    "temporaryTestMethod ^0",
	}))
	if err != nil {
		t.Fatalf("CompileMethod returned error: %v", err)
	}
	if !compileResp.Msg.Success {
		t.Fatalf("CompileMethod was not successful: %s", compileResp.Msg.ErrorMessage)
	}

	// Now remove it
	removeResp, err := svc.RemoveMethod(bg(), connectReq(&maggiev1.RemoveMethodRequest{
		ClassName: "Object",
		Selector:  "temporaryTestMethod",
	}))
	if err != nil {
		t.Fatalf("RemoveMethod returned error: %v", err)
	}
	if !removeResp.Msg.Success {
		t.Fatalf("RemoveMethod was not successful: %s", removeResp.Msg.ErrorMessage)
	}
}

func TestRemoveMethod_EmptyArguments(t *testing.T) {
	svc := newTestModifyService()

	_, err := svc.RemoveMethod(bg(), connectReq(&maggiev1.RemoveMethodRequest{
		ClassName: "",
		Selector:  "",
	}))
	if err == nil {
		t.Fatal("RemoveMethod with empty args should return error")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeInvalidArgument {
			t.Errorf("expected CodeInvalidArgument, got %v", connectErr.Code())
		}
	}
}

func TestRemoveMethod_ClassNotFound(t *testing.T) {
	svc := newTestModifyService()

	resp, err := svc.RemoveMethod(bg(), connectReq(&maggiev1.RemoveMethodRequest{
		ClassName: "NoSuchClass12345",
		Selector:  "foo",
	}))
	if err != nil {
		t.Fatalf("RemoveMethod returned transport error: %v", err)
	}
	if resp.Msg.Success {
		t.Error("RemoveMethod should not succeed for unknown class")
	}
}

func TestRemoveMethod_MethodNotFound(t *testing.T) {
	svc := newTestModifyService()

	resp, err := svc.RemoveMethod(bg(), connectReq(&maggiev1.RemoveMethodRequest{
		ClassName: "Object",
		Selector:  "noSuchMethodEver12345",
	}))
	if err != nil {
		t.Fatalf("RemoveMethod returned transport error: %v", err)
	}
	if resp.Msg.Success {
		t.Error("RemoveMethod should not succeed for unknown selector")
	}
}

// ---------------------------------------------------------------------------
// CreateClass
// ---------------------------------------------------------------------------

func TestCreateClass_Simple(t *testing.T) {
	env := newIsolatedEnv()
	defer env.Stop()

	svc := NewModifyService(env.Worker, env.Handles, env.Sessions)

	resp, err := svc.CreateClass(bg(), connectReq(&maggiev1.CreateClassRequest{
		Name: "TestNewClass",
	}))
	if err != nil {
		t.Fatalf("CreateClass returned error: %v", err)
	}
	if !resp.Msg.Success {
		t.Fatalf("CreateClass was not successful: %s", resp.Msg.ErrorMessage)
	}
	if resp.Msg.Class == nil {
		t.Fatal("CreateClass should return the class info")
	}
	if resp.Msg.Class.Name != "TestNewClass" {
		t.Errorf("Class.Name = %q, want %q", resp.Msg.Class.Name, "TestNewClass")
	}
	if resp.Msg.Class.SuperclassName != "Object" {
		t.Errorf("SuperclassName = %q, want %q", resp.Msg.Class.SuperclassName, "Object")
	}
}

func TestCreateClass_WithSuperclassAndIvars(t *testing.T) {
	env := newIsolatedEnv()
	defer env.Stop()

	svc := NewModifyService(env.Worker, env.Handles, env.Sessions)

	resp, err := svc.CreateClass(bg(), connectReq(&maggiev1.CreateClassRequest{
		Name:                  "TestSubclass",
		SuperclassName:        "Object",
		InstanceVariableNames: []string{"x", "y"},
	}))
	if err != nil {
		t.Fatalf("CreateClass returned error: %v", err)
	}
	if !resp.Msg.Success {
		t.Fatalf("CreateClass was not successful: %s", resp.Msg.ErrorMessage)
	}
	if len(resp.Msg.Class.InstanceVariableNames) != 2 {
		t.Errorf("InstanceVariableNames length = %d, want 2", len(resp.Msg.Class.InstanceVariableNames))
	}
}

func TestCreateClass_EmptyName(t *testing.T) {
	svc := newTestModifyService()

	_, err := svc.CreateClass(bg(), connectReq(&maggiev1.CreateClassRequest{
		Name: "",
	}))
	if err == nil {
		t.Fatal("CreateClass with empty name should return error")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeInvalidArgument {
			t.Errorf("expected CodeInvalidArgument, got %v", connectErr.Code())
		}
	}
}

func TestCreateClass_DuplicateName(t *testing.T) {
	svc := newTestModifyService()

	resp, err := svc.CreateClass(bg(), connectReq(&maggiev1.CreateClassRequest{
		Name: "Object", // already exists
	}))
	if err != nil {
		t.Fatalf("CreateClass returned transport error: %v", err)
	}
	if resp.Msg.Success {
		t.Error("CreateClass should not succeed when class already exists")
	}
}

func TestCreateClass_InvalidSuperclass(t *testing.T) {
	svc := newTestModifyService()

	resp, err := svc.CreateClass(bg(), connectReq(&maggiev1.CreateClassRequest{
		Name:           "TestBadSuper",
		SuperclassName: "NoSuchSuperclass",
	}))
	if err != nil {
		t.Fatalf("CreateClass returned transport error: %v", err)
	}
	if resp.Msg.Success {
		t.Error("CreateClass should not succeed with unknown superclass")
	}
}

// ---------------------------------------------------------------------------
// EvaluateWithLocals
// ---------------------------------------------------------------------------

func TestEvaluateWithLocals_EmptySource(t *testing.T) {
	svc := newTestModifyService()

	_, err := svc.EvaluateWithLocals(bg(), connectReq(&maggiev1.EvaluateWithLocalsRequest{
		Source: "",
	}))
	if err == nil {
		t.Fatal("EvaluateWithLocals with empty source should return error")
	}
}

func TestEvaluateWithLocals_SimpleExpression(t *testing.T) {
	env := newIsolatedEnv()
	defer env.Stop()

	svc := NewModifyService(env.Worker, env.Handles, env.Sessions)

	resp, err := svc.EvaluateWithLocals(bg(), connectReq(&maggiev1.EvaluateWithLocalsRequest{
		Source: "3 + 4",
	}))
	if err != nil {
		t.Fatalf("EvaluateWithLocals returned error: %v", err)
	}
	if !resp.Msg.Success {
		t.Fatalf("EvaluateWithLocals was not successful: %s", resp.Msg.ErrorMessage)
	}
	if resp.Msg.Result != "7" {
		t.Errorf("Result = %q, want %q", resp.Msg.Result, "7")
	}
}

func TestEvaluateWithLocals_CompileError(t *testing.T) {
	env := newIsolatedEnv()
	defer env.Stop()

	svc := NewModifyService(env.Worker, env.Handles, env.Sessions)

	resp, err := svc.EvaluateWithLocals(bg(), connectReq(&maggiev1.EvaluateWithLocalsRequest{
		Source: "[[[",
	}))
	if err != nil {
		t.Fatalf("EvaluateWithLocals returned transport error: %v", err)
	}
	if resp.Msg.Success {
		t.Error("EvaluateWithLocals should not succeed with invalid syntax")
	}
	if resp.Msg.ErrorMessage == "" {
		t.Error("ErrorMessage should describe the compile error")
	}
}

// ---------------------------------------------------------------------------
// Workspace eval (routes through the AST-level doIt compiler)
// ---------------------------------------------------------------------------

// TestWorkspaceEvalSurvivesDecimalsAndBlocks regresses the workspace-eval path
// away from the textual dot-splitter: `x := 3.14` and periods inside block
// bodies must not be mistaken for statement separators. The old splitter turned
// `x := 3.14` into `x := 3` / `^14` — a compile error — so a clean compile of
// these sources is exactly the fix. (Execution needs the lib image; compilation
// is where the splitter did its damage.)
func TestWorkspaceEvalSurvivesDecimalsAndBlocks(t *testing.T) {
	v := vm.NewVM()
	v.UseGoCompiler(compiler.Compile)

	sources := []string{
		"x := 3.14. x",
		"s := 0. #(1 2 3) do: [:e | s := s + e. s]. s",
		"p := 'a.b.c'. p size",
	}
	for _, src := range sources {
		if _, err := v.CompileExpression(src); err != nil {
			t.Errorf("workspace source must compile via the AST doIt path, got error for %q: %v", src, err)
		}
	}
}
