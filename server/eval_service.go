package server

import (
	"context"
	"fmt"
	"strings"

	"connectrpc.com/connect"

	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
	"github.com/chazu/maggie/gen/maggie/v1/maggiev1connect"
	"github.com/chazu/maggie/vm"
)

// EvalService implements the EvaluationService gRPC/Connect handler.
type EvalService struct {
	maggiev1connect.UnimplementedEvaluationServiceHandler
	worker   *VMWorker
	handles  *HandleStore
	sessions *SessionStore
}

// NewEvalService creates an EvalService.
func NewEvalService(worker *VMWorker, handles *HandleStore, sessions *SessionStore) *EvalService {
	return &EvalService{
		worker:   worker,
		handles:  handles,
		sessions: sessions,
	}
}

// Evaluate compiles and executes a Maggie expression.
func (s *EvalService) Evaluate(
	ctx context.Context,
	req *connect.Request[maggiev1.EvaluateRequest],
) (*connect.Response[maggiev1.EvaluateResponse], error) {
	source := req.Msg.Source
	if source == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("source is required"))
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		return s.evaluate(v, source)
	})
	if err != nil {
		return connect.NewResponse(&maggiev1.EvaluateResponse{
			Success:      false,
			ErrorMessage: err.Error(),
		}), nil
	}

	return connect.NewResponse(result.(*maggiev1.EvaluateResponse)), nil
}

// EvaluateInContext compiles and executes an expression with a receiver.
func (s *EvalService) EvaluateInContext(
	ctx context.Context,
	req *connect.Request[maggiev1.EvaluateInContextRequest],
) (*connect.Response[maggiev1.EvaluateResponse], error) {
	source := req.Msg.Source
	if source == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("source is required"))
	}

	ctxHandle := req.Msg.Context
	if ctxHandle == nil || ctxHandle.Id == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("context handle is required"))
	}

	receiver, ok := s.handles.Lookup(ctxHandle.Id)
	if !ok {
		return nil, connect.NewError(connect.CodeNotFound, fmt.Errorf("handle %q not found", ctxHandle.Id))
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		return s.evaluateInContext(v, source, receiver)
	})
	if err != nil {
		return connect.NewResponse(&maggiev1.EvaluateResponse{
			Success:      false,
			ErrorMessage: err.Error(),
		}), nil
	}

	return connect.NewResponse(result.(*maggiev1.EvaluateResponse)), nil
}

// CheckSyntax validates source code without executing it.
func (s *EvalService) CheckSyntax(
	ctx context.Context,
	req *connect.Request[maggiev1.CheckSyntaxRequest],
) (*connect.Response[maggiev1.CheckSyntaxResponse], error) {
	source := req.Msg.Source
	if source == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("source is required"))
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		_, compileErr := v.CompileExpression(source)
		if compileErr != nil {
			return &maggiev1.CheckSyntaxResponse{
				Valid: false,
				Diagnostics: []*maggiev1.Diagnostic{
					{
						Severity: maggiev1.Diagnostic_ERROR,
						Message:  compileErr.Error(),
					},
				},
			}
		}
		return &maggiev1.CheckSyntaxResponse{Valid: true}
	})
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}

	return connect.NewResponse(result.(*maggiev1.CheckSyntaxResponse)), nil
}

// evaluate compiles and runs source, returning an EvaluateResponse.
// Must be called on the VM worker goroutine.
func (s *EvalService) evaluate(v *vm.VM, source string) *maggiev1.EvaluateResponse {
	// Wrap as a doIt method like the REPL does
	wrapped := "doIt\n    ^" + strings.TrimSuffix(source, ".")

	method, err := v.Compile(wrapped, nil)
	if err != nil {
		return &maggiev1.EvaluateResponse{
			Success:      false,
			ErrorMessage: "Compile error: " + err.Error(),
		}
	}
	if method == nil {
		return &maggiev1.EvaluateResponse{
			Success:      false,
			ErrorMessage: "Compile error: compiler returned nil",
		}
	}

	result := v.Execute(method, vm.Nil, nil)

	display := formatValue(v, result)
	className := classNameFor(v, result)

	handleID := s.handles.Create(result, className, display, "")

	return &maggiev1.EvaluateResponse{
		Success: true,
		Result:  display,
		Handle: &maggiev1.ObjectHandle{
			Id:            handleID,
			ClassName:     className,
			DisplayString: display,
		},
	}
}

// evaluateInContext compiles and runs source with a specific receiver.
// Must be called on the VM worker goroutine.
func (s *EvalService) evaluateInContext(v *vm.VM, source string, receiver vm.Value) *maggiev1.EvaluateResponse {
	wrapped := "doIt\n    ^" + strings.TrimSuffix(source, ".")

	method, err := v.Compile(wrapped, nil)
	if err != nil {
		return &maggiev1.EvaluateResponse{
			Success:      false,
			ErrorMessage: "Compile error: " + err.Error(),
		}
	}
	if method == nil {
		return &maggiev1.EvaluateResponse{
			Success:      false,
			ErrorMessage: "Compile error: compiler returned nil",
		}
	}

	result := v.Execute(method, receiver, nil)

	display := formatValue(v, result)
	className := classNameFor(v, result)

	handleID := s.handles.Create(result, className, display, "")

	return &maggiev1.EvaluateResponse{
		Success: true,
		Result:  display,
		Handle: &maggiev1.ObjectHandle{
			Id:            handleID,
			ClassName:     className,
			DisplayString: display,
		},
	}
}

// formatValue converts a VM value to a display string.
// Must be called on the VM worker goroutine.
func formatValue(v *vm.VM, val vm.Value) string {
	switch {
	case val == vm.Nil:
		return "nil"
	case val == vm.True:
		return "true"
	case val == vm.False:
		return "false"
	case val.IsSmallInt():
		return fmt.Sprintf("%d", val.SmallInt())
	case val.IsFloat():
		return fmt.Sprintf("%g", val.Float64())
	case vm.IsStringValue(val):
		return "'" + v.Registry().GetStringContent(val) + "'"
	case vm.IsClassValue(val):
		cls := v.GetClassFromValue(val)
		if cls != nil {
			return cls.Name
		}
		return "<class>"
	case val.IsSymbol():
		name := v.Symbols.Name(val.SymbolID())
		return "#" + name
	case val.IsObject():
		// Try calling printString on the object
		result := v.Send(val, "printString", nil)
		if vm.IsStringValue(result) {
			return v.Registry().GetStringContent(result)
		}
		obj := vm.ObjectFromValue(val)
		if obj != nil && obj.VTablePtr() != nil && obj.VTablePtr().Class() != nil {
			return "a " + obj.VTablePtr().Class().Name
		}
		return "<object>"
	default:
		return fmt.Sprintf("<%v>", val)
	}
}

// classNameFor returns the class name for a VM value.
func classNameFor(v *vm.VM, val vm.Value) string {
	class := v.ClassFor(val)
	if class != nil {
		return class.Name
	}
	return "Object"
}
