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

// ModifyService implements the ModificationService gRPC/Connect handler.
type ModifyService struct {
	maggiev1connect.UnimplementedModificationServiceHandler
	worker   *VMWorker
	handles  *HandleStore
	sessions *SessionStore
}

// NewModifyService creates a ModifyService.
func NewModifyService(worker *VMWorker, handles *HandleStore, sessions *SessionStore) *ModifyService {
	return &ModifyService{
		worker:   worker,
		handles:  handles,
		sessions: sessions,
	}
}

// CompileMethod compiles source and installs the resulting method on a class.
func (s *ModifyService) CompileMethod(
	ctx context.Context,
	req *connect.Request[maggiev1.CompileMethodRequest],
) (*connect.Response[maggiev1.CompileMethodResponse], error) {
	if req.Msg.ClassName == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("class_name is required"))
	}
	if req.Msg.Source == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("source is required"))
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		cls := v.Classes.Lookup(req.Msg.ClassName)
		if cls == nil {
			return &maggiev1.CompileMethodResponse{
				Success:      false,
				ErrorMessage: fmt.Sprintf("class %q not found", req.Msg.ClassName),
			}
		}

		// Compile the method source in the context of the class
		method, compileErr := v.Compile(req.Msg.Source, cls)
		if compileErr != nil {
			return &maggiev1.CompileMethodResponse{
				Success:      false,
				ErrorMessage: "Compile error: " + compileErr.Error(),
				Diagnostics: []*maggiev1.Diagnostic{
					{
						Severity: maggiev1.Diagnostic_ERROR,
						Message:  compileErr.Error(),
					},
				},
			}
		}
		if method == nil {
			return &maggiev1.CompileMethodResponse{
				Success:      false,
				ErrorMessage: "Compile error: compiler returned nil",
			}
		}

		// Set the owning class
		method.SetClass(cls)

		// Install on the appropriate vtable
		selectorName := method.Name()
		selectorID := v.Selectors.Intern(selectorName)
		if req.Msg.ClassSide {
			method.IsClassMethod = true
			if cls.ClassVTable == nil {
				cls.ClassVTable = vm.NewVTable(cls, nil)
			}
			cls.ClassVTable.AddMethod(selectorID, method)
		} else {
			cls.VTable.AddMethod(selectorID, method)
		}

		return &maggiev1.CompileMethodResponse{
			Success:  true,
			Selector: selectorName,
		}
	})
	if err != nil {
		return connect.NewResponse(&maggiev1.CompileMethodResponse{
			Success:      false,
			ErrorMessage: err.Error(),
		}), nil
	}

	return connect.NewResponse(result.(*maggiev1.CompileMethodResponse)), nil
}

// RemoveMethod removes a method from a class.
func (s *ModifyService) RemoveMethod(
	ctx context.Context,
	req *connect.Request[maggiev1.RemoveMethodRequest],
) (*connect.Response[maggiev1.RemoveMethodResponse], error) {
	if req.Msg.ClassName == "" || req.Msg.Selector == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("class_name and selector are required"))
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		cls := v.Classes.Lookup(req.Msg.ClassName)
		if cls == nil {
			return &maggiev1.RemoveMethodResponse{
				Success:      false,
				ErrorMessage: fmt.Sprintf("class %q not found", req.Msg.ClassName),
			}
		}

		selID := v.Selectors.Lookup(req.Msg.Selector)
		if selID < 0 {
			return &maggiev1.RemoveMethodResponse{
				Success:      false,
				ErrorMessage: fmt.Sprintf("selector %q not found", req.Msg.Selector),
			}
		}

		vt := cls.VTable
		if req.Msg.ClassSide {
			vt = cls.ClassVTable
		}
		if vt == nil {
			return &maggiev1.RemoveMethodResponse{
				Success:      false,
				ErrorMessage: fmt.Sprintf("no %s methods on %s", sideStr(req.Msg.ClassSide), req.Msg.ClassName),
			}
		}

		if vt.LookupLocal(selID) == nil {
			return &maggiev1.RemoveMethodResponse{
				Success:      false,
				ErrorMessage: fmt.Sprintf("method %s>>%s not found", req.Msg.ClassName, req.Msg.Selector),
			}
		}

		vt.RemoveMethod(selID)
		return &maggiev1.RemoveMethodResponse{Success: true}
	})
	if err != nil {
		return connect.NewResponse(&maggiev1.RemoveMethodResponse{
			Success:      false,
			ErrorMessage: err.Error(),
		}), nil
	}

	return connect.NewResponse(result.(*maggiev1.RemoveMethodResponse)), nil
}

// CreateClass creates a new class in the image.
func (s *ModifyService) CreateClass(
	ctx context.Context,
	req *connect.Request[maggiev1.CreateClassRequest],
) (*connect.Response[maggiev1.CreateClassResponse], error) {
	if req.Msg.Name == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("name is required"))
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		// Check if class already exists
		if existing := v.Classes.Lookup(req.Msg.Name); existing != nil {
			return &maggiev1.CreateClassResponse{
				Success:      false,
				ErrorMessage: fmt.Sprintf("class %q already exists", req.Msg.Name),
			}
		}

		// Resolve superclass (default to Object)
		superName := req.Msg.SuperclassName
		if superName == "" {
			superName = "Object"
		}
		superclass := v.Classes.Lookup(superName)
		if superclass == nil {
			return &maggiev1.CreateClassResponse{
				Success:      false,
				ErrorMessage: fmt.Sprintf("superclass %q not found", superName),
			}
		}

		// Create the class
		var cls *vm.Class
		if len(req.Msg.InstanceVariableNames) > 0 {
			cls = vm.NewClassWithInstVars(req.Msg.Name, superclass, req.Msg.InstanceVariableNames)
		} else {
			cls = vm.NewClass(req.Msg.Name, superclass)
		}

		// Register in class table
		v.Classes.Register(cls)

		// Register as a global (so Maggie code can reference the class by name)
		v.Globals[req.Msg.Name] = v.Symbols.SymbolValue(req.Msg.Name)

		return &maggiev1.CreateClassResponse{
			Success: true,
			Class:   classToInfo(v, cls),
		}
	})
	if err != nil {
		return connect.NewResponse(&maggiev1.CreateClassResponse{
			Success:      false,
			ErrorMessage: err.Error(),
		}), nil
	}

	return connect.NewResponse(result.(*maggiev1.CreateClassResponse)), nil
}

// SaveImage persists the current image to disk.
func (s *ModifyService) SaveImage(
	ctx context.Context,
	req *connect.Request[maggiev1.SaveImageRequest],
) (*connect.Response[maggiev1.SaveImageResponse], error) {
	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		path := req.Msg.Path
		if path == "" {
			path = "maggie.image"
		}

		if saveErr := v.SaveImage(path); saveErr != nil {
			return &maggiev1.SaveImageResponse{
				Success:      false,
				ErrorMessage: "Save error: " + saveErr.Error(),
			}
		}

		return &maggiev1.SaveImageResponse{
			Success: true,
			Path:    path,
		}
	})
	if err != nil {
		return connect.NewResponse(&maggiev1.SaveImageResponse{
			Success:      false,
			ErrorMessage: err.Error(),
		}), nil
	}

	return connect.NewResponse(result.(*maggiev1.SaveImageResponse)), nil
}

// EvaluateWithLocals evaluates source with injected local variables.
// Locals are temporarily injected into globals, the expression is evaluated,
// and modified/new locals are captured back.
func (s *ModifyService) EvaluateWithLocals(
	ctx context.Context,
	req *connect.Request[maggiev1.EvaluateWithLocalsRequest],
) (*connect.Response[maggiev1.EvaluateWithLocalsResponse], error) {
	if req.Msg.Source == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("source is required"))
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		return s.evaluateWithLocals(v, req.Msg)
	})
	if err != nil {
		return connect.NewResponse(&maggiev1.EvaluateWithLocalsResponse{
			Success:      false,
			ErrorMessage: err.Error(),
		}), nil
	}

	return connect.NewResponse(result.(*maggiev1.EvaluateWithLocalsResponse)), nil
}

// evaluateWithLocals does the actual evaluation with locals injection.
// Must be called on the VM worker goroutine.
func (s *ModifyService) evaluateWithLocals(v *vm.VM, msg *maggiev1.EvaluateWithLocalsRequest) *maggiev1.EvaluateWithLocalsResponse {
	// Track injected local names and save existing globals for restoration
	localNames := make(map[string]bool)
	savedGlobals := make(map[string]vm.Value)

	// Inject locals into globals
	for _, local := range msg.Locals {
		if local.Name == "" {
			continue
		}
		localNames[local.Name] = true

		// Save existing global value for restoration
		if existing, ok := v.Globals[local.Name]; ok {
			savedGlobals[local.Name] = existing
		}

		// Resolve the handle to get the actual value
		if local.Handle != nil && local.Handle.Id != "" {
			if val, ok := s.handles.Lookup(local.Handle.Id); ok {
				v.Globals[local.Name] = val
			}
		}
	}

	// Snapshot global keys before execution to detect new assignments
	preExecGlobals := make(map[string]bool)
	for k := range v.Globals {
		preExecGlobals[k] = true
	}

	// Compile the expression. For multi-statement source, put ^ only before
	// the last statement so all statements execute.
	wrapped := wrapWorkspaceSource(msg.Source)
	method, compileErr := v.Compile(wrapped, nil)
	if compileErr != nil {
		restoreGlobals(v, localNames, savedGlobals)
		return &maggiev1.EvaluateWithLocalsResponse{
			Success:      false,
			ErrorMessage: "Compile error: " + compileErr.Error(),
			Diagnostics: []*maggiev1.Diagnostic{
				{
					Severity: maggiev1.Diagnostic_ERROR,
					Message:  compileErr.Error(),
				},
			},
		}
	}
	if method == nil {
		restoreGlobals(v, localNames, savedGlobals)
		return &maggiev1.EvaluateWithLocalsResponse{
			Success:      false,
			ErrorMessage: "Compile error: compiler returned nil",
		}
	}

	// Execute
	resultVal := v.Execute(method, vm.Nil, nil)

	// Capture updated locals
	var updatedLocals []*maggiev1.LocalVariable

	// Check existing locals for modifications
	for name := range localNames {
		if val, ok := v.Globals[name]; ok {
			display := formatValue(v, val)
			className := classNameFor(v, val)
			handleID := s.handles.Create(val, className, display, msg.SessionId)

			updatedLocals = append(updatedLocals, &maggiev1.LocalVariable{
				Name:  name,
				Value: display,
				Handle: &maggiev1.ObjectHandle{
					Id:            handleID,
					ClassName:     className,
					DisplayString: display,
				},
			})
		}
	}

	// Capture new variables created during evaluation
	for name, val := range v.Globals {
		if !preExecGlobals[name] && !localNames[name] {
			display := formatValue(v, val)
			className := classNameFor(v, val)
			handleID := s.handles.Create(val, className, display, msg.SessionId)

			updatedLocals = append(updatedLocals, &maggiev1.LocalVariable{
				Name:  name,
				Value: display,
				Handle: &maggiev1.ObjectHandle{
					Id:            handleID,
					ClassName:     className,
					DisplayString: display,
				},
			})
		}
	}

	// Restore globals
	restoreGlobals(v, localNames, savedGlobals)

	// Also clean up new globals that were created
	for name := range v.Globals {
		if !preExecGlobals[name] {
			delete(v.Globals, name)
		}
	}

	display := formatValue(v, resultVal)
	className := classNameFor(v, resultVal)
	handleID := s.handles.Create(resultVal, className, display, msg.SessionId)

	return &maggiev1.EvaluateWithLocalsResponse{
		Success: true,
		Result:  display,
		Handle: &maggiev1.ObjectHandle{
			Id:            handleID,
			ClassName:     className,
			DisplayString: display,
		},
		UpdatedLocals: updatedLocals,
	}
}

// restoreGlobals restores globals that were saved before local injection.
func restoreGlobals(v *vm.VM, localNames map[string]bool, saved map[string]vm.Value) {
	for name := range localNames {
		if savedVal, ok := saved[name]; ok {
			v.Globals[name] = savedVal
		} else {
			delete(v.Globals, name)
		}
	}
}

// wrapWorkspaceSource wraps workspace source as a doIt method.
// For multi-statement source, all statements execute and the last one's
// value is returned (^ only before the last statement).
func wrapWorkspaceSource(source string) string {
	source = strings.TrimSpace(source)
	source = strings.TrimSuffix(source, ".")

	// Split on statement separators
	stmts := splitStatements(source)
	if len(stmts) <= 1 {
		return "doIt\n    ^" + source
	}

	// All statements but last execute normally, last one returns
	var buf strings.Builder
	buf.WriteString("doIt\n")
	for i, stmt := range stmts {
		stmt = strings.TrimSpace(stmt)
		if stmt == "" {
			continue
		}
		if i == len(stmts)-1 {
			buf.WriteString("    ^")
		} else {
			buf.WriteString("    ")
		}
		buf.WriteString(stmt)
		if i < len(stmts)-1 {
			buf.WriteString(".\n")
		}
	}
	return buf.String()
}

// splitStatements splits Smalltalk source on period statement separators,
// handling string literals and nested constructs.
func splitStatements(source string) []string {
	var stmts []string
	var current strings.Builder
	inString := false

	for i := 0; i < len(source); i++ {
		ch := source[i]
		if ch == '\'' {
			inString = !inString
			current.WriteByte(ch)
		} else if ch == '.' && !inString {
			s := strings.TrimSpace(current.String())
			if s != "" {
				stmts = append(stmts, s)
			}
			current.Reset()
		} else {
			current.WriteByte(ch)
		}
	}
	s := strings.TrimSpace(current.String())
	if s != "" {
		stmts = append(stmts, s)
	}
	return stmts
}

func sideStr(classSide bool) string {
	if classSide {
		return "class"
	}
	return "instance"
}
