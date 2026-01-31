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

// InspectService implements the InspectionService gRPC/Connect handler.
type InspectService struct {
	maggiev1connect.UnimplementedInspectionServiceHandler
	worker  *VMWorker
	handles *HandleStore
}

// NewInspectService creates an InspectService.
func NewInspectService(worker *VMWorker, handles *HandleStore) *InspectService {
	return &InspectService{
		worker:  worker,
		handles: handles,
	}
}

// Inspect returns full inspection details for an object by handle.
func (s *InspectService) Inspect(
	ctx context.Context,
	req *connect.Request[maggiev1.InspectRequest],
) (*connect.Response[maggiev1.InspectResponse], error) {
	if req.Msg.HandleId == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("handle_id is required"))
	}

	val, ok := s.handles.Lookup(req.Msg.HandleId)
	if !ok {
		return nil, connect.NewError(connect.CodeNotFound, fmt.Errorf("handle %q not found", req.Msg.HandleId))
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		return s.inspectValue(v, val, req.Msg.HandleId)
	})
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	return connect.NewResponse(result.(*maggiev1.InspectResponse)), nil
}

// InspectSlot drills into a named slot (instance variable) of an object.
func (s *InspectService) InspectSlot(
	ctx context.Context,
	req *connect.Request[maggiev1.InspectSlotRequest],
) (*connect.Response[maggiev1.InspectResponse], error) {
	if req.Msg.HandleId == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("handle_id is required"))
	}
	if req.Msg.SlotName == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("slot_name is required"))
	}

	val, ok := s.handles.Lookup(req.Msg.HandleId)
	if !ok {
		return nil, connect.NewError(connect.CodeNotFound, fmt.Errorf("handle %q not found", req.Msg.HandleId))
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		// For indexed slot names like "[0]", "[1]", etc., extract the index
		if strings.HasPrefix(req.Msg.SlotName, "[") && strings.HasSuffix(req.Msg.SlotName, "]") {
			var idx int
			if _, scanErr := fmt.Sscanf(req.Msg.SlotName, "[%d]", &idx); scanErr == nil {
				elemVal := v.Send(val, "at:", []vm.Value{vm.FromSmallInt(int64(idx))})
				display := formatValue(v, elemVal)
				className := classNameFor(v, elemVal)
				handleID := s.handles.Create(elemVal, className, display, "")
				return s.inspectValue(v, elemVal, handleID)
			}
		}

		// Named slot: find the ivar index from the class
		obj := vm.ObjectFromValue(val)
		if obj == nil {
			return fmt.Errorf("value is not an object, cannot inspect slot %q", req.Msg.SlotName)
		}

		cls := v.ClassFor(val)
		if cls == nil {
			return fmt.Errorf("cannot determine class for object")
		}

		ivarNames := cls.AllInstVarNames()
		slotIdx := -1
		for i, name := range ivarNames {
			if name == req.Msg.SlotName {
				slotIdx = i
				break
			}
		}
		if slotIdx < 0 {
			return fmt.Errorf("slot %q not found on class %s", req.Msg.SlotName, cls.Name)
		}

		if slotIdx >= obj.NumSlots() {
			return fmt.Errorf("slot index %d out of range for object with %d slots", slotIdx, obj.NumSlots())
		}

		slotVal := obj.GetSlot(slotIdx)
		display := formatValue(v, slotVal)
		className := classNameFor(v, slotVal)
		handleID := s.handles.Create(slotVal, className, display, "")
		return s.inspectValue(v, slotVal, handleID)
	})
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	if errVal, ok := result.(error); ok {
		return nil, connect.NewError(connect.CodeNotFound, errVal)
	}
	return connect.NewResponse(result.(*maggiev1.InspectResponse)), nil
}

// InspectIndex drills into an indexed element of an indexable object.
func (s *InspectService) InspectIndex(
	ctx context.Context,
	req *connect.Request[maggiev1.InspectIndexRequest],
) (*connect.Response[maggiev1.InspectResponse], error) {
	if req.Msg.HandleId == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("handle_id is required"))
	}

	val, ok := s.handles.Lookup(req.Msg.HandleId)
	if !ok {
		return nil, connect.NewError(connect.CodeNotFound, fmt.Errorf("handle %q not found", req.Msg.HandleId))
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		elemVal := v.Send(val, "at:", []vm.Value{vm.FromSmallInt(int64(req.Msg.Index))})
		display := formatValue(v, elemVal)
		className := classNameFor(v, elemVal)
		handleID := s.handles.Create(elemVal, className, display, "")
		return s.inspectValue(v, elemVal, handleID)
	})
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	return connect.NewResponse(result.(*maggiev1.InspectResponse)), nil
}

// SendMessage sends a message to an inspected object and returns the result.
func (s *InspectService) SendMessage(
	ctx context.Context,
	req *connect.Request[maggiev1.SendMessageRequest],
) (*connect.Response[maggiev1.SendMessageResponse], error) {
	if req.Msg.HandleId == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("handle_id is required"))
	}
	if req.Msg.Selector == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("selector is required"))
	}

	receiver, ok := s.handles.Lookup(req.Msg.HandleId)
	if !ok {
		return nil, connect.NewError(connect.CodeNotFound, fmt.Errorf("handle %q not found", req.Msg.HandleId))
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		// Compile and evaluate each argument expression
		args := make([]vm.Value, 0, len(req.Msg.Arguments))
		for _, argSource := range req.Msg.Arguments {
			method, compileErr := v.CompileExpression(argSource)
			if compileErr != nil {
				return &maggiev1.SendMessageResponse{
					Success:      false,
					ErrorMessage: fmt.Sprintf("failed to compile argument %q: %s", argSource, compileErr.Error()),
				}
			}
			if method == nil {
				return &maggiev1.SendMessageResponse{
					Success:      false,
					ErrorMessage: fmt.Sprintf("failed to compile argument %q: compiler returned nil", argSource),
				}
			}
			argVal := v.Execute(method, vm.Nil, nil)
			args = append(args, argVal)
		}

		// Send the message
		resultVal := v.Send(receiver, req.Msg.Selector, args)

		display := formatValue(v, resultVal)
		className := classNameFor(v, resultVal)
		handleID := s.handles.Create(resultVal, className, display, "")

		return &maggiev1.SendMessageResponse{
			Success: true,
			Result:  display,
			ResultHandle: &maggiev1.ObjectHandle{
				Id:            handleID,
				ClassName:     className,
				DisplayString: display,
			},
		}
	})
	if err != nil {
		return connect.NewResponse(&maggiev1.SendMessageResponse{
			Success:      false,
			ErrorMessage: err.Error(),
		}), nil
	}
	return connect.NewResponse(result.(*maggiev1.SendMessageResponse)), nil
}

// ReleaseHandle releases an object handle, allowing the VM to garbage-collect it.
func (s *InspectService) ReleaseHandle(
	ctx context.Context,
	req *connect.Request[maggiev1.ReleaseHandleRequest],
) (*connect.Response[maggiev1.ReleaseHandleResponse], error) {
	if req.Msg.HandleId == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("handle_id is required"))
	}

	s.handles.Release(req.Msg.HandleId)
	return connect.NewResponse(&maggiev1.ReleaseHandleResponse{
		Success: true,
	}), nil
}

// --- helpers ---

// inspectValue builds an InspectResponse for a VM value.
// Must be called on the VM worker goroutine.
func (s *InspectService) inspectValue(v *vm.VM, val vm.Value, handleID string) *maggiev1.InspectResponse {
	inspector := vm.NewInspector(v)
	result := inspector.Inspect(val)

	className := result.ClassName
	if className == "" {
		className = classNameFor(v, val)
	}

	display := result.Value

	resp := &maggiev1.InspectResponse{
		ClassName:     className,
		DisplayString: display,
		Handle: &maggiev1.ObjectHandle{
			Id:            handleID,
			ClassName:     className,
			DisplayString: display,
		},
	}

	// Populate named slots (instance variables)
	var slots []*maggiev1.SlotInfo

	obj := vm.ObjectFromValue(val)
	if obj != nil {
		cls := v.ClassFor(val)
		if cls != nil {
			ivarNames := cls.AllInstVarNames()
			for i, name := range ivarNames {
				if i < obj.NumSlots() {
					slotVal := obj.GetSlot(i)
					slotDisplay := formatValue(v, slotVal)
					slotClassName := classNameFor(v, slotVal)
					slotHandleID := s.handles.Create(slotVal, slotClassName, slotDisplay, "")
					slots = append(slots, &maggiev1.SlotInfo{
						Name:         name,
						ValueDisplay: slotDisplay,
						ValueClass:   slotClassName,
						ValueHandle: &maggiev1.ObjectHandle{
							Id:            slotHandleID,
							ClassName:     slotClassName,
							DisplayString: slotDisplay,
						},
					})
				}
			}
		}
	}

	// Populate indexed elements for indexable objects (arrays)
	if result.Size > 0 {
		resp.IsIndexable = true
		resp.IndexableSize = int32(result.Size)

		limit := result.Size
		if limit > 100 {
			limit = 100
		}
		for i := 0; i < limit; i++ {
			elemVal := v.Send(val, "at:", []vm.Value{vm.FromSmallInt(int64(i))})
			elemDisplay := formatValue(v, elemVal)
			elemClassName := classNameFor(v, elemVal)
			elemHandleID := s.handles.Create(elemVal, elemClassName, elemDisplay, "")
			slots = append(slots, &maggiev1.SlotInfo{
				Name:         fmt.Sprintf("[%d]", i),
				ValueDisplay: elemDisplay,
				ValueClass:   elemClassName,
				ValueHandle: &maggiev1.ObjectHandle{
					Id:            elemHandleID,
					ClassName:     elemClassName,
					DisplayString: elemDisplay,
				},
			})
		}
	}

	resp.Slots = slots
	return resp
}
