package server

import (
	"context"
	"fmt"
	"sort"
	"strings"

	"connectrpc.com/connect"

	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
	"github.com/chazu/maggie/gen/maggie/v1/maggiev1connect"
	"github.com/chazu/maggie/vm"
)

// BrowseService implements the BrowsingService gRPC/Connect handler.
type BrowseService struct {
	maggiev1connect.UnimplementedBrowsingServiceHandler
	worker *VMWorker
}

// NewBrowseService creates a BrowseService.
func NewBrowseService(worker *VMWorker) *BrowseService {
	return &BrowseService{worker: worker}
}

// ListClasses returns all classes in the image.
func (s *BrowseService) ListClasses(
	ctx context.Context,
	req *connect.Request[maggiev1.ListClassesRequest],
) (*connect.Response[maggiev1.ListClassesResponse], error) {
	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		classes := v.Classes.All()
		infos := make([]*maggiev1.ClassInfo, 0, len(classes))
		for _, cls := range classes {
			if req.Msg.Category != "" && cls.Namespace != req.Msg.Category {
				continue
			}
			infos = append(infos, classToInfo(v, cls))
		}
		sort.Slice(infos, func(i, j int) bool { return infos[i].Name < infos[j].Name })
		return &maggiev1.ListClassesResponse{Classes: infos}
	})
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	return connect.NewResponse(result.(*maggiev1.ListClassesResponse)), nil
}

// GetClass returns detailed information about a single class.
func (s *BrowseService) GetClass(
	ctx context.Context,
	req *connect.Request[maggiev1.GetClassRequest],
) (*connect.Response[maggiev1.GetClassResponse], error) {
	if req.Msg.Name == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("name is required"))
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		cls := v.Classes.Lookup(req.Msg.Name)
		if cls == nil {
			return fmt.Errorf("class %q not found", req.Msg.Name)
		}
		return &maggiev1.GetClassResponse{
			Class:      classToInfo(v, cls),
			Comment:    cls.DocString,
			Categories: cls.MethodCategories(),
		}
	})
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	if errVal, ok := result.(error); ok {
		return nil, connect.NewError(connect.CodeNotFound, errVal)
	}
	return connect.NewResponse(result.(*maggiev1.GetClassResponse)), nil
}

// GetHierarchy returns the superclass chain and direct subclasses.
func (s *BrowseService) GetHierarchy(
	ctx context.Context,
	req *connect.Request[maggiev1.GetHierarchyRequest],
) (*connect.Response[maggiev1.GetHierarchyResponse], error) {
	if req.Msg.ClassName == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("class_name is required"))
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		cls := v.Classes.Lookup(req.Msg.ClassName)
		if cls == nil {
			return fmt.Errorf("class %q not found", req.Msg.ClassName)
		}

		// Superclass chain (from root to parent)
		supers := cls.Superclasses()
		superEntries := make([]*maggiev1.HierarchyEntry, len(supers))
		for i, sup := range supers {
			superEntries[i] = &maggiev1.HierarchyEntry{
				Name:        sup.Name,
				Depth:       int32(i),
				MethodCount: int32(len(sup.AllMethods())),
			}
		}

		// Self
		selfEntry := &maggiev1.HierarchyEntry{
			Name:        cls.Name,
			Depth:       int32(len(supers)),
			MethodCount: int32(len(cls.AllMethods())),
		}

		// Direct subclasses
		var subEntries []*maggiev1.HierarchyEntry
		for _, other := range v.Classes.All() {
			if other.Superclass == cls {
				subEntries = append(subEntries, &maggiev1.HierarchyEntry{
					Name:        other.Name,
					Depth:       int32(len(supers)) + 1,
					MethodCount: int32(len(other.AllMethods())),
				})
			}
		}
		sort.Slice(subEntries, func(i, j int) bool { return subEntries[i].Name < subEntries[j].Name })

		return &maggiev1.GetHierarchyResponse{
			Superclasses: superEntries,
			Self:         selfEntry,
			Subclasses:   subEntries,
		}
	})
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	if errVal, ok := result.(error); ok {
		return nil, connect.NewError(connect.CodeNotFound, errVal)
	}
	return connect.NewResponse(result.(*maggiev1.GetHierarchyResponse)), nil
}

// ListMethods returns methods for a class.
func (s *BrowseService) ListMethods(
	ctx context.Context,
	req *connect.Request[maggiev1.ListMethodsRequest],
) (*connect.Response[maggiev1.ListMethodsResponse], error) {
	if req.Msg.ClassName == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("class_name is required"))
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		cls := v.Classes.Lookup(req.Msg.ClassName)
		if cls == nil {
			return fmt.Errorf("class %q not found", req.Msg.ClassName)
		}

		var infos []*maggiev1.MethodInfo

		// Instance methods
		if req.Msg.Side == maggiev1.ListMethodsRequest_INSTANCE || req.Msg.Side == maggiev1.ListMethodsRequest_BOTH {
			infos = append(infos, collectMethods(v, cls, cls.VTable, false, req.Msg.Category)...)
		}

		// Class methods
		if req.Msg.Side == maggiev1.ListMethodsRequest_CLASS || req.Msg.Side == maggiev1.ListMethodsRequest_BOTH {
			infos = append(infos, collectMethods(v, cls, cls.ClassVTable, true, req.Msg.Category)...)
		}

		sort.Slice(infos, func(i, j int) bool { return infos[i].Selector < infos[j].Selector })
		return &maggiev1.ListMethodsResponse{Methods: infos}
	})
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	if errVal, ok := result.(error); ok {
		return nil, connect.NewError(connect.CodeNotFound, errVal)
	}
	return connect.NewResponse(result.(*maggiev1.ListMethodsResponse)), nil
}

// GetMethod returns full details for a single method including source.
func (s *BrowseService) GetMethod(
	ctx context.Context,
	req *connect.Request[maggiev1.GetMethodRequest],
) (*connect.Response[maggiev1.GetMethodResponse], error) {
	if req.Msg.ClassName == "" || req.Msg.Selector == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("class_name and selector are required"))
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		cls := v.Classes.Lookup(req.Msg.ClassName)
		if cls == nil {
			return fmt.Errorf("class %q not found", req.Msg.ClassName)
		}

		vt := cls.VTable
		if req.Msg.ClassSide {
			vt = cls.ClassVTable
		}

		selID := v.Selectors.Lookup(req.Msg.Selector)
		if selID < 0 {
			return fmt.Errorf("selector %q not found", req.Msg.Selector)
		}

		method := vt.LookupLocal(selID)
		if method == nil {
			return fmt.Errorf("method %s>>%s not found", req.Msg.ClassName, req.Msg.Selector)
		}

		info := methodToInfo(v, cls, method, selID, req.Msg.ClassSide)

		resp := &maggiev1.GetMethodResponse{
			Method: info,
		}
		if cm, ok := method.(*vm.CompiledMethod); ok {
			resp.Category = cm.Category()
			resp.Arity = int32(cm.Arity)
		}
		return resp
	})
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	if errVal, ok := result.(error); ok {
		return nil, connect.NewError(connect.CodeNotFound, errVal)
	}
	return connect.NewResponse(result.(*maggiev1.GetMethodResponse)), nil
}

// FindSenders scans bytecode of all methods for sends of the given selector.
func (s *BrowseService) FindSenders(
	ctx context.Context,
	req *connect.Request[maggiev1.FindSendersRequest],
) (*connect.Response[maggiev1.FindSendersResponse], error) {
	if req.Msg.Selector == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("selector is required"))
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		targetSelID := v.Selectors.Lookup(req.Msg.Selector)
		if targetSelID < 0 {
			return &maggiev1.FindSendersResponse{}
		}

		// Map specialized opcodes to their selector names
		specializedSends := map[string]vm.Opcode{
			"+": vm.OpSendPlus, "-": vm.OpSendMinus, "*": vm.OpSendTimes,
			"/": vm.OpSendDiv, "\\\\": vm.OpSendMod,
			"<": vm.OpSendLT, ">": vm.OpSendGT, "<=": vm.OpSendLE,
			">=": vm.OpSendGE, "=": vm.OpSendEQ, "~=": vm.OpSendNE,
			"at:": vm.OpSendAt, "at:put:": vm.OpSendAtPut,
			"size": vm.OpSendSize, "value": vm.OpSendValue,
			"value:": vm.OpSendValue1, "value:value:": vm.OpSendValue2,
			"new": vm.OpSendNew, "class": vm.OpSendClass,
		}

		// Check if the target is a specialized send
		targetSpecialized, isSpecialized := specializedSends[req.Msg.Selector]

		var senders []*maggiev1.SenderInfo
		for _, cls := range v.Classes.All() {
			// Scan instance methods
			scanVTableForSenders(v, cls, cls.VTable, false, targetSelID, isSpecialized, targetSpecialized, &senders)
			// Scan class methods
			scanVTableForSenders(v, cls, cls.ClassVTable, true, targetSelID, isSpecialized, targetSpecialized, &senders)
		}

		sort.Slice(senders, func(i, j int) bool {
			if senders[i].ClassName != senders[j].ClassName {
				return senders[i].ClassName < senders[j].ClassName
			}
			return senders[i].MethodSelector < senders[j].MethodSelector
		})
		return &maggiev1.FindSendersResponse{Senders: senders}
	})
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	return connect.NewResponse(result.(*maggiev1.FindSendersResponse)), nil
}

// FindImplementors returns classes that define a method with the given selector.
func (s *BrowseService) FindImplementors(
	ctx context.Context,
	req *connect.Request[maggiev1.FindImplementorsRequest],
) (*connect.Response[maggiev1.FindImplementorsResponse], error) {
	if req.Msg.Selector == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("selector is required"))
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		selID := v.Selectors.Lookup(req.Msg.Selector)
		if selID < 0 {
			return &maggiev1.FindImplementorsResponse{}
		}

		var impls []*maggiev1.ImplementorInfo
		for _, cls := range v.Classes.All() {
			if cls.VTable.LookupLocal(selID) != nil {
				impls = append(impls, &maggiev1.ImplementorInfo{
					ClassName:   cls.Name,
					IsClassSide: false,
				})
			}
			if cls.ClassVTable != nil && cls.ClassVTable.LookupLocal(selID) != nil {
				impls = append(impls, &maggiev1.ImplementorInfo{
					ClassName:   cls.Name,
					IsClassSide: true,
				})
			}
		}

		sort.Slice(impls, func(i, j int) bool {
			if impls[i].ClassName != impls[j].ClassName {
				return impls[i].ClassName < impls[j].ClassName
			}
			return !impls[i].IsClassSide && impls[j].IsClassSide
		})
		return &maggiev1.FindImplementorsResponse{Implementors: impls}
	})
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	return connect.NewResponse(result.(*maggiev1.FindImplementorsResponse)), nil
}

// SearchSelectors returns selectors containing the query substring.
func (s *BrowseService) SearchSelectors(
	ctx context.Context,
	req *connect.Request[maggiev1.SearchSelectorsRequest],
) (*connect.Response[maggiev1.SearchSelectorsResponse], error) {
	if req.Msg.Query == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("query is required"))
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		query := strings.ToLower(req.Msg.Query)
		var matches []string
		for _, name := range v.Selectors.All() {
			if name != "" && strings.Contains(strings.ToLower(name), query) {
				matches = append(matches, name)
			}
		}
		sort.Strings(matches)
		const maxResults = 200
		if len(matches) > maxResults {
			matches = matches[:maxResults]
		}
		return &maggiev1.SearchSelectorsResponse{Selectors: matches}
	})
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	return connect.NewResponse(result.(*maggiev1.SearchSelectorsResponse)), nil
}

// SearchClasses returns classes whose name contains the query substring.
func (s *BrowseService) SearchClasses(
	ctx context.Context,
	req *connect.Request[maggiev1.SearchClassesRequest],
) (*connect.Response[maggiev1.SearchClassesResponse], error) {
	if req.Msg.Query == "" {
		return nil, connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("query is required"))
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		query := strings.ToLower(req.Msg.Query)
		var infos []*maggiev1.ClassInfo
		for _, cls := range v.Classes.All() {
			if strings.Contains(strings.ToLower(cls.Name), query) {
				infos = append(infos, classToInfo(v, cls))
			}
		}
		sort.Slice(infos, func(i, j int) bool { return infos[i].Name < infos[j].Name })
		return &maggiev1.SearchClassesResponse{Classes: infos}
	})
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	return connect.NewResponse(result.(*maggiev1.SearchClassesResponse)), nil
}

// --- helpers ---

// classToInfo converts a Class to a ClassInfo proto message.
func classToInfo(v *vm.VM, cls *vm.Class) *maggiev1.ClassInfo {
	info := &maggiev1.ClassInfo{
		Name:                  cls.Name,
		InstanceVariableNames: cls.InstVars,
		ClassVariableNames:    cls.ClassVars,
	}
	if cls.Superclass != nil {
		info.SuperclassName = cls.Superclass.Name
	}
	info.InstanceMethodCount = int32(countLocalMethods(cls.VTable))
	if cls.ClassVTable != nil {
		info.ClassMethodCount = int32(countLocalMethods(cls.ClassVTable))
	}
	return info
}

// countLocalMethods counts methods defined directly on a vtable (not inherited).
func countLocalMethods(vt *vm.VTable) int {
	if vt == nil {
		return 0
	}
	return len(vt.LocalMethods())
}

// collectMethods gathers MethodInfo for methods in a vtable.
func collectMethods(v *vm.VM, cls *vm.Class, vt *vm.VTable, classSide bool, category string) []*maggiev1.MethodInfo {
	if vt == nil {
		return nil
	}
	var infos []*maggiev1.MethodInfo
	for selID, method := range vt.LocalMethods() {
		if category != "" {
			if cm, ok := method.(*vm.CompiledMethod); ok {
				if cm.Category() != category {
					continue
				}
			}
		}
		infos = append(infos, methodToInfo(v, cls, method, selID, classSide))
	}
	return infos
}

// methodToInfo converts a Method to a MethodInfo proto message.
func methodToInfo(v *vm.VM, cls *vm.Class, method vm.Method, selID int, classSide bool) *maggiev1.MethodInfo {
	selectorName := v.Selectors.Name(selID)
	info := &maggiev1.MethodInfo{
		Selector:    selectorName,
		ClassName:   cls.Name,
		IsClassSide: classSide,
	}

	if cm, ok := method.(*vm.CompiledMethod); ok {
		info.IsPrimitive = false
		info.Source = cm.Source
		info.DocString = cm.DocString()
	} else {
		info.IsPrimitive = true
	}
	return info
}

// scanVTableForSenders scans all compiled methods in a vtable for sends
// of the target selector.
func scanVTableForSenders(
	v *vm.VM,
	cls *vm.Class,
	vt *vm.VTable,
	classSide bool,
	targetSelID int,
	isSpecialized bool,
	targetSpecialized vm.Opcode,
	senders *[]*maggiev1.SenderInfo,
) {
	if vt == nil {
		return
	}
	for selID, method := range vt.LocalMethods() {
		cm, ok := method.(*vm.CompiledMethod)
		if !ok {
			continue
		}
		if bytecodeSendsSelector(cm.Bytecode, targetSelID, isSpecialized, targetSpecialized) {
			*senders = append(*senders, &maggiev1.SenderInfo{
				ClassName:      cls.Name,
				MethodSelector: v.Selectors.Name(selID),
				IsClassSide:    classSide,
			})
		}
	}
}

// bytecodeSendsSelector scans bytecode for OpSend/OpSendSuper with the target
// selector ID, or for a specialized send opcode.
func bytecodeSendsSelector(bytecode []byte, targetSelID int, isSpecialized bool, targetOp vm.Opcode) bool {
	i := 0
	for i < len(bytecode) {
		op := vm.Opcode(bytecode[i])

		// Check specialized send (single-byte, no operands)
		if isSpecialized && op == targetOp {
			return true
		}

		// Check OpSend / OpSendSuper (4 bytes: op + 2-byte selector + 1-byte argc)
		if op == vm.OpSend || op == vm.OpSendSuper {
			if i+2 < len(bytecode) {
				selID := int(bytecode[i+1]) | int(bytecode[i+2])<<8
				if selID == targetSelID {
					return true
				}
			}
		}

		// Advance past instruction
		info := op.Info()
		i += 1 + info.OperandBytes
	}
	return false
}
