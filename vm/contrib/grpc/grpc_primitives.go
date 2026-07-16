package grpc

import (
	"context"
	"fmt"
	"io"
	"reflect"
	"strings"
	"sync"
	"sync/atomic"

	"github.com/jhump/protoreflect/desc"
	"github.com/jhump/protoreflect/dynamic"
	"github.com/jhump/protoreflect/grpcreflect"
	grpclib "google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
	rpb "google.golang.org/grpc/reflection/grpc_reflection_v1alpha"
	"google.golang.org/protobuf/types/descriptorpb"

	vm "github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// GrpcClient Registry
// ---------------------------------------------------------------------------

// GrpcClientObject wraps a gRPC connection with reflection support.
type GrpcClientObject struct {
	conn      *grpclib.ClientConn
	refClient *grpcreflect.Client
	target    string
	closed    atomic.Bool
	mu        sync.Mutex
}

func isGrpcClientValue(v vm.Value) bool {
	return vm.IsExtensionValue(v, vm.GrpcClientMarker)
}

func vmGetGrpcClient(vmInst *vm.VM, v vm.Value) *GrpcClientObject {
	if o := vm.ExtensionObject(v, vm.GrpcClientMarker); o != nil {
		return o.(*GrpcClientObject)
	}
	return nil
}

func vmRegisterGrpcClient(vmInst *vm.VM, c *GrpcClientObject) vm.Value {
	return vm.NewExtensionValue(vm.GrpcClientMarker, c)
}

// vmUnregisterGrpcClient is now a no-op: the client object is a pointer-carrying
// heap Value reclaimed by Go's GC. Retained so call sites need not change.
func vmUnregisterGrpcClient(vmInst *vm.VM, v vm.Value) {}

// ---------------------------------------------------------------------------
// GrpcStream Registry
// ---------------------------------------------------------------------------

// GrpcStreamType identifies the type of gRPC stream
type GrpcStreamType int

const (
	GrpcStreamServerStreaming GrpcStreamType = iota
	GrpcStreamClientStreaming
	GrpcStreamBidirectional
)

// GrpcStreamObject wraps a gRPC stream for streaming calls.
type GrpcStreamObject struct {
	stream     grpclib.ClientStream
	clientVal  vm.Value // Reference to owning GrpcClientObject
	methodDesc *desc.MethodDescriptor
	streamType GrpcStreamType
	sendClosed atomic.Bool
	recvClosed atomic.Bool
	mu         sync.Mutex
}

func isGrpcStreamValue(v vm.Value) bool {
	return vm.IsExtensionValue(v, vm.GrpcStreamMarker)
}

func vmGetGrpcStream(vmInst *vm.VM, v vm.Value) *GrpcStreamObject {
	if o := vm.ExtensionObject(v, vm.GrpcStreamMarker); o != nil {
		return o.(*GrpcStreamObject)
	}
	return nil
}

func vmRegisterGrpcStream(vmInst *vm.VM, s *GrpcStreamObject) vm.Value {
	return vm.NewExtensionValue(vm.GrpcStreamMarker, s)
}

// vmUnregisterGrpcStream is now a no-op: the stream object is a pointer-carrying
// heap Value reclaimed by Go's GC. Retained so call sites need not change.
func vmUnregisterGrpcStream(vmInst *vm.VM, v vm.Value) {}

// ---------------------------------------------------------------------------
// Helper functions for Result creation
// ---------------------------------------------------------------------------

func grpcSuccess(vmInst *vm.VM, val vm.Value) vm.Value {
	return vmInst.NewSuccessResult(val)
}

func grpcFailure(vmInst *vm.VM, reason string) vm.Value {
	return vmInst.NewFailureResult(reason)
}

// ---------------------------------------------------------------------------
// Method resolution helpers
// ---------------------------------------------------------------------------

// resolveMethod resolves a method name like "service.Service/Method" to its descriptor
func resolveMethod(client *GrpcClientObject, fullMethod string) (*desc.MethodDescriptor, error) {
	// Parse "package.Service/Method" format
	parts := strings.Split(fullMethod, "/")
	if len(parts) != 2 {
		return nil, fmt.Errorf("invalid method format: %s (expected 'service/method')", fullMethod)
	}

	serviceName := parts[0]
	methodName := parts[1]

	svcDesc, err := client.refClient.ResolveService(serviceName)
	if err != nil {
		return nil, fmt.Errorf("cannot resolve service %s: %w", serviceName, err)
	}

	methodDesc := svcDesc.FindMethodByName(methodName)
	if methodDesc == nil {
		return nil, fmt.Errorf("method %s not found in service %s", methodName, serviceName)
	}

	return methodDesc, nil
}

// ---------------------------------------------------------------------------
// Message conversion: Maggie Dictionary <-> Protobuf
// ---------------------------------------------------------------------------

// dictionaryToProto converts a Maggie Dictionary to a protobuf dynamic message
func dictionaryToProto(vmInst *vm.VM, dictVal vm.Value, msgDesc *desc.MessageDescriptor) (*dynamic.Message, error) {
	dict := vmInst.Registry().GetDictionaryObject(dictVal)
	if dict == nil {
		return nil, fmt.Errorf("not a dictionary")
	}

	msg := dynamic.NewMessage(msgDesc)

	for _, entry := range dict.Entries() {
		key, val := entry.Key, entry.Value

		// Get field name from key (symbol or string)
		var fieldName string
		if key.IsSymbol() {
			fieldName = vmInst.Symbols.Name(key.SymbolID())
		} else if vm.IsStringValue(key) {
			fieldName = vmInst.Registry().GetStringContent(key)
		} else {
			continue // Skip non-string/symbol keys
		}

		field := msgDesc.FindFieldByName(fieldName)
		if field == nil {
			continue // Skip unknown fields
		}

		protoVal, err := valueToProtoField(vmInst, val, field)
		if err != nil {
			return nil, fmt.Errorf("field %s: %w", fieldName, err)
		}

		if err := msg.TrySetField(field, protoVal); err != nil {
			return nil, fmt.Errorf("setting field %s: %w", fieldName, err)
		}
	}

	return msg, nil
}

// valueToProtoField converts a Maggie Value to a protobuf field value
func valueToProtoField(vmInst *vm.VM, val vm.Value, field *desc.FieldDescriptor) (interface{}, error) {
	if field.IsRepeated() && !field.IsMap() {
		return valueToRepeatedField(vmInst, val, field)
	}

	switch field.GetType() {
	case descriptorpb.FieldDescriptorProto_TYPE_INT32,
		descriptorpb.FieldDescriptorProto_TYPE_SINT32,
		descriptorpb.FieldDescriptorProto_TYPE_SFIXED32:
		if val.IsSmallInt() {
			return int32(val.SmallInt()), nil
		}
	case descriptorpb.FieldDescriptorProto_TYPE_INT64,
		descriptorpb.FieldDescriptorProto_TYPE_SINT64,
		descriptorpb.FieldDescriptorProto_TYPE_SFIXED64:
		if val.IsSmallInt() {
			return val.SmallInt(), nil
		}
	case descriptorpb.FieldDescriptorProto_TYPE_UINT32,
		descriptorpb.FieldDescriptorProto_TYPE_FIXED32:
		if val.IsSmallInt() {
			return uint32(val.SmallInt()), nil
		}
	case descriptorpb.FieldDescriptorProto_TYPE_UINT64,
		descriptorpb.FieldDescriptorProto_TYPE_FIXED64:
		if val.IsSmallInt() {
			return uint64(val.SmallInt()), nil
		}
	case descriptorpb.FieldDescriptorProto_TYPE_FLOAT:
		if val.IsFloat() {
			return float32(val.Float64()), nil
		}
		if val.IsSmallInt() {
			return float32(val.SmallInt()), nil
		}
	case descriptorpb.FieldDescriptorProto_TYPE_DOUBLE:
		if val.IsFloat() {
			return val.Float64(), nil
		}
		if val.IsSmallInt() {
			return float64(val.SmallInt()), nil
		}
	case descriptorpb.FieldDescriptorProto_TYPE_BOOL:
		return val == vm.True, nil
	case descriptorpb.FieldDescriptorProto_TYPE_STRING:
		if vm.IsStringValue(val) {
			return vmInst.Registry().GetStringContent(val), nil
		}
		if val.IsSymbol() {
			return vmInst.Symbols.Name(val.SymbolID()), nil
		}
	case descriptorpb.FieldDescriptorProto_TYPE_BYTES:
		if vm.IsStringValue(val) {
			return []byte(vmInst.Registry().GetStringContent(val)), nil
		}
	case descriptorpb.FieldDescriptorProto_TYPE_MESSAGE:
		if vm.IsDictionaryValue(val) {
			return dictionaryToProto(vmInst, val, field.GetMessageType())
		}
	case descriptorpb.FieldDescriptorProto_TYPE_ENUM:
		if val.IsSmallInt() {
			return int32(val.SmallInt()), nil
		}
		if vm.IsStringValue(val) || val.IsSymbol() {
			name := ""
			if vm.IsStringValue(val) {
				name = vmInst.Registry().GetStringContent(val)
			} else {
				name = vmInst.Symbols.Name(val.SymbolID())
			}
			enumVal := field.GetEnumType().FindValueByName(name)
			if enumVal != nil {
				return enumVal.GetNumber(), nil
			}
			enumType := field.GetEnumType()
			var enumNames []string
			for _, ev := range enumType.GetValues() {
				enumNames = append(enumNames, ev.GetName())
			}
			return nil, fmt.Errorf("enum value %q not found in %s (available: %v)", name, enumType.GetName(), enumNames)
		}
	}

	return nil, fmt.Errorf("cannot convert value to proto type %v", field.GetType())
}

// valueToRepeatedField converts a Maggie Array to a repeated protobuf field
func valueToRepeatedField(vmInst *vm.VM, val vm.Value, field *desc.FieldDescriptor) (interface{}, error) {
	if !val.IsObject() {
		return nil, fmt.Errorf("expected array for repeated field")
	}

	obj := vm.ObjectFromValue(val)
	size := obj.NumSlots()
	result := make([]interface{}, size)

	for i := 0; i < size; i++ {
		elem := obj.GetSlot(i)
		protoVal, err := valueToProtoField(vmInst, elem, field)
		if err != nil {
			return nil, fmt.Errorf("element %d: %w", i, err)
		}
		result[i] = protoVal
	}

	return result, nil
}

// protoToDictionary converts a protobuf dynamic message to a Maggie Dictionary
func protoToDictionary(vmInst *vm.VM, msg *dynamic.Message) (vm.Value, error) {
	dict := vmInst.Registry().NewDictionaryValue()

	for _, field := range msg.GetKnownFields() {
		hasField := msg.HasField(field)
		if !hasField {
			continue
		}

		val := msg.GetField(field)
		maggieVal, err := protoFieldToValue(vmInst, val, field)
		if err != nil {
			return vm.Nil, fmt.Errorf("field %s: %w", field.GetName(), err)
		}

		key := vmInst.Symbols.SymbolValue(field.GetName())
		vmInst.DictionaryAtPut(dict, key, maggieVal)
	}

	return dict, nil
}

// protoFieldToValue converts a protobuf field value to a Maggie Value
func protoFieldToValue(vmInst *vm.VM, val interface{}, field *desc.FieldDescriptor) (vm.Value, error) {
	if field.IsRepeated() && !field.IsMap() {
		return repeatedFieldToValue(vmInst, val, field)
	}

	// Handle map fields
	if field.IsMap() {
		return mapFieldToValue(vmInst, val, field)
	}

	switch field.GetType() {
	case descriptorpb.FieldDescriptorProto_TYPE_INT32,
		descriptorpb.FieldDescriptorProto_TYPE_SINT32,
		descriptorpb.FieldDescriptorProto_TYPE_SFIXED32:
		return vm.FromSmallInt(int64(val.(int32))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_INT64,
		descriptorpb.FieldDescriptorProto_TYPE_SINT64,
		descriptorpb.FieldDescriptorProto_TYPE_SFIXED64:
		if v, ok := vm.TryFromSmallInt(val.(int64)); ok {
			return v, nil
		}
		return vm.FromFloat64(float64(val.(int64))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_UINT32,
		descriptorpb.FieldDescriptorProto_TYPE_FIXED32:
		return vm.FromSmallInt(int64(val.(uint32))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_UINT64,
		descriptorpb.FieldDescriptorProto_TYPE_FIXED64:
		if v, ok := vm.TryFromSmallInt(int64(val.(uint64))); ok {
			return v, nil
		}
		return vm.FromFloat64(float64(val.(uint64))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_FLOAT:
		return vm.FromFloat64(float64(val.(float32))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_DOUBLE:
		return vm.FromFloat64(val.(float64)), nil
	case descriptorpb.FieldDescriptorProto_TYPE_BOOL:
		if val.(bool) {
			return vm.True, nil
		}
		return vm.False, nil
	case descriptorpb.FieldDescriptorProto_TYPE_STRING:
		return vmInst.Registry().NewStringValue(val.(string)), nil
	case descriptorpb.FieldDescriptorProto_TYPE_BYTES:
		return vmInst.Registry().NewStringValue(string(val.([]byte))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_MESSAGE:
		return protoToDictionary(vmInst, val.(*dynamic.Message))
	case descriptorpb.FieldDescriptorProto_TYPE_ENUM:
		enumNum := val.(int32)
		enumVal := field.GetEnumType().FindValueByNumber(enumNum)
		if enumVal != nil {
			return vmInst.Symbols.SymbolValue(enumVal.GetName()), nil
		}
		return vm.FromSmallInt(int64(enumNum)), nil
	}

	return vm.Nil, fmt.Errorf("unsupported proto type: %v", field.GetType())
}

// repeatedFieldToValue converts a repeated protobuf field to a Maggie Array
func repeatedFieldToValue(vmInst *vm.VM, val interface{}, field *desc.FieldDescriptor) (vm.Value, error) {
	slice := reflect.ValueOf(val)
	elements := make([]vm.Value, slice.Len())

	for i := 0; i < slice.Len(); i++ {
		elem := slice.Index(i).Interface()
		maggieVal, err := protoElementToValue(vmInst, elem, field)
		if err != nil {
			return vm.Nil, err
		}
		elements[i] = maggieVal
	}

	return vmInst.NewArrayWithElements(elements), nil
}

// protoElementToValue converts a single element from a repeated field to a Maggie Value
func protoElementToValue(vmInst *vm.VM, val interface{}, field *desc.FieldDescriptor) (vm.Value, error) {
	switch field.GetType() {
	case descriptorpb.FieldDescriptorProto_TYPE_INT32,
		descriptorpb.FieldDescriptorProto_TYPE_SINT32,
		descriptorpb.FieldDescriptorProto_TYPE_SFIXED32:
		return vm.FromSmallInt(int64(val.(int32))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_INT64,
		descriptorpb.FieldDescriptorProto_TYPE_SINT64,
		descriptorpb.FieldDescriptorProto_TYPE_SFIXED64:
		if v, ok := vm.TryFromSmallInt(val.(int64)); ok {
			return v, nil
		}
		return vm.FromFloat64(float64(val.(int64))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_UINT32,
		descriptorpb.FieldDescriptorProto_TYPE_FIXED32:
		return vm.FromSmallInt(int64(val.(uint32))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_UINT64,
		descriptorpb.FieldDescriptorProto_TYPE_FIXED64:
		if v, ok := vm.TryFromSmallInt(int64(val.(uint64))); ok {
			return v, nil
		}
		return vm.FromFloat64(float64(val.(uint64))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_FLOAT:
		return vm.FromFloat64(float64(val.(float32))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_DOUBLE:
		return vm.FromFloat64(val.(float64)), nil
	case descriptorpb.FieldDescriptorProto_TYPE_BOOL:
		if val.(bool) {
			return vm.True, nil
		}
		return vm.False, nil
	case descriptorpb.FieldDescriptorProto_TYPE_STRING:
		return vmInst.Registry().NewStringValue(val.(string)), nil
	case descriptorpb.FieldDescriptorProto_TYPE_BYTES:
		return vmInst.Registry().NewStringValue(string(val.([]byte))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_MESSAGE:
		return protoToDictionary(vmInst, val.(*dynamic.Message))
	case descriptorpb.FieldDescriptorProto_TYPE_ENUM:
		enumNum := val.(int32)
		enumVal := field.GetEnumType().FindValueByNumber(enumNum)
		if enumVal != nil {
			return vmInst.Symbols.SymbolValue(enumVal.GetName()), nil
		}
		return vm.FromSmallInt(int64(enumNum)), nil
	}

	return vm.Nil, fmt.Errorf("unsupported proto type: %v", field.GetType())
}

// mapFieldToValue converts a protobuf map field to a Maggie Dictionary
func mapFieldToValue(vmInst *vm.VM, val interface{}, field *desc.FieldDescriptor) (vm.Value, error) {
	mapVal, ok := val.(map[interface{}]interface{})
	if !ok {
		return vm.Nil, fmt.Errorf("expected map, got %T", val)
	}

	dict := vmInst.Registry().NewDictionaryValue()
	mapEntry := field.GetMapKeyType()
	valueField := field.GetMapValueType()

	for k, v := range mapVal {
		var keyVal vm.Value
		switch kTyped := k.(type) {
		case string:
			keyVal = vmInst.Symbols.SymbolValue(kTyped)
		case int32:
			keyVal = vm.FromSmallInt(int64(kTyped))
		case int64:
			if sv, ok := vm.TryFromSmallInt(kTyped); ok {
				keyVal = sv
			} else {
				keyVal = vm.FromFloat64(float64(kTyped))
			}
		default:
			if mapEntry != nil {
				var err error
				keyVal, err = protoElementToValue(vmInst, k, mapEntry)
				if err != nil {
					return vm.Nil, fmt.Errorf("map key conversion: %w", err)
				}
			} else {
				keyVal = vmInst.Registry().NewStringValue(fmt.Sprintf("%v", k))
			}
		}

		var valueVal vm.Value
		if valueField != nil {
			var err error
			valueVal, err = protoElementToValue(vmInst, v, valueField)
			if err != nil {
				return vm.Nil, fmt.Errorf("map value conversion: %w", err)
			}
		} else {
			switch vTyped := v.(type) {
			case string:
				valueVal = vmInst.Registry().NewStringValue(vTyped)
			case int32:
				valueVal = vm.FromSmallInt(int64(vTyped))
			case int64:
				if sv, ok := vm.TryFromSmallInt(vTyped); ok {
					valueVal = sv
				} else {
					valueVal = vm.FromFloat64(float64(vTyped))
				}
			case bool:
				if vTyped {
					valueVal = vm.True
				} else {
					valueVal = vm.False
				}
			default:
				valueVal = vmInst.Registry().NewStringValue(fmt.Sprintf("%v", v))
			}
		}

		vmInst.DictionaryAtPut(dict, keyVal, valueVal)
	}

	return dict, nil
}

// ---------------------------------------------------------------------------
// GrpcClient Primitives Registration
// ---------------------------------------------------------------------------

func RegisterGrpcPrimitives(v *vm.VM) {
	grpcClientClass := v.CreateClass("GrpcClient", v.ObjectClass)
	grpcStreamClass := v.CreateClass("GrpcStream", v.ObjectClass)

	v.SetGlobal("GrpcClient", v.ClassValue(grpcClientClass))
	v.SetGlobal("GrpcStream", v.ClassValue(grpcStreamClass))

	v.RegisterSymbolDispatchEntry(vm.GrpcClientMarker, &vm.SymbolTypeEntry{Class: grpcClientClass})
	v.RegisterSymbolDispatchEntry(vm.GrpcStreamMarker, &vm.SymbolTypeEntry{Class: grpcStreamClass})

	c := grpcClientClass

	// GrpcClient class>>connectTo: target - connect to gRPC server
	c.AddClassMethod1(v.Selectors, "connectTo:", func(vmInst *vm.VM, recv vm.Value, target vm.Value) vm.Value {
		if !vm.IsStringValue(target) {
			return grpcFailure(vmInst, "target must be a string")
		}
		targetStr := vmInst.Registry().GetStringContent(target)

		conn, err := grpclib.Dial(targetStr, grpclib.WithTransportCredentials(insecure.NewCredentials()))
		if err != nil {
			return grpcFailure(vmInst, fmt.Sprintf("connection failed: %v", err))
		}

		refClient := grpcreflect.NewClientV1Alpha(context.Background(), rpb.NewServerReflectionClient(conn))

		clientObj := &GrpcClientObject{
			conn:      conn,
			refClient: refClient,
			target:    targetStr,
		}

		return grpcSuccess(vmInst, vmRegisterGrpcClient(vmInst, clientObj))
	})

	// GrpcClient>>close - close connection
	c.AddMethod0(v.Selectors, "close", func(vmInst *vm.VM, recv vm.Value) vm.Value {
		client := vmGetGrpcClient(vmInst, recv)
		if client == nil {
			return recv
		}
		client.mu.Lock()
		defer client.mu.Unlock()
		if !client.closed.Load() {
			client.closed.Store(true)
			client.refClient.Reset()
			client.conn.Close()
			vmUnregisterGrpcClient(vmInst, recv)
		}
		return recv
	})

	// GrpcClient>>isConnected - check if connected
	c.AddMethod0(v.Selectors, "isConnected", func(vmInst *vm.VM, recv vm.Value) vm.Value {
		client := vmGetGrpcClient(vmInst, recv)
		if client == nil || client.closed.Load() {
			return vm.False
		}
		return vm.True
	})

	// GrpcClient>>listServices - list available services via reflection
	c.AddMethod0(v.Selectors, "listServices", func(vmInst *vm.VM, recv vm.Value) vm.Value {
		client := vmGetGrpcClient(vmInst, recv)
		if client == nil || client.closed.Load() {
			return vmInst.NewArray(0)
		}

		services, err := client.refClient.ListServices()
		if err != nil {
			return vmInst.NewArray(0)
		}

		elements := make([]vm.Value, 0, len(services))
		for _, svc := range services {
			if strings.HasPrefix(svc, "grpc.reflection") {
				continue
			}
			elements = append(elements, vmInst.Registry().NewStringValue(svc))
		}
		return vmInst.NewArrayWithElements(elements)
	})

	// GrpcClient>>methodsForService: serviceName - list methods for a service
	c.AddMethod1(v.Selectors, "methodsForService:", func(vmInst *vm.VM, recv vm.Value, serviceName vm.Value) vm.Value {
		client := vmGetGrpcClient(vmInst, recv)
		if client == nil || !vm.IsStringValue(serviceName) {
			return vmInst.NewArray(0)
		}

		svcName := vmInst.Registry().GetStringContent(serviceName)
		svcDesc, err := client.refClient.ResolveService(svcName)
		if err != nil {
			return vmInst.NewArray(0)
		}

		methods := svcDesc.GetMethods()
		elements := make([]vm.Value, len(methods))
		for i, m := range methods {
			elements[i] = vmInst.Registry().NewStringValue(m.GetName())
		}
		return vmInst.NewArrayWithElements(elements)
	})

	// GrpcClient>>methodDescriptor: methodName - get method metadata
	c.AddMethod1(v.Selectors, "methodDescriptor:", func(vmInst *vm.VM, recv vm.Value, methodName vm.Value) vm.Value {
		client := vmGetGrpcClient(vmInst, recv)
		if client == nil || !vm.IsStringValue(methodName) {
			return vm.Nil
		}

		methodDesc, err := resolveMethod(client, vmInst.Registry().GetStringContent(methodName))
		if err != nil {
			return vm.Nil
		}

		dict := vmInst.Registry().NewDictionaryValue()
		vmInst.DictionaryAtPut(dict, vmInst.Symbols.SymbolValue("name"), vmInst.Registry().NewStringValue(methodDesc.GetName()))
		vmInst.DictionaryAtPut(dict, vmInst.Symbols.SymbolValue("fullName"), vmInst.Registry().NewStringValue(methodDesc.GetFullyQualifiedName()))
		vmInst.DictionaryAtPut(dict, vmInst.Symbols.SymbolValue("inputType"), vmInst.Registry().NewStringValue(methodDesc.GetInputType().GetFullyQualifiedName()))
		vmInst.DictionaryAtPut(dict, vmInst.Symbols.SymbolValue("outputType"), vmInst.Registry().NewStringValue(methodDesc.GetOutputType().GetFullyQualifiedName()))
		if methodDesc.IsClientStreaming() {
			vmInst.DictionaryAtPut(dict, vmInst.Symbols.SymbolValue("isClientStreaming"), vm.True)
		} else {
			vmInst.DictionaryAtPut(dict, vmInst.Symbols.SymbolValue("isClientStreaming"), vm.False)
		}
		if methodDesc.IsServerStreaming() {
			vmInst.DictionaryAtPut(dict, vmInst.Symbols.SymbolValue("isServerStreaming"), vm.True)
		} else {
			vmInst.DictionaryAtPut(dict, vmInst.Symbols.SymbolValue("isServerStreaming"), vm.False)
		}

		return dict
	})

	// GrpcClient>>call:with: - make a unary RPC call
	c.AddMethod2(v.Selectors, "call:with:", func(vmInst *vm.VM, recv vm.Value, methodName vm.Value, requestDict vm.Value) vm.Value {
		client := vmGetGrpcClient(vmInst, recv)
		if client == nil {
			return grpcFailure(vmInst, "invalid client")
		}
		if !vm.IsStringValue(methodName) {
			return grpcFailure(vmInst, "method name must be a string")
		}
		if !vm.IsDictionaryValue(requestDict) {
			return grpcFailure(vmInst, "request must be a dictionary")
		}

		method := vmInst.Registry().GetStringContent(methodName)
		methodDesc, err := resolveMethod(client, method)
		if err != nil {
			return grpcFailure(vmInst, err.Error())
		}

		reqMsg, err := dictionaryToProto(vmInst, requestDict, methodDesc.GetInputType())
		if err != nil {
			return grpcFailure(vmInst, fmt.Sprintf("request conversion: %v", err))
		}

		respMsg := dynamic.NewMessage(methodDesc.GetOutputType())

		err = client.conn.Invoke(context.Background(), "/"+method, reqMsg, respMsg)
		if err != nil {
			return grpcFailure(vmInst, fmt.Sprintf("call failed: %v", err))
		}

		respDict, err := protoToDictionary(vmInst, respMsg)
		if err != nil {
			return grpcFailure(vmInst, fmt.Sprintf("response conversion: %v", err))
		}

		return grpcSuccess(vmInst, respDict)
	})

	// GrpcClient>>serverStream:with: - create a server streaming call
	c.AddMethod2(v.Selectors, "serverStream:with:", func(vmInst *vm.VM, recv vm.Value, methodName vm.Value, requestDict vm.Value) vm.Value {
		client := vmGetGrpcClient(vmInst, recv)
		if client == nil {
			return grpcFailure(vmInst, "invalid client")
		}
		if !vm.IsStringValue(methodName) {
			return grpcFailure(vmInst, "method name must be a string")
		}
		if !vm.IsDictionaryValue(requestDict) {
			return grpcFailure(vmInst, "request must be a dictionary")
		}

		method := vmInst.Registry().GetStringContent(methodName)
		methodDesc, err := resolveMethod(client, method)
		if err != nil {
			return grpcFailure(vmInst, err.Error())
		}

		reqMsg, err := dictionaryToProto(vmInst, requestDict, methodDesc.GetInputType())
		if err != nil {
			return grpcFailure(vmInst, fmt.Sprintf("request conversion: %v", err))
		}

		streamDesc := &grpclib.StreamDesc{
			StreamName:    methodDesc.GetName(),
			ServerStreams: true,
		}
		stream, err := client.conn.NewStream(context.Background(), streamDesc, "/"+method)
		if err != nil {
			return grpcFailure(vmInst, fmt.Sprintf("stream creation: %v", err))
		}

		if err := stream.SendMsg(reqMsg); err != nil {
			return grpcFailure(vmInst, fmt.Sprintf("send request: %v", err))
		}
		if err := stream.CloseSend(); err != nil {
			return grpcFailure(vmInst, fmt.Sprintf("close send: %v", err))
		}

		streamObj := &GrpcStreamObject{
			stream:     stream,
			clientVal:  recv,
			methodDesc: methodDesc,
			streamType: GrpcStreamServerStreaming,
		}
		streamObj.sendClosed.Store(true)

		return grpcSuccess(vmInst, vmRegisterGrpcStream(vmInst, streamObj))
	})

	// GrpcClient>>clientStream: - create a client streaming call
	c.AddMethod1(v.Selectors, "clientStream:", func(vmInst *vm.VM, recv vm.Value, methodName vm.Value) vm.Value {
		client := vmGetGrpcClient(vmInst, recv)
		if client == nil {
			return grpcFailure(vmInst, "invalid client")
		}
		if !vm.IsStringValue(methodName) {
			return grpcFailure(vmInst, "method name must be a string")
		}

		method := vmInst.Registry().GetStringContent(methodName)
		methodDesc, err := resolveMethod(client, method)
		if err != nil {
			return grpcFailure(vmInst, err.Error())
		}

		streamDesc := &grpclib.StreamDesc{
			StreamName:    methodDesc.GetName(),
			ClientStreams: true,
		}
		stream, err := client.conn.NewStream(context.Background(), streamDesc, "/"+method)
		if err != nil {
			return grpcFailure(vmInst, fmt.Sprintf("stream creation: %v", err))
		}

		streamObj := &GrpcStreamObject{
			stream:     stream,
			clientVal:  recv,
			methodDesc: methodDesc,
			streamType: GrpcStreamClientStreaming,
		}

		return grpcSuccess(vmInst, vmRegisterGrpcStream(vmInst, streamObj))
	})

	// GrpcClient>>bidiStream: - create a bidirectional streaming call
	c.AddMethod1(v.Selectors, "bidiStream:", func(vmInst *vm.VM, recv vm.Value, methodName vm.Value) vm.Value {
		client := vmGetGrpcClient(vmInst, recv)
		if client == nil {
			return grpcFailure(vmInst, "invalid client")
		}
		if !vm.IsStringValue(methodName) {
			return grpcFailure(vmInst, "method name must be a string")
		}

		method := vmInst.Registry().GetStringContent(methodName)
		methodDesc, err := resolveMethod(client, method)
		if err != nil {
			return grpcFailure(vmInst, err.Error())
		}

		streamDesc := &grpclib.StreamDesc{
			StreamName:    methodDesc.GetName(),
			ServerStreams: true,
			ClientStreams: true,
		}
		stream, err := client.conn.NewStream(context.Background(), streamDesc, "/"+method)
		if err != nil {
			return grpcFailure(vmInst, fmt.Sprintf("stream creation: %v", err))
		}

		streamObj := &GrpcStreamObject{
			stream:     stream,
			clientVal:  recv,
			methodDesc: methodDesc,
			streamType: GrpcStreamBidirectional,
		}

		return grpcSuccess(vmInst, vmRegisterGrpcStream(vmInst, streamObj))
	})

	// ---------------------------------------------------------------------------
	// GrpcStream Primitives
	// ---------------------------------------------------------------------------

	s := grpcStreamClass

	// GrpcStream>>send: messageDict - send a message on the stream
	s.AddMethod1(v.Selectors, "send:", func(vmInst *vm.VM, recv vm.Value, messageDict vm.Value) vm.Value {
		stream := vmGetGrpcStream(vmInst, recv)
		if stream == nil {
			return grpcFailure(vmInst, "invalid stream")
		}
		if !vm.IsDictionaryValue(messageDict) {
			return grpcFailure(vmInst, "message must be a dictionary")
		}
		if stream.sendClosed.Load() {
			return grpcFailure(vmInst, "stream send closed")
		}

		msg, err := dictionaryToProto(vmInst, messageDict, stream.methodDesc.GetInputType())
		if err != nil {
			return grpcFailure(vmInst, fmt.Sprintf("message conversion: %v", err))
		}

		if err := stream.stream.SendMsg(msg); err != nil {
			return grpcFailure(vmInst, fmt.Sprintf("send failed: %v", err))
		}

		return grpcSuccess(vmInst, recv)
	})

	// GrpcStream>>receive - receive a message from the stream
	s.AddMethod0(v.Selectors, "receive", func(vmInst *vm.VM, recv vm.Value) vm.Value {
		stream := vmGetGrpcStream(vmInst, recv)
		if stream == nil {
			return grpcFailure(vmInst, "invalid stream")
		}
		if stream.recvClosed.Load() {
			return grpcFailure(vmInst, "end of stream")
		}

		msg := dynamic.NewMessage(stream.methodDesc.GetOutputType())
		if err := stream.stream.RecvMsg(msg); err != nil {
			if err == io.EOF {
				stream.recvClosed.Store(true)
				return grpcFailure(vmInst, "end of stream")
			}
			return grpcFailure(vmInst, fmt.Sprintf("receive failed: %v", err))
		}

		var respDict vm.Value
		var convErr error
		func() {
			defer func() {
				if r := recover(); r != nil {
					convErr = fmt.Errorf("panic: %v", r)
				}
			}()
			respDict, convErr = protoToDictionary(vmInst, msg)
		}()

		if convErr != nil {
			return grpcFailure(vmInst, fmt.Sprintf("response conversion: %v", convErr))
		}

		return grpcSuccess(vmInst, respDict)
	})

	// GrpcStream>>hasNext - check if more messages are available
	s.AddMethod0(v.Selectors, "hasNext", func(vmInst *vm.VM, recv vm.Value) vm.Value {
		stream := vmGetGrpcStream(vmInst, recv)
		if stream == nil || stream.recvClosed.Load() {
			return vm.False
		}
		return vm.True
	})

	// GrpcStream>>close - close the send side of the stream
	s.AddMethod0(v.Selectors, "close", func(vmInst *vm.VM, recv vm.Value) vm.Value {
		stream := vmGetGrpcStream(vmInst, recv)
		if stream == nil {
			return recv
		}
		stream.mu.Lock()
		defer stream.mu.Unlock()
		if !stream.sendClosed.Load() {
			stream.stream.CloseSend()
			stream.sendClosed.Store(true)
		}
		return recv
	})

	// GrpcStream>>closeAndReceive - close send and receive final response (for client streaming)
	s.AddMethod0(v.Selectors, "closeAndReceive", func(vmInst *vm.VM, recv vm.Value) vm.Value {
		stream := vmGetGrpcStream(vmInst, recv)
		if stream == nil {
			return grpcFailure(vmInst, "invalid stream")
		}

		stream.mu.Lock()
		if !stream.sendClosed.Load() {
			stream.stream.CloseSend()
			stream.sendClosed.Store(true)
		}
		stream.mu.Unlock()

		msg := dynamic.NewMessage(stream.methodDesc.GetOutputType())
		if err := stream.stream.RecvMsg(msg); err != nil {
			return grpcFailure(vmInst, fmt.Sprintf("receive failed: %v", err))
		}

		respDict, err := protoToDictionary(vmInst, msg)
		if err != nil {
			return grpcFailure(vmInst, fmt.Sprintf("response conversion: %v", err))
		}

		stream.recvClosed.Store(true)
		vmUnregisterGrpcStream(vmInst, recv)

		return grpcSuccess(vmInst, respDict)
	})

	// GrpcStream>>streamType - return the stream type as a symbol
	s.AddMethod0(v.Selectors, "streamType", func(vmInst *vm.VM, recv vm.Value) vm.Value {
		stream := vmGetGrpcStream(vmInst, recv)
		if stream == nil {
			return vm.Nil
		}
		switch stream.streamType {
		case GrpcStreamServerStreaming:
			return vmInst.Symbols.SymbolValue("serverStreaming")
		case GrpcStreamClientStreaming:
			return vmInst.Symbols.SymbolValue("clientStreaming")
		case GrpcStreamBidirectional:
			return vmInst.Symbols.SymbolValue("bidirectional")
		}
		return vm.Nil
	})
}
