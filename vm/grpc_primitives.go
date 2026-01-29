package vm

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
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
	rpb "google.golang.org/grpc/reflection/grpc_reflection_v1alpha"
	"google.golang.org/protobuf/types/descriptorpb"
)


// ---------------------------------------------------------------------------
// GrpcClient Registry
// ---------------------------------------------------------------------------

// GrpcClientObject wraps a gRPC connection with reflection support.
type GrpcClientObject struct {
	conn      *grpc.ClientConn
	refClient *grpcreflect.Client
	target    string
	closed    atomic.Bool
	mu        sync.Mutex
}

// grpcClientRegistry stores active gRPC clients
var grpcClientRegistry = struct {
	sync.RWMutex
	clients map[int]*GrpcClientObject
	nextID  int
}{
	clients: make(map[int]*GrpcClientObject),
	nextID:  1,
}

const grpcClientMarker = 7 << 24 // 0x07000000

func grpcClientToValue(id int) Value {
	return FromSymbolID(uint32(id) | grpcClientMarker)
}

func isGrpcClientValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return (id & (0xFF << 24)) == grpcClientMarker
}

func getGrpcClient(v Value) *GrpcClientObject {
	if !isGrpcClientValue(v) {
		return nil
	}
	id := int(v.SymbolID() & ^uint32(0xFF<<24))

	grpcClientRegistry.RLock()
	defer grpcClientRegistry.RUnlock()
	return grpcClientRegistry.clients[id]
}

func registerGrpcClient(c *GrpcClientObject) Value {
	grpcClientRegistry.Lock()
	defer grpcClientRegistry.Unlock()

	id := grpcClientRegistry.nextID
	grpcClientRegistry.nextID++
	grpcClientRegistry.clients[id] = c

	return grpcClientToValue(id)
}

func unregisterGrpcClient(v Value) {
	if !isGrpcClientValue(v) {
		return
	}
	id := int(v.SymbolID() & ^uint32(0xFF<<24))

	grpcClientRegistry.Lock()
	defer grpcClientRegistry.Unlock()
	delete(grpcClientRegistry.clients, id)
}

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
	stream     grpc.ClientStream
	clientVal  Value // Reference to owning GrpcClientObject
	methodDesc *desc.MethodDescriptor
	streamType GrpcStreamType
	sendClosed atomic.Bool
	recvClosed atomic.Bool
	mu         sync.Mutex
}

// grpcStreamRegistry stores active gRPC streams
var grpcStreamRegistry = struct {
	sync.RWMutex
	streams map[int]*GrpcStreamObject
	nextID  int
}{
	streams: make(map[int]*GrpcStreamObject),
	nextID:  1,
}

const grpcStreamMarker = 9 << 24 // 0x09000000 (8 << 24 is used by exceptions)

func grpcStreamToValue(id int) Value {
	return FromSymbolID(uint32(id) | grpcStreamMarker)
}

func isGrpcStreamValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return (id & (0xFF << 24)) == grpcStreamMarker
}

func getGrpcStream(v Value) *GrpcStreamObject {
	if !isGrpcStreamValue(v) {
		return nil
	}
	id := int(v.SymbolID() & ^uint32(0xFF<<24))

	grpcStreamRegistry.RLock()
	defer grpcStreamRegistry.RUnlock()
	return grpcStreamRegistry.streams[id]
}

func registerGrpcStream(s *GrpcStreamObject) Value {
	grpcStreamRegistry.Lock()
	defer grpcStreamRegistry.Unlock()

	id := grpcStreamRegistry.nextID
	grpcStreamRegistry.nextID++
	grpcStreamRegistry.streams[id] = s

	return grpcStreamToValue(id)
}

func unregisterGrpcStream(v Value) {
	if !isGrpcStreamValue(v) {
		return
	}
	id := int(v.SymbolID() & ^uint32(0xFF<<24))

	grpcStreamRegistry.Lock()
	defer grpcStreamRegistry.Unlock()
	delete(grpcStreamRegistry.streams, id)
}

// ---------------------------------------------------------------------------
// Helper functions for Result creation
// ---------------------------------------------------------------------------

func grpcSuccess(val Value) Value {
	r := createResult(ResultSuccess, val)
	return registerResult(r)
}

func grpcFailure(reason string) Value {
	r := createResult(ResultFailure, NewStringValue(reason))
	return registerResult(r)
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
func dictionaryToProto(vm *VM, dictVal Value, msgDesc *desc.MessageDescriptor) (*dynamic.Message, error) {
	dict := GetDictionaryObject(dictVal)
	if dict == nil {
		return nil, fmt.Errorf("not a dictionary")
	}

	msg := dynamic.NewMessage(msgDesc)

	for h, val := range dict.Data {
		key := dict.Keys[h]

		// Get field name from key (symbol or string)
		var fieldName string
		if key.IsSymbol() {
			fieldName = vm.Symbols.Name(key.SymbolID())
		} else if IsStringValue(key) {
			fieldName = GetStringContent(key)
		} else {
			continue // Skip non-string/symbol keys
		}

		field := msgDesc.FindFieldByName(fieldName)
		if field == nil {
			continue // Skip unknown fields
		}

		protoVal, err := valueToProtoField(vm, val, field)
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
func valueToProtoField(vm *VM, val Value, field *desc.FieldDescriptor) (interface{}, error) {
	if field.IsRepeated() && !field.IsMap() {
		return valueToRepeatedField(vm, val, field)
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
		return val == True, nil
	case descriptorpb.FieldDescriptorProto_TYPE_STRING:
		if IsStringValue(val) {
			return GetStringContent(val), nil
		}
		if val.IsSymbol() {
			return vm.Symbols.Name(val.SymbolID()), nil
		}
	case descriptorpb.FieldDescriptorProto_TYPE_BYTES:
		if IsStringValue(val) {
			return []byte(GetStringContent(val)), nil
		}
	case descriptorpb.FieldDescriptorProto_TYPE_MESSAGE:
		if IsDictionaryValue(val) {
			return dictionaryToProto(vm, val, field.GetMessageType())
		}
	case descriptorpb.FieldDescriptorProto_TYPE_ENUM:
		if val.IsSmallInt() {
			return int32(val.SmallInt()), nil
		}
		if IsStringValue(val) || val.IsSymbol() {
			name := ""
			if IsStringValue(val) {
				name = GetStringContent(val)
			} else {
				name = vm.Symbols.Name(val.SymbolID())
			}
			enumVal := field.GetEnumType().FindValueByName(name)
			if enumVal != nil {
				return enumVal.GetNumber(), nil
			}
		}
	}

	return nil, fmt.Errorf("cannot convert value to proto type %v", field.GetType())
}

// valueToRepeatedField converts a Maggie Array to a repeated protobuf field
func valueToRepeatedField(vm *VM, val Value, field *desc.FieldDescriptor) (interface{}, error) {
	if !val.IsObject() {
		return nil, fmt.Errorf("expected array for repeated field")
	}

	obj := ObjectFromValue(val)
	size := obj.NumSlots()
	result := make([]interface{}, size)

	for i := 0; i < size; i++ {
		elem := obj.GetSlot(i)
		protoVal, err := valueToProtoField(vm, elem, field)
		if err != nil {
			return nil, fmt.Errorf("element %d: %w", i, err)
		}
		result[i] = protoVal
	}

	return result, nil
}

// protoToDictionary converts a protobuf dynamic message to a Maggie Dictionary
func protoToDictionary(vm *VM, msg *dynamic.Message) (Value, error) {
	dict := NewDictionaryValue()

	for _, field := range msg.GetKnownFields() {
		hasField := msg.HasField(field)
		if !hasField {
			continue
		}

		val := msg.GetField(field)
		maggieVal, err := protoFieldToValue(vm, val, field)
		if err != nil {
			return Nil, fmt.Errorf("field %s: %w", field.GetName(), err)
		}

		key := vm.Symbols.SymbolValue(field.GetName())
		vm.DictionaryAtPut(dict, key, maggieVal)
	}

	return dict, nil
}

// protoFieldToValue converts a protobuf field value to a Maggie Value
func protoFieldToValue(vm *VM, val interface{}, field *desc.FieldDescriptor) (Value, error) {
	if field.IsRepeated() && !field.IsMap() {
		return repeatedFieldToValue(vm, val, field)
	}

	// Handle map fields
	if field.IsMap() {
		return mapFieldToValue(vm, val, field)
	}

	switch field.GetType() {
	case descriptorpb.FieldDescriptorProto_TYPE_INT32,
		descriptorpb.FieldDescriptorProto_TYPE_SINT32,
		descriptorpb.FieldDescriptorProto_TYPE_SFIXED32:
		return FromSmallInt(int64(val.(int32))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_INT64,
		descriptorpb.FieldDescriptorProto_TYPE_SINT64,
		descriptorpb.FieldDescriptorProto_TYPE_SFIXED64:
		// Use TryFromSmallInt to handle large values (like nanosecond timestamps)
		if v, ok := TryFromSmallInt(val.(int64)); ok {
			return v, nil
		}
		// Fall back to float64 for values too large for SmallInt
		return FromFloat64(float64(val.(int64))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_UINT32,
		descriptorpb.FieldDescriptorProto_TYPE_FIXED32:
		return FromSmallInt(int64(val.(uint32))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_UINT64,
		descriptorpb.FieldDescriptorProto_TYPE_FIXED64:
		// Use TryFromSmallInt to handle large values
		if v, ok := TryFromSmallInt(int64(val.(uint64))); ok {
			return v, nil
		}
		// Fall back to float64 for values too large for SmallInt
		return FromFloat64(float64(val.(uint64))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_FLOAT:
		return FromFloat64(float64(val.(float32))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_DOUBLE:
		return FromFloat64(val.(float64)), nil
	case descriptorpb.FieldDescriptorProto_TYPE_BOOL:
		if val.(bool) {
			return True, nil
		}
		return False, nil
	case descriptorpb.FieldDescriptorProto_TYPE_STRING:
		return NewStringValue(val.(string)), nil
	case descriptorpb.FieldDescriptorProto_TYPE_BYTES:
		return NewStringValue(string(val.([]byte))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_MESSAGE:
		return protoToDictionary(vm, val.(*dynamic.Message))
	case descriptorpb.FieldDescriptorProto_TYPE_ENUM:
		enumNum := val.(int32)
		enumVal := field.GetEnumType().FindValueByNumber(enumNum)
		if enumVal != nil {
			return vm.Symbols.SymbolValue(enumVal.GetName()), nil
		}
		return FromSmallInt(int64(enumNum)), nil
	}

	return Nil, fmt.Errorf("unsupported proto type: %v", field.GetType())
}

// repeatedFieldToValue converts a repeated protobuf field to a Maggie Array
func repeatedFieldToValue(vm *VM, val interface{}, field *desc.FieldDescriptor) (Value, error) {
	slice := reflect.ValueOf(val)
	elements := make([]Value, slice.Len())

	for i := 0; i < slice.Len(); i++ {
		elem := slice.Index(i).Interface()
		// Convert individual elements using protoElementToValue (not protoFieldToValue)
		// to avoid re-checking IsRepeated() which would cause infinite recursion
		maggieVal, err := protoElementToValue(vm, elem, field)
		if err != nil {
			return Nil, err
		}
		elements[i] = maggieVal
	}

	return vm.NewArrayWithElements(elements), nil
}

// protoElementToValue converts a single element from a repeated field to a Maggie Value
// This is similar to protoFieldToValue but skips the IsRepeated check
func protoElementToValue(vm *VM, val interface{}, field *desc.FieldDescriptor) (Value, error) {
	switch field.GetType() {
	case descriptorpb.FieldDescriptorProto_TYPE_INT32,
		descriptorpb.FieldDescriptorProto_TYPE_SINT32,
		descriptorpb.FieldDescriptorProto_TYPE_SFIXED32:
		return FromSmallInt(int64(val.(int32))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_INT64,
		descriptorpb.FieldDescriptorProto_TYPE_SINT64,
		descriptorpb.FieldDescriptorProto_TYPE_SFIXED64:
		if v, ok := TryFromSmallInt(val.(int64)); ok {
			return v, nil
		}
		return FromFloat64(float64(val.(int64))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_UINT32,
		descriptorpb.FieldDescriptorProto_TYPE_FIXED32:
		return FromSmallInt(int64(val.(uint32))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_UINT64,
		descriptorpb.FieldDescriptorProto_TYPE_FIXED64:
		if v, ok := TryFromSmallInt(int64(val.(uint64))); ok {
			return v, nil
		}
		return FromFloat64(float64(val.(uint64))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_FLOAT:
		return FromFloat64(float64(val.(float32))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_DOUBLE:
		return FromFloat64(val.(float64)), nil
	case descriptorpb.FieldDescriptorProto_TYPE_BOOL:
		if val.(bool) {
			return True, nil
		}
		return False, nil
	case descriptorpb.FieldDescriptorProto_TYPE_STRING:
		return NewStringValue(val.(string)), nil
	case descriptorpb.FieldDescriptorProto_TYPE_BYTES:
		return NewStringValue(string(val.([]byte))), nil
	case descriptorpb.FieldDescriptorProto_TYPE_MESSAGE:
		return protoToDictionary(vm, val.(*dynamic.Message))
	case descriptorpb.FieldDescriptorProto_TYPE_ENUM:
		enumNum := val.(int32)
		enumVal := field.GetEnumType().FindValueByNumber(enumNum)
		if enumVal != nil {
			return vm.Symbols.SymbolValue(enumVal.GetName()), nil
		}
		return FromSmallInt(int64(enumNum)), nil
	}

	return Nil, fmt.Errorf("unsupported proto type: %v", field.GetType())
}

// mapFieldToValue converts a protobuf map field to a Maggie Dictionary
func mapFieldToValue(vm *VM, val interface{}, field *desc.FieldDescriptor) (Value, error) {
	// Maps come as map[interface{}]interface{} from dynamic.Message
	mapVal, ok := val.(map[interface{}]interface{})
	if !ok {
		return Nil, fmt.Errorf("expected map, got %T", val)
	}

	dict := NewDictionaryValue()
	mapEntry := field.GetMapKeyType()
	valueField := field.GetMapValueType()

	for k, v := range mapVal {
		// Convert key - for string keys, use as symbol
		var keyVal Value
		switch kTyped := k.(type) {
		case string:
			keyVal = vm.Symbols.SymbolValue(kTyped)
		case int32:
			keyVal = FromSmallInt(int64(kTyped))
		case int64:
			if sv, ok := TryFromSmallInt(kTyped); ok {
				keyVal = sv
			} else {
				keyVal = FromFloat64(float64(kTyped))
			}
		default:
			// Use map key field type for proper conversion
			if mapEntry != nil {
				var err error
				keyVal, err = protoElementToValue(vm, k, mapEntry)
				if err != nil {
					return Nil, fmt.Errorf("map key conversion: %w", err)
				}
			} else {
				keyVal = NewStringValue(fmt.Sprintf("%v", k))
			}
		}

		// Convert value
		var valueVal Value
		if valueField != nil {
			var err error
			valueVal, err = protoElementToValue(vm, v, valueField)
			if err != nil {
				return Nil, fmt.Errorf("map value conversion: %w", err)
			}
		} else {
			// Fallback for unknown value types
			switch vTyped := v.(type) {
			case string:
				valueVal = NewStringValue(vTyped)
			case int32:
				valueVal = FromSmallInt(int64(vTyped))
			case int64:
				if sv, ok := TryFromSmallInt(vTyped); ok {
					valueVal = sv
				} else {
					valueVal = FromFloat64(float64(vTyped))
				}
			case bool:
				if vTyped {
					valueVal = True
				} else {
					valueVal = False
				}
			default:
				valueVal = NewStringValue(fmt.Sprintf("%v", v))
			}
		}

		vm.DictionaryAtPut(dict, keyVal, valueVal)
	}

	return dict, nil
}

// ---------------------------------------------------------------------------
// GrpcClient Primitives Registration
// ---------------------------------------------------------------------------

func (vm *VM) registerGrpcPrimitives() {
	c := vm.GrpcClientClass

	// GrpcClient class>>connectTo: target - connect to gRPC server
	c.AddClassMethod1(vm.Selectors, "connectTo:", func(_ interface{}, recv Value, target Value) Value {
		if !IsStringValue(target) {
			return grpcFailure("target must be a string")
		}
		targetStr := GetStringContent(target)

		conn, err := grpc.Dial(targetStr, grpc.WithTransportCredentials(insecure.NewCredentials()))
		if err != nil {
			return grpcFailure(fmt.Sprintf("connection failed: %v", err))
		}

		// Create reflection client
		refClient := grpcreflect.NewClientV1Alpha(context.Background(), rpb.NewServerReflectionClient(conn))

		clientObj := &GrpcClientObject{
			conn:      conn,
			refClient: refClient,
			target:    targetStr,
		}

		return grpcSuccess(registerGrpcClient(clientObj))
	})

	// GrpcClient>>close - close connection
	c.AddMethod0(vm.Selectors, "close", func(_ interface{}, recv Value) Value {
		client := getGrpcClient(recv)
		if client == nil {
			return recv
		}
		client.mu.Lock()
		defer client.mu.Unlock()
		if !client.closed.Load() {
			client.closed.Store(true)
			client.refClient.Reset()
			client.conn.Close()
			unregisterGrpcClient(recv)
		}
		return recv
	})

	// GrpcClient>>isConnected - check if connected
	c.AddMethod0(vm.Selectors, "isConnected", func(_ interface{}, recv Value) Value {
		client := getGrpcClient(recv)
		if client == nil || client.closed.Load() {
			return False
		}
		return True
	})

	// GrpcClient>>listServices - list available services via reflection
	c.AddMethod0(vm.Selectors, "listServices", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		client := getGrpcClient(recv)
		if client == nil || client.closed.Load() {
			return v.NewArray(0)
		}

		services, err := client.refClient.ListServices()
		if err != nil {
			return v.NewArray(0)
		}

		elements := make([]Value, 0, len(services))
		for _, svc := range services {
			// Skip reflection service itself
			if strings.HasPrefix(svc, "grpc.reflection") {
				continue
			}
			elements = append(elements, NewStringValue(svc))
		}
		return v.NewArrayWithElements(elements)
	})

	// GrpcClient>>methodsForService: serviceName - list methods for a service
	c.AddMethod1(vm.Selectors, "methodsForService:", func(vmPtr interface{}, recv Value, serviceName Value) Value {
		v := vmPtr.(*VM)
		client := getGrpcClient(recv)
		if client == nil || !IsStringValue(serviceName) {
			return v.NewArray(0)
		}

		svcName := GetStringContent(serviceName)
		svcDesc, err := client.refClient.ResolveService(svcName)
		if err != nil {
			return v.NewArray(0)
		}

		methods := svcDesc.GetMethods()
		elements := make([]Value, len(methods))
		for i, m := range methods {
			elements[i] = NewStringValue(m.GetName())
		}
		return v.NewArrayWithElements(elements)
	})

	// GrpcClient>>methodDescriptor: methodName - get method metadata
	c.AddMethod1(vm.Selectors, "methodDescriptor:", func(vmPtr interface{}, recv Value, methodName Value) Value {
		v := vmPtr.(*VM)
		client := getGrpcClient(recv)
		if client == nil || !IsStringValue(methodName) {
			return Nil
		}

		methodDesc, err := resolveMethod(client, GetStringContent(methodName))
		if err != nil {
			return Nil
		}

		dict := NewDictionaryValue()
		v.DictionaryAtPut(dict, v.Symbols.SymbolValue("name"), NewStringValue(methodDesc.GetName()))
		v.DictionaryAtPut(dict, v.Symbols.SymbolValue("fullName"), NewStringValue(methodDesc.GetFullyQualifiedName()))
		v.DictionaryAtPut(dict, v.Symbols.SymbolValue("inputType"), NewStringValue(methodDesc.GetInputType().GetFullyQualifiedName()))
		v.DictionaryAtPut(dict, v.Symbols.SymbolValue("outputType"), NewStringValue(methodDesc.GetOutputType().GetFullyQualifiedName()))
		if methodDesc.IsClientStreaming() {
			v.DictionaryAtPut(dict, v.Symbols.SymbolValue("isClientStreaming"), True)
		} else {
			v.DictionaryAtPut(dict, v.Symbols.SymbolValue("isClientStreaming"), False)
		}
		if methodDesc.IsServerStreaming() {
			v.DictionaryAtPut(dict, v.Symbols.SymbolValue("isServerStreaming"), True)
		} else {
			v.DictionaryAtPut(dict, v.Symbols.SymbolValue("isServerStreaming"), False)
		}

		return dict
	})

	// GrpcClient>>call:with: - make a unary RPC call
	c.AddMethod2(vm.Selectors, "call:with:", func(vmPtr interface{}, recv Value, methodName Value, requestDict Value) Value {
		v := vmPtr.(*VM)
		client := getGrpcClient(recv)
		if client == nil {
			return grpcFailure("invalid client")
		}
		if !IsStringValue(methodName) {
			return grpcFailure("method name must be a string")
		}
		if !IsDictionaryValue(requestDict) {
			return grpcFailure("request must be a dictionary")
		}

		method := GetStringContent(methodName)
		methodDesc, err := resolveMethod(client, method)
		if err != nil {
			return grpcFailure(err.Error())
		}

		// Convert Maggie Dictionary to protobuf
		reqMsg, err := dictionaryToProto(v, requestDict, methodDesc.GetInputType())
		if err != nil {
			return grpcFailure(fmt.Sprintf("request conversion: %v", err))
		}

		// Create response message
		respMsg := dynamic.NewMessage(methodDesc.GetOutputType())

		// Invoke the method
		err = client.conn.Invoke(context.Background(), "/"+method, reqMsg, respMsg)
		if err != nil {
			return grpcFailure(fmt.Sprintf("call failed: %v", err))
		}

		// Convert response back to Maggie Dictionary
		respDict, err := protoToDictionary(v, respMsg)
		if err != nil {
			return grpcFailure(fmt.Sprintf("response conversion: %v", err))
		}

		return grpcSuccess(respDict)
	})

	// GrpcClient>>serverStream:with: - create a server streaming call
	c.AddMethod2(vm.Selectors, "serverStream:with:", func(vmPtr interface{}, recv Value, methodName Value, requestDict Value) Value {
		v := vmPtr.(*VM)
		client := getGrpcClient(recv)
		if client == nil {
			return grpcFailure("invalid client")
		}
		if !IsStringValue(methodName) {
			return grpcFailure("method name must be a string")
		}
		if !IsDictionaryValue(requestDict) {
			return grpcFailure("request must be a dictionary")
		}

		method := GetStringContent(methodName)
		methodDesc, err := resolveMethod(client, method)
		if err != nil {
			return grpcFailure(err.Error())
		}

		// Convert request
		reqMsg, err := dictionaryToProto(v, requestDict, methodDesc.GetInputType())
		if err != nil {
			return grpcFailure(fmt.Sprintf("request conversion: %v", err))
		}

		// Create stream
		streamDesc := &grpc.StreamDesc{
			StreamName:    methodDesc.GetName(),
			ServerStreams: true,
		}
		stream, err := client.conn.NewStream(context.Background(), streamDesc, "/"+method)
		if err != nil {
			return grpcFailure(fmt.Sprintf("stream creation: %v", err))
		}

		// Send the single request
		if err := stream.SendMsg(reqMsg); err != nil {
			return grpcFailure(fmt.Sprintf("send request: %v", err))
		}
		if err := stream.CloseSend(); err != nil {
			return grpcFailure(fmt.Sprintf("close send: %v", err))
		}

		streamObj := &GrpcStreamObject{
			stream:     stream,
			clientVal:  recv,
			methodDesc: methodDesc,
			streamType: GrpcStreamServerStreaming,
		}
		streamObj.sendClosed.Store(true)

		return grpcSuccess(registerGrpcStream(streamObj))
	})

	// GrpcClient>>clientStream: - create a client streaming call
	c.AddMethod1(vm.Selectors, "clientStream:", func(_ interface{}, recv Value, methodName Value) Value {
		client := getGrpcClient(recv)
		if client == nil {
			return grpcFailure("invalid client")
		}
		if !IsStringValue(methodName) {
			return grpcFailure("method name must be a string")
		}

		method := GetStringContent(methodName)
		methodDesc, err := resolveMethod(client, method)
		if err != nil {
			return grpcFailure(err.Error())
		}

		// Create stream
		streamDesc := &grpc.StreamDesc{
			StreamName:    methodDesc.GetName(),
			ClientStreams: true,
		}
		stream, err := client.conn.NewStream(context.Background(), streamDesc, "/"+method)
		if err != nil {
			return grpcFailure(fmt.Sprintf("stream creation: %v", err))
		}

		streamObj := &GrpcStreamObject{
			stream:     stream,
			clientVal:  recv,
			methodDesc: methodDesc,
			streamType: GrpcStreamClientStreaming,
		}

		return grpcSuccess(registerGrpcStream(streamObj))
	})

	// GrpcClient>>bidiStream: - create a bidirectional streaming call
	c.AddMethod1(vm.Selectors, "bidiStream:", func(_ interface{}, recv Value, methodName Value) Value {
		client := getGrpcClient(recv)
		if client == nil {
			return grpcFailure("invalid client")
		}
		if !IsStringValue(methodName) {
			return grpcFailure("method name must be a string")
		}

		method := GetStringContent(methodName)
		methodDesc, err := resolveMethod(client, method)
		if err != nil {
			return grpcFailure(err.Error())
		}

		// Create stream
		streamDesc := &grpc.StreamDesc{
			StreamName:    methodDesc.GetName(),
			ServerStreams: true,
			ClientStreams: true,
		}
		stream, err := client.conn.NewStream(context.Background(), streamDesc, "/"+method)
		if err != nil {
			return grpcFailure(fmt.Sprintf("stream creation: %v", err))
		}

		streamObj := &GrpcStreamObject{
			stream:     stream,
			clientVal:  recv,
			methodDesc: methodDesc,
			streamType: GrpcStreamBidirectional,
		}

		return grpcSuccess(registerGrpcStream(streamObj))
	})

	// ---------------------------------------------------------------------------
	// GrpcStream Primitives
	// ---------------------------------------------------------------------------

	s := vm.GrpcStreamClass

	// GrpcStream>>send: messageDict - send a message on the stream
	s.AddMethod1(vm.Selectors, "send:", func(vmPtr interface{}, recv Value, messageDict Value) Value {
		v := vmPtr.(*VM)
		stream := getGrpcStream(recv)
		if stream == nil {
			return grpcFailure("invalid stream")
		}
		if !IsDictionaryValue(messageDict) {
			return grpcFailure("message must be a dictionary")
		}
		if stream.sendClosed.Load() {
			return grpcFailure("stream send closed")
		}

		msg, err := dictionaryToProto(v, messageDict, stream.methodDesc.GetInputType())
		if err != nil {
			return grpcFailure(fmt.Sprintf("message conversion: %v", err))
		}

		if err := stream.stream.SendMsg(msg); err != nil {
			return grpcFailure(fmt.Sprintf("send failed: %v", err))
		}

		return grpcSuccess(recv)
	})

	// GrpcStream>>receive - receive a message from the stream
	s.AddMethod0(vm.Selectors, "receive", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		stream := getGrpcStream(recv)
		if stream == nil {
			return grpcFailure("invalid stream")
		}
		if stream.recvClosed.Load() {
			return grpcFailure("end of stream")
		}

		msg := dynamic.NewMessage(stream.methodDesc.GetOutputType())
		if err := stream.stream.RecvMsg(msg); err != nil {
			if err == io.EOF {
				stream.recvClosed.Store(true)
				return grpcFailure("end of stream")
			}
			return grpcFailure(fmt.Sprintf("receive failed: %v", err))
		}

		// Catch any panics during conversion
		var respDict Value
		var convErr error
		func() {
			defer func() {
				if r := recover(); r != nil {
					convErr = fmt.Errorf("panic: %v", r)
				}
			}()
			respDict, convErr = protoToDictionary(v, msg)
		}()

		if convErr != nil {
			return grpcFailure(fmt.Sprintf("response conversion: %v", convErr))
		}

		return grpcSuccess(respDict)
	})

	// GrpcStream>>hasNext - check if more messages are available
	s.AddMethod0(vm.Selectors, "hasNext", func(_ interface{}, recv Value) Value {
		stream := getGrpcStream(recv)
		if stream == nil || stream.recvClosed.Load() {
			return False
		}
		return True
	})

	// GrpcStream>>close - close the send side of the stream
	s.AddMethod0(vm.Selectors, "close", func(_ interface{}, recv Value) Value {
		stream := getGrpcStream(recv)
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
	s.AddMethod0(vm.Selectors, "closeAndReceive", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		stream := getGrpcStream(recv)
		if stream == nil {
			return grpcFailure("invalid stream")
		}

		// Close send side
		stream.mu.Lock()
		if !stream.sendClosed.Load() {
			stream.stream.CloseSend()
			stream.sendClosed.Store(true)
		}
		stream.mu.Unlock()

		// Receive final response
		msg := dynamic.NewMessage(stream.methodDesc.GetOutputType())
		if err := stream.stream.RecvMsg(msg); err != nil {
			return grpcFailure(fmt.Sprintf("receive failed: %v", err))
		}

		respDict, err := protoToDictionary(v, msg)
		if err != nil {
			return grpcFailure(fmt.Sprintf("response conversion: %v", err))
		}

		stream.recvClosed.Store(true)
		unregisterGrpcStream(recv)

		return grpcSuccess(respDict)
	})

	// GrpcStream>>streamType - return the stream type as a symbol
	s.AddMethod0(vm.Selectors, "streamType", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		stream := getGrpcStream(recv)
		if stream == nil {
			return Nil
		}
		switch stream.streamType {
		case GrpcStreamServerStreaming:
			return v.Symbols.SymbolValue("serverStreaming")
		case GrpcStreamClientStreaming:
			return v.Symbols.SymbolValue("clientStreaming")
		case GrpcStreamBidirectional:
			return v.Symbols.SymbolValue("bidirectional")
		}
		return Nil
	})
}
