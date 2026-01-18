// Package main builds libtrashtalk.dylib - the shared runtime for Trashtalk.
// This is built with -buildmode=c-shared.
package main

/*
#include <stdlib.h>
#include <stdint.h>

// Forward declarations for types
typedef struct TTInstance TTInstance;
typedef struct TTBlock TTBlock;
typedef struct TTArray TTArray;

// TTValueType defines the type of a Trashtalk value
typedef enum {
    TT_TYPE_NIL = 0,
    TT_TYPE_INT = 1,
    TT_TYPE_FLOAT = 2,
    TT_TYPE_STRING = 3,
    TT_TYPE_BOOL = 4,
    TT_TYPE_INSTANCE = 5,
    TT_TYPE_BLOCK = 6,
    TT_TYPE_ARRAY = 7,
    TT_TYPE_ERROR = 8,
} TTValueType;

// TTValue is the universal value type
typedef struct {
    TTValueType type;
    union {
        int64_t intVal;
        double floatVal;
        const char* stringVal;
        TTInstance* instanceVal;
        TTBlock* blockVal;
        TTArray* arrayVal;
    };
} TTValue;

// Method function pointer type
typedef TTValue (*TTMethodFunc)(TTInstance* self, TTValue* args, int numArgs);

// Method flags
typedef enum {
    TT_METHOD_NATIVE = 1,
    TT_METHOD_BASH_FALLBACK = 2,
    TT_METHOD_CLASS_METHOD = 4,
} TTMethodFlags;

// Method entry
typedef struct {
    const char* selector;
    TTMethodFunc impl;
    int numArgs;
    uint32_t flags;
} TTMethodEntry;

// Method table
typedef struct {
    TTMethodEntry* instanceMethods;
    int numInstanceMethods;
    TTMethodEntry* classMethods;
    int numClassMethods;
} TTMethodTable;

// Helper to call a method function pointer (cgo can't call function pointers directly)
static TTValue call_method_func(TTMethodFunc fn, TTInstance* self, TTValue* args, int numArgs) {
    return fn(self, args, numArgs);
}

*/
import "C"
import (
	"unsafe"

	"github.com/chazu/procyon/pkg/bytecode"
	runtime "github.com/chazu/procyon/lib/runtime"
)

func main() {}

// ============================================================================
// Value conversion helpers
// ============================================================================

func valueToC(v runtime.Value) C.TTValue {
	var cv C.TTValue
	cv._type = C.TTValueType(v.Type)

	switch v.Type {
	case runtime.TypeInt:
		*(*C.int64_t)(unsafe.Pointer(&cv.anon0)) = C.int64_t(v.IntVal)
	case runtime.TypeFloat:
		*(*C.double)(unsafe.Pointer(&cv.anon0)) = C.double(v.FloatVal)
	case runtime.TypeString:
		cstr := C.CString(v.StringVal)
		*(**C.char)(unsafe.Pointer(&cv.anon0)) = cstr
	case runtime.TypeBool:
		*(*C.int64_t)(unsafe.Pointer(&cv.anon0)) = C.int64_t(v.IntVal)
	case runtime.TypeInstance:
		*(*unsafe.Pointer)(unsafe.Pointer(&cv.anon0)) = unsafe.Pointer(v.InstanceVal)
	case runtime.TypeBlock:
		*(*unsafe.Pointer)(unsafe.Pointer(&cv.anon0)) = unsafe.Pointer(v.BlockVal)
	case runtime.TypeArray:
		*(*unsafe.Pointer)(unsafe.Pointer(&cv.anon0)) = unsafe.Pointer(v.ArrayVal)
	}

	return cv
}

func valueFromC(cv C.TTValue) runtime.Value {
	var v runtime.Value
	v.Type = runtime.ValueType(cv._type)

	switch v.Type {
	case runtime.TypeInt:
		v.IntVal = int64(*(*C.int64_t)(unsafe.Pointer(&cv.anon0)))
	case runtime.TypeFloat:
		v.FloatVal = float64(*(*C.double)(unsafe.Pointer(&cv.anon0)))
	case runtime.TypeString:
		cstr := *(**C.char)(unsafe.Pointer(&cv.anon0))
		if cstr != nil {
			v.StringVal = C.GoString(cstr)
		}
	case runtime.TypeBool:
		v.IntVal = int64(*(*C.int64_t)(unsafe.Pointer(&cv.anon0)))
	case runtime.TypeInstance:
		v.InstanceVal = (*runtime.Instance)(*(*unsafe.Pointer)(unsafe.Pointer(&cv.anon0)))
	case runtime.TypeBlock:
		v.BlockVal = (*runtime.Block)(*(*unsafe.Pointer)(unsafe.Pointer(&cv.anon0)))
	case runtime.TypeArray:
		v.ArrayVal = (*runtime.Array)(*(*unsafe.Pointer)(unsafe.Pointer(&cv.anon0)))
	}

	return v
}

// ============================================================================
// Runtime Initialization
// ============================================================================

//export TT_Init
func TT_Init(dbPath, pluginDir *C.char) C.int {
	cfg := runtime.DefaultConfig()
	if dbPath != nil {
		cfg.DBPath = C.GoString(dbPath)
	}

	err := runtime.InitGlobal(cfg)
	if err != nil {
		return -1
	}
	return 0
}

//export TT_Close
func TT_Close() {
	runtime.CloseGlobal()
}

// ============================================================================
// Class Registration
// ============================================================================

//export TT_RegisterClass
func TT_RegisterClass(className, superclass *C.char, instanceVars **C.char, numVars C.int, methods *C.TTMethodTable) {
	r := runtime.GlobalRuntime()
	if r == nil {
		return
	}

	name := C.GoString(className)
	super := ""
	if superclass != nil {
		super = C.GoString(superclass)
	}

	// Convert instance vars
	vars := make([]string, int(numVars))
	if instanceVars != nil && numVars > 0 {
		varPtrs := unsafe.Slice(instanceVars, int(numVars))
		for i, ptr := range varPtrs {
			vars[i] = C.GoString(ptr)
		}
	}

	// Create method table
	mt := runtime.NewMethodTable()

	// Parse C method table if provided
	if methods != nil {
		// Parse instance methods
		if methods.instanceMethods != nil && methods.numInstanceMethods > 0 {
			entries := unsafe.Slice(methods.instanceMethods, int(methods.numInstanceMethods))
			for _, entry := range entries {
				selector := C.GoString(entry.selector)
				funcPtr := entry.impl
				numArgs := int(entry.numArgs)
				flags := runtime.MethodFlags(entry.flags)

				// Create a wrapper that calls the C function
				wrapper := createCMethodWrapper(funcPtr)
				mt.AddInstanceMethod(selector, wrapper, numArgs, flags)
			}
		}

		// Parse class methods
		if methods.classMethods != nil && methods.numClassMethods > 0 {
			entries := unsafe.Slice(methods.classMethods, int(methods.numClassMethods))
			for _, entry := range entries {
				selector := C.GoString(entry.selector)
				funcPtr := entry.impl
				numArgs := int(entry.numArgs)
				flags := runtime.MethodFlags(entry.flags)

				wrapper := createCMethodWrapper(funcPtr)
				mt.AddClassMethod(selector, wrapper, numArgs, flags)
			}
		}
	}

	r.RegisterClass(name, super, vars, mt)
}

// createCMethodWrapper creates a Go MethodFunc that calls a C method function pointer
func createCMethodWrapper(funcPtr C.TTMethodFunc) runtime.MethodFunc {
	// Capture the function pointer in the closure
	fn := funcPtr
	return func(self *runtime.Instance, args []runtime.Value) runtime.Value {
		// Convert self to C.TTInstance pointer
		var cSelf *C.TTInstance
		if self != nil {
			cSelf = (*C.TTInstance)(unsafe.Pointer(self))
		}

		// Convert args to C.TTValue array
		cArgs := make([]C.TTValue, len(args))
		for i, arg := range args {
			cArgs[i] = valueToC(arg)
		}

		var argsPtr *C.TTValue
		if len(cArgs) > 0 {
			argsPtr = &cArgs[0]
		}

		// Call the C function via helper
		result := C.call_method_func(fn, cSelf, argsPtr, C.int(len(args)))

		// Convert result back to Go
		return valueFromC(result)
	}
}

// ============================================================================
// Instance Creation and Lookup
// ============================================================================

//export TT_New
func TT_New(className *C.char) *C.char {
	r := runtime.GlobalRuntime()
	if r == nil {
		return nil
	}

	inst, err := r.NewInstance(C.GoString(className))
	if err != nil {
		return nil
	}

	return C.CString(inst.ID)
}

//export TT_Lookup
func TT_Lookup(instanceID *C.char) *C.TTInstance {
	r := runtime.GlobalRuntime()
	if r == nil {
		return nil
	}

	inst := r.GetInstance(C.GoString(instanceID))
	if inst == nil {
		return nil
	}

	return (*C.TTInstance)(unsafe.Pointer(inst))
}

// ============================================================================
// Instance Variable Access
// ============================================================================

//export TT_GetIVar
func TT_GetIVar(inst *C.TTInstance, name *C.char) C.TTValue {
	if inst == nil {
		return valueToC(runtime.NilValue())
	}

	goInst := (*runtime.Instance)(unsafe.Pointer(inst))
	val := goInst.GetVar(C.GoString(name))
	return valueToC(val)
}

//export TT_SetIVar
func TT_SetIVar(inst *C.TTInstance, name *C.char, val C.TTValue) {
	if inst == nil {
		return
	}

	goInst := (*runtime.Instance)(unsafe.Pointer(inst))
	goInst.SetVar(C.GoString(name), valueFromC(val))
}

//export TT_GetIVarByIndex
func TT_GetIVarByIndex(inst *C.TTInstance, index C.int) C.TTValue {
	if inst == nil {
		return valueToC(runtime.NilValue())
	}

	goInst := (*runtime.Instance)(unsafe.Pointer(inst))
	if goInst.Class == nil || int(index) >= len(goInst.Class.InstanceVars) {
		return valueToC(runtime.NilValue())
	}

	name := goInst.Class.InstanceVars[int(index)]
	return valueToC(goInst.GetVar(name))
}

//export TT_SetIVarByIndex
func TT_SetIVarByIndex(inst *C.TTInstance, index C.int, val C.TTValue) {
	if inst == nil {
		return
	}

	goInst := (*runtime.Instance)(unsafe.Pointer(inst))
	if goInst.Class == nil || int(index) >= len(goInst.Class.InstanceVars) {
		return
	}

	name := goInst.Class.InstanceVars[int(index)]
	goInst.SetVar(name, valueFromC(val))
}

// ============================================================================
// Message Dispatch
// ============================================================================

//export TT_Send
func TT_Send(receiver, selector *C.char, args *C.TTValue, numArgs C.int) C.TTValue {
	r := runtime.GlobalRuntime()
	if r == nil {
		return valueToC(runtime.ErrorValue("runtime not initialized"))
	}

	// Convert args
	goArgs := make([]runtime.Value, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			goArgs[i] = valueFromC(cArg)
		}
	}

	result := r.Send(C.GoString(receiver), C.GoString(selector), goArgs)
	return valueToC(result)
}

//export TT_SendDirect
func TT_SendDirect(inst *C.TTInstance, selector *C.char, args *C.TTValue, numArgs C.int) C.TTValue {
	r := runtime.GlobalRuntime()
	if r == nil || inst == nil {
		return valueToC(runtime.ErrorValue("runtime not initialized or nil instance"))
	}

	goInst := (*runtime.Instance)(unsafe.Pointer(inst))

	// Convert args
	goArgs := make([]runtime.Value, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			goArgs[i] = valueFromC(cArg)
		}
	}

	result := r.SendDirect(goInst, C.GoString(selector), goArgs)
	return valueToC(result)
}

// ============================================================================
// Block Operations
// ============================================================================

//export TT_RegisterBlock
func TT_RegisterBlock(bytecodeData *C.uint8_t, bytecodeLen C.size_t, captures **C.TTValue, numCaptures C.int) *C.char {
	r := runtime.GlobalRuntime()
	if r == nil {
		return nil
	}

	// Convert bytecode bytes to Go slice
	goBytes := C.GoBytes(unsafe.Pointer(bytecodeData), C.int(bytecodeLen))

	// Deserialize bytecode
	chunk, err := bytecode.Deserialize(goBytes)
	if err != nil {
		return nil
	}

	// Convert captures if provided
	var goCaptures []*runtime.CaptureCell
	if captures != nil && numCaptures > 0 {
		cCaptures := unsafe.Slice(captures, int(numCaptures))
		goCaptures = make([]*runtime.CaptureCell, int(numCaptures))
		for i, capPtr := range cCaptures {
			if capPtr != nil {
				val := valueFromC(*capPtr)
				goCaptures[i] = &runtime.CaptureCell{
					Value: val,
				}
			}
		}
	}

	// Register the block
	blockID := r.RegisterBlock(chunk, goCaptures, "", "")

	if blockID == "" {
		return nil
	}

	return C.CString(blockID)
}

//export TT_LookupBlock
func TT_LookupBlock(blockID *C.char) *C.TTBlock {
	r := runtime.GlobalRuntime()
	if r == nil {
		return nil
	}

	block := r.LookupBlock(C.GoString(blockID))
	if block == nil {
		return nil
	}

	return (*C.TTBlock)(unsafe.Pointer(block))
}

//export TT_InvokeBlock
func TT_InvokeBlock(blockID *C.char, args *C.TTValue, numArgs C.int) C.TTValue {
	r := runtime.GlobalRuntime()
	if r == nil {
		return valueToC(runtime.ErrorValue("runtime not initialized"))
	}

	// Convert args
	goArgs := make([]runtime.Value, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			goArgs[i] = valueFromC(cArg)
		}
	}

	result := r.InvokeBlock(C.GoString(blockID), goArgs)
	return valueToC(result)
}

//export TT_InvokeBlockDirect
func TT_InvokeBlockDirect(block *C.TTBlock, args *C.TTValue, numArgs C.int) C.TTValue {
	r := runtime.GlobalRuntime()
	if r == nil || block == nil {
		return valueToC(runtime.ErrorValue("runtime not initialized or nil block"))
	}

	goBlock := (*runtime.Block)(unsafe.Pointer(block))

	// Convert args
	goArgs := make([]runtime.Value, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			goArgs[i] = valueFromC(cArg)
		}
	}

	result := r.InvokeBlockDirect(goBlock, goArgs)
	return valueToC(result)
}

// ============================================================================
// Persistence
// ============================================================================

//export TT_Persist
func TT_Persist(instanceID *C.char) C.int {
	r := runtime.GlobalRuntime()
	if r == nil {
		return -1
	}

	err := r.Persist(C.GoString(instanceID))
	if err != nil {
		return -1
	}
	return 0
}

//export TT_Load
func TT_Load(instanceID *C.char) *C.TTInstance {
	r := runtime.GlobalRuntime()
	if r == nil {
		return nil
	}

	inst, err := r.Load(C.GoString(instanceID))
	if err != nil {
		return nil
	}

	return (*C.TTInstance)(unsafe.Pointer(inst))
}

// ============================================================================
// Bash Bridge
// ============================================================================

//export TT_SetSessionID
func TT_SetSessionID(sessionID *C.char) {
	r := runtime.GlobalRuntime()
	if r == nil || r.BashBridge == nil {
		return
	}
	r.BashBridge.SetSessionID(C.GoString(sessionID))
}

//export TT_BashFallback
func TT_BashFallback(receiver, selector *C.char, args **C.char, numArgs C.int) *C.char {
	r := runtime.GlobalRuntime()
	if r == nil || r.BashBridge == nil {
		return nil
	}

	// Convert args
	goArgs := make([]runtime.Value, int(numArgs))
	if args != nil && numArgs > 0 {
		argPtrs := unsafe.Slice(args, int(numArgs))
		for i, ptr := range argPtrs {
			goArgs[i] = runtime.StringValue(C.GoString(ptr))
		}
	}

	result := r.BashBridge.Fallback(C.GoString(receiver), C.GoString(selector), goArgs)
	return C.CString(result.AsString())
}

// ============================================================================
// Value Helpers
// ============================================================================

//export TT_MakeNil
func TT_MakeNil() C.TTValue {
	return valueToC(runtime.NilValue())
}

//export TT_MakeInt
func TT_MakeInt(n C.int64_t) C.TTValue {
	return valueToC(runtime.IntValue(int64(n)))
}

//export TT_MakeFloat
func TT_MakeFloat(f C.double) C.TTValue {
	return valueToC(runtime.FloatValue(float64(f)))
}

//export TT_MakeString
func TT_MakeString(s *C.char) C.TTValue {
	return valueToC(runtime.StringValue(C.GoString(s)))
}

//export TT_MakeBool
func TT_MakeBool(b C.int) C.TTValue {
	return valueToC(runtime.BoolValue(b != 0))
}

//export TT_ValueAsString
func TT_ValueAsString(val C.TTValue) *C.char {
	goVal := valueFromC(val)
	return C.CString(goVal.AsString())
}

//export TT_ValueAsInt
func TT_ValueAsInt(val C.TTValue) C.int64_t {
	goVal := valueFromC(val)
	return C.int64_t(goVal.AsInt())
}

//export TT_ValueToJSON
func TT_ValueToJSON(val C.TTValue) *C.char {
	goVal := valueFromC(val)
	return C.CString(goVal.ToJSON())
}

// ============================================================================
// Serialization (for bash boundary)
// ============================================================================

//export TT_Serialize
func TT_Serialize(inst *C.TTInstance) *C.char {
	if inst == nil {
		return nil
	}
	goInst := (*runtime.Instance)(unsafe.Pointer(inst))
	return C.CString(goInst.ToJSON())
}

//export TT_Deserialize
func TT_Deserialize(jsonStr *C.char) *C.TTInstance {
	r := runtime.GlobalRuntime()
	if r == nil || jsonStr == nil {
		return nil
	}

	// Parse JSON to extract class and create/update instance
	inst, err := r.DeserializeInstance(C.GoString(jsonStr))
	if err != nil {
		return nil
	}

	return (*C.TTInstance)(unsafe.Pointer(inst))
}

//export TT_GetInstanceID
func TT_GetInstanceID(inst *C.TTInstance) *C.char {
	if inst == nil {
		return nil
	}
	goInst := (*runtime.Instance)(unsafe.Pointer(inst))
	return C.CString(goInst.ID)
}

//export TT_GetInstanceClass
func TT_GetInstanceClass(inst *C.TTInstance) *C.char {
	if inst == nil {
		return nil
	}
	goInst := (*runtime.Instance)(unsafe.Pointer(inst))
	return C.CString(goInst.ClassName)
}

//export TT_MakeInstance
func TT_MakeInstance(inst *C.TTInstance) C.TTValue {
	if inst == nil {
		return valueToC(runtime.NilValue())
	}
	goInst := (*runtime.Instance)(unsafe.Pointer(inst))
	return valueToC(runtime.InstanceValue(goInst))
}

//export TT_GetLastError
func TT_GetLastError() *C.char {
	return nil
}
