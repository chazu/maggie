package vm

import (
	"fmt"
	"runtime"
	"strconv"
	"strings"
	"sync"
)

// ---------------------------------------------------------------------------
// VM: The Maggie Virtual Machine
// ---------------------------------------------------------------------------

// VM is the main Maggie virtual machine.
type VM struct {
	// Global tables
	Selectors *SelectorTable // selector name -> ID
	Symbols   *SymbolTable   // symbol name -> ID
	Classes   *ClassTable    // class name -> Class
	Traits    *TraitTable    // trait name -> Trait
	Globals   map[string]Value

	// Well-known classes (for fast-path checks and bootstrapping)
	ObjectClass            *Class
	ClassClass             *Class
	MetaclassClass         *Class
	BlockClass             *Class
	BooleanClass           *Class
	TrueClass              *Class
	FalseClass             *Class
	UndefinedObjectClass   *Class
	SmallIntegerClass      *Class
	FloatClass             *Class
	StringClass            *Class
	SymbolClass            *Class
	ArrayClass             *Class
	ByteArrayClass         *Class
	AssociationClass       *Class
	DictionaryClass        *Class
	CompiledMethodClass    *Class
	ChannelClass           *Class
	ProcessClass           *Class
	MutexClass                *Class
	WaitGroupClass            *Class
	SemaphoreClass            *Class
	CancellationContextClass  *Class
	ResultClass            *Class
	SuccessClass           *Class
	FailureClass           *Class
	GrpcClientClass        *Class
	GrpcStreamClass        *Class
	ContextClass           *Class
	WeakReferenceClass     *Class

	// Exception hierarchy
	ExceptionClass             *Class
	ErrorClass                 *Class
	MessageNotUnderstoodClass  *Class
	ZeroDivideClass            *Class
	SubscriptOutOfBoundsClass  *Class
	WarningClass               *Class
	HaltClass                  *Class
	NotificationClass          *Class

	// Interpreter
	interpreter *Interpreter

	// Goroutine-local interpreter tracking (for forked processes)
	// Maps goroutine ID to the interpreter running in that goroutine
	interpreters sync.Map // int64 -> *Interpreter

	// Debugger for IDE integration
	Debugger *DebugServer

	// Compiler backend (Go or Maggie)
	compilerBackend CompilerBackend

	// keepAlive holds references to objects to prevent GC.
	// Uses a map for O(1) lookup and removal during garbage collection.
	keepAlive map[*Object]struct{}

	// weakRefs tracks weak references for the GC to process.
	weakRefs *WeakRegistry

	// concurrency holds registries for channels, processes, mutexes, etc.
	concurrency *ConcurrencyRegistry

	// AOT compiled methods - maps (class, method) to AOT-compiled functions.
	// When set, these are used instead of interpreting bytecode.
	aotMethods AOTDispatchTable

	// JIT compiler for adaptive compilation of hot methods
	jit *JITCompiler
}

// NewVM creates and bootstraps a new VM.
func NewVM() *VM {
	vm := &VM{
		Selectors:   NewSelectorTable(),
		Symbols:     NewSymbolTable(),
		Classes:     NewClassTable(),
		Traits:      NewTraitTable(),
		Globals:     make(map[string]Value),
		keepAlive:   make(map[*Object]struct{}),
		weakRefs:    NewWeakRegistry(),
		concurrency: NewConcurrencyRegistry(),
	}

	// Bootstrap core classes
	vm.bootstrap()

	// Create interpreter
	vm.interpreter = vm.newInterpreter()

	// Create debug server
	vm.Debugger = NewDebugServer(vm)

	return vm
}

// newInterpreter creates an interpreter connected to this VM.
func (vm *VM) newInterpreter() *Interpreter {
	interp := NewInterpreter()
	// Share tables with VM
	interp.Selectors = vm.Selectors
	interp.Symbols = vm.Symbols
	interp.Classes = vm.Classes
	interp.Globals = vm.Globals
	interp.vm = vm // Back-reference for primitives
	return interp
}

// ---------------------------------------------------------------------------
// Bootstrap: Create core classes
// ---------------------------------------------------------------------------

func (vm *VM) bootstrap() {
	// Phase 1: Create Object and Class (chicken-and-egg problem)
	// Object is the root of all classes
	// Class is the class of all classes
	vm.ObjectClass = vm.createBootstrapClass("Object", nil)
	vm.ClassClass = vm.createBootstrapClass("Class", vm.ObjectClass)

	// Now we can properly set up metaclasses
	// For now, we skip full metaclass support for simplicity

	// Phase 2: Create behavior classes
	vm.BooleanClass = vm.createClass("Boolean", vm.ObjectClass)
	vm.TrueClass = vm.createClass("True", vm.BooleanClass)
	vm.FalseClass = vm.createClass("False", vm.BooleanClass)
	vm.UndefinedObjectClass = vm.createClass("UndefinedObject", vm.ObjectClass)

	// Phase 3: Create magnitude classes
	vm.SmallIntegerClass = vm.createClass("SmallInteger", vm.ObjectClass)
	vm.FloatClass = vm.createClass("Float", vm.ObjectClass)

	// Phase 4: Create collection classes
	vm.StringClass = vm.createClass("String", vm.ObjectClass)
	vm.SymbolClass = vm.createClass("Symbol", vm.StringClass)
	vm.ArrayClass = vm.createClass("Array", vm.ObjectClass)
	vm.ByteArrayClass = vm.createClass("ByteArray", vm.ObjectClass)
	vm.AssociationClass = vm.createClassWithIvars("Association", vm.ObjectClass, []string{"key", "value"})
	vm.DictionaryClass = vm.createClassWithIvars("Dictionary", vm.ObjectClass, []string{"table", "size"})

	// Phase 5: Create method/block classes
	vm.BlockClass = vm.createClass("Block", vm.ObjectClass)
	vm.CompiledMethodClass = vm.createClass("CompiledMethod", vm.ObjectClass)

	// Phase 5b: Create concurrency classes
	vm.ChannelClass = vm.createClass("Channel", vm.ObjectClass)
	vm.ProcessClass = vm.createClass("Process", vm.ObjectClass)
	vm.MutexClass = vm.createClass("Mutex", vm.ObjectClass)
	vm.WaitGroupClass = vm.createClass("WaitGroup", vm.ObjectClass)
	vm.SemaphoreClass = vm.createClass("Semaphore", vm.ObjectClass)
	vm.CancellationContextClass = vm.createClass("CancellationContext", vm.ObjectClass)

	// Phase 5c: Create Result pattern classes
	vm.ResultClass = vm.createClass("Result", vm.ObjectClass)
	vm.SuccessClass = vm.createClass("Success", vm.ResultClass)
	vm.FailureClass = vm.createClass("Failure", vm.ResultClass)

	// Phase 5d: Create gRPC classes
	vm.GrpcClientClass = vm.createClass("GrpcClient", vm.ObjectClass)
	vm.GrpcStreamClass = vm.createClass("GrpcStream", vm.ObjectClass)

	// Phase 5e: Create Context class for thisContext
	vm.ContextClass = vm.createClass("Context", vm.ObjectClass)

	// Phase 5g: Create WeakReference class
	vm.WeakReferenceClass = vm.createClass("WeakReference", vm.ObjectClass)

	// Phase 5f: Create Exception class hierarchy
	vm.bootstrapExceptionClasses()

	// Phase 6: Register primitives on core classes
	vm.registerObjectPrimitives()
	vm.registerBooleanPrimitives()
	vm.registerSmallIntegerPrimitives()
	vm.registerFloatPrimitives()
	vm.registerSymbolPrimitives()
	vm.registerStringPrimitives()
	vm.registerArrayPrimitives()
	vm.registerBlockPrimitives()
	vm.registerChannelPrimitives()
	vm.registerChannelSelectPrimitives()
	vm.registerProcessPrimitives()
	vm.registerMutexPrimitives()
	vm.registerWaitGroupPrimitives()
	vm.registerSemaphorePrimitives()
	vm.registerCancellationContextPrimitives()
	vm.registerResultPrimitives()
	vm.registerDictionaryPrimitives()
	vm.registerGrpcPrimitives()
	vm.registerContextPrimitives()
	vm.registerExceptionPrimitives()
	vm.registerExceptionBlockPrimitives()
	vm.registerWeakReferencePrimitives()
	vm.registerClassReflectionPrimitives()
	vm.registerCompilerPrimitives()
	vm.registerFilePrimitives()
	vm.registerDebuggerPrimitives()

	// Phase 7: Set up globals
	vm.Globals["Object"] = vm.classValue(vm.ObjectClass)
	vm.Globals["Class"] = vm.classValue(vm.ClassClass)
	vm.Globals["Boolean"] = vm.classValue(vm.BooleanClass)
	vm.Globals["True"] = vm.classValue(vm.TrueClass)
	vm.Globals["False"] = vm.classValue(vm.FalseClass)
	vm.Globals["UndefinedObject"] = vm.classValue(vm.UndefinedObjectClass)
	vm.Globals["SmallInteger"] = vm.classValue(vm.SmallIntegerClass)
	vm.Globals["Float"] = vm.classValue(vm.FloatClass)
	vm.Globals["String"] = vm.classValue(vm.StringClass)
	vm.Globals["Symbol"] = vm.classValue(vm.SymbolClass)
	vm.Globals["Array"] = vm.classValue(vm.ArrayClass)
	vm.Globals["Block"] = vm.classValue(vm.BlockClass)
	vm.Globals["Channel"] = vm.classValue(vm.ChannelClass)
	vm.Globals["Process"] = vm.classValue(vm.ProcessClass)
	vm.Globals["Mutex"] = vm.classValue(vm.MutexClass)
	vm.Globals["WaitGroup"] = vm.classValue(vm.WaitGroupClass)
	vm.Globals["Semaphore"] = vm.classValue(vm.SemaphoreClass)
	vm.Globals["CancellationContext"] = vm.classValue(vm.CancellationContextClass)
	vm.Globals["Result"] = vm.classValue(vm.ResultClass)
	vm.Globals["Success"] = vm.classValue(vm.SuccessClass)
	vm.Globals["Failure"] = vm.classValue(vm.FailureClass)
	vm.Globals["Dictionary"] = vm.classValue(vm.DictionaryClass)
	vm.Globals["GrpcClient"] = vm.classValue(vm.GrpcClientClass)
	vm.Globals["GrpcStream"] = vm.classValue(vm.GrpcStreamClass)
	vm.Globals["Context"] = vm.classValue(vm.ContextClass)
	vm.Globals["WeakReference"] = vm.classValue(vm.WeakReferenceClass)

	// Well-known symbols
	vm.Globals["nil"] = Nil
	vm.Globals["true"] = True
	vm.Globals["false"] = False
}

// createBootstrapClass creates a class during early bootstrap.
func (vm *VM) createBootstrapClass(name string, superclass *Class) *Class {
	c := NewClass(name, superclass)
	vm.Classes.Register(c)
	return c
}

// createClass creates a regular class.
func (vm *VM) createClass(name string, superclass *Class) *Class {
	c := NewClass(name, superclass)
	vm.Classes.Register(c)
	return c
}

// createClassWithIvars creates a class with instance variables.
func (vm *VM) createClassWithIvars(name string, superclass *Class, ivars []string) *Class {
	c := NewClassWithInstVars(name, superclass, ivars)
	vm.Classes.Register(c)
	return c
}

// classValue returns a Value representing a class.
// For now, we use a symbol as a placeholder.
func (vm *VM) classValue(c *Class) Value {
	return vm.Symbols.SymbolValue(c.Name)
}

// ---------------------------------------------------------------------------
// Concurrency Registry Accessors
// ---------------------------------------------------------------------------

// Concurrency returns the concurrency registry for this VM.
func (vm *VM) Concurrency() *ConcurrencyRegistry {
	return vm.concurrency
}

// SweepConcurrency runs garbage collection on concurrency objects.
// Removes closed channels and terminated processes.
// Returns the number of objects swept.
func (vm *VM) SweepConcurrency() (channels, processes, blocks int) {
	if vm.concurrency != nil {
		return vm.concurrency.Sweep()
	}
	return 0, 0, 0
}

// ConcurrencyStats returns statistics about concurrency objects.
func (vm *VM) ConcurrencyStats() map[string]int {
	if vm.concurrency != nil {
		return vm.concurrency.Stats()
	}
	return nil
}

// --- Channel helpers ---

func (vm *VM) registerChannel(ch *ChannelObject) Value {
	return vm.concurrency.RegisterChannel(ch)
}

func (vm *VM) getChannel(v Value) *ChannelObject {
	return vm.concurrency.GetChannel(v)
}

// --- Process helpers ---

func (vm *VM) createProcess() *ProcessObject {
	return vm.concurrency.CreateProcess()
}

func (vm *VM) registerProcess(proc *ProcessObject) Value {
	return vm.concurrency.RegisterProcess(proc)
}

func (vm *VM) getProcess(v Value) *ProcessObject {
	return vm.concurrency.GetProcess(v)
}

// --- Mutex helpers ---

func (vm *VM) registerMutex(mu *MutexObject) Value {
	return vm.concurrency.RegisterMutex(mu)
}

func (vm *VM) getMutex(v Value) *MutexObject {
	return vm.concurrency.GetMutex(v)
}

// --- WaitGroup helpers ---

func (vm *VM) registerWaitGroup(wg *WaitGroupObject) Value {
	return vm.concurrency.RegisterWaitGroup(wg)
}

func (vm *VM) getWaitGroup(v Value) *WaitGroupObject {
	return vm.concurrency.GetWaitGroup(v)
}

// --- Semaphore helpers ---

func (vm *VM) registerSemaphore(sem *SemaphoreObject) Value {
	return vm.concurrency.RegisterSemaphore(sem)
}

func (vm *VM) getSemaphore(v Value) *SemaphoreObject {
	return vm.concurrency.GetSemaphore(v)
}

func (vm *VM) registerCancellationContext(ctx *CancellationContextObject) Value {
	return vm.concurrency.RegisterCancellationContext(ctx)
}

func (vm *VM) getCancellationContext(v Value) *CancellationContextObject {
	return vm.concurrency.GetCancellationContext(v)
}

// registerStringPrimitives delegates to the extended string primitives.
func (vm *VM) registerStringPrimitives() {
	// Register the extended string primitives from string_primitives.go
	vm.registerStringPrimitivesExtended()
}

// ---------------------------------------------------------------------------
// Class lookup for values
// ---------------------------------------------------------------------------

func (vm *VM) primitiveClass(v Value) Value {
	var className string
	switch {
	case v == Nil:
		className = "UndefinedObject"
	case v == True:
		className = "True"
	case v == False:
		className = "False"
	case v.IsSmallInt():
		className = "SmallInteger"
	case v.IsFloat():
		className = "Float"
	case v.IsBlock():
		className = "Block"
	case IsStringValue(v):
		className = "String"
	case IsDictionaryValue(v):
		className = "Dictionary"
	case v.IsException():
		exObj := GetExceptionObject(v)
		if exObj != nil && exObj.ExceptionClass != nil {
			className = exObj.ExceptionClass.Name
		} else {
			className = "Exception"
		}
	case v.IsWeakRef():
		className = "WeakReference"
	case isChannelValue(v):
		className = "Channel"
	case isProcessValue(v):
		className = "Process"
	case isResultValue(v):
		r := getResult(v)
		if r != nil && r.resultType == ResultSuccess {
			className = "Success"
		} else {
			className = "Failure"
		}
	case isGrpcClientValue(v):
		className = "GrpcClient"
	case isGrpcStreamValue(v):
		className = "GrpcStream"
	case v.IsSymbol():
		className = "Symbol"
	case v.IsObject():
		obj := ObjectFromValue(v)
		if obj != nil && obj.VTablePtr() != nil && obj.VTablePtr().Class() != nil {
			className = obj.VTablePtr().Class().Name
		} else {
			className = "Object"
		}
	default:
		className = "Object"
	}
	return vm.Symbols.SymbolValue(className)
}

// ClassFor returns the class for a value.
func (vm *VM) ClassFor(v Value) *Class {
	switch {
	case v == Nil:
		return vm.UndefinedObjectClass
	case v == True:
		return vm.TrueClass
	case v == False:
		return vm.FalseClass
	case v.IsSmallInt():
		return vm.SmallIntegerClass
	case v.IsFloat():
		return vm.FloatClass
	case v.IsBlock():
		return vm.BlockClass
	case v.IsContext():
		return vm.ContextClass
	case v.IsException():
		// Return the specific exception class
		exObj := GetExceptionObject(v)
		if exObj != nil && exObj.ExceptionClass != nil {
			return exObj.ExceptionClass
		}
		return vm.ExceptionClass
	case v.IsWeakRef():
		return vm.WeakReferenceClass
	case IsStringValue(v):
		return vm.StringClass
	case IsDictionaryValue(v):
		return vm.DictionaryClass
	case v.IsSymbol():
		return vm.SymbolClass
	case v.IsObject():
		obj := ObjectFromValue(v)
		if obj != nil {
			vt := obj.VTablePtr()
			if vt != nil && vt.Class() != nil {
				return vt.Class()
			}
		}
		return vm.ObjectClass
	default:
		return vm.ObjectClass
	}
}

// ---------------------------------------------------------------------------
// Public execution API
// ---------------------------------------------------------------------------

// Execute runs a compiled method with the given receiver and arguments.
func (vm *VM) Execute(method *CompiledMethod, receiver Value, args []Value) Value {
	return vm.interpreter.Execute(method, receiver, args)
}

// ExecuteSafe runs a compiled method and catches Maggie errors, returning them as Go errors.
// This prevents Go panic stack traces from being shown for Maggie-level errors.
// Set MAGGIE_DEBUG=1 to see full Go stack traces.
func (vm *VM) ExecuteSafe(method *CompiledMethod, receiver Value, args []Value) (result Value, err error) {
	defer func() {
		if r := recover(); r != nil {
			if msg, ok := r.(string); ok && len(msg) > 14 && msg[:14] == "Maggie error: " {
				// Extract just the Maggie error message (already includes stack trace)
				err = fmt.Errorf("%s", msg[14:])
				result = Nil
				return
			}
			// Not a Maggie error, re-panic
			panic(r)
		}
	}()
	result = vm.interpreter.Execute(method, receiver, args)
	return result, nil
}

// GetProfiler returns the VM's profiler for inspecting hot code detection.
func (vm *VM) GetProfiler() *Profiler {
	return vm.interpreter.Profiler
}

// EnableJIT enables the JIT compiler for adaptive compilation of hot methods.
// Returns the JIT compiler for configuration.
func (vm *VM) EnableJIT() *JITCompiler {
	if vm.jit == nil {
		vm.jit = NewJITCompiler(vm)
	}
	vm.jit.Enabled = true
	return vm.jit
}

// DisableJIT disables the JIT compiler.
func (vm *VM) DisableJIT() {
	if vm.jit != nil {
		vm.jit.Enabled = false
	}
}

// GetJIT returns the JIT compiler, or nil if not enabled.
func (vm *VM) GetJIT() *JITCompiler {
	return vm.jit
}

// Send sends a message to a receiver.
func (vm *VM) Send(receiver Value, selector string, args []Value) Value {
	selectorID := vm.Selectors.Intern(selector)

	// Determine the class for method dispatch
	var class *Class
	isClassSide := false // Track if this is a class-side dispatch
	if receiver.IsSymbol() {
		// Check for string values first (they use the symbol tag but with high IDs)
		if IsStringValue(receiver) {
			class = vm.StringClass
		} else if IsDictionaryValue(receiver) {
			class = vm.DictionaryClass
		} else if isChannelValue(receiver) {
			// Check for special symbol-encoded values (channels, processes, results)
			class = vm.ChannelClass
		} else if isProcessValue(receiver) {
			class = vm.ProcessClass
		} else if isMutexValue(receiver) {
			class = vm.MutexClass
		} else if isWaitGroupValue(receiver) {
			class = vm.WaitGroupClass
		} else if isSemaphoreValue(receiver) {
			class = vm.SemaphoreClass
		} else if isCancellationContextValue(receiver) {
			class = vm.CancellationContextClass
		} else if isResultValue(receiver) {
			// Determine if it's a Success or Failure
			r := getResult(receiver)
			if r != nil && r.resultType == ResultSuccess {
				class = vm.SuccessClass
			} else {
				class = vm.FailureClass
			}
		} else if receiver.IsException() {
			// Exception - get the specific exception class
			exObj := GetExceptionObject(receiver)
			if exObj != nil && exObj.ExceptionClass != nil {
				class = exObj.ExceptionClass
			} else {
				class = vm.ExceptionClass
			}
		} else if receiver.IsWeakRef() {
			class = vm.WeakReferenceClass
		} else {
			// Check if this symbol represents a class name (for class-side messages)
			// This handles cases like: Channel new, Process sleep: 100
			symName := vm.Symbols.Name(receiver.SymbolID())
			if cls := vm.Classes.Lookup(symName); cls != nil {
				class = cls
				isClassSide = true // Use ClassVTable for class-side dispatch
			} else {
				class = vm.SymbolClass
			}
		}
	} else {
		class = vm.ClassFor(receiver)
	}

	if class == nil {
		return Nil
	}

	// Use ClassVTable for class-side dispatch, VTable for instance-side
	var method Method
	if isClassSide && class.ClassVTable != nil {
		method = class.ClassVTable.Lookup(selectorID)
	} else {
		method = class.VTable.Lookup(selectorID)
	}

	if method == nil {
		return Nil // Would trigger doesNotUnderstand:
	}

	// Check if it's a compiled method or primitive
	if cm, ok := method.(*CompiledMethod); ok {
		// Check for AOT-compiled version first
		if vm.aotMethods != nil {
			if aotMethod := vm.aotMethods[AOTDispatchKey{class.Name, selector}]; aotMethod != nil {
				return aotMethod(vm, receiver, args)
			}
		}
		return vm.interpreter.Execute(cm, receiver, args)
	}

	// Primitive method - pass VM as the vm parameter
	return method.Invoke(vm, receiver, args)
}

// Intern returns the symbol ID for a name.
func (vm *VM) Intern(name string) uint32 {
	return vm.Symbols.Intern(name)
}

// SymbolName returns the name for a symbol ID.
func (vm *VM) SymbolName(id uint32) string {
	return vm.Symbols.Name(id)
}

// LookupGlobal returns a global value by name.
func (vm *VM) LookupGlobal(name string) (Value, bool) {
	v, ok := vm.Globals[name]
	return v, ok
}

// SetGlobal sets a global value.
func (vm *VM) SetGlobal(name string, value Value) {
	vm.Globals[name] = value
}

// LookupClass returns a class by name.
func (vm *VM) LookupClass(name string) *Class {
	return vm.Classes.Lookup(name)
}

// ---------------------------------------------------------------------------
// AOT (Ahead-of-Time) Compilation Support
// ---------------------------------------------------------------------------

// RegisterAOTMethods registers a dispatch table of AOT-compiled methods.
// When a method is found in this table, the AOT-compiled version is used
// instead of interpreting bytecode.
func (vm *VM) RegisterAOTMethods(methods AOTDispatchTable) {
	if vm.aotMethods == nil {
		vm.aotMethods = make(AOTDispatchTable)
	}
	for key, method := range methods {
		vm.aotMethods[key] = method
	}
}

// LookupAOT looks up an AOT-compiled method for a class and selector.
// Returns nil if no AOT method is registered.
func (vm *VM) LookupAOT(className, selector string) AOTMethod {
	if vm.aotMethods == nil {
		return nil
	}
	return vm.aotMethods[AOTDispatchKey{className, selector}]
}

// HasAOT returns true if AOT methods are registered.
func (vm *VM) HasAOT() bool {
	return vm.aotMethods != nil && len(vm.aotMethods) > 0
}

// ---------------------------------------------------------------------------
// Garbage Collection
// ---------------------------------------------------------------------------

// CollectGarbage performs a mark-sweep garbage collection.
// It scans the stack, globals, and block captures to find reachable objects,
// then removes unreachable objects from keepAlive.
func (vm *VM) CollectGarbage() int {
	if vm.interpreter == nil {
		return 0
	}

	// Mark phase: find all reachable objects
	marked := make(map[*Object]struct{})

	// Mark objects reachable from the stack
	for i := 0; i < vm.interpreter.sp; i++ {
		vm.markValue(vm.interpreter.stack[i], marked)
	}

	// Mark objects reachable from globals
	for _, v := range vm.Globals {
		vm.markValue(v, marked)
	}

	// Mark objects reachable from block captures
	blockRegistryMu.RLock()
	for _, bv := range blockRegistry {
		if bv != nil {
			for _, capture := range bv.Captures {
				vm.markValue(capture, marked)
			}
			vm.markValue(bv.HomeSelf, marked)
		}
	}
	blockRegistryMu.RUnlock()

	// Mark objects reachable from frames (temps on stack are already covered,
	// but Receiver values need marking)
	for i := 0; i <= vm.interpreter.fp; i++ {
		if vm.interpreter.frames[i] != nil {
			vm.markValue(vm.interpreter.frames[i].Receiver, marked)
			if vm.interpreter.frames[i].HomeSelf != Nil {
				vm.markValue(vm.interpreter.frames[i].HomeSelf, marked)
			}
		}
	}

	// Process weak references: clear refs to unmarked objects and run finalizers
	if vm.weakRefs != nil {
		vm.weakRefs.ProcessGC(marked)
	}

	// Sweep phase: remove unmarked objects from keepAlive
	collected := 0
	for obj := range vm.keepAlive {
		if _, isMarked := marked[obj]; !isMarked {
			delete(vm.keepAlive, obj)
			collected++
		}
	}

	return collected
}

// markValue recursively marks an object and all objects it references.
func (vm *VM) markValue(v Value, marked map[*Object]struct{}) {
	if !v.IsObject() {
		return
	}

	obj := ObjectFromValue(v)
	if obj == nil {
		return
	}

	// Already marked? Skip to avoid infinite recursion
	if _, exists := marked[obj]; exists {
		return
	}

	// Mark this object
	marked[obj] = struct{}{}

	// Recursively mark all slots
	for i := 0; i < obj.NumSlots(); i++ {
		vm.markValue(obj.GetSlot(i), marked)
	}
}

// KeepAliveCount returns the number of objects in the keepAlive set.
// Useful for testing and debugging.
func (vm *VM) KeepAliveCount() int {
	return len(vm.keepAlive)
}

// ---------------------------------------------------------------------------
// Weak References
// ---------------------------------------------------------------------------

// NewWeakRef creates a new weak reference to the given object.
// The weak reference is registered with the VM's weak reference registry.
func (vm *VM) NewWeakRef(target *Object) *WeakReference {
	wr := NewWeakReference(target)
	vm.weakRefs.Register(wr)
	return wr
}

// LookupWeakRef looks up a weak reference by ID.
func (vm *VM) LookupWeakRef(id uint32) *WeakReference {
	return vm.weakRefs.Lookup(id)
}

// WeakRefCount returns the number of registered weak references.
func (vm *VM) WeakRefCount() int {
	return vm.weakRefs.Count()
}

// ---------------------------------------------------------------------------
// Goroutine-Local Interpreter Tracking
// ---------------------------------------------------------------------------

// getGoroutineID returns the current goroutine's ID by parsing the stack.
// This is a workaround since Go doesn't expose goroutine IDs directly.
func getGoroutineID() int64 {
	var buf [64]byte
	n := runtime.Stack(buf[:], false)
	// Stack starts with "goroutine <id> [...]"
	s := string(buf[:n])
	s = strings.TrimPrefix(s, "goroutine ")
	idx := strings.Index(s, " ")
	if idx > 0 {
		s = s[:idx]
	}
	id, _ := strconv.ParseInt(s, 10, 64)
	return id
}

// registerInterpreter registers an interpreter for the current goroutine.
// Call this when starting execution in a new goroutine (e.g., forked process).
func (vm *VM) registerInterpreter(interp *Interpreter) {
	gid := getGoroutineID()
	vm.interpreters.Store(gid, interp)
}

// unregisterInterpreter removes the interpreter for the current goroutine.
// Call this when execution is complete.
func (vm *VM) unregisterInterpreter() {
	gid := getGoroutineID()
	vm.interpreters.Delete(gid)
}

// currentInterpreter returns the interpreter for the current goroutine.
// If no interpreter is registered for this goroutine, returns the main interpreter.
func (vm *VM) currentInterpreter() *Interpreter {
	gid := getGoroutineID()
	if interp, ok := vm.interpreters.Load(gid); ok {
		return interp.(*Interpreter)
	}
	// Fallback to main interpreter
	return vm.interpreter
}
