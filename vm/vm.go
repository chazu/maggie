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
	CharacterClass         *Class
	MessageClass           *Class

	// Exception hierarchy
	ExceptionClass             *Class
	ErrorClass                 *Class
	MessageNotUnderstoodClass  *Class
	ZeroDivideClass            *Class
	SubscriptOutOfBoundsClass  *Class
	StackOverflowClass         *Class
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

	// registry holds all VM-local object registries (concurrency, exceptions,
	// results, contexts, dictionaries, strings, gRPC, HTTP, cells, class vars, etc.).
	registry *ObjectRegistry

	// symbolDispatch centralises marker-based class resolution for symbol-encoded types.
	symbolDispatch *SymbolDispatch

	// AOT compiled methods - maps (class, method) to AOT-compiled functions.
	// When set, these are used instead of interpreting bytecode.
	aotMethods AOTDispatchTable

	// JIT compiler for adaptive compilation of hot methods
	jit *JITCompiler

	// registryGC periodically sweeps concurrency and exception registries.
	registryGC *RegistryGC

	// fileInFunc compiles source text into the VM.
	// Set by cmd/mag after compiler initialization to avoid circular imports.
	fileInFunc FileInFunc
}

// NewVM creates and bootstraps a new VM.
func NewVM() *VM {
	vm := &VM{
		Selectors:      NewSelectorTable(),
		Symbols:        NewSymbolTable(),
		Classes:        NewClassTable(),
		Traits:         NewTraitTable(),
		Globals:        make(map[string]Value),
		keepAlive:      make(map[*Object]struct{}),
		weakRefs:       NewWeakRegistry(),
		registry:       NewObjectRegistry(),
		symbolDispatch: NewSymbolDispatch(),
	}

	// Bootstrap core classes
	vm.bootstrap()

	// Create interpreter
	vm.interpreter = vm.newInterpreter()

	// Create debug server
	vm.Debugger = NewDebugServer(vm)

	// Start registry GC for automatic cleanup of stale concurrency objects
	vm.registryGC = NewRegistryGC(vm, DefaultGCInterval)
	vm.registryGC.Start()

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
	// Re-intern well-known selectors against the VM's table so cached IDs
	// match the IDs used by methods registered on the VM's classes.
	interp.internWellKnownSelectors()
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

	// Phase 5h: Create Character class
	vm.CharacterClass = vm.createClass("Character", vm.ObjectClass)

	// Phase 5i: Create Message class (for doesNotUnderstand:)
	vm.bootstrapMessageClass()

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
	vm.registerCharacterPrimitives()
	vm.registerMessagePrimitives()
	vm.registerClassReflectionPrimitives()
	vm.registerDocstringPrimitives()
	vm.registerCompilerPrimitives()
	vm.registerFilePrimitives()
	vm.registerDebuggerPrimitives()
	vm.registerHttpPrimitives()

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
	vm.Globals["Character"] = vm.classValue(vm.CharacterClass)

	// Well-known symbols
	vm.Globals["nil"] = Nil
	vm.Globals["true"] = True
	vm.Globals["false"] = False

	// Phase 8: Register symbol-encoded types in the central dispatch table
	vm.registerSymbolDispatch()
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

// registerSymbolDispatch registers all symbol-encoded value types in the
// central dispatch table. This replaces the duplicated if/else chains in
// vtableFor(), Send(), and ClassFor().
func (vm *VM) registerSymbolDispatch() {
	sd := vm.symbolDispatch

	// Concurrency primitives
	sd.Register(channelMarker, &SymbolTypeEntry{Class: vm.ChannelClass})
	sd.Register(processMarker, &SymbolTypeEntry{Class: vm.ProcessClass})
	sd.Register(mutexMarker, &SymbolTypeEntry{Class: vm.MutexClass})
	sd.Register(waitGroupMarker, &SymbolTypeEntry{Class: vm.WaitGroupClass})
	sd.Register(semaphoreMarker, &SymbolTypeEntry{Class: vm.SemaphoreClass})
	sd.Register(cancellationContextMarker, &SymbolTypeEntry{Class: vm.CancellationContextClass})

	// gRPC
	sd.Register(grpcClientMarker, &SymbolTypeEntry{Class: vm.GrpcClientClass})
	sd.Register(grpcStreamMarker, &SymbolTypeEntry{Class: vm.GrpcStreamClass})

	// Weak references
	sd.Register(weakRefMarker, &SymbolTypeEntry{Class: vm.WeakReferenceClass})

	// Characters
	sd.Register(characterMarker, &SymbolTypeEntry{Class: vm.CharacterClass})

	// Class values — dispatch via ClassVTable
	sd.Register(classValueMarker, &SymbolTypeEntry{
		ClassSide: true,
		Resolve: func(v Value, _ *VM) (*Class, bool) {
			cls := getClassFromValue(v)
			if cls != nil {
				return cls, true
			}
			return nil, false
		},
	})

	// Exceptions — resolve to the specific exception subclass
	sd.Register(exceptionMarker, &SymbolTypeEntry{
		Resolve: func(v Value, resolveVM *VM) (*Class, bool) {
			exObj := resolveVM.registry.GetException(v.ExceptionID())
			if exObj != nil && exObj.ExceptionClass != nil {
				return exObj.ExceptionClass, true
			}
			return resolveVM.ExceptionClass, true
		},
	})

	// Results — resolve to Success or Failure
	sd.Register(resultMarker, &SymbolTypeEntry{
		Resolve: func(v Value, vmRef *VM) (*Class, bool) {
			r := vmRef.registry.GetResultFromValue(v)
			if r != nil && r.resultType == ResultSuccess {
				return vm.SuccessClass, true
			}
			return vm.FailureClass, true
		},
	})
}

// classValue returns a Value representing a class.
// Uses the class value registry to produce a first-class class value.
func (vm *VM) classValue(c *Class) Value {
	return registerClassValue(c)
}

// ClassValue returns a first-class Value representing the given class.
// This is the exported version for use by cmd/mag and other packages.
func (vm *VM) ClassValue(c *Class) Value {
	return registerClassValue(c)
}

// ---------------------------------------------------------------------------
// Concurrency Registry Accessors
// ---------------------------------------------------------------------------

// Concurrency returns the concurrency registry for this VM.
func (vm *VM) Concurrency() *ConcurrencyRegistry {
	return vm.registry.ConcurrencyRegistry
}

// Registry returns the VM's object registry for external package access.
func (vm *VM) Registry() *ObjectRegistry {
	return vm.registry
}

// SweepConcurrency runs garbage collection on concurrency objects.
// Removes closed channels and terminated processes.
// Returns the number of objects swept.
func (vm *VM) SweepConcurrency() (channels, processes, blocks int) {
	if vm.registry != nil {
		return vm.registry.Sweep()
	}
	return 0, 0, 0
}

// ConcurrencyStats returns statistics about concurrency objects.
func (vm *VM) ConcurrencyStats() map[string]int {
	if vm.registry != nil {
		return vm.registry.Stats()
	}
	return nil
}

// --- Channel helpers ---

func (vm *VM) registerChannel(ch *ChannelObject) Value {
	return vm.registry.RegisterChannel(ch)
}

func (vm *VM) getChannel(v Value) *ChannelObject {
	return vm.registry.GetChannel(v)
}

// --- Process helpers ---

func (vm *VM) createProcess() *ProcessObject {
	return vm.registry.CreateProcess()
}

func (vm *VM) registerProcess(proc *ProcessObject) Value {
	return vm.registry.RegisterProcess(proc)
}

func (vm *VM) getProcess(v Value) *ProcessObject {
	return vm.registry.GetProcess(v)
}

// --- Mutex helpers ---

func (vm *VM) registerMutex(mu *MutexObject) Value {
	return vm.registry.RegisterMutex(mu)
}

func (vm *VM) getMutex(v Value) *MutexObject {
	return vm.registry.GetMutex(v)
}

// --- WaitGroup helpers ---

func (vm *VM) registerWaitGroup(wg *WaitGroupObject) Value {
	return vm.registry.RegisterWaitGroup(wg)
}

func (vm *VM) getWaitGroup(v Value) *WaitGroupObject {
	return vm.registry.GetWaitGroup(v)
}

// --- Semaphore helpers ---

func (vm *VM) registerSemaphore(sem *SemaphoreObject) Value {
	return vm.registry.RegisterSemaphore(sem)
}

func (vm *VM) getSemaphore(v Value) *SemaphoreObject {
	return vm.registry.GetSemaphore(v)
}

func (vm *VM) registerCancellationContext(ctx *CancellationContextObject) Value {
	return vm.registry.RegisterCancellationContext(ctx)
}

func (vm *VM) getCancellationContext(v Value) *CancellationContextObject {
	return vm.registry.GetCancellationContext(v)
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
	cls := vm.ClassFor(v)
	if cls != nil {
		return vm.Symbols.SymbolValue(cls.Name)
	}
	return vm.Symbols.SymbolValue("Object")
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
	case v.IsSymbol():
		// Use the central dispatch table for all symbol-encoded types
		if cls, isClassSide := vm.symbolDispatch.ClassForSymbolVM(v, vm); cls != nil {
			if isClassSide {
				return vm.ClassClass
			}
			return cls
		}
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
	isClassSide := false
	if receiver.IsSymbol() {
		// Use the central dispatch table for all symbol-encoded types
		class, isClassSide = vm.symbolDispatch.ClassForSymbolVM(receiver, vm)
		if class == nil {
			// Not a registered symbol type — check if it's a class name
			symName := vm.Symbols.Name(receiver.SymbolID())
			if cls := vm.Classes.Lookup(symName); cls != nil {
				class = cls
				isClassSide = true
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
		return vm.sendDoesNotUnderstand(receiver, selector, args)
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

// sendDoesNotUnderstand dispatches doesNotUnderstand: with a reified Message.
// If doesNotUnderstand: is not defined (e.g. before .mag libraries are loaded),
// falls back to returning Nil for backward compatibility.
func (vm *VM) sendDoesNotUnderstand(receiver Value, selector string, args []Value) Value {
	dnuSelectorID := vm.Selectors.Intern("doesNotUnderstand:")
	class := vm.ClassFor(receiver)
	if class == nil {
		return Nil
	}

	dnuMethod := class.VTable.Lookup(dnuSelectorID)
	if dnuMethod == nil {
		return Nil
	}

	// If the doesNotUnderstand: method is a primitive (Go function), pass a Message.
	// If it's a compiled method (from .mag files), also pass a Message — but the
	// compiled method must expect a Message object (not a bare selector string).
	selectorSym := vm.Symbols.SymbolValue(selector)
	msg := vm.createMessage(selectorSym, args)

	return dnuMethod.Invoke(vm, receiver, []Value{msg})
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

// SendSuper performs a super send for AOT-compiled methods.
// definingClassName is the name of the class where the calling method is defined.
// The method lookup starts from definingClassName's superclass, following
// correct Smalltalk super semantics.
func (vm *VM) SendSuper(receiver Value, selector string, args []Value, definingClassName string) Value {
	definingClass := vm.Classes.Lookup(definingClassName)
	if definingClass == nil || definingClass.Superclass == nil {
		return Nil
	}

	selectorID := vm.Selectors.Intern(selector)

	// Look up the method in the superclass's VTable
	superVT := definingClass.Superclass.VTable
	if superVT == nil {
		return Nil
	}

	method := superVT.Lookup(selectorID)
	if method == nil {
		return Nil
	}

	// Execute the method with the original receiver (not the superclass)
	if cm, ok := method.(*CompiledMethod); ok {
		// Check for AOT-compiled version first
		if vm.aotMethods != nil {
			if aotMethod := vm.aotMethods[AOTDispatchKey{definingClass.Superclass.Name, selector}]; aotMethod != nil {
				return aotMethod(vm, receiver, args)
			}
		}
		return vm.interpreter.Execute(cm, receiver, args)
	}

	// Primitive method
	return method.Invoke(vm, receiver, args)
}

// CreateBlock creates a block value from a BlockMethod and captures for use in AOT-compiled code.
// methodIdx is the index of the block in the parent method's Blocks slice.
// The method parameter is the parent CompiledMethod that contains the block definition.
func (vm *VM) CreateBlock(block *BlockMethod, captures []Value, homeSelf Value) Value {
	bv := &BlockValue{
		Block:      block,
		Captures:   captures,
		HomeFrame:  -1, // AOT blocks are detached (no interpreter frame)
		HomeSelf:   homeSelf,
		HomeMethod: nil,
	}

	// If the block has an outer method, set it as HomeMethod
	if block.Outer != nil {
		bv.HomeMethod = block.Outer
	}

	id := vm.registry.RegisterBlock(bv)

	return FromBlockID(uint32(id))
}

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

	// Mark objects reachable from block captures (VM-local registry)
	vm.registry.blocksMu.RLock()
	for _, bv := range vm.registry.blocks {
		if bv != nil {
			for _, capture := range bv.Captures {
				vm.markValue(capture, marked)
			}
			vm.markValue(bv.HomeSelf, marked)
		}
	}
	vm.registry.blocksMu.RUnlock()

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

// KeepAlive pins an object to prevent garbage collection.
func (vm *VM) KeepAlive(obj *Object) {
	if obj != nil {
		vm.keepAlive[obj] = struct{}{}
	}
}

// ReleaseKeepAlive unpins an object, allowing garbage collection.
func (vm *VM) ReleaseKeepAlive(obj *Object) {
	if obj != nil {
		delete(vm.keepAlive, obj)
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
	wr := NewWeakReference(vm.registry, target)
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

// ---------------------------------------------------------------------------
// VM Lifecycle
// ---------------------------------------------------------------------------

// Shutdown stops background goroutines (registry GC, etc.) and releases
// resources. Call this when the VM is no longer needed.
func (vm *VM) Shutdown() {
	if vm.registryGC != nil {
		vm.registryGC.Stop()
	}
}

// RegistryGC returns the registry garbage collector for configuration
// and monitoring. Returns nil if the VM has not been initialized.
func (vm *VM) RegistryGC() *RegistryGC {
	return vm.registryGC
}
