package vm

import (
	"fmt"
	"sync"
	"sync/atomic"
	"time"

	"github.com/petermattis/goid"
)

// ---------------------------------------------------------------------------
// VMConfig: Configurable runtime parameters
// ---------------------------------------------------------------------------

// VMConfig holds configurable runtime parameters for the VM.
// Zero values mean "use default".
type VMConfig struct {
	MaxStackDepth   int // Operand stack depth limit (default: 131072)
	MaxFrameDepth   int // Call frame depth limit (default: 8192)
	InitialStack    int // Initial stack allocation (default: 2048)
	InitialFrames   int // Initial frame allocation (default: 512)
	MailboxCapacity int // Process mailbox capacity (default: 4096)
}

// DefaultVMConfig returns the default VM configuration.
func DefaultVMConfig() VMConfig {
	return VMConfig{
		MaxStackDepth:   DefaultMaxStackDepth,
		MaxFrameDepth:   DefaultMaxFrameDepth,
		InitialStack:    DefaultInitialStack,
		InitialFrames:   DefaultInitialFrames,
		MailboxCapacity: DefaultMailboxCapacity,
	}
}

// mergeDefaults fills zero-valued fields with defaults.
func (c VMConfig) mergeDefaults() VMConfig {
	d := DefaultVMConfig()
	if c.MaxStackDepth == 0 {
		c.MaxStackDepth = d.MaxStackDepth
	}
	if c.MaxFrameDepth == 0 {
		c.MaxFrameDepth = d.MaxFrameDepth
	}
	if c.InitialStack == 0 {
		c.InitialStack = d.InitialStack
	}
	if c.InitialFrames == 0 {
		c.InitialFrames = d.InitialFrames
	}
	if c.MailboxCapacity == 0 {
		c.MailboxCapacity = d.MailboxCapacity
	}
	return c
}

// ---------------------------------------------------------------------------
// VM: The Maggie Virtual Machine
// ---------------------------------------------------------------------------

// VM is the main Maggie virtual machine.
type VM struct {
	// Late-bound fields (atomics + freeze flag). Embedded as a value so
	// every VM instance gets its own atomics — see late_bound.go.
	lateBoundFields

	// Global tables
	Selectors *SelectorTable // selector name -> ID
	Symbols   *SymbolTable   // symbol name -> ID
	Classes   *ClassTable    // class name -> Class
	Traits    *TraitTable    // trait name -> Trait
	// globals is unexported to enforce all access through accessors that
	// take globalsMu. Use Global / SetGlobal / DeleteGlobal /
	// RangeGlobals / GlobalsSnapshot externally. Bootstrap writes from
	// inside this package may use the bare map under the convention
	// that bootstrap is single-threaded; new write sites should still
	// prefer SetGlobal so the locking remains uniform.
	globals   map[string]Value
	globalsMu sync.RWMutex // protects globals for concurrent access

	// Well-known classes (for fast-path checks and bootstrapping)
	ObjectClass              *Class
	ClassClass               *Class
	MetaclassClass           *Class
	BlockClass               *Class
	BooleanClass             *Class
	TrueClass                *Class
	FalseClass               *Class
	UndefinedObjectClass     *Class
	MagnitudeClass           *Class
	NumberClass              *Class
	SmallIntegerClass        *Class
	BigIntegerClass          *Class
	FloatClass               *Class
	CollectionClass          *Class
	StringClass              *Class
	SymbolClass              *Class
	ArrayClass               *Class
	ByteArrayClass           *Class
	AssociationClass         *Class
	DictionaryClass          *Class
	SetClass                 *Class
	CompiledMethodClass      *Class
	ChannelClass             *Class
	ProcessClass             *Class
	MutexClass               *Class
	WaitGroupClass           *Class
	SemaphoreClass           *Class
	CancellationContextClass *Class
	ResultClass              *Class
	SuccessClass             *Class
	FailureClass             *Class
	ContextClass             *Class
	WeakReferenceClass       *Class
	CharacterClass           *Class
	MessageClass             *Class
	RandomClass              *Class
	ArrayListClass           *Class

	// Exception hierarchy
	ExceptionClass            *Class
	ErrorClass                *Class
	MessageNotUnderstoodClass *Class
	ZeroDivideClass           *Class
	SubscriptOutOfBoundsClass *Class
	PrimitiveErrorClass       *Class
	TypeErrorClass            *Class
	StackOverflowClass        *Class
	RestrictedGlobalClass     *Class
	WarningClass              *Class
	HaltClass                 *Class
	NotificationClass         *Class

	// Interpreter
	interpreter *Interpreter

	// Goroutine-local interpreter tracking (for forked processes)
	// Maps goroutine ID to the interpreter running in that goroutine
	interpreters     sync.Map // int64 -> *Interpreter
	interpreterCount int32    // number of registered forked interpreters (atomic)

	// Debugger for IDE integration
	Debugger *DebugServer

	// Compiler backend (Go or Maggie)
	compilerBackend CompilerBackend

	// mainProcess is the ProcessObject for the main interpreter goroutine.
	// Created during NewVM so that Process current / Process receive work
	// from the main goroutine.
	mainProcess *ProcessObject

	// Process name registry: registered names → process ID.
	processNames     map[string]uint64
	processNamesMu   sync.RWMutex
	processNamesByID map[uint64]string

	// MailboxMessageClass is bootstrapped for mailbox message instances.
	MailboxMessageClass *Class
	FutureClass         *Class
	NodeClass           *Class
	RemoteProcessClass  *Class
	ClusterMemberClass  *Class

	RemoteChannelClass *Class

	// Remote channel registries
	remoteChannels *remoteChannelRegistry
	channelExports *channelExportRegistry

	// nodeRefs tracks every live NodeRefData so findNodeRefByID can route
	// inbound distributed messages by node public key. NodeRef Values are
	// pointer-carrying kindRemoteRef (Go GC owns their lifetime); this set is a
	// purely functional reverse index, not a liveness root.
	nodeRefs   map[*NodeRefData]struct{}
	nodeRefsMu sync.RWMutex

	// Local node identity keys (loaded lazily; atomic — read on concurrent
	// serializer/deserializer goroutines, written by nodeIdentity()/
	// SetNodeIdentityKeys)
	localIdentity   atomic.Pointer[nodeIdentityHolder]
	localIdentityMu sync.Mutex // serializes lazy generation

	// NodeRefFactory, LocalListenAddr, RemoteChannelFactory now live
	// in lateBoundFields with atomic accessors. Use:
	//   vm.GetNodeRefFactory() / SetNodeRefFactory()
	//   vm.GetLocalListenAddr() / SetLocalListenAddr()
	//   vm.GetRemoteChannelFactory() / SetRemoteChannelFactory()

	// Cross-node monitor/link tracking and health monitoring.
	// healthMonitorMu guards lazy creation of healthMonitor (ensureHealthMonitor
	// is called from interpreter goroutines and server handlers concurrently).
	remoteWatches   *RemoteWatchStore
	healthMonitor   *NodeHealthMonitor
	healthMonitorMu sync.Mutex

	// registry holds all VM-local object registries (concurrency, exceptions,
	// results, contexts, dictionaries, strings, gRPC, HTTP, cells, class vars, etc.).
	registry *ObjectRegistry

	// symbolDispatch centralises marker-based class resolution for symbol-encoded types.
	symbolDispatch *SymbolDispatch

	// AOT compiled methods, fileInFunc, fileInBatchFunc now live in
	// lateBoundFields with atomic CoW publishing. See accessors in
	// late_bound.go and file_in.go.

	// goTypeRegistry maps Go reflect.Types to Maggie classes for GoObject dispatch.
	goTypeRegistry *GoTypeRegistry

	// contentStore indexes compiled methods and class digests by content hash
	// for the distribution protocol.
	contentStore *ContentStore

	// RehydratedClasses, samplingProfiler now live in lateBoundFields
	// (atomic CoW for the map; atomic.Pointer for the profiler).

	// syncRestrictions is the default list of global names to hide when
	// running received code in a sandbox. Populated from the manifest's
	// [sync].capabilities at startup.
	syncRestrictions []string

	// pendingSpawns tracks Futures for forkOn: calls awaiting results from
	// remote nodes. Keyed by futureID embedded in the SpawnBlock.
	pendingSpawns *pendingSpawnRegistry

	// pendingReplies tracks Futures for asyncSend:with: request-response calls
	// awaiting a reply. Keyed by the correlation id carried in the envelope's
	// ReplyAddress; resolved by the inbound __reply__ handler.
	pendingReplies *pendingReplyRegistry

	// clusterCore is the membership handle the Cluster/ClusterMember primitives
	// delegate to (set by the wiring layer; nil = no cluster configured).
	clusterCore ClusterCore

	// dependents maps objects to their dependent lists for the ST-80
	// change/update notification protocol. Uses Value identity (==) as key.
	dependents   map[Value][]Value
	dependentsMu sync.RWMutex

	// cliLastRan records the CliCommandWrapper whose cobra Run callback most
	// recently fired, so executeCliCommand can surface the exit code of whichever
	// subcommand actually ran. Run callbacks execute synchronously on the
	// goroutine that invoked Execute (see the run: primitive), so no lock is
	// needed. This replaces the old linear scan over a per-id registry.
	cliLastRan *CliCommandWrapper

	// Config holds the runtime configuration for this VM instance.
	Config VMConfig
}

// NewVM creates and bootstraps a new VM. An optional VMConfig may be passed;
// if omitted, DefaultVMConfig() is used. Zero-valued fields in a provided
// config fall back to defaults.
func NewVM(configs ...VMConfig) *VM {
	var cfg VMConfig
	if len(configs) > 0 {
		cfg = configs[0].mergeDefaults()
	} else {
		cfg = DefaultVMConfig()
	}

	vm := &VM{
		Config:           cfg,
		Selectors:        NewSelectorTable(),
		Symbols:          NewSymbolTable(),
		Classes:          NewClassTable(),
		Traits:           NewTraitTable(),
		globals:          make(map[string]Value),
		registry:         NewObjectRegistry(),
		symbolDispatch:   NewSymbolDispatch(),
		processNames:     make(map[string]uint64),
		processNamesByID: make(map[uint64]string),
		dependents:       make(map[Value][]Value),
		remoteChannels:   newRemoteChannelRegistry(),
		channelExports:   newChannelExportRegistry(),
		nodeRefs:         make(map[*NodeRefData]struct{}),
		remoteWatches:    NewRemoteWatchStore(),
		pendingSpawns:    newPendingSpawnRegistry(),
		pendingReplies:   newPendingReplyRegistry(),
	}

	// Bootstrap core classes
	vm.bootstrap()

	// Create interpreter
	vm.interpreter = vm.newInterpreter()

	// Claim the main interpreter for the creating goroutine: once forked
	// interpreters exist, currentInterpreter() resolves by goroutine ID, and
	// only THIS goroutine may reach the shared main interpreter (VM-9).
	vm.interpreters.Store(getGoroutineID(), vm.interpreter)

	// Create the main process (so Process current / Process receive work
	// from the main goroutine)
	vm.mainProcess = vm.registry.CreateProcess(vm.Config.MailboxCapacity)
	vm.registry.RegisterProcess(vm.mainProcess)

	// Create debug server
	vm.Debugger = NewDebugServer(vm)

	return vm
}

// newInterpreter creates an interpreter connected to this VM.
// Uses newBareInterpreterWithConfig to allocate with the VM's config values.
func (vm *VM) newInterpreter() *Interpreter {
	interp := newBareInterpreterWithConfig(vm.Config)
	// Share tables with VM
	interp.Selectors = vm.Selectors
	interp.Symbols = vm.Symbols
	interp.Classes = vm.Classes
	interp.globals = vm.globals
	interp.vm = vm // Back-reference for primitives
	// Intern well-known selectors against the VM's table so cached IDs
	// match the IDs used by methods registered on the VM's classes.
	interp.internWellKnownSelectors()
	return interp
}

// newForkedInterpreter creates a forked interpreter with optional restrictions.
// It shares VM tables but writes go to a process-local overlay.
// Pass nil for hidden to create an unrestricted forked interpreter.
// newRequestInterpreter builds an interpreter that shares the VM's tables AND
// its global map — global writes go through globalsMu to the shared map, exactly
// like the main interpreter — but has its own execution stack and frames. The
// private stack is what makes it safe to run on a separate goroutine; unlike
// newForkedInterpreter it is NOT forked, so it does not divert global writes
// into a throwaway process-local overlay. Use it for units of work whose global
// mutations (e.g. Compiler setGlobal:, workspace variables) must persist to the
// shared VM — notably server requests via RunIsolated.
func (vm *VM) newRequestInterpreter() *Interpreter {
	interp := newBareInterpreterWithConfig(vm.Config)
	interp.Selectors = vm.Selectors
	interp.Symbols = vm.Symbols
	interp.Classes = vm.Classes
	interp.globals = vm.globals
	interp.vm = vm
	interp.internWellKnownSelectors()
	return interp
}

func (vm *VM) newForkedInterpreter(hidden map[string]bool) *Interpreter {
	interp := vm.newRequestInterpreter()
	// Forked processes get copy-on-write global isolation: writes land in a
	// process-local overlay (localWrites) rather than the shared map, and
	// `hidden` names are invisible. This is the fork/forkRestricted: semantics.
	interp.forked = true
	interp.hidden = hidden
	return interp
}

// RunIsolated runs fn on a fresh request interpreter registered for the CALLING
// goroutine, then unregisters it. While fn runs, currentInterpreter() (and thus
// vm.Send / vm.Execute) resolves to this per-call interpreter instead of falling
// back to the shared main vm.interpreter — so multiple goroutines can execute
// against one VM concurrently without racing on a single interpreter's
// stack/frames. Exposed for per-request server parallelism (Stage 5).
//
// The interpreter is NOT forked: it shares the VM's global map, so global writes
// (Compiler setGlobal:, workspace variables, class/method definition) persist to
// the shared VM exactly as they did on the pre-Stage-5 main-interpreter path.
// Only the execution stack/frames are private — that is what makes concurrent
// execution safe. (A forked interpreter would divert those writes into a
// throwaway localWrites overlay and silently lose them.)
//
// Do NOT call RunIsolated re-entrantly on the same goroutine: interpreter
// registration is keyed by goroutine id, so a nested call would overwrite then
// delete the outer registration, leaving the outer frame to fall back to the
// shared main interpreter (and unbalance interpreterCount). Server request
// handlers satisfy this — each runs in one gate closure and the helpers they
// call take the *VM directly rather than re-entering RunIsolated.
//
// RunIsolated does NOT serialize callers. Concurrent DISPATCH is race-clean
// (concurrency audit Patches 1-6) and compilation of expression source is safe
// (the backend allocates fresh per-call state, touching only the synchronized
// Selectors/Symbols/registry). Concurrent structural MUTATION of shared VM state
// (defining classes/methods, writing globals) is NOT inherently serialized and
// must be guarded by a separate exclusive gate (see server VMWorker.Do).
func (vm *VM) RunIsolated(fn func()) {
	interp := vm.newRequestInterpreter()
	vm.registerInterpreter(interp)
	defer vm.unregisterInterpreter()
	fn()
}

// inheritedHidden returns the restriction set a fork must run under: a copy of
// the CALLER's hidden globals merged with any extra restrictions requested for
// this fork. Restrictions are inherited so a child process can never widen its
// parent's global view (e.g. a restricted process must not be able to reach
// Compiler via `[ Compiler evaluate: … ] fork`). Returns nil when there is
// nothing to hide.
//
// MUST be called on the forking (caller's) goroutine, BEFORE the fork goroutine
// starts — currentInterpreter() has to resolve to the parent process, not the
// not-yet-registered child.
func (vm *VM) inheritedHidden(extra map[string]bool) map[string]bool {
	var callerHidden map[string]bool
	if caller := vm.currentInterpreter(); caller != nil {
		callerHidden = caller.hidden
	}
	if len(callerHidden) == 0 && len(extra) == 0 {
		return nil
	}
	merged := make(map[string]bool, len(callerHidden)+len(extra))
	for k := range callerHidden {
		merged[k] = true
	}
	for k, v := range extra {
		if v {
			merged[k] = true
		}
	}
	return merged
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

	// Create the Metaclass class (subclass of Class)
	vm.MetaclassClass = vm.createClass("Metaclass", vm.ClassClass)

	// Phase 2: Create behavior classes
	vm.BooleanClass = vm.createClass("Boolean", vm.ObjectClass)
	vm.TrueClass = vm.createClass("True", vm.BooleanClass)
	vm.FalseClass = vm.createClass("False", vm.BooleanClass)
	vm.UndefinedObjectClass = vm.createClass("UndefinedObject", vm.ObjectClass)

	// Phase 3: Create magnitude/number hierarchy
	vm.MagnitudeClass = vm.createClass("Magnitude", vm.ObjectClass)
	vm.NumberClass = vm.createClass("Number", vm.MagnitudeClass)
	vm.SmallIntegerClass = vm.createClass("SmallInteger", vm.NumberClass)
	vm.BigIntegerClass = vm.createClass("BigInteger", vm.NumberClass)
	vm.FloatClass = vm.createClass("Float", vm.NumberClass)

	// Phase 4: Create collection hierarchy
	vm.CollectionClass = vm.createClass("Collection", vm.ObjectClass)
	vm.StringClass = vm.createClass("String", vm.CollectionClass)
	vm.SymbolClass = vm.createClass("Symbol", vm.StringClass)
	vm.ArrayClass = vm.createClass("Array", vm.CollectionClass)
	vm.ByteArrayClass = vm.createClass("ByteArray", vm.CollectionClass)
	vm.AssociationClass = vm.createClassWithIvars("Association", vm.ObjectClass, []string{"key", "value"})
	vm.DictionaryClass = vm.createClassWithIvars("Dictionary", vm.CollectionClass, []string{"table", "size"})
	vm.SetClass = vm.createClassWithIvars("Set", vm.CollectionClass, []string{"dict"})
	vm.ArrayListClass = vm.createClass("ArrayList", vm.CollectionClass)

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

	// GrpcClient/GrpcStream classes are created by the gRPC contrib plugin.

	// Phase 5e: Create Context class for thisContext
	vm.ContextClass = vm.createClass("Context", vm.ObjectClass)

	// Phase 5g: Create WeakReference class
	vm.WeakReferenceClass = vm.createClass("WeakReference", vm.ObjectClass)

	// Phase 5h: Create Character class
	vm.CharacterClass = vm.createClass("Character", vm.ObjectClass)

	// Phase 5i: Create Message class (for doesNotUnderstand:)
	vm.bootstrapMessageClass()

	// Phase 5j: Create Random class
	vm.RandomClass = vm.createClass("Random", vm.ObjectClass)

	// Phase 5f: Create Exception class hierarchy
	vm.bootstrapExceptionClasses()

	// Phase 6: Register primitives on core classes
	vm.registerObjectPrimitives()
	vm.registerBooleanPrimitives()
	vm.registerSmallIntegerPrimitives()
	vm.registerBigIntegerPrimitives()
	vm.registerFloatPrimitives()
	vm.registerSymbolPrimitives()
	vm.registerStringPrimitives()
	vm.registerArrayPrimitives()
	vm.registerBlockPrimitives()
	vm.registerChannelPrimitives()
	vm.registerChannelSelectPrimitives()
	vm.registerProcessPrimitives()
	vm.registerMailboxPrimitives()
	vm.registerLinkMonitorPrimitives()
	vm.registerFuturePrimitives()
	vm.registerNodePrimitives()
	vm.registerRemoteProcessPrimitives()
	vm.registerSpawnPrimitives()
	vm.registerClusterPrimitives()
	vm.registerRemoteChannelPrimitives()
	vm.registerMutexPrimitives()
	vm.registerWaitGroupPrimitives()
	vm.registerSemaphorePrimitives()
	vm.registerCancellationContextPrimitives()
	vm.registerResultPrimitives()
	vm.registerDictionaryPrimitives()
	vm.registerSetPrimitives()
	vm.registerContextPrimitives()
	vm.registerExceptionPrimitives()
	vm.registerExceptionBlockPrimitives()
	vm.registerWeakReferencePrimitives()
	vm.registerCharacterPrimitives()
	vm.registerCliPrimitives()
	vm.registerMessagePrimitives()
	vm.registerClassReflectionPrimitives()
	vm.registerDocstringPrimitives()
	vm.registerCompilerPrimitives()
	vm.registerSandboxPrimitives()
	vm.registerFilePrimitives()
	vm.registerDebuggerPrimitives()
	vm.registerHttpPrimitives()
	vm.registerTomlPrimitives()
	vm.registerExecPrimitives()
	vm.registerUnixSocketPrimitives()
	vm.registerJSONPrimitives()
	// CUE, TupleSpace, ConstraintStore are registered via contrib plugins
	initContribPlugins(vm)
	vm.registerProquintPrimitives()
	vm.registerCryptoPrimitives()
	vm.registerSystemPrimitives()
	vm.registerRandomPrimitives()
	vm.registerArrayListPrimitives()
	vm.registerSignalPrimitives()
	vm.registerRegexPrimitives()
	vm.registerDateTimePrimitives()

	// Phase 7: Set up globals
	vm.globals["Object"] = vm.classValue(vm.ObjectClass)
	vm.globals["Class"] = vm.classValue(vm.ClassClass)
	vm.globals["Metaclass"] = vm.classValue(vm.MetaclassClass)
	vm.globals["Boolean"] = vm.classValue(vm.BooleanClass)
	vm.globals["True"] = vm.classValue(vm.TrueClass)
	vm.globals["False"] = vm.classValue(vm.FalseClass)
	vm.globals["UndefinedObject"] = vm.classValue(vm.UndefinedObjectClass)
	vm.globals["Magnitude"] = vm.classValue(vm.MagnitudeClass)
	vm.globals["Number"] = vm.classValue(vm.NumberClass)
	vm.globals["SmallInteger"] = vm.classValue(vm.SmallIntegerClass)
	vm.globals["BigInteger"] = vm.classValue(vm.BigIntegerClass)
	vm.globals["Float"] = vm.classValue(vm.FloatClass)
	vm.globals["Collection"] = vm.classValue(vm.CollectionClass)
	vm.globals["String"] = vm.classValue(vm.StringClass)
	vm.globals["Symbol"] = vm.classValue(vm.SymbolClass)
	vm.globals["Array"] = vm.classValue(vm.ArrayClass)
	vm.globals["Block"] = vm.classValue(vm.BlockClass)
	vm.globals["Channel"] = vm.classValue(vm.ChannelClass)
	vm.globals["Process"] = vm.classValue(vm.ProcessClass)
	vm.globals["Mutex"] = vm.classValue(vm.MutexClass)
	vm.globals["WaitGroup"] = vm.classValue(vm.WaitGroupClass)
	vm.globals["Semaphore"] = vm.classValue(vm.SemaphoreClass)
	vm.globals["CancellationContext"] = vm.classValue(vm.CancellationContextClass)
	vm.globals["Result"] = vm.classValue(vm.ResultClass)
	vm.globals["Success"] = vm.classValue(vm.SuccessClass)
	vm.globals["Failure"] = vm.classValue(vm.FailureClass)
	vm.globals["Dictionary"] = vm.classValue(vm.DictionaryClass)
	vm.globals["Set"] = vm.classValue(vm.SetClass)
	vm.globals["Context"] = vm.classValue(vm.ContextClass)
	vm.globals["WeakReference"] = vm.classValue(vm.WeakReferenceClass)
	vm.globals["Character"] = vm.classValue(vm.CharacterClass)
	vm.globals["Random"] = vm.classValue(vm.RandomClass)
	vm.globals["ArrayList"] = vm.classValue(vm.ArrayListClass)

	// Well-known symbols
	vm.globals["nil"] = Nil
	vm.globals["true"] = True
	vm.globals["false"] = False

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

	// Concurrency primitives. Process is still a symbol-encoded id (its Values
	// are constructed by-id in monitor/link/distribution paths); the rest are
	// pointer-carrying heap kinds resolved via classForHeap.

	// gRPC symbol dispatch is registered by the gRPC contrib plugin.

	// WeakReference is a pointer-carrying kindWeakRef Value resolved via classForHeap.

	// Characters
	sd.Register(characterMarker, &SymbolTypeEntry{Class: vm.CharacterClass})

}

// classValue returns a Value representing a class.
// Uses the VM-local class value registry to produce a first-class class value.
func (vm *VM) classValue(c *Class) Value {
	return vm.registry.RegisterClassValue(c)
}

// ClassValue returns a first-class Value representing the given class.
// This is the exported version for use by cmd/mag and other packages.
func (vm *VM) ClassValue(c *Class) Value {
	return vm.registry.RegisterClassValue(c)
}

// MetaclassFor returns (or lazily creates) the metaclass for the given class.
// In Smalltalk, every class X has a unique metaclass "X class" such that:
//   - X is the sole instance of "X class"
//   - "X class" has X's ClassVTable as its instance-side VTable
//   - "X class"'s superclass is "X superclass class" (metaclass inheritance
//     mirrors class inheritance)
//   - The class of every metaclass is Metaclass
func (vm *VM) MetaclassFor(c *Class) *Class {
	if c.Metaclass != nil {
		return c.Metaclass
	}

	// Build the metaclass name: "SmallInteger class"
	metaName := c.Name + " class"

	// Determine the metaclass's superclass.
	// The metaclass hierarchy mirrors the class hierarchy:
	//   SmallInteger class -> Object class -> Class
	var metaSuperclass *Class
	if c.Superclass != nil {
		metaSuperclass = vm.MetaclassFor(c.Superclass)
	} else {
		// Object's metaclass has Class as its superclass
		metaSuperclass = vm.ClassClass
	}

	// Create the metaclass. Its VTable delegates to c's ClassVTable
	// so that class-side methods are accessible through the metaclass.
	meta := &Class{
		Name:       metaName,
		Superclass: metaSuperclass,
	}

	// The metaclass's VTable reuses the class's ClassVTable.
	// We need to set the parent chain correctly: the ClassVTable's parent
	// already points to superclass.ClassVTable, which is exactly what we want
	// for metaclass method inheritance.
	meta.VTable = c.ClassVTable

	// The metaclass's ClassVTable (metaclass-of-metaclass methods) inherits
	// from Metaclass's ClassVTable. This allows Metaclass class-side methods
	// to be available on all metaclasses.
	if vm.MetaclassClass != nil {
		meta.ClassVTable = NewVTable(meta, vm.MetaclassClass.ClassVTable)
	} else {
		meta.ClassVTable = NewVTable(meta, nil)
	}

	c.Metaclass = meta
	return meta
}

// GetClassFromValue extracts the *Class from a class value using the
// VM-local registry. Returns nil if v is not a class value or not found.
func (vm *VM) GetClassFromValue(v Value) *Class {
	return vm.registry.GetClassFromValue(v)
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

// --- Channel helpers ---

func (vm *VM) registerChannel(ch *ChannelObject) Value {
	return vm.registry.RegisterChannel(ch)
}

func (vm *VM) getChannel(v Value) *ChannelObject {
	return vm.registry.GetChannel(v)
}

// --- Process helpers ---

func (vm *VM) createProcess() *ProcessObject {
	return vm.registry.CreateProcess(vm.Config.MailboxCapacity)
}

func (vm *VM) registerProcess(proc *ProcessObject) Value {
	return vm.registry.RegisterProcess(proc)
}

func (vm *VM) getProcess(v Value) *ProcessObject {
	return vm.registry.GetProcess(v)
}

// ProcessFromValue returns the ProcessObject a Value carries, or nil if the
// Value is not a process. Exported for the server package, which resolves
// processes returned by LookupProcessName.
func (vm *VM) ProcessFromValue(v Value) *ProcessObject {
	return vm.registry.GetProcess(v)
}

// MainProcessID returns the ID of the main process.
func (vm *VM) MainProcessID() uint64 {
	if vm.mainProcess != nil {
		return vm.mainProcess.id
	}
	return 0
}

// GetProcessByID retrieves a process by its raw uint64 ID.
func (vm *VM) GetProcessByID(id uint64) *ProcessObject {
	return vm.registry.ConcurrencyRegistry.GetProcessByID(id)
}

// currentProcess returns the ProcessObject for the current goroutine.
func (vm *VM) currentProcess() *ProcessObject {
	interp := vm.currentInterpreter()
	if interp == vm.interpreter {
		return vm.mainProcess
	}
	if interp.processID == 0 {
		return nil
	}
	return vm.GetProcessByID(interp.processID)
}

// currentProcessValue returns the Value for the current process.
func (vm *VM) currentProcessValue() Value {
	proc := vm.currentProcess()
	if proc == nil {
		return Nil
	}
	return processToValue(proc)
}

// RegisterProcessName registers a name for a process. Returns false if
// the name is already taken by a live process.
func (vm *VM) RegisterProcessName(name string, procID uint64) bool {
	vm.processNamesMu.Lock()
	defer vm.processNamesMu.Unlock()

	if existingID, ok := vm.processNames[name]; ok {
		proc := vm.GetProcessByID(existingID)
		if proc != nil && !proc.isDone() {
			return false // name taken by live process
		}
		delete(vm.processNamesByID, existingID)
	}
	vm.processNames[name] = procID
	vm.processNamesByID[procID] = name
	return true
}

// LookupProcessName returns the process Value for a registered name.
// Returns Nil if not found or if the process is dead.
func (vm *VM) LookupProcessName(name string) Value {
	vm.processNamesMu.RLock()
	id, ok := vm.processNames[name]
	vm.processNamesMu.RUnlock()
	if !ok {
		return Nil
	}
	proc := vm.GetProcessByID(id)
	if proc == nil || proc.isDone() {
		// Lazy cleanup
		vm.processNamesMu.Lock()
		if vm.processNames[name] == id {
			delete(vm.processNames, name)
			delete(vm.processNamesByID, id)
		}
		vm.processNamesMu.Unlock()
		return Nil
	}
	return processToValue(proc)
}

// UnregisterProcessName removes a name registration.
func (vm *VM) UnregisterProcessName(name string) {
	vm.processNamesMu.Lock()
	if id, ok := vm.processNames[name]; ok {
		delete(vm.processNames, name)
		delete(vm.processNamesByID, id)
	}
	vm.processNamesMu.Unlock()
}

// UnregisterProcessNamesFor removes any name registered for a process id.
// Called from FinishProcess so the name registry stays bounded by live
// processes.
func (vm *VM) UnregisterProcessNamesFor(procID uint64) {
	vm.processNamesMu.Lock()
	if name, ok := vm.processNamesByID[procID]; ok {
		delete(vm.processNames, name)
		delete(vm.processNamesByID, procID)
	}
	vm.processNamesMu.Unlock()
}

// MailboxMessage slot layout. Slots 0-2 (sender/selector/payload) have public
// accessors; slots 3-4 hold the request-response reply address and are read only
// by MailboxMessage>>reply:/isRequest (no accessor — effectively hidden).
const (
	mailboxSlotSender      = 0
	mailboxSlotSelector    = 1
	mailboxSlotPayload     = 2
	mailboxSlotReplyNode   = 3 // String of the requester's raw 32-byte node id, or Nil
	mailboxSlotCorrelation = 4 // SmallInt correlation id, or Nil
	mailboxNumSlots        = 5
)

// CreateMailboxMessage creates a fire-and-forget MailboxMessage (no reply address).
func (vm *VM) CreateMailboxMessage(sender Value, selector string, payload Value) Value {
	var zero [32]byte
	return vm.CreateMailboxMessageWithReply(sender, selector, payload, zero, 0)
}

// CreateMailboxMessageWithReply creates a MailboxMessage carrying an optional
// reply address. A non-zero correlation marks the message as a request the
// receiver can answer with `msg reply:`; replyNode is the requester's node id.
func (vm *VM) CreateMailboxMessageWithReply(sender Value, selector string, payload Value, replyNode [32]byte, correlation uint64) Value {
	if vm.MailboxMessageClass == nil {
		return payload // fallback if class not bootstrapped
	}
	instance := NewObject(vm.MailboxMessageClass.VTable, mailboxNumSlots)
	instance.SetSlot(mailboxSlotSender, sender)
	if selector != "" {
		instance.SetSlot(mailboxSlotSelector, vm.Symbols.SymbolValue(selector))
	} else {
		instance.SetSlot(mailboxSlotSelector, Nil)
	}
	instance.SetSlot(mailboxSlotPayload, payload)
	if correlation != 0 {
		instance.SetSlot(mailboxSlotReplyNode, vm.registry.NewStringValue(string(replyNode[:])))
		instance.SetSlot(mailboxSlotCorrelation, FromSmallInt(int64(correlation)))
	} else {
		instance.SetSlot(mailboxSlotReplyNode, Nil)
		instance.SetSlot(mailboxSlotCorrelation, Nil)
	}
	return instance.ToValue()
}

// --- ArrayList helpers ---

func (vm *VM) registerArrayList(al *ArrayListObject) Value {
	return vm.registry.RegisterArrayList(al)
}

func (vm *VM) getArrayList(v Value) *ArrayListObject {
	return vm.registry.GetArrayList(v)
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
	// If v is a class value, return its metaclass
	if isClassValue(v) {
		cls := vm.registry.GetClassFromValue(v)
		if cls != nil {
			meta := vm.MetaclassFor(cls)
			return vm.classValue(meta)
		}
	}
	// For instances, return the class as a first-class class value
	cls := vm.ClassFor(v)
	if cls != nil {
		return vm.classValue(cls)
	}
	return vm.classValue(vm.ObjectClass)
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
	case v.IsSymbolEncoded():
		// Use the central dispatch table for all symbol-encoded types
		if cls, isClassSide := vm.symbolDispatch.ClassForSymbolVM(v, vm); cls != nil {
			if isClassSide {
				// For class values, return the metaclass (lazily created)
				return vm.MetaclassFor(cls)
			}
			return cls
		}
		return vm.SymbolClass
	case v.isHeap():
		return vm.classForHeap(v)
	default:
		return vm.ObjectClass
	}
}

// classForHeap resolves the class of a pointer-carrying heap Value by its kind
// tag. As types migrate from symbol-encoded registry ids to pointers, their
// class resolution moves here from the symbol dispatch table.
func (vm *VM) classForHeap(v Value) *Class {
	switch v.heapKindOf() {
	case kindObject:
		obj := ObjectFromValue(v)
		if obj != nil {
			vt := obj.VTablePtr()
			if vt != nil && vt.Class() != nil {
				return vt.Class()
			}
		}
		return vm.ObjectClass
	case kindResult:
		r := vm.registry.GetResultFromValue(v)
		if r != nil && r.resultType == ResultSuccess {
			return vm.SuccessClass
		}
		return vm.FailureClass
	case kindString:
		return vm.StringClass
	case kindDictionary:
		return vm.DictionaryClass
	case kindArrayList:
		return vm.ArrayListClass
	case kindClassValue:
		// The class OF a class value is that class's metaclass; the metaclass's
		// instance-side VTable is the class's ClassVTable, so class-side sends
		// dispatch correctly.
		if c := (*Class)(v.ptr); c != nil {
			return vm.MetaclassFor(c)
		}
		return vm.ObjectClass
	case kindBigInt:
		return vm.BigIntegerClass
	case kindException:
		ex := vm.registry.GetExceptionFromValue(v)
		if ex != nil && ex.ExceptionClass != nil {
			return ex.ExceptionClass
		}
		return vm.ExceptionClass
	case kindGoObject:
		if cls := vm.GoObjectClass(v); cls != nil {
			return cls
		}
		return vm.ObjectClass
	case kindMutex:
		return vm.MutexClass
	case kindWaitGroup:
		return vm.WaitGroupClass
	case kindSemaphore:
		return vm.SemaphoreClass
	case kindCancellationContext:
		return vm.CancellationContextClass
	case kindChannel:
		return vm.ChannelClass
	case kindProcess:
		return vm.ProcessClass
	case kindFuture:
		return vm.FutureClass
	case kindRemoteChannel:
		return vm.RemoteChannelClass
	case kindRemoteRef:
		return vm.NodeClass
	case kindWeakRef:
		return vm.WeakReferenceClass
	case kindExtension:
		// Contrib/IO wrapper types share kindExtension; the concrete class is
		// the one registered for the wrapper's marker in symbolDispatch.
		if e := v.extensionObjectOf(); e != nil {
			if cls, _ := vm.symbolDispatch.ClassForMarkerVM(e.marker, vm); cls != nil {
				return cls
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

// DebugCallStack returns the current call stack from the main interpreter.
// Safe to call from another goroutine while the interpreter is paused.
func (vm *VM) DebugCallStack() []StackFrame {
	if vm.Debugger == nil {
		return nil
	}
	return vm.Debugger.GetCallStack(vm.interpreter)
}

// DebugVariables returns the variables visible in the given stack frame.
// Safe to call from another goroutine while the interpreter is paused.
func (vm *VM) DebugVariables(frameID int) []Variable {
	if vm.Debugger == nil {
		return nil
	}
	return vm.Debugger.GetVariables(vm.interpreter, frameID)
}

// Execute runs a compiled method with the given receiver and arguments.
//
// Routes through currentInterpreter() so callers from forked goroutines
// use their own interpreter instead of mutating the main interpreter's
// stack/frames concurrently. Pre-Patch-5 this routed through
// vm.interpreter unconditionally and only worked because the dispatch
// queue serialised external Go callers — see Patch 5 in
// docs/vm-concurrency-audit-2026-05-03.md.
func (vm *VM) Execute(method *CompiledMethod, receiver Value, args []Value) Value {
	return vm.currentInterpreter().Execute(method, receiver, args)
}

// ExecuteSafe runs a compiled method and catches Maggie errors, returning them as Go errors.
// This prevents Go panic stack traces from being shown for Maggie-level errors.
// Set MAGGIE_DEBUG=1 to see full Go stack traces.
func (vm *VM) ExecuteSafe(method *CompiledMethod, receiver Value, args []Value) (result Value, err error) {
	// Snapshot the interpreter's stack pointers so that if Execute unwinds via
	// panic (an unhandled Maggie exception), we can restore them in the
	// recover below. Otherwise the frames/operands pushed during the failed
	// Execute are abandoned and every subsequent ExecuteSafe on this shared
	// interpreter (REPL, doctest runner, LSP, docserve) builds on top of the
	// stale frames — corrupting stack traces and eventually exhausting the
	// frame budget with a spurious StackOverflow.
	interp := vm.currentInterpreter()
	savedFP, savedSP := interp.fp, interp.sp
	defer func() {
		if r := recover(); r != nil {
			interp.fp, interp.sp = savedFP, savedSP
			interp.unwinding = false
			interp.unwindValue = Nil
			if msg, ok := FormatUnhandledPanic(vm, r); ok {
				err = fmt.Errorf("%s", msg)
				result = Nil
				return
			}
			// Not a Maggie error, re-panic
			panic(r)
		}
	}()
	result = interp.Execute(method, receiver, args)
	return result, nil
}

// FormatUnhandledPanic renders a recovered panic value as a human-readable
// Maggie error message. It handles the two ways an uncaught Maggie error
// propagates: a "Maggie error: "-prefixed string panic, and a SignaledException
// carrying the exception object. The returned ok is false for any other panic
// (a genuine Go bug), which the caller should re-panic.
func FormatUnhandledPanic(vm *VM, r interface{}) (msg string, ok bool) {
	if s, isStr := r.(string); isStr && len(s) > 14 && s[:14] == "Maggie error: " {
		// Already includes the stack trace; strip the internal prefix.
		return s[14:], true
	}
	if sigEx, isSig := r.(SignaledException); isSig {
		msg = "unhandled exception"
		if sigEx.Object != nil {
			className := ""
			if sigEx.Object.ExceptionClass != nil {
				className = sigEx.Object.ExceptionClass.Name
			}
			if IsStringValue(sigEx.Object.MessageText) {
				msg = vm.registry.GetStringContent(sigEx.Object.MessageText)
			}
			if className != "" {
				msg = className + ": " + msg
			}
			// Append the captured stack trace if available so callers see
			// where the exception was raised, not just what.
			if len(sigEx.Object.CapturedFrames) > 0 {
				msg = msg + "\n" + FormatCapturedTrace(sigEx.Object.CapturedFrames)
			}
		}
		return msg, true
	}
	return "", false
}

// GetProfiler returns the VM's profiler for inspecting hot code detection.
func (vm *VM) GetProfiler() *Profiler {
	return vm.interpreter.Profiler
}

// EnableProfiler creates and attaches a Profiler to the VM's interpreter.
// The profiler is nil by default; call this to start recording invocation
// counts for hot-code detection.
func (vm *VM) EnableProfiler() {
	if vm.interpreter.Profiler == nil {
		vm.interpreter.Profiler = NewProfiler()
	}
}

// Send sends a message to a receiver.
func (vm *VM) Send(receiver Value, selector string, args []Value) Value {
	selectorID := vm.Selectors.Intern(selector)

	// Determine the class for method dispatch
	var class *Class
	isClassSide := false
	if receiver.IsSymbolEncoded() {
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
	} else if isClassValue(receiver) {
		// A class value dispatches class-side via the class's ClassVTable —
		// without materializing a metaclass on this hot path (MetaclassFor is
		// reserved for the cold "class of a class" reflection path in ClassFor).
		class = vm.registry.GetClassFromValue(receiver)
		isClassSide = true
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
		return vm.currentInterpreter().Execute(cm, receiver, args)
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

// Global returns a global value by name. Safe for concurrent use.
func (vm *VM) Global(name string) (Value, bool) {
	vm.globalsMu.RLock()
	v, ok := vm.globals[name]
	vm.globalsMu.RUnlock()
	return v, ok
}

// LookupGlobal is a legacy alias for Global. Prefer Global in new code.
func (vm *VM) LookupGlobal(name string) (Value, bool) {
	return vm.Global(name)
}

// MustGlobal returns a global by name; returns Nil if not present.
// Use only when the caller has already verified existence (e.g.,
// during bootstrap or in tests with known fixtures).
func (vm *VM) MustGlobal(name string) Value {
	v, _ := vm.Global(name)
	return v
}

// HasGlobal reports whether a global with the given name exists.
func (vm *VM) HasGlobal(name string) bool {
	_, ok := vm.Global(name)
	return ok
}

// SetGlobal sets a global value. Safe for concurrent use.
func (vm *VM) SetGlobal(name string, value Value) {
	vm.globalsMu.Lock()
	vm.globals[name] = value
	vm.globalsMu.Unlock()
}

// DeleteGlobal removes a global by name. No-op if absent.
func (vm *VM) DeleteGlobal(name string) {
	vm.globalsMu.Lock()
	delete(vm.globals, name)
	vm.globalsMu.Unlock()
}

// GlobalsLen returns the number of globals.
func (vm *VM) GlobalsLen() int {
	vm.globalsMu.RLock()
	n := len(vm.globals)
	vm.globalsMu.RUnlock()
	return n
}

// RangeGlobals iterates over all globals, calling fn for each
// (name, value) pair. If fn returns false, iteration stops. The map is
// held under read-lock for the duration of the call — fn must not
// SetGlobal or otherwise re-enter the VM globals API.
func (vm *VM) RangeGlobals(fn func(name string, value Value) bool) {
	vm.globalsMu.RLock()
	defer vm.globalsMu.RUnlock()
	for name, v := range vm.globals {
		if !fn(name, v) {
			return
		}
	}
}

// GlobalsSnapshot returns a copy of the current globals map. Safe to
// iterate without holding the VM globals lock.
func (vm *VM) GlobalsSnapshot() map[string]Value {
	vm.globalsMu.RLock()
	defer vm.globalsMu.RUnlock()
	out := make(map[string]Value, len(vm.globals))
	for k, v := range vm.globals {
		out[k] = v
	}
	return out
}

// LookupClass returns a class by name.
func (vm *VM) LookupClass(name string) *Class {
	return vm.Classes.Lookup(name)
}

// ContentStore returns the VM's content store, creating it lazily if needed.
func (vm *VM) ContentStore() *ContentStore {
	if vm.contentStore == nil {
		vm.contentStore = NewContentStore()
	}
	return vm.contentStore
}

// SetContentStore replaces the content store. Used by tests.
func (vm *VM) SetContentStore(cs *ContentStore) {
	vm.contentStore = cs
}

// SetSyncRestrictions sets the default restriction list for sandboxed execution
// of received code. Typically called at startup from the manifest config.
func (vm *VM) SetSyncRestrictions(names []string) {
	vm.syncRestrictions = names
}

// SyncRestrictions returns the default restriction list for sandboxed execution.
func (vm *VM) SyncRestrictions() []string {
	return vm.syncRestrictions
}

// MarkRehydrated records a class name as having been installed via the
// sync/rehydration pipeline (received from the network).
func (vm *VM) MarkRehydrated(className string) {
	vm.markRehydrated(className)
}

// IsRehydrated returns true if the class was installed via rehydration.
func (vm *VM) IsRehydrated(className string) bool {
	return vm.isRehydrated(className)
}

// GoTypeRegistry returns the VM's Go type registry, creating it if needed.
func (vm *VM) GoTypeRegistry() *GoTypeRegistry {
	if vm.goTypeRegistry == nil {
		vm.goTypeRegistry = NewGoTypeRegistry()
	}
	return vm.goTypeRegistry
}

// ---------------------------------------------------------------------------
// Weak References
// ---------------------------------------------------------------------------

// NewWeakRef creates a new weak reference to the given object. The reference is
// a Go-native weak pointer (weak.Pointer); Go's GC clears it — there is no VM
// registry to register with.
func (vm *VM) NewWeakRef(target *Object) *WeakReference {
	return NewWeakReference(target)
}

// ---------------------------------------------------------------------------
// Goroutine-Local Interpreter Tracking
// ---------------------------------------------------------------------------

// getGoroutineID returns the current goroutine's ID via the
// petermattis/goid trampoline. The previous implementation used
// runtime.Stack(buf, false) which costs ~10µs per call and acquires a
// runtime-internal lock that effectively serialised dispatch across
// goroutines — masking the IC/Globals/late-bound races documented in
// docs/vm-concurrency-audit-2026-05-03.md. With Patches 1-6 in place
// those races are fixed, so we can re-land the fast lookup.
func getGoroutineID() int64 {
	return goid.Get()
}

// registerInterpreter registers an interpreter for the current goroutine.
// Call this when starting execution in a new goroutine (e.g., forked process).
func (vm *VM) registerInterpreter(interp *Interpreter) {
	gid := getGoroutineID()
	vm.interpreters.Store(gid, interp)
	atomic.AddInt32(&vm.interpreterCount, 1)
}

// unregisterInterpreter removes the interpreter for the current goroutine.
// Call this when execution is complete.
func (vm *VM) unregisterInterpreter() {
	gid := getGoroutineID()
	vm.interpreters.Delete(gid)
	atomic.AddInt32(&vm.interpreterCount, -1)
}

// currentInterpreter returns the interpreter for the current goroutine.
// Fast path: when no forked interpreters exist, returns the main interpreter
// directly (avoiding the extremely expensive getGoroutineID/runtime.Stack).
// Slow path: looks up by goroutine ID for multi-goroutine programs.
func (vm *VM) currentInterpreter() *Interpreter {
	// Fast path: no forked goroutines, must be main interpreter
	if atomic.LoadInt32(&vm.interpreterCount) == 0 {
		return vm.interpreter
	}
	// Slow path: look up by goroutine ID (the NewVM goroutine is registered
	// at creation, so the main program resolves here too)
	gid := getGoroutineID()
	if interp, ok := vm.interpreters.Load(gid); ok {
		return interp.(*Interpreter)
	}
	// Unregistered goroutine (Go timer/callback invoking vm.Send, …). The
	// old fallback handed out the SHARED main interpreter — two goroutines
	// mutating one frame stack, silent corruption. Hand out a fresh isolated
	// interpreter instead: utility sends work, and concurrent misuse can no
	// longer corrupt the main program's stack (VM-9).
	return vm.newRequestInterpreter()
}

// ---------------------------------------------------------------------------
// VM Lifecycle
// ---------------------------------------------------------------------------

// Shutdown stops background goroutines and releases resources. Call this
// when the VM is no longer needed.
func (vm *VM) Shutdown() {
	if sp := vm.SamplingProfiler(); sp != nil {
		sp.Stop()
	}
	vm.healthMonitorMu.Lock()
	if vm.healthMonitor != nil {
		vm.healthMonitor.Stop()
	}
	vm.healthMonitorMu.Unlock()
}

// StartSamplingProfiler creates and starts a wall-clock sampling profiler.
// If one is already running, it is stopped first.
func (vm *VM) StartSamplingProfiler(interval time.Duration) *SamplingProfiler {
	if old := vm.SamplingProfiler(); old != nil {
		old.Stop()
	}
	sp := NewSamplingProfiler(vm, interval)
	vm.setSamplingProfiler(sp)
	sp.Start()
	return sp
}

// StopSamplingProfiler stops the sampling profiler and returns it
// (so callers can write output). Returns nil if no profiler was running.
func (vm *VM) StopSamplingProfiler() *SamplingProfiler {
	sp := vm.SamplingProfiler()
	if sp != nil {
		sp.Stop()
		vm.setSamplingProfiler(nil)
	}
	return sp
}
