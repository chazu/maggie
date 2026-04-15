package vm

import (
	"fmt"
	"sync"
	"sync/atomic"

	"github.com/fxamacker/cbor/v2"
)

// SelectorSpawnResult is the infrastructure selector for delivering spawn
// results back to the spawning node. Not a user-visible message.
const SelectorSpawnResult = "__spawn_result__"

// ---------------------------------------------------------------------------
// SpawnBlock: serialized block for remote execution
// ---------------------------------------------------------------------------

// SpawnBlock is the wire format for a block being sent to a remote node
// for execution. It carries the block's compiled method (by content hash),
// serialized captured variables, and metadata.
type SpawnBlock struct {
	MethodHash   [32]byte `cbor:"1,keyasint"`           // content hash of the block's compiled method
	TypedHash    [32]byte `cbor:"2,keyasint,omitempty"`  // typed hash (for ContentStore lookup)
	ArgCount     int      `cbor:"3,keyasint"`            // block arity
	TempCount    int      `cbor:"4,keyasint"`            // number of temporaries
	NumCaptures  int      `cbor:"5,keyasint"`            // number of captured variables
	Upvalues     [][]byte `cbor:"6,keyasint"`            // serialized captured values (positional)
	SourceClass  string   `cbor:"7,keyasint,omitempty"`  // FQN of enclosing class
	SourceMethod string   `cbor:"8,keyasint,omitempty"`  // selector of enclosing method (diagnostic)
	Arg          []byte   `cbor:"9,keyasint,omitempty"`  // optional argument for forkOn:with:
	FutureID     uint64   `cbor:"10,keyasint,omitempty"` // for result delivery back to spawner
	SpawnMode    string   `cbor:"11,keyasint"`           // "fork" (returns Future) or "spawn" (long-lived)
}

// ---------------------------------------------------------------------------
// Serialization
// ---------------------------------------------------------------------------

// SerializeBlock serializes a block value for remote execution. It encodes
// the block's method content hash, captured variables, and optional argument.
// Returns an error if any captured variable is non-serializable.
func (vm *VM) SerializeBlock(bv *BlockValue, arg Value, mode string) ([]byte, error) {
	if bv == nil || bv.Block == nil {
		return nil, fmt.Errorf("spawn: nil block")
	}

	block := bv.Block

	// The block's enclosing method must have a content hash
	var methodHash, typedHash [32]byte
	if bv.HomeMethod != nil {
		methodHash = bv.HomeMethod.GetContentHash()
		typedHash = bv.HomeMethod.GetTypedHash()
	}
	if block.Outer != nil && methodHash == ([32]byte{}) {
		methodHash = block.Outer.GetContentHash()
		typedHash = block.Outer.GetTypedHash()
	}
	if methodHash == ([32]byte{}) {
		return nil, fmt.Errorf("spawn: block's enclosing method has no content hash (was it compiled from source?)")
	}

	// Serialize captured variables eagerly — fail fast on non-serializable types
	upvalues := make([][]byte, len(bv.Captures))
	for i, cap := range bv.Captures {
		data, err := vm.SerializeValue(cap)
		if err != nil {
			return nil, fmt.Errorf("spawn: captured variable %d: %w", i, err)
		}
		upvalues[i] = data
	}

	// Serialize the argument if provided
	var argBytes []byte
	if !arg.IsNil() {
		var err error
		argBytes, err = vm.SerializeValue(arg)
		if err != nil {
			return nil, fmt.Errorf("spawn: argument: %w", err)
		}
	}

	// Build source context for diagnostics
	var sourceClass, sourceMethod string
	if block.Outer != nil {
		if c := block.Outer.Class(); c != nil {
			sourceClass = c.Name
			if c.Namespace != "" {
				sourceClass = c.Namespace + "::" + c.Name
			}
		}
		sourceMethod = block.Outer.Name()
	}

	sb := &SpawnBlock{
		MethodHash:   methodHash,
		TypedHash:    typedHash,
		ArgCount:     block.Arity,
		TempCount:    block.NumTemps,
		NumCaptures:  block.NumCaptures,
		Upvalues:     upvalues,
		SourceClass:  sourceClass,
		SourceMethod: sourceMethod,
		Arg:          argBytes,
		SpawnMode:    mode,
	}

	return cborSerialEncMode.Marshal(cbor.Tag{Number: cborTagSpawnBlock, Content: sb})
}

// DeserializeSpawnBlock decodes a CBOR-encoded SpawnBlock.
func DeserializeSpawnBlock(data []byte) (*SpawnBlock, error) {
	var raw interface{}
	if err := cbor.Unmarshal(data, &raw); err != nil {
		return nil, fmt.Errorf("spawn: CBOR decode: %w", err)
	}

	tag, ok := raw.(cbor.Tag)
	if !ok || tag.Number != cborTagSpawnBlock {
		return nil, fmt.Errorf("spawn: expected SpawnBlock tag (27013), got %T", raw)
	}

	// Re-marshal content for typed unmarshal
	contentBytes, err := cborSerialEncMode.Marshal(tag.Content)
	if err != nil {
		return nil, fmt.Errorf("spawn: re-encode: %w", err)
	}

	var sb SpawnBlock
	if err := cbor.Unmarshal(contentBytes, &sb); err != nil {
		return nil, fmt.Errorf("spawn: decode SpawnBlock: %w", err)
	}

	return &sb, nil
}

// DeserializeUpvalues decodes the upvalues from a SpawnBlock into Values.
func (vm *VM) DeserializeUpvalues(sb *SpawnBlock) ([]Value, error) {
	captures := make([]Value, len(sb.Upvalues))
	for i, data := range sb.Upvalues {
		val, err := vm.DeserializeValue(data)
		if err != nil {
			return nil, fmt.Errorf("spawn: upvalue %d: %w", i, err)
		}
		captures[i] = val
	}
	return captures, nil
}

// DeserializeSpawnArg decodes the optional argument from a SpawnBlock.
func (vm *VM) DeserializeSpawnArg(sb *SpawnBlock) (Value, error) {
	if len(sb.Arg) == 0 {
		return Nil, nil
	}
	return vm.DeserializeValue(sb.Arg)
}

// ---------------------------------------------------------------------------
// Block method resolution: find the BlockMethod by content hash
// ---------------------------------------------------------------------------

// ResolveBlockMethod looks up the enclosing CompiledMethod by content hash
// and returns its first BlockMethod (the block being spawned). If the method
// is not in the ContentStore, returns (nil, nil) to signal code-on-demand.
func (vm *VM) ResolveBlockMethod(sb *SpawnBlock) (*BlockMethod, *CompiledMethod, error) {
	cs := vm.ContentStore()
	if cs == nil {
		return nil, nil, fmt.Errorf("spawn: no content store")
	}

	method := cs.LookupMethod(sb.MethodHash)
	if method == nil {
		// Try typed hash
		method = cs.LookupMethodByTypedHash(sb.TypedHash)
	}
	if method == nil {
		return nil, nil, nil // signal caller to do code-on-demand
	}

	if len(method.Blocks) == 0 {
		return nil, nil, fmt.Errorf("spawn: method %q has no blocks", method.Name())
	}

	// Find the matching block by arity and capture count
	for _, block := range method.Blocks {
		if block.Arity == sb.ArgCount && block.NumCaptures == sb.NumCaptures {
			return block, method, nil
		}
	}

	// Fallback: use the first block (most common case — one block per method)
	return method.Blocks[0], method, nil
}

// ---------------------------------------------------------------------------
// Pending spawn registry: maps futureID → FutureObject for result delivery
// ---------------------------------------------------------------------------

type pendingSpawnRegistry struct {
	mu       sync.RWMutex
	spawns   map[uint64]*FutureObject
	nextID   atomic.Uint64
}

func newPendingSpawnRegistry() *pendingSpawnRegistry {
	return &pendingSpawnRegistry{
		spawns: make(map[uint64]*FutureObject),
	}
}

func (r *pendingSpawnRegistry) register(f *FutureObject) uint64 {
	id := r.nextID.Add(1)
	r.mu.Lock()
	r.spawns[id] = f
	r.mu.Unlock()
	return id
}

func (r *pendingSpawnRegistry) resolve(id uint64) *FutureObject {
	r.mu.Lock()
	f := r.spawns[id]
	delete(r.spawns, id)
	r.mu.Unlock()
	return f
}

// ---------------------------------------------------------------------------
// SpawnFunc callback type (injected from cmd/mag to avoid import cycle)
// ---------------------------------------------------------------------------

// SpawnFunc sends a SpawnProcess RPC to a remote node. Returns (processName, error).
type SpawnFunc func(spawnBlockBytes []byte) (string, error)

// ---------------------------------------------------------------------------
// Spawn primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerSpawnPrimitives() {
	// Block>>primForkOn: node — returns Future
	vm.BlockClass.AddMethod1(vm.Selectors, "primForkOn:", func(vmPtr interface{}, recv, nodeVal Value) Value {
		v := vmPtr.(*VM)
		return v.doRemoteSpawn(recv, nodeVal, Nil, "fork")
	})

	// Block>>primForkOn:with: node arg — returns Future
	vm.BlockClass.AddMethod2(vm.Selectors, "primForkOn:with:", func(vmPtr interface{}, recv, nodeVal, argVal Value) Value {
		v := vmPtr.(*VM)
		return v.doRemoteSpawn(recv, nodeVal, argVal, "fork")
	})

	// Block>>primSpawnOn: node — returns RemoteProcess
	vm.BlockClass.AddMethod1(vm.Selectors, "primSpawnOn:", func(vmPtr interface{}, recv, nodeVal Value) Value {
		v := vmPtr.(*VM)
		return v.doRemoteSpawn(recv, nodeVal, Nil, "spawn")
	})

	// Block>>primSpawnOn:with: node arg — returns RemoteProcess
	vm.BlockClass.AddMethod2(vm.Selectors, "primSpawnOn:with:", func(vmPtr interface{}, recv, nodeVal, argVal Value) Value {
		v := vmPtr.(*VM)
		return v.doRemoteSpawn(recv, nodeVal, argVal, "spawn")
	})
}

func (vm *VM) doRemoteSpawn(blockVal, nodeVal, argVal Value, mode string) Value {
	bv := vm.currentInterpreter().getBlockValue(blockVal)
	if bv == nil {
		return Nil
	}

	ref := vm.getNodeRef(nodeVal)
	if ref == nil || ref.SpawnFunc == nil {
		return Nil
	}

	// Serialize the block
	spawnBytes, err := vm.SerializeBlock(bv, argVal, mode)
	if err != nil {
		// TODO: raise Maggie exception once we have a clean error path
		fmt.Printf("spawn error: %v\n", err)
		return Nil
	}

	if mode == "fork" {
		return vm.doForkOn(ref, spawnBytes, nodeVal)
	}
	return vm.doSpawnOn(ref, spawnBytes, nodeVal)
}

func (vm *VM) doForkOn(ref *NodeRefData, spawnBytes []byte, nodeVal Value) Value {
	// Create a local Future for the result
	future := NewFuture()
	futureVal := vm.registerFuture(future)
	futureID := vm.pendingSpawns.register(future)

	// Embed futureID into the spawn block for result delivery
	sb, err := DeserializeSpawnBlock(spawnBytes)
	if err != nil {
		future.ResolveError(fmt.Sprintf("spawn: %v", err))
		return futureVal
	}
	sb.FutureID = futureID
	spawnBytes, err = cborSerialEncMode.Marshal(cbor.Tag{Number: cborTagSpawnBlock, Content: sb})
	if err != nil {
		future.ResolveError(fmt.Sprintf("spawn: re-encode: %v", err))
		return futureVal
	}

	go func() {
		processName, err := ref.SpawnFunc(spawnBytes)
		if err != nil {
			future.ResolveError(fmt.Sprintf("spawn RPC: %v", err))
			return
		}
		_ = processName // result comes via DeliverMessage when block completes
	}()

	return futureVal
}

func (vm *VM) doSpawnOn(ref *NodeRefData, spawnBytes []byte, nodeVal Value) Value {
	processName, err := ref.SpawnFunc(spawnBytes)
	if err != nil {
		return Nil
	}
	return vm.createRemoteProcess(nodeVal, processName)
}

// ResolvePendingSpawn looks up and removes a pending spawn Future by ID.
// Called when a spawn result is delivered back to the spawning node.
func (vm *VM) ResolvePendingSpawn(futureID uint64) *FutureObject {
	return vm.pendingSpawns.resolve(futureID)
}

// ---------------------------------------------------------------------------
// Server-side spawn execution
// ---------------------------------------------------------------------------

// ExecuteSpawnBlock deserializes and executes a SpawnBlock. It resolves the
// block's method by content hash, deserializes upvalues, creates a restricted
// interpreter, and runs the block in a new goroutine. Returns the process
// name for RemoteProcess references.
//
// pullFunc is called if the method is not in the local ContentStore. It should
// pull the method (and transitive deps) from the spawning node.
func (vm *VM) ExecuteSpawnBlock(sb *SpawnBlock, restrictions []string, pullFunc func(hash [32]byte) error) (string, error) {
	// Resolve the block method
	block, homeMethod, err := vm.ResolveBlockMethod(sb)
	if err != nil {
		return "", err
	}

	// Code-on-demand: pull if not found locally
	if block == nil && pullFunc != nil {
		if err := pullFunc(sb.MethodHash); err != nil {
			return "", fmt.Errorf("spawn: code-on-demand pull failed: %w", err)
		}
		// Retry after pull
		block, homeMethod, err = vm.ResolveBlockMethod(sb)
		if err != nil {
			return "", err
		}
	}
	if block == nil {
		return "", fmt.Errorf("spawn: method %x not found (even after pull)", sb.MethodHash[:8])
	}

	// Deserialize upvalues
	captures, err := vm.DeserializeUpvalues(sb)
	if err != nil {
		return "", err
	}

	// Deserialize argument
	arg, err := vm.DeserializeSpawnArg(sb)
	if err != nil {
		return "", err
	}

	// Build hidden map from spawn restrictions
	var hidden map[string]bool
	if len(restrictions) > 0 {
		hidden = make(map[string]bool, len(restrictions))
		for _, name := range restrictions {
			hidden[name] = true
		}
	}

	// Create process
	proc := vm.createProcess()
	vm.registerProcess(proc)

	// Auto-generate a name and register
	procName := fmt.Sprintf("_remote_%x_%d", sb.MethodHash[:4], proc.id)
	vm.RegisterProcessName(procName, proc.id)

	// Run the block in a new goroutine
	go func() {
		defer func() {
			if r := recover(); r != nil {
				switch ex := r.(type) {
				case NonLocalReturn:
					vm.FinishProcess(proc, ExitNormal(ex.Value))
				case SignaledException:
					vm.FinishProcess(proc, ExitException(
						fmt.Errorf("spawn exception: %v", r),
						ex.Exception,
					))
				default:
					vm.FinishProcess(proc, ExitError(fmt.Errorf("spawn panic: %v", r)))
				}
			}
			vm.unregisterInterpreter()
		}()

		interp := vm.newForkedInterpreter(hidden)
		interp.processID = proc.id
		vm.registerInterpreter(interp)

		var result Value
		if arg.IsNil() || len(sb.Arg) == 0 {
			result = interp.ExecuteBlockDetached(block, captures, nil, Nil, homeMethod)
		} else {
			result = interp.ExecuteBlockDetached(block, captures, []Value{arg}, Nil, homeMethod)
		}
		vm.FinishProcess(proc, ExitNormal(result))
	}()

	return procName, nil
}
