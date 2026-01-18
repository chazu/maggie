package runtime

import (
	"fmt"
	"sync"
	"sync/atomic"

	"github.com/chazu/procyon/pkg/bytecode"
)

// CaptureCell holds a captured variable with reference semantics
type CaptureCell struct {
	Value      Value
	Name       string
	Source     int // 0=local, 1=param, 2=ivar, 3=capture
	InstanceID string
	Closed     bool
}

// Get returns the current value
func (c *CaptureCell) Get() Value {
	if c == nil {
		return NilValue()
	}
	return c.Value
}

// Set updates the captured value
func (c *CaptureCell) Set(v Value) {
	if c != nil {
		c.Value = v
	}
}

// Block represents a bytecode block with captured variables
type Block struct {
	ID       string
	Chunk    *bytecode.Chunk
	Captures []*CaptureCell

	// Context from enclosing method
	InstanceID string
	ClassName  string
}

// BlockRunner manages block registration and execution
type BlockRunner struct {
	registry map[string]*Block
	mu       sync.RWMutex
	counter  uint64
	vm       *bytecode.VM

	// Callbacks for integration
	dispatcher *Dispatcher
	os         *ObjectSpace
}

// NewBlockRunner creates a new block runner
func NewBlockRunner(os *ObjectSpace) *BlockRunner {
	br := &BlockRunner{
		registry: make(map[string]*Block),
		os:       os,
		vm:       bytecode.NewVM(),
	}

	// Set up VM callbacks
	br.vm.SetMessageSender(br)
	br.vm.SetInstanceAccessor(br)

	return br
}

// SetDispatcher sets the dispatcher for message sends from blocks
func (br *BlockRunner) SetDispatcher(d *Dispatcher) {
	br.dispatcher = d
}

// RegisterBlock registers a bytecode block and returns its ID
func (br *BlockRunner) RegisterBlock(chunk *bytecode.Chunk, captures []*CaptureCell, instanceID, className string) string {
	br.mu.Lock()
	defer br.mu.Unlock()

	id := atomic.AddUint64(&br.counter, 1)
	blockID := fmt.Sprintf("bytecode_block_%d", id)

	br.registry[blockID] = &Block{
		ID:         blockID,
		Chunk:      chunk,
		Captures:   captures,
		InstanceID: instanceID,
		ClassName:  className,
	}

	return blockID
}

// RegisterBlockWithID registers a block with a specific ID
func (br *BlockRunner) RegisterBlockWithID(id string, block *Block) {
	br.mu.Lock()
	defer br.mu.Unlock()
	block.ID = id
	br.registry[id] = block
}

// GetBlock retrieves a block by ID
func (br *BlockRunner) GetBlock(id string) *Block {
	br.mu.RLock()
	defer br.mu.RUnlock()
	return br.registry[id]
}

// UnregisterBlock removes a block from the registry
func (br *BlockRunner) UnregisterBlock(id string) {
	br.mu.Lock()
	defer br.mu.Unlock()
	delete(br.registry, id)
}

// Invoke executes a block by ID with arguments
func (br *BlockRunner) Invoke(blockID string, args []Value) Value {
	br.mu.RLock()
	block, ok := br.registry[blockID]
	br.mu.RUnlock()

	if !ok {
		return ErrorValue(fmt.Sprintf("block not found: %s", blockID))
	}

	return br.InvokeDirect(block, args)
}

// InvokeDirect executes a block when you have the pointer
func (br *BlockRunner) InvokeDirect(block *Block, args []Value) Value {
	if block == nil {
		return ErrorValue("nil block")
	}

	// Convert Value args to string args for VM
	strArgs := make([]string, len(args))
	for i, arg := range args {
		strArgs[i] = arg.AsString()
	}

	// Convert our captures to bytecode captures
	bcCaptures := make([]*bytecode.CaptureCell, len(block.Captures))
	for i, cap := range block.Captures {
		if cap != nil {
			bcCaptures[i] = &bytecode.CaptureCell{
				Value:      cap.Value.AsString(),
				Name:       cap.Name,
				Source:     bytecode.VarSource(cap.Source),
				InstanceID: cap.InstanceID,
				Accessor:   br,
			}
		}
	}

	// Execute with captures
	result, err := br.vm.ExecuteWithCaptures(block.Chunk, block.InstanceID, strArgs, bcCaptures)
	if err != nil {
		return ErrorValue(err.Error())
	}

	// Update our captures from bytecode captures
	for i, bcCap := range bcCaptures {
		if bcCap != nil && i < len(block.Captures) && block.Captures[i] != nil {
			block.Captures[i].Value = StringValue(bcCap.Value)
		}
	}

	return ValueFromJSON(result)
}

// ============================================================================
// Implement bytecode.MessageSender interface
// ============================================================================

// SendMessage dispatches a message from bytecode execution
func (br *BlockRunner) SendMessage(receiver, selector string, args ...string) (string, error) {
	if br.dispatcher == nil {
		return "", fmt.Errorf("no dispatcher configured")
	}

	// Convert string args to Values
	valueArgs := make([]Value, len(args))
	for i, arg := range args {
		valueArgs[i] = StringValue(arg)
	}

	result := br.dispatcher.Send(receiver, selector, valueArgs)

	if result.Type == TypeError {
		return "", fmt.Errorf("%s", result.ErrorMsg)
	}

	return result.AsString(), nil
}

// ============================================================================
// Implement bytecode.InstanceAccessor interface
// ============================================================================

// GetInstanceVar reads an instance variable
func (br *BlockRunner) GetInstanceVar(instanceID, varName string) (string, error) {
	if br.os == nil {
		return "", fmt.Errorf("no object space configured")
	}

	inst := br.os.GetInstance(instanceID)
	if inst == nil {
		return "", fmt.Errorf("instance not found: %s", instanceID)
	}

	return inst.GetVarString(varName), nil
}

// SetInstanceVar writes an instance variable
func (br *BlockRunner) SetInstanceVar(instanceID, varName, value string) error {
	if br.os == nil {
		return fmt.Errorf("no object space configured")
	}

	inst := br.os.GetInstance(instanceID)
	if inst == nil {
		return fmt.Errorf("instance not found: %s", instanceID)
	}

	inst.SetVarString(varName, value)
	return nil
}

// BlockStats returns statistics about the block registry
func (br *BlockRunner) BlockStats() (count int) {
	br.mu.RLock()
	defer br.mu.RUnlock()
	return len(br.registry)
}

// ClearRegistry removes all blocks from the registry
func (br *BlockRunner) ClearRegistry() {
	br.mu.Lock()
	defer br.mu.Unlock()
	br.registry = make(map[string]*Block)
}
