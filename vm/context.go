package vm

import (
	"sync"
	"sync/atomic"
)

// ContextValue represents a reified execution context (activation record).
// It captures the state of a method or block execution at a point in time.
// This is the runtime representation of "thisContext" in Smalltalk.
type ContextValue struct {
	// Frame information
	Method   *CompiledMethod // The method being executed (nil for blocks)
	Block    *BlockMethod    // The block being executed (nil for methods)
	Receiver Value           // The receiver (self)
	Args     []Value         // Arguments passed to the method/block
	Temps    []Value         // Copy of temporaries at capture time

	// Stack information
	IP         int   // Instruction pointer at capture time
	SenderID   int32 // Context ID of the sender (-1 if none)
	HomeID     int32 // Context ID of the home context for blocks (-1 for methods)
	FrameIndex int   // Original frame index in interpreter (for debugging)

	// For blocks
	Captures []Value // Captured variables
}

// IsBlockContext returns true if this is a block context rather than a method context.
func (c *ContextValue) IsBlockContext() bool {
	return c.Block != nil
}

// ---------------------------------------------------------------------------
// Context Registry
// ---------------------------------------------------------------------------

// contextRegistry manages ContextValue instances by ID.
// Similar to blockRegistry, this allows us to store context IDs in Values.
type contextRegistry struct {
	mu       sync.RWMutex
	contexts map[uint32]*ContextValue
	nextID   uint32
}

var globalContextRegistry = &contextRegistry{
	contexts: make(map[uint32]*ContextValue),
	nextID:   1,
}

// Register adds a context to the registry and returns its ID.
func (r *contextRegistry) Register(ctx *ContextValue) uint32 {
	id := atomic.AddUint32(&r.nextID, 1) - 1
	r.mu.Lock()
	r.contexts[id] = ctx
	r.mu.Unlock()
	return id
}

// Get retrieves a context by ID.
func (r *contextRegistry) Get(id uint32) *ContextValue {
	r.mu.RLock()
	defer r.mu.RUnlock()
	return r.contexts[id]
}

// Remove removes a context from the registry.
// Called when contexts are no longer needed to prevent memory leaks.
func (r *contextRegistry) Remove(id uint32) {
	r.mu.Lock()
	delete(r.contexts, id)
	r.mu.Unlock()
}

// Clear removes all contexts (for testing/reset).
func (r *contextRegistry) Clear() {
	r.mu.Lock()
	r.contexts = make(map[uint32]*ContextValue)
	r.mu.Unlock()
}

// Size returns the number of registered contexts.
func (r *contextRegistry) Size() int {
	r.mu.RLock()
	defer r.mu.RUnlock()
	return len(r.contexts)
}

// ---------------------------------------------------------------------------
// Helper Functions
// ---------------------------------------------------------------------------

// GetContextValue retrieves the ContextValue for a context Value.
func GetContextValue(v Value) *ContextValue {
	if !v.IsContext() {
		return nil
	}
	return globalContextRegistry.Get(v.ContextID())
}

// RegisterContext registers a ContextValue and returns a Value representing it.
func RegisterContext(ctx *ContextValue) Value {
	id := globalContextRegistry.Register(ctx)
	return FromContextID(id)
}

// UnregisterContext removes a context from the registry.
func UnregisterContext(v Value) {
	if v.IsContext() {
		globalContextRegistry.Remove(v.ContextID())
	}
}
