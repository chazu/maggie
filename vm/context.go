package vm

// No imports needed - global registry removed, methods moved to ObjectRegistry.

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
// The context registry is now VM-local, managed by ObjectRegistry.
// Use vm.registry.RegisterContextValue(), vm.registry.GetContextFromValue(),
// and vm.registry.UnregisterContextValue() instead of the old global functions.
