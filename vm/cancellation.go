package vm

import (
	"context"
	"sync"
	"sync/atomic"
	"time"
)

// ---------------------------------------------------------------------------
// CancellationContext: Wraps Go's context.Context for cancellation and deadlines
// ---------------------------------------------------------------------------

// CancellationContextObject wraps a Go context for use in Smalltalk.
// Supports timeout, cancellation, and deadline management.
// Named to distinguish from ContextValue (Smalltalk execution contexts).
type CancellationContextObject struct {
	vtable    *VTable
	ctx       context.Context
	cancel    context.CancelFunc         // nil if not cancellable
	parent    *CancellationContextObject // parent context, nil for background
	cancelled atomic.Bool                // cached cancelled state
	doneOnce  sync.Once                  // for lazy done channel creation
	doneValue Value                      // cached done channel value
}

// CancellationContext marker for symbol encoding
const cancellationContextMarker uint32 = 35 << 24

// ---------------------------------------------------------------------------
// CancellationContext creation helpers
// ---------------------------------------------------------------------------

func createBackgroundCancellationContext() *CancellationContextObject {
	return &CancellationContextObject{
		ctx:    context.Background(),
		cancel: nil,
		parent: nil,
	}
}

func createTodoCancellationContext() *CancellationContextObject {
	return &CancellationContextObject{
		ctx:    context.TODO(),
		cancel: nil,
		parent: nil,
	}
}

func createCancellationContextWithCancel(parent *CancellationContextObject) *CancellationContextObject {
	var parentCtx context.Context
	if parent != nil {
		parentCtx = parent.ctx
	} else {
		parentCtx = context.Background()
	}

	ctx, cancel := context.WithCancel(parentCtx)
	return &CancellationContextObject{
		ctx:    ctx,
		cancel: cancel,
		parent: parent,
	}
}

func createCancellationContextWithTimeout(parent *CancellationContextObject, timeout time.Duration) *CancellationContextObject {
	var parentCtx context.Context
	if parent != nil {
		parentCtx = parent.ctx
	} else {
		parentCtx = context.Background()
	}

	ctx, cancel := context.WithTimeout(parentCtx, timeout)
	return &CancellationContextObject{
		ctx:    ctx,
		cancel: cancel,
		parent: parent,
	}
}

func createCancellationContextWithDeadline(parent *CancellationContextObject, deadline time.Time) *CancellationContextObject {
	var parentCtx context.Context
	if parent != nil {
		parentCtx = parent.ctx
	} else {
		parentCtx = context.Background()
	}

	ctx, cancel := context.WithDeadline(parentCtx, deadline)
	return &CancellationContextObject{
		ctx:    ctx,
		cancel: cancel,
		parent: parent,
	}
}

// ---------------------------------------------------------------------------
// CancellationContext methods
// ---------------------------------------------------------------------------

// Cancel cancels this context if it's cancellable.
func (c *CancellationContextObject) Cancel() {
	if c.cancel != nil {
		c.cancel()
		c.cancelled.Store(true)
	}
}

// IsCancelled returns true if this context has been cancelled.
func (c *CancellationContextObject) IsCancelled() bool {
	// Check cached value first
	if c.cancelled.Load() {
		return true
	}
	// Check actual context
	select {
	case <-c.ctx.Done():
		c.cancelled.Store(true)
		return true
	default:
		return false
	}
}

// Deadline returns the deadline and whether one is set.
func (c *CancellationContextObject) Deadline() (time.Time, bool) {
	return c.ctx.Deadline()
}

// Done returns the done channel for use with select.
func (c *CancellationContextObject) Done() <-chan struct{} {
	return c.ctx.Done()
}

// Err returns the error if the context is done.
func (c *CancellationContextObject) Err() error {
	return c.ctx.Err()
}

// ---------------------------------------------------------------------------
// CancellationContext primitives registration
// ---------------------------------------------------------------------------

func (vm *VM) registerCancellationContextPrimitives() {
	c := vm.CancellationContextClass

	// CancellationContext class>>background - returns the background context (never cancelled)
	c.AddClassMethod0(vm.Selectors, "background", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := createBackgroundCancellationContext()
		return v.registerCancellationContext(ctx)
	})

	// CancellationContext class>>todo - returns a TODO context (placeholder)
	c.AddClassMethod0(vm.Selectors, "todo", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := createTodoCancellationContext()
		return v.registerCancellationContext(ctx)
	})

	// CancellationContext class>>withCancel - create a cancellable context from background
	c.AddClassMethod0(vm.Selectors, "withCancel", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := createCancellationContextWithCancel(nil)
		return v.registerCancellationContext(ctx)
	})

	// CancellationContext class>>withTimeout: milliseconds - create a context with timeout
	c.AddClassMethod1(vm.Selectors, "withTimeout:", func(vmPtr interface{}, recv Value, msValue Value) Value {
		v := vmPtr.(*VM)
		if !msValue.IsSmallInt() {
			return Nil
		}
		ms := msValue.SmallInt()
		timeout := time.Duration(ms) * time.Millisecond
		ctx := createCancellationContextWithTimeout(nil, timeout)
		return v.registerCancellationContext(ctx)
	})

	// CancellationContext>>withCancel - create a child context with cancellation
	c.AddMethod0(vm.Selectors, "withCancel", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		parent := v.getCancellationContext(recv)
		if parent == nil {
			return Nil
		}
		ctx := createCancellationContextWithCancel(parent)
		return v.registerCancellationContext(ctx)
	})

	// CancellationContext>>withTimeout: milliseconds - create a child context with timeout
	c.AddMethod1(vm.Selectors, "withTimeout:", func(vmPtr interface{}, recv Value, msValue Value) Value {
		v := vmPtr.(*VM)
		parent := v.getCancellationContext(recv)
		if parent == nil {
			return Nil
		}
		if !msValue.IsSmallInt() {
			return Nil
		}
		ms := msValue.SmallInt()
		timeout := time.Duration(ms) * time.Millisecond
		ctx := createCancellationContextWithTimeout(parent, timeout)
		return v.registerCancellationContext(ctx)
	})

	// CancellationContext>>cancel - cancel this context
	c.AddMethod0(vm.Selectors, "cancel", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.getCancellationContext(recv)
		if ctx == nil {
			return Nil
		}
		ctx.Cancel()
		return recv
	})

	// CancellationContext>>isCancelled - check if context is cancelled
	c.AddMethod0(vm.Selectors, "isCancelled", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.getCancellationContext(recv)
		if ctx == nil {
			return True // nil context is considered cancelled
		}
		if ctx.IsCancelled() {
			return True
		}
		return False
	})

	// CancellationContext>>isDone - alias for isCancelled
	c.AddMethod0(vm.Selectors, "isDone", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.getCancellationContext(recv)
		if ctx == nil {
			return True
		}
		if ctx.IsCancelled() {
			return True
		}
		return False
	})

	// CancellationContext>>deadline - returns deadline as milliseconds since epoch, or nil
	c.AddMethod0(vm.Selectors, "deadline", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.getCancellationContext(recv)
		if ctx == nil {
			return Nil
		}
		deadline, ok := ctx.Deadline()
		if !ok {
			return Nil
		}
		// Return milliseconds since Unix epoch
		return FromSmallInt(deadline.UnixMilli())
	})

	// CancellationContext>>hasDeadline - check if context has a deadline
	c.AddMethod0(vm.Selectors, "hasDeadline", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.getCancellationContext(recv)
		if ctx == nil {
			return False
		}
		_, ok := ctx.Deadline()
		if ok {
			return True
		}
		return False
	})

	// CancellationContext>>remainingTime - milliseconds until deadline, or nil if no deadline
	c.AddMethod0(vm.Selectors, "remainingTime", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.getCancellationContext(recv)
		if ctx == nil {
			return Nil
		}
		deadline, ok := ctx.Deadline()
		if !ok {
			return Nil
		}
		remaining := time.Until(deadline)
		if remaining < 0 {
			return FromSmallInt(0)
		}
		return FromSmallInt(remaining.Milliseconds())
	})

	// CancellationContext>>wait - block until context is cancelled
	c.AddMethod0(vm.Selectors, "wait", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.getCancellationContext(recv)
		if ctx == nil {
			return Nil
		}
		<-ctx.Done()
		return recv
	})

	// CancellationContext>>do: aBlock - execute block, cancel context when done
	// Returns the result of the block
	c.AddMethod1(vm.Selectors, "do:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		ctx := v.getCancellationContext(recv)
		if ctx == nil {
			return Nil
		}

		bv := v.currentInterpreter().getBlockValue(block)
		if bv == nil {
			return Nil
		}

		// Execute the block, always cancel context when done
		defer ctx.Cancel()

		return v.currentInterpreter().ExecuteBlock(
			bv.Block, bv.Captures, nil,
			bv.HomeFrame, bv.HomeSelf, bv.HomeMethod,
		)
	})

	// CancellationContext>>ifCancelled: aBlock - execute block if context is cancelled
	c.AddMethod1(vm.Selectors, "ifCancelled:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		ctx := v.getCancellationContext(recv)
		if ctx == nil || !ctx.IsCancelled() {
			return recv
		}

		bv := v.currentInterpreter().getBlockValue(block)
		if bv == nil {
			return recv
		}

		return v.currentInterpreter().ExecuteBlock(
			bv.Block, bv.Captures, nil,
			bv.HomeFrame, bv.HomeSelf, bv.HomeMethod,
		)
	})

	// CancellationContext>>parent - returns parent context or nil
	c.AddMethod0(vm.Selectors, "parent", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.getCancellationContext(recv)
		if ctx == nil || ctx.parent == nil {
			return Nil
		}
		return v.registerCancellationContext(ctx.parent)
	})

	// CancellationContext>>doneChannel - returns a channel that closes when context is done
	// Useful for select statements
	c.AddMethod0(vm.Selectors, "doneChannel", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := v.getCancellationContext(recv)
		if ctx == nil {
			return Nil
		}

		// Create a channel that will close when context is done
		ctx.doneOnce.Do(func() {
			// Create a Maggie channel
			ch := &ChannelObject{
				ch: make(chan Value, 1),
			}
			ctx.doneValue = v.registerChannel(ch)

			// Monitor context in background
			go func() {
				<-ctx.Done()
				// Send a value to signal done
				select {
				case ch.ch <- True:
				default:
				}
				// Close the channel
				ch.mu.Lock()
				if !ch.closed.Load() {
					ch.closed.Store(true)
					close(ch.ch)
				}
				ch.mu.Unlock()
			}()
		})

		return ctx.doneValue
	})
}

// ---------------------------------------------------------------------------
// Registry helpers
// ---------------------------------------------------------------------------

// isCancellationContextValue checks if a Value represents a cancellation context.
func isCancellationContextValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return (id & (0xFF << 24)) == cancellationContextMarker
}

// cancellationContextToValue converts a context ID to a Value.
func cancellationContextToValue(id int) Value {
	return FromSymbolID(uint32(id) | cancellationContextMarker)
}
