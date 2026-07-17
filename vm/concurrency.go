package vm

import (
	"context"
	"sync"
	"sync/atomic"
	"time"
	"unsafe"
)

// NOTE: Global channel/process registries have been removed.
// All channel and process registration now goes through VM-local registries
// via vm.registerChannel(), vm.getChannel(), vm.createProcess(),
// vm.registerProcess(), and vm.getProcess() methods defined in vm.go.

// ---------------------------------------------------------------------------
// Channel: Wraps Go channels for Smalltalk
// ---------------------------------------------------------------------------

// ChannelObject wraps a Go channel for use in Smalltalk. The buffered values
// live in the Go channel `ch`; since Values are pointer-carrying, Go's GC
// traces the channel buffer directly — no separate liveness mirror is needed.
//
// Close protocol: closing a Go channel while a sender is parked on it is a
// data race (recover-ing the resulting panic does not fix that). Blocking
// senders therefore register in `senders` (under mu) and select on `done`;
// Close() closes `done` first, waits for parked senders to vacate, and only
// then closes `ch`. Receivers are unaffected — `ch` still closes, so drains
// and reflect.Select keep their semantics.
type ChannelObject struct {
	vtable  *VTable
	ch      chan Value
	done    chan struct{}  // closed by Close() before ch; wakes parked senders
	senders sync.WaitGroup // blocking senders currently parked on ch
	closed  atomic.Bool
	mu      sync.Mutex // protects the close flag and sender registration
}

// safeSend sends val, blocking until sent or the channel closes.
func (co *ChannelObject) safeSend(val Value) bool {
	sent, _ := co.sendCtx(nil, val)
	return sent
}

// sendCtx is the single blocking-send implementation. A nil ctx means "block
// until sent or closed". Returns (sent, ctxErr); sent=false with a nil ctxErr
// means the channel closed.
func (co *ChannelObject) sendCtx(ctx context.Context, val Value) (bool, error) {
	co.mu.Lock()
	if co.closed.Load() {
		co.mu.Unlock()
		return false, nil
	}
	// Buffered channel with space (or ready receiver): completes immediately.
	select {
	case co.ch <- val:
		co.mu.Unlock()
		return true, nil
	default:
	}
	// Register as a parked sender while still holding mu, so Close() cannot
	// pass senders.Wait() before this goroutine is selecting on done.
	co.senders.Add(1)
	co.mu.Unlock()
	defer co.senders.Done()

	var ctxDone <-chan struct{}
	if ctx != nil {
		ctxDone = ctx.Done()
	}
	select {
	case co.ch <- val:
		return true, nil
	case <-co.done:
		return false, nil
	case <-ctxDone: // nil when ctx == nil: never fires
		return false, ctx.Err()
	}
}

// safeTrySend attempts a non-blocking send on ch.ch while holding the
// mutex to prevent a concurrent close.
func (co *ChannelObject) safeTrySend(val Value) bool {
	co.mu.Lock()
	defer co.mu.Unlock()
	if co.closed.Load() {
		return false
	}
	select {
	case co.ch <- val:
		return true
	default:
		return false
	}
}

// NewChannelObject creates a standalone ChannelObject with the given buffer
// capacity (0 = unbuffered). Exported for server tests and wiring layers.
func NewChannelObject(capacity int) *ChannelObject {
	return createChannel(capacity)
}

func createChannel(buffered int) *ChannelObject {
	ch := &ChannelObject{done: make(chan struct{})}
	if buffered > 0 {
		ch.ch = make(chan Value, buffered)
	} else {
		ch.ch = make(chan Value)
	}
	return ch
}

// SafeSend sends a value, returning false if the channel is closed.
func (co *ChannelObject) SafeSend(val Value) bool { return co.safeSend(val) }

// SafeTrySend attempts a non-blocking send.
func (co *ChannelObject) SafeTrySend(val Value) bool { return co.safeTrySend(val) }

// Receive blocks until a value is received. Returns (value, ok).
func (co *ChannelObject) Receive() (Value, bool) {
	val, ok := <-co.ch
	return val, ok
}

// ReceiveCtx blocks until a value is received, the channel closes, or ctx is
// done. Returns (value, ok, ctxErr); ctxErr is non-nil only when ctx expired
// first. Used by server RPC handlers so a client disconnect frees the handler
// goroutine instead of pinning it forever on an empty channel.
func (co *ChannelObject) ReceiveCtx(ctx context.Context) (Value, bool, error) {
	select {
	case val, ok := <-co.ch:
		return val, ok, nil
	case <-ctx.Done():
		return Nil, false, ctx.Err()
	}
}

// SendCtx sends val, blocking until sent, the channel closes, or ctx is done.
// Returns (sent, ctxErr); sent=false with a nil ctxErr means the channel
// closed. The RPC counterpart of safeSend.
func (co *ChannelObject) SendCtx(ctx context.Context, val Value) (bool, error) {
	return co.sendCtx(ctx, val)
}

// TryReceive attempts a non-blocking receive. Returns (value, gotValue, ok).
func (co *ChannelObject) TryReceive() (Value, bool, bool) {
	select {
	case val, ok := <-co.ch:
		return val, true, ok
	default:
		return Nil, false, !co.closed.Load()
	}
}

// Close closes the channel. Parked senders are woken via done and observe
// the close as sent=false; ch itself is closed only after they vacate, so
// close(co.ch) never races a parked sender.
func (co *ChannelObject) Close() {
	co.mu.Lock()
	defer co.mu.Unlock()
	if co.closed.Load() {
		return
	}
	co.closed.Store(true)
	close(co.done)
	co.senders.Wait()
	close(co.ch)
}

// Closed returns true if the channel is closed.
func (co *ChannelObject) Closed() bool { return co.closed.Load() }

// Size returns the number of buffered items.
func (co *ChannelObject) Size() int { return len(co.ch) }

// Cap returns the buffer capacity.
func (co *ChannelObject) Cap() int { return cap(co.ch) }

// channelToValue wraps a ChannelObject pointer in a heap Value traced by the
// Go GC.
func channelToValue(ch *ChannelObject) Value {
	return makeHeap(kindChannel, unsafe.Pointer(ch))
}

func isChannelValue(v Value) bool {
	return v.ptr != nil && v.hi == kindChannel
}

// ---------------------------------------------------------------------------
// Process: Wraps goroutines for Smalltalk
// ---------------------------------------------------------------------------

// ProcessState represents the state of a process.
type ProcessState int

const (
	ProcessRunning ProcessState = iota
	ProcessSuspended
	ProcessTerminated
)

// ProcessObject wraps a goroutine for use in Smalltalk.
type ProcessObject struct {
	vtable    *VTable
	id        uint64
	state     atomic.Int32 // ProcessState
	finished  atomic.Bool  // guards one-shot finish (waitGroup.Done + close(done))
	done      chan struct{}
	result    Value
	err       error
	mu        sync.Mutex
	waitGroup sync.WaitGroup
	mailbox   *Mailbox // per-process message mailbox

	// Links and monitors
	links          map[uint64]bool              // bidirectional link partners (nil until first use)
	trapExit       bool                         // if true, exit signals become mailbox messages
	monitors       map[uint64]*MonitorRef       // monitors where THIS process is being watched
	myMonitors     map[uint64]*MonitorRef       // monitors where THIS process is the watcher
	exitReason     ExitReason                   // set by FinishProcess
	remoteMonitors map[uint64]*RemoteMonitorRef // monitors from remote nodes watching THIS process
}

// Mailbox returns the process's mailbox.
func (p *ProcessObject) Mailbox() *Mailbox { return p.mailbox }

// IsDone returns true if the process has terminated.
func (p *ProcessObject) IsDone() bool { return p.isDone() }

// processToValue wraps a ProcessObject pointer in a heap Value traced by the
// Go GC. Process identity for links/monitors/wire/name-registry purposes is
// the process's uint64 id, resolved through the live-process index.
func processToValue(proc *ProcessObject) Value {
	return makeHeap(kindProcess, unsafe.Pointer(proc))
}

func isProcessValue(v Value) bool {
	return v.ptr != nil && v.hi == kindProcess
}

// markDone is the legacy termination method. New code should use
// vm.FinishProcess() instead, which handles link/monitor notifications.
// Kept for backward compatibility with tests that don't have a VM reference.
func (p *ProcessObject) markDone(result Value, err error) {
	if !p.finished.CompareAndSwap(false, true) {
		return // already finished — second finish would panic on WaitGroup/close
	}
	p.mu.Lock()
	p.result = result
	p.err = err
	p.exitReason = ExitReason{Normal: err == nil, Result: result, Error: err}
	p.state.Store(int32(ProcessTerminated))
	p.mu.Unlock()
	p.waitGroup.Done()
	if p.mailbox != nil {
		p.mailbox.Close()
	}
	close(p.done)
}

func (p *ProcessObject) wait() Value {
	p.waitGroup.Wait()
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.result
}

// Wait blocks until the process completes and returns its result.
func (p *ProcessObject) Wait() Value { return p.wait() }

func (p *ProcessObject) isDone() bool {
	return p.state.Load() == int32(ProcessTerminated)
}

// ---------------------------------------------------------------------------
// Channel primitives registration
// ---------------------------------------------------------------------------

func (vm *VM) registerChannelPrimitives() {
	c := vm.ChannelClass

	// Channel class>>new - create unbuffered channel (class method)
	c.AddClassMethod0(vm.Selectors, "new", func(v *VM, recv Value) Value {
		ch := createChannel(0)
		return v.registerChannel(ch)
	})

	// Channel class>>new: size - create buffered channel (class method)
	c.AddClassMethod1(vm.Selectors, "new:", func(v *VM, recv Value, size Value) Value {
		if !size.IsSmallInt() {
			return Nil
		}
		bufSize := int(size.SmallInt())
		if bufSize < 0 {
			bufSize = 0
		}
		if bufSize > MaxArrayElements {
			return v.SignalPrimitiveError("Channel new:", "requested buffer size exceeds maximum")
		}
		ch := createChannel(bufSize)
		return v.registerChannel(ch)
	})

	// Channel>>primSend: value - send value to channel (blocking)
	c.AddMethod1(vm.Selectors, "primSend:", func(v *VM, recv Value, val Value) Value {
		ch := v.getChannel(recv)
		if ch == nil {
			return Nil
		}
		if ch.closed.Load() {
			return Nil // Can't send to closed channel
		}
		// A send on an unbuffered/full channel blocks until a receiver drains.
		ok := ch.safeSend(val)
		if !ok {
			return Nil // Channel closed between check and send
		}
		return recv
	})

	// Channel>>primReceive - receive value from channel (blocking)
	c.AddMethod0(vm.Selectors, "primReceive", func(v *VM, recv Value) Value {
		ch := v.getChannel(recv)
		if ch == nil {
			return Nil
		}
		val, ok := <-ch.ch
		if !ok {
			return Nil // Channel closed
		}
		return val
	})

	// Channel>>primTryReceive - non-blocking receive, returns nil if nothing available
	c.AddMethod0(vm.Selectors, "primTryReceive", func(v *VM, recv Value) Value {
		ch := v.getChannel(recv)
		if ch == nil {
			return Nil
		}
		select {
		case val, ok := <-ch.ch:
			if !ok {
				return Nil
			}
			return val
		default:
			return Nil
		}
	})

	// Channel>>primTrySend: value - non-blocking send, returns true if sent
	c.AddMethod1(vm.Selectors, "primTrySend:", func(v *VM, recv Value, val Value) Value {
		ch := v.getChannel(recv)
		if ch == nil {
			return False
		}
		if ch.closed.Load() {
			return False
		}
		if ch.safeTrySend(val) {
			return True
		}
		return False
	})

	// Channel>>primClose - close the channel
	c.AddMethod0(vm.Selectors, "primClose", func(v *VM, recv Value) Value {
		ch := v.getChannel(recv)
		if ch == nil {
			return recv
		}
		v.CloseChannel(ch)
		return recv
	})

	// Channel>>primIsClosed - check if channel is closed
	c.AddMethod0(vm.Selectors, "primIsClosed", func(v *VM, recv Value) Value {
		ch := v.getChannel(recv)
		if ch == nil {
			return True
		}
		if ch.closed.Load() {
			return True
		}
		return False
	})

	// Non-prim versions for backwards compatibility and use without Channel.mag

	// Channel>>send: - alias for primSend:
	c.AddMethod1(vm.Selectors, "send:", func(v *VM, recv Value, val Value) Value {
		ch := v.getChannel(recv)
		if ch == nil {
			return Nil
		}
		if ch.closed.Load() {
			return Nil
		}
		if !ch.safeSend(val) {
			return Nil
		}
		return recv
	})

	// Channel>>receive - alias for primReceive
	c.AddMethod0(vm.Selectors, "receive", func(v *VM, recv Value) Value {
		ch := v.getChannel(recv)
		if ch == nil {
			return Nil
		}
		val, ok := <-ch.ch
		if !ok {
			return Nil
		}
		return val
	})

	// Channel>>trySend: - alias for primTrySend:
	c.AddMethod1(vm.Selectors, "trySend:", func(v *VM, recv Value, val Value) Value {
		ch := v.getChannel(recv)
		if ch == nil {
			return False
		}
		if ch.closed.Load() {
			return False
		}
		if ch.safeTrySend(val) {
			return True
		}
		return False
	})

	// Channel>>tryReceive - alias for primTryReceive
	c.AddMethod0(vm.Selectors, "tryReceive", func(v *VM, recv Value) Value {
		ch := v.getChannel(recv)
		if ch == nil {
			return Nil
		}
		select {
		case val, ok := <-ch.ch:
			if !ok {
				return Nil
			}
			return val
		default:
			return Nil
		}
	})

	// Channel>>receiveIfClosed: - blocking receive; evaluates the block when
	// the channel is closed and drained, so a legitimately-sent nil is
	// distinguishable from closure (failure doctrine: nil never signals).
	c.AddMethod1(vm.Selectors, "receiveIfClosed:", func(v *VM, recv Value, blockVal Value) Value {
		ch := v.getChannel(recv)
		if ch == nil {
			return v.evaluateBlock(blockVal, nil)
		}
		val, ok := <-ch.ch
		if !ok {
			return v.evaluateBlock(blockVal, nil)
		}
		return val
	})

	// Channel>>tryReceiveIfEmpty: - non-blocking receive; evaluates the block
	// when no value was obtained (empty or closed+drained), so a
	// legitimately-sent nil is distinguishable from absence.
	c.AddMethod1(vm.Selectors, "tryReceiveIfEmpty:", func(v *VM, recv Value, blockVal Value) Value {
		ch := v.getChannel(recv)
		if ch == nil {
			return v.evaluateBlock(blockVal, nil)
		}
		select {
		case val, ok := <-ch.ch:
			if !ok {
				return v.evaluateBlock(blockVal, nil)
			}
			return val
		default:
			return v.evaluateBlock(blockVal, nil)
		}
	})

	// Channel>>close - alias for primClose
	c.AddMethod0(vm.Selectors, "close", func(v *VM, recv Value) Value {
		ch := v.getChannel(recv)
		if ch == nil {
			return recv
		}
		v.CloseChannel(ch)
		return recv
	})

	// Channel>>isClosed - alias for primIsClosed
	c.AddMethod0(vm.Selectors, "isClosed", func(v *VM, recv Value) Value {
		ch := v.getChannel(recv)
		if ch == nil {
			return True
		}
		if ch.closed.Load() {
			return True
		}
		return False
	})

	// Channel>>isEmpty - check if channel has no pending values (for buffered)
	c.AddMethod0(vm.Selectors, "isEmpty", func(v *VM, recv Value) Value {
		ch := v.getChannel(recv)
		if ch == nil {
			return True
		}
		if len(ch.ch) == 0 {
			return True
		}
		return False
	})

	// Channel>>size - number of pending values (for buffered)
	c.AddMethod0(vm.Selectors, "size", func(v *VM, recv Value) Value {
		ch := v.getChannel(recv)
		if ch == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(len(ch.ch)))
	})

	// Channel>>capacity - buffer capacity
	c.AddMethod0(vm.Selectors, "capacity", func(v *VM, recv Value) Value {
		ch := v.getChannel(recv)
		if ch == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(cap(ch.ch)))
	})
}

// ---------------------------------------------------------------------------
// Process primitives registration
// ---------------------------------------------------------------------------

func (vm *VM) registerProcessPrimitives() {
	// Block>>fork - create new process running this block
	// Uses ExecuteBlockDetached so that non-local returns (^) in the block
	// become local returns instead of crashing (since the home frame is unreachable)
	vm.BlockClass.AddMethod0(vm.Selectors, "fork", func(v *VM, recv Value) Value {
		bv := v.currentInterpreter().getBlockValue(recv)
		if bv == nil {
			return Nil
		}

		proc := v.createProcess()
		procValue := v.registerProcess(proc)

		// Capture the caller's restrictions on THIS goroutine; the fork must
		// inherit them (see inheritedHidden).
		callerHidden := v.inheritedHidden(nil)

		go func() {
			defer func() {
				v.HandleForkedPanic(proc, recover())
				// Unregister the interpreter when done
				v.unregisterInterpreter()
			}()

			// Create a forked interpreter for this goroutine
			interp := v.newForkedInterpreter(callerHidden)
			interp.processID = proc.id
			// Register this interpreter for the current goroutine
			v.registerInterpreter(interp)
			// Use ExecuteBlockDetached so ^ becomes local return
			result := interp.ExecuteBlockDetached(bv.Block, bv.Captures, nil, bv.HomeSelf, bv.HomeMethod)
			v.FinishProcess(proc, ExitNormal(result))
		}()

		return procValue
	})

	// Block>>forkWith: arg - fork with single argument
	// Uses ExecuteBlockDetached so that non-local returns (^) become local returns
	vm.BlockClass.AddMethod1(vm.Selectors, "forkWith:", func(v *VM, recv Value, arg Value) Value {
		bv := v.currentInterpreter().getBlockValue(recv)
		if bv == nil {
			return Nil
		}

		proc := v.createProcess()
		procValue := v.registerProcess(proc)

		callerHidden := v.inheritedHidden(nil)

		go func() {
			defer func() {
				v.HandleForkedPanic(proc, recover())
				v.unregisterInterpreter()
			}()

			interp := v.newForkedInterpreter(callerHidden)
			interp.processID = proc.id
			v.registerInterpreter(interp)
			result := interp.ExecuteBlockDetached(bv.Block, bv.Captures, []Value{arg}, bv.HomeSelf, bv.HomeMethod)
			v.FinishProcess(proc, ExitNormal(result))
		}()

		return procValue
	})

	// Block>>forkWithContext: ctx - fork with cancellation context
	// Passes the context to the block as first argument
	// Block should check ctx isCancelled periodically
	vm.BlockClass.AddMethod1(vm.Selectors, "forkWithContext:", func(v *VM, recv Value, ctxArg Value) Value {
		bv := v.currentInterpreter().getBlockValue(recv)
		if bv == nil {
			return Nil
		}

		ctx := v.getCancellationContext(ctxArg)
		if ctx == nil {
			return Nil
		}

		proc := v.createProcess()
		procValue := v.registerProcess(proc)

		callerHidden := v.inheritedHidden(nil)

		go func() {
			defer func() {
				v.HandleForkedPanic(proc, recover())
				v.unregisterInterpreter()
			}()

			interp := v.newForkedInterpreter(callerHidden)
			interp.processID = proc.id
			v.registerInterpreter(interp)

			// Cancellation is cooperative: the block receives the context and
			// polls `isCancelled` (a Go goroutine can't be force-stopped). The
			// context is passed as the block's first argument; no watcher
			// goroutine is needed — an empty select arm on ctx.Done() did
			// nothing but imply a preemption that doesn't exist.
			result := interp.ExecuteBlockDetached(bv.Block, bv.Captures, []Value{ctxArg}, bv.HomeSelf, bv.HomeMethod)
			v.FinishProcess(proc, ExitNormal(result))
		}()

		return procValue
	})

	c := vm.ProcessClass

	// Process class>>fork: block - fork a block as a new process (class method)
	// Uses ExecuteBlockDetached so that non-local returns (^) become local returns
	c.AddClassMethod1(vm.Selectors, "fork:", func(v *VM, recv Value, block Value) Value {
		bv := v.currentInterpreter().getBlockValue(block)
		if bv == nil {
			return Nil
		}

		proc := v.createProcess()
		procValue := v.registerProcess(proc)

		callerHidden := v.inheritedHidden(nil)

		go func() {
			defer func() {
				v.HandleForkedPanic(proc, recover())
				v.unregisterInterpreter()
			}()

			interp := v.newForkedInterpreter(callerHidden)
			interp.processID = proc.id
			v.registerInterpreter(interp)
			result := interp.ExecuteBlockDetached(bv.Block, bv.Captures, nil, bv.HomeSelf, bv.HomeMethod)
			v.FinishProcess(proc, ExitNormal(result))
		}()

		return procValue
	})

	// Process>>wait - wait for process to complete, return result
	c.AddMethod0(vm.Selectors, "wait", func(v *VM, recv Value) Value {
		proc := v.getProcess(recv)
		if proc == nil {
			return Nil
		}
		return proc.wait()
	})

	c.AddMethod0(vm.Selectors, "primWait", func(v *VM, recv Value) Value {
		proc := v.getProcess(recv)
		if proc == nil {
			return Nil
		}
		return proc.wait()
	})

	// Process>>primIsAlive - check if process is still running (opposite of isDone)
	c.AddMethod0(vm.Selectors, "primIsAlive", func(v *VM, recv Value) Value {
		proc := v.getProcess(recv)
		if proc == nil {
			return False
		}
		if proc.isDone() {
			return False
		}
		return True
	})

	// Process>>result - get result (nil if not done)
	c.AddMethod0(vm.Selectors, "result", func(v *VM, recv Value) Value {
		proc := v.getProcess(recv)
		if proc == nil {
			return Nil
		}
		if !proc.isDone() {
			return Nil
		}
		proc.mu.Lock()
		defer proc.mu.Unlock()
		return proc.result
	})

	c.AddMethod0(vm.Selectors, "primResult", func(v *VM, recv Value) Value {
		proc := v.getProcess(recv)
		if proc == nil {
			return Nil
		}
		if !proc.isDone() {
			return Nil
		}
		proc.mu.Lock()
		defer proc.mu.Unlock()
		return proc.result
	})

	// Process>>primTerminate - terminate process with shutdown signal
	c.AddMethod0(vm.Selectors, "primTerminate", func(v *VM, recv Value) Value {
		proc := v.getProcess(recv)
		if proc == nil || proc.isDone() {
			return recv
		}
		v.FinishProcess(proc, ExitSignal("shutdown", Nil))
		return recv
	})

	// (primPriority / primPriority: were deleted: goroutines have no
	// priorities, and the stubs backed lib-level `priority`/`forkAt:` API
	// that lied to users — see docs/CONVENTIONS.md, selector honesty.)

	// Process class>>current - get current process (class method)
	c.AddClassMethod0(vm.Selectors, "current", func(v *VM, recv Value) Value {
		return v.currentProcessValue()
	})

	// Process class>>yield - yield to other goroutines (class method)
	c.AddClassMethod0(vm.Selectors, "yield", func(_ *VM, recv Value) Value {
		// runtime.Gosched() is the Go equivalent
		// For now, just return
		return Nil
	})

	// Process class>>sleep: milliseconds - sleep for specified time (class method)
	c.AddClassMethod1(vm.Selectors, "sleep:", func(v *VM, recv Value, ms Value) Value {
		if !ms.IsSmallInt() {
			return recv
		}
		duration := time.Duration(ms.SmallInt()) * time.Millisecond
		time.Sleep(duration)
		return recv
	})

	// Block>>forkRestricted: restrictions - fork with restricted globals
	// restrictions is an Array of class name strings/symbols to hide
	vm.BlockClass.AddMethod1(vm.Selectors, "forkRestricted:", func(v *VM, recv Value, restrictionsVal Value) Value {
		bv := v.currentInterpreter().getBlockValue(recv)
		if bv == nil {
			return Nil
		}

		extra := v.extractHiddenMap(restrictionsVal)
		if extra == nil {
			return Nil
		}
		// Merge the requested restrictions with any inherited from the caller.
		hidden := v.inheritedHidden(extra)

		proc := v.createProcess()
		procValue := v.registerProcess(proc)

		go func() {
			defer func() {
				v.HandleForkedPanic(proc, recover())
				v.unregisterInterpreter()
			}()

			interp := v.newForkedInterpreter(hidden)
			interp.processID = proc.id
			v.registerInterpreter(interp)
			result := interp.ExecuteBlockDetached(bv.Block, bv.Captures, nil, bv.HomeSelf, bv.HomeMethod)
			v.FinishProcess(proc, ExitNormal(result))
		}()

		return procValue
	})

	// Process class>>forkWithout:do: - fork a block with restrictions (class method)
	// restrictions is an Array of class name strings/symbols to hide
	c.AddClassMethod2(vm.Selectors, "forkWithout:do:", func(v *VM, recv Value, restrictionsVal, blockVal Value) Value {
		bv := v.currentInterpreter().getBlockValue(blockVal)
		if bv == nil {
			return Nil
		}
		// blockVal is the parameter — already a Value

		extra := v.extractHiddenMap(restrictionsVal)
		if extra == nil {
			return Nil
		}
		// Merge the requested restrictions with any inherited from the caller.
		hidden := v.inheritedHidden(extra)

		proc := v.createProcess()
		procValue := v.registerProcess(proc)

		go func() {
			defer func() {
				v.HandleForkedPanic(proc, recover())
				v.unregisterInterpreter()
			}()

			interp := v.newForkedInterpreter(hidden)
			interp.processID = proc.id
			v.registerInterpreter(interp)
			result := interp.ExecuteBlockDetached(bv.Block, bv.Captures, nil, bv.HomeSelf, bv.HomeMethod)
			v.FinishProcess(proc, ExitNormal(result))
		}()

		return procValue
	})
}

// extractHiddenMap converts a Maggie Array of name strings/symbols into a hidden map.
// Returns nil if the input is not a valid array.
func (vm *VM) extractHiddenMap(restrictionsVal Value) map[string]bool {
	if !restrictionsVal.IsObject() {
		return nil
	}
	obj := ObjectFromValue(restrictionsVal)
	if obj == nil {
		return nil
	}
	hidden := make(map[string]bool, obj.NumSlots())
	for i := 0; i < obj.NumSlots(); i++ {
		elem := obj.GetSlot(i)
		var name string
		if IsStringValue(elem) {
			name = vm.registry.GetStringContent(elem)
		} else if elem.IsSymbol() {
			name = vm.Symbols.Name(elem.SymbolID())
		}
		if name != "" {
			hidden[name] = true
		}
	}
	return hidden
}

// ---------------------------------------------------------------------------
// Link and Monitor primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerLinkMonitorPrimitives() {
	c := vm.ProcessClass

	// Process>>primLink: otherProcess
	c.AddMethod1(vm.Selectors, "primLink:", func(v *VM, recv Value, other Value) Value {
		procA := v.getProcess(recv)
		procB := v.getProcess(other)
		if procA == nil || procB == nil {
			return False
		}
		v.LinkProcesses(procA, procB)
		return True
	})

	// Process>>primUnlink: otherProcess
	c.AddMethod1(vm.Selectors, "primUnlink:", func(v *VM, recv Value, other Value) Value {
		procA := v.getProcess(recv)
		procB := v.getProcess(other)
		if procA == nil || procB == nil {
			return False
		}
		v.UnlinkProcesses(procA, procB)
		return True
	})

	// Process>>primMonitor: otherProcess — returns monitor ref ID as SmallInt
	c.AddMethod1(vm.Selectors, "primMonitor:", func(v *VM, recv Value, other Value) Value {
		watcher := v.getProcess(recv)
		watched := v.getProcess(other)
		if watcher == nil || watched == nil {
			return Nil
		}
		ref, err := v.MonitorProcess(watcher, watched)
		if err != nil {
			return v.SignalPrimitiveError("Process primMonitor:", err.Error())
		}
		return FromSmallInt(int64(ref.ID))
	})

	// Process>>primDemonitor: refID
	c.AddMethod1(vm.Selectors, "primDemonitor:", func(v *VM, recv Value, refIDVal Value) Value {
		watcher := v.getProcess(recv)
		if watcher == nil || !refIDVal.IsSmallInt() {
			return False
		}
		refID := uint64(refIDVal.SmallInt())
		watcher.mu.Lock()
		ref, ok := watcher.myMonitors[refID]
		watcher.mu.Unlock()
		if !ok {
			return False
		}
		v.DemonitorProcess(ref)
		return True
	})

	// Process>>primTrapExit: aBoolean
	c.AddMethod1(vm.Selectors, "primTrapExit:", func(v *VM, recv Value, flag Value) Value {
		proc := v.getProcess(recv)
		if proc == nil {
			return False
		}
		proc.mu.Lock()
		proc.trapExit = (flag == True)
		proc.mu.Unlock()
		return True
	})

	// Process>>primTrapExit — query
	c.AddMethod0(vm.Selectors, "primTrapExit", func(v *VM, recv Value) Value {
		proc := v.getProcess(recv)
		if proc == nil {
			return False
		}
		proc.mu.Lock()
		trapping := proc.trapExit
		proc.mu.Unlock()
		if trapping {
			return True
		}
		return False
	})

	// Process>>primExitReason — returns exit reason as symbol or nil
	c.AddMethod0(vm.Selectors, "primExitReason", func(v *VM, recv Value) Value {
		proc := v.getProcess(recv)
		if proc == nil {
			return Nil
		}
		if !proc.isDone() {
			return Nil
		}
		proc.mu.Lock()
		reason := proc.exitReason
		proc.mu.Unlock()
		return v.exitReasonToValue(reason)
	})
}

// ---------------------------------------------------------------------------
// Mailbox primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerMailboxPrimitives() {
	c := vm.ProcessClass

	// Bootstrap MailboxMessage class
	vm.MailboxMessageClass = NewClassWithInstVars("MailboxMessage", vm.ObjectClass, []string{"sender", "selector", "payload"})
	vm.MailboxMessageClass.NumSlots = 3
	vm.Classes.Register(vm.MailboxMessageClass)
	vm.globals["MailboxMessage"] = vm.classValue(vm.MailboxMessageClass)

	// MailboxMessage accessors
	vm.MailboxMessageClass.AddMethod0(vm.Selectors, "sender", func(_ *VM, recv Value) Value {
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		return obj.GetSlot(0)
	})
	vm.MailboxMessageClass.AddMethod0(vm.Selectors, "selector", func(_ *VM, recv Value) Value {
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		return obj.GetSlot(1)
	})
	vm.MailboxMessageClass.AddMethod0(vm.Selectors, "payload", func(_ *VM, recv Value) Value {
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		return obj.GetSlot(2)
	})

	// Process>>primSend: payload — fire-and-forget to target's mailbox
	c.AddMethod1(vm.Selectors, "primSend:", func(v *VM, recv Value, payload Value) Value {
		proc := v.getProcess(recv)
		if proc == nil || proc.mailbox == nil {
			return False
		}
		senderProc := v.currentProcessValue()
		msg := v.CreateMailboxMessage(senderProc, "", payload)
		if !proc.mailbox.TrySend(msg) {
			return False
		}
		return True
	})

	// Process>>primSend:with: selector payload — selector-based message
	c.AddMethod2(vm.Selectors, "primSend:with:", func(v *VM, recv Value, selectorVal, payload Value) Value {
		proc := v.getProcess(recv)
		if proc == nil || proc.mailbox == nil {
			return False
		}
		sel := ""
		if selectorVal.IsSymbol() {
			sel = v.Symbols.Name(selectorVal.SymbolID())
		} else if IsStringValue(selectorVal) {
			sel = v.registry.GetStringContent(selectorVal)
		}
		senderProc := v.currentProcessValue()
		msg := v.CreateMailboxMessage(senderProc, sel, payload)
		if !proc.mailbox.TrySend(msg) {
			return False
		}
		return True
	})

	// Process class>>primReceive — blocking receive from current process's mailbox
	c.AddClassMethod0(vm.Selectors, "primReceive", func(v *VM, recv Value) Value {
		proc := v.currentProcess()
		if proc == nil || proc.mailbox == nil {
			return Nil
		}
		msg, ok := proc.mailbox.Receive()
		if !ok {
			return Nil
		}
		return msg
	})

	// Process class>>primReceive: timeoutMs — receive with timeout
	c.AddClassMethod1(vm.Selectors, "primReceive:", func(v *VM, recv Value, timeoutVal Value) Value {
		proc := v.currentProcess()
		if proc == nil || proc.mailbox == nil {
			return Nil
		}
		if !timeoutVal.IsSmallInt() {
			return Nil
		}
		ms := timeoutVal.SmallInt()
		msg, ok := proc.mailbox.ReceiveTimeout(time.Duration(ms) * time.Millisecond)
		if !ok {
			return Nil
		}
		return msg
	})

	// Process class>>primTryReceive — non-blocking receive
	c.AddClassMethod0(vm.Selectors, "primTryReceive", func(v *VM, recv Value) Value {
		proc := v.currentProcess()
		if proc == nil || proc.mailbox == nil {
			return Nil
		}
		msg, ok := proc.mailbox.TryReceive()
		if !ok {
			return Nil
		}
		return msg
	})

	// Process>>primRegisterAs: name — register this process with a name
	c.AddMethod1(vm.Selectors, "primRegisterAs:", func(v *VM, recv Value, nameVal Value) Value {
		proc := v.getProcess(recv)
		if proc == nil {
			return False
		}
		name := ""
		if IsStringValue(nameVal) {
			name = v.registry.GetStringContent(nameVal)
		} else if nameVal.IsSymbol() {
			name = v.Symbols.Name(nameVal.SymbolID())
		}
		if name == "" {
			return False
		}
		if v.RegisterProcessName(name, proc.id) {
			return True
		}
		return False
	})

	// Process class>>primNamed: name — look up process by registered name
	c.AddClassMethod1(vm.Selectors, "primNamed:", func(v *VM, recv Value, nameVal Value) Value {
		name := ""
		if IsStringValue(nameVal) {
			name = v.registry.GetStringContent(nameVal)
		} else if nameVal.IsSymbol() {
			name = v.Symbols.Name(nameVal.SymbolID())
		}
		if name == "" {
			return Nil
		}
		return v.LookupProcessName(name)
	})

	// Process>>primUnregister — remove name registration for this process
	c.AddMethod0(vm.Selectors, "primUnregister", func(v *VM, recv Value) Value {
		proc := v.getProcess(recv)
		if proc == nil {
			return Nil
		}
		v.processNamesMu.RLock()
		name, ok := v.processNamesByID[proc.id]
		v.processNamesMu.RUnlock()
		if ok {
			v.UnregisterProcessName(name)
		}
		return recv
	})
}
