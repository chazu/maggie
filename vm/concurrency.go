package vm

import (
	"sync"
	"sync/atomic"
	"time"
)

// NOTE: Global channel/process registries have been removed.
// All channel and process registration now goes through VM-local registries
// via vm.registerChannel(), vm.getChannel(), vm.createProcess(),
// vm.registerProcess(), and vm.getProcess() methods defined in vm.go.

// ---------------------------------------------------------------------------
// Channel: Wraps Go channels for Smalltalk
// ---------------------------------------------------------------------------

// ChannelObject wraps a Go channel for use in Smalltalk.
type ChannelObject struct {
	vtable *VTable
	ch     chan Value
	closed atomic.Bool
	mu     sync.Mutex // protects close operation
}

// safeSend attempts to send val on ch.ch while holding the mutex to prevent
// a concurrent close from causing a panic. The mutex is held only to check
// the closed flag and initiate the send; for unbuffered channels the send
// may block while holding the lock (which is acceptable because close() also
// acquires the lock, so close will wait until the send completes or another
// goroutine receives).
func (co *ChannelObject) safeSend(val Value) bool {
	co.mu.Lock()
	if co.closed.Load() {
		co.mu.Unlock()
		return false
	}
	// For buffered channels with space, the send completes immediately.
	// For unbuffered or full buffered channels, we must unlock before
	// blocking to avoid deadlock with close().
	select {
	case co.ch <- val:
		co.mu.Unlock()
		return true
	default:
		// Channel is full or unbuffered with no receiver; unlock and
		// do a blocking send with recover protection.
		co.mu.Unlock()
		return co.safeSendBlocking(val)
	}
}

// safeSendBlocking does a blocking send with defer/recover to catch
// "send on closed channel" panics that can occur if the channel is
// closed while we are waiting for a receiver.
func (co *ChannelObject) safeSendBlocking(val Value) (ok bool) {
	defer func() {
		if r := recover(); r != nil {
			ok = false
		}
	}()
	co.ch <- val
	return true
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

func createChannel(buffered int) *ChannelObject {
	ch := &ChannelObject{}
	if buffered > 0 {
		ch.ch = make(chan Value, buffered)
	} else {
		ch.ch = make(chan Value)
	}
	return ch
}


func channelToValue(id int) Value {
	// Encode channel ID in a way we can distinguish from symbols
	// Use the symbol encoding but with a special marker in the ID range
	return FromSymbolID(uint32(id) | channelMarker)
}

func isChannelValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return (id & (0xFF << 24)) == channelMarker
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
	done      chan struct{}
	result    Value
	err       error
	mu        sync.Mutex
	waitGroup sync.WaitGroup
	mailbox   *Mailbox // per-process message mailbox
}

// Mailbox returns the process's mailbox.
func (p *ProcessObject) Mailbox() *Mailbox { return p.mailbox }

// IsDone returns true if the process has terminated.
func (p *ProcessObject) IsDone() bool { return p.isDone() }


func processToValue(id uint64) Value {
	// Use symbol encoding with a different marker
	return FromSymbolID(uint32(id) | processMarker)
}

func isProcessValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return (id & (0xFF << 24)) == processMarker
}

func (p *ProcessObject) markDone(result Value, err error) {
	p.mu.Lock()
	p.result = result
	p.err = err
	p.state.Store(int32(ProcessTerminated))
	p.mu.Unlock()
	p.waitGroup.Done()
	if p.mailbox != nil {
		p.mailbox.Close() // wake blocked receivers, reject future sends
	}
	close(p.done)
}

func (p *ProcessObject) wait() Value {
	p.waitGroup.Wait()
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.result
}

func (p *ProcessObject) isDone() bool {
	return p.state.Load() == int32(ProcessTerminated)
}

// ---------------------------------------------------------------------------
// Channel primitives registration
// ---------------------------------------------------------------------------

func (vm *VM) registerChannelPrimitives() {
	c := vm.ChannelClass

	// Channel class>>new - create unbuffered channel (class method)
	c.AddClassMethod0(vm.Selectors, "new", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ch := createChannel(0)
		return v.registerChannel(ch)
	})

	// Channel class>>new: size - create buffered channel (class method)
	c.AddClassMethod1(vm.Selectors, "new:", func(vmPtr interface{}, recv Value, size Value) Value {
		v := vmPtr.(*VM)
		if !size.IsSmallInt() {
			return Nil
		}
		bufSize := int(size.SmallInt())
		if bufSize < 0 {
			bufSize = 0
		}
		ch := createChannel(bufSize)
		return v.registerChannel(ch)
	})

	// Channel>>primSend: value - send value to channel (blocking)
	c.AddMethod1(vm.Selectors, "primSend:", func(vmPtr interface{}, recv Value, val Value) Value {
		v := vmPtr.(*VM)
		ch := v.getChannel(recv)
		if ch == nil {
			return Nil
		}
		if ch.closed.Load() {
			return Nil // Can't send to closed channel
		}
		if !ch.safeSend(val) {
			return Nil // Channel closed between check and send
		}
		return recv
	})

	// Channel>>primReceive - receive value from channel (blocking)
	c.AddMethod0(vm.Selectors, "primReceive", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod0(vm.Selectors, "primTryReceive", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod1(vm.Selectors, "primTrySend:", func(vmPtr interface{}, recv Value, val Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod0(vm.Selectors, "primClose", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ch := v.getChannel(recv)
		if ch == nil {
			return recv
		}
		ch.mu.Lock()
		defer ch.mu.Unlock()
		if !ch.closed.Load() {
			ch.closed.Store(true)
			close(ch.ch)
		}
		return recv
	})

	// Channel>>primIsClosed - check if channel is closed
	c.AddMethod0(vm.Selectors, "primIsClosed", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod1(vm.Selectors, "send:", func(vmPtr interface{}, recv Value, val Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod0(vm.Selectors, "receive", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod1(vm.Selectors, "trySend:", func(vmPtr interface{}, recv Value, val Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod0(vm.Selectors, "tryReceive", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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

	// Channel>>close - alias for primClose
	c.AddMethod0(vm.Selectors, "close", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ch := v.getChannel(recv)
		if ch == nil {
			return recv
		}
		ch.mu.Lock()
		defer ch.mu.Unlock()
		if !ch.closed.Load() {
			ch.closed.Store(true)
			close(ch.ch)
		}
		return recv
	})

	// Channel>>isClosed - alias for primIsClosed
	c.AddMethod0(vm.Selectors, "isClosed", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod0(vm.Selectors, "isEmpty", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod0(vm.Selectors, "size", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ch := v.getChannel(recv)
		if ch == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(len(ch.ch)))
	})

	// Channel>>capacity - buffer capacity
	c.AddMethod0(vm.Selectors, "capacity", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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
	vm.BlockClass.AddMethod0(vm.Selectors, "fork", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		bv := v.currentInterpreter().getBlockValue(recv)
		if bv == nil {
			return Nil
		}

		proc := v.createProcess()
		procValue := v.registerProcess(proc)

		go func() {
			defer func() {
				if r := recover(); r != nil {
					// If somehow a NonLocalReturn escapes, capture its value
					if nlr, ok := r.(NonLocalReturn); ok {
						proc.markDone(nlr.Value, nil)
					} else {
						// Process crashed with other error
						proc.markDone(Nil, nil)
					}
				}
				// Unregister the interpreter when done
				v.unregisterInterpreter()
			}()

			// Create a forked interpreter for this goroutine
			interp := v.newForkedInterpreter(nil)
			interp.processID = proc.id
			// Register this interpreter for the current goroutine
			v.registerInterpreter(interp)
			// Use ExecuteBlockDetached so ^ becomes local return
			result := interp.ExecuteBlockDetached(bv.Block, bv.Captures, nil, bv.HomeSelf, bv.HomeMethod)
			proc.markDone(result, nil)
		}()

		return procValue
	})

	// Block>>forkWith: arg - fork with single argument
	// Uses ExecuteBlockDetached so that non-local returns (^) become local returns
	vm.BlockClass.AddMethod1(vm.Selectors, "forkWith:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		bv := v.currentInterpreter().getBlockValue(recv)
		if bv == nil {
			return Nil
		}

		proc := v.createProcess()
		procValue := v.registerProcess(proc)

		go func() {
			defer func() {
				if r := recover(); r != nil {
					if nlr, ok := r.(NonLocalReturn); ok {
						proc.markDone(nlr.Value, nil)
					} else {
						proc.markDone(Nil, nil)
					}
				}
				v.unregisterInterpreter()
			}()

			interp := v.newForkedInterpreter(nil)
			interp.processID = proc.id
			v.registerInterpreter(interp)
			result := interp.ExecuteBlockDetached(bv.Block, bv.Captures, []Value{arg}, bv.HomeSelf, bv.HomeMethod)
			proc.markDone(result, nil)
		}()

		return procValue
	})

	// Block>>forkWithContext: ctx - fork with cancellation context
	// Passes the context to the block as first argument
	// Block should check ctx isCancelled periodically
	vm.BlockClass.AddMethod1(vm.Selectors, "forkWithContext:", func(vmPtr interface{}, recv Value, ctxArg Value) Value {
		v := vmPtr.(*VM)
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

		go func() {
			defer func() {
				if r := recover(); r != nil {
					if nlr, ok := r.(NonLocalReturn); ok {
						proc.markDone(nlr.Value, nil)
					} else {
						proc.markDone(Nil, nil)
					}
				}
				v.unregisterInterpreter()
			}()

			interp := v.newForkedInterpreter(nil)
			interp.processID = proc.id
			v.registerInterpreter(interp)

			// Monitor context cancellation
			done := make(chan struct{})
			go func() {
				select {
				case <-ctx.Done():
					// Context cancelled - mark process as cancelled
					// The block should check isCancelled and exit gracefully
				case <-done:
					// Block completed normally
				}
			}()

			// Pass context as first argument
			result := interp.ExecuteBlockDetached(bv.Block, bv.Captures, []Value{ctxArg}, bv.HomeSelf, bv.HomeMethod)
			close(done)
			proc.markDone(result, nil)
		}()

		return procValue
	})

	c := vm.ProcessClass

	// Process class>>fork: block - fork a block as a new process (class method)
	// Uses ExecuteBlockDetached so that non-local returns (^) become local returns
	c.AddClassMethod1(vm.Selectors, "fork:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		bv := v.currentInterpreter().getBlockValue(block)
		if bv == nil {
			return Nil
		}

		proc := v.createProcess()
		procValue := v.registerProcess(proc)

		go func() {
			defer func() {
				if r := recover(); r != nil {
					if nlr, ok := r.(NonLocalReturn); ok {
						proc.markDone(nlr.Value, nil)
					} else {
						proc.markDone(Nil, nil)
					}
				}
				v.unregisterInterpreter()
			}()

			interp := v.newForkedInterpreter(nil)
			interp.processID = proc.id
			v.registerInterpreter(interp)
			result := interp.ExecuteBlockDetached(bv.Block, bv.Captures, nil, bv.HomeSelf, bv.HomeMethod)
			proc.markDone(result, nil)
		}()

		return procValue
	})

	// Process>>wait - wait for process to complete, return result
	c.AddMethod0(vm.Selectors, "wait", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		proc := v.getProcess(recv)
		if proc == nil {
			return Nil
		}
		return proc.wait()
	})

	c.AddMethod0(vm.Selectors, "primWait", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		proc := v.getProcess(recv)
		if proc == nil {
			return Nil
		}
		return proc.wait()
	})

	// Process>>isDone - check if process has completed
	c.AddMethod0(vm.Selectors, "isDone", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		proc := v.getProcess(recv)
		if proc == nil {
			return True
		}
		if proc.isDone() {
			return True
		}
		return False
	})

	// Process>>primIsAlive - check if process is still running (opposite of isDone)
	c.AddMethod0(vm.Selectors, "primIsAlive", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod0(vm.Selectors, "result", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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

	c.AddMethod0(vm.Selectors, "primResult", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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

	// Process>>primYield - instance-side yield (stub, returns self)
	c.AddMethod0(vm.Selectors, "primYield", func(_ interface{}, recv Value) Value {
		return recv
	})

	// Process>>primTerminate - terminate process (stub, returns self)
	c.AddMethod0(vm.Selectors, "primTerminate", func(_ interface{}, recv Value) Value {
		return recv
	})

	// Process>>primPriority - get priority (stub, returns 0)
	c.AddMethod0(vm.Selectors, "primPriority", func(_ interface{}, recv Value) Value {
		return FromSmallInt(0)
	})

	// Process>>primPriority: - set priority (stub, returns self)
	c.AddMethod1(vm.Selectors, "primPriority:", func(_ interface{}, recv Value, level Value) Value {
		return recv
	})

	// Process class>>current - get current process (class method)
	c.AddClassMethod0(vm.Selectors, "current", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		return v.currentProcessValue()
	})

	// Process class>>yield - yield to other goroutines (class method)
	c.AddClassMethod0(vm.Selectors, "yield", func(_ interface{}, recv Value) Value {
		// runtime.Gosched() is the Go equivalent
		// For now, just return
		return Nil
	})

	// Process class>>sleep: milliseconds - sleep for specified time (class method)
	c.AddClassMethod1(vm.Selectors, "sleep:", func(_ interface{}, recv Value, ms Value) Value {
		if !ms.IsSmallInt() {
			return recv
		}
		duration := time.Duration(ms.SmallInt()) * time.Millisecond
		time.Sleep(duration)
		return recv
	})

	// Block>>forkRestricted: restrictions - fork with restricted globals
	// restrictions is an Array of class name strings/symbols to hide
	vm.BlockClass.AddMethod1(vm.Selectors, "forkRestricted:", func(vmPtr interface{}, recv Value, restrictionsVal Value) Value {
		v := vmPtr.(*VM)
		bv := v.currentInterpreter().getBlockValue(recv)
		if bv == nil {
			return Nil
		}

		hidden := v.extractHiddenMap(restrictionsVal)
		if hidden == nil {
			return Nil
		}

		// Inherit any existing restrictions from the calling interpreter
		callerInterp := v.currentInterpreter()
		if callerInterp.hidden != nil {
			for name := range callerInterp.hidden {
				hidden[name] = true
			}
		}

		proc := v.createProcess()
		procValue := v.registerProcess(proc)

		go func() {
			defer func() {
				if r := recover(); r != nil {
					if nlr, ok := r.(NonLocalReturn); ok {
						proc.markDone(nlr.Value, nil)
					} else {
						proc.markDone(Nil, nil)
					}
				}
				v.unregisterInterpreter()
			}()

			interp := v.newForkedInterpreter(hidden)
			interp.processID = proc.id
			v.registerInterpreter(interp)
			result := interp.ExecuteBlockDetached(bv.Block, bv.Captures, nil, bv.HomeSelf, bv.HomeMethod)
			proc.markDone(result, nil)
		}()

		return procValue
	})

	// Process class>>forkWithout:do: - fork a block with restrictions (class method)
	// restrictions is an Array of class name strings/symbols to hide
	c.AddClassMethod2(vm.Selectors, "forkWithout:do:", func(vmPtr interface{}, recv Value, restrictionsVal, blockVal Value) Value {
		v := vmPtr.(*VM)
		bv := v.currentInterpreter().getBlockValue(blockVal)
		if bv == nil {
			return Nil
		}

		hidden := v.extractHiddenMap(restrictionsVal)
		if hidden == nil {
			return Nil
		}

		// Inherit any existing restrictions from the calling interpreter
		callerInterp := v.currentInterpreter()
		if callerInterp.hidden != nil {
			for name := range callerInterp.hidden {
				hidden[name] = true
			}
		}

		proc := v.createProcess()
		procValue := v.registerProcess(proc)

		go func() {
			defer func() {
				if r := recover(); r != nil {
					if nlr, ok := r.(NonLocalReturn); ok {
						proc.markDone(nlr.Value, nil)
					} else {
						proc.markDone(Nil, nil)
					}
				}
				v.unregisterInterpreter()
			}()

			interp := v.newForkedInterpreter(hidden)
			interp.processID = proc.id
			v.registerInterpreter(interp)
			result := interp.ExecuteBlockDetached(bv.Block, bv.Captures, nil, bv.HomeSelf, bv.HomeMethod)
			proc.markDone(result, nil)
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
		if elem.IsSymbol() {
			if IsStringValue(elem) {
				name = vm.registry.GetStringContent(elem)
			} else {
				name = vm.Symbols.Name(elem.SymbolID())
			}
		}
		if name != "" {
			hidden[name] = true
		}
	}
	return hidden
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
	vm.Globals["MailboxMessage"] = vm.classValue(vm.MailboxMessageClass)

	// MailboxMessage accessors
	vm.MailboxMessageClass.AddMethod0(vm.Selectors, "sender", func(_ interface{}, recv Value) Value {
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		return obj.GetSlot(0)
	})
	vm.MailboxMessageClass.AddMethod0(vm.Selectors, "selector", func(_ interface{}, recv Value) Value {
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		return obj.GetSlot(1)
	})
	vm.MailboxMessageClass.AddMethod0(vm.Selectors, "payload", func(_ interface{}, recv Value) Value {
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		return obj.GetSlot(2)
	})

	// Process>>primSend: payload — fire-and-forget to target's mailbox
	c.AddMethod1(vm.Selectors, "primSend:", func(vmPtr interface{}, recv Value, payload Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod2(vm.Selectors, "primSend:with:", func(vmPtr interface{}, recv Value, selectorVal, payload Value) Value {
		v := vmPtr.(*VM)
		proc := v.getProcess(recv)
		if proc == nil || proc.mailbox == nil {
			return False
		}
		sel := ""
		if selectorVal.IsSymbol() && !IsStringValue(selectorVal) {
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
	c.AddClassMethod0(vm.Selectors, "primReceive", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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
	c.AddClassMethod1(vm.Selectors, "primReceive:", func(vmPtr interface{}, recv Value, timeoutVal Value) Value {
		v := vmPtr.(*VM)
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
	c.AddClassMethod0(vm.Selectors, "primTryReceive", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod1(vm.Selectors, "primRegisterAs:", func(vmPtr interface{}, recv Value, nameVal Value) Value {
		v := vmPtr.(*VM)
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
	c.AddClassMethod1(vm.Selectors, "primNamed:", func(vmPtr interface{}, recv Value, nameVal Value) Value {
		v := vmPtr.(*VM)
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
	c.AddMethod0(vm.Selectors, "primUnregister", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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
