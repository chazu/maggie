package vm

import (
	"sync"
	"sync/atomic"
	"time"
)

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

// channelRegistry stores active channels (temporary solution until proper object system)
var channelRegistry = make(map[int]*ChannelObject)
var channelRegistryMu sync.Mutex
var nextChannelID = 1

func createChannel(buffered int) *ChannelObject {
	ch := &ChannelObject{}
	if buffered > 0 {
		ch.ch = make(chan Value, buffered)
	} else {
		ch.ch = make(chan Value)
	}
	return ch
}

func registerChannel(ch *ChannelObject) Value {
	channelRegistryMu.Lock()
	defer channelRegistryMu.Unlock()

	id := nextChannelID
	nextChannelID++
	channelRegistry[id] = ch

	// Use a special tag to identify channels
	// We'll use a symbol-like encoding with a high bit pattern
	return channelToValue(id)
}

const channelMarker uint32 = 1 << 24

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

func getChannel(v Value) *ChannelObject {
	if !isChannelValue(v) {
		return nil
	}
	id := int(v.SymbolID() & ^uint32(0xFF<<24))

	channelRegistryMu.Lock()
	defer channelRegistryMu.Unlock()
	return channelRegistry[id]
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
}

// processRegistry stores active processes
var processRegistry = make(map[uint64]*ProcessObject)
var processRegistryMu sync.RWMutex
var nextProcessID uint64 = 1

// currentProcess stores the current process for each goroutine
var currentProcessKey = struct{}{}

func createProcess() *ProcessObject {
	id := atomic.AddUint64(&nextProcessID, 1) - 1
	proc := &ProcessObject{
		id:   id,
		done: make(chan struct{}),
	}
	proc.state.Store(int32(ProcessRunning))
	proc.waitGroup.Add(1)

	processRegistryMu.Lock()
	processRegistry[id] = proc
	processRegistryMu.Unlock()

	return proc
}

func registerProcess(proc *ProcessObject) Value {
	// Encode process ID similar to channels
	return processToValue(proc.id)
}

const processMarker uint32 = 2 << 24

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

func getProcess(v Value) *ProcessObject {
	if !isProcessValue(v) {
		return nil
	}
	id := uint64(v.SymbolID() & ^uint32(0xFF<<24))

	processRegistryMu.RLock()
	defer processRegistryMu.RUnlock()
	return processRegistry[id]
}

func (p *ProcessObject) markDone(result Value, err error) {
	p.mu.Lock()
	p.result = result
	p.err = err
	p.state.Store(int32(ProcessTerminated))
	p.mu.Unlock()
	p.waitGroup.Done()
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
	c.AddClassMethod0(vm.Selectors, "new", func(_ interface{}, recv Value) Value {
		ch := createChannel(0)
		return registerChannel(ch)
	})

	// Channel class>>new: size - create buffered channel (class method)
	c.AddClassMethod1(vm.Selectors, "new:", func(_ interface{}, recv Value, size Value) Value {
		if !size.IsSmallInt() {
			return Nil
		}
		bufSize := int(size.SmallInt())
		if bufSize < 0 {
			bufSize = 0
		}
		ch := createChannel(bufSize)
		return registerChannel(ch)
	})

	// Channel>>primSend: value - send value to channel (blocking)
	c.AddMethod1(vm.Selectors, "primSend:", func(_ interface{}, recv Value, val Value) Value {
		ch := getChannel(recv)
		if ch == nil {
			return Nil
		}
		if ch.closed.Load() {
			return Nil // Can't send to closed channel
		}
		ch.ch <- val
		return recv
	})

	// Channel>>primReceive - receive value from channel (blocking)
	c.AddMethod0(vm.Selectors, "primReceive", func(_ interface{}, recv Value) Value {
		ch := getChannel(recv)
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
	c.AddMethod0(vm.Selectors, "primTryReceive", func(_ interface{}, recv Value) Value {
		ch := getChannel(recv)
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
	c.AddMethod1(vm.Selectors, "primTrySend:", func(_ interface{}, recv Value, val Value) Value {
		ch := getChannel(recv)
		if ch == nil {
			return False
		}
		if ch.closed.Load() {
			return False
		}
		select {
		case ch.ch <- val:
			return True
		default:
			return False
		}
	})

	// Channel>>primClose - close the channel
	c.AddMethod0(vm.Selectors, "primClose", func(_ interface{}, recv Value) Value {
		ch := getChannel(recv)
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
	c.AddMethod0(vm.Selectors, "primIsClosed", func(_ interface{}, recv Value) Value {
		ch := getChannel(recv)
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
	c.AddMethod1(vm.Selectors, "send:", func(_ interface{}, recv Value, val Value) Value {
		ch := getChannel(recv)
		if ch == nil {
			return Nil
		}
		if ch.closed.Load() {
			return Nil
		}
		ch.ch <- val
		return recv
	})

	// Channel>>receive - alias for primReceive
	c.AddMethod0(vm.Selectors, "receive", func(_ interface{}, recv Value) Value {
		ch := getChannel(recv)
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
	c.AddMethod1(vm.Selectors, "trySend:", func(_ interface{}, recv Value, val Value) Value {
		ch := getChannel(recv)
		if ch == nil {
			return False
		}
		if ch.closed.Load() {
			return False
		}
		select {
		case ch.ch <- val:
			return True
		default:
			return False
		}
	})

	// Channel>>tryReceive - alias for primTryReceive
	c.AddMethod0(vm.Selectors, "tryReceive", func(_ interface{}, recv Value) Value {
		ch := getChannel(recv)
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
	c.AddMethod0(vm.Selectors, "close", func(_ interface{}, recv Value) Value {
		ch := getChannel(recv)
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
	c.AddMethod0(vm.Selectors, "isClosed", func(_ interface{}, recv Value) Value {
		ch := getChannel(recv)
		if ch == nil {
			return True
		}
		if ch.closed.Load() {
			return True
		}
		return False
	})

	// Channel>>isEmpty - check if channel has no pending values (for buffered)
	c.AddMethod0(vm.Selectors, "isEmpty", func(_ interface{}, recv Value) Value {
		ch := getChannel(recv)
		if ch == nil {
			return True
		}
		if len(ch.ch) == 0 {
			return True
		}
		return False
	})

	// Channel>>size - number of pending values (for buffered)
	c.AddMethod0(vm.Selectors, "size", func(_ interface{}, recv Value) Value {
		ch := getChannel(recv)
		if ch == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(len(ch.ch)))
	})

	// Channel>>capacity - buffer capacity
	c.AddMethod0(vm.Selectors, "capacity", func(_ interface{}, recv Value) Value {
		ch := getChannel(recv)
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
	vm.BlockClass.AddMethod0(vm.Selectors, "fork", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		bv := v.currentInterpreter().getBlockValue(recv)
		if bv == nil {
			return Nil
		}

		proc := createProcess()
		procValue := registerProcess(proc)

		go func() {
			defer func() {
				if r := recover(); r != nil {
					// Process crashed
					proc.markDone(Nil, nil)
				}
				// Unregister the interpreter when done
				v.unregisterInterpreter()
			}()

			// Create a new interpreter for this goroutine
			interp := v.newInterpreter()
			// Register this interpreter for the current goroutine
			v.registerInterpreter(interp)
			result := interp.ExecuteBlock(bv.Block, bv.Captures, nil, bv.HomeFrame, bv.HomeSelf, bv.HomeMethod)
			proc.markDone(result, nil)
		}()

		return procValue
	})

	// Block>>forkWith: arg - fork with single argument
	vm.BlockClass.AddMethod1(vm.Selectors, "forkWith:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		bv := v.currentInterpreter().getBlockValue(recv)
		if bv == nil {
			return Nil
		}

		proc := createProcess()
		procValue := registerProcess(proc)

		go func() {
			defer func() {
				if r := recover(); r != nil {
					proc.markDone(Nil, nil)
				}
				v.unregisterInterpreter()
			}()

			interp := v.newInterpreter()
			v.registerInterpreter(interp)
			result := interp.ExecuteBlock(bv.Block, bv.Captures, []Value{arg}, bv.HomeFrame, bv.HomeSelf, bv.HomeMethod)
			proc.markDone(result, nil)
		}()

		return procValue
	})

	c := vm.ProcessClass

	// Process class>>fork: block - fork a block as a new process (class method)
	c.AddClassMethod1(vm.Selectors, "fork:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		bv := v.currentInterpreter().getBlockValue(block)
		if bv == nil {
			return Nil
		}

		proc := createProcess()
		procValue := registerProcess(proc)

		go func() {
			defer func() {
				if r := recover(); r != nil {
					proc.markDone(Nil, nil)
				}
				v.unregisterInterpreter()
			}()

			interp := v.newInterpreter()
			v.registerInterpreter(interp)
			result := interp.ExecuteBlock(bv.Block, bv.Captures, nil, bv.HomeFrame, bv.HomeSelf, bv.HomeMethod)
			proc.markDone(result, nil)
		}()

		return procValue
	})

	// Process>>wait - wait for process to complete, return result
	c.AddMethod0(vm.Selectors, "wait", func(_ interface{}, recv Value) Value {
		proc := getProcess(recv)
		if proc == nil {
			return Nil
		}
		return proc.wait()
	})

	// Process>>isDone - check if process has completed
	c.AddMethod0(vm.Selectors, "isDone", func(_ interface{}, recv Value) Value {
		proc := getProcess(recv)
		if proc == nil {
			return True
		}
		if proc.isDone() {
			return True
		}
		return False
	})

	// Process>>result - get result (nil if not done)
	c.AddMethod0(vm.Selectors, "result", func(_ interface{}, recv Value) Value {
		proc := getProcess(recv)
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

	// Process class>>current - get current process (placeholder for now) (class method)
	c.AddClassMethod0(vm.Selectors, "current", func(_ interface{}, recv Value) Value {
		// In a full implementation, we'd track the current process per goroutine
		return Nil
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
}
