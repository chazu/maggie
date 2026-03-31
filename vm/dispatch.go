package vm

// ---------------------------------------------------------------------------
// VM Work Dispatch Queue
//
// The Maggie VM is single-threaded by design — its internal data structures
// (VTable, keepAlive, Globals, etc.) are not safe for concurrent access.
// When external Go goroutines (e.g., HTTP handlers, gRPC handlers) need to
// run Maggie code, they must dispatch work through this queue so it executes
// on the VM's dedicated goroutine.
//
// Usage:
//
//	result := vm.Dispatch(func() Value {
//	    return interp.ExecuteBlockDetached(block, captures, args, self, method)
//	})
//
// The calling goroutine blocks until the work completes and the result is
// returned. Panics inside the work function are recovered and re-panicked
// on the caller's goroutine.
// ---------------------------------------------------------------------------

// workItem is a unit of work to be executed on the VM goroutine.
type workItem struct {
	fn       func() Value
	resultCh chan workResult
}

// workResult carries the result of a dispatched work item.
type workResult struct {
	value Value
	panic interface{} // non-nil if the work function panicked
}

// StartDispatcher starts the VM's work dispatch goroutine. Must be called
// before any calls to Dispatch. Safe to call multiple times (subsequent
// calls are no-ops).
func (vm *VM) StartDispatcher() {
	vm.dispatchOnce.Do(func() {
		vm.dispatchQueue = make(chan workItem, 64)
		go vm.dispatchLoop()
	})
}

// StopDispatcher shuts down the dispatch goroutine. Safe to call if the
// dispatcher was never started.
func (vm *VM) StopDispatcher() {
	if vm.dispatchQueue != nil {
		close(vm.dispatchQueue)
	}
}

// Dispatch submits a function to run on the VM's goroutine and blocks
// until it completes, returning the result. If the function panics,
// the panic is re-raised on the caller's goroutine.
//
// If the dispatcher is not running, the function is executed directly
// on the caller's goroutine (fallback for single-threaded use).
func (vm *VM) Dispatch(fn func() Value) Value {
	if vm.dispatchQueue == nil {
		// No dispatcher — run inline (single-threaded mode)
		return fn()
	}

	item := workItem{
		fn:       fn,
		resultCh: make(chan workResult, 1),
	}
	vm.dispatchQueue <- item
	res := <-item.resultCh
	if res.panic != nil {
		panic(res.panic)
	}
	return res.value
}

// dispatchLoop is the VM's work execution goroutine. It processes work
// items sequentially, ensuring only one piece of Maggie code runs at a time.
func (vm *VM) dispatchLoop() {
	for item := range vm.dispatchQueue {
		result := vm.executeWorkItem(item.fn)
		item.resultCh <- result
	}
}

// executeWorkItem runs a work function with panic recovery.
func (vm *VM) executeWorkItem(fn func() Value) (result workResult) {
	defer func() {
		if r := recover(); r != nil {
			result = workResult{panic: r}
		}
	}()
	return workResult{value: fn()}
}
