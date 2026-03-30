package server

import (
	"fmt"

	"github.com/chazu/maggie/vm"
	"github.com/chazu/maggie/vm/dist"
)

// vmRequest represents a unit of work to be executed on the VM goroutine.
type vmRequest struct {
	fn   func(*vm.VM) interface{}
	done chan vmResult
}

// vmResult holds the return value from a VM operation.
type vmResult struct {
	value interface{}
	err   error
}

// VMWorker serializes all VM access through a single goroutine.
// The Maggie interpreter is single-threaded; all gRPC handlers
// must go through the worker to avoid data races.
type VMWorker struct {
	vm       *vm.VM
	requests chan vmRequest
	quit     chan struct{}

	// pullFunc pulls code from a remote node by content hash.
	// Injected by the server wiring layer for code-on-demand.
	pullFunc func(peerID dist.NodeID, hash [32]byte) error

	// spawnResultFunc delivers the result of a forkOn: spawn back
	// to the spawning node. Injected by the server wiring layer.
	spawnResultFunc func(spawnerID dist.NodeID, futureID uint64, resultBytes []byte, errMsg error)
}

// NewVMWorker creates a VMWorker and starts the processing goroutine.
func NewVMWorker(v *vm.VM) *VMWorker {
	w := &VMWorker{
		vm:       v,
		requests: make(chan vmRequest, 64),
		quit:     make(chan struct{}),
	}
	go w.loop()
	return w
}

// loop processes VM requests sequentially on a dedicated goroutine.
func (w *VMWorker) loop() {
	for {
		select {
		case req := <-w.requests:
			result := w.execute(req.fn)
			req.done <- result
		case <-w.quit:
			return
		}
	}
}

// execute runs a function on the VM, recovering from panics.
func (w *VMWorker) execute(fn func(*vm.VM) interface{}) vmResult {
	var result vmResult
	func() {
		defer func() {
			if r := recover(); r != nil {
				result.err = fmt.Errorf("%v", r)
			}
		}()
		result.value = fn(w.vm)
	}()
	return result
}

// Do submits a function for execution on the VM goroutine and blocks
// until it completes. Returns the result and any error (including panics).
func (w *VMWorker) Do(fn func(*vm.VM) interface{}) (interface{}, error) {
	req := vmRequest{
		fn:   fn,
		done: make(chan vmResult, 1),
	}
	w.requests <- req
	result := <-req.done
	return result.value, result.err
}

// Stop shuts down the worker goroutine.
func (w *VMWorker) Stop() {
	close(w.quit)
}

// VM returns the underlying VM (for read-only metadata access that
// doesn't touch interpreter state, like Selectors/Symbols/Classes).
func (w *VMWorker) VM() *vm.VM {
	return w.vm
}
