package vm

import (
	"sync"
	"sync/atomic"
	"unsafe"
)

// FutureObject represents a pending asynchronous result. It wraps a Go
// channel of capacity 1 that receives exactly one value when the result
// is available. Futures are created by asyncSend:with: on RemoteProcess
// and can participate in Channel select: via their underlying Go channel.
type FutureObject struct {
	ch             chan Value   // capacity 1, receives the result exactly once
	result         Value       // cached after first receive
	err            string      // non-empty if the remote side returned an error
	exceptionValue Value       // typed exception value (if remote raised a Maggie exception)
	resolved       atomic.Bool // true after result is available
	mu             sync.Mutex  // protects result/err/exceptionValue
}

// NewFuture creates an unresolved Future.
func NewFuture() *FutureObject {
	return &FutureObject{
		ch: make(chan Value, 1),
	}
}

// Resolve writes a successful result. Must be called exactly once.
func (f *FutureObject) Resolve(val Value) {
	f.mu.Lock()
	f.result = val
	f.resolved.Store(true)
	f.mu.Unlock()
	f.ch <- val
}

// ResolveError writes an error result. Must be called exactly once.
func (f *FutureObject) ResolveError(errMsg string) {
	f.mu.Lock()
	f.err = errMsg
	f.result = Nil
	f.exceptionValue = Nil
	f.resolved.Store(true)
	f.mu.Unlock()
	f.ch <- Nil
}

// ResolveException writes an error result with a typed exception value.
// Must be called exactly once. The exVal should be a deserialized exception
// Value that can be re-signaled on the receiving VM.
func (f *FutureObject) ResolveException(exVal Value, errMsg string) {
	f.mu.Lock()
	f.err = errMsg
	f.result = Nil
	f.exceptionValue = exVal
	f.resolved.Store(true)
	f.mu.Unlock()
	f.ch <- Nil
}

// ExceptionValue returns the typed exception value, or Nil if none.
func (f *FutureObject) ExceptionValue() Value {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.exceptionValue
}

// GoChan returns the underlying Go channel for use with reflect.Select
// (Channel select: integration).
func (f *FutureObject) GoChan() <-chan Value { return f.ch }

// IsResolved returns true if the Future has been resolved (success or error).
func (f *FutureObject) IsResolved() bool { return f.resolved.Load() }

// Result returns the cached result, or Nil if not yet resolved.
func (f *FutureObject) Result() Value {
	if !f.resolved.Load() {
		return Nil
	}
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.result
}

// Error returns the error message, or "" if no error.
func (f *FutureObject) Error() string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.err
}

// ---------------------------------------------------------------------------
// Future values are pointer-carrying heap Values (kindFuture) traced by the Go
// GC. The concurrency registry keeps an id→future map only so the custom
// collector can mark each future's buffered result/exceptionValue (block
// liveness) until blocks migrate in stage 3.
// ---------------------------------------------------------------------------

func futureToValue(f *FutureObject) Value {
	return makeHeap(kindFuture, unsafe.Pointer(f))
}

func isFutureValue(v Value) bool {
	return v.ptr != nil && v.hi == kindFuture
}
