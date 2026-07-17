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
	ch             chan Value    // capacity 1, for GoChan/select integration (one-shot)
	done           chan struct{} // closed once on resolve — broadcasts to all awaiters
	result         Value         // cached after resolve
	err            string        // non-empty if the remote side returned an error
	exceptionValue Value         // typed exception value (if remote raised a Maggie exception)
	resolved       atomic.Bool   // true after result is available
	mu             sync.Mutex    // protects result/err/exceptionValue
}

// NewFuture creates an unresolved Future.
func NewFuture() *FutureObject {
	return &FutureObject{
		ch:   make(chan Value, 1),
		done: make(chan struct{}),
	}
}

// publish marks the Future resolved exactly once: it delivers chVal to the
// select-integration channel and closes done to wake every awaiter. Callers
// set result/err/exceptionValue under mu before calling. A duplicate resolve
// is ignored (rather than panicking on a second send/close).
func (f *FutureObject) publish(chVal Value) {
	if !f.resolved.CompareAndSwap(false, true) {
		return
	}
	f.ch <- chVal
	close(f.done)
}

// Resolve writes a successful result. Must be called exactly once.
func (f *FutureObject) Resolve(val Value) {
	f.mu.Lock()
	f.result = val
	f.mu.Unlock()
	f.publish(val)
}

// ResolveError writes an error result. Must be called exactly once.
func (f *FutureObject) ResolveError(errMsg string) {
	f.mu.Lock()
	f.err = errMsg
	f.result = Nil
	f.exceptionValue = Nil
	f.mu.Unlock()
	f.publish(Nil)
}

// ResolveException writes an error result with a typed exception value.
// Must be called exactly once. The exVal should be a deserialized exception
// Value that can be re-signaled on the receiving VM.
func (f *FutureObject) ResolveException(exVal Value, errMsg string) {
	f.mu.Lock()
	f.err = errMsg
	f.result = Nil
	f.exceptionValue = exVal
	f.mu.Unlock()
	f.publish(Nil)
}

// Done returns a channel closed when the Future resolves. Reading it is
// idempotent (a closed channel never blocks), so await is re-entrant and wakes
// all waiters — unlike the one-shot ch.
func (f *FutureObject) Done() <-chan struct{} { return f.done }

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
// GC; the object is reclaimed when the last reference drops.
// ---------------------------------------------------------------------------

func futureToValue(f *FutureObject) Value {
	return makeHeap(kindFuture, unsafe.Pointer(f))
}

func isFutureValue(v Value) bool {
	return v.ptr != nil && v.hi == kindFuture
}
