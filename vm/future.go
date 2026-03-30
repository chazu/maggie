package vm

import (
	"sync"
	"sync/atomic"
)

// FutureObject represents a pending asynchronous result. It wraps a Go
// channel of capacity 1 that receives exactly one value when the result
// is available. Futures are created by asyncSend:with: on RemoteProcess
// and can participate in Channel select: via their underlying Go channel.
type FutureObject struct {
	ch       chan Value   // capacity 1, receives the result exactly once
	result   Value       // cached after first receive
	err      string      // non-empty if the remote side returned an error
	resolved atomic.Bool // true after result is available
	mu       sync.Mutex  // protects result/err
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
	f.resolved.Store(true)
	f.mu.Unlock()
	f.ch <- Nil
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
// NaN-boxing: Future values use promiseMarker (44 << 24)
// ---------------------------------------------------------------------------

func futureToValue(id uint32) Value {
	return FromSymbolID(id | promiseMarker)
}

func isFutureValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	return (v.SymbolID() & markerMask) == promiseMarker
}

func futureIDFromValue(v Value) uint32 {
	return v.SymbolID() & ^uint32(0xFF<<24)
}
