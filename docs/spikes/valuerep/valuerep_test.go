// Package valuerep is a THROWAWAY measurement spike for the review's #1
// question: should Maggie's `Value` stay a NaN-boxed uint64 (heap objects in
// lock+map side registries, invisible to Go's GC) or become a pointer-carrying
// value the Go runtime can trace?
//
// It is NOT the VM. It models the two representations' hot-path costs in
// isolation so the trade-off can be decided on numbers:
//
//	go test -run=^$ -bench=. -benchmem ./docs/spikes/valuerep/
//
// See ../2026-07-04-value-representation.md for captured results + analysis.
package valuerep

import (
	"sync"
	"sync/atomic"
	"testing"
	"unsafe"
)

// heapObj stands in for any registry-backed heap value (a String, Dictionary,
// ArrayList, …): a small struct with a couple of fields.
type heapObj struct {
	class int32
	slots [3]uint64
}

// ---------------------------------------------------------------------------
// Model A — current design: Value is a uint64; heap objects live in a
// lock-guarded map keyed by a 24-bit id (mirrors vm/typed_registry.go
// AutoIDRegistry: RWMutex + map + monotonic id, no Go-GC visibility).
// ---------------------------------------------------------------------------

type nanValue uint64

const nanHeapTag uint64 = 1 << 48 // marks "this uint64 is a heap id, not a scalar"

type nanRegistry struct {
	mu   sync.RWMutex
	m    map[uint32]*heapObj
	next atomic.Uint32
}

func newNanRegistry() *nanRegistry {
	return &nanRegistry{m: make(map[uint32]*heapObj, 1<<16)}
}

func (r *nanRegistry) alloc(o *heapObj) nanValue {
	id := r.next.Add(1)
	r.mu.Lock()
	r.m[id] = o
	r.mu.Unlock()
	return nanValue(nanHeapTag | uint64(id))
}

func (r *nanRegistry) get(v nanValue) *heapObj {
	if uint64(v)&nanHeapTag == 0 {
		return nil
	}
	id := uint32(uint64(v) & 0xFFFFFF)
	r.mu.RLock()
	o := r.m[id]
	r.mu.RUnlock()
	return o
}

func nanScalar(x int64) nanValue   { return nanValue(uint64(x) & 0xFFFFFFFFFFFF) }
func (v nanValue) asScalar() int64 { return int64(uint64(v) & 0xFFFFFFFFFFFF) }

// ---------------------------------------------------------------------------
// Model B — proposed: Value carries a real pointer the Go GC traces. Scalars
// stay inline; heap objects are a direct *heapObj. No registry, no lock, no
// custom collector.
// ---------------------------------------------------------------------------

type ptrValue struct {
	scalar uint64
	ptr    unsafe.Pointer // *heapObj, or nil for a scalar
}

func ptrScalar(x int64) ptrValue   { return ptrValue{scalar: uint64(x)} }
func ptrHeap(o *heapObj) ptrValue  { return ptrValue{ptr: unsafe.Pointer(o)} }
func (v ptrValue) asScalar() int64 { return int64(v.scalar) }
func (v ptrValue) get() *heapObj   { return (*heapObj)(v.ptr) }

// ===========================================================================
// Benchmarks
// ===========================================================================

// --- Scalar arithmetic: both keep scalars inline; expected ~equal. ---

func BenchmarkScalarAdd_Nan(b *testing.B) {
	a, c := nanScalar(100), nanScalar(200)
	var s int64
	for i := 0; i < b.N; i++ {
		s += a.asScalar() + c.asScalar()
	}
	_ = s
}

func BenchmarkScalarAdd_Ptr(b *testing.B) {
	a, c := ptrScalar(100), ptrScalar(200)
	var s int64
	for i := 0; i < b.N; i++ {
		s += a.asScalar() + c.asScalar()
	}
	_ = s
}

// --- Heap-object access: the per-operation tax. Nan pays a RWMutex + map
// lookup on EVERY read of a string/dict/collection; Ptr is a pointer deref. ---

func BenchmarkHeapAccess_Nan(b *testing.B) {
	r := newNanRegistry()
	v := r.alloc(&heapObj{class: 7})
	var acc int32
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		acc += r.get(v).class
	}
	_ = acc
}

func BenchmarkHeapAccess_Ptr(b *testing.B) {
	v := ptrHeap(&heapObj{class: 7})
	var acc int32
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		acc += v.get().class
	}
	_ = acc
}

// --- Concurrent heap access: the contention that forces the single-VM-goroutine
// server (spike #2). Nan's RWMutex serializes readers under writes; Ptr scales. ---

func BenchmarkHeapAccessParallel_Nan(b *testing.B) {
	r := newNanRegistry()
	v := r.alloc(&heapObj{class: 7})
	b.RunParallel(func(pb *testing.PB) {
		var acc int32
		for pb.Next() {
			acc += r.get(v).class
		}
		_ = acc
	})
}

func BenchmarkHeapAccessParallel_Ptr(b *testing.B) {
	v := ptrHeap(&heapObj{class: 7})
	b.RunParallel(func(pb *testing.PB) {
		var acc int32
		for pb.Next() {
			acc += v.get().class
		}
		_ = acc
	})
}

// --- Allocation: Nan does lock + map insert + (elsewhere) needs the custom
// sweep to ever reclaim; Ptr is a plain heap alloc the Go GC reclaims for free. ---

func BenchmarkAllocHeap_Nan(b *testing.B) {
	r := newNanRegistry()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = r.alloc(&heapObj{class: int32(i)})
	}
}

func BenchmarkAllocHeap_Ptr(b *testing.B) {
	var sink ptrValue
	for i := 0; i < b.N; i++ {
		sink = ptrHeap(&heapObj{class: int32(i)})
	}
	_ = sink
}

// --- Value copy / size: the cost of a 16-byte value vs 8-byte (passed by value
// constantly on the operand stack). ---

func BenchmarkCopyValue_Nan(b *testing.B) {
	stack := make([]nanValue, 64)
	v := nanScalar(42)
	for i := 0; i < b.N; i++ {
		for j := range stack {
			stack[j] = v
		}
	}
	_ = stack
}

func BenchmarkCopyValue_Ptr(b *testing.B) {
	stack := make([]ptrValue, 64)
	v := ptrScalar(42)
	for i := 0; i < b.N; i++ {
		for j := range stack {
			stack[j] = v
		}
	}
	_ = stack
}

// TestSizes documents the memory footprint difference (informational).
func TestSizes(t *testing.T) {
	t.Logf("sizeof(nanValue) = %d bytes", unsafe.Sizeof(nanValue(0)))
	t.Logf("sizeof(ptrValue) = %d bytes", unsafe.Sizeof(ptrValue{}))
}
