package vm

import (
	"fmt"
	"sync/atomic"
)

// late_bound.go centralises the "set once at boot, read from many
// goroutines" fields on *VM. Each is wrapped in an atomic.Pointer (or
// CoW pointer-to-map) and has explicit Get/Set accessors. Setters that
// represent boot-time injection points are guarded by Freeze(): once
// the embedding application calls vm.Freeze(), further mutation
// panics, which prevents accidental races between bootstrap-only
// writers and dispatch-time readers.
//
// See docs/vm-concurrency-audit-2026-05-03.md (Patch 3) for rationale.

// holder types — atomic.Pointer requires concrete struct types so that
// nil-vs-zero is distinguishable and function values can be stored.

type samplingProfilerHolder struct{ p *SamplingProfiler }
type localListenAddrHolder struct{ addr string }
type nodeRefFactoryHolder struct{ fn NodeRefFactory }
type remoteChannelFactoryHolder struct{ fn func(*RemoteChannelRef) }
type fileInFuncHolder struct{ fn FileInFunc }
type fileInBatchFuncHolder struct{ fn FileInBatchFunc }
type aotDispatchHolder struct{ table AOTDispatchTable }
type rehydratedClassesHolder struct{ classes map[string]bool }

// lateBoundFields groups all post-bootstrap-but-pre-freeze atomics on
// the VM. Embedded into VM by composition.
type lateBoundFields struct {
	frozen atomic.Bool

	samplingProfiler     atomic.Pointer[samplingProfilerHolder]
	localListenAddr      atomic.Pointer[localListenAddrHolder]
	nodeRefFactory       atomic.Pointer[nodeRefFactoryHolder]
	remoteChannelFactory atomic.Pointer[remoteChannelFactoryHolder]
	fileInFunc           atomic.Pointer[fileInFuncHolder]
	fileInBatchFunc      atomic.Pointer[fileInBatchFuncHolder]
	aotMethods           atomic.Pointer[aotDispatchHolder]
	rehydratedClasses    atomic.Pointer[rehydratedClassesHolder]
}

// Freeze marks the VM as initialised. Subsequent calls to mutators
// guarded by checkNotFrozen will panic. Idempotent.
//
// Frozen state is a one-way door: once frozen, the VM stays frozen for
// its lifetime. This codifies the previously-implicit invariant that
// bootstrap is single-threaded and "set once at boot" fields must not
// change while user goroutines are dispatching.
func (vm *VM) Freeze() {
	vm.frozen.Store(true)
}

// IsFrozen reports whether Freeze has been called.
func (vm *VM) IsFrozen() bool {
	return vm.frozen.Load()
}

// checkNotFrozen panics if the VM has been frozen. Used by setters
// representing boot-time injection points (compiler hooks, AOT table,
// distribution wiring). Always-on (no debug-build gate) so that latent
// concurrency bugs surface in production rather than going silent.
func (vm *VM) checkNotFrozen(op string) {
	if vm.frozen.Load() {
		panic(fmt.Sprintf("vm: %s called after Freeze() — this field is "+
			"set-once at boot and read concurrently from many goroutines; "+
			"mutation post-freeze would race with dispatch", op))
	}
}

// ---------------------------------------------------------------------------
// SamplingProfiler — runtime-mutable (no freeze guard); Start/Stop are
// permitted at any time per user command.
// ---------------------------------------------------------------------------

// SamplingProfiler returns the active profiler, or nil.
func (vm *VM) SamplingProfiler() *SamplingProfiler {
	if h := vm.samplingProfiler.Load(); h != nil {
		return h.p
	}
	return nil
}

// setSamplingProfiler atomically publishes the profiler pointer.
func (vm *VM) setSamplingProfiler(p *SamplingProfiler) {
	if p == nil {
		vm.samplingProfiler.Store(nil)
		return
	}
	vm.samplingProfiler.Store(&samplingProfilerHolder{p: p})
}

// ---------------------------------------------------------------------------
// LocalListenAddr
// ---------------------------------------------------------------------------

// GetLocalListenAddr returns this node's listen address (e.g. ":8081").
func (vm *VM) GetLocalListenAddr() string {
	if h := vm.localListenAddr.Load(); h != nil {
		return h.addr
	}
	return ""
}

// SetLocalListenAddr publishes this node's listen address. Must be
// called before Freeze().
func (vm *VM) SetLocalListenAddr(addr string) {
	vm.checkNotFrozen("SetLocalListenAddr")
	vm.localListenAddr.Store(&localListenAddrHolder{addr: addr})
}

// ---------------------------------------------------------------------------
// NodeRefFactory
// ---------------------------------------------------------------------------

// GetNodeRefFactory returns the registered factory, or nil.
func (vm *VM) GetNodeRefFactory() NodeRefFactory {
	if h := vm.nodeRefFactory.Load(); h != nil {
		return h.fn
	}
	return nil
}

// SetNodeRefFactory publishes the node-ref factory. Must be called
// before Freeze().
func (vm *VM) SetNodeRefFactory(fn NodeRefFactory) {
	vm.checkNotFrozen("SetNodeRefFactory")
	vm.nodeRefFactory.Store(&nodeRefFactoryHolder{fn: fn})
}

// ---------------------------------------------------------------------------
// RemoteChannelFactory
// ---------------------------------------------------------------------------

// GetRemoteChannelFactory returns the registered factory, or nil.
func (vm *VM) GetRemoteChannelFactory() func(*RemoteChannelRef) {
	if h := vm.remoteChannelFactory.Load(); h != nil {
		return h.fn
	}
	return nil
}

// SetRemoteChannelFactory publishes the channel factory. Must be
// called before Freeze().
func (vm *VM) SetRemoteChannelFactory(fn func(*RemoteChannelRef)) {
	vm.checkNotFrozen("SetRemoteChannelFactory")
	vm.remoteChannelFactory.Store(&remoteChannelFactoryHolder{fn: fn})
}

// ---------------------------------------------------------------------------
// FileInFunc / FileInBatchFunc — guarded by freeze. The original
// SetFileInFunc / SetFileInBatchFunc accessors in file_in.go forward
// here so existing callers keep working.
// ---------------------------------------------------------------------------

// getFileInFunc returns the registered function or nil.
func (vm *VM) getFileInFunc() FileInFunc {
	if h := vm.fileInFunc.Load(); h != nil {
		return h.fn
	}
	return nil
}

// setFileInFunc publishes the function. Must be called before Freeze().
func (vm *VM) setFileInFunc(fn FileInFunc) {
	vm.checkNotFrozen("SetFileInFunc")
	vm.fileInFunc.Store(&fileInFuncHolder{fn: fn})
}

func (vm *VM) getFileInBatchFunc() FileInBatchFunc {
	if h := vm.fileInBatchFunc.Load(); h != nil {
		return h.fn
	}
	return nil
}

func (vm *VM) setFileInBatchFunc(fn FileInBatchFunc) {
	vm.checkNotFrozen("SetFileInBatchFunc")
	vm.fileInBatchFunc.Store(&fileInBatchFuncHolder{fn: fn})
}

// ---------------------------------------------------------------------------
// AOT methods — guarded by freeze. Published as a copy-on-write map
// so dispatch-time readers see a stable snapshot with no locking.
// ---------------------------------------------------------------------------

// getAOTMethods returns the current AOT dispatch table or nil.
func (vm *VM) getAOTMethods() AOTDispatchTable {
	if h := vm.aotMethods.Load(); h != nil {
		return h.table
	}
	return nil
}

// registerAOTMethods merges the supplied methods into the AOT table.
// Builds a fresh snapshot under copy-on-write and CAS-publishes.
// Always-on freeze guard: must be called before Freeze().
func (vm *VM) registerAOTMethods(methods AOTDispatchTable) {
	vm.checkNotFrozen("RegisterAOTMethods")
	for {
		old := vm.aotMethods.Load()
		var oldTable AOTDispatchTable
		if old != nil {
			oldTable = old.table
		}
		next := make(AOTDispatchTable, len(oldTable)+len(methods))
		for k, v := range oldTable {
			next[k] = v
		}
		for k, v := range methods {
			next[k] = v
		}
		if vm.aotMethods.CompareAndSwap(old, &aotDispatchHolder{table: next}) {
			return
		}
	}
}

// resetAOTMethods clears the AOT table. Used by tests.
func (vm *VM) resetAOTMethods() {
	vm.aotMethods.Store(nil)
}

// ---------------------------------------------------------------------------
// RehydratedClasses — runtime-mutable map; copy-on-write so dispatch
// readers (IsRehydrated) are lock-free.
// ---------------------------------------------------------------------------

// markRehydrated records a class as having been installed via the
// sync/rehydration pipeline. CoW: reads do not block writes.
func (vm *VM) markRehydrated(name string) {
	for {
		old := vm.rehydratedClasses.Load()
		var oldMap map[string]bool
		if old != nil {
			oldMap = old.classes
		}
		if oldMap[name] {
			return
		}
		next := make(map[string]bool, len(oldMap)+1)
		for k, v := range oldMap {
			next[k] = v
		}
		next[name] = true
		if vm.rehydratedClasses.CompareAndSwap(old, &rehydratedClassesHolder{classes: next}) {
			return
		}
	}
}

// isRehydrated reports whether a class was installed via rehydration.
func (vm *VM) isRehydrated(name string) bool {
	h := vm.rehydratedClasses.Load()
	if h == nil {
		return false
	}
	return h.classes[name]
}
