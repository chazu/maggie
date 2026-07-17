package server

import (
	"fmt"
	"sync"

	"github.com/chazu/maggie/vm"
	"github.com/chazu/maggie/vm/dist"
)

// VMWorker is the server's gate to the VM. Historically it serialized ALL VM
// access through one goroutine because the pre-migration heap lived in
// lock+map side registries that were not safe for concurrent mutators. After
// the pointer-value migration the heap is Go-GC-traced and dispatch is
// race-clean (concurrency audit Patches 1-6), so requests now run concurrently
// on per-request interpreters (vm.RunIsolated) coordinated by a single
// reader/writer gate:
//
//   - Do          — EXCLUSIVE (writer): serialized against every other caller.
//                   For requests that mutate shared VM structure (define/modify
//                   classes & methods, write globals, save the image).
//   - DoConcurrent — SHARED (reader): many run in parallel, excluded only by an
//                   in-flight exclusive Do. For read-only / inspect / eval
//                   requests. Compilation of expression source is safe under a
//                   read lock (proven race-clean in server/concurrent_eval_test.go);
//                   the shared Selectors/Symbols/registry/ClassTable each carry
//                   their own locks.
//
// Both paths recover panics (unhandled Maggie exceptions) into an error string
// via VM.DescribePanic, exactly as the old single-goroutine path did.
//
// GATE BOUNDARY (declared, SD-8): the gate governs the IDE request surface —
// modify_service (all writers use Do), browse/inspect/eval services (readers
// use DoConcurrent, evals DoForSource). The following run OUTSIDE the gate by
// design and are NOT serialized against Do:
//
//   - the sync/distribution surface (SyncService handlers: message delivery,
//     remote spawns, channel RPCs) — long-blocking channel operations must
//     not pin a read lock and starve writers, and spawned blocks run on
//     their own goroutines regardless. These handlers carry their own panic
//     recovery.
//   - locally forked Maggie processes and HTTP/SSE handler blocks.
//
// Consequently Do's exclusivity is a consistency device for the IDE surface
// (a class-defining eval will not interleave with a browse), NOT a
// stop-the-world: an image save excludes sibling IDE requests, while forked
// processes and remote deliveries proceed. Memory safety never depends on
// the gate — the pointer heap and per-structure locks provide it.
type VMWorker struct {
	vm *vm.VM

	// gate coordinates concurrent readers (DoConcurrent) against exclusive
	// writers (Do). It does not itself make VM state thread-safe — the pointer
	// heap + audit patches do; the gate serializes structural mutation
	// (class/method/global definition) against readers.
	gate sync.RWMutex

	// pullFunc pulls code from a remote node by content hash.
	// Injected by the server wiring layer for code-on-demand.
	pullFunc func(peerID dist.NodeID, hash [32]byte) error

	// spawnResultFunc delivers the result of a forkOn: spawn back
	// to the spawning node. Injected by the server wiring layer.
	spawnResultFunc func(spawnerID dist.NodeID, futureID uint64, resultBytes []byte, errMsg error, exceptionBytes []byte)

	// peerAddrs maps NodeID -> address for peers that have sent us
	// requests. Used by code-on-demand to call back to the spawning
	// node's Serve RPC. May be nil.
	peerAddrs *sync.Map
}

// NewVMWorker creates a VMWorker gating access to v.
func NewVMWorker(v *vm.VM) *VMWorker {
	return &VMWorker{vm: v}
}

// Do runs fn under EXCLUSIVE access: it is serialized against all other Do and
// DoConcurrent callers. Use for requests that mutate shared VM structure
// (define/remove methods, create classes, write globals, save the image).
// Returns fn's result and any error, including a recovered Maggie exception.
func (w *VMWorker) Do(fn func(*vm.VM) interface{}) (interface{}, error) {
	w.gate.Lock()
	defer w.gate.Unlock()
	return w.run(fn)
}

// DoConcurrent runs fn under SHARED access: many DoConcurrent calls proceed in
// parallel, excluded only while an exclusive Do holds the gate. Use for
// read-only / inspect / eval requests that do not structurally mutate the VM.
func (w *VMWorker) DoConcurrent(fn func(*vm.VM) interface{}) (interface{}, error) {
	w.gate.RLock()
	defer w.gate.RUnlock()
	return w.run(fn)
}

// DoForSource runs fn under the gate appropriate for evaluated user source:
// EXCLUSIVE (Do) when the source might structurally mutate the VM — define or
// redefine classes/methods, write globals, file in code, save the image —
// otherwise SHARED (DoConcurrent). Use for Evaluate-style requests that
// compile and execute arbitrary user source, so a class-defining eval is
// serialized against readers and other writers while ordinary expression
// evals stay parallel.
//
// Classification is two-layered: the selector text check
// (vm.SourceMayMutateSchema) catches the reflective family (evaluate:,
// setGlobal:, fileIn:, …), and a classification compile catches the plain
// global assignment `X := 42` by scanning the compiled doIt for a
// global-store opcode (vm.MethodStoresGlobal) — assignment syntax has no
// selector for the text check to see. A source that fails to compile
// classifies as SHARED; the handler surfaces the compile error itself.
// Memory safety does not depend on any of this (see SourceMayMutateSchema);
// the gate buys consistency.
func (w *VMWorker) DoForSource(source string, fn func(*vm.VM) interface{}) (interface{}, error) {
	if vm.SourceMayMutateSchema(source) {
		return w.Do(fn)
	}
	if m, err := w.vm.CompileExpression(source); err == nil && vm.MethodStoresGlobal(m) {
		return w.Do(fn)
	}
	return w.DoConcurrent(fn)
}

// run executes fn on a fresh per-request interpreter (so vm.Send/Execute
// resolve to it, not the shared main interpreter), recovering panics into an
// error. The caller holds the gate in the appropriate mode.
func (w *VMWorker) run(fn func(*vm.VM) interface{}) (result interface{}, err error) {
	w.vm.RunIsolated(func() {
		defer func() {
			if r := recover(); r != nil {
				// Render an unhandled Maggie exception as its message rather
				// than the raw NaN-boxed struct — this is the error string the
				// IDE/agent Evaluate path surfaces.
				err = fmt.Errorf("%s", w.vm.DescribePanic(r))
			}
		}()
		result = fn(w.vm)
	})
	return result, err
}

// Stop is retained for API compatibility; the worker no longer owns a
// goroutine, so there is nothing to shut down.
func (w *VMWorker) Stop() {}

// VM returns the underlying VM. Callers bypass the gate entirely: the sync
// surface uses this for deserialization, mailbox delivery, and spawn
// execution (see GATE BOUNDARY above). Callers must not assume they are
// serialized against Do writers — per-structure locks are what protect them.
func (w *VMWorker) VM() *vm.VM {
	return w.vm
}
