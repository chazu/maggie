package vm

// ---------------------------------------------------------------------------
// Tracing garbage collection for registry-backed heap values.
// ---------------------------------------------------------------------------
//
// Maggie Values are NaN-boxed uint64s, so the Go runtime GC cannot see the
// heap objects they reference: strings, dictionaries, and array-lists live in
// AutoIDRegistries keyed by an integer id, and user objects live in the
// keepAlive set. Nothing ever removed them, so any program that creates many
// distinct strings/dictionaries (e.g. a server re-rendering HTML every tick)
// grew without bound until the OS OOM-killed it.
//
// collectHeapGarbageLocked is a precise mark-sweep collector for those
// registries. It traces from the complete live-root set, descends through
// every container kind, and sweeps the strings / dictionaries / array-lists /
// keepAlive objects that are not reachable.
//
// SAFETY: the collector reads mutable VM state (operand stacks, frames,
// mailboxes, registries) without per-field locking on the hot path. It is
// therefore only correct when no other goroutine is mutating that state —
// i.e. under stop-the-world. Callers MUST hold the GC safepoint barrier
// (see CollectStringGarbage) before invoking the unexported collect* helpers.
// The single-threaded test-suite calls them directly, which is safe because
// the test goroutine is the only mutator.
//
// Trace-and-sweep registries (freed when unreachable):
//   - Strings        (or.Strings)
//   - Dictionaries   (or.Dictionaries)
//   - ArrayLists     (cr.arrayLists)
//   - keepAlive objects (and weak references to them)
//
// Treated as roots (their contents are marked, but the holder is never swept
// here — it is either genuine live execution state or low-volume): every
// interpreter's stack/frames/exception-handlers, globals, class variables,
// loaded method & block literal pools, processes (result + mailbox), blocks
// (captures + home self), cells, contexts, results, exceptions, and extension
// objects that implement RootMarker (e.g. TupleSpace).

// RootMarker is implemented by kindExtension wrapper objects that retain Maggie
// Values the tracing collector cannot otherwise reach through the object graph
// (e.g. a TupleSpace holding tuples in a contrib package the vm package cannot
// import). When mark() reaches such a wrapper via the normal roots it calls
// MarkRoots; implementations MUST enumerate every retained Value, or the
// collector will free values that are still live.
type RootMarker interface {
	MarkRoots(mark func(Value))
}

// liveSet accumulates the reachable ids/pointers for one collection cycle.
type liveSet struct {
	objects    map[*Object]struct{}
	dicts      map[Value]struct{} // visited dicts (cycle guard; not swept)
	arrayLists map[Value]struct{} // visited array-lists (cycle guard; not swept)
	cells      map[*Cell]struct{}
	blocks     map[uint32]struct{}           // reachable block slot ids
	extensions map[*extensionObject]struct{} // visited RootMarker wrappers (cycle guard)
}

func newLiveSet() *liveSet {
	return &liveSet{
		objects:    make(map[*Object]struct{}),
		dicts:      make(map[Value]struct{}),
		arrayLists: make(map[Value]struct{}),
		cells:      make(map[*Cell]struct{}),
		blocks:     make(map[uint32]struct{}),
		extensions: make(map[*extensionObject]struct{}),
	}
}

// mark traces v and everything transitively reachable from it, recording
// reachable strings/dicts/arraylists/objects/cells in ls. It uses an explicit
// worklist rather than recursion so deeply nested or cyclic structures cannot
// blow the Go stack. Cycles are broken by the per-kind "already visited"
// checks.
func (vm *VM) mark(root Value, ls *liveSet) {
	work := []Value{root}
	for len(work) > 0 {
		v := work[len(work)-1]
		work = work[:len(work)-1]

		switch {
		case v.IsObject():
			obj := ObjectFromValue(v)
			if obj == nil {
				continue
			}
			if _, seen := ls.objects[obj]; seen {
				continue
			}
			ls.objects[obj] = struct{}{}
			n := obj.NumSlots()
			for i := 0; i < n; i++ {
				work = append(work, obj.GetSlot(i))
			}

		case IsDictionaryValue(v):
			// Dictionaries are Go-GC-traced pointers now; the custom collector
			// only recurses through them so blocks stored as keys/values stay
			// reachable. A per-cycle visited check is unnecessary since dicts are
			// no longer swept, but cheap identity guards avoid infinite loops on
			// self-referential dicts.
			d := vm.registry.GetDictionaryObject(v)
			if d == nil {
				continue
			}
			if _, seen := ls.dicts[v]; seen {
				continue
			}
			ls.dicts[v] = struct{}{}
			for _, k := range d.Keys {
				work = append(work, k)
			}
			for _, val := range d.Data {
				work = append(work, val)
			}

		case isArrayListValue(v):
			// Pointer-traced by the Go GC; recurse so blocks stored as elements
			// stay reachable for the block sweep.
			al := vm.registry.GetArrayList(v)
			if al == nil {
				continue
			}
			if _, seen := ls.arrayLists[v]; seen {
				continue
			}
			ls.arrayLists[v] = struct{}{}
			work = append(work, al.elements...)

		case v.IsCell():
			cell := v.CellPtr()
			if cell == nil {
				continue
			}
			if _, seen := ls.cells[cell]; seen {
				continue
			}
			ls.cells[cell] = struct{}{}
			work = append(work, cell.value)

		case isResultValue(v):
			if r := vm.registry.GetResultFromValue(v); r != nil {
				work = append(work, r.value)
			}

		case v.IsException():
			if ex := vm.registry.GetExceptionFromValue(v); ex != nil {
				work = append(work, ex.MessageText, ex.Tag, ex.Signaler)
			}

		case v.IsBlock():
			// A reachable block keeps its registry slot live and reaches
			// through its captured variables, home self, and body literals.
			slot := v.BlockID()
			if _, seen := ls.blocks[slot]; seen {
				continue
			}
			bv := vm.registry.GetBlock(v)
			if bv == nil {
				continue // stale Value (slot already recycled)
			}
			ls.blocks[slot] = struct{}{}
			work = append(work, bv.Captures...)
			work = append(work, bv.HomeSelf)
			if bv.Block != nil {
				work = append(work, bv.Block.Literals...)
			}

		case v.isHeap() && v.heapKindOf() == kindExtension:
			// Extension wrappers (e.g. TupleSpace) may retain Maggie Values the
			// object graph cannot otherwise reach — a contrib object the vm
			// package cannot import. If the wrapped object implements RootMarker,
			// enqueue its retained Values so strings/blocks inside stay live.
			e := v.extensionObjectOf()
			if e == nil {
				continue
			}
			if _, seen := ls.extensions[e]; seen {
				continue
			}
			ls.extensions[e] = struct{}{}
			if rm, ok := e.obj.(RootMarker); ok {
				rm.MarkRoots(func(mv Value) { work = append(work, mv) })
			}

			// Symbols, small ints, floats, characters, booleans, nil, classes,
			// bigints, channels, processes: either contain no nested Values or
			// are enumerated directly as roots (processes; detached blocks).
		}
	}
}

// markRoots marks every live root. This set must be COMPLETE: a reachable
// string/dict/arraylist/object that is not reached from here will be swept,
// corrupting the program. Each category has a dedicated regression test.
func (vm *VM) markRoots(ls *liveSet) {
	// 1. Globals.
	vm.globalsMu.RLock()
	for _, v := range vm.globals {
		vm.mark(v, ls)
	}
	vm.globalsMu.RUnlock()

	// 2. Class variables.
	vm.registry.classVarsMu.RLock()
	for _, vars := range vm.registry.classVars {
		for _, v := range vars {
			vm.mark(v, ls)
		}
	}
	vm.registry.classVarsMu.RUnlock()

	// 3. Loaded method & block literal pools (compiled string/symbol literals
	//    are registered strings; they are roots via the loaded classes).
	for _, c := range vm.Classes.All() {
		vm.markClassMethodLiterals(c, ls)
	}

	// 4. Every interpreter (main + forked) — stacks, frames, handlers.
	if vm.interpreter != nil {
		vm.markInterpreter(vm.interpreter, ls)
	}
	vm.interpreters.Range(func(_, v any) bool {
		if interp, ok := v.(*Interpreter); ok && interp != vm.interpreter {
			vm.markInterpreter(interp, ls)
		}
		return true
	})

	// 5. Processes — result value + buffered mailbox messages.
	vm.registry.processesMu.RLock()
	for _, p := range vm.registry.processes {
		if p == nil {
			continue
		}
		vm.mark(p.result, ls)
		if p.mailbox != nil {
			p.mailbox.mu.Lock()
			for i := 0; i < p.mailbox.count; i++ {
				idx := (p.mailbox.head + i) % p.mailbox.capacity
				vm.mark(p.mailbox.buf[idx], ls)
			}
			p.mailbox.mu.Unlock()
		}
	}
	vm.registry.processesMu.RUnlock()

	// 6. Detached blocks (HomeFrame == -1, e.g. forked) are unconditional
	//    roots: they execute on their own goroutine and their registry Value
	//    may not be reachable from any traced mutator root. Frame-bound blocks
	//    (HomeFrame >= 0) are NOT roots — they survive only if the trace
	//    reaches them (from a stack slot, frame, ivar, global, container, …),
	//    so unreachable ones can be swept. This is the same safepoint invariant
	//    that makes string/dictionary sweeping safe: at a safepoint every live
	//    Value is in the operand stack or a frame, both of which are roots.
	vm.registry.blocksMu.RLock()
	for slot, bv := range vm.registry.blocks {
		if bv == nil || bv.HomeFrame != -1 {
			continue
		}
		ls.blocks[uint32(slot)] = struct{}{}
		for _, c := range bv.Captures {
			vm.mark(c, ls)
		}
		vm.mark(bv.HomeSelf, ls)
		vm.markBlockMethodLiterals(bv.Block, ls)
	}
	vm.registry.blocksMu.RUnlock()

	// 7. Cells (over-approximated as roots — low volume).
	vm.registry.cellsMu.RLock()
	for cell := range vm.registry.cells {
		if cell != nil {
			vm.mark(cell.value, ls)
		}
	}
	vm.registry.cellsMu.RUnlock()

	// 8. Contexts (reified activation records — over-approximated as roots).
	vm.registry.Contexts.RLock()
	for _, ctx := range vm.registry.Contexts.data {
		if ctx == nil {
			continue
		}
		vm.mark(ctx.Receiver, ls)
		for _, a := range ctx.Args {
			vm.mark(a, ls)
		}
		for _, t := range ctx.Temps {
			vm.mark(t, ls)
		}
		for _, cap := range ctx.Captures {
			vm.mark(cap, ls)
		}
	}
	vm.registry.Contexts.RUnlock()

	// Results and Exceptions are now pointer-carrying heap Values traced by the
	// Go GC; the custom collector no longer roots them (it still recurses THROUGH
	// them in mark() so blocks captured inside stay reachable).

	// 11. Pinned roots — Values retained in Go-land (e.g. HTTP handler blocks
	//     captured in net/http closures) that no traced root reaches. Marking
	//     the block Value recurses through its captures / home self / literals.
	//     Snapshot under the lock, then mark outside it.
	vm.pinnedRootsMu.Lock()
	pinned := make([]Value, 0, len(vm.pinnedRoots))
	for v := range vm.pinnedRoots {
		pinned = append(pinned, v)
	}
	vm.pinnedRootsMu.Unlock()
	for _, v := range pinned {
		vm.mark(v, ls)
	}

	// 12. Futures — a resolved Future caches its result (and any typed
	//     exception value) in fields that no traced mutator root need reach;
	//     before the consumer calls `await`, the resolved Value lives ONLY in
	//     the Future. Without marking it, the collector would free a still-live
	//     string/dict/object and (because string ids are recycled) later alias
	//     it to a different value. Resolve() also writes f.result before the
	//     channel send, so marking the cached fields covers the buffered value.
	vm.registry.futuresMu.RLock()
	for _, f := range vm.registry.futures {
		if f == nil {
			continue
		}
		f.mu.Lock()
		vm.mark(f.result, ls)
		vm.mark(f.exceptionValue, ls)
		f.mu.Unlock()
	}
	vm.registry.futuresMu.RUnlock()

	// 13. Channel buffered / in-flight values. A Value sitting in a Go channel
	//     buffer is invisible to the trace; each ChannelObject mirrors its
	//     buffered (and mid-flight) values in `pending`, which we mark here.
	//     Without this, a value sent into a buffered channel whose sender has
	//     dropped its reference is collected before the receiver reads it.
	//     Lock order is (channelsMu → ch.mu); no path takes the reverse, so no
	//     deadlock with the STW collector.
	vm.registry.channelsMu.RLock()
	for _, ch := range vm.registry.channels {
		if ch == nil {
			continue
		}
		ch.markPending(vm, ls)
	}
	vm.registry.channelsMu.RUnlock()

	// Extension wrappers (contrib: TupleSpace, …) that retain Maggie Values the
	// trace cannot otherwise reach are no longer enumerated from a registry: they
	// are pointer-carrying kindExtension Values, so mark() reaches them through
	// the normal roots (globals/ivars/stack) and calls their RootMarker there.
}

// markInterpreter marks the live roots held by one interpreter: operand-stack
// slots, per-frame receivers / home-selves, and the installed exception
// handler chain.
func (vm *VM) markInterpreter(i *Interpreter, ls *liveSet) {
	if i == nil {
		return
	}
	for s := 0; s < i.sp; s++ {
		vm.mark(i.stack[s], ls)
	}
	for f := 0; f <= i.fp && f < len(i.frames); f++ {
		fr := &i.frames[f]
		vm.mark(fr.Receiver, ls)
		vm.mark(fr.HomeSelf, ls)
		for _, c := range fr.Captures {
			vm.mark(c, ls)
		}
		// Literal pools of the method/block executing in this frame are roots:
		// the next bytecode may push one. (A doIt method may not live in any
		// class vtable, so this is not redundant with markClassMethodLiterals.)
		vm.markCompiledMethodLiterals(fr.Method, ls)
		vm.markBlockMethodLiterals(fr.Block, ls)
		vm.markCompiledMethodLiterals(fr.HomeMethod, ls)
	}
	for h := i.exceptionHandlers; h != nil; h = h.Prev {
		vm.mark(h.HandlerBlock, ls)
		vm.mark(h.HomeSelf, ls)
		for _, c := range h.Captures {
			vm.mark(c, ls)
		}
	}
	// Values published by a blocking primitive (receiver/args held in Go
	// locals while blocked). Empty unless this interpreter is in enterBlocked.
	for _, r := range i.blockedRoots {
		vm.mark(r, ls)
	}
}

// markCompiledMethodLiterals marks the literal-pool Values of a compiled
// method and the literal pools of its nested blocks.
func (vm *VM) markCompiledMethodLiterals(cm *CompiledMethod, ls *liveSet) {
	if cm == nil {
		return
	}
	for _, lit := range cm.Literals {
		vm.mark(lit, ls)
	}
	for _, blk := range cm.Blocks {
		vm.markBlockMethodLiterals(blk, ls)
	}
}

// markBlockMethodLiterals marks the literal-pool Values of a block method.
func (vm *VM) markBlockMethodLiterals(bm *BlockMethod, ls *liveSet) {
	if bm == nil {
		return
	}
	for _, lit := range bm.Literals {
		vm.mark(lit, ls)
	}
}

// markClassMethodLiterals marks the string/symbol literals embedded in a
// class's compiled methods (and their nested blocks). These are roots: a
// loaded method may push a literal string at any time.
func (vm *VM) markClassMethodLiterals(c *Class, ls *liveSet) {
	if c == nil {
		return
	}
	markMethods := func(vt *VTable) {
		if vt == nil {
			return
		}
		vt.mu.Lock()
		for _, m := range vt.methods {
			if cm, ok := m.(*CompiledMethod); ok {
				vm.markCompiledMethodLiterals(cm, ls)
			}
		}
		vt.mu.Unlock()
	}
	markMethods(c.VTable)
	markMethods(c.ClassVTable)
}

// collectHeapGarbageLocked runs one full mark-sweep cycle over every
// registry-backed heap value: strings, dictionaries, array-lists, frame-bound
// blocks, and keepAlive objects (plus weak references to swept objects). The
// caller MUST guarantee no other goroutine is mutating VM state
// (stop-the-world). Each kind is reachable only through the traced object
// graph — an id/pointer absent from the live set is unreachable and safe to
// reclaim, the same invariant that makes string sweeping safe.
//
// Returns the number of strings, dictionaries, and frame-bound blocks freed.
// (Object and array-list counts are not surfaced; they share the same live
// set and are swept in the same pass.)
func (vm *VM) collectHeapGarbageLocked() (strings, dicts, blocks int) {
	if vm.registry == nil {
		return 0, 0, 0
	}
	ls := newLiveSet()
	vm.markRoots(ls)

	// Strings and dictionaries are pointer-carrying heap Values traced by the Go
	// GC — no custom sweep needed; `strings`/`dicts` stay 0.
	blocks = vm.registry.SweepBlocksLive(ls.blocks)

	// Weak references are Go-native and objects/strings/dicts are Go-GC-traced
	// pointers; the frame-bound block sweep above is the collector's only
	// remaining job (removed entirely once blocks migrate to pointers).
	return strings, dicts, blocks
}
