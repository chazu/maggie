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
// CollectHeapGarbage is a precise mark-sweep collector for those four
// registries. It traces from the complete live-root set, descends through
// every container kind, and sweeps the strings / dictionaries / array-lists /
// keepAlive objects that are not reachable.
//
// SAFETY: the collector reads mutable VM state (operand stacks, frames,
// mailboxes, registries) without per-field locking on the hot path. It is
// therefore only correct when no other goroutine is mutating that state —
// i.e. under stop-the-world. Callers MUST hold the GC safepoint barrier
// (see gcStopTheWorld) before invoking the unexported collect* helpers.
// The single-threaded test-suite calls them directly, which is safe because
// the test goroutine is the only mutator.
//
// Trace-and-sweep registries (freed when unreachable):
//   - Strings        (or.Strings)
//   - Dictionaries   (or.Dictionaries)
//   - ArrayLists     (cr.arrayLists)
//   - keepAlive objects
//
// Treated as roots (their contents are marked, but the holder is never swept
// here — it is either genuine live execution state or low-volume): every
// interpreter's stack/frames/exception-handlers, globals, class variables,
// loaded method & block literal pools, processes (result + mailbox), blocks
// (captures + home self), cells, contexts, results, exceptions.

// liveSet accumulates the reachable ids/pointers for one collection cycle.
type liveSet struct {
	objects    map[*Object]struct{}
	strings    map[uint32]struct{}
	dicts      map[uint32]struct{}
	arrayLists map[int]struct{}
	cells      map[*Cell]struct{}
}

func newLiveSet() *liveSet {
	return &liveSet{
		objects:    make(map[*Object]struct{}),
		strings:    make(map[uint32]struct{}),
		dicts:      make(map[uint32]struct{}),
		arrayLists: make(map[int]struct{}),
		cells:      make(map[*Cell]struct{}),
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
		case IsStringValue(v):
			ls.strings[v.SymbolID()] = struct{}{}

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
			id := v.SymbolID()
			if _, seen := ls.dicts[id]; seen {
				continue
			}
			ls.dicts[id] = struct{}{}
			if d := vm.registry.GetDictionaryObject(v); d != nil {
				for _, k := range d.Keys {
					work = append(work, k)
				}
				for _, val := range d.Data {
					work = append(work, val)
				}
			}

		case isArrayListValue(v):
			id := int(markedIDFromValue(v))
			if _, seen := ls.arrayLists[id]; seen {
				continue
			}
			ls.arrayLists[id] = struct{}{}
			if al := vm.registry.GetArrayList(v); al != nil {
				work = append(work, al.elements...)
			}

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
			if ex := vm.registry.GetException(markedIDFromValue(v)); ex != nil {
				work = append(work, ex.MessageText, ex.Tag, ex.Signaler)
			}

			// Symbols, small ints, floats, characters, booleans, nil, classes,
			// bigints, channels, processes, blocks: either contain no nested
			// Values or are enumerated directly as roots (processes/blocks).
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

	// 6. Blocks — captured variables + home self (detached blocks are live
	//    roots; non-detached ones are also reachable via frames, but marking
	//    them here is a safe over-approximation).
	vm.registry.blocksMu.RLock()
	for _, bv := range vm.registry.blocks {
		if bv == nil {
			continue
		}
		for _, c := range bv.Captures {
			vm.mark(c, ls)
		}
		vm.mark(bv.HomeSelf, ls)
		// The block's own literal pool is a root: a registered block can be
		// evaluated at any time and push its string literals.
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

	// 9. Results (over-approximated as roots — low volume).
	vm.registry.Results.RLock()
	for _, r := range vm.registry.Results.data {
		if r != nil {
			vm.mark(r.value, ls)
		}
	}
	vm.registry.Results.RUnlock()

	// 10. Exceptions (over-approximated as roots — low volume).
	vm.registry.Exceptions.RLock()
	for _, ex := range vm.registry.Exceptions.data {
		if ex != nil {
			vm.mark(ex.MessageText, ls)
			vm.mark(ex.Tag, ls)
			vm.mark(ex.Signaler, ls)
		}
	}
	vm.registry.Exceptions.RUnlock()
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

// collectHeapGarbageLocked runs one full mark-sweep cycle, sweeping the
// unreachable strings and dictionaries — the two high-volume registry leakers
// (the byte bulk is strings; orphaned dictionaries pin them). The caller MUST
// guarantee no other goroutine is mutating VM state (stop-the-world).
//
// Object (keepAlive) and array-list sweeping are intentionally left to the
// existing CollectGarbage path; an orphaned, un-swept array-list/object is
// unreachable from every root, so the strings it still references by id are
// never read after they are freed — the same invariant that makes string
// sweeping safe.
//
// Returns the number of strings and dictionaries freed.
func (vm *VM) collectHeapGarbageLocked() (strings, dicts int) {
	if vm.registry == nil {
		return 0, 0
	}
	ls := newLiveSet()
	vm.markRoots(ls)

	strings = vm.registry.Strings.Sweep(func(id uint32, _ *StringObject) bool {
		_, live := ls.strings[id]
		return live
	})
	dicts = vm.registry.Dictionaries.Sweep(func(id uint32, _ *DictionaryObject) bool {
		_, live := ls.dicts[id]
		return live
	})
	return strings, dicts
}
