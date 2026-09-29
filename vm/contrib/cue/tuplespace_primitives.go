package cue

import (
	"sync"
	"time"

	"cuelang.org/go/cue/cuecontext"

	vm "github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// TupleSpace: Linda-style shared tuple space with CUE template matching
// ---------------------------------------------------------------------------

// TupleMode controls tuple consumption semantics.
type TupleMode int

const (
	TupleModeLinear     TupleMode = iota // consumed exactly once (default)
	TupleModeAffine                      // consumed at most once, can expire via TTL
	TupleModePersistent                  // never consumed, in: returns copy but keeps original
)

// TupleEntry holds a tuple value and its mode/deadline metadata.
type TupleEntry struct {
	value    vm.Value
	mode     TupleMode
	deadline int64  // unix milliseconds, 0 = no expiry (only for affine)
	seq      uint64 // insertion sequence number (identifies the entry put() just stored)
}

// isExpired returns true if this is an affine tuple past its deadline.
func (e *TupleEntry) isExpired() bool {
	if e.mode != TupleModeAffine || e.deadline == 0 {
		return false
	}
	return time.Now().UnixMilli() > e.deadline
}

// TupleSpaceObject holds tuples and waiters for blocking operations.
type TupleSpaceObject struct {
	mu      sync.Mutex
	tuples  []TupleEntry   // stored tuples
	waiters []*tupleWaiter // blocked in/read operations
	nextSeq uint64         // last TupleEntry.seq handed out
}

// MarkRoots implements vm.RootMarker: it reports every Maggie Value the tuple
// space retains so the tracing collector treats them as live. Without this, a
// value placed with `out:` lives only in the tuples slice — invisible to the
// trace — and would be swept, then aliased after id recycling. In-flight values
// handed to a waiter's wake channel are held on the receiving goroutine's Go
// stack (a traced root once received); only the persistent tuples need marking.
func (ts *TupleSpaceObject) MarkRoots(mark func(vm.Value)) {
	ts.mu.Lock()
	defer ts.mu.Unlock()
	for i := range ts.tuples {
		mark(ts.tuples[i].value)
	}
}

// tupleWaiter represents a goroutine blocked on an in: or read: operation.
type tupleWaiter struct {
	template  *CueValueObject   // CUE template to match against (nil if compound)
	templates []*CueValueObject // compound templates (for inAll:)
	ch        chan vm.Value     // wake channel — send matched tuple here (single)
	chArray   chan []vm.Value   // compound result channel (for inAll:)
	consume   bool              // true = in (destructive), false = read (copy)
}

// NaN-boxing helpers for TupleSpace values.

func isTupleSpaceValue(v vm.Value) bool {
	return vm.IsExtensionValue(v, vm.TupleSpaceMarker)
}

func vmGetTupleSpace(v *vm.VM, val vm.Value) *TupleSpaceObject {
	if o := vm.ExtensionObject(val, vm.TupleSpaceMarker); o != nil {
		return o.(*TupleSpaceObject)
	}
	return nil
}

func vmRegisterTupleSpace(v *vm.VM, ts *TupleSpaceObject) vm.Value {
	return vm.NewExtensionValue(vm.TupleSpaceMarker, ts)
}

// matchTuple checks if a tuple matches a CUE template using unification.
func matchTuple(v *vm.VM, template *CueValueObject, tuple vm.Value) bool {
	ctx := cuecontext.New()
	goVal := cueExportValue(v, tuple)
	projection := ctx.Encode(goVal)
	unified := template.val.Unify(projection)
	return unified.Err() == nil
}

// sweepExpired removes expired affine tuples lazily during scans.
// Must be called with ts.mu held.
func (ts *TupleSpaceObject) sweepExpired() {
	now := time.Now().UnixMilli()
	n := 0
	for _, entry := range ts.tuples {
		if entry.mode == TupleModeAffine && entry.deadline > 0 && now > entry.deadline {
			continue // expired, skip
		}
		ts.tuples[n] = entry
		n++
	}
	ts.tuples = ts.tuples[:n]
}

// tryMatchAll attempts to find a distinct matching tuple for each template.
// Returns the matched entries and their indices, or nil if not all matched.
// Must be called with ts.mu held. Skips expired affine tuples.
func tryMatchAll(v *vm.VM, ts *TupleSpaceObject, templates []*CueValueObject) ([]vm.Value, []int, bool) {
	now := time.Now().UnixMilli()
	used := make(map[int]bool) // indices already claimed
	results := make([]vm.Value, len(templates))
	indices := make([]int, len(templates))

	for ti, tmpl := range templates {
		found := false
		for i, entry := range ts.tuples {
			if used[i] {
				continue
			}
			// Skip expired affine tuples
			if entry.mode == TupleModeAffine && entry.deadline > 0 && now > entry.deadline {
				continue
			}
			if matchTuple(v, tmpl, entry.value) {
				results[ti] = entry.value
				indices[ti] = i
				used[i] = true
				found = true
				break
			}
		}
		if !found {
			return nil, nil, false
		}
	}
	return results, indices, true
}

// removeIndices removes tuples at the given indices (must be sorted or handled).
// Respects tuple modes: persistent tuples are NOT removed.
// Must be called with ts.mu held.
func (ts *TupleSpaceObject) removeIndices(indices []int) {
	// Build set of indices to remove (only non-persistent)
	toRemove := make(map[int]bool)
	for _, idx := range indices {
		if ts.tuples[idx].mode != TupleModePersistent {
			toRemove[idx] = true
		}
	}
	if len(toRemove) == 0 {
		return
	}
	n := 0
	for i, entry := range ts.tuples {
		if !toRemove[i] {
			ts.tuples[n] = entry
			n++
		}
	}
	ts.tuples = ts.tuples[:n]
}

// put stores entry and then wakes every parked waiter it satisfies. Storing
// FIRST matters: compound (inAll:) waiters are satisfied by the combination of
// the new tuple and tuples already present, so they must see it in the space.
//
// Invariant (maintained because every out goes through put, and in:/read:/
// inAll:/inAny: park only after a failed scan under ts.mu): no parked waiter
// is satisfiable by the tuples stored before this put. So only waiters that
// the NEW entry can satisfy need checking — single and choice waiters are
// matched against the new entry alone, compound waiters are re-tried over the
// whole space only when one of their templates matches it.
//
// Waiters are visited in FIFO order. Non-consuming read: waiters take a copy
// and dispatch continues, so a parked read: can no longer swallow the wakeup a
// parked in: needed. A consuming waiter removes the entry (unless persistent)
// and later waiters cannot see it. The entry is never re-stored, so an affine
// tuple keeps its TTL.
//
// Must be called with ts.mu held; it does not release the lock. Every waiter
// channel has capacity 1 and a waiter is removed from ts.waiters before its
// single send, so sends here never block.
func (ts *TupleSpaceObject) put(v *vm.VM, entry TupleEntry) {
	ts.nextSeq++
	entry.seq = ts.nextSeq
	ts.tuples = append(ts.tuples, entry)
	if len(ts.waiters) == 0 {
		return
	}

	// newIndex locates the just-stored entry (it is at or near the end;
	// removals preserve order and nothing appends during dispatch).
	newIndex := func() int {
		for i := len(ts.tuples) - 1; i >= 0; i-- {
			if ts.tuples[i].seq == entry.seq {
				return i
			}
		}
		return -1
	}

	remaining := ts.waiters[:0]
	for wi, w := range ts.waiters {
		idx := newIndex()
		if idx < 0 || ts.tuples[idx].isExpired() {
			// New entry already consumed (or expired): nobody else can be
			// woken by this put.
			remaining = append(remaining, ts.waiters[wi:]...)
			break
		}
		if !ts.tryWake(v, w, idx) {
			remaining = append(remaining, w)
		}
	}
	for i := len(remaining); i < len(ts.waiters); i++ {
		ts.waiters[i] = nil // drop references held by the backing array
	}
	ts.waiters = remaining
}

// tryWake delivers to w if the entry at newIdx (the tuple just stored)
// satisfies it, returning true if w was woken. Must be called with ts.mu held.
func (ts *TupleSpaceObject) tryWake(v *vm.VM, w *tupleWaiter, newIdx int) bool {
	e := ts.tuples[newIdx]
	switch {
	case w.template != nil && w.templates == nil && w.chArray == nil:
		// Single-template waiter (in: or read:)
		if !matchTuple(v, w.template, e.value) {
			return false
		}
		if w.consume && e.mode != TupleModePersistent {
			ts.tuples = append(ts.tuples[:newIdx], ts.tuples[newIdx+1:]...)
		}
		w.ch <- e.value
		return true

	case w.templates != nil && w.chArray != nil:
		// Compound waiter (inAll:) — only the new tuple can have made it
		// satisfiable, so skip the full scan unless it matches a template.
		relevant := false
		for _, tmpl := range w.templates {
			if matchTuple(v, tmpl, e.value) {
				relevant = true
				break
			}
		}
		if !relevant {
			return false
		}
		results, indices, ok := tryMatchAll(v, ts, w.templates)
		if !ok {
			return false
		}
		ts.removeIndices(indices) // persistent ones are kept
		w.chArray <- results
		return true

	case w.templates != nil && w.ch != nil:
		// Choice waiter (inAny:) — any template matching the new tuple.
		for _, tmpl := range w.templates {
			if matchTuple(v, tmpl, e.value) {
				if e.mode != TupleModePersistent {
					ts.tuples = append(ts.tuples[:newIdx], ts.tuples[newIdx+1:]...)
				}
				w.ch <- e.value
				return true
			}
		}
	}
	return false
}

// ---------------------------------------------------------------------------
// TupleSpace Primitives Registration
// ---------------------------------------------------------------------------

func registerTupleSpacePrimitives(v *vm.VM) {
	tsClass := v.CreateClass("TupleSpace", v.ObjectClass)
	v.SetGlobal("TupleSpace", v.ClassValue(tsClass))
	v.RegisterSymbolDispatchEntry(vm.TupleSpaceMarker, &vm.SymbolTypeEntry{Class: tsClass})

	// TupleSpace new — create a new tuple space
	tsClass.AddClassMethod0(v.Selectors, "new", func(v *vm.VM, recv vm.Value) vm.Value {
		ts := &TupleSpaceObject{}
		return vmRegisterTupleSpace(v, ts)
	})

	// TupleSpace>>primOut: — publish a tuple (non-blocking, linear mode)
	tsClass.AddMethod1(v.Selectors, "primOut:", func(v *vm.VM, recv vm.Value, tupleVal vm.Value) vm.Value {
		ts := vmGetTupleSpace(v, recv)
		if ts == nil {
			return vm.Nil
		}

		ts.mu.Lock()
		ts.put(v, TupleEntry{value: tupleVal, mode: TupleModeLinear})
		ts.mu.Unlock()
		return tupleVal
	})

	// TupleSpace>>primOutPersistent: — publish a persistent tuple (never consumed)
	tsClass.AddMethod1(v.Selectors, "primOutPersistent:", func(v *vm.VM, recv vm.Value, tupleVal vm.Value) vm.Value {
		ts := vmGetTupleSpace(v, recv)
		if ts == nil {
			return vm.Nil
		}

		ts.mu.Lock()

		// Persistent tuples stay in the space; put delivers a copy to every
		// parked waiter they satisfy.
		ts.put(v, TupleEntry{value: tupleVal, mode: TupleModePersistent})
		ts.mu.Unlock()
		return tupleVal
	})

	// TupleSpace>>primOutAffine:ttl: — publish with TTL in milliseconds
	tsClass.AddMethod2(v.Selectors, "primOutAffine:ttl:", func(v *vm.VM, recv vm.Value, tupleVal vm.Value, ttlVal vm.Value) vm.Value {
		ts := vmGetTupleSpace(v, recv)
		if ts == nil {
			return vm.Nil
		}

		ttlMs := int64(0)
		if ttlVal.IsSmallInt() {
			ttlMs = ttlVal.SmallInt()
		}

		deadline := int64(0)
		if ttlMs > 0 {
			deadline = time.Now().UnixMilli() + ttlMs
		}

		ts.mu.Lock()
		ts.put(v, TupleEntry{
			value:    tupleVal,
			mode:     TupleModeAffine,
			deadline: deadline,
		})
		ts.mu.Unlock()
		return tupleVal
	})

	// TupleSpace>>primOutWithContext: — tuple removed when CancellationContext cancelled
	tsClass.AddMethod2(v.Selectors, "primOut:withContext:", func(v *vm.VM, recv vm.Value, tupleVal vm.Value, ctxVal vm.Value) vm.Value {
		ts := vmGetTupleSpace(v, recv)
		if ts == nil {
			return vm.Nil
		}

		cancCtx := v.GetCancellationContext(ctxVal)
		if cancCtx == nil {
			return vm.Nil
		}

		ts.mu.Lock()
		// Store as linear tuple (a parked waiter may consume it immediately)
		ts.put(v, TupleEntry{value: tupleVal, mode: TupleModeLinear})
		ts.mu.Unlock()

		// Start goroutine to watch for cancellation
		go func() {
			<-cancCtx.Done()
			ts.mu.Lock()
			defer ts.mu.Unlock()
			// Remove the tuple if still present
			for i, e := range ts.tuples {
				if e.value == tupleVal {
					ts.tuples = append(ts.tuples[:i], ts.tuples[i+1:]...)
					return
				}
			}
		}()

		return tupleVal
	})

	// TupleSpace>>primIn: — blocking destructive read (linear consumption)
	// For persistent tuples, returns value but does not remove.
	tsClass.AddMethod1(v.Selectors, "primIn:", func(v *vm.VM, recv vm.Value, templateVal vm.Value) vm.Value {
		ts := vmGetTupleSpace(v, recv)
		if ts == nil {
			return vm.Nil
		}

		template := vmGetCueValue(v, templateVal)
		if template == nil {
			return vm.Nil
		}

		ts.mu.Lock()

		// Lazy sweep of expired affine tuples
		ts.sweepExpired()

		// Scan stored tuples for a match
		for i, entry := range ts.tuples {
			if matchTuple(v, template, entry.value) {
				result := entry.value
				if entry.mode == TupleModePersistent {
					// Persistent: return value but don't remove
					ts.mu.Unlock()
					return result
				}
				// Linear or affine: remove and return
				ts.tuples = append(ts.tuples[:i], ts.tuples[i+1:]...)
				ts.mu.Unlock()
				return result
			}
		}

		// No match — park goroutine
		w := &tupleWaiter{
			template: template,
			ch:       make(chan vm.Value, 1),
			consume:  true,
		}
		ts.waiters = append(ts.waiters, w)
		ts.mu.Unlock()

		// Block until a matching tuple arrives
		return <-w.ch
	})

	// TupleSpace>>primRead: — blocking non-destructive read
	tsClass.AddMethod1(v.Selectors, "primRead:", func(v *vm.VM, recv vm.Value, templateVal vm.Value) vm.Value {
		ts := vmGetTupleSpace(v, recv)
		if ts == nil {
			return vm.Nil
		}

		template := vmGetCueValue(v, templateVal)
		if template == nil {
			return vm.Nil
		}

		ts.mu.Lock()

		// Lazy sweep of expired affine tuples
		ts.sweepExpired()

		// Scan stored tuples for a match (don't remove)
		for _, entry := range ts.tuples {
			if matchTuple(v, template, entry.value) {
				ts.mu.Unlock()
				return entry.value
			}
		}

		// No match — park goroutine
		w := &tupleWaiter{
			template: template,
			ch:       make(chan vm.Value, 1),
			consume:  false,
		}
		ts.waiters = append(ts.waiters, w)
		ts.mu.Unlock()

		return <-w.ch
	})

	// TupleSpace>>primTryIn: — non-blocking destructive read
	tsClass.AddMethod1(v.Selectors, "primTryIn:", func(v *vm.VM, recv vm.Value, templateVal vm.Value) vm.Value {
		ts := vmGetTupleSpace(v, recv)
		if ts == nil {
			return vm.Nil
		}

		template := vmGetCueValue(v, templateVal)
		if template == nil {
			return vm.Nil
		}

		ts.mu.Lock()
		defer ts.mu.Unlock()

		// Lazy sweep
		ts.sweepExpired()

		for i, entry := range ts.tuples {
			if matchTuple(v, template, entry.value) {
				result := entry.value
				if entry.mode == TupleModePersistent {
					return result // don't remove persistent tuples
				}
				ts.tuples = append(ts.tuples[:i], ts.tuples[i+1:]...)
				return result
			}
		}

		return vm.Nil
	})

	// TupleSpace>>primTryRead: — non-blocking non-destructive read
	tsClass.AddMethod1(v.Selectors, "primTryRead:", func(v *vm.VM, recv vm.Value, templateVal vm.Value) vm.Value {
		ts := vmGetTupleSpace(v, recv)
		if ts == nil {
			return vm.Nil
		}

		template := vmGetCueValue(v, templateVal)
		if template == nil {
			return vm.Nil
		}

		ts.mu.Lock()
		defer ts.mu.Unlock()

		// Lazy sweep
		ts.sweepExpired()

		for _, entry := range ts.tuples {
			if matchTuple(v, template, entry.value) {
				return entry.value
			}
		}

		return vm.Nil
	})

	// TupleSpace>>primInAll: — atomic multi-take (tensor product)
	// Argument is a Maggie Array of CueValue templates.
	// Atomically takes ALL matching tuples, or blocks until all satisfiable.
	tsClass.AddMethod1(v.Selectors, "primInAll:", func(v *vm.VM, recv vm.Value, templatesVal vm.Value) vm.Value {
		ts := vmGetTupleSpace(v, recv)
		if ts == nil {
			return vm.Nil
		}

		// Extract array of CueValue templates
		obj := vm.ObjectFromValue(templatesVal)
		if obj == nil {
			return vm.Nil
		}
		n := obj.NumSlots()
		if n == 0 {
			return v.NewArrayWithElements(nil)
		}

		templates := make([]*CueValueObject, n)
		for i := 0; i < n; i++ {
			slot := obj.GetSlot(i)
			tmpl := vmGetCueValue(v, slot)
			if tmpl == nil {
				return vm.Nil
			}
			templates[i] = tmpl
		}

		ts.mu.Lock()
		ts.sweepExpired()

		// Try to match all templates atomically
		results, indices, ok := tryMatchAll(v, ts, templates)
		if ok {
			ts.removeIndices(indices)
			ts.mu.Unlock()
			return v.NewArrayWithElements(results)
		}

		// Not all matched — register compound waiter
		w := &tupleWaiter{
			templates: templates,
			chArray:   make(chan []vm.Value, 1),
			consume:   true,
		}
		ts.waiters = append(ts.waiters, w)
		ts.mu.Unlock()

		// Block until all templates are satisfiable
		results = <-w.chArray
		return v.NewArrayWithElements(results)
	})

	// TupleSpace>>primInAny: — choice (additive disjunction)
	// Argument is a Maggie Array of CueValue templates.
	// Returns the first tuple matching any template.
	tsClass.AddMethod1(v.Selectors, "primInAny:", func(v *vm.VM, recv vm.Value, templatesVal vm.Value) vm.Value {
		ts := vmGetTupleSpace(v, recv)
		if ts == nil {
			return vm.Nil
		}

		obj := vm.ObjectFromValue(templatesVal)
		if obj == nil {
			return vm.Nil
		}
		n := obj.NumSlots()
		if n == 0 {
			return vm.Nil
		}

		templates := make([]*CueValueObject, n)
		for i := 0; i < n; i++ {
			slot := obj.GetSlot(i)
			tmpl := vmGetCueValue(v, slot)
			if tmpl == nil {
				return vm.Nil
			}
			templates[i] = tmpl
		}

		ts.mu.Lock()
		ts.sweepExpired()

		// Scan tuples for first match against any template
		now := time.Now().UnixMilli()
		for _, tmpl := range templates {
			for i, entry := range ts.tuples {
				if entry.mode == TupleModeAffine && entry.deadline > 0 && now > entry.deadline {
					continue
				}
				if matchTuple(v, tmpl, entry.value) {
					result := entry.value
					if entry.mode != TupleModePersistent {
						ts.tuples = append(ts.tuples[:i], ts.tuples[i+1:]...)
					}
					ts.mu.Unlock()
					return result
				}
			}
		}

		// No match — register choice waiter
		w := &tupleWaiter{
			templates: templates,
			ch:        make(chan vm.Value, 1),
			consume:   true,
		}
		ts.waiters = append(ts.waiters, w)
		ts.mu.Unlock()

		return <-w.ch
	})

	// TupleSpace>>primSize — number of stored tuples
	tsClass.AddMethod0(v.Selectors, "primSize", func(v *vm.VM, recv vm.Value) vm.Value {
		ts := vmGetTupleSpace(v, recv)
		if ts == nil {
			return vm.FromSmallInt(0)
		}
		ts.mu.Lock()
		defer ts.mu.Unlock()
		return vm.FromSmallInt(int64(len(ts.tuples)))
	})

	// TupleSpace>>primIsEmpty — true if no tuples stored
	tsClass.AddMethod0(v.Selectors, "primIsEmpty", func(v *vm.VM, recv vm.Value) vm.Value {
		ts := vmGetTupleSpace(v, recv)
		if ts == nil {
			return vm.True
		}
		ts.mu.Lock()
		defer ts.mu.Unlock()
		if len(ts.tuples) == 0 {
			return vm.True
		}
		return vm.False
	})

	// TupleSpace>>printString
	tsClass.AddMethod0(v.Selectors, "primPrintString", func(v *vm.VM, recv vm.Value) vm.Value {
		ts := vmGetTupleSpace(v, recv)
		if ts == nil {
			return v.Registry().NewStringValue("a TupleSpace (invalid)")
		}
		ts.mu.Lock()
		n := len(ts.tuples)
		ts.mu.Unlock()
		return v.Registry().NewStringValue("a TupleSpace (" + itoa(n) + " tuples)")
	})
}

// itoa converts an int to a string without importing strconv.
func itoa(n int) string {
	if n == 0 {
		return "0"
	}
	neg := false
	if n < 0 {
		neg = true
		n = -n
	}
	digits := make([]byte, 0, 10)
	for n > 0 {
		digits = append(digits, byte('0'+n%10))
		n /= 10
	}
	if neg {
		digits = append(digits, '-')
	}
	// reverse
	for i, j := 0, len(digits)-1; i < j; i, j = i+1, j-1 {
		digits[i], digits[j] = digits[j], digits[i]
	}
	return string(digits)
}
