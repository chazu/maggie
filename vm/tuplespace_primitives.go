package vm

import (
	"sync"
	"time"

	"cuelang.org/go/cue/cuecontext"
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
	value    Value
	mode     TupleMode
	deadline int64 // unix milliseconds, 0 = no expiry (only for affine)
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
}

// tupleWaiter represents a goroutine blocked on an in: or read: operation.
type tupleWaiter struct {
	template  *CueValueObject   // CUE template to match against (nil if compound)
	templates []*CueValueObject  // compound templates (for inAll:)
	ch        chan Value          // wake channel — send matched tuple here (single)
	chArray   chan []Value        // compound result channel (for inAll:)
	consume   bool               // true = in (destructive), false = read (copy)
}

// NaN-boxing helpers for TupleSpace values.

func tupleSpaceToValue(id int) Value {
	return FromSymbolID(uint32(id) | tupleSpaceMarker)
}

func isTupleSpaceValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	return (v.SymbolID() & markerMask) == tupleSpaceMarker
}

func tupleSpaceIDFromValue(v Value) int {
	return int(v.SymbolID() & ^uint32(0xFF<<24))
}

func (vm *VM) vmGetTupleSpace(v Value) *TupleSpaceObject {
	if !isTupleSpaceValue(v) {
		return nil
	}
	return vm.registry.GetTupleSpace(tupleSpaceIDFromValue(v))
}

func (vm *VM) vmRegisterTupleSpace(ts *TupleSpaceObject) Value {
	id := vm.registry.RegisterTupleSpace(ts)
	return tupleSpaceToValue(id)
}

// matchTuple checks if a tuple matches a CUE template using unification.
func (vm *VM) matchTuple(template *CueValueObject, tuple Value) bool {
	ctx := cuecontext.New()
	goVal := vm.cueExportValue(tuple)
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
func (vm *VM) tryMatchAll(ts *TupleSpaceObject, templates []*CueValueObject) ([]Value, []int, bool) {
	now := time.Now().UnixMilli()
	used := make(map[int]bool) // indices already claimed
	results := make([]Value, len(templates))
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
			if vm.matchTuple(tmpl, entry.value) {
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

// notifyWaiters checks all waiters against current tuples after an out:.
// Handles single, compound (inAll:), and choice (inAny:) waiters.
// Must be called with ts.mu held. May unlock/relock ts.mu to send on channels.
func (vm *VM) notifyWaiters(ts *TupleSpaceObject, tupleVal Value) bool {
	for i, w := range ts.waiters {
		if w.template != nil && w.templates == nil && w.chArray == nil {
			// Single-template waiter (in: or read: or inAny: single)
			if vm.matchTuple(w.template, tupleVal) {
				ts.waiters = append(ts.waiters[:i], ts.waiters[i+1:]...)
				ts.mu.Unlock()

				if !w.consume {
					// Non-destructive read: keep tuple in space
					ts.mu.Lock()
					ts.tuples = append(ts.tuples, TupleEntry{value: tupleVal, mode: TupleModeLinear})
					ts.mu.Unlock()
				}

				w.ch <- tupleVal
				return true
			}
		} else if w.templates != nil && w.chArray != nil {
			// Compound waiter (inAll:) — check if ALL templates now satisfiable
			results, indices, ok := vm.tryMatchAll(ts, w.templates)
			if ok {
				ts.waiters = append(ts.waiters[:i], ts.waiters[i+1:]...)
				ts.removeIndices(indices)
				ts.mu.Unlock()
				w.chArray <- results
				return true
			}
		} else if w.templates != nil && w.ch != nil {
			// Choice waiter (inAny:) — check if ANY template matches
			now := time.Now().UnixMilli()
			for _, tmpl := range w.templates {
				for j, entry := range ts.tuples {
					if entry.mode == TupleModeAffine && entry.deadline > 0 && now > entry.deadline {
						continue
					}
					if vm.matchTuple(tmpl, entry.value) {
						ts.waiters = append(ts.waiters[:i], ts.waiters[i+1:]...)
						matched := entry.value
						if entry.mode != TupleModePersistent {
							ts.tuples = append(ts.tuples[:j], ts.tuples[j+1:]...)
						}
						ts.mu.Unlock()
						w.ch <- matched
						return true
					}
				}
			}
		}
	}
	return false
}

// ---------------------------------------------------------------------------
// TupleSpace Primitives Registration
// ---------------------------------------------------------------------------

func (vm *VM) registerTupleSpacePrimitives() {
	tsClass := vm.createClass("TupleSpace", vm.ObjectClass)
	vm.Globals["TupleSpace"] = vm.classValue(tsClass)
	vm.symbolDispatch.Register(tupleSpaceMarker, &SymbolTypeEntry{Class: tsClass})

	// TupleSpace new — create a new tuple space
	tsClass.AddClassMethod0(vm.Selectors, "new", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ts := &TupleSpaceObject{}
		return v.vmRegisterTupleSpace(ts)
	})

	// TupleSpace>>primOut: — publish a tuple (non-blocking, linear mode)
	tsClass.AddMethod1(vm.Selectors, "primOut:", func(vmPtr interface{}, recv Value, tupleVal Value) Value {
		v := vmPtr.(*VM)
		ts := v.vmGetTupleSpace(recv)
		if ts == nil {
			return Nil
		}

		ts.mu.Lock()

		// Check if any waiter matches this tuple
		if v.notifyWaiters(ts, tupleVal) {
			// notifyWaiters unlocked ts.mu
			return tupleVal
		}

		// No waiter matched — store the tuple
		ts.tuples = append(ts.tuples, TupleEntry{value: tupleVal, mode: TupleModeLinear})
		ts.mu.Unlock()
		return tupleVal
	})

	// TupleSpace>>primOutPersistent: — publish a persistent tuple (never consumed)
	tsClass.AddMethod1(vm.Selectors, "primOutPersistent:", func(vmPtr interface{}, recv Value, tupleVal Value) Value {
		v := vmPtr.(*VM)
		ts := v.vmGetTupleSpace(recv)
		if ts == nil {
			return Nil
		}

		ts.mu.Lock()

		// For persistent tuples, always store them (even if a waiter matches,
		// persistent tuples stay in the space)
		ts.tuples = append(ts.tuples, TupleEntry{value: tupleVal, mode: TupleModePersistent})

		// Still check waiters — deliver a copy to matching waiters
		// but the tuple stays. We handle this via notifyWaiters which
		// will find the tuple already stored.
		// For single waiters, check directly:
		for i, w := range ts.waiters {
			if w.template != nil && w.templates == nil && w.chArray == nil {
				if v.matchTuple(w.template, tupleVal) {
					ts.waiters = append(ts.waiters[:i], ts.waiters[i+1:]...)
					ts.mu.Unlock()
					w.ch <- tupleVal
					return tupleVal
				}
			} else if w.templates != nil && w.chArray != nil {
				// Compound waiter
				results, indices, ok := v.tryMatchAll(ts, w.templates)
				if ok {
					ts.waiters = append(ts.waiters[:i], ts.waiters[i+1:]...)
					ts.removeIndices(indices) // persistent ones won't be removed
					ts.mu.Unlock()
					w.chArray <- results
					return tupleVal
				}
			} else if w.templates != nil && w.ch != nil {
				// Choice waiter
				now := time.Now().UnixMilli()
				for _, tmpl := range w.templates {
					for j, entry := range ts.tuples {
						if entry.mode == TupleModeAffine && entry.deadline > 0 && now > entry.deadline {
							continue
						}
						if v.matchTuple(tmpl, entry.value) {
							ts.waiters = append(ts.waiters[:i], ts.waiters[i+1:]...)
							matched := entry.value
							if entry.mode != TupleModePersistent {
								ts.tuples = append(ts.tuples[:j], ts.tuples[j+1:]...)
							}
							ts.mu.Unlock()
							w.ch <- matched
							return tupleVal
						}
					}
				}
			}
		}

		ts.mu.Unlock()
		return tupleVal
	})

	// TupleSpace>>primOutAffine:ttl: — publish with TTL in milliseconds
	tsClass.AddMethod2(vm.Selectors, "primOutAffine:ttl:", func(vmPtr interface{}, recv Value, tupleVal Value, ttlVal Value) Value {
		v := vmPtr.(*VM)
		ts := v.vmGetTupleSpace(recv)
		if ts == nil {
			return Nil
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

		// Check waiters first (tuple might be consumed before TTL matters)
		if v.notifyWaiters(ts, tupleVal) {
			return tupleVal
		}

		ts.tuples = append(ts.tuples, TupleEntry{
			value:    tupleVal,
			mode:     TupleModeAffine,
			deadline: deadline,
		})
		ts.mu.Unlock()
		return tupleVal
	})

	// TupleSpace>>primOutWithContext: — tuple removed when CancellationContext cancelled
	tsClass.AddMethod2(vm.Selectors, "primOut:withContext:", func(vmPtr interface{}, recv Value, tupleVal Value, ctxVal Value) Value {
		v := vmPtr.(*VM)
		ts := v.vmGetTupleSpace(recv)
		if ts == nil {
			return Nil
		}

		cancCtx := v.getCancellationContext(ctxVal)
		if cancCtx == nil {
			return Nil
		}

		ts.mu.Lock()

		// Check waiters first
		if v.notifyWaiters(ts, tupleVal) {
			return tupleVal
		}

		// Store as linear tuple
		entry := TupleEntry{value: tupleVal, mode: TupleModeLinear}
		ts.tuples = append(ts.tuples, entry)
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
	tsClass.AddMethod1(vm.Selectors, "primIn:", func(vmPtr interface{}, recv Value, templateVal Value) Value {
		v := vmPtr.(*VM)
		ts := v.vmGetTupleSpace(recv)
		if ts == nil {
			return Nil
		}

		template := v.vmGetCueValue(templateVal)
		if template == nil {
			return Nil
		}

		ts.mu.Lock()

		// Lazy sweep of expired affine tuples
		ts.sweepExpired()

		// Scan stored tuples for a match
		for i, entry := range ts.tuples {
			if v.matchTuple(template, entry.value) {
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
			ch:       make(chan Value, 1),
			consume:  true,
		}
		ts.waiters = append(ts.waiters, w)
		ts.mu.Unlock()

		// Block until a matching tuple arrives
		return <-w.ch
	})

	// TupleSpace>>primRead: — blocking non-destructive read
	tsClass.AddMethod1(vm.Selectors, "primRead:", func(vmPtr interface{}, recv Value, templateVal Value) Value {
		v := vmPtr.(*VM)
		ts := v.vmGetTupleSpace(recv)
		if ts == nil {
			return Nil
		}

		template := v.vmGetCueValue(templateVal)
		if template == nil {
			return Nil
		}

		ts.mu.Lock()

		// Lazy sweep of expired affine tuples
		ts.sweepExpired()

		// Scan stored tuples for a match (don't remove)
		for _, entry := range ts.tuples {
			if v.matchTuple(template, entry.value) {
				ts.mu.Unlock()
				return entry.value
			}
		}

		// No match — park goroutine
		w := &tupleWaiter{
			template: template,
			ch:       make(chan Value, 1),
			consume:  false,
		}
		ts.waiters = append(ts.waiters, w)
		ts.mu.Unlock()

		return <-w.ch
	})

	// TupleSpace>>primTryIn: — non-blocking destructive read
	tsClass.AddMethod1(vm.Selectors, "primTryIn:", func(vmPtr interface{}, recv Value, templateVal Value) Value {
		v := vmPtr.(*VM)
		ts := v.vmGetTupleSpace(recv)
		if ts == nil {
			return Nil
		}

		template := v.vmGetCueValue(templateVal)
		if template == nil {
			return Nil
		}

		ts.mu.Lock()
		defer ts.mu.Unlock()

		// Lazy sweep
		ts.sweepExpired()

		for i, entry := range ts.tuples {
			if v.matchTuple(template, entry.value) {
				result := entry.value
				if entry.mode == TupleModePersistent {
					return result // don't remove persistent tuples
				}
				ts.tuples = append(ts.tuples[:i], ts.tuples[i+1:]...)
				return result
			}
		}

		return Nil
	})

	// TupleSpace>>primTryRead: — non-blocking non-destructive read
	tsClass.AddMethod1(vm.Selectors, "primTryRead:", func(vmPtr interface{}, recv Value, templateVal Value) Value {
		v := vmPtr.(*VM)
		ts := v.vmGetTupleSpace(recv)
		if ts == nil {
			return Nil
		}

		template := v.vmGetCueValue(templateVal)
		if template == nil {
			return Nil
		}

		ts.mu.Lock()
		defer ts.mu.Unlock()

		// Lazy sweep
		ts.sweepExpired()

		for _, entry := range ts.tuples {
			if v.matchTuple(template, entry.value) {
				return entry.value
			}
		}

		return Nil
	})

	// TupleSpace>>primInAll: — atomic multi-take (tensor product)
	// Argument is a Maggie Array of CueValue templates.
	// Atomically takes ALL matching tuples, or blocks until all satisfiable.
	tsClass.AddMethod1(vm.Selectors, "primInAll:", func(vmPtr interface{}, recv Value, templatesVal Value) Value {
		v := vmPtr.(*VM)
		ts := v.vmGetTupleSpace(recv)
		if ts == nil {
			return Nil
		}

		// Extract array of CueValue templates
		obj := ObjectFromValue(templatesVal)
		if obj == nil {
			return Nil
		}
		n := obj.NumSlots()
		if n == 0 {
			return v.NewArrayWithElements(nil)
		}

		templates := make([]*CueValueObject, n)
		for i := 0; i < n; i++ {
			slot := obj.GetSlot(i)
			tmpl := v.vmGetCueValue(slot)
			if tmpl == nil {
				return Nil
			}
			templates[i] = tmpl
		}

		ts.mu.Lock()
		ts.sweepExpired()

		// Try to match all templates atomically
		results, indices, ok := v.tryMatchAll(ts, templates)
		if ok {
			ts.removeIndices(indices)
			ts.mu.Unlock()
			return v.NewArrayWithElements(results)
		}

		// Not all matched — register compound waiter
		w := &tupleWaiter{
			templates: templates,
			chArray:   make(chan []Value, 1),
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
	tsClass.AddMethod1(vm.Selectors, "primInAny:", func(vmPtr interface{}, recv Value, templatesVal Value) Value {
		v := vmPtr.(*VM)
		ts := v.vmGetTupleSpace(recv)
		if ts == nil {
			return Nil
		}

		obj := ObjectFromValue(templatesVal)
		if obj == nil {
			return Nil
		}
		n := obj.NumSlots()
		if n == 0 {
			return Nil
		}

		templates := make([]*CueValueObject, n)
		for i := 0; i < n; i++ {
			slot := obj.GetSlot(i)
			tmpl := v.vmGetCueValue(slot)
			if tmpl == nil {
				return Nil
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
				if v.matchTuple(tmpl, entry.value) {
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
			ch:        make(chan Value, 1),
			consume:   true,
		}
		ts.waiters = append(ts.waiters, w)
		ts.mu.Unlock()

		return <-w.ch
	})

	// TupleSpace>>primSize — number of stored tuples
	tsClass.AddMethod0(vm.Selectors, "primSize", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ts := v.vmGetTupleSpace(recv)
		if ts == nil {
			return FromSmallInt(0)
		}
		ts.mu.Lock()
		defer ts.mu.Unlock()
		return FromSmallInt(int64(len(ts.tuples)))
	})

	// TupleSpace>>primIsEmpty — true if no tuples stored
	tsClass.AddMethod0(vm.Selectors, "primIsEmpty", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ts := v.vmGetTupleSpace(recv)
		if ts == nil {
			return True
		}
		ts.mu.Lock()
		defer ts.mu.Unlock()
		if len(ts.tuples) == 0 {
			return True
		}
		return False
	})

	// TupleSpace>>printString
	tsClass.AddMethod0(vm.Selectors, "primPrintString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ts := v.vmGetTupleSpace(recv)
		if ts == nil {
			return v.registry.NewStringValue("a TupleSpace (invalid)")
		}
		ts.mu.Lock()
		n := len(ts.tuples)
		ts.mu.Unlock()
		return v.registry.NewStringValue("a TupleSpace (" + itoa(n) + " tuples)")
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
