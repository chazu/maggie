package vm

import (
	"sync"

	"cuelang.org/go/cue/cuecontext"
)

// ---------------------------------------------------------------------------
// TupleSpace: Linda-style shared tuple space with CUE template matching
// ---------------------------------------------------------------------------

// TupleSpaceObject holds tuples and waiters for blocking operations.
type TupleSpaceObject struct {
	mu      sync.Mutex
	tuples  []Value        // stored tuples
	waiters []*tupleWaiter // blocked in/read operations
}

// tupleWaiter represents a goroutine blocked on an in: or read: operation.
type tupleWaiter struct {
	template *CueValueObject // CUE template to match against
	ch       chan Value       // wake channel — send matched tuple here
	consume  bool            // true = in (destructive), false = read (copy)
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

	// TupleSpace>>primOut: — publish a tuple (non-blocking)
	tsClass.AddMethod1(vm.Selectors, "primOut:", func(vmPtr interface{}, recv Value, tupleVal Value) Value {
		v := vmPtr.(*VM)
		ts := v.vmGetTupleSpace(recv)
		if ts == nil {
			return Nil
		}

		ts.mu.Lock()

		// Check if any waiter matches this tuple
		for i, w := range ts.waiters {
			if v.matchTuple(w.template, tupleVal) {
				// Remove waiter
				ts.waiters = append(ts.waiters[:i], ts.waiters[i+1:]...)
				ts.mu.Unlock()

				// If non-destructive read, keep the tuple in the space too
				if !w.consume {
					ts.mu.Lock()
					ts.tuples = append(ts.tuples, tupleVal)
					ts.mu.Unlock()
				}

				// Wake the waiter
				w.ch <- tupleVal
				return tupleVal
			}
		}

		// No waiter matched — store the tuple
		ts.tuples = append(ts.tuples, tupleVal)
		ts.mu.Unlock()
		return tupleVal
	})

	// TupleSpace>>primIn: — blocking destructive read (linear consumption)
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

		// Scan stored tuples for a match
		for i, tuple := range ts.tuples {
			if v.matchTuple(template, tuple) {
				// Remove and return
				ts.tuples = append(ts.tuples[:i], ts.tuples[i+1:]...)
				ts.mu.Unlock()
				return tuple
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

		// Scan stored tuples for a match (don't remove)
		for _, tuple := range ts.tuples {
			if v.matchTuple(template, tuple) {
				ts.mu.Unlock()
				return tuple
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

		for i, tuple := range ts.tuples {
			if v.matchTuple(template, tuple) {
				ts.tuples = append(ts.tuples[:i], ts.tuples[i+1:]...)
				return tuple
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

		for _, tuple := range ts.tuples {
			if v.matchTuple(template, tuple) {
				return tuple
			}
		}

		return Nil
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
