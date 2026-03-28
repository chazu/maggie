package vm

import (
	"fmt"
	"sync"

	"cuelang.org/go/cue"
	"cuelang.org/go/cue/cuecontext"
)

// ---------------------------------------------------------------------------
// ConstraintStore: Monotonic CUE constraint store (Concurrent Constraint Programming)
// ---------------------------------------------------------------------------

// ConstraintStoreObject holds a monotonic CUE constraint store.
// The store starts as CUE top (unconstrained) and monotonically narrows
// via unification. Processes tell constraints to add information and ask
// constraints to check entailment, blocking until the store implies their query.
type ConstraintStoreObject struct {
	mu       sync.Mutex
	ctx      *cue.Context
	store    cue.Value
	watchers []*constraintWatcher
}

// constraintWatcher represents a goroutine blocked on an ask operation.
type constraintWatcher struct {
	query cue.Value
	ch    chan struct{}
}

// tell unifies a new constraint into the store. Returns nil on success,
// or an error if the unification produces bottom (store unchanged on failure).
func (cs *ConstraintStoreObject) tell(newConstraint cue.Value) error {
	cs.mu.Lock()
	defer cs.mu.Unlock()

	unified := cs.store.Unify(newConstraint)
	if err := unified.Validate(); err != nil {
		return err // store unchanged — conflicting constraint
	}
	cs.store = unified

	// Check watchers: wake any whose query is now entailed by the store.
	// Entailment: query.Subsume(store) == nil means query is more general than store.
	remaining := cs.watchers[:0]
	for _, w := range cs.watchers {
		if w.query.Subsume(cs.store, cue.Final()) == nil {
			close(w.ch) // wake the blocked goroutine
		} else {
			remaining = append(remaining, w)
		}
	}
	cs.watchers = remaining
	return nil
}

// ask blocks until store entails query (query.Subsume(store) == nil).
func (cs *ConstraintStoreObject) ask(query cue.Value) {
	cs.mu.Lock()
	if query.Subsume(cs.store, cue.Final()) == nil {
		cs.mu.Unlock()
		return // already entailed
	}
	w := &constraintWatcher{query: query, ch: make(chan struct{})}
	cs.watchers = append(cs.watchers, w)
	cs.mu.Unlock()
	<-w.ch // block until woken by tell
}

// tryAsk checks if the store currently entails the query without blocking.
func (cs *ConstraintStoreObject) tryAsk(query cue.Value) bool {
	cs.mu.Lock()
	defer cs.mu.Unlock()
	return query.Subsume(cs.store, cue.Final()) == nil
}

// isConsistent returns true if the store is not bottom.
func (cs *ConstraintStoreObject) isConsistent() bool {
	cs.mu.Lock()
	defer cs.mu.Unlock()
	return cs.store.Validate() == nil
}

// NaN-boxing helpers for ConstraintStore values.

func constraintStoreToValue(id uint32) Value {
	return FromSymbolID(id | constraintStoreMarker)
}

func isConstraintStoreValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	return (v.SymbolID() & markerMask) == constraintStoreMarker
}

func constraintStoreIDFromValue(v Value) uint32 {
	return v.SymbolID() & ^uint32(0xFF<<24)
}

func (vm *VM) vmGetConstraintStore(v Value) *ConstraintStoreObject {
	if !isConstraintStoreValue(v) {
		return nil
	}
	return vm.registry.GetConstraintStore(constraintStoreIDFromValue(v))
}

func (vm *VM) vmRegisterConstraintStore(cs *ConstraintStoreObject) Value {
	id := vm.registry.RegisterConstraintStore(cs)
	return constraintStoreToValue(id)
}

// ---------------------------------------------------------------------------
// ConstraintStore Primitives Registration
// ---------------------------------------------------------------------------

func (vm *VM) registerConstraintStorePrimitives() {
	csClass := vm.createClass("ConstraintStore", vm.ObjectClass)
	vm.Globals["ConstraintStore"] = vm.classValue(csClass)
	vm.symbolDispatch.Register(constraintStoreMarker, &SymbolTypeEntry{Class: csClass})

	// ConstraintStore new — create a new constraint store (starts as top/unconstrained)
	csClass.AddClassMethod0(vm.Selectors, "new", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := cuecontext.New()
		cs := &ConstraintStoreObject{
			ctx:   ctx,
			store: ctx.CompileString("_"),
		}
		return v.vmRegisterConstraintStore(cs)
	})

	// ConstraintStore>>primTell: — unify a CueValue constraint into the store
	csClass.AddMethod1(vm.Selectors, "primTell:", func(vmPtr interface{}, recv Value, constraintVal Value) Value {
		v := vmPtr.(*VM)
		cs := v.vmGetConstraintStore(recv)
		if cs == nil {
			return v.newFailureResult("invalid ConstraintStore")
		}

		cv := v.vmGetCueValue(constraintVal)
		if cv == nil {
			return v.newFailureResult("argument must be a CueValue")
		}

		if err := cs.tell(cv.val); err != nil {
			return v.newFailureResult(fmt.Sprintf("conflicting constraint: %s", err))
		}
		return v.newSuccessResult(True)
	})

	// ConstraintStore>>primAsk: — block until the store subsumes the query CueValue
	csClass.AddMethod1(vm.Selectors, "primAsk:", func(vmPtr interface{}, recv Value, queryVal Value) Value {
		v := vmPtr.(*VM)
		cs := v.vmGetConstraintStore(recv)
		if cs == nil {
			return Nil
		}

		cv := v.vmGetCueValue(queryVal)
		if cv == nil {
			return Nil
		}

		cs.ask(cv.val)
		return True
	})

	// ConstraintStore>>primTryAsk: — non-blocking entailment check
	csClass.AddMethod1(vm.Selectors, "primTryAsk:", func(vmPtr interface{}, recv Value, queryVal Value) Value {
		v := vmPtr.(*VM)
		cs := v.vmGetConstraintStore(recv)
		if cs == nil {
			return False
		}

		cv := v.vmGetCueValue(queryVal)
		if cv == nil {
			return False
		}

		if cs.tryAsk(cv.val) {
			return True
		}
		return False
	})

	// ConstraintStore>>primWatch:do: — register callback for entailment
	csClass.AddMethod2(vm.Selectors, "primWatch:do:", func(vmPtr interface{}, recv Value, queryVal Value, blockVal Value) Value {
		v := vmPtr.(*VM)
		cs := v.vmGetConstraintStore(recv)
		if cs == nil {
			return Nil
		}

		cv := v.vmGetCueValue(queryVal)
		if cv == nil {
			return Nil
		}

		cs.mu.Lock()
		// Check if already entailed (query more general than store)
		if cv.val.Subsume(cs.store, cue.Final()) == nil {
			cs.mu.Unlock()
			// Already entailed — invoke block immediately
			v.Send(blockVal, "value", nil)
			return True
		}

		// Not yet entailed — register a watcher and invoke block asynchronously
		w := &constraintWatcher{query: cv.val, ch: make(chan struct{})}
		cs.watchers = append(cs.watchers, w)
		cs.mu.Unlock()

		go func() {
			<-w.ch
			v.Send(blockVal, "value", nil)
		}()

		return True
	})

	// ConstraintStore>>primValue — return current store state as a CueValue
	csClass.AddMethod0(vm.Selectors, "primValue", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cs := v.vmGetConstraintStore(recv)
		if cs == nil {
			return Nil
		}

		cs.mu.Lock()
		storeVal := cs.store
		cs.mu.Unlock()

		obj := &CueValueObject{val: storeVal}
		return v.vmRegisterCueValue(obj)
	})

	// ConstraintStore>>primIsConsistent — true if store is not bottom
	csClass.AddMethod0(vm.Selectors, "primIsConsistent", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cs := v.vmGetConstraintStore(recv)
		if cs == nil {
			return False
		}

		if cs.isConsistent() {
			return True
		}
		return False
	})

	// ConstraintStore>>primPrintString — human-readable description
	csClass.AddMethod0(vm.Selectors, "primPrintString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cs := v.vmGetConstraintStore(recv)
		if cs == nil {
			return v.registry.NewStringValue("a ConstraintStore (invalid)")
		}

		cs.mu.Lock()
		storeVal := cs.store
		nWatchers := len(cs.watchers)
		cs.mu.Unlock()

		desc := fmt.Sprintf("a ConstraintStore (%s", storeVal.IncompleteKind())
		if nWatchers > 0 {
			desc += fmt.Sprintf(", %d watchers", nWatchers)
		}
		desc += ")"
		return v.registry.NewStringValue(desc)
	})
}
