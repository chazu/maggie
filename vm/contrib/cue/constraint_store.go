package cue

import (
	"fmt"
	"sync"

	"cuelang.org/go/cue"
	"cuelang.org/go/cue/cuecontext"

	vm "github.com/chazu/maggie/vm"
)

// ConstraintStoreObject holds a monotonic CUE constraint store.
type ConstraintStoreObject struct {
	mu       sync.Mutex
	ctx      *cue.Context
	store    cue.Value
	watchers []*constraintWatcher
}

type constraintWatcher struct {
	query cue.Value
	ch    chan struct{}
}

func (cs *ConstraintStoreObject) tell(newConstraint cue.Value) error {
	cs.mu.Lock()
	defer cs.mu.Unlock()

	unified := cs.store.Unify(newConstraint)
	if err := unified.Validate(); err != nil {
		return err
	}
	cs.store = unified

	remaining := cs.watchers[:0]
	for _, w := range cs.watchers {
		if w.query.Subsume(cs.store, cue.Final()) == nil {
			close(w.ch)
		} else {
			remaining = append(remaining, w)
		}
	}
	cs.watchers = remaining
	return nil
}

func (cs *ConstraintStoreObject) ask(query cue.Value) {
	cs.mu.Lock()
	if query.Subsume(cs.store, cue.Final()) == nil {
		cs.mu.Unlock()
		return
	}
	w := &constraintWatcher{query: query, ch: make(chan struct{})}
	cs.watchers = append(cs.watchers, w)
	cs.mu.Unlock()
	<-w.ch
}

func (cs *ConstraintStoreObject) tryAsk(query cue.Value) bool {
	cs.mu.Lock()
	defer cs.mu.Unlock()
	return query.Subsume(cs.store, cue.Final()) == nil
}

func (cs *ConstraintStoreObject) isConsistent() bool {
	cs.mu.Lock()
	defer cs.mu.Unlock()
	return cs.store.Validate() == nil
}

func isConstraintStoreValue(v vm.Value) bool {
	return vm.IsExtensionValue(v, vm.ConstraintStoreMarker)
}

func vmGetConstraintStore(v *vm.VM, val vm.Value) *ConstraintStoreObject {
	if o := vm.ExtensionObject(val, vm.ConstraintStoreMarker); o != nil {
		return o.(*ConstraintStoreObject)
	}
	return nil
}

func vmRegisterConstraintStore(v *vm.VM, cs *ConstraintStoreObject) vm.Value {
	return vm.NewExtensionValue(vm.ConstraintStoreMarker, cs)
}

func registerConstraintStorePrimitives(v *vm.VM) {
	csClass := v.CreateClass("ConstraintStore", v.ObjectClass)
	v.SetGlobal("ConstraintStore", v.ClassValue(csClass))
	v.RegisterSymbolDispatchEntry(vm.ConstraintStoreMarker, &vm.SymbolTypeEntry{Class: csClass})

	csClass.AddClassMethod0(v.Selectors, "new", func(vmInst *vm.VM, recv vm.Value) vm.Value {
		ctx := cuecontext.New()
		cs := &ConstraintStoreObject{
			ctx:   ctx,
			store: ctx.CompileString("_"),
		}
		return vmRegisterConstraintStore(vmInst, cs)
	})

	csClass.AddMethod1(v.Selectors, "primTell:", func(vmInst *vm.VM, recv vm.Value, constraintVal vm.Value) vm.Value {
		cs := vmGetConstraintStore(vmInst, recv)
		if cs == nil {
			return vmInst.NewFailureResult("invalid ConstraintStore")
		}

		cv := vmGetCueValue(vmInst, constraintVal)
		if cv == nil {
			return vmInst.NewFailureResult("argument must be a CueValue")
		}

		if err := cs.tell(cv.val); err != nil {
			return vmInst.NewFailureResult(fmt.Sprintf("conflicting constraint: %s", err))
		}
		return vmInst.NewSuccessResult(vm.True)
	})

	csClass.AddMethod1(v.Selectors, "primAsk:", func(vmInst *vm.VM, recv vm.Value, queryVal vm.Value) vm.Value {
		cs := vmGetConstraintStore(vmInst, recv)
		if cs == nil {
			return vm.Nil
		}

		cv := vmGetCueValue(vmInst, queryVal)
		if cv == nil {
			return vm.Nil
		}

		cs.ask(cv.val)
		return vm.True
	})

	csClass.AddMethod1(v.Selectors, "primTryAsk:", func(vmInst *vm.VM, recv vm.Value, queryVal vm.Value) vm.Value {
		cs := vmGetConstraintStore(vmInst, recv)
		if cs == nil {
			return vm.False
		}

		cv := vmGetCueValue(vmInst, queryVal)
		if cv == nil {
			return vm.False
		}

		if cs.tryAsk(cv.val) {
			return vm.True
		}
		return vm.False
	})

	csClass.AddMethod2(v.Selectors, "primWatch:do:", func(vmInst *vm.VM, recv vm.Value, queryVal vm.Value, blockVal vm.Value) vm.Value {
		cs := vmGetConstraintStore(vmInst, recv)
		if cs == nil {
			return vm.Nil
		}

		cv := vmGetCueValue(vmInst, queryVal)
		if cv == nil {
			return vm.Nil
		}

		cs.mu.Lock()
		if cv.val.Subsume(cs.store, cue.Final()) == nil {
			cs.mu.Unlock()
			vmInst.Send(blockVal, "value", nil)
			return vm.True
		}

		w := &constraintWatcher{query: cv.val, ch: make(chan struct{})}
		cs.watchers = append(cs.watchers, w)
		cs.mu.Unlock()

		go func() {
			<-w.ch
			vmInst.Send(blockVal, "value", nil)
		}()

		return vm.True
	})

	csClass.AddMethod0(v.Selectors, "primValue", func(vmInst *vm.VM, recv vm.Value) vm.Value {
		cs := vmGetConstraintStore(vmInst, recv)
		if cs == nil {
			return vm.Nil
		}

		cs.mu.Lock()
		storeVal := cs.store
		cs.mu.Unlock()

		obj := &CueValueObject{val: storeVal}
		return vmRegisterCueValue(vmInst, obj)
	})

	csClass.AddMethod0(v.Selectors, "primIsConsistent", func(vmInst *vm.VM, recv vm.Value) vm.Value {
		cs := vmGetConstraintStore(vmInst, recv)
		if cs == nil {
			return vm.False
		}

		if cs.isConsistent() {
			return vm.True
		}
		return vm.False
	})

	csClass.AddMethod0(v.Selectors, "primPrintString", func(vmInst *vm.VM, recv vm.Value) vm.Value {
		cs := vmGetConstraintStore(vmInst, recv)
		if cs == nil {
			return vmInst.Registry().NewStringValue("a ConstraintStore (invalid)")
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
		return vmInst.Registry().NewStringValue(desc)
	})
}
