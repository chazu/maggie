package vm

// ---------------------------------------------------------------------------
// Sandbox Primitives
// ---------------------------------------------------------------------------
//
// The Sandbox class provides a thin wrapper around the existing forkRestricted
// machinery for running received (rehydrated) code in a sandboxed process.
//
// Usage:
//   proc := Sandbox run: [UntrustedCode new doWork].
//   proc wait.
//
// The restriction list comes from the VM's syncRestrictions field, which is
// populated from [sync].capabilities in maggie.toml at startup.

func (vm *VM) registerSandboxPrimitives() {
	sandboxClass := vm.Classes.Lookup("Sandbox")
	if sandboxClass == nil {
		sandboxClass = vm.createClass("Sandbox", vm.ObjectClass)
	}
	vm.Globals["Sandbox"] = vm.classValue(sandboxClass)

	// Sandbox class>>run: aBlock
	// Forks the block in a restricted process using the VM's sync restrictions.
	// Returns a Process that the caller can wait on.
	sandboxClass.AddClassMethod1(vm.Selectors, "run:", func(vmPtr interface{}, recv Value, blockVal Value) Value {
		v := vmPtr.(*VM)
		bv := v.currentInterpreter().getBlockValue(blockVal)
		if bv == nil {
			return Nil
		}

		// Build the hidden map from syncRestrictions
		hidden := make(map[string]bool, len(v.syncRestrictions))
		for _, name := range v.syncRestrictions {
			hidden[name] = true
		}

		// Inherit any existing restrictions from the calling interpreter
		callerInterp := v.currentInterpreter()
		if callerInterp.hidden != nil {
			for name := range callerInterp.hidden {
				hidden[name] = true
			}
		}

		proc := v.createProcess()
		procValue := v.registerProcess(proc)

		go func() {
			defer func() {
				if r := recover(); r != nil {
					if nlr, ok := r.(NonLocalReturn); ok {
						proc.markDone(nlr.Value, nil)
					} else {
						proc.markDone(Nil, nil)
					}
				}
				v.unregisterInterpreter()
			}()

			interp := v.newForkedInterpreter(hidden)
			v.registerInterpreter(interp)
			result := interp.ExecuteBlockDetached(bv.Block, bv.Captures, nil, bv.HomeSelf, bv.HomeMethod)
			proc.markDone(result, nil)
		}()

		return procValue
	})

	// Sandbox class>>restrictions
	// Returns an Array of the current sync restriction names.
	sandboxClass.AddClassMethod0(vm.Selectors, "restrictions", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		restrictions := v.SyncRestrictions()
		elems := make([]Value, len(restrictions))
		for i, name := range restrictions {
			elems[i] = v.registry.NewStringValue(name)
		}
		return v.NewArrayWithElements(elems)
	})

	// Sandbox class>>isRehydrated: className
	// Returns true if the named class was installed via the rehydration pipeline.
	sandboxClass.AddClassMethod1(vm.Selectors, "isRehydrated:", func(vmPtr interface{}, recv Value, nameVal Value) Value {
		v := vmPtr.(*VM)
		var name string
		if IsStringValue(nameVal) {
			name = v.registry.GetStringContent(nameVal)
		} else if nameVal.IsSymbol() {
			name = v.Symbols.Name(nameVal.SymbolID())
		} else {
			return False
		}
		if v.IsRehydrated(name) {
			return True
		}
		return False
	})
}
