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
	vm.globals["Sandbox"] = vm.classValue(sandboxClass)

	// Sandbox class>>run: aBlock
	// Forks the block in a restricted process using the VM's sync restrictions.
	// Returns a Process that the caller can wait on.
	sandboxClass.AddClassMethod1(vm.Selectors, "run:", func(v *VM, recv Value, blockVal Value) Value {
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

		proc, err := v.createProcess()
		if err != nil {
			return v.SignalPrimitiveError("Sandbox forkRestricted:", err.Error())
		}
		procValue, err := v.registerProcess(proc)
		if err != nil {
			return v.SignalPrimitiveError("Sandbox forkRestricted:", err.Error())
		}

		go func() {
			defer func() {
				v.HandleForkedPanic(proc, recover())
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
	sandboxClass.AddClassMethod0(vm.Selectors, "restrictions", func(v *VM, recv Value) Value {
		restrictions := v.SyncRestrictions()
		elems := make([]Value, len(restrictions))
		for i, name := range restrictions {
			elems[i] = v.registry.NewStringValue(name)
		}
		return v.NewArrayWithElements(elems)
	})

	// Sandbox class>>isRehydrated: className
	// Returns true if the named class was installed via the rehydration pipeline.
	sandboxClass.AddClassMethod1(vm.Selectors, "isRehydrated:", func(v *VM, recv Value, nameVal Value) Value {
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
