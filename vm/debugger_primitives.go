package vm

// ---------------------------------------------------------------------------
// Debugger Primitives for IDE integration
// ---------------------------------------------------------------------------

func (vm *VM) registerDebuggerPrimitives() {
	// Create Debugger class
	debuggerClass := vm.createClass("Debugger", vm.ObjectClass)
	vm.Globals["Debugger"] = vm.classValue(debuggerClass)

	// ---------------------------------------------------------------------------
	// Debugger activation
	// ---------------------------------------------------------------------------

	// activate - Enable the debug server
	debuggerClass.AddClassMethod0(vm.Selectors, "activate", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if v.Debugger != nil {
			v.Debugger.Activate()
			return True
		}
		return False
	})

	// deactivate - Disable the debug server
	debuggerClass.AddClassMethod0(vm.Selectors, "deactivate", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if v.Debugger != nil {
			v.Debugger.Deactivate()
			return True
		}
		return False
	})

	// isActive - Check if debug server is enabled
	debuggerClass.AddClassMethod0(vm.Selectors, "isActive", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if v.Debugger != nil && v.Debugger.IsActive() {
			return True
		}
		return False
	})

	// ---------------------------------------------------------------------------
	// Breakpoint management
	// ---------------------------------------------------------------------------

	// setBreakpoint:method:line: - Set a breakpoint
	// Returns Success or Failure
	debuggerClass.AddClassMethod3(vm.Selectors, "setBreakpoint:method:line:", func(vmPtr interface{}, recv Value, classArg, methodArg, lineArg Value) Value {
		v := vmPtr.(*VM)
		if v.Debugger == nil {
			return v.newFailureResult("Debugger not available")
		}

		className := v.valueToString(classArg)
		methodName := v.valueToString(methodArg)
		if className == "" || methodName == "" {
			return v.newFailureResult("Class and method name required")
		}

		if !lineArg.IsSmallInt() {
			return v.newFailureResult("Line must be an integer")
		}
		line := int(lineArg.SmallInt())

		err := v.Debugger.SetBreakpoint(className, methodName, line)
		if err != nil {
			return v.newFailureResult(err.Error())
		}
		return v.newSuccessResult(True)
	})

	// removeBreakpoint:method:line: - Remove a breakpoint
	debuggerClass.AddClassMethod3(vm.Selectors, "removeBreakpoint:method:line:", func(vmPtr interface{}, recv Value, classArg, methodArg, lineArg Value) Value {
		v := vmPtr.(*VM)
		if v.Debugger == nil {
			return v.newFailureResult("Debugger not available")
		}

		className := v.valueToString(classArg)
		methodName := v.valueToString(methodArg)
		if className == "" || methodName == "" {
			return v.newFailureResult("Class and method name required")
		}

		if !lineArg.IsSmallInt() {
			return v.newFailureResult("Line must be an integer")
		}
		line := int(lineArg.SmallInt())

		err := v.Debugger.RemoveBreakpoint(className, methodName, line)
		if err != nil {
			return v.newFailureResult(err.Error())
		}
		return v.newSuccessResult(True)
	})

	// listBreakpoints - Return array of breakpoint dictionaries
	debuggerClass.AddClassMethod0(vm.Selectors, "listBreakpoints", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if v.Debugger == nil {
			return v.NewArrayWithElements(nil)
		}

		breakpoints := v.Debugger.ListBreakpoints()
		values := make([]Value, len(breakpoints))

		for i, bp := range breakpoints {
			dict := v.NewDictionary()
			v.DictionaryAtPut(dict, v.Symbols.SymbolValue("id"), FromSmallInt(int64(bp.ID)))
			v.DictionaryAtPut(dict, v.Symbols.SymbolValue("class"), v.registry.NewStringValue(bp.Class))
			v.DictionaryAtPut(dict, v.Symbols.SymbolValue("method"), v.registry.NewStringValue(bp.Method))
			v.DictionaryAtPut(dict, v.Symbols.SymbolValue("line"), FromSmallInt(int64(bp.Line)))
			if bp.Active {
				v.DictionaryAtPut(dict, v.Symbols.SymbolValue("active"), True)
			} else {
				v.DictionaryAtPut(dict, v.Symbols.SymbolValue("active"), False)
			}
			values[i] = dict
		}

		return v.NewArrayWithElements(values)
	})

	// clearAllBreakpoints - Remove all breakpoints
	debuggerClass.AddClassMethod0(vm.Selectors, "clearAllBreakpoints", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if v.Debugger != nil {
			v.Debugger.ClearAllBreakpoints()
		}
		return True
	})

	// enableBreakpoint:method:line: - Enable a disabled breakpoint
	debuggerClass.AddClassMethod3(vm.Selectors, "enableBreakpoint:method:line:", func(vmPtr interface{}, recv Value, classArg, methodArg, lineArg Value) Value {
		v := vmPtr.(*VM)
		if v.Debugger == nil {
			return v.newFailureResult("Debugger not available")
		}

		className := v.valueToString(classArg)
		methodName := v.valueToString(methodArg)
		line := int(lineArg.SmallInt())

		err := v.Debugger.EnableBreakpoint(className, methodName, line)
		if err != nil {
			return v.newFailureResult(err.Error())
		}
		return v.newSuccessResult(True)
	})

	// disableBreakpoint:method:line: - Disable a breakpoint without removing
	debuggerClass.AddClassMethod3(vm.Selectors, "disableBreakpoint:method:line:", func(vmPtr interface{}, recv Value, classArg, methodArg, lineArg Value) Value {
		v := vmPtr.(*VM)
		if v.Debugger == nil {
			return v.newFailureResult("Debugger not available")
		}

		className := v.valueToString(classArg)
		methodName := v.valueToString(methodArg)
		line := int(lineArg.SmallInt())

		err := v.Debugger.DisableBreakpoint(className, methodName, line)
		if err != nil {
			return v.newFailureResult(err.Error())
		}
		return v.newSuccessResult(True)
	})

	// ---------------------------------------------------------------------------
	// Execution control
	// ---------------------------------------------------------------------------

	// pause - Request the interpreter to pause
	debuggerClass.AddClassMethod0(vm.Selectors, "pause", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if v.Debugger != nil {
			v.Debugger.Pause()
			return True
		}
		return False
	})

	// resume - Continue execution after a pause
	debuggerClass.AddClassMethod0(vm.Selectors, "resume", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if v.Debugger != nil {
			v.Debugger.Resume()
			return True
		}
		return False
	})

	// stepOver - Step to next line, over method calls
	debuggerClass.AddClassMethod0(vm.Selectors, "stepOver", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if v.Debugger != nil && v.interpreter != nil {
			v.Debugger.StepOver(v.interpreter.fp, 0)
			return True
		}
		return False
	})

	// stepInto - Step into the next method call
	debuggerClass.AddClassMethod0(vm.Selectors, "stepInto", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if v.Debugger != nil {
			v.Debugger.StepInto()
			return True
		}
		return False
	})

	// stepOut - Step out of the current method
	debuggerClass.AddClassMethod0(vm.Selectors, "stepOut", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if v.Debugger != nil && v.interpreter != nil {
			v.Debugger.StepOut(v.interpreter.fp)
			return True
		}
		return False
	})

	// isPaused - Check if execution is paused
	debuggerClass.AddClassMethod0(vm.Selectors, "isPaused", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if v.Debugger != nil && v.Debugger.IsPaused() {
			return True
		}
		return False
	})

	// ---------------------------------------------------------------------------
	// Stack inspection
	// ---------------------------------------------------------------------------

	// getCallStack - Return array of stack frame dictionaries
	debuggerClass.AddClassMethod0(vm.Selectors, "getCallStack", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if v.Debugger == nil || v.interpreter == nil {
			return v.NewArrayWithElements(nil)
		}

		frames := v.Debugger.GetCallStack(v.interpreter)
		values := make([]Value, len(frames))

		for i, frame := range frames {
			dict := v.NewDictionary()
			v.DictionaryAtPut(dict, v.Symbols.SymbolValue("id"), FromSmallInt(int64(frame.ID)))
			v.DictionaryAtPut(dict, v.Symbols.SymbolValue("class"), v.registry.NewStringValue(frame.Class))
			v.DictionaryAtPut(dict, v.Symbols.SymbolValue("method"), v.registry.NewStringValue(frame.Method))
			v.DictionaryAtPut(dict, v.Symbols.SymbolValue("line"), FromSmallInt(int64(frame.Line)))
			v.DictionaryAtPut(dict, v.Symbols.SymbolValue("column"), FromSmallInt(int64(frame.Column)))
			if frame.IsBlock {
				v.DictionaryAtPut(dict, v.Symbols.SymbolValue("isBlock"), True)
			} else {
				v.DictionaryAtPut(dict, v.Symbols.SymbolValue("isBlock"), False)
			}
			values[i] = dict
		}

		return v.NewArrayWithElements(values)
	})

	// getVariables: frameId - Return array of variable dictionaries for a frame
	debuggerClass.AddClassMethod1(vm.Selectors, "getVariables:", func(vmPtr interface{}, recv Value, frameIdArg Value) Value {
		v := vmPtr.(*VM)
		if v.Debugger == nil || v.interpreter == nil {
			return v.NewArrayWithElements(nil)
		}

		if !frameIdArg.IsSmallInt() {
			return v.NewArrayWithElements(nil)
		}
		frameId := int(frameIdArg.SmallInt())

		variables := v.Debugger.GetVariables(v.interpreter, frameId)
		values := make([]Value, len(variables))

		for i, variable := range variables {
			dict := v.NewDictionary()
			v.DictionaryAtPut(dict, v.Symbols.SymbolValue("name"), v.registry.NewStringValue(variable.Name))
			v.DictionaryAtPut(dict, v.Symbols.SymbolValue("value"), v.registry.NewStringValue(variable.Value))
			v.DictionaryAtPut(dict, v.Symbols.SymbolValue("type"), v.registry.NewStringValue(variable.Type))
			values[i] = dict
		}

		return v.NewArrayWithElements(values)
	})
}
