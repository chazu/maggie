package vm

// ---------------------------------------------------------------------------
// Compiler Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerCompilerPrimitives() {
	// Create Compiler class if it doesn't exist
	compilerClass := vm.Classes.Lookup("Compiler")
	if compilerClass == nil {
		compilerClass = vm.createClass("Compiler", vm.ObjectClass)
	}

	// Register global
	vm.Globals["Compiler"] = vm.classValue(compilerClass)

	// evaluate: - Compile and execute an expression string
	// Returns the result of evaluation, or a Failure if compilation fails
	compilerClass.AddClassMethod1(vm.Selectors, "evaluate:", func(vmPtr interface{}, recv Value, sourceVal Value) Value {
		v := vmPtr.(*VM)

		// Get the source string
		var source string
		if IsStringValue(sourceVal) {
			source = GetStringContent(sourceVal)
		} else if sourceVal.IsSymbol() {
			source = v.Symbols.Name(sourceVal.SymbolID())
		} else {
			return v.newFailureResult("evaluate: requires a String argument")
		}

		// Compile the expression
		method, err := v.CompileExpression(source)
		if err != nil {
			return v.newFailureResult("Compilation error: " + err.Error())
		}
		if method == nil {
			return v.newFailureResult("Compilation returned nil")
		}

		// Execute the compiled method with nil as receiver
		// (expressions don't have a specific receiver context)
		result := v.Execute(method, Nil, nil)

		return result
	})

	// evaluateFor:in: - Compile and execute an expression with a receiver
	// Useful for evaluating code in the context of an object
	compilerClass.AddClassMethod2(vm.Selectors, "evaluate:in:", func(vmPtr interface{}, recv Value, sourceVal, contextVal Value) Value {
		v := vmPtr.(*VM)

		// Get the source string
		var source string
		if IsStringValue(sourceVal) {
			source = GetStringContent(sourceVal)
		} else if sourceVal.IsSymbol() {
			source = v.Symbols.Name(sourceVal.SymbolID())
		} else {
			return v.newFailureResult("evaluate:in: requires a String argument")
		}

		// Compile the expression
		method, err := v.CompileExpression(source)
		if err != nil {
			return v.newFailureResult("Compilation error: " + err.Error())
		}
		if method == nil {
			return v.newFailureResult("Compilation returned nil")
		}

		// Execute with the provided context as receiver
		result := v.Execute(method, contextVal, nil)

		return result
	})

	// setGlobal:to: - Set a global variable directly
	// Useful for REPL bindings like 'it'
	compilerClass.AddClassMethod2(vm.Selectors, "setGlobal:to:", func(vmPtr interface{}, recv Value, nameVal, valueVal Value) Value {
		v := vmPtr.(*VM)

		// Get the global name
		var name string
		if nameVal.IsSymbol() {
			name = v.Symbols.Name(nameVal.SymbolID())
		} else if IsStringValue(nameVal) {
			name = GetStringContent(nameVal)
		} else {
			return v.newFailureResult("setGlobal:to: requires a Symbol or String name")
		}

		// Set the global
		v.interpreter.Globals[name] = valueVal

		return valueVal
	})

	// getGlobal: - Get a global variable directly
	// Returns nil if not found
	compilerClass.AddClassMethod1(vm.Selectors, "getGlobal:", func(vmPtr interface{}, recv Value, nameVal Value) Value {
		v := vmPtr.(*VM)

		// Get the global name
		var name string
		if nameVal.IsSymbol() {
			name = v.Symbols.Name(nameVal.SymbolID())
		} else if IsStringValue(nameVal) {
			name = GetStringContent(nameVal)
		} else {
			return v.newFailureResult("getGlobal: requires a Symbol or String name")
		}

		// Get the global
		if val, ok := v.interpreter.Globals[name]; ok {
			return val
		}

		return Nil
	})

	// compileMethod: - Compile a method definition and return the CompiledMethod info
	// Returns a Dictionary with bytecode, literals, etc. (same as Maggie Compiler.compile:)
	// This is mainly for tooling/IDE use
	compilerClass.AddClassMethod1(vm.Selectors, "compileMethod:", func(vmPtr interface{}, recv Value, sourceVal Value) Value {
		v := vmPtr.(*VM)

		var source string
		if IsStringValue(sourceVal) {
			source = GetStringContent(sourceVal)
		} else {
			return v.newFailureResult("compileMethod: requires a String argument")
		}

		method, err := v.Compile(source, nil)
		if err != nil {
			return v.newFailureResult("Compilation error: " + err.Error())
		}
		if method == nil {
			return v.newFailureResult("Compilation returned nil")
		}

		// Return info about the compiled method as a Dictionary
		return v.methodInfoDict(method)
	})
}

// newFailureResult creates a Failure result with the given reason string.
func (vm *VM) newFailureResult(reason string) Value {
	r := createResult(ResultFailure, NewStringValue(reason))
	return registerResult(r)
}

// methodInfoDict creates a Dictionary with info about a compiled method.
func (vm *VM) methodInfoDict(method *CompiledMethod) Value {
	dictVal := NewDictionaryValue()
	dict := GetDictionaryObject(dictVal)

	// Helper to add key-value pair
	put := func(key, value Value) {
		h := hashValue(key)
		dict.Data[h] = value
		dict.Keys[h] = key
	}

	// Add selector
	put(vm.Symbols.SymbolValue("selector"), vm.Symbols.SymbolValue(method.Name()))

	// Add arity
	put(vm.Symbols.SymbolValue("arity"), FromSmallInt(int64(method.Arity)))

	// Add bytecode size
	put(vm.Symbols.SymbolValue("bytecodeSize"), FromSmallInt(int64(len(method.Bytecode))))

	// Add literals count
	put(vm.Symbols.SymbolValue("literalsCount"), FromSmallInt(int64(len(method.Literals))))

	return dictVal
}
