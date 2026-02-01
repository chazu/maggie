package vm

import (
	"os"
	"path/filepath"
)

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

	// evaluate:withLocals: - Compile and execute an expression with local variable bindings.
	// The locals parameter is a Maggie Dictionary mapping variable names (symbols/strings) to values.
	// Locals are overlaid onto globals during evaluation. Assignments write to local scope.
	// After execution, new/modified variables are written back to the locals dictionary.
	compilerClass.AddClassMethod2(vm.Selectors, "evaluate:withLocals:", func(vmPtr interface{}, recv Value, sourceVal, localsVal Value) Value {
		v := vmPtr.(*VM)

		// Get the source string
		var source string
		if IsStringValue(sourceVal) {
			source = GetStringContent(sourceVal)
		} else if sourceVal.IsSymbol() {
			source = v.Symbols.Name(sourceVal.SymbolID())
		} else {
			return v.newFailureResult("evaluate:withLocals: requires a String argument")
		}

		// Get the locals dictionary
		dict := GetDictionaryObject(localsVal)
		if dict == nil {
			return v.newFailureResult("evaluate:withLocals: requires a Dictionary for locals")
		}

		// Extract local variable names and values from the dictionary
		localNames := make(map[string]bool)
		savedGlobals := make(map[string]Value)

		for h, keyVal := range dict.Keys {
			var name string
			if keyVal.IsSymbol() {
				name = v.Symbols.Name(keyVal.SymbolID())
			} else if IsStringValue(keyVal) {
				name = GetStringContent(keyVal)
			} else {
				continue
			}

			localNames[name] = true

			// Save existing global value (if any) for restoration
			if existing, ok := v.interpreter.Globals[name]; ok {
				savedGlobals[name] = existing
			}

			// Inject local value into globals
			v.interpreter.Globals[name] = dict.Data[h]
		}

		// Take a snapshot of all global keys before execution
		// so we can detect new assignments
		preExecGlobals := make(map[string]bool)
		for k := range v.interpreter.Globals {
			preExecGlobals[k] = true
		}

		// Compile the expression
		method, err := v.CompileExpression(source)
		if err != nil {
			// Restore globals before returning error
			v.restoreGlobals(localNames, savedGlobals)
			return v.newFailureResult("Compilation error: " + err.Error())
		}
		if method == nil {
			v.restoreGlobals(localNames, savedGlobals)
			return v.newFailureResult("Compilation returned nil")
		}

		// Execute the compiled method
		result := v.Execute(method, Nil, nil)

		// Write back modified/new locals to the dictionary
		for name := range localNames {
			if val, ok := v.interpreter.Globals[name]; ok {
				// Write current value back to locals dict
				symKey := v.Symbols.SymbolValue(name)
				h := hashValue(symKey)
				dict.Data[h] = val
				dict.Keys[h] = symKey
			}
		}

		// Also capture any NEW variables assigned during execution
		// (variables that didn't exist in globals before and aren't class names)
		for name, val := range v.interpreter.Globals {
			if !preExecGlobals[name] && !localNames[name] {
				// This is a new variable created during evaluation
				// Write it to the locals dictionary
				symKey := v.Symbols.SymbolValue(name)
				h := hashValue(symKey)
				dict.Data[h] = val
				dict.Keys[h] = symKey
				localNames[name] = true

				// Save for cleanup (it wasn't in globals before)
				// savedGlobals won't have it, which means restoreGlobals
				// will delete it from globals
			}
		}

		// Restore globals to their pre-evaluation state
		v.restoreGlobals(localNames, savedGlobals)

		return result
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

	// fileIn: - Load and compile a .mag file into the VM
	// Returns the number of methods compiled, or a Failure
	compilerClass.AddClassMethod1(vm.Selectors, "fileIn:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)

		var path string
		if IsStringValue(pathVal) {
			path = GetStringContent(pathVal)
		} else {
			return v.newFailureResult("fileIn: requires a String path")
		}

		n, err := v.FileIn(path)
		if err != nil {
			return v.newFailureResult("fileIn: " + err.Error())
		}

		return FromSmallInt(int64(n))
	})

	// fileInAll: - Recursively load all .mag files from a directory
	// Returns the total number of methods compiled, or a Failure
	compilerClass.AddClassMethod1(vm.Selectors, "fileInAll:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)

		var path string
		if IsStringValue(pathVal) {
			path = GetStringContent(pathVal)
		} else {
			return v.newFailureResult("fileInAll: requires a String path")
		}

		n, err := v.FileInAll(path)
		if err != nil {
			return v.newFailureResult("fileInAll: " + err.Error())
		}

		return FromSmallInt(int64(n))
	})

	// fileOut:to: - Write a class to a .mag file
	// Returns the path written, or a Failure
	compilerClass.AddClassMethod2(vm.Selectors, "fileOut:to:", func(vmPtr interface{}, recv Value, classNameVal, pathVal Value) Value {
		v := vmPtr.(*VM)

		var className, path string
		if IsStringValue(classNameVal) {
			className = GetStringContent(classNameVal)
		} else if classNameVal.IsSymbol() {
			className = v.Symbols.Name(classNameVal.SymbolID())
		} else {
			return v.newFailureResult("fileOut:to: requires a String or Symbol class name")
		}

		if IsStringValue(pathVal) {
			path = GetStringContent(pathVal)
		} else {
			return v.newFailureResult("fileOut:to: requires a String path")
		}

		class := v.Classes.Lookup(className)
		if class == nil {
			return v.newFailureResult("fileOut:to: class not found: " + className)
		}

		source := FileOutClass(class, v.Selectors)
		if err := os.WriteFile(path, []byte(source), 0644); err != nil {
			return v.newFailureResult("fileOut:to: cannot write: " + err.Error())
		}

		return NewStringValue(path)
	})

	// fileOutNamespace:to: - Write all classes in a namespace to a directory
	// One file per class. Returns the number of files written, or a Failure.
	compilerClass.AddClassMethod2(vm.Selectors, "fileOutNamespace:to:", func(vmPtr interface{}, recv Value, nsVal, dirVal Value) Value {
		v := vmPtr.(*VM)

		var namespace, dir string
		if IsStringValue(nsVal) {
			namespace = GetStringContent(nsVal)
		} else if nsVal.IsSymbol() {
			namespace = v.Symbols.Name(nsVal.SymbolID())
		} else {
			return v.newFailureResult("fileOutNamespace:to: requires a String or Symbol namespace")
		}

		if IsStringValue(dirVal) {
			dir = GetStringContent(dirVal)
		} else {
			return v.newFailureResult("fileOutNamespace:to: requires a String directory path")
		}

		// Ensure directory exists
		if err := os.MkdirAll(dir, 0755); err != nil {
			return v.newFailureResult("fileOutNamespace:to: cannot create directory: " + err.Error())
		}

		sources := FileOutNamespace(namespace, v.Classes, v.Selectors)

		count := 0
		for name, source := range sources {
			filePath := filepath.Join(dir, name+".mag")
			if err := os.WriteFile(filePath, []byte(source), 0644); err != nil {
				return v.newFailureResult("fileOutNamespace:to: cannot write " + filePath + ": " + err.Error())
			}
			count++
		}

		return FromSmallInt(int64(count))
	})

	// saveImage: - Save the current VM state as an image file
	// Returns the path written, or a Failure
	compilerClass.AddClassMethod1(vm.Selectors, "saveImage:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)

		var path string
		if IsStringValue(pathVal) {
			path = GetStringContent(pathVal)
		} else {
			return v.newFailureResult("saveImage: requires a String path")
		}

		if err := v.SaveImage(path); err != nil {
			return v.newFailureResult("saveImage: " + err.Error())
		}

		return NewStringValue(path)
	})
}

// restoreGlobals restores the interpreter's Globals map after evaluate:withLocals: execution.
// For each local name: if it had a saved value, restore it; otherwise delete it from globals.
func (vm *VM) restoreGlobals(localNames map[string]bool, savedGlobals map[string]Value) {
	for name := range localNames {
		if saved, ok := savedGlobals[name]; ok {
			vm.interpreter.Globals[name] = saved
		} else {
			delete(vm.interpreter.Globals, name)
		}
	}
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
