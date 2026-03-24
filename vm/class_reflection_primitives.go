package vm

import "strings"

// ---------------------------------------------------------------------------
// Class Reflection Primitives
// ---------------------------------------------------------------------------

// stripMethodPrefix removes "method:" or "classMethod:" prefix and surrounding brackets
// from source that was stored by methodSourceFor:, converting it back to old-style format
// that Compile()+convertToNewStyleFormat can handle.
// Handles leading docstrings: """...""" or "..." before the method: keyword.
func stripMethodPrefix(source string) string {
	trimmed := strings.TrimSpace(source)

	// Skip leading docstrings (triple-quoted """ or single "...")
	work := trimmed
	for {
		work = strings.TrimSpace(work)
		if strings.HasPrefix(work, `"""`) {
			// Triple-quoted docstring
			end := strings.Index(work[3:], `"""`)
			if end >= 0 {
				work = work[3+end+3:]
				continue
			}
		}
		if strings.HasPrefix(work, `"`) && !strings.HasPrefix(work, `""`) {
			// Single-line comment/docstring
			end := strings.Index(work[1:], `"`)
			if end >= 0 {
				work = work[1+end+1:]
				continue
			}
		}
		break
	}

	work = strings.TrimSpace(work)
	for _, prefix := range []string{"classMethod:", "method:"} {
		if strings.HasPrefix(work, prefix) {
			body := strings.TrimPrefix(work, prefix)
			body = strings.TrimSpace(body)
			// Remove surrounding [ ] if present
			if strings.HasSuffix(body, "]") {
				idx := strings.Index(body, "[")
				if idx > 0 {
					selector := strings.TrimSpace(body[:idx])
					inner := body[idx+1 : len(body)-1]
					return selector + "\n" + inner
				}
			}
			return body
		}
	}
	return source
}

func (vm *VM) registerClassReflectionPrimitives() {
	c := vm.ObjectClass

	// ---------------------------------------------------------------------------
	// Instance-side class reflection (sent to instances)
	// ---------------------------------------------------------------------------

	// className - returns the name of the receiver's class as a symbol
	c.AddMethod0(vm.Selectors, "className", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cls := v.ClassFor(recv)
		if cls == nil {
			return Nil
		}
		return v.Symbols.SymbolValue(cls.Name)
	})

	// ---------------------------------------------------------------------------
	// Class-side reflection (sent to class symbols)
	// ---------------------------------------------------------------------------

	// methodCategories - returns an array of all category names for this class
	c.AddClassMethod0(vm.Selectors, "methodCategories", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cls := v.classFromValue(recv)
		if cls == nil {
			return v.NewArrayWithElements(nil)
		}
		categories := cls.MethodCategories()
		values := make([]Value, len(categories))
		for i, cat := range categories {
			values[i] = v.Symbols.SymbolValue(cat)
		}
		return v.NewArrayWithElements(values)
	})

	// classMethodCategories - returns an array of all category names for class-side methods
	c.AddClassMethod0(vm.Selectors, "classMethodCategories", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cls := v.classFromValue(recv)
		if cls == nil {
			return v.NewArrayWithElements(nil)
		}
		categories := cls.ClassMethodCategories()
		values := make([]Value, len(categories))
		for i, cat := range categories {
			values[i] = v.Symbols.SymbolValue(cat)
		}
		return v.NewArrayWithElements(values)
	})

	// methodsInCategory: - returns an array of method names in the given category
	c.AddClassMethod1(vm.Selectors, "methodsInCategory:", func(vmPtr interface{}, recv Value, category Value) Value {
		v := vmPtr.(*VM)
		cls := v.classFromValue(recv)
		if cls == nil {
			return v.NewArrayWithElements(nil)
		}
		var catName string
		if category.IsSymbol() {
			catName = v.Symbols.Name(category.SymbolID())
		} else if IsStringValue(category) {
			catName = v.registry.GetStringContent(category)
		} else {
			return v.NewArrayWithElements(nil)
		}
		names := cls.MethodNamesInCategory(catName)
		values := make([]Value, len(names))
		for i, name := range names {
			values[i] = v.Symbols.SymbolValue(name)
		}
		return v.NewArrayWithElements(values)
	})

	// allMethodNames - returns an array of all method names in this class
	c.AddClassMethod0(vm.Selectors, "allMethodNames", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cls := v.classFromValue(recv)
		if cls == nil {
			return v.NewArrayWithElements(nil)
		}
		names := cls.AllMethodNames()
		values := make([]Value, len(names))
		for i, name := range names {
			values[i] = v.Symbols.SymbolValue(name)
		}
		return v.NewArrayWithElements(values)
	})

	// categoryOf: - returns the category of a method given its selector
	c.AddClassMethod1(vm.Selectors, "categoryOf:", func(vmPtr interface{}, recv Value, selector Value) Value {
		v := vmPtr.(*VM)
		cls := v.classFromValue(recv)
		if cls == nil {
			return Nil
		}
		var selName string
		if selector.IsSymbol() {
			selName = v.Symbols.Name(selector.SymbolID())
		} else if IsStringValue(selector) {
			selName = v.registry.GetStringContent(selector)
		} else {
			return Nil
		}
		method := cls.MethodNamed(selName)
		if method == nil {
			return Nil
		}
		cat := method.Category()
		if cat == "" {
			return Nil
		}
		return v.Symbols.SymbolValue(cat)
	})

	// allInstVarNames - returns an array of all instance variable names
	c.AddClassMethod0(vm.Selectors, "allInstVarNames", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cls := v.classFromValue(recv)
		if cls == nil {
			return v.NewArrayWithElements(nil)
		}
		names := cls.AllInstVarNames()
		values := make([]Value, len(names))
		for i, name := range names {
			values[i] = v.Symbols.SymbolValue(name)
		}
		return v.NewArrayWithElements(values)
	})

	// instVarNames - returns an array of this class's own instance variable names
	c.AddClassMethod0(vm.Selectors, "instVarNames", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cls := v.classFromValue(recv)
		if cls == nil {
			return v.NewArrayWithElements(nil)
		}
		values := make([]Value, len(cls.InstVars))
		for i, name := range cls.InstVars {
			values[i] = v.Symbols.SymbolValue(name)
		}
		return v.NewArrayWithElements(values)
	})

	// superclassName - returns the name of the superclass as a symbol
	c.AddClassMethod0(vm.Selectors, "superclassName", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cls := v.classFromValue(recv)
		if cls == nil || cls.Superclass == nil {
			return Nil
		}
		return v.Symbols.SymbolValue(cls.Superclass.Name)
	})

	// ---------------------------------------------------------------------------
	// System-wide class reflection
	// ---------------------------------------------------------------------------

	// allClasses - returns an array of all class objects in the system
	// Respects process-level restrictions: hidden classes are filtered out.
	c.AddClassMethod0(vm.Selectors, "allClasses", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		interp := v.currentInterpreter()
		classes := v.Classes.All()
		var values []Value
		for _, cls := range classes {
			if !interp.IsGlobalHidden(cls.FullName()) && !interp.IsGlobalHidden(cls.Name) {
				values = append(values, v.registry.RegisterClassValue(cls))
			}
		}
		return v.NewArrayWithElements(values)
	})

	// allClassesSorted - returns an array of all class objects, sorted alphabetically by name
	// Respects process-level restrictions: hidden classes are filtered out.
	c.AddClassMethod0(vm.Selectors, "allClassesSorted", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		interp := v.currentInterpreter()
		classes := v.Classes.All()
		// Filter and collect names
		var names []string
		classMap := make(map[string]*Class)
		for _, cls := range classes {
			if !interp.IsGlobalHidden(cls.FullName()) && !interp.IsGlobalHidden(cls.Name) {
				names = append(names, cls.Name)
				classMap[cls.Name] = cls
			}
		}
		// Simple bubble sort (classes list is small)
		for i := 0; i < len(names)-1; i++ {
			for j := 0; j < len(names)-i-1; j++ {
				if names[j] > names[j+1] {
					names[j], names[j+1] = names[j+1], names[j]
				}
			}
		}
		values := make([]Value, len(names))
		for i, name := range names {
			values[i] = v.registry.RegisterClassValue(classMap[name])
		}
		return v.NewArrayWithElements(values)
	})

	// ---------------------------------------------------------------------------
	// Class-side identity methods (name, superclass, printString)
	// These are on ObjectClass so all classes inherit them.
	// ---------------------------------------------------------------------------

	// name - returns the class name as a symbol
	c.AddClassMethod0(vm.Selectors, "name", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cls := v.classFromValue(recv)
		if cls == nil {
			return Nil
		}
		return v.Symbols.SymbolValue(cls.Name)
	})

	// superclass - returns the superclass as a class value, or nil
	c.AddClassMethod0(vm.Selectors, "superclass", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cls := v.classFromValue(recv)
		if cls == nil || cls.Superclass == nil {
			return Nil
		}
		return v.registry.RegisterClassValue(cls.Superclass)
	})

	// printString - returns the class name as a string
	c.AddClassMethod0(vm.Selectors, "printString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cls := v.classFromValue(recv)
		if cls == nil {
			return v.registry.NewStringValue("a Class")
		}
		return v.registry.NewStringValue(cls.Name)
	})

	// ---------------------------------------------------------------------------
	// Method source retrieval
	// ---------------------------------------------------------------------------

	// methodSourceFor: - returns the source code of a method as a string
	// Returns nil if the method doesn't exist or has no source
	c.AddClassMethod1(vm.Selectors, "methodSourceFor:", func(vmPtr interface{}, recv Value, selector Value) Value {
		v := vmPtr.(*VM)
		cls := v.classFromValue(recv)
		if cls == nil {
			return Nil
		}
		var selName string
		if selector.IsSymbol() {
			selName = v.Symbols.Name(selector.SymbolID())
		} else if IsStringValue(selector) {
			selName = v.registry.GetStringContent(selector)
		} else {
			return Nil
		}
		method := cls.MethodNamed(selName)
		if method == nil {
			return Nil
		}
		// MethodNamed returns *CompiledMethod directly
		if method.Source != "" {
			return v.registry.NewStringValue(method.Source)
		}
		return Nil
	})

	// classMethodSourceFor: - returns the source code of a class-side method
	c.AddClassMethod1(vm.Selectors, "classMethodSourceFor:", func(vmPtr interface{}, recv Value, selector Value) Value {
		v := vmPtr.(*VM)
		cls := v.classFromValue(recv)
		if cls == nil {
			return Nil
		}
		var selName string
		if selector.IsSymbol() {
			selName = v.Symbols.Name(selector.SymbolID())
		} else if IsStringValue(selector) {
			selName = v.registry.GetStringContent(selector)
		} else {
			return Nil
		}
		method := cls.LookupClassMethod(v.Selectors, selName)
		if method == nil {
			return Nil
		}
		// Try to get source from CompiledMethod
		if cm, ok := method.(*CompiledMethod); ok {
			if cm.Source != "" {
				return v.registry.NewStringValue(cm.Source)
			}
		}
		return Nil
	})

	// ---------------------------------------------------------------------------
	// Live method installation
	// ---------------------------------------------------------------------------

	// compileAndInstall: - compile a method source string and install it on this class.
	// Returns the selector name (Symbol) on success, or signals an error on failure.
	// Accepts both formats:
	//   'method: greet [ ^''hello'' ]'  (new-style, method: prefix stripped automatically)
	//   'greet\n    ^''hello'''         (old-style, selector on first line)
	c.AddClassMethod1(vm.Selectors, "compileAndInstall:", func(vmPtr interface{}, recv Value, sourceVal Value) Value {
		v := vmPtr.(*VM)
		cls := v.classFromValue(recv)
		if cls == nil {
			v.signalException(v.ErrorClass, v.registry.NewStringValue("compileAndInstall: receiver is not a class"))
			return Nil
		}
		var source string
		if IsStringValue(sourceVal) {
			source = v.registry.GetStringContent(sourceVal)
		} else {
			v.signalException(v.ErrorClass, v.registry.NewStringValue("compileAndInstall: argument must be a string"))
			return Nil
		}
		// Strip method:/classMethod: prefix if present — Compile() adds it back via convertToNewStyleFormat
		source = stripMethodPrefix(source)
		method, err := v.Compile(source, cls)
		if err != nil {
			v.signalException(v.ErrorClass, v.registry.NewStringValue("compileAndInstall: compile error: "+err.Error()))
			return Nil
		}
		if method == nil {
			v.signalException(v.ErrorClass, v.registry.NewStringValue("compileAndInstall: compilation returned nil"))
			return Nil
		}
		method.SetClass(cls)
		selectorID := v.Selectors.Intern(method.Name())
		cls.VTable.AddMethod(selectorID, method)
		return v.Symbols.SymbolValue(method.Name())
	})

	// compileAndInstallClassMethod: - compile and install a class-side method.
	c.AddClassMethod1(vm.Selectors, "compileAndInstallClassMethod:", func(vmPtr interface{}, recv Value, sourceVal Value) Value {
		v := vmPtr.(*VM)
		cls := v.classFromValue(recv)
		if cls == nil {
			v.signalException(v.ErrorClass, v.registry.NewStringValue("compileAndInstallClassMethod: receiver is not a class"))
			return Nil
		}
		var source string
		if IsStringValue(sourceVal) {
			source = v.registry.GetStringContent(sourceVal)
		} else {
			v.signalException(v.ErrorClass, v.registry.NewStringValue("compileAndInstallClassMethod: argument must be a string"))
			return Nil
		}
		source = stripMethodPrefix(source)
		method, err := v.Compile(source, cls)
		if err != nil {
			v.signalException(v.ErrorClass, v.registry.NewStringValue("compileAndInstallClassMethod: compile error: "+err.Error()))
			return Nil
		}
		if method == nil {
			v.signalException(v.ErrorClass, v.registry.NewStringValue("compileAndInstallClassMethod: compilation returned nil"))
			return Nil
		}
		method.SetClass(cls)
		selectorID := v.Selectors.Intern(method.Name())
		cls.ClassVTable.AddMethod(selectorID, method)
		return v.Symbols.SymbolValue(method.Name())
	})
}
