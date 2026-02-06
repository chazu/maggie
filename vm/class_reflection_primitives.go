package vm

// ---------------------------------------------------------------------------
// Class Reflection Primitives
// ---------------------------------------------------------------------------

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
				values = append(values, registerClassValue(cls))
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
			values[i] = registerClassValue(classMap[name])
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
		return registerClassValue(cls.Superclass)
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
}
