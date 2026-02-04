package vm

import "fmt"

// ---------------------------------------------------------------------------
// Docstring Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerDocstringPrimitives() {
	c := vm.ObjectClass

	// ---------------------------------------------------------------------------
	// Instance-side: sent to any object
	// ---------------------------------------------------------------------------

	// help - prints the class docstring and method list to stdout, returns receiver
	c.AddMethod0(vm.Selectors, "help", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cls := v.ClassFor(recv)
		if cls == nil {
			return Nil
		}
		fmt.Println(FormatClassHelp(cls, v.Selectors))
		return recv
	})

	// docString - returns the class docstring as a string (or nil)
	c.AddMethod0(vm.Selectors, "docString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cls := v.ClassFor(recv)
		if cls == nil {
			return Nil
		}
		if cls.DocString == "" {
			return Nil
		}
		return v.registry.NewStringValue(cls.DocString)
	})

	// ---------------------------------------------------------------------------
	// Class-side: sent to class values
	// ---------------------------------------------------------------------------

	// help - prints the class docstring and method list to stdout, returns receiver
	c.AddClassMethod0(vm.Selectors, "help", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cls := v.classFromValue(recv)
		if cls == nil {
			return Nil
		}
		fmt.Println(FormatClassHelp(cls, v.Selectors))
		return recv
	})

	// docString - returns the class docstring as a string (or nil)
	c.AddClassMethod0(vm.Selectors, "docString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cls := v.classFromValue(recv)
		if cls == nil {
			return Nil
		}
		if cls.DocString == "" {
			return Nil
		}
		return v.registry.NewStringValue(cls.DocString)
	})

	// help: aSymbol - prints the docstring for a specific method
	c.AddClassMethod1(vm.Selectors, "help:", func(vmPtr interface{}, recv Value, selector Value) Value {
		v := vmPtr.(*VM)
		cls := v.classFromValue(recv)
		if cls == nil {
			return Nil
		}

		selName := v.valueToString(selector)
		if selName == "" {
			return Nil
		}

		m := cls.MethodByName(selName)
		if m == nil {
			fmt.Printf("%s does not understand #%s\n", cls.Name, selName)
			return Nil
		}

		fmt.Println(formatMethodHelpAny(cls.Name, m))
		return recv
	})

	// methodDocFor: aSymbol - returns the docstring of a method as a string (or nil)
	c.AddClassMethod1(vm.Selectors, "methodDocFor:", func(vmPtr interface{}, recv Value, selector Value) Value {
		v := vmPtr.(*VM)
		cls := v.classFromValue(recv)
		if cls == nil {
			return Nil
		}

		selName := v.valueToString(selector)
		if selName == "" {
			return Nil
		}

		m := cls.MethodByName(selName)
		if m == nil {
			return Nil
		}

		doc := MethodDocString(m)
		if doc == "" {
			return Nil
		}
		return v.registry.NewStringValue(doc)
	})
}

// FormatClassHelp formats a class for display in help output.
func FormatClassHelp(cls *Class, selectors *SelectorTable) string {
	var s string

	s += cls.Name
	if cls.Superclass != nil {
		s += " (subclass of " + cls.Superclass.Name + ")"
	}
	s += "\n"

	if cls.DocString != "" {
		s += "\n" + cls.DocString + "\n"
	}

	// List instance methods
	localMethods := cls.VTable.LocalMethods()
	if len(localMethods) > 0 {
		s += "\nInstance methods:\n"
		for selectorID := range localMethods {
			name := selectors.Name(selectorID)
			if name != "" {
				s += "  " + name + "\n"
			}
		}
	}

	// List class methods
	classLocalMethods := cls.ClassVTable.LocalMethods()
	if len(classLocalMethods) > 0 {
		s += "\nClass methods:\n"
		for selectorID := range classLocalMethods {
			name := selectors.Name(selectorID)
			if name != "" {
				s += "  " + name + "\n"
			}
		}
	}

	return s
}

// formatMethodHelp formats a compiled method for display in help output.
func formatMethodHelp(className string, cm *CompiledMethod) string {
	return formatMethodHelpAny(className, cm)
}

// formatMethodHelpAny formats any method (compiled or primitive) for display in help output.
func formatMethodHelpAny(className string, m Method) string {
	var s string

	s += className + ">>" + MethodName(m) + "\n"

	doc := MethodDocString(m)
	if doc != "" {
		s += "\n" + doc + "\n"
	} else {
		s += "\n(no documentation)\n"
	}

	return s
}
