package vm

// ---------------------------------------------------------------------------
// Object Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerObjectPrimitives() {
	c := vm.ObjectClass

	// new - create a new instance (class-side primitive)
	// This is a class method - registered on ClassVTable
	// When sent to a symbol representing a class, creates an instance of that class
	_ = vm.Selectors.Intern("new") // Ensure "new" selector is interned
	c.AddClassMethod0(vm.Selectors, "new", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		// If receiver is a symbol representing a class, create an instance
		if recv.IsSymbol() {
			symName := v.Symbols.Name(recv.SymbolID())
			if cls := v.Classes.Lookup(symName); cls != nil {
				instance := cls.NewInstance()
				// Keep a reference to prevent GC - Value is just uint64 to Go
				v.keepAlive[instance] = struct{}{}
				return instance.ToValue()
			}
		}
		// Otherwise, just return self (for instance-side call)
		return recv
	})

	// basicNew - create a new instance without initialization (class-side primitive)
	// Same as new, but in Smalltalk convention, basicNew is the raw allocator
	c.AddClassMethod0(vm.Selectors, "basicNew", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		// If receiver is a symbol representing a class, create an instance
		if recv.IsSymbol() {
			symName := v.Symbols.Name(recv.SymbolID())
			if cls := v.Classes.Lookup(symName); cls != nil {
				instance := cls.NewInstance()
				v.keepAlive[instance] = struct{}{}
				return instance.ToValue()
			}
		}
		return recv
	})

	// class - return the class of the receiver
	c.AddMethod0(vm.Selectors, "class", func(_ interface{}, recv Value) Value {
		return vm.primitiveClass(recv)
	})

	// primClass - same as class, but a primitive that Maggie code can call
	c.AddMethod0(vm.Selectors, "primClass", func(_ interface{}, recv Value) Value {
		return vm.primitiveClass(recv)
	})

	// == - identity comparison
	c.AddMethod1(vm.Selectors, "==", func(_ interface{}, recv Value, arg Value) Value {
		if recv == arg {
			return True
		}
		return False
	})

	// ~~ - identity non-equality
	c.AddMethod1(vm.Selectors, "~~", func(_ interface{}, recv Value, arg Value) Value {
		if recv != arg {
			return True
		}
		return False
	})

	// isNil
	c.AddMethod0(vm.Selectors, "isNil", func(_ interface{}, recv Value) Value {
		return False
	})

	// notNil
	c.AddMethod0(vm.Selectors, "notNil", func(_ interface{}, recv Value) Value {
		return True
	})

	// yourself
	c.AddMethod0(vm.Selectors, "yourself", func(_ interface{}, recv Value) Value {
		return recv
	})

	// ifNil: - for non-nil objects, don't evaluate block
	c.AddMethod1(vm.Selectors, "ifNil:", func(_ interface{}, recv Value, block Value) Value {
		return Nil
	})

	// ifNotNil: - for non-nil objects, evaluate block
	c.AddMethod1(vm.Selectors, "ifNotNil:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(block, nil)
	})

	// ifNil:ifNotNil: - for non-nil objects, evaluate second block
	c.AddMethod2(vm.Selectors, "ifNil:ifNotNil:", func(vmPtr interface{}, recv Value, nilBlock, notNilBlock Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(notNilBlock, nil)
	})

	// perform: - send message by selector
	c.AddMethod1(vm.Selectors, "perform:", func(vmPtr interface{}, recv Value, selector Value) Value {
		v := vmPtr.(*VM)
		if selector.IsSymbol() {
			selName := v.Symbols.Name(selector.SymbolID())
			return v.Send(recv, selName, nil)
		}
		return Nil
	})

	// perform:with: - send message with one argument
	c.AddMethod2(vm.Selectors, "perform:with:", func(vmPtr interface{}, recv Value, selector, arg Value) Value {
		v := vmPtr.(*VM)
		if selector.IsSymbol() {
			selName := v.Symbols.Name(selector.SymbolID())
			return v.Send(recv, selName, []Value{arg})
		}
		return Nil
	})

	// doesNotUnderstand: - default error handler
	c.AddMethod1(vm.Selectors, "doesNotUnderstand:", func(_ interface{}, recv Value, message Value) Value {
		// In a full implementation, this would raise an error
		// For now, return nil
		return Nil
	})

	// = - value equality (default to identity)
	c.AddMethod1(vm.Selectors, "=", func(_ interface{}, recv Value, arg Value) Value {
		if recv == arg {
			return True
		}
		return False
	})

	// ~= - value inequality
	c.AddMethod1(vm.Selectors, "~=", func(_ interface{}, recv Value, arg Value) Value {
		if recv != arg {
			return True
		}
		return False
	})

	// hash - default hash (identity-based)
	c.AddMethod0(vm.Selectors, "hash", func(_ interface{}, recv Value) Value {
		return FromSmallInt(int64(recv))
	})

	// primError: - raise an error (halt execution)
	c.AddMethod1(vm.Selectors, "primError:", func(vmPtr interface{}, recv Value, message Value) Value {
		v := vmPtr.(*VM)
		var msgStr string
		if IsStringValue(message) {
			msgStr = GetStringContent(message)
		} else if message.IsSymbol() {
			msgStr = v.Symbols.Name(message.SymbolID())
		} else {
			msgStr = "<unknown error>"
		}
		// Include stack trace in error message
		stackTrace := ""
		if v.interpreter != nil {
			stackTrace = "\nStack trace:\n" + v.interpreter.StackTrace()
		}
		panic("Maggie error: " + msgStr + stackTrace)
	})
}
