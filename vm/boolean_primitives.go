package vm

// ---------------------------------------------------------------------------
// Boolean Primitives (True, False, UndefinedObject)
// ---------------------------------------------------------------------------

func (vm *VM) registerBooleanPrimitives() {
	// True class
	vm.TrueClass.AddMethod0(vm.Selectors, "not", func(_ interface{}, recv Value) Value {
		return False
	})

	vm.TrueClass.AddMethod1(vm.Selectors, "&", func(_ interface{}, recv Value, arg Value) Value {
		return arg
	})

	vm.TrueClass.AddMethod1(vm.Selectors, "|", func(_ interface{}, recv Value, arg Value) Value {
		return True
	})

	vm.TrueClass.AddMethod1(vm.Selectors, "ifTrue:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(block, nil)
	})

	vm.TrueClass.AddMethod1(vm.Selectors, "ifFalse:", func(_ interface{}, recv Value, block Value) Value {
		return Nil
	})

	vm.TrueClass.AddMethod2(vm.Selectors, "ifTrue:ifFalse:", func(vmPtr interface{}, recv Value, trueBlock, falseBlock Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(trueBlock, nil)
	})

	// and: - short-circuit and (evaluate block only if receiver is true)
	vm.TrueClass.AddMethod1(vm.Selectors, "and:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(block, nil)
	})

	// or: - short-circuit or (don't evaluate block since receiver is true)
	vm.TrueClass.AddMethod1(vm.Selectors, "or:", func(_ interface{}, recv Value, block Value) Value {
		return True
	})

	// xor: - exclusive or
	vm.TrueClass.AddMethod1(vm.Selectors, "xor:", func(_ interface{}, recv Value, arg Value) Value {
		if arg == True {
			return False
		}
		return True
	})

	// eqv: - equivalence (same as =)
	vm.TrueClass.AddMethod1(vm.Selectors, "eqv:", func(_ interface{}, recv Value, arg Value) Value {
		if arg == True {
			return True
		}
		return False
	})

	// False class
	vm.FalseClass.AddMethod0(vm.Selectors, "not", func(_ interface{}, recv Value) Value {
		return True
	})

	vm.FalseClass.AddMethod1(vm.Selectors, "&", func(_ interface{}, recv Value, arg Value) Value {
		return False
	})

	vm.FalseClass.AddMethod1(vm.Selectors, "|", func(_ interface{}, recv Value, arg Value) Value {
		return arg
	})

	vm.FalseClass.AddMethod1(vm.Selectors, "ifTrue:", func(_ interface{}, recv Value, block Value) Value {
		return Nil
	})

	vm.FalseClass.AddMethod1(vm.Selectors, "ifFalse:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(block, nil)
	})

	vm.FalseClass.AddMethod2(vm.Selectors, "ifTrue:ifFalse:", func(vmPtr interface{}, recv Value, trueBlock, falseBlock Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(falseBlock, nil)
	})

	// and: - short-circuit and (don't evaluate block since receiver is false)
	vm.FalseClass.AddMethod1(vm.Selectors, "and:", func(_ interface{}, recv Value, block Value) Value {
		return False
	})

	// or: - short-circuit or (evaluate block since receiver is false)
	vm.FalseClass.AddMethod1(vm.Selectors, "or:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(block, nil)
	})

	// xor: - exclusive or
	vm.FalseClass.AddMethod1(vm.Selectors, "xor:", func(_ interface{}, recv Value, arg Value) Value {
		if arg == False {
			return False
		}
		return True
	})

	// eqv: - equivalence
	vm.FalseClass.AddMethod1(vm.Selectors, "eqv:", func(_ interface{}, recv Value, arg Value) Value {
		if arg == False {
			return True
		}
		return False
	})

	// UndefinedObject (nil)
	vm.UndefinedObjectClass.AddMethod0(vm.Selectors, "isNil", func(_ interface{}, recv Value) Value {
		return True
	})

	vm.UndefinedObjectClass.AddMethod0(vm.Selectors, "notNil", func(_ interface{}, recv Value) Value {
		return False
	})

	vm.UndefinedObjectClass.AddMethod1(vm.Selectors, "ifNil:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(block, nil)
	})

	vm.UndefinedObjectClass.AddMethod1(vm.Selectors, "ifNotNil:", func(_ interface{}, recv Value, block Value) Value {
		return Nil
	})

	// ifNil:ifNotNil: - evaluate first block
	vm.UndefinedObjectClass.AddMethod2(vm.Selectors, "ifNil:ifNotNil:", func(vmPtr interface{}, recv Value, nilBlock, notNilBlock Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(nilBlock, nil)
	})
}

// ReRegisterBooleanPrimitives forces re-registration of True/False primitives.
// Call this after loading an image to ensure primitives override any compiled methods.
func (vm *VM) ReRegisterBooleanPrimitives() {
	// True class - must override Boolean.mag's abstract methods
	vm.TrueClass.AddMethod1(vm.Selectors, "ifTrue:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(block, nil)
	})

	vm.TrueClass.AddMethod1(vm.Selectors, "ifFalse:", func(_ interface{}, recv Value, block Value) Value {
		return Nil
	})

	vm.TrueClass.AddMethod2(vm.Selectors, "ifTrue:ifFalse:", func(vmPtr interface{}, recv Value, trueBlock, falseBlock Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(trueBlock, nil)
	})

	vm.TrueClass.AddMethod1(vm.Selectors, "and:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(block, nil)
	})

	vm.TrueClass.AddMethod1(vm.Selectors, "or:", func(_ interface{}, recv Value, block Value) Value {
		return True
	})

	// False class - must override Boolean.mag's abstract methods
	vm.FalseClass.AddMethod1(vm.Selectors, "ifTrue:", func(_ interface{}, recv Value, block Value) Value {
		return Nil
	})

	vm.FalseClass.AddMethod1(vm.Selectors, "ifFalse:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(block, nil)
	})

	vm.FalseClass.AddMethod2(vm.Selectors, "ifTrue:ifFalse:", func(vmPtr interface{}, recv Value, trueBlock, falseBlock Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(falseBlock, nil)
	})

	vm.FalseClass.AddMethod1(vm.Selectors, "and:", func(_ interface{}, recv Value, block Value) Value {
		return False
	})

	vm.FalseClass.AddMethod1(vm.Selectors, "or:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(block, nil)
	})
}

// ReRegisterNilPrimitives forces re-registration of nil-related primitives.
// Call this after loading an image to ensure primitives override any compiled methods.
func (vm *VM) ReRegisterNilPrimitives() {
	vm.UndefinedObjectClass.AddMethod0(vm.Selectors, "isNil", func(_ interface{}, recv Value) Value {
		return True
	})

	vm.UndefinedObjectClass.AddMethod0(vm.Selectors, "notNil", func(_ interface{}, recv Value) Value {
		return False
	})

	vm.UndefinedObjectClass.AddMethod1(vm.Selectors, "ifNil:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(block, nil)
	})

	vm.UndefinedObjectClass.AddMethod1(vm.Selectors, "ifNotNil:", func(_ interface{}, recv Value, block Value) Value {
		return Nil
	})

	vm.UndefinedObjectClass.AddMethod2(vm.Selectors, "ifNil:ifNotNil:", func(vmPtr interface{}, recv Value, nilBlock, notNilBlock Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(nilBlock, nil)
	})
}
