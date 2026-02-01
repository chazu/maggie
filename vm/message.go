package vm

// ---------------------------------------------------------------------------
// Message: Reified message objects for doesNotUnderstand:
// ---------------------------------------------------------------------------
//
// A Message is a regular heap-allocated Object with instance variables
// "selector" (a Symbol) and "arguments" (an Array). It is created when
// method dispatch fails, and passed to doesNotUnderstand:.

// bootstrapMessageClass creates the Message class with ivars.
func (vm *VM) bootstrapMessageClass() {
	vm.MessageClass = vm.createClassWithIvars("Message", vm.ObjectClass, []string{"selector", "arguments"})
	vm.Globals["Message"] = vm.classValue(vm.MessageClass)
}

// createMessage builds a Message instance for doesNotUnderstand:.
func (vm *VM) createMessage(selector Value, args []Value) Value {
	instance := vm.MessageClass.NewInstance()
	instance.SetSlot(0, selector) // selector (as Symbol)
	if args != nil && len(args) > 0 {
		argsArray := vm.NewArrayWithElements(args)
		instance.SetSlot(1, argsArray)
	} else {
		instance.SetSlot(1, vm.NewArray(0))
	}
	vm.keepAlive[instance] = struct{}{}
	return instance.ToValue()
}
