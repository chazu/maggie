package vm

import "fmt"

// registerMessagePrimitives registers primitives on MessageClass.
func (vm *VM) registerMessagePrimitives() {
	c := vm.MessageClass

	// selector - return slot 0 (Symbol)
	c.AddMethod0(vm.Selectors, "selector", func(_ interface{}, recv Value) Value {
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		return obj.GetSlot(0)
	})

	// arguments - return slot 1 (Array)
	c.AddMethod0(vm.Selectors, "arguments", func(_ interface{}, recv Value) Value {
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		return obj.GetSlot(1)
	})

	// sendTo: anObject - re-send the message to another object
	c.AddMethod1(vm.Selectors, "sendTo:", func(vmPtr interface{}, recv Value, target Value) Value {
		v := vmPtr.(*VM)
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		selectorSym := obj.GetSlot(0)
		argsVal := obj.GetSlot(1)

		// Get selector name
		var selectorName string
		if selectorSym.IsSymbol() {
			selectorName = v.Symbols.Name(selectorSym.SymbolID())
		}
		if selectorName == "" {
			return Nil
		}

		// Get arguments array
		var args []Value
		if argsVal.IsObject() {
			argsObj := ObjectFromValue(argsVal)
			if argsObj != nil {
				for i := 0; i < argsObj.NumSlots(); i++ {
					args = append(args, argsObj.GetSlot(i))
				}
			}
		}

		return v.Send(target, selectorName, args)
	})

	// printString - "a Message(selectorName)"
	c.AddMethod0(vm.Selectors, "printString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		obj := ObjectFromValue(recv)
		if obj == nil {
			return v.registry.NewStringValue("a Message")
		}
		selectorSym := obj.GetSlot(0)
		var selectorName string
		if selectorSym.IsSymbol() {
			selectorName = v.Symbols.Name(selectorSym.SymbolID())
		}
		return v.registry.NewStringValue(fmt.Sprintf("a Message(%s)", selectorName))
	})
}
