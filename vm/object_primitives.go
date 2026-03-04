package vm

import (
	"fmt"
	"os"
)

// ---------------------------------------------------------------------------
// Object Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerObjectPrimitives() {
	c := vm.ObjectClass

	// new - create a new instance (class-side primitive)
	// This is a class method - registered on ClassVTable
	// Handles both first-class class values and legacy symbol-based class references
	_ = vm.Selectors.Intern("new") // Ensure "new" selector is interned
	c.AddClassMethod0(vm.Selectors, "new", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		// Handle first-class class values
		if IsClassValue(recv) {
			cls := v.GetClassFromValue(recv)
			if cls != nil {
				instance := cls.NewInstance()
				v.keepAlive[instance] = struct{}{}
				return instance.ToValue()
			}
		}
		// Handle symbol-based class references (legacy)
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

	// basicNew - create a new instance without initialization (class-side primitive)
	// Same as new, but in Smalltalk convention, basicNew is the raw allocator
	c.AddClassMethod0(vm.Selectors, "basicNew", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if IsClassValue(recv) {
			cls := v.GetClassFromValue(recv)
			if cls != nil {
				instance := cls.NewInstance()
				v.keepAlive[instance] = struct{}{}
				return instance.ToValue()
			}
		}
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

	// class - return the class of the receiver (instance-side)
	c.AddMethod0(vm.Selectors, "class", func(_ interface{}, recv Value) Value {
		return vm.primitiveClass(recv)
	})

	// class - return the metaclass of the receiver (class-side)
	// When you send `class` to a class value (e.g., SmallInteger class),
	// you get the metaclass ("SmallInteger class").
	c.AddClassMethod0(vm.Selectors, "class", func(vmPtr interface{}, recv Value) Value {
		return vm.primitiveClass(recv)
	})

	// primClass - same as class, but a primitive that Maggie code can call
	c.AddMethod0(vm.Selectors, "primClass", func(_ interface{}, recv Value) Value {
		return vm.primitiveClass(recv)
	})

	// primIsKindOf: - check if receiver is an instance of aClass or a subclass
	c.AddMethod1(vm.Selectors, "primIsKindOf:", func(vmPtr interface{}, recv Value, aClass Value) Value {
		v := vmPtr.(*VM)
		receiverClass := v.ClassFor(recv)
		if receiverClass == nil {
			return False
		}
		// Extract the target class from the argument
		var targetClass *Class
		if isClassValue(aClass) {
			targetClass = v.registry.GetClassFromValue(aClass)
		} else if aClass.IsSymbol() {
			symName := v.Symbols.Name(aClass.SymbolID())
			targetClass = v.Classes.Lookup(symName)
		}
		if targetClass == nil {
			return False
		}
		if receiverClass.IsSubclassOf(targetClass) {
			return True
		}
		return False
	})

	// primIdentical: - identity comparison primitive (called from Object.mag's ==)
	c.AddMethod1(vm.Selectors, "primIdentical:", func(_ interface{}, recv Value, arg Value) Value {
		if recv == arg {
			return True
		}
		return False
	})

	// == - identity comparison (fallback if not overridden in Smalltalk)
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

	// isFailure — default: false (overridden by Failure)
	c.AddMethod0(vm.Selectors, "isFailure", func(_ interface{}, recv Value) Value {
		return False
	})

	// isSuccess — default: false (overridden by Success)
	c.AddMethod0(vm.Selectors, "isSuccess", func(_ interface{}, recv Value) Value {
		return False
	})

	// yourself
	c.AddMethod0(vm.Selectors, "yourself", func(_ interface{}, recv Value) Value {
		return recv
	})

	// ifNil: - for non-nil objects, return receiver (don't evaluate block)
	c.AddMethod1(vm.Selectors, "ifNil:", func(_ interface{}, recv Value, block Value) Value {
		return recv
	})

	// ifNotNil: - for non-nil objects, evaluate block with receiver as argument
	c.AddMethod1(vm.Selectors, "ifNotNil:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(block, []Value{recv})
	})

	// ifNil:ifNotNil: - for non-nil objects, evaluate second block with receiver
	c.AddMethod2(vm.Selectors, "ifNil:ifNotNil:", func(vmPtr interface{}, recv Value, nilBlock, notNilBlock Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(notNilBlock, []Value{recv})
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

	// doesNotUnderstand: - default error handler.
	// Receives a Message object with selector and arguments.
	// Panics with a "Maggie error: Message not understood: X" message
	// which is caught by ExecuteSafe.
	c.AddMethod1(vm.Selectors, "doesNotUnderstand:", func(vmPtr interface{}, recv Value, message Value) Value {
		v := vmPtr.(*VM)
		selectorName := "<unknown>"

		// Extract selector name from the Message object
		if message.IsObject() {
			obj := ObjectFromValue(message)
			if obj != nil && obj.NumSlots() > 0 {
				selectorSym := obj.GetSlot(0)
				if selectorSym.IsSymbol() {
					symID := selectorSym.SymbolID()
					selectorName = v.Symbols.Name(symID)
					if selectorName == "" {
						// Debug: symbol not found
						fmt.Fprintf(os.Stderr, "[DNU-handler] symbol ID %d not found in SymbolTable (size %d)\n", symID, len(v.Symbols.All()))
					}
				} else {
					fmt.Fprintf(os.Stderr, "[DNU-handler] slot 0 is not a symbol: %v\n", selectorSym)
				}
			} else {
				fmt.Fprintf(os.Stderr, "[DNU-handler] message object has %d slots\n", func() int { if obj != nil { return obj.NumSlots() }; return -1 }())
			}
		} else if IsStringValue(message) {
			// Backward compat: if a bare string is passed (old-style)
			selectorName = v.registry.GetStringContent(message)
		} else if message.IsSymbol() {
			selectorName = v.Symbols.Name(message.SymbolID())
		} else {
			fmt.Fprintf(os.Stderr, "[DNU-handler] message is not object/string/symbol: %v\n", message)
		}

		// Debug: show receiver info
		if selectorName == "" || selectorName == "<unknown>" {
			recvClass := "<nil-vtable>"
			if vt := v.interpreter.vtableFor(recv); vt != nil && vt.Class() != nil {
				recvClass = vt.Class().FullName()
			}
			fmt.Fprintf(os.Stderr, "[DNU-handler] receiver class: %s, selector: %q\n", recvClass, selectorName)
		}

		stackTrace := ""
		if v.interpreter != nil {
			stackTrace = "\nStack trace:\n" + v.interpreter.StackTrace()
		}
		panic("Maggie error: Message not understood: " + selectorName + stackTrace)
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
			msgStr = v.registry.GetStringContent(message)
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

	// ---------------------------------------------------------------------------
	// become: primitives - object identity swapping
	// ---------------------------------------------------------------------------

	// primShallowCopy - return a shallow copy of the receiver
	// For non-object values (ints, floats, symbols), returns self (immutable).
	// For objects, creates a new object with the same vtable and slot values.
	c.AddMethod0(vm.Selectors, "primShallowCopy", func(_ interface{}, recv Value) Value {
		if !recv.IsObject() {
			return recv
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return recv
		}
		n := obj.NumSlots()
		cp := NewObject(obj.VTablePtr(), n)
		for i := 0; i < n; i++ {
			cp.SetSlot(i, obj.GetSlot(i))
		}
		return cp.ToValue()
	})

	// become: - Two-way identity swap. All references to receiver see arg's
	// contents and vice versa. Swaps vtable, size, and all slot contents.
	c.AddMethod1(vm.Selectors, "become:", func(_ interface{}, recv Value, arg Value) Value {
		// Both must be objects
		if !recv.IsObject() || !arg.IsObject() {
			return recv // Can't become: non-objects
		}
		// Use Raw to get actual objects (not resolved through forwarding)
		objA := ObjectFromValueRaw(recv)
		objB := ObjectFromValueRaw(arg)
		if objA == nil || objB == nil {
			return recv
		}
		objA.Become(objB)
		return recv
	})

	// becomeForward: - One-way forwarding. All accesses to receiver will be
	// redirected to arg. The arg is unchanged. Useful for proxies and lazy loading.
	c.AddMethod1(vm.Selectors, "becomeForward:", func(_ interface{}, recv Value, arg Value) Value {
		// Both must be objects
		if !recv.IsObject() || !arg.IsObject() {
			return recv // Can't forward non-objects
		}
		// Use Raw to get actual object (not resolved through forwarding)
		objA := ObjectFromValueRaw(recv)
		objB := ObjectFromValueRaw(arg)
		if objA == nil || objB == nil {
			return recv
		}
		objA.BecomeForward(objB)
		return recv
	})

	// isForwarded - Returns true if this object has been forwarded via becomeForward:
	c.AddMethod0(vm.Selectors, "isForwarded", func(_ interface{}, recv Value) Value {
		if !recv.IsObject() {
			return False
		}
		obj := ObjectFromValueRaw(recv)
		if obj == nil {
			return False
		}
		if obj.IsForwarded() {
			return True
		}
		return False
	})

	// ---------------------------------------------------------------------------
	// Instance variable access primitives (for Inspector/reflection)
	// ---------------------------------------------------------------------------

	// instVarAt: index - Return the value of the instance variable at index (0-based)
	// Returns nil for non-objects or out-of-range indices
	c.AddMethod1(vm.Selectors, "instVarAt:", func(_ interface{}, recv Value, index Value) Value {
		if !recv.IsObject() {
			return Nil
		}
		if !index.IsSmallInt() {
			return Nil
		}
		idx := int(index.SmallInt())
		if idx < 0 {
			return Nil
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		if idx >= obj.NumSlots() {
			return Nil
		}
		return obj.GetSlot(idx)
	})

	// instVarAt:put: index value - Set the instance variable at index (0-based)
	// Returns the receiver. Does nothing for non-objects or out-of-range indices.
	c.AddMethod2(vm.Selectors, "instVarAt:put:", func(_ interface{}, recv Value, index Value, value Value) Value {
		if !recv.IsObject() {
			return recv
		}
		if !index.IsSmallInt() {
			return recv
		}
		idx := int(index.SmallInt())
		if idx < 0 {
			return recv
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return recv
		}
		if idx >= obj.NumSlots() {
			return recv
		}
		obj.SetSlot(idx, value)
		return recv
	})

	// instVarSize - Return the number of instance variables
	// Returns 0 for non-objects
	c.AddMethod0(vm.Selectors, "instVarSize", func(_ interface{}, recv Value) Value {
		if !recv.IsObject() {
			return FromSmallInt(0)
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(obj.NumSlots()))
	})
}
