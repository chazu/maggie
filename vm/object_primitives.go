package vm

// ---------------------------------------------------------------------------
// Object Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerObjectPrimitives() {
	c := vm.ObjectClass

	// new - create a new instance (class-side primitive)
	// This is a class method - registered on ClassVTable
	// Handles both first-class class values and legacy symbol-based class references
	_ = vm.Selectors.Intern("new") // Ensure "new" selector is interned
	c.AddClassMethod0(vm.Selectors, "new", func(v *VM, recv Value) Value {
		// Handle first-class class values
		if IsClassValue(recv) {
			cls := v.GetClassFromValue(recv)
			if cls != nil {
				instance := cls.NewInstance()
				return instance.ToValue()
			}
		}
		// Handle symbol-based class references (legacy)
		if recv.IsSymbol() {
			symName := v.Symbols.Name(recv.SymbolID())
			if cls := v.Classes.Lookup(symName); cls != nil {
				instance := cls.NewInstance()
				return instance.ToValue()
			}
		}
		return recv
	})

	// basicNew - create a new instance without initialization (class-side primitive)
	// Same as new, but in Smalltalk convention, basicNew is the raw allocator
	c.AddClassMethod0(vm.Selectors, "basicNew", func(v *VM, recv Value) Value {
		if IsClassValue(recv) {
			cls := v.GetClassFromValue(recv)
			if cls != nil {
				instance := cls.NewInstance()
				return instance.ToValue()
			}
		}
		if recv.IsSymbol() {
			symName := v.Symbols.Name(recv.SymbolID())
			if cls := v.Classes.Lookup(symName); cls != nil {
				instance := cls.NewInstance()
				return instance.ToValue()
			}
		}
		return recv
	})

	// class - return the class of the receiver (instance-side)
	c.AddMethod0(vm.Selectors, "class", func(_ *VM, recv Value) Value {
		return vm.primitiveClass(recv)
	})

	// class - return the metaclass of the receiver (class-side)
	// When you send `class` to a class value (e.g., SmallInteger class),
	// you get the metaclass ("SmallInteger class").
	c.AddClassMethod0(vm.Selectors, "class", func(v *VM, recv Value) Value {
		return vm.primitiveClass(recv)
	})

	// primClass - same as class, but a primitive that Maggie code can call
	c.AddMethod0(vm.Selectors, "primClass", func(_ *VM, recv Value) Value {
		return vm.primitiveClass(recv)
	})

	// primAsCueValue is registered by the CUE contrib plugin

	// primIsKindOf: - check if receiver is an instance of aClass or a subclass
	c.AddMethod1(vm.Selectors, "primIsKindOf:", func(v *VM, recv Value, aClass Value) Value {
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
	c.AddMethod1(vm.Selectors, "primIdentical:", func(_ *VM, recv Value, arg Value) Value {
		if recv == arg {
			return True
		}
		return False
	})

	// == - identity comparison (fallback if not overridden in Smalltalk)
	c.AddMethod1(vm.Selectors, "==", func(_ *VM, recv Value, arg Value) Value {
		if recv == arg {
			return True
		}
		return False
	})

	// ~~ - identity non-equality
	c.AddMethod1(vm.Selectors, "~~", func(_ *VM, recv Value, arg Value) Value {
		if recv != arg {
			return True
		}
		return False
	})

	// isNil
	c.AddMethod0(vm.Selectors, "isNil", func(_ *VM, recv Value) Value {
		return False
	})

	// notNil
	c.AddMethod0(vm.Selectors, "notNil", func(_ *VM, recv Value) Value {
		return True
	})

	// isFailure — default: false (overridden by Failure)
	c.AddMethod0(vm.Selectors, "isFailure", func(_ *VM, recv Value) Value {
		return False
	})

	// isSuccess — default: false (overridden by Success)
	c.AddMethod0(vm.Selectors, "isSuccess", func(_ *VM, recv Value) Value {
		return False
	})

	// yourself
	c.AddMethod0(vm.Selectors, "yourself", func(_ *VM, recv Value) Value {
		return recv
	})

	// ifNil: - for non-nil objects, return receiver (don't evaluate block)
	c.AddMethod1(vm.Selectors, "ifNil:", func(_ *VM, recv Value, block Value) Value {
		return recv
	})

	// ifNotNil: - for non-nil objects, evaluate block with receiver as argument
	c.AddMethod1(vm.Selectors, "ifNotNil:", func(v *VM, recv Value, block Value) Value {
		return v.evaluateBlock(block, []Value{recv})
	})

	// ifNil:ifNotNil: - for non-nil objects, evaluate second block with receiver
	c.AddMethod2(vm.Selectors, "ifNil:ifNotNil:", func(v *VM, recv Value, nilBlock, notNilBlock Value) Value {
		return v.evaluateBlock(notNilBlock, []Value{recv})
	})

	// perform: - send message by selector
	c.AddMethod1(vm.Selectors, "perform:", func(v *VM, recv Value, selector Value) Value {
		if selector.IsSymbol() {
			selName := v.Symbols.Name(selector.SymbolID())
			return v.Send(recv, selName, nil)
		}
		return Nil
	})

	// perform:with: - send message with one argument
	c.AddMethod2(vm.Selectors, "perform:with:", func(v *VM, recv Value, selector, arg Value) Value {
		if selector.IsSymbol() {
			selName := v.Symbols.Name(selector.SymbolID())
			return v.Send(recv, selName, []Value{arg})
		}
		return Nil
	})

	// respondsTo: - true if receiver's class (or any superclass) implements aSelector.
	// Walks the receiver's vtable inheritance chain via the flat dispatch table
	// without allocating an Array of method names.
	c.AddMethod1(vm.Selectors, "respondsTo:", func(v *VM, recv Value, selector Value) Value {
		var selName string
		if selector.IsSymbol() {
			selName = v.Symbols.Name(selector.SymbolID())
		} else if IsStringValue(selector) {
			selName = v.registry.GetStringContent(selector)
		} else {
			return False
		}
		cls := v.ClassFor(recv)
		if cls == nil {
			return False
		}
		if cls.LookupMethod(v.Selectors, selName) != nil {
			return True
		}
		return False
	})

	// doesNotUnderstand: - default error handler.
	// Receives a Message object with selector and arguments.
	// Panics with a "Maggie error: Message not understood: X" message
	// which is caught by ExecuteSafe.
	c.AddMethod1(vm.Selectors, "doesNotUnderstand:", func(v *VM, recv Value, message Value) Value {
		selectorName := "<unknown>"

		// Extract selector name from the Message object
		if message.IsObject() {
			obj := ObjectFromValue(message)
			if obj != nil && obj.NumSlots() > 0 {
				selectorSym := obj.GetSlot(0)
				if selectorSym.IsSymbol() {
					selectorName = v.Symbols.Name(selectorSym.SymbolID())
				}
			}
		} else if IsStringValue(message) {
			// Backward compat: if a bare string is passed (old-style)
			selectorName = v.registry.GetStringContent(message)
		} else if message.IsSymbol() {
			selectorName = v.Symbols.Name(message.SymbolID())
		}
		if selectorName == "" {
			selectorName = "<unknown>"
		}

		// Signal a proper MessageNotUnderstood exception (catchable by on:do:)
		msgText := "Message not understood: " + selectorName
		v.signalException(v.MessageNotUnderstoodClass, v.registry.NewStringValue(msgText))
		return Nil // unreachable — signalException always panics
	})

	// = - value equality (default to identity)
	c.AddMethod1(vm.Selectors, "=", func(_ *VM, recv Value, arg Value) Value {
		if recv == arg {
			return True
		}
		return False
	})

	// ~= - value inequality
	c.AddMethod1(vm.Selectors, "~=", func(_ *VM, recv Value, arg Value) Value {
		if recv != arg {
			return True
		}
		return False
	})

	// hash - default hash (identity-based). Classes with content equality
	// (String) override this with a content hash to honor the hash/= contract.
	c.AddMethod0(vm.Selectors, "hash", func(_ *VM, recv Value) Value {
		return FromSmallInt(int64((recv.hi ^ uint64(uintptr(recv.ptr))) & 0x7FFFFFFFFFFF))
	})

	// identityHash - always identity-based, even for classes that override
	// hash with content hashing. Must NOT be overridden.
	c.AddMethod0(vm.Selectors, "identityHash", func(_ *VM, recv Value) Value {
		return FromSmallInt(int64((recv.hi ^ uint64(uintptr(recv.ptr))) & 0x7FFFFFFFFFFF))
	})

	// primError: - raise an error (signal proper exception, catchable by on:do:)
	c.AddMethod1(vm.Selectors, "primError:", func(v *VM, recv Value, message Value) Value {
		var msgStr string
		if IsStringValue(message) {
			msgStr = v.registry.GetStringContent(message)
		} else if message.IsSymbol() {
			msgStr = v.Symbols.Name(message.SymbolID())
		} else {
			msgStr = "<unknown error>"
		}
		v.signalException(v.ErrorClass, v.registry.NewStringValue(msgStr))
		return Nil // unreachable — signalException always panics
	})

	// ---------------------------------------------------------------------------
	// become: primitives - object identity swapping
	// ---------------------------------------------------------------------------

	// primShallowCopy - return a shallow copy of the receiver
	// For non-object values (ints, floats, symbols), returns self (immutable).
	// For objects, creates a new object with the same vtable and slot values.
	c.AddMethod0(vm.Selectors, "primShallowCopy", func(_ *VM, recv Value) Value {
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
	c.AddMethod1(vm.Selectors, "become:", func(_ *VM, recv Value, arg Value) Value {
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
	c.AddMethod1(vm.Selectors, "becomeForward:", func(_ *VM, recv Value, arg Value) Value {
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
	c.AddMethod0(vm.Selectors, "isForwarded", func(_ *VM, recv Value) Value {
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
	c.AddMethod1(vm.Selectors, "instVarAt:", func(_ *VM, recv Value, index Value) Value {
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
	c.AddMethod2(vm.Selectors, "instVarAt:put:", func(_ *VM, recv Value, index Value, value Value) Value {
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
	c.AddMethod0(vm.Selectors, "instVarSize", func(_ *VM, recv Value) Value {
		if !recv.IsObject() {
			return FromSmallInt(0)
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(obj.NumSlots()))
	})

	// --- Dependency / Change-Notification Protocol ---

	// primAddDependent: adds a dependent to the receiver's dependents list.
	// Uses identity (Value ==) to avoid duplicates.
	c.AddMethod1(vm.Selectors, "primAddDependent:", func(v *VM, recv Value, dep Value) Value {
		v.dependentsMu.Lock()
		deps := v.dependents[recv]
		// Check for duplicate by identity
		for _, d := range deps {
			if d == dep {
				v.dependentsMu.Unlock()
				return dep
			}
		}
		v.dependents[recv] = append(deps, dep)
		v.dependentsMu.Unlock()
		return dep
	})

	// primRemoveDependent: removes a dependent from the receiver's dependents list.
	c.AddMethod1(vm.Selectors, "primRemoveDependent:", func(v *VM, recv Value, dep Value) Value {
		v.dependentsMu.Lock()
		deps := v.dependents[recv]
		for i, d := range deps {
			if d == dep {
				// Remove by swapping with last element
				deps[i] = deps[len(deps)-1]
				deps[len(deps)-1] = Nil // clear for GC
				deps = deps[:len(deps)-1]
				if len(deps) == 0 {
					delete(v.dependents, recv)
				} else {
					v.dependents[recv] = deps
				}
				v.dependentsMu.Unlock()
				return dep
			}
		}
		v.dependentsMu.Unlock()
		return dep
	})

	// primDependents returns an Array of the receiver's dependents.
	c.AddMethod0(vm.Selectors, "primDependents", func(v *VM, recv Value) Value {
		v.dependentsMu.RLock()
		deps := v.dependents[recv]
		v.dependentsMu.RUnlock()
		if len(deps) == 0 {
			return v.NewArray(0)
		}
		// Copy slice so caller doesn't hold a reference to internal storage
		copied := make([]Value, len(deps))
		copy(copied, deps)
		return v.NewArrayWithElements(copied)
	})

	// primReleaseDependents removes all dependents for the receiver.
	c.AddMethod0(vm.Selectors, "primReleaseDependents", func(v *VM, recv Value) Value {
		v.dependentsMu.Lock()
		delete(v.dependents, recv)
		v.dependentsMu.Unlock()
		return recv
	})
}
