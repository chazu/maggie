// Package runtime provides the native execution environment for Trashtalk.
// This file implements the Object base class methods.
package runtime

import (
	"fmt"
)

// RegisterObjectClass registers the Object base class with the runtime
// This should be called during runtime initialization
func RegisterObjectClass(r *Runtime) *Class {
	methods := NewMethodTable()

	// perform: selector - dynamic method dispatch by name
	methods.AddInstanceMethod("perform_", func(self *Instance, args []Value) Value {
		if len(args) < 1 {
			return ErrorValue("perform: requires selector argument")
		}
		selector := args[0].AsString()
		return r.SendDirect(self, selector, nil)
	}, 1, 0)

	// perform: selector with: arg1 - dispatch with one argument
	methods.AddInstanceMethod("perform_with_", func(self *Instance, args []Value) Value {
		if len(args) < 2 {
			return ErrorValue("perform:with: requires selector and argument")
		}
		selector := args[0].AsString()
		methodArgs := []Value{args[1]}
		return r.SendDirect(self, selector, methodArgs)
	}, 2, 0)

	// perform: selector with: arg1 with: arg2 - dispatch with two arguments
	methods.AddInstanceMethod("perform_with_with_", func(self *Instance, args []Value) Value {
		if len(args) < 3 {
			return ErrorValue("perform:with:with: requires selector and two arguments")
		}
		selector := args[0].AsString()
		methodArgs := []Value{args[1], args[2]}
		return r.SendDirect(self, selector, methodArgs)
	}, 3, 0)

	// perform: selector with: arg1 with: arg2 with: arg3 - dispatch with three arguments
	methods.AddInstanceMethod("perform_with_with_with_", func(self *Instance, args []Value) Value {
		if len(args) < 4 {
			return ErrorValue("perform:with:with:with: requires selector and three arguments")
		}
		selector := args[0].AsString()
		methodArgs := []Value{args[1], args[2], args[3]}
		return r.SendDirect(self, selector, methodArgs)
	}, 4, 0)

	// printString - return canonical string representation
	methods.AddInstanceMethod("printString", func(self *Instance, args []Value) Value {
		return StringValue(fmt.Sprintf("<%s %s>", self.ClassName, self.ID))
	}, 0, 0)

	// class - return class name
	methods.AddInstanceMethod("class", func(self *Instance, args []Value) Value {
		return StringValue(self.ClassName)
	}, 0, 0)

	// id - return instance ID
	methods.AddInstanceMethod("id", func(self *Instance, args []Value) Value {
		return StringValue(self.ID)
	}, 0, 0)

	// new - class method to create new instance
	methods.AddClassMethod("new", func(self *Instance, args []Value) Value {
		// In class methods, self is nil, but we get className from the dispatch
		// This is handled by the dispatcher's handleNew method
		return NilValue() // Placeholder - actual creation done by dispatcher
	}, 0, 0)

	return r.RegisterClass("Object", "", nil, methods)
}
