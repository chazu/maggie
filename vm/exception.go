package vm

import (
	"fmt"
)

// ---------------------------------------------------------------------------
// Exception Handling Infrastructure
// ---------------------------------------------------------------------------

// ExceptionHandler represents an installed exception handler (from on:do:).
// The handler stack is maintained per-execution context.
type ExceptionHandler struct {
	ExceptionClass *Class       // The exception class this handler catches
	HandlerBlock   Value        // The block to evaluate [:ex | ...]
	FrameIndex     int          // Frame where handler was installed
	HomeFrame      int          // For non-local return from handler
	HomeSelf       Value        // Self context for handler block
	Captures       []Value      // Captures for handler block
	Prev           *ExceptionHandler // Link to previous handler (stack)
}

// ExceptionObject represents a signaled exception instance.
type ExceptionObject struct {
	ExceptionClass *Class // The exception class
	MessageText    Value  // Optional message/description
	Tag            Value  // Optional tag for identification
	Signaler       Value  // The object that signaled the exception
	Resumable      bool   // Whether the exception can be resumed
	Handled        bool   // Whether the exception has been handled
}

// Exception values are encoded using the Symbol tag with marker 8 << 24
// (channels use 1<<24, processes use 2<<24, results use 4<<24)
const exceptionMarker uint32 = 8 << 24

// IsException returns true if this value is an exception.
func (v Value) IsException() bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return (id & (0xFF << 24)) == exceptionMarker
}

// ExceptionID returns the exception ID for an exception value.
func (v Value) ExceptionID() uint32 {
	if !v.IsException() {
		panic("Value.ExceptionID: not an exception")
	}
	return v.SymbolID() & ^exceptionMarker
}

// FromExceptionID creates an exception value from an ID.
func FromExceptionID(id uint32) Value {
	return FromSymbolID(id | exceptionMarker)
}

// ---------------------------------------------------------------------------
// Exception signaling (uses Go panic/recover like NonLocalReturn)
// ---------------------------------------------------------------------------

// SignaledException is panicked when an exception is signaled.
type SignaledException struct {
	Exception Value // The exception value
	Object    *ExceptionObject
}

// ---------------------------------------------------------------------------
// Interpreter exception handler stack
// ---------------------------------------------------------------------------

// PushExceptionHandler adds a handler to the interpreter's exception stack.
func (i *Interpreter) PushExceptionHandler(handler *ExceptionHandler) {
	handler.Prev = i.exceptionHandlers
	i.exceptionHandlers = handler
}

// PopExceptionHandler removes the top handler from the stack.
func (i *Interpreter) PopExceptionHandler() *ExceptionHandler {
	if i.exceptionHandlers == nil {
		return nil
	}
	handler := i.exceptionHandlers
	i.exceptionHandlers = handler.Prev
	return handler
}

// FindHandler searches for a handler that can handle the given exception.
// Returns nil if no handler is found.
func (i *Interpreter) FindHandler(exceptionClass *Class) *ExceptionHandler {
	for h := i.exceptionHandlers; h != nil; h = h.Prev {
		// Check if this handler's class handles the exception
		// (exception class must be the handler class or a subclass of it)
		if i.isKindOf(exceptionClass, h.ExceptionClass) {
			return h
		}
	}
	return nil
}

// isKindOf checks if class is the same as or a subclass of targetClass.
func (i *Interpreter) isKindOf(class, targetClass *Class) bool {
	for c := class; c != nil; c = c.Superclass {
		if c == targetClass {
			return true
		}
	}
	return false
}

// UnwindHandlersToFrame removes handlers installed after the given frame.
func (i *Interpreter) UnwindHandlersToFrame(frameIndex int) {
	for i.exceptionHandlers != nil && i.exceptionHandlers.FrameIndex > frameIndex {
		i.exceptionHandlers = i.exceptionHandlers.Prev
	}
}

// ---------------------------------------------------------------------------
// Exception class registration
// ---------------------------------------------------------------------------

func (vm *VM) bootstrapExceptionClasses() {
	// Exception is the root of all exceptions
	vm.ExceptionClass = vm.createClass("Exception", vm.ObjectClass)

	// Error is for errors that typically can't be recovered from
	vm.ErrorClass = vm.createClass("Error", vm.ExceptionClass)

	// MessageNotUnderstood - sent when a message is not understood
	vm.MessageNotUnderstoodClass = vm.createClass("MessageNotUnderstood", vm.ErrorClass)

	// ZeroDivide - division by zero
	vm.ZeroDivideClass = vm.createClass("ZeroDivide", vm.ErrorClass)

	// SubscriptOutOfBounds - array index out of bounds
	vm.SubscriptOutOfBoundsClass = vm.createClass("SubscriptOutOfBounds", vm.ErrorClass)

	// StackOverflow - call stack depth exceeded
	vm.StackOverflowClass = vm.createClass("StackOverflow", vm.ErrorClass)

	// Warning is for non-fatal conditions
	vm.WarningClass = vm.createClass("Warning", vm.ExceptionClass)

	// Halt - used for debugging breakpoints
	vm.HaltClass = vm.createClass("Halt", vm.ExceptionClass)

	// Notification - informational, typically auto-resumed
	vm.NotificationClass = vm.createClass("Notification", vm.ExceptionClass)

	// Register in globals
	vm.Globals["Exception"] = vm.classValue(vm.ExceptionClass)
	vm.Globals["Error"] = vm.classValue(vm.ErrorClass)
	vm.Globals["MessageNotUnderstood"] = vm.classValue(vm.MessageNotUnderstoodClass)
	vm.Globals["ZeroDivide"] = vm.classValue(vm.ZeroDivideClass)
	vm.Globals["SubscriptOutOfBounds"] = vm.classValue(vm.SubscriptOutOfBoundsClass)
	vm.Globals["StackOverflow"] = vm.classValue(vm.StackOverflowClass)
	vm.Globals["Warning"] = vm.classValue(vm.WarningClass)
	vm.Globals["Halt"] = vm.classValue(vm.HaltClass)
	vm.Globals["Notification"] = vm.classValue(vm.NotificationClass)
}

// ---------------------------------------------------------------------------
// Exception primitives registration
// ---------------------------------------------------------------------------

func (vm *VM) registerExceptionPrimitives() {
	ex := vm.ExceptionClass

	// Exception class>>signal - create and signal a new exception
	ex.AddClassMethod0(vm.Selectors, "signal", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		// Get the class from the receiver (class-side)
		exClass := v.classFromValue(recv)
		if exClass == nil {
			exClass = v.ExceptionClass
		}
		return v.signalException(exClass, Nil)
	})

	// Exception class>>signal: messageText - create and signal with message
	ex.AddClassMethod1(vm.Selectors, "signal:", func(vmPtr interface{}, recv Value, msg Value) Value {
		v := vmPtr.(*VM)
		exClass := v.classFromValue(recv)
		if exClass == nil {
			exClass = v.ExceptionClass
		}
		return v.signalException(exClass, msg)
	})

	// Exception>>signal - signal this exception instance
	ex.AddMethod0(vm.Selectors, "signal", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		exObj := v.registry.GetException(recv.ExceptionID())
		if exObj == nil {
			// Not a registered exception, create one
			return v.signalException(v.ExceptionClass, Nil)
		}
		return v.signalExceptionObject(recv, exObj)
	})

	// Exception>>messageText - return the exception message
	ex.AddMethod0(vm.Selectors, "messageText", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		exObj := v.registry.GetException(recv.ExceptionID())
		if exObj == nil {
			return Nil
		}
		return exObj.MessageText
	})

	// Exception>>messageText: - set the exception message
	ex.AddMethod1(vm.Selectors, "messageText:", func(vmPtr interface{}, recv Value, msg Value) Value {
		v := vmPtr.(*VM)
		exObj := v.registry.GetException(recv.ExceptionID())
		if exObj != nil {
			exObj.MessageText = msg
		}
		return recv
	})

	// Exception>>tag - return the exception tag
	ex.AddMethod0(vm.Selectors, "tag", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		exObj := v.registry.GetException(recv.ExceptionID())
		if exObj == nil {
			return Nil
		}
		return exObj.Tag
	})

	// Exception>>tag: - set the exception tag
	ex.AddMethod1(vm.Selectors, "tag:", func(vmPtr interface{}, recv Value, tag Value) Value {
		v := vmPtr.(*VM)
		exObj := v.registry.GetException(recv.ExceptionID())
		if exObj != nil {
			exObj.Tag = tag
		}
		return recv
	})

	// Exception>>resume - resume execution after the signal point
	ex.AddMethod0(vm.Selectors, "resume", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		exObj := v.registry.GetException(recv.ExceptionID())
		if exObj == nil || !exObj.Resumable {
			return Nil
		}
		// Mark as handled and return nil to resume
		exObj.Handled = true
		return Nil
	})

	// Exception>>resume: value - resume with a specific value
	ex.AddMethod1(vm.Selectors, "resume:", func(vmPtr interface{}, recv Value, val Value) Value {
		v := vmPtr.(*VM)
		exObj := v.registry.GetException(recv.ExceptionID())
		if exObj == nil || !exObj.Resumable {
			return Nil
		}
		exObj.Handled = true
		return val
	})

	// Exception>>pass - pass the exception to the next handler
	ex.AddMethod0(vm.Selectors, "pass", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		exObj := v.registry.GetException(recv.ExceptionID())
		if exObj == nil {
			return Nil
		}
		// Re-signal to find next handler
		return v.signalExceptionObject(recv, exObj)
	})

	// Exception>>retry - retry the protected block
	ex.AddMethod0(vm.Selectors, "retry", func(_ interface{}, recv Value) Value {
		// Retry is complex - needs to re-evaluate the protected block
		// For now, just return nil
		return Nil
	})

	// Exception>>return - return nil from the protected block
	ex.AddMethod0(vm.Selectors, "return", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		exObj := v.registry.GetException(recv.ExceptionID())
		if exObj != nil {
			exObj.Handled = true
		}
		return Nil
	})

	// Exception>>return: value - return a specific value from the protected block
	ex.AddMethod1(vm.Selectors, "return:", func(vmPtr interface{}, recv Value, val Value) Value {
		v := vmPtr.(*VM)
		exObj := v.registry.GetException(recv.ExceptionID())
		if exObj != nil {
			exObj.Handled = true
		}
		return val
	})

	// Exception>>isResumable
	ex.AddMethod0(vm.Selectors, "isResumable", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		exObj := v.registry.GetException(recv.ExceptionID())
		if exObj == nil {
			return False
		}
		return FromBool(exObj.Resumable)
	})

	// Exception>>description - return a description string
	ex.AddMethod0(vm.Selectors, "description", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		exObj := v.registry.GetException(recv.ExceptionID())
		if exObj == nil {
			return v.registry.NewStringValue("an Exception")
		}
		className := "Exception"
		if exObj.ExceptionClass != nil {
			className = exObj.ExceptionClass.Name
		}
		if exObj.MessageText != Nil && IsStringValue(exObj.MessageText) {
			msg := v.registry.GetStringContent(exObj.MessageText)
			return v.registry.NewStringValue(fmt.Sprintf("%s: %s", className, msg))
		}
		return v.registry.NewStringValue(fmt.Sprintf("a %s", className))
	})

	// Exception>>printString
	ex.AddMethod0(vm.Selectors, "printString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		// Delegate to description
		return v.Send(recv, "description", nil)
	})
}

// ---------------------------------------------------------------------------
// Block on:do: primitive
// ---------------------------------------------------------------------------

func (vm *VM) registerExceptionBlockPrimitives() {
	b := vm.BlockClass

	// Block>>on:do: - evaluate block with exception handler
	b.AddMethod2(vm.Selectors, "on:do:", func(vmPtr interface{}, recv Value, exceptionClassVal Value, handlerBlock Value) Value {
		v := vmPtr.(*VM)

		// Get the exception class
		exceptionClass := v.classFromValue(exceptionClassVal)
		if exceptionClass == nil {
			exceptionClass = v.ExceptionClass
		}

		// Evaluate the protected block with the handler installed
		return v.evaluateBlockWithHandler(recv, exceptionClass, handlerBlock)
	})

	// Block>>ensure: - evaluate block, then always evaluate ensureBlock
	b.AddMethod1(vm.Selectors, "ensure:", func(vmPtr interface{}, recv Value, ensureBlock Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlockWithEnsure(recv, ensureBlock)
	})

	// Block>>ifCurtailed: - evaluate block, run curtailBlock only if exception occurs
	b.AddMethod1(vm.Selectors, "ifCurtailed:", func(vmPtr interface{}, recv Value, curtailBlock Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlockIfCurtailed(recv, curtailBlock)
	})
}

// ---------------------------------------------------------------------------
// VM helper methods for exception handling
// ---------------------------------------------------------------------------

// signalException creates and signals a new exception.
func (vm *VM) signalException(exClass *Class, messageText Value) Value {
	ex := &ExceptionObject{
		ExceptionClass: exClass,
		MessageText:    messageText,
		Resumable:      true, // Most exceptions are resumable by default
	}
	id := vm.registry.RegisterException(ex)
	exVal := FromExceptionID(id)
	return vm.signalExceptionObject(exVal, ex)
}

// signalExceptionObject signals an existing exception.
func (vm *VM) signalExceptionObject(exVal Value, ex *ExceptionObject) Value {
	// Find a handler for this exception
	handler := vm.interpreter.FindHandler(ex.ExceptionClass)

	if handler == nil {
		// No handler found - this is an unhandled exception
		// For now, panic with the exception (can be caught at top level)
		panic(SignaledException{Exception: exVal, Object: ex})
	}

	// Found a handler - evaluate the handler block with the exception
	// First, pop the handler from the stack so it doesn't catch its own errors
	vm.interpreter.PopExceptionHandler()

	// Get the handler block
	bv := vm.interpreter.getBlockValue(handler.HandlerBlock)
	if bv == nil {
		// Handler block is invalid
		return Nil
	}

	// Evaluate handler with the exception as argument
	result := vm.interpreter.ExecuteBlock(bv.Block, bv.Captures, []Value{exVal}, bv.HomeFrame, bv.HomeSelf, bv.HomeMethod)

	// If the exception wasn't explicitly handled (resume/return),
	// the handler's return value becomes the result
	ex.Handled = true
	return result
}

// evaluateBlockWithHandler evaluates a block with an exception handler installed.
func (vm *VM) evaluateBlockWithHandler(blockVal Value, exceptionClass *Class, handlerBlock Value) Value {
	// Get the protected block
	bv := vm.interpreter.getBlockValue(blockVal)
	if bv == nil {
		return Nil
	}

	// Get handler block info
	hbv := vm.interpreter.getBlockValue(handlerBlock)
	if hbv == nil {
		// No valid handler, just evaluate the block
		return vm.evaluateBlock(blockVal, nil)
	}

	// Install the exception handler
	handler := &ExceptionHandler{
		ExceptionClass: exceptionClass,
		HandlerBlock:   handlerBlock,
		FrameIndex:     vm.interpreter.fp,
		HomeFrame:      hbv.HomeFrame,
		HomeSelf:       hbv.HomeSelf,
		Captures:       hbv.Captures,
	}
	vm.interpreter.PushExceptionHandler(handler)

	// Evaluate the protected block, catching any signaled exceptions
	var result Value
	func() {
		defer func() {
			if r := recover(); r != nil {
				if sigEx, ok := r.(SignaledException); ok {
					// Exception was signaled - check if our handler handles it
					if vm.interpreter.isKindOf(sigEx.Object.ExceptionClass, exceptionClass) {
						// Our handler handles this exception.
						// Unwind the frame stack back to where the handler was
						// installed so the handler block can execute cleanly.
						for vm.interpreter.fp > handler.FrameIndex {
							vm.interpreter.popFrame()
						}
						// Evaluate the handler block
						result = vm.interpreter.ExecuteBlock(
							hbv.Block,
							hbv.Captures,
							[]Value{sigEx.Exception},
							hbv.HomeFrame,
							hbv.HomeSelf,
							hbv.HomeMethod,
						)
						sigEx.Object.Handled = true
						return
					}
					// Not our exception - re-panic
					panic(r)
				}
				// Not a SignaledException - re-panic
				panic(r)
			}
		}()
		result = vm.interpreter.ExecuteBlock(bv.Block, bv.Captures, nil, bv.HomeFrame, bv.HomeSelf, bv.HomeMethod)
	}()

	// Remove the handler
	vm.interpreter.PopExceptionHandler()

	return result
}

// evaluateBlockWithEnsure evaluates a block, then always evaluates the ensure block.
func (vm *VM) evaluateBlockWithEnsure(blockVal Value, ensureBlock Value) Value {
	bv := vm.interpreter.getBlockValue(blockVal)
	if bv == nil {
		return Nil
	}

	ebv := vm.interpreter.getBlockValue(ensureBlock)

	var result Value
	var didPanic bool
	var panicValue interface{}

	func() {
		defer func() {
			// Always evaluate the ensure block
			if ebv != nil {
				vm.interpreter.ExecuteBlock(ebv.Block, ebv.Captures, nil, ebv.HomeFrame, ebv.HomeSelf, ebv.HomeMethod)
			}
			// If there was a panic, we'll re-panic after ensure
			if r := recover(); r != nil {
				didPanic = true
				panicValue = r
			}
		}()
		result = vm.interpreter.ExecuteBlock(bv.Block, bv.Captures, nil, bv.HomeFrame, bv.HomeSelf, bv.HomeMethod)
	}()

	if didPanic {
		panic(panicValue)
	}

	return result
}

// evaluateBlockIfCurtailed evaluates a block; if an exception occurs, evaluates curtailBlock.
func (vm *VM) evaluateBlockIfCurtailed(blockVal Value, curtailBlock Value) Value {
	bv := vm.interpreter.getBlockValue(blockVal)
	if bv == nil {
		return Nil
	}

	cbv := vm.interpreter.getBlockValue(curtailBlock)

	var result Value
	var didPanic bool
	var panicValue interface{}

	func() {
		defer func() {
			if r := recover(); r != nil {
				// Exception occurred - evaluate curtail block
				if cbv != nil {
					vm.interpreter.ExecuteBlock(cbv.Block, cbv.Captures, nil, cbv.HomeFrame, cbv.HomeSelf, cbv.HomeMethod)
				}
				didPanic = true
				panicValue = r
			}
		}()
		result = vm.interpreter.ExecuteBlock(bv.Block, bv.Captures, nil, bv.HomeFrame, bv.HomeSelf, bv.HomeMethod)
	}()

	if didPanic {
		panic(panicValue)
	}

	return result
}

// classFromValue extracts a Class from a value (for class-side methods).
func (vm *VM) classFromValue(v Value) *Class {
	// Check for first-class class values first
	if isClassValue(v) {
		return getClassFromValue(v)
	}
	// If it's a symbol, look up the class by name (backward compatibility)
	if v.IsSymbol() {
		name := vm.Symbols.Name(v.SymbolID())
		return vm.Classes.Lookup(name)
	}
	// Otherwise, get the class of the value
	return vm.ClassFor(v)
}
