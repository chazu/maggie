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

// PassException is panicked when a handler calls "pass" to forward the
// exception to the next outer handler.
type PassException struct {
	Exception Value
	Object    *ExceptionObject
}

// RetryException is panicked when a handler calls "retry" to re-execute
// the protected block from the beginning.
type RetryException struct{}

// ResumeException is panicked when a handler calls "resume:" to resume
// execution after the signal point with a specific value.
type ResumeException struct {
	Value Value
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
		// Panic with ResumeException; the enclosing evaluateHandlerBlock
		// defer will catch it and use the value as the on:do: result.
		panic(ResumeException{Value: val})
	})

	// Exception>>pass - pass the exception to the next handler
	ex.AddMethod0(vm.Selectors, "pass", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		exObj := v.registry.GetException(recv.ExceptionID())
		if exObj == nil {
			return Nil
		}
		// Panic with PassException; the enclosing evaluateBlockWithHandler
		// defer will catch this, pop its handler, and re-panic with a
		// SignaledException so the next outer handler can catch it.
		panic(PassException{Exception: recv, Object: exObj})
	})

	// Exception>>retry - retry the protected block
	ex.AddMethod0(vm.Selectors, "retry", func(_ interface{}, recv Value) Value {
		// Panic with RetryException; the enclosing evaluateBlockWithHandler
		// defer will catch this and re-execute the protected block.
		panic(RetryException{})
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
	onDoFn := func(vmPtr interface{}, recv Value, exceptionClassVal Value, handlerBlock Value) Value {
		v := vmPtr.(*VM)

		// Get the exception class
		exceptionClass := v.classFromValue(exceptionClassVal)
		if exceptionClass == nil {
			exceptionClass = v.ExceptionClass
		}

		// Evaluate the protected block with the handler installed
		return v.evaluateBlockWithHandler(recv, exceptionClass, handlerBlock)
	}
	b.AddMethod2(vm.Selectors, "on:do:", onDoFn)
	b.AddMethod2(vm.Selectors, "primOn:do:", onDoFn)

	// Block>>ensure: - evaluate block, then always evaluate ensureBlock
	b.AddMethod1(vm.Selectors, "ensure:", func(vmPtr interface{}, recv Value, ensureBlock Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlockWithEnsure(recv, ensureBlock)
	})

	// Block>>ifCurtailed: - evaluate block, run curtailBlock only if exception occurs
	ifCurtailedFn := func(vmPtr interface{}, recv Value, curtailBlock Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlockIfCurtailed(recv, curtailBlock)
	}
	b.AddMethod1(vm.Selectors, "ifCurtailed:", ifCurtailedFn)
	b.AddMethod1(vm.Selectors, "primIfCurtailed:", ifCurtailedFn)
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
// It always panics with SignaledException; the nearest enclosing
// evaluateBlockWithHandler defer/recover will catch it.
func (vm *VM) signalExceptionObject(exVal Value, ex *ExceptionObject) Value {
	panic(SignaledException{Exception: exVal, Object: ex})
}

// handlerAction represents the outcome of evaluating a handler block.
type handlerAction int

const (
	handlerDone  handlerAction = iota // handler completed normally
	handlerPass                       // handler called pass
	handlerRetry                      // handler called retry
)

// handlerOutcome captures the result and control-flow action from a handler.
type handlerOutcome struct {
	action    handlerAction
	result    Value
	passExc   SignaledException // valid when action == handlerPass
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

	// Retry loop: if the handler calls retry, we re-execute the protected block.
	for {
		outcome := vm.executeProtectedBlock(bv, hbv, handler, exceptionClass)

		switch outcome.action {
		case handlerRetry:
			// Re-install the handler and loop to re-execute the protected block
			vm.interpreter.PushExceptionHandler(handler)
			continue
		case handlerPass:
			// Re-signal as a panic so the next outer handler catches it
			panic(SignaledException{
				Exception: outcome.passExc.Exception,
				Object:    outcome.passExc.Object,
			})
		default:
			// Normal completion or handler completed
			return outcome.result
		}
	}
}

// executeProtectedBlock runs the protected block inside a defer/recover.
// If an exception is caught, it evaluates the handler block and returns
// the outcome (which may indicate pass or retry).
func (vm *VM) executeProtectedBlock(
	bv *BlockValue,
	hbv *BlockValue,
	handler *ExceptionHandler,
	exceptionClass *Class,
) handlerOutcome {
	var result Value
	var outcome handlerOutcome
	caught := false

	func() {
		defer func() {
			if r := recover(); r != nil {
				if sigEx, ok := r.(SignaledException); ok {
					// Exception was signaled - check if our handler handles it
					if vm.interpreter.isKindOf(sigEx.Object.ExceptionClass, exceptionClass) {
						caught = true
						// Pop our handler so it does not catch its own
						// re-signals or passes from within the handler block.
						vm.interpreter.PopExceptionHandler()
						// Unwind the frame stack back to where the handler
						// was installed so the handler block executes cleanly.
						for vm.interpreter.fp > handler.FrameIndex {
							vm.interpreter.popFrame()
						}
						// Evaluate the handler block
						outcome = vm.evaluateHandlerBlock(hbv, sigEx)
						return
					}
				}
				// Not our exception or not a SignaledException - re-panic
				panic(r)
			}
		}()
		result = vm.interpreter.ExecuteBlock(bv.Block, bv.Captures, nil, bv.HomeFrame, bv.HomeSelf, bv.HomeMethod)
	}()

	// If NLR is unwinding through us, pop handler and propagate
	if vm.interpreter.unwinding {
		vm.interpreter.PopExceptionHandler()
		return handlerOutcome{action: handlerDone, result: Nil}
	}

	if !caught {
		// Normal completion - remove the handler
		vm.interpreter.PopExceptionHandler()
		return handlerOutcome{action: handlerDone, result: result}
	}
	return outcome
}

// evaluateHandlerBlock evaluates the handler block for a caught exception,
// handling pass, retry, and resume control flow.
func (vm *VM) evaluateHandlerBlock(hbv *BlockValue, sigEx SignaledException) handlerOutcome {
	var handlerResult Value
	var outcome handlerOutcome

	func() {
		defer func() {
			if r := recover(); r != nil {
				switch pe := r.(type) {
				case PassException:
					// pass: signal back to evaluateBlockWithHandler
					outcome = handlerOutcome{
						action:  handlerPass,
						passExc: SignaledException{Exception: pe.Exception, Object: pe.Object},
					}
					return
				case RetryException:
					// retry: signal back to evaluateBlockWithHandler
					outcome = handlerOutcome{action: handlerRetry}
					return
				case ResumeException:
					// resume: use the provided value as the result
					sigEx.Object.Handled = true
					outcome = handlerOutcome{action: handlerDone, result: pe.Value}
					return
				default:
					panic(r)
				}
			}
		}()
		handlerResult = vm.interpreter.ExecuteBlock(
			hbv.Block,
			hbv.Captures,
			[]Value{sigEx.Exception},
			hbv.HomeFrame,
			hbv.HomeSelf,
			hbv.HomeMethod,
		)
		sigEx.Object.Handled = true
	}()

	// If a special action was set by the defer, return it
	if outcome.action != handlerDone {
		return outcome
	}
	return handlerOutcome{action: handlerDone, result: handlerResult}
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
			// If there was a panic (SignaledException etc.), run ensure then re-panic
			if r := recover(); r != nil {
				// Save/clear any NLR unwinding state before running ensure
				wasUnwinding := vm.interpreter.unwinding
				savedValue := vm.interpreter.unwindValue
				savedTarget := vm.interpreter.unwindTarget
				vm.interpreter.unwinding = false

				if ebv != nil {
					vm.interpreter.ExecuteBlock(ebv.Block, ebv.Captures, nil, ebv.HomeFrame, ebv.HomeSelf, ebv.HomeMethod)
				}

				// Restore unwinding state
				vm.interpreter.unwinding = wasUnwinding
				vm.interpreter.unwindValue = savedValue
				vm.interpreter.unwindTarget = savedTarget

				didPanic = true
				panicValue = r
			}
		}()
		result = vm.interpreter.ExecuteBlock(bv.Block, bv.Captures, nil, bv.HomeFrame, bv.HomeSelf, bv.HomeMethod)
	}()

	if didPanic {
		panic(panicValue)
	}

	// Check for NLR unwinding — save/clear state, run ensure, restore
	if vm.interpreter.unwinding {
		savedValue := vm.interpreter.unwindValue
		savedTarget := vm.interpreter.unwindTarget
		vm.interpreter.unwinding = false

		if ebv != nil {
			vm.interpreter.ExecuteBlock(ebv.Block, ebv.Captures, nil, ebv.HomeFrame, ebv.HomeSelf, ebv.HomeMethod)
		}

		// Restore unwinding state (ensure block's own NLR replaces if it did one)
		if !vm.interpreter.unwinding {
			vm.interpreter.unwinding = true
			vm.interpreter.unwindValue = savedValue
			vm.interpreter.unwindTarget = savedTarget
		}
		return Nil
	}

	// Normal completion — always run ensure
	if ebv != nil {
		vm.interpreter.ExecuteBlock(ebv.Block, ebv.Captures, nil, ebv.HomeFrame, ebv.HomeSelf, ebv.HomeMethod)
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
				// Exception occurred (SignaledException etc.) - evaluate curtail block
				// Save/clear NLR state before running curtail
				wasUnwinding := vm.interpreter.unwinding
				savedValue := vm.interpreter.unwindValue
				savedTarget := vm.interpreter.unwindTarget
				vm.interpreter.unwinding = false

				if cbv != nil {
					vm.interpreter.ExecuteBlock(cbv.Block, cbv.Captures, nil, cbv.HomeFrame, cbv.HomeSelf, cbv.HomeMethod)
				}

				// Restore unwinding state
				vm.interpreter.unwinding = wasUnwinding
				vm.interpreter.unwindValue = savedValue
				vm.interpreter.unwindTarget = savedTarget

				didPanic = true
				panicValue = r
			}
		}()
		result = vm.interpreter.ExecuteBlock(bv.Block, bv.Captures, nil, bv.HomeFrame, bv.HomeSelf, bv.HomeMethod)
	}()

	if didPanic {
		panic(panicValue)
	}

	// NLR unwinding through us — curtailed, so run the curtail block
	if vm.interpreter.unwinding {
		savedValue := vm.interpreter.unwindValue
		savedTarget := vm.interpreter.unwindTarget
		vm.interpreter.unwinding = false

		if cbv != nil {
			vm.interpreter.ExecuteBlock(cbv.Block, cbv.Captures, nil, cbv.HomeFrame, cbv.HomeSelf, cbv.HomeMethod)
		}

		// Restore unwinding state (curtail block's own NLR replaces if it did one)
		if !vm.interpreter.unwinding {
			vm.interpreter.unwinding = true
			vm.interpreter.unwindValue = savedValue
			vm.interpreter.unwindTarget = savedTarget
		}
		return Nil
	}

	return result
}

// classFromValue extracts a Class from a value (for class-side methods).
func (vm *VM) classFromValue(v Value) *Class {
	// Check for first-class class values first
	if isClassValue(v) {
		return vm.registry.GetClassFromValue(v)
	}
	// If it's a symbol, look up the class by name (backward compatibility)
	if v.IsSymbol() {
		name := vm.Symbols.Name(v.SymbolID())
		return vm.Classes.Lookup(name)
	}
	// Otherwise, get the class of the value
	return vm.ClassFor(v)
}
