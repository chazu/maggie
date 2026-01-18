package runtime

import (
	"fmt"
	"strings"
)

// Dispatcher handles message dispatch in the runtime
type Dispatcher struct {
	os          *ObjectSpace
	bashBridge  *BashBridge
	blockRunner *BlockRunner
}

// NewDispatcher creates a new message dispatcher
func NewDispatcher(os *ObjectSpace) *Dispatcher {
	return &Dispatcher{
		os: os,
	}
}

// SetBashBridge sets the bash fallback bridge
func (d *Dispatcher) SetBashBridge(bb *BashBridge) {
	d.bashBridge = bb
}

// SetBlockRunner sets the block execution engine
func (d *Dispatcher) SetBlockRunner(br *BlockRunner) {
	d.blockRunner = br
}

// Send dispatches a message to a receiver.
// receiver can be:
//   - An instance ID (lowercase_uuid format)
//   - A class name (for class method calls)
//
// Returns the result value
func (d *Dispatcher) Send(receiver string, selector string, args []Value) Value {
	// Check if receiver is a class name (starts with uppercase or contains ::)
	isClassMethod := false
	if len(receiver) > 0 {
		firstChar := receiver[0]
		isClassMethod = (firstChar >= 'A' && firstChar <= 'Z') || strings.Contains(receiver, "::")
	}

	if isClassMethod {
		return d.sendClassMessage(receiver, selector, args)
	}

	return d.sendInstanceMessage(receiver, selector, args)
}

// sendClassMessage dispatches a class method
func (d *Dispatcher) sendClassMessage(className string, selector string, args []Value) Value {
	// Handle special selectors
	if selector == "new" {
		return d.handleNew(className, args)
	}

	// Look up class method
	method := d.os.LookupMethod(className, selector, true)
	if method != nil {
		// Create a nil instance for class methods (self = nil)
		return method.Impl(nil, args)
	}

	// Fall back to bash
	if d.bashBridge != nil {
		return d.bashBridge.Fallback(className, selector, args)
	}

	return ErrorValue(fmt.Sprintf("unknown class method: %s %s", className, selector))
}

// sendInstanceMessage dispatches an instance method
func (d *Dispatcher) sendInstanceMessage(instanceID string, selector string, args []Value) Value {
	// Look up instance
	inst := d.os.GetInstance(instanceID)
	if inst == nil {
		// Try to load from persistence
		if d.bashBridge != nil {
			return d.bashBridge.Fallback(instanceID, selector, args)
		}
		return ErrorValue(fmt.Sprintf("instance not found: %s", instanceID))
	}

	// Look up method
	method := d.os.LookupMethod(inst.ClassName, selector, false)
	if method != nil {
		return method.Impl(inst, args)
	}

	// Fall back to bash
	if d.bashBridge != nil {
		return d.bashBridge.Fallback(instanceID, selector, args)
	}

	return ErrorValue(fmt.Sprintf("unknown method: %s %s", inst.ClassName, selector))
}

// handleNew creates a new instance
func (d *Dispatcher) handleNew(className string, args []Value) Value {
	inst, err := d.os.NewInstance(className)
	if err != nil {
		// Try bash fallback for unknown classes
		if d.bashBridge != nil {
			return d.bashBridge.Fallback(className, "new", args)
		}
		return ErrorValue(err.Error())
	}

	// Call initialize if provided
	initMethod := d.os.LookupMethod(className, "initialize", false)
	if initMethod != nil {
		initMethod.Impl(inst, args)
	}

	return InstanceValue(inst)
}

// SendDirect dispatches a message when you already have the instance pointer
// This is faster than Send because it skips the instance lookup
func (d *Dispatcher) SendDirect(inst *Instance, selector string, args []Value) Value {
	if inst == nil {
		return ErrorValue("nil instance")
	}

	// Look up method
	method := d.os.LookupMethod(inst.ClassName, selector, false)
	if method != nil {
		return method.Impl(inst, args)
	}

	// Fall back to bash
	if d.bashBridge != nil {
		return d.bashBridge.Fallback(inst.ID, selector, args)
	}

	return ErrorValue(fmt.Sprintf("unknown method: %s %s", inst.ClassName, selector))
}

// SendSuper dispatches a message starting from the superclass
func (d *Dispatcher) SendSuper(inst *Instance, selector string, args []Value) Value {
	if inst == nil {
		return ErrorValue("nil instance")
	}

	class := inst.Class
	if class == nil || class.SuperclassP == nil {
		return ErrorValue("no superclass")
	}

	// Look up method starting from superclass
	superClass := class.SuperclassP
	for superClass != nil {
		method := superClass.Methods.LookupInstanceMethod(selector)
		if method != nil {
			return method.Impl(inst, args)
		}
		superClass = superClass.SuperclassP
	}

	// Fall back to bash
	if d.bashBridge != nil {
		return d.bashBridge.Fallback(inst.ID, "super:"+selector, args)
	}

	return ErrorValue(fmt.Sprintf("unknown super method: %s", selector))
}

// InvokeBlock invokes a block with arguments
func (d *Dispatcher) InvokeBlock(blockID string, args []Value) Value {
	if d.blockRunner == nil {
		return ErrorValue("no block runner configured")
	}
	return d.blockRunner.Invoke(blockID, args)
}

// InvokeBlockDirect invokes a block when you have the pointer
func (d *Dispatcher) InvokeBlockDirect(block *Block, args []Value) Value {
	if d.blockRunner == nil {
		return ErrorValue("no block runner configured")
	}
	return d.blockRunner.InvokeDirect(block, args)
}

// DispatchResult contains the result of a dispatch operation
type DispatchResult struct {
	Value      Value
	ExitCode   int
	InstanceID string
}

// SendWithResult performs a dispatch and returns full result info
func (d *Dispatcher) SendWithResult(receiver string, selector string, args []Value) DispatchResult {
	result := d.Send(receiver, selector, args)

	dr := DispatchResult{
		Value:    result,
		ExitCode: 0,
	}

	if result.Type == TypeError {
		dr.ExitCode = 1
	}

	if result.Type == TypeInstance && result.InstanceVal != nil {
		dr.InstanceID = result.InstanceVal.ID
	}

	return dr
}
