package vm

import (
	"reflect"
)

// ---------------------------------------------------------------------------
// Channel Select: Multi-channel waiting using reflect.Select
// ---------------------------------------------------------------------------

// SelectCase represents a channel operation for select.
type SelectCase struct {
	Channel *ChannelObject
	Dir     reflect.SelectDir // SelectRecv or SelectSend
	Value   Value             // For send operations
	Handler Value             // Block to execute on match
}

// primitiveSelect implements Go's select statement for multiple channels.
// It takes an array of SelectCase structs and an optional default handler.
// Returns the result of executing the matched handler block.
func (vm *VM) primitiveSelect(cases []SelectCase, defaultHandler Value) Value {
	interp := vm.currentInterpreter()

	// Build reflect.SelectCase slice
	reflectCases := make([]reflect.SelectCase, len(cases))
	for i, c := range cases {
		if c.Channel == nil {
			continue
		}
		reflectCases[i] = reflect.SelectCase{
			Dir:  c.Dir,
			Chan: reflect.ValueOf(c.Channel.ch),
		}
		if c.Dir == reflect.SelectSend {
			reflectCases[i].Send = reflect.ValueOf(c.Value)
		}
	}

	// Add default case if provided
	hasDefault := defaultHandler != Nil
	if hasDefault {
		reflectCases = append(reflectCases, reflect.SelectCase{
			Dir: reflect.SelectDefault,
		})
	}

	// Perform the select
	chosen, recv, recvOK := reflect.Select(reflectCases)

	// Handle default case
	if hasDefault && chosen == len(cases) {
		bv := interp.getBlockValue(defaultHandler)
		if bv == nil {
			return Nil
		}
		return interp.ExecuteBlock(
			bv.Block, bv.Captures, nil,
			bv.HomeFrame, bv.HomeSelf, bv.HomeMethod,
		)
	}

	// Get the handler for the chosen case
	if chosen >= len(cases) {
		return Nil
	}
	handler := cases[chosen].Handler

	bv := interp.getBlockValue(handler)
	if bv == nil {
		return Nil
	}

	// For receive operations, pass the received value to the handler
	var args []Value
	if cases[chosen].Dir == reflect.SelectRecv {
		if recvOK {
			// Channel was open, got a value
			if recv.IsValid() {
				args = []Value{recv.Interface().(Value)}
			} else {
				args = []Value{Nil}
			}
		} else {
			// Channel was closed
			args = []Value{Nil}
		}
	}

	return interp.ExecuteBlock(
		bv.Block, bv.Captures, args,
		bv.HomeFrame, bv.HomeSelf, bv.HomeMethod,
	)
}

// ---------------------------------------------------------------------------
// Channel Select Primitives Registration
// ---------------------------------------------------------------------------

func (vm *VM) registerChannelSelectPrimitives() {
	c := vm.ChannelClass

	// Channel class>>select: casesArray
	// casesArray is an Array of Associations: channel -> handlerBlock
	// Blocks until one of the channels is ready, then executes its handler.
	// For receive: handler receives the value as argument [:v | ...]
	// Returns the result of the executed handler.
	c.AddClassMethod1(vm.Selectors, "select:", func(vmPtr interface{}, recv Value, casesArray Value) Value {
		v := vmPtr.(*VM)
		return v.executeSelect(casesArray, Nil)
	})

	// Channel class>>select:ifNone:
	// Like select: but with a default block that executes if no channel is ready.
	// This is non-blocking if no channels are ready.
	c.AddClassMethod2(vm.Selectors, "select:ifNone:", func(vmPtr interface{}, recv Value, casesArray Value, defaultBlock Value) Value {
		v := vmPtr.(*VM)
		return v.executeSelect(casesArray, defaultBlock)
	})

	// Channel>>onReceive: handlerBlock
	// Convenience to create a receive case for select.
	// Returns an Association of channel -> handler.
	c.AddMethod1(vm.Selectors, "onReceive:", func(vmPtr interface{}, recv Value, handler Value) Value {
		v := vmPtr.(*VM)
		// Create an Association: recv -> handler
		return v.createSelectCase(recv, handler, false, Nil)
	})

	// Channel>>onSend:do: value handlerBlock
	// Convenience to create a send case for select.
	// Returns an Association-like structure for send operations.
	c.AddMethod2(vm.Selectors, "onSend:do:", func(vmPtr interface{}, recv Value, value Value, handler Value) Value {
		v := vmPtr.(*VM)
		return v.createSelectCase(recv, handler, true, value)
	})
}

// executeSelect parses the cases array and calls primitiveSelect.
func (vm *VM) executeSelect(casesArray Value, defaultBlock Value) Value {
	// Get the array of cases
	arr := vm.getArrayValue(casesArray)
	if arr == nil {
		return Nil
	}

	cases := make([]SelectCase, len(arr))
	for i, caseVal := range arr {
		sc, ok := vm.parseSelectCase(caseVal)
		if !ok {
			continue
		}
		cases[i] = sc
	}

	return vm.primitiveSelect(cases, defaultBlock)
}

// parseSelectCase extracts channel, handler, and direction from a case value.
// Cases can be:
// - Association (channel -> handler) for receive
// - SelectCaseObject for send operations
func (vm *VM) parseSelectCase(caseVal Value) (SelectCase, bool) {
	// Try as Association first (for receive operations)
	obj := ObjectFromValue(caseVal)
	if obj == nil {
		return SelectCase{}, false
	}

	numSlots := obj.NumSlots()

	// Check if it's an Association (2 slots: key, value)
	if numSlots >= 2 {
		key := obj.GetSlot(0)   // channel
		value := obj.GetSlot(1) // handler

		ch := vm.getChannel(key)
		if ch != nil {
			// Check if it's a SelectCaseObject (for send operations)
			// We use the "send" marker stored in slot 2
			if numSlots >= 4 {
				isSend := obj.GetSlot(2)
				if isSend == True {
					sendValue := obj.GetSlot(3)
					return SelectCase{
						Channel: ch,
						Dir:     reflect.SelectSend,
						Value:   sendValue,
						Handler: value,
					}, true
				}
			}

			// Regular receive case
			return SelectCase{
				Channel: ch,
				Dir:     reflect.SelectRecv,
				Handler: value,
			}, true
		}
	}

	return SelectCase{}, false
}

// createSelectCase creates a case object for select.
func (vm *VM) createSelectCase(channel Value, handler Value, isSend bool, sendValue Value) Value {
	// Create an object to hold the case data
	// For simplicity, we'll create an Association for receive
	// and a custom object for send
	if !isSend {
		// Create Association for receive: channel -> handler
		return vm.createAssociation(channel, handler)
	}

	// Create a custom object for send operations
	// Structure: [channel, handler, isSend=true, sendValue]
	obj := NewObjectWithSlots(vm.AssociationClass.VTable, []Value{channel, handler, True, sendValue})
	return obj.ToValue()
}

// createAssociation creates an Association object.
func (vm *VM) createAssociation(key Value, value Value) Value {
	obj := NewObjectWithSlots(vm.AssociationClass.VTable, []Value{key, value})
	return obj.ToValue()
}

// getArrayValue extracts the underlying slice from an array Value.
func (vm *VM) getArrayValue(v Value) []Value {
	obj := ObjectFromValue(v)
	if obj == nil {
		return nil
	}
	n := obj.NumSlots()
	result := make([]Value, n)
	for i := 0; i < n; i++ {
		result[i] = obj.GetSlot(i)
	}
	return result
}
