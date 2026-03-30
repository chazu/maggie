package vm

import (
	"reflect"
	"time"
)

// ---------------------------------------------------------------------------
// Channel Select: Multi-channel waiting using reflect.Select
// ---------------------------------------------------------------------------

// SelectCase represents a channel operation for select.
type SelectCase struct {
	Channel *ChannelObject     // local channel (nil if remote)
	Remote  *RemoteChannelRef  // remote channel (nil if local)
	Dir     reflect.SelectDir  // SelectRecv or SelectSend
	Value   Value              // For send operations
	Handler Value              // Block to execute on match
}

// primitiveSelect implements Go's select statement for multiple channels.
// It takes an array of SelectCase structs and an optional default handler.
// Returns the result of executing the matched handler block.
func (vm *VM) primitiveSelect(cases []SelectCase, defaultHandler Value) Value {
	// Separate local and remote cases
	hasRemote := false
	for _, c := range cases {
		if c.Remote != nil {
			hasRemote = true
			break
		}
	}

	// Fast path: all local channels — use reflect.Select directly
	if !hasRemote {
		return vm.primitiveSelectLocal(cases, defaultHandler)
	}

	// Mixed local + remote: use polling strategy
	return vm.primitiveSelectMixed(cases, defaultHandler)
}

// primitiveSelectLocal handles the pure-local case using reflect.Select.
func (vm *VM) primitiveSelectLocal(cases []SelectCase, defaultHandler Value) Value {
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

	return vm.executeSelectHandler(interp, cases, chosen, recv, recvOK)
}

// primitiveSelectMixed handles select with a mix of local and remote channels.
// Uses a polling strategy: try non-blocking ops on all channels in a loop
// with exponential backoff.
func (vm *VM) primitiveSelectMixed(cases []SelectCase, defaultHandler Value) Value {
	interp := vm.currentInterpreter()

	// If we have a default, try once non-blocking then return default
	hasDefault := defaultHandler != Nil

	maxWait := 100 * time.Millisecond
	wait := time.Millisecond

	for {
		// Try each case non-blocking
		for i, c := range cases {
			if c.Dir == reflect.SelectRecv {
				val, gotValue := vm.tryReceiveAny(c)
				if gotValue {
					return vm.executeSelectHandlerWithValue(interp, cases[i], val)
				}
			} else if c.Dir == reflect.SelectSend {
				if vm.trySendAny(c) {
					return vm.executeSelectHandlerNoValue(interp, cases[i])
				}
			}
		}

		// No case ready
		if hasDefault {
			bv := interp.getBlockValue(defaultHandler)
			if bv == nil {
				return Nil
			}
			return interp.ExecuteBlock(
				bv.Block, bv.Captures, nil,
				bv.HomeFrame, bv.HomeSelf, bv.HomeMethod,
			)
		}

		// Backoff and retry
		time.Sleep(wait)
		if wait < maxWait {
			wait *= 2
			if wait > maxWait {
				wait = maxWait
			}
		}
	}
}

// tryReceiveAny attempts a non-blocking receive from either a local or remote channel.
func (vm *VM) tryReceiveAny(c SelectCase) (Value, bool) {
	if c.Channel != nil {
		// Local channel
		val, gotValue, _ := c.Channel.TryReceive()
		if gotValue {
			return val, true
		}
		return Nil, false
	}
	if c.Remote != nil && c.Remote.TryReceiveFunc != nil {
		data, gotValue, _, err := c.Remote.TryReceiveFunc(c.Remote.ChannelID)
		if err != nil || !gotValue {
			return Nil, false
		}
		val, err := vm.DeserializeValue(data)
		if err != nil {
			return Nil, false
		}
		return val, true
	}
	return Nil, false
}

// trySendAny attempts a non-blocking send on either a local or remote channel.
func (vm *VM) trySendAny(c SelectCase) bool {
	if c.Channel != nil {
		return c.Channel.SafeTrySend(c.Value)
	}
	if c.Remote != nil && c.Remote.TrySendFunc != nil {
		data, err := vm.SerializeValue(c.Value)
		if err != nil {
			return false
		}
		sent, err := c.Remote.TrySendFunc(c.Remote.ChannelID, data)
		return err == nil && sent
	}
	return false
}

// executeSelectHandler handles the result of a chosen case from reflect.Select.
func (vm *VM) executeSelectHandler(interp *Interpreter, cases []SelectCase, chosen int, recv reflect.Value, recvOK bool) Value {
	if chosen >= len(cases) {
		return Nil
	}
	handler := cases[chosen].Handler

	bv := interp.getBlockValue(handler)
	if bv == nil {
		return Nil
	}

	var args []Value
	if cases[chosen].Dir == reflect.SelectRecv {
		if recvOK && recv.IsValid() {
			args = []Value{recv.Interface().(Value)}
		} else {
			args = []Value{Nil}
		}
	}

	return interp.ExecuteBlock(
		bv.Block, bv.Captures, args,
		bv.HomeFrame, bv.HomeSelf, bv.HomeMethod,
	)
}

// executeSelectHandlerWithValue executes a receive handler with the given value.
func (vm *VM) executeSelectHandlerWithValue(interp *Interpreter, c SelectCase, val Value) Value {
	bv := interp.getBlockValue(c.Handler)
	if bv == nil {
		return Nil
	}
	return interp.ExecuteBlock(
		bv.Block, bv.Captures, []Value{val},
		bv.HomeFrame, bv.HomeSelf, bv.HomeMethod,
	)
}

// executeSelectHandlerNoValue executes a send handler (no value argument).
func (vm *VM) executeSelectHandlerNoValue(interp *Interpreter, c SelectCase) Value {
	bv := interp.getBlockValue(c.Handler)
	if bv == nil {
		return Nil
	}
	return interp.ExecuteBlock(
		bv.Block, bv.Captures, nil,
		bv.HomeFrame, bv.HomeSelf, bv.HomeMethod,
	)
}

// ---------------------------------------------------------------------------
// Channel Select Primitives Registration
// ---------------------------------------------------------------------------

func (vm *VM) registerChannelSelectPrimitives() {
	c := vm.ChannelClass

	// Channel class>>select: casesArray
	c.AddClassMethod1(vm.Selectors, "select:", func(vmPtr interface{}, recv Value, casesArray Value) Value {
		v := vmPtr.(*VM)
		return v.executeSelect(casesArray, Nil)
	})

	// Channel class>>select:ifNone:
	c.AddClassMethod2(vm.Selectors, "select:ifNone:", func(vmPtr interface{}, recv Value, casesArray Value, defaultBlock Value) Value {
		v := vmPtr.(*VM)
		return v.executeSelect(casesArray, defaultBlock)
	})

	// Channel>>onReceive: handlerBlock
	c.AddMethod1(vm.Selectors, "onReceive:", func(vmPtr interface{}, recv Value, handler Value) Value {
		v := vmPtr.(*VM)
		return v.createSelectCase(recv, handler, false, Nil)
	})

	// Channel>>onSend:do: value handlerBlock
	c.AddMethod2(vm.Selectors, "onSend:do:", func(vmPtr interface{}, recv Value, value Value, handler Value) Value {
		v := vmPtr.(*VM)
		return v.createSelectCase(recv, handler, true, value)
	})
}

// executeSelect parses the cases array and calls primitiveSelect.
func (vm *VM) executeSelect(casesArray Value, defaultBlock Value) Value {
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
// Supports both local Channel and RemoteChannel values.
func (vm *VM) parseSelectCase(caseVal Value) (SelectCase, bool) {
	obj := ObjectFromValue(caseVal)
	if obj == nil {
		return SelectCase{}, false
	}

	numSlots := obj.NumSlots()
	if numSlots < 2 {
		return SelectCase{}, false
	}

	key := obj.GetSlot(0)   // channel
	value := obj.GetSlot(1) // handler

	// Try local channel first
	ch := vm.getChannel(key)
	if ch != nil {
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
		return SelectCase{
			Channel: ch,
			Dir:     reflect.SelectRecv,
			Handler: value,
		}, true
	}

	// Try remote channel
	remote := vm.getRemoteChannel(key)
	if remote != nil {
		if numSlots >= 4 {
			isSend := obj.GetSlot(2)
			if isSend == True {
				sendValue := obj.GetSlot(3)
				return SelectCase{
					Remote:  remote,
					Dir:     reflect.SelectSend,
					Value:   sendValue,
					Handler: value,
				}, true
			}
		}
		return SelectCase{
			Remote:  remote,
			Dir:     reflect.SelectRecv,
			Handler: value,
		}, true
	}

	return SelectCase{}, false
}

// createSelectCase creates a case object for select.
func (vm *VM) createSelectCase(channel Value, handler Value, isSend bool, sendValue Value) Value {
	if !isSend {
		return vm.createAssociation(channel, handler)
	}
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
