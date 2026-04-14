package vm

import "encoding/binary"

// ---------------------------------------------------------------------------
// Extracted opcode handlers for runFrame()
//
// Each handler receives the current frame state and operates on the
// interpreter's stack. Handlers that may cause runFrame() to return
// use a (Value, bool) return signature where bool=true means "return now".
// ---------------------------------------------------------------------------

// execPushGlobal handles OpPushGlobal: push a global variable onto the stack.
func (i *Interpreter) execPushGlobal(frame *CallFrame, bc []byte, literals []Value) {
	idx := binary.LittleEndian.Uint16(bc[frame.IP:])
	frame.IP += 2
	if int(idx) < len(literals) {
		// Get global name from literal
		globalName := ""
		lit := literals[idx]
		if lit.IsSymbol() && i.Symbols != nil {
			globalName = i.Symbols.Name(lit.SymbolID())
		} else if IsStringValue(lit) {
			globalName = i.vm.registry.GetStringContent(lit)
		}
		if globalName != "" {
			if i.hidden != nil && i.hidden[globalName] {
				i.push(Nil)
			} else if i.localWrites != nil {
				if val, ok := i.localWrites[globalName]; ok {
					i.push(val)
				} else {
					if i.vm != nil {
						i.vm.globalsMu.RLock()
					}
					val, ok := i.Globals[globalName]
					if i.vm != nil {
						i.vm.globalsMu.RUnlock()
					}
					if ok {
						i.push(val)
					} else {
						i.push(Nil)
					}
				}
			} else {
				if i.vm != nil {
					i.vm.globalsMu.RLock()
				}
				val, ok := i.Globals[globalName]
				if i.vm != nil {
					i.vm.globalsMu.RUnlock()
				}
				if ok {
					i.push(val)
				} else {
					i.push(Nil)
				}
			}
		} else {
			i.push(Nil)
		}
	} else {
		i.push(Nil)
	}
}

// execStoreGlobal handles OpStoreGlobal: store TOS into a global variable.
func (i *Interpreter) execStoreGlobal(frame *CallFrame, bc []byte, literals []Value) {
	idx := binary.LittleEndian.Uint16(bc[frame.IP:])
	frame.IP += 2
	if int(idx) < len(literals) {
		// Get global name from literal
		globalName := ""
		lit := literals[idx]
		if lit.IsSymbol() && i.Symbols != nil {
			globalName = i.Symbols.Name(lit.SymbolID())
		} else if IsStringValue(lit) {
			globalName = i.vm.registry.GetStringContent(lit)
		}
		if globalName != "" {
			if i.hidden != nil && i.hidden[globalName] {
				// silently deny write to restricted name
			} else if i.forked {
				// Forked process: write to process-local overlay
				if i.localWrites == nil {
					i.localWrites = make(map[string]Value)
				}
				i.localWrites[globalName] = i.top()
			} else {
				// Main interpreter: write directly to shared Globals
				if i.vm != nil {
					i.vm.globalsMu.Lock()
				}
				i.Globals[globalName] = i.top()
				if i.vm != nil {
					i.vm.globalsMu.Unlock()
				}
			}
		}
	}
}

// execPushClassVar handles OpPushClassVar: push a class variable onto the stack.
func (i *Interpreter) execPushClassVar(frame *CallFrame, bc []byte, literals []Value) {
	idx := binary.LittleEndian.Uint16(bc[frame.IP:])
	frame.IP += 2
	if int(idx) < len(literals) {
		// Get class variable name from literal
		varName := ""
		lit := literals[idx]
		if lit.IsSymbol() && i.Symbols != nil {
			varName = i.Symbols.Name(lit.SymbolID())
		}
		if varName != "" {
			// Get class from the method's defining class
			class := frame.Method.Class()
			if class != nil {
				i.push(class.GetClassVar(i.vm.registry, varName))
			} else {
				i.push(Nil)
			}
		} else {
			i.push(Nil)
		}
	} else {
		i.push(Nil)
	}
}

// execStoreClassVar handles OpStoreClassVar: store TOS into a class variable.
func (i *Interpreter) execStoreClassVar(frame *CallFrame, bc []byte, literals []Value) {
	idx := binary.LittleEndian.Uint16(bc[frame.IP:])
	frame.IP += 2
	if int(idx) < len(literals) {
		// Get class variable name from literal
		varName := ""
		lit := literals[idx]
		if lit.IsSymbol() && i.Symbols != nil {
			varName = i.Symbols.Name(lit.SymbolID())
		}
		if varName != "" {
			// Get class from the method's defining class
			class := frame.Method.Class()
			if class != nil {
				class.SetClassVar(i.vm.registry, varName, i.top())
			}
		}
	}
}

// execTailSend handles OpTailSend: tail-call optimized message send.
// Returns (result, shouldContinue, shouldReturn).
// shouldContinue=true means the for loop should continue (tail call optimized).
// shouldReturn=true means runFrame() should return Nil (unwinding).
// Otherwise, push result onto stack.
func (i *Interpreter) execTailSend(frame *CallFrame, bc []byte, isBlock bool) (Value, bool, bool) {
	sel := int(binary.LittleEndian.Uint16(bc[frame.IP:]))
	frame.IP += 2
	argc := int(bc[frame.IP])
	frame.IP++

	// Copy args into a stack-local buffer to avoid allocation.
	// Both the optimized path (resets SP) and the fallback path
	// (re-pushes args) mutate the stack, so a copy is required.
	var tailBuf [16]Value
	var tailArgs []Value
	if argc <= len(tailBuf) {
		copy(tailBuf[:argc], i.peekN(argc))
		tailArgs = tailBuf[:argc]
		i.dropN(argc)
	} else {
		// >16 args is vanishingly rare; fall back to allocating popN.
		tailArgs = i.popN(argc)
	}
	tailRcvr := i.pop()

	// Only optimize if:
	// 1. We're in a method frame (not a block)
	// 2. The receiver is self (same as current frame's receiver)
	// 3. The target method is the same CompiledMethod as the current one
	tailOptimized := false
	if !isBlock && tailRcvr == frame.Receiver {
		// Look up the method for the receiver
		vt := i.vtableFor(tailRcvr)
		if vt != nil {
			tailMethod := vt.Lookup(sel)
			if cm, ok := tailMethod.(*CompiledMethod); ok && cm == frame.Method {
				// Self-recursive tail call: reuse the current frame
				// Reset SP to just above the base pointer (discard old temps)
				i.sp = frame.BP

				// Write new arguments into the temp slots
				for _, arg := range tailArgs {
					i.push(arg)
				}
				// Fill remaining temp slots with nil
				for j := len(tailArgs); j < cm.NumTemps; j++ {
					i.push(Nil)
				}

				// Reset instruction pointer to beginning of method
				frame.IP = 0

				tailOptimized = true
			}
		}
	}

	if tailOptimized {
		// Continue the dispatch loop (no new frame pushed)
		return Nil, true, false
	}

	// Fallback: non-self-recursive or different method — do a normal send
	// Re-push receiver and args onto the stack for i.send()
	i.push(tailRcvr)
	for _, arg := range tailArgs {
		i.push(arg)
	}
	tailResult := i.send(sel, argc)
	if i.unwinding {
		i.popFrame()
		return Nil, false, true
	}
	return tailResult, false, false
}

// execSendSuper handles OpSendSuper: send to superclass implementation.
// Returns (result, shouldReturn). shouldReturn=true means runFrame() should return Nil.
func (i *Interpreter) execSendSuper(frame *CallFrame, bc []byte, isBlock bool) (Value, bool) {
	sel := int(binary.LittleEndian.Uint16(bc[frame.IP:]))
	frame.IP += 2
	argc := int(bc[frame.IP])
	frame.IP++
	// For blocks, we need to find the method from the home frame
	var method *CompiledMethod
	if isBlock {
		if frame.HomeFrame >= 0 && frame.HomeFrame < len(i.frames) && i.frames[frame.HomeFrame] != nil {
			method = i.frames[frame.HomeFrame].Method
		}
	} else {
		method = frame.Method
	}
	if method != nil {
		result := i.sendSuper(sel, argc, method)
		if i.unwinding {
			i.popFrame()
			return Nil, true
		}
		return result, false
	}
	return Nil, false
}

// execOptimizedBinarySend handles all OpSend{Plus,Minus,...} opcodes.
// Returns (result, shouldReturn). shouldReturn=true means runFrame() should return Nil.
func (i *Interpreter) execOptimizedBinarySend(primFn func(a, b Value) Value) (Value, bool) {
	b := i.pop()
	a := i.pop()
	result := primFn(a, b)
	if i.unwinding {
		i.popFrame()
		return Nil, true
	}
	return result, false
}

// execOptimizedUnarySend handles OpSend{Size,Value,New,Class}.
// Returns (result, shouldReturn).
func (i *Interpreter) execOptimizedUnarySend(primFn func(rcvr Value) Value) (Value, bool) {
	rcvr := i.pop()
	result := primFn(rcvr)
	if i.unwinding {
		i.popFrame()
		return Nil, true
	}
	return result, false
}

// execReturnTop handles OpReturnTop: return TOS, with NLR support for blocks.
// Always causes runFrame() to return. Returns the value to return.
func (i *Interpreter) execReturnTop(frame *CallFrame, isBlock bool) Value {
	result := i.pop()
	if isBlock {
		// Check if this is a detached block (HomeFrame == -1)
		// Detached blocks treat ^ as local return instead of non-local return
		// This is used for forked blocks where the home frame is unreachable
		if frame.HomeFrame == -1 {
			// Detached block: treat ^ as local return
			i.popFrame()
			return result
		}
		// Non-local return: set unwinding flag instead of panic
		i.popFrame()
		i.unwinding = true
		i.unwindValue = result
		i.unwindTarget = frame.HomeFrame
		return Nil
	}
	// Method: local return
	i.popFrame()
	return result
}

// execCreateBlock handles OpCreateBlock: create a block closure.
func (i *Interpreter) execCreateBlock(frame *CallFrame, bc []byte, isBlock bool) {
	methodIdx := binary.LittleEndian.Uint16(bc[frame.IP:])
	frame.IP += 2
	nCaptures := int(bc[frame.IP])
	frame.IP++

	// Get the block method from the appropriate source
	var block *BlockMethod
	if isBlock {
		// For block frames, try HomeMethod first (set when executing blocks cross-interpreter)
		// This handles the case where a block is executed in a different goroutine/interpreter
		if frame.HomeMethod != nil && int(methodIdx) < len(frame.HomeMethod.Blocks) {
			block = frame.HomeMethod.Blocks[methodIdx]
		} else if frame.HomeFrame >= 0 && frame.HomeFrame < len(i.frames) && i.frames[frame.HomeFrame] != nil {
			// Fall back to looking up from the home frame's method (same-interpreter case)
			homeMethod := i.frames[frame.HomeFrame].Method
			if homeMethod != nil && int(methodIdx) < len(homeMethod.Blocks) {
				block = homeMethod.Blocks[methodIdx]
			}
		}
	} else {
		// Method frame: get block directly
		block = frame.Method.GetBlock(int(methodIdx))
	}

	// Captures are retained by BlockValue, so a copy is required.
	captures := i.popN(nCaptures)

	if block != nil {
		blockVal := i.createBlockValue(block, captures)
		i.push(blockVal)
	} else {
		i.push(Nil)
	}
}
