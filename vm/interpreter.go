package vm

import (
	"encoding/binary"
	"fmt"
)

// ---------------------------------------------------------------------------
// CallFrame: Execution state for a method invocation
// ---------------------------------------------------------------------------

// CallFrame represents the execution state of a single method invocation.
type CallFrame struct {
	Method   *CompiledMethod // the method being executed
	Receiver Value           // the object receiving the message
	IP       int             // instruction pointer (offset into bytecode)
	BP       int             // base pointer (start of this frame's temps on stack)

	// For block frames
	Block     *BlockMethod // nil for regular methods
	Captures  []Value      // captured variables for blocks
	HomeFrame int          // frame pointer of enclosing method (for home temp access)
	HomeBP    int          // base pointer of home frame (for OpPushHomeTemp/OpStoreHomeTemp)
	HomeSelf  Value        // self from enclosing method (for ivar access in blocks)
}

// IsBlock returns true if this is a block frame (not a method frame).
func (f *CallFrame) IsBlock() bool {
	return f.Block != nil
}

// Bytecode returns the bytecode for this frame (from method or block).
func (f *CallFrame) Bytecode() []byte {
	if f.Block != nil {
		return f.Block.Bytecode
	}
	return f.Method.Bytecode
}

// Literals returns the literals for this frame (from method or block).
func (f *CallFrame) Literals() []Value {
	if f.Block != nil {
		return f.Block.Literals
	}
	return f.Method.Literals
}

// Self returns the appropriate self value (Receiver for methods, HomeSelf for blocks).
func (f *CallFrame) Self() Value {
	if f.Block != nil {
		return f.HomeSelf
	}
	return f.Receiver
}

// ---------------------------------------------------------------------------
// Interpreter: Bytecode execution engine
// ---------------------------------------------------------------------------

// Interpreter executes Maggie bytecode.
type Interpreter struct {
	// Global state (would be part of VM in full implementation)
	Selectors *SelectorTable
	Symbols   *SymbolTable
	Classes   *ClassTable
	Globals   map[string]Value

	// Back-reference to VM for primitives
	vm interface{}

	// Execution state
	stack  []Value      // operand stack
	sp     int          // stack pointer (points to next free slot)
	frames []*CallFrame // call stack
	fp     int          // frame pointer (current frame index)

	// Well-known selector IDs (cached for fast dispatch)
	selectorPlus    int
	selectorMinus   int
	selectorTimes   int
	selectorDiv     int
	selectorMod     int
	selectorLT      int
	selectorGT      int
	selectorLE      int
	selectorGE      int
	selectorEQ      int
	selectorNE      int
	selectorAt      int
	selectorAtPut   int
	selectorSize    int
	selectorValue   int
	selectorValue1  int
	selectorValue2  int
	selectorNew     int
	selectorClass   int
}

// NewInterpreter creates a new interpreter.
func NewInterpreter() *Interpreter {
	interp := &Interpreter{
		Selectors: NewSelectorTable(),
		Classes:   NewClassTable(),
		Globals:   make(map[string]Value),
		stack:     make([]Value, 1024), // Fixed-size stack
		sp:        0,
		frames:    make([]*CallFrame, 256), // Fixed-size frame stack
		fp:        -1,
	}

	// Pre-intern common selectors
	interp.selectorPlus = interp.Selectors.Intern("+")
	interp.selectorMinus = interp.Selectors.Intern("-")
	interp.selectorTimes = interp.Selectors.Intern("*")
	interp.selectorDiv = interp.Selectors.Intern("/")
	interp.selectorMod = interp.Selectors.Intern("\\\\")
	interp.selectorLT = interp.Selectors.Intern("<")
	interp.selectorGT = interp.Selectors.Intern(">")
	interp.selectorLE = interp.Selectors.Intern("<=")
	interp.selectorGE = interp.Selectors.Intern(">=")
	interp.selectorEQ = interp.Selectors.Intern("=")
	interp.selectorNE = interp.Selectors.Intern("~=")
	interp.selectorAt = interp.Selectors.Intern("at:")
	interp.selectorAtPut = interp.Selectors.Intern("at:put:")
	interp.selectorSize = interp.Selectors.Intern("size")
	interp.selectorValue = interp.Selectors.Intern("value")
	interp.selectorValue1 = interp.Selectors.Intern("value:")
	interp.selectorValue2 = interp.Selectors.Intern("value:value:")
	interp.selectorNew = interp.Selectors.Intern("new")
	interp.selectorClass = interp.Selectors.Intern("class")

	return interp
}

// ---------------------------------------------------------------------------
// Stack operations
// ---------------------------------------------------------------------------

func (i *Interpreter) push(v Value) {
	if i.sp >= len(i.stack) {
		// Grow the stack dynamically instead of panicking
		newStack := make([]Value, len(i.stack)*2)
		copy(newStack, i.stack)
		i.stack = newStack
	}
	i.stack[i.sp] = v
	i.sp++
}

func (i *Interpreter) pop() Value {
	if i.sp <= 0 {
		panic("stack underflow")
	}
	i.sp--
	return i.stack[i.sp]
}

func (i *Interpreter) top() Value {
	if i.sp <= 0 {
		panic("stack underflow")
	}
	return i.stack[i.sp-1]
}

func (i *Interpreter) popN(n int) []Value {
	if i.sp < n {
		panic("stack underflow")
	}
	result := make([]Value, n)
	i.sp -= n
	copy(result, i.stack[i.sp:i.sp+n])
	return result
}

// getTemp returns a temporary variable relative to the current frame's base pointer.
func (i *Interpreter) getTemp(index int) Value {
	frame := i.frames[i.fp]
	return i.stack[frame.BP+index]
}

// setTemp sets a temporary variable relative to the current frame's base pointer.
func (i *Interpreter) setTemp(index int, v Value) {
	frame := i.frames[i.fp]
	i.stack[frame.BP+index] = v
}

// ---------------------------------------------------------------------------
// Frame management
// ---------------------------------------------------------------------------

func (i *Interpreter) pushFrame(method *CompiledMethod, receiver Value, args []Value) {
	i.fp++
	if i.fp >= len(i.frames) {
		// Grow the frame stack dynamically instead of panicking
		newFrames := make([]*CallFrame, len(i.frames)*2)
		copy(newFrames, i.frames)
		i.frames = newFrames
	}

	// Save the base pointer for this frame
	bp := i.sp

	// Push temps (args first, then locals initialized to nil)
	for _, arg := range args {
		i.push(arg)
	}
	for j := len(args); j < method.NumTemps; j++ {
		i.push(Nil)
	}

	i.frames[i.fp] = &CallFrame{
		Method:   method,
		Receiver: receiver,
		IP:       0,
		BP:       bp,
	}
}

func (i *Interpreter) pushBlockFrame(block *BlockMethod, captures []Value, args []Value, homeFrame int, homeBP int, homeSelf Value) {
	i.fp++
	if i.fp >= len(i.frames) {
		// Grow the frame stack dynamically instead of panicking
		newFrames := make([]*CallFrame, len(i.frames)*2)
		copy(newFrames, i.frames)
		i.frames = newFrames
	}

	bp := i.sp

	// Push temps (args first, then locals initialized to nil)
	for _, arg := range args {
		i.push(arg)
	}
	for j := len(args); j < block.NumTemps; j++ {
		i.push(Nil)
	}

	i.frames[i.fp] = &CallFrame{
		Method:    nil, // block frame
		Receiver:  Nil, // blocks don't have a receiver
		IP:        0,
		BP:        bp,
		Block:     block,
		Captures:  captures,
		HomeFrame: homeFrame,
		HomeBP:    homeBP,
		HomeSelf:  homeSelf,
	}
}

func (i *Interpreter) popFrame() {
	frameIndex := i.fp
	frame := i.frames[frameIndex]
	i.sp = frame.BP // Discard temps
	i.frames[frameIndex] = nil
	i.fp--
	// Clean up any blocks whose home frame is being popped
	releaseBlocksForFrame(frameIndex)
}

// ---------------------------------------------------------------------------
// Main interpreter loop
// ---------------------------------------------------------------------------

// Execute runs a compiled method with the given receiver and arguments.
// Returns the result value.
func (i *Interpreter) Execute(method *CompiledMethod, receiver Value, args []Value) (result Value) {
	// Record the home frame for this method
	i.pushFrame(method, receiver, args)
	homeFrame := i.fp

	// Catch non-local returns from blocks
	defer func() {
		if r := recover(); r != nil {
			if nlr, ok := r.(NonLocalReturn); ok {
				// If this is the target frame, return the value
				if nlr.HomeFrame == homeFrame {
					// Unwind any remaining frames to our home frame
					for i.fp > homeFrame {
						i.popFrame()
					}
					if i.fp == homeFrame {
						i.popFrame()
					}
					result = nlr.Value
					return
				}
				// Not our frame, propagate up
				panic(nlr)
			}
			// Re-panic for other errors
			panic(r)
		}
	}()

	result = i.runFrame()
	return result
}

// ExecuteBlock runs a block with captured variables and arguments.
func (i *Interpreter) ExecuteBlock(block *BlockMethod, captures []Value, args []Value, homeFrame int, homeSelf Value) Value {
	// Get the home frame's BP for OpPushHomeTemp/OpStoreHomeTemp
	homeBP := 0
	if homeFrame >= 0 && homeFrame < len(i.frames) && i.frames[homeFrame] != nil {
		homeBP = i.frames[homeFrame].BP
	}
	i.pushBlockFrame(block, captures, args, homeFrame, homeBP, homeSelf)
	return i.runFrame()
}

// runFrame is the unified interpreter loop for both methods and blocks.
func (i *Interpreter) runFrame() Value {
	for {
		frame := i.frames[i.fp]
		bc := frame.Bytecode()
		literals := frame.Literals()
		isBlock := frame.IsBlock()

		if frame.IP >= len(bc) {
			// Implicit return at end of frame
			i.popFrame()
			if isBlock {
				return Nil // Blocks return nil implicitly
			}
			return frame.Receiver // Methods return self implicitly
		}

		op := Opcode(bc[frame.IP])
		frame.IP++

		switch op {
		// --- Stack operations ---
		case OpNOP:
			// Do nothing

		case OpPOP:
			i.pop()

		case OpDUP:
			i.push(i.top())

		// --- Push constants ---
		case OpPushNil:
			i.push(Nil)

		case OpPushTrue:
			i.push(True)

		case OpPushFalse:
			i.push(False)

		case OpPushSelf:
			i.push(frame.Self())

		case OpPushInt8:
			val := int8(bc[frame.IP])
			frame.IP++
			i.push(FromSmallInt(int64(val)))

		case OpPushInt32:
			val := int32(binary.LittleEndian.Uint32(bc[frame.IP:]))
			frame.IP += 4
			i.push(FromSmallInt(int64(val)))

		case OpPushLiteral:
			idx := binary.LittleEndian.Uint16(bc[frame.IP:])
			frame.IP += 2
			if int(idx) >= len(literals) {
				panic(fmt.Sprintf("runFrame: literal index %d out of bounds (len=%d)",
					idx, len(literals)))
			}
			i.push(literals[idx])

		case OpPushFloat:
			bits := binary.LittleEndian.Uint64(bc[frame.IP:])
			frame.IP += 8
			i.push(Value(bits))

		// --- Variables ---
		case OpPushTemp:
			idx := bc[frame.IP]
			frame.IP++
			i.push(i.getTemp(int(idx)))

		case OpStoreTemp:
			idx := bc[frame.IP]
			frame.IP++
			i.setTemp(int(idx), i.top())

		case OpPushHomeTemp:
			idx := bc[frame.IP]
			frame.IP++
			// Access temp from home frame using HomeBP
			i.push(i.stack[frame.HomeBP+int(idx)])

		case OpStoreHomeTemp:
			idx := bc[frame.IP]
			frame.IP++
			// Store into temp in home frame using HomeBP
			i.stack[frame.HomeBP+int(idx)] = i.top()

		case OpPushIvar:
			idx := bc[frame.IP]
			frame.IP++
			obj := ObjectFromValue(frame.Self())
			if obj != nil && int(idx) < obj.NumSlots() {
				i.push(obj.GetSlot(int(idx)))
			} else {
				i.push(Nil)
			}

		case OpStoreIvar:
			idx := bc[frame.IP]
			frame.IP++
			obj := ObjectFromValue(frame.Self())
			if obj != nil && int(idx) < obj.NumSlots() {
				obj.SetSlot(int(idx), i.top())
			}

		case OpPushGlobal:
			idx := binary.LittleEndian.Uint16(bc[frame.IP:])
			frame.IP += 2
			if int(idx) < len(literals) {
				// Get global name from literal
				globalName := ""
				lit := literals[idx]
				if lit.IsSymbol() && i.Symbols != nil {
					globalName = i.Symbols.Name(lit.SymbolID())
				} else if IsStringValue(lit) {
					globalName = GetStringContent(lit)
				}
				if globalName != "" {
					if val, ok := i.Globals[globalName]; ok {
						i.push(val)
					} else {
						i.push(Nil)
					}
				} else {
					i.push(Nil)
				}
			} else {
				i.push(Nil)
			}

		case OpStoreGlobal:
			idx := binary.LittleEndian.Uint16(bc[frame.IP:])
			frame.IP += 2
			if int(idx) < len(literals) {
				// Get global name from literal
				globalName := ""
				lit := literals[idx]
				if lit.IsSymbol() && i.Symbols != nil {
					globalName = i.Symbols.Name(lit.SymbolID())
				} else if IsStringValue(lit) {
					globalName = GetStringContent(lit)
				}
				if globalName != "" {
					// Store the value (top of stack) to the global
					// Value remains on stack (like other store operations)
					i.Globals[globalName] = i.top()
				}
			}

		case OpPushCaptured:
			idx := bc[frame.IP]
			frame.IP++
			if int(idx) < len(frame.Captures) {
				i.push(frame.Captures[idx])
			} else {
				i.push(Nil)
			}

		case OpStoreCaptured:
			idx := bc[frame.IP]
			frame.IP++
			if int(idx) < len(frame.Captures) {
				frame.Captures[idx] = i.top()
			}

		// --- Message sends ---
		case OpSend:
			sel := int(binary.LittleEndian.Uint16(bc[frame.IP:]))
			frame.IP += 2
			argc := int(bc[frame.IP])
			frame.IP++
			result := i.send(sel, argc)
			i.push(result)

		case OpSendSuper:
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
				i.push(result)
			} else {
				i.push(Nil)
			}

		// --- Optimized sends ---
		case OpSendPlus:
			b := i.pop()
			a := i.pop()
			i.push(i.primitivePlus(a, b))

		case OpSendMinus:
			b := i.pop()
			a := i.pop()
			i.push(i.primitiveMinus(a, b))

		case OpSendTimes:
			b := i.pop()
			a := i.pop()
			i.push(i.primitiveTimes(a, b))

		case OpSendDiv:
			b := i.pop()
			a := i.pop()
			i.push(i.primitiveDiv(a, b))

		case OpSendMod:
			b := i.pop()
			a := i.pop()
			i.push(i.primitiveMod(a, b))

		case OpSendLT:
			b := i.pop()
			a := i.pop()
			i.push(i.primitiveLT(a, b))

		case OpSendGT:
			b := i.pop()
			a := i.pop()
			i.push(i.primitiveGT(a, b))

		case OpSendLE:
			b := i.pop()
			a := i.pop()
			i.push(i.primitiveLE(a, b))

		case OpSendGE:
			b := i.pop()
			a := i.pop()
			i.push(i.primitiveGE(a, b))

		case OpSendEQ:
			b := i.pop()
			a := i.pop()
			i.push(i.primitiveEQ(a, b))

		case OpSendNE:
			b := i.pop()
			a := i.pop()
			i.push(i.primitiveNE(a, b))

		case OpSendAt:
			idx := i.pop()
			rcvr := i.pop()
			i.push(i.primitiveAt(rcvr, idx))

		case OpSendAtPut:
			val := i.pop()
			idx := i.pop()
			rcvr := i.pop()
			i.push(i.primitiveAtPut(rcvr, idx, val))

		case OpSendSize:
			rcvr := i.pop()
			i.push(i.primitiveSize(rcvr))

		case OpSendValue:
			rcvr := i.pop()
			i.push(i.primitiveValue(rcvr))

		case OpSendValue1:
			arg := i.pop()
			rcvr := i.pop()
			i.push(i.primitiveValue1(rcvr, arg))

		case OpSendValue2:
			arg2 := i.pop()
			arg1 := i.pop()
			rcvr := i.pop()
			i.push(i.primitiveValue2(rcvr, arg1, arg2))

		case OpSendNew:
			rcvr := i.pop()
			i.push(i.primitiveNew(rcvr))

		case OpSendClass:
			rcvr := i.pop()
			i.push(i.primitiveClass(rcvr))

		// --- Control flow ---
		case OpJump:
			offset := int16(binary.LittleEndian.Uint16(bc[frame.IP:]))
			frame.IP += 2
			frame.IP += int(offset)

		case OpJumpTrue:
			offset := int16(binary.LittleEndian.Uint16(bc[frame.IP:]))
			frame.IP += 2
			cond := i.pop()
			if cond == True {
				frame.IP += int(offset)
			}

		case OpJumpFalse:
			offset := int16(binary.LittleEndian.Uint16(bc[frame.IP:]))
			frame.IP += 2
			cond := i.pop()
			if cond == False || cond == Nil {
				frame.IP += int(offset)
			}

		case OpJumpNil:
			offset := int16(binary.LittleEndian.Uint16(bc[frame.IP:]))
			frame.IP += 2
			cond := i.pop()
			if cond == Nil {
				frame.IP += int(offset)
			}

		case OpJumpNotNil:
			offset := int16(binary.LittleEndian.Uint16(bc[frame.IP:]))
			frame.IP += 2
			cond := i.pop()
			if cond != Nil {
				frame.IP += int(offset)
			}

		// --- Returns ---
		case OpReturnTop:
			result := i.pop()
			if isBlock {
				// Non-local return: return from the home context (enclosing method)
				i.popFrame()
				panic(NonLocalReturn{Value: result, HomeFrame: frame.HomeFrame})
			}
			// Method: local return
			i.popFrame()
			return result

		case OpReturnSelf:
			result := frame.Self()
			i.popFrame()
			return result

		case OpReturnNil:
			i.popFrame()
			return Nil

		case OpBlockReturn:
			// Local return from block (or method - same behavior)
			result := i.pop()
			i.popFrame()
			return result

		// --- Blocks ---
		case OpCreateBlock:
			methodIdx := binary.LittleEndian.Uint16(bc[frame.IP:])
			frame.IP += 2
			nCaptures := int(bc[frame.IP])
			frame.IP++

			// Get the block method from the appropriate source
			var block *BlockMethod
			if isBlock {
				// Look up block from the HOME frame's method
				if frame.HomeFrame >= 0 && frame.HomeFrame < len(i.frames) && i.frames[frame.HomeFrame] != nil {
					homeMethod := i.frames[frame.HomeFrame].Method
					if homeMethod != nil && int(methodIdx) < len(homeMethod.Blocks) {
						block = homeMethod.Blocks[methodIdx]
					}
				}
			} else {
				// Method frame: get block directly
				block = frame.Method.GetBlock(int(methodIdx))
			}

			// Pop captures from stack
			captures := i.popN(nCaptures)

			if isBlock && nCaptures > 0 {
				// Reverse captures (they were pushed in order, popped in reverse)
				for l, r := 0, len(captures)-1; l < r; l, r = l+1, r-1 {
					captures[l], captures[r] = captures[r], captures[l]
				}
			}

			if block != nil {
				blockVal := i.createBlockValue(block, captures)
				i.push(blockVal)
			} else {
				i.push(Nil)
			}

		case OpCaptureTemp:
			idx := bc[frame.IP]
			frame.IP++
			i.push(i.getTemp(int(idx)))

		case OpCaptureIvar:
			idx := bc[frame.IP]
			frame.IP++
			obj := ObjectFromValue(frame.Self())
			if obj != nil {
				i.push(obj.GetSlot(int(idx)))
			} else {
				i.push(Nil)
			}

		// --- Object creation ---
		case OpCreateArray:
			size := int(bc[frame.IP])
			frame.IP++
			elements := i.popN(size)
			// Create array with the elements
			arr := i.vm.(*VM).NewArrayWithElements(elements)
			i.push(arr)

		case OpCreateObject:
			classIdx := binary.LittleEndian.Uint16(bc[frame.IP:])
			frame.IP += 2
			nSlots := int(bc[frame.IP])
			frame.IP++
			_ = classIdx
			_ = nSlots
			// Would create object with given class and slots
			i.push(Nil)

		default:
			panic(fmt.Sprintf("unknown opcode: %02X (%s)", op, op))
		}
	}
}

// ---------------------------------------------------------------------------
// Message sending
// ---------------------------------------------------------------------------

// send performs a message send via VTable lookup.
func (i *Interpreter) send(selector int, argc int) (result Value) {
	args := i.popN(argc)
	rcvr := i.pop()

	// Get the vtable for the receiver
	vt := i.vtableFor(rcvr)
	if vt == nil {
		// No vtable - would trigger doesNotUnderstand:
		return Nil
	}

	// Lookup the method
	method := vt.Lookup(selector)
	if method == nil {
		// Method not found - would trigger doesNotUnderstand:
		return Nil
	}

	// Check if it's a compiled method or primitive
	if cm, ok := method.(*CompiledMethod); ok {
		// Push frame and record home frame for non-local return handling
		i.pushFrame(cm, rcvr, args)
		homeFrame := i.fp

		// Catch non-local returns from blocks created in this method
		defer func() {
			if r := recover(); r != nil {
				if nlr, ok := r.(NonLocalReturn); ok {
					// If this is the target frame, return the value
					if nlr.HomeFrame == homeFrame {
						// Unwind any remaining frames to our home frame
						for i.fp > homeFrame {
							i.popFrame()
						}
						if i.fp == homeFrame {
							i.popFrame()
						}
						result = nlr.Value
						return
					}
					// Not our frame, propagate up
					panic(nlr)
				}
				// Re-panic for other errors
				panic(r)
			}
		}()

		result = i.runFrame()
		return result
	}

	// Primitive method - pass the VM (for primitives that need it)
	return method.Invoke(i.vm, rcvr, args)
}

// sendSuper performs a super send.
func (i *Interpreter) sendSuper(selector int, argc int, callingMethod *CompiledMethod) (result Value) {
	args := i.popN(argc)
	rcvr := i.pop()

	// Get the defining class and determine if this is a class method
	definingClass := callingMethod.Class()
	isClassMethod := callingMethod.IsClassMethod

	// Start lookup from superclass
	if definingClass == nil || definingClass.Superclass == nil {
		return Nil
	}

	// For class methods, use ClassVTable; for instance methods, use VTable
	var vt *VTable
	if isClassMethod {
		vt = definingClass.Superclass.ClassVTable
	} else {
		vt = definingClass.Superclass.VTable
	}
	if vt == nil {
		return Nil
	}

	method := vt.Lookup(selector)
	if method == nil {
		return Nil
	}

	if cm, ok := method.(*CompiledMethod); ok {
		i.pushFrame(cm, rcvr, args)
		homeFrame := i.fp

		// Catch non-local returns
		defer func() {
			if r := recover(); r != nil {
				if nlr, ok := r.(NonLocalReturn); ok {
					if nlr.HomeFrame == homeFrame {
						for i.fp > homeFrame {
							i.popFrame()
						}
						if i.fp == homeFrame {
							i.popFrame()
						}
						result = nlr.Value
						return
					}
					panic(nlr)
				}
				panic(r)
			}
		}()

		result = i.runFrame()
		return result
	}

	return method.Invoke(i.vm, rcvr, args)
}

// vtableFor returns the vtable for a value.
func (i *Interpreter) vtableFor(v Value) *VTable {
	// Handle special values
	switch {
	case v == Nil:
		if c := i.Classes.Lookup("UndefinedObject"); c != nil {
			return c.VTable
		}
	case v == True:
		if c := i.Classes.Lookup("True"); c != nil {
			return c.VTable
		}
	case v == False:
		if c := i.Classes.Lookup("False"); c != nil {
			return c.VTable
		}
	case v.IsSmallInt():
		if c := i.Classes.Lookup("SmallInteger"); c != nil {
			return c.VTable
		}
	case v.IsFloat():
		if c := i.Classes.Lookup("Float"); c != nil {
			return c.VTable
		}
	case v.IsBlock():
		if c := i.Classes.Lookup("Block"); c != nil {
			return c.VTable
		}
	case v.IsSymbol():
		// Check for special symbol-encoded values first
		if IsStringValue(v) {
			if c := i.Classes.Lookup("String"); c != nil {
				return c.VTable
			}
		}
		if IsDictionaryValue(v) {
			if c := i.Classes.Lookup("Dictionary"); c != nil {
				return c.VTable
			}
		}
		// Check for gRPC client/stream values
		if isGrpcClientValue(v) {
			if c := i.Classes.Lookup("GrpcClient"); c != nil {
				return c.VTable
			}
		}
		if isGrpcStreamValue(v) {
			if c := i.Classes.Lookup("GrpcStream"); c != nil {
				return c.VTable
			}
		}
		// Check for Result values
		if isResultValue(v) {
			r := getResult(v)
			if r != nil {
				if r.resultType == ResultSuccess {
					if c := i.Classes.Lookup("Success"); c != nil {
						return c.VTable
					}
				} else {
					if c := i.Classes.Lookup("Failure"); c != nil {
						return c.VTable
					}
				}
			}
		}
		// Check if this symbol represents a class name (for class-side messages)
		if i.Symbols != nil {
			symName := i.Symbols.Name(v.SymbolID())
			if cls := i.Classes.Lookup(symName); cls != nil {
				// Use ClassVTable for class-side dispatch (metaclass methods)
				if cls.ClassVTable != nil {
					return cls.ClassVTable
				}
				return cls.VTable // Fallback if ClassVTable not initialized
			}
		}
		// Fall back to Symbol class for regular symbols
		if c := i.Classes.Lookup("Symbol"); c != nil {
			return c.VTable
		}
	case v.IsObject():
		obj := ObjectFromValue(v)
		if obj != nil {
			return obj.VTablePtr()
		}
	}
	return nil
}

// ---------------------------------------------------------------------------
// Block support
// ---------------------------------------------------------------------------

// BlockValue wraps a block with its captures.
// This is a simplified representation; a full implementation would
// use a proper Block class.
type BlockValue struct {
	Block      *BlockMethod
	Captures   []Value
	HomeFrame  int   // frame pointer of the method that created this block
	HomeSelf   Value // self from the enclosing method context
}

// NonLocalReturn is used to propagate non-local returns from blocks.
// When a block executes ^value, it needs to return from the enclosing method,
// not just from the block itself.
type NonLocalReturn struct {
	Value     Value
	HomeFrame int // target frame to return to
}

// blockRegistry stores active blocks (temporary solution until proper Block class)
var blockRegistry = make(map[int]*BlockValue)
var nextBlockID = 1

// blocksByHomeFrame tracks which block IDs belong to each home frame
// This allows cleanup when frames are popped to prevent memory leaks
var blocksByHomeFrame = make(map[int][]int)

// releaseBlocksForFrame removes all blocks whose home frame is being popped
func releaseBlocksForFrame(frameIndex int) {
	if blockIDs, ok := blocksByHomeFrame[frameIndex]; ok {
		for _, id := range blockIDs {
			delete(blockRegistry, id)
		}
		delete(blocksByHomeFrame, frameIndex)
	}
}

func (i *Interpreter) createBlockValue(block *BlockMethod, captures []Value) Value {
	// Store in registry and return an ID encoded as a symbol
	// A full implementation would create a proper Block object
	id := nextBlockID
	nextBlockID++

	// Determine the home frame (the enclosing method's frame)
	// If we're in a block, propagate its HomeFrame; otherwise use current frame
	homeFrame := i.fp
	var homeSelf Value = Nil
	if i.fp >= 0 {
		frame := i.frames[i.fp]
		if frame != nil {
			homeSelf = frame.Receiver
			// If we're in a block, use its home frame to find the enclosing method
			if frame.Block != nil && frame.HomeFrame >= 0 {
				homeFrame = frame.HomeFrame
				// Also get homeSelf from the actual home frame
				if homeFrame >= 0 && homeFrame < len(i.frames) && i.frames[homeFrame] != nil {
					homeSelf = i.frames[homeFrame].Receiver
				}
			}
		}
	}

// 	fmt.Printf("DEBUG createBlockValue: id=%d, HomeFrame=%d, sp=%d\n", id, homeFrame, i.sp)
	blockRegistry[id] = &BlockValue{
		Block:     block,
		Captures:  captures,
		HomeFrame: homeFrame, // remember the enclosing method's frame
		HomeSelf:  homeSelf,  // capture self from enclosing method
	}
	// Track this block by its home frame for cleanup when frame is popped
	blocksByHomeFrame[homeFrame] = append(blocksByHomeFrame[homeFrame], id)
	return FromBlockID(uint32(id))
}

func (i *Interpreter) getBlockValue(v Value) *BlockValue {
	if v.IsBlock() {
		id := int(v.BlockID())
		return blockRegistry[id]
	}
	return nil
}

// ---------------------------------------------------------------------------
// Primitive operations (fast paths)
// ---------------------------------------------------------------------------

func (i *Interpreter) primitivePlus(a, b Value) Value {
	if a.IsSmallInt() && b.IsSmallInt() {
		return FromSmallInt(a.SmallInt() + b.SmallInt())
	}
	if a.IsFloat() && b.IsFloat() {
		return FromFloat64(a.Float64() + b.Float64())
	}
	if a.IsSmallInt() && b.IsFloat() {
		return FromFloat64(float64(a.SmallInt()) + b.Float64())
	}
	if a.IsFloat() && b.IsSmallInt() {
		return FromFloat64(a.Float64() + float64(b.SmallInt()))
	}
	// Would do full send for other types
	return Nil
}

func (i *Interpreter) primitiveMinus(a, b Value) Value {
	if a.IsSmallInt() && b.IsSmallInt() {
		return FromSmallInt(a.SmallInt() - b.SmallInt())
	}
	if a.IsFloat() && b.IsFloat() {
		return FromFloat64(a.Float64() - b.Float64())
	}
	if a.IsSmallInt() && b.IsFloat() {
		return FromFloat64(float64(a.SmallInt()) - b.Float64())
	}
	if a.IsFloat() && b.IsSmallInt() {
		return FromFloat64(a.Float64() - float64(b.SmallInt()))
	}
	return Nil
}

func (i *Interpreter) primitiveTimes(a, b Value) Value {
	if a.IsSmallInt() && b.IsSmallInt() {
		return FromSmallInt(a.SmallInt() * b.SmallInt())
	}
	if a.IsFloat() && b.IsFloat() {
		return FromFloat64(a.Float64() * b.Float64())
	}
	if a.IsSmallInt() && b.IsFloat() {
		return FromFloat64(float64(a.SmallInt()) * b.Float64())
	}
	if a.IsFloat() && b.IsSmallInt() {
		return FromFloat64(a.Float64() * float64(b.SmallInt()))
	}
	return Nil
}

func (i *Interpreter) primitiveDiv(a, b Value) Value {
	if a.IsSmallInt() && b.IsSmallInt() {
		if b.SmallInt() == 0 {
			return Nil // Division by zero
		}
		return FromSmallInt(a.SmallInt() / b.SmallInt())
	}
	if a.IsFloat() && b.IsFloat() {
		return FromFloat64(a.Float64() / b.Float64())
	}
	if a.IsSmallInt() && b.IsFloat() {
		return FromFloat64(float64(a.SmallInt()) / b.Float64())
	}
	if a.IsFloat() && b.IsSmallInt() {
		return FromFloat64(a.Float64() / float64(b.SmallInt()))
	}
	return Nil
}

func (i *Interpreter) primitiveMod(a, b Value) Value {
	if a.IsSmallInt() && b.IsSmallInt() {
		if b.SmallInt() == 0 {
			return Nil
		}
		return FromSmallInt(a.SmallInt() % b.SmallInt())
	}
	return Nil
}

func (i *Interpreter) primitiveLT(a, b Value) Value {
	if a.IsSmallInt() && b.IsSmallInt() {
		if a.SmallInt() < b.SmallInt() {
			return True
		}
		return False
	}
	if a.IsFloat() && b.IsFloat() {
		if a.Float64() < b.Float64() {
			return True
		}
		return False
	}
	return Nil
}

func (i *Interpreter) primitiveGT(a, b Value) Value {
	if a.IsSmallInt() && b.IsSmallInt() {
		if a.SmallInt() > b.SmallInt() {
			return True
		}
		return False
	}
	if a.IsFloat() && b.IsFloat() {
		if a.Float64() > b.Float64() {
			return True
		}
		return False
	}
	return Nil
}

func (i *Interpreter) primitiveLE(a, b Value) Value {
	if a.IsSmallInt() && b.IsSmallInt() {
		if a.SmallInt() <= b.SmallInt() {
			return True
		}
		return False
	}
	return Nil
}

func (i *Interpreter) primitiveGE(a, b Value) Value {
	if a.IsSmallInt() && b.IsSmallInt() {
		if a.SmallInt() >= b.SmallInt() {
			return True
		}
		return False
	}
	return Nil
}

func (i *Interpreter) primitiveEQ(a, b Value) Value {
	if a == b {
		return True
	}
	// For floats, compare values
	if a.IsFloat() && b.IsFloat() {
		if a.Float64() == b.Float64() {
			return True
		}
	}
	// For strings, compare content
	if IsStringValue(a) && IsStringValue(b) {
		if GetStringContent(a) == GetStringContent(b) {
			return True
		}
	}
	return False
}

func (i *Interpreter) primitiveNE(a, b Value) Value {
	if i.primitiveEQ(a, b) == True {
		return False
	}
	return True
}

func (i *Interpreter) primitiveAt(rcvr, idx Value) Value {
	if !idx.IsSmallInt() {
		return Nil
	}
	index := idx.SmallInt()

	// Handle arrays/objects
	if rcvr.IsObject() {
		obj := ObjectFromValue(rcvr)
		if obj != nil {
			if index < 0 || index >= int64(obj.NumSlots()) {
				return Nil // Bounds error
			}
			return obj.GetSlot(int(index))
		}
	}

	// Handle strings
	if IsStringValue(rcvr) {
		str := GetStringContent(rcvr)
		if index < 0 || index >= int64(len(str)) {
			return Nil // Bounds error
		}
		// Return character as small integer (ASCII/Unicode code point)
		return FromSmallInt(int64(str[index]))
	}

	return Nil
}

func (i *Interpreter) primitiveAtPut(rcvr, idx, val Value) Value {
	if !idx.IsSmallInt() {
		return val
	}
	index := idx.SmallInt()

	// Handle arrays/objects
	if rcvr.IsObject() {
		obj := ObjectFromValue(rcvr)
		if obj != nil {
			if index < 0 || index >= int64(obj.NumSlots()) {
				return val // Bounds error - return value anyway
			}
			obj.SetSlot(int(index), val)
		}
	}

	// Strings are immutable, so at:put: on strings is a no-op
	return val
}

func (i *Interpreter) primitiveSize(rcvr Value) Value {
	// Handle arrays/objects
	if rcvr.IsObject() {
		obj := ObjectFromValue(rcvr)
		if obj != nil {
			return FromSmallInt(int64(obj.NumSlots()))
		}
	}

	// Handle strings
	if IsStringValue(rcvr) {
		str := GetStringContent(rcvr)
		return FromSmallInt(int64(len(str)))
	}

	return FromSmallInt(0)
}

func (i *Interpreter) primitiveValue(rcvr Value) Value {
	// Execute block with no arguments
	if bv := i.getBlockValue(rcvr); bv != nil {
		return i.ExecuteBlock(bv.Block, bv.Captures, nil, bv.HomeFrame, bv.HomeSelf)
	}
	return Nil
}

func (i *Interpreter) primitiveValue1(rcvr Value, arg Value) Value {
	if bv := i.getBlockValue(rcvr); bv != nil {
		return i.ExecuteBlock(bv.Block, bv.Captures, []Value{arg}, bv.HomeFrame, bv.HomeSelf)
	}
	return Nil
}

func (i *Interpreter) primitiveValue2(rcvr, arg1, arg2 Value) Value {
	if bv := i.getBlockValue(rcvr); bv != nil {
		return i.ExecuteBlock(bv.Block, bv.Captures, []Value{arg1, arg2}, bv.HomeFrame, bv.HomeSelf)
	}
	return Nil
}

func (i *Interpreter) primitiveValue3(rcvr, arg1, arg2, arg3 Value) Value {
	if bv := i.getBlockValue(rcvr); bv != nil {
		return i.ExecuteBlock(bv.Block, bv.Captures, []Value{arg1, arg2, arg3}, bv.HomeFrame, bv.HomeSelf)
	}
	return Nil
}

func (i *Interpreter) primitiveNew(rcvr Value) Value {
	// Placeholder - would create new instance of class
	return Nil
}

func (i *Interpreter) primitiveClass(rcvr Value) Value {
	// Placeholder - would return class of value
	return Nil
}
