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
	Block    *BlockMethod // nil for regular methods
	Captures []Value      // captured variables for blocks
}

// ---------------------------------------------------------------------------
// Interpreter: Bytecode execution engine
// ---------------------------------------------------------------------------

// Interpreter executes Maggie bytecode.
type Interpreter struct {
	// Global state (would be part of VM in full implementation)
	Selectors *SelectorTable
	Classes   *ClassTable
	Globals   map[string]Value

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
		frames:    make([]*CallFrame, 64), // Fixed-size frame stack
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
		panic("stack overflow")
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
		panic("call stack overflow")
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

func (i *Interpreter) pushBlockFrame(block *BlockMethod, captures []Value, args []Value) {
	i.fp++
	if i.fp >= len(i.frames) {
		panic("call stack overflow")
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
		Method:   nil, // block frame
		Receiver: Nil, // blocks don't have a receiver
		IP:       0,
		BP:       bp,
		Block:    block,
		Captures: captures,
	}
}

func (i *Interpreter) popFrame() {
	frame := i.frames[i.fp]
	i.sp = frame.BP // Discard temps
	i.frames[i.fp] = nil
	i.fp--
}

// ---------------------------------------------------------------------------
// Main interpreter loop
// ---------------------------------------------------------------------------

// Execute runs a compiled method with the given receiver and arguments.
// Returns the result value.
func (i *Interpreter) Execute(method *CompiledMethod, receiver Value, args []Value) Value {
	i.pushFrame(method, receiver, args)
	return i.run()
}

// ExecuteBlock runs a block with captured variables and arguments.
func (i *Interpreter) ExecuteBlock(block *BlockMethod, captures []Value, args []Value) Value {
	i.pushBlockFrame(block, captures, args)
	return i.runBlock()
}

// run is the main interpreter loop for methods.
func (i *Interpreter) run() Value {
	for {
		frame := i.frames[i.fp]
		bc := frame.Method.Bytecode
		literals := frame.Method.Literals

		if frame.IP >= len(bc) {
			// Implicit return self at end of method
			i.popFrame()
			return frame.Receiver
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
			i.push(frame.Receiver)

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

		case OpPushIvar:
			idx := bc[frame.IP]
			frame.IP++
			obj := ObjectFromValue(frame.Receiver)
			if obj != nil {
				i.push(obj.GetSlot(int(idx)))
			} else {
				i.push(Nil) // Non-object receiver
			}

		case OpStoreIvar:
			idx := bc[frame.IP]
			frame.IP++
			obj := ObjectFromValue(frame.Receiver)
			if obj != nil {
				obj.SetSlot(int(idx), i.top())
			}

		case OpPushGlobal:
			idx := binary.LittleEndian.Uint16(bc[frame.IP:])
			frame.IP += 2
			// For now, use literal as global name
			if int(idx) < len(literals) {
				// Assume literal is a symbol or string representing global name
				i.push(Nil) // Placeholder - would lookup in globals table
			} else {
				i.push(Nil)
			}

		case OpStoreGlobal:
			idx := binary.LittleEndian.Uint16(bc[frame.IP:])
			frame.IP += 2
			_ = idx // Would store in globals table
			// Value remains on stack

		case OpPushCaptured:
			idx := bc[frame.IP]
			frame.IP++
			if frame.Block != nil && int(idx) < len(frame.Captures) {
				i.push(frame.Captures[idx])
			} else {
				i.push(Nil)
			}

		case OpStoreCaptured:
			idx := bc[frame.IP]
			frame.IP++
			if frame.Block != nil && int(idx) < len(frame.Captures) {
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
			result := i.sendSuper(sel, argc, frame.Method.Class())
			i.push(result)

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
			i.popFrame()
			return result

		case OpReturnSelf:
			result := frame.Receiver
			i.popFrame()
			return result

		case OpReturnNil:
			i.popFrame()
			return Nil

		case OpBlockReturn:
			// Non-local return - need to unwind to method frame
			result := i.pop()
			// For now, just return from current frame
			i.popFrame()
			return result

		// --- Blocks ---
		case OpCreateBlock:
			methodIdx := binary.LittleEndian.Uint16(bc[frame.IP:])
			frame.IP += 2
			nCaptures := int(bc[frame.IP])
			frame.IP++

			// Get the block method
			block := frame.Method.GetBlock(int(methodIdx))

			// Pop captures from stack
			captures := i.popN(nCaptures)

			// Create a block object (for now, just push a special value)
			// In full implementation, this would create a Block object
			blockVal := i.createBlockValue(block, captures)
			i.push(blockVal)

		case OpCaptureTemp:
			idx := bc[frame.IP]
			frame.IP++
			i.push(i.getTemp(int(idx)))

		case OpCaptureIvar:
			idx := bc[frame.IP]
			frame.IP++
			obj := ObjectFromValue(frame.Receiver)
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
			// Create array object (placeholder)
			_ = elements
			i.push(Nil) // Would create actual array

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
			panic(fmt.Sprintf("unknown opcode: %02X", op))
		}
	}
}

// runBlock is the interpreter loop for block execution.
func (i *Interpreter) runBlock() Value {
	for {
		frame := i.frames[i.fp]
		bc := frame.Block.Bytecode
		literals := frame.Block.Literals

		if frame.IP >= len(bc) {
			// Implicit return nil at end of block
			i.popFrame()
			return Nil
		}

		op := Opcode(bc[frame.IP])
		frame.IP++

		// Most opcodes are the same as for methods
		switch op {
		case OpNOP:
			// Do nothing

		case OpPOP:
			i.pop()

		case OpDUP:
			i.push(i.top())

		case OpPushNil:
			i.push(Nil)

		case OpPushTrue:
			i.push(True)

		case OpPushFalse:
			i.push(False)

		case OpPushSelf:
			// Blocks capture self from enclosing context
			// For now, push nil
			i.push(Nil)

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
			i.push(literals[idx])

		case OpPushFloat:
			bits := binary.LittleEndian.Uint64(bc[frame.IP:])
			frame.IP += 8
			i.push(Value(bits))

		case OpPushTemp:
			idx := bc[frame.IP]
			frame.IP++
			i.push(i.getTemp(int(idx)))

		case OpStoreTemp:
			idx := bc[frame.IP]
			frame.IP++
			i.setTemp(int(idx), i.top())

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

		// Arithmetic and comparisons
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

		case OpSendLT:
			b := i.pop()
			a := i.pop()
			i.push(i.primitiveLT(a, b))

		case OpSendGT:
			b := i.pop()
			a := i.pop()
			i.push(i.primitiveGT(a, b))

		case OpSendEQ:
			b := i.pop()
			a := i.pop()
			i.push(i.primitiveEQ(a, b))

		// Control flow
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

		case OpReturnTop:
			result := i.pop()
			i.popFrame()
			return result

		default:
			// For opcodes not yet implemented in blocks, fall back
			panic(fmt.Sprintf("opcode %s not implemented for blocks", op))
		}
	}
}

// ---------------------------------------------------------------------------
// Message sending
// ---------------------------------------------------------------------------

// send performs a message send via VTable lookup.
func (i *Interpreter) send(selector int, argc int) Value {
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
		// Recursive interpretation
		i.pushFrame(cm, rcvr, args)
		return i.run()
	}

	// Primitive method
	return method.Invoke(i, rcvr, args)
}

// sendSuper performs a super send.
func (i *Interpreter) sendSuper(selector int, argc int, definingClass *Class) Value {
	args := i.popN(argc)
	rcvr := i.pop()

	// Start lookup from superclass
	if definingClass == nil || definingClass.Superclass == nil {
		return Nil
	}

	vt := definingClass.Superclass.VTable
	if vt == nil {
		return Nil
	}

	method := vt.Lookup(selector)
	if method == nil {
		return Nil
	}

	if cm, ok := method.(*CompiledMethod); ok {
		i.pushFrame(cm, rcvr, args)
		return i.run()
	}

	return method.Invoke(i, rcvr, args)
}

// vtableFor returns the vtable for a value.
func (i *Interpreter) vtableFor(v Value) *VTable {
	if v.IsObject() {
		obj := ObjectFromValue(v)
		if obj != nil {
			return obj.VTablePtr()
		}
	}
	// For non-object values, would return appropriate vtable
	// (integerVTable, floatVTable, etc.)
	return nil
}

// ---------------------------------------------------------------------------
// Block support
// ---------------------------------------------------------------------------

// BlockValue wraps a block with its captures.
// This is a simplified representation; a full implementation would
// use a proper Block class.
type BlockValue struct {
	Block    *BlockMethod
	Captures []Value
}

// blockRegistry stores active blocks (temporary solution until proper Block class)
var blockRegistry = make(map[int]*BlockValue)
var nextBlockID = 1

func (i *Interpreter) createBlockValue(block *BlockMethod, captures []Value) Value {
	// Store in registry and return an ID encoded as a symbol
	// A full implementation would create a proper Block object
	id := nextBlockID
	nextBlockID++
	blockRegistry[id] = &BlockValue{Block: block, Captures: captures}
	return FromSymbolID(uint32(id))
}

func (i *Interpreter) getBlockValue(v Value) *BlockValue {
	if v.IsSymbol() {
		id := int(v.SymbolID())
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
	return False
}

func (i *Interpreter) primitiveNE(a, b Value) Value {
	if i.primitiveEQ(a, b) == True {
		return False
	}
	return True
}

func (i *Interpreter) primitiveAt(rcvr, idx Value) Value {
	// Placeholder - would dispatch to collection type
	return Nil
}

func (i *Interpreter) primitiveAtPut(rcvr, idx, val Value) Value {
	// Placeholder
	return val
}

func (i *Interpreter) primitiveSize(rcvr Value) Value {
	// Placeholder
	return FromSmallInt(0)
}

func (i *Interpreter) primitiveValue(rcvr Value) Value {
	// Execute block with no arguments
	if bv := i.getBlockValue(rcvr); bv != nil {
		return i.ExecuteBlock(bv.Block, bv.Captures, nil)
	}
	return Nil
}

func (i *Interpreter) primitiveValue1(rcvr Value, arg Value) Value {
	if bv := i.getBlockValue(rcvr); bv != nil {
		return i.ExecuteBlock(bv.Block, bv.Captures, []Value{arg})
	}
	return Nil
}

func (i *Interpreter) primitiveValue2(rcvr, arg1, arg2 Value) Value {
	if bv := i.getBlockValue(rcvr); bv != nil {
		return i.ExecuteBlock(bv.Block, bv.Captures, []Value{arg1, arg2})
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
