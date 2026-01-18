package bytecode

import (
	"encoding/binary"
	"fmt"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
)

// MessageSender is the interface for sending messages to objects.
// This allows the VM to integrate with the runtime's message dispatch.
type MessageSender interface {
	// SendMessage sends a message to a receiver and returns the result.
	SendMessage(receiver, selector string, args ...string) (string, error)
}

// InstanceAccessor is the interface for accessing instance variables.
// This allows the VM to read/write ivars through the runtime.
type InstanceAccessor interface {
	// GetInstanceVar reads an instance variable value.
	GetInstanceVar(instanceID, varName string) (string, error)
	// SetInstanceVar writes an instance variable value.
	SetInstanceVar(instanceID, varName, value string) error
}

// CaptureCell holds a captured variable with reference semantics.
// Changes made inside a block are visible outside, and vice versa.
type CaptureCell struct {
	Value    string    // Current value
	Name     string    // Variable name (for debugging and ivar writeback)
	Source   VarSource // Where the variable originated
	Closed   bool      // True if cell owns its value (for long-lived closures)
	RefCount int32     // Reference count for garbage collection

	// For ivar captures, these enable writeback
	InstanceID string           // Instance that owns this ivar
	Accessor   InstanceAccessor // For reading/writing ivars
}

// Get returns the current value, refreshing from source if needed.
func (c *CaptureCell) Get() string {
	if c == nil {
		return ""
	}

	// For unclosed ivar captures, read fresh from database
	if c.Source == VarSourceIVar && !c.Closed && c.Accessor != nil && c.InstanceID != "" {
		if val, err := c.Accessor.GetInstanceVar(c.InstanceID, c.Name); err == nil {
			c.Value = val
		}
	}

	return c.Value
}

// Set updates the cell value, writing back to source if needed.
func (c *CaptureCell) Set(value string) {
	if c == nil {
		return
	}

	c.Value = value

	// For unclosed ivar captures, write back to database
	if c.Source == VarSourceIVar && !c.Closed && c.Accessor != nil && c.InstanceID != "" {
		c.Accessor.SetInstanceVar(c.InstanceID, c.Name, value)
	}
}

// Close captures a snapshot of the value for long-lived closures.
// After closing, the cell no longer reads/writes from the original source.
func (c *CaptureCell) Close() {
	if c == nil || c.Closed {
		return
	}

	// For ivars, read current value one last time
	if c.Source == VarSourceIVar && c.Accessor != nil && c.InstanceID != "" {
		if val, err := c.Accessor.GetInstanceVar(c.InstanceID, c.Name); err == nil {
			c.Value = val
		}
	}

	c.Closed = true
}

// Retain increments the reference count.
func (c *CaptureCell) Retain() {
	if c != nil {
		atomic.AddInt32(&c.RefCount, 1)
	}
}

// Release decrements the reference count.
func (c *CaptureCell) Release() {
	if c != nil {
		atomic.AddInt32(&c.RefCount, -1)
	}
}

// NonLocalReturnError signals a non-local return from a block.
// This error propagates up through nested block invocations until
// it reaches the enclosing method context that should handle it.
type NonLocalReturnError struct {
	Value string // The value to return
}

func (e *NonLocalReturnError) Error() string {
	return "non-local return"
}

// IsNonLocalReturn checks if an error is a non-local return.
func IsNonLocalReturn(err error) (string, bool) {
	if nlr, ok := err.(*NonLocalReturnError); ok {
		return nlr.Value, true
	}
	return "", false
}

// VM executes bytecode chunks.
type VM struct {
	// Current execution state
	chunk *Chunk  // Current bytecode chunk
	ip    int     // Instruction pointer
	stack []string // Value stack
	sp    int     // Stack pointer

	// Variable storage for current frame
	locals   []string       // Local variable slots
	params   []string       // Parameter values
	captures []*CaptureCell // Captured variable cells

	// Runtime context
	instanceID string           // Current instance ID (for ivar access)
	sender     MessageSender    // For message sends
	accessor   InstanceAccessor // For ivar access

	// Call stack for nested block invocations
	frames     []*CallFrame
	frameDepth int

	// Block registry for bytecode blocks
	blockRegistry   map[string]*RegisteredBlock
	blockRegistryMu sync.RWMutex
	blockCounter    uint64

	// Debug/trace mode
	Trace bool
}

// CallFrame represents an active block invocation on the call stack.
type CallFrame struct {
	chunk      *Chunk
	ip         int
	bp         int            // Base pointer into stack
	locals     []string
	captures   []*CaptureCell
	instanceID string
}

// RegisteredBlock holds a compiled block and its captures for invocation.
type RegisteredBlock struct {
	Chunk    *Chunk
	Captures []*CaptureCell
}

// NewVM creates a new VM instance.
func NewVM() *VM {
	return &VM{
		stack:         make([]string, 1024),
		frames:        make([]*CallFrame, 64),
		blockRegistry: make(map[string]*RegisteredBlock),
	}
}

// SetMessageSender sets the message sender for the VM.
func (vm *VM) SetMessageSender(sender MessageSender) {
	vm.sender = sender
}

// SetInstanceAccessor sets the instance accessor for the VM.
func (vm *VM) SetInstanceAccessor(accessor InstanceAccessor) {
	vm.accessor = accessor
}

// Execute runs a bytecode chunk with the given arguments.
// Returns the result value and any error.
func (vm *VM) Execute(chunk *Chunk, instanceID string, args []string) (string, error) {
	vm.chunk = chunk
	vm.ip = 0
	vm.sp = 0
	vm.instanceID = instanceID
	vm.frameDepth = 0

	// Initialize parameters
	vm.params = args

	// Initialize locals
	vm.locals = make([]string, chunk.LocalCount)

	// Initialize captures (empty - will be populated by caller or OpMakeBlock)
	vm.captures = make([]*CaptureCell, len(chunk.CaptureInfo))

	return vm.run()
}

// ExecuteWithCaptures runs bytecode with pre-populated capture cells.
// Used for invoking blocks that have captured variables.
func (vm *VM) ExecuteWithCaptures(chunk *Chunk, instanceID string, args []string, captures []*CaptureCell) (string, error) {
	vm.chunk = chunk
	vm.ip = 0
	vm.sp = 0
	vm.instanceID = instanceID
	vm.params = args
	vm.locals = make([]string, chunk.LocalCount)
	vm.captures = captures

	return vm.run()
}

// run is the main execution loop.
func (vm *VM) run() (string, error) {
	for {
		if vm.ip >= len(vm.chunk.Code) {
			break
		}

		op := Opcode(vm.chunk.Code[vm.ip])
		vm.ip++

		if vm.Trace {
			info := GetOpcodeInfo(op)
			fmt.Printf("[%04x] %-16s sp=%d\n", vm.ip-1, info.Name, vm.sp)
		}

		switch op {
		// ============ Stack Operations ============
		case OpNop:
			// Do nothing

		case OpPop:
			vm.sp--

		case OpDup:
			vm.stack[vm.sp] = vm.stack[vm.sp-1]
			vm.sp++

		case OpSwap:
			vm.stack[vm.sp-1], vm.stack[vm.sp-2] = vm.stack[vm.sp-2], vm.stack[vm.sp-1]

		case OpRot:
			// Rotate top 3: [a b c] -> [b c a]
			a := vm.stack[vm.sp-3]
			vm.stack[vm.sp-3] = vm.stack[vm.sp-2]
			vm.stack[vm.sp-2] = vm.stack[vm.sp-1]
			vm.stack[vm.sp-1] = a

		// ============ Constants ============
		case OpConst:
			idx := vm.readUint16()
			vm.push(vm.chunk.Constants[idx])

		case OpConstNil:
			vm.push("")

		case OpConstTrue:
			vm.push("true")

		case OpConstFalse:
			vm.push("false")

		case OpConstZero:
			vm.push("0")

		case OpConstOne:
			vm.push("1")

		case OpConstEmpty:
			vm.push("")

		// ============ Local Variables ============
		case OpLoadLocal:
			slot := vm.chunk.Code[vm.ip]
			vm.ip++
			vm.push(vm.locals[slot])

		case OpStoreLocal:
			slot := vm.chunk.Code[vm.ip]
			vm.ip++
			vm.locals[slot] = vm.pop()

		case OpLoadParam:
			idx := vm.chunk.Code[vm.ip]
			vm.ip++
			if int(idx) < len(vm.params) {
				vm.push(vm.params[idx])
			} else {
				vm.push("") // Missing param
			}

		// ============ Captures ============
		case OpLoadCapture:
			idx := vm.chunk.Code[vm.ip]
			vm.ip++
			if int(idx) < len(vm.captures) && vm.captures[idx] != nil {
				vm.push(vm.captures[idx].Get())
			} else {
				vm.push("")
			}

		case OpStoreCapture:
			idx := vm.chunk.Code[vm.ip]
			vm.ip++
			value := vm.pop()
			if int(idx) < len(vm.captures) && vm.captures[idx] != nil {
				vm.captures[idx].Set(value)
			}

		case OpMakeCapture:
			// Create a new capture cell from a local
			slot := vm.chunk.Code[vm.ip]
			vm.ip++
			cell := &CaptureCell{
				Value:  vm.locals[slot],
				Source: VarSourceLocal,
			}
			vm.push(fmt.Sprintf("capture:%p", cell))

		// ============ Instance Variables ============
		case OpLoadIVar:
			nameIdx := vm.readUint16()
			name := vm.chunk.Constants[nameIdx]
			value := vm.readIVar(name)
			vm.push(value)

		case OpStoreIVar:
			nameIdx := vm.readUint16()
			name := vm.chunk.Constants[nameIdx]
			value := vm.pop()
			vm.writeIVar(name, value)

		// ============ Arithmetic ============
		case OpAdd:
			b := vm.popInt()
			a := vm.popInt()
			vm.pushInt(a + b)

		case OpSub:
			b := vm.popInt()
			a := vm.popInt()
			vm.pushInt(a - b)

		case OpMul:
			b := vm.popInt()
			a := vm.popInt()
			vm.pushInt(a * b)

		case OpDiv:
			b := vm.popInt()
			a := vm.popInt()
			if b == 0 {
				vm.push("0") // Division by zero
			} else {
				vm.pushInt(a / b)
			}

		case OpMod:
			b := vm.popInt()
			a := vm.popInt()
			if b == 0 {
				vm.push("0")
			} else {
				vm.pushInt(a % b)
			}

		case OpNeg:
			a := vm.popInt()
			vm.pushInt(-a)

		// ============ Comparison ============
		case OpEq:
			b := vm.pop()
			a := vm.pop()
			vm.pushBool(a == b)

		case OpNe:
			b := vm.pop()
			a := vm.pop()
			vm.pushBool(a != b)

		case OpLt:
			b := vm.popInt()
			a := vm.popInt()
			vm.pushBool(a < b)

		case OpLe:
			b := vm.popInt()
			a := vm.popInt()
			vm.pushBool(a <= b)

		case OpGt:
			b := vm.popInt()
			a := vm.popInt()
			vm.pushBool(a > b)

		case OpGe:
			b := vm.popInt()
			a := vm.popInt()
			vm.pushBool(a >= b)

		// ============ Logical ============
		case OpNot:
			a := vm.pop()
			vm.pushBool(!vm.isTruthy(a))

		case OpAnd:
			b := vm.pop()
			a := vm.pop()
			vm.pushBool(vm.isTruthy(a) && vm.isTruthy(b))

		case OpOr:
			b := vm.pop()
			a := vm.pop()
			vm.pushBool(vm.isTruthy(a) || vm.isTruthy(b))

		// ============ String Operations ============
		case OpConcat:
			b := vm.pop()
			a := vm.pop()
			vm.push(a + b)

		case OpStrLen:
			s := vm.pop()
			vm.pushInt(len(s))

		// ============ Control Flow ============
		case OpJump:
			offset := vm.readInt16()
			vm.ip += int(offset)

		case OpJumpTrue:
			offset := vm.readInt16()
			cond := vm.pop()
			if vm.isTruthy(cond) {
				vm.ip += int(offset)
			}

		case OpJumpFalse:
			offset := vm.readInt16()
			cond := vm.pop()
			if !vm.isTruthy(cond) {
				vm.ip += int(offset)
			}

		case OpJumpNil:
			offset := vm.readInt16()
			val := vm.pop()
			if val == "" {
				vm.ip += int(offset)
			}

		case OpJumpNotNil:
			offset := vm.readInt16()
			val := vm.pop()
			if val != "" {
				vm.ip += int(offset)
			}

		// ============ Message Sends ============
		case OpSend:
			selIdx := vm.readUint16()
			argc := int(vm.chunk.Code[vm.ip])
			vm.ip++
			selector := vm.chunk.Constants[selIdx]

			// Pop args in reverse order
			args := make([]string, argc)
			for i := argc - 1; i >= 0; i-- {
				args[i] = vm.pop()
			}
			receiver := vm.pop()

			result, err := vm.sendMessage(receiver, selector, args)
			if err != nil {
				return "", err
			}
			vm.push(result)

		case OpSendSelf:
			selIdx := vm.readUint16()
			argc := int(vm.chunk.Code[vm.ip])
			vm.ip++
			selector := vm.chunk.Constants[selIdx]

			args := make([]string, argc)
			for i := argc - 1; i >= 0; i-- {
				args[i] = vm.pop()
			}
			vm.pop() // Pop "self" placeholder

			result, err := vm.sendMessage(vm.instanceID, selector, args)
			if err != nil {
				return "", err
			}
			vm.push(result)

		case OpSendSuper:
			selIdx := vm.readUint16()
			argc := int(vm.chunk.Code[vm.ip])
			vm.ip++
			selector := vm.chunk.Constants[selIdx]

			args := make([]string, argc)
			for i := argc - 1; i >= 0; i-- {
				args[i] = vm.pop()
			}

			// Super sends go to the instance but dispatch from parent class
			// For now, treat as self send (runtime will handle super dispatch)
			result, err := vm.sendMessage(vm.instanceID, "super:"+selector, args)
			if err != nil {
				return "", err
			}
			vm.push(result)

		case OpSendClass:
			classIdx := vm.readUint16()
			selIdx := vm.readUint16()
			argc := int(vm.chunk.Code[vm.ip])
			vm.ip++
			className := vm.chunk.Constants[classIdx]
			selector := vm.chunk.Constants[selIdx]

			args := make([]string, argc)
			for i := argc - 1; i >= 0; i-- {
				args[i] = vm.pop()
			}

			// Class methods are sent to the class name
			result, err := vm.sendMessage(className, selector, args)
			if err != nil {
				return "", err
			}
			vm.push(result)

		// ============ Block Operations ============
		case OpMakeBlock:
			chunkIdx := vm.readUint16()
			numCaptures := int(vm.chunk.Code[vm.ip])
			vm.ip++

			// Deserialize nested chunk from constants
			chunkBytes := vm.chunk.Constants[chunkIdx]
			nestedChunk, err := Deserialize([]byte(chunkBytes))
			if err != nil {
				return "", fmt.Errorf("failed to deserialize nested block: %w", err)
			}

			// Create capture cells from stack values
			captures := make([]*CaptureCell, numCaptures)
			for i := numCaptures - 1; i >= 0; i-- {
				value := vm.pop()
				var source VarSource
				var name string
				if i < len(nestedChunk.CaptureInfo) {
					source = nestedChunk.CaptureInfo[i].Source
					name = nestedChunk.CaptureInfo[i].Name
				}
				captures[i] = &CaptureCell{
					Value:      value,
					Source:     source,
					Name:       name,
					InstanceID: vm.instanceID,
					Accessor:   vm.accessor,
				}
			}

			// Register the block and push its ID
			blockID := vm.registerBlock(nestedChunk, captures)
			vm.push(blockID)

		case OpInvokeBlock:
			argc := int(vm.chunk.Code[vm.ip])
			vm.ip++

			// Pop args and block ID
			args := make([]string, argc)
			for i := argc - 1; i >= 0; i-- {
				args[i] = vm.pop()
			}
			blockID := vm.pop()

			result, err := vm.invokeBlock(blockID, args)
			if err != nil {
				return "", err
			}
			vm.push(result)

		case OpBlockValue:
			// Invoke block with 0 args (common case)
			blockID := vm.pop()
			result, err := vm.invokeBlock(blockID, nil)
			if err != nil {
				return "", err
			}
			vm.push(result)

		// ============ Array Operations ============
		case OpArrayNew:
			vm.push("[]")

		case OpArrayPush:
			value := vm.pop()
			arr := vm.pop()
			// Simple JSON array append
			if arr == "" || arr == "[]" {
				vm.push("[" + quoteJSON(value) + "]")
			} else {
				// Insert before closing bracket
				vm.push(arr[:len(arr)-1] + "," + quoteJSON(value) + "]")
			}

		case OpArrayAt:
			idx := vm.popInt()
			arr := vm.pop()
			// Simple JSON array access
			vm.push(jsonArrayAt(arr, idx))

		case OpArrayAtPut:
			value := vm.pop()
			idx := vm.popInt()
			arr := vm.pop()
			vm.push(jsonArrayAtPut(arr, idx, value))

		case OpArrayLen:
			arr := vm.pop()
			vm.pushInt(jsonArrayLen(arr))

		case OpArrayFirst:
			arr := vm.pop()
			vm.push(jsonArrayAt(arr, 0))

		case OpArrayLast:
			arr := vm.pop()
			length := jsonArrayLen(arr)
			if length > 0 {
				vm.push(jsonArrayAt(arr, length-1))
			} else {
				vm.push("")
			}

		case OpArrayRemove:
			idx := vm.popInt()
			arr := vm.pop()
			vm.push(jsonArrayRemove(arr, idx))

		// ============ Object Operations ============
		case OpObjectNew:
			vm.push("{}")

		case OpObjectAt:
			key := vm.pop()
			obj := vm.pop()
			vm.push(jsonObjectAt(obj, key))

		case OpObjectAtPut:
			value := vm.pop()
			key := vm.pop()
			obj := vm.pop()
			vm.push(jsonObjectAtPut(obj, key, value))

		case OpObjectHasKey:
			key := vm.pop()
			obj := vm.pop()
			vm.pushBool(jsonObjectHasKey(obj, key))

		case OpObjectKeys:
			obj := vm.pop()
			vm.push(jsonObjectKeys(obj))

		case OpObjectValues:
			obj := vm.pop()
			vm.push(jsonObjectValues(obj))

		case OpObjectRemove:
			key := vm.pop()
			obj := vm.pop()
			vm.push(jsonObjectRemove(obj, key))

		case OpObjectLen:
			obj := vm.pop()
			vm.pushInt(jsonObjectLen(obj))

		// ============ Return ============
		case OpReturn:
			return vm.pop(), nil

		case OpReturnNil:
			return "", nil

		case OpNonLocalRet:
			// Non-local return exits the enclosing method, not just the block.
			// This is signaled by returning a NonLocalReturnError which propagates
			// up through nested block invocations to the enclosing method.
			return "", &NonLocalReturnError{Value: vm.pop()}

		default:
			return "", fmt.Errorf("unknown opcode: 0x%02x at offset %d", op, vm.ip-1)
		}
	}

	// Implicit return of stack top or empty
	if vm.sp > 0 {
		return vm.stack[vm.sp-1], nil
	}
	return "", nil
}

// Stack helpers

func (vm *VM) push(val string) {
	vm.stack[vm.sp] = val
	vm.sp++
}

func (vm *VM) pop() string {
	vm.sp--
	return vm.stack[vm.sp]
}

func (vm *VM) peek() string {
	return vm.stack[vm.sp-1]
}

func (vm *VM) popInt() int {
	s := vm.pop()
	n, _ := strconv.Atoi(s)
	return n
}

func (vm *VM) pushInt(n int) {
	vm.push(strconv.Itoa(n))
}

func (vm *VM) pushBool(b bool) {
	if b {
		vm.push("true")
	} else {
		vm.push("false")
	}
}

func (vm *VM) isTruthy(s string) bool {
	return s != "" && s != "false" && s != "0" && s != "nil"
}

// Bytecode reading helpers

func (vm *VM) readUint16() uint16 {
	val := binary.BigEndian.Uint16(vm.chunk.Code[vm.ip:])
	vm.ip += 2
	return val
}

func (vm *VM) readInt16() int16 {
	return int16(vm.readUint16())
}

// Instance variable access

func (vm *VM) readIVar(name string) string {
	if vm.accessor == nil || vm.instanceID == "" {
		return ""
	}
	val, err := vm.accessor.GetInstanceVar(vm.instanceID, name)
	if err != nil {
		return ""
	}
	return val
}

func (vm *VM) writeIVar(name, value string) {
	if vm.accessor == nil || vm.instanceID == "" {
		return
	}
	vm.accessor.SetInstanceVar(vm.instanceID, name, value)
}

// Message sending

func (vm *VM) sendMessage(receiver, selector string, args []string) (string, error) {
	if vm.sender == nil {
		return "", fmt.Errorf("no message sender configured")
	}
	return vm.sender.SendMessage(receiver, selector, args...)
}

// Block management

func (vm *VM) registerBlock(chunk *Chunk, captures []*CaptureCell) string {
	vm.blockRegistryMu.Lock()
	defer vm.blockRegistryMu.Unlock()

	id := atomic.AddUint64(&vm.blockCounter, 1)
	blockID := fmt.Sprintf("bytecode_block_%d", id)
	vm.blockRegistry[blockID] = &RegisteredBlock{
		Chunk:    chunk,
		Captures: captures,
	}
	return blockID
}

func (vm *VM) invokeBlock(blockID string, args []string) (string, error) {
	vm.blockRegistryMu.RLock()
	block, ok := vm.blockRegistry[blockID]
	vm.blockRegistryMu.RUnlock()

	if !ok {
		// Try to invoke via message sender (for Bash blocks)
		if vm.sender != nil {
			return vm.sender.SendMessage(blockID, "value", args...)
		}
		return "", fmt.Errorf("block not found: %s", blockID)
	}

	// Create a new VM for nested execution to avoid state corruption
	nestedVM := NewVM()
	nestedVM.sender = vm.sender
	nestedVM.accessor = vm.accessor
	nestedVM.blockRegistry = vm.blockRegistry // Share block registry

	return nestedVM.ExecuteWithCaptures(block.Chunk, vm.instanceID, args, block.Captures)
}

// JSON helper functions (simplified implementations)

func quoteJSON(s string) string {
	// Simple JSON string quoting
	s = strings.ReplaceAll(s, `\`, `\\`)
	s = strings.ReplaceAll(s, `"`, `\"`)
	s = strings.ReplaceAll(s, "\n", `\n`)
	s = strings.ReplaceAll(s, "\t", `\t`)
	return `"` + s + `"`
}

func jsonArrayAt(arr string, idx int) string {
	// Simple JSON array element access
	arr = strings.TrimSpace(arr)
	if arr == "" || arr == "[]" {
		return ""
	}
	if !strings.HasPrefix(arr, "[") || !strings.HasSuffix(arr, "]") {
		return ""
	}

	// Parse array elements (simplified - doesn't handle nested structures well)
	inner := arr[1 : len(arr)-1]
	elements := splitJSONArray(inner)

	if idx < 0 || idx >= len(elements) {
		return ""
	}
	return unquoteJSON(strings.TrimSpace(elements[idx]))
}

func jsonArrayAtPut(arr string, idx int, value string) string {
	arr = strings.TrimSpace(arr)
	if arr == "" || arr == "[]" {
		if idx == 0 {
			return "[" + quoteJSON(value) + "]"
		}
		return arr
	}

	inner := arr[1 : len(arr)-1]
	elements := splitJSONArray(inner)

	if idx < 0 || idx >= len(elements) {
		return arr
	}

	elements[idx] = quoteJSON(value)
	return "[" + strings.Join(elements, ",") + "]"
}

func jsonArrayLen(arr string) int {
	arr = strings.TrimSpace(arr)
	if arr == "" || arr == "[]" {
		return 0
	}
	if !strings.HasPrefix(arr, "[") || !strings.HasSuffix(arr, "]") {
		return 0
	}

	inner := arr[1 : len(arr)-1]
	if strings.TrimSpace(inner) == "" {
		return 0
	}
	return len(splitJSONArray(inner))
}

func jsonArrayRemove(arr string, idx int) string {
	arr = strings.TrimSpace(arr)
	if arr == "" || arr == "[]" {
		return arr
	}

	inner := arr[1 : len(arr)-1]
	elements := splitJSONArray(inner)

	if idx < 0 || idx >= len(elements) {
		return arr
	}

	elements = append(elements[:idx], elements[idx+1:]...)
	if len(elements) == 0 {
		return "[]"
	}
	return "[" + strings.Join(elements, ",") + "]"
}

func splitJSONArray(inner string) []string {
	// Simple split that handles basic nesting
	var elements []string
	var current strings.Builder
	depth := 0
	inString := false
	escaped := false

	for _, c := range inner {
		if escaped {
			current.WriteRune(c)
			escaped = false
			continue
		}

		if c == '\\' && inString {
			current.WriteRune(c)
			escaped = true
			continue
		}

		if c == '"' {
			inString = !inString
			current.WriteRune(c)
			continue
		}

		if !inString {
			if c == '[' || c == '{' {
				depth++
			} else if c == ']' || c == '}' {
				depth--
			} else if c == ',' && depth == 0 {
				elements = append(elements, current.String())
				current.Reset()
				continue
			}
		}

		current.WriteRune(c)
	}

	if current.Len() > 0 {
		elements = append(elements, current.String())
	}

	return elements
}

func unquoteJSON(s string) string {
	s = strings.TrimSpace(s)
	if len(s) >= 2 && s[0] == '"' && s[len(s)-1] == '"' {
		s = s[1 : len(s)-1]
		s = strings.ReplaceAll(s, `\"`, `"`)
		s = strings.ReplaceAll(s, `\\`, `\`)
		s = strings.ReplaceAll(s, `\n`, "\n")
		s = strings.ReplaceAll(s, `\t`, "\t")
	}
	return s
}

func jsonObjectAt(obj string, key string) string {
	obj = strings.TrimSpace(obj)
	if obj == "" || obj == "{}" {
		return ""
	}

	// Simple key lookup (doesn't handle all edge cases)
	searchKey := quoteJSON(key) + ":"
	idx := strings.Index(obj, searchKey)
	if idx < 0 {
		return ""
	}

	// Find the value after the key
	valueStart := idx + len(searchKey)
	inner := obj[valueStart:]
	inner = strings.TrimSpace(inner)

	// Extract the value - need to handle string, number, object, array, bool, null
	if len(inner) == 0 {
		return ""
	}

	// Parse the value
	var value string
	if inner[0] == '"' {
		// String value - find closing quote
		escaped := false
		for i := 1; i < len(inner); i++ {
			if escaped {
				escaped = false
				continue
			}
			if inner[i] == '\\' {
				escaped = true
				continue
			}
			if inner[i] == '"' {
				value = inner[:i+1]
				break
			}
		}
	} else if inner[0] == '{' || inner[0] == '[' {
		// Object or array - find matching closing bracket
		depth := 0
		closeChar := byte('}')
		if inner[0] == '[' {
			closeChar = ']'
		}
		for i := 0; i < len(inner); i++ {
			if inner[i] == inner[0] {
				depth++
			} else if inner[i] == closeChar {
				depth--
				if depth == 0 {
					value = inner[:i+1]
					break
				}
			}
		}
	} else {
		// Number, bool, null - read until comma or closing brace
		for i := 0; i < len(inner); i++ {
			if inner[i] == ',' || inner[i] == '}' || inner[i] == ']' {
				value = strings.TrimSpace(inner[:i])
				break
			}
		}
		if value == "" {
			value = inner
		}
	}

	return unquoteJSON(value)
}

// findJSONKey finds the position of a JSON key in an object string,
// ensuring we only match actual keys (not keys that appear inside string values).
// Returns -1 if not found.
func findJSONKey(obj string, searchKey string) int {
	inString := false
	escaped := false
	depth := 0

	for i := 0; i < len(obj); i++ {
		if escaped {
			escaped = false
			continue
		}
		c := obj[i]

		if c == '\\' && inString {
			escaped = true
			continue
		}

		if c == '"' && !inString {
			// Starting a string - check if this could be our key
			// We're at depth 1 (inside the main object) and not in a string
			if depth == 1 && i+len(searchKey) <= len(obj) {
				if obj[i:i+len(searchKey)] == searchKey {
					return i
				}
			}
			inString = true
			continue
		}

		if c == '"' && inString {
			inString = false
			continue
		}

		if inString {
			continue
		}

		// Track nested objects/arrays
		if c == '{' || c == '[' {
			depth++
		} else if c == '}' || c == ']' {
			depth--
		}
	}
	return -1
}

func jsonObjectAtPut(obj string, key string, value string) string {
	obj = strings.TrimSpace(obj)
	quotedKey := quoteJSON(key)
	quotedValue := quoteJSON(value)

	if obj == "" || obj == "{}" {
		return "{" + quotedKey + ":" + quotedValue + "}"
	}

	// Find key position, ensuring we match only actual keys (not inside string values)
	searchKey := quotedKey + ":"
	idx := findJSONKey(obj, searchKey)
	if idx >= 0 {
		// Key exists - replace its value
		valueStart := idx + len(searchKey)
		inner := obj[valueStart:]
		inner = strings.TrimSpace(inner)

		if len(inner) == 0 {
			// Malformed JSON, just append
			return obj[:len(obj)-1] + "," + quotedKey + ":" + quotedValue + "}"
		}

		// Calculate actual start position after trimming whitespace
		whitespaceLen := len(obj[valueStart:]) - len(inner)
		actualValueStart := valueStart + whitespaceLen

		// Find the end of the existing value
		var valueEnd int
		if inner[0] == '"' {
			// String value - find closing quote
			escaped := false
			for i := 1; i < len(inner); i++ {
				if escaped {
					escaped = false
					continue
				}
				if inner[i] == '\\' {
					escaped = true
					continue
				}
				if inner[i] == '"' {
					valueEnd = actualValueStart + i + 1
					break
				}
			}
		} else if inner[0] == '{' || inner[0] == '[' {
			// Object or array - find matching closing bracket
			depth := 0
			closeChar := byte('}')
			if inner[0] == '[' {
				closeChar = ']'
			}
			inString := false
			escaped := false
			for i := 0; i < len(inner); i++ {
				if escaped {
					escaped = false
					continue
				}
				if inner[i] == '\\' && inString {
					escaped = true
					continue
				}
				if inner[i] == '"' {
					inString = !inString
				}
				if !inString {
					if inner[i] == inner[0] {
						depth++
					} else if inner[i] == closeChar {
						depth--
						if depth == 0 {
							valueEnd = actualValueStart + i + 1
							break
						}
					}
				}
			}
		} else {
			// Number, bool, null - read until comma or closing brace
			for i := 0; i < len(inner); i++ {
				if inner[i] == ',' || inner[i] == '}' || inner[i] == ']' {
					valueEnd = actualValueStart + i
					break
				}
			}
			if valueEnd == 0 {
				valueEnd = len(obj)
			}
		}

		// Replace the value
		return obj[:actualValueStart] + quotedValue + obj[valueEnd:]
	}

	// Add new key-value pair
	return obj[:len(obj)-1] + "," + quotedKey + ":" + quotedValue + "}"
}

func jsonObjectHasKey(obj string, key string) bool {
	obj = strings.TrimSpace(obj)
	if obj == "" || obj == "{}" {
		return false
	}
	searchKey := quoteJSON(key) + ":"
	return strings.Contains(obj, searchKey)
}

func jsonObjectKeys(obj string) string {
	obj = strings.TrimSpace(obj)
	if obj == "" || obj == "{}" {
		return "[]"
	}
	// Parse object and extract keys
	inner := obj[1 : len(obj)-1]
	if strings.TrimSpace(inner) == "" {
		return "[]"
	}
	keys := []string{}
	depth := 0
	inString := false
	start := -1
	for i := 0; i < len(inner); i++ {
		c := inner[i]
		if c == '"' && (i == 0 || inner[i-1] != '\\') {
			if !inString && depth == 0 {
				start = i + 1
			} else if inString && depth == 0 && start >= 0 {
				keys = append(keys, inner[start:i])
				start = -1
			}
			inString = !inString
		} else if !inString {
			if c == '{' || c == '[' {
				depth++
			} else if c == '}' || c == ']' {
				depth--
			} else if c == ':' && depth == 0 {
				// After colon, skip to next comma at depth 0
				start = -1
			}
		}
	}
	// Build result
	var sb strings.Builder
	sb.WriteString("[")
	for i, k := range keys {
		if i > 0 {
			sb.WriteString(",")
		}
		sb.WriteString(`"`)
		sb.WriteString(k)
		sb.WriteString(`"`)
	}
	sb.WriteString("]")
	return sb.String()
}

func jsonObjectValues(obj string) string {
	obj = strings.TrimSpace(obj)
	if obj == "" || obj == "{}" {
		return "[]"
	}
	// Parse object and extract values
	inner := obj[1 : len(obj)-1]
	if strings.TrimSpace(inner) == "" {
		return "[]"
	}
	values := []string{}
	depth := 0
	inString := false
	afterColon := false
	valueStart := -1
	for i := 0; i <= len(inner); i++ {
		c := byte(0)
		if i < len(inner) {
			c = inner[i]
		}
		if c == '"' && (i == 0 || inner[i-1] != '\\') {
			inString = !inString
		}
		if !inString {
			if c == '{' || c == '[' {
				depth++
			} else if c == '}' || c == ']' {
				depth--
			} else if c == ':' && depth == 0 && !afterColon {
				afterColon = true
				// Skip whitespace after colon
				for i+1 < len(inner) && (inner[i+1] == ' ' || inner[i+1] == '\t') {
					i++
				}
				valueStart = i + 1
			} else if (c == ',' || i == len(inner)) && depth == 0 && afterColon {
				if valueStart >= 0 && valueStart < len(inner) {
					val := strings.TrimSpace(inner[valueStart:i])
					values = append(values, val)
				}
				afterColon = false
				valueStart = -1
			}
		}
	}
	// Build result
	var sb strings.Builder
	sb.WriteString("[")
	for i, v := range values {
		if i > 0 {
			sb.WriteString(",")
		}
		sb.WriteString(v)
	}
	sb.WriteString("]")
	return sb.String()
}

func jsonObjectRemove(obj string, key string) string {
	obj = strings.TrimSpace(obj)
	if obj == "" || obj == "{}" {
		return obj
	}
	// Parse and rebuild without the key
	inner := obj[1 : len(obj)-1]
	result := []string{}
	depth := 0
	inString := false
	entryStart := 0
	currentKey := ""
	keyStart := -1
	for i := 0; i <= len(inner); i++ {
		c := byte(0)
		if i < len(inner) {
			c = inner[i]
		}
		if c == '"' && (i == 0 || inner[i-1] != '\\') {
			if !inString && depth == 0 && keyStart < 0 {
				keyStart = i + 1
			} else if inString && depth == 0 && keyStart >= 0 {
				currentKey = inner[keyStart:i]
				keyStart = -1
			}
			inString = !inString
		} else if !inString {
			if c == '{' || c == '[' {
				depth++
			} else if c == '}' || c == ']' {
				depth--
			} else if (c == ',' || i == len(inner)) && depth == 0 {
				entry := strings.TrimSpace(inner[entryStart:i])
				if currentKey != key && entry != "" {
					result = append(result, entry)
				}
				entryStart = i + 1
				currentKey = ""
			}
		}
	}
	return "{" + strings.Join(result, ",") + "}"
}

func jsonObjectLen(obj string) int {
	obj = strings.TrimSpace(obj)
	if obj == "" || obj == "{}" {
		return 0
	}
	// Count top-level key-value pairs
	inner := obj[1 : len(obj)-1]
	if strings.TrimSpace(inner) == "" {
		return 0
	}
	count := 0
	depth := 0
	inString := false
	for i := 0; i < len(inner); i++ {
		c := inner[i]
		if c == '"' && (i == 0 || inner[i-1] != '\\') {
			inString = !inString
		} else if !inString {
			if c == '{' || c == '[' {
				depth++
			} else if c == '}' || c == ']' {
				depth--
			} else if c == ':' && depth == 0 {
				count++
			}
		}
	}
	return count
}
