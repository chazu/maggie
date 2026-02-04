package vm

import (
	"encoding/binary"
	"fmt"
	"strings"
)

// AOTCompiler translates bytecode methods to Go source code.
// The generated code eliminates interpreter dispatch overhead by
// directly performing operations instead of interpreting bytecode.
type AOTCompiler struct {
	sb           strings.Builder
	indent       int
	stackDepth   int
	maxStack     int
	labelCounter int
	method       *CompiledMethod
	literals     []Value
	selectors    *SelectorTable
	symbols      *SymbolTable
}

// NewAOTCompiler creates a new AOT compiler.
func NewAOTCompiler(selectors *SelectorTable, symbols *SymbolTable) *AOTCompiler {
	return &AOTCompiler{
		selectors: selectors,
		symbols:   symbols,
	}
}

// CompileMethod generates Go code for a compiled method.
func (c *AOTCompiler) CompileMethod(method *CompiledMethod, className, methodName string) string {
	c.sb.Reset()
	c.indent = 0
	c.stackDepth = 0
	c.maxStack = 0
	c.labelCounter = 0
	c.method = method
	c.literals = method.Literals

	funcName := c.sanitizeName(className) + "_" + c.sanitizeName(methodName)

	// Function signature
	c.writeLine("// AOT-compiled: %s>>%s", className, methodName)
	c.writeLine("func aot_%s(vm *VM, self Value, args []Value) Value {", funcName)
	c.indent++

	// Declare temps
	numTemps := method.NumTemps
	if numTemps > 0 {
		c.writeLine("// Temporaries (including arguments)")
		c.writeLine("temps := make([]Value, %d)", numTemps)
		// Copy args into temps
		c.writeLine("copy(temps, args)")
	}

	// Declare stack
	c.writeLine("// Execution stack")
	c.writeLine("var stack [64]Value")
	c.writeLine("sp := 0")
	c.writeLine("")

	// Generate code for bytecode
	c.compileBytecode(method.Bytecode, false)

	// Default return (shouldn't reach here)
	c.writeLine("return Nil")

	c.indent--
	c.writeLine("}")

	return c.sb.String()
}

// AOTBlock is the signature for AOT-compiled blocks.
// The homeReturn function can be called to perform a non-local return.
type AOTBlock func(vm *VM, self Value, args []Value, captures []Value, homeReturn func(Value)) Value

// CompileBlock generates Go code for a compiled block.
// Blocks differ from methods in several ways:
// - They receive captures (closed-over variables) as a parameter
// - They can perform non-local returns to exit the home method
// - They may access temps from their outer scope via homeTemps
func (c *AOTCompiler) CompileBlock(block *BlockMethod, className, methodName string, blockIndex int) string {
	c.sb.Reset()
	c.indent = 0
	c.stackDepth = 0
	c.maxStack = 0
	c.labelCounter = 0
	c.literals = block.Literals

	funcName := c.sanitizeName(className) + "_" + c.sanitizeName(methodName) + fmt.Sprintf("_block%d", blockIndex)

	// Function signature - blocks take captures and homeReturn callback
	c.writeLine("// AOT-compiled block: %s>>%s[%d]", className, methodName, blockIndex)
	c.writeLine("func aot_%s(vm *VM, self Value, args []Value, captures []Value, homeReturn func(Value)) Value {", funcName)
	c.indent++

	// Declare temps for the block's own temporaries
	numTemps := block.NumTemps
	if numTemps > 0 {
		c.writeLine("// Block temporaries (including block arguments)")
		c.writeLine("temps := make([]Value, %d)", numTemps)
		// Copy block args into temps
		c.writeLine("copy(temps, args)")
	}

	// Declare stack
	c.writeLine("// Execution stack")
	c.writeLine("var stack [64]Value")
	c.writeLine("sp := 0")
	c.writeLine("")

	// Silence unused variable warnings if no captures
	if block.NumCaptures == 0 {
		c.writeLine("_ = captures // may be unused")
	}
	c.writeLine("_ = homeReturn // may be unused")
	c.writeLine("")

	// Generate code for bytecode (with block context)
	c.compileBytecode(block.Bytecode, true)

	// Default return (shouldn't reach here)
	c.writeLine("return Nil")

	c.indent--
	c.writeLine("}")

	return c.sb.String()
}

// compileBytecode generates Go code for a bytecode sequence.
// If isBlock is true, OpBlockReturn will invoke the homeReturn callback for non-local returns.
func (c *AOTCompiler) compileBytecode(bc []byte, isBlock bool) {
	// First pass: find jump targets
	jumpTargets := c.findJumpTargets(bc)

	// Second pass: generate code
	pos := 0
	for pos < len(bc) {
		// Emit label if this is a jump target
		if _, isTarget := jumpTargets[pos]; isTarget {
			c.indent--
			c.writeLine("L%d:", pos)
			c.indent++
		}

		op := Opcode(bc[pos])
		pos++

		switch op {
		case OpNOP:
			// No operation

		case OpPOP:
			c.writeLine("sp--")

		case OpDUP:
			c.writeLine("stack[sp] = stack[sp-1]")
			c.writeLine("sp++")

		case OpPushNil:
			c.writeLine("stack[sp] = Nil")
			c.writeLine("sp++")

		case OpPushTrue:
			c.writeLine("stack[sp] = True")
			c.writeLine("sp++")

		case OpPushFalse:
			c.writeLine("stack[sp] = False")
			c.writeLine("sp++")

		case OpPushSelf:
			c.writeLine("stack[sp] = self")
			c.writeLine("sp++")

		case OpPushInt8:
			val := int8(bc[pos])
			pos++
			c.writeLine("stack[sp] = FromSmallInt(%d)", val)
			c.writeLine("sp++")

		case OpPushInt32:
			val := int32(binary.LittleEndian.Uint32(bc[pos:]))
			pos += 4
			c.writeLine("stack[sp] = FromSmallInt(%d)", val)
			c.writeLine("sp++")

		case OpPushLiteral:
			idx := binary.LittleEndian.Uint16(bc[pos:])
			pos += 2
			c.writeLine("stack[sp] = literals[%d]", idx)
			c.writeLine("sp++")

		case OpPushFloat:
			bits := binary.LittleEndian.Uint64(bc[pos:])
			pos += 8
			c.writeLine("stack[sp] = FromFloat64(math.Float64frombits(0x%X))", bits)
			c.writeLine("sp++")

		case OpPushContext:
			// Create a minimal context for AOT-compiled code
			// Note: This context won't have a sender chain since we're outside the interpreter
			if isBlock {
				c.writeLine("{ // PUSH_CONTEXT (block)")
				c.writeLine("  ctx := &ContextValue{")
				c.writeLine("    Receiver: self,")
				c.writeLine("    Args:     args,")
				c.writeLine("    Captures: captures,")
				c.writeLine("    SenderID: -1,")
				c.writeLine("    HomeID:   -1,")
				c.writeLine("  }")
				c.writeLine("  stack[sp] = RegisterContext(ctx)")
				c.writeLine("  sp++")
				c.writeLine("}")
			} else {
				c.writeLine("{ // PUSH_CONTEXT (method)")
				c.writeLine("  ctx := &ContextValue{")
				c.writeLine("    Receiver: self,")
				c.writeLine("    Args:     args,")
				c.writeLine("    SenderID: -1,")
				c.writeLine("    HomeID:   -1,")
				c.writeLine("  }")
				c.writeLine("  stack[sp] = RegisterContext(ctx)")
				c.writeLine("  sp++")
				c.writeLine("}")
			}

		case OpPushTemp:
			idx := bc[pos]
			pos++
			c.writeLine("stack[sp] = temps[%d]", idx)
			c.writeLine("sp++")

		case OpStoreTemp:
			idx := bc[pos]
			pos++
			c.writeLine("temps[%d] = stack[sp-1]", idx)

		case OpPushIvar:
			idx := bc[pos]
			pos++
			c.writeLine("stack[sp] = ObjectFromValue(self).GetSlot(%d)", idx)
			c.writeLine("sp++")

		case OpStoreIvar:
			idx := bc[pos]
			pos++
			c.writeLine("ObjectFromValue(self).SetSlot(%d, stack[sp-1])", idx)

		case OpPushGlobal:
			idx := binary.LittleEndian.Uint16(bc[pos:])
			pos += 2
			c.writeLine("stack[sp] = vm.Globals[vm.Symbols.Name(literals[%d].SymbolID())]", idx)
			c.writeLine("sp++")

		case OpStoreGlobal:
			idx := binary.LittleEndian.Uint16(bc[pos:])
			pos += 2
			c.writeLine("vm.Globals[vm.Symbols.Name(literals[%d].SymbolID())] = stack[sp-1]", idx)

		case OpPushCaptured:
			idx := bc[pos]
			pos++
			c.writeLine("stack[sp] = captures[%d]", idx)
			c.writeLine("sp++")

		case OpStoreCaptured:
			idx := bc[pos]
			pos++
			c.writeLine("captures[%d] = stack[sp-1]", idx)

		case OpPushHomeTemp:
			idx := bc[pos]
			pos++
			c.writeLine("stack[sp] = homeTemps[%d]", idx)
			c.writeLine("sp++")

		case OpStoreHomeTemp:
			idx := bc[pos]
			pos++
			c.writeLine("homeTemps[%d] = stack[sp-1]", idx)

		case OpMakeCell:
			c.writeLine("stack[sp-1] = NewCell(vm.registry, stack[sp-1])")

		case OpCellGet:
			c.writeLine("if stack[sp-1].IsCell() { stack[sp-1] = stack[sp-1].CellGet() }")

		case OpCellSet:
			c.writeLine("{ val := stack[sp-1]; sp--; cell := stack[sp-1]; sp--")
			c.writeLine("  if cell.IsCell() { cell.CellSet(val) }")
			c.writeLine("  stack[sp] = val; sp++ }")

		case OpPushClassVar:
			idx := binary.LittleEndian.Uint16(bc[pos:])
			pos += 2
			// Get class name at compile time from the method
			className := ""
			if c.method != nil && c.method.Class() != nil {
				className = c.method.Class().Name
			}
			c.writeLine("stack[sp] = vm.Classes.Lookup(%q).GetClassVar(vm.registry, vm.Symbols.Name(literals[%d].SymbolID()))", className, idx)
			c.writeLine("sp++")

		case OpStoreClassVar:
			idx := binary.LittleEndian.Uint16(bc[pos:])
			pos += 2
			// Get class name at compile time from the method
			className := ""
			if c.method != nil && c.method.Class() != nil {
				className = c.method.Class().Name
			}
			c.writeLine("vm.Classes.Lookup(%q).SetClassVar(vm.registry, vm.Symbols.Name(literals[%d].SymbolID()), stack[sp-1])", className, idx)

		case OpSend:
			selIdx := binary.LittleEndian.Uint16(bc[pos:])
			pos += 2
			argc := int(bc[pos])
			pos++
			c.writeLine("{ // SEND")
			c.writeLine("  args := make([]Value, %d)", argc)
			c.writeLine("  for i := %d - 1; i >= 0; i-- { sp--; args[i] = stack[sp] }", argc)
			c.writeLine("  sp--; recv := stack[sp]")
			c.writeLine("  stack[sp] = vm.Send(recv, vm.Selectors.Name(%d), args)", selIdx)
			c.writeLine("  sp++")
			c.writeLine("}")

		case OpSendSuper:
			selIdx := binary.LittleEndian.Uint16(bc[pos:])
			pos += 2
			argc := int(bc[pos])
			pos++
			// Get the defining class name at compile time
			definingClassName := ""
			if c.method != nil && c.method.Class() != nil {
				definingClassName = c.method.Class().Name
			}
			c.writeLine("{ // SEND_SUPER (defining class: %s)", definingClassName)
			c.writeLine("  args := make([]Value, %d)", argc)
			c.writeLine("  for i := %d - 1; i >= 0; i-- { sp--; args[i] = stack[sp] }", argc)
			c.writeLine("  sp-- // pop self")
			c.writeLine("  stack[sp] = vm.SendSuper(self, vm.Selectors.Name(%d), args, %q)", selIdx, definingClassName)
			c.writeLine("  sp++")
			c.writeLine("}")

		case OpSendPlus:
			c.emitBinaryOp("+")

		case OpSendMinus:
			c.emitBinaryOp("-")

		case OpSendTimes:
			c.emitBinaryOp("*")

		case OpSendDiv:
			c.emitBinaryOp("/")

		case OpSendMod:
			c.emitBinaryOp("%%")

		case OpSendLT:
			c.emitCompareOp("<")

		case OpSendGT:
			c.emitCompareOp(">")

		case OpSendLE:
			c.emitCompareOp("<=")

		case OpSendGE:
			c.emitCompareOp(">=")

		case OpSendEQ:
			c.writeLine("{ a := stack[sp-2]; b := stack[sp-1]; sp--")
			c.writeLine("  stack[sp-1] = FromBool(a == b) }")

		case OpSendNE:
			c.writeLine("{ a := stack[sp-2]; b := stack[sp-1]; sp--")
			c.writeLine("  stack[sp-1] = FromBool(a != b) }")

		case OpSendSize:
			c.writeLine("{ recv := stack[sp-1]")
			c.writeLine("  stack[sp-1] = vm.Send(recv, \"size\", nil) }")

		case OpSendValue:
			c.writeLine("{ block := stack[sp-1]")
			c.writeLine("  stack[sp-1] = vm.Send(block, \"value\", nil) }")

		case OpSendValue1:
			c.writeLine("{ arg := stack[sp-1]; sp--; block := stack[sp-1]")
			c.writeLine("  stack[sp-1] = vm.Send(block, \"value:\", []Value{arg}) }")

		case OpSendValue2:
			c.writeLine("{ arg2 := stack[sp-1]; sp--; arg1 := stack[sp-1]; sp--; block := stack[sp-1]")
			c.writeLine("  stack[sp-1] = vm.Send(block, \"value:value:\", []Value{arg1, arg2}) }")

		case OpSendNew:
			c.writeLine("{ class := stack[sp-1]")
			c.writeLine("  stack[sp-1] = vm.Send(class, \"new\", nil) }")

		case OpSendClass:
			c.writeLine("{ recv := stack[sp-1]")
			c.writeLine("  stack[sp-1] = vm.Send(recv, \"class\", nil) }")

		case OpSendAt:
			c.writeLine("{ idx := stack[sp-1]; sp--; recv := stack[sp-1]")
			c.writeLine("  stack[sp-1] = vm.Send(recv, \"at:\", []Value{idx}) }")

		case OpSendAtPut:
			c.writeLine("{ val := stack[sp-1]; sp--; idx := stack[sp-1]; sp--; recv := stack[sp-1]")
			c.writeLine("  stack[sp-1] = vm.Send(recv, \"at:put:\", []Value{idx, val}) }")

		case OpJump:
			offset := int16(binary.LittleEndian.Uint16(bc[pos:]))
			pos += 2
			target := pos + int(offset)
			c.writeLine("goto L%d", target)

		case OpJumpTrue:
			offset := int16(binary.LittleEndian.Uint16(bc[pos:]))
			pos += 2
			target := pos + int(offset)
			c.writeLine("sp--; if stack[sp].IsTruthy() { goto L%d }", target)

		case OpJumpFalse:
			offset := int16(binary.LittleEndian.Uint16(bc[pos:]))
			pos += 2
			target := pos + int(offset)
			c.writeLine("sp--; if stack[sp].IsFalsy() { goto L%d }", target)

		case OpJumpNil:
			offset := int16(binary.LittleEndian.Uint16(bc[pos:]))
			pos += 2
			target := pos + int(offset)
			c.writeLine("sp--; if stack[sp] == Nil { goto L%d }", target)

		case OpJumpNotNil:
			offset := int16(binary.LittleEndian.Uint16(bc[pos:]))
			pos += 2
			target := pos + int(offset)
			c.writeLine("sp--; if stack[sp] != Nil { goto L%d }", target)

		case OpReturnTop:
			c.writeLine("return stack[sp-1]")

		case OpReturnSelf:
			c.writeLine("return self")

		case OpReturnNil:
			c.writeLine("return Nil")

		case OpBlockReturn:
			if isBlock {
				// Non-local return: call homeReturn to exit the enclosing method
				c.writeLine("homeReturn(stack[sp-1]) // non-local return")
				c.writeLine("return Nil // unreachable after homeReturn")
			} else {
				// In a method context, just return normally
				c.writeLine("return stack[sp-1]")
			}

		case OpCreateBlock:
			methodIdx := binary.LittleEndian.Uint16(bc[pos:])
			pos += 2
			nCaptures := bc[pos]
			pos++
			// Get defining class and method name at compile time for runtime lookup
			defClassName := ""
			defMethodName := ""
			if c.method != nil {
				defMethodName = c.method.Name()
				if c.method.Class() != nil {
					defClassName = c.method.Class().Name
				}
			}
			c.writeLine("{ // CREATE_BLOCK (method index %d)", methodIdx)
			c.writeLine("  blockCaptures := make([]Value, %d)", nCaptures)
			c.writeLine("  for i := %d - 1; i >= 0; i-- { sp--; blockCaptures[i] = stack[sp] }", nCaptures)
			c.writeLine("  // Look up the block from the defining method's bytecode method at runtime")
			c.writeLine("  if defClass := vm.LookupClass(%q); defClass != nil {", defClassName)
			c.writeLine("    if cm := defClass.MethodNamed(%q); cm != nil && %d < len(cm.Blocks) {", defMethodName, methodIdx)
			c.writeLine("      stack[sp] = vm.CreateBlock(cm.Blocks[%d], blockCaptures, self)", methodIdx)
			c.writeLine("    } else {")
			c.writeLine("      stack[sp] = Nil")
			c.writeLine("    }")
			c.writeLine("  } else {")
			c.writeLine("    stack[sp] = Nil")
			c.writeLine("  }")
			c.writeLine("  sp++")
			c.writeLine("}")

		case OpCreateArray:
			size := bc[pos]
			pos++
			c.writeLine("{ // CREATE_ARRAY")
			c.writeLine("  arr := vm.NewArray(%d)", size)
			c.writeLine("  for i := %d - 1; i >= 0; i-- { sp--; vm.ArrayPut(arr, i, stack[sp]) }", size)
			c.writeLine("  stack[sp] = arr")
			c.writeLine("  sp++")
			c.writeLine("}")

		case OpCaptureTemp:
			idx := bc[pos]
			pos++
			c.writeLine("// CAPTURE_TEMP %d - handled during block creation", idx)

		case OpCaptureIvar:
			idx := bc[pos]
			pos++
			c.writeLine("// CAPTURE_IVAR %d - handled during block creation", idx)

		case OpCreateObject:
			classIdx := binary.LittleEndian.Uint16(bc[pos:])
			pos += 2
			nSlots := bc[pos]
			pos++
			c.writeLine("{ // CREATE_OBJECT")
			c.writeLine("  class := vm.GetClassByIndex(%d)", classIdx)
			c.writeLine("  obj := vm.NewObject(class, %d)", nSlots)
			c.writeLine("  for i := %d - 1; i >= 0; i-- { sp--; obj.SetSlot(i, stack[sp]) }", nSlots)
			c.writeLine("  stack[sp] = obj.AsValue()")
			c.writeLine("  sp++")
			c.writeLine("}")

		default:
			c.writeLine("// Unknown opcode: 0x%02X", op)
		}
	}
}

// findJumpTargets returns a set of bytecode positions that are jump targets.
func (c *AOTCompiler) findJumpTargets(bc []byte) map[int]bool {
	targets := make(map[int]bool)
	pos := 0
	for pos < len(bc) {
		op := Opcode(bc[pos])
		pos++
		info := op.Info()

		switch op {
		case OpJump, OpJumpTrue, OpJumpFalse, OpJumpNil, OpJumpNotNil:
			offset := int16(binary.LittleEndian.Uint16(bc[pos:]))
			pos += 2
			target := pos + int(offset)
			targets[target] = true
		default:
			pos += info.OperandBytes
		}
	}
	return targets
}

// emitBinaryOp generates code for a binary arithmetic operation.
func (c *AOTCompiler) emitBinaryOp(op string) {
	c.writeLine("{ a := stack[sp-2]; b := stack[sp-1]; sp--")
	c.writeLine("  if a.IsSmallInt() && b.IsSmallInt() {")
	c.writeLine("    stack[sp-1] = FromSmallInt(a.SmallInt() %s b.SmallInt())", op)
	c.writeLine("  } else {")
	c.writeLine("    stack[sp-1] = vm.Send(a, \"%s\", []Value{b})", op)
	c.writeLine("  } }")
}

// emitCompareOp generates code for a comparison operation.
func (c *AOTCompiler) emitCompareOp(op string) {
	c.writeLine("{ a := stack[sp-2]; b := stack[sp-1]; sp--")
	c.writeLine("  if a.IsSmallInt() && b.IsSmallInt() {")
	c.writeLine("    stack[sp-1] = FromBool(a.SmallInt() %s b.SmallInt())", op)
	c.writeLine("  } else {")
	c.writeLine("    stack[sp-1] = vm.Send(a, \"%s\", []Value{b})", op)
	c.writeLine("  } }")
}

// writeLine writes an indented line to the output.
func (c *AOTCompiler) writeLine(format string, args ...interface{}) {
	for i := 0; i < c.indent; i++ {
		c.sb.WriteString("\t")
	}
	c.sb.WriteString(fmt.Sprintf(format, args...))
	c.sb.WriteString("\n")
}

// sanitizeName converts a Smalltalk name to a valid Go identifier.
func (c *AOTCompiler) sanitizeName(name string) string {
	result := strings.Builder{}
	for _, ch := range name {
		switch ch {
		case ':':
			result.WriteString("_")
		case '+':
			result.WriteString("Plus")
		case '-':
			result.WriteString("Minus")
		case '*':
			result.WriteString("Star")
		case '/':
			result.WriteString("Slash")
		case '<':
			result.WriteString("LT")
		case '>':
			result.WriteString("GT")
		case '=':
			result.WriteString("EQ")
		case '~':
			result.WriteString("Tilde")
		default:
			if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') || ch == '_' {
				result.WriteRune(ch)
			}
		}
	}
	return result.String()
}

// CompileAllMethods generates Go code for all methods in a class.
func (c *AOTCompiler) CompileAllMethods(class *Class) string {
	var result strings.Builder

	result.WriteString(fmt.Sprintf("// AOT-compiled methods for class %s\n", class.Name))
	result.WriteString("// Generated by Maggie AOT compiler\n\n")
	result.WriteString("package main\n\n")
	result.WriteString("import (\n")
	result.WriteString("\t\"math\"\n")
	result.WriteString("\t. \"github.com/chazu/maggie/vm\"\n")
	result.WriteString(")\n\n")

	// Compile each method
	for selID, method := range class.VTable.LocalMethods() {
		if cm, ok := method.(*CompiledMethod); ok {
			selName := c.selectors.Name(selID)
			result.WriteString(c.CompileMethod(cm, class.Name, selName))
			result.WriteString("\n")
		}
	}

	return result.String()
}

// AOTMethod is the signature for AOT-compiled methods.
type AOTMethod func(vm *VM, self Value, args []Value) Value

// AOTDispatchKey uniquely identifies a method.
type AOTDispatchKey struct {
	ClassName  string
	MethodName string
}

// AOTDispatchTable maps (class, method) pairs to AOT-compiled functions.
type AOTDispatchTable map[AOTDispatchKey]AOTMethod

// GenerateDispatchTable generates Go code that creates and populates
// a dispatch table with all AOT-compiled methods.
func (c *AOTCompiler) GenerateDispatchTable(classes []*Class) string {
	var result strings.Builder

	result.WriteString("// AOT dispatch table initialization\n")
	result.WriteString("// Generated by Maggie AOT compiler\n\n")

	result.WriteString("func initAOTDispatchTable() AOTDispatchTable {\n")
	result.WriteString("\ttable := make(AOTDispatchTable)\n\n")

	for _, class := range classes {
		for selID, method := range class.VTable.LocalMethods() {
			if _, ok := method.(*CompiledMethod); ok {
				selName := c.selectors.Name(selID)
				funcName := c.sanitizeName(class.Name) + "_" + c.sanitizeName(selName)
				result.WriteString(fmt.Sprintf("\ttable[AOTDispatchKey{%q, %q}] = aot_%s\n",
					class.Name, selName, funcName))
			}
		}
	}

	result.WriteString("\n\treturn table\n")
	result.WriteString("}\n")

	return result.String()
}

// CompileModule generates a complete Go file with all AOT-compiled methods
// for a set of classes, plus the dispatch table.
func (c *AOTCompiler) CompileModule(packageName string, classes []*Class) string {
	var result strings.Builder

	result.WriteString("// AOT-compiled Maggie module\n")
	result.WriteString("// Generated by Maggie AOT compiler\n")
	result.WriteString("// DO NOT EDIT - this file is auto-generated\n\n")
	result.WriteString(fmt.Sprintf("package %s\n\n", packageName))
	result.WriteString("import (\n")
	result.WriteString("\t\"math\"\n")
	result.WriteString("\t. \"github.com/chazu/maggie/vm\"\n")
	result.WriteString(")\n\n")
	result.WriteString("var _ = math.Float64frombits // silence unused import\n\n")

	// Generate all methods
	for _, class := range classes {
		result.WriteString(fmt.Sprintf("// --- Class: %s ---\n\n", class.Name))
		for selID, method := range class.VTable.LocalMethods() {
			if cm, ok := method.(*CompiledMethod); ok {
				selName := c.selectors.Name(selID)
				result.WriteString(c.CompileMethod(cm, class.Name, selName))
				result.WriteString("\n")
			}
		}
	}

	// Generate dispatch table
	result.WriteString(c.GenerateDispatchTable(classes))

	return result.String()
}
