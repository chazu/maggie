package compiler

import (
	"fmt"

	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// Codegen: Compile AST to bytecode
// ---------------------------------------------------------------------------

// Compiler compiles AST nodes to bytecode.
type Compiler struct {
	selectors *vm.SelectorTable
	symbols   *vm.SymbolTable

	// Current compilation context
	builder     *vm.BytecodeBuilder
	literals    []vm.Value
	literalMap  map[interface{}]int // dedup literals
	temps       map[string]int      // temp name -> slot index
	args        map[string]int      // arg name -> slot index
	instVars    map[string]int      // instance variable name -> slot index
	numArgs     int
	numTemps    int
	blocks      []*vm.BlockMethod
	errors      []string

	// For block compilation - track outer scope temps for home temp access
	outerTemps map[string]int // outer method temp name -> slot index
	outerArgs  map[string]int // outer method arg name -> slot index
	inBlock    bool           // true when compiling a block

	// For nested block variable capture
	// Variables from enclosing BLOCKS (not the method) need to be captured
	enclosingBlockVars map[string]int // vars from enclosing blocks -> their slot index in enclosing scope
	capturedVars       map[string]int // captured var name -> capture index (for OpPushCaptured)
	blockNestingDepth  int            // 0 = method, 1 = first-level block, 2+ = nested blocks
}

// NewCompiler creates a new compiler.
func NewCompiler(selectors *vm.SelectorTable, symbols *vm.SymbolTable) *Compiler {
	return &Compiler{
		selectors: selectors,
		symbols:   symbols,
	}
}

// Errors returns accumulated compilation errors.
func (c *Compiler) Errors() []string {
	return c.errors
}

// SetInstanceVars sets the instance variable names for compilation.
// Must be called before CompileMethod if the method accesses instance variables.
func (c *Compiler) SetInstanceVars(names []string) {
	c.instVars = make(map[string]int)
	for i, name := range names {
		c.instVars[name] = i
	}
}

// errorf records a compilation error without position information.
func (c *Compiler) errorf(format string, args ...interface{}) {
	c.errors = append(c.errors, fmt.Sprintf(format, args...))
}

// errorAt records a compilation error with position information from a node.
func (c *Compiler) errorAt(node Node, format string, args ...interface{}) {
	pos := node.Span().Start
	msg := fmt.Sprintf("line %d, column %d: %s", pos.Line, pos.Column, fmt.Sprintf(format, args...))
	c.errors = append(c.errors, msg)
}

// CompileMethod compiles a method definition to a CompiledMethod.
func (c *Compiler) CompileMethod(method *MethodDef) *vm.CompiledMethod {
	c.builder = vm.NewBytecodeBuilder()
	c.literals = nil
	c.literalMap = make(map[interface{}]int)
	c.blocks = nil
	c.temps = make(map[string]int)
	c.args = make(map[string]int)
	c.numArgs = len(method.Parameters)
	c.numTemps = c.numArgs + len(method.Temps) // Total temps = args + locals

	// Set up argument slots
	for i, param := range method.Parameters {
		c.args[param] = i
	}

	// Set up temp slots (after args)
	for i, temp := range method.Temps {
		c.temps[temp] = c.numArgs + i
	}

	// Compile statements
	c.compileStatements(method.Statements)

	// If no explicit return, return self
	if len(method.Statements) == 0 || !c.endsWithReturn(method.Statements) {
		c.builder.Emit(vm.OpPushSelf)
		c.builder.Emit(vm.OpReturnTop)
	}

	// Build the method
	b := vm.NewCompiledMethodBuilder(method.Selector, c.numArgs)
	b.SetNumTemps(c.numTemps)

	// Copy bytecode
	for _, code := range c.builder.Bytes() {
		b.Bytecode().EmitRaw(code)
	}

	// Add literals
	for _, lit := range c.literals {
		b.AddLiteral(lit)
	}

	// Add blocks
	for _, block := range c.blocks {
		b.AddBlock(block)
	}

	return b.Build()
}

// CompileExpression compiles an expression for REPL/eval.
func (c *Compiler) CompileExpression(expr Expr) *vm.CompiledMethod {
	c.builder = vm.NewBytecodeBuilder()
	c.literals = nil
	c.literalMap = make(map[interface{}]int)
	c.blocks = nil
	c.temps = make(map[string]int)
	c.args = make(map[string]int)
	c.numArgs = 0
	c.numTemps = 0

	c.compileExpr(expr)
	c.builder.Emit(vm.OpReturnTop)

	b := vm.NewCompiledMethodBuilder("doIt", 0)
	for _, code := range c.builder.Bytes() {
		b.Bytecode().EmitRaw(code)
	}
	for _, lit := range c.literals {
		b.AddLiteral(lit)
	}
	for _, block := range c.blocks {
		b.AddBlock(block)
	}

	return b.Build()
}

// endsWithReturn checks if statement list ends with a return.
func (c *Compiler) endsWithReturn(stmts []Stmt) bool {
	if len(stmts) == 0 {
		return false
	}
	_, ok := stmts[len(stmts)-1].(*Return)
	return ok
}

// ---------------------------------------------------------------------------
// Statement compilation
// ---------------------------------------------------------------------------

func (c *Compiler) compileStatements(stmts []Stmt) {
	for i, stmt := range stmts {
		c.compileStmt(stmt)
		// Pop result of expression statements (except last)
		if _, ok := stmt.(*ExprStmt); ok && i < len(stmts)-1 {
			c.builder.Emit(vm.OpPOP)
		}
	}
}

func (c *Compiler) compileStmt(stmt Stmt) {
	switch s := stmt.(type) {
	case *ExprStmt:
		c.compileExpr(s.Expr)
	case *Return:
		c.compileExpr(s.Value)
		c.builder.Emit(vm.OpReturnTop)
	default:
		c.errorf("unknown statement type: %T", stmt)
	}
}

// ---------------------------------------------------------------------------
// Expression compilation
// ---------------------------------------------------------------------------

func (c *Compiler) compileExpr(expr Expr) {
	switch e := expr.(type) {
	case *IntLiteral:
		c.compileInt(e.Value)
	case *FloatLiteral:
		c.compileFloat(e.Value)
	case *StringLiteral:
		c.compileString(e.Value)
	case *SymbolLiteral:
		c.compileSymbol(e.Value)
	case *CharLiteral:
		c.compileChar(e.Value)
	case *ArrayLiteral:
		c.compileArrayLiteral(e)
	case *DynamicArray:
		c.compileDynamicArray(e)
	case *Variable:
		c.compileVariable(e.Name)
	case *Assignment:
		c.compileAssignment(e)
	case *Self:
		c.builder.Emit(vm.OpPushSelf)
	case *Super:
		c.builder.Emit(vm.OpPushSelf) // Super uses self but dispatches to superclass
	case *ThisContext:
		c.builder.Emit(vm.OpPushContext)
	case *NilLiteral:
		c.builder.Emit(vm.OpPushNil)
	case *TrueLiteral:
		c.builder.Emit(vm.OpPushTrue)
	case *FalseLiteral:
		c.builder.Emit(vm.OpPushFalse)
	case *UnaryMessage:
		c.compileUnaryMessage(e)
	case *BinaryMessage:
		c.compileBinaryMessage(e)
	case *KeywordMessage:
		c.compileKeywordMessage(e)
	case *Cascade:
		c.compileCascade(e)
	case *Block:
		c.compileBlock(e)
	default:
		c.errorf("unknown expression type: %T", expr)
	}
}

// ---------------------------------------------------------------------------
// Literal compilation
// ---------------------------------------------------------------------------

func (c *Compiler) compileInt(value int64) {
	// Use OpPushInt8 for small integers
	if value >= -128 && value <= 127 {
		c.builder.EmitInt8(vm.OpPushInt8, int8(value))
	} else {
		// Use literal table
		idx := c.addLiteral(vm.FromSmallInt(value))
		c.builder.EmitUint16(vm.OpPushLiteral, uint16(idx))
	}
}

func (c *Compiler) compileFloat(value float64) {
	// Could use OpPushFloat for inline, but use literal for now
	idx := c.addLiteral(vm.FromFloat64(value))
	c.builder.EmitUint16(vm.OpPushLiteral, uint16(idx))
}

func (c *Compiler) compileString(value string) {
	// Create an actual string value in the VM's string registry
	idx := c.addLiteral(vm.NewStringValue(value))
	c.builder.EmitUint16(vm.OpPushLiteral, uint16(idx))
}

func (c *Compiler) compileSymbol(value string) {
	idx := c.addLiteral(c.symbols.SymbolValue(value))
	c.builder.EmitUint16(vm.OpPushLiteral, uint16(idx))
}

func (c *Compiler) compileChar(value rune) {
	// Characters in Maggie are single-character strings
	c.compileString(string(value))
}

func (c *Compiler) compileArrayLiteral(arr *ArrayLiteral) {
	// Push elements and create array
	for _, elem := range arr.Elements {
		c.compileExpr(elem)
	}
	c.builder.EmitByte(vm.OpCreateArray, byte(len(arr.Elements)))
}

func (c *Compiler) compileDynamicArray(arr *DynamicArray) {
	for _, elem := range arr.Elements {
		c.compileExpr(elem)
	}
	c.builder.EmitByte(vm.OpCreateArray, byte(len(arr.Elements)))
}

// addLiteral adds a literal to the literal table, returning its index.
func (c *Compiler) addLiteral(value vm.Value) int {
	// Check for duplicate
	key := uint64(value)
	if idx, ok := c.literalMap[key]; ok {
		return idx
	}

	idx := len(c.literals)
	c.literals = append(c.literals, value)
	c.literalMap[key] = idx
	return idx
}

// ---------------------------------------------------------------------------
// Variable compilation
// ---------------------------------------------------------------------------

func (c *Compiler) compileVariable(name string) {
	// Check if it's an argument
	if idx, ok := c.args[name]; ok {
		c.builder.EmitByte(vm.OpPushTemp, byte(idx))
		return
	}
	// Check if it's a temp
	if idx, ok := c.temps[name]; ok {
		c.builder.EmitByte(vm.OpPushTemp, byte(idx))
		return
	}
	// Check if it's an instance variable
	if idx, ok := c.instVars[name]; ok {
		c.builder.EmitByte(vm.OpPushIvar, byte(idx))
		return
	}
	// In a block: check captured variables first (for nested blocks)
	if c.inBlock && c.capturedVars != nil {
		if idx, ok := c.capturedVars[name]; ok {
			c.builder.EmitByte(vm.OpPushCaptured, byte(idx))
			return
		}
	}
	// In a block: check outer METHOD scope temps/args (use home frame access)
	// Only do this for first-level blocks (blockNestingDepth == 1)
	// For nested blocks, we use captures instead
	if c.inBlock && c.blockNestingDepth == 1 {
		if idx, ok := c.outerTemps[name]; ok {
			c.builder.EmitByte(vm.OpPushHomeTemp, byte(idx))
			return
		}
		if idx, ok := c.outerArgs[name]; ok {
			c.builder.EmitByte(vm.OpPushHomeTemp, byte(idx))
			return
		}
	}
	// Must be a global
	idx := c.addLiteral(c.symbols.SymbolValue(name))
	c.builder.EmitUint16(vm.OpPushGlobal, uint16(idx))
}

func (c *Compiler) compileAssignment(assign *Assignment) {
	// Compile value
	c.compileExpr(assign.Value)

	name := assign.Variable

	// Check if it's a temp
	if idx, ok := c.temps[name]; ok {
		c.builder.EmitByte(vm.OpStoreTemp, byte(idx))
		c.builder.EmitByte(vm.OpPushTemp, byte(idx)) // Leave value on stack
		return
	}

	// Check if it's an arg (can't assign to args in standard Smalltalk, but allow it)
	if idx, ok := c.args[name]; ok {
		c.builder.EmitByte(vm.OpStoreTemp, byte(idx))
		c.builder.EmitByte(vm.OpPushTemp, byte(idx))
		return
	}

	// Check if it's an instance variable
	if idx, ok := c.instVars[name]; ok {
		c.builder.EmitByte(vm.OpStoreIvar, byte(idx))
		c.builder.EmitByte(vm.OpPushIvar, byte(idx)) // Leave value on stack
		return
	}

	// In a block: check outer scope temps (use home frame access)
	if c.inBlock {
		if idx, ok := c.outerTemps[name]; ok {
			c.builder.EmitByte(vm.OpStoreHomeTemp, byte(idx))
			c.builder.EmitByte(vm.OpPushHomeTemp, byte(idx)) // Leave value on stack
			return
		}
		// Note: we don't allow assigning to outer args, treat as global
	}

	// Global assignment
	idx := c.addLiteral(c.symbols.SymbolValue(name))
	c.builder.EmitUint16(vm.OpStoreGlobal, uint16(idx))
	c.builder.EmitUint16(vm.OpPushGlobal, uint16(idx))
}

// ---------------------------------------------------------------------------
// Message compilation
// ---------------------------------------------------------------------------

func (c *Compiler) compileUnaryMessage(msg *UnaryMessage) {
	// Compile receiver
	c.compileExpr(msg.Receiver)

	// Emit send - use super send if receiver is super
	if _, isSuper := msg.Receiver.(*Super); isSuper {
		c.emitSendSuper(msg.Selector, 0)
	} else {
		c.emitSend(msg.Selector, 0)
	}
}

func (c *Compiler) compileBinaryMessage(msg *BinaryMessage) {
	// Compile receiver and argument
	c.compileExpr(msg.Receiver)
	c.compileExpr(msg.Argument)

	// Check if this is a super send
	_, isSuper := msg.Receiver.(*Super)

	// Fast-path opcodes for common operators (only for non-super sends)
	if !isSuper {
		switch msg.Selector {
		case "+":
			c.builder.Emit(vm.OpSendPlus)
			return
		case "-":
			c.builder.Emit(vm.OpSendMinus)
			return
		case "*":
			c.builder.Emit(vm.OpSendTimes)
			return
		case "/":
			c.builder.Emit(vm.OpSendDiv)
			return
		case "<":
			c.builder.Emit(vm.OpSendLT)
			return
		case ">":
			c.builder.Emit(vm.OpSendGT)
			return
		case "<=":
			c.builder.Emit(vm.OpSendLE)
			return
		case ">=":
			c.builder.Emit(vm.OpSendGE)
			return
		case "=":
			c.builder.Emit(vm.OpSendEQ)
			return
		}
	}

	// Generic send
	if isSuper {
		c.emitSendSuper(msg.Selector, 1)
	} else {
		c.emitSend(msg.Selector, 1)
	}
}

func (c *Compiler) compileKeywordMessage(msg *KeywordMessage) {
	// Compile receiver
	c.compileExpr(msg.Receiver)

	// Compile arguments
	for _, arg := range msg.Arguments {
		c.compileExpr(arg)
	}

	// Emit send - use super send if receiver is super
	if _, isSuper := msg.Receiver.(*Super); isSuper {
		c.emitSendSuper(msg.Selector, len(msg.Arguments))
	} else {
		c.emitSend(msg.Selector, len(msg.Arguments))
	}
}

func (c *Compiler) emitSend(selector string, numArgs int) {
	selectorID := c.selectors.Intern(selector)
	c.builder.EmitSend(vm.OpSend, uint16(selectorID), byte(numArgs))
}

func (c *Compiler) emitSendSuper(selector string, numArgs int) {
	selectorID := c.selectors.Intern(selector)
	c.builder.EmitSend(vm.OpSendSuper, uint16(selectorID), byte(numArgs))
}

// ---------------------------------------------------------------------------
// Cascade compilation
// ---------------------------------------------------------------------------

func (c *Compiler) compileCascade(cascade *Cascade) {
	// Compile receiver once
	c.compileExpr(cascade.Receiver)

	for i, msg := range cascade.Messages {
		// Dup receiver for all but last message
		if i < len(cascade.Messages)-1 {
			c.builder.Emit(vm.OpDUP)
		}

		// Compile message arguments and send
		switch msg.Type {
		case UnaryMsg:
			c.emitSend(msg.Selector, 0)
		case BinaryMsg:
			c.compileExpr(msg.Arguments[0])
			c.emitSend(msg.Selector, 1)
		case KeywordMsg:
			for _, arg := range msg.Arguments {
				c.compileExpr(arg)
			}
			c.emitSend(msg.Selector, len(msg.Arguments))
		}

		// Pop result of non-last messages
		if i < len(cascade.Messages)-1 {
			c.builder.Emit(vm.OpPOP)
		}
	}
}

// ---------------------------------------------------------------------------
// Block compilation
// ---------------------------------------------------------------------------

func (c *Compiler) compileBlock(block *Block) {
	// Create a nested compiler for the block
	blockBuilder := vm.NewBytecodeBuilder()

	// Save current state (including literals for nested block)
	// NOTE: c.blocks is NOT saved/restored - all blocks share method's blocks list
	oldBuilder := c.builder
	oldTemps := c.temps
	oldArgs := c.args
	oldNumArgs := c.numArgs
	oldNumTemps := c.numTemps
	oldLiterals := c.literals
	oldLiteralMap := c.literalMap
	oldInBlock := c.inBlock
	oldOuterTemps := c.outerTemps
	oldOuterArgs := c.outerArgs
	oldEnclosingBlockVars := c.enclosingBlockVars
	oldCapturedVars := c.capturedVars
	oldBlockNestingDepth := c.blockNestingDepth

	// Increment nesting depth: 0=method, 1=first block, 2+=nested blocks
	newNestingDepth := oldBlockNestingDepth + 1

	// Build outer scope maps for accessing method-level variables
	// Only first-level blocks (depth=1) can use OpPushHomeTemp for method vars
	newOuterTemps := make(map[string]int)
	newOuterArgs := make(map[string]int)

	if newNestingDepth == 1 {
		// First-level block: outer vars are from the method
		for k, v := range c.temps {
			newOuterTemps[k] = v
		}
		for k, v := range c.args {
			newOuterArgs[k] = v
		}
	} else {
		// Nested block: keep method-level outer vars but don't add current block vars
		// (block vars need to be captured, not accessed via HomeBP)
		for k, v := range c.outerTemps {
			newOuterTemps[k] = v
		}
		for k, v := range c.outerArgs {
			newOuterArgs[k] = v
		}
	}

	// Build map of enclosing block variables (for nested blocks)
	// These are variables from enclosing BLOCKS that need to be captured
	newEnclosingBlockVars := make(map[string]int)
	if oldEnclosingBlockVars != nil {
		for k, v := range oldEnclosingBlockVars {
			newEnclosingBlockVars[k] = v
		}
	}
	// If we're already in a block (nesting >= 1), current scope's vars become enclosing block vars
	if oldInBlock {
		for k, v := range c.temps {
			newEnclosingBlockVars[k] = v
		}
		for k, v := range c.args {
			newEnclosingBlockVars[k] = v
		}
	}

	// Identify which variables this block needs to capture
	// This requires analyzing the block's AST to find referenced variables
	varsToCapture := c.findCapturedVariables(block, newEnclosingBlockVars)
	numCaptures := len(varsToCapture)

	// Build capturedVars map for the inner block
	newCapturedVars := make(map[string]int)
	for i, varName := range varsToCapture {
		newCapturedVars[varName] = i
	}

	// Set up block context with fresh literals pool
	// NOTE: We keep c.blocks pointing to the METHOD's blocks list
	// so that nested blocks get correct indices. Don't reset c.blocks here!
	c.builder = blockBuilder
	c.temps = make(map[string]int)
	c.args = make(map[string]int)
	c.numArgs = len(block.Parameters)
	c.numTemps = c.numArgs + len(block.Temps) // Total temps = args + locals
	c.literals = nil
	c.literalMap = make(map[interface{}]int)
	// c.blocks intentionally NOT reset - all blocks share method's blocks list
	c.inBlock = true
	c.outerTemps = newOuterTemps
	c.outerArgs = newOuterArgs
	c.enclosingBlockVars = newEnclosingBlockVars
	c.capturedVars = newCapturedVars
	c.blockNestingDepth = newNestingDepth

	// Parameters
	for i, param := range block.Parameters {
		c.args[param] = i
	}

	// Temps
	for i, temp := range block.Temps {
		c.temps[temp] = c.numArgs + i
	}

	// Compile block body
	c.compileStatements(block.Statements)

	// If no explicit return, return nil (blocks return last expression value)
	if len(block.Statements) == 0 {
		c.builder.Emit(vm.OpPushNil)
	}
	c.builder.Emit(vm.OpBlockReturn)

	// Create BlockMethod with its own literals
	blockMethod := &vm.BlockMethod{
		Arity:       c.numArgs,
		NumTemps:    c.numTemps,
		NumCaptures: numCaptures,
		Bytecode:    c.builder.Bytes(),
		Literals:    c.literals, // Block gets its own literals
	}

	// Restore state
	// NOTE: c.blocks is NOT restored - all blocks accumulate in method's blocks list
	c.builder = oldBuilder
	c.temps = oldTemps
	c.args = oldArgs
	c.numArgs = oldNumArgs
	c.numTemps = oldNumTemps
	c.literals = oldLiterals
	c.literalMap = oldLiteralMap
	c.inBlock = oldInBlock
	c.outerTemps = oldOuterTemps
	c.outerArgs = oldOuterArgs
	c.enclosingBlockVars = oldEnclosingBlockVars
	c.capturedVars = oldCapturedVars
	c.blockNestingDepth = oldBlockNestingDepth

	// Emit capture instructions (push each captured variable onto stack)
	// The variables are captured from the enclosing scope
	for _, varName := range varsToCapture {
		// Access the variable from the enclosing scope
		// If it's in enclosing block vars, we need to use the appropriate opcode
		if idx, ok := oldEnclosingBlockVars[varName]; ok {
			// Variable from an enclosing block - already captured
			if oldCapturedVars != nil {
				if captIdx, ok := oldCapturedVars[varName]; ok {
					c.builder.EmitByte(vm.OpPushCaptured, byte(captIdx))
					continue
				}
			}
			// Variable from current block scope - use PushTemp
			c.builder.EmitByte(vm.OpPushTemp, byte(idx))
		} else if idx, ok := c.temps[varName]; ok {
			// Local temp in current scope
			c.builder.EmitByte(vm.OpPushTemp, byte(idx))
		} else if idx, ok := c.args[varName]; ok {
			// Local arg in current scope
			c.builder.EmitByte(vm.OpPushTemp, byte(idx))
		} else if idx, ok := c.outerTemps[varName]; ok {
			// Method-level temp
			c.builder.EmitByte(vm.OpPushHomeTemp, byte(idx))
		} else if idx, ok := c.outerArgs[varName]; ok {
			// Method-level arg
			c.builder.EmitByte(vm.OpPushHomeTemp, byte(idx))
		}
	}

	// Add block to list
	blockIdx := len(c.blocks)
	c.blocks = append(c.blocks, blockMethod)

	// Emit create block instruction (16-bit index, 8-bit capture count)
	c.builder.EmitCreateBlock(uint16(blockIdx), uint8(numCaptures))
}

// findCapturedVariables analyzes a block's AST to find variables that need to be captured.
// Returns a list of variable names from enclosing block scopes that are referenced.
func (c *Compiler) findCapturedVariables(block *Block, enclosingBlockVars map[string]int) []string {
	result := make([]string, 0)
	seen := make(map[string]bool)

	// Walk the block's AST to find variable references
	var walkExpr func(expr Expr)
	var walkStmt func(stmt Stmt)

	walkExpr = func(expr Expr) {
		if expr == nil {
			return
		}
		switch e := expr.(type) {
		case *Variable:
			name := e.Name
			// Check if this variable is from an enclosing block scope
			if _, ok := enclosingBlockVars[name]; ok {
				// Skip if it's a parameter or temp of this block
				for _, p := range block.Parameters {
					if p == name {
						return
					}
				}
				for _, t := range block.Temps {
					if t == name {
						return
					}
				}
				// It's a captured variable
				if !seen[name] {
					seen[name] = true
					result = append(result, name)
				}
			}
		case *Assignment:
			walkExpr(e.Value)
		case *UnaryMessage:
			walkExpr(e.Receiver)
		case *BinaryMessage:
			walkExpr(e.Receiver)
			walkExpr(e.Argument)
		case *KeywordMessage:
			walkExpr(e.Receiver)
			for _, arg := range e.Arguments {
				walkExpr(arg)
			}
		case *Cascade:
			walkExpr(e.Receiver)
			for _, msg := range e.Messages {
				// CascadedMessage has Arguments for binary/keyword messages
				for _, arg := range msg.Arguments {
					walkExpr(arg)
				}
			}
		case *Block:
			// Recurse into nested blocks
			for _, stmt := range e.Statements {
				walkStmt(stmt)
			}
		}
	}

	walkStmt = func(stmt Stmt) {
		if stmt == nil {
			return
		}
		switch s := stmt.(type) {
		case *ExprStmt:
			walkExpr(s.Expr)
		case *Return:
			walkExpr(s.Value)
		}
	}

	for _, stmt := range block.Statements {
		walkStmt(stmt)
	}

	return result
}

// ---------------------------------------------------------------------------
// Compile helper for external use
// ---------------------------------------------------------------------------

// Compile parses and compiles source code to a method.
func Compile(source string, selectors *vm.SelectorTable, symbols *vm.SymbolTable) (*vm.CompiledMethod, error) {
	parser := NewParser(source)
	method := parser.ParseMethod()
	if len(parser.Errors()) > 0 {
		return nil, fmt.Errorf("parse errors: %v", parser.Errors())
	}

	// Run semantic analysis (warnings only, don't fail)
	warnings := Analyze(method, nil)
	_ = warnings // In the future, could log these or return them

	compiler := NewCompiler(selectors, symbols)
	compiled := compiler.CompileMethod(method)
	if len(compiler.Errors()) > 0 {
		return nil, fmt.Errorf("compile errors: %v", compiler.Errors())
	}

	return compiled, nil
}

// CompileExpr parses and compiles an expression.
func CompileExpr(source string, selectors *vm.SelectorTable, symbols *vm.SymbolTable) (*vm.CompiledMethod, error) {
	parser := NewParser(source)
	expr := parser.ParseExpression()
	if len(parser.Errors()) > 0 {
		return nil, fmt.Errorf("parse errors: %v", parser.Errors())
	}

	compiler := NewCompiler(selectors, symbols)
	compiled := compiler.CompileExpression(expr)
	if len(compiler.Errors()) > 0 {
		return nil, fmt.Errorf("compile errors: %v", compiler.Errors())
	}

	return compiled, nil
}

// ParseAndCompileSourceFile parses a Trashtalk-style source file and returns the SourceFile AST.
func ParseSourceFileFromString(source string) (*SourceFile, error) {
	parser := NewParser(source)
	sf := parser.ParseSourceFile()
	if len(parser.Errors()) > 0 {
		return nil, fmt.Errorf("parse errors: %v", parser.Errors())
	}
	return sf, nil
}

// CompileMethodDef compiles a single method definition to a CompiledMethod.
func CompileMethodDef(method *MethodDef, selectors *vm.SelectorTable, symbols *vm.SymbolTable) (*vm.CompiledMethod, error) {
	return CompileMethodDefWithIvars(method, selectors, symbols, nil)
}

// CompileMethodDefWithIvars compiles a method with instance variable context.
// The instVars slice contains the instance variable names in order.
func CompileMethodDefWithIvars(method *MethodDef, selectors *vm.SelectorTable, symbols *vm.SymbolTable, instVars []string) (*vm.CompiledMethod, error) {
	// Run semantic analysis (warnings only, don't fail)
	warnings := Analyze(method, instVars)
	_ = warnings // In the future, could log these or return them

	compiler := NewCompiler(selectors, symbols)
	if len(instVars) > 0 {
		compiler.SetInstanceVars(instVars)
	}
	compiled := compiler.CompileMethod(method)
	if len(compiler.Errors()) > 0 {
		return nil, fmt.Errorf("compile errors: %v", compiler.Errors())
	}
	return compiled, nil
}
