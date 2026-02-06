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
	registry  *vm.ObjectRegistry

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

	// For mutable cell variables (captured and assigned)
	// A variable is a cell if it's captured by a nested block AND assigned anywhere
	cellVars        map[string]bool // variables that need cell boxing
	cellInitialized map[string]bool // tracks whether cell has been created for this var

	// For tail-call optimization
	methodSelector string // selector of the method being compiled (for detecting self-recursion)

	// For FQN resolution in method bodies
	namespace  string         // file's effective namespace (empty = root)
	imports    []string       // file's import paths
	classTable *vm.ClassTable // for FQN resolution (nil = no resolution)
}

// NewCompiler creates a new compiler.
func NewCompiler(selectors *vm.SelectorTable, symbols *vm.SymbolTable, registry *vm.ObjectRegistry) *Compiler {
	return &Compiler{
		selectors: selectors,
		symbols:   symbols,
		registry:  registry,
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

// SetNamespaceContext sets the namespace, imports, and class table for FQN resolution.
// When set, global variable references in method bodies will be resolved to FQN
// using the class table's LookupWithImports.
func (c *Compiler) SetNamespaceContext(namespace string, imports []string, classTable *vm.ClassTable) {
	c.namespace = namespace
	c.imports = imports
	c.classTable = classTable
}

// resolveGlobalName resolves a bare class name to its FQN using namespace context.
// If no classTable is set or the name doesn't match a known class, returns the name unchanged.
func (c *Compiler) resolveGlobalName(name string) string {
	if c.classTable == nil {
		return name
	}
	cls := c.classTable.LookupWithImports(name, c.namespace, c.imports)
	if cls != nil && cls.Namespace != "" {
		return cls.Namespace + "::" + cls.Name
	}
	return name
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
	c.methodSelector = method.Selector

	// Analyze which variables need cell boxing
	// (block-local variables that are captured AND assigned)
	c.cellVars = c.findCellVariables(method)
	c.cellInitialized = make(map[string]bool)

	// Set up argument slots
	for i, param := range method.Parameters {
		c.args[param] = i
	}

	// Set up temp slots (after args)
	for i, temp := range method.Temps {
		c.temps[temp] = c.numArgs + i
	}

	// Pre-initialize method-level cell variables.
	// Method args that are cells: wrap existing arg value in a cell.
	for name, idx := range c.args {
		if c.cellVars[name] {
			c.builder.EmitByte(vm.OpPushTemp, byte(idx)) // Push current arg value
			c.builder.Emit(vm.OpMakeCell)                 // Wrap in cell
			c.builder.EmitByte(vm.OpStoreTemp, byte(idx)) // Store cell back
			c.builder.Emit(vm.OpPOP)                      // Clean up stack
			c.cellInitialized[name] = true
		}
	}
	// Method temps that are cells: wrap nil in a cell.
	for name, idx := range c.temps {
		if c.cellVars[name] {
			c.builder.Emit(vm.OpPushNil)
			c.builder.Emit(vm.OpMakeCell)
			c.builder.EmitByte(vm.OpStoreTemp, byte(idx))
			c.builder.Emit(vm.OpPOP)
			c.cellInitialized[name] = true
		}
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
		// Check for tail-call optimization: ^self sameSelector: args
		if !c.inBlock && c.methodSelector != "" && c.isTailCallCandidate(s.Value) {
			c.compileTailCall(s.Value)
			c.builder.Emit(vm.OpReturnTop)
		} else {
			c.compileExpr(s.Value)
			c.builder.Emit(vm.OpReturnTop)
		}
	default:
		c.errorf("unknown statement type: %T", stmt)
	}
}

// ---------------------------------------------------------------------------
// Tail-call optimization
// ---------------------------------------------------------------------------

// isTailCallCandidate checks if an expression is a self-send with the same
// selector as the current method (i.e., direct self-recursion in tail position).
func (c *Compiler) isTailCallCandidate(expr Expr) bool {
	switch e := expr.(type) {
	case *UnaryMessage:
		if _, isSelf := e.Receiver.(*Self); isSelf {
			return e.Selector == c.methodSelector
		}
	case *KeywordMessage:
		if _, isSelf := e.Receiver.(*Self); isSelf {
			return e.Selector == c.methodSelector
		}
	// Note: Binary messages to self are not optimized because the common
	// arithmetic operators use fast-path opcodes (OpSendPlus, etc.) rather
	// than OpSend, so they wouldn't match during frame reuse checks in the
	// interpreter. This is acceptable since binary self-recursion is rare.
	}
	return false
}

// compileTailCall compiles a self-recursive message send using OpTailSend.
// The caller must have verified that the expression is a tail-call candidate.
func (c *Compiler) compileTailCall(expr Expr) {
	switch e := expr.(type) {
	case *UnaryMessage:
		// Compile receiver (self)
		c.compileExpr(e.Receiver)
		// Emit tail send
		c.emitTailSend(e.Selector, 0)
	case *KeywordMessage:
		// Compile receiver (self)
		c.compileExpr(e.Receiver)
		// Compile arguments
		for _, arg := range e.Arguments {
			c.compileExpr(arg)
		}
		// Emit tail send
		c.emitTailSend(e.Selector, len(e.Arguments))
	}
}

func (c *Compiler) emitTailSend(selector string, numArgs int) {
	selectorID := c.selectors.Intern(selector)
	c.builder.EmitSend(vm.OpTailSend, uint16(selectorID), byte(numArgs))
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
		// Use literal table; fall back to float if integer is too large for SmallInt encoding
		v, ok := vm.TryFromSmallInt(value)
		if !ok {
			v = vm.FromFloat64(float64(value))
		}
		idx := c.addLiteral(v)
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
	idx := c.addLiteral(c.registry.NewStringValue(value))
	c.builder.EmitUint16(vm.OpPushLiteral, uint16(idx))
}

func (c *Compiler) compileSymbol(value string) {
	idx := c.addLiteral(c.symbols.SymbolValue(value))
	c.builder.EmitUint16(vm.OpPushLiteral, uint16(idx))
}

func (c *Compiler) compileChar(value rune) {
	// Characters are first-class value types encoded via FromCharacter
	idx := c.addLiteral(vm.FromCharacter(value))
	c.builder.EmitUint16(vm.OpPushLiteral, uint16(idx))
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
		// If this is a cell variable, dereference the cell
		if c.cellVars[name] {
			c.builder.Emit(vm.OpCellGet)
		}
		return
	}
	// Check if it's a temp
	if idx, ok := c.temps[name]; ok {
		c.builder.EmitByte(vm.OpPushTemp, byte(idx))
		// If this is a cell variable, dereference the cell
		if c.cellVars[name] {
			c.builder.Emit(vm.OpCellGet)
		}
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
			// If this is a cell variable, dereference the cell
			if c.cellVars[name] {
				c.builder.Emit(vm.OpCellGet)
			}
			return
		}
	}
	// In a block: check outer METHOD scope temps/args (use home frame access)
	// HomeBP is propagated to all nested blocks, so they can also access method temps
	if c.inBlock {
		if idx, ok := c.outerTemps[name]; ok {
			c.builder.EmitByte(vm.OpPushHomeTemp, byte(idx))
			return
		}
		if idx, ok := c.outerArgs[name]; ok {
			c.builder.EmitByte(vm.OpPushHomeTemp, byte(idx))
			return
		}
	}
	// Must be a global — resolve to FQN if namespace context is available
	resolved := c.resolveGlobalName(name)
	idx := c.addLiteral(c.symbols.SymbolValue(resolved))
	c.builder.EmitUint16(vm.OpPushGlobal, uint16(idx))
}

func (c *Compiler) compileAssignment(assign *Assignment) {
	name := assign.Variable

	// Check if this is a cell variable assignment
	if c.cellVars[name] {
		// Cell variable: special handling for mutable captures.
		// Check both temps and args since both use OpPushTemp/OpStoreTemp.
		localIdx := -1
		if idx, ok := c.temps[name]; ok {
			localIdx = idx
		} else if idx, ok := c.args[name]; ok {
			localIdx = idx
		}
		if localIdx >= 0 {
			// Local cell variable in this scope (temp or arg)
			if !c.cellInitialized[name] {
				// First assignment: create the cell
				c.compileExpr(assign.Value)
				c.builder.Emit(vm.OpMakeCell)
				c.builder.EmitByte(vm.OpStoreTemp, byte(localIdx))
				c.builder.EmitByte(vm.OpPushTemp, byte(localIdx))
				c.builder.Emit(vm.OpCellGet) // Leave the VALUE on stack
				c.cellInitialized[name] = true
			} else {
				// Subsequent assignment: store into existing cell
				c.builder.EmitByte(vm.OpPushTemp, byte(localIdx)) // Get cell ref
				c.compileExpr(assign.Value)                       // Value to store
				c.builder.Emit(vm.OpCellSet)                      // Store and leave value on stack
			}
			return
		}
		// Captured cell variable from outer block
		if c.inBlock && c.capturedVars != nil {
			if idx, ok := c.capturedVars[name]; ok {
				c.builder.EmitByte(vm.OpPushCaptured, byte(idx)) // Get cell ref
				c.compileExpr(assign.Value)                      // Value to store
				c.builder.Emit(vm.OpCellSet)                     // Store and leave value on stack
				return
			}
		}
	}

	// Compile value (for non-cell variables)
	c.compileExpr(assign.Value)

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

	// In a block: check captured variables first (for nested blocks)
	if c.inBlock && c.capturedVars != nil {
		if idx, ok := c.capturedVars[name]; ok {
			c.builder.EmitByte(vm.OpStoreCaptured, byte(idx))
			c.builder.EmitByte(vm.OpPushCaptured, byte(idx)) // Leave value on stack
			return
		}
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

	// Global assignment — resolve to FQN if namespace context is available
	resolved := c.resolveGlobalName(name)
	idx := c.addLiteral(c.symbols.SymbolValue(resolved))
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
	// NOTE: c.cellVars is NOT saved/restored - it's global to the method
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
	oldCellInitialized := c.cellInitialized

	// Increment nesting depth: 0=method, 1=first block, 2+=nested blocks
	newNestingDepth := oldBlockNestingDepth + 1

	// All outer-scope variables are now accessed via captures (not HomeBP).
	// This ensures blocks work correctly even if they outlive their method frame.
	// outerTemps/outerArgs are kept empty — HomeBP is only used for non-local returns.
	newOuterTemps := make(map[string]int)
	newOuterArgs := make(map[string]int)

	// Build map of enclosing scope variables that this block can capture.
	// For depth-1 blocks, this includes method-level vars.
	// For deeper blocks, this includes vars from all enclosing scopes.
	newEnclosingBlockVars := make(map[string]int)
	if oldEnclosingBlockVars != nil {
		for k, v := range oldEnclosingBlockVars {
			newEnclosingBlockVars[k] = v
		}
	}
	if newNestingDepth == 1 {
		// First-level block: method-level vars are the enclosing scope
		for k, v := range c.temps {
			newEnclosingBlockVars[k] = v
		}
		for k, v := range c.args {
			newEnclosingBlockVars[k] = v
		}
	} else if oldInBlock {
		// Nested block: current block's vars become enclosing block vars
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
	// NOTE: c.cellVars is NOT reset - it's global to the method
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
	c.cellInitialized = make(map[string]bool) // Fresh for each block's own temps

	// Parameters
	for i, param := range block.Parameters {
		c.args[param] = i
	}

	// Temps
	for i, temp := range block.Temps {
		c.temps[temp] = c.numArgs + i
	}

	// Pre-initialize cell variables with Cell(nil) so that nested blocks
	// always capture a cell reference, even when the first assignment
	// happens in a nested block rather than in the defining scope.
	for name, idx := range c.temps {
		if c.cellVars[name] {
			c.builder.Emit(vm.OpPushNil)
			c.builder.Emit(vm.OpMakeCell)
			c.builder.EmitByte(vm.OpStoreTemp, byte(idx))
			c.builder.Emit(vm.OpPOP) // Clean up stack; no expression result needed
			c.cellInitialized[name] = true
		}
	}
	for name, idx := range c.args {
		if c.cellVars[name] {
			c.builder.EmitByte(vm.OpPushTemp, byte(idx)) // Push current arg value
			c.builder.Emit(vm.OpMakeCell)                 // Wrap in cell
			c.builder.EmitByte(vm.OpStoreTemp, byte(idx)) // Store cell back
			c.builder.Emit(vm.OpPOP)                      // Clean up stack
			c.cellInitialized[name] = true
		}
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
	// NOTE: c.cellVars is NOT restored - it's global to the method
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
	c.cellInitialized = oldCellInitialized

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
			// Check if assigning to an enclosing block variable
			name := e.Variable
			if _, ok := enclosingBlockVars[name]; ok {
				// Skip if it's a parameter or temp of this block
				isLocal := false
				for _, p := range block.Parameters {
					if p == name {
						isLocal = true
						break
					}
				}
				if !isLocal {
					for _, t := range block.Temps {
						if t == name {
							isLocal = true
							break
						}
					}
				}
				if !isLocal && !seen[name] {
					seen[name] = true
					result = append(result, name)
				}
			}
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

// findCellVariables analyzes a method to find variables that need cell boxing.
// A variable (method-level or block-local) needs a cell if it's captured by a
// nested block AND assigned in that nested block. This ensures mutations through
// the cell are visible to all scopes sharing the reference.
func (c *Compiler) findCellVariables(method *MethodDef) map[string]bool {
	cellVars := make(map[string]bool)

	// Track: for each block-local variable, is it captured? is it assigned in nested scope?
	type varInfo struct {
		definedInBlock      bool
		captured            bool
		assignedInNestedBlk bool // assigned in a block deeper than where defined
		blockDepth          int  // depth where defined
	}
	varInfos := make(map[string]*varInfo)

	// Helper to walk expressions looking for captures and assignments
	var walkExpr func(expr Expr, currentDepth int, blockVars map[string]int)
	var walkStmt func(stmt Stmt, currentDepth int, blockVars map[string]int)
	var walkBlock func(block *Block, depth int, outerBlockVars map[string]int)

	walkExpr = func(expr Expr, currentDepth int, blockVars map[string]int) {
		if expr == nil {
			return
		}
		switch e := expr.(type) {
		case *Variable:
			name := e.Name
			// Check if accessing a variable from an outer scope (block or method level)
			if vi, ok := varInfos[name]; ok && vi.blockDepth < currentDepth {
				vi.captured = true
			}
		case *Assignment:
			name := e.Variable
			// Track assignment - if assigning to an outer scope variable, it's both captured and assigned
			if vi, ok := varInfos[name]; ok && vi.blockDepth < currentDepth {
				vi.captured = true            // Need to capture the cell reference
				vi.assignedInNestedBlk = true // And mark as assigned in nested block
			}
			walkExpr(e.Value, currentDepth, blockVars)
		case *UnaryMessage:
			walkExpr(e.Receiver, currentDepth, blockVars)
		case *BinaryMessage:
			walkExpr(e.Receiver, currentDepth, blockVars)
			walkExpr(e.Argument, currentDepth, blockVars)
		case *KeywordMessage:
			walkExpr(e.Receiver, currentDepth, blockVars)
			for _, arg := range e.Arguments {
				walkExpr(arg, currentDepth, blockVars)
			}
		case *Cascade:
			walkExpr(e.Receiver, currentDepth, blockVars)
			for _, msg := range e.Messages {
				for _, arg := range msg.Arguments {
					walkExpr(arg, currentDepth, blockVars)
				}
			}
		case *Block:
			// Recurse into nested block
			walkBlock(e, currentDepth+1, blockVars)
		case *ArrayLiteral:
			for _, elem := range e.Elements {
				walkExpr(elem, currentDepth, blockVars)
			}
		}
	}

	walkStmt = func(stmt Stmt, currentDepth int, blockVars map[string]int) {
		if stmt == nil {
			return
		}
		switch s := stmt.(type) {
		case *ExprStmt:
			walkExpr(s.Expr, currentDepth, blockVars)
		case *Return:
			walkExpr(s.Value, currentDepth, blockVars)
		}
	}

	walkBlock = func(block *Block, depth int, outerBlockVars map[string]int) {
		// Build block vars map including this block's temps
		newBlockVars := make(map[string]int)
		for k, v := range outerBlockVars {
			newBlockVars[k] = v
		}

		// Register this block's parameters and temps
		for i, p := range block.Parameters {
			newBlockVars[p] = i
			varInfos[p] = &varInfo{definedInBlock: true, blockDepth: depth}
		}
		for i, t := range block.Temps {
			idx := len(block.Parameters) + i
			newBlockVars[t] = idx
			varInfos[t] = &varInfo{definedInBlock: true, blockDepth: depth}
		}

		// Walk block statements
		for _, stmt := range block.Statements {
			walkStmt(stmt, depth, newBlockVars)
		}
	}

	// Start at method level (depth 0)
	// Method-level vars don't need cells (they use HOME_TEMP)
	methodVars := make(map[string]int)
	for i, p := range method.Parameters {
		methodVars[p] = i
		// Method-level vars are NOT defined in a block
		varInfos[p] = &varInfo{definedInBlock: false, blockDepth: 0}
	}
	for i, t := range method.Temps {
		methodVars[t] = len(method.Parameters) + i
		varInfos[t] = &varInfo{definedInBlock: false, blockDepth: 0}
	}

	// Walk method statements
	for _, stmt := range method.Statements {
		walkStmt(stmt, 0, methodVars)
	}

	// Collect variables that need cells.
	// A variable needs a cell if it's captured by a block AND assigned from a block.
	// This applies to both block-local and method-level variables. Method-level
	// variables now go through captures (not HomeBP) so blocks can outlive
	// their method frame without reading stale stack data.
	for name, vi := range varInfos {
		if vi.captured && vi.assignedInNestedBlk {
			cellVars[name] = true
		}
	}

	return cellVars
}

// ---------------------------------------------------------------------------
// Compile helper for external use
// ---------------------------------------------------------------------------

// Compile parses and compiles source code to a method.
func Compile(source string, selectors *vm.SelectorTable, symbols *vm.SymbolTable, registry *vm.ObjectRegistry) (*vm.CompiledMethod, error) {
	parser := NewParser(source)
	method := parser.ParseMethod()
	if len(parser.Errors()) > 0 {
		return nil, fmt.Errorf("parse errors: %v", parser.Errors())
	}

	// Run semantic analysis (warnings only, don't fail)
	warnings := Analyze(method, nil)
	_ = warnings // In the future, could log these or return them

	compiler := NewCompiler(selectors, symbols, registry)
	compiled := compiler.CompileMethod(method)
	if len(compiler.Errors()) > 0 {
		return nil, fmt.Errorf("compile errors: %v", compiler.Errors())
	}

	return compiled, nil
}

// CompileExpr parses and compiles an expression.
func CompileExpr(source string, selectors *vm.SelectorTable, symbols *vm.SymbolTable, registry *vm.ObjectRegistry) (*vm.CompiledMethod, error) {
	parser := NewParser(source)
	expr := parser.ParseExpression()
	if len(parser.Errors()) > 0 {
		return nil, fmt.Errorf("parse errors: %v", parser.Errors())
	}

	compiler := NewCompiler(selectors, symbols, registry)
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
func CompileMethodDef(method *MethodDef, selectors *vm.SelectorTable, symbols *vm.SymbolTable, registry *vm.ObjectRegistry) (*vm.CompiledMethod, error) {
	return CompileMethodDefWithIvars(method, selectors, symbols, registry, nil)
}

// CompileMethodDefWithIvars compiles a method with instance variable context.
// The instVars slice contains the instance variable names in order.
func CompileMethodDefWithIvars(method *MethodDef, selectors *vm.SelectorTable, symbols *vm.SymbolTable, registry *vm.ObjectRegistry, instVars []string) (*vm.CompiledMethod, error) {
	// Run semantic analysis (warnings only, don't fail)
	warnings := Analyze(method, instVars)
	_ = warnings // In the future, could log these or return them

	compiler := NewCompiler(selectors, symbols, registry)
	if len(instVars) > 0 {
		compiler.SetInstanceVars(instVars)
	}
	compiled := compiler.CompileMethod(method)
	if len(compiler.Errors()) > 0 {
		return nil, fmt.Errorf("compile errors: %v", compiler.Errors())
	}
	return compiled, nil
}

// CompileMethodDefWithContext compiles a method with instance variables and namespace context.
// When namespace/imports/classTable are provided, bare class names in global lookups
// are resolved to FQN at compile time.
func CompileMethodDefWithContext(method *MethodDef, selectors *vm.SelectorTable, symbols *vm.SymbolTable, registry *vm.ObjectRegistry, instVars []string, namespace string, imports []string, classTable *vm.ClassTable) (*vm.CompiledMethod, error) {
	warnings := Analyze(method, instVars)
	_ = warnings

	comp := NewCompiler(selectors, symbols, registry)
	if len(instVars) > 0 {
		comp.SetInstanceVars(instVars)
	}
	comp.SetNamespaceContext(namespace, imports, classTable)
	compiled := comp.CompileMethod(method)
	if len(comp.Errors()) > 0 {
		return nil, fmt.Errorf("compile errors: %v", comp.Errors())
	}
	return compiled, nil
}
