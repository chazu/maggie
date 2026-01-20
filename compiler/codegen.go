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

// errorf records a compilation error.
func (c *Compiler) errorf(format string, args ...interface{}) {
	c.errors = append(c.errors, fmt.Sprintf(format, args...))
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
	c.numTemps = len(method.Temps)

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
	// For now, represent strings as symbols (full String requires heap objects)
	idx := c.addLiteral(c.symbols.SymbolValue(value))
	c.builder.EmitUint16(vm.OpPushLiteral, uint16(idx))
}

func (c *Compiler) compileSymbol(value string) {
	idx := c.addLiteral(c.symbols.SymbolValue(value))
	c.builder.EmitUint16(vm.OpPushLiteral, uint16(idx))
}

func (c *Compiler) compileChar(value rune) {
	// Represent char as small int for now
	c.compileInt(int64(value))
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
	switch name {
	case "nil":
		c.builder.Emit(vm.OpPushNil)
	case "true":
		c.builder.Emit(vm.OpPushTrue)
	case "false":
		c.builder.Emit(vm.OpPushFalse)
	default:
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
		// In a block: check outer scope temps/args (use home frame access)
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
		// Must be a global
		idx := c.addLiteral(c.symbols.SymbolValue(name))
		c.builder.EmitUint16(vm.OpPushGlobal, uint16(idx))
	}
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

	// Build outer scope maps by merging current scope into outer scope
	// This allows nested blocks to see all enclosing method/block temps
	newOuterTemps := make(map[string]int)
	newOuterArgs := make(map[string]int)

	// First copy existing outer temps (for nested blocks)
	for k, v := range c.outerTemps {
		newOuterTemps[k] = v
	}
	for k, v := range c.outerArgs {
		newOuterArgs[k] = v
	}

	// Then add current scope's temps and args (they take precedence for shadowing)
	for k, v := range c.temps {
		newOuterTemps[k] = v
	}
	for k, v := range c.args {
		newOuterArgs[k] = v
	}

	// Set up block context with fresh literals pool
	// NOTE: We keep c.blocks pointing to the METHOD's blocks list
	// so that nested blocks get correct indices. Don't reset c.blocks here!
	c.builder = blockBuilder
	c.temps = make(map[string]int)
	c.args = make(map[string]int)
	c.numArgs = len(block.Parameters)
	c.numTemps = len(block.Temps)
	c.literals = nil
	c.literalMap = make(map[interface{}]int)
	// c.blocks intentionally NOT reset - all blocks share method's blocks list
	c.inBlock = true
	c.outerTemps = newOuterTemps
	c.outerArgs = newOuterArgs

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
		Arity:    c.numArgs,
		NumTemps: c.numTemps,
		Bytecode: c.builder.Bytes(),
		Literals: c.literals, // Block gets its own literals
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

	// Add block to list
	blockIdx := len(c.blocks)
	c.blocks = append(c.blocks, blockMethod)

	// Emit create block instruction (16-bit index, 8-bit capture count)
	c.builder.EmitCreateBlock(uint16(blockIdx), 0) // TODO: capture count
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
