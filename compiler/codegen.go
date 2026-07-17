package compiler

import (
	"fmt"
	"math/big"
	"strings"

	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// Codegen: Compile AST to bytecode
// ---------------------------------------------------------------------------

// compilationFrame holds the mutable state for a single compilation scope
// (method or block). The Compiler maintains a stack of these.
type compilationFrame struct {
	builder            *vm.BytecodeBuilder
	literals           []vm.Value
	literalMap         map[interface{}]int
	temps              map[string]int
	args               map[string]int
	numArgs            int
	numTemps           int
	inBlock            bool
	outerTemps         map[string]int
	outerArgs          map[string]int
	enclosingBlockVars map[string]int
	capturedVars       map[string]int
	blockNestingDepth  int
	cellInitialized    map[string]bool
	sourceMap          []vm.SourceLoc
}

// Compiler compiles AST nodes to bytecode.
type Compiler struct {
	selectors *vm.SelectorTable
	symbols   *vm.SymbolTable
	registry  *vm.ObjectRegistry

	// Current compilation frame (per-scope mutable state)
	frame      *compilationFrame
	frameStack []*compilationFrame

	// Shared across all scopes within a method compilation
	instVars       map[string]int
	blocks         []*vm.BlockMethod
	cellVars       map[string]bool
	errors         []string
	methodSelector string

	// FQN resolution context
	namespace  string
	imports    []string
	classTable *vm.ClassTable
}

// NewCompiler creates a new compiler.
func NewCompiler(selectors *vm.SelectorTable, symbols *vm.SymbolTable, registry *vm.ObjectRegistry) *Compiler {
	return &Compiler{
		selectors: selectors,
		symbols:   symbols,
		registry:  registry,
	}
}

// pushFrame saves the current frame and activates a new one.
func (c *Compiler) pushFrame(f *compilationFrame) {
	if c.frame != nil {
		c.frameStack = append(c.frameStack, c.frame)
	}
	c.frame = f
}

// popFrame restores the previous frame.
func (c *Compiler) popFrame() {
	n := len(c.frameStack)
	if n == 0 {
		c.frame = nil
		return
	}
	c.frame = c.frameStack[n-1]
	c.frameStack = c.frameStack[:n-1]
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

// checkSlotIndex validates that a variable slot index fits in a uint8.
func (c *Compiler) checkSlotIndex(idx int, kind string, name string) {
	if idx > 0xFF {
		c.errorf("%s slot overflow: %s has index %d, exceeds maximum of 255", kind, name, idx)
	}
}

// errorAt records a compilation error with position information from a node.
func (c *Compiler) errorAt(node Node, format string, args ...interface{}) {
	pos := node.Span().Start
	msg := fmt.Sprintf("line %d, column %d: %s", pos.Line, pos.Column, fmt.Sprintf(format, args...))
	c.errors = append(c.errors, msg)
}

// emitSourcePos records a source map entry mapping the current bytecode offset
// to the given AST node's start position. Duplicate entries (same offset) are
// suppressed to keep the map compact.
func (c *Compiler) emitSourcePos(node Node) {
	pos := node.Span().Start
	if pos.Line == 0 {
		return // no position info
	}
	offset := c.frame.builder.Len()
	// Suppress duplicate: if the last entry has the same offset, skip
	if len(c.frame.sourceMap) > 0 && c.frame.sourceMap[len(c.frame.sourceMap)-1].Offset == offset {
		return
	}
	c.frame.sourceMap = append(c.frame.sourceMap, vm.SourceLoc{
		Offset: offset,
		Line:   pos.Line,
		Column: pos.Column,
	})
}

// CompileMethod compiles a method definition to a CompiledMethod.
func (c *Compiler) CompileMethod(method *MethodDef) *vm.CompiledMethod {
	c.pushFrame(&compilationFrame{
		builder:    vm.NewBytecodeBuilder(),
		literalMap: make(map[interface{}]int),
		temps:      make(map[string]int),
		args:       make(map[string]int),
		numArgs:    len(method.Parameters),
		numTemps:   len(method.Parameters) + len(method.Temps),
	})
	defer c.popFrame()
	c.blocks = nil
	c.methodSelector = method.Selector

	// Analyze which variables need cell boxing
	// (block-local variables that are captured AND assigned)
	c.cellVars = c.findCellVariables(method)
	c.frame.cellInitialized = make(map[string]bool)

	// Set up argument slots. Reject duplicate names: a duplicate silently
	// overwrote the earlier slot, making the first binding unreachable with no
	// diagnostic (e.g. `a: x b: x` left the first x permanently shadowed).
	seenNames := make(map[string]bool, len(method.Parameters)+len(method.Temps))
	for i, param := range method.Parameters {
		if seenNames[param] {
			c.errorf("duplicate parameter name %q in method %s", param, method.Selector)
		}
		seenNames[param] = true
		c.frame.args[param] = i
	}

	// Set up temp slots (after args)
	for i, temp := range method.Temps {
		if seenNames[temp] {
			c.errorf("duplicate variable name %q in method %s", temp, method.Selector)
		}
		seenNames[temp] = true
		c.frame.temps[temp] = c.frame.numArgs + i
	}

	// Pre-initialize method-level cell variables.
	// Method args that are cells: wrap existing arg value in a cell.
	for name, idx := range c.frame.args {
		if c.cellVars[name] {
			c.checkSlotIndex(idx, "argument", name)
			c.frame.builder.EmitByte(vm.OpPushTemp, byte(idx))  // Push current arg value
			c.frame.builder.Emit(vm.OpMakeCell)                 // Wrap in cell
			c.frame.builder.EmitByte(vm.OpStoreTemp, byte(idx)) // Store cell back
			c.frame.builder.Emit(vm.OpPOP)                      // Clean up stack
			c.frame.cellInitialized[name] = true
		}
	}
	// Method temps that are cells: wrap nil in a cell.
	for name, idx := range c.frame.temps {
		if c.cellVars[name] {
			c.checkSlotIndex(idx, "temp", name)
			c.frame.builder.Emit(vm.OpPushNil)
			c.frame.builder.Emit(vm.OpMakeCell)
			c.frame.builder.EmitByte(vm.OpStoreTemp, byte(idx))
			c.frame.builder.Emit(vm.OpPOP)
			c.frame.cellInitialized[name] = true
		}
	}

	// Compile statements
	c.compileStatements(method.Statements)

	// If no explicit return, return self
	if len(method.Statements) == 0 || !c.endsWithReturn(method.Statements) {
		c.frame.builder.Emit(vm.OpPushSelf)
		c.frame.builder.Emit(vm.OpReturnTop)
	}

	// Collect any builder errors (e.g., jump offset overflow)
	c.errors = append(c.errors, c.frame.builder.Errors...)

	// Build the method
	b := vm.NewCompiledMethodBuilder(method.Selector, c.frame.numArgs)
	b.SetNumTemps(c.frame.numTemps)

	// Copy bytecode
	for _, code := range c.frame.builder.Bytes() {
		b.Bytecode().EmitRaw(code)
	}

	// Add literals
	for _, lit := range c.frame.literals {
		b.AddLiteral(lit)
	}

	// Add blocks
	for _, block := range c.blocks {
		b.AddBlock(block)
	}

	result := b.Build()

	// Transfer accumulated source map
	result.SourceMap = c.frame.sourceMap

	// Peephole optimize bytecode (constant folding, push-pop elimination, dead code)
	result.Bytecode, result.SourceMap = Peephole(result.Bytecode, result.SourceMap)

	return result
}

// CompileExpression compiles an expression for REPL/eval. It delegates to
// CompileMethod with a synthetic `doIt` returning the expression, so the full
// pipeline runs — crucially findCellVariables, which the old standalone path
// skipped, causing `[:a | [ a := a + 1 ] value. a] value: 1` to answer 1
// instead of 2 (the inner block mutated a private capture copy, not a shared
// cell). Delegating also resets stale cell state on a reused Compiler.
func (c *Compiler) CompileExpression(expr Expr) *vm.CompiledMethod {
	return c.CompileMethod(&MethodDef{
		Selector:   "doIt",
		Statements: []Stmt{&Return{SpanVal: expr.Span(), Value: expr}},
	})
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
			c.frame.builder.Emit(vm.OpPOP)
		}
	}
}

func (c *Compiler) compileStmt(stmt Stmt) {
	c.emitSourcePos(stmt)
	switch s := stmt.(type) {
	case *ExprStmt:
		c.compileExpr(s.Expr)
	case *Return:
		// Check for tail-call optimization: ^self sameSelector: args
		if !c.frame.inBlock && c.methodSelector != "" && c.isTailCallCandidate(s.Value) {
			c.compileTailCall(s.Value)
			c.frame.builder.Emit(vm.OpReturnTop)
		} else {
			c.compileExpr(s.Value)
			c.frame.builder.Emit(vm.OpReturnTop)
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
	if selectorID > 0xFFFF {
		c.errorf("selector table overflow: selector %q has ID %d, exceeds maximum of 65535", selector, selectorID)
	}
	if numArgs > 0xFF {
		c.errorf("too many arguments: %d exceeds maximum of 255 for selector %q", numArgs, selector)
	}
	c.frame.builder.EmitSend(vm.OpTailSend, uint16(selectorID), byte(numArgs))
}

// ---------------------------------------------------------------------------
// Expression compilation
// ---------------------------------------------------------------------------

func (c *Compiler) compileExpr(expr Expr) {
	c.emitSourcePos(expr)
	switch e := expr.(type) {
	case *IntLiteral:
		if e.BigValue != nil {
			c.compileBigInt(e.BigValue)
		} else {
			c.compileInt(e.Value)
		}
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
	case *DictionaryLiteral:
		c.compileDictionaryLiteral(e)
	case *Variable:
		c.compileVariable(e.Name)
	case *Assignment:
		c.compileAssignment(e)
	case *Self:
		c.frame.builder.Emit(vm.OpPushSelf)
	case *Super:
		c.frame.builder.Emit(vm.OpPushSelf) // Super uses self but dispatches to superclass
	case *ThisContext:
		c.frame.builder.Emit(vm.OpPushContext)
	case *NilLiteral:
		c.frame.builder.Emit(vm.OpPushNil)
	case *TrueLiteral:
		c.frame.builder.Emit(vm.OpPushTrue)
	case *FalseLiteral:
		c.frame.builder.Emit(vm.OpPushFalse)
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
		c.frame.builder.EmitInt8(vm.OpPushInt8, int8(value))
	} else {
		// Use literal table. A value that fits int64 but exceeds the SmallInt
		// encoding range must become an exact BigInteger, not a lossy Float.
		v, ok := vm.TryFromSmallInt(value)
		if !ok {
			c.compileBigInt(big.NewInt(value))
			return
		}
		idx := c.addLiteral(v)
		c.frame.builder.EmitUint16(vm.OpPushLiteral, uint16(idx))
	}
}

// compileBigInt emits a BigInteger literal, preserving exact value for integers
// outside the SmallInt range (the alternative, a Float, silently loses precision).
func (c *Compiler) compileBigInt(value *big.Int) {
	idx := c.addLiteral(c.registry.NewBigIntValue(value))
	c.frame.builder.EmitUint16(vm.OpPushLiteral, uint16(idx))
}

func (c *Compiler) compileFloat(value float64) {
	// Could use OpPushFloat for inline, but use literal for now
	idx := c.addLiteral(vm.FromFloat64(value))
	c.frame.builder.EmitUint16(vm.OpPushLiteral, uint16(idx))
}

func (c *Compiler) compileString(value string) {
	// Create an actual string value in the VM's string registry
	idx := c.addLiteral(c.registry.NewStringValue(value))
	c.frame.builder.EmitUint16(vm.OpPushLiteral, uint16(idx))
}

func (c *Compiler) compileSymbol(value string) {
	idx := c.addLiteral(c.symbols.SymbolValue(value))
	c.frame.builder.EmitUint16(vm.OpPushLiteral, uint16(idx))
}

func (c *Compiler) compileChar(value rune) {
	// Characters are first-class value types encoded via FromCharacter
	idx := c.addLiteral(vm.FromCharacter(value))
	c.frame.builder.EmitUint16(vm.OpPushLiteral, uint16(idx))
}

func (c *Compiler) compileArrayLiteral(arr *ArrayLiteral) {
	if len(arr.Elements) > 0xFF {
		c.errorAt(arr, "array literal too large: %d elements exceeds maximum of 255", len(arr.Elements))
	}
	// Push elements and create array
	for _, elem := range arr.Elements {
		c.compileExpr(elem)
	}
	c.frame.builder.EmitByte(vm.OpCreateArray, byte(len(arr.Elements)))
}

func (c *Compiler) compileDynamicArray(arr *DynamicArray) {
	if len(arr.Elements) > 0xFF {
		c.errorAt(arr, "dynamic array too large: %d elements exceeds maximum of 255", len(arr.Elements))
	}
	for _, elem := range arr.Elements {
		c.compileExpr(elem)
	}
	c.frame.builder.EmitByte(vm.OpCreateArray, byte(len(arr.Elements)))
}

func (c *Compiler) compileDictionaryLiteral(dict *DictionaryLiteral) {
	if len(dict.Keys) > 0xFF {
		c.errorAt(dict, "dictionary literal too large: %d keys exceeds maximum of 255", len(dict.Keys))
	}
	// Push key-value pairs interleaved: key1, val1, key2, val2, ...
	for i := range dict.Keys {
		c.compileExpr(dict.Keys[i])
		c.compileExpr(dict.Values[i])
	}
	c.frame.builder.EmitByte(vm.OpCreateDict, byte(len(dict.Keys)))
}

// addLiteral adds a literal to the literal table, returning its index.
//
// Dedup keys on the raw uint64 NaN-box bits, which is sound because Maggie
// values are uniquely encoded: SmallInts/Floats/Chars are bit-equal iff
// semantically equal, and registry-backed values (Strings/Symbols) are
// interned at construction so equal payloads share an ID.
func (c *Compiler) addLiteral(value vm.Value) int {
	key := value
	if idx, ok := c.frame.literalMap[key]; ok {
		return idx
	}

	idx := len(c.frame.literals)
	if idx > 0xFFFF {
		c.errorf("literal pool overflow: method has more than 65535 literals")
		return 0
	}
	c.frame.literals = append(c.frame.literals, value)
	c.frame.literalMap[key] = idx
	return idx
}

// ---------------------------------------------------------------------------
// Variable compilation
// ---------------------------------------------------------------------------

func (c *Compiler) compileVariable(name string) {
	// Check if it's an argument
	if idx, ok := c.frame.args[name]; ok {
		c.checkSlotIndex(idx, "argument", name)
		c.frame.builder.EmitByte(vm.OpPushTemp, byte(idx))
		// If this is a cell variable, dereference the cell
		if c.cellVars[name] {
			c.frame.builder.Emit(vm.OpCellGet)
		}
		return
	}
	// Check if it's a temp
	if idx, ok := c.frame.temps[name]; ok {
		c.checkSlotIndex(idx, "temp", name)
		c.frame.builder.EmitByte(vm.OpPushTemp, byte(idx))
		// If this is a cell variable, dereference the cell
		if c.cellVars[name] {
			c.frame.builder.Emit(vm.OpCellGet)
		}
		return
	}
	// Check if it's an instance variable
	if idx, ok := c.instVars[name]; ok {
		c.checkSlotIndex(idx, "instance variable", name)
		c.frame.builder.EmitByte(vm.OpPushIvar, byte(idx))
		return
	}
	// In a block: check captured variables first (for nested blocks)
	if c.frame.inBlock && c.frame.capturedVars != nil {
		if idx, ok := c.frame.capturedVars[name]; ok {
			c.checkSlotIndex(idx, "captured variable", name)
			c.frame.builder.EmitByte(vm.OpPushCaptured, byte(idx))
			// If this is a cell variable, dereference the cell
			if c.cellVars[name] {
				c.frame.builder.Emit(vm.OpCellGet)
			}
			return
		}
	}
	// In a block: check outer METHOD scope temps/args (use home frame access)
	// HomeBP is propagated to all nested blocks, so they can also access method temps
	if c.frame.inBlock {
		if idx, ok := c.frame.outerTemps[name]; ok {
			c.checkSlotIndex(idx, "outer temp", name)
			c.frame.builder.EmitByte(vm.OpPushHomeTemp, byte(idx))
			return
		}
		if idx, ok := c.frame.outerArgs[name]; ok {
			c.checkSlotIndex(idx, "outer arg", name)
			c.frame.builder.EmitByte(vm.OpPushHomeTemp, byte(idx))
			return
		}
	}
	// Must be a global — resolve to FQN if namespace context is available
	resolved := c.resolveGlobalName(name)
	idx := c.addLiteral(c.symbols.SymbolValue(resolved))
	c.frame.builder.EmitUint16(vm.OpPushGlobal, uint16(idx))
}

func (c *Compiler) compileAssignment(assign *Assignment) {
	name := assign.Variable

	// Check if this is a cell variable assignment
	if c.cellVars[name] {
		// Cell variable: special handling for mutable captures.
		// Check both temps and args since both use OpPushTemp/OpStoreTemp.
		localIdx := -1
		if idx, ok := c.frame.temps[name]; ok {
			localIdx = idx
		} else if idx, ok := c.frame.args[name]; ok {
			localIdx = idx
		}
		if localIdx >= 0 {
			c.checkSlotIndex(localIdx, "cell variable", name)
			// Local cell variable in this scope (temp or arg)
			if !c.frame.cellInitialized[name] {
				// First assignment: create the cell
				c.compileExpr(assign.Value)
				c.frame.builder.Emit(vm.OpMakeCell)
				c.frame.builder.EmitByte(vm.OpStoreTemp, byte(localIdx))
				c.frame.builder.EmitByte(vm.OpPushTemp, byte(localIdx))
				c.frame.builder.Emit(vm.OpCellGet) // Leave the VALUE on stack
				c.frame.cellInitialized[name] = true
			} else {
				// Subsequent assignment: store into existing cell
				c.frame.builder.EmitByte(vm.OpPushTemp, byte(localIdx)) // Get cell ref
				c.compileExpr(assign.Value)                             // Value to store
				c.frame.builder.Emit(vm.OpCellSet)                      // Store and leave value on stack
			}
			return
		}
		// Captured cell variable from outer block
		if c.frame.inBlock && c.frame.capturedVars != nil {
			if idx, ok := c.frame.capturedVars[name]; ok {
				c.checkSlotIndex(idx, "captured cell variable", name)
				c.frame.builder.EmitByte(vm.OpPushCaptured, byte(idx)) // Get cell ref
				c.compileExpr(assign.Value)                            // Value to store
				c.frame.builder.Emit(vm.OpCellSet)                     // Store and leave value on stack
				return
			}
		}
	}

	// Compile value (for non-cell variables)
	c.compileExpr(assign.Value)

	// Check if it's a temp
	if idx, ok := c.frame.temps[name]; ok {
		c.checkSlotIndex(idx, "temp", name)
		c.frame.builder.EmitByte(vm.OpStoreTemp, byte(idx))
		c.frame.builder.EmitByte(vm.OpPushTemp, byte(idx)) // Leave value on stack
		return
	}

	// Check if it's an arg (can't assign to args in standard Smalltalk, but allow it)
	if idx, ok := c.frame.args[name]; ok {
		c.checkSlotIndex(idx, "argument", name)
		c.frame.builder.EmitByte(vm.OpStoreTemp, byte(idx))
		c.frame.builder.EmitByte(vm.OpPushTemp, byte(idx))
		return
	}

	// Check if it's an instance variable
	if idx, ok := c.instVars[name]; ok {
		c.checkSlotIndex(idx, "instance variable", name)
		c.frame.builder.EmitByte(vm.OpStoreIvar, byte(idx))
		c.frame.builder.EmitByte(vm.OpPushIvar, byte(idx)) // Leave value on stack
		return
	}

	// In a block: check captured variables first (for nested blocks)
	if c.frame.inBlock && c.frame.capturedVars != nil {
		if idx, ok := c.frame.capturedVars[name]; ok {
			c.checkSlotIndex(idx, "captured variable", name)
			c.frame.builder.EmitByte(vm.OpStoreCaptured, byte(idx))
			c.frame.builder.EmitByte(vm.OpPushCaptured, byte(idx)) // Leave value on stack
			return
		}
	}

	// In a block: check outer scope temps (use home frame access)
	if c.frame.inBlock {
		if idx, ok := c.frame.outerTemps[name]; ok {
			c.checkSlotIndex(idx, "outer temp", name)
			c.frame.builder.EmitByte(vm.OpStoreHomeTemp, byte(idx))
			c.frame.builder.EmitByte(vm.OpPushHomeTemp, byte(idx)) // Leave value on stack
			return
		}
		// Note: we don't allow assigning to outer args, treat as global
	}

	// Global assignment — resolve to FQN if namespace context is available
	resolved := c.resolveGlobalName(name)
	idx := c.addLiteral(c.symbols.SymbolValue(resolved))
	c.frame.builder.EmitUint16(vm.OpStoreGlobal, uint16(idx))
	c.frame.builder.EmitUint16(vm.OpPushGlobal, uint16(idx))
}

// ---------------------------------------------------------------------------
// Message compilation
// ---------------------------------------------------------------------------

func (c *Compiler) compileUnaryMessage(msg *UnaryMessage) {
	// Inline [cond] whileTrue / whileFalse on literal blocks
	if c.tryInlineUnaryControlFlow(msg) {
		return
	}

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
			c.frame.builder.Emit(vm.OpSendPlus)
			return
		case "-":
			c.frame.builder.Emit(vm.OpSendMinus)
			return
		case "*":
			c.frame.builder.Emit(vm.OpSendTimes)
			return
		case "/":
			c.frame.builder.Emit(vm.OpSendDiv)
			return
		case "<":
			c.frame.builder.Emit(vm.OpSendLT)
			return
		case ">":
			c.frame.builder.Emit(vm.OpSendGT)
			return
		case "<=":
			c.frame.builder.Emit(vm.OpSendLE)
			return
		case ">=":
			c.frame.builder.Emit(vm.OpSendGE)
			return
		case "=":
			c.frame.builder.Emit(vm.OpSendEQ)
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
	// Inline standard control flow (ifTrue:/whileTrue:/and:/…) when the
	// block arguments are literal blocks
	if c.tryInlineControlFlow(msg) {
		return
	}

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
	if selectorID > 0xFFFF {
		c.errorf("selector table overflow: selector %q has ID %d, exceeds maximum of 65535", selector, selectorID)
	}
	if numArgs > 0xFF {
		c.errorf("too many arguments: %d exceeds maximum of 255 for selector %q", numArgs, selector)
	}
	c.frame.builder.EmitSend(vm.OpSend, uint16(selectorID), byte(numArgs))
}

func (c *Compiler) emitSendSuper(selector string, numArgs int) {
	selectorID := c.selectors.Intern(selector)
	if selectorID > 0xFFFF {
		c.errorf("selector table overflow: selector %q has ID %d, exceeds maximum of 65535", selector, selectorID)
	}
	if numArgs > 0xFF {
		c.errorf("too many arguments: %d exceeds maximum of 255 for selector %q", numArgs, selector)
	}
	c.frame.builder.EmitSend(vm.OpSendSuper, uint16(selectorID), byte(numArgs))
}

// ---------------------------------------------------------------------------
// Cascade compilation
// ---------------------------------------------------------------------------

func (c *Compiler) compileCascade(cascade *Cascade) {
	// Compile receiver once. A `super` receiver pushes self but each cascaded
	// message must dispatch to the superclass, so pick the send op accordingly
	// — otherwise `super foo; bar` would dynamically dispatch on self and could
	// re-enter an override of foo (infinite recursion) instead of super's.
	_, isSuper := cascade.Receiver.(*Super)
	c.compileExpr(cascade.Receiver)

	send := func(selector string, numArgs int) {
		if isSuper {
			c.emitSendSuper(selector, numArgs)
		} else {
			c.emitSend(selector, numArgs)
		}
	}

	for i, msg := range cascade.Messages {
		// Dup receiver for all but last message
		if i < len(cascade.Messages)-1 {
			c.frame.builder.Emit(vm.OpDUP)
		}

		// Compile message arguments and send
		switch msg.Type {
		case UnaryMsg:
			send(msg.Selector, 0)
		case BinaryMsg:
			c.compileExpr(msg.Arguments[0])
			send(msg.Selector, 1)
		case KeywordMsg:
			for _, arg := range msg.Arguments {
				c.compileExpr(arg)
			}
			send(msg.Selector, len(msg.Arguments))
		}

		// Pop result of non-last messages
		if i < len(cascade.Messages)-1 {
			c.frame.builder.Emit(vm.OpPOP)
		}
	}
}

// ---------------------------------------------------------------------------
// Block compilation
// ---------------------------------------------------------------------------

func (c *Compiler) compileBlock(block *Block) {
	outerFrame := c.frame

	// Build map of enclosing scope variables that this block can capture
	newNestingDepth := outerFrame.blockNestingDepth + 1
	newEnclosingBlockVars := make(map[string]int)
	if outerFrame.enclosingBlockVars != nil {
		for k, v := range outerFrame.enclosingBlockVars {
			newEnclosingBlockVars[k] = v
		}
	}
	if newNestingDepth == 1 {
		for k, v := range outerFrame.temps {
			newEnclosingBlockVars[k] = v
		}
		for k, v := range outerFrame.args {
			newEnclosingBlockVars[k] = v
		}
	} else if outerFrame.inBlock {
		for k, v := range outerFrame.temps {
			newEnclosingBlockVars[k] = v
		}
		for k, v := range outerFrame.args {
			newEnclosingBlockVars[k] = v
		}
	}

	// Identify which variables this block needs to capture
	varsToCapture := c.findCapturedVariables(block, newEnclosingBlockVars)
	numCaptures := len(varsToCapture)
	if numCaptures > 0xFF {
		c.errorf("block capture overflow: block captures %d variables, exceeds maximum of 255", numCaptures)
	}

	// Build capturedVars map for the inner block
	newCapturedVars := make(map[string]int)
	for i, varName := range varsToCapture {
		newCapturedVars[varName] = i
	}

	// Push new frame for the block
	c.pushFrame(&compilationFrame{
		builder:            vm.NewBytecodeBuilder(),
		literalMap:         make(map[interface{}]int),
		temps:              make(map[string]int),
		args:               make(map[string]int),
		numArgs:            len(block.Parameters),
		numTemps:           len(block.Parameters) + len(block.Temps),
		inBlock:            true,
		outerTemps:         make(map[string]int),
		outerArgs:          make(map[string]int),
		enclosingBlockVars: newEnclosingBlockVars,
		capturedVars:       newCapturedVars,
		blockNestingDepth:  newNestingDepth,
		cellInitialized:    make(map[string]bool),
	})

	// Parameters
	blockSeen := make(map[string]bool, len(block.Parameters)+len(block.Temps))
	for i, param := range block.Parameters {
		if blockSeen[param] {
			c.errorf("duplicate block parameter name %q", param)
		}
		blockSeen[param] = true
		c.frame.args[param] = i
	}

	// Temps
	for i, temp := range block.Temps {
		if blockSeen[temp] {
			c.errorf("duplicate block variable name %q", temp)
		}
		blockSeen[temp] = true
		c.frame.temps[temp] = c.frame.numArgs + i
	}

	// Pre-initialize cell variables
	for name, idx := range c.frame.temps {
		if c.cellVars[name] {
			c.checkSlotIndex(idx, "block temp", name)
			c.frame.builder.Emit(vm.OpPushNil)
			c.frame.builder.Emit(vm.OpMakeCell)
			c.frame.builder.EmitByte(vm.OpStoreTemp, byte(idx))
			c.frame.builder.Emit(vm.OpPOP)
			c.frame.cellInitialized[name] = true
		}
	}
	for name, idx := range c.frame.args {
		if c.cellVars[name] {
			c.checkSlotIndex(idx, "block arg", name)
			c.frame.builder.EmitByte(vm.OpPushTemp, byte(idx))
			c.frame.builder.Emit(vm.OpMakeCell)
			c.frame.builder.EmitByte(vm.OpStoreTemp, byte(idx))
			c.frame.builder.Emit(vm.OpPOP)
			c.frame.cellInitialized[name] = true
		}
	}

	// Compile block body
	c.compileStatements(block.Statements)

	if len(block.Statements) == 0 {
		c.frame.builder.Emit(vm.OpPushNil)
	}
	c.frame.builder.Emit(vm.OpBlockReturn)

	// Create BlockMethod with its own literals
	blockMethod := &vm.BlockMethod{
		Arity:       c.frame.numArgs,
		NumTemps:    c.frame.numTemps,
		NumCaptures: numCaptures,
		Bytecode:    c.frame.builder.Bytes(),
		Literals:    c.frame.literals,
		SourceMap:   c.frame.sourceMap,
	}

	// Pop back to outer frame
	c.popFrame()

	// Emit capture instructions in the outer frame
	for _, varName := range varsToCapture {
		if idx, ok := outerFrame.enclosingBlockVars[varName]; ok {
			if outerFrame.capturedVars != nil {
				if captIdx, ok := outerFrame.capturedVars[varName]; ok {
					c.checkSlotIndex(captIdx, "captured variable", varName)
					c.frame.builder.EmitByte(vm.OpPushCaptured, byte(captIdx))
					continue
				}
			}
			c.checkSlotIndex(idx, "block variable", varName)
			c.frame.builder.EmitByte(vm.OpPushTemp, byte(idx))
		} else if idx, ok := c.frame.temps[varName]; ok {
			c.checkSlotIndex(idx, "temp", varName)
			c.frame.builder.EmitByte(vm.OpPushTemp, byte(idx))
		} else if idx, ok := c.frame.args[varName]; ok {
			c.checkSlotIndex(idx, "argument", varName)
			c.frame.builder.EmitByte(vm.OpPushTemp, byte(idx))
		} else if idx, ok := c.frame.outerTemps[varName]; ok {
			c.checkSlotIndex(idx, "outer temp", varName)
			c.frame.builder.EmitByte(vm.OpPushHomeTemp, byte(idx))
		} else if idx, ok := c.frame.outerArgs[varName]; ok {
			c.checkSlotIndex(idx, "outer arg", varName)
			c.frame.builder.EmitByte(vm.OpPushHomeTemp, byte(idx))
		}
	}

	// Add block to list
	blockIdx := len(c.blocks)
	if blockIdx > 0xFFFF {
		c.errorf("block index overflow: method has more than 65535 blocks")
	}
	c.blocks = append(c.blocks, blockMethod)

	// Emit create block instruction
	c.frame.builder.EmitCreateBlock(uint16(blockIdx), uint8(numCaptures))
}

// findCapturedVariables analyzes a block's AST to find variables that need to be captured.
// Returns a list of variable names from enclosing block scopes that are referenced.
// Traversal is centralized in ast.Walk/Inspect (see ast_walk.go) so new node
// types cannot be silently missed.
func (c *Compiler) findCapturedVariables(block *Block, enclosingBlockVars map[string]int) []string {
	result := make([]string, 0)
	seen := make(map[string]bool)

	isLocal := func(name string) bool {
		for _, p := range block.Parameters {
			if p == name {
				return true
			}
		}
		for _, t := range block.Temps {
			if t == name {
				return true
			}
		}
		return false
	}
	record := func(name string) {
		if _, ok := enclosingBlockVars[name]; !ok {
			return
		}
		if isLocal(name) || seen[name] {
			return
		}
		seen[name] = true
		result = append(result, name)
	}

	for _, stmt := range block.Statements {
		if stmt == nil {
			continue
		}
		Inspect(stmt, func(n Node) bool {
			switch e := n.(type) {
			case *Variable:
				record(e.Name)
			case *Assignment:
				record(e.Variable)
			}
			return true
		})
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
		captured            bool
		assignedInNestedBlk bool // assigned in a block deeper than where defined
		blockDepth          int  // depth where defined
	}
	varInfos := make(map[string]*varInfo)

	// Traversal is centralized in ast.Walk/Inspect (see ast_walk.go); only
	// scope entry (Block) needs explicit recursion to track depth. This
	// walker's hand-rolled predecessor was missing *DynamicArray, so
	// mutations from blocks inside {…} literals were silently lost.
	//
	// Inlined control flow (keywordInlineParts/unaryInlineParts) is
	// scope-TRANSPARENT: those block bodies compile in the enclosing frame,
	// so they must be walked at the same depth — otherwise every loop
	// variable would be needlessly cell-boxed. The predicate is shared with
	// codegen's tryInlineControlFlow, which keeps the two in lockstep: a
	// block skipped here is guaranteed to be inlined there.
	var walkNode func(n Node, depth int)
	var walkStmts func(stmts []Stmt, depth int)

	walkNode = func(n Node, depth int) {
		Inspect(n, func(m Node) bool {
			switch e := m.(type) {
			case *Variable:
				// Accessing a variable from an outer scope (block or method level)
				if vi, ok := varInfos[e.Name]; ok && vi.blockDepth < depth {
					vi.captured = true
				}
			case *Assignment:
				// Assigning to an outer-scope variable: captured AND assigned
				if vi, ok := varInfos[e.Variable]; ok && vi.blockDepth < depth {
					vi.captured = true
					vi.assignedInNestedBlk = true
				}
			case *KeywordMessage:
				if parts, ok := keywordInlineParts(e); ok {
					for _, sub := range parts.exprs {
						walkNode(sub, depth)
					}
					for _, blk := range parts.blocks {
						// Inlinable blocks have no params/temps to register.
						walkStmts(blk.Statements, depth)
					}
					return false // handled recursion ourselves
				}
			case *UnaryMessage:
				if cond, ok := unaryInlineParts(e); ok {
					walkStmts(cond.Statements, depth)
					return false
				}
			case *Block:
				// Register this block's parameters and temps at depth+1,
				// then walk its body at that depth.
				for _, p := range e.Parameters {
					varInfos[p] = &varInfo{blockDepth: depth + 1}
				}
				for _, t := range e.Temps {
					varInfos[t] = &varInfo{blockDepth: depth + 1}
				}
				walkStmts(e.Statements, depth+1)
				return false // handled recursion ourselves
			}
			return true
		})
	}

	walkStmts = func(stmts []Stmt, depth int) {
		for _, stmt := range stmts {
			if stmt == nil {
				continue
			}
			walkNode(stmt, depth)
		}
	}

	// Start at method level (depth 0)
	for _, p := range method.Parameters {
		varInfos[p] = &varInfo{blockDepth: 0}
	}
	for _, t := range method.Temps {
		varInfos[t] = &varInfo{blockDepth: 0}
	}
	walkStmts(method.Statements, 0)

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

// formatErrors renders accumulated parse/compile errors as a multi-line
// message. With one error, the result reads "<kind>: <msg>"; with several
// it reads "<kind>:\n  <msg1>\n  <msg2>". Either form is far more readable
// than %v on a []string ("[line 1: foo line 1: bar]").
func formatErrors(kind string, errs []string) error {
	if len(errs) == 1 {
		return fmt.Errorf("%s: %s", kind, errs[0])
	}
	return fmt.Errorf("%s:\n  %s", kind, strings.Join(errs, "\n  "))
}

// Compile parses and compiles source code to a method.
func Compile(source string, selectors *vm.SelectorTable, symbols *vm.SymbolTable, registry *vm.ObjectRegistry, instVars []string) (*vm.CompiledMethod, error) {
	parser := NewParser(source)
	method := parser.ParseMethod()
	if len(parser.Errors()) > 0 {
		return nil, formatErrors("parse errors", parser.Errors())
	}

	// Run semantic analysis (warnings only, don't fail)
	warnings := Analyze(method, nil)
	_ = warnings // In the future, could log these or return them

	compiler := NewCompiler(selectors, symbols, registry)
	// Without the receiver class's instance variables, every ivar reference
	// would resolve to a (nil) global — the live-coding/IDE compile path must
	// pass them so `^x` becomes PUSH_IVAR, not PUSH_GLOBAL.
	if len(instVars) > 0 {
		compiler.SetInstanceVars(instVars)
	}
	compiled := compiler.CompileMethod(method)
	if len(compiler.Errors()) > 0 {
		return nil, formatErrors("compile errors", compiler.Errors())
	}

	return compiled, nil
}

// Install CompileDoIt as the VM's doIt compiler: any program linking the
// compiler package (everything that calls UseGoCompiler) gets the AST-level
// eval path automatically.
func init() {
	vm.RegisterDoItCompiler(CompileDoIt)
}

// CompileDoIt parses a statement sequence (with optional leading | temps |)
// and compiles it as a synthetic doIt method whose value is the final
// expression. This is the ONLY supported way to compile eval input: the
// last statement is wrapped in a Return at the AST level, so no textual
// splicing can corrupt string literals or block bodies (the old dot-scanner
// turned `3 + 4. 'x.y'` into a method whose string literal was "x. ^y").
//
// Returns the compiled method, semantic warnings (advisory — e.g.
// possibly-undefined variables), and any parse/compile error. The full
// method compilation path runs, including cell-variable analysis, so blocks
// with mutable captures behave identically to method-compiled code.
func CompileDoIt(source string, selectors *vm.SelectorTable, symbols *vm.SymbolTable, registry *vm.ObjectRegistry) (*vm.CompiledMethod, []string, error) {
	method, err := ParseDoIt(source)
	if err != nil {
		return nil, nil, err
	}

	warnings := Analyze(method, nil)

	compiler := NewCompiler(selectors, symbols, registry)
	compiled := compiler.CompileMethod(method)
	if len(compiler.Errors()) > 0 {
		return nil, warnings, formatErrors("compile errors", compiler.Errors())
	}
	compiled.Source = source
	return compiled, warnings, nil
}

// ParseDoIt parses eval input — an optional | temps | declaration followed
// by a statement sequence — into a synthetic doIt MethodDef whose last
// statement is wrapped in an explicit return.
func ParseDoIt(source string) (*MethodDef, error) {
	p := NewParser(source)
	startPos := p.curToken.Pos

	var temps []string
	var tempTypes []*TypeExpr
	if p.curTokenIs(TokenBar) {
		temps, tempTypes = p.parseTemporaries()
	}

	stmts := p.ParseStatements()
	if errs := p.Errors(); len(errs) > 0 {
		return nil, formatErrors("parse errors", errs)
	}

	// The doIt's value is its final expression: wrap the last statement in a
	// Return unless it already is one.
	if len(stmts) > 0 {
		if es, ok := stmts[len(stmts)-1].(*ExprStmt); ok {
			stmts[len(stmts)-1] = &Return{SpanVal: es.SpanVal, Value: es.Expr}
		}
	}

	return &MethodDef{
		SpanVal:    MakeSpan(startPos, p.curToken.Pos),
		Selector:   "doIt",
		Temps:      temps,
		TempTypes:  tempTypes,
		Statements: stmts,
	}, nil
}

// CompileExpr parses and compiles an expression.
func CompileExpr(source string, selectors *vm.SelectorTable, symbols *vm.SymbolTable, registry *vm.ObjectRegistry) (*vm.CompiledMethod, error) {
	parser := NewParser(source)
	expr := parser.ParseExpression()
	if len(parser.Errors()) > 0 {
		return nil, formatErrors("parse errors", parser.Errors())
	}
	// Require the input to be fully consumed. Otherwise trailing tokens are
	// silently dropped (`3 4` compiled to `3`, `3 + 4 5` to `7`), masking real
	// typos and lexer mis-tokenizations.
	parser.skipTrailingPeriods()
	if !parser.curTokenIs(TokenEOF) {
		return nil, fmt.Errorf("parse errors: unexpected trailing token %q", parser.curToken.Literal)
	}

	compiler := NewCompiler(selectors, symbols, registry)
	compiled := compiler.CompileExpression(expr)
	if len(compiler.Errors()) > 0 {
		return nil, formatErrors("compile errors", compiler.Errors())
	}

	return compiled, nil
}

// ParseAndCompileSourceFile parses a Trashtalk-style source file and returns the SourceFile AST.
func ParseSourceFileFromString(source string) (*SourceFile, error) {
	parser := NewParser(source)
	sf := parser.ParseSourceFile()
	if len(parser.Errors()) > 0 {
		return nil, formatErrors("parse errors", parser.Errors())
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
		return nil, formatErrors("compile errors", compiler.Errors())
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
		return nil, formatErrors("compile errors", comp.Errors())
	}
	return compiled, nil
}
