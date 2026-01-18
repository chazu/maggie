package bytecode

import (
	"fmt"

	"github.com/chazu/procyon/pkg/parser"
)

// CompilerContext provides context for block compilation.
// This includes information about the enclosing scope for capture analysis.
type CompilerContext struct {
	// The enclosing method's variables
	MethodParams []string // Parameters of the enclosing method
	MethodLocals []string // Local variables in the enclosing method
	InstanceVars []string // Instance variables of the class
	ClassVars    []string // Class variables (future)

	// The instance ID for ivar access (filled at runtime)
	InstanceID string

	// Parent block context (for nested blocks)
	Parent *CompilerContext

	// Depth for nested blocks
	Depth int
}

// Compiler converts parser AST to bytecode.
type Compiler struct {
	chunk   *Chunk
	context *CompilerContext

	// Constant pool management (deduplication)
	constantMap map[string]uint16

	// Variable slot allocation
	paramSlots   map[string]uint8 // Block param name -> slot
	localSlots   map[string]uint8 // Local var name -> slot
	captureSlots map[string]uint8 // Captured var name -> capture index

	// Track which names are defined locally in this block
	locallyDefined map[string]bool

	// Source tracking for debug info
	currentLine   uint32
	currentColumn uint16

	// Error collection
	errors []error
}

// CompileBlock compiles a BlockExpr to bytecode.
// This is the main entry point for block compilation.
func CompileBlock(block *parser.BlockExpr, ctx *CompilerContext) (*Chunk, error) {
	c := &Compiler{
		chunk: &Chunk{
			Version:   BytecodeVersion,
			Code:      make([]byte, 0, 256),
			Constants: make([]string, 0, 16),
		},
		context:        ctx,
		constantMap:    make(map[string]uint16),
		paramSlots:     make(map[string]uint8),
		localSlots:     make(map[string]uint8),
		captureSlots:   make(map[string]uint8),
		locallyDefined: make(map[string]bool),
	}

	// Analyze captures first to identify free variables
	captures := c.analyzeCaptures(block)

	// Allocate parameter slots
	for i, param := range block.Params {
		c.paramSlots[param] = uint8(i)
		c.locallyDefined[param] = true
	}
	c.chunk.ParamCount = uint8(len(block.Params))
	c.chunk.ParamNames = append([]string{}, block.Params...)

	// Set up capture descriptors
	for i, cap := range captures {
		c.captureSlots[cap.Name] = uint8(i)
		c.chunk.CaptureInfo = append(c.chunk.CaptureInfo, cap)
	}
	if len(captures) > 0 {
		c.chunk.Flags |= ChunkFlagHasCaptures
	}

	// Compile statements
	for _, stmt := range block.Statements {
		if err := c.compileStatement(stmt); err != nil {
			return nil, err
		}
	}

	// Ensure block returns something
	if len(c.chunk.Code) == 0 || !c.endsWithReturn() {
		c.emit(OpReturnNil)
	}

	// Set local count
	c.chunk.LocalCount = uint8(len(c.localSlots))

	// Set VarNames for debugging
	c.chunk.VarNames = make([]string, len(c.localSlots))
	for name, slot := range c.localSlots {
		if int(slot) < len(c.chunk.VarNames) {
			c.chunk.VarNames[slot] = name
		}
	}

	if len(c.errors) > 0 {
		return nil, c.errors[0]
	}

	return c.chunk, nil
}

// endsWithReturn checks if the code ends with a return instruction.
func (c *Compiler) endsWithReturn() bool {
	if len(c.chunk.Code) == 0 {
		return false
	}
	lastOp := Opcode(c.chunk.Code[len(c.chunk.Code)-1])
	return lastOp.IsReturn()
}

// analyzeCaptures identifies variables that need to be captured.
// A captured variable is one that is:
// 1. Referenced in the block
// 2. Not defined in the block (not a param or local)
// 3. Defined in an enclosing scope
func (c *Compiler) analyzeCaptures(block *parser.BlockExpr) []CaptureDescriptor {
	// Build set of locally defined names
	localDefs := make(map[string]bool)
	for _, p := range block.Params {
		localDefs[p] = true
	}

	// Find locals declared in the block
	for _, stmt := range block.Statements {
		if lvd, ok := stmt.(*parser.LocalVarDecl); ok {
			for _, name := range lvd.Names {
				localDefs[name] = true
			}
		}
		// Also check assignments that create new locals
		if assign, ok := stmt.(*parser.Assignment); ok {
			// Assignment creates local if not already defined elsewhere
			if !c.isDefinedInContext(assign.Target) {
				localDefs[assign.Target] = true
			}
		}
	}

	// Find all referenced variables
	referenced := make(map[string]bool)
	for _, stmt := range block.Statements {
		c.collectReferences(stmt, referenced)
	}

	// Filter to captures (referenced but not locally defined)
	var captures []CaptureDescriptor
	seen := make(map[string]bool)

	for name := range referenced {
		if localDefs[name] || seen[name] {
			continue
		}

		// Determine source of the capture
		source, slotIndex, ok := c.resolveCapture(name)
		if !ok {
			continue // Not a capturable variable
		}

		seen[name] = true
		captures = append(captures, CaptureDescriptor{
			Name:      name,
			Source:    source,
			SlotIndex: slotIndex,
		})
	}

	return captures
}

// resolveCapture determines where a captured variable comes from.
// Returns (source, slotIndex, found).
func (c *Compiler) resolveCapture(name string) (VarSource, uint8, bool) {
	if c.context == nil {
		return 0, 0, false
	}

	// Check instance variables
	for i, ivar := range c.context.InstanceVars {
		if ivar == name {
			return VarSourceIVar, uint8(i), true
		}
	}

	// Check method parameters
	for i, param := range c.context.MethodParams {
		if param == name {
			return VarSourceParam, uint8(i), true
		}
	}

	// Check method locals
	for i, local := range c.context.MethodLocals {
		if local == name {
			return VarSourceLocal, uint8(i), true
		}
	}

	// Check parent context captures (for nested blocks)
	if c.context.Parent != nil {
		// Create a temporary compiler with parent context to check
		parentCtx := c.context.Parent
		for i, param := range parentCtx.MethodParams {
			if param == name {
				return VarSourceCapture, uint8(i), true
			}
		}
	}

	return 0, 0, false
}

// isDefinedInContext checks if a name is defined in the enclosing context.
func (c *Compiler) isDefinedInContext(name string) bool {
	if c.context == nil {
		return false
	}

	// Check instance variables
	for _, ivar := range c.context.InstanceVars {
		if ivar == name {
			return true
		}
	}

	// Check method parameters
	for _, param := range c.context.MethodParams {
		if param == name {
			return true
		}
	}

	// Check method locals
	for _, local := range c.context.MethodLocals {
		if local == name {
			return true
		}
	}

	return false
}

// collectReferences walks the AST collecting variable references.
func (c *Compiler) collectReferences(node interface{}, refs map[string]bool) {
	switch n := node.(type) {
	case *parser.Identifier:
		refs[n.Name] = true

	case *parser.Assignment:
		refs[n.Target] = true
		c.collectReferences(n.Value, refs)

	case *parser.Return:
		if n.Value != nil {
			c.collectReferences(n.Value, refs)
		}

	case *parser.ExprStmt:
		c.collectReferences(n.Expr, refs)

	case *parser.BinaryExpr:
		c.collectReferences(n.Left, refs)
		c.collectReferences(n.Right, refs)

	case *parser.ComparisonExpr:
		c.collectReferences(n.Left, refs)
		c.collectReferences(n.Right, refs)

	case *parser.MessageSend:
		if !n.IsSelf {
			c.collectReferences(n.Receiver, refs)
		}
		for _, arg := range n.Args {
			c.collectReferences(arg, refs)
		}

	case *parser.IfExpr:
		c.collectReferences(n.Condition, refs)
		for _, stmt := range n.TrueBlock {
			c.collectReferences(stmt, refs)
		}
		for _, stmt := range n.FalseBlock {
			c.collectReferences(stmt, refs)
		}

	case *parser.WhileExpr:
		c.collectReferences(n.Condition, refs)
		for _, stmt := range n.Body {
			c.collectReferences(stmt, refs)
		}

	case *parser.IfNilExpr:
		c.collectReferences(n.Subject, refs)
		for _, stmt := range n.NilBlock {
			c.collectReferences(stmt, refs)
		}
		for _, stmt := range n.NotNilBlock {
			c.collectReferences(stmt, refs)
		}

	case *parser.IterationExpr:
		c.collectReferences(n.Collection, refs)
		// Note: n.IterVar is locally defined in the iteration
		for _, stmt := range n.Body {
			c.collectReferences(stmt, refs)
		}

	case *parser.BlockExpr:
		// For nested blocks, collect references but exclude params
		for _, stmt := range n.Statements {
			c.collectReferences(stmt, refs)
		}
		// Remove block params from refs
		for _, p := range n.Params {
			delete(refs, p)
		}

	case *parser.JSONPrimitiveExpr:
		c.collectReferences(n.Receiver, refs)
		for _, arg := range n.Args {
			c.collectReferences(arg, refs)
		}

	case *parser.ClassPrimitiveExpr:
		for _, arg := range n.Args {
			c.collectReferences(arg, refs)
		}
	}
}

// compileStatement compiles a single statement.
func (c *Compiler) compileStatement(stmt parser.Statement) error {
	switch s := stmt.(type) {
	case *parser.LocalVarDecl:
		// Allocate slots for local variables
		for _, name := range s.Names {
			if _, exists := c.localSlots[name]; !exists {
				slot := uint8(len(c.localSlots))
				c.localSlots[name] = slot
				c.locallyDefined[name] = true
			}
		}
		return nil

	case *parser.Assignment:
		return c.compileAssignment(s)

	case *parser.Return:
		return c.compileReturn(s)

	case *parser.ExprStmt:
		if err := c.compileExpr(s.Expr); err != nil {
			return err
		}
		c.emit(OpPop) // Discard result
		return nil

	case *parser.IfExpr:
		return c.compileIf(s)

	case *parser.WhileExpr:
		return c.compileWhile(s)

	case *parser.IfNilExpr:
		return c.compileIfNil(s)

	case *parser.IterationExpr:
		return c.compileIteration(s)

	case *parser.MessageSend:
		if err := c.compileMessageSend(s); err != nil {
			return err
		}
		c.emit(OpPop) // Discard result
		return nil

	default:
		return fmt.Errorf("unsupported statement type: %T", stmt)
	}
}

// compileExpr compiles an expression, leaving result on stack.
func (c *Compiler) compileExpr(expr parser.Expr) error {
	switch e := expr.(type) {
	case *parser.NumberLit:
		return c.compileNumber(e)

	case *parser.StringLit:
		return c.compileString(e)

	case *parser.Identifier:
		return c.compileIdentifier(e)

	case *parser.BinaryExpr:
		return c.compileBinary(e)

	case *parser.ComparisonExpr:
		return c.compileComparison(e)

	case *parser.MessageSend:
		return c.compileMessageSend(e)

	case *parser.BlockExpr:
		return c.compileNestedBlock(e)

	case *parser.IfExpr:
		return c.compileIfExpr(e)

	case *parser.JSONPrimitiveExpr:
		return c.compileJSONPrimitive(e)

	case *parser.ClassPrimitiveExpr:
		return c.compileClassPrimitive(e)

	case *parser.IterationExprAsValue:
		return c.compileIterationAsValue(e.Iteration)

	default:
		return fmt.Errorf("unsupported expression type: %T", expr)
	}
}

// compileNumber compiles a number literal.
func (c *Compiler) compileNumber(n *parser.NumberLit) error {
	switch n.Value {
	case "0":
		c.emit(OpConstZero)
	case "1":
		c.emit(OpConstOne)
	default:
		c.emitConstant(n.Value)
	}
	return nil
}

// compileString compiles a string literal.
func (c *Compiler) compileString(s *parser.StringLit) error {
	if s.Value == "" {
		c.emit(OpConstEmpty)
	} else {
		c.emitConstant(s.Value)
	}
	return nil
}

// compileIdentifier compiles a variable reference.
func (c *Compiler) compileIdentifier(id *parser.Identifier) error {
	name := id.Name

	// Special cases
	if name == "nil" {
		c.emit(OpConstNil)
		return nil
	}
	if name == "true" {
		c.emit(OpConstTrue)
		return nil
	}
	if name == "false" {
		c.emit(OpConstFalse)
		return nil
	}
	if name == "self" {
		// Push self placeholder - resolved at runtime
		c.emitConstant("self")
		return nil
	}

	// Check block parameters first
	if slot, ok := c.paramSlots[name]; ok {
		c.emitWithOperand(OpLoadParam, slot)
		return nil
	}

	// Check local variables
	if slot, ok := c.localSlots[name]; ok {
		c.emitWithOperand(OpLoadLocal, slot)
		return nil
	}

	// Check captures
	if slot, ok := c.captureSlots[name]; ok {
		c.emitWithOperand(OpLoadCapture, slot)
		return nil
	}

	// Check instance variables (via constant pool lookup)
	if c.isInstanceVar(name) {
		idx := c.addConstant(name)
		c.emit(OpLoadIVar)
		c.emitUint16(idx)
		return nil
	}

	// Unknown variable - treat as constant (class name, etc.)
	c.emitConstant(name)
	return nil
}

// compileAssignment compiles an assignment statement.
func (c *Compiler) compileAssignment(a *parser.Assignment) error {
	// Compile the value
	if err := c.compileExpr(a.Value); err != nil {
		return err
	}

	name := a.Target

	// Check if storing to existing local
	if slot, ok := c.localSlots[name]; ok {
		c.emitWithOperand(OpStoreLocal, slot)
		return nil
	}

	// Check if storing to capture
	if slot, ok := c.captureSlots[name]; ok {
		c.emitWithOperand(OpStoreCapture, slot)
		return nil
	}

	// Check if storing to instance variable
	if c.isInstanceVar(name) {
		idx := c.addConstant(name)
		c.emit(OpStoreIVar)
		c.emitUint16(idx)
		return nil
	}

	// New local variable
	slot := uint8(len(c.localSlots))
	c.localSlots[name] = slot
	c.locallyDefined[name] = true
	c.emitWithOperand(OpStoreLocal, slot)
	return nil
}

// compileReturn compiles a return statement.
func (c *Compiler) compileReturn(r *parser.Return) error {
	if r.Value == nil {
		c.emit(OpReturnNil)
		return nil
	}

	if err := c.compileExpr(r.Value); err != nil {
		return err
	}
	c.emit(OpReturn)
	return nil
}

// compileBinary compiles a binary expression.
func (c *Compiler) compileBinary(b *parser.BinaryExpr) error {
	if err := c.compileExpr(b.Left); err != nil {
		return err
	}
	if err := c.compileExpr(b.Right); err != nil {
		return err
	}

	switch b.Op {
	case "+":
		c.emit(OpAdd)
	case "-":
		c.emit(OpSub)
	case "*":
		c.emit(OpMul)
	case "/":
		c.emit(OpDiv)
	case "%":
		c.emit(OpMod)
	case ",":
		c.emit(OpConcat)
	default:
		return fmt.Errorf("unsupported binary operator: %s", b.Op)
	}
	return nil
}

// compileComparison compiles a comparison expression.
func (c *Compiler) compileComparison(comp *parser.ComparisonExpr) error {
	if err := c.compileExpr(comp.Left); err != nil {
		return err
	}
	if err := c.compileExpr(comp.Right); err != nil {
		return err
	}

	switch comp.Op {
	case "==":
		c.emit(OpEq)
	case "!=":
		c.emit(OpNe)
	case "<":
		c.emit(OpLt)
	case "<=":
		c.emit(OpLe)
	case ">":
		c.emit(OpGt)
	case ">=":
		c.emit(OpGe)
	default:
		return fmt.Errorf("unsupported comparison operator: %s", comp.Op)
	}
	return nil
}

// compileMessageSend compiles a message send expression.
func (c *Compiler) compileMessageSend(m *parser.MessageSend) error {
	// Compile receiver
	if m.IsSelf {
		c.emitConstant("self")
	} else {
		if err := c.compileExpr(m.Receiver); err != nil {
			return err
		}
	}

	// Compile arguments
	for _, arg := range m.Args {
		if err := c.compileExpr(arg); err != nil {
			return err
		}
	}

	// Emit send instruction
	selIdx := c.addConstant(m.Selector)
	if m.IsSelf {
		c.emit(OpSendSelf)
	} else {
		c.emit(OpSend)
	}
	c.emitUint16(selIdx)
	c.emitByte(byte(len(m.Args)))

	return nil
}

// compileIf compiles an if statement.
func (c *Compiler) compileIf(ifExpr *parser.IfExpr) error {
	// Compile condition
	if err := c.compileExpr(ifExpr.Condition); err != nil {
		return err
	}

	if len(ifExpr.TrueBlock) == 0 && len(ifExpr.FalseBlock) > 0 {
		// ifFalse: only - jump if truthy
		falseJump := c.emitJump(OpJumpTrue)

		// Compile false block
		for _, stmt := range ifExpr.FalseBlock {
			if err := c.compileStatement(stmt); err != nil {
				return err
			}
		}

		c.patchJump(falseJump)
		return nil
	}

	// Normal ifTrue: [ifFalse:]
	falseJump := c.emitJump(OpJumpFalse)

	// Compile true block
	for _, stmt := range ifExpr.TrueBlock {
		if err := c.compileStatement(stmt); err != nil {
			return err
		}
	}

	if len(ifExpr.FalseBlock) > 0 {
		// Jump over false block
		endJump := c.emitJump(OpJump)

		// Patch false jump to here
		c.patchJump(falseJump)

		// Compile false block
		for _, stmt := range ifExpr.FalseBlock {
			if err := c.compileStatement(stmt); err != nil {
				return err
			}
		}

		c.patchJump(endJump)
	} else {
		c.patchJump(falseJump)
	}

	return nil
}

// compileIfExpr compiles an if expression (returns value).
func (c *Compiler) compileIfExpr(ifExpr *parser.IfExpr) error {
	// Compile condition
	if err := c.compileExpr(ifExpr.Condition); err != nil {
		return err
	}

	falseJump := c.emitJump(OpJumpFalse)

	// Compile true block - last expression is the value
	if len(ifExpr.TrueBlock) > 0 {
		for i, stmt := range ifExpr.TrueBlock {
			if i < len(ifExpr.TrueBlock)-1 {
				if err := c.compileStatement(stmt); err != nil {
					return err
				}
			} else {
				// Last statement should leave value on stack
				if exprStmt, ok := stmt.(*parser.ExprStmt); ok {
					if err := c.compileExpr(exprStmt.Expr); err != nil {
						return err
					}
				} else {
					if err := c.compileStatement(stmt); err != nil {
						return err
					}
					c.emit(OpConstNil) // Default value
				}
			}
		}
	} else {
		c.emit(OpConstNil)
	}

	endJump := c.emitJump(OpJump)
	c.patchJump(falseJump)

	// Compile false block
	if len(ifExpr.FalseBlock) > 0 {
		for i, stmt := range ifExpr.FalseBlock {
			if i < len(ifExpr.FalseBlock)-1 {
				if err := c.compileStatement(stmt); err != nil {
					return err
				}
			} else {
				if exprStmt, ok := stmt.(*parser.ExprStmt); ok {
					if err := c.compileExpr(exprStmt.Expr); err != nil {
						return err
					}
				} else {
					if err := c.compileStatement(stmt); err != nil {
						return err
					}
					c.emit(OpConstNil)
				}
			}
		}
	} else {
		c.emit(OpConstNil)
	}

	c.patchJump(endJump)
	return nil
}

// compileWhile compiles a while loop.
func (c *Compiler) compileWhile(w *parser.WhileExpr) error {
	// Loop start
	loopStart := c.currentOffset()

	// Compile condition
	if err := c.compileExpr(w.Condition); err != nil {
		return err
	}

	// Jump out if false
	exitJump := c.emitJump(OpJumpFalse)

	// Compile body
	for _, stmt := range w.Body {
		if err := c.compileStatement(stmt); err != nil {
			return err
		}
	}

	// Jump back to condition
	c.emitLoop(loopStart)

	// Patch exit jump
	c.patchJump(exitJump)

	return nil
}

// compileIfNil compiles an ifNil/ifNotNil statement.
func (c *Compiler) compileIfNil(n *parser.IfNilExpr) error {
	// Compile subject
	if err := c.compileExpr(n.Subject); err != nil {
		return err
	}

	if len(n.NilBlock) > 0 && len(n.NotNilBlock) == 0 {
		// ifNil: only
		notNilJump := c.emitJump(OpJumpNotNil)

		for _, stmt := range n.NilBlock {
			if err := c.compileStatement(stmt); err != nil {
				return err
			}
		}

		c.patchJump(notNilJump)
		return nil
	}

	if len(n.NilBlock) == 0 && len(n.NotNilBlock) > 0 {
		// ifNotNil: only
		nilJump := c.emitJump(OpJumpNil)

		// If binding variable, store subject value
		if n.BindingVar != "" {
			slot := uint8(len(c.localSlots))
			c.localSlots[n.BindingVar] = slot
			c.locallyDefined[n.BindingVar] = true
			// Subject is already on stack, dup and store
			c.emit(OpDup)
			c.emitWithOperand(OpStoreLocal, slot)
		}

		for _, stmt := range n.NotNilBlock {
			if err := c.compileStatement(stmt); err != nil {
				return err
			}
		}

		c.patchJump(nilJump)
		return nil
	}

	// Both ifNil: and ifNotNil:
	c.emit(OpDup) // Keep subject for both branches
	notNilJump := c.emitJump(OpJumpNotNil)

	// Nil block
	c.emit(OpPop) // Discard duplicated subject
	for _, stmt := range n.NilBlock {
		if err := c.compileStatement(stmt); err != nil {
			return err
		}
	}

	endJump := c.emitJump(OpJump)
	c.patchJump(notNilJump)

	// Not nil block
	if n.BindingVar != "" {
		slot := uint8(len(c.localSlots))
		c.localSlots[n.BindingVar] = slot
		c.locallyDefined[n.BindingVar] = true
		c.emitWithOperand(OpStoreLocal, slot)
	} else {
		c.emit(OpPop) // Discard subject
	}

	for _, stmt := range n.NotNilBlock {
		if err := c.compileStatement(stmt); err != nil {
			return err
		}
	}

	c.patchJump(endJump)
	return nil
}

// compileIteration compiles a do:/collect:/select: iteration.
func (c *Compiler) compileIteration(iter *parser.IterationExpr) error {
	// Compile collection
	if err := c.compileExpr(iter.Collection); err != nil {
		return err
	}

	// For collect/select, we need a result array
	if iter.Kind == "collect" || iter.Kind == "select" {
		c.emit(OpConstEmpty) // Result array starts empty
	}

	// Store collection in a temp local
	collSlot := uint8(len(c.localSlots))
	c.localSlots["__collection__"] = collSlot
	c.emitWithOperand(OpStoreLocal, collSlot)

	// Iterator index
	idxSlot := uint8(len(c.localSlots))
	c.localSlots["__idx__"] = idxSlot
	c.emit(OpConstZero)
	c.emitWithOperand(OpStoreLocal, idxSlot)

	// Loop start
	loopStart := c.currentOffset()

	// Check index < length
	c.emitWithOperand(OpLoadLocal, idxSlot)
	c.emitWithOperand(OpLoadLocal, collSlot)
	c.emit(OpArrayLen)
	c.emit(OpLt)

	exitJump := c.emitJump(OpJumpFalse)

	// Get current item
	c.emitWithOperand(OpLoadLocal, collSlot)
	c.emitWithOperand(OpLoadLocal, idxSlot)
	c.emit(OpArrayAt)

	// Store in iter var
	iterSlot := uint8(len(c.localSlots))
	c.localSlots[iter.IterVar] = iterSlot
	c.locallyDefined[iter.IterVar] = true
	c.emitWithOperand(OpStoreLocal, iterSlot)

	// Compile body
	for _, stmt := range iter.Body {
		if err := c.compileStatement(stmt); err != nil {
			return err
		}
	}

	// Increment index
	c.emitWithOperand(OpLoadLocal, idxSlot)
	c.emit(OpConstOne)
	c.emit(OpAdd)
	c.emitWithOperand(OpStoreLocal, idxSlot)

	// Loop back
	c.emitLoop(loopStart)

	c.patchJump(exitJump)

	return nil
}

// compileIterationAsValue compiles iteration that returns a value.
func (c *Compiler) compileIterationAsValue(iter *parser.IterationExpr) error {
	// For now, compile as regular iteration
	// In the future, collect: and select: will build result arrays
	return c.compileIteration(iter)
}

// compileNestedBlock compiles a nested block expression.
func (c *Compiler) compileNestedBlock(block *parser.BlockExpr) error {
	// Create nested compiler context
	nestedCtx := &CompilerContext{
		MethodParams: c.context.MethodParams,
		MethodLocals: c.context.MethodLocals,
		InstanceVars: c.context.InstanceVars,
		ClassVars:    c.context.ClassVars,
		InstanceID:   c.context.InstanceID,
		Parent:       c.context,
		Depth:        c.context.Depth + 1,
	}

	// Compile nested block
	nestedChunk, err := CompileBlock(block, nestedCtx)
	if err != nil {
		return err
	}

	// Serialize the nested chunk
	chunkBytes, err := nestedChunk.Serialize()
	if err != nil {
		return err
	}

	// Store serialized chunk in constants
	chunkIdx := c.addConstant(string(chunkBytes))

	// Push captured values onto stack
	for _, cap := range nestedChunk.CaptureInfo {
		if err := c.compileIdentifier(&parser.Identifier{Name: cap.Name}); err != nil {
			return err
		}
	}

	// Create block object
	c.emit(OpMakeBlock)
	c.emitUint16(chunkIdx)
	c.emitByte(byte(len(nestedChunk.CaptureInfo)))

	return nil
}

// compileJSONPrimitive compiles a JSON primitive operation.
func (c *Compiler) compileJSONPrimitive(jp *parser.JSONPrimitiveExpr) error {
	// Compile receiver
	if err := c.compileExpr(jp.Receiver); err != nil {
		return err
	}

	// Compile arguments
	for _, arg := range jp.Args {
		if err := c.compileExpr(arg); err != nil {
			return err
		}
	}

	// Emit appropriate opcode
	switch jp.Operation {
	case "arrayPush":
		c.emit(OpArrayPush)
	case "arrayAt":
		c.emit(OpArrayAt)
	case "arrayLength":
		c.emit(OpArrayLen)
	case "arrayFirst":
		c.emit(OpArrayFirst)
	case "arrayLast":
		c.emit(OpArrayLast)
	case "arrayAtPut":
		c.emit(OpArrayAtPut)
	case "objectAt":
		c.emit(OpObjectAt)
	case "objectAtPut":
		c.emit(OpObjectAtPut)
	case "objectKeys":
		c.emit(OpObjectKeys)
	case "objectValues":
		c.emit(OpObjectValues)
	default:
		// For unsupported operations, fall back to message send
		selIdx := c.addConstant(jp.Operation)
		c.emit(OpSend)
		c.emitUint16(selIdx)
		c.emitByte(byte(len(jp.Args)))
	}

	return nil
}

// compileClassPrimitive compiles a class primitive operation.
func (c *Compiler) compileClassPrimitive(cp *parser.ClassPrimitiveExpr) error {
	// Compile arguments
	for _, arg := range cp.Args {
		if err := c.compileExpr(arg); err != nil {
			return err
		}
	}

	// For now, emit as message send to class
	classIdx := c.addConstant(cp.ClassName)
	selIdx := c.addConstant(cp.Operation)
	c.emit(OpSendClass)
	c.emitUint16(classIdx)
	c.emitUint16(selIdx)
	c.emitByte(byte(len(cp.Args)))

	return nil
}

// isInstanceVar checks if a name is an instance variable.
func (c *Compiler) isInstanceVar(name string) bool {
	if c.context == nil {
		return false
	}
	for _, ivar := range c.context.InstanceVars {
		if ivar == name {
			return true
		}
	}
	return false
}

// Emit helpers

func (c *Compiler) emit(op Opcode) int {
	offset := len(c.chunk.Code)
	c.chunk.Code = append(c.chunk.Code, byte(op))
	return offset
}

func (c *Compiler) emitByte(b byte) {
	c.chunk.Code = append(c.chunk.Code, b)
}

func (c *Compiler) emitWithOperand(op Opcode, operand uint8) int {
	offset := len(c.chunk.Code)
	c.chunk.Code = append(c.chunk.Code, byte(op), operand)
	return offset
}

func (c *Compiler) emitUint16(val uint16) {
	c.chunk.Code = append(c.chunk.Code, byte(val>>8), byte(val))
}

func (c *Compiler) emitConstant(value string) {
	idx := c.addConstant(value)
	c.emit(OpConst)
	c.emitUint16(idx)
}

func (c *Compiler) addConstant(value string) uint16 {
	if idx, ok := c.constantMap[value]; ok {
		return idx
	}
	idx := uint16(len(c.chunk.Constants))
	c.chunk.Constants = append(c.chunk.Constants, value)
	c.constantMap[value] = idx
	return idx
}

func (c *Compiler) currentOffset() int {
	return len(c.chunk.Code)
}

func (c *Compiler) emitJump(op Opcode) int {
	c.emit(op)
	offset := len(c.chunk.Code)
	c.chunk.Code = append(c.chunk.Code, 0xFF, 0xFF) // Placeholder
	return offset
}

func (c *Compiler) patchJump(placeholderOffset int) {
	// Calculate jump distance from after the placeholder
	jumpFrom := placeholderOffset + 2
	jumpTo := len(c.chunk.Code)
	delta := jumpTo - jumpFrom

	c.chunk.Code[placeholderOffset] = byte(delta >> 8)
	c.chunk.Code[placeholderOffset+1] = byte(delta)
}

func (c *Compiler) emitLoop(loopStart int) {
	// Jump goes backward
	c.emit(OpJump)
	jumpFrom := len(c.chunk.Code) + 2 // After the offset bytes
	delta := loopStart - jumpFrom

	c.chunk.Code = append(c.chunk.Code, byte(delta>>8), byte(delta))
}

// AddSourceLocation adds a source location mapping for debugging.
func (c *Compiler) AddSourceLocation(line uint32, column uint16) {
	offset := uint32(len(c.chunk.Code))
	c.chunk.AddSourceLocation(offset, line, column)
	c.currentLine = line
	c.currentColumn = column
}
