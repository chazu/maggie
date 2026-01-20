package vm

// ---------------------------------------------------------------------------
// CompiledMethod: Bytecode-based method implementation
// ---------------------------------------------------------------------------

// CompiledMethod represents a compiled Maggie method.
// It stores bytecode, literals, and metadata needed for execution.
type CompiledMethod struct {
	// Method identity
	selector      int    // selector ID
	class         *Class // defining class (nil for detached methods)
	name          string // method name (for debugging)
	IsClassMethod bool   // true if this is a class-side method

	// Method signature
	Arity    int // number of arguments (not including self)
	NumTemps int // total temporaries (arguments + locals)

	// Compiled code
	Literals []Value // constant pool (numbers, strings, symbols, classes)
	Bytecode []byte  // the bytecode instructions

	// Nested blocks
	Blocks []*BlockMethod // block methods referenced by CREATE_BLOCK

	// Debugging support
	Source    string      // original source text
	SourceMap []SourceLoc // bytecode offset → source position
}

// SourceLoc maps a bytecode offset to source position.
type SourceLoc struct {
	Offset int // bytecode offset
	Line   int // 1-based line number
	Column int // 1-based column number
}

// BlockMethod represents a compiled block (closure).
type BlockMethod struct {
	// Block signature
	Arity       int // number of block arguments
	NumTemps    int // total temporaries (arguments + locals)
	NumCaptures int // number of captured variables

	// Compiled code
	Literals []Value // constant pool
	Bytecode []byte  // the bytecode instructions

	// Parent reference
	Outer *CompiledMethod // enclosing method (for debugging/source mapping)

	// Debugging support
	Source    string      // original source text (often empty)
	SourceMap []SourceLoc // bytecode offset → source position
}

// ---------------------------------------------------------------------------
// CompiledMethod implements the Method interface
// ---------------------------------------------------------------------------

// Invoke executes the compiled method.
// This is a placeholder - actual execution is done by the interpreter.
func (m *CompiledMethod) Invoke(vm interface{}, receiver Value, args []Value) Value {
	// The interpreter will call this method but handle execution itself
	// by reading the bytecode directly. This stub exists to satisfy the
	// Method interface.
	panic("CompiledMethod.Invoke: use interpreter")
}

// Name returns the method name.
func (m *CompiledMethod) Name() string {
	return m.name
}

// Arity returns the method arity (implements ArityMethod).
func (m *CompiledMethod) MethodArity() int {
	return m.Arity
}

// Selector returns the selector ID.
func (m *CompiledMethod) Selector() int {
	return m.selector
}

// Class returns the defining class.
func (m *CompiledMethod) Class() *Class {
	return m.class
}

// SetClass sets the defining class.
func (m *CompiledMethod) SetClass(c *Class) {
	m.class = c
}

// SetSelector sets the selector ID.
func (m *CompiledMethod) SetSelector(sel int) {
	m.selector = sel
}

// ---------------------------------------------------------------------------
// Literal access
// ---------------------------------------------------------------------------

// GetLiteral returns the literal at the given index.
// Panics if index is out of range.
func (m *CompiledMethod) GetLiteral(index int) Value {
	if index < 0 || index >= len(m.Literals) {
		panic("CompiledMethod.GetLiteral: index out of range")
	}
	return m.Literals[index]
}

// LiteralCount returns the number of literals.
func (m *CompiledMethod) LiteralCount() int {
	return len(m.Literals)
}

// ---------------------------------------------------------------------------
// Block access
// ---------------------------------------------------------------------------

// GetBlock returns the block at the given index.
// Panics if index is out of range.
func (m *CompiledMethod) GetBlock(index int) *BlockMethod {
	if index < 0 || index >= len(m.Blocks) {
		panic("CompiledMethod.GetBlock: index out of range")
	}
	return m.Blocks[index]
}

// BlockCount returns the number of nested blocks.
func (m *CompiledMethod) BlockCount() int {
	return len(m.Blocks)
}

// ---------------------------------------------------------------------------
// Source mapping
// ---------------------------------------------------------------------------

// SourceLocation returns the source location for a bytecode offset.
// Returns the most recent location at or before the offset.
func (m *CompiledMethod) SourceLocation(offset int) *SourceLoc {
	if len(m.SourceMap) == 0 {
		return nil
	}

	// Binary search for the location
	var result *SourceLoc
	for i := range m.SourceMap {
		if m.SourceMap[i].Offset <= offset {
			result = &m.SourceMap[i]
		} else {
			break
		}
	}
	return result
}

// AddSourceLocation adds a source mapping entry.
func (m *CompiledMethod) AddSourceLocation(offset, line, column int) {
	m.SourceMap = append(m.SourceMap, SourceLoc{
		Offset: offset,
		Line:   line,
		Column: column,
	})
}

// ---------------------------------------------------------------------------
// BlockMethod accessors
// ---------------------------------------------------------------------------

// GetLiteral returns the literal at the given index.
func (b *BlockMethod) GetLiteral(index int) Value {
	if index < 0 || index >= len(b.Literals) {
		panic("BlockMethod.GetLiteral: index out of range")
	}
	return b.Literals[index]
}

// LiteralCount returns the number of literals.
func (b *BlockMethod) LiteralCount() int {
	return len(b.Literals)
}

// SourceLocation returns the source location for a bytecode offset.
func (b *BlockMethod) SourceLocation(offset int) *SourceLoc {
	if len(b.SourceMap) == 0 {
		return nil
	}

	var result *SourceLoc
	for i := range b.SourceMap {
		if b.SourceMap[i].Offset <= offset {
			result = &b.SourceMap[i]
		} else {
			break
		}
	}
	return result
}

// ---------------------------------------------------------------------------
// CompiledMethod factory and builder
// ---------------------------------------------------------------------------

// NewCompiledMethod creates a new compiled method.
func NewCompiledMethod(name string, arity int) *CompiledMethod {
	return &CompiledMethod{
		name:     name,
		Arity:    arity,
		NumTemps: arity, // Initially just arguments
		Literals: make([]Value, 0, 8),
		Bytecode: make([]byte, 0, 32),
	}
}

// NewBlockMethod creates a new block method.
func NewBlockMethod(arity int) *BlockMethod {
	return &BlockMethod{
		Arity:    arity,
		NumTemps: arity,
		Literals: make([]Value, 0, 4),
		Bytecode: make([]byte, 0, 16),
	}
}

// ---------------------------------------------------------------------------
// CompiledMethodBuilder: Helper for constructing methods
// ---------------------------------------------------------------------------

// CompiledMethodBuilder helps construct CompiledMethod instances.
type CompiledMethodBuilder struct {
	method   *CompiledMethod
	bytecode *BytecodeBuilder
}

// NewCompiledMethodBuilder creates a new method builder.
func NewCompiledMethodBuilder(name string, arity int) *CompiledMethodBuilder {
	return &CompiledMethodBuilder{
		method:   NewCompiledMethod(name, arity),
		bytecode: NewBytecodeBuilder(),
	}
}

// SetSource sets the source text.
func (b *CompiledMethodBuilder) SetSource(source string) *CompiledMethodBuilder {
	b.method.Source = source
	return b
}

// SetNumTemps sets the total number of temporaries.
func (b *CompiledMethodBuilder) SetNumTemps(n int) *CompiledMethodBuilder {
	b.method.NumTemps = n
	return b
}

// AddLocal increases the temporary count by 1 and returns the index.
func (b *CompiledMethodBuilder) AddLocal() int {
	idx := b.method.NumTemps
	b.method.NumTemps++
	return idx
}

// AddLiteral adds a literal and returns its index.
func (b *CompiledMethodBuilder) AddLiteral(v Value) int {
	idx := len(b.method.Literals)
	b.method.Literals = append(b.method.Literals, v)
	return idx
}

// AddBlock adds a block method and returns its index.
func (b *CompiledMethodBuilder) AddBlock(block *BlockMethod) int {
	idx := len(b.method.Blocks)
	b.method.Blocks = append(b.method.Blocks, block)
	block.Outer = b.method
	return idx
}

// Bytecode returns the bytecode builder for direct emission.
func (b *CompiledMethodBuilder) Bytecode() *BytecodeBuilder {
	return b.bytecode
}

// MarkSource adds a source mapping at the current bytecode position.
func (b *CompiledMethodBuilder) MarkSource(line, column int) {
	b.method.AddSourceLocation(b.bytecode.Len(), line, column)
}

// Build finalizes and returns the compiled method.
func (b *CompiledMethodBuilder) Build() *CompiledMethod {
	b.method.Bytecode = b.bytecode.Bytes()
	return b.method
}

// ---------------------------------------------------------------------------
// BlockMethodBuilder: Helper for constructing blocks
// ---------------------------------------------------------------------------

// BlockMethodBuilder helps construct BlockMethod instances.
type BlockMethodBuilder struct {
	block    *BlockMethod
	bytecode *BytecodeBuilder
}

// NewBlockMethodBuilder creates a new block builder.
func NewBlockMethodBuilder(arity int) *BlockMethodBuilder {
	return &BlockMethodBuilder{
		block:    NewBlockMethod(arity),
		bytecode: NewBytecodeBuilder(),
	}
}

// SetNumTemps sets the total number of temporaries.
func (b *BlockMethodBuilder) SetNumTemps(n int) *BlockMethodBuilder {
	b.block.NumTemps = n
	return b
}

// SetNumCaptures sets the number of captured variables.
func (b *BlockMethodBuilder) SetNumCaptures(n int) *BlockMethodBuilder {
	b.block.NumCaptures = n
	return b
}

// AddLocal increases the temporary count by 1 and returns the index.
func (b *BlockMethodBuilder) AddLocal() int {
	idx := b.block.NumTemps
	b.block.NumTemps++
	return idx
}

// AddLiteral adds a literal and returns its index.
func (b *BlockMethodBuilder) AddLiteral(v Value) int {
	idx := len(b.block.Literals)
	b.block.Literals = append(b.block.Literals, v)
	return idx
}

// Bytecode returns the bytecode builder for direct emission.
func (b *BlockMethodBuilder) Bytecode() *BytecodeBuilder {
	return b.bytecode
}

// Build finalizes and returns the block method.
func (b *BlockMethodBuilder) Build() *BlockMethod {
	b.block.Bytecode = b.bytecode.Bytes()
	return b.block
}

// ---------------------------------------------------------------------------
// Debugging/display helpers
// ---------------------------------------------------------------------------

// Disassemble returns a disassembly of the method's bytecode.
func (m *CompiledMethod) Disassemble() string {
	return Disassemble(m.Bytecode)
}

// Disassemble returns a disassembly of the block's bytecode.
func (b *BlockMethod) Disassemble() string {
	return Disassemble(b.Bytecode)
}

// String returns a string representation of the method.
func (m *CompiledMethod) String() string {
	className := "?"
	if m.class != nil {
		className = m.class.Name
	}
	return className + ">>" + m.name
}
