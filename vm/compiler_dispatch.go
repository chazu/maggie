package vm

import (
	"strings"
)

// ---------------------------------------------------------------------------
// CompilerBackend: Interface for compilation backends
// ---------------------------------------------------------------------------

// CompilerBackend defines the interface for compiling source code to bytecode.
// This abstraction allows switching between the Go compiler and the Maggie
// self-hosting compiler.
type CompilerBackend interface {
	// Compile compiles a method source string for the given class.
	// Returns a CompiledMethod or an error.
	Compile(source string, class *Class) (*CompiledMethod, error)

	// CompileExpression compiles a single expression.
	// Returns a CompiledMethod that evaluates the expression when executed.
	CompileExpression(source string) (*CompiledMethod, error)

	// Name returns the name of this compiler backend.
	Name() string
}

// ---------------------------------------------------------------------------
// GoCompilerBackend: Uses the Go compiler package
// ---------------------------------------------------------------------------

// CompileFunc is the signature for compilation functions.
// This is used to inject the Go compiler without creating import cycles.
type CompileFunc func(source string, selectors *SelectorTable, symbols *SymbolTable) (*CompiledMethod, error)

// GoCompilerBackend wraps the compiler/ package written in Go.
// This is used for bootstrapping, AOT compilation, and as a fallback.
type GoCompilerBackend struct {
	vm          *VM
	compileFunc CompileFunc
}

// NewGoCompilerBackend creates a new Go compiler backend.
// The compileFunc parameter should be compiler.Compile from the compiler package.
func NewGoCompilerBackend(vm *VM, compileFunc CompileFunc) *GoCompilerBackend {
	return &GoCompilerBackend{vm: vm, compileFunc: compileFunc}
}

// Compile compiles a method source string using the Go compiler.
func (g *GoCompilerBackend) Compile(source string, class *Class) (*CompiledMethod, error) {
	method, err := g.compileFunc(source, g.vm.Selectors, g.vm.Symbols)
	if err != nil {
		return nil, err
	}
	if class != nil {
		method.SetClass(class)
	}
	return method, nil
}

// CompileExpression compiles a single expression using the Go compiler.
func (g *GoCompilerBackend) CompileExpression(source string) (*CompiledMethod, error) {
	// Wrap expression in a method
	methodSource := "doIt\n    ^" + source
	return g.Compile(methodSource, nil)
}

// Name returns the name of this backend.
func (g *GoCompilerBackend) Name() string {
	return "Go"
}

// ---------------------------------------------------------------------------
// MaggieCompilerBackend: Uses the Maggie self-hosting compiler
// ---------------------------------------------------------------------------

// MaggieCompilerBackend invokes the Compiler class written in Maggie.
// This is used for live development after bootstrapping.
type MaggieCompilerBackend struct {
	vm       *VM
	fallback CompilerBackend
}

// NewMaggieCompilerBackend creates a new Maggie compiler backend.
// The fallback is used when the Maggie compiler is not available.
func NewMaggieCompilerBackend(vm *VM, fallback CompilerBackend) *MaggieCompilerBackend {
	return &MaggieCompilerBackend{vm: vm, fallback: fallback}
}

// convertToNewStyleFormat converts old-style method source to new Trashtalk format.
// Old style: "doIt\n    ^42"
// New style: "method: doIt [\n    ^42\n]"
func convertToNewStyleFormat(source string) string {
	// If source already starts with method: or classMethod:, return as-is
	trimmed := strings.TrimSpace(source)
	if strings.HasPrefix(trimmed, "method:") || strings.HasPrefix(trimmed, "classMethod:") {
		return source
	}

	// Split into lines
	lines := strings.Split(source, "\n")
	if len(lines) == 0 {
		return source
	}

	// First line is the selector (possibly with parameters)
	selector := strings.TrimSpace(lines[0])

	// Remaining lines are the body
	var bodyLines []string
	if len(lines) > 1 {
		bodyLines = lines[1:]
	}

	// Build new format
	var result strings.Builder
	result.WriteString("method: ")
	result.WriteString(selector)
	result.WriteString(" [\n")
	for _, line := range bodyLines {
		result.WriteString(line)
		result.WriteString("\n")
	}
	result.WriteString("]")
	return result.String()
}

// Compile compiles a method source string using the Maggie compiler.
func (m *MaggieCompilerBackend) Compile(source string, class *Class) (*CompiledMethod, error) {
	// Convert old-style source to new Trashtalk format
	source = convertToNewStyleFormat(source)

	// Look up the Compiler class
	compilerClassVal, ok := m.vm.Globals["Compiler"]
	if !ok {
		// Fall back if Maggie compiler not available
		if m.fallback != nil {
			return m.fallback.Compile(source, class)
		}
		return nil, nil
	}

	// Create a new Compiler instance: Compiler new
	compilerInstance := m.vm.Send(compilerClassVal, "new", nil)
	if compilerInstance == Nil {
		if m.fallback != nil {
			return m.fallback.Compile(source, class)
		}
		return nil, nil
	}

	// Call compile: on the compiler
	// Compiler new compile: sourceString
	sourceVal := NewStringValue(source)
	resultVal := m.vm.Send(compilerInstance, "compile:", []Value{sourceVal})

	// Debug: what did compile: return?
	// fmt.Printf("DEBUG compile: resultVal = %v (IsNil: %v, IsDictionary: %v)\n",
	// 	resultVal, resultVal == Nil, IsDictionaryValue(resultVal))
	if IsDictionaryValue(resultVal) {
		dict := GetDictionaryObject(resultVal)
		if dict != nil {
// 			fmt.Printf("DEBUG compile: dictionary entries: %d\n", len(dict.Data))
		}
	}

	// Extract the CompiledMethod from the result
	// The Maggie compiler returns a Dictionary with bytecode, literals, etc.
	// We need to convert it to a CompiledMethod
	return m.extractCompiledMethod(resultVal, class)
}

// CompileExpression compiles a single expression using the Maggie compiler.
func (m *MaggieCompilerBackend) CompileExpression(source string) (*CompiledMethod, error) {
	// Look up the Compiler class
	compilerClassVal, ok := m.vm.Globals["Compiler"]
	if !ok {
		if m.fallback != nil {
			return m.fallback.CompileExpression(source)
		}
		return nil, nil
	}

	// Create a new Compiler instance
	compilerInstance := m.vm.Send(compilerClassVal, "new", nil)
	if compilerInstance == Nil {
		if m.fallback != nil {
			return m.fallback.CompileExpression(source)
		}
		return nil, nil
	}

	// Call compileExpression: on the compiler
	sourceVal := NewStringValue(source)
	resultVal := m.vm.Send(compilerInstance, "compileExpression:", []Value{sourceVal})

	return m.extractCompiledMethod(resultVal, nil)
}

// extractCompiledMethod converts a Maggie compiler result to a CompiledMethod.
func (m *MaggieCompilerBackend) extractCompiledMethod(resultVal Value, class *Class) (*CompiledMethod, error) {
	// The Maggie compiler returns a Dictionary with:
	// - #selector -> Symbol
	// - #bytecode -> Array of integers
	// - #literals -> Array of values
	// - #parameters -> Array of strings
	// - #blockMethods -> Array of nested method dictionaries

	if resultVal == Nil {
		return nil, nil
	}

	// Debug: print dictionary keys
	// if IsDictionaryValue(resultVal) {
	// 	dict := GetDictionaryObject(resultVal)
	// 	if dict != nil {
	// 		for h, key := range dict.Keys {
	// 			if key.IsSymbol() {
	// 				fmt.Printf(" (symbol: %s)", m.vm.Symbols.Name(key.SymbolID()))
	// 			}
	// 			fmt.Printf(", value=%v\n", dict.Data[h])
	// 		}
	// 	}
	// }

	// Extract selector
	selectorKey := m.vm.Symbols.SymbolValue("selector")
// 	fmt.Printf("DEBUG extractCompiledMethod: looking for 'selector' key=%v (hash=%d)\n", selectorKey, hashValue(selectorKey))
	selectorVal := m.vm.Send(resultVal, "at:", []Value{selectorKey})
// 	fmt.Printf("DEBUG extractCompiledMethod: selectorVal=%v\n", selectorVal)
	selector := ""
	if selectorVal.IsSymbol() {
		selector = m.vm.Symbols.Name(selectorVal.SymbolID())
	}

	// Extract bytecode
	bytecodeKey := m.vm.Symbols.SymbolValue("bytecode")
// 	fmt.Printf("DEBUG extractCompiledMethod: looking for 'bytecode' key=%v (hash=%d)\n", bytecodeKey, hashValue(bytecodeKey))
	bytecodeVal := m.vm.Send(resultVal, "at:", []Value{bytecodeKey})
// 	fmt.Printf("DEBUG extractCompiledMethod: bytecodeVal=%v (IsNil: %v)\n", bytecodeVal, bytecodeVal == Nil)
	bytecode := m.extractBytecodeArray(bytecodeVal)

	// Extract literals
	literalsVal := m.vm.Send(resultVal, "at:", []Value{m.vm.Symbols.SymbolValue("literals")})
	literals := m.extractLiteralsArray(literalsVal)

	// Extract parameters to determine arity
	paramsVal := m.vm.Send(resultVal, "at:", []Value{m.vm.Symbols.SymbolValue("parameters")})
	arity := m.extractArraySize(paramsVal)

	// Create the CompiledMethod using the builder
	builder := NewCompiledMethodBuilder(selector, arity)

	// Add bytecode
	for _, b := range bytecode {
		builder.Bytecode().EmitRaw(b)
	}

	// Add literals
	for _, lit := range literals {
		builder.AddLiteral(lit)
	}

	method := builder.Build()
	if class != nil {
		method.SetClass(class)
	}

	// TODO: Extract and compile nested block methods

	return method, nil
}

// extractBytecodeArray converts a Maggie Array of integers to a []byte.
func (m *MaggieCompilerBackend) extractBytecodeArray(arrVal Value) []byte {
	if arrVal == Nil {
		return nil
	}

	sizeVal := m.vm.Send(arrVal, "size", nil)
	if !sizeVal.IsSmallInt() {
		return nil
	}

	size := int(sizeVal.SmallInt())
	result := make([]byte, size)

	for i := 0; i < size; i++ {
		elemVal := m.vm.Send(arrVal, "at:", []Value{FromSmallInt(int64(i))})
		if elemVal.IsSmallInt() {
			result[i] = byte(elemVal.SmallInt())
		}
	}

	return result
}

// extractLiteralsArray converts a Maggie Array to a []Value.
func (m *MaggieCompilerBackend) extractLiteralsArray(arrVal Value) []Value {
	if arrVal == Nil {
		return nil
	}

	sizeVal := m.vm.Send(arrVal, "size", nil)
	if !sizeVal.IsSmallInt() {
		return nil
	}

	size := int(sizeVal.SmallInt())
	result := make([]Value, size)

	for i := 0; i < size; i++ {
		result[i] = m.vm.Send(arrVal, "at:", []Value{FromSmallInt(int64(i))})
	}

	return result
}

// extractArraySize returns the size of a Maggie Array.
func (m *MaggieCompilerBackend) extractArraySize(arrVal Value) int {
	if arrVal == Nil {
		return 0
	}

	sizeVal := m.vm.Send(arrVal, "size", nil)
	if sizeVal.IsSmallInt() {
		return int(sizeVal.SmallInt())
	}
	return 0
}

// Name returns the name of this backend.
func (m *MaggieCompilerBackend) Name() string {
	return "Maggie"
}

// ---------------------------------------------------------------------------
// VM compiler backend management
// ---------------------------------------------------------------------------

// CompilerBackendField returns the current compiler backend.
func (vm *VM) CompilerBackendField() CompilerBackend {
	return vm.compilerBackend
}

// SetCompilerBackend sets the compiler backend.
func (vm *VM) SetCompilerBackend(backend CompilerBackend) {
	vm.compilerBackend = backend
}

// UseGoCompiler switches to the Go compiler backend.
// The compileFunc must be provided (typically compiler.Compile).
func (vm *VM) UseGoCompiler(compileFunc CompileFunc) {
	vm.compilerBackend = NewGoCompilerBackend(vm, compileFunc)
}

// UseMaggieCompiler switches to the Maggie self-hosting compiler backend.
// Falls back to the current backend if the Maggie compiler is not available.
func (vm *VM) UseMaggieCompiler() {
	vm.compilerBackend = NewMaggieCompilerBackend(vm, vm.compilerBackend)
}

// CompilerName returns the name of the current compiler backend.
func (vm *VM) CompilerName() string {
	if vm.compilerBackend == nil {
		return "none"
	}
	return vm.compilerBackend.Name()
}

// Compile compiles a method source string using the current backend.
func (vm *VM) Compile(source string, class *Class) (*CompiledMethod, error) {
	if vm.compilerBackend == nil {
		return nil, nil
	}
	return vm.compilerBackend.Compile(source, class)
}

// CompileExpression compiles an expression using the current backend.
func (vm *VM) CompileExpression(source string) (*CompiledMethod, error) {
	if vm.compilerBackend == nil {
		return nil, nil
	}
	return vm.compilerBackend.CompileExpression(source)
}
