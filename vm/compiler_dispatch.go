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
type CompileFunc func(source string, selectors *SelectorTable, symbols *SymbolTable, registry *ObjectRegistry) (*CompiledMethod, error)

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
	method, err := g.compileFunc(source, g.vm.Selectors, g.vm.Symbols, g.vm.registry)
	if err != nil {
		return nil, err
	}
	if source != "" {
		method.Source = source
	}
	if class != nil {
		method.SetClass(class)
	}
	return method, nil
}

// CompileExpression compiles a single expression using the Go compiler.
func (g *GoCompilerBackend) CompileExpression(source string) (*CompiledMethod, error) {
	// Wrap expression in a method body.
	// For multi-statement expressions (separated by '.'), the ^ (return)
	// must be placed before the last statement only. Otherwise, the method
	// returns after the first statement and remaining statements are dead code.
	//
	// Statement separators are '. ' (dot followed by space/newline/EOF-after-content).
	// A dot inside a number literal (e.g., 3.7) is NOT a statement separator.
	source = strings.TrimSpace(source)
	lastDot := -1
	for i := len(source) - 1; i >= 0; i-- {
		if source[i] == '.' {
			// A statement-separating dot is followed by whitespace or is at the end,
			// AND is not preceded by a digit (which would make it a decimal point).
			if i > 0 && source[i-1] >= '0' && source[i-1] <= '9' {
				// Check if this dot is inside a number literal
				if i+1 < len(source) && source[i+1] >= '0' && source[i+1] <= '9' {
					continue // decimal point in number like 3.7
				}
			}
			lastDot = i
			break
		}
	}
	var methodSource string
	if lastDot < 0 {
		// Single expression: return it directly
		methodSource = "doIt\n    ^" + source
	} else {
		// Multi-statement: execute all but last, return last
		prefix := source[:lastDot+1]
		suffix := strings.TrimSpace(source[lastDot+1:])
		if suffix == "" {
			// Trailing dot with no final expression — return the whole thing
			methodSource = "doIt\n    ^" + source
		} else {
			methodSource = "doIt\n    " + prefix + " ^" + suffix
		}
	}
	return g.Compile(methodSource, nil)
}

// Name returns the name of this backend.
func (g *GoCompilerBackend) Name() string {
	return "Go"
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
