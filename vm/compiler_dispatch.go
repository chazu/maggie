package vm

import (
	"fmt"
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

// DoItCompileFunc compiles eval input (a statement sequence with optional
// leading temps) into a synthetic doIt method, returning semantic warnings
// alongside. compiler.CompileDoIt is the canonical implementation; it wraps
// the final statement in a return at the AST level, so no textual splicing
// can corrupt string literals or block bodies.
type DoItCompileFunc func(source string, selectors *SelectorTable, symbols *SymbolTable, registry *ObjectRegistry) (*CompiledMethod, []string, error)

// registeredDoItCompiler is installed by the compiler package's init (the
// compiler package imports vm; vm cannot import compiler).
var registeredDoItCompiler DoItCompileFunc

// RegisterDoItCompiler installs the doIt compiler. Called from the compiler
// package's init.
func RegisterDoItCompiler(fn DoItCompileFunc) {
	registeredDoItCompiler = fn
}

// CompileExpression compiles eval input via the registered doIt compiler,
// discarding warnings. Use CompileExpressionWithWarnings when the caller
// can surface them.
func (g *GoCompilerBackend) CompileExpression(source string) (*CompiledMethod, error) {
	method, _, err := g.CompileExpressionWithWarnings(source)
	return method, err
}

// CompileExpressionWithWarnings compiles eval input and returns semantic
// warnings (e.g. possibly-undefined variables) alongside.
func (g *GoCompilerBackend) CompileExpressionWithWarnings(source string) (*CompiledMethod, []string, error) {
	if registeredDoItCompiler == nil {
		return nil, nil, fmt.Errorf("no doIt compiler registered (import the compiler package)")
	}
	method, warnings, err := registeredDoItCompiler(source, g.vm.Selectors, g.vm.Symbols, g.vm.registry)
	if err != nil {
		return nil, warnings, err
	}
	return method, warnings, nil
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

// CompileExpressionWithWarnings compiles eval input and returns semantic
// warnings alongside (for diagnostics surfaces like the LSP).
func (vm *VM) CompileExpressionWithWarnings(source string) (*CompiledMethod, []string, error) {
	if vm.compilerBackend == nil {
		return nil, nil, nil
	}
	if g, ok := vm.compilerBackend.(*GoCompilerBackend); ok {
		return g.CompileExpressionWithWarnings(source)
	}
	method, err := vm.compilerBackend.CompileExpression(source)
	return method, nil, err
}
