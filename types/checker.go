package types

import (
	"fmt"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// Diagnostic represents a type checking warning or error.
type Diagnostic struct {
	Pos     compiler.Position
	Message string
}

func (d Diagnostic) String() string {
	return fmt.Sprintf("line %d, col %d: %s", d.Pos.Line, d.Pos.Column, d.Message)
}

// Checker performs structural type checking on Maggie ASTs.
// It produces diagnostics (warnings) but never blocks compilation.
type Checker struct {
	Protocols   *ProtocolRegistry
	ReturnTypes *ReturnTypeTable
	EffectTable *EffectTable
	VM          *vm.VM
	Verbose     bool
	Diagnostics []Diagnostic
}

// NewChecker creates a type checker with the given VM for class lookups.
func NewChecker(vmInst *vm.VM) *Checker {
	return &Checker{
		Protocols:   NewProtocolRegistry(),
		ReturnTypes: NewReturnTypeTable(),
		EffectTable: NewEffectTable(),
		VM:          vmInst,
	}
}

// CheckSourceFile checks all definitions in a source file.
func (c *Checker) CheckSourceFile(sf *compiler.SourceFile) {
	// Register protocols first (they may be referenced by classes)
	for _, protoDef := range sf.Protocols {
		c.Protocols.RegisterFromAST(protoDef)
	}

	// Harvest return type and effect annotations from all methods before inference
	for _, classDef := range sf.Classes {
		for _, method := range classDef.Methods {
			c.ReturnTypes.HarvestFromMethod(classDef.Name, method)
			c.EffectTable.HarvestFromMethod(classDef.Name, method)
		}
		for _, method := range classDef.ClassMethods {
			c.ReturnTypes.HarvestFromMethod(classDef.Name, method)
			c.EffectTable.HarvestFromMethod(classDef.Name, method)
		}
	}

	// Check class definitions
	for _, classDef := range sf.Classes {
		c.checkClassDef(classDef)
	}
}

// checkClassDef checks a class definition's methods.
func (c *Checker) checkClassDef(classDef *compiler.ClassDef) {
	for _, method := range classDef.Methods {
		c.checkMethodDef(method, classDef)
	}
	for _, method := range classDef.ClassMethods {
		c.checkMethodDef(method, classDef)
	}
}

// checkMethodDef checks a single method definition.
func (c *Checker) checkMethodDef(method *compiler.MethodDef, classDef *compiler.ClassDef) {
	// Check that parameter types reference known types/protocols
	for i, paramType := range method.ParamTypes {
		if paramType != nil {
			c.checkTypeExists(paramType, method.Parameters[i])
		}
	}

	// Check that return type references a known type/protocol
	if method.ReturnType != nil {
		c.checkTypeExists(method.ReturnType, "return type")
	}

	// Check that temp types reference known types/protocols
	for i, tempType := range method.TempTypes {
		if tempType != nil {
			c.checkTypeExists(tempType, method.Temps[i])
		}
	}

	// Validate effect annotation names
	for _, eff := range method.Effects {
		if eff != nil && !IsValidEffect(eff.Name) {
			c.addDiagnostic(eff.SpanVal.Start,
				fmt.Sprintf("unknown effect <%s>", eff.Name))
		}
	}

	// Run type inference on the method body (skip primitive stubs)
	if !method.IsPrimitiveStub && len(method.Statements) > 0 {
		inferrer := NewInferrer(c.ReturnTypes, c.Protocols, c.VM, c.Verbose)
		inferrer.SetEffectTable(c.EffectTable)
		_, inferredEffect, diags := inferrer.InferMethod(classDef.Name, method)
		for _, d := range diags {
			c.addDiagnostic(d.Pos, d.Message)
		}

		// Check declared effects against inferred effects
		if len(method.Effects) > 0 {
			declared := ParseEffects(method.Effects)
			c.checkEffects(method, declared, inferredEffect)
		}
	}
}

// builtinTypes are type names that are always valid, even if no class with
// that exact name exists. These cover common Smalltalk type vocabulary.
var builtinTypes = map[string]bool{
	"Dynamic": true, "Self": true, "Object": true,
	"Integer": true, "Number": true, "Boolean": true,
	"String": true, "Symbol": true, "Float": true,
	"Array": true, "Dictionary": true, "Block": true,
	"Character": true, "Nil": true,
}

// checkTypeExists verifies that a type name refers to a known class or protocol.
func (c *Checker) checkTypeExists(typeExpr *compiler.TypeExpr, context string) {
	name := typeExpr.Name

	// Built-in types are always valid
	if builtinTypes[name] {
		return
	}

	// Check protocols
	if c.Protocols.Lookup(name) != nil {
		return
	}

	// Check VM classes
	if c.VM != nil && c.VM.Classes.Lookup(name) != nil {
		return
	}

	c.addDiagnostic(typeExpr.SpanVal.Start,
		fmt.Sprintf("unknown type <%s> in %s", name, context))
}

// CheckProtocolSatisfaction verifies that a class satisfies a protocol.
func (c *Checker) CheckProtocolSatisfaction(className string, protocolName string) {
	protocol := c.Protocols.Lookup(protocolName)
	if protocol == nil {
		return // Unknown protocol — already reported by checkTypeExists
	}

	if c.VM == nil {
		return
	}

	class := c.VM.Classes.Lookup(className)
	if class == nil {
		return // Unknown class — not our job to report
	}

	if !Satisfies(class, protocol, c.VM.Selectors) {
		c.addDiagnostic(compiler.Position{},
			fmt.Sprintf("class %s does not satisfy protocol %s", className, protocolName))

		// Report which methods are missing
		for selector := range protocol.Methods {
			selectorID := c.VM.Selectors.Intern(selector)
			if class.VTable == nil || class.VTable.Lookup(selectorID) == nil {
				c.addDiagnostic(compiler.Position{},
					fmt.Sprintf("  missing method: %s", selector))
			}
		}
	}
}

// checkEffects verifies that a method's declared effects match its inferred effects.
func (c *Checker) checkEffects(method *compiler.MethodDef, declared, inferred Effect) {
	if declared.IsPure() {
		// Pure assertion: body must have no effects
		violating := inferred & ^EffectPure
		if !violating.IsEmpty() {
			c.addDiagnostic(method.SpanVal.Start,
				fmt.Sprintf("method %s declared Pure but body has effects: %s",
					method.Selector, violating.String()))
		}
		return
	}
	// Check for undeclared effects
	undeclared := inferred & ^declared
	if !undeclared.IsEmpty() {
		c.addDiagnostic(method.SpanVal.Start,
			fmt.Sprintf("method %s has undeclared effects: %s (declared: %s)",
				method.Selector, undeclared.String(), declared.String()))
	}
}

func (c *Checker) addDiagnostic(pos compiler.Position, message string) {
	c.Diagnostics = append(c.Diagnostics, Diagnostic{Pos: pos, Message: message})
}
