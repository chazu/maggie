package types

import (
	"fmt"
	"strings"

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

	declared     map[string]bool   // class/trait names defined in checked sources
	superclasses map[string]string // class name -> superclass, from checked sources
	namespace    string            // namespace of the file being checked
	imports      []string          // imports of the file being checked
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
	c.DeclareTypes(sf)
	c.namespace = sourceNamespace(sf)
	c.imports = sourceImports(sf)

	// Register protocols first (they may be referenced by classes)
	for _, protoDef := range sf.Protocols {
		c.Protocols.RegisterFromAST(protoDef)
	}

	// Harvest return type and effect annotations from all methods before
	// inference. Classes are keyed by their qualified name (what self is
	// bound to); class-side methods under the metaclass name so a class-side
	// and an instance-side method with the same selector don't collide.
	for _, classDef := range sf.Classes {
		className := qualifyName(c.namespace, classDef.Name)
		for _, method := range classDef.Methods {
			c.ReturnTypes.HarvestFromMethod(className, method)
			c.EffectTable.HarvestFromMethod(className, method)
		}
		for _, method := range classDef.ClassMethods {
			c.ReturnTypes.HarvestFromMethod(ClassSideName(className), method)
			c.EffectTable.HarvestFromMethod(ClassSideName(className), method)
		}
	}
	for _, traitDef := range sf.Traits {
		for _, method := range traitDef.Methods {
			c.ReturnTypes.HarvestFromMethod(traitDef.Name, method)
			c.EffectTable.HarvestFromMethod(traitDef.Name, method)
		}
	}

	// Check class definitions
	for _, classDef := range sf.Classes {
		c.checkClassDef(classDef)
	}
	// Check trait methods and extension methods, which were previously skipped
	// entirely — so a bad annotation in a trait or extension method went
	// unreported.
	for _, traitDef := range sf.Traits {
		for _, method := range traitDef.Methods {
			c.checkMethodDef(method, traitDef.Name, false)
		}
	}
	for _, method := range sf.Methods {
		c.checkMethodDef(method, "", false)
	}
}

// checkClassDef checks a class definition's methods.
func (c *Checker) checkClassDef(classDef *compiler.ClassDef) {
	className := qualifyName(c.namespace, classDef.Name)
	for _, method := range classDef.Methods {
		c.checkMethodDef(method, className, false)
	}
	for _, method := range classDef.ClassMethods {
		c.checkMethodDef(method, className, true)
	}
}

// checkMethodDef checks a single method definition.
// className is the (qualified) class name; classSide is true for class
// methods, where self is the class rather than an instance.
func (c *Checker) checkMethodDef(method *compiler.MethodDef, className string, classSide bool) {
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
		inferrer.SetNamespace(c.namespace, c.imports)
		inferrer.Superclasses = c.superclasses
		var inferredEffect Effect
		var diags []Diagnostic
		if classSide {
			_, inferredEffect, diags = inferrer.InferClassMethod(className, method)
		} else {
			_, inferredEffect, diags = inferrer.InferMethod(className, method)
		}
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

	// Check classes/traits defined in the sources being checked
	if c.declared[name] {
		return
	}

	// Check VM classes (namespace/import aware, falling back to the bare name)
	if c.VM != nil && c.VM.Classes.LookupWithImports(name, c.namespace, c.imports) != nil {
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

// DeclareTypes records the class and trait names a source file defines, so
// annotations referring to them are not reported as unknown types. Classes
// in a namespace are declared under both the short and qualified names.
// `mag typecheck` declares every file in the check set before checking any
// of them; CheckSourceFile also declares its own file.
func (c *Checker) DeclareTypes(sf *compiler.SourceFile) {
	if c.declared == nil {
		c.declared = make(map[string]bool)
		c.superclasses = make(map[string]string)
	}
	ns := sourceNamespace(sf)
	for _, classDef := range sf.Classes {
		c.declared[classDef.Name] = true
		c.declared[qualifyName(ns, classDef.Name)] = true
		if classDef.Superclass != "" && classDef.Superclass != "nil" {
			c.superclasses[qualifyName(ns, classDef.Name)] = classDef.Superclass
		}
	}
	for _, traitDef := range sf.Traits {
		c.declared[traitDef.Name] = true
		c.declared[qualifyName(ns, traitDef.Name)] = true
	}
}

// sourceNamespace returns the file's `namespace:` declaration, or "".
func sourceNamespace(sf *compiler.SourceFile) string {
	if sf.Namespace == nil {
		return ""
	}
	return sf.Namespace.Name
}

// sourceImports returns the file's `import:` paths.
func sourceImports(sf *compiler.SourceFile) []string {
	var imports []string
	for _, imp := range sf.Imports {
		imports = append(imports, imp.Path)
	}
	return imports
}

// qualifyName prefixes name with namespace (Ns::Name) unless there is no
// namespace or the name is already qualified.
func qualifyName(namespace, name string) string {
	if namespace == "" || name == "" || strings.Contains(name, "::") {
		return name
	}
	return namespace + "::" + name
}
