package types

import (
	"fmt"
	"strings"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// Inferrer performs forward type inference on method bodies.
// It walks statements in order, tracking variable types via TypeEnv
// and looking up message send return types via ReturnTypeTable.
//
// The inferrer produces warnings (Diagnostic) but never errors.
// Dynamic is the escape hatch -- untyped code gets no warnings.
type Inferrer struct {
	ReturnTypes *ReturnTypeTable
	Protocols   *ProtocolRegistry
	EffectTable *EffectTable
	VM          *vm.VM
	Verbose     bool

	// Superclasses maps class names defined in the checked sources to their
	// declared superclass names, for subtype checks on classes the VM has
	// not loaded. Optional.
	Superclasses map[string]string

	className      string          // class being checked (qualified)
	classSide      bool            // checking a class-side method
	namespace      string          // namespace of the file being checked
	imports        []string        // imports of the file being checked
	diagnostics    []Diagnostic    // collected during inference
	inferredEffect Effect          // accumulated effects during inference
	methodLocals   map[string]bool // params + temps declared in method signature
}

// NewInferrer creates an inferrer with the given dependencies.
func NewInferrer(rt *ReturnTypeTable, protocols *ProtocolRegistry, vmInst *vm.VM, verbose bool) *Inferrer {
	return &Inferrer{
		ReturnTypes: rt,
		Protocols:   protocols,
		VM:          vmInst,
		Verbose:     verbose,
	}
}

// SetEffectTable sets the effect table for callee effect propagation.
func (inf *Inferrer) SetEffectTable(et *EffectTable) {
	inf.EffectTable = et
}

// SetNamespace sets the namespace and imports used to resolve class names
// (e.g. `Command` inside `namespace: 'Cli'` is `Cli::Command`).
func (inf *Inferrer) SetNamespace(namespace string, imports []string) {
	inf.namespace = namespace
	inf.imports = imports
}

// classSideSuffix marks the class-side (metaclass) type of a class:
// "Foo class" is the type of the class object Foo itself.
const classSideSuffix = " class"

// ClassSideName returns the name of the class-side type of className.
// Class-side return types and effects are keyed under this name.
func ClassSideName(className string) string { return className + classSideSuffix }

// splitClassSide reports whether name is a class-side type name and returns
// the underlying class name.
func splitClassSide(name string) (string, bool) {
	if base, ok := strings.CutSuffix(name, classSideSuffix); ok {
		return base, true
	}
	return name, false
}

// InferMethod performs type inference on an instance-side method body.
// Returns the inferred return type, inferred effects, and any diagnostics generated.
func (inf *Inferrer) InferMethod(className string, md *compiler.MethodDef) (MaggieType, Effect, []Diagnostic) {
	return inf.inferMethod(className, false, md)
}

// InferClassMethod performs type inference on a class-side method body, where
// self is the class object rather than an instance.
func (inf *Inferrer) InferClassMethod(className string, md *compiler.MethodDef) (MaggieType, Effect, []Diagnostic) {
	return inf.inferMethod(className, true, md)
}

// selfType is the type of self in the method being checked.
func (inf *Inferrer) selfType() *NamedType {
	if inf.classSide {
		return &NamedType{Name: ClassSideName(inf.className)}
	}
	return &NamedType{Name: inf.className}
}

func (inf *Inferrer) inferMethod(className string, classSide bool, md *compiler.MethodDef) (MaggieType, Effect, []Diagnostic) {
	inf.className = className
	inf.classSide = classSide
	inf.diagnostics = nil
	inf.inferredEffect = EffectNone
	inf.methodLocals = make(map[string]bool)
	for _, p := range md.Parameters {
		inf.methodLocals[p] = true
	}
	for _, t := range md.Temps {
		inf.methodLocals[t] = true
	}

	env := NewTypeEnv(nil)

	// Bind self
	env.Set("self", inf.selfType())

	// Bind parameters from annotations (or Dynamic if untyped)
	for i, paramName := range md.Parameters {
		if i < len(md.ParamTypes) && md.ParamTypes[i] != nil {
			env.Set(paramName, typeExprToType(md.ParamTypes[i]))
		} else {
			env.Set(paramName, &DynamicType{})
		}
	}

	// Bind temps from annotations (or leave unbound for first-assignment typing)
	for i, tempName := range md.Temps {
		if i < len(md.TempTypes) && md.TempTypes[i] != nil {
			env.Set(tempName, typeExprToType(md.TempTypes[i]))
		}
		// Untyped temps are left unbound -- first assignment sets their type
	}

	// Walk statements, tracking the last return type
	var lastReturnType MaggieType
	for _, stmt := range md.Statements {
		switch s := stmt.(type) {
		case *compiler.ExprStmt:
			inf.inferExpr(env, s.Expr)
		case *compiler.Return:
			lastReturnType = inf.inferExpr(env, s.Value)
		}
	}

	// Check inferred return vs declared return type
	if md.ReturnType != nil && lastReturnType != nil {
		declared := typeExprToType(md.ReturnType)
		if !inf.isAssignable(lastReturnType, declared) {
			inf.addDiagnostic(md.SpanVal.Start,
				fmt.Sprintf("inferred return type %s is not assignable to declared %s",
					lastReturnType.String(), declared.String()))
		}
	}

	if lastReturnType == nil {
		lastReturnType = &DynamicType{}
	}

	diags := inf.diagnostics
	inf.diagnostics = nil
	eff := inf.inferredEffect
	inf.inferredEffect = EffectNone
	return lastReturnType, eff, diags
}

// inferExpr infers the type of an expression, updating the TypeEnv
// for assignments.
func (inf *Inferrer) inferExpr(env *TypeEnv, expr compiler.Expr) MaggieType {
	switch e := expr.(type) {
	case *compiler.IntLiteral:
		return &NamedType{Name: "SmallInteger"}
	case *compiler.FloatLiteral:
		return &NamedType{Name: "Float"}
	case *compiler.StringLiteral:
		return &NamedType{Name: "String"}
	case *compiler.SymbolLiteral:
		return &NamedType{Name: "Symbol"}
	case *compiler.CharLiteral:
		return &NamedType{Name: "Character"}
	case *compiler.NilLiteral:
		return &NamedType{Name: "UndefinedObject"}
	case *compiler.TrueLiteral:
		return &NamedType{Name: "Boolean"}
	case *compiler.FalseLiteral:
		return &NamedType{Name: "Boolean"}
	case *compiler.ArrayLiteral:
		// ArrayLiteral elements are compile-time literals (no sends), so no
		// traversal needed.
		return &NamedType{Name: "Array"}
	case *compiler.DynamicArray:
		// Dynamic-array elements are arbitrary expressions ({ File readAll: p })
		// — infer each so their effects are counted.
		for _, el := range e.Elements {
			inf.inferExpr(env, el)
		}
		return &NamedType{Name: "Array"}
	case *compiler.DictionaryLiteral:
		for _, k := range e.Keys {
			inf.inferExpr(env, k)
		}
		for _, v := range e.Values {
			inf.inferExpr(env, v)
		}
		return &NamedType{Name: "Dictionary"}
	case *compiler.Self:
		return inf.selfType()
	case *compiler.Super:
		return inf.selfType()
	case *compiler.ThisContext:
		return &DynamicType{}
	case *compiler.Variable:
		if t, ok := env.Lookup(e.Name); ok {
			return t
		}
		return &DynamicType{}
	case *compiler.Assignment:
		valType := inf.inferExpr(env, e.Value)
		// Global assignment (not a declared param/temp) implies State effect
		if inf.methodLocals != nil && !inf.methodLocals[e.Variable] {
			if _, isInEnv := env.Lookup(e.Variable); !isInEnv {
				inf.inferredEffect = inf.inferredEffect.Union(EffectState)
			}
		}
		env.Set(e.Variable, valType)
		return valType
	case *compiler.UnaryMessage:
		recvType := inf.inferExpr(env, e.Receiver)
		return inf.inferSend(recvType, e.Selector, e.SpanVal.Start)
	case *compiler.BinaryMessage:
		recvType := inf.inferExpr(env, e.Receiver)
		// The argument must be inferred too, or effects/assignments inside it
		// (e.g. `x + (File readAll: p)`) are invisible — unsound effect inference.
		inf.inferExpr(env, e.Argument)
		return inf.inferSend(recvType, e.Selector, e.SpanVal.Start)
	case *compiler.KeywordMessage:
		recvType := inf.inferExpr(env, e.Receiver)
		for _, arg := range e.Arguments {
			inf.inferExpr(env, arg)
		}
		return inf.inferSend(recvType, e.Selector, e.SpanVal.Start)
	case *compiler.Cascade:
		recvType := inf.inferExpr(env, e.Receiver)
		// Infer each cascaded message's arguments too (they were skipped
		// entirely, hiding their effects).
		// A cascade answers its last message's result; each part's chain
		// sends its later messages to the previous result.
		result := recvType
		for _, msg := range e.Messages {
			for _, arg := range msg.Arguments {
				inf.inferExpr(env, arg)
			}
			result = inf.inferSend(recvType, msg.Selector, e.SpanVal.Start)
			for _, next := range msg.Then {
				for _, arg := range next.Arguments {
					inf.inferExpr(env, arg)
				}
				result = inf.inferSend(result, next.Selector, e.SpanVal.Start)
			}
		}
		return result
	case *compiler.Block:
		// Walk block body for effect inference (blocks may contain effectful code)
		blockEnv := NewTypeEnv(env)
		for _, param := range e.Parameters {
			blockEnv.Set(param, &DynamicType{})
		}
		for _, temp := range e.Temps {
			blockEnv.Set(temp, &DynamicType{})
		}
		for _, stmt := range e.Statements {
			switch s := stmt.(type) {
			case *compiler.ExprStmt:
				inf.inferExpr(blockEnv, s.Expr)
			case *compiler.Return:
				inf.inferExpr(blockEnv, s.Value)
			}
		}
		return &NamedType{Name: "Block"}
	default:
		return &DynamicType{}
	}
}

// inferSend looks up the return type for a message send on a known receiver type.
// If the receiver type is Dynamic, returns Dynamic with no warning.
// If the class doesn't respond to the selector, emits a warning.
func (inf *Inferrer) inferSend(recvType MaggieType, selector string, pos compiler.Position) MaggieType {
	if IsDynamic(recvType) {
		return &DynamicType{}
	}

	className := inf.resolveTypeName(recvType)
	if className == "" {
		return &DynamicType{}
	}
	// A class-side receiver ("Foo class") dispatches through Foo's
	// ClassVTable; its return types/effects are keyed by the class-side name.
	baseName, classSide := splitClassSide(className)

	// Accumulate effects from global class usage
	if eff, ok := GlobalEffects[baseName]; ok {
		inf.inferredEffect = inf.inferredEffect.Union(eff)
	}
	// Accumulate effects from specific class+selector pairs
	if selEffects, ok := SelectorEffects[baseName]; ok {
		if eff, ok := selEffects[selector]; ok {
			inf.inferredEffect = inf.inferredEffect.Union(eff)
		}
	}
	// Propagate callee effects from the effect table (except Pure flag)
	if inf.EffectTable != nil {
		if eff, ok := inf.EffectTable.Lookup(className, selector); ok {
			inf.inferredEffect = inf.inferredEffect.Union(eff & ^EffectPure)
		}
	}

	// Look up return type in the table (direct class match)
	if retType, ok := inf.ReturnTypes.Lookup(className, selector); ok {
		if _, isSelf := retType.(*SelfType); isSelf {
			return recvType
		}
		return retType
	}

	// Check if the class actually has this method via the VM
	if inf.VM != nil {
		class := inf.lookupClass(baseName)
		if class != nil {
			vt := class.VTable
			if classSide {
				vt = class.ClassVTable
			}
			selectorID := inf.VM.Selectors.Lookup(selector)
			hasMethod := false
			if selectorID >= 0 {
				if vt != nil && vt.Lookup(selectorID) != nil {
					hasMethod = true
				}
			}
			if !hasMethod {
				// Check if inherited from Object (common methods)
				if retType, ok := inf.ReturnTypes.Lookup("Object", selector); ok {
					if _, isSelf := retType.(*SelfType); isSelf {
						return recvType
					}
					return retType
				}
				inf.addDiagnostic(pos,
					fmt.Sprintf("%s does not understand #%s", className, selector))
			}
			// Class exists but method has no return type info -> Dynamic
			return &DynamicType{}
		}
	}

	// Class not in VM -- fall back to Object entries in the return type table
	if retType, ok := inf.ReturnTypes.Lookup("Object", selector); ok {
		if _, isSelf := retType.(*SelfType); isSelf {
			return recvType
		}
		return retType
	}

	return &DynamicType{}
}

// resolveTypeName extracts the class name from a MaggieType.
func (inf *Inferrer) resolveTypeName(t MaggieType) string {
	switch v := t.(type) {
	case *NamedType:
		return v.Name
	case *SelfType:
		return inf.selfType().Name
	default:
		return ""
	}
}

// lookupClass resolves a class name to a VM class, honoring the current
// namespace and imports and falling back to the bare name.
func (inf *Inferrer) lookupClass(name string) *vm.Class {
	if inf.VM == nil || name == "" {
		return nil
	}
	return inf.VM.Classes.LookupWithImports(name, inf.namespace, inf.imports)
}

// isAssignable reports whether a value of type from may be returned where
// type to is declared: Dynamic either way, nil to anything, anything to
// Object, SmallInteger/BigInteger to Integer, a class to its superclasses,
// and a class to a protocol it satisfies. When the relationship cannot be
// determined (a class neither loaded nor defined in the checked sources)
// the value is accepted — the checker only warns on what it can prove.
func (inf *Inferrer) isAssignable(from, to MaggieType) bool {
	if IsDynamic(from) || IsDynamic(to) {
		return true
	}
	fromName := inf.resolveTypeName(from)
	toName := inf.resolveTypeName(to)
	if fromName == "" || toName == "" {
		return true
	}
	if fromName == "UndefinedObject" || toName == "Object" {
		return true
	}
	if protocol := inf.Protocols.Lookup(toName); protocol != nil {
		if _, classSide := splitClassSide(fromName); classSide || inf.VM == nil {
			return true
		}
		if class := inf.lookupClass(fromName); class != nil {
			return Satisfies(class, protocol, inf.VM.Selectors)
		}
		return true
	}
	fromBase, fromSide := splitClassSide(fromName)
	toBase, toSide := splitClassSide(toName)
	if fromSide != toSide {
		return false
	}
	return inf.isSubclassName(fromBase, toBase)
}

// isSubclassName walks from's superclass chain — through classes defined
// in the checked sources, then VM classes — looking for to.
func (inf *Inferrer) isSubclassName(from, to string) bool {
	toClass := inf.lookupClass(to)
	seen := make(map[string]bool)
	for cur := from; cur != "" && !seen[cur]; {
		seen[cur] = true
		if inf.sameClassName(cur, to) {
			return true
		}
		// Integer is an alias for the concrete integer classes.
		if to == "Integer" && (cur == "SmallInteger" || cur == "BigInteger") {
			return true
		}
		if super, ok := inf.Superclasses[cur]; ok {
			cur = super
			continue
		}
		if super, ok := inf.Superclasses[qualifyName(inf.namespace, cur)]; ok {
			cur = super
			continue
		}
		class := inf.lookupClass(cur)
		if class == nil {
			return true // unknown hierarchy: can't prove a mismatch
		}
		if toClass != nil {
			return class.IsSubclassOf(toClass)
		}
		// to isn't a VM class: continue up through VM superclass names
		// (to may be an alias such as Integer).
		if class.Superclass == nil {
			return false
		}
		cur = class.Superclass.FullName()
	}
	return false
}

// sameClassName reports whether a and b name the same class, allowing one
// to be namespace-qualified and the other not.
func (inf *Inferrer) sameClassName(a, b string) bool {
	if a == b || qualifyName(inf.namespace, a) == qualifyName(inf.namespace, b) {
		return true
	}
	ca, cb := inf.lookupClass(a), inf.lookupClass(b)
	return ca != nil && ca == cb
}

func (inf *Inferrer) addDiagnostic(pos compiler.Position, message string) {
	inf.diagnostics = append(inf.diagnostics, Diagnostic{Pos: pos, Message: message})
}
