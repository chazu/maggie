package types

import (
	"fmt"

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
	VM          *vm.VM
	Verbose     bool

	className   string       // class being checked
	diagnostics []Diagnostic // collected during inference
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

// InferMethod performs type inference on a method body.
// Returns the inferred return type and any diagnostics generated.
func (inf *Inferrer) InferMethod(className string, md *compiler.MethodDef) (MaggieType, []Diagnostic) {
	inf.className = className
	inf.diagnostics = nil

	env := NewTypeEnv(nil)

	// Bind self
	env.Set("self", &NamedType{Name: className})

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
		if !IsDynamic(declared) && !IsDynamic(lastReturnType) {
			// Resolve Self to the class name for comparison
			declaredName := inf.resolveTypeName(declared)
			inferredName := inf.resolveTypeName(lastReturnType)
			if declaredName != inferredName && declaredName != "" && inferredName != "" {
				inf.addDiagnostic(md.SpanVal.Start,
					fmt.Sprintf("inferred return type %s is not assignable to declared %s",
						lastReturnType.String(), declared.String()))
			}
		}
	}

	if lastReturnType == nil {
		lastReturnType = &DynamicType{}
	}

	diags := inf.diagnostics
	inf.diagnostics = nil
	return lastReturnType, diags
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
		return &NamedType{Name: "Array"}
	case *compiler.DynamicArray:
		return &NamedType{Name: "Array"}
	case *compiler.Self:
		return &NamedType{Name: inf.className}
	case *compiler.Super:
		return &NamedType{Name: inf.className}
	case *compiler.ThisContext:
		return &DynamicType{}
	case *compiler.Variable:
		if t, ok := env.Lookup(e.Name); ok {
			return t
		}
		return &DynamicType{}
	case *compiler.Assignment:
		valType := inf.inferExpr(env, e.Value)
		env.Set(e.Variable, valType)
		return valType
	case *compiler.UnaryMessage:
		recvType := inf.inferExpr(env, e.Receiver)
		return inf.inferSend(recvType, e.Selector, e.SpanVal.Start)
	case *compiler.BinaryMessage:
		recvType := inf.inferExpr(env, e.Receiver)
		return inf.inferSend(recvType, e.Selector, e.SpanVal.Start)
	case *compiler.KeywordMessage:
		recvType := inf.inferExpr(env, e.Receiver)
		return inf.inferSend(recvType, e.Selector, e.SpanVal.Start)
	case *compiler.Cascade:
		recvType := inf.inferExpr(env, e.Receiver)
		// Cascade returns the receiver
		return recvType
	case *compiler.Block:
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

	// Look up return type in the table (direct class match)
	if retType, ok := inf.ReturnTypes.Lookup(className, selector); ok {
		if _, isSelf := retType.(*SelfType); isSelf {
			return recvType
		}
		return retType
	}

	// Check if the class actually has this method via the VM
	if inf.VM != nil {
		class := inf.VM.Classes.Lookup(className)
		if class != nil {
			selectorID := inf.VM.Selectors.Lookup(selector)
			hasMethod := false
			if selectorID >= 0 {
				if class.VTable != nil && class.VTable.Lookup(selectorID) != nil {
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
		return inf.className
	default:
		return ""
	}
}

func (inf *Inferrer) addDiagnostic(pos compiler.Position, message string) {
	inf.diagnostics = append(inf.diagnostics, Diagnostic{Pos: pos, Message: message})
}
