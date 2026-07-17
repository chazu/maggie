package types

import (
	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// ProtocolRegistry maps protocol names to their definitions.
type ProtocolRegistry struct {
	protocols map[string]*ProtocolType
}

// NewProtocolRegistry creates an empty protocol registry.
func NewProtocolRegistry() *ProtocolRegistry {
	return &ProtocolRegistry{protocols: make(map[string]*ProtocolType)}
}

// Register adds a protocol to the registry.
func (r *ProtocolRegistry) Register(p *ProtocolType) {
	r.protocols[p.Name] = p
}

// Lookup returns a protocol by name, or nil if not found.
func (r *ProtocolRegistry) Lookup(name string) *ProtocolType {
	return r.protocols[name]
}

// RegisterFromAST builds a ProtocolType from a parsed ProtocolDef and
// registers it. Resolves includes by looking up previously registered
// protocols.
func (r *ProtocolRegistry) RegisterFromAST(def *compiler.ProtocolDef) *ProtocolType {
	pt := &ProtocolType{
		Name:    def.Name,
		Methods: make(map[string]*MethodSig),
	}

	// Copy included protocol entries
	for _, includeName := range def.Includes {
		if included := r.Lookup(includeName); included != nil {
			for sel, sig := range included.Methods {
				pt.Methods[sel] = sig
			}
		}
	}

	// Add this protocol's own entries
	for _, entry := range def.Entries {
		sig := &MethodSig{
			ReturnType: typeExprToType(entry.ReturnType),
		}
		for _, paramType := range entry.ParamTypes {
			sig.ParamTypes = append(sig.ParamTypes, typeExprToType(paramType))
		}
		pt.Methods[entry.Selector] = sig
	}

	r.Register(pt)
	return pt
}

// Satisfies checks whether a VM class structurally satisfies a protocol: it
// responds to every selector the protocol requires. Arity is covered
// implicitly — a keyword/binary selector string already encodes its argument
// count — but parameter and return TYPE compatibility is NOT checked here.
func Satisfies(class *vm.Class, protocol *ProtocolType, selectors *vm.SelectorTable) bool {
	for selector := range protocol.Methods {
		selectorID := selectors.Intern(selector)

		// Check instance methods
		var method vm.Method
		if class.VTable != nil {
			method = class.VTable.Lookup(selectorID)
		}
		if method == nil {
			return false
		}
	}
	return true
}

// typeExprToType converts a parsed TypeExpr to a MaggieType.
func typeExprToType(expr *compiler.TypeExpr) MaggieType {
	if expr == nil {
		return &DynamicType{}
	}
	switch expr.Name {
	case "Dynamic":
		return &DynamicType{}
	case "Self":
		return &SelfType{}
	default:
		return &NamedType{Name: expr.Name}
	}
}
