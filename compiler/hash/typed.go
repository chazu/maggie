package hash

import (
	"crypto/sha256"
	"sort"

	"github.com/chazu/maggie/compiler"
)

// HashTypedMethod computes the typed content hash of a method definition.
// It includes type annotations in the hash. Methods without type annotations
// produce a distinct hash from HashMethod (the typed domain includes a
// presence byte that the semantic domain does not).
func HashTypedMethod(method *compiler.MethodDef, instVars map[string]int, resolveGlobal func(string) string) [32]byte {
	hm := NormalizeTypedMethod(method, instVars, resolveGlobal)
	data := SerializeTyped(hm)
	return sha256.Sum256(data)
}

// NormalizeTypedMethod transforms a compiler MethodDef into a frozen HMethodDef
// with type annotation fields populated. It reuses the existing NormalizeMethod
// for the semantic fields, then populates type annotations from the AST.
func NormalizeTypedMethod(method *compiler.MethodDef, instVars map[string]int, resolveGlobal func(string) string) *HMethodDef {
	if resolveGlobal == nil {
		resolveGlobal = func(name string) string { return name }
	}

	// Get the base normalized method (semantic fields)
	hm := NormalizeMethod(method, instVars, resolveGlobal)

	// Populate type annotations from the AST
	if len(method.ParamTypes) > 0 {
		hm.ParamTypes = make([]HTypeAnnotation, len(method.ParamTypes))
		for i, pt := range method.ParamTypes {
			if pt != nil {
				hm.ParamTypes[i] = HTypeAnnotation{Name: resolveGlobal(pt.Name)}
			}
		}
	}

	if len(method.TempTypes) > 0 {
		hm.TempTypes = make([]HTypeAnnotation, len(method.TempTypes))
		for i, tt := range method.TempTypes {
			if tt != nil {
				hm.TempTypes[i] = HTypeAnnotation{Name: resolveGlobal(tt.Name)}
			}
		}
	}

	if method.ReturnType != nil {
		hm.ReturnType = HTypeAnnotation{Name: resolveGlobal(method.ReturnType.Name)}
	}

	// Populate effect annotations, sorted for determinism
	if len(method.Effects) > 0 {
		effects := make([]string, 0, len(method.Effects))
		for _, eff := range method.Effects {
			if eff != nil {
				effects = append(effects, eff.Name)
			}
		}
		sort.Strings(effects)
		hm.Effects = effects
	}

	return hm
}
