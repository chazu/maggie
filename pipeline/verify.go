package pipeline

import (
	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/compiler/hash"
	"github.com/chazu/maggie/vm"
	"github.com/chazu/maggie/vm/dist"
)

// HashMethodSource parses method source text (the "method:"/"classMethod:"
// form carried by CompiledMethod.Source and sync chunks) and computes its
// semantic and typed content hashes in the owning class's context.
//
// This is the same hashing path CompileAll uses for freshly compiled methods
// (hashAndSetMethod), so chunk verification cannot drift from pipeline
// hashing: ParseMethodDef entry point, ivar references bound by index into
// instVars, bare global references resolved to FQNs against the class table.
//
// instVars is the full inherited chain in Class.AllInstVarNames order; pass
// nil for class-side and trait methods. Imports are nil by design: synced
// source relies on globally unique or already-qualified names — the same
// documented limitation as RehydrateFromStore.
func HashMethodSource(source string, instVars []string, namespace string, classes *vm.ClassTable) (semantic, typed [32]byte, err error) {
	methodDef, err := compiler.ParseMethodDef(source)
	if err != nil {
		return semantic, typed, err
	}

	var instVarMap map[string]int
	if len(instVars) > 0 {
		instVarMap = make(map[string]int, len(instVars))
		for idx, name := range instVars {
			instVarMap[name] = idx
		}
	}

	resolveGlobal := func(name string) string {
		return resolveGlobalForHash(name, namespace, nil, classes)
	}

	semantic = hash.HashMethod(methodDef, instVarMap, resolveGlobal)
	typed = hash.HashTypedMethod(methodDef, instVarMap, resolveGlobal)
	return semantic, typed, nil
}

// VerifyCompileFunc returns a dist.CompileFunc suitable for sync-chunk
// verification, hashing through HashMethodSource against vmInst's class
// table. This is the canonical compile function to hand to the sync service.
func VerifyCompileFunc(vmInst *vm.VM) dist.CompileFunc {
	return func(source string, ctx dist.MethodContext) (dist.CompileResult, error) {
		semantic, typed, err := HashMethodSource(source, ctx.InstVars, ctx.Namespace, vmInst.Classes)
		if err != nil {
			return dist.CompileResult{}, err
		}
		return dist.CompileResult{SemanticHash: semantic, TypedHash: typed}, nil
	}
}
