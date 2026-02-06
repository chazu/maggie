package hash

import (
	"crypto/sha256"

	"github.com/chazu/maggie/compiler"
)

// HashMethod computes the SHA-256 content hash of a method definition.
//
// The hash is computed over a deterministic serialization of the method's
// normalized AST with de Bruijn variable indexing. Two methods with the
// same semantics (same body, ignoring variable names) produce the same hash.
//
// instVars maps instance variable names to their index in the class's
// all-instance-variables list. resolveGlobal maps a bare class/global name
// to its fully-qualified name.
func HashMethod(method *compiler.MethodDef, instVars map[string]int, resolveGlobal func(string) string) [32]byte {
	hm := NormalizeMethod(method, instVars, resolveGlobal)
	data := Serialize(hm)
	return sha256.Sum256(data)
}
