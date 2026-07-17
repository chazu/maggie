package hash

import (
	"testing"

	"github.com/chazu/maggie/compiler"
)

// TestHashMethod_PrimitiveStubDiffersFromEmpty regresses the content-address
// collision: a `[ <primitive> ]` stub and an empty-body method with the same
// signature must hash differently (the hasher keyed on the never-set
// MethodDef.Primitive instead of IsPrimitiveStub).
func TestHashMethod_PrimitiveStubDiffersFromEmpty(t *testing.T) {
	stub := &compiler.MethodDef{Selector: "foo", IsPrimitiveStub: true}
	empty := &compiler.MethodDef{Selector: "foo"}

	hs := HashMethod(stub, nil, nil)
	he := HashMethod(empty, nil, nil)
	if hs == he {
		t.Error("a <primitive> stub and an empty-body method must not share a content hash")
	}
}
