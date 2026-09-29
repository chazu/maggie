package hash

import (
	"encoding/hex"
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

// Cascade message chains (`x foo; bar baz`) must hash differently from the
// same messages as separate cascade parts — while a chain-free cascade keeps
// exactly the hash it had before chains existed (content addresses of
// existing code must not move).
func TestHashCascadeChains(t *testing.T) {
	plain := HashMethod(mustParseMethod(t, "method: m [ ^self foo; bar: 1 + 2; baz ]"), nil, nil)
	const pinned = "8629343085a5f437c258136a01fd062146290032e3f052fc139fbc20c17808e7"
	if got := hex.EncodeToString(plain[:]); got != pinned {
		t.Errorf("chain-free cascade hash moved: %s, want %s", got, pinned)
	}
	chained := HashMethod(mustParseMethod(t, "method: m [ ^self foo; bar baz ]"), nil, nil)
	split := HashMethod(mustParseMethod(t, "method: m [ ^self foo; bar; baz ]"), nil, nil)
	if chained == split {
		t.Error("`foo; bar baz` and `foo; bar; baz` must not share a content hash")
	}
}
