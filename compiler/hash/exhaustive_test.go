package hash

import (
	"testing"

	"github.com/chazu/maggie/compiler"
)

func mustParseMethod(t *testing.T, source string) *compiler.MethodDef {
	t.Helper()
	md, err := compiler.ParseMethodDef(source)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	return md
}

// TestHashMethod_BigIntLiteralsDistinct is the regression test for the
// bigint collision: literals outside the int64 range used to hash as
// hIntLiteral{0}, so distinct methods shared one content hash — a
// correctness/security hole in a store that dedups and trusts by hash.
func TestHashMethod_BigIntLiteralsDistinct(t *testing.T) {
	h1 := HashMethod(mustParseMethod(t, "method: a [ ^123456789012345678901234567890 ]"), nil, nil)
	h2 := HashMethod(mustParseMethod(t, "method: a [ ^999999999999999999999999999999 ]"), nil, nil)
	h3 := HashMethod(mustParseMethod(t, "method: a [ ^0 ]"), nil, nil)
	hNeg := HashMethod(mustParseMethod(t, "method: a [ ^0 - 123456789012345678901234567890 ]"), nil, nil)

	if h1 == h2 {
		t.Error("distinct big integer literals hash equal")
	}
	if h1 == h3 || h2 == h3 {
		t.Error("big integer literal hashes equal to ^0")
	}
	if h1 == hNeg {
		t.Error("sign not reflected in big integer hash")
	}

	// Same literal still hashes identically.
	h1b := HashMethod(mustParseMethod(t, "method: a [ ^123456789012345678901234567890 ]"), nil, nil)
	if h1 != h1b {
		t.Error("identical big integer literals hash differently")
	}
}

// fakeExpr is an AST node type the hasher does not know about.
type fakeExpr struct{ compiler.NilLiteral }

// TestNormalize_UnknownNodePanics locks in the guard: an AST node type
// without a normalize case must panic loudly (dev-time failure) instead of
// silently hashing as nil and colliding distinct methods.
func TestNormalize_UnknownNodePanics(t *testing.T) {
	defer func() {
		if recover() == nil {
			t.Fatal("normalizing an unknown node type must panic")
		}
	}()

	md := mustParseMethod(t, "method: a [ ^1 ]")
	md.Statements = append(md.Statements, &compiler.ExprStmt{Expr: &fakeExpr{}})
	HashMethod(md, nil, nil)
}

// TestHashMethod_EveryNodeType hashes a method exercising every expression
// node type the parser can produce. The normalizer and serializer panic on
// unhandled types, so this catches drift between ast.go and the hasher.
func TestHashMethod_EveryNodeType(t *testing.T) {
	source := `method: kitchenSink: p [ | t |
  t := 1.
  t := 1.5.
  t := 'string'.
  t := #symbol.
  t := $c.
  t := #(1 2 3).
  t := { t. p }.
  t := 123456789012345678901234567890.
  t := nil.
  t := true.
  t := false.
  t := self.
  t := thisContext.
  t := Global.
  t := t foo.
  t := t + p.
  t := t at: 1 put: p.
  t := [ :a | a value ].
  t println; foo; add: 1.
  super foo.
  ^t
]`
	md := mustParseMethod(t, source)
	h := HashMethod(md, nil, nil)
	if h == ([32]byte{}) {
		t.Fatal("zero hash")
	}
}
