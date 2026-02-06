package hash_test

import (
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/compiler/hash"
)

func TestHashMethod_EndToEnd_NonZero(t *testing.T) {
	src := `Foo subclass: Object
  method: add: x to: y [ ^x + y ]
`
	sf, err := compiler.ParseSourceFileFromString(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	if len(sf.Classes) == 0 || len(sf.Classes[0].Methods) == 0 {
		t.Fatal("expected at least one class with one method")
	}

	md := sf.Classes[0].Methods[0]
	h := hash.HashMethod(md, nil, nil)

	var zero [32]byte
	if h == zero {
		t.Error("hash should be non-zero for a valid method")
	}
}

func TestHashMethod_EndToEnd_Deterministic(t *testing.T) {
	src := `Foo subclass: Object
  method: add: x to: y [ ^x + y ]
`
	sf1, _ := compiler.ParseSourceFileFromString(src)
	sf2, _ := compiler.ParseSourceFileFromString(src)

	md1 := sf1.Classes[0].Methods[0]
	md2 := sf2.Classes[0].Methods[0]

	h1 := hash.HashMethod(md1, nil, nil)
	h2 := hash.HashMethod(md2, nil, nil)

	if h1 != h2 {
		t.Error("same source should produce identical hashes")
	}
}

func TestHashMethod_EndToEnd_DifferentBodyDifferentHash(t *testing.T) {
	src1 := `Foo subclass: Object
  method: add: x to: y [ ^x + y ]
`
	src2 := `Foo subclass: Object
  method: add: x to: y [ ^x - y ]
`
	sf1, _ := compiler.ParseSourceFileFromString(src1)
	sf2, _ := compiler.ParseSourceFileFromString(src2)

	h1 := hash.HashMethod(sf1.Classes[0].Methods[0], nil, nil)
	h2 := hash.HashMethod(sf2.Classes[0].Methods[0], nil, nil)

	if h1 == h2 {
		t.Error("different bodies should produce different hashes")
	}
}

func TestHashMethod_EndToEnd_AlphaEquivalent(t *testing.T) {
	// These two methods have different parameter names but identical semantics
	src1 := `Foo subclass: Object
  method: add: x to: y [ ^x + y ]
`
	src2 := `Foo subclass: Object
  method: add: a to: b [ ^a + b ]
`
	sf1, _ := compiler.ParseSourceFileFromString(src1)
	sf2, _ := compiler.ParseSourceFileFromString(src2)

	h1 := hash.HashMethod(sf1.Classes[0].Methods[0], nil, nil)
	h2 := hash.HashMethod(sf2.Classes[0].Methods[0], nil, nil)

	if h1 != h2 {
		t.Error("alpha-equivalent methods should produce identical hashes")
	}
}

func TestHashMethod_EndToEnd_WithInstanceVars(t *testing.T) {
	src := `Foo subclass: Object
  instanceVars: name age
  method: name [ ^name ]
`
	sf, err := compiler.ParseSourceFileFromString(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	md := sf.Classes[0].Methods[0]
	instVars := map[string]int{"name": 0, "age": 1}

	h := hash.HashMethod(md, instVars, nil)

	var zero [32]byte
	if h == zero {
		t.Error("hash should be non-zero")
	}

	// Different ivar index should change hash
	instVars2 := map[string]int{"name": 1, "age": 0}
	h2 := hash.HashMethod(md, instVars2, nil)

	if h == h2 {
		t.Error("different ivar indices should produce different hashes")
	}
}

func TestHashMethod_EndToEnd_WithGlobalResolve(t *testing.T) {
	src := `Foo subclass: Object
  method: test [ ^Button new ]
`
	sf, _ := compiler.ParseSourceFileFromString(src)
	md := sf.Classes[0].Methods[0]

	// Without FQN resolution
	h1 := hash.HashMethod(md, nil, nil)

	// With FQN resolution
	h2 := hash.HashMethod(md, nil, func(name string) string {
		if name == "Button" {
			return "Widgets::Button"
		}
		return name
	})

	if h1 == h2 {
		t.Error("different global resolution should produce different hashes")
	}
}
