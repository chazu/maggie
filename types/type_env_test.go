package types

import "testing"

func TestTypeEnvSetAndLookup(t *testing.T) {
	env := NewTypeEnv(nil)
	env.Set("x", &NamedType{Name: "SmallInteger"})

	typ, ok := env.Lookup("x")
	if !ok {
		t.Fatal("expected to find x")
	}
	if typ.String() != "SmallInteger" {
		t.Errorf("expected SmallInteger, got %s", typ)
	}
}

func TestTypeEnvUnbound(t *testing.T) {
	env := NewTypeEnv(nil)
	_, ok := env.Lookup("y")
	if ok {
		t.Error("expected unbound variable to return false")
	}
}

func TestTypeEnvParentScope(t *testing.T) {
	parent := NewTypeEnv(nil)
	parent.Set("x", &NamedType{Name: "SmallInteger"})

	child := NewTypeEnv(parent)
	child.Set("y", &NamedType{Name: "String"})

	// Child can see parent's binding
	typ, ok := child.Lookup("x")
	if !ok {
		t.Fatal("child should see parent binding")
	}
	if typ.String() != "SmallInteger" {
		t.Errorf("expected SmallInteger, got %s", typ)
	}

	// Child has its own binding
	typ, ok = child.Lookup("y")
	if !ok {
		t.Fatal("child should see own binding")
	}
	if typ.String() != "String" {
		t.Errorf("expected String, got %s", typ)
	}

	// Parent cannot see child's binding
	_, ok = parent.Lookup("y")
	if ok {
		t.Error("parent should not see child binding")
	}
}

func TestTypeEnvShadowing(t *testing.T) {
	parent := NewTypeEnv(nil)
	parent.Set("x", &NamedType{Name: "SmallInteger"})

	child := NewTypeEnv(parent)
	child.Set("x", &NamedType{Name: "String"})

	// Child shadows parent
	typ, ok := child.Lookup("x")
	if !ok {
		t.Fatal("expected to find x in child")
	}
	if typ.String() != "String" {
		t.Errorf("expected String (shadowed), got %s", typ)
	}

	// Parent still has original
	typ, ok = parent.Lookup("x")
	if !ok {
		t.Fatal("expected to find x in parent")
	}
	if typ.String() != "SmallInteger" {
		t.Errorf("expected SmallInteger in parent, got %s", typ)
	}
}

func TestTypeEnvSnapshot(t *testing.T) {
	parent := NewTypeEnv(nil)
	parent.Set("x", &NamedType{Name: "SmallInteger"})

	child := NewTypeEnv(parent)
	child.Set("y", &NamedType{Name: "String"})
	child.Set("x", &NamedType{Name: "Float"}) // shadows

	snap := child.Snapshot()
	if len(snap) != 2 {
		t.Errorf("expected 2 bindings in snapshot, got %d", len(snap))
	}
	if snap["x"].String() != "Float" {
		t.Errorf("expected x=Float in snapshot, got %s", snap["x"])
	}
	if snap["y"].String() != "String" {
		t.Errorf("expected y=String in snapshot, got %s", snap["y"])
	}
}

func TestTypeEnvOverwrite(t *testing.T) {
	env := NewTypeEnv(nil)
	env.Set("x", &NamedType{Name: "SmallInteger"})
	env.Set("x", &NamedType{Name: "String"}) // last-write-wins

	typ, ok := env.Lookup("x")
	if !ok {
		t.Fatal("expected to find x")
	}
	if typ.String() != "String" {
		t.Errorf("expected String after overwrite, got %s", typ)
	}
}
