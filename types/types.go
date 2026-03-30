// Package types provides a structural type checker for Maggie.
// It operates on the compiler's AST and produces warnings — it never
// blocks compilation or affects bytecode generation.
package types

// MaggieType represents a type in the Maggie type system.
type MaggieType interface {
	String() string
	Equals(other MaggieType) bool
}

// NamedType is a concrete class type (e.g., Integer, String, MyClass).
type NamedType struct {
	Name string
}

func (t *NamedType) String() string { return t.Name }
func (t *NamedType) Equals(other MaggieType) bool {
	if o, ok := other.(*NamedType); ok {
		return t.Name == o.Name
	}
	return false
}

// ProtocolType is a structural type defined by a set of message signatures.
type ProtocolType struct {
	Name    string
	Methods map[string]*MethodSig // selector → signature
}

func (t *ProtocolType) String() string { return t.Name }
func (t *ProtocolType) Equals(other MaggieType) bool {
	if o, ok := other.(*ProtocolType); ok {
		return t.Name == o.Name
	}
	return false
}

// MethodSig describes the type signature of a method.
type MethodSig struct {
	ParamTypes []MaggieType
	ReturnType MaggieType // nil means Dynamic
}

// DynamicType is the top type — compatible with everything.
// Untyped code implicitly uses Dynamic.
type DynamicType struct{}

func (t *DynamicType) String() string                { return "Dynamic" }
func (t *DynamicType) Equals(other MaggieType) bool  { _, ok := other.(*DynamicType); return ok }

// SelfType represents the type of the receiver.
type SelfType struct{}

func (t *SelfType) String() string                { return "Self" }
func (t *SelfType) Equals(other MaggieType) bool  { _, ok := other.(*SelfType); return ok }

// IsDynamic returns true if the type is Dynamic or nil (untyped).
func IsDynamic(t MaggieType) bool {
	if t == nil {
		return true
	}
	_, ok := t.(*DynamicType)
	return ok
}

// Compatible returns true if `from` is assignable to `to`.
// Dynamic is compatible with everything (gradual typing).
func Compatible(from, to MaggieType) bool {
	if IsDynamic(from) || IsDynamic(to) {
		return true
	}
	if _, ok := to.(*SelfType); ok {
		return true // Self accepts any type (checked contextually)
	}
	return from.Equals(to)
}
