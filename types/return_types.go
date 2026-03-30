package types

import (
	"github.com/chazu/maggie/compiler"
)

// ReturnTypeTable maps (className, selector) pairs to their return types.
// It is populated with built-in entries for core classes and can be
// extended by harvesting ^<Type> annotations from parsed methods.
type ReturnTypeTable struct {
	entries map[returnTypeKey]MaggieType
}

type returnTypeKey struct {
	className string
	selector  string
}

// NewReturnTypeTable creates a return type table pre-populated with
// built-in entries for core Maggie classes.
func NewReturnTypeTable() *ReturnTypeTable {
	rt := &ReturnTypeTable{
		entries: make(map[returnTypeKey]MaggieType),
	}
	rt.populateBuiltins()
	return rt
}

// Lookup returns the return type for a (className, selector) pair.
// Returns (type, true) if found, or (nil, false) if unknown.
func (rt *ReturnTypeTable) Lookup(className, selector string) (MaggieType, bool) {
	t, ok := rt.entries[returnTypeKey{className, selector}]
	if ok {
		return t, true
	}
	// Try aliases: Integer -> SmallInteger
	if className == "Integer" {
		t, ok = rt.entries[returnTypeKey{"SmallInteger", selector}]
		if ok {
			return t, true
		}
	}
	return nil, false
}

// Register adds a return type entry for a (className, selector) pair.
func (rt *ReturnTypeTable) Register(className, selector string, retType MaggieType) {
	rt.entries[returnTypeKey{className, selector}] = retType
}

// HarvestFromMethod extracts a return type annotation from a parsed method
// and registers it in the table.
func (rt *ReturnTypeTable) HarvestFromMethod(className string, md *compiler.MethodDef) {
	if md.ReturnType == nil {
		return
	}
	retType := typeExprToType(md.ReturnType)
	if !IsDynamic(retType) {
		rt.Register(className, md.Selector, retType)
	}
}

// populateBuiltins registers the ~40 core return type entries.
func (rt *ReturnTypeTable) populateBuiltins() {
	integer := &NamedType{Name: "SmallInteger"}
	float := &NamedType{Name: "Float"}
	str := &NamedType{Name: "String"}
	boolean := &NamedType{Name: "Boolean"}
	array := &NamedType{Name: "Array"}
	self := &SelfType{}

	// SmallInteger arithmetic -> Integer
	for _, sel := range []string{"+", "-", "*", "//", "\\\\", "abs", "negated"} {
		rt.Register("SmallInteger", sel, integer)
	}
	// SmallInteger comparison -> Boolean
	for _, sel := range []string{"=", "<", ">", "<=", ">=", "~="} {
		rt.Register("SmallInteger", sel, boolean)
	}
	rt.Register("SmallInteger", "asFloat", float)
	rt.Register("SmallInteger", "asString", str)
	rt.Register("SmallInteger", "printString", str)

	// Float arithmetic -> Float
	for _, sel := range []string{"+", "-", "*", "/", "abs", "negated"} {
		rt.Register("Float", sel, float)
	}
	// Float comparison -> Boolean
	for _, sel := range []string{"=", "<", ">", "<=", ">="} {
		rt.Register("Float", sel, boolean)
	}
	// Float rounding -> Integer
	for _, sel := range []string{"ceiling", "floor", "truncated", "rounded"} {
		rt.Register("Float", sel, integer)
	}
	rt.Register("Float", "asString", str)
	rt.Register("Float", "printString", str)

	// String
	rt.Register("String", "size", integer)
	rt.Register("String", ",", str)
	rt.Register("String", "copyFrom:to:", str)
	rt.Register("String", "=", boolean)
	rt.Register("String", "includes:", boolean)
	rt.Register("String", "asInteger", integer)
	rt.Register("String", "asFloat", float)

	// Array
	rt.Register("Array", "size", integer)
	rt.Register("Array", "=", boolean)
	rt.Register("Array", "isEmpty", boolean)
	rt.Register("Array", "notEmpty", boolean)
	rt.Register("Array", "includes:", boolean)
	rt.Register("Array", ",", array)
	rt.Register("Array", "copyFrom:to:", array)
	rt.Register("Array", "asString", str)
	rt.Register("Array", "printString", str)

	// Boolean
	rt.Register("Boolean", "not", boolean)
	rt.Register("Boolean", "and:", boolean)
	rt.Register("Boolean", "or:", boolean)
	rt.Register("Boolean", "asString", str)
	rt.Register("Boolean", "printString", str)

	// Object (inherited by all)
	rt.Register("Object", "=", boolean)
	rt.Register("Object", "~=", boolean)
	rt.Register("Object", "isNil", boolean)
	rt.Register("Object", "notNil", boolean)
	rt.Register("Object", "asString", str)
	rt.Register("Object", "printString", str)
	rt.Register("Object", "yourself", self)
}
