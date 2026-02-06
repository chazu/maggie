// Package gowrap introspects Go packages and generates Maggie bindings.
package gowrap

import "go/types"

// PackageModel is the in-memory representation of a Go package's exported API.
type PackageModel struct {
	ImportPath string
	Name       string // short package name (e.g., "json")
	Functions  []FunctionModel
	Types      []TypeModel
	Constants  []ConstantModel
}

// TypeModel represents an exported Go type (struct or named type).
type TypeModel struct {
	Name       string
	GoType     types.Type
	IsStruct   bool
	Fields     []FieldModel
	Methods    []FunctionModel // pointer-receiver methods
}

// FunctionModel represents an exported function or method.
type FunctionModel struct {
	Name       string
	IsMethod   bool
	RecvType   string // non-empty for methods (e.g., "*Server")
	Params     []ParamModel
	Results    []ParamModel
	ReturnsErr bool // true if last result is error
}

// ParamModel represents a function parameter or result.
type ParamModel struct {
	Name    string
	GoType  types.Type
	TypeStr string // human-readable type string (e.g., "string", "*http.Server")
}

// FieldModel represents a struct field.
type FieldModel struct {
	Name    string
	GoType  types.Type
	TypeStr string
}

// ConstantModel represents an exported constant.
type ConstantModel struct {
	Name    string
	TypeStr string
	Value   string // literal value
}
