// Package runtime provides the shared runtime for Trashtalk native plugins.
// All plugins link against this library and share a common object space,
// message dispatch, and block execution infrastructure.
package runtime

import (
	"encoding/json"
	"fmt"
	"strconv"
)

// ValueType represents the type of a Trashtalk value
type ValueType int

const (
	TypeNil ValueType = iota
	TypeInt
	TypeFloat
	TypeString
	TypeBool
	TypeInstance
	TypeBlock
	TypeArray
	TypeError
)

// Value is the Go representation of a Trashtalk value
type Value struct {
	Type        ValueType
	IntVal      int64
	FloatVal    float64
	StringVal   string
	InstanceVal *Instance
	BlockVal    *Block
	ArrayVal    *Array
	ErrorMsg    string
}

// NilValue returns a nil value
func NilValue() Value {
	return Value{Type: TypeNil}
}

// IntValue creates an integer value
func IntValue(n int64) Value {
	return Value{Type: TypeInt, IntVal: n}
}

// FloatValue creates a float value
func FloatValue(f float64) Value {
	return Value{Type: TypeFloat, FloatVal: f}
}

// StringValue creates a string value
func StringValue(s string) Value {
	return Value{Type: TypeString, StringVal: s}
}

// BoolValue creates a boolean value
func BoolValue(b bool) Value {
	if b {
		return Value{Type: TypeBool, IntVal: 1}
	}
	return Value{Type: TypeBool, IntVal: 0}
}

// InstanceValue creates an instance reference value
func InstanceValue(inst *Instance) Value {
	return Value{Type: TypeInstance, InstanceVal: inst}
}

// BlockValue creates a block reference value
func BlockValue(block *Block) Value {
	return Value{Type: TypeBlock, BlockVal: block}
}

// ArrayValue creates an array value
func ArrayValue(arr *Array) Value {
	return Value{Type: TypeArray, ArrayVal: arr}
}

// ErrorValue creates an error value
func ErrorValue(msg string) Value {
	return Value{Type: TypeError, ErrorMsg: msg}
}

// IsNil returns true if the value is nil
func (v Value) IsNil() bool {
	return v.Type == TypeNil
}

// IsTruthy returns true for values that are considered "true" in conditionals
func (v Value) IsTruthy() bool {
	switch v.Type {
	case TypeNil:
		return false
	case TypeBool:
		return v.IntVal != 0
	case TypeInt:
		return v.IntVal != 0
	case TypeFloat:
		return v.FloatVal != 0
	case TypeString:
		return v.StringVal != "" && v.StringVal != "false" && v.StringVal != "nil"
	case TypeError:
		return false
	default:
		return true
	}
}

// AsString converts the value to a string representation
func (v Value) AsString() string {
	switch v.Type {
	case TypeNil:
		return ""
	case TypeInt:
		return strconv.FormatInt(v.IntVal, 10)
	case TypeFloat:
		return strconv.FormatFloat(v.FloatVal, 'f', -1, 64)
	case TypeString:
		return v.StringVal
	case TypeBool:
		if v.IntVal != 0 {
			return "true"
		}
		return "false"
	case TypeInstance:
		if v.InstanceVal != nil {
			return v.InstanceVal.ID
		}
		return ""
	case TypeBlock:
		if v.BlockVal != nil {
			return v.BlockVal.ID
		}
		return ""
	case TypeArray:
		if v.ArrayVal != nil {
			return v.ArrayVal.ToJSON()
		}
		return "[]"
	case TypeError:
		return "Error: " + v.ErrorMsg
	default:
		return ""
	}
}

// AsInt converts the value to an integer
func (v Value) AsInt() int64 {
	switch v.Type {
	case TypeInt:
		return v.IntVal
	case TypeFloat:
		return int64(v.FloatVal)
	case TypeBool:
		return v.IntVal
	case TypeString:
		n, _ := strconv.ParseInt(v.StringVal, 10, 64)
		return n
	default:
		return 0
	}
}

// AsFloat converts the value to a float
func (v Value) AsFloat() float64 {
	switch v.Type {
	case TypeFloat:
		return v.FloatVal
	case TypeInt:
		return float64(v.IntVal)
	case TypeBool:
		return float64(v.IntVal)
	case TypeString:
		f, _ := strconv.ParseFloat(v.StringVal, 64)
		return f
	default:
		return 0
	}
}

// ToJSON serializes the value to JSON
func (v Value) ToJSON() string {
	switch v.Type {
	case TypeNil:
		return "null"
	case TypeInt:
		return strconv.FormatInt(v.IntVal, 10)
	case TypeFloat:
		return strconv.FormatFloat(v.FloatVal, 'f', -1, 64)
	case TypeString:
		data, _ := json.Marshal(v.StringVal)
		return string(data)
	case TypeBool:
		if v.IntVal != 0 {
			return "true"
		}
		return "false"
	case TypeInstance:
		if v.InstanceVal != nil {
			return v.InstanceVal.ToJSON()
		}
		return "null"
	case TypeBlock:
		if v.BlockVal != nil {
			return fmt.Sprintf(`{"_block_id":"%s"}`, v.BlockVal.ID)
		}
		return "null"
	case TypeArray:
		if v.ArrayVal != nil {
			return v.ArrayVal.ToJSON()
		}
		return "[]"
	case TypeError:
		return fmt.Sprintf(`{"_error":"%s"}`, v.ErrorMsg)
	default:
		return "null"
	}
}

// ValueFromJSON parses a JSON value
func ValueFromJSON(jsonStr string) Value {
	if jsonStr == "" || jsonStr == "null" {
		return NilValue()
	}
	if jsonStr == "true" {
		return BoolValue(true)
	}
	if jsonStr == "false" {
		return BoolValue(false)
	}
	// Try integer
	if n, err := strconv.ParseInt(jsonStr, 10, 64); err == nil {
		return IntValue(n)
	}
	// Try float
	if f, err := strconv.ParseFloat(jsonStr, 64); err == nil {
		return FloatValue(f)
	}
	// Try JSON string
	if len(jsonStr) >= 2 && jsonStr[0] == '"' && jsonStr[len(jsonStr)-1] == '"' {
		var s string
		if err := json.Unmarshal([]byte(jsonStr), &s); err == nil {
			return StringValue(s)
		}
	}
	// Default to string
	return StringValue(jsonStr)
}

// Array represents a Trashtalk array
type Array struct {
	Elements []Value
}

// NewArray creates a new empty array
func NewArray() *Array {
	return &Array{Elements: make([]Value, 0)}
}

// Push adds an element to the array
func (a *Array) Push(v Value) {
	a.Elements = append(a.Elements, v)
}

// At returns the element at the given index
func (a *Array) At(idx int) Value {
	if idx < 0 || idx >= len(a.Elements) {
		return NilValue()
	}
	return a.Elements[idx]
}

// AtPut sets the element at the given index
func (a *Array) AtPut(idx int, v Value) {
	if idx >= 0 && idx < len(a.Elements) {
		a.Elements[idx] = v
	}
}

// Len returns the length of the array
func (a *Array) Len() int {
	return len(a.Elements)
}

// ToJSON serializes the array to JSON
func (a *Array) ToJSON() string {
	if a == nil || len(a.Elements) == 0 {
		return "[]"
	}
	result := "["
	for i, elem := range a.Elements {
		if i > 0 {
			result += ","
		}
		result += elem.ToJSON()
	}
	result += "]"
	return result
}

