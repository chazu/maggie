package vm

import (
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/BurntSushi/toml"
)

// ---------------------------------------------------------------------------
// TOML Parsing and Encoding Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerTomlPrimitives() {
	// Create Toml class
	tomlClass := vm.createClass("Toml", vm.ObjectClass)
	vm.Globals["Toml"] = vm.classValue(tomlClass)

	// Create TomlParseError exception class
	tomlParseErrorClass := vm.createClass("TomlParseError", vm.ErrorClass)
	vm.Globals["TomlParseError"] = vm.classValue(tomlParseErrorClass)

	// ---------------------------------------------------------------------------
	// decode: aString - Parse a TOML string into a Dictionary
	// ---------------------------------------------------------------------------
	tomlClass.AddClassMethod1(vm.Selectors, "decode:", func(vmPtr interface{}, recv Value, strVal Value) Value {
		v := vmPtr.(*VM)

		src := v.valueToString(strVal)
		if src == "" && !IsStringValue(strVal) {
			return v.signalException(tomlParseErrorClass,
				v.registry.NewStringValue("decode: requires a string argument"))
		}

		var data map[string]interface{}
		_, err := toml.Decode(src, &data)
		if err != nil {
			return v.signalTomlParseError(tomlParseErrorClass, err)
		}

		return v.goMapToMaggieDictionary(data)
	})

	// ---------------------------------------------------------------------------
	// decodeFile: aPath - Read and parse a TOML file into a Dictionary
	// ---------------------------------------------------------------------------
	tomlClass.AddClassMethod1(vm.Selectors, "decodeFile:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)

		path := v.valueToString(pathVal)
		if path == "" {
			return v.signalException(tomlParseErrorClass,
				v.registry.NewStringValue("decodeFile: requires a path string"))
		}

		content, err := os.ReadFile(path)
		if err != nil {
			return v.signalException(tomlParseErrorClass,
				v.registry.NewStringValue("Cannot read file: "+err.Error()))
		}

		var data map[string]interface{}
		_, err = toml.Decode(string(content), &data)
		if err != nil {
			return v.signalTomlParseError(tomlParseErrorClass, err)
		}

		return v.goMapToMaggieDictionary(data)
	})

	// ---------------------------------------------------------------------------
	// encode: aDictionary - Encode a Dictionary as a TOML string
	// ---------------------------------------------------------------------------
	tomlClass.AddClassMethod1(vm.Selectors, "encode:", func(vmPtr interface{}, recv Value, dictVal Value) Value {
		v := vmPtr.(*VM)

		if !IsDictionaryValue(dictVal) {
			return v.signalException(tomlParseErrorClass,
				v.registry.NewStringValue("encode: requires a Dictionary argument"))
		}

		goMap := v.maggieDictionaryToGoMap(dictVal)

		var buf strings.Builder
		enc := toml.NewEncoder(&buf)
		err := enc.Encode(goMap)
		if err != nil {
			return v.signalException(tomlParseErrorClass,
				v.registry.NewStringValue("TOML encode error: "+err.Error()))
		}

		return v.registry.NewStringValue(buf.String())
	})
}

// signalTomlParseError signals a TomlParseError with line/column info from the parse error.
func (vm *VM) signalTomlParseError(tomlParseErrorClass *Class, err error) Value {
	msg := fmt.Sprintf("TOML parse error: %s", err.Error())
	return vm.signalException(tomlParseErrorClass, vm.registry.NewStringValue(msg))
}

// goMapToMaggieDictionary converts a Go map[string]interface{} to a Maggie Dictionary.
func (vm *VM) goMapToMaggieDictionary(m map[string]interface{}) Value {
	dict := vm.NewDictionary()
	for k, v := range m {
		key := vm.registry.NewStringValue(k)
		val := vm.goValueToMaggieValue(v)
		vm.DictionaryAtPut(dict, key, val)
	}
	return dict
}

// goValueToMaggieValue converts a Go value (from TOML decoding) to a Maggie Value.
func (vm *VM) goValueToMaggieValue(v interface{}) Value {
	switch val := v.(type) {
	case string:
		return vm.registry.NewStringValue(val)
	case int64:
		return FromSmallInt(val)
	case float64:
		return FromFloat64(val)
	case bool:
		if val {
			return True
		}
		return False
	case time.Time:
		// TOML datetime -> ISO 8601 string
		return vm.registry.NewStringValue(val.Format(time.RFC3339))
	case map[string]interface{}:
		return vm.goMapToMaggieDictionary(val)
	case []map[string]interface{}:
		// Array of tables
		elements := make([]Value, len(val))
		for i, item := range val {
			elements[i] = vm.goMapToMaggieDictionary(item)
		}
		return vm.NewArrayWithElements(elements)
	case []interface{}:
		elements := make([]Value, len(val))
		for i, item := range val {
			elements[i] = vm.goValueToMaggieValue(item)
		}
		return vm.NewArrayWithElements(elements)
	default:
		return Nil
	}
}

// maggieDictionaryToGoMap converts a Maggie Dictionary to a Go map for TOML encoding.
func (vm *VM) maggieDictionaryToGoMap(dictVal Value) map[string]interface{} {
	dict := vm.registry.GetDictionaryObject(dictVal)
	if dict == nil {
		return map[string]interface{}{}
	}

	result := make(map[string]interface{})
	for h, key := range dict.Keys {
		keyStr := vm.valueToString(key)
		if keyStr == "" {
			// Convert non-string keys to string representation
			keyStr = fmt.Sprintf("%v", key)
		}
		val := dict.Data[h]
		result[keyStr] = vm.maggieValueToGoValue(val)
	}
	return result
}

// maggieValueToGoValue converts a Maggie Value to a Go value for TOML encoding.
func (vm *VM) maggieValueToGoValue(v Value) interface{} {
	if v == Nil {
		return nil
	}
	if v == True {
		return true
	}
	if v == False {
		return false
	}
	if v.IsSmallInt() {
		return v.SmallInt()
	}
	if v.IsFloat() {
		return v.Float64()
	}
	if IsStringValue(v) {
		return vm.registry.GetStringContent(v)
	}
	if v.IsSymbol() {
		// Check for dictionary
		if IsDictionaryValue(v) {
			return vm.maggieDictionaryToGoMap(v)
		}
		// Symbol as string
		return vm.Symbols.Name(v.SymbolID())
	}
	// Check for array (object with slots)
	if obj := ObjectFromValue(v); obj != nil {
		if vt := obj.VTablePtr(); vt != nil && vt == vm.ArrayClass.VTable {
			size := obj.NumSlots()
			elements := make([]interface{}, size)
			for i := 0; i < size; i++ {
				elements[i] = vm.maggieValueToGoValue(obj.GetSlot(i))
			}
			return elements
		}
	}
	return nil
}
