package vm

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"math"
	"strings"
)

// ---------------------------------------------------------------------------
// JSON Primitives: Native JSON encode/decode for Maggie
// ---------------------------------------------------------------------------

// JsonReaderObject wraps a Go JSON decoder for streaming reads.
type JsonReaderObject struct {
	decoder *json.Decoder
	source  string // original source for error messages
}

// JsonWriterObject wraps a Go JSON encoder for streaming writes.
type JsonWriterObject struct {
	buf    *bytes.Buffer
	enc    *json.Encoder
	pretty bool
}

// ---------------------------------------------------------------------------
// JsonReader Registry helpers
// ---------------------------------------------------------------------------

func jsonReaderToValue(id int) Value {
	return FromSymbolID(uint32(id) | jsonReaderMarker)
}

func isJsonReaderValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	return (v.SymbolID() & markerMask) == jsonReaderMarker
}

func jsonReaderIDFromValue(v Value) int {
	return int(v.SymbolID() & ^markerMask)
}

// ---------------------------------------------------------------------------
// JsonWriter Registry helpers
// ---------------------------------------------------------------------------

func jsonWriterToValue(id int) Value {
	return FromSymbolID(uint32(id) | jsonWriterMarker)
}

func isJsonWriterValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	return (v.SymbolID() & markerMask) == jsonWriterMarker
}

func jsonWriterIDFromValue(v Value) int {
	return int(v.SymbolID() & ^markerMask)
}

// ---------------------------------------------------------------------------
// Registration
// ---------------------------------------------------------------------------

func (vm *VM) registerJSONPrimitives() {
	jsonClass := vm.createClass("Json", vm.ObjectClass)
	vm.Globals["Json"] = vm.classValue(jsonClass)

	jsonParseErrorClass := vm.createClass("JsonParseError", vm.ErrorClass)
	vm.Globals["JsonParseError"] = vm.classValue(jsonParseErrorClass)

	jsonReaderClass := vm.createClass("JsonReader", vm.ObjectClass)
	vm.Globals["JsonReader"] = vm.classValue(jsonReaderClass)

	jsonWriterClass := vm.createClass("JsonWriter", vm.ObjectClass)
	vm.Globals["JsonWriter"] = vm.classValue(jsonWriterClass)

	vm.symbolDispatch.Register(jsonReaderMarker, &SymbolTypeEntry{Class: jsonReaderClass})
	vm.symbolDispatch.Register(jsonWriterMarker, &SymbolTypeEntry{Class: jsonWriterClass})

	// -------------------------------------------------------------------
	// Json class methods (stateless encode/decode)
	// -------------------------------------------------------------------

	// Json encode: anObject -> String
	jsonClass.AddClassMethod1(vm.Selectors, "primEncode:", func(vmPtr interface{}, recv Value, obj Value) Value {
		v := vmPtr.(*VM)
		goVal := v.valueToGoJSON(obj)
		data, err := json.Marshal(goVal)
		if err != nil {
			return v.signalException(jsonParseErrorClass,
				v.registry.NewStringValue(fmt.Sprintf("Json encode: error: %v", err)))
		}
		return v.registry.NewStringValue(string(data))
	})

	// Json encodePretty: anObject -> String
	jsonClass.AddClassMethod1(vm.Selectors, "primEncodePretty:", func(vmPtr interface{}, recv Value, obj Value) Value {
		v := vmPtr.(*VM)
		goVal := v.valueToGoJSON(obj)
		data, err := json.MarshalIndent(goVal, "", "  ")
		if err != nil {
			return v.signalException(jsonParseErrorClass,
				v.registry.NewStringValue(fmt.Sprintf("Json encodePretty: error: %v", err)))
		}
		return v.registry.NewStringValue(string(data))
	})

	// Json decode: aString -> Object
	jsonClass.AddClassMethod1(vm.Selectors, "primDecode:", func(vmPtr interface{}, recv Value, strVal Value) Value {
		v := vmPtr.(*VM)
		if !IsStringValue(strVal) {
			return v.signalException(jsonParseErrorClass,
				v.registry.NewStringValue("Json decode: argument must be a String"))
		}
		content := v.registry.GetStringContent(strVal)

		// Use json.Decoder with UseNumber to preserve integer precision
		dec := json.NewDecoder(strings.NewReader(content))
		dec.UseNumber()

		var goResult interface{}
		if err := dec.Decode(&goResult); err != nil {
			return v.signalException(jsonParseErrorClass,
				v.registry.NewStringValue(fmt.Sprintf("Json decode: invalid JSON: %v", err)))
		}
		return v.goJSONToValue(goResult)
	})

	// -------------------------------------------------------------------
	// JsonReader (streaming decode)
	// -------------------------------------------------------------------

	// JsonReader new: aString -> JsonReader
	jsonReaderClass.AddClassMethod1(vm.Selectors, "primNew:", func(vmPtr interface{}, recv Value, strVal Value) Value {
		v := vmPtr.(*VM)
		if !IsStringValue(strVal) {
			return v.signalException(jsonParseErrorClass,
				v.registry.NewStringValue("JsonReader new: argument must be a String"))
		}
		content := v.registry.GetStringContent(strVal)
		dec := json.NewDecoder(strings.NewReader(content))
		dec.UseNumber()
		reader := &JsonReaderObject{decoder: dec, source: content}
		id := v.registry.RegisterJsonReader(reader)
		return jsonReaderToValue(id)
	})

	// JsonReader >> next -> next decoded value or nil at EOF
	jsonReaderClass.AddMethod0(vm.Selectors, "primNext", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if !isJsonReaderValue(recv) {
			return Nil
		}
		reader := v.registry.GetJsonReader(jsonReaderIDFromValue(recv))
		if reader == nil {
			return Nil
		}
		var goResult interface{}
		if err := reader.decoder.Decode(&goResult); err != nil {
			if err == io.EOF {
				return Nil
			}
			return v.signalException(jsonParseErrorClass,
				v.registry.NewStringValue(fmt.Sprintf("JsonReader next: parse error: %v", err)))
		}
		return v.goJSONToValue(goResult)
	})

	// JsonReader >> hasMore -> Boolean
	jsonReaderClass.AddMethod0(vm.Selectors, "primHasMore", func(vmPtr interface{}, recv Value) Value {
		if !isJsonReaderValue(recv) {
			return False
		}
		v := vmPtr.(*VM)
		reader := v.registry.GetJsonReader(jsonReaderIDFromValue(recv))
		if reader == nil {
			return False
		}
		if reader.decoder.More() {
			return True
		}
		return False
	})

	// -------------------------------------------------------------------
	// JsonWriter (streaming encode)
	// -------------------------------------------------------------------

	// JsonWriter new -> JsonWriter
	jsonWriterClass.AddClassMethod0(vm.Selectors, "primNew", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		buf := &bytes.Buffer{}
		enc := json.NewEncoder(buf)
		enc.SetEscapeHTML(false)
		writer := &JsonWriterObject{buf: buf, enc: enc, pretty: false}
		id := v.registry.RegisterJsonWriter(writer)
		return jsonWriterToValue(id)
	})

	// JsonWriter newPretty -> JsonWriter
	jsonWriterClass.AddClassMethod0(vm.Selectors, "primNewPretty", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		buf := &bytes.Buffer{}
		enc := json.NewEncoder(buf)
		enc.SetEscapeHTML(false)
		enc.SetIndent("", "  ")
		writer := &JsonWriterObject{buf: buf, enc: enc, pretty: true}
		id := v.registry.RegisterJsonWriter(writer)
		return jsonWriterToValue(id)
	})

	// JsonWriter >> write: anObject -> self
	jsonWriterClass.AddMethod1(vm.Selectors, "primWrite:", func(vmPtr interface{}, recv Value, obj Value) Value {
		v := vmPtr.(*VM)
		if !isJsonWriterValue(recv) {
			return recv
		}
		writer := v.registry.GetJsonWriter(jsonWriterIDFromValue(recv))
		if writer == nil {
			return recv
		}
		goVal := v.valueToGoJSON(obj)
		if err := writer.enc.Encode(goVal); err != nil {
			return v.signalException(jsonParseErrorClass,
				v.registry.NewStringValue(fmt.Sprintf("JsonWriter write: error: %v", err)))
		}
		return recv
	})

	// JsonWriter >> contents -> String
	jsonWriterClass.AddMethod0(vm.Selectors, "primContents", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if !isJsonWriterValue(recv) {
			return v.registry.NewStringValue("")
		}
		writer := v.registry.GetJsonWriter(jsonWriterIDFromValue(recv))
		if writer == nil {
			return v.registry.NewStringValue("")
		}
		// Trim trailing newline that json.Encoder adds
		s := writer.buf.String()
		s = strings.TrimRight(s, "\n")
		return v.registry.NewStringValue(s)
	})

	// JsonWriter >> reset -> self
	jsonWriterClass.AddMethod0(vm.Selectors, "primReset", func(vmPtr interface{}, recv Value) Value {
		if !isJsonWriterValue(recv) {
			return recv
		}
		v := vmPtr.(*VM)
		writer := v.registry.GetJsonWriter(jsonWriterIDFromValue(recv))
		if writer == nil {
			return recv
		}
		writer.buf.Reset()
		return recv
	})
}

// ---------------------------------------------------------------------------
// Value <-> Go conversion helpers
// ---------------------------------------------------------------------------

// valueToGoJSON converts a Maggie Value to a Go interface{} suitable for json.Marshal.
func (vm *VM) valueToGoJSON(v Value) interface{} {
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
	if IsDictionaryValue(v) {
		dict := vm.registry.GetDictionaryObject(v)
		if dict == nil {
			return nil
		}
		m := make(map[string]interface{}, len(dict.Keys))
		for h, key := range dict.Keys {
			var keyStr string
			if IsStringValue(key) {
				keyStr = vm.registry.GetStringContent(key)
			} else if key.IsSmallInt() {
				keyStr = fmt.Sprintf("%d", key.SmallInt())
			} else {
				keyStr = fmt.Sprintf("%v", key)
			}
			m[keyStr] = vm.valueToGoJSON(dict.Data[h])
		}
		return m
	}
	if v.IsObject() {
		obj := ObjectFromValue(v)
		if obj == nil {
			return nil
		}
		n := obj.NumSlots()
		arr := make([]interface{}, n)
		for i := 0; i < n; i++ {
			arr[i] = vm.valueToGoJSON(obj.GetSlot(i))
		}
		return arr
	}
	// Symbols and other types: convert to string
	if v.IsSymbol() {
		return vm.Symbols.Name(v.SymbolID())
	}
	return nil
}

// goJSONToValue converts a Go interface{} (from json.Decode with UseNumber) to a Maggie Value.
func (vm *VM) goJSONToValue(v interface{}) Value {
	if v == nil {
		return Nil
	}
	switch val := v.(type) {
	case bool:
		return FromBool(val)
	case json.Number:
		// Try integer first
		if i, err := val.Int64(); err == nil {
			return FromSmallInt(i)
		}
		// Fall back to float
		if f, err := val.Float64(); err == nil {
			if !math.IsInf(f, 0) && !math.IsNaN(f) {
				return FromFloat64(f)
			}
		}
		// Shouldn't happen with valid JSON
		return Nil
	case float64:
		if val == math.Trunc(val) && !math.IsInf(val, 0) && val >= math.MinInt64 && val <= math.MaxInt64 {
			return FromSmallInt(int64(val))
		}
		return FromFloat64(val)
	case string:
		return vm.registry.NewStringValue(val)
	case []interface{}:
		elems := make([]Value, len(val))
		for i, el := range val {
			elems[i] = vm.goJSONToValue(el)
		}
		return vm.NewArrayWithElements(elems)
	case map[string]interface{}:
		dict := vm.registry.NewDictionaryValue()
		d := vm.registry.GetDictionaryObject(dict)
		for k, vv := range val {
			key := vm.registry.NewStringValue(k)
			value := vm.goJSONToValue(vv)
			h := hashValue(vm.registry, key)
			d.Data[h] = value
			d.Keys[h] = key
		}
		return dict
	}
	return Nil
}
