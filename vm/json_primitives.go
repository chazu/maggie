package vm

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"math"
	"math/big"
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

func makeJsonReaderValue(r *JsonReaderObject) Value {
	return makeExtensionValue(jsonReaderMarker, r)
}

func isJsonReaderValue(v Value) bool {
	return isExtensionValue(v, jsonReaderMarker)
}

func getJsonReader(v Value) *JsonReaderObject {
	if o := ExtensionObject(v, jsonReaderMarker); o != nil {
		return o.(*JsonReaderObject)
	}
	return nil
}

// ---------------------------------------------------------------------------
// JsonWriter Registry helpers
// ---------------------------------------------------------------------------

func makeJsonWriterValue(w *JsonWriterObject) Value {
	return makeExtensionValue(jsonWriterMarker, w)
}

func isJsonWriterValue(v Value) bool {
	return isExtensionValue(v, jsonWriterMarker)
}

func getJsonWriter(v Value) *JsonWriterObject {
	if o := ExtensionObject(v, jsonWriterMarker); o != nil {
		return o.(*JsonWriterObject)
	}
	return nil
}

// ---------------------------------------------------------------------------
// Registration
// ---------------------------------------------------------------------------

func (vm *VM) registerJSONPrimitives() {
	jsonClass := vm.createClass("Json", vm.ObjectClass)
	vm.globals["Json"] = vm.classValue(jsonClass)

	jsonParseErrorClass := vm.createClass("JsonParseError", vm.ErrorClass)
	vm.globals["JsonParseError"] = vm.classValue(jsonParseErrorClass)

	jsonReaderClass := vm.createClass("JsonReader", vm.ObjectClass)
	vm.globals["JsonReader"] = vm.classValue(jsonReaderClass)

	jsonWriterClass := vm.createClass("JsonWriter", vm.ObjectClass)
	vm.globals["JsonWriter"] = vm.classValue(jsonWriterClass)

	vm.symbolDispatch.Register(jsonReaderMarker, &SymbolTypeEntry{Class: jsonReaderClass})
	vm.symbolDispatch.Register(jsonWriterMarker, &SymbolTypeEntry{Class: jsonWriterClass})

	// -------------------------------------------------------------------
	// Json class methods (stateless encode/decode)
	// -------------------------------------------------------------------

	// Json encode: anObject -> String
	jsonClass.AddClassMethod1(vm.Selectors, "primEncode:", func(v *VM, recv Value, obj Value) Value {
		goVal, err := v.valueToGoJSON(obj, 0)
		if err != nil {
			return v.signalException(jsonParseErrorClass,
				v.registry.NewStringValue(fmt.Sprintf("Json encode: %v", err)))
		}
		data, err := json.Marshal(goVal)
		if err != nil {
			return v.signalException(jsonParseErrorClass,
				v.registry.NewStringValue(fmt.Sprintf("Json encode: error: %v", err)))
		}
		return v.registry.NewStringValue(string(data))
	})

	// Json encodePretty: anObject -> String
	jsonClass.AddClassMethod1(vm.Selectors, "primEncodePretty:", func(v *VM, recv Value, obj Value) Value {
		goVal, err := v.valueToGoJSON(obj, 0)
		if err != nil {
			return v.signalException(jsonParseErrorClass,
				v.registry.NewStringValue(fmt.Sprintf("Json encodePretty: %v", err)))
		}
		data, err := json.MarshalIndent(goVal, "", "  ")
		if err != nil {
			return v.signalException(jsonParseErrorClass,
				v.registry.NewStringValue(fmt.Sprintf("Json encodePretty: error: %v", err)))
		}
		return v.registry.NewStringValue(string(data))
	})

	// Json decode: aString -> Object
	jsonClass.AddClassMethod1(vm.Selectors, "primDecode:", func(v *VM, recv Value, strVal Value) Value {
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
		// The whole string must be ONE JSON value: `{"a":1} garbage` or `1 2`
		// is invalid, not the first value with the rest silently dropped
		// (JsonReader is the API for a stream of values).
		if _, err := dec.Token(); err != io.EOF {
			return v.signalException(jsonParseErrorClass,
				v.registry.NewStringValue("Json decode: invalid JSON: unexpected data after the top-level value"))
		}
		return v.goJSONToValue(goResult)
	})

	// -------------------------------------------------------------------
	// JsonReader (streaming decode)
	// -------------------------------------------------------------------

	// JsonReader new: aString -> JsonReader
	jsonReaderClass.AddClassMethod1(vm.Selectors, "primNew:", func(v *VM, recv Value, strVal Value) Value {
		if !IsStringValue(strVal) {
			return v.signalException(jsonParseErrorClass,
				v.registry.NewStringValue("JsonReader new: argument must be a String"))
		}
		content := v.registry.GetStringContent(strVal)
		dec := json.NewDecoder(strings.NewReader(content))
		dec.UseNumber()
		reader := &JsonReaderObject{decoder: dec, source: content}
		return makeJsonReaderValue(reader)
	})

	// JsonReader >> next -> next decoded value or nil at EOF
	jsonReaderClass.AddMethod0(vm.Selectors, "primNext", func(v *VM, recv Value) Value {
		if !isJsonReaderValue(recv) {
			return Nil
		}
		reader := getJsonReader(recv)
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
	jsonReaderClass.AddMethod0(vm.Selectors, "primHasMore", func(v *VM, recv Value) Value {
		if !isJsonReaderValue(recv) {
			return False
		}
		reader := getJsonReader(recv)
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
	jsonWriterClass.AddClassMethod0(vm.Selectors, "primNew", func(v *VM, recv Value) Value {
		buf := &bytes.Buffer{}
		enc := json.NewEncoder(buf)
		enc.SetEscapeHTML(false)
		writer := &JsonWriterObject{buf: buf, enc: enc, pretty: false}
		return makeJsonWriterValue(writer)
	})

	// JsonWriter newPretty -> JsonWriter
	jsonWriterClass.AddClassMethod0(vm.Selectors, "primNewPretty", func(v *VM, recv Value) Value {
		buf := &bytes.Buffer{}
		enc := json.NewEncoder(buf)
		enc.SetEscapeHTML(false)
		enc.SetIndent("", "  ")
		writer := &JsonWriterObject{buf: buf, enc: enc, pretty: true}
		return makeJsonWriterValue(writer)
	})

	// JsonWriter >> write: anObject -> self
	jsonWriterClass.AddMethod1(vm.Selectors, "primWrite:", func(v *VM, recv Value, obj Value) Value {
		if !isJsonWriterValue(recv) {
			return recv
		}
		writer := getJsonWriter(recv)
		if writer == nil {
			return recv
		}
		goVal, err := v.valueToGoJSON(obj, 0)
		if err != nil {
			return v.signalException(jsonParseErrorClass,
				v.registry.NewStringValue(fmt.Sprintf("JsonWriter write: %v", err)))
		}
		if err := writer.enc.Encode(goVal); err != nil {
			return v.signalException(jsonParseErrorClass,
				v.registry.NewStringValue(fmt.Sprintf("JsonWriter write: error: %v", err)))
		}
		return recv
	})

	// JsonWriter >> contents -> String
	jsonWriterClass.AddMethod0(vm.Selectors, "primContents", func(v *VM, recv Value) Value {
		if !isJsonWriterValue(recv) {
			return v.registry.NewStringValue("")
		}
		writer := getJsonWriter(recv)
		if writer == nil {
			return v.registry.NewStringValue("")
		}
		// Trim trailing newline that json.Encoder adds
		s := writer.buf.String()
		s = strings.TrimRight(s, "\n")
		return v.registry.NewStringValue(s)
	})

	// JsonWriter >> reset -> self
	jsonWriterClass.AddMethod0(vm.Selectors, "primReset", func(v *VM, recv Value) Value {
		if !isJsonWriterValue(recv) {
			return recv
		}
		writer := getJsonWriter(recv)
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

// maxJSONDepth bounds valueToGoJSON's recursion, like maxSerialDepth does
// for the serializer: a self-containing collection would otherwise recurse
// until the Go stack overflows — a fatal error no Maggie handler can catch.
const maxJSONDepth = 256

// valueToGoJSON converts a Maggie Value to a Go interface{} suitable for
// json.Marshal. Values with no JSON representation (and Dictionary keys that
// are not strings, symbols or integers) are an error rather than a silent
// null or a dump of internal slots.
func (vm *VM) valueToGoJSON(v Value, depth int) (interface{}, error) {
	if depth > maxJSONDepth {
		return nil, fmt.Errorf("nesting deeper than %d (cyclic structure?)", maxJSONDepth)
	}
	switch {
	case v == Nil:
		return nil, nil
	case v == True:
		return true, nil
	case v == False:
		return false, nil
	case v.IsSmallInt():
		return v.SmallInt(), nil
	case v.IsFloat():
		return v.Float64(), nil
	case IsStringValue(v):
		return vm.registry.GetStringContent(v), nil
	case v.IsSymbol():
		return vm.Symbols.Name(v.SymbolID()), nil
	case IsBigIntValue(v):
		if bi := vm.registry.GetBigInt(v); bi != nil {
			return json.Number(bi.Value.String()), nil
		}
	case IsDictionaryValue(v):
		dict := vm.registry.GetDictionaryObject(v)
		if dict == nil {
			return nil, nil
		}
		entries := dict.Entries()
		m := make(map[string]interface{}, len(entries))
		for _, e := range entries {
			var keyStr string
			switch {
			case IsStringValue(e.Key):
				keyStr = vm.registry.GetStringContent(e.Key)
			case e.Key.IsSymbol():
				keyStr = vm.Symbols.Name(e.Key.SymbolID())
			case e.Key.IsSmallInt():
				keyStr = fmt.Sprintf("%d", e.Key.SmallInt())
			default:
				return nil, fmt.Errorf("cannot encode a %s as an object key", vm.jsonClassName(e.Key))
			}
			val, err := vm.valueToGoJSON(e.Value, depth+1)
			if err != nil {
				return nil, err
			}
			m[keyStr] = val
		}
		return m, nil
	case isArrayListValue(v):
		return vm.jsonArray(vm.registry.GetArrayList(v).Snapshot(), depth)
	case v.IsObject():
		if obj := ObjectFromValue(v); obj != nil && vm.ArrayClass != nil && obj.VTablePtr() == vm.ArrayClass.VTable {
			return vm.jsonArray(obj.AllSlots(), depth)
		}
	}
	return nil, fmt.Errorf("cannot encode a %s", vm.jsonClassName(v))
}

// jsonArray converts a sequence of elements for valueToGoJSON.
func (vm *VM) jsonArray(elems []Value, depth int) (interface{}, error) {
	arr := make([]interface{}, len(elems))
	for i, e := range elems {
		val, err := vm.valueToGoJSON(e, depth+1)
		if err != nil {
			return nil, err
		}
		arr[i] = val
	}
	return arr, nil
}

// jsonClassName names v's class for an encode error message.
func (vm *VM) jsonClassName(v Value) string {
	if cls := vm.ClassFor(v); cls != nil {
		return cls.Name
	}
	return "value"
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
			return vm.registry.NewIntegerValue(i)
		}
		// Integer literals beyond int64 (large ids) stay exact as BigInteger
		// rather than silently losing precision through float64.
		if bi, ok := new(big.Int).SetString(string(val), 10); ok {
			return vm.registry.NewBigIntValue(bi)
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
		if val == math.Trunc(val) && val >= float64(MinSmallInt) && val <= float64(MaxSmallInt) {
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
			d.Put(vm.registry, key, value)
		}
		return dict
	}
	return Nil
}
