package vm

import (
	"os"
	"path/filepath"
	"testing"
)

// tomlClass returns the Toml class value from the VM globals.
func tomlClass(vm *VM) Value {
	return vm.Globals["Toml"]
}

// ---------------------------------------------------------------------------
// decode: basic types
// ---------------------------------------------------------------------------

func TestTomlDecodeString(t *testing.T) {
	vm := NewVM()
	tc := tomlClass(vm)

	result := vm.Send(tc, "decode:", []Value{
		vm.registry.NewStringValue(`name = "Alice"`),
	})
	if !IsDictionaryValue(result) {
		t.Fatalf("decode: did not return a Dictionary")
	}
	val := vm.DictionaryAt(result, vm.registry.NewStringValue("name"))
	if !IsStringValue(val) {
		t.Fatalf("expected string value, got %v", val)
	}
	if vm.registry.GetStringContent(val) != "Alice" {
		t.Errorf("name = %q, want %q", vm.registry.GetStringContent(val), "Alice")
	}
}

func TestTomlDecodeInteger(t *testing.T) {
	vm := NewVM()
	tc := tomlClass(vm)

	result := vm.Send(tc, "decode:", []Value{
		vm.registry.NewStringValue("port = 8080"),
	})
	if !IsDictionaryValue(result) {
		t.Fatalf("decode: did not return a Dictionary")
	}
	val := vm.DictionaryAt(result, vm.registry.NewStringValue("port"))
	if !val.IsSmallInt() {
		t.Fatalf("expected SmallInt, got %v", val)
	}
	if val.SmallInt() != 8080 {
		t.Errorf("port = %d, want 8080", val.SmallInt())
	}
}

func TestTomlDecodeFloat(t *testing.T) {
	vm := NewVM()
	tc := tomlClass(vm)

	result := vm.Send(tc, "decode:", []Value{
		vm.registry.NewStringValue("pi = 3.14"),
	})
	if !IsDictionaryValue(result) {
		t.Fatalf("decode: did not return a Dictionary")
	}
	val := vm.DictionaryAt(result, vm.registry.NewStringValue("pi"))
	if !val.IsFloat() {
		t.Fatalf("expected Float, got %v", val)
	}
	if val.Float64() != 3.14 {
		t.Errorf("pi = %f, want 3.14", val.Float64())
	}
}

func TestTomlDecodeBoolean(t *testing.T) {
	vm := NewVM()
	tc := tomlClass(vm)

	result := vm.Send(tc, "decode:", []Value{
		vm.registry.NewStringValue("enabled = true\ndisabled = false"),
	})
	if !IsDictionaryValue(result) {
		t.Fatalf("decode: did not return a Dictionary")
	}

	enabled := vm.DictionaryAt(result, vm.registry.NewStringValue("enabled"))
	if enabled != True {
		t.Errorf("enabled should be True")
	}

	disabled := vm.DictionaryAt(result, vm.registry.NewStringValue("disabled"))
	if disabled != False {
		t.Errorf("disabled should be False")
	}
}

func TestTomlDecodeDatetime(t *testing.T) {
	vm := NewVM()
	tc := tomlClass(vm)

	result := vm.Send(tc, "decode:", []Value{
		vm.registry.NewStringValue(`created = 2024-01-15T10:30:00Z`),
	})
	if !IsDictionaryValue(result) {
		t.Fatalf("decode: did not return a Dictionary")
	}
	val := vm.DictionaryAt(result, vm.registry.NewStringValue("created"))
	if !IsStringValue(val) {
		t.Fatalf("expected String (ISO 8601) for datetime, got %v", val)
	}
	s := vm.registry.GetStringContent(val)
	if s != "2024-01-15T10:30:00Z" {
		t.Errorf("created = %q, want %q", s, "2024-01-15T10:30:00Z")
	}
}

// ---------------------------------------------------------------------------
// decode: nested tables
// ---------------------------------------------------------------------------

func TestTomlDecodeNestedTable(t *testing.T) {
	vm := NewVM()
	tc := tomlClass(vm)

	tomlStr := `
[server]
host = "localhost"
port = 8080

[server.tls]
enabled = true
`
	result := vm.Send(tc, "decode:", []Value{
		vm.registry.NewStringValue(tomlStr),
	})
	if !IsDictionaryValue(result) {
		t.Fatalf("decode: did not return a Dictionary")
	}

	server := vm.DictionaryAt(result, vm.registry.NewStringValue("server"))
	if !IsDictionaryValue(server) {
		t.Fatalf("server should be a Dictionary")
	}

	host := vm.DictionaryAt(server, vm.registry.NewStringValue("host"))
	if vm.registry.GetStringContent(host) != "localhost" {
		t.Errorf("host = %q, want %q", vm.registry.GetStringContent(host), "localhost")
	}

	port := vm.DictionaryAt(server, vm.registry.NewStringValue("port"))
	if port.SmallInt() != 8080 {
		t.Errorf("port = %d, want 8080", port.SmallInt())
	}

	tls := vm.DictionaryAt(server, vm.registry.NewStringValue("tls"))
	if !IsDictionaryValue(tls) {
		t.Fatalf("tls should be a Dictionary")
	}

	enabled := vm.DictionaryAt(tls, vm.registry.NewStringValue("enabled"))
	if enabled != True {
		t.Errorf("tls.enabled should be True")
	}
}

// ---------------------------------------------------------------------------
// decode: arrays and arrays of tables
// ---------------------------------------------------------------------------

func TestTomlDecodeArray(t *testing.T) {
	vm := NewVM()
	tc := tomlClass(vm)

	result := vm.Send(tc, "decode:", []Value{
		vm.registry.NewStringValue(`tags = ["web", "api", "go"]`),
	})
	if !IsDictionaryValue(result) {
		t.Fatalf("decode: did not return a Dictionary")
	}

	tags := vm.DictionaryAt(result, vm.registry.NewStringValue("tags"))
	obj := ObjectFromValue(tags)
	if obj == nil {
		t.Fatalf("tags should be an Array object")
	}
	if obj.NumSlots() != 3 {
		t.Fatalf("tags length = %d, want 3", obj.NumSlots())
	}
	if vm.registry.GetStringContent(obj.GetSlot(0)) != "web" {
		t.Errorf("tags[0] = %q, want %q", vm.registry.GetStringContent(obj.GetSlot(0)), "web")
	}
}

func TestTomlDecodeArrayOfTables(t *testing.T) {
	vm := NewVM()
	tc := tomlClass(vm)

	tomlStr := `
[[products]]
name = "Hammer"
price = 10

[[products]]
name = "Nail"
price = 1
`
	result := vm.Send(tc, "decode:", []Value{
		vm.registry.NewStringValue(tomlStr),
	})
	if !IsDictionaryValue(result) {
		t.Fatalf("decode: did not return a Dictionary")
	}

	products := vm.DictionaryAt(result, vm.registry.NewStringValue("products"))
	obj := ObjectFromValue(products)
	if obj == nil {
		t.Fatalf("products should be an Array")
	}
	if obj.NumSlots() != 2 {
		t.Fatalf("products length = %d, want 2", obj.NumSlots())
	}

	first := obj.GetSlot(0)
	if !IsDictionaryValue(first) {
		t.Fatalf("products[0] should be a Dictionary")
	}
	name := vm.DictionaryAt(first, vm.registry.NewStringValue("name"))
	if vm.registry.GetStringContent(name) != "Hammer" {
		t.Errorf("products[0].name = %q, want %q", vm.registry.GetStringContent(name), "Hammer")
	}
}

// ---------------------------------------------------------------------------
// decode: all TOML types in one document
// ---------------------------------------------------------------------------

func TestTomlDecodeAllTypes(t *testing.T) {
	vm := NewVM()
	tc := tomlClass(vm)

	tomlStr := `
title = "TOML Test"
count = 42
ratio = 0.75
active = true
tags = ["a", "b"]
created = 2024-01-15T10:30:00Z

[metadata]
version = "1.0"
`
	result := vm.Send(tc, "decode:", []Value{
		vm.registry.NewStringValue(tomlStr),
	})
	if !IsDictionaryValue(result) {
		t.Fatalf("decode: did not return a Dictionary")
	}

	// String
	title := vm.DictionaryAt(result, vm.registry.NewStringValue("title"))
	if vm.registry.GetStringContent(title) != "TOML Test" {
		t.Errorf("title wrong")
	}

	// Integer
	count := vm.DictionaryAt(result, vm.registry.NewStringValue("count"))
	if count.SmallInt() != 42 {
		t.Errorf("count wrong")
	}

	// Float
	ratio := vm.DictionaryAt(result, vm.registry.NewStringValue("ratio"))
	if ratio.Float64() != 0.75 {
		t.Errorf("ratio wrong")
	}

	// Boolean
	active := vm.DictionaryAt(result, vm.registry.NewStringValue("active"))
	if active != True {
		t.Errorf("active wrong")
	}

	// Array
	tags := vm.DictionaryAt(result, vm.registry.NewStringValue("tags"))
	if ObjectFromValue(tags) == nil {
		t.Errorf("tags should be an array")
	}

	// Datetime
	created := vm.DictionaryAt(result, vm.registry.NewStringValue("created"))
	if !IsStringValue(created) {
		t.Errorf("created should be a string")
	}

	// Nested table
	metadata := vm.DictionaryAt(result, vm.registry.NewStringValue("metadata"))
	if !IsDictionaryValue(metadata) {
		t.Errorf("metadata should be a dictionary")
	}
}

// ---------------------------------------------------------------------------
// decode: malformed input
// ---------------------------------------------------------------------------

func TestTomlDecodeMalformed(t *testing.T) {
	vm := NewVM()
	tc := tomlClass(vm)

	var caught *SignaledException
	func() {
		defer func() {
			if r := recover(); r != nil {
				if sigEx, ok := r.(SignaledException); ok {
					caught = &sigEx
				} else {
					t.Errorf("expected SignaledException, got %T: %v", r, r)
				}
			}
		}()
		vm.Send(tc, "decode:", []Value{
			vm.registry.NewStringValue("this is not valid [[[toml"),
		})
	}()

	if caught == nil {
		t.Fatal("expected TomlParseError to be raised")
	}
	if caught.Object == nil {
		t.Fatal("SignaledException.Object is nil")
	}

	// Verify the exception class name
	tomlParseErrorClass := vm.Globals["TomlParseError"]
	exClass := vm.classFromValue(tomlParseErrorClass)
	if caught.Object.ExceptionClass != exClass {
		t.Errorf("exception class = %v, want TomlParseError", caught.Object.ExceptionClass.Name)
	}

	// Verify error message contains parse error info
	if caught.Object.MessageText == Nil {
		t.Error("error message should not be nil")
	}
	if IsStringValue(caught.Object.MessageText) {
		msg := vm.registry.GetStringContent(caught.Object.MessageText)
		if msg == "" {
			t.Error("error message should not be empty")
		}
	}
}

// ---------------------------------------------------------------------------
// decodeFile:
// ---------------------------------------------------------------------------

func TestTomlDecodeFile(t *testing.T) {
	vm := NewVM()
	tc := tomlClass(vm)
	tmpDir := t.TempDir()

	tomlContent := `
[project]
name = "test-app"
version = "1.0.0"
`
	testFile := filepath.Join(tmpDir, "test.toml")
	if err := os.WriteFile(testFile, []byte(tomlContent), 0644); err != nil {
		t.Fatal(err)
	}

	result := vm.Send(tc, "decodeFile:", []Value{
		vm.registry.NewStringValue(testFile),
	})
	if !IsDictionaryValue(result) {
		t.Fatalf("decodeFile: did not return a Dictionary")
	}

	project := vm.DictionaryAt(result, vm.registry.NewStringValue("project"))
	if !IsDictionaryValue(project) {
		t.Fatalf("project should be a Dictionary")
	}
	name := vm.DictionaryAt(project, vm.registry.NewStringValue("name"))
	if vm.registry.GetStringContent(name) != "test-app" {
		t.Errorf("name = %q, want %q", vm.registry.GetStringContent(name), "test-app")
	}
}

func TestTomlDecodeFileNotFound(t *testing.T) {
	vm := NewVM()
	tc := tomlClass(vm)

	var caught *SignaledException
	func() {
		defer func() {
			if r := recover(); r != nil {
				if sigEx, ok := r.(SignaledException); ok {
					caught = &sigEx
				}
			}
		}()
		vm.Send(tc, "decodeFile:", []Value{
			vm.registry.NewStringValue("/nonexistent/path/file.toml"),
		})
	}()

	if caught == nil {
		t.Fatal("expected TomlParseError for nonexistent file")
	}
}

// ---------------------------------------------------------------------------
// encode:
// ---------------------------------------------------------------------------

func TestTomlEncode(t *testing.T) {
	vm := NewVM()
	tc := tomlClass(vm)

	dict := vm.NewDictionary()
	vm.DictionaryAtPut(dict, vm.registry.NewStringValue("name"), vm.registry.NewStringValue("test"))
	vm.DictionaryAtPut(dict, vm.registry.NewStringValue("port"), FromSmallInt(8080))
	vm.DictionaryAtPut(dict, vm.registry.NewStringValue("debug"), True)

	result := vm.Send(tc, "encode:", []Value{dict})
	if !IsStringValue(result) {
		t.Fatalf("encode: did not return a String")
	}

	encoded := vm.registry.GetStringContent(result)
	if encoded == "" {
		t.Error("encode: returned empty string")
	}

	// Verify round-trip by decoding the result
	decoded := vm.Send(tc, "decode:", []Value{result})
	if !IsDictionaryValue(decoded) {
		t.Fatalf("round-trip decode failed")
	}

	nameVal := vm.DictionaryAt(decoded, vm.registry.NewStringValue("name"))
	if vm.registry.GetStringContent(nameVal) != "test" {
		t.Errorf("round-trip: name = %q, want %q", vm.registry.GetStringContent(nameVal), "test")
	}

	portVal := vm.DictionaryAt(decoded, vm.registry.NewStringValue("port"))
	if portVal.SmallInt() != 8080 {
		t.Errorf("round-trip: port = %d, want 8080", portVal.SmallInt())
	}

	debugVal := vm.DictionaryAt(decoded, vm.registry.NewStringValue("debug"))
	if debugVal != True {
		t.Errorf("round-trip: debug should be true")
	}
}

func TestTomlEncodeNestedDict(t *testing.T) {
	vm := NewVM()
	tc := tomlClass(vm)

	inner := vm.NewDictionary()
	vm.DictionaryAtPut(inner, vm.registry.NewStringValue("host"), vm.registry.NewStringValue("localhost"))
	vm.DictionaryAtPut(inner, vm.registry.NewStringValue("port"), FromSmallInt(3000))

	outer := vm.NewDictionary()
	vm.DictionaryAtPut(outer, vm.registry.NewStringValue("server"), inner)

	result := vm.Send(tc, "encode:", []Value{outer})
	if !IsStringValue(result) {
		t.Fatalf("encode: did not return a String")
	}

	// Round-trip
	decoded := vm.Send(tc, "decode:", []Value{result})
	server := vm.DictionaryAt(decoded, vm.registry.NewStringValue("server"))
	if !IsDictionaryValue(server) {
		t.Fatalf("round-trip: server should be a Dictionary")
	}
	host := vm.DictionaryAt(server, vm.registry.NewStringValue("host"))
	if vm.registry.GetStringContent(host) != "localhost" {
		t.Errorf("round-trip: host = %q, want %q", vm.registry.GetStringContent(host), "localhost")
	}
}

func TestTomlEncodeNonDict(t *testing.T) {
	vm := NewVM()
	tc := tomlClass(vm)

	var caught *SignaledException
	func() {
		defer func() {
			if r := recover(); r != nil {
				if sigEx, ok := r.(SignaledException); ok {
					caught = &sigEx
				}
			}
		}()
		vm.Send(tc, "encode:", []Value{vm.registry.NewStringValue("not a dict")})
	}()

	if caught == nil {
		t.Fatal("expected error when encoding non-Dictionary")
	}
}

// ---------------------------------------------------------------------------
// Round-trip: encode then decode
// ---------------------------------------------------------------------------

func TestTomlRoundTrip(t *testing.T) {
	vm := NewVM()
	tc := tomlClass(vm)

	// Parse a complex document
	tomlStr := `
title = "Round Trip"
version = 2
pi = 3.14159
active = true
tags = ["go", "toml"]

[database]
host = "db.example.com"
port = 5432
`
	original := vm.Send(tc, "decode:", []Value{
		vm.registry.NewStringValue(tomlStr),
	})
	if !IsDictionaryValue(original) {
		t.Fatalf("decode failed")
	}

	// Encode back
	encoded := vm.Send(tc, "encode:", []Value{original})
	if !IsStringValue(encoded) {
		t.Fatalf("encode failed")
	}

	// Decode again
	roundTripped := vm.Send(tc, "decode:", []Value{encoded})
	if !IsDictionaryValue(roundTripped) {
		t.Fatalf("round-trip decode failed")
	}

	// Verify key values survived
	title := vm.DictionaryAt(roundTripped, vm.registry.NewStringValue("title"))
	if vm.registry.GetStringContent(title) != "Round Trip" {
		t.Errorf("title mismatch")
	}

	version := vm.DictionaryAt(roundTripped, vm.registry.NewStringValue("version"))
	if version.SmallInt() != 2 {
		t.Errorf("version mismatch")
	}

	database := vm.DictionaryAt(roundTripped, vm.registry.NewStringValue("database"))
	if !IsDictionaryValue(database) {
		t.Fatalf("database should be a Dictionary")
	}
	dbHost := vm.DictionaryAt(database, vm.registry.NewStringValue("host"))
	if vm.registry.GetStringContent(dbHost) != "db.example.com" {
		t.Errorf("database.host mismatch")
	}
}

// ---------------------------------------------------------------------------
// Empty document
// ---------------------------------------------------------------------------

func TestTomlDecodeEmpty(t *testing.T) {
	vm := NewVM()
	tc := tomlClass(vm)

	result := vm.Send(tc, "decode:", []Value{
		vm.registry.NewStringValue(""),
	})
	if !IsDictionaryValue(result) {
		t.Fatalf("decode of empty string should return empty Dictionary")
	}
}
