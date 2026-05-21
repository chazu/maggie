package vm

import (
	"bytes"
	"testing"
	"unsafe"

	"github.com/fxamacker/cbor/v2"
)

// ---------------------------------------------------------------------------
// TestCborEncodeImageValue: subtests for each value type
// ---------------------------------------------------------------------------

func TestCborEncodeImageValue(t *testing.T) {
	vm := NewVM()
	enc := NewImageEncoder()
	enc.registry = vm.registry

	t.Run("nil", func(t *testing.T) {
		raw, err := encodeImageValue(enc, Nil)
		if err != nil {
			t.Fatal(err)
		}
		var decoded interface{}
		if err := cbor.Unmarshal(raw, &decoded); err != nil {
			t.Fatal(err)
		}
		if decoded != nil {
			t.Fatalf("expected nil, got %v", decoded)
		}
	})

	t.Run("true", func(t *testing.T) {
		raw, err := encodeImageValue(enc, True)
		if err != nil {
			t.Fatal(err)
		}
		var decoded bool
		if err := cbor.Unmarshal(raw, &decoded); err != nil {
			t.Fatal(err)
		}
		if !decoded {
			t.Fatal("expected true")
		}
	})

	t.Run("false", func(t *testing.T) {
		raw, err := encodeImageValue(enc, False)
		if err != nil {
			t.Fatal(err)
		}
		var decoded bool
		if err := cbor.Unmarshal(raw, &decoded); err != nil {
			t.Fatal(err)
		}
		if decoded {
			t.Fatal("expected false")
		}
	})

	t.Run("small_int", func(t *testing.T) {
		raw, err := encodeImageValue(enc, FromSmallInt(42))
		if err != nil {
			t.Fatal(err)
		}
		var decoded int64
		if err := cbor.Unmarshal(raw, &decoded); err != nil {
			t.Fatal(err)
		}
		if decoded != 42 {
			t.Fatalf("expected 42, got %d", decoded)
		}
	})

	t.Run("negative_int", func(t *testing.T) {
		raw, err := encodeImageValue(enc, FromSmallInt(-7))
		if err != nil {
			t.Fatal(err)
		}
		var decoded int64
		if err := cbor.Unmarshal(raw, &decoded); err != nil {
			t.Fatal(err)
		}
		if decoded != -7 {
			t.Fatalf("expected -7, got %d", decoded)
		}
	})

	t.Run("float", func(t *testing.T) {
		raw, err := encodeImageValue(enc, FromFloat64(3.14))
		if err != nil {
			t.Fatal(err)
		}
		var decoded float64
		if err := cbor.Unmarshal(raw, &decoded); err != nil {
			t.Fatal(err)
		}
		if decoded != 3.14 {
			t.Fatalf("expected 3.14, got %f", decoded)
		}
	})

	t.Run("symbol", func(t *testing.T) {
		symID := vm.Symbols.Intern("testSymbol")
		enc.RegisterSymbol(symID)
		val := FromSymbolID(symID)

		raw, err := encodeImageValue(enc, val)
		if err != nil {
			t.Fatal(err)
		}
		// Should decode as a CBOR tag
		var tag cbor.Tag
		if err := cbor.Unmarshal(raw, &tag); err != nil {
			t.Fatal(err)
		}
		if tag.Number != imgTagSymbolRef {
			t.Fatalf("expected tag %d, got %d", imgTagSymbolRef, tag.Number)
		}
	})

	t.Run("string", func(t *testing.T) {
		strVal := vm.registry.NewStringValue("hello world")

		raw, err := encodeImageValue(enc, strVal)
		if err != nil {
			t.Fatal(err)
		}
		var decoded string
		if err := cbor.Unmarshal(raw, &decoded); err != nil {
			t.Fatal(err)
		}
		if decoded != "hello world" {
			t.Fatalf("expected 'hello world', got %q", decoded)
		}
	})

	t.Run("character", func(t *testing.T) {
		charVal := FromCharacter('A')

		raw, err := encodeImageValue(enc, charVal)
		if err != nil {
			t.Fatal(err)
		}
		var tag cbor.Tag
		if err := cbor.Unmarshal(raw, &tag); err != nil {
			t.Fatal(err)
		}
		if tag.Number != cborTagCharacter {
			t.Fatalf("expected tag %d, got %d", cborTagCharacter, tag.Number)
		}
	})

	t.Run("object_ref", func(t *testing.T) {
		obj := NewObject(vm.ObjectClass.VTable, 0)
		ptr := uintptr(unsafe.Pointer(obj))
		enc.RegisterObject(ptr)
		val := obj.ToValue()

		raw, err := encodeImageValue(enc, val)
		if err != nil {
			t.Fatal(err)
		}
		var tag cbor.Tag
		if err := cbor.Unmarshal(raw, &tag); err != nil {
			t.Fatal(err)
		}
		if tag.Number != imgTagValueRef {
			t.Fatalf("expected tag %d, got %d", imgTagValueRef, tag.Number)
		}
	})

	t.Run("class_ref", func(t *testing.T) {
		cls := vm.ObjectClass
		enc.RegisterClass(cls)
		val := vm.ClassValue(cls)

		raw, err := encodeImageValue(enc, val)
		if err != nil {
			t.Fatal(err)
		}
		var tag cbor.Tag
		if err := cbor.Unmarshal(raw, &tag); err != nil {
			t.Fatal(err)
		}
		if tag.Number != imgTagClassRef {
			t.Fatalf("expected tag %d, got %d", imgTagClassRef, tag.Number)
		}
	})
}

// ---------------------------------------------------------------------------
// TestCborImageWriterEmpty: write an empty (fresh) VM to CBOR
// ---------------------------------------------------------------------------

func TestCborImageWriterEmpty(t *testing.T) {
	vm := NewVM()

	data, err := vm.SaveImageCborBytes()
	if err != nil {
		t.Fatalf("SaveImageCborBytes failed: %v", err)
	}

	if len(data) == 0 {
		t.Fatal("expected non-empty CBOR output")
	}

	// Verify it's valid CBOR by unmarshaling into a generic tag
	var tag cbor.Tag
	if err := cbor.Unmarshal(data, &tag); err != nil {
		t.Fatalf("output is not valid CBOR: %v", err)
	}

	if tag.Number != imgTagHeader {
		t.Fatalf("expected tag number %d, got %d", imgTagHeader, tag.Number)
	}
}

// ---------------------------------------------------------------------------
// TestCborImageWriterDeterministic: write twice, bytes must be identical
// ---------------------------------------------------------------------------

func TestCborImageWriterDeterministic(t *testing.T) {
	vm := NewVM()

	data1, err := vm.SaveImageCborBytes()
	if err != nil {
		t.Fatalf("first write failed: %v", err)
	}

	data2, err := vm.SaveImageCborBytes()
	if err != nil {
		t.Fatalf("second write failed: %v", err)
	}

	if !bytes.Equal(data1, data2) {
		t.Fatalf("CBOR output is non-deterministic: %d bytes vs %d bytes", len(data1), len(data2))
	}
}

// ---------------------------------------------------------------------------
// TestCborImageWriterBootstrapSmoke: verify size and CBOR tag prefix
// ---------------------------------------------------------------------------

func TestCborImageWriterBootstrapSmoke(t *testing.T) {
	vm := NewVM()

	data, err := vm.SaveImageCborBytes()
	if err != nil {
		t.Fatalf("SaveImageCborBytes failed: %v", err)
	}

	// CBOR tag encoding: 0xD9 = 2-byte tag, 0xDA = 4-byte tag
	// imgTagHeader = 27100 which is > 255, so it uses 0xD9 (2-byte tag)
	if len(data) < 3 {
		t.Fatalf("output too short: %d bytes", len(data))
	}
	if data[0] != 0xD9 {
		t.Fatalf("expected first byte 0xD9 (2-byte CBOR tag), got 0x%02X", data[0])
	}

	// Verify reasonable size (at least 100 bytes for a VM with core classes)
	if len(data) < 100 {
		t.Fatalf("output suspiciously small: %d bytes", len(data))
	}

	t.Logf("CBOR image size: %d bytes", len(data))
}
