package vm

import (
	"testing"
)

// ---------------------------------------------------------------------------
// ImageEncoder tests
// ---------------------------------------------------------------------------

func TestImageEncoderNewEmpty(t *testing.T) {
	enc := NewImageEncoder()
	if enc.ObjectCount() != 0 {
		t.Errorf("ObjectCount() = %d, want 0", enc.ObjectCount())
	}
	if enc.SymbolCount() != 0 {
		t.Errorf("SymbolCount() = %d, want 0", enc.SymbolCount())
	}
	if enc.StringCount() != 0 {
		t.Errorf("StringCount() = %d, want 0", enc.StringCount())
	}
	if enc.ClassCount() != 0 {
		t.Errorf("ClassCount() = %d, want 0", enc.ClassCount())
	}
	if enc.MethodCount() != 0 {
		t.Errorf("MethodCount() = %d, want 0", enc.MethodCount())
	}
}

func TestImageEncoderRegisterObject(t *testing.T) {
	enc := NewImageEncoder()

	// Register first object
	idx1 := enc.RegisterObject(0x1000)
	if idx1 != 0 {
		t.Errorf("First object index = %d, want 0", idx1)
	}

	// Register second object
	idx2 := enc.RegisterObject(0x2000)
	if idx2 != 1 {
		t.Errorf("Second object index = %d, want 1", idx2)
	}

	// Re-register first object should return same index
	idx1Again := enc.RegisterObject(0x1000)
	if idx1Again != 0 {
		t.Errorf("Re-registered first object index = %d, want 0", idx1Again)
	}

	// Lookup
	if idx, ok := enc.LookupObject(0x1000); !ok || idx != 0 {
		t.Errorf("LookupObject(0x1000) = %d, %v; want 0, true", idx, ok)
	}
	if _, ok := enc.LookupObject(0x9999); ok {
		t.Error("LookupObject(0x9999) should return false")
	}

	if enc.ObjectCount() != 2 {
		t.Errorf("ObjectCount() = %d, want 2", enc.ObjectCount())
	}
}

func TestImageEncoderRegisterSymbol(t *testing.T) {
	enc := NewImageEncoder()

	idx1 := enc.RegisterSymbol(100)
	idx2 := enc.RegisterSymbol(200)
	idx1Again := enc.RegisterSymbol(100)

	if idx1 != 0 || idx2 != 1 || idx1Again != 0 {
		t.Errorf("Symbol indices: got %d, %d, %d; want 0, 1, 0", idx1, idx2, idx1Again)
	}

	if idx, ok := enc.LookupSymbol(100); !ok || idx != 0 {
		t.Errorf("LookupSymbol(100) = %d, %v; want 0, true", idx, ok)
	}

	if enc.SymbolCount() != 2 {
		t.Errorf("SymbolCount() = %d, want 2", enc.SymbolCount())
	}
}

func TestImageEncoderRegisterString(t *testing.T) {
	enc := NewImageEncoder()

	idx1 := enc.RegisterString("hello")
	idx2 := enc.RegisterString("world")
	idx1Again := enc.RegisterString("hello")

	if idx1 != 0 || idx2 != 1 || idx1Again != 0 {
		t.Errorf("String indices: got %d, %d, %d; want 0, 1, 0", idx1, idx2, idx1Again)
	}

	if enc.StringCount() != 2 {
		t.Errorf("StringCount() = %d, want 2", enc.StringCount())
	}
}

func TestImageEncoderRegisterClass(t *testing.T) {
	enc := NewImageEncoder()

	c1 := NewClass("Foo", nil)
	c2 := NewClass("Bar", nil)

	idx1 := enc.RegisterClass(c1)
	idx2 := enc.RegisterClass(c2)
	idx1Again := enc.RegisterClass(c1)

	if idx1 != 0 || idx2 != 1 || idx1Again != 0 {
		t.Errorf("Class indices: got %d, %d, %d; want 0, 1, 0", idx1, idx2, idx1Again)
	}

	if enc.ClassCount() != 2 {
		t.Errorf("ClassCount() = %d, want 2", enc.ClassCount())
	}
}

func TestImageEncoderRegisterMethod(t *testing.T) {
	enc := NewImageEncoder()

	m1 := &CompiledMethod{name: "foo"}
	m2 := &CompiledMethod{name: "bar"}

	idx1 := enc.RegisterMethod(m1)
	idx2 := enc.RegisterMethod(m2)
	idx1Again := enc.RegisterMethod(m1)

	if idx1 != 0 || idx2 != 1 || idx1Again != 0 {
		t.Errorf("Method indices: got %d, %d, %d; want 0, 1, 0", idx1, idx2, idx1Again)
	}

	if enc.MethodCount() != 2 {
		t.Errorf("MethodCount() = %d, want 2", enc.MethodCount())
	}
}

// ---------------------------------------------------------------------------
// ImageDecoder tests
// ---------------------------------------------------------------------------

func TestImageDecoderNewEmpty(t *testing.T) {
	dec := NewImageDecoder()
	if dec.ObjectCount() != 0 || dec.SymbolCount() != 0 || dec.StringCount() != 0 || dec.ClassCount() != 0 || dec.MethodCount() != 0 {
		t.Error("New decoder should have all zero counts")
	}
}

func TestImageDecoderAddAndGet(t *testing.T) {
	dec := NewImageDecoder()

	obj := &Object{}
	idx := dec.AddObject(obj)
	if idx != 0 {
		t.Errorf("AddObject index = %d, want 0", idx)
	}

	got := dec.GetObject(0)
	if got != obj {
		t.Error("GetObject(0) returned wrong object")
	}

	if dec.GetObject(999) != nil {
		t.Error("GetObject(999) should return nil")
	}

	if dec.ObjectCount() != 1 {
		t.Errorf("ObjectCount() = %d, want 1", dec.ObjectCount())
	}
}

func TestImageDecoderSetObject(t *testing.T) {
	dec := NewImageDecoder()

	obj := &Object{}
	dec.SetObject(5, obj)

	if dec.ObjectCount() != 6 {
		t.Errorf("ObjectCount() = %d, want 6 (grown to index 5)", dec.ObjectCount())
	}

	got := dec.GetObject(5)
	if got != obj {
		t.Error("GetObject(5) returned wrong object")
	}

	// Indices 0-4 should be nil
	for i := uint32(0); i < 5; i++ {
		if dec.GetObject(i) != nil {
			t.Errorf("GetObject(%d) should be nil", i)
		}
	}
}

func TestImageDecoderSymbols(t *testing.T) {
	dec := NewImageDecoder()

	dec.AddSymbol(42)
	dec.SetSymbol(3, 99)

	if dec.GetSymbol(0) != 42 {
		t.Errorf("GetSymbol(0) = %d, want 42", dec.GetSymbol(0))
	}
	if dec.GetSymbol(3) != 99 {
		t.Errorf("GetSymbol(3) = %d, want 99", dec.GetSymbol(3))
	}
	if dec.GetSymbol(999) != 0 {
		t.Errorf("GetSymbol(999) = %d, want 0", dec.GetSymbol(999))
	}
}

func TestImageDecoderStrings(t *testing.T) {
	dec := NewImageDecoder()

	dec.AddString("hello")
	dec.SetString(3, "world")

	if dec.GetString(0) != "hello" {
		t.Errorf("GetString(0) = %q, want %q", dec.GetString(0), "hello")
	}
	if dec.GetString(3) != "world" {
		t.Errorf("GetString(3) = %q, want %q", dec.GetString(3), "world")
	}
	if dec.GetString(999) != "" {
		t.Errorf("GetString(999) = %q, want empty", dec.GetString(999))
	}
}

func TestImageDecoderClasses(t *testing.T) {
	dec := NewImageDecoder()

	c := NewClass("Test", nil)
	dec.AddClass(c)
	dec.SetClass(3, NewClass("Other", nil))

	if dec.GetClass(0) != c {
		t.Error("GetClass(0) returned wrong class")
	}
	if dec.GetClass(3) == nil {
		t.Error("GetClass(3) should not be nil")
	}
	if dec.GetClass(999) != nil {
		t.Error("GetClass(999) should be nil")
	}
}

func TestImageDecoderMethods(t *testing.T) {
	dec := NewImageDecoder()

	m := &CompiledMethod{name: "test"}
	dec.AddMethod(m)
	dec.SetMethod(3, &CompiledMethod{name: "other"})

	if dec.GetMethod(0) != m {
		t.Error("GetMethod(0) returned wrong method")
	}
	if dec.GetMethod(3) == nil {
		t.Error("GetMethod(3) should not be nil")
	}
	if dec.GetMethod(999) != nil {
		t.Error("GetMethod(999) should be nil")
	}
}
