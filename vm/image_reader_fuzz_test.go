package vm

import (
	"bytes"
	"testing"
)

// ---------------------------------------------------------------------------
// FuzzImageReader: ensure the image reader never panics or OOMs on
// arbitrary input. Errors are expected and acceptable; panics are bugs.
// ---------------------------------------------------------------------------

// buildMinimalValidImage creates a minimal but fully valid image by saving
// a real VM state. This gives the fuzzer a well-formed starting point to
// mutate from.
func buildMinimalValidImage(t testing.TB) []byte {
	t.Helper()

	vm := NewVM()

	// Add a class with an instance variable
	cls := NewClassWithInstVars("FuzzClass", vm.ObjectClass, []string{"x"})
	cls.VTable = NewVTable(cls, vm.ObjectClass.VTable)
	cls.ClassVTable = NewVTable(cls, vm.ObjectClass.ClassVTable)
	vm.Classes.Register(cls)

	// Add a simple method on the class
	sel := vm.Selectors.Intern("getX")
	method := &CompiledMethod{
		selector: sel,
		name:     "getX",
		Arity:    0,
		NumTemps: 0,
		Literals: []Value{FromSmallInt(42)},
		Bytecode: []byte{byte(OpPushLiteral), 0, 0, byte(OpReturnTop)},
		Blocks:   nil,
		Source:   "getX [^x]",
	}
	method.class = cls
	cls.VTable.AddMethod(sel, method)

	// Add a global
	vm.Globals["testVar"] = FromSmallInt(99)

	// Save to bytes
	var buf bytes.Buffer
	if err := vm.SaveImageTo(&buf); err != nil {
		t.Fatalf("SaveImageTo failed: %v", err)
	}
	return buf.Bytes()
}

// buildMinimalHandcraftedImage creates a small valid image using the test
// builder, matching the format from TestImageReaderReadAll.
func buildMinimalHandcraftedImage() []byte {
	b := newTestImageBuilder()
	b.writeHeader(1, 0)

	// String table: 2 entries
	b.writeUint32(2)
	b.writeString("Obj")
	b.writeString("g")

	// Symbol table: 0
	b.writeUint32(0)

	// Selector table: 0
	b.writeUint32(0)

	// Classes: 0
	b.writeUint32(0)

	// Methods: 0
	b.writeUint32(0)

	// Objects: 0
	b.writeUint32(0)

	// Globals: 1
	b.writeUint32(1)
	b.writeUint32(1) // name: "g"
	b.writeValue(FromSmallInt(1))

	// Class variables (v3): 0
	b.writeUint32(0)

	return b.bytes()
}

// buildImageWithMethod creates a valid image containing a method with
// literals, blocks, and source map entries.
func buildImageWithMethod() []byte {
	b := newTestImageBuilder()
	b.writeHeader(0, 0)

	// String table: 3 entries
	b.writeUint32(3)
	b.writeString("TestClass")
	b.writeString("doIt")
	b.writeString("doIt [^42]")

	// Symbol table: 0
	b.writeUint32(0)

	// Selector table: 1
	b.writeUint32(1)
	b.writeUint32(1) // "doIt"

	// Classes: 1
	b.writeUint32(1)
	b.writeUint32(0)          // name: "TestClass"
	b.writeUint32(0xFFFFFFFF) // namespace: none
	b.writeUint32(0xFFFFFFFF) // superclass: none
	b.writeUint32(0)          // numSlots
	b.writeUint32(0)          // instVarCount
	b.writeUint32(1)          // 1 instance method
	b.writeUint32(0)          // method index 0
	b.writeUint32(0)          // 0 class methods
	b.writeBytes([]byte{0})   // no class docstring

	// Methods: 1
	b.writeUint32(1)
	// method 0
	b.writeUint32(0)          // selector ID
	b.writeUint32(0)          // class index
	b.writeUint32(1)          // name: "doIt"
	b.writeBytes([]byte{0})   // isClassMethod: false
	b.writeUint32(0)          // arity
	b.writeUint32(0)          // numTemps
	b.writeUint32(1)          // 1 literal
	b.writeValue(FromSmallInt(42))
	b.writeUint32(4)          // bytecode length
	b.writeBytes([]byte{byte(OpPushLiteral), 0, 0, byte(OpReturnTop)})
	b.writeUint32(0)          // 0 blocks
	b.writeBytes([]byte{1})   // hasSource: true
	b.writeUint32(2)          // source: "doIt [^42]"
	b.writeBytes([]byte{0})   // no docstring (v2+)
	b.writeUint32(1)          // 1 source map entry
	b.writeUint32(0)          // offset
	b.writeUint32(1)          // line
	b.writeUint32(0)          // column

	// Objects: 0
	b.writeUint32(0)

	// Globals: 0
	b.writeUint32(0)

	// Class variables (v3): 0
	b.writeUint32(0)

	return b.bytes()
}

func FuzzImageReader(f *testing.F) {
	// Seed 1: Real image from SaveImageTo
	f.Add(buildMinimalValidImage(f))

	// Seed 2: Handcrafted minimal image
	f.Add(buildMinimalHandcraftedImage())

	// Seed 3: Image with method, literals, source map
	f.Add(buildImageWithMethod())

	// Seed 4: Just the magic bytes (valid prefix, truncated)
	f.Add(ImageMagic[:])

	// Seed 5: Valid header only (no sections)
	func() {
		b := newTestImageBuilder()
		b.writeHeader(0, 0)
		f.Add(b.bytes())
	}()

	// Seed 6: Header + empty string table only
	func() {
		b := newTestImageBuilder()
		b.writeHeader(0, 0)
		b.writeUint32(0) // 0 strings
		f.Add(b.bytes())
	}()

	// Seed 7: All sections empty
	func() {
		b := newTestImageBuilder()
		b.writeHeader(0, 0)
		b.writeUint32(0) // 0 strings
		b.writeUint32(0) // 0 symbols
		b.writeUint32(0) // 0 selectors
		b.writeUint32(0) // 0 classes
		b.writeUint32(0) // 0 methods
		b.writeUint32(0) // 0 objects
		b.writeUint32(0) // 0 globals
		b.writeUint32(0) // 0 class vars
		f.Add(b.bytes())
	}()

	// Seed 8: Empty bytes
	f.Add([]byte{})

	// Seed 9: Single zero byte
	f.Add([]byte{0})

	// Seed 10: Almost-valid magic with garbage after
	f.Add([]byte{'M', 'A', 'G', 'I', 0xFF, 0xFF, 0xFF, 0xFF})

	// Seed 11: Valid header with huge counts to test allocation guards
	func() {
		b := newTestImageBuilder()
		b.writeHeader(0xFFFFFFFF, 0)
		b.writeUint32(0xFFFFFFFF) // huge string count
		f.Add(b.bytes())
	}()

	// Seed 12: Valid header with v1 version (backward compat path)
	func() {
		b := &testImageBuilder{}
		b.buf.Write(ImageMagic[:])
		b.writeUint32(1) // v1
		b.writeUint32(ImageFlagNone)
		b.writeUint32(0)
		b.writeUint64(0)
		b.writeUint64(0)
		b.writeUint32(0)
		b.writeUint32(0) // strings
		b.writeUint32(0) // symbols
		b.writeUint32(0) // selectors
		b.writeUint32(0) // classes
		b.writeUint32(0) // methods
		b.writeUint32(0) // objects
		b.writeUint32(0) // globals
		// no class vars section in v1
		f.Add(b.bytes())
	}()

	f.Fuzz(func(t *testing.T, data []byte) {
		defer func() {
			if r := recover(); r != nil {
				t.Fatalf("image reader panicked on %d bytes of input: %v", len(data), r)
			}
		}()

		ir, err := NewImageReaderFromBytes(data)
		if err != nil {
			return // corrupt header is fine
		}

		vm := NewVM()
		_ = ir.ReadAll(vm)
		// Errors are expected and acceptable; panics/OOM are not.
	})
}
