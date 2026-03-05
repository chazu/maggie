package vm

import "testing"

// ---------------------------------------------------------------------------
// Sandbox primitive tests
// ---------------------------------------------------------------------------

func TestSandboxRun_RestrictsGlobals(t *testing.T) {
	v := NewVM()

	// Set up a global that we want to restrict
	v.Globals["File"] = FromSmallInt(999)

	// Configure sync restrictions
	v.SetSyncRestrictions([]string{"File"})

	// Create a block that tries to read the "File" global
	fileSym := v.Symbols.SymbolValue("File")
	block := &BlockMethod{
		Arity:    0,
		NumTemps: 0,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitUint16(OpPushGlobal, 0) // literal 0 = "File"
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
		Literals: []Value{fileSym},
	}
	blockVal := v.interpreter.createBlockValue(block, nil)

	// Run via Sandbox run:
	sandboxClass := v.classValue(v.Classes.Lookup("Sandbox"))
	proc := v.Send(sandboxClass, "run:", []Value{blockVal})
	result := v.Send(proc, "wait", nil)

	// File should be hidden -- result should be Nil
	if result != Nil {
		t.Errorf("Sandbox run: saw File = %v, want Nil", result)
	}
}

func TestSandboxRun_AllowsUnrestrictedGlobals(t *testing.T) {
	v := NewVM()

	// Configure restrictions only on "File"
	v.SetSyncRestrictions([]string{"File"})

	// Create a block that accesses "Array" (not restricted)
	arraySym := v.Symbols.SymbolValue("Array")
	block := &BlockMethod{
		Arity:    0,
		NumTemps: 0,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitUint16(OpPushGlobal, 0)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
		Literals: []Value{arraySym},
	}
	blockVal := v.interpreter.createBlockValue(block, nil)

	sandboxClass := v.classValue(v.Classes.Lookup("Sandbox"))
	proc := v.Send(sandboxClass, "run:", []Value{blockVal})
	result := v.Send(proc, "wait", nil)

	if result == Nil {
		t.Error("Sandbox run: Array should be visible but got Nil")
	}
}

func TestSandboxRun_NoRestrictions(t *testing.T) {
	v := NewVM()

	// No sync restrictions set -- everything should be visible
	v.Globals["File"] = FromSmallInt(999)

	fileSym := v.Symbols.SymbolValue("File")
	block := &BlockMethod{
		Arity:    0,
		NumTemps: 0,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitUint16(OpPushGlobal, 0)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
		Literals: []Value{fileSym},
	}
	blockVal := v.interpreter.createBlockValue(block, nil)

	sandboxClass := v.classValue(v.Classes.Lookup("Sandbox"))
	proc := v.Send(sandboxClass, "run:", []Value{blockVal})
	result := v.Send(proc, "wait", nil)

	// No restrictions, so File should be visible
	if !result.IsSmallInt() || result.SmallInt() != 999 {
		t.Errorf("Sandbox run: with no restrictions, File = %v, want 999", result)
	}
}

func TestSandboxRestrictions(t *testing.T) {
	v := NewVM()
	v.SetSyncRestrictions([]string{"File", "HTTP", "Network"})

	sandboxClass := v.classValue(v.Classes.Lookup("Sandbox"))
	result := v.Send(sandboxClass, "restrictions", nil)

	if result == Nil {
		t.Fatal("Sandbox restrictions returned nil")
	}

	obj := ObjectFromValue(result)
	if obj == nil {
		t.Fatal("Sandbox restrictions did not return an array")
	}

	if obj.NumSlots() != 3 {
		t.Errorf("Sandbox restrictions returned %d elements, want 3", obj.NumSlots())
	}
}

func TestSandboxIsRehydrated(t *testing.T) {
	v := NewVM()

	// Mark a class as rehydrated
	v.MarkRehydrated("RemoteClass")

	sandboxClass := v.classValue(v.Classes.Lookup("Sandbox"))

	// Test with a rehydrated class
	nameVal := v.registry.NewStringValue("RemoteClass")
	result := v.Send(sandboxClass, "isRehydrated:", []Value{nameVal})
	if result != True {
		t.Error("Sandbox isRehydrated: should return true for rehydrated class")
	}

	// Test with a non-rehydrated class
	nameVal2 := v.registry.NewStringValue("LocalClass")
	result2 := v.Send(sandboxClass, "isRehydrated:", []Value{nameVal2})
	if result2 != False {
		t.Error("Sandbox isRehydrated: should return false for non-rehydrated class")
	}
}

func TestVM_MarkRehydrated(t *testing.T) {
	v := NewVM()

	if v.IsRehydrated("Foo") {
		t.Error("Foo should not be rehydrated before marking")
	}

	v.MarkRehydrated("Foo")

	if !v.IsRehydrated("Foo") {
		t.Error("Foo should be rehydrated after marking")
	}
}

func TestVM_SyncRestrictions(t *testing.T) {
	v := NewVM()

	if len(v.SyncRestrictions()) != 0 {
		t.Error("SyncRestrictions should be empty by default")
	}

	v.SetSyncRestrictions([]string{"File", "HTTP"})

	got := v.SyncRestrictions()
	if len(got) != 2 || got[0] != "File" || got[1] != "HTTP" {
		t.Errorf("SyncRestrictions = %v, want [File HTTP]", got)
	}
}
