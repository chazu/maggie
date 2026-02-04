package main

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

// newTestVM creates a VM loaded with the embedded default image, with the Go
// compiler wired up — the same setup that main() performs.
func newTestVM(t *testing.T) *vm.VM {
	t.Helper()
	vmInst := vm.NewVM()
	if err := vmInst.LoadImageFromBytes(embeddedImage); err != nil {
		t.Fatalf("loading embedded image: %v", err)
	}
	vmInst.ReRegisterNilPrimitives()
	vmInst.ReRegisterBooleanPrimitives()
	vmInst.UseGoCompiler(compiler.Compile)
	return vmInst
}

// writeMagFile writes a .mag source file into the given directory and returns
// its absolute path.
func writeMagFile(t *testing.T, dir, name, source string) string {
	t.Helper()
	p := filepath.Join(dir, name)
	if err := os.WriteFile(p, []byte(source), 0644); err != nil {
		t.Fatalf("writing %s: %v", p, err)
	}
	return p
}

// lookupClassMethod finds a class-side compiled method by class name and
// selector on the given VM. Returns nil if not found.
func lookupClassMethod(vmInst *vm.VM, className, methodName string) *vm.CompiledMethod {
	cls := vmInst.LookupClass(className)
	if cls == nil {
		return nil
	}
	selID := vmInst.Selectors.Lookup(methodName)
	if selID < 0 {
		return nil
	}
	m := cls.ClassVTable.Lookup(selID)
	if m == nil {
		return nil
	}
	cm, ok := m.(*vm.CompiledMethod)
	if !ok {
		return nil
	}
	return cm
}

// lookupInstanceMethod finds an instance-side compiled method by class name
// and selector on the given VM. Returns nil if not found.
func lookupInstanceMethod(vmInst *vm.VM, className, methodName string) *vm.CompiledMethod {
	cls := vmInst.LookupClass(className)
	if cls == nil {
		return nil
	}
	selID := vmInst.Selectors.Lookup(methodName)
	if selID < 0 {
		return nil
	}
	m := cls.VTable.Lookup(selID)
	if m == nil {
		return nil
	}
	cm, ok := m.(*vm.CompiledMethod)
	if !ok {
		return nil
	}
	return cm
}

// ---------------------------------------------------------------------------
// --save-image tests
// ---------------------------------------------------------------------------

func TestSaveImage_CreatesNonEmptyFile(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()
	imagePath := filepath.Join(tmpDir, "test.image")

	if err := vmInst.SaveImage(imagePath); err != nil {
		t.Fatalf("SaveImage failed: %v", err)
	}

	info, err := os.Stat(imagePath)
	if err != nil {
		t.Fatalf("image file not created: %v", err)
	}
	if info.Size() == 0 {
		t.Fatal("image file is empty")
	}
}

func TestSaveImage_AfterCompilingSource(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	// Write a simple Maggie class
	writeMagFile(t, tmpDir, "Greeter.mag", `Greeter subclass: Object
  classMethod: hello [
    ^'Hello from image'
  ]
`)

	// Compile the source file (mimics what compilePath does for a single file)
	methods, err := compilePath(tmpDir, vmInst, false)
	if err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}
	if methods == 0 {
		t.Fatal("expected at least 1 compiled method")
	}

	// Save image
	imagePath := filepath.Join(tmpDir, "greeter.image")
	if err := vmInst.SaveImage(imagePath); err != nil {
		t.Fatalf("SaveImage failed: %v", err)
	}

	info, err := os.Stat(imagePath)
	if err != nil {
		t.Fatalf("image file not created: %v", err)
	}
	if info.Size() == 0 {
		t.Fatal("image file is empty after compiling source")
	}
}

func TestSaveImage_ErrorOnUnwritablePath(t *testing.T) {
	vmInst := newTestVM(t)

	// Attempt to save to a path inside a nonexistent directory
	err := vmInst.SaveImage("/nonexistent/directory/should/not/exist/test.image")
	if err == nil {
		t.Fatal("expected error when saving to unwritable path, got nil")
	}
}

// ---------------------------------------------------------------------------
// --image tests
// ---------------------------------------------------------------------------

func TestLoadImage_ClassesSurviveRoundTrip(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	// Define a class with instance variables and methods
	source := `Counter subclass: Object
  instanceVars: count

  method: init [
    count := 0
  ]

  method: count [
    ^count
  ]

  method: increment [
    count := count + 1
  ]
`
	writeMagFile(t, tmpDir, "Counter.mag", source)

	if _, err := compilePath(tmpDir, vmInst, false); err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	// Verify class exists before save
	cls := vmInst.LookupClass("Counter")
	if cls == nil {
		t.Fatal("Counter class not found in original VM")
	}

	// Save image
	imagePath := filepath.Join(tmpDir, "counter.image")
	if err := vmInst.SaveImage(imagePath); err != nil {
		t.Fatalf("SaveImage failed: %v", err)
	}

	// Load into a fresh VM (mirroring what --image does)
	vm2 := vm.NewVM()
	if err := vm2.LoadImage(imagePath); err != nil {
		t.Fatalf("LoadImage failed: %v", err)
	}
	vm2.ReRegisterNilPrimitives()
	vm2.ReRegisterBooleanPrimitives()
	vm2.UseGoCompiler(compiler.Compile)

	// Verify class survived
	cls2 := vm2.LookupClass("Counter")
	if cls2 == nil {
		t.Fatal("Counter class not found after loading image")
	}
	if cls2.Name != "Counter" {
		t.Errorf("class name = %q, want Counter", cls2.Name)
	}
}

func TestLoadImage_MethodsSurviveRoundTrip(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	// Define a class with instance methods
	writeMagFile(t, tmpDir, "Calculator.mag", `Calculator subclass: Object
  method: double: n [
    ^n + n
  ]

  method: negate: n [
    ^0 - n
  ]
`)

	if _, err := compilePath(tmpDir, vmInst, false); err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	// Verify methods exist before save
	dm := lookupInstanceMethod(vmInst, "Calculator", "double:")
	if dm == nil {
		t.Fatal("double: not found before save")
	}
	im := lookupInstanceMethod(vmInst, "Calculator", "negate:")
	if im == nil {
		t.Fatal("negate: not found before save")
	}

	// Save image
	imagePath := filepath.Join(tmpDir, "calc.image")
	if err := vmInst.SaveImage(imagePath); err != nil {
		t.Fatalf("SaveImage failed: %v", err)
	}

	// Load into a fresh VM
	vm2 := vm.NewVM()
	if err := vm2.LoadImage(imagePath); err != nil {
		t.Fatalf("LoadImage failed: %v", err)
	}
	vm2.ReRegisterNilPrimitives()
	vm2.ReRegisterBooleanPrimitives()

	// Verify instance methods survived
	dm2 := lookupInstanceMethod(vm2, "Calculator", "double:")
	if dm2 == nil {
		t.Error("double: instance method not found after image load")
	}

	im2 := lookupInstanceMethod(vm2, "Calculator", "negate:")
	if im2 == nil {
		t.Error("negate: instance method not found after image load")
	}
}

func TestLoadImage_ErrorOnNonexistentFile(t *testing.T) {
	vmInst := vm.NewVM()
	err := vmInst.LoadImage("/tmp/maggie_nonexistent_test_image_12345.image")
	if err == nil {
		t.Fatal("expected error when loading nonexistent image file, got nil")
	}
}

// ---------------------------------------------------------------------------
// Round-trip: save then load and execute a method
// ---------------------------------------------------------------------------

func TestImageRoundTrip_ExecuteMethod(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	// Define a method on Object so we can execute it via runMain on the
	// original VM AND look it up on the loaded VM.
	source := `Object subclass: Object
  method: addThreeAndFour [
    ^3 + 4
  ]
`
	writeMagFile(t, tmpDir, "Adder.mag", source)

	if _, err := compilePath(tmpDir, vmInst, false); err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	// Run the method on the original VM to verify it works
	result, err := runMain(vmInst, "addThreeAndFour", false)
	if err != nil {
		t.Fatalf("runMain on original VM failed: %v", err)
	}
	if !result.IsSmallInt() || result.SmallInt() != 7 {
		t.Fatalf("original VM result = %v, want SmallInt(7)", result)
	}

	// Save image
	imagePath := filepath.Join(tmpDir, "adder.image")
	if err := vmInst.SaveImage(imagePath); err != nil {
		t.Fatalf("SaveImage failed: %v", err)
	}

	// Load into a fresh VM
	vm2 := vm.NewVM()
	if err := vm2.LoadImage(imagePath); err != nil {
		t.Fatalf("LoadImage failed: %v", err)
	}
	vm2.ReRegisterNilPrimitives()
	vm2.ReRegisterBooleanPrimitives()
	vm2.UseGoCompiler(compiler.Compile)

	// Look up the instance method by name on the loaded VM and execute directly
	cm := lookupInstanceMethod(vm2, "Object", "addThreeAndFour")
	if cm == nil {
		t.Fatal("addThreeAndFour not found on loaded VM")
	}

	// Execute the method directly via the interpreter
	result2 := vm2.Execute(cm, vm.Nil, nil)
	if !result2.IsSmallInt() || result2.SmallInt() != 7 {
		t.Fatalf("loaded VM result = %v, want SmallInt(7)", result2)
	}
}

func TestImageRoundTrip_ExecuteInstanceMethod(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	// Define a class with an instance method on Object
	source := `Object subclass: Object
  method: magicNumber [
    ^42
  ]
`
	writeMagFile(t, tmpDir, "Magic.mag", source)

	if _, err := compilePath(tmpDir, vmInst, false); err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	// Verify on original VM
	result, err := runMain(vmInst, "magicNumber", false)
	if err != nil {
		t.Fatalf("runMain on original VM failed: %v", err)
	}
	if !result.IsSmallInt() || result.SmallInt() != 42 {
		t.Fatalf("original VM result = %v, want SmallInt(42)", result)
	}

	// Save and reload
	imagePath := filepath.Join(tmpDir, "magic.image")
	if err := vmInst.SaveImage(imagePath); err != nil {
		t.Fatalf("SaveImage failed: %v", err)
	}

	vm2 := vm.NewVM()
	if err := vm2.LoadImage(imagePath); err != nil {
		t.Fatalf("LoadImage failed: %v", err)
	}
	vm2.ReRegisterNilPrimitives()
	vm2.ReRegisterBooleanPrimitives()
	vm2.UseGoCompiler(compiler.Compile)

	// Look up and execute directly
	cm := lookupInstanceMethod(vm2, "Object", "magicNumber")
	if cm == nil {
		t.Fatal("magicNumber not found on loaded VM")
	}

	result2 := vm2.Execute(cm, vm.Nil, nil)
	if !result2.IsSmallInt() || result2.SmallInt() != 42 {
		t.Fatalf("loaded VM result = %v, want SmallInt(42)", result2)
	}
}

func TestImageRoundTrip_GlobalsSurvive(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	// Set globals directly on the VM (mimics what Compiler evaluate: does)
	vmInst.SetGlobal("testGlobalInt", vm.FromSmallInt(99))
	vmInst.SetGlobal("testGlobalBool", vm.True)

	// Save image
	imagePath := filepath.Join(tmpDir, "globals.image")
	if err := vmInst.SaveImage(imagePath); err != nil {
		t.Fatalf("SaveImage failed: %v", err)
	}

	// Load into fresh VM
	vm2 := vm.NewVM()
	if err := vm2.LoadImage(imagePath); err != nil {
		t.Fatalf("LoadImage failed: %v", err)
	}

	// Verify globals survived
	val, ok := vm2.LookupGlobal("testGlobalInt")
	if !ok {
		t.Fatal("testGlobalInt not found after image load")
	}
	if !val.IsSmallInt() || val.SmallInt() != 99 {
		t.Errorf("testGlobalInt = %v, want SmallInt(99)", val)
	}

	val, ok = vm2.LookupGlobal("testGlobalBool")
	if !ok {
		t.Fatal("testGlobalBool not found after image load")
	}
	if val != vm.True {
		t.Errorf("testGlobalBool = %v, want True", val)
	}
}

func TestImageRoundTrip_MultipleClasses(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	// Create multiple classes
	writeMagFile(t, tmpDir, "Dog.mag", `Dog subclass: Object
  instanceVars: name

  method: setName: aName [
    name := aName
  ]

  method: name [
    ^name
  ]
`)

	writeMagFile(t, tmpDir, "Cat.mag", `Cat subclass: Object
  instanceVars: lives

  method: initLives [
    lives := 9
  ]

  method: lives [
    ^lives
  ]
`)

	if _, err := compilePath(tmpDir, vmInst, false); err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	// Save
	imagePath := filepath.Join(tmpDir, "animals.image")
	if err := vmInst.SaveImage(imagePath); err != nil {
		t.Fatalf("SaveImage failed: %v", err)
	}

	// Load
	vm2 := vm.NewVM()
	if err := vm2.LoadImage(imagePath); err != nil {
		t.Fatalf("LoadImage failed: %v", err)
	}
	vm2.ReRegisterNilPrimitives()
	vm2.ReRegisterBooleanPrimitives()

	// Verify both classes survived
	dogClass := vm2.LookupClass("Dog")
	if dogClass == nil {
		t.Error("Dog class not found after image load")
	}

	catClass := vm2.LookupClass("Cat")
	if catClass == nil {
		t.Error("Cat class not found after image load")
	}

	// Verify methods on each class
	if lookupInstanceMethod(vm2, "Dog", "name") == nil {
		t.Error("Dog>>name method not found after image load")
	}
	if lookupInstanceMethod(vm2, "Cat", "lives") == nil {
		t.Error("Cat>>lives method not found after image load")
	}
}

// ---------------------------------------------------------------------------
// Image file is loadable by a fresh VM (mirroring --image flag path)
// ---------------------------------------------------------------------------

func TestLoadImage_FreshVMCanLoadSavedImage(t *testing.T) {
	// This test mirrors the exact --image code path in main():
	//   vmInst := vm.NewVM()
	//   vmInst.LoadImage(*customImagePath)
	//   vmInst.ReRegisterNilPrimitives()
	//   vmInst.ReRegisterBooleanPrimitives()
	//   vmInst.UseGoCompiler(compiler.Compile)

	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	// Compile a class with an instance method
	writeMagFile(t, tmpDir, "Ping.mag", `Ping subclass: Object
  method: pong [
    ^99
  ]
`)
	if _, err := compilePath(tmpDir, vmInst, false); err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	imagePath := filepath.Join(tmpDir, "ping.image")
	if err := vmInst.SaveImage(imagePath); err != nil {
		t.Fatalf("SaveImage failed: %v", err)
	}

	// Load using the exact same sequence as main()
	vm2 := vm.NewVM()
	if err := vm2.LoadImage(imagePath); err != nil {
		t.Fatalf("LoadImage failed: %v", err)
	}
	vm2.ReRegisterNilPrimitives()
	vm2.ReRegisterBooleanPrimitives()
	vm2.UseGoCompiler(compiler.Compile)

	// Verify the class is there
	cls := vm2.LookupClass("Ping")
	if cls == nil {
		t.Fatal("Ping class not found after loading saved image")
	}

	// Verify the instance method exists and can be executed
	cm := lookupInstanceMethod(vm2, "Ping", "pong")
	if cm == nil {
		t.Fatal("Ping>>pong instance method not found after image load")
	}

	result := vm2.Execute(cm, vm.Nil, nil)
	if !result.IsSmallInt() || result.SmallInt() != 99 {
		t.Fatalf("Ping>>pong result = %v, want SmallInt(99)", result)
	}
}

// ---------------------------------------------------------------------------
// Additional source can be compiled on top of a loaded image
// ---------------------------------------------------------------------------

func TestLoadImage_CanCompileAdditionalSource(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	// Create and save a base image with one class
	writeMagFile(t, tmpDir, "Base.mag", `Base subclass: Object
  classMethod: value [ ^10 ]
`)
	if _, err := compilePath(filepath.Join(tmpDir, "Base.mag"), vmInst, false); err != nil {
		t.Fatalf("compilePath Base.mag failed: %v", err)
	}

	imagePath := filepath.Join(tmpDir, "base.image")
	if err := vmInst.SaveImage(imagePath); err != nil {
		t.Fatalf("SaveImage failed: %v", err)
	}

	// Load the image into a fresh VM
	vm2 := vm.NewVM()
	if err := vm2.LoadImage(imagePath); err != nil {
		t.Fatalf("LoadImage failed: %v", err)
	}
	vm2.ReRegisterNilPrimitives()
	vm2.ReRegisterBooleanPrimitives()
	vm2.UseGoCompiler(compiler.Compile)

	// Compile additional source on top of the loaded image
	extraDir := filepath.Join(tmpDir, "extra")
	if err := os.Mkdir(extraDir, 0755); err != nil {
		t.Fatalf("mkdir extra: %v", err)
	}
	writeMagFile(t, extraDir, "Extra.mag", `Extra subclass: Object
  classMethod: value [ ^20 ]
`)
	if _, err := compilePath(extraDir, vm2, false); err != nil {
		t.Fatalf("compilePath Extra.mag failed: %v", err)
	}

	// Verify both Base (from image) and Extra (from source) exist
	if vm2.LookupClass("Base") == nil {
		t.Error("Base class not found after loading image + additional source")
	}
	if vm2.LookupClass("Extra") == nil {
		t.Error("Extra class not found after compiling additional source")
	}
}

// ---------------------------------------------------------------------------
// Error cases
// ---------------------------------------------------------------------------

func TestSaveImage_OverwritesExistingFile(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()
	imagePath := filepath.Join(tmpDir, "overwrite.image")

	// Save once
	if err := vmInst.SaveImage(imagePath); err != nil {
		t.Fatalf("first SaveImage failed: %v", err)
	}
	info1, _ := os.Stat(imagePath)
	size1 := info1.Size()

	// Compile additional source to change the VM state
	extraDir := filepath.Join(tmpDir, "src")
	if err := os.Mkdir(extraDir, 0755); err != nil {
		t.Fatalf("mkdir src: %v", err)
	}
	writeMagFile(t, extraDir, "Extra.mag", `Extra subclass: Object
  classMethod: value [ ^100 ]
`)
	if _, err := compilePath(extraDir, vmInst, false); err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	// Save again to the same path
	if err := vmInst.SaveImage(imagePath); err != nil {
		t.Fatalf("second SaveImage failed: %v", err)
	}
	info2, _ := os.Stat(imagePath)
	size2 := info2.Size()

	// Second image should be at least as large (likely larger since we added a class)
	if size2 < size1 {
		t.Errorf("second image (%d bytes) is smaller than first (%d bytes)", size2, size1)
	}
}

func TestLoadImage_CorruptFileReturnsError(t *testing.T) {
	tmpDir := t.TempDir()
	corruptPath := filepath.Join(tmpDir, "corrupt.image")

	// Write garbage data
	if err := os.WriteFile(corruptPath, []byte("this is not a valid image file"), 0644); err != nil {
		t.Fatalf("writing corrupt file: %v", err)
	}

	vmInst := vm.NewVM()
	err := vmInst.LoadImage(corruptPath)
	if err == nil {
		t.Fatal("expected error when loading corrupt image, got nil")
	}
}

func TestLoadImage_EmptyFileReturnsError(t *testing.T) {
	tmpDir := t.TempDir()
	emptyPath := filepath.Join(tmpDir, "empty.image")

	// Write empty file
	if err := os.WriteFile(emptyPath, []byte{}, 0644); err != nil {
		t.Fatalf("writing empty file: %v", err)
	}

	vmInst := vm.NewVM()
	err := vmInst.LoadImage(emptyPath)
	if err == nil {
		t.Fatal("expected error when loading empty image, got nil")
	}
}

// ---------------------------------------------------------------------------
// Class method IsClassMethod flag survives image round-trip
// ---------------------------------------------------------------------------

func TestImageRoundTrip_ClassMethodIsClassMethodFlag(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	// Define a class with both an instance method and a class method.
	source := `Gadget subclass: Object

  method: instanceValue [
    ^100
  ]

  classMethod: classValue [
    ^200
  ]
`
	writeMagFile(t, tmpDir, "Gadget.mag", source)

	if _, err := compilePath(tmpDir, vmInst, false); err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	// --- Verify before save ---

	// Instance method should be on VTable, not ClassVTable
	im := lookupInstanceMethod(vmInst, "Gadget", "instanceValue")
	if im == nil {
		t.Fatal("instanceValue not found on VTable before save")
	}
	if im.IsClassMethod {
		t.Error("instanceValue.IsClassMethod should be false before save")
	}

	// Class method should be on ClassVTable with IsClassMethod = true
	cm := lookupClassMethod(vmInst, "Gadget", "classValue")
	if cm == nil {
		t.Fatal("classValue not found on ClassVTable before save")
	}
	if !cm.IsClassMethod {
		t.Error("classValue.IsClassMethod should be true before save")
	}

	// Execute both methods to confirm correct results
	gadgetCls := vmInst.LookupClass("Gadget")
	if gadgetCls == nil {
		t.Fatal("Gadget class not found")
	}

	instResult := vmInst.Execute(im, vm.Nil, nil)
	if !instResult.IsSmallInt() || instResult.SmallInt() != 100 {
		t.Fatalf("instanceValue returned %v, want 100", instResult)
	}

	classReceiver := vmInst.ClassValue(gadgetCls)
	classResult := vmInst.Execute(cm, classReceiver, nil)
	if !classResult.IsSmallInt() || classResult.SmallInt() != 200 {
		t.Fatalf("classValue returned %v, want 200", classResult)
	}

	// --- Save image ---
	imagePath := filepath.Join(tmpDir, "gadget.image")
	if err := vmInst.SaveImage(imagePath); err != nil {
		t.Fatalf("SaveImage failed: %v", err)
	}

	// --- Load into a fresh VM ---
	vm2 := vm.NewVM()
	if err := vm2.LoadImage(imagePath); err != nil {
		t.Fatalf("LoadImage failed: %v", err)
	}
	vm2.ReRegisterNilPrimitives()
	vm2.ReRegisterBooleanPrimitives()
	vm2.UseGoCompiler(compiler.Compile)

	// --- Verify after load ---

	// Instance method should be on VTable
	im2 := lookupInstanceMethod(vm2, "Gadget", "instanceValue")
	if im2 == nil {
		t.Fatal("instanceValue not found on VTable after image load")
	}
	if im2.IsClassMethod {
		t.Error("instanceValue.IsClassMethod should be false after image load")
	}

	// Class method should be on ClassVTable with IsClassMethod = true
	cm2 := lookupClassMethod(vm2, "Gadget", "classValue")
	if cm2 == nil {
		t.Fatal("classValue not found on ClassVTable after image load")
	}
	if !cm2.IsClassMethod {
		t.Error("classValue.IsClassMethod should be true after image load")
	}

	// Instance method should NOT be on ClassVTable
	gadgetCls2 := vm2.LookupClass("Gadget")
	if gadgetCls2 == nil {
		t.Fatal("Gadget class not found in loaded VM")
	}
	instSelID := vm2.Selectors.Lookup("instanceValue")
	if instSelID >= 0 && gadgetCls2.ClassVTable.LookupLocal(instSelID) != nil {
		t.Error("instanceValue should NOT be on ClassVTable after image load")
	}

	// Class method should NOT be on VTable
	classSelID := vm2.Selectors.Lookup("classValue")
	if classSelID >= 0 && gadgetCls2.VTable.LookupLocal(classSelID) != nil {
		t.Error("classValue should NOT be on VTable after image load")
	}

	// Execute both methods on the loaded VM
	instResult2 := vm2.Execute(im2, vm.Nil, nil)
	if !instResult2.IsSmallInt() || instResult2.SmallInt() != 100 {
		t.Fatalf("instanceValue after load returned %v, want 100", instResult2)
	}

	classReceiver2 := vm2.ClassValue(gadgetCls2)
	classResult2 := vm2.Execute(cm2, classReceiver2, nil)
	if !classResult2.IsSmallInt() || classResult2.SmallInt() != 200 {
		t.Fatalf("classValue after load returned %v, want 200", classResult2)
	}
}

func TestSaveImage_ReadOnlyDirectoryFails(t *testing.T) {
	tmpDir := t.TempDir()
	readOnlyDir := filepath.Join(tmpDir, "readonly")
	if err := os.Mkdir(readOnlyDir, 0555); err != nil {
		t.Fatalf("creating read-only dir: %v", err)
	}
	// Ensure cleanup can remove the dir
	t.Cleanup(func() { os.Chmod(readOnlyDir, 0755) })

	vmInst := newTestVM(t)
	err := vmInst.SaveImage(filepath.Join(readOnlyDir, "fail.image"))
	if err == nil {
		t.Fatal("expected error when saving to read-only directory, got nil")
	}
}
