package vm

import (
	"os"
	"path/filepath"
	"testing"

	"cuelang.org/go/cue"
	"cuelang.org/go/cue/cuecontext"
)

// ---------------------------------------------------------------------------
// CueContext Registry Tests
// ---------------------------------------------------------------------------

func TestCueContextRegistration(t *testing.T) {
	vm := NewVM()
	ctx := cuecontext.New()
	obj := &CueContextObject{ctx: ctx}
	val := vm.vmRegisterCueContext(obj)

	if !isCueContextValue(val) {
		t.Fatal("vmRegisterCueContext should produce a CueContext value")
	}

	got := vm.vmGetCueContext(val)
	if got == nil {
		t.Fatal("vmGetCueContext returned nil for registered context")
	}
	if got.ctx != ctx {
		t.Error("stored CUE context does not match original")
	}
}

func TestIsCueContextValueFalse(t *testing.T) {
	vm := NewVM()
	_ = vm

	if isCueContextValue(Nil) {
		t.Error("Nil should not be a CueContext value")
	}
	if isCueContextValue(FromSmallInt(42)) {
		t.Error("SmallInt should not be a CueContext value")
	}
	if isCueContextValue(True) {
		t.Error("True should not be a CueContext value")
	}
}

// ---------------------------------------------------------------------------
// CueValue Registry Tests
// ---------------------------------------------------------------------------

func TestCueValueRegistration(t *testing.T) {
	vm := NewVM()
	ctx := cuecontext.New()
	cueVal := ctx.CompileString(`x: 42`)
	obj := &CueValueObject{val: cueVal}
	val := vm.vmRegisterCueValue(obj)

	if !isCueValueValue(val) {
		t.Fatal("vmRegisterCueValue should produce a CueValue value")
	}

	got := vm.vmGetCueValue(val)
	if got == nil {
		t.Fatal("vmGetCueValue returned nil for registered value")
	}
}

func TestIsCueValueValueFalse(t *testing.T) {
	if isCueValueValue(Nil) {
		t.Error("Nil should not be a CueValue value")
	}
	if isCueValueValue(FromSmallInt(99)) {
		t.Error("SmallInt should not be a CueValue value")
	}
	if isCueValueValue(False) {
		t.Error("False should not be a CueValue value")
	}
}

// ---------------------------------------------------------------------------
// CueContext Compile String Tests
// ---------------------------------------------------------------------------

func TestCueContextCompileStringSuccess(t *testing.T) {
	vm := NewVM()
	ctx := cuecontext.New()
	cctx := &CueContextObject{ctx: ctx}
	val := cctx.ctx.CompileString(`a: 1, b: "hello"`)
	if err := val.Err(); err != nil {
		t.Fatalf("compile failed: %v", err)
	}
	obj := &CueValueObject{val: val}
	registered := vm.vmRegisterCueValue(obj)
	got := vm.vmGetCueValue(registered)
	if got == nil {
		t.Fatal("should retrieve registered CueValue")
	}
}

func TestCueContextCompileStringError(t *testing.T) {
	ctx := cuecontext.New()
	val := ctx.CompileString(`{{{invalid`)
	if val.Err() == nil {
		t.Error("expected compile error for invalid CUE")
	}
}

// ---------------------------------------------------------------------------
// CueValue Validate Tests
// ---------------------------------------------------------------------------

func TestCueValueValidateSuccess(t *testing.T) {
	ctx := cuecontext.New()
	val := ctx.CompileString(`a: 1, b: "hello"`)
	if err := val.Validate(); err != nil {
		t.Errorf("expected valid CUE, got error: %v", err)
	}
}

func TestCueValueValidateFailure(t *testing.T) {
	ctx := cuecontext.New()
	val := ctx.CompileString(`a: int & string`) // conflicting types
	if err := val.Validate(); err == nil {
		t.Error("expected validation error for conflicting types")
	}
}

// ---------------------------------------------------------------------------
// CueValue Unify Tests
// ---------------------------------------------------------------------------

func TestCueValueUnify(t *testing.T) {
	ctx := cuecontext.New()
	schema := ctx.CompileString(`name: string, age: int`)
	data := ctx.CompileString(`name: "Alice", age: 30`)
	unified := schema.Unify(data)
	if err := unified.Validate(); err != nil {
		t.Errorf("unification should succeed: %v", err)
	}
}

func TestCueValueUnifyConflict(t *testing.T) {
	ctx := cuecontext.New()
	a := ctx.CompileString(`x: 1`)
	b := ctx.CompileString(`x: 2`)
	unified := a.Unify(b)
	if err := unified.Validate(); err == nil {
		t.Error("unification of conflicting values should produce error")
	}
}

// ---------------------------------------------------------------------------
// CueValue Lookup Tests
// ---------------------------------------------------------------------------

func TestCueValueLookup(t *testing.T) {
	ctx := cuecontext.New()
	val := ctx.CompileString(`a: {b: {c: 42}}`)

	result := val.LookupPath(cuePathParse("a.b.c"))
	if !result.Exists() {
		t.Fatal("lookup a.b.c should exist")
	}
	n, err := result.Int64()
	if err != nil {
		t.Fatalf("expected int64: %v", err)
	}
	if n != 42 {
		t.Errorf("expected 42, got %d", n)
	}
}

func TestCueValueLookupMissing(t *testing.T) {
	ctx := cuecontext.New()
	val := ctx.CompileString(`a: 1`)
	result := val.LookupPath(cuePathParse("b"))
	if result.Exists() {
		t.Error("lookup of missing field should not exist")
	}
}

// ---------------------------------------------------------------------------
// CueValue Fields Iteration Tests
// ---------------------------------------------------------------------------

func TestCueValueFields(t *testing.T) {
	ctx := cuecontext.New()
	val := ctx.CompileString(`x: 1, y: 2, z: 3`)
	iter, err := val.Fields()
	if err != nil {
		t.Fatalf("Fields() error: %v", err)
	}
	count := 0
	for iter.Next() {
		count++
	}
	if count != 3 {
		t.Errorf("expected 3 fields, got %d", count)
	}
}

// ---------------------------------------------------------------------------
// CueValue toMaggie Conversion Tests
// ---------------------------------------------------------------------------

func TestCueToMaggieInt(t *testing.T) {
	vm := NewVM()
	ctx := cuecontext.New()
	val := ctx.CompileString(`42`)
	result := vm.cueToMaggie(val)
	if !result.IsSmallInt() {
		t.Fatalf("expected SmallInt, got something else")
	}
	if result.SmallInt() != 42 {
		t.Errorf("expected 42, got %d", result.SmallInt())
	}
}

func TestCueToMaggieString(t *testing.T) {
	vm := NewVM()
	ctx := cuecontext.New()
	val := ctx.CompileString(`"hello"`)
	result := vm.cueToMaggie(val)
	s := vm.registry.GetStringContent(result)
	if s != "hello" {
		t.Errorf("expected 'hello', got %q", s)
	}
}

func TestCueToMaggieBool(t *testing.T) {
	vm := NewVM()
	ctx := cuecontext.New()
	val := ctx.CompileString(`true`)
	result := vm.cueToMaggie(val)
	if result != True {
		t.Error("expected True")
	}
}

func TestCueToMaggieNull(t *testing.T) {
	vm := NewVM()
	ctx := cuecontext.New()
	val := ctx.CompileString(`null`)
	result := vm.cueToMaggie(val)
	if result != Nil {
		t.Error("expected Nil")
	}
}

func TestCueToMaggieStruct(t *testing.T) {
	vm := NewVM()
	ctx := cuecontext.New()
	val := ctx.CompileString(`a: 1, b: "two"`)
	result := vm.cueToMaggie(val)
	dict := vm.registry.GetDictionaryObject(result)
	if dict == nil {
		t.Fatal("expected Dictionary for struct conversion")
	}
	if len(dict.Data) != 2 {
		t.Errorf("expected 2 entries, got %d", len(dict.Data))
	}
}

func TestCueToMaggieList(t *testing.T) {
	vm := NewVM()
	ctx := cuecontext.New()
	val := ctx.CompileString(`[1, 2, 3]`)
	result := vm.cueToMaggie(val)
	if !result.IsObject() {
		t.Fatal("expected Object (Array) for list conversion")
	}
	obj := ObjectFromValue(result)
	if obj == nil {
		t.Fatal("expected non-nil object")
	}
	if obj.NumSlots() != 3 {
		t.Errorf("expected 3 elements, got %d", obj.NumSlots())
	}
}

// ---------------------------------------------------------------------------
// CueExportValue Tests
// ---------------------------------------------------------------------------

func TestCueExportValuePrimitives(t *testing.T) {
	vm := NewVM()

	if vm.cueExportValue(Nil) != nil {
		t.Error("Nil should export as nil")
	}
	if vm.cueExportValue(True) != true {
		t.Error("True should export as true")
	}
	if vm.cueExportValue(False) != false {
		t.Error("False should export as false")
	}
	if vm.cueExportValue(FromSmallInt(99)).(int64) != 99 {
		t.Error("SmallInt 99 should export as int64(99)")
	}

	s := vm.registry.NewStringValue("test")
	if vm.cueExportValue(s).(string) != "test" {
		t.Error("String 'test' should export as 'test'")
	}
}

// ---------------------------------------------------------------------------
// CueValue FillPath Tests
// ---------------------------------------------------------------------------

func TestCueValueFillPath(t *testing.T) {
	ctx := cuecontext.New()
	val := ctx.CompileString(`x: _, output: x`)
	filled := val.FillPath(cuePathParse("x"), "hello")
	if err := filled.Err(); err != nil {
		t.Fatalf("FillPath error: %v", err)
	}

	out := filled.LookupPath(cuePathParse("output"))
	s, err := out.String()
	if err != nil {
		t.Fatalf("expected string result: %v", err)
	}
	if s != "hello" {
		t.Errorf("expected 'hello', got %q", s)
	}
}

func TestCueValueFillPathHidden(t *testing.T) {
	ctx := cuecontext.New()
	val := ctx.CompileString(`_input: _, output: _input`)
	hiddenPath := cue.MakePath(cue.Hid("_input", "_"))
	filled := val.FillPath(hiddenPath, "hello")
	if err := filled.Err(); err != nil {
		t.Fatalf("FillPath error: %v", err)
	}

	out := filled.LookupPath(cuePathParse("output"))
	s, err := out.String()
	if err != nil {
		t.Fatalf("expected string result: %v", err)
	}
	if s != "hello" {
		t.Errorf("expected 'hello', got %q", s)
	}
}

// ---------------------------------------------------------------------------
// CueContext LoadFile Tests
// ---------------------------------------------------------------------------

func TestCueContextLoadFile(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "test.cue")
	err := os.WriteFile(path, []byte(`x: 42, y: "hello"`), 0644)
	if err != nil {
		t.Fatalf("write temp file: %v", err)
	}

	ctx := cuecontext.New()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read temp file: %v", err)
	}
	val := ctx.CompileBytes(data)
	if err := val.Err(); err != nil {
		t.Fatalf("compile error: %v", err)
	}

	result := val.LookupPath(cuePathParse("x"))
	n, err := result.Int64()
	if err != nil {
		t.Fatalf("expected int64: %v", err)
	}
	if n != 42 {
		t.Errorf("expected 42, got %d", n)
	}
}

// ---------------------------------------------------------------------------
// Schema Validation Tests
// ---------------------------------------------------------------------------

func TestCueSchemaValidation(t *testing.T) {
	ctx := cuecontext.New()

	schema := ctx.CompileString(`name: string, age: int & >0`)
	data := ctx.CompileString(`name: "Alice", age: 30`)
	unified := schema.Unify(data)
	if err := unified.Validate(); err != nil {
		t.Errorf("valid data should pass: %v", err)
	}
}

func TestCueSchemaValidationFail(t *testing.T) {
	ctx := cuecontext.New()

	schema := ctx.CompileString(`name: string, age: int & >0`)
	data := ctx.CompileString(`name: "Alice", age: -5`)
	unified := schema.Unify(data)
	if err := unified.Validate(); err == nil {
		t.Error("invalid data (negative age) should fail validation")
	}
}

// ---------------------------------------------------------------------------
// Hidden Field Injection Tests
// ---------------------------------------------------------------------------

func TestCueHiddenFieldInjection(t *testing.T) {
	ctx := cuecontext.New()
	val := ctx.CompileString(`_input: _, result: _input`)
	hiddenPath := cue.MakePath(cue.Hid("_input", "_"))
	filled := val.FillPath(hiddenPath, "injected")
	if err := filled.Err(); err != nil {
		t.Fatalf("FillPath error: %v", err)
	}

	out := filled.LookupPath(cuePathParse("result"))
	s, err := out.String()
	if err != nil {
		t.Fatalf("expected string: %v", err)
	}
	if s != "injected" {
		t.Errorf("expected 'injected', got %q", s)
	}
}

// ---------------------------------------------------------------------------
// Re-compilation with Context Tests
// ---------------------------------------------------------------------------

func TestCueRecompilationWithContext(t *testing.T) {
	ctx := cuecontext.New()

	// Phase 1: compile with stub
	step := ctx.CompileString(`_ctx: _, greeting: "Hello " + _ctx.name`)
	// Phase 2: fill with concrete params
	hiddenPath := cue.MakePath(cue.Hid("_ctx", "_"))
	filled := step.FillPath(hiddenPath, map[string]interface{}{
		"name": "World",
	})
	if err := filled.Err(); err != nil {
		t.Fatalf("FillPath error: %v", err)
	}

	result := filled.LookupPath(cuePathParse("greeting"))
	s, err := result.String()
	if err != nil {
		t.Fatalf("expected string: %v", err)
	}
	if s != "Hello World" {
		t.Errorf("expected 'Hello World', got %q", s)
	}
}

// ---------------------------------------------------------------------------
// Error Message Tests
// ---------------------------------------------------------------------------

func TestCueErrorMessages(t *testing.T) {
	ctx := cuecontext.New()
	val := ctx.CompileString(`{{{invalid`)
	if val.Err() == nil {
		t.Fatal("expected error for invalid CUE")
	}
	errMsg := val.Err().Error()
	if errMsg == "" {
		t.Error("error message should not be empty")
	}
}

func TestCueValidationErrorPath(t *testing.T) {
	ctx := cuecontext.New()
	schema := ctx.CompileString(`name: string, age: int`)
	data := ctx.CompileString(`name: 123, age: "wrong"`)
	unified := schema.Unify(data)
	err := unified.Validate()
	if err == nil {
		t.Fatal("expected validation error")
	}
	// Error should mention the problematic fields
	errMsg := err.Error()
	if errMsg == "" {
		t.Error("validation error message should not be empty")
	}
}

// ---------------------------------------------------------------------------
// Marker Tests
// ---------------------------------------------------------------------------

func TestCueContextMarkerDistinct(t *testing.T) {
	if cueContextMarker == cueValueMarker {
		t.Error("CueContext and CueValue markers must be distinct")
	}
	if cueContextMarker == httpServerMarker {
		t.Error("CueContext marker must not collide with httpServer")
	}
}

func TestCueValueMarkerDistinct(t *testing.T) {
	if cueValueMarker == channelMarker {
		t.Error("CueValue marker must not collide with channel")
	}
	if cueValueMarker == resultMarker {
		t.Error("CueValue marker must not collide with result")
	}
}

// ---------------------------------------------------------------------------
// Helper to avoid importing cue in test file signatures
// ---------------------------------------------------------------------------

func cuePathParse(s string) cue.Path {
	return cue.ParsePath(s)
}
