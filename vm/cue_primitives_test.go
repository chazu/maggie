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
// objectAsCueValue Tests
// ---------------------------------------------------------------------------

func TestObjectAsCueValueWithIvars(t *testing.T) {
	vm := NewVM()

	// Create a Point class with x and y ivars
	pointClass := vm.createClass("Point", vm.ObjectClass)
	pointClass.InstVars = []string{"x", "y"}
	pointClass.NumSlots = 2
	vm.Globals["Point"] = vm.classValue(pointClass)

	// Create an instance with x=3, y=7
	obj := NewObject(pointClass.VTable, 2)
	obj.SetSlot(0, FromSmallInt(3))
	obj.SetSlot(1, FromSmallInt(7))

	cueObj := vm.objectAsCueValue(obj.ToValue())
	if cueObj == nil {
		t.Fatal("objectAsCueValue returned nil")
	}

	// Check that the CUE value has the right fields
	xVal := cueObj.val.LookupPath(cue.ParsePath("x"))
	if !xVal.Exists() {
		t.Fatal("CUE value should have field 'x'")
	}
	n, err := xVal.Int64()
	if err != nil {
		t.Fatalf("expected int64 for x: %v", err)
	}
	if n != 3 {
		t.Errorf("expected x=3, got %d", n)
	}

	yVal := cueObj.val.LookupPath(cue.ParsePath("y"))
	if !yVal.Exists() {
		t.Fatal("CUE value should have field 'y'")
	}
	n, err = yVal.Int64()
	if err != nil {
		t.Fatalf("expected int64 for y: %v", err)
	}
	if n != 7 {
		t.Errorf("expected y=7, got %d", n)
	}
}

func TestObjectAsCueValueScalar(t *testing.T) {
	vm := NewVM()

	// SmallInt should project as a scalar
	cueObj := vm.objectAsCueValue(FromSmallInt(42))
	if cueObj == nil {
		t.Fatal("objectAsCueValue returned nil for SmallInt")
	}
	n, err := cueObj.val.Int64()
	if err != nil {
		t.Fatalf("expected int64: %v", err)
	}
	if n != 42 {
		t.Errorf("expected 42, got %d", n)
	}
}

func TestObjectAsCueValueString(t *testing.T) {
	vm := NewVM()

	strVal := vm.registry.NewStringValue("hello")
	cueObj := vm.objectAsCueValue(strVal)
	if cueObj == nil {
		t.Fatal("objectAsCueValue returned nil for String")
	}
	s, err := cueObj.val.String()
	if err != nil {
		t.Fatalf("expected string: %v", err)
	}
	if s != "hello" {
		t.Errorf("expected 'hello', got %q", s)
	}
}

func TestObjectAsCueValueNoIvars(t *testing.T) {
	vm := NewVM()

	// Object with no ivars should produce an empty struct
	obj := NewObject(vm.ObjectClass.VTable, 0)
	cueObj := vm.objectAsCueValue(obj.ToValue())
	if cueObj == nil {
		t.Fatal("objectAsCueValue returned nil")
	}
	if err := cueObj.val.Err(); err != nil {
		t.Fatalf("CUE value has error: %v", err)
	}
}

func TestObjectAsCueValueInheritedIvars(t *testing.T) {
	vm := NewVM()

	// Parent class with 'name'
	personClass := vm.createClass("Person", vm.ObjectClass)
	personClass.InstVars = []string{"name"}
	personClass.NumSlots = 1

	// Child class with 'age' (inherits 'name')
	studentClass := vm.createClass("Student", personClass)
	studentClass.InstVars = []string{"age"}
	studentClass.NumSlots = 2

	obj := NewObject(studentClass.VTable, 2)
	obj.SetSlot(0, vm.registry.NewStringValue("Alice"))
	obj.SetSlot(1, FromSmallInt(20))

	cueObj := vm.objectAsCueValue(obj.ToValue())

	nameVal := cueObj.val.LookupPath(cue.ParsePath("name"))
	if !nameVal.Exists() {
		t.Fatal("should have inherited field 'name'")
	}
	s, err := nameVal.String()
	if err != nil {
		t.Fatalf("expected string for name: %v", err)
	}
	if s != "Alice" {
		t.Errorf("expected 'Alice', got %q", s)
	}

	ageVal := cueObj.val.LookupPath(cue.ParsePath("age"))
	if !ageVal.Exists() {
		t.Fatal("should have field 'age'")
	}
	n, err := ageVal.Int64()
	if err != nil {
		t.Fatalf("expected int for age: %v", err)
	}
	if n != 20 {
		t.Errorf("expected 20, got %d", n)
	}
}

func TestObjectAsCueValueNestedObject(t *testing.T) {
	vm := NewVM()

	// Inner class: Point with x, y
	pointClass := vm.createClass("Point", vm.ObjectClass)
	pointClass.InstVars = []string{"x", "y"}
	pointClass.NumSlots = 2

	// Outer class: Line with start, end
	lineClass := vm.createClass("Line", vm.ObjectClass)
	lineClass.InstVars = []string{"start", "end"}
	lineClass.NumSlots = 2

	p1 := NewObject(pointClass.VTable, 2)
	p1.SetSlot(0, FromSmallInt(1))
	p1.SetSlot(1, FromSmallInt(2))

	p2 := NewObject(pointClass.VTable, 2)
	p2.SetSlot(0, FromSmallInt(10))
	p2.SetSlot(1, FromSmallInt(20))

	line := NewObject(lineClass.VTable, 2)
	line.SetSlot(0, p1.ToValue())
	line.SetSlot(1, p2.ToValue())

	cueObj := vm.objectAsCueValue(line.ToValue())

	// start.x should be 1
	startX := cueObj.val.LookupPath(cue.ParsePath("start.x"))
	if !startX.Exists() {
		t.Fatal("should have nested field start.x")
	}
	n, err := startX.Int64()
	if err != nil {
		t.Fatalf("expected int for start.x: %v", err)
	}
	if n != 1 {
		t.Errorf("expected start.x=1, got %d", n)
	}

	// end.y should be 20
	endY := cueObj.val.LookupPath(cue.ParsePath("end.y"))
	if !endY.Exists() {
		t.Fatal("should have nested field end.y")
	}
	n, err = endY.Int64()
	if err != nil {
		t.Fatalf("expected int for end.y: %v", err)
	}
	if n != 20 {
		t.Errorf("expected end.y=20, got %d", n)
	}
}

func TestObjectAsCueValueNilSlots(t *testing.T) {
	vm := NewVM()

	pointClass := vm.createClass("Point", vm.ObjectClass)
	pointClass.InstVars = []string{"x", "y"}
	pointClass.NumSlots = 2

	// Create with default nil slots
	obj := NewObject(pointClass.VTable, 2)

	cueObj := vm.objectAsCueValue(obj.ToValue())
	if cueObj == nil {
		t.Fatal("objectAsCueValue returned nil")
	}
	if err := cueObj.val.Err(); err != nil {
		t.Fatalf("CUE value has error: %v", err)
	}

	// x should be null (nil maps to null in CUE)
	xVal := cueObj.val.LookupPath(cue.ParsePath("x"))
	if !xVal.Exists() {
		t.Fatal("should have field 'x' even if nil")
	}
}

// ---------------------------------------------------------------------------
// matchesObject Tests
// ---------------------------------------------------------------------------

func TestMatchesObjectSuccess(t *testing.T) {
	vm := NewVM()

	// Create Point class
	pointClass := vm.createClass("Point", vm.ObjectClass)
	pointClass.InstVars = []string{"x", "y"}
	pointClass.NumSlots = 2

	// Create instance x=3, y=7
	obj := NewObject(pointClass.VTable, 2)
	obj.SetSlot(0, FromSmallInt(3))
	obj.SetSlot(1, FromSmallInt(7))

	// Schema: x and y must be numbers
	ctx := cuecontext.New()
	schema := ctx.CompileString(`x: number, y: number`)
	if err := schema.Err(); err != nil {
		t.Fatalf("schema compile error: %v", err)
	}

	cv := &CueValueObject{val: schema}
	projection := vm.objectAsCueValue(obj.ToValue())
	unified := cv.val.Unify(projection.val)
	if unified.Err() != nil {
		t.Errorf("expected match, got error: %v", unified.Err())
	}
}

func TestMatchesObjectFailure(t *testing.T) {
	vm := NewVM()

	// Create Point class
	pointClass := vm.createClass("Point", vm.ObjectClass)
	pointClass.InstVars = []string{"x", "y"}
	pointClass.NumSlots = 2

	// Create instance x=3, y=7
	obj := NewObject(pointClass.VTable, 2)
	obj.SetSlot(0, FromSmallInt(3))
	obj.SetSlot(1, FromSmallInt(7))

	// Schema: x must be a string (should fail)
	ctx := cuecontext.New()
	schema := ctx.CompileString(`x: string, y: number`)
	if err := schema.Err(); err != nil {
		t.Fatalf("schema compile error: %v", err)
	}

	cv := &CueValueObject{val: schema}
	projection := vm.objectAsCueValue(obj.ToValue())
	unified := cv.val.Unify(projection.val)
	if unified.Err() == nil {
		t.Error("expected unification failure for string vs int")
	}
}

func TestMatchesObjectWithConstraint(t *testing.T) {
	vm := NewVM()

	// Create Point class
	pointClass := vm.createClass("Point", vm.ObjectClass)
	pointClass.InstVars = []string{"x", "y"}
	pointClass.NumSlots = 2

	// x=50, y=200
	obj := NewObject(pointClass.VTable, 2)
	obj.SetSlot(0, FromSmallInt(50))
	obj.SetSlot(1, FromSmallInt(200))

	ctx := cuecontext.New()

	// Schema: x > 0 & < 100 — should match
	schema1 := ctx.CompileString(`x: >0 & <100, y: number`)
	cv1 := &CueValueObject{val: schema1}
	p1 := vm.objectAsCueValue(obj.ToValue())
	u1 := cv1.val.Unify(p1.val)
	if u1.Err() != nil {
		t.Errorf("x=50 should satisfy >0 & <100: %v", u1.Err())
	}

	// Schema: x > 100 — should fail (x=50)
	schema2 := ctx.CompileString(`x: >100, y: number`)
	cv2 := &CueValueObject{val: schema2}
	p2 := vm.objectAsCueValue(obj.ToValue())
	u2 := cv2.val.Unify(p2.val)
	if u2.Err() == nil {
		t.Error("x=50 should not satisfy >100")
	}
}

// ---------------------------------------------------------------------------
// Helper to avoid importing cue in test file signatures
// ---------------------------------------------------------------------------

func cuePathParse(s string) cue.Path {
	return cue.ParsePath(s)
}
