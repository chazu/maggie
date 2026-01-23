package vm

import (
	"strings"
	"testing"
)

// ---------------------------------------------------------------------------
// Context Value Tests
// ---------------------------------------------------------------------------

func TestContextValueCreation(t *testing.T) {
	ctx := &ContextValue{
		Receiver: FromSmallInt(42),
		SenderID: -1,
		HomeID:   -1,
	}

	val := RegisterContext(ctx)

	if !val.IsContext() {
		t.Error("RegisterContext should return a context value")
	}

	retrieved := GetContextValue(val)
	if retrieved != ctx {
		t.Error("GetContextValue should return the original context")
	}
}

func TestContextValueIsBlockContext(t *testing.T) {
	// Method context
	methodCtx := &ContextValue{
		Method: &CompiledMethod{},
	}
	if methodCtx.IsBlockContext() {
		t.Error("Method context should not be a block context")
	}

	// Block context
	blockCtx := &ContextValue{
		Block: &BlockMethod{},
	}
	if !blockCtx.IsBlockContext() {
		t.Error("Block context should be a block context")
	}
}

func TestContextRegistry(t *testing.T) {
	// Clear registry for clean test
	globalContextRegistry.Clear()

	ctx1 := &ContextValue{Receiver: FromSmallInt(1)}
	ctx2 := &ContextValue{Receiver: FromSmallInt(2)}

	id1 := globalContextRegistry.Register(ctx1)
	id2 := globalContextRegistry.Register(ctx2)

	if id1 == id2 {
		t.Error("Different contexts should have different IDs")
	}

	if globalContextRegistry.Size() != 2 {
		t.Errorf("Registry size should be 2, got %d", globalContextRegistry.Size())
	}

	// Retrieve
	if globalContextRegistry.Get(id1) != ctx1 {
		t.Error("Should retrieve ctx1 by id1")
	}
	if globalContextRegistry.Get(id2) != ctx2 {
		t.Error("Should retrieve ctx2 by id2")
	}

	// Remove
	globalContextRegistry.Remove(id1)
	if globalContextRegistry.Get(id1) != nil {
		t.Error("Removed context should return nil")
	}
	if globalContextRegistry.Size() != 1 {
		t.Errorf("Registry size should be 1 after removal, got %d", globalContextRegistry.Size())
	}
}

// ---------------------------------------------------------------------------
// Interpreter thisContext Tests
// ---------------------------------------------------------------------------

func TestInterpreterPushContext(t *testing.T) {
	vm := NewVM()

	// Create a method that pushes thisContext and returns it
	class := vm.createClass("ContextTest", vm.ObjectClass)
	builder := NewCompiledMethodBuilder("getContext", 0)
	builder.Bytecode().Emit(OpPushContext)
	builder.Bytecode().Emit(OpReturnTop)
	method := builder.Build()
	method.SetClass(class)

	selID := vm.Selectors.Intern("getContext")
	class.VTable.AddMethod(selID, method)

	// Create an instance and call the method
	obj := NewObject(class.VTable, 0)
	result := vm.Send(obj.ToValue(), "getContext", nil)

	if !result.IsContext() {
		t.Fatal("getContext should return a context value")
	}

	ctx := GetContextValue(result)
	if ctx == nil {
		t.Fatal("Should be able to get context value")
	}

	if ctx.Method != method {
		t.Error("Context should have correct method")
	}
}

func TestContextReceiver(t *testing.T) {
	vm := NewVM()

	class := vm.createClass("ReceiverTest", vm.ObjectClass)
	builder := NewCompiledMethodBuilder("testReceiver", 0)
	builder.Bytecode().Emit(OpPushContext)
	// Call receiver on the context
	receiverSelID := vm.Selectors.Intern("receiver")
	builder.Bytecode().EmitSend(OpSend, uint16(receiverSelID), 0)
	builder.Bytecode().Emit(OpReturnTop)
	method := builder.Build()
	method.SetClass(class)

	selID := vm.Selectors.Intern("testReceiver")
	class.VTable.AddMethod(selID, method)

	obj := NewObject(class.VTable, 0)
	objValue := obj.ToValue()
	result := vm.Send(objValue, "testReceiver", nil)

	// The receiver should be the original object
	if result != objValue {
		t.Error("Context receiver should be self")
	}
}

func TestContextIsBlockContext(t *testing.T) {
	vm := NewVM()

	class := vm.createClass("BlockContextTest", vm.ObjectClass)

	// Method that gets thisContext and checks if it's a block context
	builder := NewCompiledMethodBuilder("isBlockInMethod", 0)
	builder.Bytecode().Emit(OpPushContext)
	isBlockSelID := vm.Selectors.Intern("isBlockContext")
	builder.Bytecode().EmitSend(OpSend, uint16(isBlockSelID), 0)
	builder.Bytecode().Emit(OpReturnTop)
	method := builder.Build()
	method.SetClass(class)

	selID := vm.Selectors.Intern("isBlockInMethod")
	class.VTable.AddMethod(selID, method)

	obj := NewObject(class.VTable, 0)
	result := vm.Send(obj.ToValue(), "isBlockInMethod", nil)

	if result != False {
		t.Error("Method context isBlockContext should return false")
	}
}

func TestContextArguments(t *testing.T) {
	vm := NewVM()

	class := vm.createClass("ArgsTest", vm.ObjectClass)

	// Method that takes args and returns thisContext arguments
	builder := NewCompiledMethodBuilder("argsOf:", 1)
	builder.SetNumTemps(1)
	builder.Bytecode().Emit(OpPushContext)
	argsSelID := vm.Selectors.Intern("arguments")
	builder.Bytecode().EmitSend(OpSend, uint16(argsSelID), 0)
	builder.Bytecode().Emit(OpReturnTop)
	method := builder.Build()
	method.SetClass(class)

	selID := vm.Selectors.Intern("argsOf:")
	class.VTable.AddMethod(selID, method)

	obj := NewObject(class.VTable, 0)
	argValue := FromSmallInt(42)
	result := vm.Send(obj.ToValue(), "argsOf:", []Value{argValue})

	// Result should be an array
	if result == Nil {
		t.Fatal("arguments should return an array, not nil")
	}

	// Check array contents
	argObj := ObjectFromValue(result)
	if argObj == nil || argObj.NumSlots() < 1 {
		t.Fatal("arguments array should have at least one element")
	}

	if argObj.GetSlot(0) != argValue {
		t.Errorf("First argument should be %v, got %v", argValue, argObj.GetSlot(0))
	}
}

func TestContextSelector(t *testing.T) {
	vm := NewVM()

	class := vm.createClass("SelectorTest", vm.ObjectClass)

	builder := NewCompiledMethodBuilder("getSelector", 0)
	builder.Bytecode().Emit(OpPushContext)
	selectorSelID := vm.Selectors.Intern("selector")
	builder.Bytecode().EmitSend(OpSend, uint16(selectorSelID), 0)
	builder.Bytecode().Emit(OpReturnTop)
	method := builder.Build()
	method.SetClass(class)

	selID := vm.Selectors.Intern("getSelector")
	class.VTable.AddMethod(selID, method)

	obj := NewObject(class.VTable, 0)
	result := vm.Send(obj.ToValue(), "getSelector", nil)

	if !result.IsSymbol() {
		t.Fatal("selector should return a symbol")
	}

	name := vm.Symbols.Name(result.SymbolID())
	if name != "getSelector" {
		t.Errorf("Selector should be 'getSelector', got '%s'", name)
	}
}

func TestContextSender(t *testing.T) {
	vm := NewVM()

	class := vm.createClass("SenderTest", vm.ObjectClass)

	// Inner method that returns sender's selector
	innerBuilder := NewCompiledMethodBuilder("inner", 0)
	innerBuilder.Bytecode().Emit(OpPushContext)
	senderSelID := vm.Selectors.Intern("sender")
	innerBuilder.Bytecode().EmitSend(OpSend, uint16(senderSelID), 0)
	innerBuilder.Bytecode().Emit(OpReturnTop)
	innerMethod := innerBuilder.Build()
	innerMethod.SetClass(class)

	innerSelID := vm.Selectors.Intern("inner")
	class.VTable.AddMethod(innerSelID, innerMethod)

	// Outer method that calls inner
	outerBuilder := NewCompiledMethodBuilder("outer", 0)
	outerBuilder.Bytecode().Emit(OpPushSelf)
	outerBuilder.Bytecode().EmitSend(OpSend, uint16(innerSelID), 0)
	outerBuilder.Bytecode().Emit(OpReturnTop)
	outerMethod := outerBuilder.Build()
	outerMethod.SetClass(class)

	outerSelID := vm.Selectors.Intern("outer")
	class.VTable.AddMethod(outerSelID, outerMethod)

	obj := NewObject(class.VTable, 0)
	result := vm.Send(obj.ToValue(), "outer", nil)

	// Result should be a context (the sender)
	if !result.IsContext() {
		t.Fatal("sender should return a context")
	}

	senderCtx := GetContextValue(result)
	if senderCtx == nil {
		t.Fatal("Should get sender context")
	}

	// Sender should be the 'outer' method
	if senderCtx.Method == nil || senderCtx.Method.Name() != "outer" {
		t.Error("Sender context should be 'outer' method")
	}
}

func TestContextPrintString(t *testing.T) {
	vm := NewVM()

	class := vm.createClass("PrintTest", vm.ObjectClass)

	builder := NewCompiledMethodBuilder("contextString", 0)
	builder.Bytecode().Emit(OpPushContext)
	printStringSelID := vm.Selectors.Intern("printString")
	builder.Bytecode().EmitSend(OpSend, uint16(printStringSelID), 0)
	builder.Bytecode().Emit(OpReturnTop)
	method := builder.Build()
	method.SetClass(class)

	selID := vm.Selectors.Intern("contextString")
	class.VTable.AddMethod(selID, method)

	obj := NewObject(class.VTable, 0)
	result := vm.Send(obj.ToValue(), "contextString", nil)

	if !IsStringValue(result) {
		t.Fatal("printString should return a string")
	}

	str := GetStringContent(result)
	if !strings.Contains(str, "MethodContext") {
		t.Errorf("printString should mention MethodContext, got: %s", str)
	}
	if !strings.Contains(str, "PrintTest") {
		t.Errorf("printString should mention class name, got: %s", str)
	}
}

// ---------------------------------------------------------------------------
// AOT thisContext Tests
// ---------------------------------------------------------------------------

func TestAOTCompilerPushContext(t *testing.T) {
	selectors := NewSelectorTable()
	symbols := NewSymbolTable()

	// Method that uses thisContext
	builder := NewCompiledMethodBuilder("withContext", 0)
	builder.Bytecode().Emit(OpPushContext)
	builder.Bytecode().Emit(OpReturnTop)
	method := builder.Build()

	aot := NewAOTCompiler(selectors, symbols)
	code := aot.CompileMethod(method, "Test", "withContext")

	t.Logf("Generated AOT code with context:\n%s", code)

	// Verify it creates a context
	if !strings.Contains(code, "ContextValue") {
		t.Error("AOT code should create ContextValue")
	}
	if !strings.Contains(code, "RegisterContext") {
		t.Error("AOT code should call RegisterContext")
	}
	if !strings.Contains(code, "Receiver: self") {
		t.Error("AOT code should set Receiver to self")
	}
}

func TestAOTCompilerBlockPushContext(t *testing.T) {
	selectors := NewSelectorTable()
	symbols := NewSymbolTable()

	// Block that uses thisContext
	block := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 1,
		Bytecode: []byte{
			byte(OpPushContext),
			byte(OpReturnTop),
		},
	}

	aot := NewAOTCompiler(selectors, symbols)
	code := aot.CompileBlock(block, "Test", "example", 0)

	t.Logf("Generated AOT block code with context:\n%s", code)

	// Verify block context includes captures
	if !strings.Contains(code, "ContextValue") {
		t.Error("AOT block code should create ContextValue")
	}
	if !strings.Contains(code, "Captures: captures") {
		t.Error("AOT block code should set Captures")
	}
}

// ---------------------------------------------------------------------------
// JIT thisContext Tests
// ---------------------------------------------------------------------------

func TestJITCompiledMethodWithContext(t *testing.T) {
	vm := NewVM()
	jit := vm.EnableJIT()
	jit.LogCompilation = false

	class := vm.createClass("JITContextTest", vm.ObjectClass)

	builder := NewCompiledMethodBuilder("jitContext", 0)
	builder.Bytecode().Emit(OpPushContext)
	builder.Bytecode().Emit(OpReturnTop)
	method := builder.Build()
	method.SetClass(class)

	// Compile with JIT
	jit.compileMethod(method)

	code := jit.GetCompiledMethod("JITContextTest", "jitContext")
	if code == "" {
		t.Fatal("Method should be compiled")
	}

	if !strings.Contains(code, "ContextValue") {
		t.Error("JIT code should include ContextValue creation")
	}
}

// ---------------------------------------------------------------------------
// Context Class Tests
// ---------------------------------------------------------------------------

func TestContextClassRegistered(t *testing.T) {
	vm := NewVM()

	if vm.ContextClass == nil {
		t.Fatal("ContextClass should be registered")
	}

	if vm.ContextClass.Name != "Context" {
		t.Errorf("ContextClass name should be 'Context', got '%s'", vm.ContextClass.Name)
	}

	// Check it's in globals
	global := vm.Globals["Context"]
	if global == Nil {
		t.Error("Context should be in globals")
	}
}

func TestContextClassForValue(t *testing.T) {
	vm := NewVM()

	ctx := &ContextValue{Receiver: Nil}
	val := RegisterContext(ctx)

	class := vm.ClassFor(val)
	if class != vm.ContextClass {
		t.Error("ClassFor context value should return ContextClass")
	}
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

func BenchmarkPushContext(b *testing.B) {
	vm := NewVM()

	class := vm.createClass("BenchContext", vm.ObjectClass)
	builder := NewCompiledMethodBuilder("bench", 0)
	builder.Bytecode().Emit(OpPushContext)
	builder.Bytecode().Emit(OpPOP) // discard it
	builder.Bytecode().Emit(OpReturnNil)
	method := builder.Build()
	method.SetClass(class)

	selID := vm.Selectors.Intern("bench")
	class.VTable.AddMethod(selID, method)

	obj := NewObject(class.VTable, 0)
	objVal := obj.ToValue()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(objVal, "bench", nil)
	}
}

func BenchmarkContextRegistration(b *testing.B) {
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		ctx := &ContextValue{Receiver: FromSmallInt(int64(i))}
		val := RegisterContext(ctx)
		UnregisterContext(val)
	}
}
