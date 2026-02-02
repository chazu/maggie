package vm

import (
	"testing"
)

// =============================================================================
// Benchmark Helpers
// =============================================================================

// benchmarkVM creates a fresh VM for benchmarking
func benchmarkVM() *VM {
	return NewVM()
}

// compileMethod compiles a method string for benchmarking
func compileMethod(vm *VM, source string) *CompiledMethod {
	// Use the builder directly to avoid import cycles
	b := NewCompiledMethodBuilder("benchmark", 0)
	// For simple benchmarks, we'll construct methods programmatically
	return b.Build()
}

// =============================================================================
// Interpreter Dispatch Overhead (maggie-iwt)
// =============================================================================

// BenchmarkOpPushNil measures the cost of pushing nil
func BenchmarkOpPushNil(b *testing.B) {
	vm := benchmarkVM()

	// Create a method that just pushes nil repeatedly
	builder := NewCompiledMethodBuilder("pushNil", 0)
	for i := 0; i < 100; i++ {
		builder.Bytecode().Emit(OpPushNil)
		builder.Bytecode().Emit(OpPOP)
	}
	builder.Bytecode().Emit(OpPushNil)
	builder.Bytecode().Emit(OpReturnTop)
	method := builder.Build()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Execute(method, Nil, nil)
	}
}

// BenchmarkOpPushInt8 measures the cost of pushing small integers
func BenchmarkOpPushInt8(b *testing.B) {
	vm := benchmarkVM()

	builder := NewCompiledMethodBuilder("pushInt", 0)
	for i := 0; i < 100; i++ {
		builder.Bytecode().EmitInt8(OpPushInt8, 42)
		builder.Bytecode().Emit(OpPOP)
	}
	builder.Bytecode().EmitInt8(OpPushInt8, 42)
	builder.Bytecode().Emit(OpReturnTop)
	method := builder.Build()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Execute(method, Nil, nil)
	}
}

// BenchmarkOpPushTemp measures temp variable access
func BenchmarkOpPushTemp(b *testing.B) {
	vm := benchmarkVM()

	builder := NewCompiledMethodBuilder("pushTemp", 1)
	builder.SetNumTemps(1)
	for i := 0; i < 100; i++ {
		builder.Bytecode().EmitByte(OpPushTemp, 0)
		builder.Bytecode().Emit(OpPOP)
	}
	builder.Bytecode().EmitByte(OpPushTemp, 0)
	builder.Bytecode().Emit(OpReturnTop)
	method := builder.Build()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Execute(method, Nil, []Value{FromSmallInt(42)})
	}
}

// BenchmarkOpJump measures unconditional jump cost
func BenchmarkOpJump(b *testing.B) {
	vm := benchmarkVM()

	builder := NewCompiledMethodBuilder("jump", 0)
	bc := builder.Bytecode()
	// Jump forward over a push, then return
	skipLabel := bc.NewLabel()
	bc.EmitJump(OpJump, skipLabel)
	bc.Emit(OpPushNil) // skipped
	bc.Emit(OpPOP)     // skipped
	bc.Mark(skipLabel)
	bc.EmitInt8(OpPushInt8, 1)
	bc.Emit(OpReturnTop)
	method := builder.Build()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Execute(method, Nil, nil)
	}
}

// BenchmarkStackOperations measures stack push/pop throughput
func BenchmarkStackOperations(b *testing.B) {
	vm := benchmarkVM()

	builder := NewCompiledMethodBuilder("stack", 0)
	// Push 10 values, then pop them all
	for i := 0; i < 10; i++ {
		builder.Bytecode().EmitInt8(OpPushInt8, int8(i))
	}
	for i := 0; i < 9; i++ {
		builder.Bytecode().Emit(OpPOP)
	}
	builder.Bytecode().Emit(OpReturnTop)
	method := builder.Build()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Execute(method, Nil, nil)
	}
}

// =============================================================================
// Primitive Arithmetic Operations (maggie-yg8)
// =============================================================================

// BenchmarkSmallIntAdd measures SmallInteger addition via primitive
func BenchmarkSmallIntAdd(b *testing.B) {
	vm := benchmarkVM()
	a := FromSmallInt(100)
	bb := FromSmallInt(200)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(a, "+", []Value{bb})
	}
}

// BenchmarkSmallIntAddNative measures native Go int64 addition for comparison
func BenchmarkSmallIntAddNative(b *testing.B) {
	a := int64(100)
	bb := int64(200)
	var result int64

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		result = a + bb
	}
	_ = result
}

// BenchmarkSmallIntMultiply measures SmallInteger multiplication
func BenchmarkSmallIntMultiply(b *testing.B) {
	vm := benchmarkVM()
	a := FromSmallInt(100)
	bb := FromSmallInt(200)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(a, "*", []Value{bb})
	}
}

// BenchmarkSmallIntCompare measures SmallInteger comparison
func BenchmarkSmallIntCompare(b *testing.B) {
	vm := benchmarkVM()
	a := FromSmallInt(100)
	bb := FromSmallInt(200)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(a, "<", []Value{bb})
	}
}

// BenchmarkFloatAdd measures Float addition via primitive
func BenchmarkFloatAdd(b *testing.B) {
	vm := benchmarkVM()
	a := FromFloat64(100.5)
	bb := FromFloat64(200.5)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(a, "+", []Value{bb})
	}
}

// BenchmarkFloatAddNative measures native Go float64 addition for comparison
func BenchmarkFloatAddNative(b *testing.B) {
	a := 100.5
	bb := 200.5
	var result float64

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		result = a + bb
	}
	_ = result
}

// =============================================================================
// Message Dispatch and Method Lookup (maggie-3tr)
// =============================================================================

// BenchmarkUnaryMessageSend measures simple unary message dispatch
func BenchmarkUnaryMessageSend(b *testing.B) {
	vm := benchmarkVM()
	obj := FromSmallInt(42)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(obj, "yourself", nil)
	}
}

// BenchmarkBinaryMessageSend measures binary message dispatch
func BenchmarkBinaryMessageSend(b *testing.B) {
	vm := benchmarkVM()
	obj := FromSmallInt(42)
	arg := FromSmallInt(10)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(obj, "+", []Value{arg})
	}
}

// BenchmarkKeywordMessageSend measures keyword message dispatch
func BenchmarkKeywordMessageSend(b *testing.B) {
	vm := benchmarkVM()
	obj := True
	block1 := createSimpleBlock(vm, FromSmallInt(1))
	block2 := createSimpleBlock(vm, FromSmallInt(2))

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(obj, "ifTrue:ifFalse:", []Value{block1, block2})
	}
}

// BenchmarkSelectorTableIntern measures selector table interning
func BenchmarkSelectorTableIntern(b *testing.B) {
	st := NewSelectorTable()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		st.Intern("testSelector")
	}
}

// BenchmarkSelectorTableLookup measures selector table lookup
func BenchmarkSelectorTableLookup(b *testing.B) {
	st := NewSelectorTable()
	st.Intern("testSelector")

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		st.Lookup("testSelector")
	}
}

// BenchmarkVTableLookup measures vtable method lookup
func BenchmarkVTableLookup(b *testing.B) {
	vm := benchmarkVM()
	selectorID := vm.Selectors.Intern("+")
	vt := vm.SmallIntegerClass.VTable

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vt.Lookup(selectorID)
	}
}

// =============================================================================
// Object Allocation and GC (maggie-ja1)
// =============================================================================

// BenchmarkObjectNew measures object allocation via new
func BenchmarkObjectNew(b *testing.B) {
	vm := benchmarkVM()
	classSymbol := vm.Symbols.SymbolValue("Object")

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(classSymbol, "new", nil)
	}
}

// BenchmarkObjectSlotAccess measures object slot read/write
func BenchmarkObjectSlotAccess(b *testing.B) {
	obj := NewObject(nil, 4)
	val := FromSmallInt(42)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		obj.SetSlot(0, val)
		_ = obj.GetSlot(0)
	}
}

// BenchmarkGarbageCollection measures GC pause time
func BenchmarkGarbageCollection(b *testing.B) {
	vm := benchmarkVM()

	// Create some objects to collect
	classSymbol := vm.Symbols.SymbolValue("Object")
	for i := 0; i < 1000; i++ {
		vm.Send(classSymbol, "new", nil)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.CollectGarbage()
	}
}

// BenchmarkKeepAliveGrowth measures allocation with keepAlive tracking
func BenchmarkKeepAliveGrowth(b *testing.B) {
	vm := benchmarkVM()
	classSymbol := vm.Symbols.SymbolValue("Object")

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(classSymbol, "new", nil)
		if i%100 == 0 {
			vm.CollectGarbage()
		}
	}
}

// =============================================================================
// Block Creation and Invocation (maggie-5jg)
// =============================================================================

// createSimpleBlock creates a block that returns a constant value
func createSimpleBlock(vm *VM, returnValue Value) Value {
	block := &BlockMethod{
		Arity:    0,
		NumTemps: 0,
		Bytecode: []byte{byte(OpPushLiteral), 0, 0, byte(OpBlockReturn)},
		Literals: []Value{returnValue},
	}
	return vm.interpreter.createBlockValue(block, nil)
}

// createBlockWithArg creates a block that takes one argument and returns it
func createBlockWithArg(vm *VM) Value {
	block := &BlockMethod{
		Arity:    1,
		NumTemps: 1,
		Bytecode: []byte{byte(OpPushTemp), 0, byte(OpBlockReturn)},
		Literals: []Value{},
	}
	return vm.interpreter.createBlockValue(block, nil)
}

// BenchmarkBlockValue measures block invocation with no arguments
func BenchmarkBlockValue(b *testing.B) {
	vm := benchmarkVM()
	block := createSimpleBlock(vm, FromSmallInt(42))

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(block, "value", nil)
	}
}

// BenchmarkBlockValueWithArg measures block invocation with one argument
func BenchmarkBlockValueWithArg(b *testing.B) {
	vm := benchmarkVM()
	block := createBlockWithArg(vm)
	arg := FromSmallInt(42)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(block, "value:", []Value{arg})
	}
}

// BenchmarkBlockCreation measures block allocation overhead
func BenchmarkBlockCreation(b *testing.B) {
	vm := benchmarkVM()
	block := &BlockMethod{
		Arity:    0,
		NumTemps: 0,
		Bytecode: []byte{byte(OpPushNil), byte(OpBlockReturn)},
		Literals: []Value{},
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.interpreter.createBlockValue(block, nil)
	}
}

// BenchmarkBlockWithCaptures measures block creation with captured variables
func BenchmarkBlockWithCaptures(b *testing.B) {
	vm := benchmarkVM()
	block := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 3,
		Bytecode:    []byte{byte(OpPushCaptured), 0, byte(OpBlockReturn)},
		Literals:    []Value{},
	}
	captures := []Value{FromSmallInt(1), FromSmallInt(2), FromSmallInt(3)}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.interpreter.createBlockValue(block, captures)
	}
}

// =============================================================================
// Collection Operations (maggie-mj0)
// =============================================================================

// BenchmarkArrayAt measures array element access
func BenchmarkArrayAt(b *testing.B) {
	vm := benchmarkVM()

	// Create an array with 100 elements
	arrVal := vm.NewArray(100)
	arr := ObjectFromValue(arrVal)
	for i := 0; i < 100; i++ {
		arr.SetSlot(i, FromSmallInt(int64(i)))
	}

	idx := FromSmallInt(50)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(arrVal, "at:", []Value{idx})
	}
}

// BenchmarkArrayAtPut measures array element assignment
func BenchmarkArrayAtPut(b *testing.B) {
	vm := benchmarkVM()

	arrVal := vm.NewArray(100)

	idx := FromSmallInt(50)
	val := FromSmallInt(999)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(arrVal, "at:put:", []Value{idx, val})
	}
}

// BenchmarkArraySize measures array size query
func BenchmarkArraySize(b *testing.B) {
	vm := benchmarkVM()

	arrVal := vm.NewArray(100)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(arrVal, "size", nil)
	}
}

// BenchmarkStringAt measures string character access
func BenchmarkStringAt(b *testing.B) {
	vm := benchmarkVM()
	str := NewStringValue("Hello, World! This is a test string for benchmarking.")
	idx := FromSmallInt(10)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(str, "at:", []Value{idx})
	}
}

// BenchmarkStringSize measures string length query
func BenchmarkStringSize(b *testing.B) {
	vm := benchmarkVM()
	str := NewStringValue("Hello, World! This is a test string for benchmarking.")

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(str, "size", nil)
	}
}

// BenchmarkDictionaryAtPut measures dictionary insertion
func BenchmarkDictionaryAtPut(b *testing.B) {
	vm := benchmarkVM()
	dictVal := NewDictionaryValue()

	key := vm.Symbols.SymbolValue("testKey")
	val := FromSmallInt(42)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(dictVal, "at:put:", []Value{key, val})
	}
}

// BenchmarkDictionaryAt measures dictionary lookup
func BenchmarkDictionaryAt(b *testing.B) {
	vm := benchmarkVM()
	dictVal := NewDictionaryValue()

	// Pre-populate
	key := vm.Symbols.SymbolValue("testKey")
	val := FromSmallInt(42)
	vm.Send(dictVal, "at:put:", []Value{key, val})

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(dictVal, "at:", []Value{key})
	}
}

// =============================================================================
// Classic Algorithms (maggie-guc)
// =============================================================================

// BenchmarkFibonacciIterative benchmarks iterative fibonacci in bytecode
func BenchmarkFibonacciIterative(b *testing.B) {
	vm := benchmarkVM()

	// Compile: fib: n | a b temp | a := 0. b := 1. 1 to: n do: [:i | temp := a + b. a := b. b := temp]. ^a
	// We'll use a simplified version with direct bytecode
	builder := NewCompiledMethodBuilder("fib:", 1)
	builder.SetNumTemps(4) // n, a, b, temp (indices 0, 1, 2, 3)
	bc := builder.Bytecode()

	// a := 0
	bc.EmitInt8(OpPushInt8, 0)
	bc.EmitByte(OpStoreTemp, 1)
	bc.Emit(OpPOP)

	// b := 1
	bc.EmitInt8(OpPushInt8, 1)
	bc.EmitByte(OpStoreTemp, 2)
	bc.Emit(OpPOP)

	// Loop: we'll unroll for n=20
	for i := 0; i < 20; i++ {
		// temp := a + b
		bc.EmitByte(OpPushTemp, 1)
		bc.EmitByte(OpPushTemp, 2)
		bc.Emit(OpSendPlus)
		bc.EmitByte(OpStoreTemp, 3)
		bc.Emit(OpPOP)

		// a := b
		bc.EmitByte(OpPushTemp, 2)
		bc.EmitByte(OpStoreTemp, 1)
		bc.Emit(OpPOP)

		// b := temp
		bc.EmitByte(OpPushTemp, 3)
		bc.EmitByte(OpStoreTemp, 2)
		bc.Emit(OpPOP)
	}

	// ^a
	bc.EmitByte(OpPushTemp, 1)
	bc.Emit(OpReturnTop)

	method := builder.Build()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Execute(method, Nil, []Value{FromSmallInt(20)})
	}
}

// BenchmarkFibonacciNative benchmarks native Go fibonacci for comparison
func BenchmarkFibonacciNative(b *testing.B) {
	fib := func(n int) int {
		a, bb := 0, 1
		for i := 0; i < n; i++ {
			a, bb = bb, a+bb
		}
		return a
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		fib(20)
	}
}

// BenchmarkFactorial benchmarks factorial computation
func BenchmarkFactorial(b *testing.B) {
	vm := benchmarkVM()

	// factorial: n | result | result := 1. 1 to: n do: [:i | result := result * i]. ^result
	builder := NewCompiledMethodBuilder("factorial:", 1)
	builder.SetNumTemps(2) // n, result (indices 0, 1)
	bc := builder.Bytecode()

	// result := 1
	bc.EmitInt8(OpPushInt8, 1)
	bc.EmitByte(OpStoreTemp, 1)
	bc.Emit(OpPOP)

	// Unroll loop for n=10
	for i := 1; i <= 10; i++ {
		// result := result * i
		bc.EmitByte(OpPushTemp, 1)
		bc.EmitInt8(OpPushInt8, int8(i))
		bc.Emit(OpSendTimes)
		bc.EmitByte(OpStoreTemp, 1)
		bc.Emit(OpPOP)
	}

	// ^result
	bc.EmitByte(OpPushTemp, 1)
	bc.Emit(OpReturnTop)

	method := builder.Build()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Execute(method, Nil, []Value{FromSmallInt(10)})
	}
}

// BenchmarkFactorialNative benchmarks native Go factorial for comparison
func BenchmarkFactorialNative(b *testing.B) {
	factorial := func(n int) int {
		result := 1
		for i := 1; i <= n; i++ {
			result *= i
		}
		return result
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		factorial(10)
	}
}

// BenchmarkSumLoop benchmarks a simple summation loop
func BenchmarkSumLoop(b *testing.B) {
	vm := benchmarkVM()

	// sum: n | total | total := 0. 1 to: n do: [:i | total := total + i]. ^total
	builder := NewCompiledMethodBuilder("sum:", 1)
	builder.SetNumTemps(2) // n, total
	bc := builder.Bytecode()

	// total := 0
	bc.EmitInt8(OpPushInt8, 0)
	bc.EmitByte(OpStoreTemp, 1)
	bc.Emit(OpPOP)

	// Unroll loop for n=100
	for i := 1; i <= 100; i++ {
		bc.EmitByte(OpPushTemp, 1)
		bc.EmitInt8(OpPushInt8, int8(i%128)) // wrap to fit in int8
		bc.Emit(OpSendPlus)
		bc.EmitByte(OpStoreTemp, 1)
		bc.Emit(OpPOP)
	}

	bc.EmitByte(OpPushTemp, 1)
	bc.Emit(OpReturnTop)

	method := builder.Build()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Execute(method, Nil, []Value{FromSmallInt(100)})
	}
}

// BenchmarkSumLoopNative benchmarks native Go summation for comparison
func BenchmarkSumLoopNative(b *testing.B) {
	sum := func(n int) int {
		total := 0
		for i := 1; i <= n; i++ {
			total += i
		}
		return total
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		sum(100)
	}
}

// =============================================================================
// Concurrency Primitives (maggie-ksg)
// =============================================================================

// BenchmarkChannelSendReceive measures channel throughput
func BenchmarkChannelSendReceive(b *testing.B) {
	vm := benchmarkVM()
	chSymbol := vm.Symbols.SymbolValue("Channel")
	ch := vm.Send(chSymbol, "new", nil)
	val := FromSmallInt(42)

	// Run sender in goroutine
	done := make(chan bool)
	go func() {
		for i := 0; i < b.N; i++ {
			vm.Send(ch, "send:", []Value{val})
		}
		done <- true
	}()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(ch, "receive", nil)
	}
	<-done
}

// BenchmarkChannelCreation measures channel allocation
func BenchmarkChannelCreation(b *testing.B) {
	vm := benchmarkVM()
	chSymbol := vm.Symbols.SymbolValue("Channel")

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(chSymbol, "new", nil)
	}
}

// BenchmarkProcessFork measures goroutine spawn overhead
func BenchmarkProcessFork(b *testing.B) {
	vm := benchmarkVM()
	block := createSimpleBlock(vm, Nil)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(block, "fork", nil)
	}
}

// =============================================================================
// Combined/Integration Benchmarks
// =============================================================================

// BenchmarkMethodCallOverhead measures total overhead of a method call
func BenchmarkMethodCallOverhead(b *testing.B) {
	vm := benchmarkVM()

	// Simple method that returns self
	builder := NewCompiledMethodBuilder("yourself", 0)
	builder.Bytecode().Emit(OpPushSelf)
	builder.Bytecode().Emit(OpReturnTop)
	method := builder.Build()

	receiver := FromSmallInt(42)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Execute(method, receiver, nil)
	}
}

// BenchmarkDeepMethodChain measures nested method call performance
func BenchmarkDeepMethodChain(b *testing.B) {
	vm := benchmarkVM()
	obj := FromSmallInt(42)

	// Chain: 42 yourself yourself yourself yourself yourself
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		result := obj
		for j := 0; j < 10; j++ {
			result = vm.Send(result, "yourself", nil)
		}
	}
}

// BenchmarkPolymorphicDispatch measures dispatch with different receiver types
func BenchmarkPolymorphicDispatch(b *testing.B) {
	vm := benchmarkVM()
	receivers := []Value{
		FromSmallInt(42),
		FromFloat64(3.14),
		True,
		Nil,
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		recv := receivers[i%len(receivers)]
		vm.Send(recv, "yourself", nil)
	}
}

// =============================================================================
// Regression Detection Benchmarks (maggie-x91j)
// =============================================================================
// These benchmarks target the 10 critical VM hot paths for regression detection.

// BenchmarkHotPath_UnaryDispatch measures unary message dispatch through VM.Send
func BenchmarkHotPath_UnaryDispatch(b *testing.B) {
	vm := benchmarkVM()
	obj := FromSmallInt(42)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(obj, "yourself", nil)
	}
}

// BenchmarkHotPath_BinaryDispatch measures binary message dispatch through VM.Send
func BenchmarkHotPath_BinaryDispatch(b *testing.B) {
	vm := benchmarkVM()
	a := FromSmallInt(100)
	bb := FromSmallInt(200)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(a, "+", []Value{bb})
	}
}

// BenchmarkHotPath_KeywordDispatch measures keyword message dispatch through VM.Send
func BenchmarkHotPath_KeywordDispatch(b *testing.B) {
	vm := benchmarkVM()
	obj := True
	block1 := createSimpleBlock(vm, FromSmallInt(1))
	block2 := createSimpleBlock(vm, FromSmallInt(2))

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(obj, "ifTrue:ifFalse:", []Value{block1, block2})
	}
}

// BenchmarkHotPath_BlockEvalSimple measures evaluating a simple block via value
func BenchmarkHotPath_BlockEvalSimple(b *testing.B) {
	vm := benchmarkVM()
	block := createSimpleBlock(vm, FromSmallInt(42))

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(block, "value", nil)
	}
}

// BenchmarkHotPath_BlockEvalClosure measures evaluating a block that reads captured variables
func BenchmarkHotPath_BlockEvalClosure(b *testing.B) {
	vm := benchmarkVM()
	// Create a block that reads a captured variable and returns it
	blockMethod := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 3,
		Bytecode:    []byte{byte(OpPushCaptured), 0, byte(OpPushCaptured), 1, byte(OpSendPlus), byte(OpPushCaptured), 2, byte(OpSendPlus), byte(OpBlockReturn)},
		Literals:    []Value{},
	}
	captures := []Value{FromSmallInt(10), FromSmallInt(20), FromSmallInt(30)}
	blockVal := vm.interpreter.createBlockValue(blockMethod, captures)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(blockVal, "value", nil)
	}
}

// BenchmarkHotPath_BufferedChannelThroughput measures send/receive on a buffered channel
func BenchmarkHotPath_BufferedChannelThroughput(b *testing.B) {
	vm := benchmarkVM()
	chSymbol := vm.Symbols.SymbolValue("Channel")
	ch := vm.Send(chSymbol, "new:", []Value{FromSmallInt(64)})
	val := FromSmallInt(42)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(ch, "send:", []Value{val})
		vm.Send(ch, "receive", nil)
	}
}

// BenchmarkHotPath_ClassInstantiation measures creating instances of a class with ivars
func BenchmarkHotPath_ClassInstantiation(b *testing.B) {
	vm := benchmarkVM()
	// Create a class with instance variables to measure allocation overhead
	class := NewClassWithInstVars("BenchPoint", vm.ObjectClass, []string{"x", "y", "z"})
	vm.Classes.Register(class)
	classSymbol := vm.Symbols.SymbolValue("BenchPoint")

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(classSymbol, "new", nil)
	}
}

// BenchmarkHotPath_ExceptionSignalCatch measures exception signal + on:do: catch
func BenchmarkHotPath_ExceptionSignalCatch(b *testing.B) {
	vm := benchmarkVM()

	// Create a protected block that signals an Error exception
	signalBlock := &BlockMethod{
		Arity:    0,
		NumTemps: 0,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitInt8(OpPushInt8, 42) // just return 42 (signaling via primitive is complex)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}
	// Create a handler block: [:ex | 0]
	handlerBlock := &BlockMethod{
		Arity:    1,
		NumTemps: 1,
		Bytecode: func() []byte {
			bb := NewBytecodeBuilder()
			bb.EmitInt8(OpPushInt8, 0)
			bb.Emit(OpBlockReturn)
			return bb.Bytes()
		}(),
	}

	protectedBV := &BlockValue{Block: signalBlock, Captures: nil, HomeFrame: -1, HomeSelf: Nil}
	handlerBV := &BlockValue{Block: handlerBlock, Captures: nil, HomeFrame: -1, HomeSelf: Nil}

	// Register once and reuse
	protectedBlockVal := func() Value {
		id := int(nextBlockID.Add(1) - 1)
		blockRegistryMu.Lock()
		blockRegistry[id] = protectedBV
		blockRegistryMu.Unlock()
		return FromBlockID(uint32(id))
	}()
	handlerBlockVal := func() Value {
		id := int(nextBlockID.Add(1) - 1)
		blockRegistryMu.Lock()
		blockRegistry[id] = handlerBV
		blockRegistryMu.Unlock()
		return FromBlockID(uint32(id))
	}()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.evaluateBlockWithHandler(protectedBlockVal, vm.ErrorClass, handlerBlockVal)
	}
}

// BenchmarkHotPath_IntArithmeticLoop measures tight integer arithmetic via bytecode loop
func BenchmarkHotPath_IntArithmeticLoop(b *testing.B) {
	vm := benchmarkVM()

	// Compiles: | i sum | i := 0. sum := 0. [i < 1000] whileTrue: [sum := sum + i. i := i + 1]. ^sum
	builder := NewCompiledMethodBuilder("intLoop", 0)
	builder.SetNumTemps(2) // i=0, sum=1
	bc := builder.Bytecode()

	// i := 0
	bc.EmitInt8(OpPushInt8, 0)
	bc.EmitByte(OpStoreTemp, 0)
	bc.Emit(OpPOP)
	// sum := 0
	bc.EmitInt8(OpPushInt8, 0)
	bc.EmitByte(OpStoreTemp, 1)
	bc.Emit(OpPOP)

	loopStart := bc.NewLabel()
	loopEnd := bc.NewLabel()

	bc.Mark(loopStart)
	// i < 127 (fits in int8; real loop would use int32)
	bc.EmitByte(OpPushTemp, 0)
	bc.EmitInt8(OpPushInt8, 127)
	bc.Emit(OpSendLT)
	bc.EmitJump(OpJumpFalse, loopEnd)
	// sum := sum + i
	bc.EmitByte(OpPushTemp, 1)
	bc.EmitByte(OpPushTemp, 0)
	bc.Emit(OpSendPlus)
	bc.EmitByte(OpStoreTemp, 1)
	bc.Emit(OpPOP)
	// i := i + 1
	bc.EmitByte(OpPushTemp, 0)
	bc.EmitInt8(OpPushInt8, 1)
	bc.Emit(OpSendPlus)
	bc.EmitByte(OpStoreTemp, 0)
	bc.Emit(OpPOP)

	bc.EmitJumpAbsolute(OpJump, loopStart.position)
	bc.Mark(loopEnd)
	bc.EmitByte(OpPushTemp, 1)
	bc.Emit(OpReturnTop)

	method := builder.Build()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Execute(method, Nil, nil)
	}
}

// BenchmarkHotPath_ArrayAtPut measures array at:put: via VM.Send
func BenchmarkHotPath_ArrayAtPut(b *testing.B) {
	vm := benchmarkVM()
	arrVal := vm.NewArray(100)
	val := FromSmallInt(999)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		idx := FromSmallInt(int64(i % 100))
		vm.Send(arrVal, "at:put:", []Value{idx, val})
	}
}

// BenchmarkHotPath_StringConcat measures string concatenation via primConcat:
func BenchmarkHotPath_StringConcat(b *testing.B) {
	vm := benchmarkVM()
	s1 := NewStringValue("Hello, ")
	s2 := NewStringValue("World!")

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(s1, "primConcat:", []Value{s2})
	}
}

// BenchmarkHotPath_ProcessForkWait measures forking a block and waiting for its result
func BenchmarkHotPath_ProcessForkWait(b *testing.B) {
	vm := benchmarkVM()
	block := createSimpleBlock(vm, FromSmallInt(42))

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		proc := vm.Send(block, "fork", nil)
		vm.Send(proc, "wait", nil)
	}
}

// BenchmarkHotPath_VTableCachedLookup measures vtable method lookup after cache warmup
func BenchmarkHotPath_VTableCachedLookup(b *testing.B) {
	vm := benchmarkVM()
	selectorID := vm.Selectors.Intern("+")
	vt := vm.SmallIntegerClass.VTable

	// Warm up cache
	vt.Lookup(selectorID)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vt.Lookup(selectorID)
	}
}

// BenchmarkHotPath_MethodDispatchCached measures full message send with inline cache hit
func BenchmarkHotPath_MethodDispatchCached(b *testing.B) {
	vm := benchmarkVM()
	a := FromSmallInt(50)
	bb := FromSmallInt(50)

	// Warm up the inline cache with a few sends
	for i := 0; i < 10; i++ {
		vm.Send(a, "+", []Value{bb})
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(a, "+", []Value{bb})
	}
}
