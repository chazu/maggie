package vm

import (
	"strings"
	"testing"
	"time"
)

func TestJITCompilerCreation(t *testing.T) {
	vm := NewVM()
	jit := vm.EnableJIT()

	if jit == nil {
		t.Fatal("EnableJIT should return JIT compiler")
	}
	if !jit.Enabled {
		t.Error("JIT should be enabled")
	}
	if vm.GetJIT() != jit {
		t.Error("GetJIT should return same instance")
	}
}

func TestJITCompilerDisable(t *testing.T) {
	vm := NewVM()
	jit := vm.EnableJIT()

	vm.DisableJIT()
	if jit.Enabled {
		t.Error("JIT should be disabled")
	}

	// Re-enable
	vm.EnableJIT()
	if !jit.Enabled {
		t.Error("JIT should be re-enabled")
	}
}

func TestJITCompilerHotMethodCompilation(t *testing.T) {
	vm := NewVM()
	jit := vm.EnableJIT()
	jit.LogCompilation = false

	// Create a test class and method
	class := vm.createClass("TestClass", vm.ObjectClass)

	// Create a simple method that returns 42
	builder := NewCompiledMethodBuilder("answer", 0)
	builder.Bytecode().EmitInt8(OpPushInt8, 42)
	builder.Bytecode().Emit(OpReturnTop)
	method := builder.Build()
	method.SetClass(class)

	// Set low threshold for testing
	profiler := vm.GetProfiler()
	profiler.MethodHotThreshold = 5

	// Invoke enough times to trigger hot
	for i := 0; i < 10; i++ {
		profiler.RecordMethodInvocation(method)
	}

	// Wait for background compilation
	time.Sleep(50 * time.Millisecond)

	// Check that method was compiled
	stats := jit.Stats()
	if stats.MethodsCompiled == 0 {
		t.Error("Expected at least one method to be compiled")
	}

	// Check compiled code exists
	code := jit.GetCompiledMethod("TestClass", "answer")
	if code == "" {
		t.Error("Expected compiled code for TestClass>>answer")
	}

	// Verify code contains expected elements
	if !strings.Contains(code, "aot_TestClass_answer") {
		t.Error("Compiled code should contain function name")
	}
	if !strings.Contains(code, "return") {
		t.Error("Compiled code should contain return")
	}
}

func TestJITCompilerDuplicatePrevention(t *testing.T) {
	vm := NewVM()
	jit := vm.EnableJIT()

	// Create a test method
	class := vm.createClass("TestClass", vm.ObjectClass)
	builder := NewCompiledMethodBuilder("test", 0)
	builder.Bytecode().Emit(OpPushNil)
	builder.Bytecode().Emit(OpReturnTop)
	method := builder.Build()
	method.SetClass(class)

	// Set low threshold
	profiler := vm.GetProfiler()
	profiler.MethodHotThreshold = 2

	// Trigger hot multiple times
	for i := 0; i < 20; i++ {
		profiler.RecordMethodInvocation(method)
	}

	// Wait for compilation
	time.Sleep(50 * time.Millisecond)

	// Should only compile once
	stats := jit.Stats()
	if stats.MethodsCompiled != 1 {
		t.Errorf("Expected 1 method compiled, got %d", stats.MethodsCompiled)
	}
}

func TestJITCompilerStats(t *testing.T) {
	vm := NewVM()
	jit := vm.EnableJIT()

	stats := jit.Stats()
	if stats.MethodsCompiled != 0 {
		t.Error("Initial methods compiled should be 0")
	}
	if stats.HotMethodCount != 0 {
		t.Error("Initial hot method count should be 0")
	}
}

func TestJITCompilerReset(t *testing.T) {
	vm := NewVM()
	jit := vm.EnableJIT()

	// Create and compile a method
	class := vm.createClass("TestClass", vm.ObjectClass)
	builder := NewCompiledMethodBuilder("test", 0)
	builder.Bytecode().Emit(OpReturnNil)
	method := builder.Build()
	method.SetClass(class)

	profiler := vm.GetProfiler()
	profiler.MethodHotThreshold = 1
	profiler.RecordMethodInvocation(method)

	time.Sleep(50 * time.Millisecond)

	// Verify something was compiled
	stats := jit.Stats()
	if stats.MethodsCompiled == 0 {
		t.Skip("Method not compiled yet")
	}

	// Reset
	jit.Reset()

	stats = jit.Stats()
	if stats.MethodsCompiled != 0 {
		t.Error("Reset should clear methods compiled count")
	}
	if stats.HotMethodCount != 0 {
		t.Error("Reset should clear hot method count")
	}
}

func TestJITCompilerCompiledMethods(t *testing.T) {
	vm := NewVM()
	jit := vm.EnableJIT()

	// Create multiple methods
	class := vm.createClass("TestClass", vm.ObjectClass)
	profiler := vm.GetProfiler()
	profiler.MethodHotThreshold = 1

	methods := []string{"method1", "method2", "method3"}
	for _, name := range methods {
		builder := NewCompiledMethodBuilder(name, 0)
		builder.Bytecode().Emit(OpReturnNil)
		method := builder.Build()
		method.SetClass(class)
		profiler.RecordMethodInvocation(method)
	}

	// Wait for compilation
	time.Sleep(100 * time.Millisecond)

	compiled := jit.CompiledMethods()
	if len(compiled) != 3 {
		t.Errorf("Expected 3 compiled methods, got %d", len(compiled))
	}
}

func TestJITCompilerGenerateAOTPackage(t *testing.T) {
	vm := NewVM()
	jit := vm.EnableJIT()

	// Create a method
	class := vm.createClass("TestClass", vm.ObjectClass)
	builder := NewCompiledMethodBuilder("compute", 0)
	builder.Bytecode().EmitInt8(OpPushInt8, 42)
	builder.Bytecode().Emit(OpReturnTop)
	method := builder.Build()
	method.SetClass(class)

	profiler := vm.GetProfiler()
	profiler.MethodHotThreshold = 1
	profiler.RecordMethodInvocation(method)

	time.Sleep(50 * time.Millisecond)

	// Generate package
	pkg := jit.GenerateAOTPackage("hotcode")

	if !strings.Contains(pkg, "package hotcode") {
		t.Error("Package should have correct name")
	}
	if !strings.Contains(pkg, "Code generated by Maggie JIT") {
		t.Error("Package should have generation comment")
	}
	if !strings.Contains(pkg, "RegisterAll") {
		t.Error("Package should have RegisterAll function")
	}
	if !strings.Contains(pkg, "AOTDispatchKey") {
		t.Error("Package should register methods in dispatch table")
	}
}

func TestJITCompilerDisabledNoCompilation(t *testing.T) {
	vm := NewVM()
	jit := vm.EnableJIT()
	jit.Enabled = false // Disable

	// Create a method
	class := vm.createClass("TestClass", vm.ObjectClass)
	builder := NewCompiledMethodBuilder("test", 0)
	builder.Bytecode().Emit(OpReturnNil)
	method := builder.Build()
	method.SetClass(class)

	profiler := vm.GetProfiler()
	profiler.MethodHotThreshold = 1
	profiler.RecordMethodInvocation(method)

	time.Sleep(50 * time.Millisecond)

	stats := jit.Stats()
	if stats.MethodsCompiled != 0 {
		t.Error("Disabled JIT should not compile methods")
	}
}

func TestJITCompilerMethodKey(t *testing.T) {
	vm := NewVM()
	jit := vm.EnableJIT()

	class := vm.createClass("MyClass", vm.ObjectClass)
	builder := NewCompiledMethodBuilder("myMethod:", 1)
	builder.Bytecode().Emit(OpReturnNil)
	method := builder.Build()
	method.SetClass(class)

	key := jit.methodKey(method)
	if key != "MyClass>>myMethod:" {
		t.Errorf("Expected 'MyClass>>myMethod:', got %q", key)
	}
}

func TestJITCompilerSanitizeName(t *testing.T) {
	vm := NewVM()
	jit := vm.EnableJIT()

	tests := []struct {
		input    string
		expected string
	}{
		{"simple", "simple"},
		{"with:colons:", "with_colons_"},
		{"+", "Plus"},
		{"-", "Minus"},
		{"at:put:", "at_put_"},
		{"<", "LT"},
		{">", "GT"},
		{"<=", "LTEQ"},
	}

	for _, tc := range tests {
		result := jit.sanitizeName(tc.input)
		if result != tc.expected {
			t.Errorf("sanitizeName(%q) = %q, want %q", tc.input, result, tc.expected)
		}
	}
}

func TestJITCompilerBlockCompilation(t *testing.T) {
	vm := NewVM()
	jit := vm.EnableJIT()
	jit.LogCompilation = false

	// Create a method with a block
	class := vm.createClass("BlockTest", vm.ObjectClass)
	builder := NewCompiledMethodBuilder("withBlock", 0)
	builder.Bytecode().Emit(OpReturnNil)
	method := builder.Build()
	method.SetClass(class)

	// Create a block
	block := &BlockMethod{
		Arity:       1,
		NumTemps:    1,
		NumCaptures: 1,
		Bytecode: []byte{
			byte(OpPushCaptured), 0,
			byte(OpPushTemp), 0,
			byte(OpSendPlus),
			byte(OpReturnTop),
		},
		Outer: method,
	}

	// Compile the block directly
	jit.compileBlock(block, method, 0)

	// Check that block was compiled
	stats := jit.Stats()
	if stats.BlocksCompiled != 1 {
		t.Errorf("Expected 1 block compiled, got %d", stats.BlocksCompiled)
	}

	// Check compiled code exists
	code := jit.GetCompiledBlock("BlockTest", "withBlock", 0)
	if code == "" {
		t.Error("Expected compiled code for block")
	}

	// Verify code contains expected elements
	if !strings.Contains(code, "aot_BlockTest_withBlock_block0") {
		t.Error("Compiled code should contain block function name")
	}
	if !strings.Contains(code, "captures") {
		t.Error("Compiled code should reference captures")
	}
	if !strings.Contains(code, "homeReturn") {
		t.Error("Compiled code should have homeReturn parameter")
	}
}

func TestJITCompilerBlockDuplicatePrevention(t *testing.T) {
	vm := NewVM()
	jit := vm.EnableJIT()
	jit.LogCompilation = false

	class := vm.createClass("DupBlockTest", vm.ObjectClass)
	builder := NewCompiledMethodBuilder("test", 0)
	builder.Bytecode().Emit(OpReturnNil)
	method := builder.Build()
	method.SetClass(class)

	block := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 0,
		Bytecode:    []byte{byte(OpReturnNil)},
		Outer:       method,
	}

	// Compile same block multiple times
	for i := 0; i < 5; i++ {
		jit.compileBlock(block, method, 0)
	}

	// Should only compile once
	stats := jit.Stats()
	if stats.BlocksCompiled != 1 {
		t.Errorf("Expected 1 block compiled (deduplication), got %d", stats.BlocksCompiled)
	}
}

func TestJITCompilerCompiledBlocks(t *testing.T) {
	vm := NewVM()
	jit := vm.EnableJIT()
	jit.LogCompilation = false

	class := vm.createClass("MultiBlockTest", vm.ObjectClass)
	builder := NewCompiledMethodBuilder("test", 0)
	builder.Bytecode().Emit(OpReturnNil)
	method := builder.Build()
	method.SetClass(class)

	// Compile multiple blocks
	for i := 0; i < 3; i++ {
		block := &BlockMethod{
			Arity:       0,
			NumTemps:    0,
			NumCaptures: 0,
			Bytecode:    []byte{byte(OpPushInt8), byte(i), byte(OpReturnTop)},
			Outer:       method,
		}
		jit.compileBlock(block, method, i)
	}

	blocks := jit.CompiledBlocks()
	if len(blocks) != 3 {
		t.Errorf("Expected 3 compiled blocks, got %d", len(blocks))
	}
}

func TestJITCompilerAOTPackageWithBlocks(t *testing.T) {
	vm := NewVM()
	jit := vm.EnableJIT()
	jit.LogCompilation = false

	class := vm.createClass("PackageTest", vm.ObjectClass)

	// Compile a method
	builder := NewCompiledMethodBuilder("compute", 0)
	builder.Bytecode().EmitInt8(OpPushInt8, 42)
	builder.Bytecode().Emit(OpReturnTop)
	method := builder.Build()
	method.SetClass(class)
	jit.compileMethod(method)

	// Compile a block
	block := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 0,
		Bytecode:    []byte{byte(OpPushInt8), 99, byte(OpReturnTop)},
		Outer:       method,
	}
	jit.compileBlock(block, method, 0)

	// Generate package
	pkg := jit.GenerateAOTPackage("testpkg")

	// Verify package contains both methods and blocks
	if !strings.Contains(pkg, "Compiled Methods") {
		t.Error("Package should have methods section")
	}
	if !strings.Contains(pkg, "Compiled Blocks") {
		t.Error("Package should have blocks section")
	}
	if !strings.Contains(pkg, "aot_PackageTest_compute") {
		t.Error("Package should contain method")
	}
	if !strings.Contains(pkg, "aot_PackageTest_compute_block0") {
		t.Error("Package should contain block")
	}
}

// BenchmarkJITCompilation measures JIT compilation overhead.
func BenchmarkJITCompilation(b *testing.B) {
	vm := NewVM()
	jit := vm.EnableJIT()

	class := vm.createClass("BenchClass", vm.ObjectClass)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		builder := NewCompiledMethodBuilder("bench", 0)
		builder.Bytecode().EmitInt8(OpPushInt8, 42)
		builder.Bytecode().Emit(OpReturnTop)
		method := builder.Build()
		method.SetClass(class)

		jit.compileMethod(method)

		// Reset to allow recompilation
		jit.mu.Lock()
		delete(jit.compiledKeys, jit.methodKey(method))
		delete(jit.hotMethods, jit.methodKey(method))
		jit.mu.Unlock()
	}
}

// BenchmarkJITBlockCompilation measures block compilation overhead.
func BenchmarkJITBlockCompilation(b *testing.B) {
	vm := NewVM()
	jit := vm.EnableJIT()

	class := vm.createClass("BenchClass", vm.ObjectClass)
	builder := NewCompiledMethodBuilder("test", 0)
	builder.Bytecode().Emit(OpReturnNil)
	method := builder.Build()
	method.SetClass(class)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		block := &BlockMethod{
			Arity:       1,
			NumTemps:    1,
			NumCaptures: 2,
			Bytecode: []byte{
				byte(OpPushCaptured), 0,
				byte(OpPushTemp), 0,
				byte(OpSendPlus),
				byte(OpReturnTop),
			},
			Outer: method,
		}

		jit.compileBlock(block, method, 0)

		// Reset
		jit.mu.Lock()
		key := jit.blockKey(method, 0)
		delete(jit.compiledKeys, key)
		delete(jit.hotBlocks, key)
		jit.mu.Unlock()
	}
}
