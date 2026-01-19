package vm

import (
	"testing"
)

// ---------------------------------------------------------------------------
// Block creation and evaluation tests
// ---------------------------------------------------------------------------

func TestBlockClassFor(t *testing.T) {
	vm := NewVM()
	blockVal := FromBlockID(1)

	class := vm.ClassFor(blockVal)
	if class != vm.BlockClass {
		t.Errorf("ClassFor(block) = %v, want BlockClass", class)
	}
}

func TestBlockVTableLookup(t *testing.T) {
	vm := NewVM()

	// Check that BlockClass has the expected methods
	methods := []string{"value", "value:", "value:value:", "primValue", "primValue:", "primValue:value:"}
	for _, name := range methods {
		selID := vm.Selectors.Lookup(name)
		method := vm.BlockClass.VTable.Lookup(selID)
		if method == nil {
			t.Errorf("BlockClass should have method %q", name)
		}
	}
}

func TestBlockEvaluationNoArgs(t *testing.T) {
	vm := NewVM()

	// Create a simple block that returns a constant: [42]
	block := &BlockMethod{
		Arity:    0,
		NumTemps: 0,
		Bytecode: []byte{0x14, 42, 0x73}, // OpPushInt8 42, OpBlockReturn
		Literals: nil,
	}

	// Register the block
	interp := vm.interpreter
	blockVal := interp.createBlockValue(block, nil)

	// Evaluate via primValue
	result := vm.evaluateBlock(blockVal, nil)
	if !result.IsSmallInt() {
		t.Fatalf("Expected SmallInt, got %v", result)
	}
	if result.SmallInt() != 42 {
		t.Errorf("Expected 42, got %d", result.SmallInt())
	}
}

func TestBlockEvaluationOneArg(t *testing.T) {
	vm := NewVM()

	// Create a block that returns its argument: [:x | x]
	block := &BlockMethod{
		Arity:  1,
		NumTemps: 0,
		Bytecode:  []byte{0x20, 0, 0x73}, // OpPushTemp 0, OpBlockReturn
		Literals:  nil,
	}

	// Register the block
	interp := vm.interpreter
	blockVal := interp.createBlockValue(block, nil)

	// Evaluate with argument
	result := vm.evaluateBlock(blockVal, []Value{FromSmallInt(99)})
	if !result.IsSmallInt() {
		t.Fatalf("Expected SmallInt, got %v", result)
	}
	if result.SmallInt() != 99 {
		t.Errorf("Expected 99, got %d", result.SmallInt())
	}
}

func TestBlockEvaluationTwoArgs(t *testing.T) {
	vm := NewVM()

	// Create a block that adds its arguments: [:a :b | a + b]
	// Actually simpler: just return the second arg
	block := &BlockMethod{
		Arity:  2,
		NumTemps: 0,
		Bytecode:  []byte{0x20, 1, 0x73}, // OpPushTemp 1, OpBlockReturn (return second arg)
		Literals:  nil,
	}

	// Register the block
	interp := vm.interpreter
	blockVal := interp.createBlockValue(block, nil)

	// Evaluate with arguments
	result := vm.evaluateBlock(blockVal, []Value{FromSmallInt(10), FromSmallInt(20)})
	if !result.IsSmallInt() {
		t.Fatalf("Expected SmallInt, got %v", result)
	}
	if result.SmallInt() != 20 {
		t.Errorf("Expected 20, got %d", result.SmallInt())
	}
}

func TestBlockSendValue(t *testing.T) {
	vm := NewVM()

	// Create a simple block: [123]
	block := &BlockMethod{
		Arity:  0,
		NumTemps: 0,
		Bytecode:  []byte{0x14, 123, 0x73}, // OpPushInt8 123, OpBlockReturn
		Literals:  nil,
	}

	// Register the block
	interp := vm.interpreter
	blockVal := interp.createBlockValue(block, nil)

	// Call via Send
	result := vm.Send(blockVal, "value", nil)
	if !result.IsSmallInt() {
		t.Fatalf("Expected SmallInt from Send, got %v", result)
	}
	if result.SmallInt() != 123 {
		t.Errorf("Expected 123, got %d", result.SmallInt())
	}
}

func TestBlockSendValueWithArg(t *testing.T) {
	vm := NewVM()

	// Create a block: [:x | x]
	block := &BlockMethod{
		Arity:  1,
		NumTemps: 0,
		Bytecode:  []byte{0x20, 0, 0x73}, // OpPushTemp 0, OpBlockReturn
		Literals:  nil,
	}

	// Register the block
	interp := vm.interpreter
	blockVal := interp.createBlockValue(block, nil)

	// Call via Send with argument
	result := vm.Send(blockVal, "value:", []Value{FromSmallInt(77)})
	if !result.IsSmallInt() {
		t.Fatalf("Expected SmallInt from Send, got %v", result)
	}
	if result.SmallInt() != 77 {
		t.Errorf("Expected 77, got %d", result.SmallInt())
	}
}

func TestBlockSendPrimValue(t *testing.T) {
	vm := NewVM()

	// Create a simple block: [456]
	block := &BlockMethod{
		Arity:  0,
		NumTemps: 0,
		Bytecode:  []byte{0x14, 0, 0x15, 0xC8, 0x01, 0x00, 0x00, 0x73}, // OpPushInt8 0, OpPushInt32 456, OpBlockReturn
		Literals:  nil,
	}

	// Actually simpler - just use int8
	block = &BlockMethod{
		Arity:  0,
		NumTemps: 0,
		Bytecode:  []byte{0x14, 100, 0x73}, // OpPushInt8 100, OpBlockReturn
		Literals:  nil,
	}

	// Register the block
	interp := vm.interpreter
	blockVal := interp.createBlockValue(block, nil)

	// Call via Send with primValue
	result := vm.Send(blockVal, "primValue", nil)
	if !result.IsSmallInt() {
		t.Fatalf("Expected SmallInt from primValue, got %v", result)
	}
	if result.SmallInt() != 100 {
		t.Errorf("Expected 100, got %d", result.SmallInt())
	}
}

func TestBlockGetBlockValue(t *testing.T) {
	vm := NewVM()

	// Create and register a block
	block := &BlockMethod{
		Arity:  0,
		NumTemps: 0,
		Bytecode:  []byte{0x10, 0x73}, // OpPushNil, OpBlockReturn
		Literals:  nil,
	}

	interp := vm.interpreter
	blockVal := interp.createBlockValue(block, nil)

	// Verify we can get it back
	bv := interp.getBlockValue(blockVal)
	if bv == nil {
		t.Fatal("getBlockValue returned nil")
	}
	if bv.Block != block {
		t.Error("getBlockValue returned wrong block")
	}
}

func TestBlockGetBlockValueNotABlock(t *testing.T) {
	vm := NewVM()
	interp := vm.interpreter

	// Try to get block value from non-block values
	tests := []Value{
		Nil,
		True,
		False,
		FromSmallInt(42),
		FromSymbolID(1),
	}

	for _, v := range tests {
		bv := interp.getBlockValue(v)
		if bv != nil {
			t.Errorf("getBlockValue(%v) should return nil for non-block", v)
		}
	}
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

func BenchmarkBlockEvaluation(b *testing.B) {
	vm := NewVM()

	block := &BlockMethod{
		Arity:  0,
		NumTemps: 0,
		Bytecode:  []byte{0x14, 42, 0x73},
		Literals:  nil,
	}

	interp := vm.interpreter
	blockVal := interp.createBlockValue(block, nil)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.evaluateBlock(blockVal, nil)
	}
}

func BenchmarkBlockSendValue(b *testing.B) {
	vm := NewVM()

	block := &BlockMethod{
		Arity:  0,
		NumTemps: 0,
		Bytecode:  []byte{0x14, 42, 0x73},
		Literals:  nil,
	}

	interp := vm.interpreter
	blockVal := interp.createBlockValue(block, nil)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Send(blockVal, "value", nil)
	}
}
